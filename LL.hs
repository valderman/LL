{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module LL where

import Data.Monoid
import Control.Lens
import Data.List (elemIndex, insert)

-- | Type of names. These are just comments for pretty-printing
-- purposes; internally the code relies only on deBrujn indices.
type Name = String

-- | Types
data Type = Type :⊕: Type
             | Type :⊗: Type
             | Type :|: Type -- ⅋ 
             | Type :&: Type
             | Zero | One | Top | Bot
             | TVar Bool Int -- False if the variable is negated.
             | Forall Name Type 
             | Exists Name Type
             | Bang Type
             | Quest Type
             | Meta Bool String [Type] -- 'meta' type (just for the paper, not found in actual code)
                                       --  2nd arg are the types occuring in it.
  deriving Eq



-- Type-construction helper functions
a ⅋ b = a :|: b
a ⊗ b = a :⊗: b
a ⊕ b = a :⊕: b
a & b = a :&: b
a ⊸ b = neg a ⅋ b
meta x = Meta True x []
var = TVar True
infixr 6 ⊗,&
infixr 5 ⅋, ⊸, ⊕

type Forced x = x
dum = meta "dummy type"

-- | Linear negation
neg :: Type -> Type       
neg (x :⊗: y) = neg x :|: neg y
neg (x :|: y) = neg x :⊗: neg y
neg (x :⊕: y) = neg x :&: neg y
neg (x :&: y) = neg x :⊕: neg y
neg Zero = Top
neg Top = Zero
neg Bot = One
neg One = Bot
neg (Exists v t) = Forall v (neg t)
neg (Forall v t) = Exists v (neg t)
neg (TVar b x) = TVar (not b) x
neg (Bang t) = Quest (neg t)
neg (Quest t) = Bang (neg t)
neg (Meta b x xs) = Meta (not b) x xs

-- | Classification of types as positive and negative
positiveType t = case t of
  One -> True
  Zero -> True
  _ :⊗: _ -> True
  _ :⊕: _ -> True            
  Bang _ -> True
  Exists _ _ -> True
  Meta b _ _ -> b -- strictly speaking, this is wrong since we do not know the instance of the meta-type.
  TVar b _ -> b -- same remark.
  _ -> False

-- | Negate if negative.
mkPositive t = if positiveType t then t else neg t

type Permutation = [Int]

-- | Sequents. A 'Forced Type' is a type which is fully determined by
-- the context. They can be filled in by the function 'fillTypes' below.
data Seq = Ax (Forced Type) -- Exactly 2 vars
         | Cut Name Name (Type) Int (Seq) (Seq) -- new vars in position 0
           
         | Cross (Forced Type) Name Name Int (Seq) 
         | Par (Forced Type) Name Name Int -- Variable worked on
                                       [Int] -- these variables go to the left 
                                       Seq (Seq) 
         | Plus Name Name Int (Seq) (Seq) -- Rename to Case
         | With Name Bool Int (Seq) -- Rename to Choose
           
         | SOne Int Seq      -- Rename to ...
         | SZero Int         -- Rename to Crash/Loop
         | SBot              -- Rename to Terminate
         | What Name [Int]   -- 'meta' program (just for the paper, not found in actual code)
           
         | TApp (Forced Type) Name Int Type (Seq)
         | TUnpack Name Int (Seq)

         | Offer Name Int (Seq)
         | Demand Name (Forced Type) Int (Seq)
         | Ignore Int (Seq)
         | Alias Int Name (Seq)

-- | A full derivation
data Deriv = Deriv {derivTypeVars :: [Name], derivContext :: [(Name,Type)], derivSequent :: Seq}

-- | The variable that the sequent operates on. Remark that there is
-- always exactly one.
varOf (Cut _ _ _ x _ _) = x
varOf (Cross _ _ _ x _) = x
varOf (Par   _ _ _ x _ _ _) = x
varOf (Plus _ _ x _ _) = x
varOf (With _ _ x _) = x
varOf (SOne x _) = x
varOf (SZero x) = x
varOf (TApp _ _ x _ _) = x
varOf (TUnpack _ x _) = x
varOf (Offer _ x _) = x
varOf (Demand _ _ x _) = x
varOf (Ignore x _) = x
varOf (Alias x _ _) = x

---------------------------
-- Substitution machinery


-- | Type-variables substitutions               
type Subst = [Type]

subst0 x = x:map var [0..]

-- | Types which can be applied a 'Subst'
class Substitute a where
  (∙) :: Subst -> a -> a
         
instance Substitute (Type)  where
  (∙) = apply
        
instance Substitute (Seq) where
  (∙) = applyS
        
instance (Substitute a, Substitute b) => Substitute (a,b) where
  f ∙ (x,y) = (f∙x, f∙y)

instance (Substitute a) => Substitute [a] where
  f ∙ xs = map (f ∙) xs
             

wk :: Subst
wk = map var [1..]

if_ True f = id
if_ False f = f

apply :: Subst -> Type -> Type
apply f t = case t of
  x :⊕: y -> s x :⊕: s y
  x :&: y -> s x :&: s y
  x :|: y -> s x :|: s y
  x :⊗: y -> s x :⊗: s y
  Zero -> Zero
  One -> One
  Top -> Top
  Bot -> Bot
  TVar pol x -> if_ pol neg (f!!x)
  Forall w t -> Forall w (s' t)
  Exists w t -> Forall w (s' t)
  Bang t -> Bang (s t)
  Quest t -> Quest (s t)
  Meta b x ns -> Meta b x (f ∙ ns)
 where s = apply f
       s' = apply (var 0 : wk ∙ f)
  
applyS :: Subst -> Seq -> Seq
applyS f t = case t of
  Cut w w' ty x a b -> Cut w w' (f ∙ ty) x (s a) (s b)
  Cross ty w w' x a -> Cross ty w w' x (s a)         
  Par ty w w' x xs a b -> Par ty w w' x xs (s a) (s b)
  Plus w w' x a b -> Plus w w' x (s a) (s b)
  With w c x a -> With w c x (s a)
  SOne x a -> SOne x (s a) 
  TApp tp w x ty a -> TApp ((var 0:wk∙f)∙tp) w x (f ∙ ty) (s a)
  TUnpack w x a -> TUnpack w x (s' a)
  Offer w x a -> Offer w x (s a)
  Demand w ty x a -> Demand w ty x (s a)
  Ignore x a -> Ignore x (s a)
  Alias x w a -> Alias x w (s a)
  a -> a
 where s = applyS f
       s' = applyS (var 0 : wk ∙ f)
       
---------------------------------------
-- Cut-elimination machinery

-- | One step to eliminate the principal cut.
eval :: Deriv -> Deriv
eval (Deriv ts vs (Cut w w' ty γ a b)) = Deriv ts vs $ cut (length vs) w w' ty γ a b

remove0 π = [x-1 | x <- π, x > 0]

cut' n = Cut

cut :: Int -> -- ^ size of the context
       Name -> 
       Name -> 
       Type -> 
       Int -> -- ^ where to cut it
       Seq -> Seq -> Seq
-- FIXME: in the absence of "What" cut can be eliminated so these
-- recursive calls terminate. Otherwise, we have a problem.  At the
-- moment it seems we never use hereditary cut, so this should
-- probably go away.
       

-- cut n w ty γ (Cut w' ty' δ a b) c = cut n w ty γ (cut γ w' ty' δ a b) c
-- cut n w ty γ a (Cut w' ty' δ b c) = cut n w ty γ a (cut (n-γ+1) w' ty' δ b c)

-- cut n w w' ty γ (Par ty' v x s t) u | x > 0 = Par ty' v (x-1) 
cut n w w' ty γ (Cross ty' v v' x s) t | x > 0 = Cross ty' v v' (x-1) (Cut w w' ty (γ+1) s t)
cut n w w' ty γ (SOne x s) t | x > 0 = SOne (x-1) (Cut w w' ty (γ-1) s t)
cut n w w' ty γ (Plus v v' x s t) u | x > 0 = Plus v v' (x-1) (Cut w w' ty γ s u) (Cut w w' ty γ t u) 
cut n w w' ty γ (With v c x s) t | x > 0 = With v c (x-1) (Cut w w' ty γ s t)
cut n w w' ty γ (SZero x) t | x > 0 = SZero (x-1) 
cut n w w' ty γ (TApp tp v x ty' s) t | x > 0 = TApp tp v (x-1) ty' (Cut w w' ty γ s t)
cut n w w' ty γ (TUnpack v x s) t | x > 0 = TUnpack v (x-1) (Cut w w' ty γ s t)
cut n w w' ty γ (Offer v x s) t | x > 0 = Offer v (x-1) (Cut w w' ty γ s t)
cut n w w' ty γ (Demand v ty' x s) t | x > 0 = Demand v ty' (x-1) (Cut w w' ty γ s t)
-- TODO: other commutation rules.
cut 2 _ _ ty 1 (Ax _) a = a
-- cut n _ _ (ta :⊗: tb) 
--            γδ (Par _ _ _ γ a b) (Cross _ w w' 0 c) = exchange (remove0 π++[length π-1..n-1]) $ cut' n w w' ta γ 
--                                                           a  
--                                                           (exchange ([1..δ] ++ [0] ++ [δ+1..n-1]) $ cut' (n-γ+1) w w' tb δ b c )
--    where δ = γδ - γ
cut n _ _ (ta :⊕: tb) 
           γ (With z c 0 a) (Plus w w' 0 s t) = cut' n z (if c then w else w') (if c then ta else tb) γ a (if c then s else t)
cut n _ _ (Exists v ty) 
           γ (TApp _ z 0 t a) (TUnpack w 0 b) = cut' n z w (subst0 t ∙ ty) γ a (subst0 t ∙ b)
cut n _ _ (Bang ty) 
           γ (Offer z 0 a) (Demand w _ 0 b) = cut' n z w ty γ a b
cut n _ _ ty γ (Offer _ 0 a) (Ignore 0 b) = ignore γ b
cut n _ _ ty γ (Offer w 0 b) (Alias 0 w' a) = alias (reverse [0..γ-1]) (cut' (n+γ) w w' ty γ (Offer w 0 b) ((exchange ([1..γ] ++ [0] ++ [γ+1..n] ) $ cut' (n+1) w w' ty γ (Offer w 0 b) a)))
cut n _ _ ty γ SBot (SOne 0 a) = a
cut n w w' ty γ a b | isPos b = exchange ([γ..n-1] ++ [0..γ]) (cut n w w' (neg ty) (n-γ) b a)
cut n w w' ty γ a b = Cut w w' ty γ a b

ignore 0 a = a
ignore n a = Ignore 0 (ignore (n-1) a)

alias [] a = a
alias (x:xs) a = Alias x mempty $ alias xs a

isPos (Ax _) = True
isPos (Par _ _ _ _ _ _ _) = True
isPos (With _ _  _ _) = True
isPos (Offer _ _ _) = True
isPos (TApp _ _ _ _ _) = True
isPos SBot = True
isPos _ = False

inverse :: Permutation -> Permutation
inverse π = [found $ elemIndex x π | x <- [0..length π-1]]
  where found (Just x) = x

exchange = subst
-- | Application of variables-only substitution
subst π t = case t of
  (Ax ty) -> (Ax ty)
  (Cut w w' ty x t u) -> Cut w w' ty (f x) (s t) (s u)
  (Par ty w w' x xs s t) -> Par ty w w' (f x) (map f xs) (keep (x:xs) `subst` s) ((s' 0 $ del (x:xs) $ π) `subst` t)
  (Cross ty w w' x c) -> Cross ty w w' (f x) (s' x π `subst` c)
  (With w c x a) -> With w c (f x) (s a) 
  (Plus w w' x a b) -> Plus w w' (f x) (s a) (s b)
  (TApp tp w x t a) -> TApp tp w (f x) t (s a)
  (TUnpack w x a) -> TUnpack w (f x) (s a)
  (Offer w x a) -> Offer w (f x) (s a)
  (Demand w ty x a) -> Demand w ty (f x) (s a)
  (Alias x w a) -> Alias (f x) w (s' x π `subst` a)
  (Ignore x a) -> Ignore (f x) (del [x] π `subst` a)
  (SOne x a) -> SOne (f x) (del [x] π `subst` a)
  (SZero x) -> SZero (f x)
  SBot -> SBot
  What nm xs -> What nm (map f xs)
 where f = (π!!)
       s = subst π
       s' x ρ = (l++x:r)
                  where (l,r) = splitAt x $ map (\y -> if y >= x then y+1 else y) ρ
       del [] ρ = ρ
       del (x:xs) ρ = map (\y -> if y > x then y-1 else y) $ filter (/= x) $ del xs ρ
       keep xs = map f xs


----------------------------------        
-- Paramorphism for sequents.
        
data SeqFinal t a = SeqFinal
     { sty :: [Name] -> Type -> t
     , sax :: (Name -> Name -> Type -> a)
     , scut :: (Name -> Name -> t -> a -> t -> a -> a)
     , scross :: (Name -> Name -> t -> Name -> t -> a -> a)
     , spar   :: (Name -> Name -> t -> Name -> t -> a -> a -> a)
     , swith :: (Bool -> Name -> Name -> t -> a -> a)
     , splus :: (Name -> Name -> t -> Name -> t -> a -> a -> a)
     , stapp :: (Name -> t -> Name -> t -> a -> a)
     , stunpack :: (Name -> Name -> Name -> a -> a)
     , sbot :: (Name -> a)
     , szero :: (Name -> [(Name, t)] -> a)
     , sone :: (Name -> a -> a)
     , soffer :: (Name -> Name -> t -> a -> a)
     , sdemand :: (Name ->  Name -> t -> a -> a)
     , signore :: (Name -> t -> a -> a)
     , salias :: (Name -> Name -> t -> a -> a)
     , swhat :: (Name -> [Name] -> a)
     }

foldSeq :: (Deriv -> SeqFinal t a)
     -> [Name] -- ^ ty vars
     -> [(Name, Type)] -- ^ vars
     -> Seq
     -> a

foldSeq sf ts0 vs0 s0 = 
 recurse ts0 vs0 s0 where
 recurse ts vs seq = case seq of
      Ax _ -> sax v0 v1 vt where [(v0,_),(v1,vt)] = vs 
      (Cut v v' vt x s t) -> scut v v' (fty (neg vt)) (recurse ts ((v,neg vt):v0) s)
                                       (fty      vt ) (recurse ts ((v',vt):v1) t)
        where (v0,v1) = splitAt x vs
      (Cross _ v v' x t) -> scross w v (fty vt) v' (fty vt') $ recurse ts (v0++(v,vt):(v',vt'):v1) t
        where (v0,(w,~(vt :⊗: vt')):v1) = splitAt x vs
      (Par _ v v' x xs s t) -> spar w v (fty vt) v' (fty vt') (recurse ts ((v,vt):w0) s) (recurse ts ((v',vt'):w1) t)
        where (v0,(w,~(vt :|: vt')):v1) = splitAt x vs
              w0 = [w | (i,w) <- zip [0..] vs , i `elem` xs]
              w1 = [w | (i,w) <- zip [0..] vs, not (i `elem` xs) && i /= x]
      (Plus v v' x s t) -> splus w v (fty vt) v' (fty vt') (recurse ts (v0++(v,vt ):v1) s) (recurse ts (v0++(v',vt'):v1) t)
        where (v0,(w,~(vt :⊕: vt')):v1) = splitAt x vs
      (With v b x t) -> swith b w v (fty wt) $ recurse ts (v0++(v,wt):v1) t
         where wt = if b then vt else vt'
               (v0,(w,~(vt :&: vt')):v1) = splitAt x vs
      SBot -> sbot v
         where [(v,~Bot)] = vs
      (SZero x) -> szero w (fctx (v0 ++ v1))
         where (v0,(w,~Zero):v1) = splitAt x vs
      (SOne x t) -> sone w $ recurse ts (v0++v1) t
        where (v0,(w,~One):v1) = splitAt x vs
      (TApp _ v x tyB s) -> stapp w (sty (v:ts) tyA) v (fty tyB) $ recurse ts (v0++(v,ty):v1) s
        where (v0,(w,~(Forall _ tyA)):v1) = splitAt x vs
              ty = subst0 tyB ∙ tyA
      (TUnpack v x s) -> stunpack tw w v $ recurse (tw:ts) (upd v0++(v,tyA):upd v1) s
        where (v0,(w,~(Exists tw tyA)):v1) = splitAt x vs
              upd = map (second (wk ∙))
      (Offer v x s) -> soffer w v  (fty tyA) $ recurse ts (v0++(v,tyA):v1) s
        where (v0,(w,~(Quest tyA)):v1) = splitAt x vs
      (Demand v _ x s) -> sdemand w v (fty tyA) $ recurse ts (v0++(v,tyA):v1) s
        where (v0,(w,~(Bang tyA)):v1) = splitAt x vs
      (Ignore x s) -> signore w (fty tyA) $ recurse ts (v0++v1) s
        where (v0,(w,~(Bang tyA)):v1) = splitAt x vs
      (Alias x w' s) -> salias w w' (fty tyA) $ recurse ts ((w,Bang tyA):v0++(w',Bang tyA):v1) s 
        where (v0,(w,~(Bang tyA)):v1) = splitAt x vs
      What x ws -> swhat x [fst (vs !! w) | w <- ws]
   where fty = sty ts 
         fctx = over (mapped._2) fty
         SeqFinal{..} = sf (Deriv ts vs seq)

second f (a,b) = (a,f b)

fillTypes' :: [Name] -> [(Name, Type)] -> Seq -> Seq
fillTypes' = foldSeq sf where
  sf :: Deriv -> SeqFinal Type Seq
  sf (Deriv ts vs seq) = SeqFinal {..} where
    sty _ t = t
    sax _ _ t = Ax t
    scut v v' _ s vt t = Cut v v' vt x s t
    scross _ v ty v' ty' s = Cross ty v v' x s 
    spar _ v ty v' ty' s t = Par ty v v' x xs s t where Par _ _ _ _ xs _ _ = seq
    splus _ v vt v' vt' s t = Plus v v' x s t
    swith b _ v _ t = With v b x t
    sbot _ = SBot
    szero _ _ = SZero x
    sone _ t = SOne x t
    stapp _ tp v tyB s = TApp tp v x tyB s
    stunpack _ _ v s = TUnpack v x s
    soffer _ v _ s = Offer v x s
    sdemand _ v ty s = Demand v ty x s
    signore _ _ s = Ignore x s
    salias _ w' _ s = Alias x w' s
    swhat a _  = What a xs where What _ xs = seq
    x = varOf seq
    
-- | Fill in the forced types in the derivation.
fillTypes (Deriv ts vs s) = Deriv ts vs (fillTypes' ts vs s)  

