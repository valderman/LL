{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module LL where

import Data.Monoid

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

dum :: Type
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
type Boson = Bool

-- | Sequents. A 'Forced Type' is a type which is fully determined by
-- the context. They can be filled in by the function 'fillTypes' below.
data Seq = Exchange Permutation Seq -- Permute variables
         | Ax (Forced Type) -- Exactly 2 vars
         | Cut Name Name (Type) Int (Seq) (Seq) -- new vars in position 0

         | Cross Boson (Forced Type) Name Name Int (Seq)
         | Par Boson (Forced Type) Name Name Int (Seq) (Seq) -- splits at given pos.
         | Plus Name Name Int (Seq) (Seq) -- Rename to Case
         | With Boson (Forced Type) Name Bool Int (Seq) -- Rename to Choose

         | SOne Boson Int Seq      -- Rename to ...
         | SZero Int         -- Rename to Crash/Loop
         | SBot Boson        -- Rename to Terminate
         | What Name [Int]   -- 'meta' program (just for the paper, not found in actual code)

         | TApp Boson (Forced Type) Name Int Type (Seq)
         | TUnpack Name Int (Seq)

         | Offer Boson Name Int (Seq) -- FIXME: no boson
         | Demand Name (Forced Type) Int (Seq)
         | Ignore Int Seq
         | Alias Boson Int Name (Seq)
           
         -- | Channel (Forced Type) -- Exactly 2 vars
         | ChanPlus Bool -- Bit boson
         | ChanCross Type Type -- Channel split on the cross side
         | ChanPar Type Type -- Channel split on the par side
         | ChanTyp Type -- Monotype boson
         | MemEmpty Type Int -- Memory with n readers, but not written yet
         | MemFull Type Int  -- Memory with n readers, already written to
         | Mem Type Int Int Seq Seq


{-
memSplit :: [Int] -> [a] -> [[a]]
memSplit [] x = [x]
memSplit (i:is) x = l:memSplit is r
  where (l,r) = splitAt i x
-}
-- | A full derivation
data Deriv = Deriv {derivTypeVars :: [Name], derivContext :: [(Name,Type)], derivSequent :: Seq}

-- | The variable that the sequent operates on. Remark that there is
-- always exactly one.
varOf (Cut _ _ _ x _ _) = x
varOf (Cross _ _ _ _ x _) = x
varOf (Par  _ _ _ _ x _ _) = x
varOf (Plus _ _ x _ _) = x
varOf (With _ _ _ _ x _) = x
varOf (SOne _ x _) = x
varOf (SZero x) = x
varOf (TApp _ _ _ x _ _) = x
varOf (TUnpack _ x _) = x
varOf (Offer _ _ x _) = x
varOf (Demand _ _ x _) = x
varOf (Ignore x _) = x
varOf (Alias _ x _ _) = x
varOf (Mem _ x _ _ _) = x

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
  Exists w t -> Exists w (s' t)
  Bang t -> Bang (s t)
  Quest t -> Quest (s t)
  Meta b x ns -> Meta b x (f ∙ ns)
 where s = apply f
       s' = apply (var 0 : wk ∙ f)

applyS :: Subst -> Seq -> Seq
applyS f t = case t of
  Ax ty -> Ax (f ∙ ty)
--  Channel ty -> Channel (f ∙ ty)
  (Exchange π a) -> Exchange π (s a)
  Cut w w' ty x a b -> Cut w w' (f ∙ ty) x (s a) (s b)
  Cross β ty w w' x a -> Cross β ty w w' x (s a)
  Par β ty w w' x a b -> Par β ty w w' x (s a) (s b)
  Plus w w' x a b -> Plus w w' x (s a) (s b)
  With β ty w c x a -> With β ty w c x (s a)
  SOne β x a -> SOne β x (s a)
  TApp β tp w x ty a -> TApp β ((var 0:wk∙f)∙tp) w x (f ∙ ty) (s a)
  TUnpack w x a -> TUnpack w x (s' a)
  Offer β w x a -> Offer β w x (s a)
  Mem ty x n u ts -> Mem (f ∙ ty) x n (s u) (s ts)
  Demand w ty x a -> Demand w ty x (s a)
  Ignore x a -> Ignore x (s a)
  Alias β x w a -> Alias β x w (s a)
  a -> a -- FIXME: other channel types
 where s = applyS f
       s' = applyS (var 0 : wk ∙ f)

--------------
-- Extraction
rightChild,leftChild :: Deriv -> Deriv
leftChild  (Deriv ts vs (Cut w _ ty x a _)) = Deriv ts ((w,neg ty):take x vs) a
rightChild (Deriv ts vs (Cut _ w ty x _ a)) = Deriv ts ((w,    ty):drop x vs) a

----------------------------
-- Evaluation via channels

eval' (Deriv ts vs s) = Deriv ts vs $ cheval (length vs) s

cheval :: Int -> Seq -> Seq
-- cheval _ (Cut w w' ty 1 (Cut v v' ty' 1 a b) c) = Cut v v' (neg ty') 1 b (Cut w w' ty 1 (a) c) -- FIXME: exchange [1,0] b

-- Sending bosons
cheval _ (With False ty w c x s) = With True ty w c x s  -- Cut "w" "_w" ty 1 (ChanPlus c) s 
cheval _ (TApp False tp w x ty a) = TApp True tp w x ty a -- Cut "w" "_w" (subst0 ty ∙ tp) 1 (ChanTyp ty) a
cheval _ (Cross False ty w w' x c) = Cross True ty w w' x c
cheval _ (Par False ty w w' γ a b) = Par True ty w w' γ a b
cheval _ (SOne False x s) = SOne True x s
cheval _ (SBot False) = SBot True
cheval _ (Alias False x w s) = Alias True x w s
-- cheval _ (Offer False w x s) = Offer True w x s
cheval n (Cut _ _ (Bang ty) γ (Offer False _ 0 s) t) = Mem ty γ 1 s (cheval  (n-γ+1) t)

-- Interactions
cheval _ (Mem tA x n s (Alias True 0 _ t)) = Mem tA x (1+n) s t
cheval m (Mem tA x n s (Demand _ _ 1 t)) = 
  alias True [0..x-1] $ 
  Mem tA x (n-1) s $
  exchange [1,0,2] $ -- FIXME: do not hardcode!
  Cut "_w" "w" tA x s $ 
  exchange [1,0,2] $
  t

{-
cheval _ (Cut _ _ _ 1 (ChanPlus c) (Plus _ _ 0 s t)) = if c then s else t
cheval _ (Cut _ _ _ 1 (ChanTyp ty) (TUnpack _ 0 s)) = subst0 ty ∙ s
cheval n (Cut _ _ (ta :⊗: tb) γδ (Exchange π (Par True _ _ _ γ a b)) (Cross True _ w w' 0 c)) 
  = exchange (remove 0 π++[length π-1..n-1]) $ Cut w w' ta γ
    a
    (exchange ([1..δ] ++ [0] ++ [δ+1..n-1]) $ cut' (n-γ+1) w w' tb δ b (exchange ([1,0]++[2..n-γ]) c))
   where δ = γδ - γ
cheval _ (Cut _ _ (ta :⊕: tb) γ (With True _ z c 0 a) (Plus w w' 0 s t)) = Cut z (if c then w else w') (if c then ta else tb) γ a (if c then s else t)    

cheval n (Cut _ _ (Exists v ty)
                   γ (TApp β _ z 0 t a) (TUnpack w 0 b)) = cut' n z w (subst0 t ∙ ty) γ a (subst0 t ∙ b)
cheval n (Cut _ _ (Bang ty) γ (Offer β z 0 a) (Demand w _ 0 b)) = Cut z w ty γ a b
cheval n (Cut _ _ ty γ (Offer β _ 0 a) (Ignore 0 b)) = ignore γ b
cheval n (Cut _ _ ty γ (Offer β w 0 b) (Alias β' 0 w' a)) = alias β' (reverse [0..γ-1]) (cut' (n+γ) w w' ty γ (Offer β w 0 b) ((exchange ([1..γ] ++ [0] ++ [γ+1..n] ) $ cut' (n+1) w w' ty γ (Offer β w 0 b) a)))
cheval n (Cut _ _ ty γ (SBot β') (SOne β 0 a)) = a
-}
-- Recursing         
cheval n (Cut w w' ty γ a b) = Cut w w' ty γ (cheval (1+γ) a) (cheval (1+n-γ) b)
cheval n (Exchange π s) = Exchange π (cheval n s)
cheval _ s = s

  {-
cut n w w' ty γ (With v c 0 s) (Channel (ta :&: tb)) = Cut w w' (neg $ if c then ta else tb) γ s (Exchange [1,0] $ ChanPlus c ta tb)
cut n w w' _  γ (ChanPlus b ta tb) (Plus _ _ 0 s t) = Cut w w' (neg ty) γ (Channel (neg ty)) (if b then s else t)
  where ty = if b then ta else tb
cut n w w' ty γ (Exchange [1,0,2] (Par ta v v' 1 s t)) (Channel (_ta :|: tb)) = Cut w w' (neg ta) 1 s (Exchange [1,0,2] $ Cut w w' (neg tb) 1 t (Exchange [2,1,0] $ ChanPar ta tb))
cut n w w' ty γ (ChanPar ta tb) (Cross _ v v' 0 s) =  Exchange [1,0,2] $ Cut w w' (neg tb) 1 (Channel $ neg tb) $ Exchange [1,0,2] $ Cut w w' (neg ta) 1 (Channel $ neg ta) s
-}

---------------------------------------
-- Cut-elimination machinery

-- | One step to eliminate the principal cut.
eval :: Deriv -> Deriv
eval (Deriv ts vs (Cut w w' ty γ a b)) = Deriv ts vs $ cut (length vs) w w' ty γ a b
eval x = x

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

cut n w w' ty γ (SOne β x s) t | x > 0 = SOne β (x-1) (Cut w w' ty (γ-1) s t)
cut n w w' ty γ (Par β ty' v v' x s t) u | x > 0 = Exchange ([γ..n-1] ++ [0..γ-1]) $ Par  β ty' v v' ((n-γ)+x-1) (Cut w w' (neg ty) (n-γ) s t) u
cut n w w' ty γ (Exchange π (Par β ty' v v' x s t)) u | π!!x > 0 = exchange ((remove 0 π) ++ [γ..n-1])  $
                                                                Par β ty' v v' x s (Cut w w' ty (γ-x) (exchange ([1..γ-x] ++ [0]) t) u)
cut n w w' ty γ (Cross β ty' v v' x s) t | x > 0 = Cross β ty' v v' (x-1) (Cut w w' ty (γ+1) s t)
cut n w w' ty γ (Plus v v' x s t) u | x > 0 = Plus v v' (x-1) (Cut w w' ty γ s u) (Cut w w' ty γ t u)
cut n w w' ty γ (With β ta v c x s) t | x > 0 = With β ta v c (x-1) (Cut w w' ty γ s t)
cut n w w' ty γ (SZero x) t | x > 0 = SZero (x-1)
cut n w w' ty γ (TApp β tp v x ty' s) t | x > 0 = TApp β tp v (x-1) ty' (Cut w w' ty γ s t)
cut n w w' ty γ (TUnpack v x s) t | x > 0 = TUnpack v (x-1) (Cut w w' ty γ s t)
cut n w w' ty γ (Offer β v x s) t | x > 0 = Offer β v (x-1) (Cut w w' ty γ s t)
cut n w w' ty γ (Demand v ty' x s) t | x > 0 = Demand v ty' (x-1) (Cut w w' ty γ s t)
cut n w w' ty γ (Ignore x s) t | x > 0 = Ignore (x-1) (Cut w w' ty (γ-1) s t)
cut n w w' ty γ (Alias β x v s) t | x > 0 = Alias β (x-1) v (Cut w w' ty (γ+1) s t)


cut 2 _ _ ty 1 (Ax _) a = a
-- FIXME: side condition π!!x == 0
cut n _ _ (ta :⊗: tb)
           γδ (Exchange π (Par _ _ _ _ γ a b)) (Cross _ _ w w' 0 c) = exchange (remove 0 π++[length π-1..n-1]) $ cut' n w w' ta γ
                                                          a
                                                          (exchange ([1..δ] ++ [0] ++ [δ+1..n-1]) $ cut' (n-γ+1) w w' tb δ b (exchange ([1,0]++[2..n-γ]) c))
   where δ = γδ - γ
cut n _ _ (ta :⊕: tb)
           γ (With β _ z c 0 a) (Plus w w' 0 s t) = cut' n z (if c then w else w') (if c then ta else tb) γ a (if c then s else t)
cut n _ _ (Exists v ty)
           γ (TApp β _ z 0 t a) (TUnpack w 0 b) = cut' n z w (subst0 t ∙ ty) γ a (subst0 t ∙ b)
cut n _ _ (Bang ty)
           γ (Offer β z 0 a) (Demand w _ 0 b) = cut' n z w ty γ a b
cut n _ _ ty γ (Offer β _ 0 a) (Ignore 0 b) = ignore γ b
cut n _ _ ty γ (Offer β w 0 b) (Alias β' 0 w' a) = alias β' (reverse [0..γ-1]) (cut' (n+γ) w w' ty γ (Offer β w 0 b) ((exchange ([1..γ] ++ [0] ++ [γ+1..n] ) $ cut' (n+1) w w' ty γ (Offer β w 0 b) a)))
cut n _ _ ty γ (SBot β') (SOne β 0 a) = a

  -- Cut w w' ta 1 (What "arst" []) (What "qwft" [])
cut n w w' ty γ a b | isPos b = exchange ([γ..n-1] ++ [0..γ-1]) (cut n w w' (neg ty) (n-γ) b a)
cut n w w' ty γ a b = Cut w w' ty γ a b

ignore 0 a = a
ignore n a = Ignore 0 (ignore (n-1) a)

alias β [] a = a
alias β (x:xs) a = Alias β x mempty $ alias β xs a

isPos (Ax _) = True
isPos (Exchange _ (Par _ _ _ _ _ _ _)) = True
isPos (With _ _ _ _  _ _) = True
isPos (Offer _ _ _ _) = True
isPos (TApp _ _ _ _ _ _) = True
isPos (SBot _) = True
isPos _ = False

indexOf :: Eq a => a -> [a] -> Int
indexOf _x [] = error "indexOf: not found"
indexOf x (y:ys) | x == y = 0
                 | otherwise = 1 + indexOf x ys

inverse :: Permutation -> Permutation
inverse π = [indexOf x π | x <- [0..length π-1]]

applyPerm π e = [e !! i | i <- π]


exchange = subst

isIdentity π = π == [0..length π-1]
-- | Application of variables-only substitution
subst π t | isIdentity π = t
subst π t = case t of
  (Ax ty) -> (Ax (neg ty)) -- because the identity case was handled above.
  b@(ChanPlus _) -> b
  (Cross β ty w w' x c) -> Cross β ty w w' (f x) (s' x c)
  Exchange ρ a -> subst (map f ρ) a
  (With β ty w c x a) -> With β ty w c (f x) (s a)
  (Plus w w' x a b) -> Plus w w' (f x) (s a) (s b)
  (TApp β tp w x t a) -> TApp β tp w (f x) t (s a)
  (TUnpack w x a) -> TUnpack w (f x) (s a)
  (Offer β w x a) -> Offer β w (f x) (s a)
  (Demand w ty x a) -> Demand w ty (f x) (s a)
  (Alias β x w a) -> Alias β (f x) w (s' 0 a)
  (Ignore x a) -> Ignore (f x) (del x a)
  (SOne β x a) -> SOne β (f x) (del x a)
  (SZero x) -> SZero (f x)
  (SBot β) -> SBot β
  -- What nm xs -> What nm (map f xs)
  a -> Exchange π a
 where f = (π!!)
       s = subst π
       s' x = subst (l++x:r)
              where (l,r) = splitAt x $ map (\y -> if y >= x then y+1 else y) π
       del x = subst (remove x π)

remove y π = [if x > y then x-1 else x | x <- π, x /= y]



----------------------------------
-- Paramorphism for sequents.

data SeqFinal t a = SeqFinal
     { sty :: [Name] -> Type -> t
     , sax :: (Name -> Name -> Type -> a)
     , scut :: (Name -> Name -> t -> a -> t -> a -> a)
     , scross :: (Boson -> Name -> Name -> t -> Name -> t -> a -> a)
     , spar   :: (Boson -> Name -> Name -> t -> Name -> t -> a -> a -> a)
     , swith :: (Boson -> t -> Bool -> Name -> Name -> t -> a -> a)
     , splus :: (Name -> Name -> t -> Name -> t -> a -> a -> a)
     , sxchg :: (Permutation -> a -> a)
     , stapp :: (Boson -> Name -> t -> Name -> t -> a -> a)
     , stunpack :: (Name -> Name -> Name -> a -> a)
     , sbot :: (Boson -> Name -> a)
     , szero :: (Name -> [(Name, t)] -> a)
     , sone :: (Boson -> Name -> a -> a)
     , soffer :: (Boson -> Name -> Name -> t -> a -> a)
     , sdemand :: (Name ->  Name -> t -> a -> a)
     , signore :: (Name -> t -> a -> a)
     , salias :: (Boson -> Name -> Name -> t -> a -> a)
     , swhat :: (Name -> [Name] -> [(Name,Type)] -> a)
     , smem :: t -> a -> a -> a
     , schplus :: Bool -> Type -> Type -> a
     , schcross :: Type -> Type -> a
     , schpar :: Type -> Type -> a
     , schtyp :: Type -> Type -> a
     , schempty :: Type -> Int -> a
     , schfull :: Type -> Int -> a
     }
foldSeq :: (Deriv -> SeqFinal t a)
     -> [Name] -- ^ ty vars
     -> [(Name, Type)] -- ^ vars
     -> Seq
     -> a

foldSeq sf ts0 vs0 s0 =
 recurse ts0 vs0 s0 where
 recurse ts vs seq = case seq of
--      Channel _ ->  schannel vt where [(v0,_),(v1,vt)] = vs
      ChanPlus b -> schplus b ta tb where [(v1,ta :&: tb),_] = vs
      ChanCross _ _ -> schcross ta tb where [(v1,ta :⊗: tb),_,_] = vs
      ChanPar _ _ -> schpar ta tb where [(v1,ta :|: tb),_,_] = vs
      ChanTyp ty -> schtyp ty tgen where [(v0,tgen),_] = vs
      MemEmpty _ _ -> schempty ty (length rest) where ((_,Bang ty):rest) = vs
      MemFull _ _ -> schfull ty (length rest) where ((_,ty):rest) = vs
      Ax _ -> sax v0 v1 vt where [(v0,_),(v1,vt)] = vs
      (Cut v v' vt x s t) -> scut v v' (fty (neg vt)) (recurse ts ((v,neg vt):v0) s)
                                       (fty      vt ) (recurse ts ((v',vt):v1) t)
        where (v0,v1) = splitAt x vs
      (Cross β _ v v' x t) -> scross β w v (fty vt) v' (fty vt') $ recurse ts (v0++(v,vt):(v',vt'):v1) t
        where (v0,(w,~(vt :⊗: vt')):v1) = splitAt x vs
      (Par β _ v v' x s t) -> spar β w v (fty vt) v' (fty vt') (recurse ts (v0++[(v,vt)]) s) (recurse ts ((v',vt'):v1) t)
        where (v0,(w,~(vt :|: vt')):v1) = splitAt x vs
      (Plus v v' x s t) -> splus w v (fty vt) v' (fty vt') (recurse ts (v0++(v,vt ):v1) s) (recurse ts (v0++(v',vt'):v1) t)
        where (v0,(w,~(vt :⊕: vt')):v1) = splitAt x vs
      (With β _ v b x t) -> swith β (fty $ if b then vt else vt') b w v (fty wt) $ recurse ts (v0++(v,wt):v1) t
         where wt = if b then vt else vt'
               (v0,(w,~(vt :&: vt')):v1) = splitAt x vs
      (SBot β) -> sbot β v
         where [(v,~Bot)] = vs
      (SZero x) -> szero w (fctx (v0 ++ v1))
         where (v0,(w,~Zero):v1) = splitAt x vs
      (SOne β x t) -> sone β w $ recurse ts (v0++v1) t
        where (v0,(w,~One):v1) = splitAt x vs
      (Exchange p t) -> sxchg p $ recurse ts (applyPerm p vs) t
      (TApp β _ v x tyB s) -> stapp β w (sty (v:ts) tyA) v (fty tyB) $ recurse ts (v0++(v,ty):v1) s
        where (v0,(w,~(Forall _ tyA)):v1) = splitAt x vs
              ty = subst0 tyB ∙ tyA
      (TUnpack v x s) -> stunpack tw w v $ recurse (tw:ts) (upd v0++(v,tyA):upd v1) s
        where (v0,(w,~(Exists tw tyA)):v1) = splitAt x vs
              upd = map (second (wk ∙))
      (Offer β v x s) -> soffer β w v  (fty tyA) $ recurse ts (v0++(v,tyA):v1) s
        where (v0,(w,~(Quest tyA)):v1) = splitAt x vs
      (Demand v _ x s) -> sdemand w v (fty tyA) $ recurse ts (v0++(v,tyA):v1) s
        where (v0,(w,~(Bang tyA)):v1) = splitAt x vs
      (Ignore x s) -> signore w (fty tyA) $ recurse ts (v0++v1) s
        where (v0,(w,~(Bang tyA)):v1) = splitAt x vs
      (Alias β x w' s) -> salias β w w' (fty tyA) $ recurse ts ((w,Bang tyA):v0++(w',Bang tyA):v1) s
        where (v0,(w,~(Bang tyA)):v1) = splitAt x vs
      What x ws -> swhat x [fst (vs !! w) | w <- ws] vs
      Mem ty x n t u -> smem (fty ty) (recurse ts (("_x",neg ty):t') t)
                                      (recurse ts (replicate n ("x",Bang ty) ++ u') u)
        where (t',u') = splitAt x vs
   where fty = sty ts
         fctx = map (second fty)
         SeqFinal{..} = sf (Deriv ts vs seq)

second f (a,b) = (a,f b)

fillTypes' :: [Name] -> [(Name, Type)] -> Seq -> Seq
fillTypes' = foldSeq sf where
  sf :: Deriv -> SeqFinal Type Seq
  sf (Deriv ts vs seq) = SeqFinal {..} where
    sty _ t = t
    sax _ _ t = Ax t
    scut v v' _ s vt t = Cut v v' vt x s t
    scross β _ v ty v' _ty' s = Cross β ty v v' x s
    spar β _ v ty v' _ty' s t = Par β ty v v' x s t
    splus _ v _vt v' _vt' s t = Plus v v' x s t
    swith β ty b _ v _ t = With β ty v b x t
    sbot β _ = SBot β
    szero _ _ = SZero x
    sone β _ t = SOne β x t
    sxchg p t = Exchange p t
    stapp β _ tp v tyB s = TApp β tp v x tyB s
    stunpack _ _ v s = TUnpack v x s
    soffer β _ v _ s = Offer β v x s
    sdemand _ v ty s = Demand v ty x s
    signore _ _ s = Ignore x s
    salias β _ w' _ s = Alias β x w' s
--    schannel t = Channel t
    schplus b _ta _tb = ChanPlus b
    schcross ta tb = ChanCross ta tb
    schpar ta tb = ChanPar ta tb
    schtyp tmono _ = ChanTyp tmono
    schempty ty n = MemEmpty ty n
    schfull ty n = MemFull ty n
    smem ty t u = Mem ty x n t u where Mem _ _ n _ _ = seq
    swhat a _ _ = What a xs where What _ xs = seq
    x = varOf seq

-- | Fill in the forced types in the derivation.
fillTypes (Deriv ts vs s) = Deriv ts vs (fillTypes' ts vs s)

