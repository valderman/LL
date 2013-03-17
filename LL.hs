{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module LL where

import Data.Monoid
import Data.List (mapAccumL)
import Control.Lens

type Name = String
-- | Types
data Type = Type :⊕: Type
             | Type :⊗: Type
             | Type :|: Type
             | Type :&: Type
             | Zero | One | Top | Bot
             | TVar Bool Int -- False if the variable is negated.
             | Forall Name (Type)
             | Exists Name (Type)
             | Bang (Type)
             | Quest (Type)
             | Meta Bool String [Type] --  A meta-variable, with types with types occuring in it.

a ⊸ b = neg a :|: b
dum = meta "dummy type" 

positiveType t = case t of
  One -> True
  Zero -> True
  _ :⊗: _ -> True
  _ :⊕: _ -> True            
  Bang _ -> True
  Exists _ _ -> True
  TVar b _ -> b
  Meta b _ _ -> b
  _ -> False

mkPositive t = if positiveType t then t else neg t

subst0 x = x:map var [0..]

type Permutation = [Int]
-- | Sequents                              
data (Seq) = Exchange Permutation (Seq) -- Permute variables
         | Ax (Type) -- Exactly 2 vars
         | Cut Name Name (Type) Int (Seq) (Seq) -- new vars in position 0
           
         | Cross (Type) Name Name Int (Seq) 
         | Par (Type) Name Name Int (Seq) (Seq) -- splits at given pos.
         | Plus Name Name Int (Seq) (Seq) -- Rename to Case
         | With Name Bool Int (Seq) -- Rename to Choose
           
         | SOne Int (Seq) -- Rename to ...
         | SZero Int         -- Rename to Crash/Loop
         | SBot              -- Rename to Terminate
         | What Name
           
         | TApp Type Name Int (Type) (Seq)
         | TUnpack Name Int (Seq)

         | Offer Name Int (Seq)
         | Demand Name (Type) Int (Seq)
         | Ignore Int (Seq)
         | Alias Int Name (Seq)

varOf (Cut _ _ _ x _ _) = x
varOf (Cross _ _ _ x _) = x
varOf (Par   _ _ _ x _ _) = x
varOf (Plus _ _ x _ _) = x
varOf (With _ _ x _) = x
varOf (SOne x _) = x
varOf (TApp _ _ x _ _) = x
varOf (TUnpack _ x _) = x
varOf (Offer _ x _) = x
varOf (Demand _ _ x _) = x
varOf (Ignore x _) = x
varOf (Alias x _ _) = x


-- Abstract Machine

-- | A Cell of the Heap
data Cell ref where
  Freed :: Cell ref
  Tag   :: Bool -> Cell ref
  New   :: Cell ref
  Delay :: Int -> Closure ref -> Cell ref
  Q     :: Type -> ref -> Cell ref -- 1st arg should be a monotype
  NewMeta :: Type -> Cell ref

type CellRef = Int

type TypeEnv = [Type]

type Env ref = [(Name,ref)]

type Closure ref = (Seq,Env ref,TypeEnv)

class IsRef ref where
  shift :: Type -> ref -> ref
  next  :: ref -> ref
  
class IsRef (Ref heap) => IsHeap heap where
  type Ref heap
  (!) :: heap -> Ref heap -> Cell (Ref heap) 
  replace :: Ref heap -> Cell (Ref heap) -> heap -> heap
  alloc :: Type -> heap -> (heap,Ref heap)
  allocAtom :: Ref heap -> Cell (Ref heap) -> heap -> heap
  emptyHeap :: heap

type Heap = [Cell Int]

instance IsRef Int where
  shift t = (sizeOf t +)
  next = succ
instance IsHeap Heap where
  type Ref Heap = CellRef
--  type Size Heap = Int
  (!) = (!!)
  replace n v (h) = let (l,_:r) = splitAt n h
                in l ++ v : r
  alloc t (h) = (h ++ replicate (sizeOf t) New,length h) -- FIXME
  emptyHeap = []


-- FIXME: pretty sure we need this:
-- This means we also need to allocate in & rule (fst/snd)
allocAt r t = do
  case mkPositive t of
      Zero -> id
      One -> id
      a :⊗: b -> allocAt r a . allocAt (shift a r) b
      a :⊕: b -> allocAtom r New
      -- FIXME: need unallocated, but shared area
      -- Bang _ -> allocAtom r (Delay 0 Nothing)
      Exists _ _ -> allocAtom r New
      t@(Meta _ _ _) -> allocAtom r (NewMeta t)
      TVar _ _ -> error "can only allocate concrete types"


type System h = ([Closure (Ref h)],h)

(!!+) :: Env ref -> Int -> ref
e !!+ v = snd (e!!v)

runClosure :: IsHeap h => h -> Closure (Ref h) -> Maybe (h,[Closure (Ref h)])
runClosure h (Plus x y v a b,e,te)
  | Tag c <- h!(e!!+v) = Just (replace (e!!+v) Freed h,
                              [(if c then a else b,increment v (if c then x else y) e,te)])
runClosure h (Cross ty x y v a,e,te)
  = Just (h,[(a,el++[(x,z),(y,shift (te∙ty) z)] ++ er,te)])
  where (el,(_,z):er) = splitAt v e
runClosure h (Par ty x y v a b,e,te)
  = Just (h,[(a,el++[(x,z)]++er,te)
            ,(b,el++[(y,shift (te∙ty) z)]++er,te)])
    where (el,(_,z):er) = splitAt v e
runClosure h (With x t v a,e,te)
  = Just (replace (e!!+v) (Tag t) h,[(a,increment v x e,te)])
  -- FIXME: free the part which is unused (half of the time)
runClosure h (SOne v a,e,te)
  = Just(h,[(a,el++er,te)])
    where (el,_:er) = splitAt v e
runClosure h (SBot,e,te)
  = Just (h,[])

runClosure h (TApp tp x v ty a,e,te)
  = Just (replace (e!!+v) (Q ty q) h',[(a,el++(x,z):er,te)])
  where (el,(_,z):er) = splitAt v e
        (h',q) = alloc ((ty:te)∙tp) h
runClosure h (TUnpack x v a,e,te)
  | Q ty p <- h!w = Just (replace w Freed h,[(a,el++[(x,p)]++er,ty:te)])
  where (el,(_,w):er) = splitAt v e
runClosure h (Exchange π a,e,te)
  = Just (h,[(a,[e!!x | x <- π],te)])
runClosure h (Ax (Bang ty),[(_,w),(_,x)],te)
  | d@(Delay n cl) <- h!x = Just (replace w d (replace x Freed h),[])
runClosure h (Ax (TVar True v),e,te)
  = Just (h,[(copy'' (te!!v),e,te)])
runClosure h (Ax (Forall _ ty),[(_,w),(_,x)],te)
  | q@(Q ty p) <- h!x = Just (replace w q (replace x Freed h),[])
runClosure h (Ax (Meta _ _ _),_,_) = Nothing -- FIXME: should show something
runClosure h (Ax ty,[w,x],te)
  = Just (h,[(copy'' ty,[w,x],te)])
runClosure h (Cut x y ty v a b,e,te)
  = Just (h',[(a,(x,q):ea,te),(b,(y,q):eb,te)])
  where (ea,eb) = splitAt v e
        (h',q)  = alloc (te∙ty) h

runClosure h (Offer x v a,e,te)
  = Just (replace (e!!+v) (Delay 1 (a,el++(x,w):er,te)) h,[])
    where (el,(_,w):er) = splitAt v e
runClosure h (Demand x ty v a,e,te)
  | (Delay n cl) <- h!p
  = Just (modifyRefCount (subtract 1) p $ h'
         ,[(a,el++[(x,q)]++er,te)])
    where (el,(_,p):er) = splitAt v e
          (h',q) = alloc (te ∙ ty) h
runClosure h (Ignore v a,e,te)
  = Just (modifyRefCount (subtract 1) m h,[(a,el++er,te)])
  where (el,(_,m):er) = splitAt v e
runClosure h (Alias v x a,e,te)
  = Just (modifyRefCount (+1) (e!!+v) h,[(a,(x,e!!+v):e,te)])
runClosure _ _ = Nothing

modifyRefCount f r h = replace r (Delay (f c) cl) h
  where (Delay c cl) = h!r

increment :: IsRef ref => Int -> Name -> Env ref -> Env ref
increment n nm e = let (l,(_,x):r) = splitAt n e
                    in l ++ (nm,next x) : r

copy'' :: Type -> Seq
copy'' (t1 :⊕: t2) = Plus "" ""  0 (copy'' t1) (copy'' t2) -- FIXME: probably a With is necessary here
copy'' (t1 :⊗: t2) = Cross t1 "" "" 0 $
                     Exchange [0,2,1] $
                     Par t1 "" "" 1 (copy'' t1) (copy'' t2)
copy'' Zero = error "Impossible"
copy'' One = SOne 0 SBot
copy'' t@(TVar True _) = Ax t
copy'' t@(Bang _) = Ax t
copy'' t@(Forall _ _) = Ax t
copy'' t = Exchange [1,0] $ copy'' (neg t)

sizeOf :: Type -> Int
sizeOf (t1 :⊕: t2) = 1 + max (sizeOf t1) (sizeOf t2)
sizeOf (t1 :⊗: t2) = sizeOf t1 + sizeOf t2
sizeOf (TVar _ v) = error "can only evaluate the size of closed types"
sizeOf (Forall _ _) = 1
sizeOf (Bang _)     = 1
sizeOf One     = 0
sizeOf Zero     = 0
sizeOf (Meta _ _ _) = error "cannot get size of meta-type"
sizeOf x = sizeOf (neg x)

stepSystem :: IsHeap h => System h -> Maybe (System h)
stepSystem ([],h) = Nothing
stepSystem (cl:cls,h) | Just (h',cl') <- runClosure h cl = Just (cl'++cls,h')
stepSystem (cl:cls,h) = do (cls',h') <- stepSystem (cls,h)
                           return (cl:cls',h')

runSystem :: IsHeap h => System h -> System h
runSystem s | Just s' <- stepSystem s = runSystem s'
runSystem s = s

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
             

-- | Type of substitutions               
type Subst = [Type]

meta x = Meta True x []
var = TVar True

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
  
applyS :: Subst -> (Seq) -> (Seq)
applyS f t = case t of
  (Exchange π a) -> Exchange π (s a)
  Cut w w' ty x a b -> Cut w w' (f ∙ ty) x (s a) (s b)
  Cross ty w w' x a -> Cross ty w w' x (s a)         
  Par ty w w' x a b -> Par ty w w' x (s a) (s b)
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


eval :: Deriv -> Deriv
eval (Deriv ts vs (Cut w w' ty γ a b)) = Deriv ts vs $ cut (length vs) w w' ty γ a b

remove0 π = [x-1 | x <- π, x > 0]

-- Hereditary cut
cut :: Int -> -- ^ size of the context
       Name -> 
       Name -> 
       Type -> 
       Int -> -- ^ where to cut it
       (Seq) -> (Seq) -> (Seq)
-- FIXME: in the absence of "What" cut can be eliminated so these recursive calls terminate. Otherwise, we have a problem. 
-- At the moment it seems we never use hereditary cut, so this should probably go away.
-- cut n w ty γ (Cut w' ty' δ a b) c = cut n w ty γ (cut γ w' ty' δ a b) c
-- cut n w ty γ a (Cut w' ty' δ b c) = cut n w ty γ a (cut (n-γ+1) w' ty' δ b c)
cut 2 _ _ ty 1 (Ax _) a = a
cut n _ _ (ta :⊗: tb) 
           γδ (Exchange π (Par _ _ _ γ a b)) (Cross _ w w' 0 c) = exchange (remove0 π++[length π-1..n-1]) $ cut n w w' ta γ 
                                                          a  
                                                          (exchange ([1..δ] ++ [0] ++ [δ+1..n-1]) $ cut (n-γ+1) w w' tb δ b c )
   where δ = γδ - γ
cut n _ _ (ta :⊕: tb) 
           γ (With z c 0 a) (Plus w w' 0 s t) = cut n z (if c then w else w') (if c then ta else tb) γ a (if c then s else t)
cut n _ _ (Exists v ty) 
           γ (TApp _ z 0 t a) (TUnpack w 0 b) = cut n z w (subst0 t ∙ ty) γ a (subst0 t ∙ b)
cut n _ _ (Bang ty) 
           γ (Offer z 0 a) (Demand w _ 0 b) = cut n z w ty γ a b
cut n _ _ ty γ (Offer _ 0 a) (Ignore 0 b) = ignore γ b
cut n _ _ ty γ (Offer w 0 b) (Alias 0 w' a) = alias (reverse [0..γ-1]) (cut (n+γ) w w' ty γ (Offer w 0 b) ((exchange ([1..γ] ++ [0] ++ [γ+1..n] ) $ cut (n+1) w w' ty γ (Offer w 0 b) a)))
  -- cut n w ty γ (Offer 0 b) (cut (n+1) w' ty γ (Offer 0 b) a)
cut n _ _ ty γ SBot (SOne 0 a) = a
cut n w w' ty γ a b | isPos b = exchange ([γ..n-1] ++ [0..γ]) (cut n w w' (neg ty) (n-γ) b a)
cut n w w' ty γ a b = Cut w w' ty γ a b

ignore 0 a = a
ignore n a = Ignore 0 (ignore (n-1) a)

alias [] a = a
alias (x:xs) a = Alias x mempty $ alias xs a

isPos (Ax _) = True
isPos (Exchange _ (Par _ _ _ _ _ _)) = True
isPos (With _ _  _ _) = True
isPos (Offer _ _ _) = True
isPos (TApp _ _ _ _ _) = True
isPos SBot = True
isPos _ = False

inverse :: Permutation -> Permutation
inverse π = [π!!x | x <- [0..length π-1]]

exchange = subst
-- | Application of variable substitution
subst π t = case t of
  (Ax ty) -> (Ax ty)
  (Cross ty w w' x c) -> Cross ty w w' (f x) (s' x c)
  Exchange ρ a -> subst (map f ρ) a
  (With w c x a) -> With w c (f x) (s a) 
  (Plus w w' x a b) -> Plus w w' (f x) (s a) (s b)
  (TApp tp w x t a) -> TApp tp w (f x) t (s a)
  (TUnpack w x a) -> TUnpack w (f x) (s a)
  (Offer w x a) -> Offer w (f x) (s a)
  (Demand w ty x a) -> Demand w ty (f x) (s a)
  (Alias x w a) -> Alias (f x) w (s' x a)
  (Ignore x a) -> Ignore (f x) (del x a)
  (SOne x a) -> SOne (f x) (s a)
  (SZero x) -> SZero (f x)
  SBot -> SBot
  a -> Exchange π a
 where f = (π!!)
       s = subst π
       s' x = subst (l++x:r)
              where (l,r) = splitAt x $ map (\y -> if y >= x then y+1 else x) π
       del x = subst (l++r)
              where (l,_:r) = splitAt x $ map (\y -> if y > x then y-1 else x) π


data Deriv = Deriv {derivTypeVars :: [Name], derivContext :: [(Name,Type)], derivSequent :: Seq}

-- | Convert a derivation to a System. Assumes the type env. is
-- ignorable (empty or contains only metavars)
derivToSystem :: IsHeap h => Deriv -> System h
derivToSystem (Deriv _ ctx a) = ([closure],heap)
  where closure = (a,zip (map fst ctx) refs,[])
        (heap,refs) = mapAccumL (flip alloc) emptyHeap (map snd ctx)
 
data SeqFinal t a = SeqFinal
     { sty :: [Name] -> Type -> t
     , sax :: (Name -> Name -> Type -> a)
     , scut :: (Name -> Name -> t -> a -> t -> a -> a)
     , scross :: (Name -> Name -> t -> Name -> t -> a -> a)
     , spar   :: (Name -> Name -> t -> Name -> t -> a -> a -> a)
     , swith :: (Bool -> Name -> Name -> t -> a -> a)
     , splus :: (Name -> Name -> t -> Name -> t -> a -> a -> a)
     , sxchg :: (Permutation -> a -> a)
     , stapp :: (Name -> t -> Name -> t -> a -> a)
     , stunpack :: (Name -> Name -> Name -> a -> a)
     , sbot :: (Name -> a)
     , szero :: (Name -> [(Name, t)] -> a)
     , sone :: (Name -> a -> a)
     , soffer :: (Name -> Name -> t -> a -> a)
     , sdemand :: (Name ->  Name -> t -> a -> a)
     , signore :: (Name -> t -> a -> a)
     , salias :: (Name -> Name -> t -> a -> a)
     , swhat :: (Name -> a)
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
      (Par _ v v' x s t) -> spar w v (fty vt) v' (fty vt') (recurse ts (v0++[(v,vt)]) s) (recurse ts ((v',vt'):v1) t)
        where (v0,(w,~(vt :|: vt')):v1) = splitAt x vs
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
      (Exchange p t) -> sxchg p $ recurse ts [vs !! i | i <- p] t        
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
      What x -> swhat x
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
    spar _ v ty v' ty' s t = Par ty v v' x s t
    splus _ v vt v' vt' s t = Plus v v' x s t
    swith b _ v _ t = With v b x t
    sbot _ = SBot
    szero _ _ = SZero x
    sone _ t = SOne x t
    sxchg p t = Exchange p t
    stapp _ tp v tyB s = TApp tp v x tyB s
    stunpack _ _ v s = TUnpack v x s
    soffer _ v _ s = Offer v x s
    sdemand _ v ty s = Demand v ty x s
    signore _ _ s = Ignore x s
    salias _ w' _ s = Alias x w' s
    swhat a = What a
    x = varOf seq
    
fillTypes (Deriv ts vs s) = (Deriv ts vs (fillTypes' ts vs s))    

