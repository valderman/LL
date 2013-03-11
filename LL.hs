{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module LL where

import Data.Monoid
import Data.List (mapAccumL)

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


subst0 x = x:map var [0..]

type Permutation = [Int]
-- | Sequents                              
data (Seq) = Exchange Permutation (Seq) -- Permute variables
         | Ax (Type) -- Exactly 2 vars
         | Cut Name (Type) Int (Seq) (Seq) -- new vars in position 0
           
         | Cross (Type) Name Name Int (Seq) 
         | Par (Type) Int (Seq) (Seq) -- splits at given pos.
         | Plus Int (Seq) (Seq) -- Rename to Case
         | With Bool Int (Seq) -- Rename to Choose
           
         | SOne Int (Seq) -- Rename to ...
         | SZero Int         -- Rename to Crash/Loop
         | SBot              -- Rename to Terminate
         | What Name
           
         | TApp Int (Type) (Seq)
         | TUnpack Int (Seq)

         | Offer Int (Seq)
         | Demand (Type) Int (Seq)
         | Ignore Int (Seq)
         | Alias Int Name (Seq)

-- Abstract Machine

-- | A Cell of the Heap
data Cell ref where
  Freed :: Cell ref
  Tag   :: Bool -> Cell ref
  New   :: Cell ref
  Delay :: Int -> Closure ref -> Cell ref
  Q     :: Type -> ref -> Cell ref -- 1st arg should be a monotype

type CellRef = Int

type TypeEnv = [Type]

type Env ref = [ref]

type Closure ref = (Seq,Env ref,TypeEnv)

class IsRef ref where
  shift :: Type -> ref -> ref
  next  :: ref -> ref
  
class IsRef (Ref heap) => IsHeap heap where
  type Ref heap
  (!) :: heap -> Ref heap -> Cell (Ref heap) 
  replace :: Ref heap -> Cell (Ref heap) -> heap -> heap
  alloc :: Type -> heap -> (heap,Ref heap)
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
  alloc t (h) = (h ++ replicate (sizeOf t) New,length h)
  emptyHeap = []

type System h = ([Closure (Ref h)],h)

runClosure :: IsHeap h => h -> Closure (Ref h) -> Maybe (h,[Closure (Ref h)])
runClosure h (Plus v a b,e,te)
  | Tag c <- h!(e!!v) = Just (replace (e!!v) Freed h,
                             [(if c then a else b,increment v e,te)])
runClosure h (Cross ty _ _ v a,e,te)
  = Just (h,[(a,el++[x,shift (te∙ty) x] ++ er,te)])
  where (el,x:er) = splitAt v e
runClosure h (Par ty v a b,e,te)
  = Just (h,[(a,el++[z]++er,te)
            ,(b,el++[shift (te∙ty) z]++er,te)])
    where (el,z:er) = splitAt v e
runClosure h (With t v a,e,te)
  = Just (replace (e!!v) (Tag t) h,[(a,increment v e,te)])

runClosure h (SOne v a,e,te)
  = Just(h,[(a,el++er,te)])
    where (el,_:er) = splitAt v e
runClosure h (SBot,e,te)
  = Just (h,[])

runClosure h (TApp v ty a,e,te)
  = Just (replace (e!!v) (Q ty q) h',[(a,e,te)])
  where (el,z:er) = splitAt v e
        (h',q) = alloc ((ty:te)∙(error "need the type of the polymorphic value")) h
runClosure h (TUnpack v a,e,te)
  | Q ty p <- h!w = Just (replace w Freed h,[(a,el++[p]++er,ty:te)])
  where (el,w:er) = splitAt v e
runClosure h (Exchange π a,e,te)
  = Just (h,[(a,[e!!x | x <- π],te)])
runClosure h (Ax (Bang ty),[w,x],te)
  | d@(Delay n cl) <- h!x = Just (replace w d (replace x Freed h),[])
runClosure h (Ax (TVar True v),e,te)
  = Just (h,[(copy'' (te!!v),e,te)])
runClosure h (Ax (Forall _ ty),[w,x],te)
  | q@(Q ty p) <- h!x = Just (replace w q (replace x Freed h),[])
runClosure h (Ax ty,[w,x],te)
  = Just (h,[(copy'' ty,[w,x],te)])
runClosure h (Cut _ ty v a b,e,te)
  = Just (h',[(a,q:ea,te),(b,q:eb,te)])
  where (ea,eb) = splitAt v e
        (h',q)  = alloc (te∙ty) h

runClosure h (Offer v a,e,te)
  = Just (replace (e!!v) (Delay 1 (a,e,te)) h,[])
runClosure h (Demand ty v a,e,te)
  | (Delay n cl) <- h!p
  = Just (modifyRefCount (subtract 1) p $ h'
         ,[(a,el++[q]++er,te)])
    where (el,p:er) = splitAt v e
          (h',q) = alloc (te ∙ ty) h
runClosure h (Ignore v a,e,te)
  = Just (modifyRefCount (subtract 1) m h,[(a,el++er,te)])
  where (el,m:er) = splitAt v e
runClosure h (Alias v _ a,e,te)
  = Just (modifyRefCount (+1) (e!!v) h,[(a,e!!v:e,te)])

modifyRefCount f r h = replace r (Delay (f c) cl) h
  where (Delay c cl) = h!r

increment :: IsRef ref => Int -> Env ref -> Env ref
increment n e = let (l,x:r) = splitAt n e
                in l ++ next x : r

copy'' :: Type -> Seq
copy'' (t1 :⊕: t2) = Plus 0 (copy'' t1) (copy'' t2)
copy'' (t1 :⊗: t2) = Cross t1 "" "" 0 $
                     Exchange [0,2,1] $
                     Par t1 1 (copy'' t1) (copy'' t2)
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
  Cut w ty x a b -> Cut w (f ∙ ty) x (s a) (s b)
  Cross ty w w' x a -> Cross ty w w' x (s a)         
  Par ty x a b -> Par ty x (s a) (s b)
  Plus x a b -> Plus x (s a) (s b)
  With c x a -> With c x (s a)
  SOne x a -> SOne x (s a) 
  TApp x ty a -> TApp x (f ∙ ty) (s a)
  TUnpack x a -> TUnpack x (s' a)
  Offer x a -> Offer x (s a)
  Demand ty x a -> Demand ty x (s a)
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
eval (Deriv ts vs (Cut w ty γ a b)) = Deriv ts vs $ cut (length vs) w ty γ a b

cut' _ = Cut

remove0 π = [x-1 | x <- π, x > 0]

-- Hereditary cut
cut :: Int -> -- ^ size of the context
       Name -> 
       Type -> 
       Int -> -- ^ where to cut it
       (Seq) -> (Seq) -> (Seq)
-- FIXME: in the absence of "What" cut can be eliminated so these recursive calls terminate. Otherwise, we have a problem.
-- cut n w ty γ (Cut w' ty' δ a b) c = cut n w ty γ (cut γ w' ty' δ a b) c
-- cut n w ty γ a (Cut w' ty' δ b c) = cut n w ty γ a (cut (n-γ+1) w' ty' δ b c)
cut 2 w ty 1 (Ax _) a = a
cut n _ (ta :⊗: tb) 
           γδ (Exchange π (Par _ γ a b)) (Cross _ w w' 0 c) = exchange (remove0 π++[length π-1..n-1]) $ cut n w ta γ 
                                                          a  
                                                          (exchange ([1..δ] ++ [0] ++ [δ+1..n-1]) $ cut (n-γ+1) w' tb δ b c )
   where δ = γδ - γ
cut n w (ta :⊕: tb) 
           γ (With c 0 a) (Plus 0 s t) = cut n w (if c then ta else tb) γ a (if c then s else t)
cut n w (Exists v ty) 
           γ (TApp 0 t a) (TUnpack 0 b) = cut n w (subst0 t ∙ ty) γ a (subst0 t ∙ b)
cut n w (Bang ty) 
           γ (Offer 0 a) (Demand _ 0 b) = cut n w ty γ a b
cut n w ty γ (Offer 0 a) (Ignore 0 b) = ignore γ b
cut n w ty γ (Offer 0 b) (Alias 0 w' a) = alias (reverse [0..γ-1]) (cut (n+γ) w ty γ (Offer 0 b) ((exchange ([1..γ] ++ [0] ++ [γ+1..n] ) $ cut (n+1) w' ty γ (Offer 0 b) a)))
  -- cut n w ty γ (Offer 0 b) (cut (n+1) w' ty γ (Offer 0 b) a)
cut n w ty γ SBot (SOne 0 a) = a
cut n w ty γ a b | isPos b = exchange ([γ..n-1] ++ [0..γ]) (cut n w (neg ty) (n-γ) b a)
cut n w ty γ a b = Cut w ty γ a b

ignore 0 a = a
ignore n a = Ignore 0 (ignore (n-1) a)

alias [] a = a
alias (x:xs) a = Alias x mempty $ alias xs a

isPos (Ax _) = True
isPos (Exchange _ (Par _ _ _ _)) = True
isPos (With _ _ _) = True
isPos (Offer _ _) = True
isPos (TApp _ _ _) = True
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
  (With c x a) -> With c (f x) (s a) 
  (Plus x a b) -> Plus (f x) (s a) (s b)
  (TApp x t a) -> TApp (f x) t (s a)
  (TUnpack x a) -> TUnpack (f x) (s a)
  (Offer x a) -> Offer (f x) (s a)
  (Demand ty x a) -> Demand ty (f x) (s a)
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
  where closure = (a,refs,[])
        (heap,refs) = mapAccumL (flip alloc) emptyHeap (map snd ctx)
 
------------------
-- Symbolic heap



