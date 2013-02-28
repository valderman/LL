{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module LL where

import Data.Monoid
import Text.PrettyPrint.HughesPJ hiding ((<>))
import Data.String

type Name = Doc

-- | Types
data Type nm = Type nm :⊕: Type nm
             | Type nm :⊗: Type nm
             | Type nm :⊸: Type nm
             | Type nm :&: Type  nm
             | Zero | One | Top | Bot
             | TVar Bool Int
             | Forall nm (Type nm)
             | Exists nm (Type nm)
             | Bang (Type nm)
             | Quest (Type nm)

subst0 x = x:map var [0..]

-- | Sequents                              
data (Seq nm) = Exchange [Int] (Seq nm) -- Permute variables
         | Ax -- Exactly 2 vars
         | Cut nm (Type nm) Int (Seq nm) (Seq nm) -- new vars in position 0
           
         | Cross nm nm Int (Seq nm)
         | Par Int (Seq nm) (Seq nm) -- splits at given pos.
         | Plus Int (Seq nm) (Seq nm)
         | Amp Bool Int (Seq nm)
           
         | SOne Int (Seq nm)
         | SZero Int
         | SBot
         | What
           
         | TApp Int (Type nm) (Seq nm)
         | TUnpack Int (Seq nm)

         | Offer Int (Seq nm)
         | Demand Int (Seq nm)
         | Ignore Int (Seq nm)
         | Alias Int nm (Seq nm)

data Cell where
  Freed :: Cell
  Tag   :: Bool -> Cell
  New   :: Cell
  Delay :: Int -> Closure -> Cell
  Q     :: (Type nm) -> CellRef -> Cell

type CellRef = Int

type TypeEnv = [Type ()]

type Env = [CellRef]

type Closure = (Seq (),Env,TypeEnv)

type Heap = [Cell]

type System = ([Closure],Heap)

runClosure :: Heap -> Closure -> Maybe (Heap,Closure)
runClosure h (Plus v a b,e,te)
  | Tag c <- h!!(e!!v) = Just (replace (e!!v) Freed h,
                             (if c then a else b,increment v e,te))
runClosure h (Cross _ _ v a,e,te)
  = Just (h,(a,el++[x,x+sizeOf te (error "Game Over!")] ++ er,te))
  where (el,x:er) = splitAt v e

replace n v h = let (l,_:r) = splitAt n h
                in l ++ v : r

increment n e = let (l,x:r) = splitAt n e
                in l ++ succ x : r

sizeOf :: TypeEnv -> Type () -> Int
sizeOf e (t1 :⊕: t2) = 1 + max (sizeOf e t1) (sizeOf e t2)
sizeOf e (t1 :⊗: t2) = sizeOf e t1 + sizeOf e t2
sizeOf e (t1 :⊸: t2) = sizeOf e t1 + sizeOf e t2
sizeOf e (t1 :&: t2) = 1 + max (sizeOf e t1) (sizeOf e t2)
sizeOf e (TVar _ v) = sizeOf e (e!!v)
sizeOf e (Forall _ _) = 1
sizeOf e (Exists _ _) = 1
sizeOf e (Bang _)     = 1
sizeOf e (Quest _)    = 1
sizeOf e _ = 0

-- Substitution

class Substitute a where
  type Nm a
  (∙) :: Subst (Nm a) -> a -> a
         
instance Substitute (Type nm)  where
  type Nm (Type nm) = nm
  (∙) = apply
        
instance Substitute (Seq nm) where
  type Nm (Seq nm) = nm
  (∙) = applyS
        
instance Substitute Name where -- yuck
  type Nm Doc = Doc
  s ∙ x = x
       
        
instance (Substitute a, Substitute b,Nm a ~ Nm b) => Substitute (a,b) where
  type Nm (a,b) = Nm a
  f ∙ (x,y) = (f∙x, f∙y)

instance (Substitute a) => Substitute [a] where
  type Nm [a] = Nm a
  f ∙ xs = map (f ∙) xs
             
               
type Subst nm = [Type nm]

var = TVar True

wk :: Subst nm
wk = map var [1..]

if_ True f = id
if_ False f = f

apply :: Subst nm -> Type nm -> Type nm
apply f t = case t of
  x :⊕: y -> s x :⊕: s y
  x :&: y -> s x :&: s y
  x :⊸: y -> s x :⊸: s y
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
 where s = apply f
       s' = apply (var 0 : wk ∙ f)
  
applyS :: Subst nm -> (Seq nm) -> (Seq nm)
applyS f t = case t of
  (Exchange π a) -> Exchange π (s a)
  Cut w ty x a b -> Cut w (f ∙ ty) x (s a) (s b)
  Cross w w' x a -> Cross w w' x (s a)         
  Par x a b -> Par x (s a) (s b)
  Plus x a b -> Plus x (s a) (s b)
  Amp c x a -> Amp c x (s a)
  SOne x a -> SOne x (s a) 
  TApp x ty a -> TApp x (f ∙ ty) (s a)
  TUnpack x a -> TUnpack x (s' a)
  Offer x a -> Offer x (s a)
  Demand x a -> Demand x (s a)
  Ignore x a -> Ignore x (s a)
  Alias x w a -> Alias x w (s a)
  a -> a
 where s = applyS f
       s' = applyS (var 0 : wk ∙ f)
       
neg :: Type nm -> Type nm       
neg (x :⊗: y) = x :⊸: neg y
neg (x :⊸: y) = x :⊗: neg y
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


eval :: Deriv nm -> Deriv nm
eval (Deriv ts vs (Cut w ty γ a b)) = Deriv ts vs $ cut (length vs) w ty γ a b

-- Hereditary cut
cut :: Int -> -- ^ size of the context
       nm -> 
       Type nm -> 
       Int -> -- ^ where to cut it
       (Seq nm) -> (Seq nm) -> (Seq nm)
cut n w ty γ (Cut w' ty' δ a b) c = cut n w ty γ (cut γ w' ty' δ a b) c
cut n w ty γ a (Cut w' ty' δ b c) = cut n w ty γ a (cut (n-γ+1) w' ty' δ b c)
cut 2 w ty 1 Ax a = a
cut n _ (ta :⊗: tb) 
           γ (Exchange π (Par δ a b)) (Cross w w' 0 c) = exchange (π++[length π..n-1]) $ cut n w ta δ a (cut (n-δ+1) w' tb (γ-δ) b c)
cut n w (ta :⊕: tb) 
           γ (Amp c 0 a) (Plus 0 s t) = cut n w (if c then ta else tb) γ a (if c then s else t)
cut n w (Exists v ty) 
           γ (TApp 0 t a) (TUnpack 0 b) = cut n w (subst0 t ∙ ty) γ a (subst0 t ∙ b)
cut n w (Bang ty) 
           γ (Offer 0 a) (Demand 0 b) = cut n w ty γ a b
cut n w ty γ (Offer 0 a) (Ignore 0 b) = b
cut n w ty γ (Offer 0 b) (Alias 0 w' a) = cut n w ty γ (Offer 0 b) (cut (n+1) w' ty γ (Offer 0 b) a)
cut n w ty γ SBot (SOne 0 a) = a
cut n w ty γ What a = Cut w ty γ What a
cut n w ty γ a What = Cut w ty γ a What
cut n w ty γ a b = exchange ([γ..n-1] ++ [0..γ]) (cut n w (neg ty) (n-γ) b a)

-- Hereditary exchange
exchange π t = case t of
  Ax -> Ax
  (Cross w w' x c) -> Cross w w' (π!!x) (s' x c)
  Exchange ρ a -> exchange (map (π!!) ρ) a
  (Amp c x a) -> Amp c (π!!x) (s a) 
  (Plus x a b) -> Plus (π!!x) (s a) (s b)
  (TApp x t a) -> TApp (π!!x) t (s a)
  (TUnpack x a) -> TUnpack (π!!x) (s a)
  (Offer x a) -> Offer (π!!x) (s a)
  (Demand x a) -> Demand (π!!x) (s a)
  (Alias x w a) -> Alias (π!!x) w (s' x a)
  (Ignore x a) -> Ignore (π!!x) (del x a)
  (SOne x a) -> SOne (π!!x) (s a)
  (SZero x) -> SZero (π!!x)
  SBot -> SBot
  a -> Exchange π a
 where s = exchange π
       s' x = exchange (l++x:r)
              where (l,r) = splitAt x $ map (\y -> if y >= x then y+1 else x) π
       del x = exchange (l++r)
              where (l,_:r) = splitAt x $ map (\y -> if y > x then y-1 else x) π


data Deriv nm = Deriv {derivTypeVars :: [nm], derivContext :: [(nm,Type nm)], derivSequent :: Seq nm}

