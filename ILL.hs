{-# LANGUAGE GADTs #-}


module ILL where

import Pretty
import LL

-- TODO: polymorphism, exponential
data ILL where
  Var  :: ILL
  Lam  :: Name -> Type -> ILL -> ILL
  App  :: Int -> -- ^ where to cut the context
         ILL -> ILL -> ILL
  Case :: Int -> ILL -> ILL -> ILL
  Inl  :: ILL -> ILL
  Inr  :: ILL -> ILL

fromILL :: Int -> ILL -> Seq
fromILL γ (Var) = Ax dum
fromILL γ (Lam nm t body) = Cross dum " fx " nm 0 $ exchange ([1..γ-1] ++ [0]) $ fromILL (γ+1) body
fromILL γδ (App γ f a) = Cut "a" "_a" dum γ (unCross γ $ fromILL γ f) (fromILL (γδ-γ) a)
fromILL γ (Case x l r) = Plus "l" "r" (x+1) (fromILL γ l) (fromILL γ r)
fromILL γ (Inl a) = With "l" True 0  (fromILL γ a)
fromILL γ (Inr a) = With "r" False 0 (fromILL γ a)

unCross γ s = Cut "x" "_x" dum γ s (Exchange [1,0,2] $ Par dum "a" "b" 1 (Ax dum) (Ax dum))

-- examples

bool = One :⊕: One

not_ill = Lam "x" bool (Case 0 (Inr Var) (Inl Var))

pp_not_ill = fillTypes $ Deriv [] [("result", neg $ bool ⊸ bool)] (fromILL 0 not_ill)
