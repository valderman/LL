{-# LANGUAGE GADTs #-}


module ILL where

import LL

-- TODO: polymorphism, exponential
data ILL where
  Var :: ILL
  Lam :: Name -> Type -> ILL -> ILL
  App :: Int -> -- ^ where to cut the context
         ILL -> ILL -> ILL

  

fromILL :: Int -> ILL -> Seq
fromILL γ (Var) = Ax dum
fromILL γ (Lam nm t body) = Cross dum " fx " nm 0 $ exchange ([1..γ-1] ++ [0]) $ fromILL body
fromILL γδ (App γ f a) = Cut "a" "_a" dum γ (unCross γ $ fromILL f) (fromILL a)

unCross γ s = Cut "x" "_x" dum γ s (Exchange [1,0,2] $ Par dum "a" "b" 1 (Ax dum) (Ax dum))

{-
unCross :: Int -> LL -> LL
unCross x (Cross _ _ _  y s) | x == y = s
unCross x (Cross v v' ty y s) = Cross v v' (if x < y then y + 1 else y)  (unCross (if x > y then x + 1 else x) s)
unCross x (Plus v v' y s t) = Plus v v' (if x < y then y + 1 else y) (unCross x s) (unCross x t)
...
unCross x (Exchange π t) = Exchange π' (unCross (π!!x) t)
unCross 0 (Ax _) = Exchange [0,2,1] $ Par dum "a" "b" 1 (Ax dum) (Ax dum)
unCross 1 (Ax _) = ...
unCross x (Cut v v' ty y s t) | x <  y = Cut v v' ty (y+1) (unCross x s) t
                              | x >= y = Cut v v' ty y     s             (unCross (x - y) t)
  
-}