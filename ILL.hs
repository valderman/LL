{-# LANGUAGE GADTs #-}


module ILL where

import LL

-- TODO: polymorphism, exponential
data ILL where
  Var :: Int -> ILL
  Lam :: Name -> Type -> ILL -> ILL
  App :: ILL -> ILL -> ILL

  
fromILL :: ILL -> Seq
fromILL (Var _) = Ax dum
fromILL (Lam nm t body) = Cross dum " fx " nm 0 $ fromILL body
fromILL (App f a) = -- Missing exchange rule here.
  Cut "a" "_a" dum variablesUsedInF (exchange ([1,0] ++ [2..variablesUsedInF+1]) $ unCross 0 $ fromILL f) (fromILL a)

variablesUsedInF = error "todo"

unCross = error "invert Cross rule"