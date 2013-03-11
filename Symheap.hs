{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Symheap where

import qualified Data.Map as M
import Data.Map (Map)
import LL

data SymRef = Named String | Shift (Type) SymRef | Next SymRef
instance Eq SymRef where
  Named x == Named y = x == y
  Shift _ x == Shift _ y = x == y
  Next x == Next y = x == y
  _ == _ = False
instance Ord SymRef where
  compare (Named x) (Named y) = compare x y
  compare (Shift _ x) (Shift _ y) = compare x y
  compare (Next x) (Next y) = compare x y
  compare (Named _) _ = LT
  compare (Shift _ _) _ = LT
  compare _ _ = GT

type SymHeap = Map SymRef (Cell SymRef)

instance IsRef SymRef where
  shift = Shift
  next  = Next

instance IsHeap SymHeap where
  type Ref SymHeap = SymRef
  h ! r = M.findWithDefault New r h
  replace r v = M.alter (\_ -> Just v) r 
  alloc t h = (M.alter (\_ -> Just New) r h,r)
    where r:_ = filter (\x -> not (x `elem` M.keys h)) $ map Named $ [nam ++ idx | nam <- ["p","q"], idx <- "":map show [1..] ]
  emptyHeap = M.empty

toSystem :: Deriv -> System SymHeap
toSystem = derivToSystem

