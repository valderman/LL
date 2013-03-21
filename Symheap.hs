{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Symheap where

import Data.List (find,mapAccumL)
import qualified Data.Map as M
import Data.Map (Map)
import LL

data SymRef = Named Layout String | Shift Layout SymRef | Next SymRef | Null
instance Eq SymRef where
  Null == Null = True
  Named _ x == Named _ y = x == y
  Shift _ x == Shift _ y = x == y
  Next x == Next y = x == y
  _ == _ = False
instance Ord SymRef where
  compare Null Null = EQ
  compare (Named _ x) (Named _ y) = compare x y
  compare (Shift _ x) (Shift _ y) = compare x y
  compare (Next x) (Next y) = compare x y
  
  compare Null _ = LT
  compare (Named _ _) _ = LT
  compare (Shift _ _) _ = LT
  compare _ _ = GT

type SymHeap = Map SymRef (Cell SymRef)

instance IsRef SymRef where
  shift = Shift
  next  = Next
  nullPointer = Null



allocAt r t = case t of 
      -- Empty -> id -- I want to see empty things
      a `Then` b -> allocAt r a . allocAt (shift a r) b
      Bit b -> allocAtom r New . allocAt (next r) b
      Pointer _ -> allocAtom r New -- alternative: (Delay "n" Nothing)       
      t -> allocAtom r (NewMeta t)

allocAtom r t = M.alter (\_ -> Just t) r

instance IsHeap SymHeap where
  type Ref SymHeap = SymRef
  h ! r = M.findWithDefault New r h
  replace r v = M.alter (\_ -> Just v) r 
  alloc' t h = (allocAt r t h,r)
    where r:_ = filter (\x -> not (x `elem` M.keys h)) $ 
                 map (Named t) $ [nam ++ idx | idx <- "":map show [1..], nam <- ["p","q","r","s"] ]
  emptyHeap = M.empty

-- Similar to derivToSystem, re-uses parts an already existing heap if the types match.
toSystem :: SymHeap -> Deriv -> System SymHeap
toSystem h0 (Deriv _ ctx a) = ([closure],heap)
  where closure = (a,zip (map fst ctx) refs,[])
        (heap,refs) = mapAccumL allocMaybe h0 (map snd ctx)
        
        allocMaybe :: SymHeap -> Type -> (SymHeap, SymRef)
        allocMaybe h t = case find goodType $ M.keys h of
                           Just p -> (h,p)
                           Nothing -> alloc t h
            where goodType (Named t' p) = mkLayout t == t'
                  goodType _ = False
