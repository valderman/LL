{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}

module DiagPretty where

import Control.Lens
import LL 
import MarXup.Tex
import MarXup.Latex
import MarXup.Diagrams
import MarXup.MetaPost
import Data.List
import Data.String
import Data.Monoid
import qualified Data.Map as M
import Symheap
import Control.Monad.State

par = cmd0 "parr"
amp = cmd "hspace" "1pt" <> cmd0 "&"  <> cmd "hspace" "1pt" 

unknownTypeEnv = repeat "VAR"

type Render a = StateT (M.Map SymRef ObjectRef) MP a

renderHeapPart :: SymHeap -> SymRef -> Type -> Render ObjectRef
renderHeapPart h r t = do
  s <- get
  case M.lookup r s of
    Just b -> return b
    Nothing -> do
      b <- mkBox
      modify (M.insert r b)
      case M.lookup r h of
        Nothing -> do 
           setWidth b 20
           setHeight b 10
        Just c -> do x <- renderCell c
                     -- y <- renderHeapPart
                     W ▸ x === W ▸ b
                     E ▸ x === E ▸ b
      return b


renderCell :: Cell SymRef -> Render mpLabel
renderCell c = case c of
      _ -> textObj "?"

renderHeap :: SymHeap -> Render ()
renderHeap h = sequence_ $ [renderHeapPart h (Named t r) t | Named t r <- M.keys h]

{-
texSystem :: System SymHeap -> TeX
texSystem (cls,h) = do
  "H=…," <>texHeap h <>";"
  "C=ξ," <> commas (map pClosure cls)

commas = mconcat . intersperse ", "

pClosure :: Closure SymRef -> TeX
pClosure (seq,env,typeEnv) = 
  brac (block' $ texUntypedProg unknownTypeEnv (map fst env) seq)  <> brack (commas $ [texVar' nm r | (nm,r) <- env])
  
texVar' :: String -> SymRef -> TeX  
texVar' "" r = "…"
texVar' ('?':x) r = textual x
texVar' x r = textual x <> "=" <> texRef r
  

-}





