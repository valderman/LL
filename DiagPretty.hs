{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}

module DiagPretty where

import Control.Applicative
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
import TexPretty

unknownTypeEnv = repeat "VAR"

type Render a = StateT (M.Map SymRef (Expr ObjectRef)) MP a

followup :: Cell SymRef -> Type -> SymRef -> [(SymRef,Type)]
followup c t r = case t of 
  a :⊕: b -> nxt a b
  a :&: b -> nxt a b
  a :⊗: b -> [(Shift a r,b)]
  a :|: b -> [(Shift a r,b)]
  _ -> []
 where nxt a b = case c of
                     Tag True -> [(Next r,a)]
                     Tag False -> [(Next r,b)]
                     _ -> []          

renderHeapPart :: SymHeap -> SymRef -> Type -> Render (Expr ObjectRef)
renderHeapPart h r t = do
  s <- get
  case M.lookup r s of
    Just b -> return b
    Nothing -> do
      b <- lift $ boxObj
      modify (M.insert r b)
      case M.lookup r h of
        Nothing -> do
            -- lift $ width b === 0
            return ()
        Just c -> do x <- renderCell c
                     xs <- forM (followup c t r) $ \(r', t') -> do
                       renderHeapPart h r' t'
                     let allObjs = (x:xs)
                     sequenceObjs 0 allObjs
                     lift $ N ▸ x =-= N ▸ b
                     lift $ S ▸ x =-= S ▸ b
                     lift $ W ▸ x =|= W ▸ b
                     lift $ E ▸ (last allObjs) =|= E ▸ b
                     -- lift $ S ▸ b =-= S ▸ x
                     -- lift $ N ▸ b =-= N ▸ x
                     
      return b

strut x = cmd0 "strut" <> x

renderCell :: Cell SymRef -> Render (Expr ObjectRef)
renderCell c = lift $ case c of
      New -> textObj $ strut $ "X"
      Freed -> textObj $ strut $ math $ cmd0 "dagger"
      Tag True -> textObj $ strut "1"
      Tag False -> textObj $strut "0"
      Q ty r -> textObj $ strut "Q" -- paren $ texClosedType ty <> "," <> texRef r
      -- Delay _ c -> brac $ pClosure c -- FIXME: this makes the eval code crash
      Delay _ _ -> textObj $ strut "?"

renderTopHeapPart h t r = do
  o <- renderHeapPart h (Named t r) t
  l <- lift $ textObj $ strut $ math $ texClosedType t
  lift $ NW ▸ l === SW ▸ o
  return o

renderHeap :: SymHeap -> Render ()
renderHeap h = sequenceObjs 5 =<< sequence [renderTopHeapPart h t r | Named t r <- M.keys h]

sequenceObjs d [] = return ()
sequenceObjs d [x] = return ()
sequenceObjs d (x:y:xs) = do 
  lift $ (BaselineE ▸ x) + (d +: 0) === Baseline ▸ y 
  sequenceObjs d (y:xs)                         

runRender x = runStateT x M.empty

renderClosure :: Closure SymRef -> Render ()
renderClosure c = return ()

renderSystem :: System SymHeap -> Render ()
renderSystem (cls,h) = do
  renderHeap h
  mapM_ renderClosure cls

diagSystem :: System SymHeap -> TeX 
diagSystem s = mpFigure [] (fst <$> (runRender $ renderSystem s)) >> return ()

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





