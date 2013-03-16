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

lk :: SymRef ->  Render (Maybe (Expr ObjectRef))
lk r = do
  s <- get
  return $ M.lookup r s 

link :: Expr ObjectRef -> Expr ObjectRef -> D ()
link source target = do
  delay $ mpRaw "drawarrow " <> out (Center ▸ source) <> "{down}.." <> out (NW ▸ target) <> "{down};\n"

renderHeapPart :: SymHeap -> SymRef -> Type -> Render (Expr ObjectRef)
renderHeapPart h r t = do
  oref <- lk r
  case oref of
    Just b -> return b
    Nothing -> do
      b <- case M.lookup r h of
        Nothing -> lift $ textObj $ strut $ "..."
        Just c -> do x <- renderCell c
                     xs <- forM (followup c t r) $ \(r', t') -> do
                       renderHeapPart h r' t'
                     let allObjs = (x:xs)
                     sequenceObjs 0 allObjs
      modify (M.insert r b)
      lift $ drawBounds b
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
  lift $ do
    l <- textObj $ strut $ math $ texClosedType t
    ypart (N ▸ o) === (-50)
    NW ▸ l === SW ▸ o
    return o

renderHeap :: SymHeap -> Render (Expr ObjectRef)
renderHeap h = sequenceObjs 30 =<< sequence [renderTopHeapPart h t r | Named t r <- M.keys h]

sequenceObjs' :: Expr Numeric -> [Expr ObjectRef] -> D (Expr ObjectRef)
sequenceObjs' d [] = textObj $ strut ""
sequenceObjs' d [x] = return x
sequenceObjs' d (x:xs) = do 
  b <- abstractBox
  sequence_ [(BaselineE ▸ x1) + (d +: 0) === Baseline ▸ x0 | (x0,x1) <- zip (x:xs) xs]
  Baseline ▸ b === Baseline ▸ x
  N ▸ b =-= N ▸ x
  S ▸ b =-= S ▸ x
  E ▸ b =|= E ▸ x
  return b
 where y = last xs

sequenceObjs :: Expr Numeric -> [Expr ObjectRef] -> Render (Expr ObjectRef)
sequenceObjs d xs = lift $ sequenceObjs' d xs
       
runRender x = runStateT x M.empty

renderEnv :: Env SymRef -> Render (Expr ObjectRef)
renderEnv env = sequenceObjs 0 =<< forM env (\(nm,ref) -> do
  target <- lk ref
  lift $ do
     lab <- textObj $ strut $ math $ texVar $ nm
     val <- boxObj 
     NW ▸ val === SW ▸ lab
     height val === 12
     width val === 12
     case target of
       Just t -> link val t
       _ -> return () 
     return val)

renderClosure :: Closure SymRef -> Render (Expr ObjectRef)
renderClosure (code,env,typeEnv) = renderEnv env

renderSystem :: System SymHeap -> Render ()
renderSystem (cls,h) = do
  renderHeap h
  sequenceObjs 40 =<< mapM renderClosure cls
  return ()

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





