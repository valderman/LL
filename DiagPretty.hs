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

type Render a = StateT (M.Map SymRef (Expr ObjectRef)) MP a

lk :: SymRef ->  Render (Maybe (Expr ObjectRef))
lk r = do
  s <- get
  return $ M.lookup r s 

link :: Expr ObjectRef -> Expr ObjectRef -> D ()
link source target = do   
  delay $ mpRaw "drawarrow " <> out (Center ▸ source) <> "{down}..." <> out (NW ▸ target) <> "{down};\n"

boxIt o = do
  x <- o
  drawBounds x
  return x

addSpace dx dy o = do
  o' <- abstractBox
  Center ▸ o === Center ▸ o'
  width o' === width o + 2*dx 
  height o' === height o + 2*dy
  return o'

renderHeapPart :: SymHeap -> SymRef -> Type -> Render (Expr ObjectRef)
renderHeapPart h r t = do
  oref <- lk r
  case oref of
    Just oref' -> return oref'
    Nothing -> do
       oref' <- case mkPositive t of
          Meta _ nm _ -> lift $ boxIt $ addSpace 6 0 =<< renderVar nm
          TVar _ _ -> lift $ boxObj
          t0 :⊗: t1 -> do p1 <- renderHeapPart h r t0
                          p2 <- renderHeapPart h (Shift t0 r) t1    
                          sequenceObjs 0 [p1,p2]
          t0 :⊕: t1 -> case M.lookup r h of
            Just (Tag t) -> do 
              p1 <- lift $ boxIt $ textObj $ strut $ if t then "1" else "0"
              p2 <- renderHeapPart h (Next r) (if t then t0 else t1)
              sequenceObjs 0 [p1,p2]
            _ -> do
              p1 <- lift $ boxIt $ textObj $ strut $ "X" 
              p2 <- lift $ boxIt $ textObj $ strut $ (renderTyp t0 <> " or " <> renderTyp t1)
              modify (M.insert (Next r) p2)
              sequenceObjs 0 [p1,p2]
          _ -> lift $ boxIt $ textObj "OT"         
       modify (M.insert r oref')
       return oref'

renderTyp = math . texClosedType . mkPositive

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
    -- ypart (N ▸ o) === (-50)
    l <- textObj $ strut $ math $ texClosedType t
    NW ▸ l === SW ▸ o
    return o

renderHeap :: SymHeap -> Render (Expr ObjectRef)
renderHeap h = sequenceObjs 30 =<< sequence [renderTopHeapPart h t r | Named t r <- M.keys h]

sequenceObjs' :: Expr Numeric -> [Expr ObjectRef] -> D (Expr ObjectRef)
sequenceObjs' d [] = textObj $ strut ""
sequenceObjs' d [x] = return x
sequenceObjs' d (x:xs) = do 
  b <- abstractBox
  sequence_ [W ▸ x1 === (E ▸ x0) + (d +: 0) | (x0,x1) <- zip (x:xs) xs]
  NW ▸ b === NW ▸ x
  S ▸ b =-= S ▸ x
  E ▸ b =|= E ▸ y
  return b
 where y = last xs

sequenceObjs :: Expr Numeric -> [Expr ObjectRef] -> Render (Expr ObjectRef)
sequenceObjs d xs = lift $ sequenceObjs' d xs
       
runRender x = runStateT x M.empty

renderVar ('?':_) = abstractBox
renderVar nm = textObj $ strut $ math $ texVar $ nm

renderEnv :: Env SymRef -> Render (Expr ObjectRef)
renderEnv env = sequenceObjs 0 =<< forM env (\(nm,ref) -> do
  target <- lk ref
  lift $ do
     lab <- renderVar nm
     val <- boxObj 
     NW ▸ val === SW ▸ lab
     height val === 12
     width val === 12
     case target of
       Just t -> link val t
       _ -> return () 
     return val)

renderClosure :: Closure SymRef -> Render (Expr ObjectRef)
renderClosure (code,env,typeEnv) = do
   code' <- lift $ textObj $ block' $ texUntypedProg unknownTypeEnv (map fst env) code
   env' <- renderEnv env
   lift $ S ▸ code' === N ▸ env' + (0 +: 20) 
   return $ env'
   
renderSystem :: System SymHeap -> Render ()
renderSystem (cls,h) = do
  h' <- renderHeap h
  cls' <- sequenceObjs 40 =<< mapM renderClosure cls
  lift $ Center ▸ h' + (0+:50) === Center ▸ cls' 
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





