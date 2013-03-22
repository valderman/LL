{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs, PostfixOperators #-}

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
  delay $ if_else (ypart (Center ▸ source - S ▸ target) .>. 0 ) 
              (mpRaw "drawarrow " <> out (Center ▸ source) <> "{down}..." <> out (NW ▸ target) <> "{down};\n")
              (mpRaw "drawarrow " <> out (Center ▸ source) <> "{down}..." <> out (SW ▸ target) <> "{up};\n")

boxIt o = do
  x <- o
  drawBounds x
  return x

drawFlexBox' open l = delay $ do
  freezeBounds l
  draw (NW ▸ l .-- (NE ▸ l .!)) [dashed evenly]
  draw (SW ▸ l .-- (SE ▸ l .!)) [dashed evenly]
  draw (NW ▸ l + (dx+:0) .-- NW ▸ l .-- SW ▸ l.-- (SW ▸ l + (dx+:0) .!)) []  
  unless open $ draw (NE ▸ l - (dx+:0) .-- NE ▸ l .-- SE ▸ l.-- (SE ▸ l - (dx+:0) .!)) []

 where dx = 7

drawFlexBox = drawFlexBox' False 
         
addSpace dx dy o = do
  o' <- abstractBox
  Center ▸ o === Center ▸ o'
  width o' === width o + 2*dx 
  height o' === height o + 2*dy
  return o'

forceWidth w o = do
  o' <- abstractBox
  Center ▸ o === Center ▸ o'
  width o' === w
  height o' === height o
  return o'

renderHeapPart :: SymHeap -> SymRef -> Render [Expr ObjectRef]
renderHeapPart h r = do
  oref <- lk r
  case oref of
    Just oref' -> return [oref']
    Nothing -> case M.lookup r h of 
      Nothing -> return []
      Just v -> do
       x0 <- renderCell h v
       x1 <- renderHeapPart h (Next r)
       x2 <- renderHeapPart h (Shift (error "this should not be used in lookup") r)
       y <- sequenceObjs 0 (x0 : x1 ++ x2)
       modify (M.insert r y)
       return [y]

halfSize = 14
fullSize = halfSize * 2

cellSz w t = lift (boxIt . forceWidth w =<< (textObj $ strut $ math t))

oneCell = cellSz fullSize
halfCell = cellSz halfSize

metaBox = lift (forceWidth 40 =<< (textObj $ strut $ ""))

renderCell h c = case c of
   Freed -> oneCell $ cmd0 "dagger" 
   Tag t -> oneCell $ if t then "1" else "0"
   New ->  oneCell ""
   NewMeta Empty -> do
     v <- metaBox
     lift $ drawFlexBox' True v
     return v
   NewMeta l -> do v <- metaBox
                   lift $ drawFlexBox v
                   text <- lift $ textObj $ strut $ math $ texLayout l
                   lift $ NW ▸ text + (0 +: 2) === SW ▸ v 
                   return v
   (Delay _ mc) -> do
       p <- oneCell ""
       case mc of
         Nothing -> return ()
         Just c -> do x <- renderClosure c 
                      lift $ NW ▸ x === SW ▸ p + (0 +: (-20))
                      lift $ link p x
       return p
   (Q t' r') -> do
              p1 <- halfCell $ renderTyp t'
              p2 <- halfCell $ ""
              renderHeapPart h r'
              link' p2 r'
              sequenceObjs 0 [p1,p2]

renderTyp = math . texClosedType . mkPositive

strut x = cmd0 "strut" <> x

renderTopHeapPart h t r = do
  [o] <- renderHeapPart h (Named t r)
  lift $ do
    -- l <- textObj $ strut $ math $ texLayout t
    -- NW ▸ l === SW ▸ o
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

link' val Null = lift $ delay $ do
                   draw (NE ▸ val .-- (SW ▸ val .!)) []
                   draw (NW ▸ val .-- (SE ▸ val .!)) []
link' val ref = do
   target <- lk ref
   case target of
       Just t -> lift $ link val t
       _ -> return () -- FIXME: show null pointers

renderEnv :: Env SymRef -> Render (Expr ObjectRef)
renderEnv env = sequenceObjs 0 =<< forM env (\(nm,ref) -> do
  target <- lk ref
  val <- lift $ boxObj 
  lift $ do
     lab <- renderVar nm
     NW ▸ val === SW ▸ lab
     height val === 12
     width val === 12
  link' val ref
  return val)

renderClosure :: Closure SymRef -> Render (Expr ObjectRef)
renderClosure (code,env,typeEnv) = do
   code' <- lift $ textObj $ texUntypedProg unknownTypeEnv (map fst env) code
   env' <- renderEnv env
   lift $ E ▸ code' + (5 +: 0)  === W ▸ env' 
   return $ env'
   
renderSystem :: System SymHeap -> Render ()
renderSystem (cls,h) = do
  h' <- renderHeap h
  cls' <- sequenceObjs 40 =<< mapM renderClosure cls
  lift $ Center ▸ h' + (0+:50) === Center ▸ cls' 
  return ()

diagSystem :: System SymHeap -> TeX 
diagSystem s = mpFigure [] (fst <$> (runRender $ renderSystem s)) >> return ()





