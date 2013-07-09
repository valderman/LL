{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}

module TexPretty where

import LL 
import AM
import MarXup.Tex
import MarXup.Latex
import MarXup.DerivationTrees
import Data.List
import Data.Monoid
import qualified Data.Map as M
import Symheap

-- infix 1 `for`
-- x `for` lens = over lens x

par, amp :: TeX
par = math $ cmd0 "parr"
amp = math $ cmd "hspace" "1pt" <> cmd0 "&"  <> cmd "hspace" "1pt" 

ruleName = math . cmd "text" . smallcaps

index ix = tex "_" <> braces ix

indicator b = if b then "1" else "0"

isBoson' False x = x
isBoson' True  x = ruleName "B" <> x

seqName = seqName' unknownTypeEnv
seqName' ctx s = case s of
   (Exchange _ _)      -> ruleName "Ex."
   (Ax _ )             -> ruleName "Ax"
   (Cut _ _ _ _ _ _)   -> ruleName "Cut"
   (Cross  β _ _ _ _ _)   -> isBoson' β "⊗"
   (Par  β _ _ _ _ _ _)   -> isBoson' β "⅋"
   (Plus  _ _ _ _ _)   -> "⊕"
   (With β _ _ b _ _)  -> isBoson' β $ math $ (amp <> index (indicator b))
   (SOne  β _ _)          -> isBoson' β $ "1"
   (SZero _)           -> "0"
   SBot                -> "⊥"
   (TApp  β _ _ _ _ _)    -> isBoson' β "∀"
   (TUnpack _ _ _)     -> "∃"
   (Offer  β _ _ _)       -> isBoson' β "?"
   (Demand _ _ _ _)    -> "!"
   (Ignore β _ _)        -> isBoson' β $ ruleName "Wk"
   (Alias β _ _ _)       -> isBoson' β $ ruleName "Con"
--   (Channel ty)        -> ruleName "Ch" <> index (texType 0 ctx ty)
   (ChanPlus b)  -> ruleName "B" <>  (indicator b)
   -- (ChanCross ta tb)   -> ruleName "Ch(<)"
   -- (ChanPar   ta tb)   -> ruleName "Ch(>)" <> math (texType 0 ctx (ta :|: tb))
   (ChanTyp   tmono)   -> ruleName "B" <> math ( (index (texType 0 ctx tmono)))
   (MemEmpty  _ n)     -> ruleName "Empty" <> math ( (textual (show n)))
   (MemFull   _ n)     -> ruleName "Full" <> math ( (textual (show n)))
   (Mem ty x n _ _)      -> ruleName "BM" <> math ( index (textual (show n)) 
                                                    -- <> texType 0 ctx ty 
                                                  ) 

seqLab :: Seq -> String 
seqLab s = case s of
   (Exchange _ s)      -> "X"
   (Ax _ )             -> "Ax"
   (Cut _ _ _ _ _ _)   -> "Cut"
   (Cross β _ _ _ _ _) -> "⊗"
   (Par β _ _ _ _ _ _) -> "⅋"
   (Plus  _ _ _ _ _)   -> "⊕"
   (With β _ _ b _ _)      -> "\\&"
   (SOne β _ _)          -> "1"
   (SZero _)           -> "0"
   (SBot)               -> "⊥"
   (TApp β _ _ _ _ _)    -> "∀"
   (TUnpack _ _ _)     -> "∃"
   (Offer β _ _ _)       -> "?"
   (Demand _ _ _ _)    -> "!"
   (Ignore _ _ _)        -> "Wk"
   (Alias β _ _ _)       -> "Con"
   (Mem _ _ _ _ _)       -> "M"
--   (Channel ty)        -> "Ch"
   (ChanPlus b)  -> indicator b
   (ChanCross ta tb)   -> "<"
   (ChanPar   ta tb)   -> ">"
   (ChanTyp   tmono)   -> "T" 
   (MemEmpty  _ n)     -> "E"
   (MemFull   _ n)     -> "F"


texSeq :: Bool -> [String] -> [(String,Type)] -> Seq -> Derivation
texSeq showProg = foldSeq sf where
 sf (Deriv ts vs seq) = SeqFinal {..} where
  sty = texType 0
  schannel _ = rul []
  schplus _ _ _ = rul []
  schcross _ _ = rul []
  schpar _ _ = rul []
  schtyp _ _ = rul []
  schempty _ _ = rul []
  schfull _ _ = rul []
  
  sax v v' _ = rul []
  scut v _ _ s _ t = rul [s,t]
  scross _ w v vt v' vt' t = rul [t]
  spar _ w _ vt _ vt' s t = rul [s,t]
  splus w _ vt _ vt' s t = rul [s,t]
  swith _ _ b w _ _ s = rul [s]
  sbot v = rul []
  szero w vs = rul []
  sone _ w t = rul [t]
  -- sxchg _ s = rul [s] -- to display the exchange rules
  sxchg _ s = s -- to hide them
  stapp _ w _ _ tyB s = rul [s]
  stunpack tw w _ s = rul [s]
  soffer _ w _ ty s = rul [s] 
  sdemand w _ ty s = rul [s]
  signore w ty s = rul [s]
  salias _ w w' ty s = rul [s]
  smem _ t u = rul [t,u]
  swhat _ _ _ = Node (Rule () None mempty mempty (texCtx showProg ts vs <> "⊢" <> 
                                                  sureProg -- always show the program so we know how to refer to this proof continuation.
                                                 )) [] 
                -- [defaultLink ::> Node (Rule () None mempty mempty (textual a)) []]
                
  rul :: [Derivation] -> Derivation
  rul subs = Node (Rule () Simple mempty (seqName' ts seq) (texCtx showProg ts vs <> "⊢" <> maybeProg)) (map (defaultLink ::>) subs)
  maybeProg = if showProg then sureProg else mempty
  sureProg = linearize (texProg ts vs seq)



keyword :: String -> TeX 
keyword = math . mathsf . tex
let_ = keyword "let "
case_ = keyword "case "
connect_ = keyword "connect "
ignore_ = keyword "ignore "
dump_ = keyword "dump "
[fst_,snd_] = map keyword ["fst ","snd "]
separator :: TeX
separator = cmd "hline" mempty

block' = xblock "l" . map (:[])
xblock :: TeX -> [[TeX]] -> TeX
xblock format bod@(firstRow:_) = do
  env' "array" ["t"] $ do
    braces format
    mkrows (map mkcols bod)
  return ()

-- left_ = cmd "Leftarrow" mempty
-- right_ = cmd "Rightarrow" mempty

indent :: TeX
indent = cmd "hspace" $ tex "1em"

data Block = Split TeX [(TeX,Block)] | Final TeX | Instr TeX Block

texProg :: [Name] -> [(Name, Type)] -> Seq -> Block
texProg = texProg' True
texUntypedProg :: [Name] -> [Name] -> Seq -> TeX
texUntypedProg ts vs s = math $ linearize $ texProg' False ts (zip vs (repeat $ error "accessing type when texing untyped term")) s

linearize :: Block -> TeX
linearize (Final t) = t
linearize (Instr h t) = h <> ";" -- keyword " in " 
                          <> linearize t
linearize (Split h xs) = h <> brac (punctuate "; " [x<>cmd0 "mapsto"<> linearize ts | (x,ts) <- xs])

indentation :: Block -> Tex ()
indentation t = math $ block' $ indentation' t

indentation' :: Block -> [TeX]
indentation' (Final t) = [t]
indentation' (Split h xs) = [h,
                           indent <> xblock (tex "l@{}l") [[x,mapsto (indentation' a)] | (x,a) <- xs]]
indentation' (Instr h t) = h : indentation' t

mapsto :: [TeX] -> TeX
mapsto xs = cmd "mapsto" (xblock "l" $ map (:[]) xs)


connect z x a y b = Split (connect_ <> z) [(x,a),(y,b)]

texProg' :: Bool -> [Name] -> [(Name, Type)] -> Seq -> Block
texProg' = texProg'' what
  where what a [] _ = texVar a
        what a ws _ = texVar a <> brack (commas $ map texVar ws)

texProg'' :: (Name -> [Name] -> [(Name,Type)] -> TeX) -> Bool -> [Name] -> [(Name,Type)] -> Seq -> Block
texProg'' what showTypes = foldSeq sf where
   sf :: Deriv -> SeqFinal TeX Block
   sf (Deriv ts vs _) = SeqFinal {..} where
      sty ts t  = texType 0 ts t
      schannel _    = Final "CHAN"
      schplus _ _ _ = Final "CHAN"
      schcross _ _  = Final "CHAN"
      schpar _ _    = Final "CHAN"
      schtyp _ _    = Final "CHAN"
      schempty _ _  = Final "CHAN"
      schfull _ _   = Final "CHAN"
      sax v v' _ = Final $ texVar v <> " ↔ " <> texVar v'
      scut v v' vt' s vt t = connect mempty (texVarT' v   vt') s
                                            (texVarT' v'  vt ) t
      scross _ w v vt v' vt' t = Instr (let_ <> texVar v <> "," <> texVar v' <> " = " <> texVar w) t
      spar _ w v vt v' vt' s t = connect (keyword "via " <> texVar w) 
                        (texVarT' v  vt ) s
                        (texVarT' v' vt') t
      splus w v vt v' vt' s t = Split (case_ <> texVar w <> keyword " of")
                      [(keyword "inl " <> texVar v,s),
                       (keyword "inr " <> texVar v',t)]
      swith _ _ b w v' ty s = let'' (texVarT' v' ty) (c <> texVar w) s
         where c = if b then fst_ else snd_
      sbot v = Final $ keyword "halt " <> texVar v
      szero w vs  = Final $ dump_ <> whenShowTypes (texCtx' True vs) <> keyword " in " <> texVar w
      sone _ w t = let'' (cmd0 "diamond") (texVar w) t
      sxchg _ t = t
      stapp _ v _ w tyB s = let'' (texVar w) (texVar v <> cmd0 "bullet" <> tyB)  s
      stunpack tw w v s = let'' (whenShowTypes (texVar tw) <> "," <> texVar v) (texVar w)  s
      soffer _ v w ty s = let'' (texVarT' w ty) (keyword "offer " <> texVar v)  s
      sdemand v w ty s = let'' (texVarT' w ty) (keyword "demand " <> texVar v)  s
      signore w ty s = Instr (ignore_ <> texVar w)  s
      salias _ w w' ty s = let'' (texVarT' w' ty) (keyword "alias " <> texVar w)  s 
      swhat a ws fs = Final $ what a ws fs
      smem ty t tx = Final $ "MEM"
      let'' w    v t = Instr (let_ <> w <> "=" <> v) t
   texVarT' x y | showTypes = texVarT x y
                | otherwise = texVar x                            
   whenShowTypes | showTypes = id                            
                 | otherwise = const "?"
     
texVarT [] t = t
texVarT ('?':_) t = t
texVarT v t = texVar v <> ":" <> t
       
texVar :: String -> TeX
texVar ['?','γ'] = "Γ"
texVar ('_':nm) = cmd "bar" $ texVar nm
texVar nm | length pre > 0 && length post > 1 = textual pre <> tex "_{" <> 
                                                textual (tail post) <> tex "}"
          | otherwise = textual nm
  where (pre,post) = break (=='_') nm

texCtx :: Bool -> [String] -> [(String,Type)] ->  TeX
texCtx showVars ts vs = do
  -- uncomment to show the types context
  -- commas (map texVar $ reverse ts) >>  textual ";"
  texCtx' showVars (map (second (texType 0 ts)) vs)


texCtx' True vs = commas [texVarT v t | (v,t) <- vs]
texCtx' False vs = commas [t | (v,t) <- vs]
    
texLayout :: Layout -> TeX
texLayout (a `Then`b) = texLayout a <> "+" <> texLayout b
texLayout (Bit a) = "1+" <> texLayout a
texLayout (Pointer a) = cmd0 "rightarrow" <> texLayout a
texLayout (MetaL t) = "|" <> texClosedType t <> "|"
texLayout (Union a b) = texLayout a <> "⊔" <> texLayout b
texLayout Empty = "0"

texType :: Int -> [String] -> Type -> TeX
texType p vs (MetaNeg t) = prn p 5 $ texType 5 vs t <> texNeg False
texType p vs (Forall v t) = prn p 0 $ "∀" <> texVar v <> ". "  <> texType 0 (v:vs) t
texType p vs (Exists v t) = prn p 0 $ "∃" <> texVar v <> ". "  <> texType 0 (v:vs) t
texType p vs (x :|: y) = prn p 0 $ texType 1 vs x <> "⅋" <> texType 0 vs y
texType p vs (x :⊕: y) = prn p 1 $ texType 2 vs x <> "⊕" <> texType 1 vs y
texType p vs (x :⊗: y) = prn p 2 $ texType 2 vs x <> "⊗" <> texType 2 vs y
texType p vs (x :&: y) = prn p 3 $ texType 3 vs x <> amp <> texType 3 vs y
texType _  _ Zero = "0"
texType _  _ One = "1"
texType _  _ Top = "⊤"
texType _  _ Bot = "⊥"
texType _ vs (TVar b x) = texVar (vs!!x) <> texNeg b
texType p vs (Bang t) = prn p 4 $ "!" <> texType 4 vs t
texType p vs (Quest t) = prn p 4 $ "?" <> texType 4 vs t
texType p vs (Meta b x as) = prn p 4 $ textual x <> as' <> texNeg b
  where as' = if null as then mempty else  brack (commas $ map (texType 0 vs) as)
texType p vs (Lollipop x y) = prn p 0 $ texType 1 vs x <> "⊸" <> texType 0 vs y
prn p k = if p > k then paren else id
       

{-
unicodeTextual :: String -> TeX
unicodeTextual = foldMap unicodeToTex

unicodeToTex :: Char -> TeX
unicodeToTex 'α' = cmd0 "alpha"
unicodeToTex '\945' = cmd0 "oops"
unicodeToTex 'β' = cmd0 "beta"
unicodeToTex 'Γ' = cmd0 "Gamma"
unicodeToTex 'Δ' = cmd0 "Delta"
unicodeToTex 'Ξ' = cmd0 "Xi"
unicodeToTex c = tex [c]
-}

texNeg :: Bool -> TeX
texNeg True = mempty
texNeg False = "⟂" -- I am using just one character so that graphviz
                   -- doesn't get crazy; the proper rendering is done
                   -- at the latex level.

--------------------------------
-- Pretty printing of closures

unknownTypeEnv = repeat "VAR"

texClosedType = math . texClosedTypeNoMath
texClosedTypeNoMath = texType 0 unknownTypeEnv

texRef Null = cmd "mathsf" "NULL"
texRef (Named _ x) = textual x
texRef (Shift t x) = texRef x <> "+" <> texLayout t
texRef (Next x) = texRef x <> "+1"


heapPart :: SymHeap -> SymRef -> [Maybe TeX]
heapPart h r = case v of
    Nothing -> [Nothing]
    Just c -> Just (texCell c) : heapPart h (Next r) ++ heapPart h (Shift (error "this should not be used in lookup") r)
  where v = M.lookup r h
        
texCell :: Cell SymRef -> TeX
texCell c = case c of
      New -> "□"
      NewMeta _ -> "□"
      Freed -> cmd0 "dagger"
      Tag True -> "1"
      Tag False -> "0"
      Q ty r -> paren $ texClosedType ty <> "," <> texRef r
      Delay (SymCount n) Nothing -> brac "" <> textual n
      Delay (SymCount n) (Just c) -> brac (pClosure c) <> textual n

doNothings (Nothing:Nothing:xs) = doNothings (Nothing:xs)
doNothings (x:xs) = x : doNothings xs
doNothings [] = []

texHeapPart h r = commas $ map (maybe "…" id) $ doNothings $ heapPart h r

texHeap :: SymHeap -> TeX
texHeap h = commas [textual r <> "↦" <> texHeapPart h (Named t r) | Named t r <- M.keys h, displayed t]
  where displayed (MetaL (Meta _ x _)) | x `elem` ["Γ","Δ"] = False
        displayed Empty = False
        displayed _ = True

texSystem :: System SymHeap -> TeX
texSystem (cls,h) = do
  "H=…," <>texHeap h <>";"
  "C=ζ," <> commas (map pClosure cls)

punctuate p = mconcat . intersperse p
commas = punctuate ", "

pClosure :: Closure SymRef -> TeX
pClosure (seq,env,typeEnv) = 
  brac (texUntypedProg unknownTypeEnv (map fst env) seq) <> brack (commas $ [texVar' nm r | (nm,r) <- env])
  
texVar' :: String -> SymRef -> TeX  
texVar' "" _r = "…"
texVar' ('?':x) _r = texVar x
texVar' x r = texVar x <> "=" <> texRef r
  


