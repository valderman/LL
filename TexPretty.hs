{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}

module TexPretty where

import Control.Lens
import LL 
import MarXup.Tex
import MarXup.Latex
import MarXup.DerivationTrees
import Data.List
import Data.String
import Data.Monoid

infix 1 `for`
x `for` lens = over lens x

par = cmd0 "parr"
amp = cmd "hspace" "1pt" <> cmd0 "&"  <> cmd "hspace" "1pt" 

smallcaps :: TeX -> TeX
smallcaps x = braces (cmd0 "sc" <> x)

rulText = cmd "text" . smallcaps 

texSeq :: Bool -> [String] -> [(String,Type)] -> Seq -> Derivation
texSeq showProg = foldSeq sf where
 sf (Deriv ts vs seq) = SeqFinal {..} where
  sty = texType 0
  sax v v' _ = rul (rulText "Ax") []
  scut v _ s _ t = rul (rulText "Cut") [s,t]
  scross _ w v vt v' vt' t =  rul "⊗" [t]
  spar _ w vt vt' s t = rul par [s,t]
  splus _ w vt vt' s t = rul "⊕" [s,t]
  swith b _ w _ t = rul  (amp<>tex"_"<>if b then "1" else "2") [t]
  sbot v = rul "⊥" []
  szero w vs = rul "0" []
  sone w t = rul "1" [t]
  sxchg _ t = rul (rulText "Exch.") [t]
  stapp w tyB s = rul "∀" [s]
  stunpack tw w s = rul "∃" [s]
  soffer w ty s = rul "?" [s] 
  sdemand w ty s = rul "!" [s]
  signore w ty s = rul (rulText "Weaken") [s]
  salias w w' ty s = rul (rulText "Contract") [s]
  swhat a = Node (Rule () None mempty mempty (texCtx ts vs <> "⊢" <> texVar a))  []
  rul :: TeX -> [Derivation] -> Derivation
  rul n subs = Node (Rule () Simple mempty n (texCtx ts vs <> "⊢" <> maybeProg)) (map (defaultLink ::>) subs)
  maybeProg = if showProg then block (texProg ts vs seq) else mempty


keyword :: String -> TeX 
keyword = mathsf . tex
let_ = keyword "let~"
case_ = keyword "case~"
in_ = keyword "~in~"
connect_ = keyword "connect~"
[fst_,snd_] = map keyword ["fst~","snd~"]
separator :: TeX
separator = cmd "hline" mempty

xblock :: TeX -> [[TeX]] -> TeX
xblock format bod@(firstRow:_) = do
  env' "array" ["t"] $ do
    braces format
    mkrows (map mkcols bod)
  return ()

mapsto :: [TeX] -> TeX
mapsto xs = cmd "mapsto" (xblock "l" $ map (:[]) xs)
-- left_ = cmd "Leftarrow" mempty
-- right_ = cmd "Rightarrow" mempty

indent :: TeX
indent = cmd "hspace" $ tex "1em"

alts :: TeX -> [TeX] -> TeX -> [TeX] -> [TeX]
alts x a y b = [indent <> xblock (tex "l@{}l") [[x,mapsto a],
                                                [y,mapsto b]]]

connect z x a y b = connect_ <> z : alts x a y b

texProg :: [Name] -> [(Name,Type)] -> Seq -> [TeX]
texProg = foldSeq sf where
 sf :: Deriv -> SeqFinal TeX [TeX]
 sf (Deriv ts vs _) = SeqFinal {..} where
  sty = texType 0
  sax v v' _ = [texVar v <> " ↔ " <> texVar v']
  scut v vt' s vt t = connect mempty (texVarT v vt') s
                                     (texVarT v vt ) t
  scross _ w v vt v' vt' t = (let_ <> texVar v <> "," <> texVar v' <> " = " <> texVar w <> in_) : t
  spar _ w vt vt' s t = connect (keyword "via~" <> texVar w) 
                    (texVarT w vt ) s
                    (texVarT w vt') s
  splus _ w vt vt' s t = case_ <> texVar w <> keyword "~of" :
                  alts (keyword "inl~" <> texVar w) s
                       (keyword "inr~" <> texVar w) t
  swith b _ w ty s = let'' (texVarT w ty) (c <> texVar w) : s
     where c = if b then fst_ else snd_
  sbot v = [texVar v]
  szero w vs = [keyword "dump~" <> texCtx' vs <> in_ <> texVar w]
  sone w t = let'' (cmd0 "diamond") (texVar w) : t
  sxchg _ t = t
  stapp w tyB s = let'' (texVar w) (texVar w <> cmd0 "bullet" <> tyB) : s
  stunpack tw w s = let'' (texVar tw <> "," <> texVar w) (texVar w) : s
  soffer w ty s = (keyword "offer~" <> texVarT w ty) : s
  sdemand w ty s = (keyword "demand~" <> texVarT w ty) : s
  signore w ty s = (keyword "ignore~" <> texVarT w ty) : s
  salias w w' ty s = let'' (texVarT w' ty) (keyword "alias~" <> texVar w) : s 
  swhat a = [texVar a]
  let'' w    v = let_ <> w <> "=" <> v
     
texVarT [] t = t
texVarT v t = texVar v <> ":" <> t
       
texVar :: String -> TeX              
texVar = textual
             
prn p k = if p > k then paren else id
       
texCtx :: [String] -> [(String,Type)] ->  TeX
texCtx ts vs = texCtx' (over (mapped._2) (texType 0 ts) vs)


texCtx' vs = mconcat $ intersperse "," [texVarT v t | (v,t) <- vs]
    

texType :: Int -> [String] -> Type -> TeX
texType p vs (Forall v t) = prn p 0 $ "∀" <> texVar v <> ". "  <> texType 0 (v:vs) t
texType p vs (Exists v t) = prn p 0 $ "∃" <> texVar v <> ". "  <> texType 0 (v:vs) t
texType p vs (x :|: y) = prn p 0 $ texType 1 vs x <> par <> texType 0 vs y
texType p vs (x :⊕: y) = prn p 1 $ texType 2 vs x <> " ⊕ " <> texType 1 vs y
texType p vs (x :⊗: y) = prn p 2 $ texType 2 vs x <> " ⊗ " <> texType 2 vs y
texType p vs (x :&: y) = prn p 3 $ texType 3 vs x <> amp <> texType 3 vs y
texType p vs Zero = "0"
texType p vs One = "1"
texType p vs Top = "⊤"
texType p vs Bot = "⊥"
texType p vs (TVar b x) = texVar (vs!!x) <> texNeg b
texType p vs (Bang t) = prn p 4 $ "!" <> texType 4 vs t
texType p vs (Quest t) = prn p 4 $ "?" <> texType 4 vs t
texType p vs (Meta b x as) = textual x <> as' <> texNeg b
  where as' = if null as then mempty else  brack (mconcat $ intersperse "," $ map (texType 0 vs) as)

texNeg True = mempty
texNeg False = tex "^" <> braces "⊥"

--------------------------------
-- Pretty printing of closures
{-
texRef (Named x) = text x
texRef (Shift t x) = texRef x <> "+ |" <> texType 0 (repeat "var. in texSizeOf") ty <> "|"
texRef (Next x) = texRef x <> "+1"

texHeapPart :: SymHeap -> Ref -> TeX
texHeapPart h r = case v of
    Nothing -> "..."
    Just c -> texCell c <> texHeapPart h (Next r) <> texHeapPart (Shift r)
  where v = M.lookup r h
        
texHeap h :: SymHeap -> TeX
texHeap = cat $ punctuate ", " $ [text r <> cmd0 "mapsto" <> texHeapPart h (Named r) | Named r <- M.keys h]

-}

