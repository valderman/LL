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
  scut v _ s t = rul (rulText "Cut") [s,t]
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

texProg :: [String] -> [(String,Type)] -> Seq -> [TeX]
texProg ts vs s0 = case s0 of
  Ax _ -> [vv 0 <> cmd0 "leftrightarrow" <> vv 1]
  (Cut v vt x s t) -> connect mempty --(keyword "directly") 
                               (texVarT' v (neg vt)) (texProg ts ((v,neg vt):v0) s)
                               (texVarT' v (    vt)) (texProg ts ((v,vt):v1) t)
    where (v0,v1) = splitAt x vs
  (Cross _ v v' x t) -> (let_ <> texVar v <> "," <> texVar v' <> " = " <> texVar w <> in_) :
                      texProg ts (v0++(v,vt):(v',vt'):v1) t
    where (v0,(w,(vt :⊗: vt')):v1) = splitAt x vs
  (Par _ x s t) -> connect (keyword "via~" <> texVar w) 
                    (texVarT' w vt ) (texProg ts (v0++[(w,vt)]) s)
                    (texVarT' w vt') (texProg ts ((w,vt'):v1) t)
    where (v0,(w,(vt :|: vt')):v1) = splitAt x vs
  (Plus x s t) -> case_ <> texVar w <> keyword "~of" :
                  alts (keyword "inl~" <> texVar w) (texProg ts (v0++(w,vt ):v1) s)
                       (keyword "inr~" <> texVar w) (texProg ts (v0++(w,vt'):v1) t)
    where (v0,(w,(vt :⊕: vt')):v1) = splitAt x vs
  (With b x t) -> let' (texVar w) wt (c <> texVar w) : texProg ts (v0++(w,wt):v1) t
     where (c,wt) = case b of True -> (fst_,vt); False -> (snd_,vt')
           (v0,(w,(vt :&: vt')):v1) = splitAt x vs
  SBot -> [texVar v] 
     where ((v,Bot):_) = vs
  (SZero x) -> [keyword "dump~" <> texCtx ts (v0 ++ v1) <> in_ <> texVar w]
     where (v0,(w,Zero):v1) = splitAt x vs
  (SOne x t ) -> let'' (cmd0 "diamond") (texVar w) : texProg ts (v0++v1) t
    where (v0,(w,One):v1) = splitAt x vs
  (Exchange p t) -> texProg ts [vs !! i | i <- p] t        
  (TApp x tyB s) -> let'' (texVar w) (texVar w <> cmd0 "bullet" <> texType 0 ts tyB) : texProg ts (v0++(w,ty):v1) s
    where (v0,(w,Forall _ tyA):v1) = splitAt x vs
          ty = subst0 tyB ∙ tyA
  (TUnpack x s) -> let'' (texVar tw <> "," <> texVar w) (texVar w) : texProg (tw:ts) (upd v0++(w,tyA):upd v1) s
    where (v0,(w,Exists tw tyA):v1) = splitAt x vs
          upd = (wk ∙) `for` mapped._2
  (Offer x s) -> (keyword "offer~" <> texVarT w (texType 0 ts tyA)) : texProg ts (v0++(w,tyA):v1) s
    where (v0,(w,Quest tyA):v1) = splitAt x vs
  (Demand _ x s) -> (keyword "demand~" <> texVarT w (texType 0 ts tyA)) : texProg ts (v0++(w,tyA):v1) s
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  (Ignore x s) -> (keyword "ignore~" <> texVarT w (texType 0 ts tyA)) : texProg ts (v0++v1) s
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  (Alias x w' s) -> let' (texVar w') (Bang tyA) (keyword "alias~" <> texVar w) : texProg ts ((w,Bang tyA):v0++(w',Bang tyA):v1) s 
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  What x -> [texVar x]
 where vv = vax ts vs
       let' :: TeX -> Type -> TeX -> TeX
       let'  w ty v = let_ <> w <> ":" <> texType 0 ts ty <> "=" <> v
       let'' w    v = let_ <> w <> "=" <> v
       letNew w ty = let' w ty "new"
       texVarT' v t = texVarT v (texType 0 ts t)

vax ts vs x | x < length vs = texVar v <> " : " <> texType 0 ts t
  where (v,t) = vs!!x 
vax ts vs x | otherwise = "v" <> tex (show (x-length vs))
     
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

