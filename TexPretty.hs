{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}

module TexPretty where

import Control.Lens
import LL hiding (prn,vax)
import MarXup.Tex
import MarXup.Latex
import MarXup.DerivationTrees
import Data.List
import Data.String

infix 1 `for`
x `for` lens = over lens x

par = cmd0 "parr"
amp = cmd "hspace" "1pt" <> cmd0 "&"  <> cmd "hspace" "1pt" 

smallcaps :: TeX -> TeX
smallcaps x = braces (cmd0 "sc" <> x)

isEmpty :: TeX -> Bool
isEmpty (Tex x) = null x 
isEmpty _ = False

rulText = cmd "text" . smallcaps 

texSeq :: Bool -> [TeX] -> [(TeX,Type TeX)] -> Seq TeX -> Derivation
texSeq showProg ts vs s0 = case s0 of
  Ax -> rul "Ax" []
  (Cut v vt x s t) -> rul (rulText "Cut") [fun ts ((v,neg vt):v0) s,fun ts ((v,vt):v1) t]
    where (v0,v1) = splitAt x vs
  (Cross v v' x t) -> rul "⊗" [fun ts (v0++(v,vt):(v',vt'):v1) t]
    where (v0,(w,(vt :⊗: vt')):v1) = splitAt x vs
  (Par x s t) -> rul par [fun ts (v0++[(w,vt)]) s,fun ts ((w,vt'):v1) t]
    where (v0,(w,(vt :|: vt')):v1) = splitAt x vs
  (Plus x s t) -> rul "⊕" [fun ts (v0++(w,vt ):v1) s,fun ts (v0++(w,vt'):v1) t]
    where (v0,(w,(vt :⊕: vt')):v1) = splitAt x vs
  (With b x t) -> rul (amp<>tex"_"<>if b then "1" else "2") [fun ts (v0++(w,wt):v1) t]
     where (c,wt) = case b of True -> ("fst",vt); False -> ("snd",vt')
           (v0,(w,(vt :&: vt')):v1) = splitAt x vs
  SBot -> rul "⊥" []
     where ((v,Bot):_) = vs
  (SZero x) -> rul "0" []
     where (v0,(w,Zero):v1) = splitAt x vs
  (SOne x t ) -> rul "1" [fun ts (v0++v1) t]
    where (v0,(w,One):v1) = splitAt x vs
  (Exchange p t) -> rul (rulText "Exch.") [fun ts [vs !! i | i <- p] t]
  (TApp x tyB s) -> rul "∀" [fun ts (v0++(w,subst0 tyB ∙ tyA):v1) s]
    where (v0,(w,Forall _ tyA):v1) = splitAt x vs
  (TUnpack x s) -> rul "∃" [fun (tw:ts) (upd v0++(w,tyA):upd v1) s]
    where (v0,(w,Exists tw tyA):v1) = splitAt x vs
          upd = (wk ∙) `for` mapped._2
  (Offer x s) -> rul "?" [fun ts (v0++(w,tyA):v1) s]
    where (v0,(w,Quest tyA):v1) = splitAt x vs
  (Demand x s) -> rul "!" [fun ts (v0++(w,tyA):v1) s]
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  (Ignore x s) -> rul (rulText "Weaken") [fun ts (v0++v1) s]
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  (Alias x w' s) -> rul (rulText "Contract") [fun ts ((w',Bang tyA):v0++(w,Bang tyA):v1) s]
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  What x -> Node (Rule () None mempty mempty (texCtx ts vs <> "⊢" <> x))  []
 where vv = vax ts vs
       rul :: TeX -> [Derivation] -> Derivation
       rul n subs = Node (Rule () Simple mempty n (texCtx ts vs <> "⊢" <> maybeProg)) (map (defaultLink ::>) subs)
       fun = texSeq showProg
       maybeProg = if showProg then block (texProg ts vs s0) else mempty


keyword = mathsf
let_ = keyword "let~"
case_ = keyword "case~"
new_ = keyword "new"
in_ = keyword "~in~"
[fst_,snd_] = map keyword ["fst~","snd~"]
connect_ = keyword "connect~"
separator = cmd "hline" mempty

hang x ys = env' "array" ["t"] (mkCols [x,ys])

blocks :: [[TeX]] -> TeX
blocks [] = mempty
blocks (x:xs) = block (block x: map (\y -> separator <> block y) xs)

bblock = bigBraces . blocks

mapsto :: [TeX] -> TeX
mapsto xs = cmd "mapsto" (block xs)
left_ = cmd "Leftarrow" mempty
right_ = cmd "Rightarrow" mempty

indent = cmd "hspace" $ tex "1em"

texProg :: [TeX] -> [(TeX,Type TeX)] -> Seq TeX -> [TeX]
texProg ts vs s0 = case s0 of
  Ax -> [vv 0 <> cmd0 "leftrightarrow" <> vv 1]
  (Cut v vt x s t) -> [connect_ <>  bblock  
                               [letNew v (neg vt) : texProg ts ((v,neg vt):v0) s,                               
                                letNew v (    vt) : texProg ts ((v,vt):v1) t]]
    where (v0,v1) = splitAt x vs
  (Cross v v' x t) -> (let_ <> v <> "," <> v' <> " = " <> w <> in_) :
                      texProg ts (v0++(v,vt):(v',vt'):v1) t
    where (v0,(w,(vt :⊗: vt')):v1) = splitAt x vs
  (Par x s t) -> [connect_ <> w <> bblock 
                   [let' w vt  left_  : texProg ts (v0++[(w,vt)]) s,
                    let' w vt' right_ : texProg ts ((w,vt'):v1) t]]
    where (v0,(w,(vt :|: vt')):v1) = splitAt x vs
  (Plus x s t) -> [block [case_ <> w <> keyword "~of",
                          indent <> keyword "inl~" <> w <> mapsto (texProg ts (v0++(w,vt ):v1) s), 
                          indent <> keyword "inr~" <> w <> mapsto (texProg ts (v0++(w,vt'):v1) t)]]
    where (v0,(w,(vt :⊕: vt')):v1) = splitAt x vs
  (With b x t) -> let' w wt (c <> w) : texProg ts (v0++(w,wt):v1) t
     where (c,wt) = case b of True -> (fst_,vt); False -> (snd_,vt')
           (v0,(w,(vt :&: vt')):v1) = splitAt x vs
  SBot -> [v] 
     where ((v,Bot):_) = vs
  (SZero x) -> [keyword "dump~" <> texCtx ts (v0 ++ v1) <> in_ <> w]
     where (v0,(w,Zero):v1) = splitAt x vs
  (SOne x t ) -> let'' (cmd0 "diamond") w : texProg ts (v0++v1) t
    where (v0,(w,One):v1) = splitAt x vs
  (Exchange p t) -> texProg ts [vs !! i | i <- p] t        
  (TApp x tyB s) -> let'' w (w <> cmd0 "bullet" <> texType 0 ts tyB) : texProg ts (v0++(w,ty):v1) s
    where (v0,(w,Forall _ tyA):v1) = splitAt x vs
          ty = subst0 tyB ∙ tyA
  (TUnpack x s) -> let'' (tw <> "," <> w) w : texProg (tw:ts) (upd v0++(w,tyA):upd v1) s
    where (v0,(w,Exists tw tyA):v1) = splitAt x vs
          upd = (wk ∙) `for` mapped._2
  (Offer x s) -> (keyword "offer~" <> w <> " : " <> texType 0 ts tyA) : texProg ts (v0++(w,tyA):v1) s
    where (v0,(w,Quest tyA):v1) = splitAt x vs
  (Demand x s) -> (keyword "demand~" <> w <> " : " <> texType 0 ts tyA) : texProg ts (v0++(w,tyA):v1) s
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  (Ignore x s) -> (keyword "ignore~" <> w <> " : " <> texType 0 ts tyA) : texProg ts (v0++v1) s
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  (Alias x w' s) -> let' w' (Bang tyA) (keyword "alias~" <> w) : texProg ts ((w,Bang tyA):v0++(w',Bang tyA):v1) s 
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  What x -> [x]
 where vv = vax ts vs
       let' :: TeX -> Type TeX -> TeX -> TeX
       let' w ty v = let_ <> w <> ":" <> texType 0 ts ty <> "=" <> v
       let'' w v = let_ <> w <> "=" <> v
       letNew w ty = let' w ty new_ 
       

vax ts vs x | x < length vs = v <> " : " <> texType 0 ts t
  where (v,t) = vs!!x 
vax ts vs x | otherwise = "v" <> tex (show (x-length vs))
     
texVar (Tex []) t = t
texVar v t = v <> ":" <> t
       
prn p k = if p > k then paren else id
       
texCtx :: [TeX] -> [(TeX,Type TeX)] ->  TeX
texCtx ts vs = mconcat $ intersperse (text ",") [texVar v (texType 0 ts t) | (v,t) <- vs]
    
texType :: Int -> [TeX] -> Type TeX -> TeX
texType p vs (Forall v t) = prn p 0 $ "∀" <> v <> ". "  <> texType 0 (v:vs) t
texType p vs (Exists v t) = prn p 0 $ "∃" <> v <> ". "  <> texType 0 (v:vs) t
texType p vs (x :|: y) = prn p 0 $ texType 1 vs x <> par <> texType 0 vs y
texType p vs (x :⊕: y) = prn p 1 $ texType 2 vs x <> " ⊕ " <> texType 1 vs y
texType p vs (x :⊗: y) = prn p 2 $ texType 2 vs x <> " ⊗ " <> texType 2 vs y
texType p vs (x :&: y) = prn p 3 $ texType 3 vs x <> amp <> texType 3 vs y
texType p vs Zero = "0"
texType p vs One = "1"
texType p vs Top = "⊤"
texType p vs Bot = "⊥"
texType p vs (TVar True x) = vs!!x 
texType p vs (TVar False x) = (vs!!x) <> tex "^" <> braces "⊥"
texType p vs (Bang t) = prn p 4 $ "!" <> texType 4 vs t
texType p vs (Quest t) = prn p 4 $ "?" <> texType 4 vs t

-- texType p vs (MetaVar x) = vs!!x 
-- texType p vs (Subst t f) = texType 4 vs t <> texSubst vs f
-- 
-- texSubst :: [TeX] -> Subst TeX -> TeX
-- texSubst vs s = mconcat $ intersperse "," $ [(s!!t) <> "/" <> (s!!i) | (i,t) <- zip [0..] vs]
       
