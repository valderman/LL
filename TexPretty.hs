{-# LANGUAGE OverloadedStrings #-}
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

texSeq :: Bool -> [String] -> [(String,Type String)] -> Seq String -> Derivation
texSeq showProg ts vs s0 = case s0 of
  Ax _ -> rul "Ax" []
  (Cut v vt x s t) -> rul (rulText "Cut") [fun ts ((v,neg vt):v0) s,fun ts ((v,vt):v1) t]
    where (v0,v1) = splitAt x vs
  (Cross _ v v' x t) -> rul "⊗" [fun ts (v0++(v,vt):(v',vt'):v1) t]
    where (v0,(w,(vt :⊗: vt')):v1) = splitAt x vs
  (Par _ x s t) -> rul par [fun ts (v0++[(w,vt)]) s,fun ts ((w,vt'):v1) t]
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
  (Demand _ x s) -> rul "!" [fun ts (v0++(w,tyA):v1) s]
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  (Ignore x s) -> rul (rulText "Weaken") [fun ts (v0++v1) s]
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  (Alias x w' s) -> rul (rulText "Contract") [fun ts ((w',Bang tyA):v0++(w,Bang tyA):v1) s]
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  What x -> Node (Rule () None mempty mempty (texCtx ts vs <> "⊢" <> texVar x))  []
 where vv = vax ts vs
       rul :: TeX -> [Derivation] -> Derivation
       rul n subs = Node (Rule () Simple mempty n (texCtx ts vs <> "⊢" <> maybeProg)) (map (defaultLink ::>) subs)
       fun = texSeq showProg
       maybeProg = if showProg then block (texProg ts vs s0) else mempty


keyword :: String -> TeX 
keyword = mathsf . tex
let_ = keyword "let~"
case_ = keyword "case~"
in_ = keyword "~in~"
connect_ = keyword "connect~"
[fst_,snd_] = map keyword ["fst~","snd~"]
separator = cmd "hline" mempty

blocks :: [[TeX]] -> TeX
blocks [] = mempty
blocks (x:xs) = block (block x: map (\y -> separator <> block y) xs)

bblock = bigBraces . blocks

xblock :: [TeX] -> TeX
xblock  bod = do
  env' "array" ["t"] $ do
    braces (tex "l") 
    mkrows $ bod
  return ()

mapsto :: [TeX] -> TeX
mapsto xs = cmd "mapsto" (xblock xs)
-- left_ = cmd "Leftarrow" mempty
-- right_ = cmd "Rightarrow" mempty

indent = cmd "hspace" $ tex "1em"

connect z x a y b = 
  [xblock [connect_ <> z,
          indent <> x <> mapsto a,
          indent <> y <> mapsto b]]

texProg :: [String] -> [(String,Type String)] -> Seq String -> [TeX]
texProg ts vs s0 = case s0 of
  Ax _ -> [vv 0 <> cmd0 "leftrightarrow" <> vv 1]
  (Cut v vt x s t) -> connect (keyword "directly") 
                               (texVarT' v (neg vt)) (texProg ts ((v,neg vt):v0) s)
                               (texVarT' v (    vt)) (texProg ts ((v,vt):v1) t)
    where (v0,v1) = splitAt x vs
  (Cross _ v v' x t) -> (let_ <> texVar v <> "," <> texVar v' <> " = " <> texVar w <> in_) :
                      texProg ts (v0++(v,vt):(v',vt'):v1) t
    where (v0,(w,(vt :⊗: vt')):v1) = splitAt x vs
  (Par _ x s t) -> connect (texVar w) 
                    (texVarT' w vt ) (texProg ts (v0++[(w,vt)]) s)
                    (texVarT' w vt') (texProg ts ((w,vt'):v1) t)
    where (v0,(w,(vt :|: vt')):v1) = splitAt x vs
  (Plus x s t) -> [xblock [case_ <> texVar w <> keyword "~of",
                          indent <> keyword "inl~" <> texVar w <> mapsto (texProg ts (v0++(w,vt ):v1) s), 
                          indent <> keyword "inr~" <> texVar w <> mapsto (texProg ts (v0++(w,vt'):v1) t)]]
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
       let' :: TeX -> Type String -> TeX -> TeX
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
texVar = text
             
prn p k = if p > k then paren else id
       
texCtx :: [String] -> [(String,Type String)] ->  TeX
texCtx ts vs = mconcat $ intersperse (text ",") [texVarT v (texType 0 ts t) | (v,t) <- vs]
    
texType :: Int -> [String] -> Type String -> TeX
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
texType p vs (TVar True x) = texVar $ vs!!x 
texType p vs (TVar False x) = (texVar $ vs!!x) <> tex "^" <> braces "⊥"
texType p vs (Bang t) = prn p 4 $ "!" <> texType 4 vs t
texType p vs (Quest t) = prn p 4 $ "?" <> texType 4 vs t

-- texType p vs (MetaVar x) = vs!!x 
-- texType p vs (Subst t f) = texType 4 vs t <> texSubst vs f
-- 
-- texSubst :: [String] -> Subst String -> TeX
-- texSubst vs s = mconcat $ intersperse "," $ [(s!!t) <> "/" <> (s!!i) | (i,t) <- zip [0..] vs]
       
