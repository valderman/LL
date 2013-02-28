{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TexPretty where


import LL hiding (prn,vax)
import MarXup.Tex
import MarXup.Latex
import MarXup.DerivationTrees
import Data.List
import Data.String

second f (a,b) = (a,f b)

texSeq :: [TeX] -> [(TeX,Type TeX)] -> Seq TeX -> Derivation
texSeq ts vs s0 = case s0 of
  Ax -> rul "Ax" []
  (Cut v vt x s t) -> rul "Cut" [texSeq ts ((v,neg vt):v0) s,texSeq ts ((v,vt):v1) t]
    where (v0,v1) = splitAt x vs
  (Cross v v' x t) -> rul "⊗" [texSeq ts (v0++(v,vt):(v',vt'):v1) t]
    where (v0,(w,(vt :⊗: vt')):v1) = splitAt x vs
  (Par x s t) -> rul "par" [texSeq ts (v0++[(w,neg vt)]) s,texSeq ts ((w,vt'):v1) t]
    where (v0,(w,(vt :⊸: vt')):v1) = splitAt x vs
  (Plus x s t) -> rul "⊕" [texSeq ts (v0++(w,vt ):v1) s,texSeq ts (v0++(w,vt'):v1) t]
    where (v0,(w,(vt :⊕: vt')):v1) = splitAt x vs
  (Amp b x t) -> rul "amp" [texSeq ts (v0++(w,wt):v1) t]
     where (c,wt) = case b of True -> ("fst",vt); False -> ("snd",vt')
           (v0,(w,(vt :&: vt')):v1) = splitAt x vs
  SBot -> rul "⊥" []
     where ((v,Bot):_) = vs
  (SZero x) -> rul "0" []
     where (v0,(w,Zero):v1) = splitAt x vs
  (SOne x t ) -> rul "1" [texSeq ts (v0++v1) t]
    where (v0,(w,One):v1) = splitAt x vs
  (Exchange p t) -> texSeq ts [vs !! i | i <- p] t        
  (TApp x tyB s) -> rul "∀" [texSeq ts (v0++(w,subst0 tyB ∙ tyA):v1) s]
    where (v0,(w,Forall _ tyA):v1) = splitAt x vs
  (TUnpack x s) -> rul "∃" [texSeq (tw:ts) (map (second (wk ∙)) v0++(w,tyA):map (second (wk ∙)) v1) s]
    where (v0,(w,Exists tw tyA):v1) = splitAt x vs
  (Offer x s) -> rul "?" [texSeq ts (v0++(w,tyA):v1) s]
    where (v0,(w,Quest tyA):v1) = splitAt x vs
  (Demand x s) -> rul "!" [texSeq ts (v0++(w,tyA):v1) s]
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  (Ignore x s) -> rul "Weaken" [texSeq ts (v0++v1) s]
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  (Alias x w' s) -> rul "Contract" [texSeq ts (v0++(w,Bang tyA):(w',Bang tyA):v1) s]
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  What -> rul "What?" []
 where vv = vax ts vs
       rul :: String -> [Derivation] -> Derivation
       rul n subs = Node (Rule () Simple mempty (tex n) (texCtx ts vs)) (map (defaultLink ::>) subs)
       
vax ts vs x = if x < length vs then let (v,t) = vs!!x in v <> " : " <> texType 0 ts t
                               else "v" <> tex (show (x-length vs))
     
       
prn p k = if p > k then paren else id
       
texCtx :: [TeX] -> [(TeX,Type TeX)] ->  TeX
texCtx ts vs = mconcat $ intercalate [tex ","] [[v <> " : " <> (texType 0 ts t) | (v,t) <- vs]]
    
texType :: Int -> [TeX] -> Type TeX -> TeX
texType p vs (Forall v t) = prn p 0 $ "∀" <> v <> ". "  <> texType 0 (v:vs) t
texType p vs (Exists v t) = prn p 0 $ "∃" <> v <> ". "  <> texType 0 (v:vs) t
texType p vs (x :⊸: y) = prn p 0 $ texType 1 vs x <> " ⊸ " <> texType 0 vs y
texType p vs (x :⊕: y) = prn p 1 $ texType 2 vs x <> " ⊕ " <> texType 1 vs y
texType p vs (x :⊗: y) = prn p 2 $ texType 2 vs x <> " ⊗ " <> texType 2 vs y
texType p vs (x :&: y) = prn p 3 $ texType 3 vs x <> " & " <> texType 3 vs y
texType p vs Zero = "0"
texType p vs One = "1"
texType p vs Top = "⊤"
texType p vs Bot = "⊥"
texType p vs (TVar True x) = vs!!x 
texType p vs (TVar False x) = "~" <> (vs!!x)
texType p vs (Bang t) = prn p 4 $ "!" <> texType 4 vs t
texType p vs (Quest t) = prn p 4 $ "?" <> texType 4 vs t

       
