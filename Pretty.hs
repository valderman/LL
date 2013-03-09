{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pretty where

import Text.PrettyPrint.HughesPJ hiding ((<>))
import Data.Monoid
import LL
import Control.Lens


------------------------------------
-- Pretty printing

type Name = String

instance Show (Deriv Name) where
  show (Deriv ts vs s) = render $ (pCtx ts vs <> " ⊢") $$ pSeq ts vs s


prn p k = if p > k then parens else id

pType :: Int -> [String] -> Type String -> Doc
pType p vs (Forall v t) = prn p 0 $ "∀" <> text v <> ". "  <> pType 0 (v:vs) t
pType p vs (Exists v t) = prn p 0 $ "∃" <> text v <> ". "  <> pType 0 (v:vs) t
pType p vs (x :|: y) = prn p 0 $ pType 1 vs x <> " | " <> pType 0 vs y
pType p vs (x :⊕: y) = prn p 1 $ pType 2 vs x <> " ⊕ " <> pType 1 vs y
pType p vs (x :⊗: y) = prn p 2 $ pType 2 vs x <> " ⊗ " <> pType 2 vs y
pType p vs (x :&: y) = prn p 3 $ pType 3 vs x <> " & " <> pType 3 vs y
pType p vs Zero = "0"
pType p vs One = "1"
pType p vs Top = "⊤"
pType p vs Bot = "⊥"
pType p vs (TVar True x) = text $ vs!!x 
pType p vs (TVar False x) = "~" <> text (vs!!x)
pType p vs (Bang t) = prn p 4 $ "!" <> pType 4 vs t
pType p vs (Quest t) = prn p 4 $ "?" <> pType 4 vs t
pType p vs (Meta b s xs) = if_ b ("~" <>) $ text s <> args
  where args = if null xs then mempty else brackets (cat $ punctuate "," $ map (pType 0 vs) xs)

instance Show (Type Name) where
  show x = render $ pType 0 ["v" <> show i | i <- [0..]] x

pSeq :: [Name] -> [(Name,Type Name)] -> Seq Name -> Doc
pSeq ts vs s0 = case s0 of
  Ax _ -> vv 0 <> " ↔ " <> vv 1
  (Cut v vt x s t) -> "connect new " <> text v <> " in {" <> 
                      vcat [pSeq ts ((v,neg vt):v0) s<> ";",
                            pSeq ts ((v,vt):v1) t] <>"}"
    where (v0,v1) = splitAt x vs
  (Cross _ v v' x t) -> "let " <> text v <> "," <> text v' <> " = " <> text w <> " in " $$ 
                      pSeq ts (v0++(v,vt):(v',vt'):v1) t
    where (v0,(w,(vt :⊗: vt')):v1) = splitAt x vs
  (Par _ x s t) -> "connect {"<> vcat [text w <> " : " <>  pType 0 ts vt <> " in " <> pSeq ts (v0++[(w,vt)]) s <> ";",
                                       text w <> " : " <>  pType 0 ts vt' <> " in " <> pSeq ts ((w,vt'):v1) t] <>"}"
    where (v0,(w,(vt :|: vt')):v1) = splitAt x vs
  (Plus x s t) -> "case " <> text w <> " of {" <> 
                  vcat ["inl " <> text w <> " ↦ " <> pSeq ts (v0++(w,vt ):v1) s<> ";", 
                        "inr " <> text w <> " ↦ " <> pSeq ts (v0++(w,vt'):v1) t] <> "}"
    where (v0,(w,(vt :⊕: vt')):v1) = splitAt x vs
  (With b x t) -> "let " <> text w <> " = " <> c <> " " <> text w <> " in " $$ 
                 pSeq ts (v0++(w,wt):v1) t
     where (c,wt) = case b of True -> ("fst",vt); False -> ("snd",vt')
           (v0,(w,(vt :&: vt')):v1) = splitAt x vs
  SBot -> text v 
     where ((v,Bot):_) = vs
  (SZero x) -> "dump " <> pCtx ts (v0 ++ v1) <> " in " <> text w
     where (v0,(w,Zero):v1) = splitAt x vs
  (SOne x t ) -> "let ◇ = " <> text w <> " in " $$ pSeq ts (v0++v1) t
    where (v0,(w,One):v1) = splitAt x vs
  (Exchange p t) -> pSeq ts [vs !! i | i <- p] t        
  (TApp x tyB s) -> "let " <> text w <> " = " <> text w <> "∙" <> pType 0 ts tyB <> " in " $$ pSeq ts (v0++(w,subst0 tyB ∙ tyA):v1) s
    where (v0,(w,Forall _ tyA):v1) = splitAt x vs
  (TUnpack x s) -> "let ⟨" <> text tw <> "," <> text w <> "⟩ = " <> text w <> " in " $$ pSeq (tw:ts) (upd v0++(w,tyA):upd v1) s
    where (v0,(w,Exists tw tyA):v1) = splitAt x vs
          upd = over (mapped._2) (wk ∙) 
  (Offer x s) -> "offer " <> text w <> " : " <> pType 0 ts tyA $$ pSeq ts (v0++(w,tyA):v1) s
    where (v0,(w,Quest tyA):v1) = splitAt x vs
  (Demand _ x s) -> "demand " <> text w <> " : " <> pType 0 ts tyA $$ pSeq ts (v0++(w,tyA):v1) s
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  (Ignore x s) -> "ignore " <> text w <> " : " <> pType 0 ts tyA $$ pSeq ts (v0++v1) s
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  (Alias x w' s) -> "let " <> text w' <> " = alias " <> text w <> " : " <> pType 0 ts tyA $$ pSeq ts ((w,Bang tyA):v0++(w',Bang tyA):v1) s 
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  What a -> braces $ pCtx ts vs
 where vv = vax ts vs
       
vax ts vs x = if x < length vs then let (v,t) = vs!!x in text v <> " : " <> pType 0 ts t
                               else "v" <> int (x-length vs)

instance Show (Seq Name) where
  show = render . pSeq [] []

pCtx :: [String] -> [(String,Type String)] ->  Doc
pCtx ts vs = sep $ punctuate comma $ [text v <> " : " <> (pType 0 ts t) | (v,t) <- vs]
  
----------------------
