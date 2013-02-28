module Pretty where

------------------------------------
-- Pretty printing

type Name = Doc

instance Show (Deriv Name) where
  show (Deriv ts vs s) = render $ (pCtx ts vs <> " ⊢") $$ pSeq ts vs s


prn p k = if p > k then parens else id

pType :: Int -> [Name] -> Type Name -> Doc
pType p vs (Forall v t) = prn p 0 $ "∀" <> v <> ". "  <> pType 0 (v:vs) t
pType p vs (Exists v t) = prn p 0 $ "∃" <> v <> ". "  <> pType 0 (v:vs) t
pType p vs (x :⊸: y) = prn p 0 $ pType 1 vs x <> " ⊸ " <> pType 0 vs y
pType p vs (x :⊕: y) = prn p 1 $ pType 2 vs x <> " ⊕ " <> pType 1 vs y
pType p vs (x :⊗: y) = prn p 2 $ pType 2 vs x <> " ⊗ " <> pType 2 vs y
pType p vs (x :&: y) = prn p 3 $ pType 3 vs x <> " & " <> pType 3 vs y
pType p vs Zero = "0"
pType p vs One = "1"
pType p vs Top = "⊤"
pType p vs Bot = "⊥"
pType p vs (TVar True x) = vs!!x 
pType p vs (TVar False x) = "~" <> (vs!!x)
pType p vs (Bang t) = prn p 4 $ "!" <> pType 4 vs t
pType p vs (Quest t) = prn p 4 $ "?" <> pType 4 vs t

instance Show (Type Name) where
  show x = render $ pType 0 ["v" <> int i | i <- [0..]] x

pSeq :: [Name] -> [(Name,Type Name)] -> Seq Doc -> Doc
pSeq ts vs s0 = case s0 of
  Ax -> vv 0 <> " ↔ " <> vv 1
  (Cut v vt x s t) -> "connect new " <> v <> " in {" <> 
                      vcat [pSeq ts ((v,neg vt):v0) s<> ";",
                            pSeq ts ((v,vt):v1) t] <>"}"
    where (v0,v1) = splitAt x vs
  (Cross v v' x t) -> "let " <> v <> "," <> v' <> " = " <> w <> " in " $$ 
                      pSeq ts (v0++(v,vt):(v',vt'):v1) t
    where (v0,(w,(vt :⊗: vt')):v1) = splitAt x vs
  (Par x s t) -> "connect {"<> vcat [w <> " : " <>  pType 0 ts (neg vt) <> " in " <> pSeq ts (v0++[(w,neg vt)]) s <> ";",
                                     w <> " : " <>  pType 0 ts      vt' <> " in " <> pSeq ts ((w,vt'):v1) t] <>"}"
    where (v0,(w,(vt :⊸: vt')):v1) = splitAt x vs
  (Plus x s t) -> "case " <> w <> " of {" <> 
                  vcat ["inl " <> w <> " ↦ " <> pSeq ts (v0++(w,vt ):v1) s<> ";", 
                        "inr " <> w <> " ↦ " <> pSeq ts (v0++(w,vt'):v1) t] <> "}"
    where (v0,(w,(vt :⊕: vt')):v1) = splitAt x vs
  (Amp b x t) -> "let " <> w <> " = " <> c <> " " <> w <> " in " $$ 
                 pSeq ts (v0++(w,wt):v1) t
     where (c,wt) = case b of True -> ("fst",vt); False -> ("snd",vt')
           (v0,(w,(vt :&: vt')):v1) = splitAt x vs
  SBot -> v 
     where ((v,Bot):_) = vs
  (SZero x) -> "dump " <> pCtx ts (v0 ++ v1) <> " in " <> w
     where (v0,(w,Zero):v1) = splitAt x vs
  (SOne x t ) -> "let ◇ = " <> w <> " in " $$ pSeq ts (v0++v1) t
    where (v0,(w,One):v1) = splitAt x vs
  (Exchange p t) -> pSeq ts [vs !! i | i <- p] t        
  (TApp x tyB s) -> "let " <> w <> " = " <> w <> "∙" <> pType 0 ts tyB <> " in " $$ pSeq ts (v0++(w,subst0 tyB ∙ tyA):v1) s
    where (v0,(w,Forall _ tyA):v1) = splitAt x vs
  (TUnpack x s) -> "let ⟨" <> tw <> "," <> w <> "⟩ = " <> w <> " in " $$ pSeq (tw:ts) ((wk ∙ v0)++(w,tyA):(wk ∙ v1)) s
    where (v0,(w,Exists tw tyA):v1) = splitAt x vs
  (Offer x s) -> "offer " <> w <> " : " <> pType 0 ts tyA $$ pSeq ts (v0++(w,tyA):v1) s
    where (v0,(w,Quest tyA):v1) = splitAt x vs
  (Demand x s) -> "demand " <> w <> " : " <> pType 0 ts tyA $$ pSeq ts (v0++(w,tyA):v1) s
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  (Ignore x s) -> "ignore " <> w <> " : " <> pType 0 ts tyA $$ pSeq ts (v0++v1) s
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  (Alias x w' s) -> "let " <> w' <> " = alias " <> w <> " : " <> pType 0 ts tyA $$ pSeq ts (v0++(w,Bang tyA):(w',Bang tyA):v1) s
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  What -> braces $ pCtx ts vs
 where vv = vax ts vs
       
vax ts vs x = if x < length vs then let (v,t) = vs!!x in v <> " : " <> pType 0 ts t
                               else "v" <> int (x-length vs)

instance Show (Seq Doc) where
  show = render . pSeq [] []

pCtx :: [Name] -> [(Name,Type Name)] ->  Doc
pCtx ts vs = sep $ punctuate comma $ [v <> " : " <> (pType 0 ts t) | (v,t) <- vs]
  
----------------------
