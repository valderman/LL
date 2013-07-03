{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pretty where

import Text.PrettyPrint.HughesPJ hiding ((<>))
import Data.Monoid
import LL
import AM
import Symheap
import qualified Data.Map as M
import Data.List (intercalate)

------------------------------------
-- Pretty printing of sequents



deriving instance Show Seq
deriving instance Show Deriv
deriving instance Show Type

{-
instance Show Type where
  show x = render $ pType 0 ["v" <> show i | i <- [0..]] x

instance Show Seq where
   show = render . pSeq [] []

instance Show (Deriv) where show (Deriv ts vs s) = render $ (pCtx ts vs  <> " ⊢") $$ pSeq True ts vs s
-}
prn p k = if p > k then parens else id

pType :: Int -> [String] -> Type -> Doc
pType p vs (Forall v t) = prn p 0 $ "∀" <> text v <> ". "  <> pType 0 (v:vs) t
pType p vs (Exists v t) = prn p 0 $ "∃" <> text v <> ". "  <> pType 0 (v:vs) t
pType p vs (x :|: y) = prn p 0 $ pType 1 vs x <> " ⅋ " <> pType 0 vs y
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


pSeq :: Bool -> [Name] -> [(Name,Type)] -> Seq -> Doc
pSeq showTypes = foldSeq sf where
   sf (Deriv ts vs _) = SeqFinal {..} where
        sty = pType 0
        sax v v' _ = text v <> " ↔ " <> text v'
        scut v v' vt s vt' t = "connect " <>
                                      "{"<> vcat [varT v  vt  <> " in " <> s <> ";",
                                                  varT v' vt' <> " in " <> t] <>"}"
        scross _ w v vt v' vt' t = "let " <> text v <> "," <> text v' <> " = " <> text w <> " in " $$ t
        spar _ w v vt v' vt' s t = "connect "<>text w <>
                                      "{"<> vcat [varT v  vt  <> " in " <> s <> ";",
                                                  varT v' vt' <> " in " <> t] <>"}"
        splus w v vt v' vt' s t = "case " <> text w <> " of {" <> 
                                vcat ["inl " <> text v <> " ↦ " <> s<> ";", 
                                      "inr " <> text v' <> " ↦ " <> t]<> "}"
        swith _ _ b w v' _ t = "let " <> text v' <> " = " <> c <> " " <> text w <> " in " $$ t
           where c = if b then "fst" else "snd"
        sbot v = text v 
        szero w vs = "dump " <> whenShowTypes (pCtx' vs) <> " in " <> text w
        sone _ w t = "let ◇ = " <> text w <> " in " $$ t
        sxchg _ t = t
        stapp _ v _ w tyB s = "let " <> text v <> " = " <> text w <> "∙" <> tyB <> " in " $$ s
        stunpack tw w v s = "let ⟨" <> whenShowTypes (text tw) <> "," <> text v <> "⟩ = " <> text w <> " in " $$ s
        soffer _ v w ty s = "offer " <> varT v ty $$ s
        sdemand v w ty s = "let" <+> text w <+> "=" <+> text "demand" <+> varT v ty <+> "in" $$ s
        signore w ty s = "ignore " <> text w $$ s
        salias _ w w' ty s = "let" <+> text w' <+> equals <+> "alias" <+> varT w ty <+> "in" $$ s
        swhat a _ _ = braces $ pCtx ts vs
        smem ty t u = "serve " <> ty <> " " <> vcat ["server" <> u
                                                    ,"clients" <> t
                                                    ]
                        
   varT x y | showTypes = text x <> " : " <> y
            | otherwise = text x                            
   whenShowTypes | showTypes = id                            
                 | otherwise = const "?"
       
pCtx ts vs = pCtx' (fmap (second (pType 0 ts)) vs)

pCtx' :: [(Name,Doc)] ->  Doc
pCtx' vs = sep $ punctuate comma $ [text v <> " : " <> t | (v,t) <- vs]

-----------

pClosedType = pType 0 (repeat "<VAR>")

pLayout :: Layout -> Doc
pLayout (a `Then`b) = pLayout a <> "+" <> pLayout b
pLayout (Bit a) = pLayout a <> "+1" 
pLayout (Pointer _) = "1"
pLayout (MetaL t) = "|" <> pClosedType t <> "|"
pLayout (Union a b) = pLayout a <> "⊔" <> pLayout b
pLayout Empty = "0"


pRef (Named t x) = text x
pRef (Shift t x) = pRef x <> "+ |" <> pLayout t <> "|"
pRef (Next x) = pRef x <> "+1"

pHeapPart :: SymHeap -> SymRef -> Doc
pHeapPart h r = case v of
    Nothing -> "..."
    Just c -> pCell c <> pHeapPart h (Next r) <> pHeapPart h (Shift (error "yada") r)
  where v = M.lookup r h
        
pCell :: Cell SymRef -> Doc
pCell c = case c of
      New -> "[ ]"
      Freed -> "Freed"
      Tag True -> "1"
      Tag False -> "0"
      _ -> "?"

pHeap :: SymHeap -> Doc
pHeap h = cat $ punctuate ", " [text r <> "↦" <> pHeapPart h (Named t r) | Named t r <- M.keys h]

showSystem :: System SymHeap -> String
showSystem = render . pSystem

pSystem :: System SymHeap -> Doc
pSystem (cls,h) = hang "Heap:" 2 (pHeap h) $$
                  hang "Closures:" 2 (vcat $ map pClosure cls)

pClosure :: Closure SymRef -> Doc
pClosure (seq,env,typeEnv) = 
         hang "Closure:" 2 (vcat [
            hang "Code:" 2 (pSeq False (zipWith const typeNames typeEnv) (mkEnvDummy env) seq),
            hang "Env: " 2 (cat $ punctuate ", " [text nm <> " = " <> pRef r | (nm,r) <- env]),
            hang "TypeEnv:" 2 (cat $ punctuate ", " $ map pClosedType typeEnv)])

sshow = putStrLn . render . pSystem  

-- TODO: Dummy definitions to be able to print the code. Needs a proper solution
typeNames = ["α","β","γ","δ","ε","ζ","η","ι","κ","λ","μ"]
mkEnvDummy = map (\(n,_) -> (n,dum))
