{-# OPTIONS_GHC -XTypeFamilies -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}

module Framework where

import MarXup
import MarXup.Latex
import MarXup.Tex
import MarXup.DerivationTrees
import Control.Applicative
import MarXup.MultiRef
import Reductions
import Symheap
import TexPretty
import LL
import AM
import DiagPretty
import Control.Monad
import GraphViz


----------
-- Text

comment :: Tex a -> TeX
comment x = ""

italic :: Tex a -> Tex a
italic = cmd "textit"

-------------
-- Bib

bibliography :: TeX
bibliography = do
  cmd "bibliographystyle" $ textual "abbrvnat"
  cmd "bibliography" $ textual "jp"

--------------
-- Math

mathpreamble :: TeX
mathpreamble = do
  usepackage "graphicx" [] 
  usepackage "amsmath"  [] 
  usepackage "amsthm"   [] 
  usepackage "amssymb"  []   -- extra symbols such as □ 
  
  newtheorem "theorem" "Theorem"
  newtheorem "corollary" "Corollary"
  newtheorem "lemma" "Lemma"
  newtheorem "definition" "Definition"
  

newtheorem :: String -> TeX -> TeX
newtheorem ident txt = cmd "newtheorem" (tex ident) >> braces txt

deflike :: String -> String -> TeX -> Tex Label
deflike nv name statement = env' nv [name] $ do
  statement
  newLabel
  
thmlike :: String -> String -> TeX -> TeX -> Tex Label
thmlike nv name statement proof = do
  x <- deflike nv name statement
  env "proof" proof
  return x

theorem,lemma ::  String -> TeX -> TeX -> Tex Label
[theorem,lemma] = map thmlike ["theorem","lemma"]


definition,corollary :: String -> TeX -> Tex Label
[definition,corollary] = map deflike ["definition", "corollary"]

-------
-- LL

-- | Render a derivation tree. 
deriv :: Bool -- ^ Show terms?
         -> Deriv -> TeX
deriv showProg (Deriv tvs vs s) = displayMath $ derivationTree $ texSeq showProg tvs vs s

derivation, sequent :: Deriv -> TeX
derivation = deriv True 
sequent = deriv False

-- | Render a derivation as a program (term)
program :: Deriv -> Tex ()
program (Deriv tvs vs s) = indentation (texProg tvs vs s)


-- Element instances

instance Element Type where
  type Target Type = TeX
  element = texClosedType

instance Element Layout where
  type Target Layout = TeX
  element = math . texLayout

--------------------


preamble :: Bool -> Tex ()
preamble inMetaPost = do
  stdPreamble
  usepackage "cmll" [] -- for the operator "par"
  mathpreamble
  cmd "input" (tex "unicodedefs")
  unless inMetaPost $ do
    -- usepackage "dot2texi" []
    usepackage "tikz" []
    cmd "usetikzlibrary" $ tex "shapes,arrows"
    
  title "Linear Logic: I see what it means!"
  authorinfo SIGPlan [("Jean-Philippe Bernardy","bernardy@chalmers.se",ch),
                      ("Josef Svenningsson","",ch)]
 where ch = "Chalmers University of Technology and University of Gothenburg"
