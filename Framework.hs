{-# OPTIONS_GHC -XTypeFamilies -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}

module Framework where

import MarXup
import MarXup.Latex
import MarXup.Tex
import MarXup.DerivationTrees hiding (label)
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
import Data.Monoid


----------
-- Text

comment :: Tex a -> TeX
comment _ = ""

italic :: Tex a -> Tex a
italic = cmd "textit"

-------------
-- Bib

bibliography :: TeX
bibliography = do
  cmd "bibliographystyle" $ textual "abbrvnat"
  cmd "bibliography" $ textual "jp"

citet,citep :: TeX -> TeX
[citet,citep] = map cmd ["citet","cite"]

--------------
-- Math 

-- Envs

mathpreamble :: TeX
mathpreamble = do
  usepackage "graphicx" [] 
  usepackage "amsmath"  [] 
  usepackage "amsthm"   [] 
  usepackage "amssymb"  []   -- extra symbols such as □ 
  usepackage "stmaryrd" [] -- has ⟦ and ⟧
  
  newtheorem "theorem" "Theorem"
  newtheorem "corollary" "Corollary"
  newtheorem "lemma" "Lemma"
  newtheorem "definition" "Definition"
  

newtheorem :: String -> TeX -> TeX
newtheorem ident txt = cmd "newtheorem" (tex ident) >> braces txt

deflike :: String -> String -> TeX -> Tex SortedLabel
deflike nv name statement = env' nv [name] $ do
  statement
  label nv 
  
thmlike :: String -> String -> TeX -> TeX -> Tex SortedLabel
thmlike nv name statement proof = do
  x <- deflike nv name statement
  env "proof" proof
  return x

theorem,lemma ::  String -> TeX -> TeX -> Tex SortedLabel
[theorem,lemma] = map thmlike ["theorem","lemma"]


definition,corollary :: String -> TeX -> Tex SortedLabel
[definition,corollary] = map deflike ["definition", "corollary"]

-- Other stuff
oxford :: Tex a -> Tex a
oxford = bigParenthesize (textual "⟦") (textual "⟧")

(.=.) :: TeX -> TeX -> TeX
x .=. y = x <> "=" <> y
 
dm = displayMath          

multiline' body = env "multline*" $ mkrows body

figure_ :: TeX -> TeX -> Tex SortedLabel
figure_ caption body = env "figure*" $ do
  body
  cmd "caption" caption
  label "Fig."

-------
-- LL

-- | Render a derivation tree. 
deriv :: Bool -- ^ Show terms?
         -> Deriv -> TeX
deriv showProg (Deriv tvs vs s) = derivationTree $ texSeq showProg tvs vs s

derivation, sequent :: Deriv -> TeX
derivation = deriv True 
sequent = deriv False

-- | Render a derivation tree, showing terms.
deriv' = deriv True

deriv'' (x,_) = deriv' x

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

