{-# OPTIONS_GHC -XTypeFamilies -XTypeSynonymInstances -XOverloadedStrings #-}

module Framework where

import MarXup
import MarXup.Latex
import MarXup.Tex
import Control.Applicative
import MarXup.MultiRef
import Data.Monoid
import Data.List (intersperse)

return' :: a -> Tex a
return' = return

----------
-- Basics



-- | Environment with options
env'' :: String -> TeX -> Tex a -> Tex a
env'' e opts body = do
  cmd "begin" $ tex e
  brackets opts
  x <- body
  cmd "end" $ tex e
  return x


----------
-- Text
  
specTab x = env "tabularx" $ do 
    braces $ tex "\\textwidth"
    braces $ tex ">{\\centering}X"
    mconcat $ intersperse (newline <> vspace "1ex") x
-- Should replace array
array' :: [String] -> String -> [[TeX]] -> TeX
array' opts format bod = math $ do
  env' "array" opts $ do
    braces $ tex format
    mkrows (map mkcols bod)
  return ()

text = math . cmd "text"

acks = cmd "acks"

kern :: String -> TeX
kern x = braces $ tex $ "\\kern " ++ x

comment :: Tex a -> TeX
comment _ = ""

italic :: Tex a -> Tex a
italic = cmd "textit"

figure_ :: TeX -> TeX -> Tex SortedLabel
figure_ caption body = env "figure*" $ do
  body
  cmd "caption" caption
  label "Fig."

instance Element SortedLabel where
  type Target SortedLabel = TeX
  element x = fxref x >> return ()

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

frac x y = cmdn_ "frac" [x,y]

centerVertically = math . cmd "vcenter" . cmd "hbox"

qedhere = cmd0 "qedhere"

x ^^^ y = braces x <> tex "^" <> braces y

-- Envs

mathpreamble :: TeX
mathpreamble = do
  usepackage "graphicx" [] 
  usepackage "amsmath"  [] 
  usepackage "amsthm"   [] 
  usepackage "amssymb"  []   -- extra symbols such as □ 
  usepackage "stmaryrd" [] -- has ⟦ and ⟧
  usepackage "mathpartir" [] -- mathpar environment
  
  newtheorem "theorem" "Theorem"
  newtheorem "corollary" "Corollary"
  newtheorem "lemma" "Lemma"
  newtheorem "definition" "Definition"
  
mathpar :: [[TeX]] -> TeX
mathpar = env "mathpar" . mkrows . map mk . filter (not . null)
 where mk = foldr1 (\x y -> x <> cmd0 "and" <> y) 

mathbox = mbox . math

newtheorem :: String -> TeX -> TeX
newtheorem ident txt = cmd "newtheorem" (tex ident) >> braces txt

deflike :: String -> String -> TeX -> TeX -> Tex SortedLabel
deflike reference nv name statement = env'' nv name $ do
  statement
  label reference
  
thmlike :: String -> String -> TeX -> TeX -> TeX -> Tex SortedLabel
thmlike reference nv name statement proof = do
  x <- deflike reference nv name statement
  env "proof" proof
  return x

theorem,lemma ::  TeX -> TeX -> TeX -> Tex SortedLabel
theorem = thmlike "Thm." "theorem"
lemma = thmlike "Lem." "lemma"


definition,corollary :: TeX -> TeX -> Tex SortedLabel
definition = deflike "Def." "definition"
corollary = deflike "Cor." "corollary"

-- Other stuff
oxford :: Tex a -> Tex a
oxford = bigParenthesize (textual "⟦") (textual "⟧")

(.=.) :: TeX -> TeX -> TeX
x .=. y = x <> "=" <> y
 
dm = displayMath          

multiline' body = env "multline*" $ mkrows body

space = tex "\\:"

mkIf str = tex "\\newif" <> tex ("\\if" ++ str)

--------------------


