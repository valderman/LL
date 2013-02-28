{-# OPTIONS_GHC -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}

import MarXup.Latex
import MarXup.Tex
import MarXup.DerivationTrees
import Control.Applicative

import TexPretty
import LL

preamble :: Tex ()
preamble = do
  usepackage ["utf8"] "inputenc"
  usepackage [] "graphicx"
  cmd "input" (tex "unicodedefs")
  usepackage [] "amsmath"
  usepackage [] "cmll"

-- cutParCross Deriv ["A","B"] ["Γ"]
cutWithPlus = Deriv ["Γ","Δ","A","B"] [(mempty,var 0), (mempty,var 1)] 
              (Cut "x" (var 2 :⊕: var 3) 1 (Amp True 0 What) (Plus 0 What What))

deriv :: Deriv TeX -> Tex Label
deriv (Deriv tvs vs s) = derivationTree [] $ texSeq tvs vs s

main = render $ latexDocument "article" ["11pt"] preamble $ @"

@deriv(cutWithPlus) --- @deriv(eval(cutWithPlus))

Done.

@"