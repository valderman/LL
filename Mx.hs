{-# OPTIONS_GHC -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}

import MarXup.Latex
import MarXup.Tex
import MarXup.DerivationTrees
import Control.Applicative
import MarXup.MultiRef
import Reductions

import TexPretty
import LL

preamble :: Tex ()
preamble = do
  usepackage ["utf8"] "inputenc"
  usepackage [] "graphicx"
  cmd "input" (tex "unicodedefs")
  usepackage [] "amsmath"
  usepackage [] "cmll" -- for the operator "par"

deriv :: Deriv -> Tex Label
deriv (Deriv tvs vs s) = derivationTree [] $ texSeq False tvs vs s

program :: Deriv -> Tex ()
program (Deriv tvs vs s) = math (block (texProg tvs vs s))

allReductions displayer = mapM_ redRule 
   [(amp,cutWithPlus),
    (math par,cutParCross),
    ("!?", cutBang),
    ("1⊥",cutUnit),
    ("∀∃",cutQuant),
    ("Contract",cutContract),
    ("Weaken",cutIgnore)]

  where redRule (name,input) = do
          name
          displayMath $ do 
            displayer input
            cmd0 "Longrightarrow"
            displayer (eval input)
          newline

todo = cmd "marginpar"

main = render $ latexDocument "article" ["11pt"] preamble $ @"

@todo{Linear logic as a low-level logic}

@todo{propositions as types, proofs as programs}

We won't dwell on the general benefits of the parallel: this has been done countless times before, 
remarkably by Girard in a paragraph which starts with the following provocative sentence.

@env("quote"){
There are still people saying that, in order to make computer
science, one essentially needs a soldering iron; this opinion is
shared by logicians who despise computer science and by engineers
who despise theoreticians.
}
Very much in the spirit of this conference, this paper aims to build
another bridge between logicians and engineers. 


We provide an interpretation
of linear logic as an programming language with ISWIM syntax, together with an
abstract machine able to run programs written for it.

@todo{π-calculus as a low-level programming language: not quite. We fill the niche}

@section{Cut-elimination rules}
@allReductions(deriv)

@section{Reduction rules}
@allReductions(program)
@"



