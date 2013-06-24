{-# OPTIONS_GHC -XTypeFamilies -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}

module Popl where

import Pretty
import MarXup
import MarXup.Latex
import MarXup.Tex
import MarXup.DerivationTrees
import Control.Applicative
import MarXup.MultiRef
import Reductions
import Data.Monoid
import Symheap
import TexPretty
import LL
import AM
import Rules
import DiagPretty
import Control.Monad
import GraphViz
import Mem

newtheorem :: String -> TeX -> TeX
newtheorem ident text = cmd "newtheorem" (tex ident) >> braces text

preamble :: Tex ()
preamble = do
  usepackage ["utf8"] "inputenc"
  usepackage [] "graphicx" -- used for import metapost diagrams
  usepackage [] "amsmath"
  usepackage [] "amsthm"
  usepackage [] "amssymb" -- extra symbols such as □ 
  usepackage [] "cmll" -- for the operator "par"
  usepackage [] "dot2texi"
  -- usepackage [] "tikz" >> cmd "usetikzlibrary" $ tex "shapes,arrows"
  -- usepackage ["a4paper","margin=2cm"] "geometry"
  newtheorem "theorem" "Theorem"
  newtheorem "corollary" "Corollary"
  newtheorem "lemma" "Lemma"
  newtheorem "definition" "Definition"

  cmd "input" (tex "unicodedefs")
  title "Linear Logic: I see what it means!"
  authorinfo SIGPlan [("Jean-Philippe Bernardy","bernardy@chalmers.se",ch),
                      ("Josef Svenningsson","",ch)]
 where ch = "Chalmers University of Technology and University of Gothenburg"

outputTexMp name = renderToDisk' name $ latexDocument "sigplanconf" ["preprint"] preamble $ @"
@maketitle

@env("abstract"){
Awesome Paper
}

@intro<-section{Intro}

@section{Syntax}

@subsection{Types}
@subsection{Typing rules (with term assignment)}
@subsection{Note CPS relation}
@subsection{Examples}
@subsection{Reduction rules}


@section{Trees}

@subsection{Explain representation}
Topologisation of structural rules
@subsection{Outermost Evaluation Strategy}
@subsection{Cut-elimination result}



@section{Explicit Channels}
@subsection{Translation}
@subsection{Reduction rules}
Multiplicative Fragment

@subsection{``black'' reduction}
@subsection{Completeness}

@section{Abstract Machine}

@subsection{Reduction rules}
Equivalent to outermost reductions in the system with explicit channels.

@section{Discussion}

Efficiency?

@subsection{Related Work}


@xref(intro)

@"