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
import Framework


outputTexMp :: String -> IO ()
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

We introduce the following graphical representation for programs in an environment.

We will represent a program by a node (typically labeled with the program that it represents).
Edges will represent types. The fact that @tA is in the environment of the program can be  
represented either by an ingoing egde labeled with an edge @tA or an outgoing edge labeled 
with @neg(tA). 

Using this convention, we can represent the program @deriv

We introduce a convenient notation for outermost cuts in sequents.

@outermostCut<-definition("Outermost Cut"){
A instance of a cut rule in a derivation tree is called an outermost cut if
it is either the first rule in a derivation or if it is an outermost cut in a subderivation of
an outermost cut.
}

An outermost cut can be moved to the first position in a derivation by using the rule TODO.


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