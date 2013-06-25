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
import Framework
import Mem

simpleCut :: Deriv
simpleCut = Deriv [] [gamma,xi] $ Cut "x" "y" tA 1 whatA whatB 

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


@section{Graphical Representation}

@subsection{Explain representation}

We introduce the following graphical representation for programs in an environment.

We will represent a program by a node (typically labeled with the program that it represents).
Edges connected to a node will represent its environment. 
The fact that @tA is in the environment of the program can be  
represented either by an ingoing egde labeled with an edge @tA or an outgoing edge labeled 
with @neg(tA). 

Using this convention, we can represent cuts by edges between nodes. For example:
@sequent(simpleCut)
@couplingDiag(simpleCut)

It can be useful to think of a node in such diagrams as a process, 
and an edge as a communication channel; and the label defines which 
communication protocol which is employed on the channel.

We can so represent all outermost cuts in a sequent.
@outermostCut<-definition("Outermost Cut"){
A instance of a cut rule in a derivation tree is called an outermost cut if
it is either the first rule in a derivation or if it is an outermost cut in a subderivation of
an outermost cut.
}

Topologisation of structural rules

This notation relieves much burden from the cut notation, and it may be enlightening 
to review some cut-reduction rules with this view. Here is the multiplicative cut elimination:

@couplingDiag(cutParCross)

@couplingDiag(eval cutParCross)

@subsection{Outermost Evaluation Strategy}
In this section we show that the strategy of evaluation which
considers only outermost cuts is sufficient to evaluate closed 
programs to normal form.

@definition("Outermost Evaluation"){
  Outermost evaluation is evaluation restricted to the outermost cuts.
}


@env("definition"){
We say that a program is @emph{waiting} on a variable in its context if:
@itemize{
 @item it is an axiom, or
 @item if is a non-structural rule acting on that variable, or
 @item if is a contract or weakening rule acting on that variable.
}}

Note that none of the commutation rules are employed by this evaluation strategy.

In general, an edge either represents a cut or an open hypothesis. 
We call a cut ready if all programs connected to it are waiting on the variables connected 
to it. In particular,
an hypothesis edge is ready if the single nodes connected to it is waiting on it.


@lemma(""){In any tree, there is always at least an edge ready, or the system is terminated.
}{
Remark that every node is waiting on at least one port; except for the terminated node
which cannot be connected to any node. The terminated system trivially satisfies the
theorem.

We then proceed by induction on the size of the tree. If the tree has a single node, then
all the edges are hypotheses. It must be waiting on one of them, which is then ready.

For the inductive case, we assume two systems σ and σ' satisfying the induction hypothesis, 
with and hypothesis @math{x} in @math{σ} and an hypothesis @math{x'} in @math{σ'}. 
We show that the system obtained by connecting @math{x} and @math{x'} satisfies the theorem.

We have the following cases:

@enumerate{
@item σ is waiting on @math{x} and @math{σ'} is waiting on @math{x'}. Then the new edge is ready.
@item Either system is not waiting on the designated hypothesis. In this case, some other
   edge in that system must be ready; and it remains ready in the combined system.
}}

@corollary(""){In a closed system, the outermost evaluation strategy is a terminating
reduction strategy}

In sum, the above theorem means that linear logic programs can be run in a way similar
to traditional ways to run the lambda calculus. 
A LL system with an edge ready corresponds to a lambda term with a redex in head position. 
Inner cuts correspond to redexes under lambdas. Non-structural rules correspond to constructors.

We now have exposed a reduction strategy which may satisfy 
those familiar with the lambda calculus. However this evaluation
mechanism has an important shortcoming for the interpretation of
LL as concurrent processes. Namely, if one thinks of a node as a process, 
then every communication is synchronous. Indeed, when a cut-reduction rule
fires, the processes at both ends change state simultaneously.

This goes against at least two commonly admitted principles:
@itemize{
@item On the programming side, processes writing in a channel typically can proceed without waiting 
      for acknowledgement of the reader.
@item On the logic side, it generally admitted that the multiplicative fragment 
      represents connection between processes where no communication occurs.
}

In the next section we proceed to attack this shortcoming.

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

Bi-cut; mix.

@subsection{Related Work}


@xref(intro)

@"