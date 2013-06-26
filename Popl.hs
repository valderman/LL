{-# OPTIONS_GHC -XTypeFamilies -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}

module Popl where

import Pretty
import MarXup
import MarXup.Latex
import MarXup.Tex
import Control.Applicative
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

doubleCut :: Deriv
doubleCut = Deriv [] [gamma,xi] $ Cut "_x" "x" tA 1 (Cut "_y" "y" tB 1 whatA whatB) (Cut "_z" "z" tB 1 whatC whatD)

vX,vX' :: TeX
vX = math "x"
vX' = math "x'"

graph1, graph2 :: TeX
graph1 = math "σ"
graph2 = math "σ'"

outputTexMp :: String -> IO ()
outputTexMp name = renderToDisk' name $ latexDocument "sigplanconf" ["preprint"] preamble $ @"
@maketitle

@env("abstract"){
Awesome Paper
}
TODO: Keywords
Acm Classification
@intro<-section{Intro}

@section{Syntax}

@subsection{Types}
@subsection{Typing rules (with term assignment)}
@subsection{Note CPS relation}
@subsection{Examples}
@subsection{Reduction rules}


@section{Graphical Representation}

In this section we introduce a graphical representation for programs.

We represent a program by a node, labeled with the program that it represents (typically we
omit this label because it is obvious from the context).
Edges connected to a node represent its environment. 
The fact that @tA is in the environment of the program can be  
represented either by an ingoing egde labeled with an edge @tA or an outgoing edge labeled 
with @neg(tA). 

Using this convention, we can then represent cuts by edges between nodes. For example the sequent
@sequent(simpleCut)
can be represented by the graph
@couplingDiag(simpleCut)
and
@sequent(doubleCut)
can be represented by the graph
@couplingDiag(doubleCut)

It can be useful to think of a node in such diagrams as a process, 
and an edge as a communication channel; and the label defines which 
communication protocol which is employed on the channel.

Because each cut connects two subgraphs by at least one edge, it follows that every graph 
representing a sequent is a tree.

We can represent graphically all outermost cuts in a sequent.
@outermostCut<-definition("Outermost Cut"){
A instance of a cut rule in a derivation tree is called an outermost cut if
it is either the first rule in a derivation or if it is an outermost cut in a subderivation of
an outermost cut.
}

Equivalent sequents (by rule TODO) are represented by the same graph.

This notation relieves much burden from the cut notation, and it may be enlightening 
to review some cut-reduction rules using it. Here is the multiplicative cut reduction:

@couplingDiag(cutParCross)

@couplingDiag(eval cutParCross)

@subsection{Outermost Evaluation Strategy}
In this section we show that the strategy of evaluation which
considers only outermost cuts is sufficient to evaluate closed 
programs to normal form.

@definition("Outermost Evaluation"){
  Outermost evaluation is evaluation restricted to the reduction of outermost cuts.
}

@env("definition"){
We say that a program is @emph{waiting} on a variable @vX in its context if:
@itemize{
 @item it is an axiom, or
 @item if is a contract or weakening rule acting on @vX, or
 @item if is a non-structural rule acting on @id(vX).
}}

None of the commutation rules can ever fire in this evaluation strategy.

We call an edge ready if all programs connected to it are waiting on the variables connected 
to it. In general, an edge either represents a cut or an open hypothesis. 
So in particular, 
an hypothesis edge is ready if the single node  connected to it is  waiting on it,
and a cut     edge is ready if both       nodes connected to it are waiting on it.


@lemma(""){In a graph where all outermost cuts are represented by an edge,
           there is always at least an edge ready, or the system is terminated.
}{
If all outermost cuts are represented, then every node begins either with a rule non-strucutral rule
or Contract or Weakening. Therefore, every node is waiting on at least one variable, except if it
is just the Bot program, which cannot be connected to any node. 


We then proceed by induction on the size of the tree. If the tree has a single node, then
all the edges are hypotheses. It must either be waiting on one of them, which is then ready; or be the
terminated system (Bot).

For the inductive case, we assume two graphs @graph1 and @graph2 satisfying the induction hypothesis, 
with and hypothesis @vX in @graph1 and an hypothesis @vX' in @graph2.
We show that the system obtained by connecting @vX and @vX' satisfies the theorem.

We have the following cases:

@enumerate{
@item @graph1 is waiting on @vX and @graph2 is waiting on @vX'. Then the new edge is ready.
@item Either system is not waiting on the designated hypothesis. In this case, some other
   edge in that system must be ready; and it remains ready in the combined system.
}}

@theorem(""){In a closed system, the outermost evaluation strategy is a terminating
reduction strategy}{
Indeed, a closed system is either the terminated program, or must start with a cut.
By the above lemma, this cut latter case cannot occur in a program evaluated using the
outermost evaluation strategy, which must terminate (any reduction strategy terminates). 
}

In sum, the above theorem means that linear logic programs can be run in a way similar
to traditional ways to run the lambda calculus. 
A ready edge corresponds to a lambda-calculus redex in head position. 
Inner cuts correspond to redexes under lambdas. Non-structural rules correspond to constructors.

The behaviour of Krivine's (TODO SECD) abstract machine is to traverse
a term  inwards and leftwards until it finds a redex, then reduce it.
The reduction yields then another redex in the same position, or one must continue the traversal inwards and leftwards.
An abstract machine for linear logic must traverse the term, potentially considering all outermost
cuts, to eventually find a one which is ready, and reduce it. The difficulty in the
linear case is that one cannot @italic{a priori} know where the ready cut is located. Furthermore,
the next ready cut may not be in the neighborhood, so a working list of potentially ready cuts must 
be maintained.

We have exposed a reduction strategy which may satisfy 
those familiar with the lambda calculus. However this evaluation
mechanism has an important shortcoming for the interpretation of
LL as concurrent processes. Namely, if one thinks of a node as a process, 
then every communication is synchronous. Indeed, when a cut-reduction rule
fires, the processes at both ends change state simultaneously.

This goes against at least two commonly admitted principles:
@itemize{
@item On the programming side, processes writing in a channel 
      typically can proceed without waiting 
      for acknowledgement of the reader.
@item On the logic side, it is generally admitted that the multiplicative fragment 
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

@bibliography

@xref(intro)

@"