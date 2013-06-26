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
import Data.List

simpleCut :: Deriv
simpleCut = Deriv [] [gamma,xi] $ Cut "x" "y" tA 1 whatA whatB 

doubleCut :: Deriv
doubleCut = Deriv [] [gamma,xi] $ Cut "_x" "x" tB 1 (Cut "_y" "y" (neg tA) 1 whatA whatB) (Cut "_z" "z" tC 1 whatC whatD)

doubleCut' :: Deriv
doubleCut' = Deriv [] [gamma,delta] $ Cut "_x" "x" (neg tA) 0 whatA $ Cut "_y" "y" (neg tC) 1 (Cut "_z" "z" (neg tB) 0 whatB whatD) whatC 
vX,vX' :: TeX
vX = math "x"
vX' = math "x'"

graph1, graph2 :: TeX
graph1 = math "σ"
graph2 = math "σ'"

acmCategories,acmKeywords :: TeX
acmCategories = do
  cmdn_ "category" ["F.4.1","Mathematical Logic","Lambda calculus and related system"]
  cmdn_ "category" ["D.3.3","Language Constructs and Features","Concurrent programming structures"]
  
acmKeywords = do  
  cmd0 "keywords"
  mconcat $ intersperse "," ["linear logic","lambda calculus"]

abstract = env "abstract" $ @"
Awesome Paper
@"

header = do
  maketitle
  abstract
  acmCategories
  acmKeywords

cut_,with_,plus_ :: TeX
cut_ = ruleName "Cut"
with_ = "&"
plus_ = "⊕"

chanRules :: [(Deriv,TeX)]               
chanRules =   
  [(channelRule,       "A channel containing no data")
  ,(chanPlusRule True, "A channel containing a bit")
  ,(chanCrossRule,     "A half-split channel (par side)")
  ,(chanParRule,       "A half-split channel (par side)")
  ,(chanTypRule,       "A channel containing a type")
  ,(chanEmptyRule 3,   "A memory cell (empty)")
  ,(chanFullRule 3,    "A memory cell (full)")]

typesetChanRules = figure "Rules for explicit channel management" $ 
    env "center" $ do
    forM_ chanRules $ \(r,comment) -> do 
        math $ deriv False r
        cmd0 "hspace{1em}"

outputTexMp :: String -> IO ()
outputTexMp name = renderToDisk' name $ latexDocument "sigplanconf" ["preprint"] preamble $ @"
@header

@intro<-section{Intro}

@paragraph{Line of work}

Computational interpretation of proofs.

Linear Logic as Concurrent Programming Language.

Propositions as Sessions.


@paragraph{Gap/Identify a niche}

Lack of presence of CLL in programming language research.

Current work is mostly based on ILL (DILL). 

CLL treated as a pi-calculus.


@paragraph{Contributions}
Presentation of CLL in a functional, ISWIM style. 

No reference to an external calculus.

Proper treatment of the multiplicative fragment.  (allow asynchronicity).

Technically:
@itemize{
 @item An ISWIM-style term assignment for the one-sided sequent calculus of linear logic
 @item An adaptation of standard concepts of lambda-calculus evaluation (head normal form, etc.)
       to linear logic. 
 @item An abstract machine capable of running linear logic sequents in a concurrent manner,
       based on a von neumann architecture.
}

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
@dm(sequent(simpleCut))
can be represented by the graph
@dm(couplingDiag(simpleCut))
and
@dm(sequent(doubleCut'))
can be represented by the graph
@dm(couplingDiag(doubleCut'))

It can be useful to think of a node in such diagrams as a process, 
and an edge as a communication channel; and the label defines which 
communication protocol which is employed on the channel.

However we emphasize that the direction of edges do not represent the
direction of flow of data; they are a mere convention which allows to
know on which side of the arrow should be interpreted as @tA and which side should be interpreted 
as @neg(tA).

We can so represent all outermost cuts in a sequent.
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
to review some cut-reduction rules with this view. Here is the reduction of a cut in the 
multiplicative fragment:

@dm(couplingDiag(cutParCross))

@dm(couplingDiag(eval cutParCross))

The graph notation make plain that this reduction 
splits the process on the par side into two separate, 
non-communicating processes.

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


@theorem("No deadlock"){In a graph where all outermost cuts are represented by an edge,
           there is always at least an edge ready.
}{
If all outermost cuts are represented, then every node begins either with a rule non-strucutral rule
or Contract or Weakening. Therefore, every node is waiting on at least one variable, except if it
is just the Bot program, which cannot be connected to any node. 


We then proceed by induction on the size of the tree. If the tree has a single node, then
all the edges are hypotheses. It must either be waiting on one of them, which is then ready.

For the inductive case, we assume two graphs @graph1 and @graph2 satisfying the induction hypothesis, 
with and hypothesis @vX in @graph1 and an hypothesis @vX' in @graph2.
We show that the system obtained by connecting @vX and @vX' satisfies the theorem.

We have the following cases:

@enumerate{
@item @graph1 is waiting on @vX and @graph2 is waiting on @vX'. Then the new edge is ready.
@item Either system is not waiting on the designated hypothesis. In this case, some other
   edge in that system must be ready; and it remains ready in the combined system.
}}

The proof depends crucially on the graph structure being a tree. That is, if a cut could
create two edges between subsystems, then the proof would fail. On the other hand, and 
perhaps surprisingly, this property does not depend on the specifics of the evaluation, 
only on the structure of the rules.

@theorem("Liveness"){
There is no infinite chain of outermost evaluations. This means that, eventually, 
outermost evaluation will yield a program waiting on one of its channels. 
In other words: every process must eventually communicate with its environment.
}{
Because there is no infinite chain of evaluations (TODO cite), 
there cannot be in particular an infinite chain of outermost ones.
}

In sum, the above theorem means that linear logic programs can be run in a way similar
to usual ways of running the lambda calculus. 
A ready edge corresponds to a lambda-calculus redex in head position. 
Inner cuts correspond to redexes under lambdas. Non-structural rules correspond to constructors.
A lambda term in head normal form corresponds to a linear program in with a ready hypothesis.

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

There is a standard trick to transform a synchronous communication into an asynchronous one:
insert buffers between the communicating parties.  
We will apply this trick here (calling the buffers explicit channels). 
Notably, we perform this insertion of buffers within the framework of linear logic,
merely adding new rules and reductions.

The idea is then that channels will mediate the interaction between
the non-structural rules. For example, when a @with_ rule is connected
to a @plus_ rule via a channel, the @with_ rule will write a bit of information
to the channel indicating which side of the sum to take. The contination of with will be 
ready to run. Asynchronously, the @plus_ rule will read the bit of info as soon as 
it becomes available, and restore the communication channel to an unwritten one.



@subsection{Reduction rules}


Multiplicative Fragment

@subsection{``black'' reduction}
@subsection{Completeness}

@section{Abstract Machine}

@subsection{Reduction rules}
Equivalent to outermost reductions in the system with explicit channels.

@section{Discussion}

Bi-cut; mix.

Deadlock freedom ~ tree structure ~ resource-management process.

@subsection{Future Work}
Efficiency?

@subsection{Related Work}



@paragraph{Systems based on intuitionistic variants}
@citep{hyland_full_1993}
@citep{barber_dual_1996}
@citep{benton_term_1993}

@paragraph{Session Types}
@citep{caires_concurrent_2012} (also ILL)



@citet{wadler_propositions_2012}

No need for a session-typed functional programming language.

The ⊗ and ⅋ connectors, together with negation, suffice to express all sessions.

Complete symmetry between @id(tA :⊗: tB) and @id(tB :⊗: tA).

A functional fragment can be trivially embedded into LL. 

We also refine the understanding of deadlock (a purely structural property) and liveness.

@paragraph{Graphical representation}
@citet{hirschowitz_topological_2008}

Proof nets: we do not attempt to represent the whole proof graphically; only the top-level structure.

@bibliography

@xref(intro)

@"