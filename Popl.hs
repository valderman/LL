{-# OPTIONS_GHC -XTypeFamilies -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}

module Popl where

import Pretty
import MarXup
import MarXup.Latex
import MarXup.Tex
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
import PaperData

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

outputTexMp :: String -> IO ()
outputTexMp name = renderToDisk' name $ latexDocument "sigplanconf" ["authoryear","preprint"] preamble $ @"
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

@syntaxSec<-section{Syntax}

This section will present our language based on  classical linear logic.

@subsection{Types}

The types of our language are directly taken from classical linear logic.

Explanation of the types:
@itemize{
@item @id(tA ⊗ tB): both @tA and @tB. The program chooses when to use either of them.
@item @id(tA ⅋ tB): both @tA and @tB. The context chooses when to use either of them.
@item @id(tA ⊕ tB): either @tA and @tB. The context chooses which one.
@item @id(tA & tB): both @tA and @tB. The program chooses which one.
@item @id(Forall "α" tAofAlpha): Usual polymorphic type.
@item @id(Exists "α" tAofAlpha): Usual existential.
@item @id(Bang tA): As many @tA 's as one wants.
@item @id(Quest tA): Must produce as many @tA as the context wants.
}
The neutrals are respectively @One, @Bot, @Zero and @Top.



@subsection{Typing rules (with term assignment)}

The judgement form we use for classical linear logic is a one-sided, Tait-style
sequent calculus with only hypotheses. The inference rules can be seen in figure
@xref(rules). 

@rules<-typeRules

One way to understand the judgement form is that there is always 

@subsection{Note CPS relation}
@subsection{Examples}
@subsection{Reduction rules}


@section{Coupling Diagrams}

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

@section{Mediating Rules}

There is a standard trick to transform a synchronous communication into an asynchronous one:
insert buffers between the communicating parties.  
We will apply this trick here.
Notably, we perform this insertion of buffers within the framework of linear logic,
merely adding new rules, and replacing the standard (synchronous) reduction rules by another 
set of reductions which all involve an intermediate buffer.

The idea is that buffers will mediate the interaction between
the non-structural rules. For example, when a @with_ rule is connected
to a @plus_ rule via a channel, the @with_ rule will write a bit of information
to the channel indicating which side of the sum to take. The children of @with_ will be 
ready to run regardless of a @plus_ rule being ready to read. 
Asynchronously, the @plus_ rule will read the bit of info as soon as 
it becomes available.

@dm(couplingDiag(cutWithPlus True))
@dm(couplingDiag $ eval' $ cutWithPlus True)
@comment{
Switches the cut structure.
@dm(couplingDiag $ eval $ eval' $ cutWithPlus True)}
@dm(couplingDiag $ eval' $ eval' $ eval' $ cutWithPlus True)

One can metaphorically talk about the intermediate rule being created
as a particle travelling from left to right. 
By analogy with the elementary particles mediating physical 
forces, we will call such mediating rules bosons.

@texBosons
@texBosonReds

For the quantifiers fragment, a similar boson and set of reduction rule exists. The
difference is that a type is being transmitted instead of a bit.

For the multiplicative fragment, we propose two possible ways to encode asynchronicity,
to eventually settle on the second proposition.

The first possible way is similar to what happens in the additive fragment: a boson (called >) travels
from the @par_ rule to the @tensor_ rule. In this case however, on its 'left hand side' the 
boson must connect two processes. The downside of this approach is that, until the
@par_ rule is ready, the @tensor_ must wait. This is sub-optimal because the @tensor_ rule
does not actually need to wait for any information: the behaviour of the continuation does
not depend on anything that the @par_ rule @emph{itself} will provide. In fact, one might
just as well imagine that a boson (called <) should travel in the other direction, from @tensor_ to @par_.
(On the face of it, such a particle would break the invariant we have so far, it would create a cycle
in the coupling graph. Fortunately, this boson has exactly the same structure as the @tensor_ rule
itself, so no possible deadlock can occur.)

This observation leads us to a second and, in our opinion, preferable option to
model asynchronicity. The solution is to send both bosons, and add a reduction rule between them.
(The bosons < and > are in fact mere renamings of the rules themselves. Sending the bosons is equivalent to
bring the cuts under @par_ and @tensor_ to the outermost level.)
This means @par_ and @tensor_ behave completely asynchronously: as soon as they are
encountered their children are ready to run.

@dm(couplingDiag(cutParCross))
@dm(couplingDiag(eval' $ cutParCross))
@dm(couplingDiag(eval' $ eval' $ cutParCross))

TODO: Exponentials

Axioms transmit the bosons from one side to the other.

@subsection{Boson-oblivious reduction}

The boson-aware reduction relation is a strict refinement of the reduction relation presented 
in @fxref(syntaxSec).

@theorem(""){
  if neither a nor b contain a boson, then
  a reduces to b if and only if a boson-reduces to b
}{
  This is a consequence of the local result,  and the fact that bosons travel linearly
  in the coupling graph.
}

@section{Abstract Machine}

In this section we describe an abstract machine for execution of LL programs. The
machine follows closely the refined reduction relation presented in the previous section.
It is similar in spirit to classical abstract machines for the lambda calculus, such as the 
SECD machine.

The machine state is composed of a list of closures and a heap.
A closure is a sequent, together with an environment associating each variable
to a pointer in the heap, and an environment associating each type variable to a
a type representation.

The heap is an ordered sequence of cells. Each cell can evetually be used to 
transmit some piece of information between closure. Each cell starts its lifetime
as empty (devoid of information). It may then contain some information, which will
be eventually read. Then the cell is deallocated. (In a real system is should made 
available for reuse, but we do bother do do so in this presentation.)

A number of contiguous cells is allocated for each channel in the heap. The number of
cells allocated depends on the type of the channel, and is computed as follows.

@definition("layout"){
  @dm(array[] "ccc" [[element t,element (neg t),element (mkLayout t)] | t <- allPosTypes]
)
}

There will be exactly two closures pointing to each channel of non-exponential a type @tA. One
of these will consider the channel as @tA and the other as @neg(tA), which justifies @mkLayout(tA) = |@neg(tA)|.


@subsection{Reduction rules}

@subsection{Adequacy}

@definition("Sequent to AM "){
  Straightforward.
}
@definition("AM to Sequent"){
}

@theorem("AM Adequacy"){
- Operational rules are in clear one-to-one correspondance.
- Difficulty: it is not possible to discover if the >< rule has been executed or not.
- Solution: we merely claim adequacy up to that rule (as we do up to cut reordering)
}{

}

@section{Discussion}

Bi-cut; mix.

Deadlock freedom ~ tree structure ~ resource-management process.

Complete asynchronicity of multiplicative essential for efficient handling of tuples.

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

@section{Conclusion}

In our journey to build an AM for LL, we have encountered a number of useful concepts.
Coupling diagrams, the structural reasons of deadlock avoidance, mediating information 
particles. This has allowed us to shed a new light to some poorly understood
aspects of LL, such as the structural character of the multiplicative fragment.


@bibliography


@"