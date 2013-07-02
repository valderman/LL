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

outputTexMp :: MPOutFormat -> String -> IO ()
outputTexMp fmt name = renderToDisk' fmt name $ latexDocument preamble $ @"
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
They are shown in the figure @xref(tytab).

@tytab<-typeTable

We provide some intuition for how to understand these types.
@itemize{
@item @id(tA ⊗ tB): the tensor product provide both @tA and @tB. 
@item @id(tA ⅋ tB): both @tA and @tB. The context chooses when to use either of them.
@item @id(tA ⊕ tB): either @tA and @tB. The context chooses which one.
@item @id(tA & tB): both @tA and @tB. The program chooses which one.
@item @id(Forall "α" tAofAlpha): Polymorphism
@item @id(Exists "α" tAofAlpha): Existentials
@item @id(Bang tA): As many @tA 's as one wants.
@item @id(Quest tA): Must produce as many @tA as the context wants.
}

Four of the types come with neutral elements. In figure @xref(tytab) the 
neutral element are written to the right of their respective type. For example, 
the tensor product has neutral element @One, which means that 
@tA @tensor_ @One = @tA. 

There is no type representing negation. Instead, negation is a defined notion 
along the rules below. We only show half of the rule, the other half
are demorgan duals. (Negation is an involution.)

@texNegationTable 

@array([])("ccc")(
  [ [ @" (@tA @tensor_ @tB)^@Bot@" , @" = @", @" @tA^@Bot @par_ @tB^@Bot @"]
  , [ @" (@tA @par_ @tB)^@Bot@"    , @" = @", @" @tA^@Bot @tensor_ @tB^@Bot@"]
  , [ @" (@id(Forall "α" tAofAlpha))^@Bot@" , @" = @", @" @id(Exists "α" tAofAlpha) @"]
  , [ @" (!@tA)^@Bot @", @" = @", @" ?@tA^@Bot @"]
  , [ @" @One^@Bot @" , @" = @", @" @Bot @" ]
  , [ @" @Zero^@Bot @" , @" = @" , @" @Top @" ]
  ]
)

Furthermore , there is no type for linear functions.
Functions can be defined using as 
follows:
@tA @lollipop_ @tB = @id(tA ⊸ tB)

@subsection{Typing rules (with term assignment)}

Figure @xref(rules) shows the typing rules for our language. The judgement form 
we use is a one-sided, Tait-style sequent calculus with only hypotheses. This 
means that there is only hypotheses in our judgement, no conclusion. The program
terms are the only thing occurring to the right of the turnstile. The judgement
may look peculiar at first, in particular since the terms don't have any
return types. However, it can be helpful to think of the programs as "returning"
@Bot. (TODO CPS explanation)

@rules<-typeRules

Similar to other languages based on linear logic, ours is also a concurrent
language. Computation corresponds to communication over channels. Each variable 
in the context corresponds to one end of a channel.

We will now explain the different language constructs as found in figure 
@xref(rules). The name of the rules reflect their logical meaning
whereas the name of the language constructs are meant to suggest their 
operational behaviour.

The axiom rule connects two channels and exchange information. The types of the 
channels must be duals such that one channel is a producer and the other a
consumer. It also ends the current thread; there is nothing happening after
the exchange.

The cut rule creates a new channel with two ends, @math{x} and @math{y}, which 
are connected. The channels are used in two separate threads, @math{a} and 
@math{b}, which run concurrently and in different, disjoint contexts. It is 
worth noting that the two ends of the channel have different types which are 
duals.

There is a similar construct to the cut rule which is the @par_ rule. The 
difference is that the @par_ rule doesn't create a new channel, it splits the
channel @math{z} of type @tA @par_ @tB and each of the parts are used in
different processes. 
Dual to the @par_ rule is the @tensor_ rule which also splits the channel but 
uses the two parts, @math{x} and @math{y},  of the channel in the same process. 
There is no order imposed on how @math{x} and @math{y} should be used. The
order is dictated by how they are used inside the process @math{a}.

There are two symmetric rules for the @with_ type, only one is shown in the 
figure. Given a channel of type @tA @with_ @tB, the rule @with_ decides to
send something of type @tA along the channel. That value is in turn produced in 
the channel @math{x}. This rule realizes internal choice, the process decides 
which choice to make. Dually, external choice is realized by the @plus_ rule 
which examines the contents of the channel @math{z} and branches depending on
the value.

There are three rules for the neutral types @Bot, @One and @Zero. The types
@Bot and @One corresponds to a singleton data type. The rule for
@Bot effectively just ends evaluation of the current process and sends a single 
value along the channel. The rule for @One 
is essentially a noop, since it can only ever receive one value,
denoted @diamond_ in the rules.

The types @Zero and @Top corresponds to the empty data type. The @Zero rule 
corresponds to eliminating the empty type, something which should never happen.
Operationally it will crashes the entire machine, including all other 
concurrently running processes. The @gamma_ in the dump construct is there for 
formal reasons: every variable need to be used once. There is no rule for @Top
as that would mean being able to construct an element of the empty type.

The @forall_ and @exists_ rules deal with channels of polymorphic and 
existential types. The rule @forall_ instantiates a polymorphic channel with a 
particular type @tB which means sending the type along the channel. The type is 
received in the @exists_ rule where the type is given the name @alpha_.

Finally, there are four rules for dealing with exponentials, allowing values
to be duplicated and ignored. The rule ? sends values which have no restriction
on how many times they can be used.
Since values are linear by default we must be careful how we construct such
values. Notice that the context in the ? rule appears with a ! in front of them.
This notation means that all the types in the context must themselves be
exponentials. The ! rule extracts an unrestricted value so that it can used
according to the linear rules.
The rules Weaken and Contract allows for ignoring and duplicating values
respectively.

@subsection{Note CPS relation}
@subsection{Examples}
@subsection{Reduction rules}


@syncFig


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

Perhaps surprisingly, this property does not depend on the specifics of evaluation rules,
only on the structure of the logical rules: namely that each rule is ready to interact on at least
one port, and, crucially that the coupling structure is a tree. 



@theorem("Liveness"){
There is no infinite chain of outermost evaluations. This means that, eventually, 
outermost evaluation will yield a program waiting on one of the variables of its environment.
In other words: every process eventually communicates with its environment.
}{
Because there is no infinite chain of evaluations (TODO cite), 
there cannot be in particular an infinite chain of outermost ones.
}

In sum, the above theorems means that linear logic programs can be run in a way similar
to usual ways of running the lambda calculus. In both cases, the elimination of a top-level
cut involves in-depth rewriting of the term, which is costly and moreover does not correspond
to the notion that programs are static entities. In both cases, it is possible to delay the 
elimination of a cut up to the point where direct interaction occurs.

The analogy between the above execution scheme and execution of lambda terms as programs is deep. 
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

@subsection{Detour: Mix and Bi-cut}

That is, if a @cut_ could create two edges between subsystems, then the proof would fail. 

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

@dm(couplingDiag $(cutWithPlus True))
@dm(couplingDiag $ ( eval' $ cutWithPlus True))
@dm(couplingDiag $ eval $ eval' $ cutWithPlus True)

One can metaphorically talk about the intermediate rule being created
as a particle travelling from left to right. 
By analogy with the elementary particles mediating physical 
forces, we will call such mediating rules bosons.

@texBosons

For the quantifiers fragment, a similar boson and set of reduction rule exists. The
difference is that a type is being transmitted instead of a bit.

For the multiplicative fragment, we examine two possible ways to encode asynchronicity,
to eventually settle on the second one.

The first option is similar to what happens in the additive fragment: a boson (called >) travels
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
@dm(couplingDiag(eval $ eval' $ cutParCross))


The buffer for exponentials is different from the others: it does not merely hold data
which is to be consumed one time, but many times. Hence, it is more proper to see it as
a memory rather than a buffer. Technically, the 
behaviour of exponentials borrow concepts from both additive and multiplicative fragment. 
In a fashion similar to additives, a 'ready to run' boson propagates from @offer_ to
@demand_. That is, @demand_ does not send a boson, only receives one. This boson corresponds
signals that the server obeying protocol @tA is ready to run, by storing its
closure in the memory.

The @contract_ rule behaves similarly to @tensor_: it sends a boson whose effect is to
create a pointer to data. However, a difference in this case is that both new pointers
point to the same thing. Consider the following sequent as an example, where a server
is connected to a client, which we know actually connects to the server at least once.
@dm(sequent $ exponentialSimple)
The server is immediately ready, and this is represented by sending the @math{M} boson. 
Likewise, copying the pointer using the @contract_ rule 
requires no synchronization (a @math{Ct} boson is emitted). This can be represented by the
following diagram
@dm(couplingDiag $ eval' $ exponentialSimple)
As for multiplicatives, the bosons interact. In this case however, we do not immediately
duplicate the @math{M} boson: this would mean that the code for the server is duplicated as well.
Instead, we wish to capture the intuition that we endup with multiple pointers to the same
server. This is supported by the @math{M} boson, which can connect to multiple clients.
Hence, the interaction between bosons yields a system which is represented by the following
diagram:
@dm(couplingDiag $ eval' $ eval' $ exponentialSimple)
Eventually, the client will start interacting with an instance of the server. This is done by 
duplicating its closure @math{a}, which we represent as follows. 
@dm(couplingDiag $ eval' $ eval' $ eval' $ exponentialSimple)
The new process needs to access the environment, which can be accessed by copying pointers
to it, thanks to @math{Ct}. Because exponentials are less regular than the rest of the 
system, they require a more @italic{ad hoc} implementation, and multiple implementations 
are possible. Our choice of implementation is justified by our desire to represent the 
exponential channel as a closure to a server which can be pointed at by many clients.

Finally we turn ourselves to the execution of @ax_. Conceptually, an axiom does nothing.
As we have seen in @fxref(syntaxSec), a @cut_ with an axiom is equivalent to just a @cut_ link.
However, merely removing axioms and adapting links is not an option if we want processes to
behave asynchronously: the adaptation of links requires synchronisation. Hence, what we 
do is have the axiom perform the copy explicitly: for the additive fragment it copies bits of
data from one side to the other, for the multiplicative fragment it divides the type and
spawns two axioms in parallel, etc.

@subsection{Boson-oblivious reduction}

The boson-aware reduction relation is a strict refinement of the reduction relation presented 
in @fxref(syntaxSec).

@theorem(""){
  if neither a nor b contain a boson or an intermediate axiom rule, then
  a == b if and only if a =(new)= b
}{
  For additive, multiplicative and quantifiers, this is immediate because the structure of
  bosons is the same as the original structure. 

  For axioms the result stems from the following lemma.
}

@lemma("Axiom confluence"){
  If a boson (or in case of multiplicative fragment, a pair of bosons) is sent
  on an axiom link, it then reduces to a link (resp. a pair links) between the  
  processes having sent the bosons.
}{
  A simple case analysis of all possible execution paths.
}

@section{Abstract Machine}

In this section we describe an abstract machine for execution of LL programs. The
machine follows closely the refined reduction relation presented in the previous section.
It is similar in spirit to classical abstract machines for the lambda calculus, such as the 
SECD machine.

The machine state is composed of a list of closures and a heap.
The processes from the above section will be represented by closures, and the bosons by the heap.
A closure is a sequent of LL, 
together with an environment associating each variable
to a pointer in the heap, and an environment associating each type variable to a
a type representation. Each closure corresponds to a proccess, and each variable
in its environment corresponds to a port into a channel.
One step of execution of a closure corresponds to sending or receiving a boson, except for the @cut_ rule which 
spawns a process. Every execution step is
 implemented by interacting with the heap: no synchronisation primitive is assumed. 

The heap is an indexable sequence of cells. Each cell can evetually be used to 
transmit some piece of information between closure. Each cell starts its lifetime
as empty (devoid of information). It may then contain some information, which will
be eventually read. Then the cell is deallocated. (In a real system is should made 
available for reuse, but we do bother do do so in this presentation.)

A number of contiguous cells is allocated for each channel in the heap. The number of
cells allocated depends on the type of the channel. 

The number of cells allocated for a channel of type @tA 
is computed by the function @norm(tA).

@definition("layout"){
  The layout function maps a type to a number of cells, 
  in an environment @rho  mapping type variables to type representations.
  @dm(layoutTable)
} 
There will be exactly two closures pointing to each channel of non-exponential a type @tA. One
of these will consider the channel as @tA and the other as @neg(tA), which justifies @norm(tA) = @norm(neg $ tA).

Neither @Zero nor @Top can ever occur in the execution of a program.
We emphasize this fact by assigning them an infinite number of cells.


@subsection{Reduction rules}


@subsection{Adequacy}

@definition("Sequent to AM "){
  Straightforward.
}
@definition("AM to Sequent"){
}

The boson-aware sequents are more fine-grained than the abstract machine:
some distinct sequents will be represented by the same state of the abstract machine.
For example, it is not possible to discover if multiplicative boson have 
interacted or not.

The relation between equivalent sequents is a subset of the boson-reduction.
@definition("Unobservable reductions"){
  
}

@theorem("AM Adequacy"){
- Operational rules are in clear one-to-one correspondance.
}{

}

Combined with the above results, this shows that our abstract machine is sound and complete
with respect to the outermost evaluation relation.

@section{Discussion}

@paragraph{Quantifiers}
Our implementation of type-variables and quantifiers may be surprising. 
It might seem natural to layour store a value of a type variable α as 
a single cell, at it is usual in functional programming languages,
instead of having to lookup it size in an environment.

Our choice is dictated by our not wanting to box every value. This is consistent
with the view that a linear programming language is low level, and
that pointers should be introduced by exponentials only. 


@paragraph{Optimising @ax_}
To simplify presentation, we have made our implementation of @ax_ less 
efficient than it could be. It appears wasteful to have a process which
copies data around, while this data is guaranteed to be produced and consumed
exactly once. Indeed, it is possible to optimise axioms as follows. Consider
first the exponentials and quantifiers. Because these are represented by pointers,
instead of opening the pointer on one side and re-creating an indirectinon the 
other, one can simply directly copy pointers. Consider second additives. Instead
of transmitting a single bit, on could have to possibility to transmit a pointer
to a memory area. In the @plus_ rule, if this pointer is read, then the
process proceeds with reading the bit from the pointed area. The implementation
of axiom can then, insead of copying a potentially large amount of data, send a 
pointer to the source area and terminate immediately.

@paragraph{Asynchronicity}
Furthermore, we eventually wish to develop low-level,
efficient linear programming languages
based on framework laid out in the above. 
In such languages, one will typically represent large arrays by commensurably
large tensors. With the synchronous view of multiplicatives, this would mean
that the layout of an array itself has to be transmitted by @par_, and that @tensor_
cannot proceed until it has received the blueprint. This means that, 
to avoid sacrificing parallelism opportunities, one must go with the asynchronous view.


Bi-cut; mix.

Deadlock freedom ~ tree structure ~ resource-management process.


@subsection{Future Work}
@paragraph{Non-concurrent fragment}
The language we have presented here is fully concurrent. 
That is, at no point we assume that communication is uni-directional.
This means that communication occurs at the bit level, This is obviously
wasteful in real applications, where data is transmitted in larger chunks.
Optimising communication in such a way is compatible with the framework presented.

Another consequence of full concurrency is that a process is spawned at every 
occurence of @par_ and @cut_. However, in many cases, one should be able to discover
that data flows in a particular direction (for example when the code comes from the
translation of a functional program into LL). Again, this optimisation is compatible with
the general framework. The only apparent issue is with polymorphism: an optimiser
will not be able to discover one-sided data flow in the presence of quantification over
arbitrary protocols. The obvious solution, which we plan to investigate, is to 
add a construction for quantification over one-sided protocols.

@subsection{Related Work}

@paragraph{Systems based on intuitionistic variants}

Many presentations of LL for functional programming dualize the logic
@citep{hyland_full_1993,barber_dual_1996,benton_term_1993}. 
That is, every rule introduces a type former either on the left-hand-side or
on the right-hand-side of the turnstile. 

An issue with this version of LL is that ⅋ does not have the same properties 
as Girard's version of it @citep{hyland_full_1993}. Furthermore, 
calling them intuitionistic variants is misleading: LL has good
computational behaviour.

We remain faithful to the spirit of Girard's LL ---
LL is already intuitionistic: there is no need to restrict the system
to give it computational content.

@paragraph{Session Types}
@citep{caires_concurrent_2012} (also ILL)



@citet{wadler_propositions_2012}

A correspondance has recently been identified between linear logic
propositions and session types. A proof of a proposition @tA can be
identified with process obeying protocol @tA. This correspondance
departs from the usual linear logic in that the type @element(tA ⊗ tB) is
interpreted as @tA then @tB, whereas the usual interpretation of the
linear formula is symmetric with respect to time. Our interpretation
keeps the symmetry intact. The associated calculus is close to the
π-calculus, which we observe is unintuitive to functional programmers
in two respect. On a superficial level, they much prefer ISWIM-like
syntaxes. On a semantic level, the ability to transmit channel names,
departs fundamentally from the tradition of functional programming.

@citet{wadler_propositions_2012} presents a translation from a 
session-typed functional programming language into LL. However,
this translation appears to be unnecessary, as LL combines all 
the necessary properties. First, is expressive enough to
directly program functionally: we have assigned functional syntax 
to its proofs (however a large dose of syntactic sugar, the usual translation of intutionistic
logics into LL, will be healthy
to write non-trivial programs). Second, the types 
of LL can express sessions directly. Indeed, 
we have interpreted the type formers in these terms in @fxref(syntaxSec).

Another small improvement of this presentation over that of @citet{wadler_propositions_2012}
is that we have refined the notion of deadlock (shaving off liveness) in LL, 
showing that it is a purely structural property.

@paragraph{Graphical representations}

Graphical representations of proofs in linear logic abund.
Besides the proof nets of Girard, one find many categorically motivated 
representations
@citep{cockett_proof_1997,hirschowitz_topological_2008}.

Our representation is motivated by simplicity. First, it is a direct 
representation of sequents. The reification of a proof from a diagram
has only to choose where to cut first. Second, we hope that it is intuitive: 
it is very close to the component diagrams routinely used by software engineers. 
A difference is that in our diagrams the protocol between components is fully formalised by a type,
intead of being an informal reference to some interface.

The diagrams are also closely related to proof nets.
Considers the multiplicative logic (MLL): then all possible bosons can be emited. If one writes 
the coupling diagram with all bosons represented, it is topologically equivalent to the proof net for
the same sequent. (In a proof-net all hypotheses are at the bottom, and bosons point in a particular direction.)

@section{Conclusion}

In our journey to build an AM for LL, we have encountered a number of useful concepts.
Coupling diagrams, the structural reasons of deadlock avoidance, mediating information 
particles. This has allowed us to shed a new light to some poorly understood
aspects of LL, such as the structural character of the multiplicative fragment.


@bibliography

@cmd0("appendix")

@section{Auxiliary reduction rules}



@syncFig
@pushFig1
@pushFig2


@texBosonReds

@section{Abstract Machine Reduction}
We do not detail the execution of the @ax_ rule: it stems directly 
from the boson-reduction presented in the previous section.
@texAmRulesExplanation
@texAmRules

@"