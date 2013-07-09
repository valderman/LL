{-# OPTIONS_GHC -XTypeFamilies -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}

module Popl where

import MarXup
import MarXup.Latex
import MarXup.Tex
import Reductions
import Data.Monoid
import TexPretty
import LL
import Rules
import GraphViz
import Framework
import Data.List
import PaperData
import Examples

acmCategories,acmKeywords,abstract,header :: TeX
acmCategories = do
  cmdn_ "category" ["F.4.1","Mathematical Logic","Lambda calculus and related system"]
  cmdn_ "category" ["D.3.3","Language Constructs and Features","Concurrent programming structures"]
  
acmKeywords = do  
  cmd0 "keywords"
  mconcat $ intersperse ", " ["linear logic","lambda calculus"]

abstract = env "abstract" $ @"
Girard's Linear Logic (LL) is a low-level logic: System F or Classical Logic can be 
embedded into it. Thanks to the isomorphism between logics and programming
languages, LL corresponds to a low-level programming language where management of resources
is explicit, and concurrent aspects of programs can be expressed.

However, LL does not appear to be very much used as such in the programming language
research community. We make steps in this direction, 
by giving computational presentation of LL. In particular, we describe an 
abstract machine capable of one-sided sequent calculus proofs, interpreted as programs.
The style of presentation
is in the tradition of execution models for functional programming languages,
typically those following Landin's ISWIM. By using time-tested
methods in the presentation, we hope to make Linear Logic accessible to a wide 
portion of the programming languages community.
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

There is a strong relationship between evaluating programs and proofs.
The correspondance is usually known as the Curry-Howard isomorphism, and goes
as follows:
@dm(array[]("cc")(map (map (cmd "text")) [
  ["Propositions", "Types"],
  ["Proofs", "Programs"],
  ["Verification", "Type-Checking"],
  ["Normalisation", "Evaluation"]
 ]))
This correspondance is quite natural for intuitionistic logics, but
not so much for classical ones. Motivated by this apparent mismatch,
@citet{girard_linear_1987} has developed Linear Logic. 
Linear Logic acts as a low-level logic, where both intuitionistic and
classical logics can be embedded. From its inception, Linear Logic
has been recognised as corresponding to concurrent programming
langaguages. Fleshing out this correspondance has given rise to 
a long line of work @citep{}.

Additionally, as Linear Logic is a ``low-level'' logic, it corresponds
to a low-level programming language, where management of resources is
explicit. As such, we believe that a language based on linear logic 
would be ideal low-level functional programming language, with explicit
management of resources, and where opportunities for concurrent execution
can be expressed at a fine-grained level.

Whereas linear types have made incursions into the world of programming
languages, some fundamental ideas present in Girard's work have not percolated
to the area of programming language research. Notably, duality is a central notion
of linear logic: every type has a dual, which means that there is no @italic{a priori}
distinction between input and outputs. However this distinction appears to be 
solidly anchored in PL research, and hence most attempts to integrate linearity
break the symmetry, and forcibly re-introduce the syntactic distinction
between input and output.

The versions linear logic featuring this distinction are labeled @emph{intuitionistic}.
(When one wants to emphasize that inputs and outputs are unified one sometimes 
use the @emph{clasical} label.) However, as Girard points out, the classical version
is already grounded in computational intuitions, so the intuitionistic label
appears unjustified. One of our aims is to explain the computational grounds
of classical linear logic by using similar means as was used for explaining 
intuitionistic logic.

@citet{wadler_propositions_2012} has recently given an interpretation of 
classical linear logic by reference to the π-calculus. However,
coming from a functional programming perspective, this indirect explanation seems 
superfluous. Our aim is find the natural programming language corresponding
to LL, in the same way as λ-calculi correspond to intuitionistic logics.
Furthermore, as much as possible, 
we desire a design which follows ISWIM conventions @citep{landin_next_1966},
in particular regarding syntax and the naming of intermediate values.

@dm(array[]("cc")(map (map (cmd "text")) [
  ["Simply-Typed λ-calculus", "Propositional Logic"],
  ["Polymorphic λ-calculus", "System F"],
  ["?", "Classical Linear Logic"]
 ]))

As concrete steps towards this goal, we make the following contributions:
@itemize{
 @item A term assignment for classical linear logic proofs @syntaxSec, with 
       a functional syntax inspired by ISWIM (@syntaxSec).
 @item An abstract machine capable of running programs written in the above language (@amSec).
       This abstract machine is concurrent, and based on time-tested concepts, 
       such has closures and heap.
 @item We show that the execution steps of the abstract machine correspond
       to proof normalisation steps. We do this by introducing a series of interconnected
       reduction relations (@redOMDef, @redAXDef, @redBODef, @redAMDef).
 @item We show how standard concepts of lambda-calculus evaluation 
       (redex,head normal form, etc.) generalize in linear logic.  (@outerSec)
}

@syntaxSec<-section{Syntax and Intuitions}

This section presents the syntax of our language. The
syntax is functional, and suggestive of the operational behaviour.

@subsection{Types}

The types of our language are directly taken from classical linear logic.
They are shown in @tytab. The metasyntactic variables @tA,@tB and @tC range over types.

@tytab<-typeTable


The binary type former all come with neutral elements. In @tytab the 
neutral element are written to the right of their respective type. For example, 
the tensor product has neutral element @One, which means that 
@tA @tensor_ @One = @tA. 

We can provide immediately an intuition for how to understand these types, even
if it is eventually refined by the operational semantics given in further sections. 
Assuming a variable @vX in the context of a program, what can the program expect to obtain from @vX?
The answer depends on the type of @vX:
@itemize{
@item @id(tA ⊗ tB): both @tA and @tB. The program decides in what order to use them.
@item @id(tA ⅋ tB): both @tA and @tB. The environment decides in what order to use them.
@item @id(tA ⊕ tB): either @tA and @tB. The environment chooses which one.
@item @id(tA & tB): either @tA and @tB. The program chooses which one.
@item @id(Forall "α" tAofAlpha): @math{A[B]}. @tB of its chosen by the program.
@item @id(Exists "α" tAofAlpha): @math{A[B]}. @tB of its chosen by the environment.
@item @id(Bang tA): Can demand as many @tA 's as one wants.
@item @id(Quest tA): Must prodvide as many @tA as the environment wants.
}
Two aspects become obvious from this enumeration. First, having a variable in the
environment is not all roses: it may provide something to the program, but it may
also generate an obligation. For example, in @id(tA & tB), the program @emph{must}
make a choice between @tA or @tB: ``I don't care'' is not an option. In essence, a variable
means that there is a contract between the program and its environment.
Second, one can see that the environment and the program are on equal footing:
any constraint which can be imposed on an environment may be imposed on a program, 
and @italic{vice versa}. The dual of a type @tA is written @neg(tA), and computes
as follows:
@texNegationTable
(We only show half of the rule, the other half are demorgan duals.)
Even though we often write @tA@texNeg(False) for the negation
of a type, one should realise that dualisation is not represented in the syntax,
except variables, in other cases it is evaluated as shown above. Hence the only concrete 
representation of duals in the syntax is for variables: we have both α and α⟂.
(When substituting a concrete type for α the dual computes further.)
As expected, dualisation is an involutive: @math{(@tA^@Bot)^@Bot = @tA}.

Using the above syntax, the linear arrow can be defined as follows:
@tA @lollipop_ @tB = @id(tA ⊸ tB). That is, according the the intuitions built
so far, having @math{x:A ⊸ B} means that if we provide the environment with @tA,
we will get @tB. The processing order will be at the discretion of the environment.

@subsection{Terms, Typings and Their Meaning}

We metasyntactic variables @math{x}, @math{y} and @math{z} range over variables in our
language; @alpha_ and @beta_ range over types; 
@gamma_, @delta_ and @xi_ range over contexts. Contexts are unordered maps
of variables to types, and these mappings are written @math{x : @tA}. Variable
names in contexts are assumed distinct. Contexts are used to enforce linearity.
It is therefore important that when combining two contexts, the names in them
are distinct. Variables are never implicitly dropped from a context.

Terms are defined by the following grammar:
@termFigure

Name binding works as follows: in the two @connect_ constructs and in the @case_ 
construct, @math{x} is 
bound in @math{a} and @math{y} is bound in @math{b}. 
In all the @let_ constructs, all variables @math{x}, @math{z} and/or @alpha_ 
appearing to the left of the equals sign, are bound in @math{a}. In the @ignore_
construct @math{z} is no longer in scope in @math{a}.

@rules shows the typing rules for our language. 
We use a one-sided judgement form, with only hypotheses. 
This means no conclusion: the program
terms are the only thing occurring to the right of the turnstile. The judgement
may look peculiar at first sight, in particular since the terms do not have any
return type. However, it can be helpful to think of the programs as ``returning''
@Bot, with the intuitive meaning that every program eventually terminates.

This feature is reminiscent of continuation-passing style intermediate languages,
often used as low-level intermediate representations in the compilation of functional
languages.

Thanks to duality, there is no need for constructors: constructing a value
is implemented by eliminating its dual.

@rules<-typeRules

Similarly to other languages based on linear logic, ours is also a concurrent
language. Computation corresponds to communication over channels. Each variable 
in the context can be understood as a reference to one end of a channel, whose
type expresses the protocol employed on the channel.

We now explain the different language constructs. While the syntax suggests
the operational behaviour, the rule names follow the convention found in the
linear logic literature. In particular, elimination rules are simply named after
the type constructor that they eliminate.

The @ax_ rule connects two channels and exchange information. The types of the 
channels must be duals, so that one channel provides what the other requires.
It is also the last instruction on the current thread: there is nothing happening after
the exchange.

The @cut_ rule creates a new channel with two ends, @math{x} and @math{y}, which 
are connected and have dual types. The channels are used in two separate threads, 
respectively running @math{a} and @math{b}. The threads concurrently and in different,
disjoint contexts. 
The @par_ construct is similar to @cut_. The 
difference is that the @par_ rule does not create a new channel, but splits the
channel @math{z} of type @tA @par_ @tB, and each of the parts are used in
different processes. 
Dual to the @par_ rule is the @tensor_ rule which also splits the channel but 
uses the two parts, @math{x} and @math{y}, of the channel in a single process @math{a}. 
The program @math{a} has complete freedom regarding the order in which  @math{x} and @math{y}
are used. This means that, conversely, the @par_ rule must be able to honour 
any order whatsoever between the subchannels. This is indeed enforced by having
those two parts handled by separate processes.

Given a channel of type @id(tA :&: tB), the rule @math{&_1} commits to protocol @tA,
naming the new channel @math{x}. This rule realizes active choice: the process decides 
which choice to make. Dually, passive choice is realized by the @plus_ rule 
which examines the choice expressed on the channel @math{z} and branches depending on
the value.

There are three rules for the neutral types @Bot, @One and @Zero. The types
@Bot and @One corresponds to a singleton data type. The rule for
@Bot effectively just terminates the current process. The reference to a channel
of type @Bot ensures that some other process (connected at the other end) exists, 
so that there will always be at least one process running.
The dual rule for @One is a no op. Conceptually, it acknowledges that the process 
connected to it is terminated. However, because this eventual termination is guaranteed
by the system, we choose not to transmit any information. If explicit notification of
termination is wanted, it can always be encoded by an explicit bit of info.

The @Zero is empty: there is no rule eliminating its dual @Top, so it cannot
be constructed. Accordingly, no value of type @Zero 
can ever be eliminated, so the @Zero rule can safely be interpreted
as crashing the machine. The @gamma_ in the dump construct is there for 
formal reasons: every variable need to be used once. 

The @forall_ and @exists_ rules deal with channels of polymorphic and 
existential types. The rule @forall_ instantiates a polymorphic channel with a 
particular type @tB which means sending the type along the channel. The type is 
received in the @exists_ rule where the type is given the name @alpha_.

Finally, there are four rules for dealing with exponentials, allowing values
to be duplicated and ignored. The rule ? provides a protocol which can be 
used an arbitrary number of times (a service).
Because values are linear by default we must be careful how we construct such
services: all protocols used to offer it must be services themselves.
Formally, the context in the ? rule appears with a ! in front of it.
This notation means that all the types in the context must be prefixed fith !.
The ! rule extracts an unrestricted value so that it can used
according to the linear rules.
The rules @weaken_ and @contract_ allow for ignoring and duplicating values
respectively.

@subsection{Examples}

To illustrate our language we give some example programs. Because the language is rather austere and 
does not have much in the way of data types the examples are by necessity rather simple.

The first example is a program to swap the values in a tensor product.
A natural approach for functional programmers is to start with the type 
@swapType and try to inhabitate it. 
However, however programs in our language all have type ⊥. We have then to take an approach
similar to CPS and invert the flow of control: we assume a variable swappee of type 
@MetaNeg(swapType) = @neg(swapType), and eliminate it.
@id(programWithCtx swap)

The swap program invokes the swapee to
retrieve the channels to swap and reorders them accordingly. In order to type
our swap program the swapee occurs in the context and have a type which is dual
to the type above @math{(@id(swapType))^⊥}. The type judgement for the
swap program becomes as follows:

@math(sequent swap)

No programmer in their right mind will want to write programs in the above style.
Indeed, we intend our language to be used as a low-level, intermediate representation;
concrete programs can be written in direct style, whose desugaring follows closely
the translation from direct to continuation-passing style.

@subsection{Reduction rules}

@assocFig<-structFig

@redFig<-syncFig

The meaning of our language is defined in terms of the reduction rules given in 
figure @redFig. As is usual in logic, the reductions eliminates the use of the
@cut_ rule. From a programming language point of view we can understand reduction
as communication between processes. Channels in our language are one-shot, meaning that they are
only ever used for a single exchange, although that exchange can be arbitrarily
complicated and communicate in both directions. This means that once the
communication has happened the channel can be eliminated and hence also the @cut_
rule. However, not all reduction rules perform communication. Some reductions
are administrative and in some cases there is no information to communicate.

First, there are the structural equivalences, show in figure @assocFig. The
first equivalence implies that the @cut_ rule is associative. The second
equivalence

operationally, this equivalence corresponds to introducing and removing channel
forwarding.

The principal reduction rules are shown in figure @redFig.

The first reduction rule deal with the axiom construct which forwards channels. 
The process doing the forward transmits a new channel @math{w} and terminates. 
The other process will continue and communicate with the new channel replaced 
by the old one.

Reduction between tensor and par isn't so much communication as splitting the
channel and forking off a new thread. No information as such is transmitted
during reduction.

There are two rules for plus and with, one for each choice in the with 
construct. During communication one bit will be sent from the with process
which will determine which branch to choose in the plus process. After
reduction the processes will continue and communicate over the chosen channel.

The rules for polymorphism and existentials means sending the type over the
channel from the type application to the unpack construct.

The reduction rules for exponentials all involve the offer construct on one 
end of the communication. When the other end is a demand construct, 
the exponential will simply be dropped and the processes will continue 
and communicate the value.

When paired with alias, the offer process is
duplicated and the new process will communicate over the new channel indicated
by the alias construct.

Finally, when paired with an ignore, the offer process will simply be dropped
as it is no longer needed. Evaluation proceeds only in the ignore process.

Finally, reduction for @One and @Bot simply ends the @Bot process.

@outerSec<-section{Outermost Reduction}

The fact that the program @math{a} is working in a environment @math{Γ = x:A,y:B,z:C⟂} is
usually represented by a judgement @derivation(simpleEnv). In the
rest of the paper we also use the following graphical representation:
we label a node with the program that it represents, and 
connect edges to it to represent the environment.
@simpleEnv
Two remarks are worth making. First, the edges could be labeled explicitly with variable names, but in all
the examples we use, the types alone are enough to lift any ambiguity.
Second, the representation of @tA being in the environment of the program can be  
either an ingoing egde labeled with an edge @tA or an outgoing edge labeled 
with @neg(tA). 

Using this convention, we can then represent @cut_ by an edge between nodes. For example the derivation
@dm(derivation(simpleCut))
can be represented by
@dm(couplingDiag(simpleCut))
and
@dm(sequent(doubleCut'))
can be represented by
@dm(couplingDiag(doubleCut'))
It can be useful to think of a node as a process, 
and an edge as a communication channel, whose label defines which 
communication protocol which is employed on the channel.
We emphasize that the direction of edges do @emph{not} represent the
direction of flow of data; they are a mere convention which allows to
know which side of the arrow interprets the label literally, 
and which side interprets it as its dual.
We call such a picture a @emph{coupling diagram}.

@outermostCut<-definition("Outermost Cut"){
An instance of a @cut_ rule in a derivation tree is called an outermost cut if
it is either the first rule in a derivation or if it is an outermost cut in a subderivation of
an outermost cut.
}
As we have seen on the above examples, all outermost occurences of
@cut_ can be represented graphically. We remark right away that, 
because each @cut_ connects two subgraphs by exactly one edge, the coupling structure 
of outermost cuts is necessarily a tree.

Representing a @cut_ structure by a coupling diagram has two immediate advantages.
First, it is a concise notation that relieves much notational burden compared to
full derivations. Second, equivalent sequents (by the @cutAssoc rule) are represented by 
topologically equivalent diagrams, making the equivalence intuitive.

It may be enlightening to review cut reduction of the multiplicative fragment with this view.
A multiplicative cut is represented by
@dm(couplingDiag(cutParCross))
and it reduces to
@dm(couplingDiag(eval cutParCross))
which makes plain that the reduction 
splits the process on the @par_ side into two separate 
processes, which may communicate only via the process
coming from the @tensor_ side.

@subsection{Outermost Evaluation Strategy}
Equipped with coupling diagrams, we can describe the
first step towards execution of linear logic proofs as
concurrent processes.

@redOMDef<-definition("Outermost Evaluation"){
  We define outer evaluation (@redOM) by taking the union of
  the operational reduction rules (@operationalRules_), 
  their swapped version (@math(swapped operationalRules_)), 
  and the congruence of these rules with @cut_:
  if @math{a @redOM a'} and @math{b @redOM b'} then 
  @dm(sequent(simpleCut' "a" "b") <> redOM <> sequent (simpleCut' "a'" "b'"))

  In summary,
  @math(redOM <> " = " <> redOMDef_).
}
The main difference between this evaluation relation and @redLL is the  
absence of commuting conversions. This means that top-level cuts are not 
pushed under communicating primitives. Instead they remain part of the 
outer coupling structure until communication can happen.

The remainder of the section is devoted to showing that this  strategy of evaluation 
is well-behaved, and corresponds to usual evaluation strategies of the λ-calculus.

@definition("Waiting on a variable"){
We say that a program is @emph{waiting} on a variable @vX in its context if:
@itemize{
 @item it is an axiom, or
 @item if starts with a @contract_ or @weaken_ rule acting on @vX, or
 @item if starts with an operational rule acting on @id(vX).
}}

@definition("Ready edge"){
We call an edge @emph{ready} if all programs connected to it are waiting on the variables connected 
to it. Recall that an edge either represents a @cut_ or a reference to the environment (an hypothesis). 
So in particular, 
an hypothesis edge is ready if the single node  connected to it is  waiting on it,
and a @cut_   edge is ready if both       nodes connected to it are waiting on it.
}
Remark that a ready @cut_ is reductible.

@noDeadlockThm<-theorem("No deadlock"){In a coupling diagram where all outermost cuts are represented,
           there is always at least an edge ready.
}{
Remark first that if all outermost cuts are represented, then every node begins either with an operational rule
or @contract_ or @weaken_. Therefore, every node is waiting on at least one variable.

We proceed by induction on the size of the tree. If the tree has a single node, then
all the edges are hypotheses. It must either be waiting on one of them, which is then ready.

For the inductive case, we assume two graphs @graph1 and @graph2 satisfying the induction hypothesis, 
with an hypothesis @vX in @graph1 and an hypothesis @vX' in @graph2.
We show that the system obtained by connecting @vX and @vX' satisfies the theorem.

We have the following cases:

@enumerate{
@item @graph1 is waiting on @vX and @graph2 is waiting on @vX'. Then the new edge is ready.
@item Either system is not waiting on the designated hypothesis. In this case, some other
   edge in that system must be ready, and it remains ready in the combined system.
@qedhere
}}

Perhaps surprisingly, the freedom from deadlock does not depend on the specifics of evaluation rules,
but on their broad structure: namely that each rule is ready to interact on at least
one variable, and crucially that the coupling structure is a tree. 


@livenessThm<-theorem("No Starvation"){
If the @cutAx_ rule is never used to create a @cut_, there is no infinite chain of reduction steps.
This means that 
outermost evaluation eventually yields a program waiting on one of the variables of its environment.
In other words: every process eventually provides its environment with what it asks.
}{
The result is a consequence a classical result: cut-elimination in LL. 
The proof is based on the observation that the products of @cut_ reduction are smaller than 
the initial @cut_. Two cases are non-obvious. First, the @offer_/@contract_ rule creates syntactically
bigger trees, but the resulting contractions happen on smaller trees. Second, because of
impredicativity, some extra care is necessary to ensure termination @citep{gallier_girards_1989}. 
}

In sum, the above theorems means that linear logic programs can be run in a way similar
to usual ways of running the lambda calculus. In fact, the correspondance is deep: we can draw 
parallels between both sides for every concept used by the respective evaluators.

A ready @cut_ corresponds to a redex. In both cases, the reduction of a top-level
@cut_ (or redex) involves in-depth rewriting of the term, which is costly and does not correspond
to the notion that programs are static entities. In both cases, it is possible to delay the 
elimination of a @cut_ up to the point where direct interaction occurs.
A ready edge corresponds to a lambda-calculus redex in head position. 
Inner cuts correspond to redexes under lambdas. Operational rules correspond to constructors.
A lambda term in head normal form corresponds to a linear program in with an hypothesis edge ready.

The behaviour of the abstract machine of @citet{krivine_call-by-name_2007} is to traverse
the spine of applications inwards and leftwards until it finds a redex, then reduce it. 
(The SECD machine of @citet{landin_mechanical_1964} behaves likewise)
The reduction yields then another redex in the same position, or one must continue the traversal inwards and leftwards.
An abstract machine for linear logic must traverse the coupling structure, potentially considering all outermost
cuts, to eventually find a one which is ready, and reduce it. The difficulty in the
linear/concurrent case is that one cannot @italic{a priori} know where the ready cut is located. Furthermore,
the next ready cut may not be in the neighborhood, so a working list of potentially ready cuts must 
be maintained.

In this light, one understands commuting conversions as the ability to bring a waiting 
communication primitive to the front of a term --- a concept that is not meaningful in  
λ-calculus.

The execution strategy outlined above is a direct generalisation of
classical execution strategies for lambda calculi.
 However this evaluation
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

We attack this shortcoming before taking a brief detour.

@subsection{@mix_ and @bicut_}

The @cut_ rule allows two processes to communicate via exactly one channel. 
Variants of the rule allowing zero (@mix_) two (@bicut_)
channels have been proposed.
  
  @mathpar[[
  frac(@"Γ⊢ @hspace("2em") Δ⊢ @")(@"Γ,Δ⊢ @") <> mix_,
  frac(@"Γ,A,B⊢ @hspace("2em") A⟂,B⟂,Δ⊢ @")(@"Γ,Δ⊢ @") <> bicut_
  ]]


The @mix_ rule has been proposed by 
@citet{girard_linear_1987}, and is a safe extension. Indeed, the proof of @noDeadlockThm remains
valid: if a cut creates no edge then it is clear that ready edges are preserved. However, @bicut_
is not safe: if two edges are created, then it is possible to create a symmetric situation where
each subsystem waits for the other to be ready.

@asyncSec<-section{Asynchronous Outermost Reduction}

@subsection{Explicit Axioms}

Conceptually, an axiom does nothing:
as we have seen in @syntaxSec, a @cut_ with an axiom is equivalent to just a @cut_ link.
However, merely removing axioms and adapting links is not an option if we want processes to
behave asynchronously: the adaptation of links requires two-way synchronisation. 

However, in the same way that one can implement polymorphic identity by explicit copy 
of data, one can implement axioms by processes which explicitly interface between
two dual protocols. That is, for the additive fragment, one can to case analysis on one 
side, and send the information of which side is taken to the other side.
For the multiplicative fragment, one can explicitly split the axiom into a pair of axioms,
etc. The complete set of rules is show in @axiomRedsFig.

@axiomRedsFig<-texAxiomReds

Replacing the @cut_/@ax_ rule by the conversions rule does not change the meaning of programs.
It means however that @ax_ rules rules disappear only when they apply to unit types. 
This is problematic operationally (we discuss this issue in @axiomOptSec), but not 
for formal purposes, because @cut_/@ax_ is an equivalence.

@redAXDef<-definition{@redAX}{
@math{@redAX = @redAXDef_}
}

@theorem(""){
 For any terms @math{a,b}, @math{a @many(redOM) b} iff @math{a @many(redAX) b}.
}{
 The proof proceeds by case analysis. For each type where @ax_ opers, 
 it can be shown that the evaluation via
 the explicit copy is confluent with the following steps: elimination of @cut_,
 reduction of the original rule, then recreation of @ax_ instance(s). This recreation
 explain why we need @cutAx_ to be an equivalence.
}

@subsection{Mediating Rules}

There is a standard trick to transform a synchronous communication into an asynchronous one:
insert buffers between the communicating parties.  
We apply this trick here.
Notably, we perform this insertion of buffers within the framework of linear logic,
merely adding new rules, and replacing the standard (synchronous) reduction rules by another 
set of reductions which all involve an intermediate buffer.

The idea is that buffers mediate the interaction between
the non-structural rules. For example, when a @with_ rule is connected
to a @plus_ rule via a channel, the @with_ rule writes a bit of information
to the channel indicating which side of the sum to take. The children of @with_ is 
ready to run regardless of a @plus_ rule being ready to read. 
The @plus_ rule reads the bit of info asynchronously as soon as 
it becomes available. This execution process can be depicted using coupling diagrams as 
follows:
@dm(couplingDiag $(cutWithPlus True))
@dm(couplingDiag $ (eval' $ cutWithPlus True))
@dm(couplingDiag $ eval $ eval' $ cutWithPlus True)
The intermediate rule can be metaphorically seen
as a particle travelling from left to right. 
By analogy with the elementary particles mediating physical 
forces, we will call such mediating rules bosons.
In diagrams, we write them without a circle around them.
The diagrammatic representation of the & boson suggests to
implements it as the rules
@mathpar[map (sequent . chanPlusRuleBad) [True, False]]
However, we make another choice: to represent it as a rule with the
same premiss and conclusion as @with_, and merely consider the premiss
to be ready to run in our execution model (the rule will be congruent for evaluation). 
@mathpar[map (sequent . withRule True) [True, False]]
In this sense, it is as if
the new rule had a structural aspect to it (it embeds a virtual cut).
This feature is made explicit in the diagram by drawing the virtual cut
with a dotted edge.

For the quantifiers fragment, a similar boson and set of reduction rule exists. The
only difference is that a type is being transmitted instead of a bit.

For the multiplicative fragment, we examine two possible ways to encode asynchronicity,
to eventually settle on the second one.
The first option is similar to what happens in the additive fragment: a ⅋ boson travels
from the @par_ rule to the @tensor_ rule. In this case however, on its 'left hand side' the 
boson must connect two processes. The downside of this approach is that, until the
@par_ rule is ready, the @tensor_ must wait. This is sub-optimal because the @tensor_ rule
does not actually need to wait for any information: the behaviour of the continuation does
not depend on anything that the @par_ rule @emph{itself} will provide. In fact, one might
just as well imagine that a ⊗ boson should travel in the other direction, 
from @tensor_ to @par_.

This observation leads us to a second and, in our opinion, preferable option to
model asynchronicity. The solution is to send both bosons, and add a reduction rule between them.
This means @par_ and @tensor_ behave completely asynchronously: as soon as they are
encountered their children are ready to run.

@dm(couplingDiag(cutParCross))
@dm(couplingDiag(eval' $ cutParCross))
@dm(couplingDiag(eval $ eval' $ cutParCross))

If we had chosen to represent bosons as rules a without structural component
(eg. separated the @cut_ from the & boson as considered above), then the 
emission of the ⊗ boson would have required the use of @bicut_. This requirement  
mainly explains our preferrence for the alternative, where the ⊗ boson has the
same structure as the @tensor_ rule.

The buffer for exponentials is different from the others: it does not merely hold data
which is to be consumed one time, but many times. Hence, it is more proper to see it as
a memory rather than a buffer. Technically, the 
behaviour of exponentials borrow concepts from both additive and multiplicative fragment. 
In a fashion similar to additives, a 'ready to run' boson (called @math{M} for memory) 
propagates from @offer_ to @demand_. That is, @demand_ does not send a boson, 
only receives one. This boson signals that the server obeying protocol @tA is ready 
to run, by storing its closure in the memory. Its absorption corresponds to spawning
an instance of the server process.

The @contract_ rule behaves similarly to @tensor_: it sends a boson (@math{Con}) whose effect is to
create a pointer to data. However, a difference in this case is that both new pointers
point to the same thing, the pointer is duplicated. 
Consider the following sequent as an example, where a server
is connected to a client, which we know actually connects to the server at least once.
@dm(sequent $ exponentialSimple)
The server is immediately ready, and this is represented by sending the @math{M} boson. 
Likewise, copying the pointer using the @contract_ rule 
requires no synchronization (a @math{Con} boson is emitted). This can be represented by the
following diagram
@dm(couplingDiag $ eval' $ exponentialSimple)
As for multiplicatives, the bosons interact. In this case however, the behaviour of bosons does
not mimic the behaviour of regular rules. That is, we do not immediately
duplicate the @math{M} boson. Indeed this would mean that a server instance is spawned, 
however we wish to do this only when the @demand_ rule is executed.
Instead, we wish to capture the intuition that we end up with multiple pointers to the same
server. This is supported by the @math{M} boson, which can connect to multiple clients.
Hence, the interaction between bosons yields a system which is represented by the following
diagram:
@dm(couplingDiag $ eval' $ eval' $ exponentialSimple)
Eventually, the client will start interacting with an instance of the server. This is done by 
duplicating its closure @math{a}, which we represent as follows. 
@dm(couplingDiag $ eval' $ eval' $ eval' $ exponentialSimple)
The new process needs to access the environment, which can be accessed by copying pointers
to it, thanks to the @math{Con} boson. Because exponentials are less regular than the rest of the 
system, they require a more @italic{ad hoc} implementation, and multiple implementations 
are possible. Our choice of implementation is justified by our desire to represent the 
exponential channel as a closure to a server which can be pointed at by many clients.
The @weaken_ rule behaves in a manner similar to @contract_.
This concludes our description of bosons, whose complete list is shown in @bosonsFig
@bosonsFig<-texBosons

@redBODef<-definition{@redBO}{
  The asynchronous reduction, explained above, replaces the @operationalRules_ rules by
  boson emission and reception (@bosonOper_, @bosonOperFig) and @bosonBoson_ (@bosonBosonFig) interactions.
  The boson rules are also congruent for this relation.
  @math{@redBO = @redBODef_}
}

The full asynchronous reduction is a refinement of the synchronous reduction with asynchronous
axioms.

@theorem(""){
If @math{a @redOM a'} then @math{a @many(redBO) a'}
}{
  Base case analysis of each of the @operationalRules_.
  For additive, multiplicative and quantifiers fragments, this is immediate because the structure of
  bosons is the same as the original structure. 

  For exponentials, one must use the memory boson in 
  intermediate states.
}


@theorem(""){
If @math{a @many(redBO) a'} and neither @math{a} or @math{b} contains a boson then @math{a @many(redOM) a'}.
For each of the reductions using an intermediate state containing bosons, it is possible to find a direct
route.
}{
By case analysis, as above. Additionally, one remarks that there is only one way to consume 
an intermediate boson; rules firing concurrently have no bad effect.
}


@amSec<-section{Abstract Machine}

In this section we describe an abstract machine for execution of LL programs. The
machine follows closely the refined reduction relation presented in the previous section.
It is similar in spirit to classical abstract machines for the lambda calculus, such as the 
SECD machine.

The machine state is composed of a multiset of closures and a heap.
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
of these will consider the channel as @tA and the other as @neg(tA), which 
justifies @norm(tA) = @norm(neg $ tA).

Neither @Zero nor @Top can ever occur in the execution of a program.
We emphasize this fact by assigning them an infinite number of cells.

@subsection{Reduction rules}

Each rule in the @bosonOper_ category involves exactly one rule of the original logic. Hence
each top rule in a proof can be interpreted as an instruction of the machine, whose
meaning follows the asynchronous reduction. Rules in the @bosonBoson_ category, on the other
hand, correspond to structural properties of the heap. For example, the interaction between
the ⊗ and ⅋ boson correspond to the property that the components of a ⊗ or ⅋ types are
laid out in sequence in the heap. Hence, there is no computation associated with them in
the machine. Lastly, the commutativity and associativity of @cut_ corresponds to the 
structure of the muliset of closures.

@redAMDef<-definition("Abstract Machine Evaluation"){
The evaluation relation for the abstract machine is written @redAM, and
the rules for it are shown in @amRulesFig
}

The set of reduction rules in the machine is particularly tedious 
to read in mathematical
notation, but not difficult to understand using the usual memory layout diagrams,
which we present in the rest of the section.
@amRulesFig<-texAmRules{
  Abstract Machine Rules.
  The execution of the @ax_ rule merely involves updating the code in its closure 
  when the type where it operates becomes concrete (when it is not just a type variable). 
  This is done using rules
  shown in @axiomRedsFig, which we do not repeat here.
  The notation is as follows. The heap is prefixed by @math{H=}. To reduce clutter, we 
  do not name the parts of the heap which are untouched be rules, but use a wildcard (…) for them.
  Position in the heap can be named using the notation @math{name ↦}.
  A new cell is represented by a square (□). A deallocated cell is represented by a dagger. A tag 
  (for additives) is represented by either 0 or 1. A server (for exponentials) is represented by a 
  closure in braces, followed by a natural (reference count). A polymorphic value (for quantifiers) is
  represented by a type followed by a pointer to the monomorphic representation. The set of closures
  is prefixed by @math{C=}. A rule always acts on a single closure in the set and leaves the rest of
  the closures (ζ) untouched. A closure is represented by its code in braces, followed by an environment
  in brackets. The environment is a map of variables to positions in the heap. Arbitarary subsets of 
  an environment are represented by metasyntactic variables @math{γ,δ,ξ}.
  }

In each diagram, the set of closures is presented first (only the closures relevant to
the rule are shown, and it is always the case that any number of other closures can be
present and remain untouched by the rule). For each closure, its code is shown, followed 
by its environment. Each element of the environment is a pointer into the heap, represented
as an arrow pointing to some area of the heap, which is shown in the second row. 

We proceed to explain a few important ones (multiplicatives, additives, @offer_ and @demand_); 
the complete list of diagrams with explanation
can be found in appendix. 
To simplify diagrams, whenever an arbitrary context Γ can be handled, we write a single
variable pointing to a memory area of type Γ.

@texAmRulesExplanation[additives,multiplicatives,offerDemand]


@subsection{Adequacy}

We can show the adequacy of the abstract machine with respect to the @redBO reduction.

@definition("Proof to Machine"){
  One can map a proof to a machine with a single closure whose code point to the  
  program, and whose each variable in the environment points to 
  an area of the heap freshly allocated.
}

@theorem{Soundness}{
  If @math{a @bosonOper_  b} then @math{@toMachine{a} @redAM @toMachine{b}}.
  If @math{a @bosonBoson_ b} then @math{a = b}.
}{
By case analysis.
}

The boson-aware sequents are more fine-grained than the abstract machine:
some distinct sequents will be represented by the same state of the abstract machine.
For example, it is not possible to discover if multiplicative boson have 
interacted or not.

@definition("Machine to Proof"){
  To map a machine state to a proof, one can recover the cut structure
}

@definition{@equivAM}{
  @math{@equivAM = @equivAMDef_}
}

@theorem{Completeness}{
  If @math{a @redAM b} then @math{@fromMachine{a} @equivAM @fromMachine{b}}
}{
By case analysis.
}

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


@axiomOptSec<-paragraph{Optimising @ax_}
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


@paragraph{Truely concurrent language}

As we have seen, the coupling structure provided by linear logic is
is limited to tree topologies (even though we have seen that more
complex strucutres can be dynamically created, the language itself mandates
tree structures). 
This means for example that multiway communication between @math{n} processes
must be mediated by a central server routing the messages.

However, the proof of @noDeadlockThm suggests ways to construct logics allowing
graph structures while remaining deadlock free. For example: the connection by two edges
is possible if connection points are guaranteed to be ready at the same time.


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

Our presentation is faithful to the spirit of Girard's LL:
LL is already intuitionistic, and there is no need to restrict it
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
we have interpreted the type formers in these terms in @syntaxSec.

Another small improvement of this presentation over that of @citet{wadler_propositions_2012}
is that we have refined the notion of deadlock (shaving off liveness) in LL, 
underlining the strcutrual character of this property in LL.

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
the same sequent. (In a proof-net all hypotheses are at the bottom, and bosons are represented 
in a particular direction.)

@paragraph{Abstract Machines}
We identify two serious attempts to provide abstract machines for linear logic.

@citet{lafont_linear_1988} describes an abstract machine to evaluate proofs of
ILL. According to Lafont, CLL is more promising than ILL, but he chose to attack
ILL for simplicity. Another difference is that 
Lafont's machine is based on categorical combinators, while
we remain purely based on the syntax of CLL.

@citet{abramsky_computational_1993} presents a linear version of the 
Chemical Abstract Machine (CHAM) @citep{berry_chemical_1992}, which can handle
CLL. Abramsky's machine appears to be similar to the execution scheme we
outline in @outerSec. However, Abramsky's use of the CHAM's concepts
have some cost: he introduces a new syntax for proofs within the machine, and
a system of names is employed to link proof terms together.
This use of new concepts is superfluous: we are able to give an account of 
our AM which relies soley on concepts coming straight from LL syntax.


@section{Conclusion}

In our journey to build an abstract machine for linear logic, we have encountered
two generally useful concepts: coupling diagrams, and rules for asynchronous mediation
of communication. We have also shed a new light to some poorly understood
aspects of LL, such as the structural character of the multiplicative fragment. We
have shown that no communication is necessary to implement it.

@acks{This work is partially funded by some agency, which we cannot name for DBR.}

@bibliography

@cmd0("newpage")
@cmd0("appendix")

@section{Auxiliary reduction rules}



@syncFig
@pushFig

@bosonBosonFig<-texBosonBoson
@bosonOperFig<-texBosonOper

@section{Abstract Machine Reduction: all diagrams}

@texAmRulesExplanation(amRules)

@"