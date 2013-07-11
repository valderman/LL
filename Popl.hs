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
is explicit, and concurrent aspects of programs can be explicitly specified.

However, LL does not appear to be much used as such in the programming language
research community, at least in its fully general (classical) version. 
We hope to make classical linear logic more accessible 
by giving it a new computational presentation. In particular, we describe an 
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

There is a strong relationship between programs and proofs.
The correspondence is usually known as the Curry-Howard isomorphism, and goes
as follows:
@dm(array[]("cc")(map (map (cmd "text")) [
  ["Propositions", "Types"],
  ["Proofs", "Programs"],
  ["Verification", "Type-Checking"],
  ["Normalisation", "Evaluation"]
 ]))
In an effort to extend this correspondence, in particular to classical logic,
@citet{girard_linear_1987} has developed Linear Logic (LL). 
Linear Logic acts as a low-level logic, where both intuitionistic and
classical logics can be embedded. From its inception, LL
has been recognised as corresponding to concurrent programming
languages. Fleshing out this correspondence has given rise to 
a long line of work @citep{abramsky_computational_1993,wadler_propositions_2012,caires_linear_????,bellin_-calculus_1994}.

Additionally, as LL is a ``low-level'' logic, it corresponds
to a low-level programming language, where management of resources is
explicit. As such, we believe that a language based on LL
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

The versions Linear Logic featuring this distinction are labeled @emph{intuitionistic} (or ILL).
(When one wants to emphasize that inputs and outputs are unified one sometimes 
use the @emph{classical} label --- or CLL) However, as Girard points out, the classical version
is already grounded in computational intuitions, so the intuitionistic restriction
appears unnecessary. One of our aims is to explain the computational grounds
of CLL logic by using similar means as was used for explaining intuitionistic logics.

@citet{wadler_propositions_2012} has recently given an interpretation of 
CLL logic by reference to the π-calculus. However,
coming from a functional programming perspective, this explanation seems 
indirect. Our aim is find a natural programming language corresponding directly
to LL, in the same way as λ-calculi correspond to intuitionistic logics.
Furthermore, as much as possible, 
we desire a design which follows ISWIM conventions @citep{landin_next_1966},
in particular regarding syntax and the naming of intermediate values.
@comment{
%Too arrogant.
In sum we aim to fill the gap 
in the following table:
@dm(array[]("cc")(map (map (cmd "text")) [
  ["Simply-Typed λ-calculus", "Propositional Logic"],
  ["Polymorphic λ-calculus", "System F"],
  ["?", "Classical Linear Logic"]
 ]))}
As concrete steps towards this goal, we make the following contributions:
@itemize{
 @item A term assignment for classical linear logic proofs (@syntaxSec), with 
       a functional syntax inspired by ISWIM. We also show that a common graphical
       notation to express coupling between software componenents can be used as 
       a semi-graphical representation for CLL programs.
 @item We explain how standard concepts of λ-calculus evaluation 
       (redex,head normal form, etc.) generalize in linear logic (@outerSec).
 @item An abstract machine capable of running programs written in the above language (@amSec).
       This abstract machine combines the following desirable features: 
       concurrently running processes, 
       asynchronous communication, 
       the reduction in the multiplicative fragment is communication-less,
       and it is based on time-tested concepts, such has closures and heap.
 @item We show that the execution steps of the abstract machine correspond
       to proof normalisation steps. We do this by gradual refinement of cut-reduction for LL
       (@asyncSec and @redOMDef, @redAXDef, @redBODef, @redAMDef).
}

@syntaxSec<-section{Syntax and Intuitions}

This section presents the syntax of our language. The
syntax is functional, and suggestive of the operational behaviour.

@subsection{Types}

The types of our language are directly taken from classical linear logic.
They are shown in @tytab. The metasyntactic variables @tA, @tB and @tC range over types, and we use @alpha_ and @beta_ for type variables.

@tytab<-typeTable


The binary type formers all come with neutral elements. In @tytab the 
neutral element are written to the right of their respective type. For example, 
the tensor product has neutral element @One, which means that 
@tA @tensor_ @One ≅ @tA. 

We can provide immediately an intuition for how to understand these types.
Assuming a variable @vX in the context of a program, what can the program expect to obtain from @vX?
The answer depends on the type of @vX:
@itemize{
@item @id(tA ⊗ tB): both @tA and @tB. The program decides in what order to use them.
@item @id(tA ⅋ tB): both @tA and @tB. The environment decides in what order to use them.
@item @id(tA & tB): either @tA and @tB. The program chooses which one.
@item @id(tA ⊕ tB): either @tA and @tB. The environment chooses which one.
@item @id(Forall "α" tAofAlpha): @math{A[B]} where @tB is chosen by the program.
@item @id(Exists "α" tAofAlpha): @math{A[B]} where @tB is chosen by the environment.
@item @id(Bang tA): As many @id(tA)'s as the program wants.
@item @id(Quest tA): As many @id(tA)'s as the environment wants.
}
Two aspects become obvious from this enumeration. First, having a variable in the
environment is not all roses: it may provide something to the program, but it may
also generate an obligation. For example, in @id(tA & tB), the program @emph{must}
make a choice between @tA or @tB: ``I don't care'' is not an option. In essence, a variable
represents a contract between the program and its environment, given by its type.
Second, one can see that the environment and the program are on equal footing:
any constraint which can be imposed on an environment may be imposed on a program, 
and @italic{vice versa}. The dual of a type @tA is written @neg(tA), and computes
as follows:
@texNegationTable
(We only show half of the rule, the other half are demorgan duals.)
Even though we often write @tA@texNeg(False) for the negation
of a type, one should realise that dualisation is not represented in the syntax,
except for variables, in other cases it is evaluated as shown above. Hence the only concrete 
representation of duals in the syntax is for variables: we have both α and α⟂.
(When substituting a concrete type for α the dual computes further.)
As expected, dualisation is an involution: @math{(@tA^@Bot)^@Bot = @tA}.

Using the above syntax, the linear arrow can be defined as follows:
@tA @lollipop_ @tB = @id(tA ⊸ tB). That is, according the intuitions built
so far, having @math{x:A ⊸ B} means that if the program provides the environment with @tA,
it will get @tB in return. The processing order is be at the discretion of the environment.

@subsection{Terms and Typings}

We let metasyntactic variables @math{x}, @math{y} and @math{z} range over variables in our
language; @gamma_, @delta_ and @xi_ range over contexts. Contexts are unordered maps
of variables to types, and these mappings are written @math{x : @tA}. Variable
names in contexts are assumed distinct. Contexts are used to enforce linearity.
It is therefore important that when combining two contexts, the names in them
are distinct. For the same reason, variables are never implicitly dropped from a context.
Terms are defined by the following grammar, where @math{a,b} and @math{c} range
over terms.
@termFigure
(There is also an active right choice construction, which we consistently omit for reasons of space).

Name binding works as follows: in the two @connect_ constructs and in the @case_ 
construct, @math{x} is 
bound in @math{a} and @math{y} is bound in @math{b}. 
In all the @let_ constructs, all variables @math{x}, @math{z} and/or @alpha_ 
appearing to the left of the equals sign, are bound in @math{a}. Whenever a variable is
mentioned, it is no longer available for use in subcomputations, except for @alias_. For example, in the @ignore_
construct @math{z} is no longer in scope in @math{a}.

@rules<-typeRules
@rules shows the typing rules for our language. 
While the syntax suggests the operational behaviour, the rule names follow the convention 
of the linear logic literature. In particular, elimination rules are simply named after
the type constructor that they eliminate.

The convention usually used in linear logic literature is to have no
hypotheses, and many conclusions. This style is however difficult to fit with a
programmer's intuition of contexts and variables. Therefore, we choose to use the dual: 
our judgements have many hypotheses and no conclusion. The many hypotheses can be understood
as a usual context. The lack of a conclusion means that terms have no apparent
return type, but they can be thought of as returning 
@Bot. Indeed, ⊥ corresponds to a terminating value (not a diverging one!).

Similarly to other languages based on linear logic, ours is also a concurrent
language. Computation corresponds to communication over channels. Each variable 
in the context can be understood as a reference to one end of a channel, whose
type expresses the protocol employed on the channel. 
The judgement @derivation(simpleEnv) expresses that @math{a} communicates on the channels @math{x,y} and @math{z},
obeying respectively the protocols @tA, @tB and @neg(tC).
To express the same thing, we also use the following graphical representation:
we label a node with the program that it represents, and 
connect edges to it to represent the environment.
@simpleEnv
Two remarks are worth making.
First, the edges could be labeled explicitly with variable names, but in all
the examples we use, the types alone are enough to lift any ambiguity.
Second, the representation of @tA being in the environment of the program can be  
either an ingoing edge labeled with an edge @tA or an outgoing edge labeled 
with @neg(tA). 

@subsection{Structural Rules}

The @ax_ rule connects two channels and forwards information. The types of the 
channels must be duals, so that one channel provides what the other requires.
It is also the last instruction on the current thread: there is nothing happening after
the exchange.
@dm(couplingDiag axRule)
Channels in our language are one-shot, meaning that they are
only ever used for a single exchange, although that exchange can be arbitrarily
complicated and communicate in both directions. This means that once the
communication has happened the channel is eliminated (together with the corresponding @cut_,
at the greatest satisfaction of the logician).

@dm(derivation(cutRule))
The @cut_ rule creates a new channel with two ends, @math{x} and @math{y}, which 
are connected and have dual types. The channels are used in two separate threads, 
respectively running @math{a} and @math{b}. The threads run concurrently and in different,
disjoint contexts: any communication must occur via the new channel. Using our graphical convention,
we can represent @cut_ by an edge between nodes, and call such a representation a @emph{coupling diagram}.
@dm(couplingDiag(cutRule))
We emphasize that the direction of edges do @emph{not} represent the
direction of flow of data; they are a mere convention which allows to
know which side of the arrow interprets the label literally, 
and which side interprets it as its dual.

The @cut_ and @ax_ rules are deemed structural, because they represent the backbone of
the communication structure of the program. A given communication structure can be 
represented in many ways by these rules (we detail this point in @outerSec), all equivalent
under the relation (@math{≡}) shown in @structEquivFig: @cut_ is commutative, associative, and @ax_ can be  
inserted or deleted at every @cut_ without changing the meaning of the program.
Besides being more concise than derivations, coupling diagrams have the advantage that
equivalent programs are represented by 
topologically equivalent diagrams.
Consider for example the following derivation (we omit some terms to reduce clutter):
@dm(sequent(doubleCut'))
which it is represented by
@dm(couplingDiag(doubleCut'))
For every binarisation of the tree, program equivalent to the original can be constructed.
@structEquivFig<-structFig

@outermostCut<-definition("Outermost Cut"){
An instance of a @cut_ rule in a derivation tree is called an outermost cut if
it is either the first rule in a derivation or if it is an outermost cut in a subderivation of
an outermost cut.
}
As we have seen on the above examples, all outermost occurrences of
@cut_ can be represented graphically. We remark right away that, 
because each @cut_ connects two subgraphs by exactly one edge, the coupling structure 
of outermost cuts is necessarily a tree.


@subsection{Operational Rules}

The rest of the constructions transmit or receive information along the channels laid out
by @cut_. We group them all in a class which we call @oper_ rules, which includes (contrary to convention) @weaken_ and @contract_.

Thanks to duality, CLL is economical: we need no constructors in the language,
eliminators suffice. Indeed, constructing a value
is implemented by eliminating its dual, and connecting that eliminator to some other 
part of a program via @cut_ provides the type. (An example is given in @exampleSec.)

The operational semantics of our language is given by the reduction rules of
LL, where two @oper_ rules are connected by a @cut_ reduce.
From a programming language point of view we can understand this reduction
as synchronous communication occurring between processes. All these reduction rules are listed in @redFig, 
and we place them in a class called @operationalRules_.
@redFig<-syncFig

@paragraph{Additives}
The @case_ construction eliminates the type @id(tA ⊕ tB), waiting for a choice to be
made between @tA and @tB. Conversely, the rule @math{&_1} chooses protocol @tA (Symmetrically
rule @math{&_2} chooses protocol @tB; and we shall not mention this variant again.)
The reduction of a @cut_ between these two rules correspond to sending the choice (a bit of info)
over the channel, yielding a connection on @tA between the direct subprograms.

The @Zero type corresponds to a choice which cannot be made: there is no rule eliminating its dual @Top,
so there can never be an active connection involving @Zero.
Accordingly, the elimination rule for @Zero 
can never trigger at runtime, so it can safely be interpreted
as crashing the machine. The @gamma_ in the @dump_ construct is there for 
formal reasons: every variable need to be used exactly once. 

@paragraph{Multiplicatives}
A connection between the multiplicative processes @programOneLine(leftChild cutParCross) and @programOneLine(rightChild cutParCross) 
can be represented as follows (For concision, we label nodes only with the first rule of the program):
@dm(couplingDiag(cutParCross))
The reduction is not so much communication as splitting the
channel and forking off a new thread. No information as such is transmitted
during reduction.
Noteworthy is that the program @math{c} has complete freedom regarding the order in which  @math{x} and @math{y}
are used. This means that, conversely, the @par_ rule must be able to honour 
any order whatsoever between the subchannels. This is indeed enforced by having
those two parts handled by separate processes, which can communicate only via
the subchannels, as can be seen on the diagram of the reduct:
@dm(couplingDiag(eval cutParCross))


The multiplicative units are @Bot and @One, and the corresponding communication corresponds 
to notification of the termination of a process (@programOneLine(botRule)).
(In @asyncSec, we do away with this unnecessary synchronisation.)

@paragraph{Quantifiers}
The @forall_ and @exists_ rules deal with channels of polymorphic and 
existential types. The rule @forall_ instantiates a polymorphic channel with a 
particular type @tB which means sending the type along the channel. The type is 
received in the @exists_ rule where the type is given the name @alpha_.

@paragraph{Exponentials}
Recall that the exponential type @Bang(tA) represents a number of copies of @tA, and 
the number is chosen by the program. 
Hence, the eliminator for the dual (the ? rule) is at the mercy of its environment: it provides
a protocol which can be demanded an arbitrary number of times (a @emph{service}).
This means in particular that the service provider (?) has no choice but
to depend on protocols which are themselves services. 
(Formally, the context in the ? rule appears with a ! in front of it. 
This notation means that all the types in the context must be prefixed with !.)

The !-eliminator derelicts the service to a mere protocol, restoring symmetry with the connected process. 
The @weaken_ rule declines to use the service, discarding the connected process,
and the @contract_ rule takes a copy of the service, forcing to request 
a copy of every service it required by the connected process.

@paragraph{Commuting Conversions}
The rules presented so far do not allow the elimination of every @cut_. To do so
one needs to push a @cut_ inside an @oper_ rule, if the @oper_ rule does not operate on the channel
introduced by the @cut_. (A so called commuting conversion.) 
While the addition of commuting conversions gives @cut_-elimination for LL, they
do not correspond closely to evaluations of concurrent processes, and therefore
we take a different route, beginning in @outerSec.

@subsection{Relation to CPS}

The language presented above corresponds closely to functional programming in 
continuation passing style
(CPS). This connection should come as no surprise: linear logic is a "low-level"
logic which can be used to represent or explain other logics. Similarly, CPS
has been used as a low-level language to compile and optimize programs 
@citep{appel_compiling_1992}.

Most terms in our language contain a subterm, a continuation, which is invoked 
after the term is evaluated. Just as in CPS, expressions are evaluated for 
their side-effect, not to return a value.
On the type level, this is reflected by the fact that terms do not have a 
return type, or, as noted above, implicitly return ⊥. This is corresponds to the
@emph{answer type} in CPS @citep{thielecke_control_2003}.

@exampleSec<-subsection{Example}

To illustrate our language we give an example program, which 
swaps the values in a tensor product.
A natural approach for functional programmers is to start with the type 
@swapType and try to inhabitate it. 
However, programs in our language all have type ⊥. We have then to take an approach
similar to CPS and invert the flow of control: we assume a variable swappee of type 
@MetaNeg(swapType) = @neg(swapType), and eliminate it.
@displayMath(programWithCtx swap)

The swap program invokes the swapee to
retrieve the channels to swap and reorders them accordingly. In order to type
our swap program the swapee occurs in the context and have a type which is dual
to the type above @math{(@id(swapType))^⊥}. 
No programmer in their right mind should want to write programs in the above style.
Indeed, we intend our language to be used as a low-level, intermediate representation
and the bulk of concrete programs should be written using syntactic sugar mimicing direct style.
(The desugaring follows closely
the translation from direct to continuation-passing style.)

@outerSec<-section{Synchronous Outermost Reduction}

In this section we show how to interpret the @operationalRules_ and structural equivalences 
to obtain an evaluation strategy similar to standard evaluation strategies of the
λ-calculus. The strategy is expressed as is a small-step reduction relation, 
which corresponds to synchronous communication of ready processes.
@redOMDef<-definition("Outermost Evaluation"){
  We define outer evaluation (@redOM) by taking the union of
  the operational reduction rules (@operationalRules_), 
  their swapped version (@math(swapped operationalRules_)), 
  and the @cut_-congruences of these rules:
  if @math{a @redOM a'} and @math{b @redOM b'} then 
  @dm(sequent(simpleCut' "a" "b") <> redOM <> sequent (simpleCut' "a'" "b'"))

  In summary,
  @math(redOM <> " = " <> redOMDef_).
}

The remainder of the section is devoted to showing that this  strategy of evaluation 
is well-behaved, and corresponds to usual evaluation strategies of the λ-calculus.

@definition("Waiting on a variable"){
We say that a program is @emph{waiting} on a variable @vX in its context if:
@itemize{
 @item it is an axiom, or
 @item it starts with an @oper_ rule acting on @id(vX).
}}
We can make the notion of edge in a coupling diagram formal:
@definition("Edge"){
An edge is either an outermost @cut_ or a variable in the top-level context (an hypothesis).
}
@definition("Ready edge"){
We call an edge @emph{ready} if all programs connected to it are waiting on the variables connected 
to it. 
}
So in particular, 
an hypothesis edge is ready if the single node  connected to it is  waiting on it,
and a @cut_   edge is ready if both       nodes connected to it are waiting on it.
Remark that a ready @cut_ is reductible.

@noDeadlockThm<-theorem("No deadlock"){
           In every program, there is always at least one edge ready.
}{
The proof uses the coupling diagram representation of the program, where all outermost cuts are represented.
Remark first that every node begins either with an @oper_ rule or an axiom. Therefore, every node is waiting 
on at least one variable.

Because @cut_ links to subdiagrams by exactly one edge, the coupling diagram must be a tree.
We proceed by induction on the size of that tree. If the tree has a single node, then
all the edges are hypotheses. The node must be waiting on one of them, which is then ready.

For the inductive case, we assume two graphs @graph1 and @graph2 satisfying the induction hypothesis, 
with an hypothesis @vX in @graph1 and an hypothesis @vX' in @graph2.
We now show that the system obtained by connecting @vX and @vX' satisfies the theorem.
We have the following cases:

@enumerate{
@item @graph1 is waiting on @vX and @graph2 is waiting on @vX'. Then the new edge is ready.
@item Either system is not waiting on the designated hypothesis. In this case, 
due to the induction hypothesis, some other
   edge in that system must be ready, and it remains ready in the combined system.
@qedhere
}}

Perhaps surprisingly, the freedom from deadlock does not depend on the specifics of evaluation rules,
but on their broad structure: namely that each rule is ready to interact on at least
one variable, and crucially that the coupling structure is a tree. 


@livenessThm<-theorem("No Starvation"){
If the @cutAx_ rule is never used to create a @cut_ (in the right to left direction),
there is no infinite chain of reduction steps. This means that 
outermost evaluation eventually yields a program waiting on one of the variables of its environment.
In other words: every process eventually provides its environment with what it asks.
}{
The theorem is a corollary of a classical result: cut-elimination in LL. 
One can re-do the proof based on the observation that the products of @cut_ reduction are smaller than 
the initial @cut_. Two cases are non-obvious. First, the @offer_/@contract_ rule creates syntactically
bigger trees, but the resulting contractions happen on smaller trees. Second, because of
impredicativity, some extra care is necessary to ensure termination @citep{gallier_girards_1989}. 
}

In sum, the above theorems mean that LL can be run in a way similar
to usual ways of running the λ-calculus. In fact, the correspondence is deep: we can draw 
parallels between both sides for every concept used by the respective evaluators.

A ready @cut_ corresponds to a redex. In both cases, the reduction of a top-level
@cut_ (or redex) involves in-depth rewriting of the term, which is costly and does not correspond
to the notion that programs are static entities. In both cases, it is possible to delay the 
elimination of a @cut_ up to the point where direct interaction occurs.
A ready edge corresponds to a λ-calculus redex in head position. 
Inner cuts correspond to redexes under λ. @oper_ rules (minus exponentials) 
correspond to constructors.
A λ-term in head normal form corresponds to a linear program in which all hypotheses edges are ready.

The behaviour of the call-by-name abstract machine of @citet{krivine_call-by-name_2007} is to traverse
the spine of applications inwards and leftwards until it finds a redex, then reduce it. 
(The call-by-value SECD machine of @citet{landin_mechanical_1964} behaves likewise, but evaluates arguments first.)
The reduction yields then another redex in the same position, or one must continue the traversal inwards and leftwards.
An abstract machine for linear logic must traverse the coupling structure, potentially considering all outermost
cuts, to eventually find a one which is ready, and reduce it. The difficulty in the
linear/concurrent case is that one cannot @italic{a priori} know where the ready cut is located. Furthermore,
the next ready cut may be far away in the term, so a working list of potentially ready cuts must 
be maintained.

In this light, one understands commuting conversions as the ability to bring a waiting 
communication primitive to the front of a term --- a concept which is not meaningful in  
λ-calculus.

The execution strategy outlined above is a direct generalisation of
classical execution strategies for λ-calculi.
 However this evaluation
mechanism has an important shortcoming for the interpretation of
LL as concurrent processes. Namely, if one thinks of a node as a process, 
then every communication is synchronous. Indeed, when a reduction rule
fires, the processes at both ends change state simultaneously.
This style of synchronous communication is unfortunate for two reasons:
@itemize{
@item Processes writing to a channel 
      typically can proceed without waiting 
      for acknowledgement of the reader. Using this kind asynchronous 
      communication enables more concurrency.
@item Our interpretation of the multiplicative fragment is that no 
      communication occurs at a reduction point. Hence implementing it with synchronisation 
      appears twice as wasteful of concurrency opportunities as in other cases.
}

We attack this shortcoming after taking a brief detour.

@subsection{@mix_ and @bicut_}

The @cut_ rule allows two processes to communicate via exactly one channel. 
Variants of the rule allowing zero (@mix_) two (@bicut_)
channels have been proposed.
  @braces(
  cmd0"small" <>
  mathpar[[
  frac(@"Γ⊢ @hspace("2em") Δ⊢ @")(@"Γ,Δ⊢ @") <> mix_,
  frac(@"Γ,A,B⊢ @hspace("2em") A⟂,B⟂,Δ⊢ @")(@"Γ,Δ⊢ @") <> bicut_
  ]])


The @mix_ rule has been proposed by 
@citet{girard_linear_1987}, and is a safe extension. Indeed, the proof of @noDeadlockThm remains
valid: @mix_ corresponds to placing coupling diagrams side by side with no connection, which 
preserves ready edges. However, @bicut_ @citep{abramsky_interaction_1996}
is not safe: if two edges are created, then it is possible to connect the only ready edge in each system
to another hypothesis in the other one, and no ready edge remains in the result.

@asyncSec<-section{Asynchronous Outermost Reduction}

@axiomSec<-subsection{Explicit Axioms}

Conceptually, an axiom does nothing:
as we have seen in @syntaxSec, a @cut_ with an axiom is equivalent to just a @cut_ link.
However, merely removing axioms and adapting links is not an option if we want processes to
behave asynchronously: the adaptation of links requires two-way synchronisation.

However, in the same way that one can implement polymorphic identity by explicit copy 
of data, one can implement axioms by processes which explicitly forward messages. 
That is, for the additive fragment, one can do case analysis on one 
side, and send the information of which side is taken to the other side.
For the multiplicative fragment, one can explicitly split the axiom into a pair of axioms,
etc. The complete set of rules implementing this idea is show in @axiomRedsFig and called @explicitAxiom.

@axiomRedsFig<-texAxiomReds

Replacing the @cutAx_ rule by explicit forwarding does not change the meaning of programs.
It means however that @ax_ rules disappear only when they apply to unit types. 
This is problematic operationally (we discuss this issue in @axiomOptSec), but not 
for formal purposes, because @cutAx_ is an equivalence.

@redAXDef<-definition{}{
@math{@redAX = @redAXDef_}
}

@theorem(""){
 For any terms @math{a,b}, @hspace("1em")  @math{a @many(redOM) b} iff @math{a @many(redAX) b}.
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
A novel characteristic is that we perform this insertion of buffers within the framework of linear logic,
merely adding new rules, and replacing the standard (synchronous) reduction rules by another 
set of reductions which all involve an intermediate buffer.

The idea is that buffers mediate the interaction between
the @oper_ rules. For example, when a @with_ rule is connected
to a @plus_ rule via a channel, the @with_ rule writes a bit of information
to the channel indicating which side of the sum to take. The continuation of @with_ is 
ready to run regardless of a @plus_ rule being ready to read. 
The @plus_ rule reads the bit of info asynchronously as soon as 
it becomes available. This execution process can be depicted using coupling diagrams as 
follows:
@dm(couplingDiag $(cutWithPlus True))
@dm(couplingDiag $ (eval' $ cutWithPlus True))
@dm(couplingDiag $ eval $ eval' $ cutWithPlus True)
The intermediate rule can be metaphorically seen
as a particle traveling from left to right. 
By analogy with the elementary particles mediating physical 
forces, we call such mediating rules bosons.
In diagrams, we write them without a circle around them.
The above diagrammatic representation of the & boson suggests to
implements it as the rules
@mathpar[map (sequent . chanPlusRuleBad) [True, False]]
However, we make another choice: to represent it as a rule with the
same premise and conclusion as @with_, and merely consider the premise
to be ready to run in our execution model (allow evaluation under that rule). 
@mathpar[map (sequent . withRule True) [True, False]]
In this sense, it is as if
the new rule had a structural aspect to it (it embeds a virtual cut).
This feature is made explicit in the diagram by drawing the virtual cut
with a dotted edge.

For the quantifiers fragment (see figure @bosonsFig), a similar boson and set of reduction rule exists. The
only difference is that a type is being transmitted instead of a bit.

For the multiplicative fragment, we consider two possible ways to encode asynchronicity,
to eventually settle on the second one.
The first option is modeled after the same pattern as the additive fragment: a ⅋ boson travels
from the @par_ rule to the @tensor_ rule. In this case however, on its 'left hand side' the 
boson must connect two processes. The downside of this approach is that, until the
@par_ rule is ready, the @tensor_ must wait. This is sub-optimal because the @tensor_ rule
does not actually need to wait for any information: the behaviour of the continuation does
not depend on anything that the @par_ rule @emph{itself} provides. In fact, one might
just as well imagine that a ⊗ boson should travel in the other direction, 
from @tensor_ to @par_.

This observation leads us to a second and, in our opinion, preferable option to
model asynchronicity. The solution is to send both bosons, and add a reduction rule between them.
This means @par_ and @tensor_ behave asynchronously: as soon as they are
encountered their continuations are ready to run.

@dm(couplingDiag(cutParCross))
@dm(couplingDiag(eval' $ cutParCross))
@dm(couplingDiag(eval $ eval' $ cutParCross))

If we had chosen to represent bosons as rules a without structural component
(eg. separated the @cut_ from the & boson as considered above), then the 
emission of the ⊗ boson would have required the use of @bicut_. This requirement  
explains our preference for the alternative, where the ⊗ boson has the
same structure as the @tensor_ rule.

The buffer for exponentials is different from the others: it does not merely hold data
which is to be consumed one time, but many times. Hence, it is more proper to see it as
memory storage rather than a buffer. Technically, the 
behaviour of exponentials borrow concepts from both additive and multiplicative fragments. 
In a fashion similar to additives, a 'ready to run' boson (called @math{M} for memory) 
propagates from @offer_ to @demand_. That is, @demand_ does not send a boson, 
only receives one. This boson signals that the server obeying protocol @tA is ready 
to run, by storing its closure in the memory. Its absorption corresponds to spawning
an instance of the server process.

The @contract_ rule behaves similarly to @tensor_: it sends a boson (@math{Con}) whose effect is to
create a pointer to data. However, a difference in this case is that both new pointers
point to the same thing, the pointer is duplicated. 
Consider the following program as an example, where a server
is connected to a client, which we know actually demands the service at least once.
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
service. This is supported by the @math{M} boson, which can connect to multiple clients.
Hence, the interaction between bosons yields a system which is represented by the following
diagram:
@dm(couplingDiag $ eval' $ eval' $ exponentialSimple)
Eventually, the client will start interacting with an instance of the server (@demand_ rule). This is done by 
duplicating its closure @math{a}, which is represented as follows. 
@dm(couplingDiag $ eval' $ eval' $ eval' $ exponentialSimple)
The new process needs to access the environment !Γ, which can be accessed by copying pointers
to it, thanks to the @math{Con} boson. 
The @weaken_ rule behaves in a manner similar to @contract_.

Because exponentials are less regular than the rest of the 
system, they require a more @italic{ad hoc} implementation, and multiple implementations 
are possible. Our choice of implementation is justified by our desire to represent the 
exponential channel as a closure to a server which can be pointed at by multiple clients.
This concludes our description of bosons, whose complete list is shown in @bosonsFig. 
The @bosonBoson_ and @bosonOper_ rules are shown in the appendix(@bosonBosonFig and @bosonOperFig).
To sum up, the asynchronous reduction, explained above, replaces the @operationalRules_ rules from
the synchronous reduction by
boson emission and reception (@bosonOper_ and @bosonBoson_) interactions.
The @boson_-concruences are also available in this relation.

@bosonsFig<-texBosons

@redBODef<-definition{}{
  @math{@redBO = @redBODef_}
}

The full asynchronous reduction is a refinement of the synchronous reduction with asynchronous
axioms:

@theorem(""){
If @math{a @redAX a'} then @math{a @many(redBO) a'}
}{
  By case analysis of each of the @operationalRules_.
  For additive, multiplicative and quantifiers fragments, this is immediate because the structure of
  bosons is the same as the original structure. 

  For exponentials, one must use the memory boson in intermediate states.
}


@theorem(""){
If @math{a @many(redBO) a'} and neither @math{a} or @math{b} contains a boson then @math{a @many(redAX) a'}.
That is, for each of the reductions using an intermediate state containing bosons, it is possible to find a direct
route.
}{
By case analysis, as above. 
For additive, multiplicative and quantifiers fragments, there is only one way to consume an intermediate boson, 
so the proof needs to take into account only one boson (pair) emission. For exponentials, one needs another induction
to deal with the fact that any number of @contract_ and @ignore_ bosons must be considered.
}


@amSec<-section{Abstract Machine}

In this section we describe an abstract machine for execution of LL programs. The
machine follows closely the refined reduction relation presented in the previous section.
It is similar in spirit to classical abstract machines for the λ-calculus, such as the 
SECD machine.

The machine state is composed of a multiset of closures and a heap.
The @oper_ nodes from the above section are represented by a closure, and the bosons are represented by the heap.
A closure is a triplet of a program (whose syntax is given in @syntaxSec),
an environment associating each variable
to a pointer in the heap, and a type-environment associating each type variable to a
type representation. Each closure corresponds to a process, and each variable
in its environment corresponds to a port of a channel.
One step of execution of a closure corresponds to sending or receiving a boson, except for structural rules: @cut_ 
spawns a process and @ax_ is handled as outlined in @axiomSec. Every execution step is
 implemented by interacting with the heap: no synchronisation primitive is assumed. 

The heap is an indexable sequence of cells. Each cell can eventually be used to 
transmit some piece of information between closure. Each cell starts its lifetime
as empty (devoid of information). It may then be populated with some information, which will
be read if another process is connected to the corresponding channel.
Then the cell is deallocated. (In a real system is should made 
available for reuse, but we do not describe this detail in this presentation.)

A number of contiguous cells are allocated for each channel in the heap. 
For a channel of type @tA, @norm(tA) cells are allocated.

@definition("layout"){
  The layout function maps a type @tA and a type environment @rho to a number of cells @norm(tA).
  @dm(layoutTable)
} 
There are exactly two closures pointing to each channel of non-exponential a type @tA. One
of these considers the channel as @tA and the other as @neg(tA), which 
justifies @norm(tA) = @norm(neg $ tA).
Neither @Zero nor @Top can ever occur in the execution of a program.
We emphasize this fact by assigning them an infinite number of cells.

@subsection{Reduction rules}

Each rule in the @bosonOper_ category involves exactly one rule of the original logic. Hence
each initial rule in a program can be interpreted as an instruction of the machine, whose
meaning follows the asynchronous reduction relation. Rules in the @bosonBoson_ category, on the other
hand, correspond to structural properties of the heap. For example, the interaction between
the ⊗ and ⅋ boson correspond to the property that the components of ⊗ or ⅋ types are
laid out in sequence in the heap. Hence, there is no computation associated with them in
the machine. Lastly, the commutativity and associativity of @cut_ correspond to the 
structure of the multiset of closures.

@redAMDef<-definition("Abstract Machine Evaluation"){
The evaluation relation for the abstract machine is written @redAM, and
the rules for it are shown in @amRulesFig
}

The set of reduction rules for the machine is particularly tedious 
to read in mathematical
notation, but not difficult to understand using usual memory layout diagrams,
which we present in the rest of the section.
@amRulesFig<-texAmRules{
  Abstract Machine Rules.
  The notation is as follows. The heap is prefixed by @math{H=}. To reduce clutter, we 
  do not name the parts of the heap which are untouched be rules, but use a wildcard (…) for them.
  A position in the heap can be named using the notation @math{name ↦}.
  A new cell is represented by a square (□). A deallocated cell is represented by @dagger_. A tag 
  (for additives) is represented by either 0 or 1. A server (for exponentials) is represented by a 
  closure in braces, followed by a number (reference count). A polymorphic value (for quantifiers) is
  represented by a type followed by a pointer to the monomorphic representation. The set of closures
  is prefixed by @math{C=}. A rule always acts on a single closure in the set and leaves the rest of
  the closures (ζ) untouched. A closure is represented by its code in braces, followed by an environment
  in brackets. The environment is a map of variables to positions in the heap. Arbitrary subsets of 
  an environment are represented by metasyntactic variables @math{γ,δ,ξ}.
  The execution of the @ax_ rule merely involves updating the code in its closure 
  when the type where it operates becomes concrete (when it is not just a type variable). 
  This is done using rules shown in @axiomRedsFig.
  }

In each diagram, the set of closures is presented first (only the closures relevant to
the rule are shown, and it is always the case that any number of other closures can be
present and remain untouched by the rule). For each closure, its code is shown, followed 
by its environment (the type environment is omitted). 
Each element of the environment is a pointer into the heap, represented
as an arrow pointing to some area of the heap, which is shown in the second row. 

We proceed to explain a few interesting rules: multiplicatives, additives, @offer_ and @demand_.
The complete list of diagrams with explanation can be found in appendix. 
To simplify diagrams, whenever an arbitrary context Γ can be handled, we write a single
variable pointing to a memory area of type Γ.

@texAmRulesExplanation[additives,multiplicatives,offerDemand]

@subsection{Adequacy}

The abstract machine adequately models the @redBO reduction, up to @bosonBoson_ reductions.

One can map a program @math{a} (possibly starting with bosons)
to a machine state @math{@toMachine{a}}
with a single closure whose code point to the 1st non boson rule in the
program, and whose each variable in the environment points to 
an area of the heap freshly allocated. If the program @math{a} starts with a 
additive or quantifier boson, the contents is written at the appropriate place in
the heap. If it starts with a @contract_ bosons, the pointer is duplicated, etc.

The asynchronous reduction (@redBO) is more fine-grained than the abstract machine:
some distinct states containing bosons are represented by the same state of the machine.
@theorem{Soundness}{
  If @math{a @space @stackrel(bosonOper_)(doubleArrow) @space b} then @math{@toMachine{a} @redAM @toMachine{b}}.
  If @math{a @space @stackrel(bosonBoson_)(doubleArrow) @space b} then @math{@toMachine{a} = @toMachine{b}}.
}{
By case analysis.
}

Conversely, one can map a machine state σ back to a program @math{@fromMachine{σ}}. Firstly, the outermost
coupling structure can be recovered by analysing which closures point to which memory
areas. Second, if the heap contains non-empty cells, these can be represented by 
bosons associated with the appropriate closure.

@theorem{Completeness}{
  If @math{σ @redAM σ'} then @math{@fromMachine{σ} @many(redBO) @fromMachine{σ'}}
}{
By case analysis.
}

Together, the theorems 3 to 7 show that our abstract machine is sound and complete
with respect to the outermost evaluation relation.

@section{Discussion}

@paragraph{Quantifiers}
Our implementation of type-variables and quantifiers may be surprising. 
It might seem natural to store a value of a type variable α in
a single cell, at it is usual in functional programming languages,
instead of having to lookup it size in an environment.

Our choice is dictated by our not wanting to box every value. This choice is consistent
with the view that a linear programming language is low level, and
that pointers should be introduced by exponentials or polymorphism only. 


@axiomOptSec<-paragraph{Optimising @ax_}
To simplify presentation, we have made our implementation of @ax_ less 
efficient than it could be. It appears wasteful to have a process which
copies data around, while this data is guaranteed to be produced and consumed
exactly once. Indeed, it is possible to optimise axioms as follows. Consider
first the exponentials and quantifiers. The current implementation of @ax_ 
follows a pointer on one side, and re-creates an indirection the 
other side and recursively forward data between the pointed zones. Instead, 
one can simply directly copy one pointer and be done. Consider second additives. Instead
of transmitting a single bit, one could have as a possibility to transmit a pointer
to a memory area. In the @plus_ rule, if this pointer is read, then the
process proceeds with reading the bit from the pointed area. The implementation
of axiom can then, instead of copying a potentially large amount of data, send a 
pointer to the source area and terminate immediately.

@paragraph{Truely concurrent language}

As we have seen, the coupling structure provided by linear logic
is limited to tree topologies (even though we have seen that more
complex structures can be dynamically created, the language itself mandates
tree structures). 
This restriction implies for example that multiway communication between @math{n} processes
must be mediated by a central server routing the messages: peer to peer communication is 
forbidden.

However, the proof of @noDeadlockThm suggests ways to construct logics allowing
graph structures while remaining deadlock free. For example: the connection by two edges
is possible if connection points are guaranteed to be ready at the same time.

@subsection{Future Work}

@paragraph{Taming concurrency costs}
The language we have presented here is fully concurrent,
implying that communication occurs at the bit level. Such a fine-grained communication is 
wasteful: in real applications data is transmitted in larger chunks. In
future work, we to allow the programmer to opt out of concurrency.

Another consequence of full concurrency is that a process is spawned at every 
occurrence of @par_ and @cut_. However, in many useful cases, one should be able to discover
that data flows in a particular direction (for example when the code comes from the
translation of a functional program into LL). Again, it appears to be possible to support
this optimisation by opting out of concurrency in relevant cases.
The only apparent issue is with polymorphism: one cannot opt out of concurrency
in the presence of quantification over
arbitrary protocols. The obvious solution, which we plan to investigate in future work, is to 
add a construction for quantification over unidirectional protocols (pure data).

@subsection{Related Work}

@paragraph{Systems based on intuitionistic variants}

Most presentations of LL for functional programming dualise the logic
@citep{hyland_full_1993,barber_dual_1996,benton_term_1993,caires_concurrent_2012}. 
That is, every rule introduces a type former either on the left-hand-side or
on the right-hand-side of the turnstile. 

An issue with this version of LL is that ⅋ does not have the same properties 
as Girard's version of it @citep{hyland_full_1993}. Furthermore, 
calling them intuitionistic variants is misleading: classical LL already has good 
computational behaviour.

Our presentation is faithful to the spirit of Girard's LL:
LL is already intuitionistic, and there is no need to restrict it
to give it computational content.

@paragraph{Session Types}

A correspondence has recently been identified between linear logic
propositions and session types @citep{wadler_propositions_2012,caires_linear_????,caires_concurrent_2012}.
A proof of a proposition @tA can be identified with process following the session protocol @tA. 

The calculus associated with interpretation of propositions as sessions is close to the
π-calculus, which we observe is unintuitive to functional programmers
in two respects. On a superficial level, we have anecdotal evidence that 
they much prefer ISWIM-like
syntaxes. On a semantic level, founding computation on the transmission of channel names
departs fundamentally from the tradition of functional programming.

Further, the correspondence between types and sessions
departs from the usual linear logic in that the type @element(tA ⊗ tB) is
interpreted as @tA @emph{then} @tB, whereas the usual interpretation of the
linear formula is symmetric. (Our interpretation
keeps the symmetry intact.) 
Another reason to interpret the multiplicative fragment as we do
is one of efficiency. While @citet{wadler_propositions_2012,caires_linear_????} 
interpret the multiplicative fragment as
the transmission of a channel name, which is fine from a logic and
π-calculus perspective, this interpretation does not computationally efficient.
We eventually wish to develop a low-level,
efficient linear programming language
based on framework laid out in the above. 
In such a language, one typically represents large arrays by commensurably
large tensors. With the synchronous view of multiplicatives, this would mean
that the layout of an array itself has to be transmitted by @par_, and that @tensor_
cannot proceed until it has received the blueprint. 
To avoid sacrificing parallelism opportunities, one must go with the asynchronous view.

Another small improvement of our presentation over that of @citet{wadler_propositions_2012}
is that we have refined the notion of deadlock, separating it from starvation,
which allows us to underline the structural character of absence of deadlock in LL.

@paragraph{Graphical representations}

Graphical representations of proofs in linear logic abound.
Besides the proof nets of Girard, one find many categorically motivated 
representations
@citep{cockett_proof_1997,hirschowitz_topological_2008}.

Our representation is motivated by simplicity: it aims to represent the 
outermost @cut_ structure only. (The reification of a proof from a diagram
has only to choose where to cut.) Second, we believe that it is intuitive.
Indeed, it is close to the component diagrams routinely used by software engineers.
@comment{a principal difference is that in our diagrams the protocol between components is fully formalised by a type,
instead of being an informal reference to some interface.}

Our diagrams are also closely related to proof nets. Indeed,
proof nets work best in multiplicative fragment, and in this fragment, 
all possible bosons can be emitted. 
If one writes the coupling diagram with all such bosons represented, 
it is topologically equivalent to the proof net for
the same proof. (The proof-net is laid out differently: 
all hypotheses are at the bottom, and bosons are oriented
with the composite edge down.)

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
outline in @outerSec. However, Abramsky's use of the CHAM concepts
have some cost: he introduces a new syntax for proofs within the machine, and
a system of names is employed to link proof terms together.
This use of new concepts is superfluous: we are able to give an account of 
our AM which relies solely on concepts coming straight from LL syntax.

@section{Conclusion}

We have described an ISWIM-like language based on CLL, and abstract machine capable
of running it.
In our journey to derive the abstract machine from the syntax, we have encountered
two generally useful concepts: coupling diagrams, and rules for asynchronous mediation
of communication. We have also shed a new light to some poorly understood
aspects of LL, such as the structural character of the multiplicative fragment: we
have shown that no communication is necessary to implement it.

@comment{@acks{This work is partially funded by some agency, which we cannot name for DBR.}}

@bibliography

@cmd0("newpage")
@cmd0("appendix")

@section{Auxiliary reduction rules}

@syncFigLong
@commutingConvFig<-pushFig

@bosonBosonFig<-texBosonBoson
@bosonOperFig<-texBosonOper

@section{Abstract Machine Reduction: all diagrams}

@texAmRulesExplanation(amRules)

@"

-- Local Variables:
-- ispell-local-dictionary: "british"
-- End:

--  LocalWords:  GHC XTypeFamilies XTypeSynonymInstances XRecursiveDo
--  LocalWords:  XOverloadedStrings pgmF marxup Popl MarXup Monoid dm
--  LocalWords:  TexPretty GraphViz PaperData acmCategories cmdn cmd
--  LocalWords:  acmKeywords mconcat env Landin's maketitle fmt citet
--  LocalWords:  outputTexMp MPOutFormat renderToDisk latexDocument
--  LocalWords:  Normalisation girard recognised citep abramsky emph
--  LocalWords:  wadler caires bellin priori landin syntaxSec redex
--  LocalWords:  redAXDef redBODef redAMDef tytab metasyntactic tC tA
--  LocalWords:  typeTable vX tB demorgan
