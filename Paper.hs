{-# OPTIONS_GHC -XTypeFamilies -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}

module Paper where

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
-- import Mem
import Framework
import PaperData ()

preamble :: Bool -> Tex ()
preamble inMetaPost = do
  stdPreamble
  usepackage "cmll" [] -- for the operator "par"
  mathpreamble
  cmd "input" (tex "unicodedefs")
  -- unless inMetaPost $ do
    -- usepackage "dot2texi" []
    -- usepackage "tikz" []
    -- cmd "usetikzlibrary" $ tex "shapes,arrows"
    
  title "Linear Logic: I see what it means!"
  authorinfo Plain [("Jean-Philippe Bernardy","bernardy@chalmers.se",ch),
                      ("Josef Svenningsson","",ch)]
 where ch = "Chalmers University of Technology and University of Gothenburg"



rul s s' = displayMath $ cmdn "frac" [block[diagSystem s,texSystem s], block[texSystem s',diagSystem s']] >> return ()

toSystem' h =  toSystem h . fillTypes

allPosTypes = [One,Zero,tA:⊕:tB,tA:⊗:tB,Bang tA,Forall "α" tAofAlpha]

allRules = 
  [[(axRule, "Copy the data between the closures; when it's ready.")]
  ,[(cutRule, "similar to ⅋ but connects the two closures directly together.")]
  ,[(parRule, "split the environment and spawn a new closure. (No communication)"),
    (crossRule, "add an entry in the context for @tB, at location @math{n + @mkLayout(tA)} (No communication)")]
  ,[(withRule True,"Write the tag and the pointer. Deallocate if the other possibility uses less memory."),
    (plusRule,"wait for the data to be ready; then chose a branch according to the tag. Free the pointer and the tag.  Free the non-used part of the disjunction.")]
  ,[(botRule,"terminate (delete the closure)"),
    (oneRule,"continue")]  
  ,[(zeroRule,"crash")]
  ,[(forallRule,"Write the (pointer to) representation of the concrete type @tB (found in the code) to the 1st cell.")
   ,(existsRule,existComment)]
  ,[(questRule,@"place a pointer to the closure @math{a} in the zone pointed by @math{x:A}, mark as ready; terminate.@"),
    (bangRule,@"wait for ready. Allocate and initialise memory of @mkLayout(tA), spawn a closure from 
                  the zone pointed by @math{x:!A}, link it with @math{x} and  continue. Decrement reference count.@")]
  ,[(weakenRule,"discard the pointer, decrement reference count. Don't forget about recursively decrementing counts upon deallocation.")]
  ,[(contractRule,"copy the pointer to the thunk, increment reference count.  Note this is easy in the AM compared to the cut-elim.")]
  ] 
  
existComment = @"Wait for the type representation to be ready. Copy the
  (pointer to) the representation to the type environment. Free the
  type variable from the memory. Rename the linear variable (as in
  ⊗). NOTE: It is tempting to avoid the sync. point here and instead have 
  one when the type-variable is accessed. However
  because the type variable will be copied around (when a closure is spawned), 
  then this rule must be responsible for freeing the memory (or we need garbage collection;
  yuck).@"
     
-- | Print all derivation rules               
typeRules = figure "Typing rules of Classical Linear Logic, with an ISWIM-style term assignment." $
    env "center" $ do
    forM_ allRules $ \r -> do 
         case r of
            [a] -> math $ deriv'' a
            [a,b] -> math $ deriv'' a >> cmd0 "hspace{1em}" >> deriv'' b
         newline  
         cmd0 "vspace{1em}"

deriv'' (x,_) = deriv' x
               
operationalRules = itemize $ forM_ allRules $ amRule

program :: Deriv -> Tex ()
program (Deriv tvs vs s) = indentation (texProg tvs vs s)
           
                           
amRule' :: SymHeap -> [(Deriv,TeX)] -> TeX
amRule' _ [] = ""
amRule' h0 ((seq,comment):seqs) = do
  item
  @"Rule: @seqName(derivSequent seq) @"
  displayMath $ deriv' seq
  comment
  newline
  case msys1 of
     Nothing -> cmd "text" "no rule for" <> program seq
     Just sys1 -> rul sys0 sys1 >> amRule' (snd sys1) seqs
  
  where sys0 = toSystem' h0 seq
        msys1 = stepSystem sys0

-- | Render abstract machine rules
amRule = amRule' emptyHeap

allReductions = typesetReductions (syncRules ++ pushRules ++ chanRedRules)

typesetReductions reds = env "center" $ 
    forM_ reds $ \(name,input) -> do
          let red1 :: (Deriv -> Tex a) -> Tex ()
              red1 displayer = do          
                
                displayer input
                math $ cmd0 "Longrightarrow"
                displayer (eval input)
                return ()
          "name:" <> name
          newline
          cmd0 "vspace{3pt}"
          red1 deriv
          newline
          cmd "fbox" $ red1 (program)
          cmd0 "vspace{1em}"
          -- renderTree input
          -- renderTree (eval input)
          newline
          

todo = cmd "marginpar"

pole :: TeX
pole = "⊥" <> tex "{\\kern -1ex}" <> "⊥"

arr = meta "a"
keys = meta "k"
vals = meta "v"


-- _x  = math "x"
-- _x' = math "x'"
-- _σ  = math "σ"
-- _σ' = math "σ'"

norm :: TeX -> TeX
norm x = math $ "|" <> x <> "|"

nothing _ = mempty



memory = array [] (braces (text "lcl")) $
         [[ mem t "x" "y",text "=",linearize (texProgM [] [("x",t),("y",neg t)] (copy'' t)) >> return ()] | t <- allPosTypes , t /= Zero]
         ++[[mem metaT "x" "y",text "=",mem (neg metaT) "x" "y"]]
  where metaT = Meta True "T" []

texProgM = texProg'' what True
  where what a ws fs = mem (Meta True a []) z w
          where (z:w:_) = map fst fs

{-
memTranslation = do
  deriv False cutRule
  textual "="
  deriv False $ Deriv ["Θ"] [gamma,delta] (Cut "x" "z" (meta "A") 1 whatA $
                                           Cut "z" "y" (meta "A") 1 (Channel dum) whatB)
-}

-- | Render a derivation tree, showing terms.
deriv' (Deriv tvs vs s) =  derivationTree $ texSeq True tvs vs s 

deriv (Deriv tvs vs s) =  derivationTree $ texSeq False tvs vs s 

outputTexMp name = renderToDisk' name $ latexDocument "article" ["10pt"] Paper.preamble $ @"
@maketitle

@section{Introduction}

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


Landin gave an intuitive syntax for the lambda-calculus.
We attempt to make a linear language a member of the family of 700.
We revist an old idea (LL) in the light of an even older idea (ISWIM).

We provide an interpretation
of linear logic as an programming language with ISWIM syntax, together with an
abstract machine able to run programs written for it.

@todo{π-calculus as a low-level programming language: not quite. We fill the niche}

@section{Types}

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

@section{Terms and Typing rules}

We have a judgement @deriv'(Deriv [] [gamma] whatA) with only hypotheses, no conclusion.
Remarks: 
@itemize{
@item 
In fact, it is as if there were a (single and meaningless) @Bot conclusion: @math{Γ ⊢ a
  : ⊥}.  Essentially the programs are written in CPS.  Because the
  return type is always the same (@Bot), writing it is redundant, so we omit
  it.

@item 
As usual in LL, we have only elimination rules; because
introduction rules are recovered using @ruleName{Ax} and @ruleName{Cut}.

@item
In sum, when we have an set of hypothesis, if the program is
  ``fed'' inputs for all of them but one, the last one will ``spit
  out'' a result. (In fact, where and how data is read/written cannot
  be predicted from the shallow structure of the sequent, one must to
  a deep analysis of the types.)

}

@typeRules

Explanation of the rules:
@itemize{

@item @ruleName{Ax} 
plugs @math{x} and @math{y} together.  
Remark: we do not know from the rule in isolation the ``direction of
  travel'' of information.  Indeed, negation is involutive; so we can
  just reverse the rule by substituting 
   @neg(tA) for 
   @tA

  However, if @tA is a closed basic type, we can interpret the @emph{instance}
  of the rule to be communication in a specific direction.

@item @ruleName{Cut} 
creates a new communication channel of type @tA;
  naming the ends @math{x} and @math{y}. Programs ``talking'' on either end
  execute in parallel.

  Note that the communication channel is ``one-shot''; but the type of
  the channel may be a list/stream/what have you. There may even be
  back and forth communication if the type involes eg. a linear arrows.

  (iirc, Wadler 2012 makes the opposite choice).

@item @math{⊗}   Essentially no-op. Just gives names to more hypothesis.
@item @par: executes two processes in parallel. Note that the
  only with possible communication is via @math{z}; the parts @math{Γ} and @math{Δ}
  can be seen as separate memories in the rhs. The situation is
  similar for @ruleName{Cut}.


@item @math{⊕}: does case analysis; very similar to the usual rule for disjunction.

@item @amp: choses a side

@item @One: Another kind of nop.

@item @Bot: terminate the program

@item Because there is no elim rule for @Top, @Zero can never be constructed.

@item !: offer a way to give as many @tA as needed.
@item ?: demand an @tA.
}


@section{Evaluation rules}
  @allReductions


@section{Abstract machine rules}

Problems of the reduction rules:
@itemize{
@item they are synchronous (eg. @Top/@Bot are eliminated @emph{at the same time})
@item they force arbitrary sequentialisation. (eg. Par/Tensor is asymetric)
}

The idea of the AM is to delay cut-elimination in order to get a more
efficient execution (as in eg. SECD or Krivine's abstract machine). (One might
say that the AM realises ``cut-preservation''.) Concurrent semantics
naturally arise.

The AM reduces/executes a number of closures concurrently. (Each
closure can be thought of as a process.)  A closure is a sequent
together with an environment. An environment has for each variable
of type @tA in the context a pointer to a memory area of layout @mkLayout(tA).
(Each variable can be thought of as a channel.) 

We can meaningfully execute open programs, contrary to what happens in the cut-elimination view of execution. This is similar to the situation in the lambda calculus.

@subsection{Heap structure}
The heap is divided in a number of cells. 
@itemize{
  @item Cells not ready
  @item tag cells (if ready, points to a bit)
  @item polymorphic cells (if ready, points to a concrete type and a concrete value)
  @item thunk cells (can be pointed to by many clients. if ready, points to a (quasi) closure)
}

The layout of each type in memory is the following.
@displayMath{
@array[]{ccc}[[element t,element (neg t),element (mkLayout t)] | t <- allPosTypes]
}

Note that @norm{@tA} = @norm{@neg(tA)}, but the data is used differently, see below.

@subsection{Operational behaviour}

@operationalRules

@section{Ax. alternative}

It is a bit disheartening that the axioms do not ``fizzle'' immediately, 
but stick around to copying things. They stop at polymorphic values and exponentials.
The best we can do is to also stop them at additives --- multiplicatives are communication-less.

To do this we'd need another kind of Tag cell that would say ``look somewhere else''.

@section{Phase Space}

We define:

@itemize{
@item @math{M = @brac{closures in a context}}
@item @math{1 = empty set}
@item @math{· = concatenation of closure lists (and heap).}
@item @math{@pole = any tree of axiom closures}
}

@itemize{
@item @math{M = @brac{closures (bubbles with outgoing edges)}}
@item @math{1 = empty set}
@item @math{· = concatenation of closures with matching channels, with the simple eval. strategy.}
@item @math{@pole = the terminated program}
}


@section{Change the world?}

Assume the following interface, where @arr is an absract type of arrays; 
@keys is a type of indices in the array and @vals a type of values.

@itemize{
 @item get : @id(arr ⊗ keys ⊸ arr ⊗ vals)
 @item set : @id(arr ⊗ keys ⊗ vals ⊸ arr ⊗ vals)
 @item withArray : @id((arr ⊗ tgamma ⊸ arr ⊗ tdelta) ⊸ (tgamma ⊸ tdelta))
}

The with array function can create a single array with both @arr and @neg(arr)
pointing to the same memory area.

@section{A calculus with explicit memory}

We introduce a new form of expression: @id(math mem_).

@section{Termination of the tree-based machine}

@env("definition"){
We say that a node is waiting on an edge if:
@itemize{
 @item it is an axiom node or,
 @item if is a non-structural rule acting on the port connected to that edge.
}

We call an edge ready if all nodes connected to it are waiting on it. In particular,
an hypothesis edge is ready if the single nodes connected to it is waiting on it.
}

@lemma(""){In any tree, there is always at least an edge ready, or the system is terminated.
}{
Remark that every node is waiting on at least one port; except for the terminated node
which cannot be connected to any node. The terminated system trivially satisfies the
theorem.

We then proceed by induction on the size of the tree. If the tree has a single node, then
all the edges are hypotheses, it  must be waiting on one of them, which is then ready.

For the inductive case, we assume two systems σ and σ' satisfying the induction hypothesis, 
with and hypothesis @math{x} in @math{σ} and an hypothesis @math{x'} in @math{σ'}. 
We show that the system obtained by connecting $x$ and $x'$ satisfies the theorem.

We have the following cases:

@enumerate{
@item σ is waiting on @math{x} and @math{σ'} is waiting on @math{x'}. Then the new edge is ready.
@item Either system is not waiting on the designated hypothesis. In this case, some other
   edge in that system must be ready; and it remains ready in the combined system.
}}

@corollary(""){In a closed system, reducing only the top-level cuts is a terminating
reduction strategy}


@section{Related Work}

Many presentations of LL for programming needlessly polarize (dualize)
the presentation. We remain faithful to the spirit of Girard's LL ---
LL is already intuitionistic: there is no need to restrict the system
to give it computational content.


On Intuitionistic Linear Logic, G.M. Bierman 93 
Full Intuitionistic Linear Logic, Hyland & de Paiva 93
A Term Calculus for Intuitionistic Linear Logic; Benton, Bierman, de Paiva, Hyland, 03
Dual Intuitionistic Linear Logic, Andrew Barber 96


A correspondance has recently been identified between linear logic
propositions and session types. A proof of a proposition A can be
identified with process obeying protocol A. This correspondance
departs from the usual linear logic in that the type @math{A ⊗ B} is
interpreted as @math{A} then @math{B}, whereas the usual interpretation of the
linear formula is symmetric with respect to time. Our interpretation
keeps the symmetry intact. The associated calculus is close to the
π-calculus, which we observe is unintuitive to functional programmers
in two respect. On a superficial level, they much prefer ISWIM-like
syntaxes. On a semantic level, the ability to transmit channel names,
departs fundamentally from the tradition of functional programming.


@section{Discussion}
Could we make a purely demand-driven version of the machine? That is, 
instead of waiting, call the code of the closure responsible for giving the data.

@"



