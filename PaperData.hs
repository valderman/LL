{-# OPTIONS_GHC -XTypeFamilies -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}

module PaperData where

import Pretty
import MarXup
import MarXup.Latex
import MarXup.Tex
import MarXup.DerivationTrees
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


-------------------------
-- Generic renderers.

-- | Render a derivation tree. 
deriv :: Bool -- ^ Show terms?
         -> Deriv -> TeX
deriv showProg (Deriv tvs vs s) = derivationTree $ texSeq showProg tvs vs s

derivation, sequent :: Deriv -> TeX
derivation = deriv True 
sequent = deriv False

-- | Render a derivation as a program (term)
program :: Deriv -> Tex ()
program (Deriv tvs vs s) = indentation (texProg tvs vs s)

-- | Render a derivation tree, showing terms.
deriv'' (x,_) = program x


-- Element instances

instance Element Type where
  type Target Type = TeX
  element = texClosedType

instance Element Layout where
  type Target Layout = TeX
  element = math . texLayout

--------------------------------
-- "Identifiers"

vX,vX' :: TeX
vX = math "x"
vX' = math "x'"

graph1, graph2 :: TeX
graph1 = math "σ"
graph2 = math "σ'"
  
  
cut_,with_,plus_,par_,tensor_ :: TeX
cut_ = ruleName "Cut"
with_ = "&"
plus_ = "⊕"
par_ = "⅋"
tensor_ = "⊗"

---------------------------------------
-- Types


allPosTypes :: [Type]
allPosTypes = [One,Zero,tA:⊕:tB,tA:⊗:tB,Bang tA,Forall "α" tAofAlpha]
  
  
------------------------              
-- Typing rules

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
typeRules = figure_ "Typing rules of Classical Linear Logic, with an ISWIM-style term assignment." $
    env "center" $ do
    forM_ allRules $ \r -> do 
         case r of
            [a] -> math $ deriv'' a
            [a,b] -> math $ deriv'' a >> cmd0 "hspace{1em}" >> deriv'' b
         newline  
         cmd0 "vspace{1em}"


----------
-- Examples

simpleCut :: Deriv
simpleCut = Deriv [] [gamma,xi] $ Cut "x" "y" tA 1 whatA whatB 

doubleCut :: Deriv
doubleCut = Deriv [] [gamma,xi] $ Cut "_x" "x" tB 1 (Cut "_y" "y" (neg tA) 1 whatA whatB) (Cut "_z" "z" tC 1 whatC whatD)

doubleCut' :: Deriv
doubleCut' = Deriv [] [gamma,delta] $ Cut "_x" "x" (neg tA) 0 whatA $ Cut "_y" "y" (neg tC) 1 (Cut "_z" "z" (neg tB) 0 whatB whatD) whatC 

  
-------------------------------
-- Mediating interaction

chanRules :: [(Deriv,TeX)]               
chanRules =   
  [(chanPlusRule True,  "A channel containing a bit")
  ,(chanPlusRule False, "A channel containing a bit")
--  ,(chanCrossRule,     "A half-split channel (par side)")
--  ,(chanParRule,       "A half-split channel (par side)")
--  ,(chanTypRule,       "A channel containing a type")
--  ,(chanEmptyRule 3,   "A memory cell (empty)")
--  ,(chanFullRule 3,    "A memory cell (full)")
  ]

texBosons :: Tex SortedLabel
texBosons = figure "Rules mediating interaction" $ 
    env "center" $ do
    forM_ chanRules $ \(r,_comment) -> do 
        math $ deriv False r
        cmd0 "hspace{1em}"

typesetBosonReds reds = env "center" $ 
    forM_ reds $ \(name,input) -> math $ do
      sequent input
      cmd0 "Longrightarrow"
      sequent (eval' input)
      
      return ()

texBosonReds =  figure_ "Asynchronous reduction rules" $ 
                env "center" $ 
                typesetBosonReds chanRedRules
                
                