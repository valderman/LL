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

-----------------
-- Preamble 
preamble :: Bool -> Tex ()
preamble inMetaPost = do
  stdPreamble
  usepackage "cmll" [] -- for the operator "par"
  mathpreamble
  cmd "input" (tex "unicodedefs")
  unless inMetaPost $ do
    -- usepackage "dot2texi" []
    usepackage "tikz" []
    cmd "usetikzlibrary" $ tex "shapes,arrows"
    
  title "Linear Logic: I see what it means!"
  authorinfo SIGPlan [("Jean-Philippe Bernardy","bernardy@chalmers.se",ch),
                      ("Josef Svenningsson","",ch)]
 where ch = "Chalmers University of Technology and University of Gothenburg"

  
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
deriv'' (x,_) = derivation x
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
  
rho = "ρ"         
  
-- To be used as rule names within the text. (some should probably be expanded).
ax_,cut_,with_,plus_,par_,tensor_,contract_,weaken_,offer_,demand_::TeX
cut_ = ruleName "Cut"
ax_ = ruleName "Ax"
with_ = "&"
plus_ = "⊕"
par_ = "⅋"
tensor_ = "⊗"
contract_ = ruleName "Contract"
weaken_ = ruleName "Weaken"
offer_ = ruleName "Offer"
demand_ = ruleName "Demand"

lollipop_ :: TeX
lollipop_ = "⊸"

---------------------------------------
-- Types

norm :: Type -> TeX
norm ty = math $ "|" <> texClosedType ty <> "|" <> index rho


allPosTypes :: [(Type,TeX)]
allPosTypes = [(One,"0")
              ,(Zero,"∞")
              ,(tA:⊕:tB,@"1 + @norm(tA) ⊔ @norm(tB) @")
              ,(tA:⊗:tB,@"@norm(tA) + @norm(tB) @")
              ,(Bang tA,"1")
              ,(Forall "α" tAofAlpha,"1")
              ,(meta "α", "|ρ(α)|" <> index rho)
              ]

layoutTable = array [] (tex "c@{~=~}c@{~=~}c")
  [[norm t,norm (neg t),sz] | (t,sz) <- allPosTypes]

multicolumn n fmt c = cmdn "multicolumn" [(tex (show n)),(textual fmt),c]

typeTable = figure "Types " $
    env "center" $ math $ do
      array [] (textual "cccc") $
        [ [ @" @multicolumn(2)("c")("Positive") @", @" @multicolumn(2)("c")("Negative") @"]
        , [  @" @id(tA ⊗ tB) @", @" @One @" , @" @id(tA ⅋ tB) @" , @" @Bot @"]
        , [  @" @id(tA ⊕ tB) @", @" @Zero @", @"  @id(tA & tB) @" , @" @Top @"]
        , [  @" @id(Forall "α" tAofAlpha) @" , @" @" , @" @id(Exists "α" tAofAlpha) @" , @"@"]
        , [  @" @id(Bang tA) @", @"@", @" @id(Quest tA) @" , @"@"]
          ]

 -- [ multicolumn 2 "c" "Positive", multicolumn 2 "c" "Negative"]
          
------------------------              
-- Typing rules

allRules, structuralRules, operationalRules, cutRules :: [[(Deriv, TeX)]]
allRules = structuralRules ++ operationalRules

cutRules = [[(cutRule, @"It allocates @math{|@tA|} cells on the heap. The children become new closures, each with an additional 
                         entry in the environment pointing to the newly allocated area.@")]]

structuralRules = cutRules ++ [[(axRule, "Copy the data between the closures; when it's ready.")]]

operationalRules = 
  [[(parRule, "It splits the environment in two parts. Each becomes a closure, whose environment is either of the parts."),
    (crossRule, @"It adds an entry in the context for @tB, pointing to the location @math{z + @mkLayout(tA)}.@")]
  ,[(withRule False True,@"It writes a tag (in the depicted heap 1) to the heap. If applicable, it deallocates the memory which is known 
                     not to be used (in this case @math{|@tB|-|@tA|}.@"),
    (plusRule,@"In particular the tag must have been written, otherwise the execution cannot proceed. 
                A branch is then chosen according to the tag. The cell holding the tag is freed.@")]
  ,[(botRule False,"The closure is deleted."),
    (oneRule False,"An entry of the environment is deleted.")]  
  ,[(zeroRule,"The rule represents a crashed system and can never be ready to run in a well-typed system.")]
  ,[(forallRule False,@"An area in the heap of size @math{|@tAofB|} is allocated.
                  The representation of the concrete type @tB is written in the cell,
                  together with a pointer to the newly allocated area.
                @")
   ,(existsRule,existComment)]
  ,[(questRule False,@"The pointer to the closure @math{a} is written to the cell pointed by @math{x}. In the environment of that closure, 
                 replace the pointer to TODO by a null pointer.
                 The closure is then removed from 
                 the list of ready closures.@"),
    (bangRule,@"The process can run only when a closure can be found in the cell pointed by @vX. Then it allocates @math{|@tA|} 
                cells, and spawn a new closure, which is obtained from copying that pointed by 
                 by @math{x:!A}. The null pointer in that closure's environment is replaced by a pointed to @math{x}. 
                 The reference count of the cell is decremented.@")]
  ,[(weakenRule,@"The reference count of the cell pointed by @math{z} is decremented, and the pointer discarded. 
                  The closure is deallocated if the count reached zero.@")]
  ,[(contractRule False,"The the pointer is copied to a new environment entry, and the reference count incremented.")]
  ] 

existComment = @"(In particular the closure waits if the cell pointed by @math{z} is empty.) 
                 One copies the type representation found in the cell to the type environment. 
                 The cell is then freed, and one proceeds with the execution of the child.@"

-- | Print all derivation rules               
typeRules = figure_ "Typing rules of Classical Linear Logic, with an ISWIM-style term assignment." $
    env "center" $ do
    forM_ allRules $ \r -> do 
         case r of
            [a] -> math $ deriv'' a
            [a,b] -> math $ deriv'' a >> cmd0 "hspace{1em}" >> deriv'' b
         newline  
         cmd0 "vspace{1em}"

--------------------
-- Abstract Machine         
         
texAmRulesExplanation = itemize $ forM_ (cutRules ++ operationalRules) $ amRule

-- | Render abstract machine rules
amRule = amRule' emptyHeap

toSystem' h =  toSystem h . fillTypes
               
rul s s' = displayMath $ cmdn "frac" [block[diagSystem s,texSystem s], block[texSystem s',diagSystem s']] >> return () 
               
amRule' :: SymHeap -> [(Deriv,TeX)] -> TeX
amRule' _ [] = ""
amRule' h0 ((sequ,explanation):seqs) = do
  item
  @"@seqName(derivSequent sequ). @"
  case msys1 of
     Nothing -> explanation
     Just sys1 -> do
       @"The rule assumes an input heap of this form:@"
       displayMath $ diagSystem sys0
       explanation
       @"The heap after execution is:@"
       displayMath $ diagSystem sys1

       amRule' (snd sys1) seqs
  
  where sys0 = toSystem' h0 sequ
        msys1 = stepSystem sys0       

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
  [
    -- (chanPlusRule True,  "A channel containing a bit")
    --  ,(chanPlusRule False, "A channel containing a bit")
--  ,(chanTypRule,       "A channel containing a type")
--  ,(chanEmptyRule 3,   "A memory cell (empty)")
--  ,(chanFullRule 3,    "A memory cell (full)")
   (chanCrossRule,     "A half-split channel (conjuction)")
  ,(chanParRule,       "A half-split channel (disjunction)")
  ,(oneRule True, "severing channel (conjunction)")
  ,(botRule True, "severing channel (disjunction)")
  ,(withRule True True, "A channel containing a bit")
  ,(forallRule True, "A channel containing a type")
  ,(questRule True, "A reference to a server")
  ,(contractRule True, "A pointer copy")
  ]

texBosons :: Tex SortedLabel
texBosons = figure "Rules mediating interaction" $ mathpar 
            [ [ deriv False r | (r,_comment) <- chanRules ] ]

{-
typesetBosonReds reds = env "center" $ 
    forM_ reds $ \(_name,input) -> math $ do
      sequent input
      cmd0 "Longrightarrow"
      sequent (eval' input)
      
      return ()
-}

texBosonReds :: Tex SortedLabel
texBosonReds =  figure_ @"Asynchronous reduction rules. (Rules involving @ax_ are omitted) @" $ 
                mathpar [
                  [ sequent input <>
                    cmd0 "Longrightarrow" <>
                    sequent (evaluator input)
                   | (evaluator,_name,input) <- chanRedRules ] 
                        ]

chanRedRules =
   [(eval' ,"bit write"  ,          fillTypes $ withRule False True)
   ,(eval,"bit read"   ,          fillTypes $ cutWithPlus' True True)
   ,(eval',"type write" ,          fillTypes $ forallRule False) 
   ,(eval ,"type read"  ,          fillTypes $ cutQuant' True)
   ,(eval',"left split" ,          fillTypes $ parRule' False)
   ,(eval',"right split",          fillTypes $ crossRule' False)
   ,(eval ,"split meet" ,          fillTypes $ cutParCross' True)
   ,(eval',"left sever" ,          fillTypes $ botRule False)
   ,(eval',"right sever",          fillTypes $ oneRule False)
   ,(eval ,"sever meet" ,          fillTypes $ cutUnit' True)
   ,(eval',"server wait",          fillTypes $ questRule False)
   ,(eval',"pointer copy (right)", fillTypes $ contractRule False)   
--   ,(eval',"pointer copy (left)", contractRule False)
--   ,("client spawn", cutBang' True)
   ]
--   ,("output",    Deriv ["Θ"] [gamma,delta,("z",neg (tA :⊗: tB))] $ Cut "w" "_w" (tA :⊗: tB) 2 (Exchange [1,0,2] $ Par dum "x" "y" 1 whatA whatB) (Channel dum))
--   ,("input",    Deriv ["Θ"]  [("x",tA), ("y",tB),delta] $ Cut "w" "_w" (tA :⊗: tB) 2 (ChanPar dum dum) (Cross dum "x" "y" 0 whatA) )
    
               
