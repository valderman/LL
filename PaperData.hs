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
  if inMetaPost 
      then documentClass "article" ["9pt"]
      else documentClass "sigplanconf" ["authoryear","preprint"] 
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
                        ("Josef Svenningsson","josefs@chalmers.se",ch)]
 where ch = "Chalmers University of Technology and University of Gothenburg"

  
-------------------------
-- Generic renderers.

-- | Render a derivation tree. 
deriv :: Bool -- ^ Show terms?
         -> Deriv -> TeX
deriv showProg (Deriv tvs vs s) = derivationTree $ texSeq showProg tvs vs s

derivation, sequent, program :: Deriv -> TeX
derivation = deriv True 
sequent = deriv False

-- | Render a derivation as a program (term)
program (Deriv tvs vs s) = indentation (texProg tvs vs s)

-- Element instances

instance Element Type where
  type Target Type = TeX
  element = texClosedType

instance Element Layout where
  type Target Layout = TeX
  element = math . texLayout

instance Element Deriv where
  type Target Deriv = TeX
  element = displayMath . couplingDiag 

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
ax_,cut_,with_,plus_,par_,tensor_,contract_,weaken_,offer_,demand_,mem_::TeX
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
mem_ = ruleName "Mem"

lollipop_ :: TeX
lollipop_ = "⊸"

diamond_ :: TeX
diamond_ = math "⋄"

gamma_ :: TeX
gamma_ = "Γ"

forall_, exists_ :: TeX
forall_ = math "∀"
exists_ = math "∃"

alpha_ :: TeX
alpha_ = "α"

---------------------------------------
-- Types

norm :: Type -> TeX
norm ty = math $ "|" <> texClosedType ty <> "|" <> index rho


allPosTypesTable :: [(Type,TeX)]
allPosTypesTable = [(One,"0")
              ,(Zero,"∞")
              ,(tA:⊕:tB,@"1 + @norm(tA) ⊔ @norm(tB) @")
              ,(tA:⊗:tB,@"@norm(tA) + @norm(tB) @")
              ,(Bang tA,"1")
              ,(Forall "α" tAofAlpha,"1")
              ,(meta "α", "|ρ(α)|" <> index rho)
              ]

allPosTypes = map fst allPosTypesTable


texNegationTable = array [] (tex "c@{~=~}c")
  [ map element [MetaNeg t, neg t] | t <- init allPosTypes]
  -- Note the last row (variable case) is dropped.

layoutTable = array [] (tex "c@{~=~}c@{~=~}c")
  [[norm t,norm (neg t),sz] | (t,sz) <- allPosTypesTable]

multicolumn n fmt c = cmdn "multicolumn" [(tex (show n)),(textual fmt),c]
                      >> return ()

typeTable = figure "Types " $
    env "center" $ math $ do
      array [] (textual "cccc") $
        [ [ multicolumn 2 "c" "Positive", multicolumn 2 "c" "Negative" ]
        , [  @" @id(tA ⊗ tB) @", @" @One @" , @" @id(tA ⅋ tB) @" , @" @Bot @"]
        , [  @" @id(tA ⊕ tB) @", @" @Zero @", @"  @id(tA & tB) @" , @" @Top @"]
        , [  @" @id(Forall "α" tAofAlpha) @" , @" @" , @" @id(Exists "α" tAofAlpha) @" , @"@"]
        , [  @" @id(Bang tA) @", @"@", @" @id(Quest tA) @" , @"@"]
          ]

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
  where deriv'' (x,_) = derivation x


--------------
-- Reductions

syncFig = figure_ "Reduction rules" $
          env "center" $
          typesetReductions program syncRules

pushFig = figure_ "Reduction rules" $
          env "center" $
          typesetReductions program pushRules

pushFig1 = figure_ "Auxiliary reduction rules I" $
           env "center" $
           typesetReductions program (take 7 pushRules)

pushFig2 = figure_ "Auxiliary reduction rules II" $
           env "center" $
           typesetReductions program (drop 7 pushRules)

typesetReductions displayer reds = env "center" $
    forM_ reds $ \(name,input) -> do
      "name:" <> name
      newline
      cmd0 "vspace{3pt}"
      displayer input
      math $ cmd0 "Longrightarrow"
      displayer (eval input)
      newline
      cmd0 "vspace{1em}"

--------------------
-- Abstract Machine         
         

texAmRules = figure_ "Abstract Machine Rules" $ 
  mathpar [forAllAmRules $ \(sequ,explanation) s s' -> do
    amRuleAsMath s s' 
    seqName(derivSequent sequ)]

texAmRulesExplanation = 
  itemize $ 
  sequence_ $ forAllAmRules $ \(sequ,explanation) sys0 sys1 -> do
    item
    @"Rule @seqName(derivSequent sequ). @"
    @"The rule assumes an input heap of this form:@"
    displayMath $ diagSystem sys0
    explanation
    @"The heap after execution is:@"
    displayMath $ diagSystem sys1

forAllAmRules :: ((Deriv, TeX) -> System SymHeap -> System SymHeap -> TeX) -> [TeX]
forAllAmRules f = concatMap (amRule f) (cutRules ++ operationalRules)

-- | Render abstract machine rules
amRule f = amRule' f emptyHeap 

toSystem' h =  toSystem h . fillTypes
               
amRuleAsMath s s' = do
  cmdn "frac" [texSystem s, texSystem s']
  return ()
    
amRule' _ _ [] = []
amRule' f h0 ((sequ,explanation):seqs) = do
  case msys1 of
     Nothing -> []
     Just sys1 -> f (sequ,explanation) sys0 sys1 : 
                   amRule' f (snd sys1) seqs
  where sys0 = toSystem' h0 sequ
        msys1 = stepSystem sys0       



----------
-- Examples

simpleEnv :: Deriv
simpleEnv = Deriv [] [("x",tA),("y",tB),("z",tC)] whatA

simpleCut :: Deriv
simpleCut = Deriv [] [gamma,xi] $ Cut "x" "y" tA 1 whatA whatB 

doubleCut :: Deriv
doubleCut = Deriv [] [gamma,xi] $ Cut "_x" "x" tB 1 (Cut "_y" "y" (neg tA) 1 whatA whatB) (Cut "_z" "z" tC 1 whatC whatD)

doubleCut' :: Deriv
doubleCut' = Deriv [] [gamma,delta] $ Cut "_x" "x" (neg tA) 0 whatA $ Cut "_y" "y" (neg tC) 1 (Cut "_z" "z" (neg tB) 0 whatB whatD) whatC 

exponentialSimple = fillTypes $
  Deriv ["θ"] [gammaBang,xi] $ 
  Cut "_x" "x" (Bang tA) 1 (Offer False "x" 0 whatA) $
  Alias False 0 "y" $ 
  Demand "z" dum 1 $ 
  whatB
                    
  
-------------------------------
-- Mediating interaction


chanRules :: [(Deriv,TeX)]
chanRules =   
  [
   (chanCrossRule,     "A half-split channel (conjuction)")
  ,(chanParRule,       "A half-split channel (disjunction)")
  ,(oneRule True, "severing channel")
  ,(withRule True True, "A channel containing a bit")
  ,(forallRule True, "A channel containing a type")
  ,(questRule True, "A reference to a server")
  ,(contractRule True, "A pointer copy")
  ,(memRule 4, "A memory cell")
  ]

texBosons :: Tex SortedLabel
texBosons = figure 
  @"Rules mediating interaction. The @mem_ rule can provide any number of copies of @Bang(tA).
    Here just the one instance is shown. @" $ 
  mathpar [ [ deriv False r | (r,_comment) <- chanRules ] ]

{-
typesetBosonReds reds = env "center" $ 
    forM_ reds $ \(_name,input) -> math $ do
      sequent input
      cmd0 "Longrightarrow"
      sequent (eval' input)
      
      return ()
-}

texBosonReds :: Tex SortedLabel
texBosonReds =  figure_ @"Asynchronous reduction rules.@" $ 
                mathpar [
                  [ sequent input <>
                    cmd0 "Longrightarrow" <>
                    sequent (evaluator input)
                   | (evaluator,_name,input) <- chanRedRules ++ chanAxRedRules ] 
                        ]

chanRedRules,chanAxRedRules :: [(Deriv -> Deriv, String, Deriv)]
chanRedRules =
   [(eval' ,"bit write"  ,         fillTypes $ withRule False True)
   ,(eval,"bit read"   ,           fillTypes $ cutWithPlus' True True)
   ,(eval',"type write" ,          fillTypes $ forallRule False) 
   ,(eval ,"type read"  ,          fillTypes $ cutQuant' True)
   ,(eval',"left split" ,          fillTypes $ parRule' False)
   ,(eval',"right split",          fillTypes $ crossRule' False)
   ,(eval ,"split meet" ,          fillTypes $ cutParCross' True)
--   ,(eval',"left sever" ,          fillTypes $ botRule False) -- USELESS!
   ,(eval',"right sever",          fillTypes $ oneRule False)
   ,(eval ,"sever meet" ,          fillTypes $ cutUnit' True)
   ,(eval',"server wait",          fillTypes $ 
                                   Deriv ["Θ"] [(mempty, Bang (meta "Γ")), delta] $
                                   Cut "z" "_z" (Bang tA) 1 (Offer False "x" 0 whatA) whatB)
   ,(eval',"pointer copy (right)", fillTypes $ contractRule False)
   ,(eval',"pointer copy (left)",  fillTypes $ 
                                   Deriv ["θ"] [gammaBang,xi] $
                                   -- Cut "w" "_w" (Bang tA) $
                                   Mem tA 1 1 whatA (Alias True 0 "x" whatB) )
   ,(eval',"pointer delete (right)",  fillTypes $ weakenRule) -- TODO? Probably we can't asynchronicize this.
   ,(eval',"pointer delete (left)",  
     fillTypes $ 
     Deriv ["θ"] [gammaBang,xi] $
     -- Cut "w" "_w" (Bang tA) $
     Mem tA 1 1 whatA (Ignore 0 whatB) ) -- TODO
    
   ,(eval',"mem dealloc",  fillTypes $ 
                                   Deriv ["θ"] [gammaBang,xi] $
                                   -- Cut "w" "_w" (Bang tA) $
                                   Mem tA 1 0 whatA whatB) -- TODO
    
--   ,("client spawn", cutBang' True)
   ]
--   ,("output",    Deriv ["Θ"] [gamma,delta,("z",neg (tA :⊗: tB))] $ Cut "w" "_w" (tA :⊗: tB) 2 (Exchange [1,0,2] $ Par dum "x" "y" 1 whatA whatB) (Channel dum))
--   ,("input",    Deriv ["Θ"]  [("x",tA), ("y",tB),delta] $ Cut "w" "_w" (tA :⊗: tB) 2 (ChanPar dum dum) (Cross dum "x" "y" 0 whatA) )
    
               
chanAxRedRules =   
  [
    (eval', "axiom eval", Deriv ["Θ"] [("_w",neg t),("_w",t)] $ Ax t) | t <- init allPosTypes
  ]