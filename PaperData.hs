{-# OPTIONS_GHC -XTypeFamilies -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}

module PaperData where

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
    authorinfo SIGPlan [("Anonymous","email","institution")]
    cmd "titlebanner" ("Draft of " <> cmd0 "today")
    cmd "preprintfooter" "Submitted to POPL 2014"


authors = [("Jean-Philippe Bernardy","bernardy@chalmers.se",ch),
           ("Josef Svenningsson","josefs@chalmers.se",ch)]
 where ch = "Chalmers University of Technology and University of Gothenburg"
  
-------------------------
-- Generic renderers.

-- | Render a derivation tree. 
deriv :: Bool -- ^ Show terms?
         -> Deriv -> TeX
deriv showProg (Deriv tvs vs s) = derivationTree' $ texSeq showProg tvs vs s

derivationTree' x = do
   centerVertically $ 
     derivationTreeMP [] x
   return ()
-- "scale=0.1" can be passed as an option to compress things.

derivation, sequent, program :: Deriv -> TeX
derivation = deriv True 
sequent = deriv False

-- | Render a derivation as a program (term)
program (Deriv tvs vs s) = indentation (texProg tvs vs s)

programWithCtx (Deriv tvs vs s) = 
  array[] "l" [[texCtx True tvs vs <> "⊢"], [program (Deriv tvs vs s)]]

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
mix_,bicut_,ax_,cut_,with_,plus_,par_,tensor_,contract_,weaken_,offer_,demand_,mem_::TeX
mix_ = ruleName "Mix"
bicut_ = ruleName "BiCut"
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
mem_ = ruleName "BM"

lollipop_ :: TeX
lollipop_ = "⊸"

diamond_ :: TeX
diamond_ = math "⋄"

cdot :: TeX
cdot = cmd0 "cdot"

gamma_ :: TeX
gamma_ = "Γ"

forall_, exists_ :: TeX
forall_ = math "∀"
exists_ = math "∃"

alpha_ :: TeX
alpha_ = "α"



-------------------------------------
-- Reductions

operationalRules_, cutAssoc, cutSwap, cutAx_, explicitAxiom, commutingConversions :: TeX
operationalRules_ = math $ text $ "Oper/Oper"
swapped x = x <> tex "^{∘}"
bidir x = x ∪ swapped x 
cutAssoc = math $ text $ smallcaps "CutAssoc"
cutSwap = math $ text $ smallcaps "CutSwap"
cutAx_ = math $ text $ smallcaps "CutAx"
bosonBoson = math $ text $ smallcaps "Boson/Boson"
bosonOper = math $ text $ smallcaps "Boson/Oper"
boson_ = ruleName "Boson"
explicitAxiom = math $ text $ smallcaps $ "Ax/Oper"
commutingConversions = text "commuting conversions"
x `cong` y = x <> text " congruences of " <> y

(∪) :: TeX -> TeX -> TeX
x ∪ y = x <> " ∪ " <> y

redLL :: TeX
redLL = math $ cmd0 "Longrightarrow"

redLLDef_ = foldr1 (∪) [operationalRules_, cutAssoc, cutSwap, cutAx_, commutingConversions]

redOM :: TeX
redOM = math $ cmdn_ "stackrel" ["o", redLL]

redOMDef_ :: TeX
redOMDef_ = foldr1 (∪) [bidir operationalRules_, cutAx_, cut_ `cong` redOM ]

redAX :: TeX
redAX = math $ cmdn_ "stackrel" [ax_, redLL]
redAXDef_ = foldr1 (∪) [bidir operationalRules_, explicitAxiom, cut_ `cong` redAX ]

redBO :: TeX
redBO = math $ cmdn_ "stackrel" ["oa", redLL]
redBODef_ = foldr1 (∪) [bidir bosonBoson, bidir bosonOper, explicitAxiom, cut_ `cong` redBO, boson_ `cong` redBO ]


redHeap :: TeX
redHeap = math $ cmdn_ "stackrel" [cdot, redBO]

redHeapDef_ :: TeX
redHeapDef_ = foldr1 (∪) [bidir bosonBoson, cut_ `cong` redHeap, boson_ `cong` redHeap ]


redAM :: TeX
redAM = math $ cmd0 "Rrightarrow"

---------------------------------------
-- Types

norm :: Type -> TeX
norm ty = math $ "|" <> texClosedType ty <> "|" <> index rho


allPosTypesTable :: [(Type,TeX)]
allPosTypesTable = 
  [(Zero,"∞")
  ,(One,"0")
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

multicolumn n fmt c = cmdn_ "multicolumn" [tex (show n),tex fmt,c]

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

allRules, structuralRules, operationalRules, cutRules :: [[(Deriv, TeX, TeX)]]
allRules = structuralRules ++ operationalRules

cutRules = [[(cutRule, @"It allocates @math{|@tA|} cells on the heap. The children become new closures, each with an additional 
                         entry in the environment pointing to the newly allocated area.@"
                       , "Parallel execution"
             )]]

structuralRules = cutRules ++ [[(axRule, "Copy the data between the closures; when it's ready."
                                ,"Channel forwarding")]]

operationalRules = 
  [multiplicatives
  ,additives
  ,[(botRule,"The closure is deleted."
    ,"termination"),
    (oneRule False,"An entry of the environment is deleted."
    ,"empty output")]  
  ,[(zeroRule,"The rule represents a crashed system and can never be ready to run in a well-typed system."
    ,"empty choice")]
  ,[(forallRule False,@"An area in the heap of size @math{|@tAofB|} is allocated.
                  The representation of the concrete type @tB is written in the cell,
                  together with a pointer to the newly allocated area.
                @"
    ,"sending type")
   ,(existsRule,existComment
    ,"receiving type")]
  ,offerDemand,
   [(weakenRule False,@"The reference count of the cell pointed by @math{z} is decremented, and the pointer discarded. 
                  The closure is deallocated if the count reached zero.@"
    ,"drop reference")]
  ,[(contractRule False,"The the pointer is copied to a new environment entry, and the reference count incremented."
    ,"duplicate reference")]
  ] 

 where existComment = @"(In particular the closure waits if the cell pointed by @math{z} is empty.) 
                 One copies the type representation found in the cell to the type environment. 
                 The cell is then freed, and one proceeds with the execution of the child.@"

multiplicatives, additives, offerDemand :: [(Deriv, TeX, TeX)]
multiplicatives = [(parRule, @"An additional process is spawned, hence we have one process for each
                              of the children of the rule. The original environment is split into two parts,
                              which become the new environments of the new processes. A new variable is
                              added to each environment, which points respectively to either the @tA or @tB part
                              of the heap. The pointer to the second part is computed by @math{z + @mkLayout(tA)}.@"
                   ,"parallel execution"),
                  (crossRule, @"It adds an entry in the environment for @math{y}, pointing to @math{z + @mkLayout(tA)}.@"
                  ,"consume both")]
additives = [(withRule False True,@"It writes a tag (in the depicted heap 1) to the heap. If applicable, it deallocates the memory which is known 
                     not to be used (in this case @math{|@tB|-|@tA|}).@"
             ,"left selection"),
             (plusRule,@"In particular the tag must have been written, otherwise the execution cannot proceed. 
                A branch is then chosen according to the tag. The cell holding the tag is freed.@"
             ,"choice")]
offerDemand = [(questRule False,
  @"The pointer to the closure @math{a} is written to the cell pointed by @math{z}. 
    The environment of the new closure is the current environment, but where the 
    pointer @math{z} is replaced by the NULL pointer (represented by a cross below).
    The created closure not ready to run: the current process is then terminated.@",
  "create a closure"),
               (bangRule,@"The process can run only when a closure can be found in the cell pointed by @vX. Then it allocates @math{|@tA|} 
                cells, and spawn a new closure, which is obtained from copying that pointed by 
                 by @math{x:!A}. The null pointer in that closure's environment is replaced by a pointed to @math{x}. 
                 The reference count of the cell is decremented. In the situation represented in the diagram, 
                 there is no other reference to that cell, so it should be deallocated.@",
                "invoke the closure")]
  
-- | Print all derivation rules               
typeRules = figure_ "Typing rules of Classical Linear Logic, with an ISWIM-style term assignment." $
            mathpar $ multiSplit [2,2,2,3,2,2] $
            concat $ map (map deriv'') allRules 
  where deriv'' (x,_,_) = derivation x

multiSplit [] xs = [xs]
multiSplit (i:is) xs = let (l,r) = splitAt i xs in l : multiSplit is r

termFigure = array [] "rl" $ map return $ concat $ map (map apa) allRules
  where apa (x,_,short) = program x <> tex "&" <> short

--------------
-- Reductions

structFig = figure_ "Structural cut equivalences" $
            env "center" $
            typesetEquivalences derivation structRules

syncFig = figure_ "Reduction rules" $
          env "center" $
          typesetReductions programWithCtx syncRules

pushFig = figure_ "Commuting conversions" $
          env "center" $
          typesetReductions program pushRules
{-
pushFig1 = figure_ "Auxiliary reduction rules I" $
           env "center" $
           typesetReductions program (take 7 pushRules)

pushFig2 = figure_ "Auxiliary reduction rules II" $
           env "center" $
           typesetReductions program (drop 7 pushRules)
-}

typesetReductions = typesetRules redLL

typesetEquivalences = typesetRules (cmd0 "equiv")

typesetRules relation displayer reds = mathpar  [[
      -- "name:" <> name
      displayer input {- <> kern "-1em"-} <> relation <> displayer (eval input)
    | (name,input) <- reds]]


--------------------
-- Abstract Machine         
         

amRules = cutRules ++ operationalRules

texAmRules caption = figure_ caption $ do
  cmd0 "small"
  mathpar [forAmRules amRules $ \(sequ,explanation) s s' -> do
              amRuleAsMath sequ s s' 
   ]  

texAmRulesExplanation whichRules = 
  itemize $ 
  sequence_ $ forAmRules whichRules $ \(sequ,explanation) sys0 sys1 -> do
    item
    @" Rule @seqName(derivSequent sequ).@"
    @" The rule assumes an input heap of this form:@"
    displayMath $ diagSystem sys0
    explanation
    @" The heap after execution is:@"
    displayMath $ diagSystem sys1

forAmRules ::  [[(Deriv, t,s)]]  -> ((Deriv, t) -> System SymHeap -> System SymHeap -> b) -> [b]
forAmRules rules f = concatMap (amRule f) rules

-- | Render abstract machine rules
amRule f = amRule' f emptyHeap 

toSystem' h =  toSystem h . fillTypes
               
amRuleAsMath sequ s s' = do
  cmdn "frac" [texSystem s, redAM <> texSystem s']  >> seqName(derivSequent sequ)
  return ()
    
amRule' _ _ [] = []
amRule' f h0 ((sequ,explanation,_):seqs) = do
  case msys1 of
     Nothing -> []
     Just sys1 -> f (sequ,explanation) sys0 sys1 : 
                   amRule' f (snd sys1) seqs
  where sys0 = toSystem' h0 sequ
        msys1 = stepSystem sys0       



----------
-- Examples

simpleEnv :: Deriv
simpleEnv = Deriv [] [("x",tA),("y",tB),("z",neg tC)] whatA

simpleCut :: Deriv
simpleCut = Deriv [] [gamma,xi] $ Cut "x" "y" tA 1 whatA whatB 

simpleCut' :: String -> String -> Deriv
simpleCut' a b = Deriv [] [gamma,xi] $ Cut "x" "y" tA 1 (What a []) (What b [])


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
  ,(withRule True False, "A channel containing a bit")
  ,(withRule True True, "A channel containing a bit")
  ,(forallRule True, "A channel containing a type")
  ,(questRule True, "A reference to a server")
  ,(contractRule True, "A pointer copy")
  ,(weakenRule True, "A pointer deletion")
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

texReductionFigure caption relation displayer rules = 
  figure_ caption $ mathpar [
                  [ displayer input <>
                    relation <>
                    displayer (evaluator input)
                   | (evaluator,_name,input) <- rules ] 
                  ]

texBosonReds :: Tex SortedLabel
texBosonReds =  texReductionFigure 
                @"Asynchronous reduction rules.
                  The @mem_/@contract_ rule is shown for arity @math{n=1} of @mem_,
                  but it exists for any @math{n≥1}.
                  Similarly the @mem_/@weaken_ rule is shown for arity 3.
                @" 
                redBO sequent chanRedRules

texAxiomReds :: Tex SortedLabel
texAxiomReds = texReductionFigure 
               @"Explicit Axiom Reductions@"
               redAX sequent chanAxRedRules

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
   ,(eval',"pointer delete (right)",  fillTypes $ weakenRule True)
   ,(eval',"pointer delete (left)",  
     fillTypes $ 
     Deriv ["θ"] [gammaBang,xi] $
     -- Cut "w" "_w" (Bang tA) $
     Mem tA 1 3 whatA (Ignore True 0 whatB) ) 
    
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
    (eval', "axiom eval", Deriv ["Θ"] [("_w",neg t),("_w",t)] $ Ax t) | t <- drop 1 $ -- This drops the zero type, which can never occur dynamically
                                                                             init allPosTypes -- Drops type variable case
  ]