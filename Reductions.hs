{-# LANGUAGE OverloadedStrings #-}  
module Reductions where
import Data.Monoid
import LL
import Rules
import Pretty
import TexPretty 
import MarXup.Tex
import MarXup.Latex (math)

cutAssoc1 = Deriv [] [gamma,delta,xi] $ Cut "y" "_y" tB 2 (Cut "x" "_x" tA 2 whatA whatB) whatC
-- cutAssoc2 = Deriv [] [gamma,delta,xi] $ Cut "x" "_x" tA 1 whatA (Cut "y" "_y" tB 1 whatB whatC)

cutAx = Deriv ["Θ"] [gamma,("w",neg $ meta "A")] $
              Cut "x" "y" (meta "A") 1 (What "a" [0]) (Ax dum)

cutWithPlus' β b = fillTypes $ 
                Deriv ["Θ"] [gamma,xi]
                (Cut "z" "_z" (meta "A" :⊕: meta "B") 1 
                (With β dum "x" b 0 (whatA)) 
                (Plus "_x" "_y" 0 (whatB) (whatC)))
cutWithPlus = cutWithPlus' False
cutParCross' β = Deriv ["Θ"] [gamma,delta,(mempty,meta "Ξ")]
              (Cut "z" "_z" (meta "A" :⊗: meta "B") 2 
               (Exchange [1,0,2] $ Par β dum "_x" "_y" 1 whatA whatB)
               (Cross β dum "x" "y" 0 whatC))
cutParCross = cutParCross' False
cutBang' β = Deriv ["Θ"] [(mempty, Bang (meta "Γ")), delta] $
          Cut "z" "_z" (Bang $ meta "A") 1 (Offer β "x" 0 whatA) (Demand "_x" dum 0 whatB)
cutBang = cutBang' False
cutContract' β = Deriv ["Θ"] [(mempty, Bang (meta "Γ")), delta] $
          Cut "z" "_z" (Bang $ meta "A") 1 (Offer β "x" 0 whatA) (Alias β 0 "y" whatB)
cutContract = cutContract' False
cutIgnore' β = Deriv ["Θ"] [(mempty, Bang (meta "Γ")), delta] $
            Cut "z" "_z" (Bang $ meta "A") 1 (Offer β "x" 0 whatA) (Ignore β 0 whatB)
cutIgnore = cutIgnore' False
cutUnit' β = Deriv ["Θ"] [gamma] $ Cut "z" "_z" One 0 (SBot) (SOne β 0 whatA)
cutUnit = cutUnit' False
cutQuant' β = Deriv ["Θ"] [gamma,xi] $
           Cut "z" "_z" (Exists "α" (Meta True "A" [var 0])) 1 (TApp β dum "x" 0 (meta "B") whatA) (TUnpack "_x" 0 whatB)
cutQuant = cutQuant' False


assocRules,syncRules,pushRules :: [(TeX,Deriv)]
syncRules = [
    (math par<>"⊗",cutParCross),
    (amp<>"⊕",cutWithPlus True),
    ("?!", cutBang),
    ("⊥!",cutUnit),
    ("∃∀",cutQuant),
    ("?Contract",cutContract),
    ("?Weaken",cutIgnore)
    ]
assocRules = [("CutCut",cutAssoc1)]
structRules = [("CutCut",cutAssoc1)
              ,("AxCut",cutAx)]

altParPush = Deriv ["Θ"] (derivContext parRule ++ [xi]) (Cut "z" "_z" (meta "C") 3 (Par False dum "_x" "_y" 2 whatA whatB) whatB)

pushRules = (textual "κ⅋0", altParPush) :
   [(textual "κ"<>seqName s, Deriv ["Θ"] (derivContext d ++ [xi]) 
      (Cut "z" "_z" (meta "C") 
           l
           (subst ([1..l] ++ [0])
            s) whatC))
   | d <- map fillTypes [
       parRule,
       crossRule,
       oneRule False,
       plusRule,
       withRule False True,
       zeroRule,
       forallRule False,
       existsRule,
       questRule False,
       bangRule,
       weakenRule False,
       contractRule False
       ],
     let l = length $ derivContext d
         s = derivSequent d
   ]



