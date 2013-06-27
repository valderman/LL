{-# LANGUAGE OverloadedStrings #-}  
module Reductions where
import Data.Monoid
import LL
import Rules
import Pretty
import TexPretty 
import MarXup.Tex
import MarXup.Latex (math)

cutAx = Deriv ["Θ"] [gamma,("w",neg $ meta "A")] $
              Cut "x" "y" (meta "A") 1 (What "a" [0]) (Ax dum)

cutWithPlus b = fillTypes $ 
                Deriv ["Θ"] [gamma,xi]
                (Cut "z" "_z" (meta "A" :⊕: meta "B") 1 
                (With dum "x" b 0 (whatA)) 
                (Plus "_x" "_y" 0 (whatB) (whatC)))
              
cutParCross = Deriv ["Θ"] [gamma,delta,(mempty,meta "Ξ")]
              (Cut "z" "_z" (meta "A" :⊗: meta "B") 2 
               (Exchange [1,0,2] $ Par dum "_x" "_y" 1 whatA whatB)
               (Cross dum "x" "y" 0 whatC))

cutBang = Deriv ["Θ"] [(mempty, Bang (meta "Γ")), delta] $
          Cut "z" "_z" (Bang $ meta "A") 1 (Offer "x" 0 whatA) (Demand "_x" dum 0 whatB)

cutContract = Deriv ["Θ"] [(mempty, Bang (meta "Γ")), delta] $
          Cut "z" "_z" (Bang $ meta "A") 1 (Offer "x" 0 whatA) (Alias 0 "y" whatB)

cutIgnore = Deriv ["Θ"] [(mempty, Bang (meta "Γ")), delta] $
            Cut "z" "_z" (Bang $ meta "A") 1 (Offer "x" 0 whatA) (Ignore 0 whatB)

cutUnit = Deriv ["Θ"] [gamma] $ Cut "z" "_z" One 0 SBot (SOne 0 whatA)

cutQuant = Deriv ["Θ"] [gamma,xi] $
           Cut "z" "_z" (Exists "α" (Meta True "A" [var 0])) 1 (TApp dum "x" 0 (meta "B") whatA) (TUnpack "_x" 0 whatB)

syncRules,pushRules,chanRedRules :: [(TeX,Deriv)]
syncRules = [
    ("AxCut",cutAx),
    (math par<>"⊗",cutParCross),
    (amp<>"⊕",cutWithPlus True),
    ("?!", cutBang),
    ("⊥!",cutUnit),
    ("∃∀",cutQuant),
    ("?Contract",cutContract),
    ("?Weaken",cutIgnore)
    ] 

altParPush = Deriv ["Θ"] (derivContext parRule ++ [xi]) (Cut "z" "_z" (meta "C") 3 (Par dum "_x" "_y" 2 whatA whatB) whatB)

pushRules = (textual "κ⅋0", altParPush) :
   [(textual "κ"<>seqName s, Deriv ["Θ"] (derivContext d ++ [xi]) 
      (Cut "z" "_z" (meta "C") 
           l
           (subst ([1..l] ++ [0])
            s) whatC))
   | d <- map fillTypes [
       parRule,
       crossRule,
       oneRule,
       plusRule,
       withRule True,
       zeroRule,
       forallRule,
       existsRule,
       questRule,
       bangRule,
       weakenRule,
       contractRule
       ],
     let l = length $ derivContext d
         s = derivSequent d
   ]

chanRedRules = map (second fillTypes)
   [("bit write", leftChild $ cutWithPlus True) -- because the boson evaluation is not complete.
   ,("bit read" , Deriv ["Θ"] [("z",tA),gamma] $ Cut "x" "_x" (tA :⊕: tB) 1 (ChanPlus True)  (Plus "x" "y" 0 whatA whatB))
--   ,("output",    Deriv ["Θ"] [gamma,delta,("z",neg (tA :⊗: tB))] $ Cut "w" "_w" (tA :⊗: tB) 2 (Exchange [1,0,2] $ Par dum "x" "y" 1 whatA whatB) (Channel dum))
--   ,("input",    Deriv ["Θ"]  [("x",tA), ("y",tB),delta] $ Cut "w" "_w" (tA :⊗: tB) 2 (ChanPar dum dum) (Cross dum "x" "y" 0 whatA) )
    ]
               
