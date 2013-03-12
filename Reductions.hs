module Reductions where
import Data.Monoid
import LL
import Rules

cutWithPlus b = Deriv ["Θ"] [gamma,delta]
                (Cut "z" "_z" (meta "A" :⊕: meta "B") 1 
                (With "x" b 0 (What "a")) 
                (Plus "_x" "_y" 0 (What "b") (What "c")))
              
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

cutQuant = Deriv ["Θ"] [gamma,delta] $
           Cut "z" "_z" (Exists "α" (Meta True "A" [var 0])) 1 (TApp "x" 0 (meta "B") whatA) (TUnpack "_x" 0 whatB)




