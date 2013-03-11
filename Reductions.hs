module Reductions where
import Data.Monoid
import LL
import Pretty

gamma = (mempty,meta "Γ")
delta = (mempty,meta "Δ")

whatA = What "a"
whatB = What "b"
whatC = What "c"
whatD = What "d"

cutWithPlus b = Deriv ["Θ"] [gamma,delta]
                (Cut "x" (meta "A" :⊕: meta "B") 1 
                (With b 0 (What "a")) 
                (Plus 0 (What "b") (What "c")))
              
cutParCross = Deriv ["Θ"] [gamma,delta,(mempty,meta "Ξ")]
              (Cut "x" (meta "A" :⊗: meta "B") 2 
               (Exchange [1,0,2] $ Par dum 1 whatA whatB)
               (Cross dum "x" "y" 0 whatC))

cutBang = Deriv ["Θ"] [(mempty, Bang (meta "Γ")), delta] $
          Cut "x" (Bang $ meta "A") 1 (Offer 0 whatA) (Demand dum 0 whatB)

cutContract = Deriv ["Θ"] [(mempty, Bang (meta "Γ")), delta] $
          Cut "x" (Bang $ meta "A") 1 (Offer 0 whatA) (Alias 0 "y" whatB)

cutIgnore = Deriv ["Θ"] [(mempty, Bang (meta "Γ")), delta] $
            Cut "x" (Bang $ meta "A") 1 (Offer 0 whatA) (Ignore 0 whatB)

cutUnit = Deriv ["Θ"] [gamma] $ Cut "x" One 0 SBot (SOne 0 whatA)

cutQuant = Deriv ["Θ"] [gamma,delta] $
           Cut "x" (Exists "α" (Meta True "A" [var 0])) 1 (TApp 0 (meta "B") whatA) (TUnpack 0 whatB)




