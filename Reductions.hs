module Reductions where
import Data.Monoid
import LL
import Pretty

whatA = What "a"
whatB = What "b"
whatC = What "c"
whatD = What "d"

dum = Meta True "dummy type" []

cutWithPlus = Deriv ["Γ","Δ","A","B"] [(mempty,var 0), (mempty,var 1)] 
              (Cut "x" (var 2 :⊕: var 3) 1 
               (With True 0 (What "a")) 
               (Plus 0 (What "b") (What "c")))
              
cutParCross = Deriv ["Γ","Δ","Ξ","A","B"] [(mempty,var 0), (mempty,var 1), (mempty,var 2)]
              (Cut "x" (var 3 :⊗: var 4) 2 
               (Exchange [1,0,2] $ Par dum 1 whatA whatB)
               (Cross dum "x" "y" 0 whatC))

cutBang = Deriv ["Γ","Δ","A"] [(mempty, Bang (var 0)), (mempty, var 1)] $
          Cut "x" (Bang (var 2)) 1 (Offer 0 whatA) (Demand dum 0 whatB)

cutContract = Deriv ["Γ","Δ","A"] [(mempty, Bang (var 0)), (mempty, var 1)] $
          Cut "x" (Bang (var 2)) 1 (Offer 0 whatA) (Alias 0 "y" whatB)

cutIgnore = Deriv ["Γ","Δ","A"] [(mempty, Bang (var 0)), (mempty, var 1)] $
            Cut "x" (Bang (var 2)) 1 (Offer 0 whatA) (Ignore 0 whatB)

cutQuant = Deriv ["Γ","Δ","A","B"] [(mempty, var 0), (mempty, var 1)] $
           Cut "x" (Exists "α" (var 3)) 1 (TApp 0 (var 3) whatA) (TUnpack 0 whatB)

gamma = (mempty,Meta True "Γ" [])
delta = (mempty,Meta True "Δ" [])

cutQuant' = Deriv ["Θ"] [gamma,delta] $
           Cut "x" (Exists "α" (meta "A" [var 0])) 1 (TApp 0 (meta "B" []) whatA) (TUnpack 0 whatB)

cutUnit = Deriv ["Γ"] [(mempty, var 0)] $ Cut "x" One 0 SBot (SOne 0 whatA)



