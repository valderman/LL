module Rules where

import LL
import Pretty
import Symheap

tgamma = meta "Γ"
tdelta = meta "Δ"

gamma = ("?γ",tgamma)
delta = ("?δ",tdelta)

whatA = What "a"
whatB = What "b"
whatC = What "c"
whatD = What "d"

axRule     = Deriv ["Θ"] [("x",meta "A"),("y",neg (meta "A"))] (Ax dum)
cutRule    = Deriv ["Θ"] [gamma,delta] (Cut "x" "y" (meta "A") 1 whatA whatB)
crossRule  = Deriv ["Θ"] [gamma, ("z",meta "A" :⊗: meta "B"),delta] (Cross dum "x" "y" 1 whatA)
parRule    = Deriv ["Θ"] [gamma, ("z",meta "A" :|: meta "B"),delta] (Par dum "x" "y" 1 whatA whatB)
withRule b = Deriv ["Θ"] [gamma,("z",meta "A" :&: meta "B")] (With "x" b 1 (What "a"))
plusRule   = Deriv ["Θ"] [gamma,("z",meta "A" :⊕: meta "B")] (Plus "x" "y" 1 whatA whatB)
oneRule    = Deriv ["Θ"] [gamma,("x",One)] (SOne 1 whatA)
zeroRule   = Deriv ["Θ"] [gamma,("x",Zero)] (SZero 1)
botRule    = Deriv ["Θ"] [("x",Bot)] SBot
forallRule = Deriv ["Θ"] [gamma,("z",Forall "α" (Meta True "A" [var 0]))] $ TApp dum "x" 1 (meta "B") whatA
existRule  = Deriv ["Θ"] [gamma,("z",Exists "α" (Meta True "A" [var 0]))] $ TUnpack "x" 1 whatA
offerRule  = Deriv ["Θ"] [("?γ",Bang (meta "Δ")),("z",Quest (meta "A"))] $ Offer "_x" 1 whatA
demandRule = Deriv ["Θ"] [gamma,("z",Bang (meta "A"))] $ Demand "x" dum 1 whatB
ignoreRule = Deriv ["Θ"] [gamma,("z",Bang (meta "A"))] $ Ignore 1 whatA
aliasRule  = Deriv ["Θ"] [gamma,("z",Bang (meta "A"))] $ Alias 1 "z'" whatA






