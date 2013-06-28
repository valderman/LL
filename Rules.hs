module Rules where
import MarXup
import MarXup.Tex
import MarXup.Latex
import LL
import Pretty
import Symheap

tgamma = meta "Γ"
tdelta = meta "Δ"

tA = meta "A"
tB = meta "B"
tC = meta "C"

gamma = ("?γ",tgamma)
delta = ("?δ",tdelta)
xi    = ("?ξ",meta "Ξ")

whatA = What "a" []
whatB = What "b" []
whatC = What "c" []
whatD = What "d" []

tAofAlpha = Meta True "A" [var 0]
tAofB     = Meta True "A" [tB]


axRule     = Deriv ["Θ"] [("x",meta "A"),("y",neg (meta "A"))] (Ax dum)
cutRule    = Deriv ["Θ"] [gamma,xi] (Cut "x" "y" (meta "A") 1 whatA whatB)
crossRule' b = Deriv ["Θ"] [gamma, ("z",meta "A" :⊗: meta "B"),delta] (Cross b dum "x" "y" 1 whatA)
parRule' b  = Deriv ["Θ"] [gamma, ("z",meta "A" :|: meta "B"),delta] (Par b dum "x" "y" 1 whatA whatB)
crossRule = crossRule' False
parRule = parRule' False
withRule b = Deriv ["Θ"] [gamma,("z",meta "A" :&: meta "B")] (With dum "x" b 1 whatA)
plusRule   = Deriv ["Θ"] [gamma,("z",meta "A" :⊕: meta "B")] (Plus "x" "y" 1 whatA whatB)
oneRule    = Deriv ["Θ"] [gamma,("x",One)] (SOne 1 whatA)
zeroRule   = Deriv ["Θ"] [gamma,("x",Zero)] (SZero 1)
botRule    = Deriv ["Θ"] [("x",Bot)] SBot
forallRule = Deriv ["Θ"] [gamma,("z",Forall "α" tAofAlpha)] $ TApp dum "x" 1 (meta "B") whatA
existsRule  = Deriv ["Θ"] [gamma,("z",Exists "α" tAofAlpha)] $ TUnpack "x" 1 whatA
questRule  = Deriv ["Θ"] [("?γ",Bang (meta "Δ")),("z",Quest (meta "A"))] $ Offer "_x" 1 whatA
bangRule = Deriv ["Θ"] [gamma,("z",Bang (meta "A"))] $ Demand "x" dum 1 whatB
weakenRule = Deriv ["Θ"] [gamma,("z",Bang (meta "A"))] $ Ignore 1 whatA
contractRule  = Deriv ["Θ"] [gamma,("z",Bang (meta "A"))] $ Alias 1 "z'" whatA


-- channelRule = fillTypes $ Deriv ["θ"] [("x",meta "A"),("y",neg (meta "A"))] (Channel dum)
chanPlusRule b = Deriv ["θ"] [("z",neg (tA :⊕: tB)),("x",if b then tA else tB)] (ChanPlus b)
chanCrossRule = crossRule' True
chanParRule = parRule' True
chanTypRule = Deriv ["θ"] [("z",neg (Exists "α" tAofAlpha)),("x",tAofB)] (ChanTyp tB)
chanEmptyRule n = Deriv ["θ"] (("z",Bang tA):[("x"++show k,neg (Bang tA))|k<-[1..n]]) (MemEmpty tA n)
chanFullRule n = Deriv ["θ"] (("z",tA):[("x"++show k,neg (Bang tA))|k<-[1..n]]) (MemFull tA n)
