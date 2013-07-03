module Rules where
import MarXup
import MarXup.Tex
import MarXup.Latex
import LL
import Pretty
import Symheap
import Numeric (showIntAtBase)

tgamma = meta "Γ"
tdelta = meta "Δ"

tA = meta "A"
tB = meta "B"
tC = meta "C"

gamma = ("?γ",tgamma)
gammaBang = ("?γ",Bang tgamma)
delta = ("?δ",tdelta)
xi    = ("?ξ",meta "Ξ")
xi' i = ("?ξ" ++ show i,meta $ "Ξ" ++ subscriptShow i)

scriptShow ::  (Integral a, Show a) => [Char] -> a -> [Char]
scriptShow []             _ = error "scriptShow on empty list"
scriptShow (minus:digits) x = if x < 0 then minus : sho (negate x) else sho x
  where sho z = showIntAtBase 10 (\i -> digits !! i) z []

subscriptShow :: Int -> String
subscriptShow  = scriptShow "-₀₁₂₃₄₅₆₇₈₉"


whatA = What "a" []
whatB = What "b" []
whatC = What "c" []
whatD = What "d" []

tAofAlpha = Meta True "A" [var 0]
tAofB     = Meta True "A" [tB]


axRule     = Deriv ["Θ"] [("x",meta "A"),("y",neg (meta "A"))] (Ax dum)
cutRule    = Deriv ["Θ"] [gamma,xi] (Cut "x" "y" (meta "A") 1 whatA whatB)
crossRule' β = Deriv ["Θ"] [gamma, ("z",meta "A" :⊗: meta "B"),delta] (Cross β dum "x" "y" 1 whatA)
parRule' β  = Deriv ["Θ"] [gamma, ("z",meta "A" :|: meta "B"),delta] (Par β dum "x" "y" 1 whatA whatB)
crossRule = crossRule' False
parRule = parRule' False
withRule β b = Deriv ["Θ"] [gamma,("z",meta "A" :&: meta "B")] (With  β dum "x" b 1 whatA)
plusRule   = Deriv ["Θ"] [gamma,("z",meta "A" :⊕: meta "B")] (Plus "x" "y" 1 whatA whatB)
oneRule  β   = Deriv ["Θ"] [gamma,("x",One)] (SOne  β 1 whatA)
zeroRule   = Deriv ["Θ"] [gamma,("x",Zero)] (SZero 1)
botRule β    = Deriv ["Θ"] [("x",Bot)] (SBot β)
forallRule  β = Deriv ["Θ"] [gamma,("z",Forall "α" tAofAlpha)] $ TApp β dum "x" 1 (meta "B") whatA
existsRule  = Deriv ["Θ"] [gamma,("z",Exists "α" tAofAlpha)] $ TUnpack "x" 1 whatA
questRule β  = Deriv ["Θ"] [("?γ",Bang (meta "Δ")),("z",Quest (meta "A"))] $ Offer β "_x" 1 whatA
bangRule = Deriv ["Θ"] [gamma,("z",Bang (meta "A"))] $ Demand "x" dum 1 whatB
weakenRule = Deriv ["Θ"] [gamma,("z",Bang (meta "A"))] $ Ignore 1 whatA
contractRule β  = Deriv ["Θ"] [gamma,("z",Bang (meta "A"))] $ Alias β 1 "z'" whatA


bosonRules = [crossRule' True, parRule' True, withRule True True, oneRule True, botRule True, questRule True, contractRule True]
-- channelRule = fillTypes $ Deriv ["θ"] [("x",meta "A"),("y",neg (meta "A"))] (Channel dum)
chanPlusRule b = Deriv ["θ"] [("z",neg (tA :⊕: tB)),("x",if b then tA else tB)] (ChanPlus b)

chanCrossRule = crossRule' True
chanParRule = parRule' True

chanTypRule = Deriv ["θ"] [("z",neg (Exists "α" tAofAlpha)),("x",tAofB)] (ChanTyp tB)
chanEmptyRule n = Deriv ["θ"] (("z",Bang tA):[("x"++show k,neg (Bang tA))|k<-[1..n]]) (MemEmpty tA n)
chanFullRule n = Deriv ["θ"] (("z",tA):[("x"++show k,neg (Bang tA))|k<-[1..n]]) (MemFull tA n)

memRule n = Deriv ["θ"] [gammaBang,xi] $ Mem tA 1 n whatA whatB

