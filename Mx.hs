{-# OPTIONS_GHC -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup -F #-}

import MarXup.Latex
import MarXup.Tex
import MarXup.DerivationTrees
import Control.Applicative

import TexPretty
import LL

preamble :: Tex ()
preamble = do
  usepackage ["utf8"] "inputenc"
  usepackage [] "graphicx"
  cmd "input" (tex "unicodedefs")
  usepackage [] "amsmath"
  usepackage [] "cmll"

whatA = What "a"
whatB = What "b"
whatC = What "c"
whatD = What "d"

cutWithPlus = Deriv ["Γ","Δ","A","B"] [(mempty,var 0), (mempty,var 1)] 
              (Cut "x" (var 2 :⊕: var 3) 1 
               (With True 0 (What "a")) 
               (Plus 0 (What "b") (What "c")))
              
cutParCross = Deriv ["Γ","Δ","Θ","A","B"] [(mempty,var 0), (mempty,var 1), (mempty,var 2)]
              (Cut "x" (var 3 :⊗: var 4) 2 
               (Exchange [1,0,2] $ Par 1 whatA whatB)
               (Cross "x" "y" 0 whatC))

cutBang = Deriv ["Γ","Δ","A"] [(mempty, Bang (var 0)), (mempty, var 1)] $
          Cut "x" (Bang (var 2)) 1 (Offer 0 whatA) (Demand 0 whatB)

cutContract = Deriv ["Γ","Δ","A"] [(mempty, Bang (var 0)), (mempty, var 1)] $
          Cut "x" (Bang (var 2)) 1 (Offer 0 whatA) (Alias 0 "y" whatB)

cutIgnore = Deriv ["Γ","Δ","A"] [(mempty, Bang (var 0)), (mempty, var 1)] $
            Cut "x" (Bang (var 2)) 1 (Offer 0 whatA) (Ignore 0 whatB)

cutQuant = Deriv ["Γ","Δ","A","B"] [(mempty, Bang (var 0)), (mempty, var 1)] $
           Cut "x" (Exists "α" (var 3)) 1 (TApp 0 (var 3) whatA) (TUnpack 0 whatB)

cutUnit = Deriv ["Γ"] [(mempty, var 0)] $ Cut "x" One 0 SBot (SOne 0 whatA)

deriv :: Deriv TeX -> Tex Label
deriv (Deriv tvs vs s) = derivationTree [] $ texSeq False tvs vs s

program :: Deriv TeX -> Tex ()
program (Deriv tvs vs s) = math (block (texProg tvs vs s))


main = render $ latexDocument "article" ["11pt"] preamble $ @"

@deriv(cutWithPlus) --- @deriv(eval(cutWithPlus))


@program(cutWithPlus) --- @program(eval(cutWithPlus))

@program(cutParCross) --- @program(eval(cutParCross))

@program(cutBang) --- @program(eval(cutBang))

@program(cutContract) --- @program(eval(cutContract))

@program(cutIgnore) --- @program(eval(cutIgnore))

@program(cutUnit) --- @program(eval(cutUnit))

@program(cutQuant) --- @program(eval(cutQuant))

Done.

@"