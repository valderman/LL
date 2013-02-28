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

cutWithPlus = Deriv ["Γ","Δ","A","B"] [(mempty,var 0), (mempty,var 1)] 
              (Cut "x" (var 2 :⊕: var 3) 1 
               (With True 0 What) 
               (Plus 0 What What))
              
cutParCross = Deriv ["Γ","Δ","Θ","A","B"] [(mempty,var 0), (mempty,var 1), (mempty,var 2)]
              (Cut "x" (var 3 :⊗: var 4) 2 
               (Exchange [1,0,2] $ Par 1 What What)
               (Cross "x" "y" 0 What))

cutBang = Deriv ["Γ","Δ","A"] [(mempty, Bang (var 0)), (mempty, var 1)] $
          Cut "x" (Bang (var 2)) 1 (Offer 0 What) (Demand 0 What)

cutContract = Deriv ["Γ","Δ","A"] [(mempty, Bang (var 0)), (mempty, var 1)] $
          Cut "x" (Bang (var 2)) 1 (Offer 0 What) (Alias 0 "y" What)

cutIgnore = Deriv ["Γ","Δ","A"] [(mempty, Bang (var 0)), (mempty, var 1)] $
            Cut "x" (Bang (var 2)) 1 (Offer 0 What) (Ignore 0 What)

cutQuant = Deriv ["Γ","Δ","A","B"] [(mempty, Bang (var 0)), (mempty, var 1)] $
           Cut "x" (Exists "α" (var 3)) 1 (TApp 0 (var 3) What) (TUnpack 0 What)

cutUnit = Deriv ["Γ"] [(mempty, var 0)] $ Cut "x" One 0 SBot (SOne 0 What)

deriv :: Deriv TeX -> Tex Label
deriv (Deriv tvs vs s) = derivationTree [] $ texSeq tvs vs s


main = render $ latexDocument "article" ["11pt"] preamble $ @"

@deriv(cutWithPlus) --- @deriv(eval(cutWithPlus))

@deriv(cutParCross) --- @deriv(eval(cutParCross))

@deriv(cutBang) --- @deriv(eval(cutBang))

@deriv(cutContract) --- @deriv(eval(cutContract))

@deriv(cutIgnore) --- @deriv(eval(cutIgnore))

@deriv(cutUnit) --- @deriv(eval(cutUnit))

@deriv(cutQuant) --- @deriv(eval(cutQuant))

Done.

@"