import LL
import Pretty
import SysStepper

t0 = Forall "α" $ Forall "β" $ (a :⊗: b) ⊸ (b :⊗: a)
  where a = var 1
        b = var 0
        
        
ctx = [("x",var 0 :⊗: var 1),("y",var 0 ⊸ TVar False 1)]  

s0 = Deriv ["a","b"] ctx $
       Cross dum "v" "w" 0 $
       Exchange [0,2,1] $
       par "x" "y" 1 ax
             ax
bool = One :⊕: One        

s1 = Deriv [] [("x",bool), ("y",neg (bool :⊗: bool))] $
       Plus "m" "n" 0 (SOne 0 $ Par dum "a" "b" 0 (With "c" True  0 SBot) (With "d" True  0 SBot)) 
              (SOne 0 $ Par dum "a" "b" 0 (With "c" False 0 SBot) (With "d" False 0 SBot)) 

test = Deriv [] [("x",neg t0)]  $
       TUnpack "y" 0 $
       TUnpack "z" 0 $
       Cross dum "m" "n" 0 $ 
       Cross dum "o" "p" 0 $ 
       Exchange [1,2,0] $ 
       Par dum "a" "b" 1 ax ax

ax = Ax dum
par = Par dum

-- | Test exponentials
expTest = Deriv ["α"] [("x",neg (Bang (var 0) ⊸ (Bang (var 0) :⊗: Bang (var 0))))] $
          Cross dum "y" "z" 0 $
          Alias 0 "w" $
          Exchange [0,2,1] $
          par "a" "b" 1 ax ax

-- | Test exchange
exchTest = Deriv ["a","b","c"] [("x",var 0),("y",One),("z",neg (var 0))] $
           Exchange [1,2,0] $ SOne 0 $ ax

-- | A program for boolean not
notProgram
    = Deriv [] [("x",bool),("r",neg bool)]
    $ Plus "y" "z" 0
        (SOne 0 (With "u" True 0 SBot))
        (SOne 0 (With "u" False 0 SBot))

-- | The not program wrapped as a function
notFunction
    = Deriv [] [("not",bool ⊸ bool)]
    $ Cross dum "x" "r" 0 (derivSequent notProgram)


-- | Testing cuts
cutTest = Deriv [] [("x",Zero)] $
          Cut "a" "b" Bot 1
            (SOne 0 (SZero 0))
            SBot

mpTest = Deriv ["α"] [("p",var 0 :⊗: neg (var 0))] $
         Cross (var 0) "a" "b" 0 $
         Cut "x" "y" (var 0) 1
           (Ax (var 0))
           (Ax (neg (var 0)))

-- Small example program

two = One :⊕: One

bType 0 = One
bType n = two :⊗: bType (n-1)

semiAdder = Deriv [] [("x",two),("y",two),("z",neg (two :⊗: two))] $
            Plus "t" "u" 0
              (What "a")
              (SOne 0 $
               Plus "v" "w" 0
               (SOne 0 $
                Par (neg two) "k" "l" 0
                  (With "m" True 0 SBot)
                  (With "n" False 0 SBot)
               )
               (What "c"))
--p = Deriv [] [("x",bType b),("y",bType b),("z",neg (bType b))] $
    