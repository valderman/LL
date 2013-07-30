module Examples where

import AM
import LL
import Pretty
import Symheap
import SysStepper

t0 = Forall "α" $ Forall "β" $ (a :⊗: b) ⊸ (b :⊗: a)
  where a = var 1
        b = var 0


ctx = [("x",var 0 :⊗: var 1),("y",var 0 ⊸ TVar False 1)]

s0 = Deriv ["a","b"] ctx $
       Cross False dum "v" "w" 0 $
       Exchange [0,2,1] $
       par "x" "y" 1 ax
             ax
bool = One :⊕: One

s1 = Deriv [] [("x",bool), ("y",neg (bool :⊗: bool))] $
       Plus "m" "n" 0 (SOne False 0 $ Par False dum "a" "b" 0 (With False dum "c" True  0 SBot) (With False dum "d" True  0 SBot))
              (SOne False 0 $ Par False dum "a" "b" 0 (With False dum "c" False 0 SBot) (With False dum "d" False 0 SBot))

test = Deriv [] [("x",neg t0)]  $
       TUnpack "y" 0 $
       TUnpack "z" 0 $
       Cross False dum "m" "n" 0 $
       Cross False dum "o" "p" 0 $
       Exchange [1,2,0] $
       Par False dum "a" "b" 1 ax ax

ax = Ax dum
par = Par False dum

-- | Test exponentials
expTest = Deriv ["α"] [("x",neg (Bang (var 0) ⊸ (Bang (var 0) :⊗: Bang (var 0))))] $
          Cross False dum "y" "z" 0 $
          Alias False 0 "w" $
          Exchange [0,2,1] $
          par "a" "b" 1 ax ax

-- | Test exchange
exchTest = Deriv ["a","b","c"] [("x",var 0),("y",One),("z",neg (var 0))] $
           Exchange [1,2,0] $ SOne False 0 ax

-- | A program for boolean not
notProgram
    = Deriv [] [("x",bool),("r",neg bool)]
    $ Plus "y" "z" 0
        (SOne False 0 (With False dum "u" True 0 SBot))
        (SOne False 0 (With False dum "u" False 0 SBot))

-- | The not program wrapped as a function
notFunction
    = Deriv [] [("not",bool ⊸ bool)]
    $ Cross False dum "x" "r" 0 (derivSequent notProgram)


-- | Testing cuts
cutTest = Deriv [] [("x",Zero)] $
          Cut "a" "b" Bot 1
            (SOne False 0 (SZero 0))
            SBot

mpTest = Deriv ["α"] [("p",var 0 :⊗: neg (var 0))] $
         Cross False (var 0) "a" "b" 0 $
         Cut "x" "y" (var 0) 1
           (Ax (var 0))
           (Ax (neg (var 0)))

-- Small example program

two = One :⊕: One

bType 0 = One
bType n = two :⊗: bType (n-1)

semiAdder = Deriv [] [("x",two),("y",two),("z",neg (two :⊗: two))] $
            Plus "t" "u" 0
              (SOne False 0 $
               Plus "v" "w" 0
               (SOne False 0 $
                Par False (neg two) "k" "l" 0
                  (With False dum "m" True 0 SBot)
                  (With False dum "n" True 0 SBot)
               )
               (SOne False 0 $
                Par False (neg two) "k" "l" 0
                  (With False dum "m" True 0 SBot)
                  (With False dum "n" False 0 SBot)
               ))
              (Par False (neg two) "k" "l" 2
                 (SOne False 0 $
                  Ax two)
                 (With False dum "n" False 0 SBot)
              )
--p = Deriv [] [("x",bType b),("y",bType b),("z",neg (bType b))] $
--

swapType = Forall "α" $ Forall "β" $ (a :⊗: b) `Lollipop` (b :⊗: a)
  where
    a = var 1
    b = var 0

swap = Deriv [] [("swapee",neg $ swapType)]
   $ TUnpack "swapee'" 0
   $ TUnpack "swapee''" 0
   $ Cross False dum "input" "output" 0
   $ Cross False dum "a_in" "b_in" 0
   $ Exchange [1,2,0]
   $ Par False dum "b_out" "a_out" 1 (Ax dum) (Ax dum)

use_swap_once = Deriv ["a","b"] [("in",a⊗b),("out",neg (b⊗a))]
    $ Cut "mkswap" "swap" t0 0 (derivSequent swap)
    $ TApp False dum "swap'" 0 a
    $ TApp False dum "swap''" 0 b
    $ Exchange [1,0,2]
    $ Par False dum "u" "v" 1 (Ax dum) (Ax dum)
  where
    a = var 0
    b = var 1

use_swap_twice = Deriv ["a","b"] [("input",a⊗b),("out",neg (a⊗b))]
    $ Cut "mkswap" "swapfun" (Bang t0) 0 (Offer False "swappers" 0 $ derivSequent swap)
    $ Alias False 0 "swapfun'"
    $ Exchange [0,2,1,3]
    $ Cut "flop" "glop" (b⊗a) 2
        ( Demand "swap" dum 1
        $ TApp False dum "swap'" 1 a
        $ TApp False dum "swap''" 1 b
        $ Exchange [2,1,0]
        $ Par False dum "u" "v" 1 (Ax dum) (Ax dum)
        )
        ( Demand "swap" dum 1
        $ TApp False dum "swap'" 1 b
        $ TApp False dum "swap''" 1 a
        $ Par False dum "u" "v" 1 (Ax dum) (Ax dum)
        )
  where
    a = var 0
    b = var 1

