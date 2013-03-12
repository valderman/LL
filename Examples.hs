import LL

t0 = Forall "α" $ Forall "β" $ (a :⊗: b) ⊸ (b :⊗: a)
  where a = var 1
        b = var 0
        
        
ctx = [("x",var 0 :⊗: var 1),("y",var 0 ⊸ TVar False 1)]  

s0 = Deriv ["a","b"] ctx $
       Cross dum "v" "w" 0 $
       Exchange [0,2,1] $
       par 1 ax
             ax
bool = One :⊕: One        

s1 = Deriv [] [("x",bool), ("y",neg (bool :⊗: bool))] $
       Plus 0 (SOne 0 $ Par dum 0 (With True  0 SBot) (With True  0 SBot)) 
              (SOne 0 $ Par dum 0 (With False 0 SBot) (With False 0 SBot)) 

test = Deriv [] [("x",neg t0)]  $
       TUnpack 0 $
       TUnpack 0 $
       Cross dum "x" "y" 0 $ 
       Cross dum "v" "w" 0 $ 
       Exchange [1,2,0] $ 
       Par dum 1 ax ax

ax = Ax dum
par = Par dum

-- | Test exponentials
expTest = Deriv ["a"] [("x",neg (Bang (var 0) ⊸ (Bang (var 0) :⊗: Bang (var 0))))] $
          Cross dum "y" "z" 0 $
          Alias 0 "w" $
          Exchange [0,2,1] $
          par 1 ax ax

-- | Test exchange
exchTest = Deriv ["a","b","c"] [("x",var 0),("y",One),("z",neg (var 0))] $
           Exchange [1,2,0] $ SOne 0 $ ax
