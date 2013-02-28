t0 = Forall "α" $ Forall "β" $ (a :⊗: b) :⊸: (b :⊗: a)
  where a = TVar True 1
        b = TVar True 0
        
        
ctx = [("x",TVar True 0 :⊗: TVar True 1),("y",TVar True 0 :⊸: TVar False 1)]  

cx0 = putStrLn $ render $ pCtx ["a","b"] ctx

s0 = Deriv ["a","b"] ctx $
       Cross "v" "w" 0 $
       Exchange [0,2,1] $
       Par 1 Ax
             Ax


bool = One :⊕: One        

s1 = Deriv [] [("x",bool), ("y",neg (bool :⊗: bool))] $
       Plus 0 (SOne 0 $ Par 0 (Amp True  0 SBot) (Amp True  0 SBot)) 
              (SOne 0 $ Par 0 (Amp False 0 SBot) (Amp False 0 SBot)) 

test = Deriv [] [("x",neg t0)]  $
       TUnpack 0 $
       TUnpack 0 $
       Cross "x" "y" 0 $ 
       Cross "v" "w" 0 $ 
       Exchange [1,2,0] $ 
       Par 1 Ax Ax

expTest = Deriv ["a"] [("x",neg (Bang (TVar True 0) :⊸: (Bang (TVar True 0) :⊗: Bang (TVar True 0) )))] $
          Cross "y" "z" 0 $
          Alias 0 "w" $
          Exchange [0,2,1] $
          Par 1 Ax Ax