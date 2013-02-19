{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import Text.PrettyPrint.HughesPJ hiding ((<>))
import Data.String

type Name = Doc

-- | Types
data Type = Type :⊕: Type
          | Type :⊗: Type
          | Type :⊸: Type
          | Type :&: Type 
          | Zero | One | Top | Bot
          | TVar Bool Int
          | Forall Name Type
          | Exists Name Type
                               
subst0 x = x:map var [0..]

-- | Sequents                              
data Seq = Exchange [Int] Seq -- Permute variables
         | Ax -- Exactly 2 vars
         | Cut Name Type Int Seq Seq -- new vars in position 0
           
         | Cross Name Name Int Seq
         | Par Int Seq Seq -- splits at given pos.
         | Plus Int Seq Seq
         | Amp Bool Int Seq
           
         | SOne Int Seq
         | SZero Int
         | SBot
         | What
           
         | TApp Int Type Seq
         | TUnpack Int Seq

class Substitute a where
  (∙) :: Subst -> a -> a
         
instance Substitute Type where
  (∙) = apply
        
instance Substitute Name where
  _ ∙ x = x

instance (Substitute a, Substitute b) => Substitute (a,b) where
  f ∙ (x,y) = (f∙x, f∙y)

instance (Substitute a) => Substitute [a] where
  f ∙ xs = map (f ∙) xs
             
               
type Subst = [Type]
var = TVar True

wk :: Subst
wk = map var [1..]

if_ True f = id
if_ False f = f

apply :: Subst -> Type -> Type
apply f t = case t of
  x :⊕: y -> s x :⊕: s y
  x :&: y -> s x :&: s y
  x :⊸: y -> s x :⊸: s y
  x :⊗: y -> s x :⊗: s y
  Zero -> Zero
  One -> One
  Top -> Top
  Bot -> Bot
  TVar pol x -> if_ pol neg (f!!x)
  Forall w t -> Forall w (s' t)
  Exists w t -> Forall w (s' t)
 where s = apply f
       s' = apply (var 0 : wk ∙ f)
  
neg (x :⊗: y) = x :⊸: neg y
neg (x :⊸: y) = x :⊗: neg y
neg (x :⊕: y) = neg x :&: neg y
neg (x :&: y) = neg x :⊕: neg y
neg Zero = Top
neg Top = Zero
neg Bot = One
neg One = Bot
neg (Exists v t) = Forall v (neg t)
neg (Forall v t) = Exists v (neg t)
neg (TVar b x) = TVar (not b) x

prn p k = if p > k then parens else id

pType :: Int -> [Name] -> Type -> Doc
pType p vs (Forall v t) = prn p 0 $ "∀" <> v <> ". "  <> pType 0 (v:vs) t
pType p vs (Exists v t) = prn p 0 $ "∃" <> v <> ". "  <> pType 0 (v:vs) t
pType p vs (x :⊸: y) = prn p 0 $ pType 1 vs x <> " ⊸ " <> pType 0 vs y
pType p vs (x :⊕: y) = prn p 1 $ pType 2 vs x <> " ⊕ " <> pType 1 vs y
pType p vs (x :⊗: y) = prn p 2 $ pType 2 vs x <> " ⊗ " <> pType 2 vs y
pType p vs (x :&: y) = prn p 3 $ pType 3 vs x <> " & " <> pType 3 vs y
pType p vs Zero = "0"
pType p vs One = "1"
pType p vs Top = "⊤"
pType p vs Bot = "⊥"
pType p vs (TVar True x) = vs!!x 
pType p vs (TVar False x) = "~" <> (vs!!x)

instance Show Type where
  show x = render $ pType 0 ["v" <> int i | i <- [0..]] x

pSeq :: [Name] -> [(Name,Type)] -> Seq -> Doc
pSeq ts vs s0 = case s0 of
  Ax -> vv 0 <> " ↔ " <> vv 1
  (Cut v vt x s t) -> "connect new " <> v <> " in {" <> 
                      vcat [pSeq ts ((v,neg vt):v0) s<> ";",
                            pSeq ts ((v,vt):v1) t] <>"}"
    where (v0,v1) = splitAt x vs
  (Cross v v' x t) -> "let " <> v <> "," <> v' <> " = " <> w <> " in " $$ 
                      pSeq ts (v0++(v,vt):(v',vt'):v1) t
    where (v0,(w,(vt :⊗: vt')):v1) = splitAt x vs
  (Par x s t) -> "connect {"<> vcat [w <> " : " <>  pType 0 ts (neg vt) <> " in " <> pSeq ts (v0++[(w,neg vt)]) s <> ";",
                                     w <> " : " <>  pType 0 ts      vt' <> " in " <> pSeq ts ((w,vt'):v1) t] <>"}"
    where (v0,(w,(vt :⊸: vt')):v1) = splitAt x vs
  (Plus x s t) -> "case " <> w <> " of {" <> 
                  vcat ["inl " <> w <> " ↦ " <> pSeq ts (v0++(w,vt ):v1) s<> ";", 
                        "inr " <> w <> " ↦ " <> pSeq ts (v0++(w,vt'):v1) t] <> "}"
    where (v0,(w,(vt :⊕: vt')):v1) = splitAt x vs
  (Amp b x t) -> "let " <> w <> " = " <> c <> " " <> w <> " in " $$ 
                 pSeq ts (v0++(w,wt):v1) t
     where (c,wt) = case b of True -> ("fst",vt); False -> ("snd",vt')
           (v0,(w,(vt :&: vt')):v1) = splitAt x vs
  SBot -> v 
     where ((v,Bot):_) = vs
  (SZero x) -> "dump " <> pCtx ts (v0 ++ v1) <> " in " <> w
     where (v0,(w,Zero):v1) = splitAt x vs
  (SOne x t ) -> "let ◇ = " <> w <> " in " $$ pSeq ts (v0++v1) t
    where (v0,(w,One):v1) = splitAt x vs
  (Exchange p t) -> pSeq ts [vs !! i | i <- p] t        
  (TApp x tyB s) -> "let " <> w <> " = " <> w <> "∙" <> pType 0 ts tyB <> " in " $$ pSeq ts (v0++(w,subst0 tyB ∙ tyA):v1) s
    where (v0,(w,Forall _ tyA):v1) = splitAt x vs
  (TUnpack x s) -> "let ⟨" <> tw <> "," <> w <> "⟩ = " <> w <> " in " $$ pSeq (tw:ts) ((wk ∙ v0)++(w,tyA):(wk ∙ v1)) s
    where (v0,(w,Exists tw tyA):v1) = splitAt x vs
  What -> braces $ pCtx ts vs
 where vv = vax ts vs
       
vax ts vs x = if x < length vs then let (v,t) = vs!!x in v <> " : " <> pType 0 ts t
                               else "v" <> int (x-length vs)

instance Show Seq where
  show = render . pSeq [] []

pCtx :: [Name] -> [(Name,Type)] ->  Doc
pCtx ts vs = sep $ punctuate comma $ [v <> " : " <> (pType 0 ts t) | (v,t) <- vs]
  
----------------------

data Deriv = Deriv {derivTypeVars :: [Name], derivContext :: [(Name,Type)], derivSequent :: Seq}

instance Show Deriv where
  show (Deriv ts vs s) = render $ (pCtx ts vs <> " ⊢") $$ pSeq ts vs s

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