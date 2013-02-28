{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module LL where

import Data.Monoid
import Text.PrettyPrint.HughesPJ hiding ((<>))
import Data.String

type Name = Doc

-- | Types
data Type nm = Type nm :⊕: Type nm
             | Type nm :⊗: Type nm
             | Type nm :⊸: Type nm
             | Type nm :&: Type  nm
             | Zero | One | Top | Bot
             | TVar Bool Int
             | Forall nm (Type nm)
             | Exists nm (Type nm)
             | Bang (Type nm)
             | Quest (Type nm)

subst0 x = x:map var [0..]

-- | Sequents                              
data (Seq nm) = Exchange [Int] (Seq nm) -- Permute variables
         | Ax -- Exactly 2 vars
         | Cut nm (Type nm) Int (Seq nm) (Seq nm) -- new vars in position 0
           
         | Cross nm nm Int (Seq nm)
         | Par Int (Seq nm) (Seq nm) -- splits at given pos.
         | Plus Int (Seq nm) (Seq nm)
         | Amp Bool Int (Seq nm)
           
         | SOne Int (Seq nm)
         | SZero Int
         | SBot
         | What
           
         | TApp Int (Type nm) (Seq nm)
         | TUnpack Int (Seq nm)

         | Offer Int (Seq nm)
         | Demand Int (Seq nm)
         | Ignore Int (Seq nm)
         | Alias Int nm (Seq nm)

data Cell where
  Freed :: Cell
  Tag   :: Bool -> Cell
  New   :: Cell
  Delay :: Int -> Closure -> Cell
  Q     :: (Type nm) -> CellRef -> Cell

type CellRef = Int

type TypeEnv = [Type ()]

type Env = [CellRef]

type Closure = (Seq (),Env,TypeEnv)

type Heap = [Cell]

type System = ([Closure],Heap)

runClosure :: Heap -> Closure -> Maybe (Heap,Closure)
runClosure h (Plus v a b,e,te)
  | Tag c <- h!!(e!!v) = Just (replace (e!!v) Freed h,
                             (if c then a else b,increment v e,te))
runClosure h (Cross _ _ v a,e,te)
  = Just (h,(a,el++[x,x+sizeOf te (error "Game Over!")] ++ er,te))
  where (el,x:er) = splitAt v e

replace n v h = let (l,_:r) = splitAt n h
                in l ++ v : r

increment n e = let (l,x:r) = splitAt n e
                in l ++ succ x : r

sizeOf :: TypeEnv -> Type () -> Int
sizeOf e (t1 :⊕: t2) = 1 + max (sizeOf e t1) (sizeOf e t2)
sizeOf e (t1 :⊗: t2) = sizeOf e t1 + sizeOf e t2
sizeOf e (t1 :⊸: t2) = sizeOf e t1 + sizeOf e t2
sizeOf e (t1 :&: t2) = 1 + max (sizeOf e t1) (sizeOf e t2)
sizeOf e (TVar _ v) = sizeOf e (e!!v)
sizeOf e (Forall _ _) = 1
sizeOf e (Exists _ _) = 1
sizeOf e (Bang _)     = 1
sizeOf e (Quest _)    = 1
sizeOf e _ = 0

-- Substitution

class Substitute a where
  type Nm a
  (∙) :: Subst (Nm a) -> a -> a
         
instance Substitute (Type nm)  where
  type Nm (Type nm) = nm
  (∙) = apply
        
instance Substitute (Seq nm) where
  type Nm (Seq nm) = nm
  (∙) = applyS
        
instance Substitute Name where -- yuck
  type Nm Doc = Doc
  s ∙ x = x
       
        
instance (Substitute a, Substitute b,Nm a ~ Nm b) => Substitute (a,b) where
  type Nm (a,b) = Nm a
  f ∙ (x,y) = (f∙x, f∙y)

instance (Substitute a) => Substitute [a] where
  type Nm [a] = Nm a
  f ∙ xs = map (f ∙) xs
             
               
type Subst nm = [Type nm]

var = TVar True

wk :: Subst nm
wk = map var [1..]

if_ True f = id
if_ False f = f

apply :: Subst nm -> Type nm -> Type nm
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
  Bang t -> Bang (s t)
  Quest t -> Quest (s t)
 where s = apply f
       s' = apply (var 0 : wk ∙ f)
  
applyS :: Subst nm -> (Seq nm) -> (Seq nm)
applyS f t = case t of
  (Exchange π a) -> Exchange π (s a)
  Cut w ty x a b -> Cut w (f ∙ ty) x (s a) (s b)
  Cross w w' x a -> Cross w w' x (s a)         
  Par x a b -> Par x (s a) (s b)
  Plus x a b -> Plus x (s a) (s b)
  Amp c x a -> Amp c x (s a)
  SOne x a -> SOne x (s a) 
  TApp x ty a -> TApp x (f ∙ ty) (s a)
  TUnpack x a -> TUnpack x (s' a)
  Offer x a -> Offer x (s a)
  Demand x a -> Demand x (s a)
  Ignore x a -> Ignore x (s a)
  Alias x w a -> Alias x w (s a)
  a -> a
 where s = applyS f
       s' = applyS (var 0 : wk ∙ f)
       
neg :: Type nm -> Type nm       
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
neg (Bang t) = Quest (neg t)
neg (Quest t) = Bang (neg t)

-- Hereditary cut
cut :: Int -> -- ^ size of the context
       nm -> 
       Type nm -> 
       Int -> -- ^ where to cut it
       (Seq nm) -> (Seq nm) -> (Seq nm)
cut n w ty γ (Cut w' ty' δ a b) c = cut n w ty γ (cut γ w' ty' δ a b) c
cut n w ty γ a (Cut w' ty' δ b c) = cut n w ty γ a (cut (n-γ+1) w' ty' δ b c)
cut 2 w ty 1 Ax a = a
cut n _ (ta :⊗: tb) 
           γ (Exchange π (Par δ a b)) (Cross w w' 0 c) = exchange (π++[length π..n-1]) $ cut n w ta δ a (cut (n-δ+1) w' tb (γ-δ) b c)
cut n w (ta :⊕: tb) 
           γ (Amp c 0 a) (Plus 0 s t) = cut n w (if c then ta else tb) γ a (if c then s else t)
cut n w (Exists v ty) 
           γ (TApp 0 t a) (TUnpack 0 b) = cut n w (subst0 t ∙ ty) γ a (subst0 t ∙ b)
cut n w (Bang ty) 
           γ (Offer 0 a) (Demand 0 b) = cut n w ty γ a b
cut n w ty γ (Offer 0 a) (Ignore 0 b) = b
cut n w ty γ (Offer 0 b) (Alias 0 w' a) = cut n w ty γ (Offer 0 b) (cut (n+1) w' ty γ (Offer 0 b) a)
cut n w ty γ SBot (SOne 0 a) = a
cut n w ty γ What a = Cut w ty γ What a
cut n w ty γ a What = Cut w ty γ a What
cut n w ty γ a b = exchange ([γ..n-1] ++ [0..γ]) (cut n w (neg ty) (n-γ) b a)

-- Hereditary exchange
exchange π t = case t of
  Ax -> Ax
  (Cross w w' x c) -> Cross w w' (π!!x) (s' x c)
  Exchange ρ a -> exchange (map (π!!) ρ) a
  (Amp c x a) -> Amp c (π!!x) (s a) 
  (Plus x a b) -> Plus (π!!x) (s a) (s b)
  (TApp x t a) -> TApp (π!!x) t (s a)
  (TUnpack x a) -> TUnpack (π!!x) (s a)
  (Offer x a) -> Offer (π!!x) (s a)
  (Demand x a) -> Demand (π!!x) (s a)
  (Alias x w a) -> Alias (π!!x) w (s' x a)
  (Ignore x a) -> Ignore (π!!x) (del x a)
  (SOne x a) -> SOne (π!!x) (s a)
  (SZero x) -> SZero (π!!x)
  SBot -> SBot
  a -> Exchange π a
 where s = exchange π
       s' x = exchange (l++x:r)
              where (l,r) = splitAt x $ map (\y -> if y >= x then y+1 else x) π
       del x = exchange (l++r)
              where (l,_:r) = splitAt x $ map (\y -> if y > x then y-1 else x) π

------------------------------------
-- Pretty printing

prn p k = if p > k then parens else id

pType :: Int -> [Name] -> Type Name -> Doc
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
pType p vs (Bang t) = prn p 4 $ "!" <> pType 4 vs t
pType p vs (Quest t) = prn p 4 $ "?" <> pType 4 vs t

instance Show (Type Name) where
  show x = render $ pType 0 ["v" <> int i | i <- [0..]] x

pSeq :: [Name] -> [(Name,Type Name)] -> Seq Doc -> Doc
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
  (Offer x s) -> "offer " <> w <> " : " <> pType 0 ts tyA $$ pSeq ts (v0++(w,tyA):v1) s
    where (v0,(w,Quest tyA):v1) = splitAt x vs
  (Demand x s) -> "demand " <> w <> " : " <> pType 0 ts tyA $$ pSeq ts (v0++(w,tyA):v1) s
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  (Ignore x s) -> "ignore " <> w <> " : " <> pType 0 ts tyA $$ pSeq ts (v0++v1) s
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  (Alias x w' s) -> "let " <> w' <> " = alias " <> w <> " : " <> pType 0 ts tyA $$ pSeq ts (v0++(w,Bang tyA):(w',Bang tyA):v1) s
    where (v0,(w,Bang tyA):v1) = splitAt x vs
  What -> braces $ pCtx ts vs
 where vv = vax ts vs
       
vax ts vs x = if x < length vs then let (v,t) = vs!!x in v <> " : " <> pType 0 ts t
                               else "v" <> int (x-length vs)

instance Show (Seq Doc) where
  show = render . pSeq [] []

pCtx :: [Name] -> [(Name,Type Name)] ->  Doc
pCtx ts vs = sep $ punctuate comma $ [v <> " : " <> (pType 0 ts t) | (v,t) <- vs]
  
----------------------

data Deriv nm = Deriv {derivTypeVars :: [nm], derivContext :: [(nm,Type nm)], derivSequent :: Seq nm}

instance Show (Deriv Name) where
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

expTest = Deriv ["a"] [("x",neg (Bang (TVar True 0) :⊸: (Bang (TVar True 0) :⊗: Bang (TVar True 0) )))] $
          Cross "y" "z" 0 $
          Alias 0 "w" $
          Exchange [0,2,1] $
          Par 1 Ax Ax