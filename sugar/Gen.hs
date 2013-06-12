module Main where

import Data.Map (Map)
import qualified Data.Map as M

import AbsMx
import PrintMx

import Control.Applicative
import Control.Monad

import Data.Maybe
import Data.Either

import ToLL hiding (i,name,along)

import Test.QuickCheck

import Pretty()

i :: String -> Id
i x = Id ((0,0),x)

type Ctx = [(String,Type)]

instance Arbitrary Type where
    arbitrary = sized ty
      where
        ty s = frequency
            [(1,return Bot)
            ,(1,return One)
            ,(1,return Zero)
            ,(1,return Top)
            ,(s,bin Tensor)
            ,(s,bin Par)
            ,(s,bin Plus)
            ,(s,bin Choice)
            ]
          where
            bin mk = mk <$> ty s' <*> ty s'
            s' = s `div` 2

    shrink c = case c of
        Tensor a b -> [a,b] ++ (Tensor <$> shrink a <*> shrink b)
        Par a b    -> [a,b] ++ (Par <$> shrink a <*> shrink b)
        Plus a b   -> [a,b] ++ (Plus <$> shrink a <*> shrink b)
        Choice a b -> [a,b] ++ (Choice <$> shrink a <*> shrink b)
        _          -> []


finAll :: (Testable prop,Show a) => [a] -> (a -> prop) -> Property
finAll = finAll' show

finAll' :: (Testable prop) => (a -> String) -> [a] -> (a -> prop) -> Property
finAll' s xs f = conjoin [ printTestCase (s x) $ f x | x  <- xs ]

newtype Ctx' = Ctx' [(String,Type)]

instance Show Ctx' where
    show (Ctx' ctx) = printTree (binders ctx)

binders :: Ctx -> [Binder]
binders ctx = [ Binder (i x) t | (x,t) <- ctx ]

instance Arbitrary Ctx' where
    arbitrary = name <$> arbitrary
    shrink (Ctx' ctx) = map name (shrink types)
        where types = map snd ctx

name :: [Type] -> Ctx'
name = Ctx' . zip names

names = [ x:n | n <- "" : "'" : map show [0..], x <- "xyzuvwrstabc" ]

prop_desugar :: Ctx' -> Property
prop_desugar (Ctx' ctx) = finAll' printTree (gen ctx) $ \ s ->
    case fst (desugar (mkDeriv s)) of
         Right d -> length (show d) `seq` True
         Left{}  -> False
  where
    mkDeriv = Deriv [] [] (binders ctx)

main :: IO ()
main = quickCheck prop_desugar

-- Generates sequents for a given context
-- A theorem prover!
gen :: Ctx -> [Seq]
gen [(x,t),(y,t')] | canon t == neg t = [Ax (i x) (i y)]
gen [(x,Bot)] = [Bottom (i x)]
gen xs = concat [ kill x t ys | ((x,t),ys) <- selections xs ]

splits :: [a] -> [([a],[a])]
splits = map partitionEithers . go
  where
    go []     = [[]]
    go (x:xs) = let ys = go xs in map (Left x:) ys ++ map (Right x:) ys

kill :: String -> Type -> Ctx -> [Seq]
kill z t m = case t of
    Tensor t1 t2 -> do
        let (x,y,m') = fresh2 m z t1 t2
        TensorSeq (i x) (i y) (i z) <$> gen m'
    Par t1 t2 -> do
        (ml,mr) <- splits m
        let (x,mx) = fresh ml z t1
            (y,my) = fresh mr (z ++ "'") t2
            par_ u v = ParSeq (i z) (i x) u (i y) v
        par_ <$> gen mx <*> gen my
    Plus t1 t2 -> do
        let (x,mx) = fresh m z t1
            (y,my) = fresh m (z ++ "'") t2
            case_ u v = Case (i z) (i x) u (i y) v
        case_ <$> gen mx <*> gen my
    Choice t1 t2 -> concat
        [ ChoiceSeq (i x) ch (i z) <$> gen m'
        | (ch,t') <- [(Fst,t1),(Snd,t2)]
        , let (x,m') = fresh m z t'
        ]
    Top -> []
    One -> Unit (i z) <$> gen m
    Bot -> []
    Zero -> [Crash (i z) (along (map (i . fst) m))]
    Lollipop t1 t2 -> gen ((z,neg t1 `Par` t2):m)
    TyId{} -> []
    Bang{} -> []
    Quest{} -> []
    Forall{} -> []
    Exists{} -> []
    Neg t -> gen ((z,neg t):m)

along :: [Id] -> Along
along [] = ANothing
along xs = AJust xs

-- | Negate type
neg :: Type -> Type
neg t = case t of
    Tensor t s -> Par (neg t) (neg s)
    Par t s -> Tensor (neg t) (neg s)
    One -> Bot
    Bot -> One
    Plus t s -> Choice (neg t) (neg s)
    Choice t s -> Plus (neg t) (neg s)
    Top -> Zero
    Zero -> Top
    Lollipop t s -> Tensor t (neg s)
    TyId x -> Neg (TyId x)
    Bang t -> Quest (neg t)
    Quest t -> Bang (neg t)
    Neg t -> canon t
    Forall x t -> Exists x (neg t)
    Exists x t -> Forall x (neg t)

-- | Canonicalize type
canon :: Type -> Type
canon = neg . neg

selections :: [a] -> [(a,[a])]
selections []     = []
selections (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- selections xs ]

fresh :: Ctx -> String -> Type -> (String,Ctx)
fresh m x t = fromJust $  msum
    [ case lookup y m of
        Nothing -> Just (y,(y,t):m)
        Just{}  -> Nothing
    | i <- [0..], let y = x ++ show i
    ]

fresh2 :: Ctx -> String -> Type -> Type -> (String,String,Ctx)
fresh2 m x t1 t2 = (x1,x2,m2)
  where
    (x1,m1) = fresh m x t1
    (x2,m2) = fresh m1 x t2

