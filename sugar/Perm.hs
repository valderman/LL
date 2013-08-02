{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Perm(swap,swap0,perm,permute,compose,getPermutation,isId,without) where

import LL (Permutation,inverse)
import Control.Applicative
import Test.QuickCheck
import Data.List (elemIndex)

swap0 :: Int -> Permutation
swap0 = swap 0

swap :: Int -> Int -> Permutation
swap x y
    | x == y = [0..]
    | y < x = swap y x
    | otherwise = [0..x-1] ++ [y] ++ [x+1..y-1] ++ [x] ++ [y+1..]

chop :: [a] -> Permutation -> Permutation
chop xs p = zipWith const p xs

permute :: [a] -> Permutation -> [a]
permute xs p = [ xs !! x | x <- p ]

perm :: [a] -> Permutation -> ([a],Permutation)
perm xs p = let p' = chop xs p in (permute xs p',p')

compose :: Permutation -> Permutation -> Permutation
compose p1 p2 = [ p1 !! x | x <- p2 ]

getPermutation :: Eq a => [a] -> [a] -> Either ([a],[a]) Permutation
getPermutation []     [] = Right []
getPermutation (x:xs) ys
    | Just n <- elemIndex x ys
    = (n:) . map (\ k -> if k >= n then succ k else k) <$> getPermutation xs (ys `without` n)
getPermutation xs     ys = Left (xs,ys)

without :: [a] -> Int -> [a]
without (_:xs) 0 = xs
without (x:xs) n = x:without xs (n-1)

isId :: Permutation -> Bool
isId p = and (zipWith (==) p [0..])

-- Tests

prop_compose_assoc :: Permutation -> Permutation -> Permutation -> Property
prop_compose_assoc p1 p2 p3 = all ((length p1 ==) . length) [p2,p3] && all valid [p1,p2,p3] ==>
    compose p1 (compose p2 p3) == compose (compose p1 p2) p2

prop_compose_inverse :: Permutation -> Property
prop_compose_inverse p = valid p ==> compose p (inverse p) == p

valid :: Permutation -> Bool
valid p = all (`elem` p) [0..length p - 1]

prop_inv :: NonNegative Int -> NonNegative Int -> Bool
prop_inv (NonNegative x) (NonNegative y) =
    let p = swap x y
        q = swap y x
    in  isId (compose p q `chop` [0..10 + max x y])

prop_inv' :: NonNegative Int -> NonNegative Int -> String -> Bool
prop_inv' (NonNegative x) (NonNegative y) s =
    let p = swap x y
        q = swap y x
    in  permute' s (compose p q) == s

permute' :: [a] -> Permutation -> [a]
permute' xs p = [ xs !! x | x <- chop xs p ]

permutations :: Int -> [Permutation]
permutations 0 = [[]]
permutations n =
    [ l ++ (n-1) : r
    | i <- [0..n-1]
    , xs <- permutations (n - 1)
    , let (l,r) = splitAt i xs
    ]

-- testing getPermutation
getPerms :: Either (Int,Permutation) ()
getPerms = sequence_
    [ either (const (Left (n,p))) Right (getPermutation [0..n-1] p)
    | n <- [0..8]
    , p <- permutations n
    ]

