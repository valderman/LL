{-# LANGUAGE PatternGuards, RecordWildCards, OverloadedStrings #-}
module ErlangPrelude (parsePrelude, preludeCompile) where

import Control.Applicative
import Erlang
import Text.PrettyPrint
import LL (Deriv(..))
import Data.List (find)
import Control.Monad

type Bif = (String,Loc,[String])

data Prelude = Prelude
    { prel_str  :: String
    , prel_bifs :: [Bif]
    }
  deriving Show

inPercs :: String -> String -> Bool
inPercs x s = s == ("%%% " ++ x ++ " %%%")

isStart,isStop,isEnd :: String -> Bool
isStart = inPercs "START"
isStop  = inPercs "STOP"
isEnd   = inPercs "END"

rmP :: String -> Maybe String
rmP ('%':'%':'%':' ':s) = Just s
rmP _ = Nothing

maybeWord :: String -> String -> Maybe String
maybeWord x s = do
    s' <- rmP s
    s'' <- reverse <$> rmP (reverse s')
    (w,' ':f) <- Just (splitAt (length x) s'')
    guard (w == x)
    return f

data Loc = Init | Final deriving Show

maybeInit,maybeFinal :: String -> Maybe String
maybeInit = maybeWord "INIT"
maybeFinal = maybeWord "FINAL"

parsePrelude :: String -> Prelude
parsePrelude s = Prelude (unlines prel_strs) (parseBifs bifs)
  where
    (prel_strs,bifs) = break isStop . drop 1 . dropWhile (not . isStart) . lines $ s

parseBifs :: [String] -> [Bif]
parseBifs = go
  where
    go (x:xs)
        | Just f <- maybeInit x  = go' f Init xs
        | Just f <- maybeFinal x = go' f Final xs
        | otherwise = parseBifs xs
    go [] = []

    go' f l xs = (f,l,ys):go zs
      where (ys,zs) = go'' xs

    go'' :: [String] -> ([String],[String])
    go'' (x:xs)
        | isEnd x   = ([],xs)
        | otherwise = (dropSpaces x:ys,zs)
            where (ys,zs) = go'' xs
    go'' [] = error "INIT or FINAL without END"

dropSpaces :: String -> String
dropSpaces = dropWhile (== ' ')

preludeCompile :: Prelude -> Deriv -> String
preludeCompile Prelude{..} d =
    let d'@(Deriv _ vs _) = erlangUnique d
        bifs = getBifs (map fst vs) prel_bifs
        c = Spawn (compileDeriv d')
    in  (unlines
            [ "-module(compiled)."
            , "-export([main/0])."
            ])
        ++ prel_str ++
        (render $ hang "main() ->" 4 $ vcat $
            [ pp (Var n := NewChannel) <> comma
            | (n,_,_) <- bifs
            ] ++
            concat [ map text s | (_,Init,s) <- bifs ] ++
            [ pp c <> comma ] ++
            concat [ map text s | (_,Final,s) <- bifs ] ++
            ["ok."]
        )

getBifs :: [String] -> [Bif] -> [Bif]
getBifs (x:xs) bifs = case find (\ (y,_,_) -> x == y) bifs of
    Just bif -> bif:getBifs xs bifs
    Nothing  -> error $ x ++ " is not defined as a built-in-function"
getBifs [] _ = []

