{-# LANGUAGE PatternGuards #-}
module Erll.Unique where

import Data.Char
import Control.Applicative hiding (empty)
import Control.Monad.State

import LL

infixl 4 <.>

-- | Applies a pure value in an applicative computation
(<.>) :: Applicative f => f (a -> b) -> a -> f b
m <.> x = m <*> pure x

type UM = State [Name]

runUM :: UM a -> a
runUM m = evalState m []

uq :: Name -> UM Name
uq n = do
    env <- get
    let s = makeName n env
    put (s:env)
    return s

-- | Makes all names in the derivation unique Erlang-valid identifiers
erlangUnique :: Deriv -> Deriv
erlangUnique (Deriv ts vs sq0) = runUM $ do

    vs' <- mapM (\ (n,t) -> (,) <$> uq n <.> t) vs

    Deriv ts vs' <$> go sq0
  where
    go :: Seq -> UM Seq
    go sq = case sq of
      Exchange p s        -> Exchange p <$> go s
      Ax ty               -> return (Ax ty)
      Cut w w' t x c d    -> Cut <$> uq w <*> uq w' <.> t <.> x <*> go c <*> go d
      Cross b ty w w' x c -> Cross b ty <$> uq w <*> uq w' <.> x <*> go c
      Par b ty w w' x c d -> Par b ty <$> uq w <*> uq w' <.> x <*> go c <*> go d
      Plus w w' x a b     -> Plus <$> uq w <*> uq w' <.> x <*> go a <*> go b
      With b ty w c x a   -> With b ty <$> uq w <.> c <.> x <*> go a

      SOne b x a          -> SOne b x <$> go a
      SZero x             -> return (SZero x)
      SBot                -> return SBot

      TApp b t w x t' a   -> TApp b t <$> uq w <.> x <.> t' <*> go a
      TUnpack w x a       -> TUnpack <$> uq w <.> x <*> go a

      Offer b w x a       -> Offer b <$> uq w <.> x <*> go a
      Demand w t x a      -> Demand <$> uq w <.> t <.> x <*> go a
      Alias b x w a       -> Alias b x <$> uq w <*> go a
      Ignore b x a        -> Ignore b x <$> go a

      What nm xs          -> error $ "erlangUnique: What " ++ nm ++ " " ++ show xs ++ "‽"

-- | Makes a name from a suggestion that does not collide with an environment
makeName :: String -> [String] -> String
makeName = nonCollide . prepare

-- | Makes a valid erlang identifier out of a suggestion string
prepare :: String -> String
prepare s@(c:cs)
    | Just s' <- trGreek c = prepare (s' ++ cs)
    | not (isAlphaAscii c) = prepare ('X':s)
    | otherwise = toUpper c : filter isOk cs
prepare "" = "X"

trGreek :: Char -> Maybe String
trGreek c = lookup c tr
  where
    tr = [('α',"Alpha")
         ,('β',"Beta")
         ,('γ',"Gamma")
         ,('δ',"Delta")
         ,('ε',"Epsilon")
         ,('ζ',"Zeta")
         ,('η',"Eta")
         ,('ι',"Iota")
         ,('κ',"Kappa")
         ,('λ',"Lambda")
         ,('μ',"Mu")
         ]

isAlphaAscii :: Char -> Bool
isAlphaAscii c = isAlpha c && isAscii c

-- | (possible underapproximation) of valid characters in Erlang identifiers
isOk :: Char -> Bool
isOk c = isAlphaAscii c || isDigit c || c == '_'

-- | Checks if a given string does not collide in an environment,
--   otherwise appends _0, _1, _2... until it does not.
--   (termination because environments are finite)
nonCollide :: String -> [String] -> String
nonCollide s ss
    | ok s      = s
    | otherwise = go 0
  where
    ok s' = s' `notElem` ss
    go :: Int -> String
    go n | ok s' = s'
         | otherwise = go (n+1)
      where s' = s ++ "_" ++ show n

