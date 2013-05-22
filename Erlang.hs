{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards,
             TemplateHaskell, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}
module Erlang where

import Data.Generics.Geniplate

import Control.Applicative hiding (empty)
import Control.Monad.State
import Data.Char
import Text.PrettyPrint.HughesPJ

import Examples hiding (t0)

import LL hiding ((&),var)

-- | A slightly weird representation of erlang expressions

data Code
    = NewChannel
    | Spawn Code
    | Tell String Pattern
    | Ask String
    | Axiom String String
    | Pattern := Code
    | Pattern Pattern
    | Case Code [(Pattern,Code)]
    | Crash
    | Then Code Code


infixr 2 :=

data R
data L

data Pattern
    = Var String
    | Tup [Pattern]
    | InL
    | InR

instanceTransformBi [t| (String,Code) |]

(//) :: String -> String -> Code -> Code
new // old = transformBi rep
  where
    rep :: String -> String
    rep s | s == old  = new
          | otherwise = s

(|>) :: Code -> Code -> Code
Then a b |> c = Then a (b |> c)
a        |> c = Then a c

infixr 0 |>

(!) :: String -> Pattern -> Code
x ! p = Tell x p

var :: String -> Code
var = Pattern . Var

infix 3 !

(|||) :: Code -> Code -> Code
a ||| b = Spawn a |> b

infixr 1 |||

chunk :: [Code] -> Code
chunk = foldr1 (|>)

tt :: Pattern
tt = Tup []

(&) :: Pattern -> Pattern -> Pattern
p1 & p2 = Tup [p1,p2]

infixr 5 &

-- | Compile a derivation to Erlang
compile :: Deriv -> Code
compile = compileDeriv . erlangUnique

-- | Compile a derivation to Erlang, without renamings
compileDeriv :: Deriv -> Code
compileDeriv (Deriv ts vs sq) = foldSeq sf ts vs sq
  where
    sf :: Deriv -> SeqFinal () Code
    sf = const SeqFinal {..}
      where
        sxchg _ c = c

        sax a b _ = Axiom a b

        scut x y _ cx _ cy = chunk
            [ Var x := NewChannel
            , (x // y) (cx ||| cy)
            ]

        scross z x _ y _ c = Var x & Var y := Ask z |> c

        spar z x _ y _ cx cy = chunk
            [ Var x := NewChannel
            , Var y := NewChannel
            , z ! Var x & Var y
            , cx ||| cy
            ]

        swith b z x _ c = chunk
            [ Var x := NewChannel
            , z ! (if b then InR else InL) & Var x
            , c
            ]

        splus z x _ y _ cx cy = Case (Ask z)
            [ (InL & Var x,cx)
            , (InR & Var y,cy)
            ]


        sbot x = x ! tt

        sone x c = tt := Ask x |> c

        szero _ _ = Crash

        stapp z _ x _ c = (z // x) c

        stunpack _ z x c = (z // x) c

        soffer _ _ _ _ = error "offer"
        sdemand _ _ _ _ = error "demand"
        signore _ _ _ = error "ignore"
        salias _ _ _ _ = error "alias"
        swhat _ _  = err "what"

        sty _ _ = ()

        err s = error $ "compile: " ++ s ++ " not supported yet"

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
erlangUnique (Deriv ts vs sq) = runUM $ do

    vs' <- mapM (\ (n,t) -> (,) <$> uq n <.> t) vs

    Deriv ts vs' <$> go sq
  where
    go :: Seq -> UM Seq
    go sq = case sq of
      Exchange p s      -> Exchange p <$> go s
      Ax ty             -> return (Ax ty)
      Cut w w' t x c d  -> Cut <$> uq w <*> uq w' <.> t <.> x <*> go c <*> go d
      Cross ty w w' x c -> Cross ty <$> uq w <*> uq w' <.> x <*> go c
      Par ty w w' x c d -> Par ty <$> uq w <*> uq w' <.> x <*> go c <*> go d
      Plus w w' x a b   -> Plus <$> uq w <*> uq w' <.> x <*> go a <*> go b
      With w c x a      -> With <$> uq w <.> c <.> x <*> go a

      SOne x a          -> SOne x <$> go a
      SZero x           -> return (SZero x)
      SBot              -> return SBot

      TApp t w x t' a   -> TApp t <$> uq w <.> x <.> t' <*> go a
      TUnpack w x a     -> TUnpack <$> uq w <.> x <*> go a

      Offer w x a       -> err "Offer"
      Demand w ty x a   -> err "Demand"
      Alias x w a       -> err "Alias"
      Ignore x a        -> err "Ignore"

      What nm xs        -> err "What"

    err s = error $ "erlangUnique: " ++ s ++ " not supported yet"

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

-- Pretty printing

instance Show Code where
    show = render . pc

arrow :: Doc
arrow = text "->"

dot :: Doc
dot = char '.'

pc :: Code -> Doc
pc c = pp c <> dot

pp :: Code -> Doc
pp c = case c of
    NewChannel -> text "newChannel" <> parens empty
    Spawn n ->
        text "spawn" <> parens (hang
            (text "fun" <+> parens empty <+> arrow) 4
            (pp n) $$ text "end")
    Tell n p -> text "tell" <> parens (text n <> comma <> pt p)
    Pattern p -> pt p
    Ask n  -> text "ask" <> parens (text n)
    l := r -> pt l <+> equals <+> pp r
    Case s brs -> hang (text "case" <+> pp s <+> text "of") 4
        (vcat (punctuate semi [ hang (pt p <+> arrow) 4 (pp b) | (p,b) <- brs ]))
        $$ text "end"
    Crash -> text "error" <> parens (text "crash")
    Then d e -> pp d <> comma $$ pp e
    Axiom x y -> text "axiom" <> parens (text x <> comma <> text y)

pt :: Pattern -> Doc
pt p = case p of
    Var x    -> text x
    Tup ps   -> braces (cat (punctuate comma (map pt ps)))
    InL      -> text "left"
    InR      -> text "right"

