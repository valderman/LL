{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveFunctor,
             TypeSynonymInstances, FlexibleInstances, PatternGuards #-}
module Erlang (compile, compileDeriv, erlangUnique, pp, Code(..), Pattern(..)) where

import Control.Applicative hiding (empty)
import Control.Monad.State
import Data.Char
import Text.PrettyPrint.HughesPJ

import LL hiding ((&),var,Alias,Demand,Ignore)
import qualified LL

-- | A slightly weird representation of erlang expressions

data Code v
    = NewChannel
    | Spawn (Code v)
    | Tell v (Pattern v)
    | Ask v
    | Axiom v v
    | Pattern v := Code v
    | Pattern (Pattern v)
    | Case (Code v) [(Pattern v,Code v)]
    | Crash
    | Promote v v (Code v)
    | Demand v
    | Ignore v
    | Alias v
    | Then (Code v) (Code v)
  deriving Functor

type Code' = Code String

infixr 2 :=

data Pattern v
    = Var v
    | Tup [Pattern v]
    | InL
    | InR
  deriving Functor

type Pattern' = Pattern String

(//) :: Eq a => a -> a -> Code a -> Code a
new // old = fmap (\ s -> if s == old then new else s)

(|>) :: Code a -> Code a -> Code a
Then a b |> c = Then a (b |> c)
a        |> c = Then a c

infixr 0 |>

(!) :: a -> Pattern a -> Code a
x ! p = Tell x p

var :: a -> Code a
var = Pattern . Var

infix 3 !

(|||) :: Code a -> Code a -> Code a
a ||| b = Spawn a |> b

infixr 1 |||

chunk :: [Code a] -> Code a
chunk = foldr1 (|>)

tt :: Pattern a
tt = Tup []

(&) :: Pattern a -> Pattern a -> Pattern a
p1 & p2 = Tup [p1,p2]

infixr 5 &

-- | Compile a derivation to Erlang
compile :: Deriv -> Code'
compile = compileDeriv . erlangUnique

-- | Compile a derivation to Erlang, without renamings
compileDeriv :: Deriv -> Code'
compileDeriv (Deriv ts vs sq) = foldSeq sf ts vs sq
  where
    sf :: Deriv -> SeqFinal () Code'
    sf = const SeqFinal {..}
      where
        sxchg _ c = c

        sax a b _ = Axiom a b

        scut x y _ cx _ cy = chunk
            [ Var x := NewChannel
            , (x // y) (cx ||| cy)
            ]

        scross _b z x _ y _ c = Var x & Var y := Ask z |> c

        spar _b z x _ y _ cx cy = chunk
            [ Var x := NewChannel
            , Var y := NewChannel
            , z ! Var x & Var y
            , cx ||| cy
            ]

        swith _b _ b z x _ c = chunk
            [ Var x := NewChannel
            , z ! (if b then InR else InL) & Var x
            , c
            ]

        splus z x _ y _ cx cy = Case (Ask z)
            [ (InL & Var x,cx)
            , (InR & Var y,cy)
            ]

        sbot x = x ! tt

        sone _b x c = tt := Ask x |> c

        szero _ _ = Crash

        stapp _b z _ x _ c = (z // x) c

        stunpack _ z x c = (z // x) c

        soffer _b z x _ c = Promote z x c

        sdemand z x _ c = Var x := Demand z |> c

        signore z _ c = Ignore z |> c

        salias _b z z' _ c = Alias z |> (z // z') c

        swhat z xs = error $ "compile: What " ++ z ++ "[" ++ unwords xs ++ "]‽"

        sty _ _ = ()

        ~[sfold, sunfold, smem, schplus, schcross, schpar, schtyp, schempty, schfull] =[]

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
      LL.Demand w t x a   -> LL.Demand <$> uq w <.> t <.> x <*> go a
      LL.Alias b x w a    -> LL.Alias b x <$> uq w <*> go a
      LL.Ignore b x a     -> LL.Ignore b x <$> go a

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

-- Pretty printing

instance Show Code' where
    show = render . pc

arrow :: Doc
arrow = text "->"

dot :: Doc
dot = char '.'

pc :: Code' -> Doc
pc c = pp c <> dot

pp :: Code' -> Doc
pp c0 = case c0 of
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

    Promote z x c -> text "promote" <> parens
        (hang (text z <> comma <> text "fun" <> parens (text x) <+> arrow) 4 (pc c) $$ text "end")

    Demand z -> text "demand" <> parens (text z)
    Ignore z -> text "ignore" <> parens (text z)
    Alias z -> text "alias" <> parens (text z)
    Axiom x y -> text "axiom" <> parens (text x <> comma <> text y)
    Then d e -> pp d <> comma $$ pp e

pt :: Pattern' -> Doc
pt p = case p of
    Var x    -> text x
    Tup ps   -> braces (cat (punctuate comma (map pt ps)))
    InL      -> text "left"
    InR      -> text "right"

