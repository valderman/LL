{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module Erlang where

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Text.PrettyPrint.HughesPJ

import Examples

import LL hiding ((&))

-- | A slightly weird representation of erlang expressions
data Code
    = NewChannel
    | Spawn Code
    | String :! Pattern
    | Ask Name
    | Pattern := Code
    | Var Name
    | Case Code [(Pattern,Code)]
    | Crash
    | Then Code Code

infix 3 :!
infixr 2 :=

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
    n :! p -> text "tell" <> parens (text n <> comma <> pt p)
    Ask n  -> text "ask" <> parens (text n)
    l := r -> pt l <+> equals <+> pp r
    Case s brs -> hang (text "case" <+> pp s <+> text "of") 4
        (vcat (punctuate semi [ hang (pt p <+> arrow) 4 (pp b) | (p,b) <- brs ]))
        $$ text "end"
    Crash -> text "error" <> parens (text "crash")
    Then d e -> pp d <> comma $$ pp e

pt :: Pattern -> Doc
pt p = case p of
    PVar x -> text x
    PTup ps -> braces (cat (punctuate comma (map pt ps)))
    PLeft -> text "left"
    PRight -> text "right"

infixr 0 |>

(|>) :: Code -> Code -> Code
Then a b |> c = Then a (b |> c)
a        |> c = Then a c

infixr 1 |||

infixr 5 &

(|||) :: Code -> Code -> Code
a ||| b = Spawn a |> b

chunk :: [Code] -> Code
chunk = foldr1 (|>)

data Pattern
    = PVar String
    | PTup [Pattern]
    | PLeft
    | PRight
  deriving Show

tt :: Pattern
tt = PTup []

(&) :: Pattern -> Pattern -> Pattern
p1 & p2 = PTup [p1,p2]

-- | Compile a derivation to Erlang
compile :: Deriv -> Code
compile = compileDeriv . erlangUnique

-- | Compile a derivation to Erlang, without renamings
compileDeriv :: Deriv -> Code
compileDeriv (Deriv ts vs s) = foldSeq (const SeqFinal {..}) ts vs s
  where
    sxchg _ c = c

    sax a b t
        | positiveType t = a :! PVar b
        | otherwise      = b :! PVar a

    scut x y _tx cx _ty cy = chunk
        [ PVar x := NewChannel
        , PVar y := Var x
        , cx ||| cy
        ]

    scross z x _ y _ c = PVar x & PVar y := Ask z |> c

    spar z x _ y _ cx cy = chunk
        [ PVar x := NewChannel
        , PVar y := NewChannel
        , z :! PVar x & PVar y
        , cx ||| cy
        ]

    swith b z x _ c = chunk
        [ PVar x := NewChannel
        , z :! (if b then PRight else PLeft) & PVar x
        , c
        ]

    splus z x _ y _ cx cy = Case (Ask z)
        [ (PLeft & PVar x,cx)
        , (PRight & PVar y,cy)
        ]


    sbot x = x :! tt

    sone x c = tt := Ask x |> c

    szero _ _ = Crash

    sty _ _ = err "type"
    stapp = err "type application"
    stunpack = err "type unpacking"

    soffer _ _ _ _ = error "offer"
    sdemand _ _ _ _ = error "demand"
    signore _ _ _ = error "ignore"
    salias _ _ _ _ = error "alias"
    swhat _ _  = err "what"

    err s = error $ "compile: " ++ s ++ " not supported yet"

infixl 4 <.>

-- | Applies a pure value in an applicative computation
(<.>) :: Applicative f => f (a -> b) -> a -> f b
m <.> x = m <*> pure x

-- | Makes all names in the derivation unique Erlang-valid identifiers
erlangUnique :: Deriv -> Deriv
erlangUnique (Deriv ts vs s) = Deriv ts vs' (evalState (go s) (map fst vs'))
  where
    vs' = makeContext (map fst vs) `zip` map snd vs

    go :: Seq -> State [Name] Seq
    go s = case s of
      Exchange p s      -> Exchange p <$> go s
      Ax ty             -> return (Ax ty)
      Cross ty w w' x c -> Cross ty <$> uq w <*> uq w' <.> x <*> go c
      Par ty w w' x c d -> Par ty <$> uq w <*> uq w' <.> x <*> go c <*> go d
      Plus w w' x a b   -> Plus <$> uq w <*> uq w' <.> x <*> go a <*> go b
      With w c x a      -> With <$> uq w <.> c <.> x <*> go a

      SOne x a          -> SOne x <$> go a
      SZero x           -> return (SZero x)
      SBot              -> return SBot

      TApp tp w x t a   -> err "TApp"
      TUnpack w x a     -> err "TUnpack"
      Offer w x a       -> err "Offer"
      Demand w ty x a   -> err "Demand"
      Alias x w a       -> err "Alias"
      Ignore x a        -> err "Ignore"

      What nm xs        -> err "What"

    uq :: Name -> State [Name] Name
    uq n = do
        seen <- get
        let s = makeName n seen
        modify (s:)
        return s

    err s = error $ "erlangUnique: " ++ s ++ " not supported yet"

-- | Makes a name from a suggestion that does not collide with an environment
makeName :: String -> [String] -> String
makeName = nonCollide . prepare

-- | Makes a valid erlang identifier out of a suggestion string
prepare :: String -> String
prepare s@(c:cs)
    | not (isAlpha c) = prepare ('X':s)
    | otherwise = toUpper c : filter isOk cs
prepare "" = "X"

-- | (possible underapproximation) of valid characters in Erlang identifiers
isOk :: Char -> Bool
isOk c = isAlpha c || isDigit c || c == '_'

-- | Checks if a given string does not collide in an environment,
--   otherwise appends _0, _1, _2... until it does not.
--   (termination because environments are finite)
nonCollide :: String -> [String] -> String
nonCollide s ss
    | ok s      = s
    | otherwise = go 0
  where
    ok s' = s' `notElem` ss
    go n | ok s' = s'
         | otherwise = go (n+1)
      where s' = s ++ "_" ++ show n

makeContext :: [String] -> [String]
makeContext []     = []
makeContext (s:ss) = makeName s ss' : ss'
  where ss' = makeContext ss

