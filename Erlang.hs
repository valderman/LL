{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module Erlang where

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
    | String :! Code
    | Pat Pattern
    | Ask Code
    | Pattern := Code
    | Case Code [(Pattern,Code)]
    | Crash
    | Then Code Code

infix 3 :!
infixr 2 :=

data Pattern
    = PVar String
    | PTup [Pattern]
    | PLeft
    | PRight
    | PNeg
    | PPos
    | Escape Code
  deriving Show

var :: String -> Code
var = Pat . PVar

(|>) :: Code -> Code -> Code
Then a b |> c = Then a (b |> c)
a        |> c = Then a c

infixr 0 |>

(!) :: String -> Pattern -> Code
x ! p = x :! Pat p

infix 3 !

ask :: String -> Code
ask x = Ask (var x)

(|||) :: Code -> Code -> Code
a ||| b = Spawn a |> b

infixr 1 |||

chunk :: [Code] -> Code
chunk = foldr1 (|>)

tt :: Pattern
tt = PTup []

(&) :: Pattern -> Pattern -> Pattern
p1 & p2 = PTup [p1,p2]

infixr 5 &

-- | Compile a derivation to Erlang
compile :: Deriv -> Code
compile = compileDeriv . erlangUnique

-- | Compile a derivation to Erlang, without renamings
compileDeriv :: Deriv -> Code
compileDeriv (Deriv ts vs sq) = foldSeq sf ts vs sq
  where
    sf :: Deriv -> SeqFinal Type Code
    sf (Deriv tvs _ _) = SeqFinal {..}
      where
        sxchg _ c = c

        sax a b t
            | TVar k i <- t = Case (var (tvs !! i))
                [ (pol,if f k then ask_a else ask_b)
                | (pol,f) <- [(PPos,not),(PNeg,id)]
                ]
            | positiveType t  = ask_b
            | otherwise       = ask_a
          where
            ask_a = b :! ask a
            ask_b = a :! ask b

        scut x y _tx cx _ty cy = chunk
            [ PVar y := PVar x := NewChannel
            , cx ||| cy
            ]

        scross z x _ y _ c = PVar x & PVar y := ask z |> c

        spar z x _ y _ cx cy = chunk
            [ PVar x := NewChannel
            , PVar y := NewChannel
            , z ! PVar x & PVar y
            , cx ||| cy
            ]

        swith b z x _ c = chunk
            [ PVar x := NewChannel
            , z ! (if b then PRight else PLeft) & PVar x
            , c
            ]

        splus z x _ y _ cx cy = Case (ask z)
            [ (PLeft & PVar x,cx)
            , (PRight & PVar y,cy)
            ]


        sbot x = x ! tt

        sone x c = tt := ask x |> c

        szero _ _ = Crash

        stapp z _ x t c = chunk
            [ PVar x := NewChannel
            , z ! (pol & PVar x)
            , c
            ]
          where
            pol | TVar True i <- t  = negationOf (tvs !! i)
                | TVar False i <- t = PVar (tvs !! i)
                | positiveType t    = PPos
                | otherwise         = PNeg

            negationOf u = Escape (Case (var u) [(PNeg,Pat PPos),(PPos,Pat PNeg)])

        stunpack a z x c = PVar a & PVar x := ask z |> c

        soffer _ _ _ _ = error "offer"
        sdemand _ _ _ _ = error "demand"
        signore _ _ _ = error "ignore"
        salias _ _ _ _ = error "alias"
        swhat _ _  = err "what"

        sty _ t = t

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

    ts' <- mapM uq ts

    vs' <- mapM (\ (n,t) -> (,) <$> uq n <*> ty t) vs

    Deriv ts' vs' <$> go sq
  where
    ty :: Type -> UM Type
    ty t0 = case t0 of
        x :⊕: y     -> (:⊕:) <$> ty x <*> ty y
        x :&: y     -> (:&:) <$> ty x <*> ty y
        x :|: y     -> (:|:) <$> ty x <*> ty y
        x :⊗: y     -> (:⊗:) <$> ty x <*> ty y
        Forall w t  -> Forall <$> uq w <*> ty t
        Exists w t  -> Exists <$> uq w <*> ty t
        Bang t      -> Bang <$> ty t
        Quest t     -> Quest <$> ty t
        Meta b x ns -> Meta b x <$> mapM ty ns
        Zero        -> return t0
        One         -> return t0
        Top         -> return t0
        Bot         -> return t0
        TVar{}      -> return t0

    go :: Seq -> UM Seq
    go s = case s of
      Exchange p c      -> Exchange p <$> go c
      Ax t              -> Ax <$> ty t
      Cut w w' t x c d  -> Cut <$> uq w <*> uq w' <*> ty t <.> x <*> go c <*> go d
      Cross t w w' x c  -> Cross <$> ty t <*> uq w <*> uq w' <.> x <*> go c
      Par t w w' x c d  -> Par <$> ty t <*> uq w <*> uq w' <.> x <*> go c <*> go d
      Plus w w' x a b   -> Plus <$> uq w <*> uq w' <.> x <*> go a <*> go b
      With w c x a      -> With <$> uq w <.> c <.> x <*> go a

      SOne x a          -> SOne x <$> go a
      SZero x           -> return (SZero x)
      SBot              -> return SBot

      TApp t w x t' a   -> TApp <$> ty t <*> uq w <.> x <*> ty t' <*> go a
      TUnpack w x a     -> TUnpack <$> uq w <.> x <*> go a
      Offer w x a       -> err "Offer"
      Demand w t x a    -> err "Demand"
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
    n :! p -> text "tell" <> parens (text n <> comma <> pp p)
    Pat p  -> pt p
    Ask n  -> text "ask" <> parens (pp n)
    l := r -> pt l <+> equals <+> pp r
    Case s brs -> hang (text "case" <+> pp s <+> text "of") 4
        (vcat (punctuate semi [ hang (pt p <+> arrow) 4 (pp b) | (p,b) <- brs ]))
        $$ text "end"
    Crash -> text "error" <> parens (text "crash")
    Then d e -> pp d <> comma $$ pp e

pt :: Pattern -> Doc
pt p = case p of
    PVar x   -> text x
    PTup ps  -> braces (cat (punctuate comma (map pt ps)))
    PLeft    -> text "left"
    PRight   -> text "right"
    PNeg     -> text "neg"
    PPos     -> text "pos"
    Escape c -> pp c

