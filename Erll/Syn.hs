{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
module Erll.Syn where

import Text.PrettyPrint

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

