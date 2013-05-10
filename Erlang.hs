{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Erlang where

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Text.PrettyPrint.HughesPJ

import Examples

import LL hiding ((&))

-- Names for channels
type Env = [String]

comp :: Deriv -> Code
comp (Deriv _ ctx s) = runReader (evalStateT m init) init
  where
    Erl m = compile s
    init = makeContext (map fst ctx)

newtype Erl a = Erl (StateT Env (Reader Env) a)
  deriving (Functor,Applicative,Monad,MonadReader Env,MonadState Env)

data Code
    = NewChannel String
    | Spawn Code
    | Tell String Pattern
    | Ask Pattern String
    | Assign String String
    | Case String [(Pattern,Code)]
    | Crash
    | Then Code Code

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
    NewChannel n -> text n <+> equals <+> text "newChannel" <> parens empty
    Spawn n ->
        text "spawn" <> parens (hang
            (text "fun" <+> parens empty <+> arrow) 4
            (pp n) $$ text "end")
    Tell n p -> text "tell" <> parens (text n <> comma <> pt p)
    Ask p n  -> pt p <+> equals <+> text "ask" <> parens (text n)
    Assign l r -> text l <+> equals <+> text r
    Case n brs -> hang (text "case" <+> text n <+> text "of") 4
        (vcat (punctuate semi [ hang (pt p <+> arrow) 4 (pp b) | (p,b) <- brs ]))
        $$ text "end"
    Crash -> text "crash"
    Then d e -> pp d <> comma $$ pp e

pt :: Pattern -> Doc
pt p = case p of
    PVar x -> text x
    PTup ps -> braces (cat (punctuate comma (map pt ps)))
    PLeft -> text "left"
    PRight -> text "right"

(|>) :: Code -> Code -> Code
Then a b |> c = Then a (b |> c)
a        |> c = Then a c

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

-- | Eliminates a name at some position
elim :: Int -> (String -> Erl a) -> Erl a
elim i k = do
    (l,s:r) <- asks (splitAt i)
    local (const (l ++ r)) (k s)

-- | Introduces a name at some position
introduce :: Name -> Int -> (String -> Erl a) -> Erl a
introduce n i k = do
    s <- make n
    env <- ask
    let (l,r) = splitAt (i `mod` 1 + length env) env
    local (const (l ++ [s] ++ r)) (k s)

make :: String -> Erl String
make n = do
    seen <- get
    let s = makeName n seen
    modify (s:)
    return s

-- | Introduces many names at some positions
--   To avoid confusion, let the list be sorted descendingly
introduces :: [(Name,Int)] -> ([String] -> Erl a) -> Erl a
introduces []         k = k []
introduces ((n,i):xs) k =
    introduce n i $ \ s ->
    introduces xs $ \ ss ->
    k (s:ss)

compileAndPair :: Seq -> Name -> Erl (Name,Code)
compileAndPair seq n = (,) n <$> compile seq

compile :: Seq -> Erl Code
compile seq = case seq of
    Exchange perm s -> local (\ env -> [ env !! p | p <- perm ]) (compile s)

    Ax t
        | positiveType t -> do
            [a,b] <- ask
            return (Tell a (PVar b)) -- TODO: check if this is correct
        | otherwise      -> compile (Exchange [1,0] (Ax (neg t)))

    Cut x y _ i sx sy -> do
        (c1,c2) <- asks (splitAt i)
        (nx,cx) <- local (const c1) $ introduce x 0 $ compileAndPair sx
        (ny,cy) <- local (const c2) $ introduce y 0 $ compileAndPair sy
        return $ chunk
            [ NewChannel nx
            , Assign ny nx
            , Spawn cx
            , cy
            ]

    Par _ x y i sx sy ->
        elim i $ \ z -> do
        (c1,c2) <- asks (splitAt i)
        (nx,cx) <- local (const c1) $ introduce x (-1) $ compileAndPair sx
        (ny,cy) <- local (const c1) $ introduce y 0 $ compileAndPair sy
        return $ chunk
            [ NewChannel nx
            , NewChannel ny
            , Tell z (PVar nx & PVar ny)
            , Spawn cx
            , cy
            ]

    Cross _ x y i s ->
        elim i $ \ z ->
        introduces [(y,i),(x,i)] $ \ [ny,nx] ->
        (Ask (PVar nx & PVar ny) z |>) <$> compile s

    Plus x y i sx sy -> do
        elim i $ \ z -> do
        z' <- make z  -- make a case variable that looks like z
        (nx,cx) <- introduce x 0 $ compileAndPair sx
        (ny,cy) <- introduce y 0 $ compileAndPair sy
        return $ Ask (PVar z') z |> Case z'
            [ (PLeft & PVar nx,cx)
            , (PRight & PVar ny,cy)
            ]

    With x r i s ->
        elim i $ \ z ->
        introduce x (-1) $ \ nx -> do
        c <- compile s
        return $ chunk
            [ NewChannel nx
            , Tell z ((if r then PRight else PLeft) & PVar nx)
            , c
            ]

    SOne i s -> elim i $ \ x -> (Ask tt x |>) <$> compile s

    SBot -> elim 0 $ \ x -> return (Tell x tt)

    SZero i -> return Crash

    _ -> error "unsupported sequent"

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
