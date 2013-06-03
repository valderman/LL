{-# LANGUAGE EmptyDataDecls,KindSignatures,MultiParamTypeClasses,GeneralizedNewtypeDeriving #-}
module ToLL where

import Data.Map (Map)
import qualified Data.Map as M

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Error

import Pretty()

import Data.List (findIndex)

import LL hiding (exchange)

import Perm

import qualified AbsMx as C
import AbsMx (Ident(..),Choice(..),Binder(..),Prog)

desugar :: Prog -> Either Err Deriv
desugar (C.Deriv [] bs s) = runT $ do
    bs' <- forM bs $ \ (Binder x t) -> (,) x <$> bind x t
    (s',sb) <- trSeq s
    ctx <- sequence
        [ case lookup x bs' of
            Just t  -> return (name x,t)
            Nothing -> throwError (UnboundIdentifier x)
        | x <- sb
        ]
    forM_ bs' $ \ (b,_) -> when (b `notElem` sb) (throwError (IdentifierEscapes b))
    return $ Deriv [] ctx s'

newtype T a = T { unT :: StateT (Map Ident Type) (Either Err) a }
  deriving (Functor,Applicative,Monad,MonadState (Map Ident Type),MonadError Err)

runT :: T a -> Either Err a
runT (T m) = evalStateT m M.empty

data Err
    = MismatchTypes Seq_ Ident Ident Type Type
    | UnboundIdentifier Ident
    | BoundIdentifier Ident Type Type
    | IdentifierEscapes Ident
    | TypeError Ident Type Type_
    | PermutationError [Ident] [Ident]
    | Fail String
  deriving Show

instance Error Err where
    strMsg = Fail

data Seq_ = Ax_ | Cut_
  deriving Show

data Type_ = Par_ | Tensor_ | Plus_ | With_ | Bot_ | One_ | Zero_
  deriving Show

todo :: String -> a
todo = error . (++ " todo")

name :: Ident -> Name
name (Ident n) = n

eat :: Ident -> T Type
eat x = do
    m <- get
    case (M.lookup x m,M.delete x m) of
        (Just t,m') -> put m' >> return t
        (Nothing,_) -> throwError (UnboundIdentifier x)

-- watch out for shadowing
bind :: Ident -> C.Type -> T Type
bind x t = bind' x t' >> return t'
  where t' = trType t

bind' :: Ident -> Type -> T ()
bind' x t = do
    m <- get
    case (M.lookup x m,M.insert x t m) of
        (Just t',_)  -> throwError (BoundIdentifier x t t')
        (Nothing,m') -> put m'

trType :: C.Type -> Type
trType = go where
    go t = case t of
        C.Tensor x y   -> go x :⊗: go y
        C.Par x y      -> go x :|: go y
        C.Plus x y     -> go x :⊕: go y
        C.Choice x y   -> go x :&: go y
        C.One          -> One
        C.Bot          -> Bot
        C.Top          -> Top
        C.Zero         -> Zero
        C.Lollipop x y -> go x ⊸ go y
        C.Bang x       -> Bang (go x)
        C.Quest x      -> Quest (go x)
        C.Neg x        -> neg (go x)
        C.Forall i x   -> todo "trType forall"
        C.Exists i x   -> todo "trType exists"

-- Puts it first, and then removes it (for cut/par)
putFirst :: Ident -> (Seq,[Ident]) -> T (Seq,[Ident])
putFirst z (s,zb) = case findIndex (== z) zb of
    Nothing -> throwError (IdentifierEscapes z)
    Just n  -> let (zb',pm) = perm zb (swap0 n) in return (exchange pm s,tail zb')

tensorOrder :: Ident -> Ident -> (Seq,[Ident]) -> T (Seq,[Ident],[Ident])
tensorOrder x y (s,bs) = case findIndex (== x) bs of
    Just nx -> case findIndex (== y) bs of
        Just ny ->
            let (pm,l,r) = tensorOrder' nx ny bs
            in  return (exchange pm s,l,r)
        Nothing -> throwError (IdentifierEscapes y)
    Nothing -> throwError (IdentifierEscapes x)

tensorOrder' :: Int -> Int -> [a] -> (Permutation,[a],[a])
tensorOrder' x y bs = (pm',l,r)
  where
    (pos,pm)
        | x < y = (x,swap (x+1) y)
        | y + 1 == x = (y,swap x y)
        | y < x = (y,swap y (y+1) `compose` swap x y )
    (bs',pm') = perm bs pm
    (l,_x:_y:r) = splitAt pos bs'

trSeq :: C.Seq -> T (Seq,[Ident])
trSeq seq = case seq of

    C.Ax x y -> do
        tx <- eat x
        ty <- eat y
        when (tx /= neg ty) (throwError (MismatchTypes Ax_ x y tx ty))
        return (Ax ty,[x,y])

    C.Cut (Binder x tx) sx (Binder y ty) sy -> do

        tx' <- bind x tx
        (sx',xb) <- putFirst x =<< trSeq sx

        ty' <- bind y ty
        (sy',yb) <- putFirst y =<< trSeq sy

        when (tx' /= neg ty') (throwError (MismatchTypes Cut_ x y tx' ty'))

        return (Cut (name x) (name y) tx' (length xb) sx' sy',xb ++ yb)

    C.TensorSeq x y z s -> do

        tz <- eat z

        (tx,ty) <- case tz of
            tx :⊗: ty -> return (tx,ty)
            _         -> throwError (TypeError z tz Tensor_)

        bind' x tx
        bind' y ty
        (s',lb,rb) <- tensorOrder x y =<< trSeq s

        return (Cross tz (name x) (name y) (length lb) s',lb ++ [z] ++ rb)

    C.ParSeq z x sx y sy -> do

        tz <- eat z

        (tx,ty) <- case tz of
            tx :|: ty -> return (tx,ty)
            _         -> throwError (TypeError z tz Par_)

        bind' x tx
        (sx',xb) <- putFirst x =<< trSeq sx

        bind' y ty
        (sy',yb) <- putFirst y =<< trSeq sy

        return (Par tz (name x) (name y) (length xb) sx' sy',xb ++ [z] ++ yb)

    -- :⊕: :&:
    C.Case z x sx y sy -> do

        tz <- eat z
        (tx,ty) <- case tz of
            tx :⊕: ty -> return (tx,ty)
            _         -> throwError (TypeError z tz Plus_)


        ctx <- get

        put ctx
        bind' x tx
        (sx',xb) <- putFirst x =<< trSeq sx

        put ctx
        bind' y ty
        (sy',yb) <- putFirst y =<< trSeq sy

        p <- either (throwError . uncurry PermutationError) return
                    (getPermutation xb yb)

        return (Plus (name x) (name y) 0 sx' (exchange p sy'),z:xb)

    C.ChoiceSeq x ch z s -> do

        tz <- eat z
        (tx,ty) <- case tz of
            tx :&: ty -> return (tx,ty)
            _         -> throwError (TypeError z tz With_)

        bind' x (choice ch tx ty)
        (s',b) <- trSeq s

        case break (== x) b of
            (l,_:r) -> return (With (name x) (choice ch True False) (length l) s',l ++ [z] ++ r)
            _       -> throwError (IdentifierEscapes x)

    C.Bottom x -> do

        tx <- eat x
        when (tx /= Bot) (throwError (TypeError x tx Bot_))
        return (SBot,[x])

    C.Unit x s -> do

        tx <- eat x
        when (tx /= One) (throwError (TypeError x tx One_))
        (s',b) <- trSeq s
        return (SOne 0 s',x:b)

    C.Crash x -> do
        tx <- eat x
        when (tx /= Zero) (throwError (TypeError x tx Zero_))
        return (SZero 0,[x]) -- TODO: this actually has a bigger context

    -- TODO: quantifiers and exponentials

choice :: Choice -> a -> a -> a
choice Fst x _ = x
choice Snd _ y = y

-- Smart constructor for exchange
exchange :: Permutation -> Seq -> Seq
exchange pm
    | isId pm   = id
    | otherwise = Exchange pm

