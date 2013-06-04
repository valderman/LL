{-# LANGUAGE ViewPatterns,MultiParamTypeClasses,GeneralizedNewtypeDeriving #-}
module ToLL where

import Data.Map (Map)
import qualified Data.Map as M

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader

import Pretty()

import Data.List (findIndex)
import Data.Function (on)
import Data.Ord (comparing)

import LL hiding (exchange)

import Perm

import qualified AbsMx as C
import AbsMx (Id(..),Choice(..),Binder(..),Prog)

data Ident = Ident { name :: String, pos :: (Int,Int) }

instance Eq Ident where
    (==) = (==) `on` name

instance Ord Ident where
    compare = comparing name

instance Show Ident where
    show (Ident n (y,x)) = show y ++ ":" ++ show x ++ ":" ++ n

i :: Id -> Ident
i (Id (p,s)) = Ident s p

desugar :: Prog -> Either Err Deriv
desugar (C.Deriv as tvs bs s) = runT init_alias init_tyvars $ do
    bs' <- forM bs $ \ (Binder (i -> x) t) -> (,) x <$> bind x t
    (s',sb) <- trSeq s
    ctx <- sequence
        [ case lookup x bs' of
            Just t  -> return (name x,t)
            Nothing -> throwError (UnboundIdentifier x)
        | x <- sb
        ]
    forM_ bs' $ \ (b,_) -> when (b `notElem` sb) (throwError (IdentifierEscapes b))
    return $ Deriv (map name init_tyvars) ctx s'
  where
    init_alias  = M.fromList [ (x,t) | C.TyAlias (i -> x) t <- as ]
    init_tyvars = [ tv | C.TyVar (i -> tv) <- tvs ]

newtype T a = T { unT :: ReaderT (Map Ident C.Type) (StateT ([Ident],Map Ident Type) (Either Err)) a }
  deriving
    ( Functor, Applicative, Monad
    , MonadState ([Ident],Map Ident Type)     -- type variables and identifiers in scope
    , MonadReader (Map Ident C.Type)  -- type aliases
    , MonadError Err
    )

runT :: Map Ident C.Type -> [Ident] -> T a -> Either Err a
runT as tvs (T m) = evalStateT (runReaderT m as) (tvs,M.empty)

data Err
    = MismatchTypes Seq_ Ident Ident Type Type
    | InsufficientTypeInfo Ident Ident
    | UnboundIdentifier Ident
    | BoundIdentifier Ident Type Type
    | IdentifierEscapes Ident
    | TypeError Ident Type Type_
    | NotBang Ident Type Ident
    | PermutationError [Ident] [Ident]
    | Fail String
    | Hole [(Ident,Type)]
  deriving Show

instance Error Err where
    strMsg = Fail

data Seq_ = Ax_ | Cut_
  deriving Show

data Type_ = Par_ | Tensor_ | Plus_ | With_ | Bot_ | One_ | Zero_ | Forall_ | Exists_ | Bang_ | Quest_
  deriving Show

todo :: String -> a
todo = error . (++ " todo")

typeOf :: Ident -> T Type
typeOf x = do
    (_,m) <- get
    case M.lookup x m of
        Just t -> return t
        Nothing -> throwError (UnboundIdentifier x)

eat :: Ident -> T Type
eat x = do
    t <- typeOf x
    (tvs,m) <- get
    put (tvs,M.delete x m)
    return t

-- watch out for shadowing
bind :: Ident -> C.Type -> T Type
bind x t = trType t >>= \ t' -> bind' x t' >> return t'

bind' :: Ident -> Type -> T ()
bind' x t = do
    (tvs,m) <- get
    case (M.lookup x m,M.insert x t m) of
        (Just t',_)  -> throwError (BoundIdentifier x t t')
        (Nothing,m') -> put (tvs,m')

locally :: T a -> T a
locally u = do
    x <- get
    r <- u
    put x
    return r

insertTyVar :: Ident -> T ()
insertTyVar x = do
    (tvs,m) <- get
    put (x:tvs,M.map (apply wk) m)
    -- all identifiers gets their variables shifted

trType :: C.Type -> T Type
trType = locally . go
  where
    go t0 = case t0 of
        C.Tensor x y   -> (:⊗:) <$> go x <*> go y
        C.Par x y      -> (:|:) <$> go x <*> go y
        C.Plus x y     -> (:⊕:) <$> go x <*> go y
        C.Choice x y   -> (:&:) <$> go x <*> go y
        C.One          -> return One
        C.Bot          -> return Bot
        C.Top          -> return Top
        C.Zero         -> return Zero
        C.Lollipop x y -> (⊸) <$> go x <*> go y
        C.Bang x       -> Bang <$> go x
        C.Quest x      -> Quest <$> go x
        C.Neg x        -> neg <$> go x
        C.Forall (i -> x) t -> insertTyVar x >> (Forall (name x) <$> go t)
        C.Exists (i -> x) t -> insertTyVar x >> (Exists (name x) <$> go t)
        C.TyId (i -> x)     -> do
            (tvs,_) <- get
            case findIndex (== x) tvs of
                Just ix -> return (var ix)
                Nothing -> do
                    as <- ask
                    case M.lookup x as of
                        Just t  -> go t
                        Nothing -> throwError (UnboundIdentifier x)

negTy :: C.Type -> C.Type
negTy = C.Neg

reorder :: (Int -> Int) -> Ident -> (Seq,[Ident]) -> T (Seq,[Ident])
reorder k z (s,zb) = case findIndex (== z) zb of
    Nothing -> throwError (IdentifierEscapes z)
    Just n  -> let (zb',pm) = perm zb (swap x n) in return (exchange pm s,zb' `without` x)
      where x = k (length zb)

putFirst,putLast :: Ident -> (Seq,[Ident]) -> T (Seq,[Ident])
putFirst = reorder (const 0)
putLast = reorder pred

tensorOrder :: Ident -> Ident -> (Seq,[Ident]) -> T (Seq,[Ident],[Ident])
tensorOrder x y (s,bs) = case findIndex (== x) bs of
    Just nx -> case findIndex (== y) bs of
        Just ny ->
            let (pm,l,r) = tensorOrder' nx ny bs
            in  return (exchange pm s,l,r)
        Nothing -> throwError (IdentifierEscapes y)
    Nothing -> throwError (IdentifierEscapes x)

munch :: Ident -> [Ident] -> T ([Ident],[Ident])
munch x b = case break (== x) b of
    (l,_:r) -> return (l,r)
    _       -> throwError (IdentifierEscapes x)

tensorOrder' :: Int -> Int -> [a] -> (Permutation,[a],[a])
tensorOrder' x y bs = (pm',l,r)
  where
    (p,pm)
        | x < y = (x,swap (x+1) y)
        | y + 1 == x = (y,swap x y)
        | y < x = (y,swap y (y+1) `compose` swap x y )
    (bs',pm') = perm bs pm
    (l,_x:_y:r) = splitAt p bs'

saveCtx :: T (Ident -> Type)
saveCtx = do
    (_,m) <- get
    return $ \ x -> case M.lookup x m of
        Just t  -> t
        Nothing -> error $ "saveCtx: internal error, lost identifier " ++ show x

isBang :: Type -> Bool
isBang Bang{} = True
isBang _      = False

-- Returns the translated sequents and its context
trSeq :: C.Seq -> T (Seq,[Ident])
trSeq sq = case sq of

    C.Ax (i -> x) (i -> y) -> do
        tx <- eat x
        ty <- eat y
        when (tx /= neg ty) (throwError (MismatchTypes Ax_ x y tx ty))
        return (Ax ty,[x,y])

    C.Cut b1 sx b2 sy -> do

        ((x,tx),(y,ty)) <- case (b1,b2) of
            (C.BNothing (i -> x),C.BNothing (i -> y)) -> throwError (InsufficientTypeInfo x y)
            (C.BJust (i -> x) tx,C.BNothing (i -> y)) -> return ((x,tx),(y,negTy tx))
            (C.BNothing (i -> x),C.BJust (i -> y) ty) -> return ((x,negTy ty),(y,ty))
            (C.BJust (i -> x) tx,C.BJust (i -> y) ty) -> return ((x,tx),(y,ty))

        tx' <- bind x tx
        (sx',xb) <- putFirst x =<< trSeq sx

        ty' <- bind y ty
        (sy',yb) <- putFirst y =<< trSeq sy

        when (tx' /= neg ty') (throwError (MismatchTypes Cut_ x y tx' ty'))

        return (Cut (name x) (name y) ty' (length xb) sx' sy',xb ++ yb)

    C.TensorSeq (i -> x) (i -> y) (i -> z) s -> do

        tz <- eat z

        (tx,ty) <- case tz of
            tx :⊗: ty -> return (tx,ty)
            _         -> throwError (TypeError z tz Tensor_)

        bind' x tx
        bind' y ty
        (s',l,r) <- tensorOrder x y =<< trSeq s

        return (Cross tz (name x) (name y) (length l) s',l ++ [z] ++ r)

    C.ParSeq (i -> z) (i -> x) sx (i -> y) sy -> do

        tz <- eat z

        (tx,ty) <- case tz of
            tx :|: ty -> return (tx,ty)
            _         -> throwError (TypeError z tz Par_)

        bind' x tx
        (sx',xb) <- putLast x =<< trSeq sx

        bind' y ty
        (sy',yb) <- putFirst y =<< trSeq sy

        return (Par tz (name x) (name y) (length xb) sx' sy',xb ++ [z] ++ yb)

    -- :⊕: :&:
    C.Case (i -> z) (i -> x) sx (i -> y) sy -> do

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

    C.ChoiceSeq (i -> x) ch (i -> z) s -> do

        tz <- eat z
        (tx,ty) <- case tz of
            tx :&: ty -> return (tx,ty)
            _         -> throwError (TypeError z tz With_)

        bind' x (choice ch tx ty)
        (s',b) <- trSeq s

        (l,r) <- munch x b
        return (With (name x) (choice ch True False) (length l) s',l ++ [z] ++ r)

    C.Bottom (i -> x) -> do

        tx <- eat x
        when (tx /= Bot) (throwError (TypeError x tx Bot_))
        return (SBot,[x])

    C.Unit (i -> x) s -> do

        tx <- eat x
        when (tx /= One) (throwError (TypeError x tx One_))
        (s',b) <- trSeq s
        return (SOne 0 s',x:b)

    C.Crash (i -> x) (map i -> xs) -> do
        tx <- eat x
        when (tx /= Zero) (throwError (TypeError x tx Zero_))
        _rest <- mapM eat xs
        return (SZero 0,x:xs) -- TODO: this actually has a bigger context

    -- let x = z @ t in s
    C.Pack (i -> x) (i -> z) t s -> do

        t' <- trType t

        tz <- eat z
        tx <- case tz of
            Forall _ tu -> return (apply (subst0 t') tu)
            _           -> throwError (TypeError z tz Forall_)

        bind' x tx
        (s',b) <- trSeq s
        (l,r) <- munch x b
        return (TApp tz (name x) (length l) t' s',l ++ [z] ++ r)

    -- let a @ x = z in s
    C.Unpack (i -> a) (i -> x) (i -> z) s -> do

        tz <- eat z
        tx <- case tz of
            Exists _ tu -> return tu
            _           -> throwError (TypeError z tz Exists_)

        insertTyVar a
        bind' x tx
        (s',b) <- trSeq s
        (l,r) <- munch x b
        return (TUnpack (name x) (length l) s',l ++ [z] ++ r)

    -- offer x for z in s
    -- let x = offer z in s (?)
    C.Offer (i -> x) (i -> z) s -> do

        tz <- eat z
        tx <- case tz of
            Quest t -> return t
            _       -> throwError (TypeError z tz Quest_)


        k <- saveCtx

        bind' x tx
        (s',b) <- trSeq s
        (l,r) <- munch x b

        -- need to check that the types of l and r are all bang
        -- (hence saveCtx to k)
        forM_ (l ++ r) $ \ u -> do
            let t = k u
            unless (isBang t) (throwError (NotBang u t z))

        return (Offer (name x) (length l) s',l ++ [z] ++ r)

    -- let x = demand z in s
    C.Demand (i -> x) (i -> z) s -> do

        tz <- eat z
        tx <- case tz of
            Bang t -> return t
            _      -> throwError (TypeError z tz Bang_)

        bind' x tx
        (s',b) <- trSeq s
        (l,r) <- munch x b
        return (Demand (name x) tz (length l) s',l ++ [z] ++ r)

    -- ignore z in s
    C.Ignore (i -> z) s -> do

        tz <- eat z
        case tz of
            Bang{} -> return ()
            _      -> throwError (TypeError z tz Bang_)

        (s',b) <- trSeq s
        return (Ignore 0 s', z : b)

    -- let z' = alias z in s
    C.Alias (i -> z') (i -> z) s -> do

        tz <- typeOf z
        case tz of
            Bang{} -> return ()
            _      -> throwError (TypeError z tz Bang_)

        bind' z' tz

        (s',l,r) <- tensorOrder z z' =<< trSeq s
        return (Alias (length l) (name z') s',l ++ [z] ++ r)

    C.Hole -> throwError . Hole . M.toList . snd =<< get

choice :: Choice -> a -> a -> a
choice Fst x _ = x
choice Snd _ y = y

-- Smart constructor for exchange
exchange :: Permutation -> Seq -> Seq
exchange pm
    | isId pm   = id
    | otherwise = Exchange pm

