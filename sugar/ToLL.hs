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

import Data.List (elemIndex)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

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
    -- ^ Ax and Cut with types not matching up
    | InsufficientTypeInfo Ident Ident
    -- ^ Cut with no type information
    | UnboundIdentifier Ident
    -- ^ Unbound
    | BoundIdentifier Ident Type Ident Type
    -- ^ Identifier shadowed
    --   @BoundIdentifier x t1 y t2@ means x : t1 tried to be bound,
    --   but is already bound at y : t2 (hopefully at another position in the source code!)
    | IdentifierEscapes Ident
    -- ^ An identifier that had to be used wasn't
    | TypeError Ident Type Type
    -- ^ Type error
    --   @TypeError z t1 t2@ means z should have a type of form t1, but had t2
    | NotBang Ident Type Ident
    -- ^ Offering (?-destruction) with non-bang typed identifiers in context
    --   @NotBang x t z@ means x had type t which is not bang when offering for z
    | PermutationError [Ident] [Ident]
    -- ^ Case with different contexts in the two branches
    | Hole [(Ident,Type)]
    -- ^ The identifiers and their types in a hole
    | Fail String
    -- ^ Fail in the monad
  deriving Show

instance Error Err where
    strMsg = Fail

data Seq_ = Ax_ | Cut_
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

bind :: Ident -> C.Type -> T Type
bind x t = trType t >>= \ t' -> bind' x t' >> return t'

bind' :: Ident -> Type -> T ()
bind' x t = do
    (tvs,m) <- get
    case (lookupWithKey x m,M.insert x t m) of
        (Just (x',t'),_)  -> throwError (BoundIdentifier x t x' t')
                             -- Shadowing is not allowed, so we throw an error here
        (Nothing,m') -> put (tvs,m')

lookupWithKey :: Ord k => k -> Map k v -> Maybe (k,v)
lookupWithKey k m = (`M.elemAt` m) <$> M.lookupIndex k m

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
            case elemIndex x tvs of
                Just ix -> return (var ix)
                Nothing -> do
                    as <- ask
                    case M.lookup x as of
                        Just t  -> go t
                        Nothing -> throwError (UnboundIdentifier x)

negTy :: C.Type -> C.Type
negTy = C.Neg

munch :: Ident -> [Ident] -> T ([Ident],[Ident])
munch x b = case break (== x) b of
    (l,_:r) -> return (l,r)
    _       -> throwError (IdentifierEscapes x)

reorder :: (Int -> Int) -> Ident -> (Seq,[Ident]) -> T (Seq,[Ident])
reorder k z (s,zb) = case elemIndex z zb of
    Nothing -> throwError (IdentifierEscapes z)
    Just n  -> let (zb',pm) = perm zb (swap x n) in return (exchange pm s,zb' `without` x)
      where x = k (length zb)

putFirst,putLast :: Ident -> (Seq,[Ident]) -> T (Seq,[Ident])
putFirst = reorder (const 0)
putLast = reorder pred

tensorOrder :: Ident -> Ident -> (Seq,[Ident]) -> T (Seq,[Ident],[Ident])
tensorOrder x y (s,bs) = case elemIndex x bs of
    Just nx -> case elemIndex y bs of
        Just ny ->
            let (pm,l,r) = tensorOrder' nx ny bs
            in  return (exchange pm s,l,r)
        Nothing -> throwError (IdentifierEscapes y)
    Nothing -> throwError (IdentifierEscapes x)

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
    return $ \ x -> fromMaybe
        (error $ "saveCtx: internal error, lost identifier " ++ show x)
        (M.lookup x m)

isBang :: Type -> Bool
isBang Bang{} = True
isBang _      = False

matchType :: Ident -> Type -> Type -> T [Type]
matchType z t0 t1 = maybe (throwError (TypeError z t0 t1)) return (go t0 t1)
  where
    go :: Type -> Type -> Maybe [Type]
    go (a :⊗: b) (x :⊗: y) = (++) <$> go a x <*> go b y
    go (a :|: b) (x :|: y) = (++) <$> go a x <*> go b y
    go (a :⊕: b) (x :⊕: y) = (++) <$> go a x <*> go b y
    go (a :&: b) (x :&: y) = (++) <$> go a x <*> go b y
    go (Bang u)     (Bang v)     = go u v
    go (Quest u)    (Quest v)    = go u v
    go (Forall _ u) (Forall _ v) = go u v
    go (Exists _ u) (Exists _ v) = go u v
    go Meta{}       t            = return [t]
    go u            v
        | u == v    = return []
        | otherwise = mzero

__ :: Type
__ = meta ""

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

        [tx,ty] <- matchType z (__ :⊗: __) tz

        bind' x tx
        bind' y ty
        (s',l,r) <- tensorOrder x y =<< trSeq s

        return (Cross tz (name x) (name y) (length l) s',l ++ [z] ++ r)

    C.ParSeq (i -> z) (i -> x) sx (i -> y) sy -> do

        tz <- eat z

        [tx,ty] <- matchType z (__ :|: __) tz

        bind' x tx
        (sx',xb) <- putLast x =<< trSeq sx

        bind' y ty
        (sy',yb) <- putFirst y =<< trSeq sy

        return (Par tz (name x) (name y) (length xb) sx' sy',xb ++ [z] ++ yb)

    -- :⊕: :&:
    C.Case (i -> z) (i -> x) sx (i -> y) sy -> do

        tz <- eat z
        [tx,ty] <- matchType z (__ :⊕: __) tz

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
        [tx,ty] <- matchType z (__ :&: __) tz

        (s',l,r) <- bindTrSeqMunch x (choice ch tx ty) s
        return (With (name x) (choice ch True False) (length l) s',l ++ [z] ++ r)

    C.Bottom (i -> x) -> do

        tx <- eat x
        [] <- matchType x Bot tx
        return (SBot,[x])

    C.Unit (i -> x) s -> do

        tx <- eat x
        [] <- matchType x One tx
        (s',b) <- trSeq s
        return (SOne 0 s',x:b)

    C.Crash (i -> x) (map i -> xs) -> do
        tx <- eat x
        [] <- matchType x Zero tx
        _rest <- mapM eat xs
        return (SZero 0,x:xs)

    -- let x = z @ t in s
    C.Pack (i -> x) (i -> z) t s -> do

        t' <- trType t

        tz <- eat z
        [apply (subst0 t') -> tx] <- matchType z (Forall "" __) tz

        (s',l,r) <- bindTrSeqMunch x tx s
        return (TApp tz (name x) (length l) t' s',l ++ [z] ++ r)

    -- let a @ x = z in s
    C.Unpack (i -> a) (i -> x) (i -> z) s -> do

        tz <- eat z
        [tx] <- matchType z (Exists "" __) tz

        insertTyVar a
        (s',l,r) <- bindTrSeqMunch x tx s
        return (TUnpack (name x) (length l) s',l ++ [z] ++ r)

    -- offer x for z in s
    C.Offer (i -> x) (i -> z) s -> do

        tz <- eat z
        [tx] <- matchType z (Quest __) tz

        k <- saveCtx

        (s',l,r) <- bindTrSeqMunch x tx s

        -- need to check that the types of l and r are all bang
        -- (hence saveCtx to k)
        forM_ (l ++ r) $ \ u -> do
            let t = k u
            unless (isBang t) (throwError (NotBang u t z))

        return (Offer (name x) (length l) s',l ++ [z] ++ r)

    -- let x = demand z in s
    C.Demand (i -> x) (i -> z) s -> do

        tz <- eat z
        [tx] <- matchType z (Bang __) tz

        (s',l,r) <- bindTrSeqMunch x tx s
        return (Demand (name x) tz (length l) s',l ++ [z] ++ r)

    -- ignore z in s
    C.Ignore (i -> z) s -> do

        tz <- eat z
        [_] <- matchType z (Bang __) tz

        (s',b) <- trSeq s
        return (Ignore 0 s', z : b)

    -- let z' = alias z in s
    C.Alias (i -> z') (i -> z) s -> do

        tz <- typeOf z
        [_] <- matchType z (Bang __) tz

        bind' z' tz

        -- tensorOrder because the two identifiers need to be next to each other
        (s',l,r) <- tensorOrder z z' =<< trSeq s
        return (Alias (length l) (name z') s',l ++ [z] ++ r)

    C.Hole -> throwError . Hole . M.toList . snd =<< get

bindTrSeqMunch :: Ident -> Type -> C.Seq -> T (Seq,[Ident],[Ident])
bindTrSeqMunch x tx s = do
    bind' x tx
    (s',b) <- trSeq s
    (l,r) <- munch x b
    return (s',l,r)

choice :: Choice -> a -> a -> a
choice Fst x _ = x
choice Snd _ y = y

-- Smart constructor for exchange
exchange :: Permutation -> Seq -> Seq
exchange pm
    | isId pm   = id
    | otherwise = Exchange pm

