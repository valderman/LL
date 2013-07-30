{-# LANGUAGE ViewPatterns,MultiParamTypeClasses,GeneralizedNewtypeDeriving,
             OverloadedStrings,ScopedTypeVariables #-}
module ToLL where

import Text.PrettyPrint.HughesPJ

import Data.Map (Map)
import qualified Data.Map as M

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer hiding ((<>))

import Pretty(pType,pSeq)

import Data.List (elemIndex,delete)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

import Test.QuickCheck

import LL hiding (exchange)

import Perm

import qualified AbsMx as C
import AbsMx (Id(..),Choice(..),Binder(..),Along(..),Prog)

data Ident = Ident { name :: String, pos :: (Int,Int) }

idName :: Ident -> Doc
idName = text . name

instance Eq Ident where
    (==) = (==) `on` name

instance Ord Ident where
    compare = comparing name

instance Show Ident where
    show (Ident n (y,x)) = show y ++ ":" ++ show x ++ ":" ++ n

i :: Id -> Ident
i (Id (p,s)) = Ident s p

desugar :: Prog -> (Either Err Deriv,Doc)
desugar (C.Deriv as tvs bs s) = runT init_alias init_tyvars $ do
    bs' <- forM bs $ \ (Binder (i -> x) t) -> (,) x <$> bind x t
    (s',sb) <- trSeq s

    pm <- either (throw . uncurry (PermutationError Nothing)) return
                 (getPermutation sb (map fst bs'))

    let ctx = [ (name x,t) | (x,t) <- bs' ]

    return $ Deriv (map name init_tyvars) ctx (exchange pm s')
  where
    init_alias  = M.fromList [ (x,t) | C.TyAlias (i -> x) [] t <- as ]
    init_tyvars = [ tv | C.TyVar (i -> tv) <- tvs ]

newtype T a = T
    { unT :: ReaderT (Map Ident C.Type)
             (StateT ([Ident],Map Ident Type)
             (ErrorT Err
             (Writer [Doc]))) a
    }
  deriving
    ( Functor, Applicative, Monad
    , MonadState ([Ident],Map Ident Type)   -- type variables and identifiers in scope
    , MonadReader (Map Ident C.Type)        -- type aliases
    , MonadWriter [Doc]                     -- debug messages
    , MonadError Err                        -- errors
    )

-- | Report a debug message
report :: Doc -> T ()
report = tell . (:[])

-- | Make the context a doc for debug messages
ctxDoc :: T Doc
ctxDoc = do
    (tvs,ctx) <- get
    let tvs' = map name tvs
    return (prettyCtx tvs' (M.toList ctx))

throw :: Err' -> T a
throw e = do
    (tvs,_) <- get
    throwError (Err (map name tvs) e)

runT :: forall a . Map Ident C.Type -> [Ident] -> T a -> (Either Err a,Doc)
runT as tvs (T m)
    = (\ (a,ws) -> (a,vcat ws)) . runWriter . runErrorT
    $ evalStateT (runReaderT m as) (tvs,M.empty)

data Err = Err
    [Name] -- ^ type variables in scope
    Err'   -- ^ actual error

instance Show Err where
    show (Err tvs e) = render (prettyErr tvs e)

data Err'
    = MismatchTypes Seq_ Ident Type Ident Type
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
    | PermutationError (Maybe Ident) [Ident] [Ident]
    -- ^ Case with different contexts in the two branches
    | Hole [(Ident,Type)]
    -- ^ The identifiers and their types in a hole
    | Fail String
    -- ^ Fail in the monad

prettyErr :: [Name] -> Err' -> Doc
prettyErr tvs e = case e of
    MismatchTypes s x xt y yt ->
        hang ("Types are not dual of each other in a usage of" <+> prettySeq s <> ":") 4
             (p_ctx [(x,xt),(y,yt)])
    InsufficientTypeInfo x y ->
        "At least one of" <+> prettyIdent x <+> "and" <+> prettyIdent y <+>
        "needs to have a type specified in a usage of connect"
    UnboundIdentifier x -> prettyId x <+> "is not bound"
    BoundIdentifier x xt y yt ->
        hang (prettyId x <+> "is already bound:") 4
             (p_ctx [(x,xt),(y,yt)])
    IdentifierEscapes x -> prettyIdent x <+> "is never used"
    TypeError x t t' ->
        prettyId x <+> "has type" <+> p_ty t' <+> "which does not match the shape" <+> p_ty t
    NotBang x t z ->
        prettyId x <+> "has type" <+> p_ty t <+> "which is not of bang type when offering" <+> prettyIdent z
    PermutationError Nothing xs [] ->
        hang "Assumptions not used in the sequent:" 4 (ids xs)
    PermutationError Nothing xs ys ->
        -- This case cannot happen, the variables would be reported as unbound instead
        hang "Derivation variables and assumption are not a permutation of each other" 4
            (ids xs $$ ids ys)
    PermutationError (Just z) xs ys ->
        hang (prettyId z <+> "deconstructed, but the case arms do not use the same variables") 4
            $  "Left:" <+> ids xs
            $$ "Right:" <+> ids ys
    Hole ctx -> hang ("Hole context:") 4 (p_ctx ctx)
    Fail s -> "Unknown error:" <+> text s
  where
    ids :: [Ident] -> Doc
    ids [] = parens "none"
    ids xs = hcat (punctuate comma (map prettyIdent xs))

    p_ty  = prettyType tvs
    p_ctx = prettyCtx tvs

prettyCtx :: [Name] -> [(Ident,Type)] -> Doc
prettyCtx tvs ctx = vcat [ prettyIdent x <+> colon <+> prettyType tvs t | (x,t) <- ctx ]

prettyIdent :: Ident -> Doc
prettyIdent (Ident n (y,x)) = text n <> parens (int y <> comma <> int x)

prettyId :: Ident -> Doc
prettyId (Ident n (y,x)) = int y <> colon <> int x <> colon <+> text n
-- prettyIdent = text . show

prettyType :: [Name] -> Type -> Doc
prettyType tvs = pType 0 (tvs ++ ["<gbl:" ++ show i ++ ">" | i <- [0..]])

prettySeq :: Seq_ -> Doc
prettySeq s = case s of
    Ax_ -> "<->"
    Cut_ -> "connect"

instance Error Err where
    strMsg = Err [] . Fail

data Seq_ = Ax_ | Cut_
  deriving Show

todo :: String -> a
todo = error . (++ " todo")

typeOf :: Ident -> T Type
typeOf x = do
    (_,m) <- get
    case M.lookup x m of
        Just t -> return t
        Nothing -> throw (UnboundIdentifier x)

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
        (Just (x',t'),_)  -> throw (BoundIdentifier x t x' t')
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
        C.Forall (i -> x) t -> insertTyVar x >> (Forall (name x)  <$> go t)
        C.Exists (i -> x) t -> insertTyVar x >> (Exists (name x) <$> go t)
        C.Mu (i -> x) t     -> insertTyVar x >> (Mu True (name x) <$> go t)
        C.TyId (i -> x)     -> do
            (tvs,_) <- get
            case elemIndex x tvs of
                Just ix -> return (var ix)
                Nothing -> do
                    as <- ask
                    case M.lookup x as of
                        Just t  -> go t
                        Nothing -> throw (UnboundIdentifier x)

negTy :: C.Type -> C.Type
negTy = C.Neg

munch :: Ident -> [Ident] -> T ([Ident],[Ident])
munch x b = case break (== x) b of
    (l,_:r) -> return (l,r)
    _       -> throw (IdentifierEscapes x)

reorder :: (Int -> Int) -> Ident -> (Seq,[Ident]) -> T (Seq,[Ident])
reorder k z (s,zb) = case elemIndex z zb of
    Nothing -> throw (IdentifierEscapes z)
    Just n  -> let (zb',pm) = perm zb (swap x n) in return (exchange pm s,zb' `without` x)
      where x = k (length zb)

putFirst,putLast :: Ident -> (Seq,[Ident]) -> T (Seq,[Ident])
putFirst = reorder (const 0)
putLast = reorder pred

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
matchType z t0 t1 = maybe (throw (TypeError z t0 t1)) return (go t0 t1)
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
    go (Mu b1 _ u)  (Mu b2 _ v)  | b1 == b2  = go u v
                                 | otherwise = mzero
    go Meta{}       t            = return [t]
    go u            v
        | u == v    = return []
        | otherwise = mzero

__ :: Type
__ = meta ""

-- | The 'false' Boson (‽)
false :: Boson
false = False

-- Returns the translated sequent and its context
trSeq :: C.Seq -> T (Seq,[Ident])
trSeq sq = case sq of

    C.Ax (i -> x) (i -> y) -> do
        tx <- eat x
        ty <- eat y
        when (tx /= neg ty) (throw (MismatchTypes Ax_ x tx y ty))
        return (Ax ty,[x,y])

    C.Cut b1 sx b2 sy -> do

        ((x,tx),(y,ty)) <- case (b1,b2) of
            (C.BNothing (i -> x),C.BNothing (i -> y)) -> throw (InsufficientTypeInfo x y)
            (C.BJust (i -> x) tx,C.BNothing (i -> y)) -> return ((x,tx),(y,negTy tx))
            (C.BNothing (i -> x),C.BJust (i -> y) ty) -> return ((x,negTy ty),(y,ty))
            (C.BJust (i -> x) tx,C.BJust (i -> y) ty) -> return ((x,tx),(y,ty))

        tx' <- bind x tx
        (sx',xb) <- putFirst x =<< trSeq sx

        ty' <- bind y ty
        (sy',yb) <- putFirst y =<< trSeq sy

        when (tx' /= neg ty') (throw (MismatchTypes Cut_ x tx' y ty'))

        return (Cut (name x) (name y) ty' (length xb) sx' sy',xb ++ yb)

    C.TensorSeq (i -> x) (i -> y) (i -> z) s -> do

        tz <- eat z

        [tx,ty] <- matchType z (__ :⊗: __) tz

        bind' x tx
        bind' y ty
        (s',bs) <- trSeq s
        let bs' = delete x (delete y bs)
            pm :: Permutation
            pm = either (error "trSeq: TensorSeq malformed context (or identifier escapes?)")
                    id (getPermutation bs (x:y:bs'))

        report $ hang (idName x <> comma <> idName y <> equals <> idName z <+> colon) 4
             $ hsep (map idName bs)
            $$ hsep (map idName (x:y:bs'))
            $$ text (show pm)

        return (Cross false tz (name x) (name y) 0 (exchange pm s'),z:bs')

    C.ParSeq (i -> z) (i -> x) sx (i -> y) sy -> do

        tz <- eat z

        [tx,ty] <- matchType z (__ :|: __) tz

        bind' x tx
        (sx',xb) <- putLast x =<< trSeq sx

        bind' y ty
        (sy',yb) <- putFirst y =<< trSeq sy

        return (Par false tz (name x) (name y) (length xb) sx' sy',xb ++ [z] ++ yb)

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

        p <- either (throw . uncurry (PermutationError (Just z)))
                    (return . (0:) . map succ)
                    (getPermutation yb xb)

        report $ hang ("case" <+> idName z <+> "{" <+> idName x <> comma <> idName y <+> "}" <+> colon) 4
             $ hsep (map idName xb)
            $$ hsep (map idName yb)
            $$ text (show p)

        return (Plus (name x) (name y) 0 sx' (exchange p sy'),z:xb)

    C.ChoiceSeq (i -> x) ch (i -> z) s -> do

        tz <- eat z
        [tx,ty] <- matchType z (__ :&: __) tz

        (s',l,r) <- bindTrSeqMunch x (choice ch tx ty) s
        return (With false tz (name x) (choice ch True False) (length l) s',l ++ [z] ++ r)

    C.Bottom (i -> x) -> do

        tx <- eat x
        [] <- matchType x Bot tx
        return (SBot,[x])

    C.Unit (i -> x) s -> do

        tx <- eat x
        [] <- matchType x One tx
        (s',b) <- trSeq s
        return (SOne false 0 s',x:b)

    C.Crash (i -> x) (along -> xs) -> do
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
        return (TApp false tz (name x) (length l) t' s',l ++ [z] ++ r)

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
            unless (isBang t) (throw (NotBang u t z))

        return (Offer false (name x) (length l) s',l ++ [z] ++ r)

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
        return (Ignore false 0 s', z : b)

    -- let z' = alias z in s
    C.Alias (i -> z') (i -> z) s -> do

        tz <- typeOf z
        [_] <- matchType z (Bang __) tz

        bind' z' tz

        --          z:!t,l,z':!t,r |- s
        -- --------------------------------------
        --    l,z:!t,r |- let z' = alias z in s

        (s1,b1) <- trSeq s
        -- b1 is now some mess and we should put it on a form z,l,z',r
        (s2,b2) <- putFirst z (s1,b1)
        -- b2 is now on the form l,z',r
        (l,r) <- munch z' b2
        return (Alias false (length l) (name z') s2,l ++ [z] ++ r)

    C.Fold (i -> x) (i -> z) s -> do

        tz <- eat z
        [tr] <- matchType z (Mu True "" __) tz
        (s',l,r) <- bindTrSeqMunch x (foldTy (name x) tr) s
        return (Fold (name z) (length l) s',l ++ [z] ++ r)

    C.Unfold (i -> x) (i -> z) s -> do

        tz <- eat z
        [tr] <- matchType z (Mu False "" __) tz
        (s',l,r) <- bindTrSeqMunch x (unfoldTy (name x) tr) s
        return (Unfold (name z) (length l) s',l ++ [z] ++ r)

    C.Hole -> throw . Hole . M.toList . snd =<< get

bindTrSeqMunch :: Ident -> Type -> C.Seq -> T (Seq,[Ident],[Ident])
bindTrSeqMunch x tx s = do
    bind' x tx
    (s',b) <- trSeq s
    (l,r) <- munch x b
    return (s',l,r)

choice :: Choice -> a -> a -> a
choice Fst x _ = x
choice Snd _ y = y

along :: Along -> [Ident]
along (AJust xs) = map i xs
along ANothing   = []

-- Smart constructor for exchange
exchange :: Permutation -> Seq -> Seq
exchange pm
    | isId pm   = id
    | otherwise = Exchange pm

