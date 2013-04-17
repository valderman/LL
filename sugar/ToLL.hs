{-# LANGUAGE EmptyDataDecls #-}
module ToLL where

import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad
import Control.Monad.Reader

import qualified LL as LL

data T :: * -> *

instance Monad T
instance MonadReader T

data Error = AxWrongIndicies Seq Int Int

data Ctx :: *

todo :: String -> a
todo = error . (++ " todo")

cons :: Ident -> Ctx -> Ctx
cons = todo "cons"

empty :: Ctx
empty = todo "empty"

position :: Ident -> Ctx -> Int
position = todo "position"

-- | > change x [y,z] (Gamma,x,Delta) = Gamma,y,z,Delta
change :: Ident -> [Ident] -> Ctx -> (Ctx,Int)
change = todo "change"

trSeq :: Seq -> LL.Seq
trSeq seq = case seq of
    Ax x y -> do
        ix <- lookup x
        iy <- lookup y
        case (ix,iy) of
            (0,1) -> return $ LL.Ax dum
            (1,0) -> return $ LL.Exchange [1,0] (LL.Ax dum)
            _     -> throwError (AxWrongIndicies seq ix iy)
    Cut (Binder x _tx) sx (Binder y _ty) sy -> do
        let sxis = sx `idsBut` x
            syis = sy `idsBut` y
        (cx,cy,perm,pos) <- splitCtx sxis syis `fmap` ask
        sx' <- local (const $ x `cons` cx) (trSeq sx)
        sy' <- local (const $ y `cons` cy) (trSeq sy)
        return $ exchange perm $
            LL.Cross dum (name x) (name y) pos sx' sy'
    ParSeq z (Binder x _tx) sx (Binder y _ty) sy -> do
        -- Add splits and rearranges here
        sx' <- local (cons x) (trSeq sx)
        sy' <- local (cons y) (trSeq sy)
        return (LL.Cross dum (name x) (name y) sx' sy')

    Case z l sl r sr -> do
        ctx <- ask
        let (ctx',pos) = change z [l,r]
        local (const ctx') $ do
            sl' <- trSeq sl
            sr' <- trSeq sr
            return (LL.Plus (name l) (name r) sl' sr')

    -- And so on!!!!!!

splitCtx :: [Ident] -> [Ident] -> Ctx -> (Ctx,Ctx,Permutation,Int)
splitCtx l r c = todo "splitCtx"

idsIn :: Seq -> [Ident]
idsIn s = todo "idsIn"

idsBut :: Seq -> Ident -> [Ident]
idsBut s i = todo "idsBut"

-- Smart constructor for exchange
exchange :: Permutation -> Seq -> Seq
exchange perm
    | and (zipWith (==) perm [0..]) = id
    | otherwise                     = LL.Exchange perm

