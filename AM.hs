{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}


-- | The abstract machine
module AM where 

import LL
import Data.List (mapAccumL, elemIndex, find)
import Data.Graph
import Data.Tree

-- | Layout of a type in the heap
data Layout = Layout `Then` Layout 
            | Bit Layout
            | Pointer Layout
            | MetaL Type
            | Union Layout Layout
            | Empty
  deriving Eq              

mkLayout :: Type -> Layout
mkLayout t = case mkPositive t of
              One      -> Empty
              Zero     -> Empty
              a :⊗: b  -> mkLayout a `Then` mkLayout b
              a :⊕: b  -> Bit (mkLayout a `Union` mkLayout b)
              Bang x   -> Pointer (mkLayout x)
              Exists{} -> Pointer (MetaL $ meta "Polymorphic")
              Meta{}   -> MetaL (mkPositive t)
              TVar{}   -> error "cannot know layout of var"

class Ty ty where
  splitProd :: ty -> Maybe (ty,ty)

instance Ty () where
  splitProd _ = Just ((),())

-- | A Cell of the Heap
data Cell ref ty where
    Freed   :: Cell ref ty
    Tag     :: Bool -> Cell ref ty
    New     :: Cell ref ty
    Delay   :: RefCount ref -> Maybe (Closure ref ty) -> Cell ref ty
    Q       :: Type -> ref -> Cell ref ty -- 1st arg should be a monotype
    NewMeta :: Layout -> Cell ref ty

countRefs New = 0
countRefs (Delay n _) = n

type CellRef = Int

type TypeEnv = [Type]

type Env ref ty = [(Name,ref,ty)]

type Closure ref ty = (Seq,Env ref ty,TypeEnv)

class (Num (RefCount ref), Eq ref) => IsRef ref where
  type RefCount ref 
  shift :: Layout -> ref -> ref
  next  :: ref -> ref
  nullPointer  :: ref
  
class (IsRef (Ref heap),Ty ty) => IsHeap heap ty where
  type Ref heap
  (!) :: heap -> Ref heap -> Cell (Ref heap) ty
  replace :: Ref heap -> Cell (Ref heap) ty -> heap -> heap
  alloc' :: Layout -> heap -> (heap,Ref heap)
  emptyHeap :: heap

alloc :: IsHeap heap ty => Type -> heap -> (heap,Ref heap)
alloc = alloc' . mkLayout

type Heap = [Cell Int ()]

instance IsRef Int where
  type RefCount Int = Int
  shift t = (sizeOf t +)
  next = succ
  nullPointer = -1
  
instance IsHeap Heap () where
  type Ref Heap = CellRef
--  type Size Heap = Int
  (!) = (!!)
  replace n v (h) = let (l,_:r) = splitAt n h
                in l ++ v : r
  alloc' t (h) = (h ++ replicate (sizeOf t) New,length h)
  emptyHeap = []

type System h ty = ([Closure (Ref h) ty],h)

(!!+) :: Env ref ty -> Int -> ref
e !!+ v = snd3 (e!!v)
  where snd3 (_,a,_) =  a

runClosure :: IsHeap h ty => h -> Closure (Ref h) ty -> Maybe (h,[Closure (Ref h) ty])
runClosure h (Plus x y v a b,e,te)
--   | Redirect r <- ... Plus x y v a b,...r.
  | Tag c <- h!(e!!+v) = Just (replace (e!!+v) Freed h,
                              [(if c then a else b,increment v (if c then x else y) e,te)])
runClosure h (Cross ty x y v a,e,te)
  = Just (h,[(a,el++[(x,z),(y,shift (mkLayout (te∙ty)) z)] ++ er,te)])
  where (el,(_,z,_):er) = splitAt v e
runClosure h (Par ty x y v a b,e,te)
  = Just (h,[(a,el++[(x,z)],te)
            ,(b,[(y,shift (mkLayout (te∙ty)) z)]++er,te)])
    where (el,(_,z):er) = splitAt v e
runClosure h (With x t v a,e,te)
  = Just (replace (e!!+v) (Tag t) h,[(a,increment v x e,te)])
  -- FIXME: free the part which is unused (half of the time)
runClosure h (SOne v a,e,te)
  = Just(h,[(a,el++er,te)])
    where (el,_:er) = splitAt v e
runClosure h (SBot,e,te)
  = Just (h,[])

runClosure h (TApp tp x v ty a,e,te)
  = Just (replace (e!!+v) (Q ty q) h',[(a,el++(x,z):er,te)])
  where (el,(_,z):er) = splitAt v e
        (h',q) = alloc ((ty:te)∙tp) h
runClosure h (TUnpack x v a,e,te)
  | Q ty p <- h!w = Just (replace w Freed h,[(a,el++[(x,p)]++er,ty:te)])
  where (el,(_,w):er) = splitAt v e
runClosure h (Exchange π a,e,te)
  = Just (h,[(a,[e!!x | x <- π],te)])
runClosure h (Ax (Bang ty),[(_,w),(_,x)],te)
  | d@(Delay n cl) <- h!x = Just (replace w d (replace x Freed h),[])
runClosure h (Ax (TVar True v),e,te)
  = Just (h,[(copy'' (te!!v),e,te)])
runClosure h (Ax (Forall _ ty),[(_,w),(_,x)],te)
  | q@(Q ty p) <- h!x = Just (replace w q (replace x Freed h),[])
runClosure h (Ax (Meta _ _ _),_,_) = Nothing -- FIXME: should show something
runClosure h (Ax ty,[w,x],te)
  = Just (h,[(copy'' ty,[w,x],te)])
runClosure h (Cut x y ty v a b,e,te)
  = Just (h',[(a,(x,q):ea,te),(b,(y,q):eb,te)])
  where (ea,eb) = splitAt v e
        (h',q)  = alloc (te∙ty) h

runClosure h (Offer x v a,e,te)
  = Just (replace (e!!+v) (Delay n (Just (a,el++(x,nullPointer):er,te))) h,[])
    where (el,(_,w):er) = splitAt v e
          n = countRefs (h!(e!!+v)) -- FIXME: JP: how much should the refcount be? I am suspecting only the "clients" should be counted.
runClosure h (Demand x ty v a,e,te)
  | (Delay n (Just (u,e',te'))) <- h!p
  = Just (modifyRefCount (subtract 1) p $ h'
         ,[(a,el++[(x,q)]++er,te),(u,setNull e' q,te')])
    where (el,(_,p):er) = splitAt v e
          (h',q) = alloc (te ∙ ty) h
runClosure h (Ignore v a,e,te)
  = Just (modifyRefCount (subtract 1) m h,[(a,el++er,te)])
  where (el,(_,m):er) = splitAt v e
runClosure h (Alias v x a,e,te)
  = Just (modifyRefCount (+1) (e!!+v) h,[(a,(x,e!!+v):e,te)])
runClosure _ _ = Nothing

setNull [] q = []
setNull ((x,w):xs) q | w == nullPointer = (x,q):xs
                     | otherwise = (x,w):setNull xs q

modifyRefCount f r h | Delay c cl <- h!r = replace r (Delay (f c) cl)      h
                     | New        <- h!r = replace r (Delay (f 0) Nothing) h

increment :: IsRef ref => Int -> Name -> Env ref ty -> Env ref ty
increment n nm e = let (l,(_,x):r) = splitAt n e
                    in l ++ (nm,next x) : r

copy'' :: Type -> Seq
copy'' (t1 :⊕: t2) = Plus "" ""  0 (copy'' t1) (copy'' t2) -- FIXME: probably a With is necessary here
copy'' (t1 :⊗: t2) = Cross t1 "" "" 0 $
                     Exchange [0,2,1] $
                     Par t1 "" "" 1 (copy'' t1) (copy'' t2)
copy'' Zero = error "Impossible"
copy'' One = SOne 0 SBot
copy'' t@(TVar True _) = Ax t
copy'' t@(Bang _) = Ax t
copy'' t@(Forall _ _) = Ax t
copy'' t = Exchange [1,0] $ copy'' (neg t)

sizeOf :: Layout -> Int
sizeOf (Bit t) = 1 + sizeOf t
sizeOf (t1 `Union` t2) = max (sizeOf t1) (sizeOf t2)
sizeOf (t1 `Then` t2) = sizeOf t1 + sizeOf t2
sizeOf (Pointer _) = 1
sizeOf Empty    = 0
sizeOf (MetaL _) = error "cannot get size of meta-type"

stepSystem :: IsHeap h ty => System h ty -> Maybe (System h ty)
stepSystem ([],h) = Nothing
stepSystem (cl:cls,h) | Just (h',cl') <- runClosure h cl = Just (cl'++cls,h')
stepSystem (cl:cls,h) = do (cls',h') <- stepSystem (cls,h)
                           return (cl:cls',h')

runSystem :: IsHeap h ty => System h ty -> System h ty
runSystem s | Just s' <- stepSystem s = runSystem s'
runSystem s = s

stepClosure :: IsHeap h ty => Int -> System h ty -> Maybe (System h ty)
stepClosure i (cls,h) | i < length cls = do (h',c') <- runClosure h c
                                            return (clsL ++ c' ++ clsR,h')
                      | otherwise      = Nothing
  where (clsL,c:clsR) = splitAt i cls

noClosures :: System h ty -> Int
noClosures (cls,h) = length cls

-- | Convert a derivation to a System. Assumes the type env. is
-- ignorable (empty or contains only metavars)
derivToSystem :: IsHeap h ty => Deriv -> System h ty
derivToSystem (Deriv _ ctx a) = ([closure],heap)
  where closure = (a,zip (map fst ctx) refs,[])
        (heap,refs) = mapAccumL (flip alloc) emptyHeap (map snd ctx)
 

data Key = Cl Int | Ref Int
data Node = NCell (Cell Int ()) | NCl -- could be more precise: the code of all closures will be considered equivalent.
type GrData = (Node,Key,[Key])

systemGr :: System Heap ty -> [GrData]
systemGr (cls,h) = zipWith closureGr cls [0..] ++ concat (zipWith cellGr h [0..])

closureGr :: Closure Int ty -> Int -> GrData
closureGr (s,e,_) i = (NCl,Cl i,[Ref x | (_,x) <- e])

cellGr :: Cell Int ty -> Int -> [GrData]
cellGr (Delay _ Nothing) i = cellGr New i 
                        -- Remark that the ref-count is not important
                        -- (already encoded in the graph structure).
cellGr (Delay _ (Just cl)) i = closureGr cl (negate i) : cellGr New i
cellGr (Q t r) i = [(NCell (Q t 0),Ref i,[Ref r])]
cellGr atom i = [(NCell atom,Ref i,[])]

{-

type RefMap ref =  Map ref ref

newtype Comparator ref a = ErrorT String (State (RefMap ref)) a
  deriving (Monad, MonadState (RefMap ref), MonadError String)

unify :: Ord ref => ref -> ref -> Comparator ref ()
unify r1 r2 = do
  r' <- M.lookup r1 <$> get
  case r' of
    Nothing -> modify (M.insert r1 r2)
    Just r'' -> assert r'' r2
    
assert x x' = when (x /= x') (fail "not equal")
    
              
              
compareHeaps () h () h' = case x of
  
contentsOfAt :: Layout -> SymRef -> [SymRef]
contentsOf _ Null = []
contentsOf (Next x) = contentsOf 1 x
contentsOf (Shift l x) = contentsOf l x
contentsOf Empty (Named l x) = ...
contentsOf (0) (Named l x) = ...

  
  
  
fetch :: SymRef -> SymHeap -> [Cell SymRef]  
fetch (Named l s)  
    

compareClosures (s,e,te) h (s',e',te') h' = do
  assert s s'
  assert te te'
  forM_ (zip e e') $ \((v,x),(v',x')) -> do
    assert v v'
    unify x x'
    compareHeaps (fetch x h) (fetch x' h')
-}    


-- Invariant: a blocked system will have only new cells in the
-- heap. (!except for exponentials and anything that can write in the heap)
        
-- TODO: sort the closures.

                

mkGraph :: forall r ty. Eq r => [Closure r ty] -> [((),Int,[Int])]
mkGraph cls = map mkOne cls'
  where cls' :: [(Closure r,Int)]
        cls' = zip cls [0..]
        mkOne (cl@(_,e,_),ix) = ((),ix,connected)
          where 
            connected :: [Int]
            connected = [ix' | ((_,e',_),ix') <- cls', (_,x') <- e', (_,x) <- e, x == x']
        
-- | From a blocked system, recover the tree-structure of closures.        
mkTree :: forall r ty. Eq r => [Closure r ty] -> Tree (Closure r ty)
mkTree cls = fmap (cls!!) t
  where [t] = dff $ fst $ graphFromEdges' $ mkGraph cls
        -- there can be only one tree because we do not have mix.

-- | Rename variables in sequents so they form a meaningful cut
-- structure.  In the output: the last variable communicates with the
-- parent; the others with each of the children in reverse order.
recoverCutStructure :: Eq r => Tree (Closure r ty) -> Tree (Closure r ty)
recoverCutStructure (Node c []) = Node c []
recoverCutStructure (Node c@(_,e,_) cs) = 
  Node c (map recoverCutStructure $ 
          reverse [placeLast x c' | (_,x) <- e, Just c' <- [connectedTo x cs]])

connectedTo x cs = find (\(Node (_,e,_) _) -> any ((== x) . snd) e) cs

-- | Place the given reference in the 1st position in the env in the
-- closure, and adapt the subtrees.
placeLast :: Eq r => r -> Tree (Closure r ty) -> Tree (Closure r ty)
placeLast x (Node (s,e,te) ts) = Node (Exchange ρ s, [e!!j | j <- ρ], te) ts
  where Just i = elemIndex x (map snd e)
        ρ = [0..i-1]++[i+1..length e]++[i]

mkCuts :: Tree (Closure r ty) -> Seq
mkCuts (Node (s,e,te) []) = s
mkCuts (Node (s,_:e,te) ts) = foldr (\(t,γ) -> Cut "_" "_" dum γ t) s $ zip (map mkCuts ts) [0..]

sequentialize :: (Eq (Ref h)) => System h ty -> Seq
sequentialize (cls,_heap) = mkCuts . recoverCutStructure . mkTree $ cls
      
