module Mem where

import LL
import AM

translate r (Cut x y ty v a b) = Cut x "z" ty v (r a) (Cut "w" y ty 0 (What "mem" []) (r b))

insertMemSeq :: Seq -> Seq
insertMemSeq cut@(Cut x y ty v a b) = translate insertMemSeq cut
insertMemSeq (Exchange ρ s) = Exchange ρ (insertMemSeq s)
insertMemSeq (Ax ty) = Ax ty
insertMemSeq (Cross ty x y v a) = Cross ty x y v (insertMemSeq a)
insertMemSeq (Par ty x y v a b) = Par ty x y v (insertMemSeq a) (insertMemSeq b)
insertMemSeq (Plus x y v a b) = Plus x y v (insertMemSeq a) (insertMemSeq b)
insertMemSeq (With x b v a) = With x b v (insertMemSeq a)
insertMemSeq (SOne v a) = SOne v (insertMemSeq a)
insertMemSeq (SZero v) = SZero v
insertMemSeq (SBot) = SBot
insertMemSeq (What x e) = What x e
insertMemSeq (TApp ty x v t a) = TApp ty x v t (insertMemSeq a)
insertMemSeq (TUnpack x v a) = TUnpack x v (insertMemSeq a)
insertMemSeq (Offer x v a) = Offer x v (insertMemSeq a)
insertMemSeq (Demand x ty v a) = Demand x ty v (insertMemSeq a)
insertMemSeq (Ignore v a) = Ignore v (insertMemSeq a)
insertMemSeq (Alias v x a) = Alias v x (insertMemSeq a)

insertMem :: Deriv -> Deriv
insertMem (Deriv tys ctx seq) = Deriv tys ctx (insertMemSeq seq)
