module SkelMx where

-- Haskell module generated by the BNF converter

import AbsMx
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transId :: Id -> Result
transId x = case x of
  Id str  -> failure x


transProg :: Prog -> Result
transProg x = case x of
  Prog aliass derivs  -> failure x


transDeriv :: Deriv -> Result
transDeriv x = case x of
  Deriv tyvars binders mderivname seq  -> failure x


transAlias :: Alias -> Result
transAlias x = case x of
  TyAlias id idlist type'  -> failure x


transIdList :: IdList -> Result
transIdList x = case x of
  ILNil  -> failure x
  ILCons id idlist  -> failure x


transTyVar :: TyVar -> Result
transTyVar x = case x of
  TyVar id  -> failure x


transBinder :: Binder -> Result
transBinder x = case x of
  Binder id type'  -> failure x


transMBinder :: MBinder -> Result
transMBinder x = case x of
  BJust id type'  -> failure x
  BNothing id  -> failure x


transMDerivName :: MDerivName -> Result
transMDerivName x = case x of
  DerivName id  -> failure x
  NoDerivName  -> failure x


transAlong :: Along -> Result
transAlong x = case x of
  AJust ids  -> failure x
  ANothing  -> failure x


transType :: Type -> Result
transType x = case x of
  Tensor type'1 type'2  -> failure x
  Par type'1 type'2  -> failure x
  One  -> failure x
  Bot  -> failure x
  Plus type'1 type'2  -> failure x
  Choice type'1 type'2  -> failure x
  Top  -> failure x
  Zero  -> failure x
  Lollipop type'1 type'2  -> failure x
  TyId id  -> failure x
  Bang type'  -> failure x
  Quest type'  -> failure x
  Neg type'  -> failure x
  Forall id type'  -> failure x
  Exists id type'  -> failure x
  Mu id type'  -> failure x


transChoice :: Choice -> Result
transChoice x = case x of
  Fst  -> failure x
  Snd  -> failure x


transSeq :: Seq -> Result
transSeq x = case x of
  Ax id1 id2  -> failure x
  Cut mbinder1 seq2 mbinder3 seq4  -> failure x
  ParSeq id1 id2 seq3 id4 seq5  -> failure x
  TensorSeq id1 id2 id3 seq4  -> failure x
  ChoiceSeq id1 choice2 id3 seq4  -> failure x
  Case id1 id2 seq3 id4 seq5  -> failure x
  Bottom id  -> failure x
  Unit id seq  -> failure x
  Crash id along  -> failure x
  Pack id1 id2 type'3 seq4  -> failure x
  Unpack id1 id2 id3 seq4  -> failure x
  Offer id1 id2 seq3  -> failure x
  Demand id1 id2 seq3  -> failure x
  Ignore id seq  -> failure x
  Alias id1 id2 seq3  -> failure x
  Fold id1 id2 seq3  -> failure x
  Unfold id1 id2 seq3  -> failure x
  Refer id types ids  -> failure x
  Hole  -> failure x



