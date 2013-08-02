module AbsMx where

-- Haskell module generated by the BNF converter


newtype Id = Id ((Int,Int),String) deriving (Eq,Ord,Show)
data Prog =
   Prog [Alias] [Deriv]
  deriving (Eq,Ord,Show)

data Deriv =
   Deriv [TyVar] [Binder] MDerivName Seq
  deriving (Eq,Ord,Show)

data Alias =
   TyAlias Id IdList Type
  deriving (Eq,Ord,Show)

data IdList =
   ILNil
 | ILCons Id IdList
  deriving (Eq,Ord,Show)

data TyList =
   TLNil
 | TLSingle Type
 | TLCons Type TyList
  deriving (Eq,Ord,Show)

data TyVar =
   TyVar Id
  deriving (Eq,Ord,Show)

data Binder =
   Binder Id Type
  deriving (Eq,Ord,Show)

data MBinder =
   BJust Id Type
 | BNothing Id
  deriving (Eq,Ord,Show)

data MDerivName =
   DerivName Id
 | NoDerivName
  deriving (Eq,Ord,Show)

data Along =
   AJust [Id]
 | ANothing
  deriving (Eq,Ord,Show)

data Type =
   Tensor Type Type
 | Par Type Type
 | One
 | Bot
 | Plus Type Type
 | Choice Type Type
 | Top
 | Zero
 | Lollipop Type Type
 | AliasTy Id TyList
 | TyId Id
 | Bang Type
 | Quest Type
 | Neg Type
 | Forall Id Type
 | Exists Id Type
 | Mu Id Type
  deriving (Eq,Ord,Show)

data Choice =
   Fst
 | Snd
  deriving (Eq,Ord,Show)

data Seq =
   Ax Id Id
 | Cut MBinder Seq MBinder Seq
 | ParSeq Id Id Seq Id Seq
 | TensorSeq Id Id Id Seq
 | ChoiceSeq Id Choice Id Seq
 | Case Id Id Seq Id Seq
 | Bottom Id
 | Unit Id Seq
 | Crash Id Along
 | Pack Id Id Type Seq
 | Unpack Id Id Id Seq
 | Offer Id Id Seq
 | Demand Id Id Seq
 | Ignore Id Seq
 | Alias Id Id Seq
 | Fold Id Id Seq
 | Unfold Id Id Seq
 | Refer Id [Type] [Id]
 | Hole
  deriving (Eq,Ord,Show)

