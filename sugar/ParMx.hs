{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParMx where
import AbsMx
import LexMx
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.18.10

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn26 :: (Id) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (Id)
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (Prog) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (Prog)
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (Deriv) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (Deriv)
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: ([Deriv]) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> ([Deriv])
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: ([Binder]) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> ([Binder])
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: ([Id]) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> ([Id])
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: ([TyVar]) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> ([TyVar])
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: ([Alias]) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> ([Alias])
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: ([Type]) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> ([Type])
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (Alias) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (Alias)
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (IdList) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (IdList)
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (TyVar) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (TyVar)
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (Binder) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (Binder)
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: (MBinder) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> (MBinder)
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (MDerivName) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (MDerivName)
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (Along) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (Along)
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (Type) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (Type)
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (Type) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (Type)
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: (Type) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> (Type)
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: (Type) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> (Type)
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: (Type) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> (Type)
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (Type) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (Type)
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (Choice) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (Choice)
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: (Seq) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> (Seq)
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x00\x00\x00\x00\x85\x01\x85\x01\x00\x00\x00\x00\x04\x00\x89\x01\x84\x01\x84\x01\x84\x01\x84\x01\x82\x01\x86\x01\x07\x00\x07\x00\x07\x00\x07\x00\x04\x00\x07\x00\x32\x00\x28\x00\x83\x01\x00\x00\x81\x01\x80\x01\x7f\x01\x00\x00\x7f\x01\x2c\x00\x7f\x01\x7f\x01\xfe\xff\x7f\x01\x7e\x01\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x1e\x01\x7e\x01\x07\x00\x04\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x07\x00\x00\x00\x7e\x01\x77\x01\x7d\x01\x7d\x01\x7d\x01\x7c\x01\xff\xff\x7c\x01\xfd\xff\x7c\x01\x7b\x01\x7a\x01\x79\x01\x78\x01\x75\x01\x71\x01\x74\x01\x00\x00\x74\x01\x73\x01\x6f\x01\x6f\x01\x6e\x01\x68\x01\x76\x01\xe4\x00\x23\x00\x72\x01\x67\x01\x67\x01\x70\x01\x6d\x01\x65\x01\x64\x01\x17\x01\x17\x01\x63\x01\x00\x00\x6c\x01\x6b\x01\x62\x01\x5e\x01\x00\x00\x5d\x01\x5d\x01\x04\x00\x5d\x01\x00\x00\x04\x00\x04\x00\x61\x01\x00\x00\x07\x00\x07\x00\x07\x00\x07\x00\x6a\x01\x69\x01\x66\x01\x07\x00\x00\x00\x00\x00\x60\x01\x00\x00\x5c\x01\xa0\x00\x5f\x01\x5b\x01\x59\x01\x57\x01\x57\x01\x25\x01\x58\x01\x56\x01\x00\x00\x04\x00\x55\x01\x54\x01\x0f\x01\x00\x00\x28\x00\x53\x01\x53\x01\x12\x00\x53\x01\x53\x01\x00\x00\x00\x00\x04\x00\x04\x00\x04\x00\x06\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x52\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5a\x01\x00\x00\x28\x00\x04\x00\x00\x00\x00\x00\x00\x00\x51\x01\x50\x01\x4f\x01\x27\x01\x27\x01\x27\x01\x27\x01\x27\x01\x4e\x01\x4d\x01\x00\x00\x20\x01\x28\x00\x18\x01\x4c\x01\x1f\x01\x1f\x01\x4b\x01\x4a\x01\x28\x00\x1b\x01\x49\x01\x48\x01\x47\x01\x46\x01\x45\x01\x04\x00\x14\x01\x28\x00\x00\x00\x00\x00\x00\x00\x44\x01\x43\x01\x28\x00\x28\x00\x28\x00\x28\x00\x28\x00\x42\x01\x00\x00\x28\x00\x12\x01\x41\x01\x40\x01\x00\x00\x28\x00\x3f\x01\x3e\x01\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x28\x00\x00\x00\x00\x00\x00\x00\x11\x01\x28\x00\x3d\x01\x16\x01\x09\x01\x3b\x01\x28\x00\x00\x00\x06\x01\x3a\x01\x05\x01\x00\x00\x28\x00\x04\x01\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x8b\x00\x21\x01\x10\x01\x0c\x01\x1d\x01\x3c\x01\x39\x01\x83\x00\x38\x01\x0d\x01\xf9\x00\xb2\x00\xb0\x00\x37\x01\x2b\x01\xf0\x00\xf8\x00\x17\x00\xd8\x00\xcb\x00\xe1\x00\x23\x01\x54\x00\x00\x00\x00\x00\x00\x00\x00\x00\x36\x01\x00\x00\x35\x01\x00\x00\x34\x01\x33\x01\x32\x01\x31\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf7\x00\xbc\x00\x00\x00\x00\x00\xf5\x00\x00\x00\x00\x00\xf4\x00\x00\x00\x00\x00\x00\x00\x30\x01\x2f\x01\x2e\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x01\x00\x00\x2d\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x01\x00\x00\x00\x00\x2c\x01\x00\x00\x00\x00\x22\x01\xef\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x01\x00\x00\x00\x00\xd1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x6a\x00\x1a\x01\x6d\x00\x0a\x01\x00\x00\xb5\x00\xaf\x00\x00\x00\x00\x00\xd5\x00\x57\x00\xee\x00\xec\x00\x00\x00\x00\x00\x00\x00\xd2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x2a\x01\xa7\x00\x00\x00\x00\x00\x29\x01\x00\x00\x67\x00\x00\x00\x00\x00\x00\x00\x00\x00\x52\x00\x28\x01\x26\x01\x56\x00\x24\x01\x02\x01\x00\x00\x00\x00\xa6\x00\x99\x00\x90\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\xff\x00\x00\x4f\x00\x8a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfb\x00\xd9\x00\xd6\x00\x91\x00\x8f\x00\x00\x00\x00\x00\x00\x00\x8e\x00\x4e\x00\x00\x00\x00\x00\x19\x01\x8d\x00\x00\x00\x00\x00\x4d\x00\x73\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x74\x00\x72\x00\x4b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4a\x00\x49\x00\x48\x00\x42\x00\x3a\x00\x00\x00\x00\x00\x2f\x00\x63\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x15\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x71\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xdc\xff\xde\xff\xde\xff\xe3\xff\x00\x00\xde\xff\xdc\xff\xda\xff\x00\x00\xd6\xff\x00\x00\x00\x00\x00\x00\xcf\xff\xcd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe8\xff\xae\xff\x00\x00\x00\x00\xa2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb6\xff\xb5\xff\xc5\xff\xbe\xff\xca\xff\xb7\xff\x00\x00\x00\x00\x00\x00\xc6\xff\xc9\xff\x00\x00\xc7\xff\xc8\xff\x00\x00\xb8\xff\x00\x00\xbc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd1\xff\x00\x00\x00\x00\x00\x00\xd4\xff\x00\x00\xd6\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd9\xff\x00\x00\x00\x00\xe0\xff\x00\x00\x00\x00\xe2\xff\xe5\xff\x00\x00\xe3\xff\x00\x00\x00\x00\xde\xff\xe7\xff\x00\x00\xd4\xff\x00\x00\x00\x00\xde\xff\xe3\xff\x00\x00\xda\xff\xd6\xff\xd5\xff\x00\x00\x00\x00\x00\x00\xce\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc2\xff\xc3\xff\x00\x00\xc4\xff\x00\x00\x00\x00\x00\x00\x00\x00\xcd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb4\xff\xda\xff\x00\x00\x00\x00\x00\x00\xac\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc1\xff\xbd\xff\x00\x00\x00\x00\x00\x00\xc0\xff\xbf\xff\xcb\xff\xcc\xff\xd0\xff\xd2\xff\xd3\xff\x00\x00\xd8\xff\xdf\xff\xe1\xff\xe4\xff\xdd\xff\xcf\xff\xdb\xff\x00\x00\x00\x00\xb9\xff\xbb\xff\xba\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd7\xff\xe6\xff\xa9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xad\xff\x00\x00\x00\x00\x00\x00\x00\x00\xa3\xff\x00\x00\x00\x00\x00\x00\x00\x00\xa4\xff\xa5\xff\xa8\xff\xa6\xff\xb0\xff\x00\x00\x00\x00\xaa\xff\xab\xff\xb1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb3\xff\x00\x00\x00\x00\x00\x00\xb2\xff\x00\x00\x00\x00\xaf\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x04\x00\x06\x00\x0e\x00\x01\x00\x07\x00\x03\x00\x01\x00\x00\x00\x03\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x0d\x00\x0f\x00\x0c\x00\x0d\x00\x00\x00\x12\x00\x00\x00\x14\x00\x12\x00\x00\x00\x14\x00\x18\x00\x02\x00\x03\x00\x18\x00\x17\x00\x06\x00\x17\x00\x1f\x00\x17\x00\x17\x00\x22\x00\x10\x00\x11\x00\x12\x00\x00\x00\x19\x00\x17\x00\x29\x00\x31\x00\x00\x00\x1e\x00\x17\x00\x20\x00\x35\x00\x37\x00\x23\x00\x37\x00\x31\x00\x34\x00\x35\x00\x00\x00\x34\x00\x35\x00\x15\x00\x2c\x00\x17\x00\x2e\x00\x17\x00\x00\x00\x1b\x00\x1c\x00\x1d\x00\x17\x00\x35\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x28\x00\x17\x00\x00\x00\x2b\x00\x00\x00\x23\x00\x00\x00\x00\x00\x35\x00\x17\x00\x37\x00\x2f\x00\x30\x00\x35\x00\x2c\x00\x17\x00\x17\x00\x17\x00\x17\x00\x00\x00\x17\x00\x17\x00\x17\x00\x00\x00\x11\x00\x17\x00\x00\x00\x17\x00\x16\x00\x00\x00\x04\x00\x08\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x0c\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x08\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x00\x00\x08\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x00\x00\x11\x00\x00\x00\x13\x00\x0d\x00\x00\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x0d\x00\x0c\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x02\x00\x03\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x09\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x10\x00\x11\x00\x12\x00\x13\x00\x11\x00\x15\x00\x10\x00\x11\x00\x12\x00\x13\x00\x00\x00\x15\x00\x00\x00\x00\x00\x00\x00\x10\x00\x11\x00\x12\x00\x00\x00\x00\x00\x15\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00\x10\x00\x11\x00\x10\x00\x11\x00\x10\x00\x11\x00\x00\x00\x00\x00\x0b\x00\x11\x00\x11\x00\x04\x00\x11\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x0c\x00\x04\x00\x2d\x00\x02\x00\x03\x00\x0a\x00\x0a\x00\x06\x00\x0a\x00\x0c\x00\x00\x00\x00\x00\x37\x00\x00\x00\x00\x00\x05\x00\x05\x00\x02\x00\x05\x00\x05\x00\x02\x00\x00\x00\x07\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x33\x00\x16\x00\x0f\x00\x35\x00\x33\x00\x27\x00\x26\x00\x30\x00\x07\x00\x09\x00\x06\x00\x09\x00\x09\x00\x0e\x00\x35\x00\x35\x00\x09\x00\x35\x00\x09\x00\xff\xff\x0f\x00\x0f\x00\x37\x00\x2a\x00\x35\x00\xff\xff\xff\xff\x09\x00\x35\x00\x35\x00\x16\x00\xff\xff\xff\xff\xff\xff\x0f\x00\x0f\x00\x35\x00\x09\x00\xff\xff\x11\x00\xff\xff\x11\x00\x13\x00\x11\x00\xff\xff\x05\x00\x08\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x15\x00\x11\x00\x0b\x00\x25\x00\x1a\x00\x0b\x00\x0b\x00\x25\x00\x16\x00\x08\x00\x0e\x00\x08\x00\x0f\x00\x0f\x00\x21\x00\x08\x00\x0e\x00\x25\x00\x0a\x00\xff\xff\xff\xff\xff\xff\x30\x00\x0e\x00\xff\xff\x35\x00\x31\x00\xff\xff\x35\x00\x35\x00\xff\xff\xff\xff\xff\xff\x2d\x00\x10\x00\x35\x00\xff\xff\x32\x00\xff\xff\xff\xff\x15\x00\xff\xff\x35\x00\xff\xff\xff\xff\x37\x00\xff\xff\x37\x00\x37\x00\x1a\x00\xff\xff\xff\xff\x35\x00\xff\xff\xff\xff\x37\x00\xff\xff\x35\x00\xff\xff\xff\xff\x37\x00\x37\x00\xff\xff\x35\x00\xff\xff\x35\x00\x37\x00\x35\x00\x37\x00\x35\x00\x37\x00\x2d\x00\x37\x00\x35\x00\x35\x00\x35\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x6b\x00\x77\x00\x69\x00\x9e\x00\x2c\x00\x6c\x00\x2d\x00\x2c\x00\x19\x00\x2d\x00\x19\x00\x69\x00\x19\x00\x19\x00\xe8\x00\x2e\x00\x2f\x00\x83\x00\x2e\x00\x2f\x00\x19\x00\x30\x00\x26\x00\x31\x00\x30\x00\x19\x00\x31\x00\x32\x00\x53\x00\x9a\x00\x32\x00\xec\x00\x55\x00\xe9\x00\x37\x00\xe3\x00\xdc\x00\x38\x00\x27\x00\x28\x00\x3a\x00\x19\x00\xa8\x00\xdd\x00\x39\x00\x6a\x00\x19\x00\xa9\x00\xde\x00\xaa\x00\x19\x00\xff\xff\x25\x00\xff\xff\x6a\x00\x33\x00\x19\x00\x19\x00\x33\x00\x19\x00\x1c\x00\x26\x00\x1d\x00\xab\x00\xe1\x00\x19\x00\x1e\x00\x1f\x00\x20\x00\xd3\x00\x19\x00\x19\x00\x19\x00\x19\x00\x19\x00\x21\x00\x19\x00\x19\x00\x19\x00\x22\x00\xd5\x00\x19\x00\x23\x00\x19\x00\x25\x00\xa5\x00\x26\x00\x19\x00\xd6\x00\xff\xff\x7a\x00\x7b\x00\x19\x00\x26\x00\xd7\x00\xd8\x00\xd9\x00\xc2\x00\x41\x00\xcb\x00\xb4\x00\xc1\x00\x26\x00\x91\x00\xad\x00\x43\x00\x1a\x00\xa6\x00\x26\x00\x99\x00\xb1\x00\xd2\x00\xe4\x00\xc3\x00\xca\x00\x26\x00\x97\x00\x52\x00\x27\x00\x28\x00\x29\x00\x33\x00\x4c\x00\x35\x00\x27\x00\x28\x00\x29\x00\x33\x00\x4c\x00\x35\x00\x26\x00\x27\x00\x28\x00\x29\x00\x33\x00\xc4\x00\x35\x00\x26\x00\x4b\x00\x57\x00\xce\x00\xb5\x00\xb8\x00\x26\x00\xb9\x00\x58\x00\x27\x00\x28\x00\x29\x00\x33\x00\x4c\x00\x35\x00\x26\x00\x27\x00\x28\x00\x29\x00\x33\x00\xc0\x00\x35\x00\x27\x00\x28\x00\x29\x00\x33\x00\xa0\x00\x35\x00\x26\x00\x41\x00\x87\x00\x27\x00\x28\x00\x29\x00\x33\x00\xa1\x00\x35\x00\x26\x00\x41\x00\x88\x00\x43\x00\x89\x00\x81\x00\x26\x00\x27\x00\x28\x00\x29\x00\x33\x00\xa2\x00\x35\x00\x26\x00\x42\x00\x44\x00\x27\x00\x28\x00\x29\x00\x33\x00\x94\x00\x35\x00\x27\x00\x28\x00\x29\x00\x33\x00\x95\x00\x35\x00\x26\x00\x27\x00\x28\x00\x29\x00\x33\x00\x72\x00\x35\x00\x26\x00\x53\x00\x59\x00\x26\x00\xba\x00\x55\x00\x26\x00\xbb\x00\x5a\x00\x27\x00\x28\x00\x29\x00\x33\x00\x34\x00\x35\x00\x26\x00\x27\x00\x28\x00\x29\x00\x8b\x00\x92\x00\x35\x00\x27\x00\x28\x00\x29\x00\x39\x00\x26\x00\x35\x00\x26\x00\x45\x00\x26\x00\x27\x00\x28\x00\x29\x00\x26\x00\x26\x00\x2a\x00\x26\x00\x26\x00\x45\x00\x5d\x00\xbc\x00\x8f\x00\x28\x00\x90\x00\x28\x00\x3c\x00\x28\x00\xa3\x00\x5b\x00\x46\x00\x70\x00\x71\x00\x5c\x00\x73\x00\x3b\x00\x47\x00\x47\x00\x43\x00\x47\x00\x5d\x00\x52\x00\x51\x00\x4b\x00\x53\x00\x54\x00\x96\x00\x63\x00\x55\x00\x48\x00\x52\x00\x4f\x00\x4f\x00\xff\xff\x4f\x00\x4f\x00\xcf\x00\x98\x00\x6b\x00\x67\x00\x50\x00\x56\x00\xa4\x00\x6c\x00\xab\x00\x55\x00\xac\x00\x7e\x00\x82\x00\x5a\x00\x62\x00\x66\x00\x6c\x00\x6d\x00\x6e\x00\x74\x00\x75\x00\x77\x00\x78\x00\x7b\x00\x7c\x00\xee\x00\xeb\x00\x23\x00\x3d\x00\x19\x00\xe7\x00\xe8\x00\xb4\x00\xaf\x00\x4d\x00\x49\x00\x4e\x00\xec\x00\xe6\x00\x3f\x00\x19\x00\x19\x00\xe1\x00\x19\x00\xd2\x00\x00\x00\xe3\x00\xe0\x00\xff\xff\x81\x00\x19\x00\x00\x00\x00\x00\xcd\x00\x19\x00\x19\x00\xd1\x00\x00\x00\x00\x00\x00\x00\xce\x00\xb3\x00\x19\x00\xb0\x00\x00\x00\xb8\x00\x00\x00\xbf\x00\xbe\x00\xa0\x00\x00\x00\x8b\x00\x9c\x00\xd5\x00\xdb\x00\xdc\x00\xc6\x00\xc7\x00\xc8\x00\xc9\x00\xca\x00\x41\x00\x86\x00\x8d\x00\xb7\x00\x3f\x00\x8e\x00\x8f\x00\xc0\x00\x94\x00\x60\x00\x65\x00\x61\x00\x9e\x00\x5f\x00\x8a\x00\x62\x00\x65\x00\x85\x00\x70\x00\x00\x00\x00\x00\x00\x00\xb1\x00\x66\x00\x00\x00\x19\x00\x80\x00\x00\x00\x19\x00\x19\x00\x00\x00\x00\x00\x00\x00\x4b\x00\x7e\x00\x19\x00\x00\x00\x9d\x00\x00\x00\x00\x00\x41\x00\x00\x00\x19\x00\x00\x00\x00\x00\xff\xff\x00\x00\xff\xff\xff\xff\x3f\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\xff\xff\x00\x00\x19\x00\x00\x00\x00\x00\xff\xff\xff\xff\x00\x00\x19\x00\x00\x00\x19\x00\xff\xff\x19\x00\xff\xff\x19\x00\xff\xff\x4b\x00\xff\xff\x19\x00\x19\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (23, 93) [
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93)
	]

happy_n_terms = 56 :: Int
happy_n_nonterms = 24 :: Int

happyReduce_23 = happySpecReduce_1  0# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (Id (mkPosToken happy_var_1)
	)}

happyReduce_24 = happySpecReduce_2  1# happyReduction_24
happyReduction_24 happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_2 of { happy_var_2 -> 
	happyIn27
		 (Prog (reverse happy_var_1) happy_var_2
	)}}

happyReduce_25 = happyReduce 5# 2# happyReduction_25
happyReduction_25 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut32 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	case happyOut40 happy_x_4 of { happy_var_4 -> 
	case happyOut49 happy_x_5 of { happy_var_5 -> 
	happyIn28
		 (Deriv (reverse happy_var_1) happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest}}}}

happyReduce_26 = happySpecReduce_1  3# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 ((:[]) happy_var_1
	)}

happyReduce_27 = happySpecReduce_3  3# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_3 of { happy_var_3 -> 
	happyIn29
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_28 = happySpecReduce_0  4# happyReduction_28
happyReduction_28  =  happyIn30
		 ([]
	)

happyReduce_29 = happySpecReduce_1  4# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((:[]) happy_var_1
	)}

happyReduce_30 = happySpecReduce_3  4# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn30
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_31 = happySpecReduce_1  5# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((:[]) happy_var_1
	)}

happyReduce_32 = happySpecReduce_3  5# happyReduction_32
happyReduction_32 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn31
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_33 = happySpecReduce_0  6# happyReduction_33
happyReduction_33  =  happyIn32
		 ([]
	)

happyReduce_34 = happySpecReduce_3  6# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_2 of { happy_var_2 -> 
	happyIn32
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_35 = happySpecReduce_0  7# happyReduction_35
happyReduction_35  =  happyIn33
		 ([]
	)

happyReduce_36 = happySpecReduce_3  7# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn33
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_37 = happySpecReduce_0  8# happyReduction_37
happyReduction_37  =  happyIn34
		 ([]
	)

happyReduce_38 = happySpecReduce_1  8# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 ((:[]) happy_var_1
	)}

happyReduce_39 = happySpecReduce_3  8# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	case happyOut34 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_40 = happyReduce 5# 9# happyReduction_40
happyReduction_40 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut36 happy_x_3 of { happy_var_3 -> 
	case happyOut46 happy_x_5 of { happy_var_5 -> 
	happyIn35
		 (TyAlias happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_41 = happySpecReduce_0  10# happyReduction_41
happyReduction_41  =  happyIn36
		 (ILNil
	)

happyReduce_42 = happySpecReduce_2  10# happyReduction_42
happyReduction_42 happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_2 of { happy_var_2 -> 
	happyIn36
		 (ILCons happy_var_1 happy_var_2
	)}}

happyReduce_43 = happySpecReduce_1  11# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (TyVar happy_var_1
	)}

happyReduce_44 = happySpecReduce_3  12# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut46 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 (Binder happy_var_1 happy_var_3
	)}}

happyReduce_45 = happySpecReduce_3  13# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut46 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 (BJust happy_var_1 happy_var_3
	)}}

happyReduce_46 = happySpecReduce_1  13# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (BNothing happy_var_1
	)}

happyReduce_47 = happySpecReduce_3  14# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_2 of { happy_var_2 -> 
	happyIn40
		 (DerivName happy_var_2
	)}

happyReduce_48 = happySpecReduce_0  14# happyReduction_48
happyReduction_48  =  happyIn40
		 (NoDerivName
	)

happyReduce_49 = happySpecReduce_2  15# happyReduction_49
happyReduction_49 happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn41
		 (AJust happy_var_2
	)}

happyReduce_50 = happySpecReduce_0  15# happyReduction_50
happyReduction_50  =  happyIn41
		 (ANothing
	)

happyReduce_51 = happySpecReduce_3  16# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 (Tensor happy_var_1 happy_var_3
	)}}

happyReduce_52 = happySpecReduce_3  16# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 (Par happy_var_1 happy_var_3
	)}}

happyReduce_53 = happySpecReduce_1  16# happyReduction_53
happyReduction_53 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (happy_var_1
	)}

happyReduce_54 = happySpecReduce_1  17# happyReduction_54
happyReduction_54 happy_x_1
	 =  happyIn43
		 (One
	)

happyReduce_55 = happySpecReduce_1  17# happyReduction_55
happyReduction_55 happy_x_1
	 =  happyIn43
		 (Bot
	)

happyReduce_56 = happySpecReduce_1  17# happyReduction_56
happyReduction_56 happy_x_1
	 =  happyIn43
		 (Top
	)

happyReduce_57 = happySpecReduce_1  17# happyReduction_57
happyReduction_57 happy_x_1
	 =  happyIn43
		 (Zero
	)

happyReduce_58 = happySpecReduce_1  17# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (TyId happy_var_1
	)}

happyReduce_59 = happySpecReduce_2  17# happyReduction_59
happyReduction_59 happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn43
		 (Bang happy_var_2
	)}

happyReduce_60 = happySpecReduce_2  17# happyReduction_60
happyReduction_60 happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn43
		 (Quest happy_var_2
	)}

happyReduce_61 = happySpecReduce_2  17# happyReduction_61
happyReduction_61 happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn43
		 (Neg happy_var_2
	)}

happyReduce_62 = happySpecReduce_3  17# happyReduction_62
happyReduction_62 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_2 of { happy_var_2 -> 
	happyIn43
		 (happy_var_2
	)}

happyReduce_63 = happySpecReduce_3  18# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (Plus happy_var_1 happy_var_3
	)}}

happyReduce_64 = happySpecReduce_3  18# happyReduction_64
happyReduction_64 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (Choice happy_var_1 happy_var_3
	)}}

happyReduce_65 = happySpecReduce_1  18# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (happy_var_1
	)}

happyReduce_66 = happySpecReduce_3  19# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 (Lollipop happy_var_1 happy_var_3
	)}}

happyReduce_67 = happySpecReduce_1  19# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (happy_var_1
	)}

happyReduce_68 = happyReduce 4# 20# happyReduction_68
happyReduction_68 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut46 happy_x_4 of { happy_var_4 -> 
	happyIn46
		 (Forall happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_69 = happyReduce 4# 20# happyReduction_69
happyReduction_69 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut46 happy_x_4 of { happy_var_4 -> 
	happyIn46
		 (Exists happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_70 = happyReduce 4# 20# happyReduction_70
happyReduction_70 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut46 happy_x_4 of { happy_var_4 -> 
	happyIn46
		 (Mu happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_71 = happySpecReduce_1  20# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (happy_var_1
	)}

happyReduce_72 = happySpecReduce_1  21# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (happy_var_1
	)}

happyReduce_73 = happySpecReduce_1  22# happyReduction_73
happyReduction_73 happy_x_1
	 =  happyIn48
		 (Fst
	)

happyReduce_74 = happySpecReduce_1  22# happyReduction_74
happyReduction_74 happy_x_1
	 =  happyIn48
		 (Snd
	)

happyReduce_75 = happySpecReduce_3  23# happyReduction_75
happyReduction_75 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (Ax happy_var_1 happy_var_3
	)}}

happyReduce_76 = happyReduce 10# 23# happyReduction_76
happyReduction_76 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut39 happy_x_3 of { happy_var_3 -> 
	case happyOut49 happy_x_5 of { happy_var_5 -> 
	case happyOut39 happy_x_7 of { happy_var_7 -> 
	case happyOut49 happy_x_9 of { happy_var_9 -> 
	happyIn49
		 (Cut happy_var_3 happy_var_5 happy_var_7 happy_var_9
	) `HappyStk` happyRest}}}}

happyReduce_77 = happyReduce 12# 23# happyReduction_77
happyReduction_77 (happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_3 of { happy_var_3 -> 
	case happyOut26 happy_x_5 of { happy_var_5 -> 
	case happyOut49 happy_x_7 of { happy_var_7 -> 
	case happyOut26 happy_x_9 of { happy_var_9 -> 
	case happyOut49 happy_x_11 of { happy_var_11 -> 
	happyIn49
		 (ParSeq happy_var_3 happy_var_5 happy_var_7 happy_var_9 happy_var_11
	) `HappyStk` happyRest}}}}}

happyReduce_78 = happyReduce 8# 23# happyReduction_78
happyReduction_78 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_4 of { happy_var_4 -> 
	case happyOut26 happy_x_6 of { happy_var_6 -> 
	case happyOut49 happy_x_8 of { happy_var_8 -> 
	happyIn49
		 (TensorSeq happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_79 = happyReduce 7# 23# happyReduction_79
happyReduction_79 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut48 happy_x_4 of { happy_var_4 -> 
	case happyOut26 happy_x_5 of { happy_var_5 -> 
	case happyOut49 happy_x_7 of { happy_var_7 -> 
	happyIn49
		 (ChoiceSeq happy_var_2 happy_var_4 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}}

happyReduce_80 = happyReduce 14# 23# happyReduction_80
happyReduction_80 (happy_x_14 `HappyStk`
	happy_x_13 `HappyStk`
	happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_6 of { happy_var_6 -> 
	case happyOut49 happy_x_8 of { happy_var_8 -> 
	case happyOut26 happy_x_11 of { happy_var_11 -> 
	case happyOut49 happy_x_13 of { happy_var_13 -> 
	happyIn49
		 (Case happy_var_2 happy_var_6 happy_var_8 happy_var_11 happy_var_13
	) `HappyStk` happyRest}}}}}

happyReduce_81 = happySpecReduce_1  23# happyReduction_81
happyReduction_81 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 (Bottom happy_var_1
	)}

happyReduce_82 = happyReduce 6# 23# happyReduction_82
happyReduction_82 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_4 of { happy_var_4 -> 
	case happyOut49 happy_x_6 of { happy_var_6 -> 
	happyIn49
		 (Unit happy_var_4 happy_var_6
	) `HappyStk` happyRest}}

happyReduce_83 = happySpecReduce_3  23# happyReduction_83
happyReduction_83 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut41 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (Crash happy_var_2 happy_var_3
	)}}

happyReduce_84 = happyReduce 8# 23# happyReduction_84
happyReduction_84 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_4 of { happy_var_4 -> 
	case happyOut46 happy_x_6 of { happy_var_6 -> 
	case happyOut49 happy_x_8 of { happy_var_8 -> 
	happyIn49
		 (Pack happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_85 = happyReduce 8# 23# happyReduction_85
happyReduction_85 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_4 of { happy_var_4 -> 
	case happyOut26 happy_x_6 of { happy_var_6 -> 
	case happyOut49 happy_x_8 of { happy_var_8 -> 
	happyIn49
		 (Unpack happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_86 = happyReduce 6# 23# happyReduction_86
happyReduction_86 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_4 of { happy_var_4 -> 
	case happyOut49 happy_x_6 of { happy_var_6 -> 
	happyIn49
		 (Offer happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_87 = happyReduce 7# 23# happyReduction_87
happyReduction_87 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_5 of { happy_var_5 -> 
	case happyOut49 happy_x_7 of { happy_var_7 -> 
	happyIn49
		 (Demand happy_var_2 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_88 = happyReduce 4# 23# happyReduction_88
happyReduction_88 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut49 happy_x_4 of { happy_var_4 -> 
	happyIn49
		 (Ignore happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_89 = happyReduce 7# 23# happyReduction_89
happyReduction_89 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_5 of { happy_var_5 -> 
	case happyOut49 happy_x_7 of { happy_var_7 -> 
	happyIn49
		 (Alias happy_var_2 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_90 = happyReduce 7# 23# happyReduction_90
happyReduction_90 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_5 of { happy_var_5 -> 
	case happyOut49 happy_x_7 of { happy_var_7 -> 
	happyIn49
		 (Fold happy_var_2 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_91 = happyReduce 7# 23# happyReduction_91
happyReduction_91 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_5 of { happy_var_5 -> 
	case happyOut49 happy_x_7 of { happy_var_7 -> 
	happyIn49
		 (Unfold happy_var_2 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_92 = happyReduce 7# 23# happyReduction_92
happyReduction_92 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut34 happy_x_4 of { happy_var_4 -> 
	case happyOut31 happy_x_6 of { happy_var_6 -> 
	happyIn49
		 (Refer happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_93 = happySpecReduce_1  23# happyReduction_93
happyReduction_93 happy_x_1
	 =  happyIn49
		 (Hole
	)

happyNewToken action sts stk [] =
	happyDoAction 55# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TS _ 41) -> cont 41#;
	PT _ (TS _ 42) -> cont 42#;
	PT _ (TS _ 43) -> cont 43#;
	PT _ (TS _ 44) -> cont 44#;
	PT _ (TS _ 45) -> cont 45#;
	PT _ (TS _ 46) -> cont 46#;
	PT _ (TS _ 47) -> cont 47#;
	PT _ (TS _ 48) -> cont 48#;
	PT _ (TS _ 49) -> cont 49#;
	PT _ (TS _ 50) -> cont 50#;
	PT _ (TS _ 51) -> cont 51#;
	PT _ (TS _ 52) -> cont 52#;
	PT _ (T_Id _) -> cont 53#;
	_ -> cont 54#;
	_ -> happyError' (tk:tks)
	}

happyError_ 55# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pProg tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut27 x))

pDeriv tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut28 x))

pListDeriv tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut29 x))

pListBinder tks = happySomeParser where
  happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut30 x))

pListId tks = happySomeParser where
  happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut31 x))

pListTyVar tks = happySomeParser where
  happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut32 x))

pListAlias tks = happySomeParser where
  happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut33 x))

pListType tks = happySomeParser where
  happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut34 x))

pAlias tks = happySomeParser where
  happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut35 x))

pIdList tks = happySomeParser where
  happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut36 x))

pTyVar tks = happySomeParser where
  happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut37 x))

pBinder tks = happySomeParser where
  happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut38 x))

pMBinder tks = happySomeParser where
  happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (happyOut39 x))

pMDerivName tks = happySomeParser where
  happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (happyOut40 x))

pAlong tks = happySomeParser where
  happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (happyOut41 x))

pType4 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (happyOut42 x))

pType5 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (happyOut43 x))

pType3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (happyOut44 x))

pType1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (happyOut45 x))

pType tks = happySomeParser where
  happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (happyOut46 x))

pType2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (happyOut47 x))

pChoice tks = happySomeParser where
  happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (happyOut48 x))

pSeq tks = happySomeParser where
  happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (happyOut49 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4























# 8 "<command-line>" 2
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n Happy_GHC_Exts.<# (0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

				     (happyReduceArr Happy_Data_Array.! rule) i tk st
				     where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where (new_state) = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where (off)    = indexShortOffAddr happyActOffsets st
         (off_i)  = (off Happy_GHC_Exts.+# i)
	 check  = if (off_i Happy_GHC_Exts.>=# (0# :: Happy_GHC_Exts.Int#))
			then (indexShortOffAddr happyCheck off_i Happy_GHC_Exts.==#  i)
			else False
         (action)
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st

{-# LINE 130 "templates/GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
	Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 163 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let (i) = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             (off) = indexShortOffAddr happyGotoOffsets st1
             (off_i) = (off Happy_GHC_Exts.+# nt)
             (new_state) = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where (off) = indexShortOffAddr happyGotoOffsets st
         (off_i) = (off Happy_GHC_Exts.+# nt)
         (new_state) = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
