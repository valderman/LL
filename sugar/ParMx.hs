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
happyIn18 :: (Ident) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Ident)
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (Prog) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (Prog)
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: ([Binder]) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> ([Binder])
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: ([Ident]) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> ([Ident])
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: ([Alias]) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> ([Alias])
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (Alias) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (Alias)
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (Binder) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (Binder)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (MBinder) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (MBinder)
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (Type) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (Type)
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (Type) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (Type)
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (Type) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (Type)
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (Type) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (Type)
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: (Type) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> (Type)
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (Type) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (Type)
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (Choice) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (Choice)
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (Seq) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (Seq)
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x31\x01\x31\x01\x00\x00\x35\x01\x30\x01\x30\x01\x07\x00\x07\x00\x07\x00\x04\x00\x04\x00\x04\x00\xc2\x00\x0b\x00\x30\x01\x00\x00\x2e\x01\x2d\x01\x00\x00\x2c\x01\x58\x00\x2c\x01\x2c\x01\x05\x00\x2c\x01\x2b\x01\x00\x00\x00\x00\x00\x00\xfe\xff\x00\x00\xf4\x00\x00\x00\x32\x01\x29\x01\x07\x00\x04\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x2a\x01\x2a\x01\x07\x00\xf9\xff\x00\x00\x27\x01\xff\xff\x27\x01\xfc\xff\x28\x01\x26\x01\x24\x01\x23\x01\x23\x01\x21\x01\xb7\x00\x25\x01\x20\x01\x20\x01\x22\x01\x1a\x01\xe3\xff\x1c\x01\x1f\x01\x17\x01\x17\x01\x1b\x01\x04\x00\x04\x00\x07\x00\x07\x00\x07\x00\x07\x00\x04\x00\x00\x00\x1e\x01\x1d\x01\x00\x00\xe8\x00\x00\x00\x19\x01\xc0\x00\x16\x01\x14\x01\x18\x01\x13\x01\x12\x01\x12\x01\x0f\x01\x0e\x01\x00\x00\x0d\x01\x15\x01\xe7\x00\x0c\x01\x0b\x00\x0c\x01\xa4\x00\x0c\x01\xf0\xff\x0c\x01\x0b\x00\x00\x00\x04\x00\x04\x00\x11\x01\xfe\xff\xfe\xff\x00\x00\x00\x00\x10\x01\x10\x01\x04\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00\x10\x01\x10\x01\x10\x01\x00\x00\x0b\x01\x0a\x01\x09\x01\x07\x01\x06\x01\x06\x01\x05\x01\x00\x00\x00\x00\xf3\x00\x0b\x00\xee\x00\xea\x00\x08\x01\x04\x01\x0b\x00\x03\x01\xf5\x00\xe6\x00\xf2\x00\x04\x00\xe5\x00\xed\x00\x02\x00\x0b\x00\xeb\x00\x0b\x00\x0b\x00\x00\x00\x0b\x00\xda\x00\x02\x01\x0b\x00\x01\x01\x00\x01\x00\x00\x00\x00\x0b\x00\x00\x00\x0b\x00\x0b\x00\x00\x00\x00\x00\x00\x00\xd7\x00\x0b\x00\xf6\x00\xe1\x00\xd6\x00\xff\x00\x0b\x00\x00\x00\xce\x00\xfe\x00\xd5\x00\x00\x00\x0b\x00\xd1\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xbb\x00\x6b\x00\x94\x00\xfd\x00\xfb\x00\xa7\x00\xec\x00\xd2\x00\xde\x00\xc1\x00\xba\x00\xac\x00\xa6\x00\xf1\x00\x49\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\x00\x00\x00\xfa\x00\xf9\x00\x93\x00\x7f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\x00\x98\x00\x00\x00\x00\x00\xdc\x00\x00\x00\x00\x00\xf8\x00\xf7\x00\xd0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\x00\xe9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xef\x00\x00\x00\x00\x00\x57\x00\x80\x00\x00\x00\x92\x00\x84\x00\xcd\x00\x15\x00\xcf\x00\xcc\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe2\x00\xe4\x00\x00\x00\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6c\x00\x46\x00\xbe\x00\x00\x00\xbd\x00\x4c\x00\xab\x00\x45\x00\x00\x00\x70\x00\x6a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaa\x00\x00\x00\xa9\x00\xa8\x00\x00\x00\x00\x00\x00\x00\x96\x00\x43\x00\x00\x00\x95\x00\x00\x00\x00\x00\x42\x00\x00\x00\x00\x00\x6e\x00\x00\x00\x56\x00\x5b\x00\x00\x00\x00\x00\x41\x00\x00\x00\x40\x00\x3f\x00\x00\x00\x3e\x00\xe3\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x39\x00\x00\x00\x38\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x4a\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xe8\xff\xee\xff\xeb\xff\xe8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xff\xc5\xff\x00\x00\xbc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcd\xff\xcc\xff\xdb\xff\xd4\xff\xe0\xff\xd2\xff\xce\xff\x00\x00\xcf\xff\x00\x00\x00\x00\xdc\xff\xdf\xff\x00\x00\xdd\xff\xde\xff\x00\x00\x00\x00\x00\x00\x00\x00\xcf\xff\xce\xff\x00\x00\x00\x00\x00\x00\xe3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xea\xff\x00\x00\x00\x00\xed\xff\x00\x00\xee\xff\x00\x00\x00\x00\xee\xff\xeb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd8\xff\x00\x00\x00\x00\xd9\xff\x00\x00\xda\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcb\xff\x00\x00\x00\x00\x00\x00\xeb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd7\xff\x00\x00\x00\x00\xd3\xff\xd6\xff\xd5\xff\xe1\xff\xe2\xff\xe4\xff\xe5\xff\x00\x00\xe9\xff\xec\xff\xe7\xff\x00\x00\xef\xff\xe6\xff\xd1\xff\xd0\xff\xc0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbe\xff\xc3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\xff\xbd\xff\x00\x00\xc7\xff\x00\x00\x00\x00\xc1\xff\xc2\xff\xc8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xca\xff\x00\x00\x00\x00\x00\x00\xc9\xff\x00\x00\x00\x00\xc6\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x06\x00\x0a\x00\x06\x00\x01\x00\x07\x00\x03\x00\x01\x00\x04\x00\x03\x00\x28\x00\x0a\x00\x00\x00\x00\x00\x1f\x00\x0c\x00\x0d\x00\x2f\x00\x0c\x00\x0d\x00\x00\x00\x12\x00\x27\x00\x14\x00\x12\x00\x16\x00\x14\x00\x0f\x00\x16\x00\x09\x00\x2f\x00\x15\x00\x1d\x00\x1e\x00\x21\x00\x19\x00\x1a\x00\x1b\x00\x2b\x00\x00\x00\x2b\x00\x31\x00\x20\x00\x00\x00\x31\x00\x00\x00\x24\x00\x31\x00\x26\x00\x2e\x00\x2f\x00\x2f\x00\x2e\x00\x2f\x00\x0f\x00\x00\x00\x00\x00\x2f\x00\x0f\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x0f\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x00\x00\x00\x00\x0f\x00\x02\x00\x0e\x00\x00\x00\x00\x00\x06\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x06\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x29\x00\x2a\x00\x03\x00\x00\x00\x06\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x06\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x17\x00\x01\x00\x00\x00\x00\x00\x04\x00\x1c\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x08\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x11\x00\x00\x00\x13\x00\x08\x00\x09\x00\x09\x00\x08\x00\x09\x00\x09\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x09\x00\x09\x00\x09\x00\x31\x00\x27\x00\x07\x00\x07\x00\x00\x00\x05\x00\x05\x00\x00\x00\x00\x00\x02\x00\x0a\x00\x07\x00\x05\x00\x06\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x2f\x00\x2d\x00\x0e\x00\x05\x00\x04\x00\x2d\x00\x2d\x00\x23\x00\x0f\x00\x2f\x00\x09\x00\x09\x00\x2f\x00\x09\x00\x09\x00\x21\x00\x09\x00\x21\x00\x0f\x00\x22\x00\x2a\x00\xff\xff\x21\x00\x2f\x00\x2f\x00\x21\x00\x0f\x00\x11\x00\x2f\x00\x0a\x00\x0a\x00\x11\x00\x13\x00\x09\x00\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x21\x00\x11\x00\x21\x00\x11\x00\x0b\x00\x0b\x00\x08\x00\x18\x00\x11\x00\x08\x00\x0f\x00\xff\xff\xff\xff\xff\xff\x0e\x00\xff\xff\x25\x00\x2f\x00\x0e\x00\x2a\x00\x2f\x00\x21\x00\x21\x00\x2f\x00\x0a\x00\x2f\x00\x10\x00\xff\xff\xff\xff\x2f\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x2c\x00\xff\xff\xff\xff\x31\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x31\x00\xff\xff\xff\xff\x31\x00\xff\xff\xff\xff\x31\x00\x31\x00\x2f\x00\x31\x00\x2f\x00\x31\x00\x28\x00\x31\x00\x2f\x00\x2f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x4b\x00\x49\x00\x4d\x00\x49\x00\x25\x00\x4c\x00\x26\x00\x25\x00\x57\x00\x26\x00\x3a\x00\x4d\x00\x11\x00\xb2\x00\x1c\x00\x27\x00\x28\x00\x11\x00\x27\x00\x28\x00\x1d\x00\x29\x00\x1d\x00\x2a\x00\x29\x00\x2b\x00\x2a\x00\xb6\x00\x2b\x00\x6f\x00\x11\x00\x14\x00\x2c\x00\x2d\x00\xa5\x00\x15\x00\x16\x00\x17\x00\x4a\x00\x11\x00\x4a\x00\xff\xff\x18\x00\x11\x00\xff\xff\x11\x00\x19\x00\xff\xff\x1a\x00\x2e\x00\x11\x00\x11\x00\x2e\x00\x11\x00\xb3\x00\x11\x00\x11\x00\x11\x00\xad\x00\x11\x00\xa6\x00\x11\x00\x11\x00\x11\x00\x11\x00\x11\x00\x11\x00\x11\x00\x11\x00\x11\x00\xa7\x00\xa8\x00\x11\x00\xae\x00\xab\x00\x7e\x00\x9f\x00\xa0\x00\xa1\x00\xa3\x00\x99\x00\x8a\x00\x78\x00\x7c\x00\x84\x00\x1d\x00\x36\x00\x12\x00\x75\x00\x7f\x00\x93\x00\x1d\x00\x3e\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x94\x00\x2f\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x79\x00\x2f\x00\x1d\x00\x36\x00\x3b\x00\x3d\x00\x96\x00\x85\x00\x1d\x00\x3e\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x7a\x00\x2f\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x7b\x00\x2f\x00\x1d\x00\x36\x00\x3b\x00\x5a\x00\x5b\x00\x74\x00\x1d\x00\x53\x00\x1e\x00\x1f\x00\x20\x00\x6c\x00\x22\x00\x2f\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x71\x00\x2f\x00\x1d\x00\x54\x00\x3b\x00\x9c\x00\x8b\x00\x3c\x00\x1d\x00\x55\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x72\x00\x2f\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x51\x00\x2f\x00\x1d\x00\x36\x00\x8d\x00\x8e\x00\x90\x00\x7d\x00\x1d\x00\x37\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x2e\x00\x2f\x00\x1d\x00\x82\x00\x3f\x00\x80\x00\x83\x00\x40\x00\x83\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x30\x00\x22\x00\x2f\x00\x66\x00\x1e\x00\x1f\x00\x31\x00\x1d\x00\x1d\x00\x47\x00\x1d\x00\x1d\x00\x67\x00\x1d\x00\x68\x00\x6d\x00\x1f\x00\x70\x00\x6e\x00\x1f\x00\x4d\x00\x33\x00\x1f\x00\x1d\x00\x1d\x00\x1d\x00\x3a\x00\x5d\x00\x1c\x00\x60\x00\x34\x00\x34\x00\x50\x00\x52\x00\x32\x00\xff\xff\x1d\x00\x9e\x00\x5f\x00\x34\x00\x6a\x00\x42\x00\x36\x00\x45\x00\x41\x00\x4d\x00\x35\x00\x42\x00\x3e\x00\x4b\x00\x4e\x00\x4f\x00\x57\x00\x58\x00\x4c\x00\x5b\x00\x11\x00\xb8\x00\x1a\x00\x38\x00\x3a\x00\xb5\x00\xb1\x00\xb2\x00\xad\x00\x11\x00\xb6\x00\xb0\x00\x11\x00\xab\x00\x9e\x00\xa3\x00\x9b\x00\xa6\x00\xaa\x00\x8a\x00\x87\x00\x00\x00\x96\x00\x11\x00\x11\x00\x98\x00\x9c\x00\x90\x00\x11\x00\x4d\x00\xce\xff\x93\x00\x92\x00\x88\x00\x00\x00\x00\x00\x00\x00\x11\x00\x00\x00\x99\x00\x64\x00\x8d\x00\x65\x00\x6b\x00\x6c\x00\x44\x00\x62\x00\x74\x00\x45\x00\x77\x00\x00\x00\x00\x00\x00\x00\x47\x00\x00\x00\x5f\x00\x11\x00\x48\x00\x89\x00\x11\x00\x63\x00\x69\x00\x11\x00\x4d\x00\x11\x00\x5d\x00\x00\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x00\x00\x78\x00\x00\x00\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\xff\xff\x00\x00\x00\x00\xff\xff\x00\x00\x00\x00\xff\xff\xff\xff\x11\x00\xff\xff\x11\x00\xff\xff\x3a\x00\xff\xff\x11\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (15, 67) [
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
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
	(67 , happyReduce_67)
	]

happy_n_terms = 50 :: Int
happy_n_nonterms = 16 :: Int

happyReduce_15 = happySpecReduce_1  0# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn18
		 (Ident happy_var_1
	)}

happyReduce_16 = happyReduce 4# 1# happyReduction_16
happyReduction_16 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_2 of { happy_var_2 -> 
	case happyOut33 happy_x_4 of { happy_var_4 -> 
	happyIn19
		 (Deriv (reverse happy_var_1) happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_17 = happySpecReduce_0  2# happyReduction_17
happyReduction_17  =  happyIn20
		 ([]
	)

happyReduce_18 = happySpecReduce_1  2# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 ((:[]) happy_var_1
	)}

happyReduce_19 = happySpecReduce_3  2# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_20 = happySpecReduce_0  3# happyReduction_20
happyReduction_20  =  happyIn21
		 ([]
	)

happyReduce_21 = happySpecReduce_1  3# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 ((:[]) happy_var_1
	)}

happyReduce_22 = happySpecReduce_3  3# happyReduction_22
happyReduction_22 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_23 = happySpecReduce_0  4# happyReduction_23
happyReduction_23  =  happyIn22
		 ([]
	)

happyReduce_24 = happySpecReduce_3  4# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_2 of { happy_var_2 -> 
	happyIn22
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_25 = happyReduce 4# 5# happyReduction_25
happyReduction_25 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_4 of { happy_var_4 -> 
	happyIn23
		 (TyAlias happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_26 = happySpecReduce_3  6# happyReduction_26
happyReduction_26 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (Binder happy_var_1 happy_var_3
	)}}

happyReduce_27 = happySpecReduce_3  7# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 (BJust happy_var_1 happy_var_3
	)}}

happyReduce_28 = happySpecReduce_1  7# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (BNothing happy_var_1
	)}

happyReduce_29 = happySpecReduce_3  8# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 (Tensor happy_var_1 happy_var_3
	)}}

happyReduce_30 = happySpecReduce_3  8# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 (Par happy_var_1 happy_var_3
	)}}

happyReduce_31 = happySpecReduce_1  8# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (happy_var_1
	)}

happyReduce_32 = happySpecReduce_1  9# happyReduction_32
happyReduction_32 happy_x_1
	 =  happyIn27
		 (One
	)

happyReduce_33 = happySpecReduce_1  9# happyReduction_33
happyReduction_33 happy_x_1
	 =  happyIn27
		 (Bot
	)

happyReduce_34 = happySpecReduce_1  9# happyReduction_34
happyReduction_34 happy_x_1
	 =  happyIn27
		 (Top
	)

happyReduce_35 = happySpecReduce_1  9# happyReduction_35
happyReduction_35 happy_x_1
	 =  happyIn27
		 (Zero
	)

happyReduce_36 = happySpecReduce_1  9# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (TyId happy_var_1
	)}

happyReduce_37 = happySpecReduce_2  9# happyReduction_37
happyReduction_37 happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_2 of { happy_var_2 -> 
	happyIn27
		 (Bang happy_var_2
	)}

happyReduce_38 = happySpecReduce_2  9# happyReduction_38
happyReduction_38 happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_2 of { happy_var_2 -> 
	happyIn27
		 (Quest happy_var_2
	)}

happyReduce_39 = happySpecReduce_2  9# happyReduction_39
happyReduction_39 happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_2 of { happy_var_2 -> 
	happyIn27
		 (Neg happy_var_2
	)}

happyReduce_40 = happySpecReduce_3  9# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn27
		 (happy_var_2
	)}

happyReduce_41 = happySpecReduce_3  10# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (Plus happy_var_1 happy_var_3
	)}}

happyReduce_42 = happySpecReduce_3  10# happyReduction_42
happyReduction_42 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (Choice happy_var_1 happy_var_3
	)}}

happyReduce_43 = happySpecReduce_1  10# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (happy_var_1
	)}

happyReduce_44 = happySpecReduce_3  11# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_3 of { happy_var_3 -> 
	happyIn29
		 (Lollipop happy_var_1 happy_var_3
	)}}

happyReduce_45 = happySpecReduce_1  11# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (happy_var_1
	)}

happyReduce_46 = happyReduce 4# 12# happyReduction_46
happyReduction_46 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_4 of { happy_var_4 -> 
	happyIn30
		 (Forall happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_47 = happyReduce 4# 12# happyReduction_47
happyReduction_47 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_4 of { happy_var_4 -> 
	happyIn30
		 (Exists happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_48 = happySpecReduce_1  12# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (happy_var_1
	)}

happyReduce_49 = happySpecReduce_1  13# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (happy_var_1
	)}

happyReduce_50 = happySpecReduce_1  14# happyReduction_50
happyReduction_50 happy_x_1
	 =  happyIn32
		 (Fst
	)

happyReduce_51 = happySpecReduce_1  14# happyReduction_51
happyReduction_51 happy_x_1
	 =  happyIn32
		 (Snd
	)

happyReduce_52 = happySpecReduce_3  15# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 (Ax happy_var_1 happy_var_3
	)}}

happyReduce_53 = happyReduce 10# 15# happyReduction_53
happyReduction_53 (happy_x_10 `HappyStk`
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
	 = case happyOut25 happy_x_3 of { happy_var_3 -> 
	case happyOut33 happy_x_5 of { happy_var_5 -> 
	case happyOut25 happy_x_7 of { happy_var_7 -> 
	case happyOut33 happy_x_9 of { happy_var_9 -> 
	happyIn33
		 (Cut happy_var_3 happy_var_5 happy_var_7 happy_var_9
	) `HappyStk` happyRest}}}}

happyReduce_54 = happyReduce 12# 15# happyReduction_54
happyReduction_54 (happy_x_12 `HappyStk`
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
	 = case happyOut18 happy_x_3 of { happy_var_3 -> 
	case happyOut18 happy_x_5 of { happy_var_5 -> 
	case happyOut33 happy_x_7 of { happy_var_7 -> 
	case happyOut18 happy_x_9 of { happy_var_9 -> 
	case happyOut33 happy_x_11 of { happy_var_11 -> 
	happyIn33
		 (ParSeq happy_var_3 happy_var_5 happy_var_7 happy_var_9 happy_var_11
	) `HappyStk` happyRest}}}}}

happyReduce_55 = happyReduce 8# 15# happyReduction_55
happyReduction_55 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_4 of { happy_var_4 -> 
	case happyOut18 happy_x_6 of { happy_var_6 -> 
	case happyOut33 happy_x_8 of { happy_var_8 -> 
	happyIn33
		 (TensorSeq happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_56 = happyReduce 7# 15# happyReduction_56
happyReduction_56 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut32 happy_x_4 of { happy_var_4 -> 
	case happyOut18 happy_x_5 of { happy_var_5 -> 
	case happyOut33 happy_x_7 of { happy_var_7 -> 
	happyIn33
		 (ChoiceSeq happy_var_2 happy_var_4 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}}

happyReduce_57 = happyReduce 14# 15# happyReduction_57
happyReduction_57 (happy_x_14 `HappyStk`
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
	 = case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_6 of { happy_var_6 -> 
	case happyOut33 happy_x_8 of { happy_var_8 -> 
	case happyOut18 happy_x_11 of { happy_var_11 -> 
	case happyOut33 happy_x_13 of { happy_var_13 -> 
	happyIn33
		 (Case happy_var_2 happy_var_6 happy_var_8 happy_var_11 happy_var_13
	) `HappyStk` happyRest}}}}}

happyReduce_58 = happySpecReduce_1  15# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 (Bottom happy_var_1
	)}

happyReduce_59 = happyReduce 6# 15# happyReduction_59
happyReduction_59 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_4 of { happy_var_4 -> 
	case happyOut33 happy_x_6 of { happy_var_6 -> 
	happyIn33
		 (Unit happy_var_4 happy_var_6
	) `HappyStk` happyRest}}

happyReduce_60 = happyReduce 4# 15# happyReduction_60
happyReduction_60 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut21 happy_x_4 of { happy_var_4 -> 
	happyIn33
		 (Crash happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_61 = happyReduce 8# 15# happyReduction_61
happyReduction_61 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_4 of { happy_var_4 -> 
	case happyOut30 happy_x_6 of { happy_var_6 -> 
	case happyOut33 happy_x_8 of { happy_var_8 -> 
	happyIn33
		 (Pack happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_62 = happyReduce 8# 15# happyReduction_62
happyReduction_62 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_4 of { happy_var_4 -> 
	case happyOut18 happy_x_6 of { happy_var_6 -> 
	case happyOut33 happy_x_8 of { happy_var_8 -> 
	happyIn33
		 (Unpack happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_63 = happyReduce 4# 15# happyReduction_63
happyReduction_63 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_2 of { happy_var_2 -> 
	case happyOut33 happy_x_4 of { happy_var_4 -> 
	happyIn33
		 (Offer happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_64 = happyReduce 7# 15# happyReduction_64
happyReduction_64 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_5 of { happy_var_5 -> 
	case happyOut33 happy_x_7 of { happy_var_7 -> 
	happyIn33
		 (Demand happy_var_2 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_65 = happyReduce 4# 15# happyReduction_65
happyReduction_65 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut33 happy_x_4 of { happy_var_4 -> 
	happyIn33
		 (Ignore happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_66 = happyReduce 7# 15# happyReduction_66
happyReduction_66 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_5 of { happy_var_5 -> 
	case happyOut33 happy_x_7 of { happy_var_7 -> 
	happyIn33
		 (Alias happy_var_2 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_67 = happySpecReduce_1  15# happyReduction_67
happyReduction_67 happy_x_1
	 =  happyIn33
		 (Hole
	)

happyNewToken action sts stk [] =
	happyDoAction 49# notHappyAtAll action sts stk []

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
	PT _ (TV happy_dollar_dollar) -> cont 47#;
	_ -> cont 48#;
	_ -> happyError' (tk:tks)
	}

happyError_ 49# tk tks = happyError' tks
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
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut19 x))

pListBinder tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut20 x))

pListIdent tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut21 x))

pListAlias tks = happySomeParser where
  happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut22 x))

pAlias tks = happySomeParser where
  happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut23 x))

pBinder tks = happySomeParser where
  happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut24 x))

pMBinder tks = happySomeParser where
  happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut25 x))

pType4 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut26 x))

pType5 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut27 x))

pType3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut28 x))

pType2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut29 x))

pType tks = happySomeParser where
  happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut30 x))

pType1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (happyOut31 x))

pChoice tks = happySomeParser where
  happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (happyOut32 x))

pSeq tks = happySomeParser where
  happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (happyOut33 x))

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
