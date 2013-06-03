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
happyIn16 :: (Ident) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Ident)
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Prog) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (Prog)
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: ([Binder]) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> ([Binder])
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ([Alias]) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ([Alias])
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Alias) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (Alias)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (Binder) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (Binder)
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (Type) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (Type)
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (Type) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (Type)
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (Type) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (Type)
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (Type) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (Type)
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
happyIn28 :: (Choice) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (Choice)
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (Seq) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (Seq)
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x18\x01\x00\x00\x1b\x01\x17\x01\x08\x00\x08\x00\x08\x00\x04\x00\x04\x00\x04\x00\xf2\xff\x0b\x00\x17\x01\x00\x00\x16\x01\x14\x01\x13\x01\x2f\x00\x13\x01\x13\x01\x06\x00\x13\x01\x0f\x01\x00\x00\x00\x00\x00\x00\xfe\xff\x00\x00\x02\x01\x00\x00\x1a\x01\x0e\x01\x08\x00\x04\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x0c\x01\x0c\x01\x08\x00\xf9\xff\x00\x00\x0b\x01\xff\xff\x0b\x01\xfc\xff\x15\x01\x09\x01\x09\x01\x06\x01\x43\x00\xf8\x00\x19\x01\xf1\x00\xc4\x00\x01\x01\x10\x01\xf5\x00\x0d\x01\x04\x00\x08\x00\x08\x00\x08\x00\x08\x00\x04\x00\x00\x00\x12\x01\x11\x01\x00\x00\x03\x00\x00\x00\xf7\x00\xc9\x00\x08\x01\x07\x01\xf6\x00\x00\x00\xee\x00\xee\x00\xf4\x00\xed\x00\x00\x00\xec\x00\x0a\x01\xea\x00\x0b\x00\xe0\x00\x68\x00\xe0\x00\x0d\x00\xe0\x00\x0b\x00\x00\x00\x04\x00\x04\x00\x05\x01\xfe\xff\xfe\xff\x00\x00\x00\x00\x04\x01\x04\x00\x00\x00\x00\x00\x0b\x00\x00\x00\x04\x01\x04\x01\x04\x01\x00\x00\xff\x00\xfe\x00\xde\x00\xfb\x00\xda\x00\xda\x00\xeb\x00\x00\x00\xd9\x00\x0b\x00\xb6\x00\xbe\x00\xc6\x00\xc1\x00\x0b\x00\xb5\x00\xb4\x00\x9c\x00\xa9\x00\x04\x00\x9a\x00\xa7\x00\x02\x00\x0b\x00\x9f\x00\x0b\x00\x0b\x00\x00\x00\x0b\x00\x90\x00\xb3\x00\x0b\x00\xb2\x00\xab\x00\x00\x00\x00\x00\x0b\x00\x00\x00\x0b\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x85\x00\x0b\x00\xa2\x00\x8f\x00\x84\x00\xa5\x00\x0b\x00\x00\x00\x79\x00\x9b\x00\x6f\x00\x00\x00\x0b\x00\x6e\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x57\x00\xf9\x00\xa0\x00\x9e\x00\x03\x01\xdd\x00\xe9\x00\xd8\x00\xc5\x00\xb9\x00\xad\x00\x99\x00\x51\x00\x00\x00\x00\x00\x00\x00\x00\x00\x98\x00\x00\x00\x97\x00\x96\x00\x00\x01\xfd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe8\x00\xa1\x00\x00\x00\x00\x00\xe7\x00\x00\x00\x00\x00\x8e\x00\x8d\x00\xe6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x00\x64\x00\x00\x00\x00\x00\x00\x00\xf2\x00\x00\x00\x00\x00\xf3\x00\x00\x00\x95\x00\xe5\x00\xd2\x00\xdb\x00\x55\x00\x89\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8b\x00\xfc\x00\x00\x00\x8a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x81\x00\x00\x00\x80\x00\x12\x00\x7f\x00\x47\x00\x00\x00\x7d\x00\x71\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x65\x00\x00\x00\x00\x00\x46\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x76\x00\x00\x00\x75\x00\x74\x00\x00\x00\x00\x00\x73\x00\x45\x00\x00\x00\x6a\x00\x00\x00\x00\x00\x42\x00\x00\x00\x00\x00\x67\x00\x00\x00\x59\x00\x66\x00\x00\x00\x00\x00\x41\x00\x00\x00\x40\x00\x3f\x00\x00\x00\x3e\x00\xfa\x00\x00\x00\x3d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x3b\x00\x37\x00\x00\x00\x00\x00\x00\x00\x26\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x00\x00\x13\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xed\xff\xf0\xff\xed\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf2\xff\xcc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd4\xff\xd3\xff\xe2\xff\xdb\xff\xe7\xff\xd9\xff\xd5\xff\x00\x00\xd6\xff\x00\x00\x00\x00\xe3\xff\xe6\xff\x00\x00\xe4\xff\xe5\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd6\xff\xd5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xef\xff\x00\x00\xf0\xff\x00\x00\x00\x00\xf0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdf\xff\x00\x00\x00\x00\xe0\xff\x00\x00\xe1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xca\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xde\xff\x00\x00\x00\x00\xda\xff\xdd\xff\xdc\xff\xe8\xff\xe9\xff\xea\xff\x00\x00\xee\xff\xec\xff\x00\x00\xf1\xff\xeb\xff\xd8\xff\xd7\xff\xc7\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc6\xff\xc4\xff\x00\x00\xce\xff\x00\x00\x00\x00\xc8\xff\xc9\xff\xcf\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd1\xff\x00\x00\x00\x00\x00\x00\xd0\xff\x00\x00\x00\x00\xcd\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x06\x00\x0a\x00\x06\x00\x01\x00\x07\x00\x03\x00\x05\x00\x01\x00\x04\x00\x03\x00\x0a\x00\x0a\x00\x00\x00\x1d\x00\x0c\x00\x0d\x00\x00\x00\x00\x00\x0c\x00\x0d\x00\x12\x00\x25\x00\x14\x00\x15\x00\x12\x00\x0d\x00\x14\x00\x15\x00\x0c\x00\x1b\x00\x1c\x00\x1f\x00\x17\x00\x18\x00\x19\x00\x29\x00\x00\x00\x29\x00\x2f\x00\x1e\x00\x1d\x00\x2f\x00\x00\x00\x22\x00\x2f\x00\x24\x00\x2c\x00\x2d\x00\x25\x00\x2d\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x2d\x00\x0d\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x0d\x00\x00\x00\x00\x00\x0d\x00\x0d\x00\x0d\x00\x00\x00\x27\x00\x28\x00\x01\x00\x00\x00\x03\x00\x06\x00\x07\x00\x0d\x00\x0d\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x04\x00\x26\x00\x00\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x1a\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2b\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x04\x00\x03\x00\x09\x00\x0c\x00\x2d\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x09\x00\x2b\x00\x21\x00\x0f\x00\x2d\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x0f\x00\x09\x00\x09\x00\x2d\x00\x1f\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x1f\x00\x2d\x00\x1f\x00\x2d\x00\x09\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x08\x00\x00\x00\x1f\x00\x1f\x00\x0f\x00\x20\x00\x0e\x00\x00\x00\x07\x00\x11\x00\x00\x00\x13\x00\x00\x00\x06\x00\x07\x00\x08\x00\x06\x00\x07\x00\x06\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x2d\x00\x07\x00\x07\x00\x07\x00\x07\x00\x07\x00\x2d\x00\x00\x00\x00\x00\x02\x00\x02\x00\x04\x00\x05\x00\x05\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x05\x00\x05\x00\x00\x00\x05\x00\x05\x00\x00\x00\x02\x00\x05\x00\x2d\x00\x2d\x00\x05\x00\x07\x00\x1f\x00\x2d\x00\x11\x00\x2d\x00\x0a\x00\x0a\x00\x11\x00\x13\x00\x28\x00\x09\x00\x28\x00\x1f\x00\x1f\x00\x23\x00\x11\x00\x11\x00\x2d\x00\x2d\x00\x0b\x00\x0b\x00\x11\x00\x0f\x00\x2f\x00\x08\x00\x2d\x00\x0e\x00\x0a\x00\xff\xff\x10\x00\x2f\x00\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x2d\x00\x2f\x00\xff\xff\xff\xff\x2f\x00\x2f\x00\xff\xff\x2d\x00\x26\x00\xff\xff\x2f\x00\x2d\x00\x2d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x42\x00\x40\x00\x44\x00\x40\x00\x22\x00\x43\x00\x23\x00\x60\x00\x22\x00\x4e\x00\x23\x00\x44\x00\x44\x00\x0f\x00\x19\x00\x24\x00\x25\x00\x72\x00\xa5\x00\x24\x00\x25\x00\x26\x00\x1a\x00\x27\x00\x28\x00\x26\x00\xa9\x00\x27\x00\x28\x00\x73\x00\x29\x00\x2a\x00\x98\x00\x12\x00\x13\x00\x14\x00\x41\x00\xa1\x00\x41\x00\xff\xff\x15\x00\x19\x00\xff\xff\x0f\x00\x16\x00\xff\xff\x17\x00\x2b\x00\x0f\x00\x1a\x00\x0f\x00\x2b\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\xa6\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\x0f\x00\xa0\x00\x99\x00\x0f\x00\x0f\x00\x0f\x00\x9a\x00\x9b\x00\x9e\x00\x92\x00\x93\x00\x94\x00\x96\x00\x8c\x00\x0f\x00\x0f\x00\x7d\x00\x6c\x00\x70\x00\x1a\x00\x51\x00\x52\x00\x38\x00\x1a\x00\x39\x00\x63\x00\x1c\x00\x78\x00\x10\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x87\x00\x2c\x00\x1a\x00\x86\x00\x89\x00\x3b\x00\x35\x00\x8f\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x6d\x00\x2c\x00\x1a\x00\xff\xff\x7e\x00\x80\x00\x81\x00\x83\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x6e\x00\x2c\x00\x1a\x00\x76\x00\x71\x00\x74\x00\x77\x00\x77\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x6f\x00\x2c\x00\x1a\x00\x54\x00\x57\x00\x3d\x00\x45\x00\x46\x00\x1b\x00\x1c\x00\x1d\x00\x62\x00\x1f\x00\x2c\x00\x1a\x00\x4e\x00\x4f\x00\x52\x00\xab\x00\xa8\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x67\x00\x2c\x00\x1a\x00\x33\x00\x35\x00\xa9\x00\x17\x00\x0f\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x48\x00\x2c\x00\x1a\x00\xa3\x00\xa4\x00\xa5\x00\xa0\x00\x0f\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x1a\x00\x9d\x00\x9e\x00\x91\x00\x0f\x00\x96\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x2b\x00\x2c\x00\x1a\x00\x99\x00\x0f\x00\x89\x00\x0f\x00\x8e\x00\x1b\x00\x1c\x00\x1d\x00\x2d\x00\x1f\x00\x2c\x00\x5c\x00\x1a\x00\x8b\x00\x8c\x00\x8f\x00\x7d\x00\x3f\x00\x1a\x00\x65\x00\x5d\x00\x1a\x00\x5e\x00\x1a\x00\x1b\x00\x1c\x00\x2e\x00\x64\x00\x1c\x00\x30\x00\x1c\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x1a\x00\x35\x00\x0f\x00\x66\x00\x44\x00\x47\x00\x49\x00\x2f\x00\x0f\x00\x31\x00\x31\x00\x3a\x00\x69\x00\x3b\x00\x37\x00\x37\x00\x31\x00\x31\x00\x36\x00\x31\x00\x31\x00\x37\x00\x91\x00\x4b\x00\x56\x00\x4a\x00\x31\x00\x42\x00\x4c\x00\x0f\x00\x0f\x00\x32\x00\x43\x00\x80\x00\x0f\x00\x83\x00\x0f\x00\x44\x00\xd5\xff\x86\x00\x85\x00\x7a\x00\x7b\x00\x7c\x00\x59\x00\x5f\x00\x56\x00\x5a\x00\x5b\x00\x0f\x00\x0f\x00\x61\x00\x62\x00\x69\x00\x6b\x00\xff\xff\x3d\x00\x0f\x00\x3f\x00\x44\x00\x00\x00\x54\x00\xff\xff\x00\x00\x00\x00\x00\x00\x6c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\x0f\x00\xff\xff\x00\x00\x00\x00\xff\xff\xff\xff\x00\x00\x0f\x00\x35\x00\x00\x00\xff\xff\x0f\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (13, 59) [
	(13 , happyReduce_13),
	(14 , happyReduce_14),
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
	(59 , happyReduce_59)
	]

happy_n_terms = 48 :: Int
happy_n_nonterms = 14 :: Int

happyReduce_13 = happySpecReduce_1  0# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn16
		 (Ident happy_var_1
	)}

happyReduce_14 = happyReduce 4# 1# happyReduction_14
happyReduction_14 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut29 happy_x_4 of { happy_var_4 -> 
	happyIn17
		 (Deriv (reverse happy_var_1) happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_15 = happySpecReduce_0  2# happyReduction_15
happyReduction_15  =  happyIn18
		 ([]
	)

happyReduce_16 = happySpecReduce_1  2# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn18
		 ((:[]) happy_var_1
	)}

happyReduce_17 = happySpecReduce_3  2# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	happyIn18
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_18 = happySpecReduce_0  3# happyReduction_18
happyReduction_18  =  happyIn19
		 ([]
	)

happyReduce_19 = happySpecReduce_3  3# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_20 = happyReduce 4# 4# happyReduction_20
happyReduction_20 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_4 of { happy_var_4 -> 
	happyIn20
		 (TyAlias happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_21 = happySpecReduce_3  5# happyReduction_21
happyReduction_21 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 (Binder happy_var_1 happy_var_3
	)}}

happyReduce_22 = happySpecReduce_3  6# happyReduction_22
happyReduction_22 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (Tensor happy_var_1 happy_var_3
	)}}

happyReduce_23 = happySpecReduce_3  6# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (Par happy_var_1 happy_var_3
	)}}

happyReduce_24 = happySpecReduce_1  6# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (happy_var_1
	)}

happyReduce_25 = happySpecReduce_1  7# happyReduction_25
happyReduction_25 happy_x_1
	 =  happyIn23
		 (One
	)

happyReduce_26 = happySpecReduce_1  7# happyReduction_26
happyReduction_26 happy_x_1
	 =  happyIn23
		 (Bot
	)

happyReduce_27 = happySpecReduce_1  7# happyReduction_27
happyReduction_27 happy_x_1
	 =  happyIn23
		 (Top
	)

happyReduce_28 = happySpecReduce_1  7# happyReduction_28
happyReduction_28 happy_x_1
	 =  happyIn23
		 (Zero
	)

happyReduce_29 = happySpecReduce_1  7# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (TyId happy_var_1
	)}

happyReduce_30 = happySpecReduce_2  7# happyReduction_30
happyReduction_30 happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_2 of { happy_var_2 -> 
	happyIn23
		 (Bang happy_var_2
	)}

happyReduce_31 = happySpecReduce_2  7# happyReduction_31
happyReduction_31 happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_2 of { happy_var_2 -> 
	happyIn23
		 (Quest happy_var_2
	)}

happyReduce_32 = happySpecReduce_2  7# happyReduction_32
happyReduction_32 happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_2 of { happy_var_2 -> 
	happyIn23
		 (Neg happy_var_2
	)}

happyReduce_33 = happySpecReduce_3  7# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_2 of { happy_var_2 -> 
	happyIn23
		 (happy_var_2
	)}

happyReduce_34 = happySpecReduce_3  8# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (Plus happy_var_1 happy_var_3
	)}}

happyReduce_35 = happySpecReduce_3  8# happyReduction_35
happyReduction_35 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (Choice happy_var_1 happy_var_3
	)}}

happyReduce_36 = happySpecReduce_1  8# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (happy_var_1
	)}

happyReduce_37 = happySpecReduce_3  9# happyReduction_37
happyReduction_37 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 (Lollipop happy_var_1 happy_var_3
	)}}

happyReduce_38 = happySpecReduce_1  9# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (happy_var_1
	)}

happyReduce_39 = happyReduce 4# 10# happyReduction_39
happyReduction_39 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_4 of { happy_var_4 -> 
	happyIn26
		 (Forall happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_40 = happyReduce 4# 10# happyReduction_40
happyReduction_40 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_4 of { happy_var_4 -> 
	happyIn26
		 (Exists happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_41 = happySpecReduce_1  10# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (happy_var_1
	)}

happyReduce_42 = happySpecReduce_1  11# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (happy_var_1
	)}

happyReduce_43 = happySpecReduce_1  12# happyReduction_43
happyReduction_43 happy_x_1
	 =  happyIn28
		 (Fst
	)

happyReduce_44 = happySpecReduce_1  12# happyReduction_44
happyReduction_44 happy_x_1
	 =  happyIn28
		 (Snd
	)

happyReduce_45 = happySpecReduce_3  13# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn29
		 (Ax happy_var_1 happy_var_3
	)}}

happyReduce_46 = happyReduce 10# 13# happyReduction_46
happyReduction_46 (happy_x_10 `HappyStk`
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
	 = case happyOut21 happy_x_3 of { happy_var_3 -> 
	case happyOut29 happy_x_5 of { happy_var_5 -> 
	case happyOut21 happy_x_7 of { happy_var_7 -> 
	case happyOut29 happy_x_9 of { happy_var_9 -> 
	happyIn29
		 (Cut happy_var_3 happy_var_5 happy_var_7 happy_var_9
	) `HappyStk` happyRest}}}}

happyReduce_47 = happyReduce 12# 13# happyReduction_47
happyReduction_47 (happy_x_12 `HappyStk`
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
	 = case happyOut16 happy_x_3 of { happy_var_3 -> 
	case happyOut16 happy_x_5 of { happy_var_5 -> 
	case happyOut29 happy_x_7 of { happy_var_7 -> 
	case happyOut16 happy_x_9 of { happy_var_9 -> 
	case happyOut29 happy_x_11 of { happy_var_11 -> 
	happyIn29
		 (ParSeq happy_var_3 happy_var_5 happy_var_7 happy_var_9 happy_var_11
	) `HappyStk` happyRest}}}}}

happyReduce_48 = happyReduce 8# 13# happyReduction_48
happyReduction_48 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	case happyOut16 happy_x_6 of { happy_var_6 -> 
	case happyOut29 happy_x_8 of { happy_var_8 -> 
	happyIn29
		 (TensorSeq happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_49 = happyReduce 7# 13# happyReduction_49
happyReduction_49 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut28 happy_x_4 of { happy_var_4 -> 
	case happyOut16 happy_x_5 of { happy_var_5 -> 
	case happyOut29 happy_x_7 of { happy_var_7 -> 
	happyIn29
		 (ChoiceSeq happy_var_2 happy_var_4 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}}

happyReduce_50 = happyReduce 14# 13# happyReduction_50
happyReduction_50 (happy_x_14 `HappyStk`
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
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_6 of { happy_var_6 -> 
	case happyOut29 happy_x_8 of { happy_var_8 -> 
	case happyOut16 happy_x_11 of { happy_var_11 -> 
	case happyOut29 happy_x_13 of { happy_var_13 -> 
	happyIn29
		 (Case happy_var_2 happy_var_6 happy_var_8 happy_var_11 happy_var_13
	) `HappyStk` happyRest}}}}}

happyReduce_51 = happySpecReduce_1  13# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (Bottom happy_var_1
	)}

happyReduce_52 = happyReduce 6# 13# happyReduction_52
happyReduction_52 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_4 of { happy_var_4 -> 
	case happyOut29 happy_x_6 of { happy_var_6 -> 
	happyIn29
		 (Unit happy_var_4 happy_var_6
	) `HappyStk` happyRest}}

happyReduce_53 = happySpecReduce_2  13# happyReduction_53
happyReduction_53 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn29
		 (Crash happy_var_2
	)}

happyReduce_54 = happyReduce 8# 13# happyReduction_54
happyReduction_54 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	case happyOut26 happy_x_6 of { happy_var_6 -> 
	case happyOut29 happy_x_8 of { happy_var_8 -> 
	happyIn29
		 (Pack happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_55 = happyReduce 8# 13# happyReduction_55
happyReduction_55 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	case happyOut16 happy_x_6 of { happy_var_6 -> 
	case happyOut29 happy_x_8 of { happy_var_8 -> 
	happyIn29
		 (Unpack happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_56 = happyReduce 4# 13# happyReduction_56
happyReduction_56 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_2 of { happy_var_2 -> 
	case happyOut29 happy_x_4 of { happy_var_4 -> 
	happyIn29
		 (Offer happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_57 = happyReduce 7# 13# happyReduction_57
happyReduction_57 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_5 of { happy_var_5 -> 
	case happyOut29 happy_x_7 of { happy_var_7 -> 
	happyIn29
		 (Demand happy_var_2 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_58 = happyReduce 4# 13# happyReduction_58
happyReduction_58 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut29 happy_x_4 of { happy_var_4 -> 
	happyIn29
		 (Ignore happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_59 = happyReduce 7# 13# happyReduction_59
happyReduction_59 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut21 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_5 of { happy_var_5 -> 
	case happyOut29 happy_x_7 of { happy_var_7 -> 
	happyIn29
		 (Alias happy_var_2 happy_var_5 happy_var_7
	) `HappyStk` happyRest}}}

happyNewToken action sts stk [] =
	happyDoAction 47# notHappyAtAll action sts stk []

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
	PT _ (TV happy_dollar_dollar) -> cont 45#;
	_ -> cont 46#;
	_ -> happyError' (tk:tks)
	}

happyError_ 47# tk tks = happyError' tks
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
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut17 x))

pListBinder tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut18 x))

pListAlias tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut19 x))

pAlias tks = happySomeParser where
  happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut20 x))

pBinder tks = happySomeParser where
  happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut21 x))

pType4 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut22 x))

pType5 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut23 x))

pType3 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut24 x))

pType2 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut25 x))

pType tks = happySomeParser where
  happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut26 x))

pType1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut27 x))

pChoice tks = happySomeParser where
  happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut28 x))

pSeq tks = happySomeParser where
  happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (happyOut29 x))

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
