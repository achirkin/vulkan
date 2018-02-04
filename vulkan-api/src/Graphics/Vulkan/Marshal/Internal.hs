{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE UnboxedTuples    #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- | This module is not part of auto-generated code based on vk.xml.
--   Instead, it is hand-written to provide common types and classes.
module Graphics.Vulkan.Marshal.Internal
  ( cmpImmutableContent, fromForeignPtr#
  ) where

import           Data.Void          (Void)
import           Foreign.C.Types    (CInt (..), CSize (..))
import           Foreign.ForeignPtr (ForeignPtr)
import           Foreign.Storable   (Storable (..))
import           GHC.Base           (isTrue#)
import           GHC.Prim
import           GHC.Types          (IO (..), Int (..))

import           GHC.ForeignPtr     (ForeignPtr (..), ForeignPtrContents (..))


-- | Create a `ByteArray#`-based type from `ForeignPtr`.
--   Try to not copy data, but do it if necessary.
fromForeignPtr# :: Storable a => (ByteArray# -> a) -> ForeignPtr a -> IO a
fromForeignPtr# constr (ForeignPtr addr PlainForeignPtr{})
  | I# n <- sizeOf (constr undefined)
  , I# a <- alignment (constr undefined)
  = IO
  (\s0 -> case newAlignedPinnedByteArray# n a s0 of
    (# s1, mba #) -> case copyAddrToByteArray# addr mba 0# n s1 of
      s2 -> case unsafeFreezeByteArray# mba s2 of
        (# s3, ba #) -> (# s3, constr ba  #)
  )
fromForeignPtr# constr (ForeignPtr _ (MallocPtr mba _))
  = IO
  (\s0 -> case unsafeFreezeByteArray# mba s0 of
    (# s1, ba #) -> (# s1, constr ba  #)
  )
fromForeignPtr# constr (ForeignPtr _ (PlainPtr mba))
  = IO
  (\s0 -> case unsafeFreezeByteArray# mba s0 of
    (# s1, ba #) -> (# s1, constr ba  #)
  )

-- | Internal function used to implement Eq and Ord instances for Vulkan structs.
--
--   Uses lexicographic ordering (c memcmp inside).
--   Note: I assume arrays have equal size, because this is the case
--   for all structs in the library.
--
--   This is a helper that should be used in VulkanMarshal instances only.
cmpImmutableContent :: ByteArray# -> ByteArray# -> Ordering
cmpImmutableContent a b
  | isTrue# (reallyUnsafePtrEquality#
              (unsafeCoerce# a :: Void)
              (unsafeCoerce# b :: Void)
            ) = EQ
  | otherwise = case c_memcmp_ba a b . fromIntegral
                                     $ I# (sizeofByteArray# a) of
     r | r > 0 -> GT
       | r < 0 -> LT
       | otherwise -> EQ

foreign import ccall unsafe "memcmp"
  c_memcmp_ba :: ByteArray# -> ByteArray# -> CSize -> CInt
