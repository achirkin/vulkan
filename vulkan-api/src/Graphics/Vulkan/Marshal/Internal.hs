{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE Strict                    #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE UnliftedFFITypes          #-}
-- | This module is not part of auto-generated code based on vk.xml.
--   Instead, it is hand-written to provide common types and classes.
--
--   DANGER!
--   This is an internal module; it can change a lot between package versions;
--   it provides low-level functions, most of which have user-friendly analogues.
module Graphics.Vulkan.Marshal.Internal
  ( VulkanMarshalPrim (..)
  , fromForeignPtr#
  , toForeignPtr#, toPlainForeignPtr#
  , touchVkData#
  , cmpBytes#
  , newVkData#
  , mallocVkData#, mallocVkDataArray#
  , peekVkData#, pokeVkData#
  ) where

import           Foreign.C.Types    (CInt (..), CSize (..))
import           Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import           Foreign.Storable   (Storable (..))
import           GHC.Base           (Addr#, ByteArray#, IO (..), Int (..), Int#,
                                     byteArrayContents#, copyAddrToByteArray#,
                                     eqAddr#, isTrue#, minusAddr#,
                                     newAlignedPinnedByteArray#, touch#,
                                     unsafeCoerce#, unsafeFreezeByteArray#,
                                     (*#), (+#), (>=#))
import           GHC.Ptr            (Ptr (..))

import           GHC.ForeignPtr     (ForeignPtr (..), ForeignPtrContents (..))


-- | This class gives low-level access to memory location occupied by Vulkan data.
--
--   Meant for internal use only.
class VulkanMarshalPrim a where
  -- | Get address of vulkan structure.
  --   Note, the address is only valid as long as a given vulkan structure exists.
  --   Structures created with newVkData are stored in pinned byte arrays,
  --   so their memory is maintained by Haskell GC.
  unsafeAddr :: a -> Addr#
  -- | Get a @ByteArray#@ that keeps the data.
  --
  --   Note, the data structure does not necessarily starts at zero offset.
  unsafeByteArray :: a -> ByteArray#
  -- | Combine a vulkan structure from ByteArray and an offset in this array.
  unsafeFromByteArrayOffset :: Int# -> ByteArray# -> a




-- | Create a `ByteArray#`-based type from `ForeignPtr`.
--   Try to not copy data, but do it if necessary.
fromForeignPtr# :: forall a . (Storable a, VulkanMarshalPrim a)
                => ForeignPtr a -> IO a
fromForeignPtr# (ForeignPtr addr PlainForeignPtr{})
  | I# n <- sizeOf (undefined :: a)
  , I# a <- alignment (undefined :: a)
  = IO
  (\s0 -> case newAlignedPinnedByteArray# n a s0 of
    (# s1, mba #) -> case copyAddrToByteArray# addr mba 0# n s1 of
      s2 -> case unsafeFreezeByteArray# mba s2 of
        (# s3, ba #) -> (# s3, unsafeFromByteArrayOffset 0# ba  #)
  )
fromForeignPtr# (ForeignPtr addr (MallocPtr mba _))
  = IO
  (\s0 -> case unsafeFreezeByteArray# mba s0 of
    (# s1, ba #) -> (# s1, unsafeFromByteArrayOffset
                           (minusAddr# addr (byteArrayContents# ba)) ba  #)
  )
fromForeignPtr# (ForeignPtr addr (PlainPtr mba))
  = IO
  (\s0 -> case unsafeFreezeByteArray# mba s0 of
    (# s1, ba #) -> (# s1, unsafeFromByteArrayOffset
                           (minusAddr# addr (byteArrayContents# ba)) ba  #)
  )
{-# INLINE fromForeignPtr# #-}


-- | Create a `ForeignPtr` referencing the structure without copying data.
toForeignPtr# :: VulkanMarshalPrim a => a -> IO (ForeignPtr a)
toForeignPtr# x
  | a <- unsafeAddr x
  , b <- unsafeByteArray x = do
    ForeignPtr _ (PlainForeignPtr r)
      <- newForeignPtr_ (Ptr a)
    IO (\s -> (# s, ForeignPtr a (MallocPtr (unsafeCoerce# b) r) #))
{-# INLINE toForeignPtr# #-}

-- | Create a `ForeignPtr` referencing the structure without copying data.
--   This version of a pointer carries no finalizers.
--
-- It is not possible to add a finalizer to a ForeignPtr created with
-- @toPlainForeignPtr@.
-- Attempts to add a finalizer to a ForeignPtr created this way, or to
-- finalize such a pointer, will throw an exception.
toPlainForeignPtr# :: VulkanMarshalPrim a => a -> IO (ForeignPtr a)
toPlainForeignPtr# x
  | a <- unsafeAddr x
  , b <- unsafeByteArray x = IO
    (\s -> (# s, ForeignPtr a (PlainPtr (unsafeCoerce# b)) #))
{-# INLINE toPlainForeignPtr# #-}

-- | Make sure the region of memory is not collected at this moment in time.
touchVkData# :: VulkanMarshalPrim a => a -> IO ()
touchVkData# a = IO (\s -> (# touch# (unsafeByteArray a) s, () #))
{-# INLINE touchVkData# #-}


newVkData# :: forall a
            . (Storable a, VulkanMarshalPrim a)
           => (Ptr a -> IO ()) -> IO a
newVkData# f
  | I# n <- sizeOf (undefined :: a)
  , I# a <- alignment (undefined :: a)
  = IO
  (\s0 -> case newAlignedPinnedByteArray# n a s0 of
    (# s1, mba #) -> case unsafeFreezeByteArray# mba s1 of
      (# s2, ba #) -> case f (Ptr (byteArrayContents# ba)) of
        IO k -> case k s2 of
          (# s3, () #) -> (# s3, unsafeFromByteArrayOffset 0# ba #)
  )
{-# INLINE newVkData# #-}

mallocVkData# :: forall a
             . (Storable a, VulkanMarshalPrim a)
            => IO a
mallocVkData#
  | I# n <- sizeOf (undefined :: a)
  , I# a <- alignment (undefined :: a)
  = IO
  (\s0 -> case newAlignedPinnedByteArray# n a s0 of
    (# s1, mba #) -> case unsafeFreezeByteArray# mba s1 of
      (# s2, ba #) -> (# s2, unsafeFromByteArrayOffset 0# ba #)
  )
{-# INLINE mallocVkData# #-}

mallocVkDataArray# :: forall a
                    . (Storable a, VulkanMarshalPrim a)
                   => Int -> IO (Ptr a, [a])
mallocVkDataArray# (I# m)
  | I# n <- sizeOf (undefined :: a)
  , I# a <- alignment (undefined :: a)
  , nm <- n *# m
  = IO
  (\s0 -> case newAlignedPinnedByteArray# nm a s0 of
    (# s1, mba #) -> case unsafeFreezeByteArray# mba s1 of
      (# s2, ba #) ->
        (# s2
        , ( Ptr (byteArrayContents# ba)
          , let go k | isTrue# (k >=# nm) = []
                     | otherwise = unsafeFromByteArrayOffset k ba
                                 : go (k +# n)
            in go 0#
          )
        #)
  )
{-# INLINE mallocVkDataArray# #-}


peekVkData# :: forall a
             . (Storable a, VulkanMarshalPrim a)
            => Ptr a -> IO a
peekVkData# (Ptr addr)
  | I# n <- sizeOf (undefined :: a)
  , I# a <- alignment (undefined :: a)
  = IO
  (\s -> case newAlignedPinnedByteArray# n a s of
    (# s1, mba #) -> case copyAddrToByteArray# addr mba 0# n s1 of
      s2 -> case unsafeFreezeByteArray# mba s2 of
        (# s3, ba #) -> (# s3, unsafeFromByteArrayOffset 0# ba #)
  )
{-# INLINE peekVkData# #-}

pokeVkData# :: forall a
             . (Storable a, VulkanMarshalPrim a)
            => Ptr a -> a -> IO ()
pokeVkData# (Ptr addr) x
  = c_memcpy addr (unsafeAddr x) (fromIntegral $ sizeOf x)
{-# INLINE pokeVkData# #-}


-- | Internal function used to implement Eq and Ord instances for Vulkan structs.
--   Compares first n bytes of two memory areas.
--
--   Uses lexicographic ordering (c memcmp inside).
--
--   This is a helper that should be used in VulkanMarshal instances only.
cmpBytes# :: Int -> Addr# -> Addr# -> Ordering
cmpBytes# n a b
  | isTrue# (eqAddr# a b) = EQ
  | otherwise = c_memcmp a b (fromIntegral n) `compare` 0


foreign import ccall unsafe "memcmp"
  c_memcmp :: Addr# -> Addr# -> CSize -> CInt


foreign import ccall unsafe "memcpy"
  c_memcpy :: Addr# -> Addr# -> CSize -> IO ()
