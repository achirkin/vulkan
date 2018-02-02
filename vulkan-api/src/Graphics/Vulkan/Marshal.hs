{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE UnboxedTuples    #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- | This module is not part of auto-generated code based on vk.xml.
--   Instead, it is hand-written to provide common types and classes.
module Graphics.Vulkan.Marshal
  ( VulkanMarshal (..)
  , Mutable (..)
  , unsafeRef, newVkData, fromPtrFun, allocaVkData
  , pokeMutable, peekMutable
  , touchMutable, addMutableFinalizer
  , touchImmutableContent, addImmutableContentFinalizer
  , cmpImmutableContent
  ) where

import           Data.Void        (Void)
import           GHC.Word         (Word64 (..))
import           Foreign.C.Types  (CInt (..), CSize (..))
import           Foreign.Storable (Storable (..))
import           GHC.Base         (isTrue#, runRW#)
import           GHC.Prim
import           GHC.Ptr          (Ptr (..))
import           GHC.Types        (IO (..), Int (..))

-- | All Vulkan structures are stored as-is in byte arrays to avoid any overheads
--   for wrapping and unwrapping haskell values.
--   VulkanMarshal provides an interfaces to write and read these structures
--   in an imperative way.
class Storable a => VulkanMarshal a where
  -- | Copy data into an immutable byte array
  freeze       :: Mutable a -> IO a
  -- | Freeze without copying;
  --   a mutable version must not be updated after this to keep data valid.
  unsafeFreeze :: Mutable a -> IO a
  -- | Copy data into a mutable byte array
  thaw         :: a -> IO (Mutable a)
  -- | Thaw without copying;
  --   an immutable version contains an invalid data after its mutable version
  --   is updated.
  unsafeThaw   :: a -> IO (Mutable a)
  -- | Make sure this data is alive at a given point in a sequence of IO actions.
  touchVkData  :: a -> IO ()
  -- | Execute an action when immutable data is garbage-collected.
  addVkDataFinalizer :: a -> IO () -> IO ()

-- | A version of Vulkan structure with `MutableByteArray#`
--   in place of immutable `ByteArray#`.
data Mutable a = Mutable# (MutableByteArray# RealWorld)

instance Storable a => Storable (Mutable a) where
  sizeOf = sizeOf . dummyImmutable
  {-# INLINE sizeOf #-}
  alignment = alignment . dummyImmutable
  {-# INLINE alignment #-}
  peek = peekDo undefined
    where
      peekDo :: Storable a' => a' -> Ptr a -> IO (Mutable a')
      peekDo ~x (Ptr addr) = case (# sizeOf x, alignment x #) of
        (# I# size, I# align #) ->
          IO (\s -> case newAlignedPinnedByteArray# size align s of
               (# s', mba #) -> case copyAddrToByteArray# addr mba 0# size s' of
                 s'' -> (# s'' , Mutable# mba #)
         )
      {-# INLINE peekDo #-}
  {-# INLINE peek #-}
  poke (Ptr addr) (Mutable# mba)
    = IO (\s -> case getSizeofMutableByteArray# mba s of
           (# s1, size #) ->
             (# copyMutableByteArrayToAddr# mba 0# addr size s1, () #)
         )
  {-# INLINE poke #-}


dummyImmutable :: Mutable a -> a
dummyImmutable ~_ = undefined
{-# INLINE dummyImmutable #-}

-- | Get a pointer referencing the mutable data without copying.
--   Mutable byte array inside the data must be pinned to make sure
--   GC does not move it from current location.
--   Also, the pointer must be used only as long as the data inside is alive
--   (i.e. is referenced by any other live Haskell object).
unsafeRef  :: Mutable a -> Ptr a
unsafeRef (Mutable# mba) = case runRW# (unsafeFreezeByteArray# mba) of
  (# _, ba #) -> Ptr (byteArrayContents# ba)
{-# INLINE unsafeRef #-}

-- | Storable 'poke' on mutable data
pokeMutable :: VulkanMarshal a => Ptr a -> Mutable a -> IO ()
pokeMutable ptr = poke (unsafeCoerce# ptr)

-- | Storable 'peek' on mutable data
peekMutable :: VulkanMarshal a => Ptr a -> IO (Mutable a)
peekMutable ptr = peek (unsafeCoerce# ptr)

-- | Allocate memory for a new vulkan object using `Storable` instance
--   to find out aligment and size.
newVkData :: Storable a => IO (Mutable a)
newVkData = newVkDataDo undefined
  where
    newVkDataDo :: Storable a' => a' -> IO (Mutable a')
    newVkDataDo ~x = case (# sizeOf x, alignment x #) of
      (# I# size, I# align #) ->
        IO (\s -> case newAlignedPinnedByteArray# size align s of
             (# s', mba #) -> (# s', Mutable# mba #)
       )
    {-# INLINE newVkDataDo #-}
{-# INLINE newVkData #-}

-- | Useful helper function that allocates memory in the garbage-collected region
--   to be filled in by some Vulkan command.
--
--   Simplified version of allocaVkData.
fromPtrFun :: VulkanMarshal a
           => (Ptr a -> IO ()) -> IO a
fromPtrFun = fmap fst . allocaVkData . const
{-# INLINE fromPtrFun #-}

-- | Useful helper function that allocates memory in the garbage-collected region
--   to be filled in by some Vulkan command.
allocaVkData :: VulkanMarshal a
             => (Mutable a -> Ptr a -> IO b) -> IO ( a, b )
allocaVkData = allocaVkDataDo undefined
  where
    allocaVkDataDo :: VulkanMarshal a'
                   => a' -> (Mutable a' -> Ptr a' -> IO b') -> IO ( a', b' )
    allocaVkDataDo ~dummy action = do
       (ma, r) <- allocaVkData' (sizeOf dummy) (alignment dummy) action
       a <- unsafeFreeze ma
       return (a, r)
    {-# INLINE allocaVkDataDo #-}
{-# INLINE allocaVkData #-}


allocaVkData' ::
    Int -> Int -> (Mutable a -> Ptr a -> IO b) -> IO ( Mutable a, b )
allocaVkData' (I# size) (I# align) action = IO $ \ s0 ->
     case newAlignedPinnedByteArray# size align s0 of { (# s1, mbarr# #) ->
     case unsafeFreezeByteArray# mbarr# s1 of { (# s2, barr#  #) ->
     let ma   = Mutable# mbarr#
         addr = Ptr (byteArrayContents# barr#) in
     case action ma addr     of { IO action' ->
     case action' s2      of { (# s3, r #) ->
     case touch# barr# s3 of { s4 ->
     (# s4, ( ma, r ) #)
  }}}}}
-- Based on Foreign.Marshal.Alloc
-- See Note [NOINLINE for touch#] in that file.
{-# NOINLINE allocaVkData' #-}


-- | Make sure this data is alive at a given point in a sequence of IO actions.
--   This is a helper that should be used in VulkanMarshal instances only.
touchImmutableContent :: a -> ByteArray# -> IO ()
touchImmutableContent a ba = IO (\s -> (# touch# a (touch# ba s), () #) )
{-# NOINLINE touchImmutableContent #-}

-- | Execute an action when immutable data is garbage-collected.
--   This is a helper that should be used in VulkanMarshal instances only.
addImmutableContentFinalizer :: a -> IO () -> IO ()
addImmutableContentFinalizer a (IO finalizer) = IO
  (\s -> case mkWeak# a a finalizer s of (# s', _ #) -> (# s', () #))
{-# NOINLINE addImmutableContentFinalizer #-}

-- | Make sure this data is alive at a given point in a sequence of IO actions.
touchMutable :: Mutable a -> IO ()
touchMutable m@(Mutable# mba) = IO (\s -> (# touch# m (touch# mba s), () #) )
{-# NOINLINE touchMutable #-}

-- | Execute an action when mutable data is garbage-collected.
addMutableFinalizer :: Mutable a -> IO () -> IO ()
addMutableFinalizer m@(Mutable# _) (IO finalizer) = IO
  (\s -> case mkWeak# m m finalizer s of (# s', _ #) -> (# s', () #))
{-# NOINLINE addMutableFinalizer #-}


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


instance VulkanMarshal (Ptr a) where
  freeze (Mutable# mba) = IO
    (\s -> case readAddrArray# mba 0# s of (# s', a #) -> (# s', Ptr a #))
  {-# INLINE freeze #-}
  unsafeFreeze = freeze
  {-# INLINE unsafeFreeze #-}
  thaw x@(Ptr a)
    | I# align <- alignment x
    , I# size  <- sizeOf x
    = IO
    (\s -> case newAlignedPinnedByteArray# size align s of
      (# s', mba #) -> (# writeAddrArray# mba 0# a s', Mutable# mba #)
    )
  {-# INLINE thaw #-}
  unsafeThaw = thaw
  {-# INLINE unsafeThaw #-}
  touchVkData x = IO (\s -> (# touch# x s, () #) )
  {-# NOINLINE touchVkData #-}
  addVkDataFinalizer x (IO finalizer) = IO
    (\s -> case mkWeak# x x finalizer s of (# s', _ #) -> (# s', () #))
  {-# NOINLINE addVkDataFinalizer #-}


instance VulkanMarshal Word64 where
  freeze (Mutable# mba) = IO
    (\s -> case readWord64Array# mba 0# s of (# s', a #) -> (# s', W64# a #))
  {-# INLINE freeze #-}
  unsafeFreeze = freeze
  {-# INLINE unsafeFreeze #-}
  thaw x@(W64# a)
    | I# align <- alignment x
    , I# size  <- sizeOf x
    = IO
    (\s -> case newAlignedPinnedByteArray# size align s of
      (# s', mba #) -> (# writeWord64Array# mba 0# a s', Mutable# mba #)
    )
  {-# INLINE thaw #-}
  unsafeThaw = thaw
  {-# INLINE unsafeThaw #-}
  touchVkData x = IO (\s -> (# touch# x s, () #) )
  {-# NOINLINE touchVkData #-}
  addVkDataFinalizer x (IO finalizer) = IO
    (\s -> case mkWeak# x x finalizer s of (# s', _ #) -> (# s', () #))
  {-# NOINLINE addVkDataFinalizer #-}
