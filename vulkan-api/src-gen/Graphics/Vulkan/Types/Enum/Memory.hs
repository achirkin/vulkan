{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.Memory
       (VkMemoryAllocateFlagBitsKHR(..),
        VkMemoryAllocateBitmask(VkMemoryAllocateBitmask,
                                VkMemoryAllocateFlags, VkMemoryAllocateFlagBits,
                                VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT),
        VkMemoryAllocateFlags, VkMemoryAllocateFlagBits,
        VkMemoryHeapBitmask(VkMemoryHeapBitmask, VkMemoryHeapFlags,
                            VkMemoryHeapFlagBits, VK_MEMORY_HEAP_DEVICE_LOCAL_BIT),
        VkMemoryHeapFlags, VkMemoryHeapFlagBits,
        VkMemoryPropertyBitmask(VkMemoryPropertyBitmask,
                                VkMemoryPropertyFlags, VkMemoryPropertyFlagBits,
                                VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
                                VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT,
                                VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
                                VK_MEMORY_PROPERTY_HOST_CACHED_BIT,
                                VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT),
        VkMemoryPropertyFlags, VkMemoryPropertyFlagBits)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkMemoryAllocateFlagBitsKHR = VkMemoryAllocateFlagBitsKHR VkFlags
                                      deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkMemoryAllocateFlagBitsKHR where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkMemoryAllocateFlagBitsKHR where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)

newtype VkMemoryAllocateBitmask (a ::
                                   FlagType) = VkMemoryAllocateBitmask VkFlags
                                               deriving (Eq, Ord, Storable)

type VkMemoryAllocateFlags = VkMemoryAllocateBitmask FlagMask

type VkMemoryAllocateFlagBits = VkMemoryAllocateBitmask FlagBit

pattern VkMemoryAllocateFlagBits ::
        VkFlags -> VkMemoryAllocateBitmask FlagBit

pattern VkMemoryAllocateFlagBits n = VkMemoryAllocateBitmask n

pattern VkMemoryAllocateFlags ::
        VkFlags -> VkMemoryAllocateBitmask FlagMask

pattern VkMemoryAllocateFlags n = VkMemoryAllocateBitmask n

deriving instance Bits (VkMemoryAllocateBitmask FlagMask)

deriving instance FiniteBits (VkMemoryAllocateBitmask FlagMask)

instance Show (VkMemoryAllocateBitmask a) where
    showsPrec _ VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT
      = showString "VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT"
    showsPrec p (VkMemoryAllocateBitmask x)
      = showParen (p >= 11)
          (showString "VkMemoryAllocateBitmask " . showsPrec 11 x)

instance Read (VkMemoryAllocateBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT",
               pure VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT)]
             +++
             prec 10
               (expectP (Ident "VkMemoryAllocateBitmask") >>
                  (VkMemoryAllocateBitmask <$> step readPrec)))

-- | Force allocation on specific devices
--
--   bitpos = @0@
pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT ::
        VkMemoryAllocateBitmask a

pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT =
        VkMemoryAllocateBitmask 1

newtype VkMemoryHeapBitmask (a ::
                               FlagType) = VkMemoryHeapBitmask VkFlags
                                           deriving (Eq, Ord, Storable)

type VkMemoryHeapFlags = VkMemoryHeapBitmask FlagMask

type VkMemoryHeapFlagBits = VkMemoryHeapBitmask FlagBit

pattern VkMemoryHeapFlagBits ::
        VkFlags -> VkMemoryHeapBitmask FlagBit

pattern VkMemoryHeapFlagBits n = VkMemoryHeapBitmask n

pattern VkMemoryHeapFlags ::
        VkFlags -> VkMemoryHeapBitmask FlagMask

pattern VkMemoryHeapFlags n = VkMemoryHeapBitmask n

deriving instance Bits (VkMemoryHeapBitmask FlagMask)

deriving instance FiniteBits (VkMemoryHeapBitmask FlagMask)

instance Show (VkMemoryHeapBitmask a) where
    showsPrec _ VK_MEMORY_HEAP_DEVICE_LOCAL_BIT
      = showString "VK_MEMORY_HEAP_DEVICE_LOCAL_BIT"
    showsPrec p (VkMemoryHeapBitmask x)
      = showParen (p >= 11)
          (showString "VkMemoryHeapBitmask " . showsPrec 11 x)

instance Read (VkMemoryHeapBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_MEMORY_HEAP_DEVICE_LOCAL_BIT",
               pure VK_MEMORY_HEAP_DEVICE_LOCAL_BIT)]
             +++
             prec 10
               (expectP (Ident "VkMemoryHeapBitmask") >>
                  (VkMemoryHeapBitmask <$> step readPrec)))

-- | If set, heap represents device memory
--
--   bitpos = @0@
pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT :: VkMemoryHeapBitmask a

pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = VkMemoryHeapBitmask 1

newtype VkMemoryPropertyBitmask (a ::
                                   FlagType) = VkMemoryPropertyBitmask VkFlags
                                               deriving (Eq, Ord, Storable)

type VkMemoryPropertyFlags = VkMemoryPropertyBitmask FlagMask

type VkMemoryPropertyFlagBits = VkMemoryPropertyBitmask FlagBit

pattern VkMemoryPropertyFlagBits ::
        VkFlags -> VkMemoryPropertyBitmask FlagBit

pattern VkMemoryPropertyFlagBits n = VkMemoryPropertyBitmask n

pattern VkMemoryPropertyFlags ::
        VkFlags -> VkMemoryPropertyBitmask FlagMask

pattern VkMemoryPropertyFlags n = VkMemoryPropertyBitmask n

deriving instance Bits (VkMemoryPropertyBitmask FlagMask)

deriving instance FiniteBits (VkMemoryPropertyBitmask FlagMask)

instance Show (VkMemoryPropertyBitmask a) where
    showsPrec _ VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
      = showString "VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT"
    showsPrec _ VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
      = showString "VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT"
    showsPrec _ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
      = showString "VK_MEMORY_PROPERTY_HOST_COHERENT_BIT"
    showsPrec _ VK_MEMORY_PROPERTY_HOST_CACHED_BIT
      = showString "VK_MEMORY_PROPERTY_HOST_CACHED_BIT"
    showsPrec _ VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT
      = showString "VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT"
    showsPrec p (VkMemoryPropertyBitmask x)
      = showParen (p >= 11)
          (showString "VkMemoryPropertyBitmask " . showsPrec 11 x)

instance Read (VkMemoryPropertyBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT",
               pure VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
              ("VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT",
               pure VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
              ("VK_MEMORY_PROPERTY_HOST_COHERENT_BIT",
               pure VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
              ("VK_MEMORY_PROPERTY_HOST_CACHED_BIT",
               pure VK_MEMORY_PROPERTY_HOST_CACHED_BIT),
              ("VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT",
               pure VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT)]
             +++
             prec 10
               (expectP (Ident "VkMemoryPropertyBitmask") >>
                  (VkMemoryPropertyBitmask <$> step readPrec)))

-- | If otherwise stated, then allocate memory on device
--
--   bitpos = @0@
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT ::
        VkMemoryPropertyBitmask a

pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT =
        VkMemoryPropertyBitmask 1

-- | Memory is mappable by host
--
--   bitpos = @1@
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ::
        VkMemoryPropertyBitmask a

pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT =
        VkMemoryPropertyBitmask 2

-- | Memory will have i/o coherency. If not set, application may need to use vkFlushMappedMemoryRanges and vkInvalidateMappedMemoryRanges to flush/invalidate host cache
--
--   bitpos = @2@
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ::
        VkMemoryPropertyBitmask a

pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT =
        VkMemoryPropertyBitmask 4

-- | Memory will be cached by the host
--
--   bitpos = @3@
pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT ::
        VkMemoryPropertyBitmask a

pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT =
        VkMemoryPropertyBitmask 8

-- | Memory may be allocated by the driver when it is required
--
--   bitpos = @4@
pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT ::
        VkMemoryPropertyBitmask a

pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT =
        VkMemoryPropertyBitmask 16
