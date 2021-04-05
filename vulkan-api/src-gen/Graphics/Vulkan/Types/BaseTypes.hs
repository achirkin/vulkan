{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.BaseTypes
       (AHardwareBuffer(), ANativeWindow(), CAMetalLayer(), VkBool32(..),
        VkDeviceAddress(..), VkDeviceSize(..), VkFlags(..),
        VkSampleMask(..))
       where
import Data.Bits               (Bits, FiniteBits)
import Data.Coerce             (coerce)
import Foreign.Storable        (Storable)
import Graphics.Vulkan.Marshal (Word32, Word64)

-- | > struct AHardwareBuffer;
data AHardwareBuffer

-- | > struct ANativeWindow;
data ANativeWindow

-- | >
--   > #ifdef __OBJC__
--   > @class CAMetalLayer;
--   > #else
--   > typedef void CAMetalLayer;
--   > #endif
data CAMetalLayer

newtype VkBool32 = VkBool32 Word32
                   deriving (Eq, Ord, Num, Bounded, Enum, Integral, Real, Bits,
                             FiniteBits, Storable)

instance Show VkBool32 where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> Word32 -> ShowS)

instance Read VkBool32 where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS Word32)

newtype VkDeviceAddress = VkDeviceAddress Word64
                          deriving (Eq, Ord, Num, Bounded, Enum, Integral, Real, Bits,
                                    FiniteBits, Storable)

instance Show VkDeviceAddress where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> Word64 -> ShowS)

instance Read VkDeviceAddress where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS Word64)

newtype VkDeviceSize = VkDeviceSize Word64
                       deriving (Eq, Ord, Num, Bounded, Enum, Integral, Real, Bits,
                                 FiniteBits, Storable)

instance Show VkDeviceSize where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> Word64 -> ShowS)

instance Read VkDeviceSize where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS Word64)

newtype VkFlags = VkFlags Word32
                  deriving (Eq, Ord, Num, Bounded, Enum, Integral, Real, Bits,
                            FiniteBits, Storable)

instance Show VkFlags where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> Word32 -> ShowS)

instance Read VkFlags where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS Word32)

newtype VkSampleMask = VkSampleMask Word32
                       deriving (Eq, Ord, Num, Bounded, Enum, Integral, Real, Bits,
                                 FiniteBits, Storable)

instance Show VkSampleMask where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> Word32 -> ShowS)

instance Read VkSampleMask where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS Word32)
