{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.FramebufferCreateFlagBits
       (VkFramebufferCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Foreign.Storable                (Storable)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkFramebufferCreateFlagBits = VkFramebufferCreateFlagBits VkFlags
                                      deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkFramebufferCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkFramebufferCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
