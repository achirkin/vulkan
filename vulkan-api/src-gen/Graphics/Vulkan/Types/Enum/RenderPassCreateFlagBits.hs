{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.RenderPassCreateFlagBits
       (VkRenderPassCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Foreign.Storable                (Storable)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkRenderPassCreateFlagBits = VkRenderPassCreateFlagBits VkFlags
                                     deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkRenderPassCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkRenderPassCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
