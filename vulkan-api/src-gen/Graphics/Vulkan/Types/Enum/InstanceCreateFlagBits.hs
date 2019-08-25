{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.InstanceCreateFlagBits
       (VkInstanceCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Foreign.Storable                (Storable)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkInstanceCreateFlagBits = VkInstanceCreateFlagBits VkFlags
                                   deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkInstanceCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkInstanceCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
