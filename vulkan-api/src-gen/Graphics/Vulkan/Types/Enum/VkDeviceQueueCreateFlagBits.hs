{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkDeviceQueueCreateFlagBits
       (VkDeviceQueueCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkDeviceQueueCreateFlagBits = VkDeviceQueueCreateFlagBits VkFlags
                                        deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                  FiniteBits, Storable, Real, Data, Generic)

instance Show VkDeviceQueueCreateFlagBits where
        {-# INLINE show #-}
        show (VkDeviceQueueCreateFlagBits x) = show x

instance Read VkDeviceQueueCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
