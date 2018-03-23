{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkMemoryAllocateFlagBitsKHR
       (VkMemoryAllocateFlagBitsKHR(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkMemoryAllocateFlagBitsKHR = VkMemoryAllocateFlagBitsKHR VkFlags
                                        deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                  FiniteBits, Storable, Real, Data, Generic)

instance Show VkMemoryAllocateFlagBitsKHR where
        {-# INLINE show #-}
        show (VkMemoryAllocateFlagBitsKHR x) = show x

instance Read VkMemoryAllocateFlagBitsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
