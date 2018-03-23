{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlagBitsKHR
       (VkExternalMemoryFeatureFlagBitsKHR(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkExternalMemoryFeatureFlagBitsKHR = VkExternalMemoryFeatureFlagBitsKHR VkFlags
                                               deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                         Bits, FiniteBits, Storable, Real, Data,
                                                         Generic)

instance Show VkExternalMemoryFeatureFlagBitsKHR where
        {-# INLINE show #-}
        show (VkExternalMemoryFeatureFlagBitsKHR x) = show x

instance Read VkExternalMemoryFeatureFlagBitsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
