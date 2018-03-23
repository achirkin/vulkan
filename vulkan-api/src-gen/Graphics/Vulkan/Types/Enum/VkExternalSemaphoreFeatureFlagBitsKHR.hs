{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkExternalSemaphoreFeatureFlagBitsKHR
       (VkExternalSemaphoreFeatureFlagBitsKHR(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkExternalSemaphoreFeatureFlagBitsKHR = VkExternalSemaphoreFeatureFlagBitsKHR VkFlags
                                                  deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                            Bits, FiniteBits, Storable, Real, Data,
                                                            Generic)

instance Show VkExternalSemaphoreFeatureFlagBitsKHR where
        {-# INLINE show #-}
        show (VkExternalSemaphoreFeatureFlagBitsKHR x) = show x

instance Read VkExternalSemaphoreFeatureFlagBitsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
