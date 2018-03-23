{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkExternalFenceFeatureFlagBitsKHR
       (VkExternalFenceFeatureFlagBitsKHR(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkExternalFenceFeatureFlagBitsKHR = VkExternalFenceFeatureFlagBitsKHR VkFlags
                                              deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                        FiniteBits, Storable, Real, Data, Generic)

instance Show VkExternalFenceFeatureFlagBitsKHR where
        {-# INLINE show #-}
        show (VkExternalFenceFeatureFlagBitsKHR x) = show x

instance Read VkExternalFenceFeatureFlagBitsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
