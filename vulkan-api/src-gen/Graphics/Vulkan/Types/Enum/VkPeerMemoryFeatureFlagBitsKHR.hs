{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkPeerMemoryFeatureFlagBitsKHR
       (VkPeerMemoryFeatureFlagBitsKHR(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkPeerMemoryFeatureFlagBitsKHR = VkPeerMemoryFeatureFlagBitsKHR VkFlags
                                           deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                     FiniteBits, Storable, Real, Data, Generic)

instance Show VkPeerMemoryFeatureFlagBitsKHR where
        {-# INLINE show #-}
        show (VkPeerMemoryFeatureFlagBitsKHR x) = show x

instance Read VkPeerMemoryFeatureFlagBitsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
