{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkSamplerYcbcrRangeKHR
       (VkSamplerYcbcrRangeKHR(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkSamplerYcbcrRangeKHR = VkSamplerYcbcrRangeKHR VkFlags
                                   deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                             FiniteBits, Storable, Real, Data, Generic)

instance Show VkSamplerYcbcrRangeKHR where
        {-# INLINE show #-}
        show (VkSamplerYcbcrRangeKHR x) = show x

instance Read VkSamplerYcbcrRangeKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
