{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlagBitsKHR
       (VkExternalFenceHandleTypeFlagBitsKHR(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkExternalFenceHandleTypeFlagBitsKHR = VkExternalFenceHandleTypeFlagBitsKHR VkFlags
                                                 deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                           Bits, FiniteBits, Storable, Real, Data,
                                                           Generic)

instance Show VkExternalFenceHandleTypeFlagBitsKHR where
        {-# INLINE show #-}
        show (VkExternalFenceHandleTypeFlagBitsKHR x) = show x

instance Read VkExternalFenceHandleTypeFlagBitsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
