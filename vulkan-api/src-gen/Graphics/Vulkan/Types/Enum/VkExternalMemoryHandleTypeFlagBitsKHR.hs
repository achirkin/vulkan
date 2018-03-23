{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagBitsKHR
       (VkExternalMemoryHandleTypeFlagBitsKHR(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkExternalMemoryHandleTypeFlagBitsKHR = VkExternalMemoryHandleTypeFlagBitsKHR VkFlags
                                                  deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                            Bits, FiniteBits, Storable, Real, Data,
                                                            Generic)

instance Show VkExternalMemoryHandleTypeFlagBitsKHR where
        {-# INLINE show #-}
        show (VkExternalMemoryHandleTypeFlagBitsKHR x) = show x

instance Read VkExternalMemoryHandleTypeFlagBitsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
