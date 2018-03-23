{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkSemaphoreImportFlagBitsKHR
       (VkSemaphoreImportFlagBitsKHR(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkSemaphoreImportFlagBitsKHR = VkSemaphoreImportFlagBitsKHR VkFlags
                                         deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                   FiniteBits, Storable, Real, Data, Generic)

instance Show VkSemaphoreImportFlagBitsKHR where
        {-# INLINE show #-}
        show (VkSemaphoreImportFlagBitsKHR x) = show x

instance Read VkSemaphoreImportFlagBitsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
