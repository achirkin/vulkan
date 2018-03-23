{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkFenceImportFlagBitsKHR
       (VkFenceImportFlagBitsKHR(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkFenceImportFlagBitsKHR = VkFenceImportFlagBitsKHR VkFlags
                                     deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                               FiniteBits, Storable, Real, Data, Generic)

instance Show VkFenceImportFlagBitsKHR where
        {-# INLINE show #-}
        show (VkFenceImportFlagBitsKHR x) = show x

instance Read VkFenceImportFlagBitsKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
