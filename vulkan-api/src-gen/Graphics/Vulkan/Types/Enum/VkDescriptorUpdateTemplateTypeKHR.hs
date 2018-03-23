{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkDescriptorUpdateTemplateTypeKHR
       (VkDescriptorUpdateTemplateTypeKHR(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkDescriptorUpdateTemplateTypeKHR = VkDescriptorUpdateTemplateTypeKHR VkFlags
                                              deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                        FiniteBits, Storable, Real, Data, Generic)

instance Show VkDescriptorUpdateTemplateTypeKHR where
        {-# INLINE show #-}
        show (VkDescriptorUpdateTemplateTypeKHR x) = show x

instance Read VkDescriptorUpdateTemplateTypeKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
