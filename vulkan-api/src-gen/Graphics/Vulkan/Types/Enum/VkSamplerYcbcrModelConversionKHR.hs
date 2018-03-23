{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkSamplerYcbcrModelConversionKHR
       (VkSamplerYcbcrModelConversionKHR(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkSamplerYcbcrModelConversionKHR = VkSamplerYcbcrModelConversionKHR VkFlags
                                             deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                       FiniteBits, Storable, Real, Data, Generic)

instance Show VkSamplerYcbcrModelConversionKHR where
        {-# INLINE show #-}
        show (VkSamplerYcbcrModelConversionKHR x) = show x

instance Read VkSamplerYcbcrModelConversionKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
