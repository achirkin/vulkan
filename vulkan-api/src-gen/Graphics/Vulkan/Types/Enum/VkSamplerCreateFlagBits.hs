{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkSamplerCreateFlagBits
       (VkSamplerCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkSamplerCreateFlagBits = VkSamplerCreateFlagBits VkFlags
                                    deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                              FiniteBits, Storable, Real, Data, Generic)

instance Show VkSamplerCreateFlagBits where
        {-# INLINE show #-}
        show (VkSamplerCreateFlagBits x) = show x

instance Read VkSamplerCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
