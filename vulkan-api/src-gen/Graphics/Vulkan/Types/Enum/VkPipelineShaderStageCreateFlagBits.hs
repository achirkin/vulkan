{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkPipelineShaderStageCreateFlagBits
       (VkPipelineShaderStageCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkPipelineShaderStageCreateFlagBits = VkPipelineShaderStageCreateFlagBits VkFlags
                                                deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                          Bits, FiniteBits, Storable, Real, Data,
                                                          Generic)

instance Show VkPipelineShaderStageCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineShaderStageCreateFlagBits x) = show x

instance Read VkPipelineShaderStageCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
