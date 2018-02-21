{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkPipelineRasterizationStateCreateFlagBits
       (VkPipelineRasterizationStateCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkPipelineRasterizationStateCreateFlagBits = VkPipelineRasterizationStateCreateFlagBits VkFlags
                                                       deriving (Eq, Ord, Num, Bounded, Enum,
                                                                 Integral, Bits, FiniteBits,
                                                                 Storable, Real, Data, Generic)

instance Show VkPipelineRasterizationStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineRasterizationStateCreateFlagBits x) = show x

instance Read VkPipelineRasterizationStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
