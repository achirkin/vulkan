{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkPipelineVertexInputStateCreateFlagBits
       (VkPipelineVertexInputStateCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkPipelineVertexInputStateCreateFlagBits = VkPipelineVertexInputStateCreateFlagBits VkFlags
                                                     deriving (Eq, Ord, Num, Bounded, Enum,
                                                               Integral, Bits, FiniteBits, Storable,
                                                               Real, Data, Generic)

instance Show VkPipelineVertexInputStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineVertexInputStateCreateFlagBits x) = show x

instance Read VkPipelineVertexInputStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
