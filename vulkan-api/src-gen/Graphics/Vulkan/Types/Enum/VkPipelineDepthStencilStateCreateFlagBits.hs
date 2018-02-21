{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkPipelineDepthStencilStateCreateFlagBits
       (VkPipelineDepthStencilStateCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkPipelineDepthStencilStateCreateFlagBits = VkPipelineDepthStencilStateCreateFlagBits VkFlags
                                                      deriving (Eq, Ord, Num, Bounded, Enum,
                                                                Integral, Bits, FiniteBits,
                                                                Storable, Real, Data, Generic)

instance Show VkPipelineDepthStencilStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineDepthStencilStateCreateFlagBits x) = show x

instance Read VkPipelineDepthStencilStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
