{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkPipelineInputAssemblyStateCreateFlagBits
       (VkPipelineInputAssemblyStateCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkPipelineInputAssemblyStateCreateFlagBits = VkPipelineInputAssemblyStateCreateFlagBits VkFlags
                                                       deriving (Eq, Ord, Num, Bounded, Enum,
                                                                 Integral, Bits, FiniteBits,
                                                                 Storable, Real, Data, Generic)

instance Show VkPipelineInputAssemblyStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineInputAssemblyStateCreateFlagBits x) = show x

instance Read VkPipelineInputAssemblyStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
