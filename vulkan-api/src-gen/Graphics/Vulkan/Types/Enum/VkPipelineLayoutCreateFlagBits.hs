{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkPipelineLayoutCreateFlagBits
       (VkPipelineLayoutCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkPipelineLayoutCreateFlagBits = VkPipelineLayoutCreateFlagBits VkFlags
                                           deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                     FiniteBits, Storable, Real, Data, Generic)

instance Show VkPipelineLayoutCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineLayoutCreateFlagBits x) = show x

instance Read VkPipelineLayoutCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
