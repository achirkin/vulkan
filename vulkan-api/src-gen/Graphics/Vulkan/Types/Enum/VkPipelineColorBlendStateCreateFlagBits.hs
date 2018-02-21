{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkPipelineColorBlendStateCreateFlagBits
       (VkPipelineColorBlendStateCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkPipelineColorBlendStateCreateFlagBits = VkPipelineColorBlendStateCreateFlagBits VkFlags
                                                    deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                              Bits, FiniteBits, Storable, Real,
                                                              Data, Generic)

instance Show VkPipelineColorBlendStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineColorBlendStateCreateFlagBits x) = show x

instance Read VkPipelineColorBlendStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
