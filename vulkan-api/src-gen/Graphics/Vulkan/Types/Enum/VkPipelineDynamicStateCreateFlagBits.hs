{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkPipelineDynamicStateCreateFlagBits
       (VkPipelineDynamicStateCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkPipelineDynamicStateCreateFlagBits = VkPipelineDynamicStateCreateFlagBits VkFlags
                                                 deriving (Eq, Ord, Num, Bounded, Enum, Integral,
                                                           Bits, FiniteBits, Storable, Real, Data,
                                                           Generic)

instance Show VkPipelineDynamicStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineDynamicStateCreateFlagBits x) = show x

instance Read VkPipelineDynamicStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
