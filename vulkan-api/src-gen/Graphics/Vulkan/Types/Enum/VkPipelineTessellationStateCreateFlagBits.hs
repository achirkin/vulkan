{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkPipelineTessellationStateCreateFlagBits
       (VkPipelineTessellationStateCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkPipelineTessellationStateCreateFlagBits = VkPipelineTessellationStateCreateFlagBits VkFlags
                                                      deriving (Eq, Ord, Num, Bounded, Enum,
                                                                Integral, Bits, FiniteBits,
                                                                Storable, Real, Data, Generic)

instance Show VkPipelineTessellationStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineTessellationStateCreateFlagBits x) = show x

instance Read VkPipelineTessellationStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
