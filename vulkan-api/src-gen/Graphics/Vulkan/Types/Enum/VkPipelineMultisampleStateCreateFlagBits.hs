{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkPipelineMultisampleStateCreateFlagBits
       (VkPipelineMultisampleStateCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkPipelineMultisampleStateCreateFlagBits = VkPipelineMultisampleStateCreateFlagBits VkFlags
                                                     deriving (Eq, Ord, Num, Bounded, Enum,
                                                               Integral, Bits, FiniteBits, Storable,
                                                               Real, Data, Generic)

instance Show VkPipelineMultisampleStateCreateFlagBits where
        {-# INLINE show #-}
        show (VkPipelineMultisampleStateCreateFlagBits x) = show x

instance Read VkPipelineMultisampleStateCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
