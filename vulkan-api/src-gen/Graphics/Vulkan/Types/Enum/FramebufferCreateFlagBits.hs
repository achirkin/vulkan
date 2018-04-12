{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.FramebufferCreateFlagBits
       (VkFramebufferCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkFramebufferCreateFlagBits = VkFramebufferCreateFlagBits VkFlags
                                        deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                  FiniteBits, Storable, Real, Data, Generic)

instance Show VkFramebufferCreateFlagBits where
        {-# INLINE show #-}
        show (VkFramebufferCreateFlagBits x) = show x

instance Read VkFramebufferCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
