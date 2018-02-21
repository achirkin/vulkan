{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkQueryPoolCreateFlagBits
       (VkQueryPoolCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkQueryPoolCreateFlagBits = VkQueryPoolCreateFlagBits VkFlags
                                      deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                FiniteBits, Storable, Real, Data, Generic)

instance Show VkQueryPoolCreateFlagBits where
        {-# INLINE show #-}
        show (VkQueryPoolCreateFlagBits x) = show x

instance Read VkQueryPoolCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
