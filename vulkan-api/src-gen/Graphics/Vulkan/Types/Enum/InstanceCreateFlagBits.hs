{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.InstanceCreateFlagBits
       (VkInstanceCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkInstanceCreateFlagBits = VkInstanceCreateFlagBits VkFlags
                                     deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                               FiniteBits, Storable, Real, Data, Generic)

instance Show VkInstanceCreateFlagBits where
        {-# INLINE show #-}
        show (VkInstanceCreateFlagBits x) = show x

instance Read VkInstanceCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
