{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkBufferViewCreateFlagBits
       (VkBufferViewCreateFlagBits(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkBufferViewCreateFlagBits = VkBufferViewCreateFlagBits VkFlags
                                       deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                 FiniteBits, Storable, Real, Data, Generic)

instance Show VkBufferViewCreateFlagBits where
        {-# INLINE show #-}
        show (VkBufferViewCreateFlagBits x) = show x

instance Read VkBufferViewCreateFlagBits where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
