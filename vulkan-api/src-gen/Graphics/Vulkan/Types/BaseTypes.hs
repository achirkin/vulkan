{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.BaseTypes
       (VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..))
       where
import           Data.Bits               (Bits, FiniteBits)
import           Data.Coerce             (coerce)
import           Data.Data               (Data)
import           Foreign.Storable        (Storable)
import           GHC.Generics            (Generic)
import           Graphics.Vulkan.Marshal (Word32, Word64)

newtype VkBool32 = VkBool32 Word32
                     deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                               Storable, Real, Data, Generic)

instance Show VkBool32 where
        {-# INLINE show #-}
        show (VkBool32 x) = show x

instance Read VkBool32 where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS Word32)

newtype VkDeviceSize = VkDeviceSize Word64
                         deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                                   Storable, Real, Data, Generic)

instance Show VkDeviceSize where
        {-# INLINE show #-}
        show (VkDeviceSize x) = show x

instance Read VkDeviceSize where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS Word64)

newtype VkFlags = VkFlags Word32
                    deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                              Storable, Real, Data, Generic)

instance Show VkFlags where
        {-# INLINE show #-}
        show (VkFlags x) = show x

instance Read VkFlags where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS Word32)

newtype VkSampleMask = VkSampleMask Word32
                         deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits, FiniteBits,
                                   Storable, Real, Data, Generic)

instance Show VkSampleMask where
        {-# INLINE show #-}
        show (VkSampleMask x) = show x

instance Read VkSampleMask where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS Word32)
