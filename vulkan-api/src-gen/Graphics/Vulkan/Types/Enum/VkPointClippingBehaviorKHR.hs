{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkPointClippingBehaviorKHR
       (VkPointClippingBehaviorKHR(..)) where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags)

newtype VkPointClippingBehaviorKHR = VkPointClippingBehaviorKHR VkFlags
                                       deriving (Eq, Ord, Num, Bounded, Enum, Integral, Bits,
                                                 FiniteBits, Storable, Real, Data, Generic)

instance Show VkPointClippingBehaviorKHR where
        {-# INLINE show #-}
        show (VkPointClippingBehaviorKHR x) = show x

instance Read VkPointClippingBehaviorKHR where
        {-# INLINE readsPrec #-}
        readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
