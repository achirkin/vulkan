{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.VkMemoryAllocateFlagsKHX
       (VkMemoryAllocateBitmaskKHX(VkMemoryAllocateBitmaskKHX,
                                   VkMemoryAllocateFlagsKHX, VkMemoryAllocateFlagBitsKHX,
                                   VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX),
        VkMemoryAllocateFlagsKHX, VkMemoryAllocateFlagBitsKHX)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkMemoryAllocateBitmaskKHX (a ::
                                      FlagType) = VkMemoryAllocateBitmaskKHX VkFlags
                                                    deriving (Eq, Ord, Storable, Data, Generic)

type VkMemoryAllocateFlagsKHX = VkMemoryAllocateBitmaskKHX FlagMask

type VkMemoryAllocateFlagBitsKHX =
     VkMemoryAllocateBitmaskKHX FlagBit

pattern VkMemoryAllocateFlagBitsKHX ::
        VkFlags -> VkMemoryAllocateBitmaskKHX FlagBit

pattern VkMemoryAllocateFlagBitsKHX n =
        VkMemoryAllocateBitmaskKHX n

pattern VkMemoryAllocateFlagsKHX ::
        VkFlags -> VkMemoryAllocateBitmaskKHX FlagMask

pattern VkMemoryAllocateFlagsKHX n = VkMemoryAllocateBitmaskKHX n

deriving instance Bits (VkMemoryAllocateBitmaskKHX FlagMask)

deriving instance FiniteBits (VkMemoryAllocateBitmaskKHX FlagMask)

deriving instance Integral (VkMemoryAllocateBitmaskKHX FlagMask)

deriving instance Num (VkMemoryAllocateBitmaskKHX FlagMask)

deriving instance Bounded (VkMemoryAllocateBitmaskKHX FlagMask)

deriving instance Enum (VkMemoryAllocateBitmaskKHX FlagMask)

deriving instance Real (VkMemoryAllocateBitmaskKHX FlagMask)

instance Show (VkMemoryAllocateBitmaskKHX a) where
        showsPrec _ VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX
          = showString "VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX"
        showsPrec p (VkMemoryAllocateBitmaskKHX x)
          = showParen (p >= 11)
              (showString "VkMemoryAllocateBitmaskKHX " . showsPrec 11 x)

instance Read (VkMemoryAllocateBitmaskKHX a) where
        readPrec
          = parens
              (choose
                 [("VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX",
                   pure VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX)]
                 +++
                 prec 10
                   (expectP (Ident "VkMemoryAllocateBitmaskKHX") >>
                      (VkMemoryAllocateBitmaskKHX <$> step readPrec)))

-- | Force allocation on specific devices
--
--   bitpos = @0@
pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX ::
        VkMemoryAllocateBitmaskKHX a

pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHX =
        VkMemoryAllocateBitmaskKHX 1
