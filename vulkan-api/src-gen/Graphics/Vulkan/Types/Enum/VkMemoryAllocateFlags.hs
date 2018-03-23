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
module Graphics.Vulkan.Types.Enum.VkMemoryAllocateFlags
       (VkMemoryAllocateBitmask(VkMemoryAllocateBitmask,
                                VkMemoryAllocateFlags, VkMemoryAllocateFlagBits,
                                VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT),
        VkMemoryAllocateFlags, VkMemoryAllocateFlagBits)
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

newtype VkMemoryAllocateBitmask (a ::
                                   FlagType) = VkMemoryAllocateBitmask VkFlags
                                                 deriving (Eq, Ord, Storable, Data, Generic)

type VkMemoryAllocateFlags = VkMemoryAllocateBitmask FlagMask

type VkMemoryAllocateFlagBits = VkMemoryAllocateBitmask FlagBit

pattern VkMemoryAllocateFlagBits ::
        VkFlags -> VkMemoryAllocateBitmask FlagBit

pattern VkMemoryAllocateFlagBits n = VkMemoryAllocateBitmask n

pattern VkMemoryAllocateFlags ::
        VkFlags -> VkMemoryAllocateBitmask FlagMask

pattern VkMemoryAllocateFlags n = VkMemoryAllocateBitmask n

deriving instance Bits (VkMemoryAllocateBitmask FlagMask)

deriving instance FiniteBits (VkMemoryAllocateBitmask FlagMask)

deriving instance Integral (VkMemoryAllocateBitmask FlagMask)

deriving instance Num (VkMemoryAllocateBitmask FlagMask)

deriving instance Bounded (VkMemoryAllocateBitmask FlagMask)

deriving instance Enum (VkMemoryAllocateBitmask FlagMask)

deriving instance Real (VkMemoryAllocateBitmask FlagMask)

instance Show (VkMemoryAllocateBitmask a) where
        showsPrec _ VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT
          = showString "VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT"
        showsPrec p (VkMemoryAllocateBitmask x)
          = showParen (p >= 11)
              (showString "VkMemoryAllocateBitmask " . showsPrec 11 x)

instance Read (VkMemoryAllocateBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT",
                   pure VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkMemoryAllocateBitmask") >>
                      (VkMemoryAllocateBitmask <$> step readPrec)))

-- | Force allocation on specific devices
--
--   bitpos = @0@
pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT ::
        VkMemoryAllocateBitmask a

pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT =
        VkMemoryAllocateBitmask 1
