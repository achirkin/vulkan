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
module Graphics.Vulkan.Types.Enum.VkMemoryHeapFlags
       (VkMemoryHeapBitmask(VkMemoryHeapBitmask, VkMemoryHeapFlags,
                            VkMemoryHeapFlagBits, VK_MEMORY_HEAP_DEVICE_LOCAL_BIT),
        VkMemoryHeapFlags, VkMemoryHeapFlagBits)
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

newtype VkMemoryHeapBitmask (a ::
                               FlagType) = VkMemoryHeapBitmask VkFlags
                                             deriving (Eq, Ord, Storable, Data, Generic)

type VkMemoryHeapFlags = VkMemoryHeapBitmask FlagMask

type VkMemoryHeapFlagBits = VkMemoryHeapBitmask FlagBit

pattern VkMemoryHeapFlagBits ::
        VkFlags -> VkMemoryHeapBitmask FlagBit

pattern VkMemoryHeapFlagBits n = VkMemoryHeapBitmask n

pattern VkMemoryHeapFlags ::
        VkFlags -> VkMemoryHeapBitmask FlagMask

pattern VkMemoryHeapFlags n = VkMemoryHeapBitmask n

deriving instance Bits (VkMemoryHeapBitmask FlagMask)

deriving instance FiniteBits (VkMemoryHeapBitmask FlagMask)

deriving instance Integral (VkMemoryHeapBitmask FlagMask)

deriving instance Num (VkMemoryHeapBitmask FlagMask)

deriving instance Bounded (VkMemoryHeapBitmask FlagMask)

deriving instance Enum (VkMemoryHeapBitmask FlagMask)

deriving instance Real (VkMemoryHeapBitmask FlagMask)

instance Show (VkMemoryHeapBitmask a) where
        showsPrec _ VK_MEMORY_HEAP_DEVICE_LOCAL_BIT
          = showString "VK_MEMORY_HEAP_DEVICE_LOCAL_BIT"
        showsPrec p (VkMemoryHeapBitmask x)
          = showParen (p >= 11)
              (showString "VkMemoryHeapBitmask " . showsPrec 11 x)

instance Read (VkMemoryHeapBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_MEMORY_HEAP_DEVICE_LOCAL_BIT",
                   pure VK_MEMORY_HEAP_DEVICE_LOCAL_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkMemoryHeapBitmask") >>
                      (VkMemoryHeapBitmask <$> step readPrec)))

-- | If set, heap represents device memory
--
--   bitpos = @0@
pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT :: VkMemoryHeapBitmask a

pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = VkMemoryHeapBitmask 1
