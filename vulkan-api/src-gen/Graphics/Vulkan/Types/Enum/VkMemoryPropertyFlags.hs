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
module Graphics.Vulkan.Types.Enum.VkMemoryPropertyFlags
       (VkMemoryPropertyBitmask(VkMemoryPropertyBitmask,
                                VkMemoryPropertyFlags, VkMemoryPropertyFlagBits,
                                VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT,
                                VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT,
                                VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
                                VK_MEMORY_PROPERTY_HOST_CACHED_BIT,
                                VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT),
        VkMemoryPropertyFlags, VkMemoryPropertyFlagBits)
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

newtype VkMemoryPropertyBitmask (a ::
                                   FlagType) = VkMemoryPropertyBitmask VkFlags
                                                 deriving (Eq, Ord, Storable, Data, Generic)

type VkMemoryPropertyFlags = VkMemoryPropertyBitmask FlagMask

type VkMemoryPropertyFlagBits = VkMemoryPropertyBitmask FlagBit

pattern VkMemoryPropertyFlagBits ::
        VkFlags -> VkMemoryPropertyBitmask FlagBit

pattern VkMemoryPropertyFlagBits n = VkMemoryPropertyBitmask n

pattern VkMemoryPropertyFlags ::
        VkFlags -> VkMemoryPropertyBitmask FlagMask

pattern VkMemoryPropertyFlags n = VkMemoryPropertyBitmask n

deriving instance Bits (VkMemoryPropertyBitmask FlagMask)

deriving instance FiniteBits (VkMemoryPropertyBitmask FlagMask)

deriving instance Integral (VkMemoryPropertyBitmask FlagMask)

deriving instance Num (VkMemoryPropertyBitmask FlagMask)

deriving instance Bounded (VkMemoryPropertyBitmask FlagMask)

deriving instance Enum (VkMemoryPropertyBitmask FlagMask)

deriving instance Real (VkMemoryPropertyBitmask FlagMask)

instance Show (VkMemoryPropertyBitmask a) where
        showsPrec _ VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
          = showString "VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT"
        showsPrec _ VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
          = showString "VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT"
        showsPrec _ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
          = showString "VK_MEMORY_PROPERTY_HOST_COHERENT_BIT"
        showsPrec _ VK_MEMORY_PROPERTY_HOST_CACHED_BIT
          = showString "VK_MEMORY_PROPERTY_HOST_CACHED_BIT"
        showsPrec _ VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT
          = showString "VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT"
        showsPrec p (VkMemoryPropertyBitmask x)
          = showParen (p >= 11)
              (showString "VkMemoryPropertyBitmask " . showsPrec 11 x)

instance Read (VkMemoryPropertyBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT",
                   pure VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                  ("VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT",
                   pure VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT),
                  ("VK_MEMORY_PROPERTY_HOST_COHERENT_BIT",
                   pure VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                  ("VK_MEMORY_PROPERTY_HOST_CACHED_BIT",
                   pure VK_MEMORY_PROPERTY_HOST_CACHED_BIT),
                  ("VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT",
                   pure VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkMemoryPropertyBitmask") >>
                      (VkMemoryPropertyBitmask <$> step readPrec)))

-- | If otherwise stated, then allocate memory on device
--
--   bitpos = @0@
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT ::
        VkMemoryPropertyBitmask a

pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT =
        VkMemoryPropertyBitmask 1

-- | Memory is mappable by host
--
--   bitpos = @1@
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ::
        VkMemoryPropertyBitmask a

pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT =
        VkMemoryPropertyBitmask 2

-- | Memory will have i/o coherency. If not set, application may need to use vkFlushMappedMemoryRanges and vkInvalidateMappedMemoryRanges to flush/invalidate host cache
--
--   bitpos = @2@
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT ::
        VkMemoryPropertyBitmask a

pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT =
        VkMemoryPropertyBitmask 4

-- | Memory will be cached by the host
--
--   bitpos = @3@
pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT ::
        VkMemoryPropertyBitmask a

pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT =
        VkMemoryPropertyBitmask 8

-- | Memory may be allocated by the driver when it is required
--
--   bitpos = @4@
pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT ::
        VkMemoryPropertyBitmask a

pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT =
        VkMemoryPropertyBitmask 16
