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
module Graphics.Vulkan.Types.Enum.VkBufferUsageFlags
       (VkBufferUsageBitmask(VkBufferUsageBitmask, VkBufferUsageFlags,
                             VkBufferUsageFlagBits, VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
                             VK_BUFFER_USAGE_TRANSFER_DST_BIT,
                             VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT,
                             VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT,
                             VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
                             VK_BUFFER_USAGE_STORAGE_BUFFER_BIT,
                             VK_BUFFER_USAGE_INDEX_BUFFER_BIT,
                             VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
                             VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT),
        VkBufferUsageFlags, VkBufferUsageFlagBits)
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

newtype VkBufferUsageBitmask (a ::
                                FlagType) = VkBufferUsageBitmask VkFlags
                                              deriving (Eq, Ord, Storable, Data, Generic)

type VkBufferUsageFlags = VkBufferUsageBitmask FlagMask

type VkBufferUsageFlagBits = VkBufferUsageBitmask FlagBit

pattern VkBufferUsageFlagBits ::
        VkFlags -> VkBufferUsageBitmask FlagBit

pattern VkBufferUsageFlagBits n = VkBufferUsageBitmask n

pattern VkBufferUsageFlags ::
        VkFlags -> VkBufferUsageBitmask FlagMask

pattern VkBufferUsageFlags n = VkBufferUsageBitmask n

deriving instance Bits (VkBufferUsageBitmask FlagMask)

deriving instance FiniteBits (VkBufferUsageBitmask FlagMask)

deriving instance Integral (VkBufferUsageBitmask FlagMask)

deriving instance Num (VkBufferUsageBitmask FlagMask)

deriving instance Bounded (VkBufferUsageBitmask FlagMask)

deriving instance Enum (VkBufferUsageBitmask FlagMask)

deriving instance Real (VkBufferUsageBitmask FlagMask)

instance Show (VkBufferUsageBitmask a) where
        showsPrec _ VK_BUFFER_USAGE_TRANSFER_SRC_BIT
          = showString "VK_BUFFER_USAGE_TRANSFER_SRC_BIT"
        showsPrec _ VK_BUFFER_USAGE_TRANSFER_DST_BIT
          = showString "VK_BUFFER_USAGE_TRANSFER_DST_BIT"
        showsPrec _ VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT
          = showString "VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT"
        showsPrec _ VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
          = showString "VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT"
        showsPrec _ VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
          = showString "VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT"
        showsPrec _ VK_BUFFER_USAGE_STORAGE_BUFFER_BIT
          = showString "VK_BUFFER_USAGE_STORAGE_BUFFER_BIT"
        showsPrec _ VK_BUFFER_USAGE_INDEX_BUFFER_BIT
          = showString "VK_BUFFER_USAGE_INDEX_BUFFER_BIT"
        showsPrec _ VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
          = showString "VK_BUFFER_USAGE_VERTEX_BUFFER_BIT"
        showsPrec _ VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT
          = showString "VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT"
        showsPrec p (VkBufferUsageBitmask x)
          = showParen (p >= 11)
              (showString "VkBufferUsageBitmask " . showsPrec 11 x)

instance Read (VkBufferUsageBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_BUFFER_USAGE_TRANSFER_SRC_BIT",
                   pure VK_BUFFER_USAGE_TRANSFER_SRC_BIT),
                  ("VK_BUFFER_USAGE_TRANSFER_DST_BIT",
                   pure VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                  ("VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT",
                   pure VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT),
                  ("VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT",
                   pure VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT),
                  ("VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT",
                   pure VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                  ("VK_BUFFER_USAGE_STORAGE_BUFFER_BIT",
                   pure VK_BUFFER_USAGE_STORAGE_BUFFER_BIT),
                  ("VK_BUFFER_USAGE_INDEX_BUFFER_BIT",
                   pure VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
                  ("VK_BUFFER_USAGE_VERTEX_BUFFER_BIT",
                   pure VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
                  ("VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT",
                   pure VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkBufferUsageBitmask") >>
                      (VkBufferUsageBitmask <$> step readPrec)))

-- | Can be used as a source of transfer operations
--
--   bitpos = @0@
pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT :: VkBufferUsageBitmask a

pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT = VkBufferUsageBitmask 1

-- | Can be used as a destination of transfer operations
--
--   bitpos = @1@
pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT :: VkBufferUsageBitmask a

pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT = VkBufferUsageBitmask 2

-- | Can be used as TBO
--
--   bitpos = @2@
pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT ::
        VkBufferUsageBitmask a

pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT =
        VkBufferUsageBitmask 4

-- | Can be used as IBO
--
--   bitpos = @3@
pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT ::
        VkBufferUsageBitmask a

pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT =
        VkBufferUsageBitmask 8

-- | Can be used as UBO
--
--   bitpos = @4@
pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT ::
        VkBufferUsageBitmask a

pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT =
        VkBufferUsageBitmask 16

-- | Can be used as SSBO
--
--   bitpos = @5@
pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT ::
        VkBufferUsageBitmask a

pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT =
        VkBufferUsageBitmask 32

-- | Can be used as source of fixed-function index fetch (index buffer)
--
--   bitpos = @6@
pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT :: VkBufferUsageBitmask a

pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT = VkBufferUsageBitmask 64

-- | Can be used as source of fixed-function vertex fetch (VBO)
--
--   bitpos = @7@
pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT :: VkBufferUsageBitmask a

pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT =
        VkBufferUsageBitmask 128

-- | Can be the source of indirect parameters (e.g. indirect buffer, parameter buffer)
--
--   bitpos = @8@
pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT ::
        VkBufferUsageBitmask a

pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT =
        VkBufferUsageBitmask 256
