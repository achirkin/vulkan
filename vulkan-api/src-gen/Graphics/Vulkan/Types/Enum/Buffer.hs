{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.Buffer
       (VkBufferCreateBitmask(VkBufferCreateBitmask, VkBufferCreateFlags,
                              VkBufferCreateFlagBits, VK_BUFFER_CREATE_SPARSE_BINDING_BIT,
                              VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT,
                              VK_BUFFER_CREATE_SPARSE_ALIASED_BIT),
        VkBufferCreateFlags, VkBufferCreateFlagBits,
        VkBufferUsageBitmask(VkBufferUsageBitmask, VkBufferUsageFlags,
                             VkBufferUsageFlagBits, VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
                             VK_BUFFER_USAGE_TRANSFER_DST_BIT,
                             VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT,
                             VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT,
                             VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
                             VK_BUFFER_USAGE_STORAGE_BUFFER_BIT,
                             VK_BUFFER_USAGE_INDEX_BUFFER_BIT,
                             VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
                             VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT),
        VkBufferUsageFlags, VkBufferUsageFlagBits,
        VkBufferViewCreateFlagBits(..))
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Coerce                     (coerce)
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkBufferCreateBitmask (a ::
                                 FlagType) = VkBufferCreateBitmask VkFlags
                                             deriving (Eq, Ord, Storable)

type VkBufferCreateFlags = VkBufferCreateBitmask FlagMask

type VkBufferCreateFlagBits = VkBufferCreateBitmask FlagBit

pattern VkBufferCreateFlagBits ::
        VkFlags -> VkBufferCreateBitmask FlagBit

pattern VkBufferCreateFlagBits n = VkBufferCreateBitmask n

pattern VkBufferCreateFlags ::
        VkFlags -> VkBufferCreateBitmask FlagMask

pattern VkBufferCreateFlags n = VkBufferCreateBitmask n

deriving instance Bits (VkBufferCreateBitmask FlagMask)

deriving instance FiniteBits (VkBufferCreateBitmask FlagMask)

instance Show (VkBufferCreateBitmask a) where
    showsPrec _ VK_BUFFER_CREATE_SPARSE_BINDING_BIT
      = showString "VK_BUFFER_CREATE_SPARSE_BINDING_BIT"
    showsPrec _ VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT
      = showString "VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT"
    showsPrec _ VK_BUFFER_CREATE_SPARSE_ALIASED_BIT
      = showString "VK_BUFFER_CREATE_SPARSE_ALIASED_BIT"
    showsPrec p (VkBufferCreateBitmask x)
      = showParen (p >= 11)
          (showString "VkBufferCreateBitmask " . showsPrec 11 x)

instance Read (VkBufferCreateBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_BUFFER_CREATE_SPARSE_BINDING_BIT",
               pure VK_BUFFER_CREATE_SPARSE_BINDING_BIT),
              ("VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT",
               pure VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT),
              ("VK_BUFFER_CREATE_SPARSE_ALIASED_BIT",
               pure VK_BUFFER_CREATE_SPARSE_ALIASED_BIT)]
             +++
             prec 10
               (expectP (Ident "VkBufferCreateBitmask") >>
                  (VkBufferCreateBitmask <$> step readPrec)))

-- | Buffer should support sparse backing
--
--   bitpos = @0@
pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT ::
        VkBufferCreateBitmask a

pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT =
        VkBufferCreateBitmask 1

-- | Buffer should support sparse backing with partial residency
--
--   bitpos = @1@
pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT ::
        VkBufferCreateBitmask a

pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT =
        VkBufferCreateBitmask 2

-- | Buffer should support constent data access to physical memory ranges mapped into multiple locations of sparse buffers
--
--   bitpos = @2@
pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT ::
        VkBufferCreateBitmask a

pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT =
        VkBufferCreateBitmask 4

newtype VkBufferUsageBitmask (a ::
                                FlagType) = VkBufferUsageBitmask VkFlags
                                            deriving (Eq, Ord, Storable)

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

newtype VkBufferViewCreateFlagBits = VkBufferViewCreateFlagBits VkFlags
                                     deriving (Eq, Ord, Enum, Bits, FiniteBits, Storable)

instance Show VkBufferViewCreateFlagBits where
    {-# INLINE showsPrec #-}
    showsPrec = coerce (showsPrec :: Int -> VkFlags -> ShowS)

instance Read VkBufferViewCreateFlagBits where
    {-# INLINE readsPrec #-}
    readsPrec = coerce (readsPrec :: Int -> ReadS VkFlags)
