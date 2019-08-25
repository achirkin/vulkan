{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.AccessFlags
       (VkAccessBitmask(VkAccessBitmask, VkAccessFlags, VkAccessFlagBits,
                        VK_ACCESS_INDIRECT_COMMAND_READ_BIT, VK_ACCESS_INDEX_READ_BIT,
                        VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT, VK_ACCESS_UNIFORM_READ_BIT,
                        VK_ACCESS_INPUT_ATTACHMENT_READ_BIT, VK_ACCESS_SHADER_READ_BIT,
                        VK_ACCESS_SHADER_WRITE_BIT, VK_ACCESS_COLOR_ATTACHMENT_READ_BIT,
                        VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
                        VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT,
                        VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT,
                        VK_ACCESS_TRANSFER_READ_BIT, VK_ACCESS_TRANSFER_WRITE_BIT,
                        VK_ACCESS_HOST_READ_BIT, VK_ACCESS_HOST_WRITE_BIT,
                        VK_ACCESS_MEMORY_READ_BIT, VK_ACCESS_MEMORY_WRITE_BIT),
        VkAccessFlags, VkAccessFlagBits)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkAccessBitmask (a :: FlagType) = VkAccessBitmask VkFlags
                                          deriving (Eq, Ord, Storable)

type VkAccessFlags = VkAccessBitmask FlagMask

type VkAccessFlagBits = VkAccessBitmask FlagBit

pattern VkAccessFlagBits :: VkFlags -> VkAccessBitmask FlagBit

pattern VkAccessFlagBits n = VkAccessBitmask n

pattern VkAccessFlags :: VkFlags -> VkAccessBitmask FlagMask

pattern VkAccessFlags n = VkAccessBitmask n

deriving instance Bits (VkAccessBitmask FlagMask)

deriving instance FiniteBits (VkAccessBitmask FlagMask)

instance Show (VkAccessBitmask a) where
    showsPrec _ VK_ACCESS_INDIRECT_COMMAND_READ_BIT
      = showString "VK_ACCESS_INDIRECT_COMMAND_READ_BIT"
    showsPrec _ VK_ACCESS_INDEX_READ_BIT
      = showString "VK_ACCESS_INDEX_READ_BIT"
    showsPrec _ VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT
      = showString "VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT"
    showsPrec _ VK_ACCESS_UNIFORM_READ_BIT
      = showString "VK_ACCESS_UNIFORM_READ_BIT"
    showsPrec _ VK_ACCESS_INPUT_ATTACHMENT_READ_BIT
      = showString "VK_ACCESS_INPUT_ATTACHMENT_READ_BIT"
    showsPrec _ VK_ACCESS_SHADER_READ_BIT
      = showString "VK_ACCESS_SHADER_READ_BIT"
    showsPrec _ VK_ACCESS_SHADER_WRITE_BIT
      = showString "VK_ACCESS_SHADER_WRITE_BIT"
    showsPrec _ VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
      = showString "VK_ACCESS_COLOR_ATTACHMENT_READ_BIT"
    showsPrec _ VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
      = showString "VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT"
    showsPrec _ VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
      = showString "VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT"
    showsPrec _ VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
      = showString "VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT"
    showsPrec _ VK_ACCESS_TRANSFER_READ_BIT
      = showString "VK_ACCESS_TRANSFER_READ_BIT"
    showsPrec _ VK_ACCESS_TRANSFER_WRITE_BIT
      = showString "VK_ACCESS_TRANSFER_WRITE_BIT"
    showsPrec _ VK_ACCESS_HOST_READ_BIT
      = showString "VK_ACCESS_HOST_READ_BIT"
    showsPrec _ VK_ACCESS_HOST_WRITE_BIT
      = showString "VK_ACCESS_HOST_WRITE_BIT"
    showsPrec _ VK_ACCESS_MEMORY_READ_BIT
      = showString "VK_ACCESS_MEMORY_READ_BIT"
    showsPrec _ VK_ACCESS_MEMORY_WRITE_BIT
      = showString "VK_ACCESS_MEMORY_WRITE_BIT"
    showsPrec p (VkAccessBitmask x)
      = showParen (p >= 11)
          (showString "VkAccessBitmask " . showsPrec 11 x)

instance Read (VkAccessBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_ACCESS_INDIRECT_COMMAND_READ_BIT",
               pure VK_ACCESS_INDIRECT_COMMAND_READ_BIT),
              ("VK_ACCESS_INDEX_READ_BIT", pure VK_ACCESS_INDEX_READ_BIT),
              ("VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT",
               pure VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT),
              ("VK_ACCESS_UNIFORM_READ_BIT", pure VK_ACCESS_UNIFORM_READ_BIT),
              ("VK_ACCESS_INPUT_ATTACHMENT_READ_BIT",
               pure VK_ACCESS_INPUT_ATTACHMENT_READ_BIT),
              ("VK_ACCESS_SHADER_READ_BIT", pure VK_ACCESS_SHADER_READ_BIT),
              ("VK_ACCESS_SHADER_WRITE_BIT", pure VK_ACCESS_SHADER_WRITE_BIT),
              ("VK_ACCESS_COLOR_ATTACHMENT_READ_BIT",
               pure VK_ACCESS_COLOR_ATTACHMENT_READ_BIT),
              ("VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT",
               pure VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
              ("VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT",
               pure VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT),
              ("VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT",
               pure VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT),
              ("VK_ACCESS_TRANSFER_READ_BIT", pure VK_ACCESS_TRANSFER_READ_BIT),
              ("VK_ACCESS_TRANSFER_WRITE_BIT",
               pure VK_ACCESS_TRANSFER_WRITE_BIT),
              ("VK_ACCESS_HOST_READ_BIT", pure VK_ACCESS_HOST_READ_BIT),
              ("VK_ACCESS_HOST_WRITE_BIT", pure VK_ACCESS_HOST_WRITE_BIT),
              ("VK_ACCESS_MEMORY_READ_BIT", pure VK_ACCESS_MEMORY_READ_BIT),
              ("VK_ACCESS_MEMORY_WRITE_BIT", pure VK_ACCESS_MEMORY_WRITE_BIT)]
             +++
             prec 10
               (expectP (Ident "VkAccessBitmask") >>
                  (VkAccessBitmask <$> step readPrec)))

-- | Controls coherency of indirect command reads
--
--   bitpos = @0@
pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT :: VkAccessBitmask a

pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT = VkAccessBitmask 1

-- | Controls coherency of index reads
--
--   bitpos = @1@
pattern VK_ACCESS_INDEX_READ_BIT :: VkAccessBitmask a

pattern VK_ACCESS_INDEX_READ_BIT = VkAccessBitmask 2

-- | Controls coherency of vertex attribute reads
--
--   bitpos = @2@
pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT :: VkAccessBitmask a

pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT = VkAccessBitmask 4

-- | Controls coherency of uniform buffer reads
--
--   bitpos = @3@
pattern VK_ACCESS_UNIFORM_READ_BIT :: VkAccessBitmask a

pattern VK_ACCESS_UNIFORM_READ_BIT = VkAccessBitmask 8

-- | Controls coherency of input attachment reads
--
--   bitpos = @4@
pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT :: VkAccessBitmask a

pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT = VkAccessBitmask 16

-- | Controls coherency of shader reads
--
--   bitpos = @5@
pattern VK_ACCESS_SHADER_READ_BIT :: VkAccessBitmask a

pattern VK_ACCESS_SHADER_READ_BIT = VkAccessBitmask 32

-- | Controls coherency of shader writes
--
--   bitpos = @6@
pattern VK_ACCESS_SHADER_WRITE_BIT :: VkAccessBitmask a

pattern VK_ACCESS_SHADER_WRITE_BIT = VkAccessBitmask 64

-- | Controls coherency of color attachment reads
--
--   bitpos = @7@
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT :: VkAccessBitmask a

pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT = VkAccessBitmask 128

-- | Controls coherency of color attachment writes
--
--   bitpos = @8@
pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT :: VkAccessBitmask a

pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT = VkAccessBitmask 256

-- | Controls coherency of depth/stencil attachment reads
--
--   bitpos = @9@
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT ::
        VkAccessBitmask a

pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT =
        VkAccessBitmask 512

-- | Controls coherency of depth/stencil attachment writes
--
--   bitpos = @10@
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT ::
        VkAccessBitmask a

pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT =
        VkAccessBitmask 1024

-- | Controls coherency of transfer reads
--
--   bitpos = @11@
pattern VK_ACCESS_TRANSFER_READ_BIT :: VkAccessBitmask a

pattern VK_ACCESS_TRANSFER_READ_BIT = VkAccessBitmask 2048

-- | Controls coherency of transfer writes
--
--   bitpos = @12@
pattern VK_ACCESS_TRANSFER_WRITE_BIT :: VkAccessBitmask a

pattern VK_ACCESS_TRANSFER_WRITE_BIT = VkAccessBitmask 4096

-- | Controls coherency of host reads
--
--   bitpos = @13@
pattern VK_ACCESS_HOST_READ_BIT :: VkAccessBitmask a

pattern VK_ACCESS_HOST_READ_BIT = VkAccessBitmask 8192

-- | Controls coherency of host writes
--
--   bitpos = @14@
pattern VK_ACCESS_HOST_WRITE_BIT :: VkAccessBitmask a

pattern VK_ACCESS_HOST_WRITE_BIT = VkAccessBitmask 16384

-- | Controls coherency of memory reads
--
--   bitpos = @15@
pattern VK_ACCESS_MEMORY_READ_BIT :: VkAccessBitmask a

pattern VK_ACCESS_MEMORY_READ_BIT = VkAccessBitmask 32768

-- | Controls coherency of memory writes
--
--   bitpos = @16@
pattern VK_ACCESS_MEMORY_WRITE_BIT :: VkAccessBitmask a

pattern VK_ACCESS_MEMORY_WRITE_BIT = VkAccessBitmask 65536
