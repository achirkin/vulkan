{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Graphics.Vulkan.Types.Enum.Sparse
       (VkSparseImageFormatBitmask(VkSparseImageFormatBitmask,
                                   VkSparseImageFormatFlags, VkSparseImageFormatFlagBits,
                                   VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT,
                                   VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT,
                                   VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT),
        VkSparseImageFormatFlags, VkSparseImageFormatFlagBits,
        VkSparseMemoryBindBitmask(VkSparseMemoryBindBitmask,
                                  VkSparseMemoryBindFlags, VkSparseMemoryBindFlagBits,
                                  VK_SPARSE_MEMORY_BIND_METADATA_BIT),
        VkSparseMemoryBindFlags, VkSparseMemoryBindFlagBits)
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Foreign.Storable                (Storable)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkSparseImageFormatBitmask (a ::
                                      FlagType) = VkSparseImageFormatBitmask VkFlags
                                                  deriving (Eq, Ord, Storable)

type VkSparseImageFormatFlags = VkSparseImageFormatBitmask FlagMask

type VkSparseImageFormatFlagBits =
     VkSparseImageFormatBitmask FlagBit

pattern VkSparseImageFormatFlagBits ::
        VkFlags -> VkSparseImageFormatBitmask FlagBit

pattern VkSparseImageFormatFlagBits n =
        VkSparseImageFormatBitmask n

pattern VkSparseImageFormatFlags ::
        VkFlags -> VkSparseImageFormatBitmask FlagMask

pattern VkSparseImageFormatFlags n = VkSparseImageFormatBitmask n

deriving instance Bits (VkSparseImageFormatBitmask FlagMask)

deriving instance FiniteBits (VkSparseImageFormatBitmask FlagMask)

instance Show (VkSparseImageFormatBitmask a) where
    showsPrec _ VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT
      = showString "VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT"
    showsPrec _ VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT
      = showString "VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT"
    showsPrec _ VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT
      = showString "VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT"
    showsPrec p (VkSparseImageFormatBitmask x)
      = showParen (p >= 11)
          (showString "VkSparseImageFormatBitmask " . showsPrec 11 x)

instance Read (VkSparseImageFormatBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT",
               pure VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT),
              ("VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT",
               pure VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT),
              ("VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT",
               pure VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT)]
             +++
             prec 10
               (expectP (Ident "VkSparseImageFormatBitmask") >>
                  (VkSparseImageFormatBitmask <$> step readPrec)))

-- | Image uses a single mip tail region for all array layers
--
--   bitpos = @0@
pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT ::
        VkSparseImageFormatBitmask a

pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT =
        VkSparseImageFormatBitmask 1

-- | Image requires mip level dimensions to be an integer multiple of the sparse image block dimensions for non-tail mip levels.
--
--   bitpos = @1@
pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT ::
        VkSparseImageFormatBitmask a

pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT =
        VkSparseImageFormatBitmask 2

-- | Image uses a non-standard sparse image block dimensions
--
--   bitpos = @2@
pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT ::
        VkSparseImageFormatBitmask a

pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT =
        VkSparseImageFormatBitmask 4

newtype VkSparseMemoryBindBitmask (a ::
                                     FlagType) = VkSparseMemoryBindBitmask VkFlags
                                                 deriving (Eq, Ord, Storable)

type VkSparseMemoryBindFlags = VkSparseMemoryBindBitmask FlagMask

type VkSparseMemoryBindFlagBits = VkSparseMemoryBindBitmask FlagBit

pattern VkSparseMemoryBindFlagBits ::
        VkFlags -> VkSparseMemoryBindBitmask FlagBit

pattern VkSparseMemoryBindFlagBits n = VkSparseMemoryBindBitmask n

pattern VkSparseMemoryBindFlags ::
        VkFlags -> VkSparseMemoryBindBitmask FlagMask

pattern VkSparseMemoryBindFlags n = VkSparseMemoryBindBitmask n

deriving instance Bits (VkSparseMemoryBindBitmask FlagMask)

deriving instance FiniteBits (VkSparseMemoryBindBitmask FlagMask)

instance Show (VkSparseMemoryBindBitmask a) where
    showsPrec _ VK_SPARSE_MEMORY_BIND_METADATA_BIT
      = showString "VK_SPARSE_MEMORY_BIND_METADATA_BIT"
    showsPrec p (VkSparseMemoryBindBitmask x)
      = showParen (p >= 11)
          (showString "VkSparseMemoryBindBitmask " . showsPrec 11 x)

instance Read (VkSparseMemoryBindBitmask a) where
    readPrec
      = parens
          (choose
             [("VK_SPARSE_MEMORY_BIND_METADATA_BIT",
               pure VK_SPARSE_MEMORY_BIND_METADATA_BIT)]
             +++
             prec 10
               (expectP (Ident "VkSparseMemoryBindBitmask") >>
                  (VkSparseMemoryBindBitmask <$> step readPrec)))

-- | Operation binds resource metadata to memory
--
--   bitpos = @0@
pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT ::
        VkSparseMemoryBindBitmask a

pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT =
        VkSparseMemoryBindBitmask 1
