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
module Graphics.Vulkan.Types.Enum.VkImageCreateFlags
       (VkImageCreateBitmask(VkImageCreateBitmask, VkImageCreateFlags,
                             VkImageCreateFlagBits, VK_IMAGE_CREATE_SPARSE_BINDING_BIT,
                             VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT,
                             VK_IMAGE_CREATE_SPARSE_ALIASED_BIT,
                             VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT,
                             VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT),
        VkImageCreateFlags, VkImageCreateFlagBits)
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

newtype VkImageCreateBitmask (a ::
                                FlagType) = VkImageCreateBitmask VkFlags
                                              deriving (Eq, Ord, Storable, Data, Generic)

type VkImageCreateFlags = VkImageCreateBitmask FlagMask

type VkImageCreateFlagBits = VkImageCreateBitmask FlagBit

pattern VkImageCreateFlagBits ::
        VkFlags -> VkImageCreateBitmask FlagBit

pattern VkImageCreateFlagBits n = VkImageCreateBitmask n

pattern VkImageCreateFlags ::
        VkFlags -> VkImageCreateBitmask FlagMask

pattern VkImageCreateFlags n = VkImageCreateBitmask n

deriving instance Bits (VkImageCreateBitmask FlagMask)

deriving instance FiniteBits (VkImageCreateBitmask FlagMask)

deriving instance Integral (VkImageCreateBitmask FlagMask)

deriving instance Num (VkImageCreateBitmask FlagMask)

deriving instance Bounded (VkImageCreateBitmask FlagMask)

deriving instance Enum (VkImageCreateBitmask FlagMask)

deriving instance Real (VkImageCreateBitmask FlagMask)

instance Show (VkImageCreateBitmask a) where
        showsPrec _ VK_IMAGE_CREATE_SPARSE_BINDING_BIT
          = showString "VK_IMAGE_CREATE_SPARSE_BINDING_BIT"
        showsPrec _ VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
          = showString "VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT"
        showsPrec _ VK_IMAGE_CREATE_SPARSE_ALIASED_BIT
          = showString "VK_IMAGE_CREATE_SPARSE_ALIASED_BIT"
        showsPrec _ VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT
          = showString "VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT"
        showsPrec _ VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT
          = showString "VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT"
        showsPrec p (VkImageCreateBitmask x)
          = showParen (p >= 11)
              (showString "VkImageCreateBitmask " . showsPrec 11 x)

instance Read (VkImageCreateBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_CREATE_SPARSE_BINDING_BIT",
                   pure VK_IMAGE_CREATE_SPARSE_BINDING_BIT),
                  ("VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT",
                   pure VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT),
                  ("VK_IMAGE_CREATE_SPARSE_ALIASED_BIT",
                   pure VK_IMAGE_CREATE_SPARSE_ALIASED_BIT),
                  ("VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT",
                   pure VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT),
                  ("VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT",
                   pure VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageCreateBitmask") >>
                      (VkImageCreateBitmask <$> step readPrec)))

-- | Image should support sparse backing
--
--   bitpos = @0@
pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT ::
        VkImageCreateBitmask a

pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT = VkImageCreateBitmask 1

-- | Image should support sparse backing with partial residency
--
--   bitpos = @1@
pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT ::
        VkImageCreateBitmask a

pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT =
        VkImageCreateBitmask 2

-- | Image should support constent data access to physical memory ranges mapped into multiple locations of sparse images
--
--   bitpos = @2@
pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT ::
        VkImageCreateBitmask a

pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT = VkImageCreateBitmask 4

-- | Allows image views to have different format than the base image
--
--   bitpos = @3@
pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT ::
        VkImageCreateBitmask a

pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT = VkImageCreateBitmask 8

-- | Allows creating image views with cube type from the created image
--
--   bitpos = @4@
pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT ::
        VkImageCreateBitmask a

pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT =
        VkImageCreateBitmask 16
