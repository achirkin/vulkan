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
module Graphics.Vulkan.Types.Enum.Image
       (VkImageAspectBitmask(VkImageAspectBitmask, VkImageAspectFlags,
                             VkImageAspectFlagBits, VK_IMAGE_ASPECT_COLOR_BIT,
                             VK_IMAGE_ASPECT_DEPTH_BIT, VK_IMAGE_ASPECT_STENCIL_BIT,
                             VK_IMAGE_ASPECT_METADATA_BIT),
        VkImageAspectFlags, VkImageAspectFlagBits,
        VkImageCreateBitmask(VkImageCreateBitmask, VkImageCreateFlags,
                             VkImageCreateFlagBits, VK_IMAGE_CREATE_SPARSE_BINDING_BIT,
                             VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT,
                             VK_IMAGE_CREATE_SPARSE_ALIASED_BIT,
                             VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT,
                             VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT),
        VkImageCreateFlags, VkImageCreateFlagBits,
        VkImageLayout(VkImageLayout, VK_IMAGE_LAYOUT_UNDEFINED,
                      VK_IMAGE_LAYOUT_GENERAL, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                      VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                      VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,
                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                      VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                      VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                      VK_IMAGE_LAYOUT_PREINITIALIZED),
        VkImageTiling(VkImageTiling, VK_IMAGE_TILING_OPTIMAL,
                      VK_IMAGE_TILING_LINEAR),
        VkImageType(VkImageType, VK_IMAGE_TYPE_1D, VK_IMAGE_TYPE_2D,
                    VK_IMAGE_TYPE_3D),
        VkImageUsageBitmask(VkImageUsageBitmask, VkImageUsageFlags,
                            VkImageUsageFlagBits, VK_IMAGE_USAGE_TRANSFER_SRC_BIT,
                            VK_IMAGE_USAGE_TRANSFER_DST_BIT, VK_IMAGE_USAGE_SAMPLED_BIT,
                            VK_IMAGE_USAGE_STORAGE_BIT, VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
                            VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT,
                            VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT,
                            VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT),
        VkImageUsageFlags, VkImageUsageFlagBits,
        VkImageViewType(VkImageViewType, VK_IMAGE_VIEW_TYPE_1D,
                        VK_IMAGE_VIEW_TYPE_2D, VK_IMAGE_VIEW_TYPE_3D,
                        VK_IMAGE_VIEW_TYPE_CUBE, VK_IMAGE_VIEW_TYPE_1D_ARRAY,
                        VK_IMAGE_VIEW_TYPE_2D_ARRAY, VK_IMAGE_VIEW_TYPE_CUBE_ARRAY))
       where
import           Data.Bits                       (Bits, FiniteBits)
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType,
                                                  Int32)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

newtype VkImageAspectBitmask (a ::
                                FlagType) = VkImageAspectBitmask VkFlags
                                              deriving (Eq, Ord, Storable, Data, Generic)

type VkImageAspectFlags = VkImageAspectBitmask FlagMask

type VkImageAspectFlagBits = VkImageAspectBitmask FlagBit

pattern VkImageAspectFlagBits ::
        VkFlags -> VkImageAspectBitmask FlagBit

pattern VkImageAspectFlagBits n = VkImageAspectBitmask n

pattern VkImageAspectFlags ::
        VkFlags -> VkImageAspectBitmask FlagMask

pattern VkImageAspectFlags n = VkImageAspectBitmask n

deriving instance Bits (VkImageAspectBitmask FlagMask)

deriving instance FiniteBits (VkImageAspectBitmask FlagMask)

deriving instance Integral (VkImageAspectBitmask FlagMask)

deriving instance Num (VkImageAspectBitmask FlagMask)

deriving instance Bounded (VkImageAspectBitmask FlagMask)

deriving instance Enum (VkImageAspectBitmask FlagMask)

deriving instance Real (VkImageAspectBitmask FlagMask)

instance Show (VkImageAspectBitmask a) where
        showsPrec _ VK_IMAGE_ASPECT_COLOR_BIT
          = showString "VK_IMAGE_ASPECT_COLOR_BIT"
        showsPrec _ VK_IMAGE_ASPECT_DEPTH_BIT
          = showString "VK_IMAGE_ASPECT_DEPTH_BIT"
        showsPrec _ VK_IMAGE_ASPECT_STENCIL_BIT
          = showString "VK_IMAGE_ASPECT_STENCIL_BIT"
        showsPrec _ VK_IMAGE_ASPECT_METADATA_BIT
          = showString "VK_IMAGE_ASPECT_METADATA_BIT"
        showsPrec p (VkImageAspectBitmask x)
          = showParen (p >= 11)
              (showString "VkImageAspectBitmask " . showsPrec 11 x)

instance Read (VkImageAspectBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_ASPECT_COLOR_BIT", pure VK_IMAGE_ASPECT_COLOR_BIT),
                  ("VK_IMAGE_ASPECT_DEPTH_BIT", pure VK_IMAGE_ASPECT_DEPTH_BIT),
                  ("VK_IMAGE_ASPECT_STENCIL_BIT", pure VK_IMAGE_ASPECT_STENCIL_BIT),
                  ("VK_IMAGE_ASPECT_METADATA_BIT",
                   pure VK_IMAGE_ASPECT_METADATA_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageAspectBitmask") >>
                      (VkImageAspectBitmask <$> step readPrec)))

-- | bitpos = @0@
pattern VK_IMAGE_ASPECT_COLOR_BIT :: VkImageAspectBitmask a

pattern VK_IMAGE_ASPECT_COLOR_BIT = VkImageAspectBitmask 1

-- | bitpos = @1@
pattern VK_IMAGE_ASPECT_DEPTH_BIT :: VkImageAspectBitmask a

pattern VK_IMAGE_ASPECT_DEPTH_BIT = VkImageAspectBitmask 2

-- | bitpos = @2@
pattern VK_IMAGE_ASPECT_STENCIL_BIT :: VkImageAspectBitmask a

pattern VK_IMAGE_ASPECT_STENCIL_BIT = VkImageAspectBitmask 4

-- | bitpos = @3@
pattern VK_IMAGE_ASPECT_METADATA_BIT :: VkImageAspectBitmask a

pattern VK_IMAGE_ASPECT_METADATA_BIT = VkImageAspectBitmask 8

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

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageLayout VkImageLayout registry at www.khronos.org>
newtype VkImageLayout = VkImageLayout Int32
                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkImageLayout where
        showsPrec _ VK_IMAGE_LAYOUT_UNDEFINED
          = showString "VK_IMAGE_LAYOUT_UNDEFINED"
        showsPrec _ VK_IMAGE_LAYOUT_GENERAL
          = showString "VK_IMAGE_LAYOUT_GENERAL"
        showsPrec _ VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
          = showString "VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL"
        showsPrec _ VK_IMAGE_LAYOUT_PREINITIALIZED
          = showString "VK_IMAGE_LAYOUT_PREINITIALIZED"
        showsPrec p (VkImageLayout x)
          = showParen (p >= 11)
              (showString "VkImageLayout " . showsPrec 11 x)

instance Read VkImageLayout where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_LAYOUT_UNDEFINED", pure VK_IMAGE_LAYOUT_UNDEFINED),
                  ("VK_IMAGE_LAYOUT_GENERAL", pure VK_IMAGE_LAYOUT_GENERAL),
                  ("VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL",
                   pure VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL),
                  ("VK_IMAGE_LAYOUT_PREINITIALIZED",
                   pure VK_IMAGE_LAYOUT_PREINITIALIZED)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageLayout") >>
                      (VkImageLayout <$> step readPrec)))

-- | Implicit layout an image is when its contents are undefined due to various reasons (e.g. right after creation)
pattern VK_IMAGE_LAYOUT_UNDEFINED :: VkImageLayout

pattern VK_IMAGE_LAYOUT_UNDEFINED = VkImageLayout 0

-- | General layout when image can be used for any kind of access
pattern VK_IMAGE_LAYOUT_GENERAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_GENERAL = VkImageLayout 1

-- | Optimal layout when image is only used for color attachment read/write
pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = VkImageLayout 2

-- | Optimal layout when image is only used for depth/stencil attachment read/write
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL ::
        VkImageLayout

pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL =
        VkImageLayout 3

-- | Optimal layout when image is used for read only depth/stencil attachment and shader access
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL ::
        VkImageLayout

pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL =
        VkImageLayout 4

-- | Optimal layout when image is used for read only shader access
pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = VkImageLayout 5

-- | Optimal layout when image is used only as source of transfer operations
pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = VkImageLayout 6

-- | Optimal layout when image is used only as destination of transfer operations
pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = VkImageLayout 7

-- | Initial layout used when the data is populated by the CPU
pattern VK_IMAGE_LAYOUT_PREINITIALIZED :: VkImageLayout

pattern VK_IMAGE_LAYOUT_PREINITIALIZED = VkImageLayout 8

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageTiling VkImageTiling registry at www.khronos.org>
newtype VkImageTiling = VkImageTiling Int32
                          deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkImageTiling where
        showsPrec _ VK_IMAGE_TILING_OPTIMAL
          = showString "VK_IMAGE_TILING_OPTIMAL"
        showsPrec _ VK_IMAGE_TILING_LINEAR
          = showString "VK_IMAGE_TILING_LINEAR"
        showsPrec p (VkImageTiling x)
          = showParen (p >= 11)
              (showString "VkImageTiling " . showsPrec 11 x)

instance Read VkImageTiling where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_TILING_OPTIMAL", pure VK_IMAGE_TILING_OPTIMAL),
                  ("VK_IMAGE_TILING_LINEAR", pure VK_IMAGE_TILING_LINEAR)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageTiling") >>
                      (VkImageTiling <$> step readPrec)))

pattern VK_IMAGE_TILING_OPTIMAL :: VkImageTiling

pattern VK_IMAGE_TILING_OPTIMAL = VkImageTiling 0

pattern VK_IMAGE_TILING_LINEAR :: VkImageTiling

pattern VK_IMAGE_TILING_LINEAR = VkImageTiling 1

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageType VkImageType registry at www.khronos.org>
newtype VkImageType = VkImageType Int32
                        deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkImageType where
        showsPrec _ VK_IMAGE_TYPE_1D = showString "VK_IMAGE_TYPE_1D"
        showsPrec _ VK_IMAGE_TYPE_2D = showString "VK_IMAGE_TYPE_2D"
        showsPrec _ VK_IMAGE_TYPE_3D = showString "VK_IMAGE_TYPE_3D"
        showsPrec p (VkImageType x)
          = showParen (p >= 11) (showString "VkImageType " . showsPrec 11 x)

instance Read VkImageType where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_TYPE_1D", pure VK_IMAGE_TYPE_1D),
                  ("VK_IMAGE_TYPE_2D", pure VK_IMAGE_TYPE_2D),
                  ("VK_IMAGE_TYPE_3D", pure VK_IMAGE_TYPE_3D)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageType") >> (VkImageType <$> step readPrec)))

pattern VK_IMAGE_TYPE_1D :: VkImageType

pattern VK_IMAGE_TYPE_1D = VkImageType 0

pattern VK_IMAGE_TYPE_2D :: VkImageType

pattern VK_IMAGE_TYPE_2D = VkImageType 1

pattern VK_IMAGE_TYPE_3D :: VkImageType

pattern VK_IMAGE_TYPE_3D = VkImageType 2

newtype VkImageUsageBitmask (a ::
                               FlagType) = VkImageUsageBitmask VkFlags
                                             deriving (Eq, Ord, Storable, Data, Generic)

type VkImageUsageFlags = VkImageUsageBitmask FlagMask

type VkImageUsageFlagBits = VkImageUsageBitmask FlagBit

pattern VkImageUsageFlagBits ::
        VkFlags -> VkImageUsageBitmask FlagBit

pattern VkImageUsageFlagBits n = VkImageUsageBitmask n

pattern VkImageUsageFlags ::
        VkFlags -> VkImageUsageBitmask FlagMask

pattern VkImageUsageFlags n = VkImageUsageBitmask n

deriving instance Bits (VkImageUsageBitmask FlagMask)

deriving instance FiniteBits (VkImageUsageBitmask FlagMask)

deriving instance Integral (VkImageUsageBitmask FlagMask)

deriving instance Num (VkImageUsageBitmask FlagMask)

deriving instance Bounded (VkImageUsageBitmask FlagMask)

deriving instance Enum (VkImageUsageBitmask FlagMask)

deriving instance Real (VkImageUsageBitmask FlagMask)

instance Show (VkImageUsageBitmask a) where
        showsPrec _ VK_IMAGE_USAGE_TRANSFER_SRC_BIT
          = showString "VK_IMAGE_USAGE_TRANSFER_SRC_BIT"
        showsPrec _ VK_IMAGE_USAGE_TRANSFER_DST_BIT
          = showString "VK_IMAGE_USAGE_TRANSFER_DST_BIT"
        showsPrec _ VK_IMAGE_USAGE_SAMPLED_BIT
          = showString "VK_IMAGE_USAGE_SAMPLED_BIT"
        showsPrec _ VK_IMAGE_USAGE_STORAGE_BIT
          = showString "VK_IMAGE_USAGE_STORAGE_BIT"
        showsPrec _ VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
          = showString "VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT"
        showsPrec _ VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
          = showString "VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT"
        showsPrec _ VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
          = showString "VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT"
        showsPrec _ VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT
          = showString "VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT"
        showsPrec p (VkImageUsageBitmask x)
          = showParen (p >= 11)
              (showString "VkImageUsageBitmask " . showsPrec 11 x)

instance Read (VkImageUsageBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_USAGE_TRANSFER_SRC_BIT",
                   pure VK_IMAGE_USAGE_TRANSFER_SRC_BIT),
                  ("VK_IMAGE_USAGE_TRANSFER_DST_BIT",
                   pure VK_IMAGE_USAGE_TRANSFER_DST_BIT),
                  ("VK_IMAGE_USAGE_SAMPLED_BIT", pure VK_IMAGE_USAGE_SAMPLED_BIT),
                  ("VK_IMAGE_USAGE_STORAGE_BIT", pure VK_IMAGE_USAGE_STORAGE_BIT),
                  ("VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT",
                   pure VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT),
                  ("VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT",
                   pure VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT),
                  ("VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT",
                   pure VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT),
                  ("VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT",
                   pure VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageUsageBitmask") >>
                      (VkImageUsageBitmask <$> step readPrec)))

-- | Can be used as a source of transfer operations
--
--   bitpos = @0@
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT :: VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT = VkImageUsageBitmask 1

-- | Can be used as a destination of transfer operations
--
--   bitpos = @1@
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT :: VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT = VkImageUsageBitmask 2

-- | Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
--
--   bitpos = @2@
pattern VK_IMAGE_USAGE_SAMPLED_BIT :: VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_SAMPLED_BIT = VkImageUsageBitmask 4

-- | Can be used as storage image (STORAGE_IMAGE descriptor type)
--
--   bitpos = @3@
pattern VK_IMAGE_USAGE_STORAGE_BIT :: VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_STORAGE_BIT = VkImageUsageBitmask 8

-- | Can be used as framebuffer color attachment
--
--   bitpos = @4@
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT ::
        VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT =
        VkImageUsageBitmask 16

-- | Can be used as framebuffer depth/stencil attachment
--
--   bitpos = @5@
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT ::
        VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT =
        VkImageUsageBitmask 32

-- | Image data not needed outside of rendering
--
--   bitpos = @6@
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT ::
        VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT =
        VkImageUsageBitmask 64

-- | Can be used as framebuffer input attachment
--
--   bitpos = @7@
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT ::
        VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT =
        VkImageUsageBitmask 128

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageViewType VkImageViewType registry at www.khronos.org>
newtype VkImageViewType = VkImageViewType Int32
                            deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkImageViewType where
        showsPrec _ VK_IMAGE_VIEW_TYPE_1D
          = showString "VK_IMAGE_VIEW_TYPE_1D"
        showsPrec _ VK_IMAGE_VIEW_TYPE_2D
          = showString "VK_IMAGE_VIEW_TYPE_2D"
        showsPrec _ VK_IMAGE_VIEW_TYPE_3D
          = showString "VK_IMAGE_VIEW_TYPE_3D"
        showsPrec _ VK_IMAGE_VIEW_TYPE_CUBE
          = showString "VK_IMAGE_VIEW_TYPE_CUBE"
        showsPrec _ VK_IMAGE_VIEW_TYPE_1D_ARRAY
          = showString "VK_IMAGE_VIEW_TYPE_1D_ARRAY"
        showsPrec _ VK_IMAGE_VIEW_TYPE_2D_ARRAY
          = showString "VK_IMAGE_VIEW_TYPE_2D_ARRAY"
        showsPrec _ VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
          = showString "VK_IMAGE_VIEW_TYPE_CUBE_ARRAY"
        showsPrec p (VkImageViewType x)
          = showParen (p >= 11)
              (showString "VkImageViewType " . showsPrec 11 x)

instance Read VkImageViewType where
        readPrec
          = parens
              (choose
                 [("VK_IMAGE_VIEW_TYPE_1D", pure VK_IMAGE_VIEW_TYPE_1D),
                  ("VK_IMAGE_VIEW_TYPE_2D", pure VK_IMAGE_VIEW_TYPE_2D),
                  ("VK_IMAGE_VIEW_TYPE_3D", pure VK_IMAGE_VIEW_TYPE_3D),
                  ("VK_IMAGE_VIEW_TYPE_CUBE", pure VK_IMAGE_VIEW_TYPE_CUBE),
                  ("VK_IMAGE_VIEW_TYPE_1D_ARRAY", pure VK_IMAGE_VIEW_TYPE_1D_ARRAY),
                  ("VK_IMAGE_VIEW_TYPE_2D_ARRAY", pure VK_IMAGE_VIEW_TYPE_2D_ARRAY),
                  ("VK_IMAGE_VIEW_TYPE_CUBE_ARRAY",
                   pure VK_IMAGE_VIEW_TYPE_CUBE_ARRAY)]
                 +++
                 prec 10
                   (expectP (Ident "VkImageViewType") >>
                      (VkImageViewType <$> step readPrec)))

pattern VK_IMAGE_VIEW_TYPE_1D :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_1D = VkImageViewType 0

pattern VK_IMAGE_VIEW_TYPE_2D :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_2D = VkImageViewType 1

pattern VK_IMAGE_VIEW_TYPE_3D :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_3D = VkImageViewType 2

pattern VK_IMAGE_VIEW_TYPE_CUBE :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_CUBE = VkImageViewType 3

pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY = VkImageViewType 4

pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY = VkImageViewType 5

pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY :: VkImageViewType

pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY = VkImageViewType 6
