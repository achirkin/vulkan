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
module Graphics.Vulkan.Types.Enum.VkFormatFeatureFlags
       (VkFormatFeatureBitmask(VkFormatFeatureBitmask,
                               VkFormatFeatureFlags, VkFormatFeatureFlagBits,
                               VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT,
                               VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT,
                               VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT,
                               VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT,
                               VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT,
                               VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT,
                               VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT,
                               VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT,
                               VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT,
                               VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT,
                               VK_FORMAT_FEATURE_BLIT_SRC_BIT, VK_FORMAT_FEATURE_BLIT_DST_BIT,
                               VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT),
        VkFormatFeatureFlags, VkFormatFeatureFlagBits)
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

newtype VkFormatFeatureBitmask (a ::
                                  FlagType) = VkFormatFeatureBitmask VkFlags
                                                deriving (Eq, Ord, Storable, Data, Generic)

type VkFormatFeatureFlags = VkFormatFeatureBitmask FlagMask

type VkFormatFeatureFlagBits = VkFormatFeatureBitmask FlagBit

pattern VkFormatFeatureFlagBits ::
        VkFlags -> VkFormatFeatureBitmask FlagBit

pattern VkFormatFeatureFlagBits n = VkFormatFeatureBitmask n

pattern VkFormatFeatureFlags ::
        VkFlags -> VkFormatFeatureBitmask FlagMask

pattern VkFormatFeatureFlags n = VkFormatFeatureBitmask n

deriving instance Bits (VkFormatFeatureBitmask FlagMask)

deriving instance FiniteBits (VkFormatFeatureBitmask FlagMask)

deriving instance Integral (VkFormatFeatureBitmask FlagMask)

deriving instance Num (VkFormatFeatureBitmask FlagMask)

deriving instance Bounded (VkFormatFeatureBitmask FlagMask)

deriving instance Enum (VkFormatFeatureBitmask FlagMask)

deriving instance Real (VkFormatFeatureBitmask FlagMask)

instance Show (VkFormatFeatureBitmask a) where
        showsPrec _ VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT
          = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT"
        showsPrec _ VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT
          = showString "VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT"
        showsPrec _ VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT
          = showString "VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT"
        showsPrec _ VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT
          = showString "VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT"
        showsPrec _ VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT
          = showString "VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT"
        showsPrec _ VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT
          = showString "VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT"
        showsPrec _ VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT
          = showString "VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT"
        showsPrec _ VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT
          = showString "VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT"
        showsPrec _ VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT
          = showString "VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT"
        showsPrec _ VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
          = showString "VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT"
        showsPrec _ VK_FORMAT_FEATURE_BLIT_SRC_BIT
          = showString "VK_FORMAT_FEATURE_BLIT_SRC_BIT"
        showsPrec _ VK_FORMAT_FEATURE_BLIT_DST_BIT
          = showString "VK_FORMAT_FEATURE_BLIT_DST_BIT"
        showsPrec _ VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
          = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT"
        showsPrec p (VkFormatFeatureBitmask x)
          = showParen (p >= 11)
              (showString "VkFormatFeatureBitmask " . showsPrec 11 x)

instance Read (VkFormatFeatureBitmask a) where
        readPrec
          = parens
              (choose
                 [("VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT",
                   pure VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT),
                  ("VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT",
                   pure VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT),
                  ("VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT",
                   pure VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT),
                  ("VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT",
                   pure VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT),
                  ("VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT",
                   pure VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT),
                  ("VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT",
                   pure VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT),
                  ("VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT",
                   pure VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT),
                  ("VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT",
                   pure VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT),
                  ("VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT",
                   pure VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT),
                  ("VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT",
                   pure VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT),
                  ("VK_FORMAT_FEATURE_BLIT_SRC_BIT",
                   pure VK_FORMAT_FEATURE_BLIT_SRC_BIT),
                  ("VK_FORMAT_FEATURE_BLIT_DST_BIT",
                   pure VK_FORMAT_FEATURE_BLIT_DST_BIT),
                  ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT",
                   pure VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT)]
                 +++
                 prec 10
                   (expectP (Ident "VkFormatFeatureBitmask") >>
                      (VkFormatFeatureBitmask <$> step readPrec)))

-- | Format can be used for sampled images (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
--
--   bitpos = @0@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT =
        VkFormatFeatureBitmask 1

-- | Format can be used for storage images (STORAGE_IMAGE descriptor type)
--
--   bitpos = @1@
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT =
        VkFormatFeatureBitmask 2

-- | Format supports atomic operations in case it is used for storage images
--
--   bitpos = @2@
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT =
        VkFormatFeatureBitmask 4

-- | Format can be used for uniform texel buffers (TBOs)
--
--   bitpos = @3@
pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT =
        VkFormatFeatureBitmask 8

-- | Format can be used for storage texel buffers (IBOs)
--
--   bitpos = @4@
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT =
        VkFormatFeatureBitmask 16

-- | Format supports atomic operations in case it is used for storage texel buffers
--
--   bitpos = @5@
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT =
        VkFormatFeatureBitmask 32

-- | Format can be used for vertex buffers (VBOs)
--
--   bitpos = @6@
pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT =
        VkFormatFeatureBitmask 64

-- | Format can be used for color attachment images
--
--   bitpos = @7@
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT =
        VkFormatFeatureBitmask 128

-- | Format supports blending in case it is used for color attachment images
--
--   bitpos = @8@
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT =
        VkFormatFeatureBitmask 256

-- | Format can be used for depth/stencil attachment images
--
--   bitpos = @9@
pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT =
        VkFormatFeatureBitmask 512

-- | Format can be used as the source image of blits with vkCmdBlitImage
--
--   bitpos = @10@
pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT :: VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT =
        VkFormatFeatureBitmask 1024

-- | Format can be used as the destination image of blits with vkCmdBlitImage
--
--   bitpos = @11@
pattern VK_FORMAT_FEATURE_BLIT_DST_BIT :: VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_BLIT_DST_BIT =
        VkFormatFeatureBitmask 2048

-- | Format can be filtered with VK_FILTER_LINEAR when being sampled
--
--   bitpos = @12@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT =
        VkFormatFeatureBitmask 4096
