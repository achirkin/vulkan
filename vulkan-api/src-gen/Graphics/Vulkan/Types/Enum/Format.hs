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
module Graphics.Vulkan.Types.Enum.Format
       (VkFormat(VkFormat, VK_FORMAT_UNDEFINED,
                 VK_FORMAT_R4G4_UNORM_PACK8, VK_FORMAT_R4G4B4A4_UNORM_PACK16,
                 VK_FORMAT_B4G4R4A4_UNORM_PACK16, VK_FORMAT_R5G6B5_UNORM_PACK16,
                 VK_FORMAT_B5G6R5_UNORM_PACK16, VK_FORMAT_R5G5B5A1_UNORM_PACK16,
                 VK_FORMAT_B5G5R5A1_UNORM_PACK16, VK_FORMAT_A1R5G5B5_UNORM_PACK16,
                 VK_FORMAT_R8_UNORM, VK_FORMAT_R8_SNORM, VK_FORMAT_R8_USCALED,
                 VK_FORMAT_R8_SSCALED, VK_FORMAT_R8_UINT, VK_FORMAT_R8_SINT,
                 VK_FORMAT_R8_SRGB, VK_FORMAT_R8G8_UNORM, VK_FORMAT_R8G8_SNORM,
                 VK_FORMAT_R8G8_USCALED, VK_FORMAT_R8G8_SSCALED,
                 VK_FORMAT_R8G8_UINT, VK_FORMAT_R8G8_SINT, VK_FORMAT_R8G8_SRGB,
                 VK_FORMAT_R8G8B8_UNORM, VK_FORMAT_R8G8B8_SNORM,
                 VK_FORMAT_R8G8B8_USCALED, VK_FORMAT_R8G8B8_SSCALED,
                 VK_FORMAT_R8G8B8_UINT, VK_FORMAT_R8G8B8_SINT,
                 VK_FORMAT_R8G8B8_SRGB, VK_FORMAT_B8G8R8_UNORM,
                 VK_FORMAT_B8G8R8_SNORM, VK_FORMAT_B8G8R8_USCALED,
                 VK_FORMAT_B8G8R8_SSCALED, VK_FORMAT_B8G8R8_UINT,
                 VK_FORMAT_B8G8R8_SINT, VK_FORMAT_B8G8R8_SRGB,
                 VK_FORMAT_R8G8B8A8_UNORM, VK_FORMAT_R8G8B8A8_SNORM,
                 VK_FORMAT_R8G8B8A8_USCALED, VK_FORMAT_R8G8B8A8_SSCALED,
                 VK_FORMAT_R8G8B8A8_UINT, VK_FORMAT_R8G8B8A8_SINT,
                 VK_FORMAT_R8G8B8A8_SRGB, VK_FORMAT_B8G8R8A8_UNORM,
                 VK_FORMAT_B8G8R8A8_SNORM, VK_FORMAT_B8G8R8A8_USCALED,
                 VK_FORMAT_B8G8R8A8_SSCALED, VK_FORMAT_B8G8R8A8_UINT,
                 VK_FORMAT_B8G8R8A8_SINT, VK_FORMAT_B8G8R8A8_SRGB,
                 VK_FORMAT_A8B8G8R8_UNORM_PACK32, VK_FORMAT_A8B8G8R8_SNORM_PACK32,
                 VK_FORMAT_A8B8G8R8_USCALED_PACK32,
                 VK_FORMAT_A8B8G8R8_SSCALED_PACK32, VK_FORMAT_A8B8G8R8_UINT_PACK32,
                 VK_FORMAT_A8B8G8R8_SINT_PACK32, VK_FORMAT_A8B8G8R8_SRGB_PACK32,
                 VK_FORMAT_A2R10G10B10_UNORM_PACK32,
                 VK_FORMAT_A2R10G10B10_SNORM_PACK32,
                 VK_FORMAT_A2R10G10B10_USCALED_PACK32,
                 VK_FORMAT_A2R10G10B10_SSCALED_PACK32,
                 VK_FORMAT_A2R10G10B10_UINT_PACK32,
                 VK_FORMAT_A2R10G10B10_SINT_PACK32,
                 VK_FORMAT_A2B10G10R10_UNORM_PACK32,
                 VK_FORMAT_A2B10G10R10_SNORM_PACK32,
                 VK_FORMAT_A2B10G10R10_USCALED_PACK32,
                 VK_FORMAT_A2B10G10R10_SSCALED_PACK32,
                 VK_FORMAT_A2B10G10R10_UINT_PACK32,
                 VK_FORMAT_A2B10G10R10_SINT_PACK32, VK_FORMAT_R16_UNORM,
                 VK_FORMAT_R16_SNORM, VK_FORMAT_R16_USCALED, VK_FORMAT_R16_SSCALED,
                 VK_FORMAT_R16_UINT, VK_FORMAT_R16_SINT, VK_FORMAT_R16_SFLOAT,
                 VK_FORMAT_R16G16_UNORM, VK_FORMAT_R16G16_SNORM,
                 VK_FORMAT_R16G16_USCALED, VK_FORMAT_R16G16_SSCALED,
                 VK_FORMAT_R16G16_UINT, VK_FORMAT_R16G16_SINT,
                 VK_FORMAT_R16G16_SFLOAT, VK_FORMAT_R16G16B16_UNORM,
                 VK_FORMAT_R16G16B16_SNORM, VK_FORMAT_R16G16B16_USCALED,
                 VK_FORMAT_R16G16B16_SSCALED, VK_FORMAT_R16G16B16_UINT,
                 VK_FORMAT_R16G16B16_SINT, VK_FORMAT_R16G16B16_SFLOAT,
                 VK_FORMAT_R16G16B16A16_UNORM, VK_FORMAT_R16G16B16A16_SNORM,
                 VK_FORMAT_R16G16B16A16_USCALED, VK_FORMAT_R16G16B16A16_SSCALED,
                 VK_FORMAT_R16G16B16A16_UINT, VK_FORMAT_R16G16B16A16_SINT,
                 VK_FORMAT_R16G16B16A16_SFLOAT, VK_FORMAT_R32_UINT,
                 VK_FORMAT_R32_SINT, VK_FORMAT_R32_SFLOAT, VK_FORMAT_R32G32_UINT,
                 VK_FORMAT_R32G32_SINT, VK_FORMAT_R32G32_SFLOAT,
                 VK_FORMAT_R32G32B32_UINT, VK_FORMAT_R32G32B32_SINT,
                 VK_FORMAT_R32G32B32_SFLOAT, VK_FORMAT_R32G32B32A32_UINT,
                 VK_FORMAT_R32G32B32A32_SINT, VK_FORMAT_R32G32B32A32_SFLOAT,
                 VK_FORMAT_R64_UINT, VK_FORMAT_R64_SINT, VK_FORMAT_R64_SFLOAT,
                 VK_FORMAT_R64G64_UINT, VK_FORMAT_R64G64_SINT,
                 VK_FORMAT_R64G64_SFLOAT, VK_FORMAT_R64G64B64_UINT,
                 VK_FORMAT_R64G64B64_SINT, VK_FORMAT_R64G64B64_SFLOAT,
                 VK_FORMAT_R64G64B64A64_UINT, VK_FORMAT_R64G64B64A64_SINT,
                 VK_FORMAT_R64G64B64A64_SFLOAT, VK_FORMAT_B10G11R11_UFLOAT_PACK32,
                 VK_FORMAT_E5B9G9R9_UFLOAT_PACK32, VK_FORMAT_D16_UNORM,
                 VK_FORMAT_X8_D24_UNORM_PACK32, VK_FORMAT_D32_SFLOAT,
                 VK_FORMAT_S8_UINT, VK_FORMAT_D16_UNORM_S8_UINT,
                 VK_FORMAT_D24_UNORM_S8_UINT, VK_FORMAT_D32_SFLOAT_S8_UINT,
                 VK_FORMAT_BC1_RGB_UNORM_BLOCK, VK_FORMAT_BC1_RGB_SRGB_BLOCK,
                 VK_FORMAT_BC1_RGBA_UNORM_BLOCK, VK_FORMAT_BC1_RGBA_SRGB_BLOCK,
                 VK_FORMAT_BC2_UNORM_BLOCK, VK_FORMAT_BC2_SRGB_BLOCK,
                 VK_FORMAT_BC3_UNORM_BLOCK, VK_FORMAT_BC3_SRGB_BLOCK,
                 VK_FORMAT_BC4_UNORM_BLOCK, VK_FORMAT_BC4_SNORM_BLOCK,
                 VK_FORMAT_BC5_UNORM_BLOCK, VK_FORMAT_BC5_SNORM_BLOCK,
                 VK_FORMAT_BC6H_UFLOAT_BLOCK, VK_FORMAT_BC6H_SFLOAT_BLOCK,
                 VK_FORMAT_BC7_UNORM_BLOCK, VK_FORMAT_BC7_SRGB_BLOCK,
                 VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK,
                 VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK,
                 VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK,
                 VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK,
                 VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK,
                 VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK, VK_FORMAT_EAC_R11_UNORM_BLOCK,
                 VK_FORMAT_EAC_R11_SNORM_BLOCK, VK_FORMAT_EAC_R11G11_UNORM_BLOCK,
                 VK_FORMAT_EAC_R11G11_SNORM_BLOCK, VK_FORMAT_ASTC_4x4_UNORM_BLOCK,
                 VK_FORMAT_ASTC_4x4_SRGB_BLOCK, VK_FORMAT_ASTC_5x4_UNORM_BLOCK,
                 VK_FORMAT_ASTC_5x4_SRGB_BLOCK, VK_FORMAT_ASTC_5x5_UNORM_BLOCK,
                 VK_FORMAT_ASTC_5x5_SRGB_BLOCK, VK_FORMAT_ASTC_6x5_UNORM_BLOCK,
                 VK_FORMAT_ASTC_6x5_SRGB_BLOCK, VK_FORMAT_ASTC_6x6_UNORM_BLOCK,
                 VK_FORMAT_ASTC_6x6_SRGB_BLOCK, VK_FORMAT_ASTC_8x5_UNORM_BLOCK,
                 VK_FORMAT_ASTC_8x5_SRGB_BLOCK, VK_FORMAT_ASTC_8x6_UNORM_BLOCK,
                 VK_FORMAT_ASTC_8x6_SRGB_BLOCK, VK_FORMAT_ASTC_8x8_UNORM_BLOCK,
                 VK_FORMAT_ASTC_8x8_SRGB_BLOCK, VK_FORMAT_ASTC_10x5_UNORM_BLOCK,
                 VK_FORMAT_ASTC_10x5_SRGB_BLOCK, VK_FORMAT_ASTC_10x6_UNORM_BLOCK,
                 VK_FORMAT_ASTC_10x6_SRGB_BLOCK, VK_FORMAT_ASTC_10x8_UNORM_BLOCK,
                 VK_FORMAT_ASTC_10x8_SRGB_BLOCK, VK_FORMAT_ASTC_10x10_UNORM_BLOCK,
                 VK_FORMAT_ASTC_10x10_SRGB_BLOCK, VK_FORMAT_ASTC_12x10_UNORM_BLOCK,
                 VK_FORMAT_ASTC_12x10_SRGB_BLOCK, VK_FORMAT_ASTC_12x12_UNORM_BLOCK,
                 VK_FORMAT_ASTC_12x12_SRGB_BLOCK),
        VkFormatFeatureBitmask(VkFormatFeatureBitmask,
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
import           Graphics.Vulkan.Marshal         (FlagBit, FlagMask, FlagType,
                                                  Int32)
import           Graphics.Vulkan.Types.BaseTypes (VkFlags (..))
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | Vulkan format definitions
--
--   type = @enum@
--
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkFormat VkFormat registry at www.khronos.org>
newtype VkFormat = VkFormat Int32
                     deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkFormat where
        showsPrec _ VK_FORMAT_UNDEFINED = showString "VK_FORMAT_UNDEFINED"
        showsPrec _ VK_FORMAT_R4G4_UNORM_PACK8
          = showString "VK_FORMAT_R4G4_UNORM_PACK8"
        showsPrec _ VK_FORMAT_R4G4B4A4_UNORM_PACK16
          = showString "VK_FORMAT_R4G4B4A4_UNORM_PACK16"
        showsPrec _ VK_FORMAT_B4G4R4A4_UNORM_PACK16
          = showString "VK_FORMAT_B4G4R4A4_UNORM_PACK16"
        showsPrec _ VK_FORMAT_R5G6B5_UNORM_PACK16
          = showString "VK_FORMAT_R5G6B5_UNORM_PACK16"
        showsPrec _ VK_FORMAT_B5G6R5_UNORM_PACK16
          = showString "VK_FORMAT_B5G6R5_UNORM_PACK16"
        showsPrec _ VK_FORMAT_R5G5B5A1_UNORM_PACK16
          = showString "VK_FORMAT_R5G5B5A1_UNORM_PACK16"
        showsPrec _ VK_FORMAT_B5G5R5A1_UNORM_PACK16
          = showString "VK_FORMAT_B5G5R5A1_UNORM_PACK16"
        showsPrec _ VK_FORMAT_A1R5G5B5_UNORM_PACK16
          = showString "VK_FORMAT_A1R5G5B5_UNORM_PACK16"
        showsPrec _ VK_FORMAT_R8_UNORM = showString "VK_FORMAT_R8_UNORM"
        showsPrec _ VK_FORMAT_R8_SNORM = showString "VK_FORMAT_R8_SNORM"
        showsPrec _ VK_FORMAT_R8_USCALED
          = showString "VK_FORMAT_R8_USCALED"
        showsPrec _ VK_FORMAT_R8_SSCALED
          = showString "VK_FORMAT_R8_SSCALED"
        showsPrec _ VK_FORMAT_R8_UINT = showString "VK_FORMAT_R8_UINT"
        showsPrec _ VK_FORMAT_R8_SINT = showString "VK_FORMAT_R8_SINT"
        showsPrec _ VK_FORMAT_R8_SRGB = showString "VK_FORMAT_R8_SRGB"
        showsPrec _ VK_FORMAT_R8G8_UNORM
          = showString "VK_FORMAT_R8G8_UNORM"
        showsPrec _ VK_FORMAT_R8G8_SNORM
          = showString "VK_FORMAT_R8G8_SNORM"
        showsPrec _ VK_FORMAT_R8G8_USCALED
          = showString "VK_FORMAT_R8G8_USCALED"
        showsPrec _ VK_FORMAT_R8G8_SSCALED
          = showString "VK_FORMAT_R8G8_SSCALED"
        showsPrec _ VK_FORMAT_R8G8_UINT = showString "VK_FORMAT_R8G8_UINT"
        showsPrec _ VK_FORMAT_R8G8_SINT = showString "VK_FORMAT_R8G8_SINT"
        showsPrec _ VK_FORMAT_R8G8_SRGB = showString "VK_FORMAT_R8G8_SRGB"
        showsPrec _ VK_FORMAT_R8G8B8_UNORM
          = showString "VK_FORMAT_R8G8B8_UNORM"
        showsPrec _ VK_FORMAT_R8G8B8_SNORM
          = showString "VK_FORMAT_R8G8B8_SNORM"
        showsPrec _ VK_FORMAT_R8G8B8_USCALED
          = showString "VK_FORMAT_R8G8B8_USCALED"
        showsPrec _ VK_FORMAT_R8G8B8_SSCALED
          = showString "VK_FORMAT_R8G8B8_SSCALED"
        showsPrec _ VK_FORMAT_R8G8B8_UINT
          = showString "VK_FORMAT_R8G8B8_UINT"
        showsPrec _ VK_FORMAT_R8G8B8_SINT
          = showString "VK_FORMAT_R8G8B8_SINT"
        showsPrec _ VK_FORMAT_R8G8B8_SRGB
          = showString "VK_FORMAT_R8G8B8_SRGB"
        showsPrec _ VK_FORMAT_B8G8R8_UNORM
          = showString "VK_FORMAT_B8G8R8_UNORM"
        showsPrec _ VK_FORMAT_B8G8R8_SNORM
          = showString "VK_FORMAT_B8G8R8_SNORM"
        showsPrec _ VK_FORMAT_B8G8R8_USCALED
          = showString "VK_FORMAT_B8G8R8_USCALED"
        showsPrec _ VK_FORMAT_B8G8R8_SSCALED
          = showString "VK_FORMAT_B8G8R8_SSCALED"
        showsPrec _ VK_FORMAT_B8G8R8_UINT
          = showString "VK_FORMAT_B8G8R8_UINT"
        showsPrec _ VK_FORMAT_B8G8R8_SINT
          = showString "VK_FORMAT_B8G8R8_SINT"
        showsPrec _ VK_FORMAT_B8G8R8_SRGB
          = showString "VK_FORMAT_B8G8R8_SRGB"
        showsPrec _ VK_FORMAT_R8G8B8A8_UNORM
          = showString "VK_FORMAT_R8G8B8A8_UNORM"
        showsPrec _ VK_FORMAT_R8G8B8A8_SNORM
          = showString "VK_FORMAT_R8G8B8A8_SNORM"
        showsPrec _ VK_FORMAT_R8G8B8A8_USCALED
          = showString "VK_FORMAT_R8G8B8A8_USCALED"
        showsPrec _ VK_FORMAT_R8G8B8A8_SSCALED
          = showString "VK_FORMAT_R8G8B8A8_SSCALED"
        showsPrec _ VK_FORMAT_R8G8B8A8_UINT
          = showString "VK_FORMAT_R8G8B8A8_UINT"
        showsPrec _ VK_FORMAT_R8G8B8A8_SINT
          = showString "VK_FORMAT_R8G8B8A8_SINT"
        showsPrec _ VK_FORMAT_R8G8B8A8_SRGB
          = showString "VK_FORMAT_R8G8B8A8_SRGB"
        showsPrec _ VK_FORMAT_B8G8R8A8_UNORM
          = showString "VK_FORMAT_B8G8R8A8_UNORM"
        showsPrec _ VK_FORMAT_B8G8R8A8_SNORM
          = showString "VK_FORMAT_B8G8R8A8_SNORM"
        showsPrec _ VK_FORMAT_B8G8R8A8_USCALED
          = showString "VK_FORMAT_B8G8R8A8_USCALED"
        showsPrec _ VK_FORMAT_B8G8R8A8_SSCALED
          = showString "VK_FORMAT_B8G8R8A8_SSCALED"
        showsPrec _ VK_FORMAT_B8G8R8A8_UINT
          = showString "VK_FORMAT_B8G8R8A8_UINT"
        showsPrec _ VK_FORMAT_B8G8R8A8_SINT
          = showString "VK_FORMAT_B8G8R8A8_SINT"
        showsPrec _ VK_FORMAT_B8G8R8A8_SRGB
          = showString "VK_FORMAT_B8G8R8A8_SRGB"
        showsPrec _ VK_FORMAT_A8B8G8R8_UNORM_PACK32
          = showString "VK_FORMAT_A8B8G8R8_UNORM_PACK32"
        showsPrec _ VK_FORMAT_A8B8G8R8_SNORM_PACK32
          = showString "VK_FORMAT_A8B8G8R8_SNORM_PACK32"
        showsPrec _ VK_FORMAT_A8B8G8R8_USCALED_PACK32
          = showString "VK_FORMAT_A8B8G8R8_USCALED_PACK32"
        showsPrec _ VK_FORMAT_A8B8G8R8_SSCALED_PACK32
          = showString "VK_FORMAT_A8B8G8R8_SSCALED_PACK32"
        showsPrec _ VK_FORMAT_A8B8G8R8_UINT_PACK32
          = showString "VK_FORMAT_A8B8G8R8_UINT_PACK32"
        showsPrec _ VK_FORMAT_A8B8G8R8_SINT_PACK32
          = showString "VK_FORMAT_A8B8G8R8_SINT_PACK32"
        showsPrec _ VK_FORMAT_A8B8G8R8_SRGB_PACK32
          = showString "VK_FORMAT_A8B8G8R8_SRGB_PACK32"
        showsPrec _ VK_FORMAT_A2R10G10B10_UNORM_PACK32
          = showString "VK_FORMAT_A2R10G10B10_UNORM_PACK32"
        showsPrec _ VK_FORMAT_A2R10G10B10_SNORM_PACK32
          = showString "VK_FORMAT_A2R10G10B10_SNORM_PACK32"
        showsPrec _ VK_FORMAT_A2R10G10B10_USCALED_PACK32
          = showString "VK_FORMAT_A2R10G10B10_USCALED_PACK32"
        showsPrec _ VK_FORMAT_A2R10G10B10_SSCALED_PACK32
          = showString "VK_FORMAT_A2R10G10B10_SSCALED_PACK32"
        showsPrec _ VK_FORMAT_A2R10G10B10_UINT_PACK32
          = showString "VK_FORMAT_A2R10G10B10_UINT_PACK32"
        showsPrec _ VK_FORMAT_A2R10G10B10_SINT_PACK32
          = showString "VK_FORMAT_A2R10G10B10_SINT_PACK32"
        showsPrec _ VK_FORMAT_A2B10G10R10_UNORM_PACK32
          = showString "VK_FORMAT_A2B10G10R10_UNORM_PACK32"
        showsPrec _ VK_FORMAT_A2B10G10R10_SNORM_PACK32
          = showString "VK_FORMAT_A2B10G10R10_SNORM_PACK32"
        showsPrec _ VK_FORMAT_A2B10G10R10_USCALED_PACK32
          = showString "VK_FORMAT_A2B10G10R10_USCALED_PACK32"
        showsPrec _ VK_FORMAT_A2B10G10R10_SSCALED_PACK32
          = showString "VK_FORMAT_A2B10G10R10_SSCALED_PACK32"
        showsPrec _ VK_FORMAT_A2B10G10R10_UINT_PACK32
          = showString "VK_FORMAT_A2B10G10R10_UINT_PACK32"
        showsPrec _ VK_FORMAT_A2B10G10R10_SINT_PACK32
          = showString "VK_FORMAT_A2B10G10R10_SINT_PACK32"
        showsPrec _ VK_FORMAT_R16_UNORM = showString "VK_FORMAT_R16_UNORM"
        showsPrec _ VK_FORMAT_R16_SNORM = showString "VK_FORMAT_R16_SNORM"
        showsPrec _ VK_FORMAT_R16_USCALED
          = showString "VK_FORMAT_R16_USCALED"
        showsPrec _ VK_FORMAT_R16_SSCALED
          = showString "VK_FORMAT_R16_SSCALED"
        showsPrec _ VK_FORMAT_R16_UINT = showString "VK_FORMAT_R16_UINT"
        showsPrec _ VK_FORMAT_R16_SINT = showString "VK_FORMAT_R16_SINT"
        showsPrec _ VK_FORMAT_R16_SFLOAT
          = showString "VK_FORMAT_R16_SFLOAT"
        showsPrec _ VK_FORMAT_R16G16_UNORM
          = showString "VK_FORMAT_R16G16_UNORM"
        showsPrec _ VK_FORMAT_R16G16_SNORM
          = showString "VK_FORMAT_R16G16_SNORM"
        showsPrec _ VK_FORMAT_R16G16_USCALED
          = showString "VK_FORMAT_R16G16_USCALED"
        showsPrec _ VK_FORMAT_R16G16_SSCALED
          = showString "VK_FORMAT_R16G16_SSCALED"
        showsPrec _ VK_FORMAT_R16G16_UINT
          = showString "VK_FORMAT_R16G16_UINT"
        showsPrec _ VK_FORMAT_R16G16_SINT
          = showString "VK_FORMAT_R16G16_SINT"
        showsPrec _ VK_FORMAT_R16G16_SFLOAT
          = showString "VK_FORMAT_R16G16_SFLOAT"
        showsPrec _ VK_FORMAT_R16G16B16_UNORM
          = showString "VK_FORMAT_R16G16B16_UNORM"
        showsPrec _ VK_FORMAT_R16G16B16_SNORM
          = showString "VK_FORMAT_R16G16B16_SNORM"
        showsPrec _ VK_FORMAT_R16G16B16_USCALED
          = showString "VK_FORMAT_R16G16B16_USCALED"
        showsPrec _ VK_FORMAT_R16G16B16_SSCALED
          = showString "VK_FORMAT_R16G16B16_SSCALED"
        showsPrec _ VK_FORMAT_R16G16B16_UINT
          = showString "VK_FORMAT_R16G16B16_UINT"
        showsPrec _ VK_FORMAT_R16G16B16_SINT
          = showString "VK_FORMAT_R16G16B16_SINT"
        showsPrec _ VK_FORMAT_R16G16B16_SFLOAT
          = showString "VK_FORMAT_R16G16B16_SFLOAT"
        showsPrec _ VK_FORMAT_R16G16B16A16_UNORM
          = showString "VK_FORMAT_R16G16B16A16_UNORM"
        showsPrec _ VK_FORMAT_R16G16B16A16_SNORM
          = showString "VK_FORMAT_R16G16B16A16_SNORM"
        showsPrec _ VK_FORMAT_R16G16B16A16_USCALED
          = showString "VK_FORMAT_R16G16B16A16_USCALED"
        showsPrec _ VK_FORMAT_R16G16B16A16_SSCALED
          = showString "VK_FORMAT_R16G16B16A16_SSCALED"
        showsPrec _ VK_FORMAT_R16G16B16A16_UINT
          = showString "VK_FORMAT_R16G16B16A16_UINT"
        showsPrec _ VK_FORMAT_R16G16B16A16_SINT
          = showString "VK_FORMAT_R16G16B16A16_SINT"
        showsPrec _ VK_FORMAT_R16G16B16A16_SFLOAT
          = showString "VK_FORMAT_R16G16B16A16_SFLOAT"
        showsPrec _ VK_FORMAT_R32_UINT = showString "VK_FORMAT_R32_UINT"
        showsPrec _ VK_FORMAT_R32_SINT = showString "VK_FORMAT_R32_SINT"
        showsPrec _ VK_FORMAT_R32_SFLOAT
          = showString "VK_FORMAT_R32_SFLOAT"
        showsPrec _ VK_FORMAT_R32G32_UINT
          = showString "VK_FORMAT_R32G32_UINT"
        showsPrec _ VK_FORMAT_R32G32_SINT
          = showString "VK_FORMAT_R32G32_SINT"
        showsPrec _ VK_FORMAT_R32G32_SFLOAT
          = showString "VK_FORMAT_R32G32_SFLOAT"
        showsPrec _ VK_FORMAT_R32G32B32_UINT
          = showString "VK_FORMAT_R32G32B32_UINT"
        showsPrec _ VK_FORMAT_R32G32B32_SINT
          = showString "VK_FORMAT_R32G32B32_SINT"
        showsPrec _ VK_FORMAT_R32G32B32_SFLOAT
          = showString "VK_FORMAT_R32G32B32_SFLOAT"
        showsPrec _ VK_FORMAT_R32G32B32A32_UINT
          = showString "VK_FORMAT_R32G32B32A32_UINT"
        showsPrec _ VK_FORMAT_R32G32B32A32_SINT
          = showString "VK_FORMAT_R32G32B32A32_SINT"
        showsPrec _ VK_FORMAT_R32G32B32A32_SFLOAT
          = showString "VK_FORMAT_R32G32B32A32_SFLOAT"
        showsPrec _ VK_FORMAT_R64_UINT = showString "VK_FORMAT_R64_UINT"
        showsPrec _ VK_FORMAT_R64_SINT = showString "VK_FORMAT_R64_SINT"
        showsPrec _ VK_FORMAT_R64_SFLOAT
          = showString "VK_FORMAT_R64_SFLOAT"
        showsPrec _ VK_FORMAT_R64G64_UINT
          = showString "VK_FORMAT_R64G64_UINT"
        showsPrec _ VK_FORMAT_R64G64_SINT
          = showString "VK_FORMAT_R64G64_SINT"
        showsPrec _ VK_FORMAT_R64G64_SFLOAT
          = showString "VK_FORMAT_R64G64_SFLOAT"
        showsPrec _ VK_FORMAT_R64G64B64_UINT
          = showString "VK_FORMAT_R64G64B64_UINT"
        showsPrec _ VK_FORMAT_R64G64B64_SINT
          = showString "VK_FORMAT_R64G64B64_SINT"
        showsPrec _ VK_FORMAT_R64G64B64_SFLOAT
          = showString "VK_FORMAT_R64G64B64_SFLOAT"
        showsPrec _ VK_FORMAT_R64G64B64A64_UINT
          = showString "VK_FORMAT_R64G64B64A64_UINT"
        showsPrec _ VK_FORMAT_R64G64B64A64_SINT
          = showString "VK_FORMAT_R64G64B64A64_SINT"
        showsPrec _ VK_FORMAT_R64G64B64A64_SFLOAT
          = showString "VK_FORMAT_R64G64B64A64_SFLOAT"
        showsPrec _ VK_FORMAT_B10G11R11_UFLOAT_PACK32
          = showString "VK_FORMAT_B10G11R11_UFLOAT_PACK32"
        showsPrec _ VK_FORMAT_E5B9G9R9_UFLOAT_PACK32
          = showString "VK_FORMAT_E5B9G9R9_UFLOAT_PACK32"
        showsPrec _ VK_FORMAT_D16_UNORM = showString "VK_FORMAT_D16_UNORM"
        showsPrec _ VK_FORMAT_X8_D24_UNORM_PACK32
          = showString "VK_FORMAT_X8_D24_UNORM_PACK32"
        showsPrec _ VK_FORMAT_D32_SFLOAT
          = showString "VK_FORMAT_D32_SFLOAT"
        showsPrec _ VK_FORMAT_S8_UINT = showString "VK_FORMAT_S8_UINT"
        showsPrec _ VK_FORMAT_D16_UNORM_S8_UINT
          = showString "VK_FORMAT_D16_UNORM_S8_UINT"
        showsPrec _ VK_FORMAT_D24_UNORM_S8_UINT
          = showString "VK_FORMAT_D24_UNORM_S8_UINT"
        showsPrec _ VK_FORMAT_D32_SFLOAT_S8_UINT
          = showString "VK_FORMAT_D32_SFLOAT_S8_UINT"
        showsPrec _ VK_FORMAT_BC1_RGB_UNORM_BLOCK
          = showString "VK_FORMAT_BC1_RGB_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC1_RGB_SRGB_BLOCK
          = showString "VK_FORMAT_BC1_RGB_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_BC1_RGBA_UNORM_BLOCK
          = showString "VK_FORMAT_BC1_RGBA_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC1_RGBA_SRGB_BLOCK
          = showString "VK_FORMAT_BC1_RGBA_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_BC2_UNORM_BLOCK
          = showString "VK_FORMAT_BC2_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC2_SRGB_BLOCK
          = showString "VK_FORMAT_BC2_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_BC3_UNORM_BLOCK
          = showString "VK_FORMAT_BC3_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC3_SRGB_BLOCK
          = showString "VK_FORMAT_BC3_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_BC4_UNORM_BLOCK
          = showString "VK_FORMAT_BC4_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC4_SNORM_BLOCK
          = showString "VK_FORMAT_BC4_SNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC5_UNORM_BLOCK
          = showString "VK_FORMAT_BC5_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC5_SNORM_BLOCK
          = showString "VK_FORMAT_BC5_SNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC6H_UFLOAT_BLOCK
          = showString "VK_FORMAT_BC6H_UFLOAT_BLOCK"
        showsPrec _ VK_FORMAT_BC6H_SFLOAT_BLOCK
          = showString "VK_FORMAT_BC6H_SFLOAT_BLOCK"
        showsPrec _ VK_FORMAT_BC7_UNORM_BLOCK
          = showString "VK_FORMAT_BC7_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_BC7_SRGB_BLOCK
          = showString "VK_FORMAT_BC7_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK
          = showString "VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK
          = showString "VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK
          = showString "VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK
          = showString "VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK
          = showString "VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK
          = showString "VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_EAC_R11_UNORM_BLOCK
          = showString "VK_FORMAT_EAC_R11_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_EAC_R11_SNORM_BLOCK
          = showString "VK_FORMAT_EAC_R11_SNORM_BLOCK"
        showsPrec _ VK_FORMAT_EAC_R11G11_UNORM_BLOCK
          = showString "VK_FORMAT_EAC_R11G11_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_EAC_R11G11_SNORM_BLOCK
          = showString "VK_FORMAT_EAC_R11G11_SNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_4x4_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_4x4_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_4x4_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_4x4_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_5x4_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_5x4_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_5x4_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_5x4_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_5x5_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_5x5_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_5x5_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_5x5_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_6x5_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_6x5_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_6x5_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_6x5_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_6x6_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_6x6_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_6x6_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_6x6_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_8x5_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_8x5_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_8x5_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_8x5_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_8x6_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_8x6_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_8x6_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_8x6_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_8x8_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_8x8_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_8x8_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_8x8_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_10x5_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_10x5_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_10x5_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_10x5_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_10x6_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_10x6_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_10x6_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_10x6_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_10x8_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_10x8_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_10x8_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_10x8_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_10x10_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_10x10_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_10x10_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_10x10_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_12x10_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_12x10_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_12x10_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_12x10_SRGB_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_12x12_UNORM_BLOCK
          = showString "VK_FORMAT_ASTC_12x12_UNORM_BLOCK"
        showsPrec _ VK_FORMAT_ASTC_12x12_SRGB_BLOCK
          = showString "VK_FORMAT_ASTC_12x12_SRGB_BLOCK"
        showsPrec p (VkFormat x)
          = showParen (p >= 11) (showString "VkFormat " . showsPrec 11 x)

instance Read VkFormat where
        readPrec
          = parens
              (choose
                 [("VK_FORMAT_UNDEFINED", pure VK_FORMAT_UNDEFINED),
                  ("VK_FORMAT_R4G4_UNORM_PACK8", pure VK_FORMAT_R4G4_UNORM_PACK8),
                  ("VK_FORMAT_R4G4B4A4_UNORM_PACK16",
                   pure VK_FORMAT_R4G4B4A4_UNORM_PACK16),
                  ("VK_FORMAT_B4G4R4A4_UNORM_PACK16",
                   pure VK_FORMAT_B4G4R4A4_UNORM_PACK16),
                  ("VK_FORMAT_R5G6B5_UNORM_PACK16",
                   pure VK_FORMAT_R5G6B5_UNORM_PACK16),
                  ("VK_FORMAT_B5G6R5_UNORM_PACK16",
                   pure VK_FORMAT_B5G6R5_UNORM_PACK16),
                  ("VK_FORMAT_R5G5B5A1_UNORM_PACK16",
                   pure VK_FORMAT_R5G5B5A1_UNORM_PACK16),
                  ("VK_FORMAT_B5G5R5A1_UNORM_PACK16",
                   pure VK_FORMAT_B5G5R5A1_UNORM_PACK16),
                  ("VK_FORMAT_A1R5G5B5_UNORM_PACK16",
                   pure VK_FORMAT_A1R5G5B5_UNORM_PACK16),
                  ("VK_FORMAT_R8_UNORM", pure VK_FORMAT_R8_UNORM),
                  ("VK_FORMAT_R8_SNORM", pure VK_FORMAT_R8_SNORM),
                  ("VK_FORMAT_R8_USCALED", pure VK_FORMAT_R8_USCALED),
                  ("VK_FORMAT_R8_SSCALED", pure VK_FORMAT_R8_SSCALED),
                  ("VK_FORMAT_R8_UINT", pure VK_FORMAT_R8_UINT),
                  ("VK_FORMAT_R8_SINT", pure VK_FORMAT_R8_SINT),
                  ("VK_FORMAT_R8_SRGB", pure VK_FORMAT_R8_SRGB),
                  ("VK_FORMAT_R8G8_UNORM", pure VK_FORMAT_R8G8_UNORM),
                  ("VK_FORMAT_R8G8_SNORM", pure VK_FORMAT_R8G8_SNORM),
                  ("VK_FORMAT_R8G8_USCALED", pure VK_FORMAT_R8G8_USCALED),
                  ("VK_FORMAT_R8G8_SSCALED", pure VK_FORMAT_R8G8_SSCALED),
                  ("VK_FORMAT_R8G8_UINT", pure VK_FORMAT_R8G8_UINT),
                  ("VK_FORMAT_R8G8_SINT", pure VK_FORMAT_R8G8_SINT),
                  ("VK_FORMAT_R8G8_SRGB", pure VK_FORMAT_R8G8_SRGB),
                  ("VK_FORMAT_R8G8B8_UNORM", pure VK_FORMAT_R8G8B8_UNORM),
                  ("VK_FORMAT_R8G8B8_SNORM", pure VK_FORMAT_R8G8B8_SNORM),
                  ("VK_FORMAT_R8G8B8_USCALED", pure VK_FORMAT_R8G8B8_USCALED),
                  ("VK_FORMAT_R8G8B8_SSCALED", pure VK_FORMAT_R8G8B8_SSCALED),
                  ("VK_FORMAT_R8G8B8_UINT", pure VK_FORMAT_R8G8B8_UINT),
                  ("VK_FORMAT_R8G8B8_SINT", pure VK_FORMAT_R8G8B8_SINT),
                  ("VK_FORMAT_R8G8B8_SRGB", pure VK_FORMAT_R8G8B8_SRGB),
                  ("VK_FORMAT_B8G8R8_UNORM", pure VK_FORMAT_B8G8R8_UNORM),
                  ("VK_FORMAT_B8G8R8_SNORM", pure VK_FORMAT_B8G8R8_SNORM),
                  ("VK_FORMAT_B8G8R8_USCALED", pure VK_FORMAT_B8G8R8_USCALED),
                  ("VK_FORMAT_B8G8R8_SSCALED", pure VK_FORMAT_B8G8R8_SSCALED),
                  ("VK_FORMAT_B8G8R8_UINT", pure VK_FORMAT_B8G8R8_UINT),
                  ("VK_FORMAT_B8G8R8_SINT", pure VK_FORMAT_B8G8R8_SINT),
                  ("VK_FORMAT_B8G8R8_SRGB", pure VK_FORMAT_B8G8R8_SRGB),
                  ("VK_FORMAT_R8G8B8A8_UNORM", pure VK_FORMAT_R8G8B8A8_UNORM),
                  ("VK_FORMAT_R8G8B8A8_SNORM", pure VK_FORMAT_R8G8B8A8_SNORM),
                  ("VK_FORMAT_R8G8B8A8_USCALED", pure VK_FORMAT_R8G8B8A8_USCALED),
                  ("VK_FORMAT_R8G8B8A8_SSCALED", pure VK_FORMAT_R8G8B8A8_SSCALED),
                  ("VK_FORMAT_R8G8B8A8_UINT", pure VK_FORMAT_R8G8B8A8_UINT),
                  ("VK_FORMAT_R8G8B8A8_SINT", pure VK_FORMAT_R8G8B8A8_SINT),
                  ("VK_FORMAT_R8G8B8A8_SRGB", pure VK_FORMAT_R8G8B8A8_SRGB),
                  ("VK_FORMAT_B8G8R8A8_UNORM", pure VK_FORMAT_B8G8R8A8_UNORM),
                  ("VK_FORMAT_B8G8R8A8_SNORM", pure VK_FORMAT_B8G8R8A8_SNORM),
                  ("VK_FORMAT_B8G8R8A8_USCALED", pure VK_FORMAT_B8G8R8A8_USCALED),
                  ("VK_FORMAT_B8G8R8A8_SSCALED", pure VK_FORMAT_B8G8R8A8_SSCALED),
                  ("VK_FORMAT_B8G8R8A8_UINT", pure VK_FORMAT_B8G8R8A8_UINT),
                  ("VK_FORMAT_B8G8R8A8_SINT", pure VK_FORMAT_B8G8R8A8_SINT),
                  ("VK_FORMAT_B8G8R8A8_SRGB", pure VK_FORMAT_B8G8R8A8_SRGB),
                  ("VK_FORMAT_A8B8G8R8_UNORM_PACK32",
                   pure VK_FORMAT_A8B8G8R8_UNORM_PACK32),
                  ("VK_FORMAT_A8B8G8R8_SNORM_PACK32",
                   pure VK_FORMAT_A8B8G8R8_SNORM_PACK32),
                  ("VK_FORMAT_A8B8G8R8_USCALED_PACK32",
                   pure VK_FORMAT_A8B8G8R8_USCALED_PACK32),
                  ("VK_FORMAT_A8B8G8R8_SSCALED_PACK32",
                   pure VK_FORMAT_A8B8G8R8_SSCALED_PACK32),
                  ("VK_FORMAT_A8B8G8R8_UINT_PACK32",
                   pure VK_FORMAT_A8B8G8R8_UINT_PACK32),
                  ("VK_FORMAT_A8B8G8R8_SINT_PACK32",
                   pure VK_FORMAT_A8B8G8R8_SINT_PACK32),
                  ("VK_FORMAT_A8B8G8R8_SRGB_PACK32",
                   pure VK_FORMAT_A8B8G8R8_SRGB_PACK32),
                  ("VK_FORMAT_A2R10G10B10_UNORM_PACK32",
                   pure VK_FORMAT_A2R10G10B10_UNORM_PACK32),
                  ("VK_FORMAT_A2R10G10B10_SNORM_PACK32",
                   pure VK_FORMAT_A2R10G10B10_SNORM_PACK32),
                  ("VK_FORMAT_A2R10G10B10_USCALED_PACK32",
                   pure VK_FORMAT_A2R10G10B10_USCALED_PACK32),
                  ("VK_FORMAT_A2R10G10B10_SSCALED_PACK32",
                   pure VK_FORMAT_A2R10G10B10_SSCALED_PACK32),
                  ("VK_FORMAT_A2R10G10B10_UINT_PACK32",
                   pure VK_FORMAT_A2R10G10B10_UINT_PACK32),
                  ("VK_FORMAT_A2R10G10B10_SINT_PACK32",
                   pure VK_FORMAT_A2R10G10B10_SINT_PACK32),
                  ("VK_FORMAT_A2B10G10R10_UNORM_PACK32",
                   pure VK_FORMAT_A2B10G10R10_UNORM_PACK32),
                  ("VK_FORMAT_A2B10G10R10_SNORM_PACK32",
                   pure VK_FORMAT_A2B10G10R10_SNORM_PACK32),
                  ("VK_FORMAT_A2B10G10R10_USCALED_PACK32",
                   pure VK_FORMAT_A2B10G10R10_USCALED_PACK32),
                  ("VK_FORMAT_A2B10G10R10_SSCALED_PACK32",
                   pure VK_FORMAT_A2B10G10R10_SSCALED_PACK32),
                  ("VK_FORMAT_A2B10G10R10_UINT_PACK32",
                   pure VK_FORMAT_A2B10G10R10_UINT_PACK32),
                  ("VK_FORMAT_A2B10G10R10_SINT_PACK32",
                   pure VK_FORMAT_A2B10G10R10_SINT_PACK32),
                  ("VK_FORMAT_R16_UNORM", pure VK_FORMAT_R16_UNORM),
                  ("VK_FORMAT_R16_SNORM", pure VK_FORMAT_R16_SNORM),
                  ("VK_FORMAT_R16_USCALED", pure VK_FORMAT_R16_USCALED),
                  ("VK_FORMAT_R16_SSCALED", pure VK_FORMAT_R16_SSCALED),
                  ("VK_FORMAT_R16_UINT", pure VK_FORMAT_R16_UINT),
                  ("VK_FORMAT_R16_SINT", pure VK_FORMAT_R16_SINT),
                  ("VK_FORMAT_R16_SFLOAT", pure VK_FORMAT_R16_SFLOAT),
                  ("VK_FORMAT_R16G16_UNORM", pure VK_FORMAT_R16G16_UNORM),
                  ("VK_FORMAT_R16G16_SNORM", pure VK_FORMAT_R16G16_SNORM),
                  ("VK_FORMAT_R16G16_USCALED", pure VK_FORMAT_R16G16_USCALED),
                  ("VK_FORMAT_R16G16_SSCALED", pure VK_FORMAT_R16G16_SSCALED),
                  ("VK_FORMAT_R16G16_UINT", pure VK_FORMAT_R16G16_UINT),
                  ("VK_FORMAT_R16G16_SINT", pure VK_FORMAT_R16G16_SINT),
                  ("VK_FORMAT_R16G16_SFLOAT", pure VK_FORMAT_R16G16_SFLOAT),
                  ("VK_FORMAT_R16G16B16_UNORM", pure VK_FORMAT_R16G16B16_UNORM),
                  ("VK_FORMAT_R16G16B16_SNORM", pure VK_FORMAT_R16G16B16_SNORM),
                  ("VK_FORMAT_R16G16B16_USCALED", pure VK_FORMAT_R16G16B16_USCALED),
                  ("VK_FORMAT_R16G16B16_SSCALED", pure VK_FORMAT_R16G16B16_SSCALED),
                  ("VK_FORMAT_R16G16B16_UINT", pure VK_FORMAT_R16G16B16_UINT),
                  ("VK_FORMAT_R16G16B16_SINT", pure VK_FORMAT_R16G16B16_SINT),
                  ("VK_FORMAT_R16G16B16_SFLOAT", pure VK_FORMAT_R16G16B16_SFLOAT),
                  ("VK_FORMAT_R16G16B16A16_UNORM",
                   pure VK_FORMAT_R16G16B16A16_UNORM),
                  ("VK_FORMAT_R16G16B16A16_SNORM",
                   pure VK_FORMAT_R16G16B16A16_SNORM),
                  ("VK_FORMAT_R16G16B16A16_USCALED",
                   pure VK_FORMAT_R16G16B16A16_USCALED),
                  ("VK_FORMAT_R16G16B16A16_SSCALED",
                   pure VK_FORMAT_R16G16B16A16_SSCALED),
                  ("VK_FORMAT_R16G16B16A16_UINT", pure VK_FORMAT_R16G16B16A16_UINT),
                  ("VK_FORMAT_R16G16B16A16_SINT", pure VK_FORMAT_R16G16B16A16_SINT),
                  ("VK_FORMAT_R16G16B16A16_SFLOAT",
                   pure VK_FORMAT_R16G16B16A16_SFLOAT),
                  ("VK_FORMAT_R32_UINT", pure VK_FORMAT_R32_UINT),
                  ("VK_FORMAT_R32_SINT", pure VK_FORMAT_R32_SINT),
                  ("VK_FORMAT_R32_SFLOAT", pure VK_FORMAT_R32_SFLOAT),
                  ("VK_FORMAT_R32G32_UINT", pure VK_FORMAT_R32G32_UINT),
                  ("VK_FORMAT_R32G32_SINT", pure VK_FORMAT_R32G32_SINT),
                  ("VK_FORMAT_R32G32_SFLOAT", pure VK_FORMAT_R32G32_SFLOAT),
                  ("VK_FORMAT_R32G32B32_UINT", pure VK_FORMAT_R32G32B32_UINT),
                  ("VK_FORMAT_R32G32B32_SINT", pure VK_FORMAT_R32G32B32_SINT),
                  ("VK_FORMAT_R32G32B32_SFLOAT", pure VK_FORMAT_R32G32B32_SFLOAT),
                  ("VK_FORMAT_R32G32B32A32_UINT", pure VK_FORMAT_R32G32B32A32_UINT),
                  ("VK_FORMAT_R32G32B32A32_SINT", pure VK_FORMAT_R32G32B32A32_SINT),
                  ("VK_FORMAT_R32G32B32A32_SFLOAT",
                   pure VK_FORMAT_R32G32B32A32_SFLOAT),
                  ("VK_FORMAT_R64_UINT", pure VK_FORMAT_R64_UINT),
                  ("VK_FORMAT_R64_SINT", pure VK_FORMAT_R64_SINT),
                  ("VK_FORMAT_R64_SFLOAT", pure VK_FORMAT_R64_SFLOAT),
                  ("VK_FORMAT_R64G64_UINT", pure VK_FORMAT_R64G64_UINT),
                  ("VK_FORMAT_R64G64_SINT", pure VK_FORMAT_R64G64_SINT),
                  ("VK_FORMAT_R64G64_SFLOAT", pure VK_FORMAT_R64G64_SFLOAT),
                  ("VK_FORMAT_R64G64B64_UINT", pure VK_FORMAT_R64G64B64_UINT),
                  ("VK_FORMAT_R64G64B64_SINT", pure VK_FORMAT_R64G64B64_SINT),
                  ("VK_FORMAT_R64G64B64_SFLOAT", pure VK_FORMAT_R64G64B64_SFLOAT),
                  ("VK_FORMAT_R64G64B64A64_UINT", pure VK_FORMAT_R64G64B64A64_UINT),
                  ("VK_FORMAT_R64G64B64A64_SINT", pure VK_FORMAT_R64G64B64A64_SINT),
                  ("VK_FORMAT_R64G64B64A64_SFLOAT",
                   pure VK_FORMAT_R64G64B64A64_SFLOAT),
                  ("VK_FORMAT_B10G11R11_UFLOAT_PACK32",
                   pure VK_FORMAT_B10G11R11_UFLOAT_PACK32),
                  ("VK_FORMAT_E5B9G9R9_UFLOAT_PACK32",
                   pure VK_FORMAT_E5B9G9R9_UFLOAT_PACK32),
                  ("VK_FORMAT_D16_UNORM", pure VK_FORMAT_D16_UNORM),
                  ("VK_FORMAT_X8_D24_UNORM_PACK32",
                   pure VK_FORMAT_X8_D24_UNORM_PACK32),
                  ("VK_FORMAT_D32_SFLOAT", pure VK_FORMAT_D32_SFLOAT),
                  ("VK_FORMAT_S8_UINT", pure VK_FORMAT_S8_UINT),
                  ("VK_FORMAT_D16_UNORM_S8_UINT", pure VK_FORMAT_D16_UNORM_S8_UINT),
                  ("VK_FORMAT_D24_UNORM_S8_UINT", pure VK_FORMAT_D24_UNORM_S8_UINT),
                  ("VK_FORMAT_D32_SFLOAT_S8_UINT",
                   pure VK_FORMAT_D32_SFLOAT_S8_UINT),
                  ("VK_FORMAT_BC1_RGB_UNORM_BLOCK",
                   pure VK_FORMAT_BC1_RGB_UNORM_BLOCK),
                  ("VK_FORMAT_BC1_RGB_SRGB_BLOCK",
                   pure VK_FORMAT_BC1_RGB_SRGB_BLOCK),
                  ("VK_FORMAT_BC1_RGBA_UNORM_BLOCK",
                   pure VK_FORMAT_BC1_RGBA_UNORM_BLOCK),
                  ("VK_FORMAT_BC1_RGBA_SRGB_BLOCK",
                   pure VK_FORMAT_BC1_RGBA_SRGB_BLOCK),
                  ("VK_FORMAT_BC2_UNORM_BLOCK", pure VK_FORMAT_BC2_UNORM_BLOCK),
                  ("VK_FORMAT_BC2_SRGB_BLOCK", pure VK_FORMAT_BC2_SRGB_BLOCK),
                  ("VK_FORMAT_BC3_UNORM_BLOCK", pure VK_FORMAT_BC3_UNORM_BLOCK),
                  ("VK_FORMAT_BC3_SRGB_BLOCK", pure VK_FORMAT_BC3_SRGB_BLOCK),
                  ("VK_FORMAT_BC4_UNORM_BLOCK", pure VK_FORMAT_BC4_UNORM_BLOCK),
                  ("VK_FORMAT_BC4_SNORM_BLOCK", pure VK_FORMAT_BC4_SNORM_BLOCK),
                  ("VK_FORMAT_BC5_UNORM_BLOCK", pure VK_FORMAT_BC5_UNORM_BLOCK),
                  ("VK_FORMAT_BC5_SNORM_BLOCK", pure VK_FORMAT_BC5_SNORM_BLOCK),
                  ("VK_FORMAT_BC6H_UFLOAT_BLOCK", pure VK_FORMAT_BC6H_UFLOAT_BLOCK),
                  ("VK_FORMAT_BC6H_SFLOAT_BLOCK", pure VK_FORMAT_BC6H_SFLOAT_BLOCK),
                  ("VK_FORMAT_BC7_UNORM_BLOCK", pure VK_FORMAT_BC7_UNORM_BLOCK),
                  ("VK_FORMAT_BC7_SRGB_BLOCK", pure VK_FORMAT_BC7_SRGB_BLOCK),
                  ("VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK",
                   pure VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK),
                  ("VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK",
                   pure VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK),
                  ("VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK",
                   pure VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK),
                  ("VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK",
                   pure VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK),
                  ("VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK",
                   pure VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK),
                  ("VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK",
                   pure VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK),
                  ("VK_FORMAT_EAC_R11_UNORM_BLOCK",
                   pure VK_FORMAT_EAC_R11_UNORM_BLOCK),
                  ("VK_FORMAT_EAC_R11_SNORM_BLOCK",
                   pure VK_FORMAT_EAC_R11_SNORM_BLOCK),
                  ("VK_FORMAT_EAC_R11G11_UNORM_BLOCK",
                   pure VK_FORMAT_EAC_R11G11_UNORM_BLOCK),
                  ("VK_FORMAT_EAC_R11G11_SNORM_BLOCK",
                   pure VK_FORMAT_EAC_R11G11_SNORM_BLOCK),
                  ("VK_FORMAT_ASTC_4x4_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_4x4_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_4x4_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_4x4_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_5x4_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_5x4_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_5x4_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_5x4_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_5x5_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_5x5_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_5x5_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_5x5_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_6x5_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_6x5_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_6x5_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_6x5_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_6x6_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_6x6_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_6x6_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_6x6_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_8x5_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_8x5_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_8x5_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_8x5_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_8x6_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_8x6_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_8x6_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_8x6_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_8x8_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_8x8_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_8x8_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_8x8_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_10x5_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_10x5_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_10x5_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_10x5_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_10x6_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_10x6_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_10x6_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_10x6_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_10x8_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_10x8_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_10x8_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_10x8_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_10x10_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_10x10_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_10x10_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_10x10_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_12x10_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_12x10_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_12x10_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_12x10_SRGB_BLOCK),
                  ("VK_FORMAT_ASTC_12x12_UNORM_BLOCK",
                   pure VK_FORMAT_ASTC_12x12_UNORM_BLOCK),
                  ("VK_FORMAT_ASTC_12x12_SRGB_BLOCK",
                   pure VK_FORMAT_ASTC_12x12_SRGB_BLOCK)]
                 +++
                 prec 10
                   (expectP (Ident "VkFormat") >> (VkFormat <$> step readPrec)))

pattern VK_FORMAT_UNDEFINED :: VkFormat

pattern VK_FORMAT_UNDEFINED = VkFormat 0

pattern VK_FORMAT_R4G4_UNORM_PACK8 :: VkFormat

pattern VK_FORMAT_R4G4_UNORM_PACK8 = VkFormat 1

pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16 :: VkFormat

pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16 = VkFormat 2

pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16 :: VkFormat

pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16 = VkFormat 3

pattern VK_FORMAT_R5G6B5_UNORM_PACK16 :: VkFormat

pattern VK_FORMAT_R5G6B5_UNORM_PACK16 = VkFormat 4

pattern VK_FORMAT_B5G6R5_UNORM_PACK16 :: VkFormat

pattern VK_FORMAT_B5G6R5_UNORM_PACK16 = VkFormat 5

pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16 :: VkFormat

pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16 = VkFormat 6

pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16 :: VkFormat

pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16 = VkFormat 7

pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16 :: VkFormat

pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16 = VkFormat 8

pattern VK_FORMAT_R8_UNORM :: VkFormat

pattern VK_FORMAT_R8_UNORM = VkFormat 9

pattern VK_FORMAT_R8_SNORM :: VkFormat

pattern VK_FORMAT_R8_SNORM = VkFormat 10

pattern VK_FORMAT_R8_USCALED :: VkFormat

pattern VK_FORMAT_R8_USCALED = VkFormat 11

pattern VK_FORMAT_R8_SSCALED :: VkFormat

pattern VK_FORMAT_R8_SSCALED = VkFormat 12

pattern VK_FORMAT_R8_UINT :: VkFormat

pattern VK_FORMAT_R8_UINT = VkFormat 13

pattern VK_FORMAT_R8_SINT :: VkFormat

pattern VK_FORMAT_R8_SINT = VkFormat 14

pattern VK_FORMAT_R8_SRGB :: VkFormat

pattern VK_FORMAT_R8_SRGB = VkFormat 15

pattern VK_FORMAT_R8G8_UNORM :: VkFormat

pattern VK_FORMAT_R8G8_UNORM = VkFormat 16

pattern VK_FORMAT_R8G8_SNORM :: VkFormat

pattern VK_FORMAT_R8G8_SNORM = VkFormat 17

pattern VK_FORMAT_R8G8_USCALED :: VkFormat

pattern VK_FORMAT_R8G8_USCALED = VkFormat 18

pattern VK_FORMAT_R8G8_SSCALED :: VkFormat

pattern VK_FORMAT_R8G8_SSCALED = VkFormat 19

pattern VK_FORMAT_R8G8_UINT :: VkFormat

pattern VK_FORMAT_R8G8_UINT = VkFormat 20

pattern VK_FORMAT_R8G8_SINT :: VkFormat

pattern VK_FORMAT_R8G8_SINT = VkFormat 21

pattern VK_FORMAT_R8G8_SRGB :: VkFormat

pattern VK_FORMAT_R8G8_SRGB = VkFormat 22

pattern VK_FORMAT_R8G8B8_UNORM :: VkFormat

pattern VK_FORMAT_R8G8B8_UNORM = VkFormat 23

pattern VK_FORMAT_R8G8B8_SNORM :: VkFormat

pattern VK_FORMAT_R8G8B8_SNORM = VkFormat 24

pattern VK_FORMAT_R8G8B8_USCALED :: VkFormat

pattern VK_FORMAT_R8G8B8_USCALED = VkFormat 25

pattern VK_FORMAT_R8G8B8_SSCALED :: VkFormat

pattern VK_FORMAT_R8G8B8_SSCALED = VkFormat 26

pattern VK_FORMAT_R8G8B8_UINT :: VkFormat

pattern VK_FORMAT_R8G8B8_UINT = VkFormat 27

pattern VK_FORMAT_R8G8B8_SINT :: VkFormat

pattern VK_FORMAT_R8G8B8_SINT = VkFormat 28

pattern VK_FORMAT_R8G8B8_SRGB :: VkFormat

pattern VK_FORMAT_R8G8B8_SRGB = VkFormat 29

pattern VK_FORMAT_B8G8R8_UNORM :: VkFormat

pattern VK_FORMAT_B8G8R8_UNORM = VkFormat 30

pattern VK_FORMAT_B8G8R8_SNORM :: VkFormat

pattern VK_FORMAT_B8G8R8_SNORM = VkFormat 31

pattern VK_FORMAT_B8G8R8_USCALED :: VkFormat

pattern VK_FORMAT_B8G8R8_USCALED = VkFormat 32

pattern VK_FORMAT_B8G8R8_SSCALED :: VkFormat

pattern VK_FORMAT_B8G8R8_SSCALED = VkFormat 33

pattern VK_FORMAT_B8G8R8_UINT :: VkFormat

pattern VK_FORMAT_B8G8R8_UINT = VkFormat 34

pattern VK_FORMAT_B8G8R8_SINT :: VkFormat

pattern VK_FORMAT_B8G8R8_SINT = VkFormat 35

pattern VK_FORMAT_B8G8R8_SRGB :: VkFormat

pattern VK_FORMAT_B8G8R8_SRGB = VkFormat 36

pattern VK_FORMAT_R8G8B8A8_UNORM :: VkFormat

pattern VK_FORMAT_R8G8B8A8_UNORM = VkFormat 37

pattern VK_FORMAT_R8G8B8A8_SNORM :: VkFormat

pattern VK_FORMAT_R8G8B8A8_SNORM = VkFormat 38

pattern VK_FORMAT_R8G8B8A8_USCALED :: VkFormat

pattern VK_FORMAT_R8G8B8A8_USCALED = VkFormat 39

pattern VK_FORMAT_R8G8B8A8_SSCALED :: VkFormat

pattern VK_FORMAT_R8G8B8A8_SSCALED = VkFormat 40

pattern VK_FORMAT_R8G8B8A8_UINT :: VkFormat

pattern VK_FORMAT_R8G8B8A8_UINT = VkFormat 41

pattern VK_FORMAT_R8G8B8A8_SINT :: VkFormat

pattern VK_FORMAT_R8G8B8A8_SINT = VkFormat 42

pattern VK_FORMAT_R8G8B8A8_SRGB :: VkFormat

pattern VK_FORMAT_R8G8B8A8_SRGB = VkFormat 43

pattern VK_FORMAT_B8G8R8A8_UNORM :: VkFormat

pattern VK_FORMAT_B8G8R8A8_UNORM = VkFormat 44

pattern VK_FORMAT_B8G8R8A8_SNORM :: VkFormat

pattern VK_FORMAT_B8G8R8A8_SNORM = VkFormat 45

pattern VK_FORMAT_B8G8R8A8_USCALED :: VkFormat

pattern VK_FORMAT_B8G8R8A8_USCALED = VkFormat 46

pattern VK_FORMAT_B8G8R8A8_SSCALED :: VkFormat

pattern VK_FORMAT_B8G8R8A8_SSCALED = VkFormat 47

pattern VK_FORMAT_B8G8R8A8_UINT :: VkFormat

pattern VK_FORMAT_B8G8R8A8_UINT = VkFormat 48

pattern VK_FORMAT_B8G8R8A8_SINT :: VkFormat

pattern VK_FORMAT_B8G8R8A8_SINT = VkFormat 49

pattern VK_FORMAT_B8G8R8A8_SRGB :: VkFormat

pattern VK_FORMAT_B8G8R8A8_SRGB = VkFormat 50

pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32 :: VkFormat

pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32 = VkFormat 51

pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32 :: VkFormat

pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32 = VkFormat 52

pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32 :: VkFormat

pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32 = VkFormat 53

pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32 :: VkFormat

pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32 = VkFormat 54

pattern VK_FORMAT_A8B8G8R8_UINT_PACK32 :: VkFormat

pattern VK_FORMAT_A8B8G8R8_UINT_PACK32 = VkFormat 55

pattern VK_FORMAT_A8B8G8R8_SINT_PACK32 :: VkFormat

pattern VK_FORMAT_A8B8G8R8_SINT_PACK32 = VkFormat 56

pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32 :: VkFormat

pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32 = VkFormat 57

pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32 :: VkFormat

pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32 = VkFormat 58

pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32 :: VkFormat

pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32 = VkFormat 59

pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32 :: VkFormat

pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32 = VkFormat 60

pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32 :: VkFormat

pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32 = VkFormat 61

pattern VK_FORMAT_A2R10G10B10_UINT_PACK32 :: VkFormat

pattern VK_FORMAT_A2R10G10B10_UINT_PACK32 = VkFormat 62

pattern VK_FORMAT_A2R10G10B10_SINT_PACK32 :: VkFormat

pattern VK_FORMAT_A2R10G10B10_SINT_PACK32 = VkFormat 63

pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32 :: VkFormat

pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32 = VkFormat 64

pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32 :: VkFormat

pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32 = VkFormat 65

pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32 :: VkFormat

pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32 = VkFormat 66

pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32 :: VkFormat

pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32 = VkFormat 67

pattern VK_FORMAT_A2B10G10R10_UINT_PACK32 :: VkFormat

pattern VK_FORMAT_A2B10G10R10_UINT_PACK32 = VkFormat 68

pattern VK_FORMAT_A2B10G10R10_SINT_PACK32 :: VkFormat

pattern VK_FORMAT_A2B10G10R10_SINT_PACK32 = VkFormat 69

pattern VK_FORMAT_R16_UNORM :: VkFormat

pattern VK_FORMAT_R16_UNORM = VkFormat 70

pattern VK_FORMAT_R16_SNORM :: VkFormat

pattern VK_FORMAT_R16_SNORM = VkFormat 71

pattern VK_FORMAT_R16_USCALED :: VkFormat

pattern VK_FORMAT_R16_USCALED = VkFormat 72

pattern VK_FORMAT_R16_SSCALED :: VkFormat

pattern VK_FORMAT_R16_SSCALED = VkFormat 73

pattern VK_FORMAT_R16_UINT :: VkFormat

pattern VK_FORMAT_R16_UINT = VkFormat 74

pattern VK_FORMAT_R16_SINT :: VkFormat

pattern VK_FORMAT_R16_SINT = VkFormat 75

pattern VK_FORMAT_R16_SFLOAT :: VkFormat

pattern VK_FORMAT_R16_SFLOAT = VkFormat 76

pattern VK_FORMAT_R16G16_UNORM :: VkFormat

pattern VK_FORMAT_R16G16_UNORM = VkFormat 77

pattern VK_FORMAT_R16G16_SNORM :: VkFormat

pattern VK_FORMAT_R16G16_SNORM = VkFormat 78

pattern VK_FORMAT_R16G16_USCALED :: VkFormat

pattern VK_FORMAT_R16G16_USCALED = VkFormat 79

pattern VK_FORMAT_R16G16_SSCALED :: VkFormat

pattern VK_FORMAT_R16G16_SSCALED = VkFormat 80

pattern VK_FORMAT_R16G16_UINT :: VkFormat

pattern VK_FORMAT_R16G16_UINT = VkFormat 81

pattern VK_FORMAT_R16G16_SINT :: VkFormat

pattern VK_FORMAT_R16G16_SINT = VkFormat 82

pattern VK_FORMAT_R16G16_SFLOAT :: VkFormat

pattern VK_FORMAT_R16G16_SFLOAT = VkFormat 83

pattern VK_FORMAT_R16G16B16_UNORM :: VkFormat

pattern VK_FORMAT_R16G16B16_UNORM = VkFormat 84

pattern VK_FORMAT_R16G16B16_SNORM :: VkFormat

pattern VK_FORMAT_R16G16B16_SNORM = VkFormat 85

pattern VK_FORMAT_R16G16B16_USCALED :: VkFormat

pattern VK_FORMAT_R16G16B16_USCALED = VkFormat 86

pattern VK_FORMAT_R16G16B16_SSCALED :: VkFormat

pattern VK_FORMAT_R16G16B16_SSCALED = VkFormat 87

pattern VK_FORMAT_R16G16B16_UINT :: VkFormat

pattern VK_FORMAT_R16G16B16_UINT = VkFormat 88

pattern VK_FORMAT_R16G16B16_SINT :: VkFormat

pattern VK_FORMAT_R16G16B16_SINT = VkFormat 89

pattern VK_FORMAT_R16G16B16_SFLOAT :: VkFormat

pattern VK_FORMAT_R16G16B16_SFLOAT = VkFormat 90

pattern VK_FORMAT_R16G16B16A16_UNORM :: VkFormat

pattern VK_FORMAT_R16G16B16A16_UNORM = VkFormat 91

pattern VK_FORMAT_R16G16B16A16_SNORM :: VkFormat

pattern VK_FORMAT_R16G16B16A16_SNORM = VkFormat 92

pattern VK_FORMAT_R16G16B16A16_USCALED :: VkFormat

pattern VK_FORMAT_R16G16B16A16_USCALED = VkFormat 93

pattern VK_FORMAT_R16G16B16A16_SSCALED :: VkFormat

pattern VK_FORMAT_R16G16B16A16_SSCALED = VkFormat 94

pattern VK_FORMAT_R16G16B16A16_UINT :: VkFormat

pattern VK_FORMAT_R16G16B16A16_UINT = VkFormat 95

pattern VK_FORMAT_R16G16B16A16_SINT :: VkFormat

pattern VK_FORMAT_R16G16B16A16_SINT = VkFormat 96

pattern VK_FORMAT_R16G16B16A16_SFLOAT :: VkFormat

pattern VK_FORMAT_R16G16B16A16_SFLOAT = VkFormat 97

pattern VK_FORMAT_R32_UINT :: VkFormat

pattern VK_FORMAT_R32_UINT = VkFormat 98

pattern VK_FORMAT_R32_SINT :: VkFormat

pattern VK_FORMAT_R32_SINT = VkFormat 99

pattern VK_FORMAT_R32_SFLOAT :: VkFormat

pattern VK_FORMAT_R32_SFLOAT = VkFormat 100

pattern VK_FORMAT_R32G32_UINT :: VkFormat

pattern VK_FORMAT_R32G32_UINT = VkFormat 101

pattern VK_FORMAT_R32G32_SINT :: VkFormat

pattern VK_FORMAT_R32G32_SINT = VkFormat 102

pattern VK_FORMAT_R32G32_SFLOAT :: VkFormat

pattern VK_FORMAT_R32G32_SFLOAT = VkFormat 103

pattern VK_FORMAT_R32G32B32_UINT :: VkFormat

pattern VK_FORMAT_R32G32B32_UINT = VkFormat 104

pattern VK_FORMAT_R32G32B32_SINT :: VkFormat

pattern VK_FORMAT_R32G32B32_SINT = VkFormat 105

pattern VK_FORMAT_R32G32B32_SFLOAT :: VkFormat

pattern VK_FORMAT_R32G32B32_SFLOAT = VkFormat 106

pattern VK_FORMAT_R32G32B32A32_UINT :: VkFormat

pattern VK_FORMAT_R32G32B32A32_UINT = VkFormat 107

pattern VK_FORMAT_R32G32B32A32_SINT :: VkFormat

pattern VK_FORMAT_R32G32B32A32_SINT = VkFormat 108

pattern VK_FORMAT_R32G32B32A32_SFLOAT :: VkFormat

pattern VK_FORMAT_R32G32B32A32_SFLOAT = VkFormat 109

pattern VK_FORMAT_R64_UINT :: VkFormat

pattern VK_FORMAT_R64_UINT = VkFormat 110

pattern VK_FORMAT_R64_SINT :: VkFormat

pattern VK_FORMAT_R64_SINT = VkFormat 111

pattern VK_FORMAT_R64_SFLOAT :: VkFormat

pattern VK_FORMAT_R64_SFLOAT = VkFormat 112

pattern VK_FORMAT_R64G64_UINT :: VkFormat

pattern VK_FORMAT_R64G64_UINT = VkFormat 113

pattern VK_FORMAT_R64G64_SINT :: VkFormat

pattern VK_FORMAT_R64G64_SINT = VkFormat 114

pattern VK_FORMAT_R64G64_SFLOAT :: VkFormat

pattern VK_FORMAT_R64G64_SFLOAT = VkFormat 115

pattern VK_FORMAT_R64G64B64_UINT :: VkFormat

pattern VK_FORMAT_R64G64B64_UINT = VkFormat 116

pattern VK_FORMAT_R64G64B64_SINT :: VkFormat

pattern VK_FORMAT_R64G64B64_SINT = VkFormat 117

pattern VK_FORMAT_R64G64B64_SFLOAT :: VkFormat

pattern VK_FORMAT_R64G64B64_SFLOAT = VkFormat 118

pattern VK_FORMAT_R64G64B64A64_UINT :: VkFormat

pattern VK_FORMAT_R64G64B64A64_UINT = VkFormat 119

pattern VK_FORMAT_R64G64B64A64_SINT :: VkFormat

pattern VK_FORMAT_R64G64B64A64_SINT = VkFormat 120

pattern VK_FORMAT_R64G64B64A64_SFLOAT :: VkFormat

pattern VK_FORMAT_R64G64B64A64_SFLOAT = VkFormat 121

pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32 :: VkFormat

pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32 = VkFormat 122

pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 :: VkFormat

pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 = VkFormat 123

pattern VK_FORMAT_D16_UNORM :: VkFormat

pattern VK_FORMAT_D16_UNORM = VkFormat 124

pattern VK_FORMAT_X8_D24_UNORM_PACK32 :: VkFormat

pattern VK_FORMAT_X8_D24_UNORM_PACK32 = VkFormat 125

pattern VK_FORMAT_D32_SFLOAT :: VkFormat

pattern VK_FORMAT_D32_SFLOAT = VkFormat 126

pattern VK_FORMAT_S8_UINT :: VkFormat

pattern VK_FORMAT_S8_UINT = VkFormat 127

pattern VK_FORMAT_D16_UNORM_S8_UINT :: VkFormat

pattern VK_FORMAT_D16_UNORM_S8_UINT = VkFormat 128

pattern VK_FORMAT_D24_UNORM_S8_UINT :: VkFormat

pattern VK_FORMAT_D24_UNORM_S8_UINT = VkFormat 129

pattern VK_FORMAT_D32_SFLOAT_S8_UINT :: VkFormat

pattern VK_FORMAT_D32_SFLOAT_S8_UINT = VkFormat 130

pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK = VkFormat 131

pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK = VkFormat 132

pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK = VkFormat 133

pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK = VkFormat 134

pattern VK_FORMAT_BC2_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC2_UNORM_BLOCK = VkFormat 135

pattern VK_FORMAT_BC2_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_BC2_SRGB_BLOCK = VkFormat 136

pattern VK_FORMAT_BC3_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC3_UNORM_BLOCK = VkFormat 137

pattern VK_FORMAT_BC3_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_BC3_SRGB_BLOCK = VkFormat 138

pattern VK_FORMAT_BC4_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC4_UNORM_BLOCK = VkFormat 139

pattern VK_FORMAT_BC4_SNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC4_SNORM_BLOCK = VkFormat 140

pattern VK_FORMAT_BC5_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC5_UNORM_BLOCK = VkFormat 141

pattern VK_FORMAT_BC5_SNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC5_SNORM_BLOCK = VkFormat 142

pattern VK_FORMAT_BC6H_UFLOAT_BLOCK :: VkFormat

pattern VK_FORMAT_BC6H_UFLOAT_BLOCK = VkFormat 143

pattern VK_FORMAT_BC6H_SFLOAT_BLOCK :: VkFormat

pattern VK_FORMAT_BC6H_SFLOAT_BLOCK = VkFormat 144

pattern VK_FORMAT_BC7_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_BC7_UNORM_BLOCK = VkFormat 145

pattern VK_FORMAT_BC7_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_BC7_SRGB_BLOCK = VkFormat 146

pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK = VkFormat 147

pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK = VkFormat 148

pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK = VkFormat 149

pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK = VkFormat 150

pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK = VkFormat 151

pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK = VkFormat 152

pattern VK_FORMAT_EAC_R11_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_EAC_R11_UNORM_BLOCK = VkFormat 153

pattern VK_FORMAT_EAC_R11_SNORM_BLOCK :: VkFormat

pattern VK_FORMAT_EAC_R11_SNORM_BLOCK = VkFormat 154

pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK = VkFormat 155

pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK :: VkFormat

pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK = VkFormat 156

pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK = VkFormat 157

pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK = VkFormat 158

pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK = VkFormat 159

pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK = VkFormat 160

pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK = VkFormat 161

pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK = VkFormat 162

pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK = VkFormat 163

pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK = VkFormat 164

pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK = VkFormat 165

pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK = VkFormat 166

pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK = VkFormat 167

pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK = VkFormat 168

pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK = VkFormat 169

pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK = VkFormat 170

pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK = VkFormat 171

pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK = VkFormat 172

pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK = VkFormat 173

pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK = VkFormat 174

pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK = VkFormat 175

pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK = VkFormat 176

pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK = VkFormat 177

pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK = VkFormat 178

pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK = VkFormat 179

pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK = VkFormat 180

pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK = VkFormat 181

pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK = VkFormat 182

pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK = VkFormat 183

pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK :: VkFormat

pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK = VkFormat 184

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
