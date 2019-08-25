{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_AMD_texture_gather_bias_lod
       (VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkExtent3D, VkImageFormatProperties, VkImageFormatProperties2,
        VkSampleCountBitmask(..), VkSampleCountFlagBits(),
        VkSampleCountFlags(), VkStructureType(..),
        VkTextureLODGatherFormatPropertiesAMD,
        -- > #include "vk_platform.h"
        VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION,
        pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION,
        VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME,
        pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD)
       where
import           GHC.Ptr
                                                                                   (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Struct.Extent
                                                                                   (VkExtent3D)
import           Graphics.Vulkan.Types.Struct.Image
                                                                                   (VkImageFormatProperties,
                                                                                   VkImageFormatProperties2)
import           Graphics.Vulkan.Types.Struct.TextureLODGatherFormatPropertiesAMD
                                                                                   (VkTextureLODGatherFormatPropertiesAMD)

pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION = 1

type VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION = 1

pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME :: CString

pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME <-
        (is_VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME -> True)
  where
    VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
      = _VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME

{-# INLINE _VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME #-}

_VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME :: CString
_VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
  = Ptr "VK_AMD_texture_gather_bias_lod\NUL"#

{-# INLINE is_VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME #-}

is_VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME :: CString -> Bool
is_VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME

type VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME =
     "VK_AMD_texture_gather_bias_lod"

pattern VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
        = VkStructureType 1000041000
