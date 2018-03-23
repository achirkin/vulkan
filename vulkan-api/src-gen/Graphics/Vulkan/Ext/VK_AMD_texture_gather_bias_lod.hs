{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_AMD_texture_gather_bias_lod
       (-- * Vulkan extension: @VK_AMD_texture_gather_bias_lod@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Rex Xu @amdrexu@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @42@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties2,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Struct.VkTextureLODGatherFormatPropertiesAMD,
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
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Struct.VkExtent3D
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties2
import           Graphics.Vulkan.Types.Struct.VkTextureLODGatherFormatPropertiesAMD

pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION = 1

type VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION = 1

pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME :: CString

pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME <-
        (is_VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME -> True)
  where VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
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
