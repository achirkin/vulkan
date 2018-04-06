{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_AMD_shader_core_properties
       (-- * Vulkan extension: @VK_AMD_shader_core_properties@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Martin Dinkov @mdinkov@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @186@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceShaderCorePropertiesAMD,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties,
        module Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION,
        pattern VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION,
        VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME,
        pattern VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD)
       where
import           GHC.Ptr
                                                                                       (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceShaderCorePropertiesAMD
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties

pattern VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION = 1

type VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION = 1

pattern VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME :: CString

pattern VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME <-
        (is_VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME -> True)
  where VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME
          = _VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME

{-# INLINE _VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME #-}

_VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME :: CString
_VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME
  = Ptr "VK_AMD_shader_core_properties\NUL"#

{-# INLINE is_VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME #-}

is_VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME :: CString -> Bool
is_VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME

type VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME =
     "VK_AMD_shader_core_properties"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD
        = VkStructureType 1000185000
