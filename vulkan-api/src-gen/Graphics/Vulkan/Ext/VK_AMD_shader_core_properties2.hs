{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_AMD_shader_core_properties2
       (-- * Vulkan extension: @VK_AMD_shader_core_properties2@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Matthaeus G. Chajdas @anteru@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @228@
        --
        -- Required extensions: 'VK_AMD_shader_core_properties'.
        --

        -- ** Required extensions: 'VK_AMD_shader_core_properties'.
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkPhysicalDeviceLimits, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2,
        VkPhysicalDeviceShaderCoreProperties2AMD,
        VkPhysicalDeviceSparseProperties, VkPhysicalDeviceType(..),
        VkSampleCountBitmask(..), VkSampleCountFlagBits(),
        VkSampleCountFlags(), VkShaderFloatControlsIndependence(..),
        VkShaderInfoTypeAMD(..), VkShaderStageBitmask(..),
        VkShaderCorePropertiesBitmaskAMD(..),
        VkShaderCorePropertiesFlagBitsAMD(),
        VkShaderCorePropertiesFlagsAMD(),
        VkShaderFloatControlsIndependenceKHR(..),
        VkShaderModuleCreateBitmask(..), VkShaderModuleCreateFlagBits(),
        VkShaderModuleCreateFlags(), VkShaderStageFlagBits(),
        VkShaderStageFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION,
        pattern VK_AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION,
        VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME,
        pattern VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD)
       where
import GHC.Ptr                                       (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.Shader
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Struct.PhysicalDevice   (VkPhysicalDeviceLimits,
                                                      VkPhysicalDeviceProperties,
                                                      VkPhysicalDeviceProperties2,
                                                      VkPhysicalDeviceShaderCoreProperties2AMD,
                                                      VkPhysicalDeviceSparseProperties)

pattern VK_AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION = 1

type VK_AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION = 1

pattern VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME :: CString

pattern VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME <-
        (is_VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME -> True)
  where
    VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME
      = _VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME

{-# INLINE _VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME #-}

_VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME :: CString
_VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME
  = Ptr "VK_AMD_shader_core_properties2\NUL"#

{-# INLINE is_VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME #-}

is_VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME ::
                                                  CString -> Bool
is_VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME

type VK_AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME =
     "VK_AMD_shader_core_properties2"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD
        = VkStructureType 1000227000
