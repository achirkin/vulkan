{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_shader_float_controls
       (-- * Vulkan extension: @VK_KHR_shader_float_controls@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Alexander Galazin @alegal-arm@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @198@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkPhysicalDeviceFloatControlsPropertiesKHR,
        VkShaderFloatControlsIndependence(..), VkShaderInfoTypeAMD(..),
        VkShaderStageBitmask(..), VkShaderCorePropertiesBitmaskAMD(..),
        VkShaderCorePropertiesFlagBitsAMD(),
        VkShaderCorePropertiesFlagsAMD(),
        VkShaderFloatControlsIndependenceKHR(..),
        VkShaderModuleCreateBitmask(..), VkShaderModuleCreateFlagBits(),
        VkShaderModuleCreateFlags(), VkShaderStageFlagBits(),
        VkShaderStageFlags(), VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION,
        pattern VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION,
        VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME,
        pattern VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR,
        pattern VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY_KHR,
        pattern VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL_KHR,
        pattern VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE_KHR)
       where
import GHC.Ptr                                     (Ptr (..))
import Graphics.Vulkan.Core_1_2                    (pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Enum.Shader
import Graphics.Vulkan.Types.Struct.PhysicalDevice (VkPhysicalDeviceFloatControlsPropertiesKHR)

pattern VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION = 4

type VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION = 4

pattern VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME :: CString

pattern VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME <-
        (is_VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME -> True)
  where
    VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
      = _VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME

{-# INLINE _VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME #-}

_VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME :: CString
_VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
  = Ptr "VK_KHR_shader_float_controls\NUL"#

{-# INLINE is_VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME #-}

is_VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME

type VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME =
     "VK_KHR_shader_float_controls"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES

pattern VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY_KHR =
        VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_32_BIT_ONLY

pattern VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL_KHR =
        VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_ALL

pattern VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE_KHR =
        VK_SHADER_FLOAT_CONTROLS_INDEPENDENCE_NONE
