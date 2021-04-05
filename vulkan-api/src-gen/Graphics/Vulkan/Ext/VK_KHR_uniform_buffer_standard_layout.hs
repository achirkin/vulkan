{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_uniform_buffer_standard_layout
       (-- * Vulkan extension: @VK_KHR_uniform_buffer_standard_layout@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Graeme Leese @gnl21@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @254@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkPhysicalDeviceUniformBufferStandardLayoutFeaturesKHR,
        VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION,
        pattern VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION,
        VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME,
        pattern VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES_KHR)
       where
import GHC.Ptr                                     (Ptr (..))
import Graphics.Vulkan.Core_1_2                    (pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Struct.PhysicalDevice (VkPhysicalDeviceUniformBufferStandardLayoutFeaturesKHR)

pattern VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION = 1

type VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION = 1

pattern VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME ::
        CString

pattern VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME <-
        (is_VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME -> True)
  where
    VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME
      = _VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME

{-# INLINE _VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME
           #-}

_VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME :: CString
_VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME
  = Ptr "VK_KHR_uniform_buffer_standard_layout\NUL"#

{-# INLINE is_VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME
           #-}

is_VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME ::
                                                        CString -> Bool
is_VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME

type VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME =
     "VK_KHR_uniform_buffer_standard_layout"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES_KHR
        =
        VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES
