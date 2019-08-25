{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_multiview
       (-- * Vulkan extension: @VK_KHR_multiview@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jeffbolznv@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @54@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkPhysicalDeviceMultiviewFeaturesKHR,
        VkPhysicalDeviceMultiviewPropertiesKHR,
        VkRenderPassMultiviewCreateInfoKHR, VK_KHR_MULTIVIEW_SPEC_VERSION,
        pattern VK_KHR_MULTIVIEW_SPEC_VERSION,
        VK_KHR_MULTIVIEW_EXTENSION_NAME,
        pattern VK_KHR_MULTIVIEW_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR,
        pattern VK_DEPENDENCY_VIEW_LOCAL_BIT_KHR)
       where
import           GHC.Ptr                                     (Ptr (..))
import           Graphics.Vulkan.Core_1_1                    (pattern VK_DEPENDENCY_VIEW_LOCAL_BIT,
                                                              pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES,
                                                              pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES,
                                                              pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Struct.PhysicalDevice (VkPhysicalDeviceMultiviewFeaturesKHR,
                                                              VkPhysicalDeviceMultiviewPropertiesKHR)
import           Graphics.Vulkan.Types.Struct.RenderPass     (VkRenderPassMultiviewCreateInfoKHR)

pattern VK_KHR_MULTIVIEW_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_MULTIVIEW_SPEC_VERSION = 1

type VK_KHR_MULTIVIEW_SPEC_VERSION = 1

pattern VK_KHR_MULTIVIEW_EXTENSION_NAME :: CString

pattern VK_KHR_MULTIVIEW_EXTENSION_NAME <-
        (is_VK_KHR_MULTIVIEW_EXTENSION_NAME -> True)
  where
    VK_KHR_MULTIVIEW_EXTENSION_NAME = _VK_KHR_MULTIVIEW_EXTENSION_NAME

{-# INLINE _VK_KHR_MULTIVIEW_EXTENSION_NAME #-}

_VK_KHR_MULTIVIEW_EXTENSION_NAME :: CString
_VK_KHR_MULTIVIEW_EXTENSION_NAME = Ptr "VK_KHR_multiview\NUL"#

{-# INLINE is_VK_KHR_MULTIVIEW_EXTENSION_NAME #-}

is_VK_KHR_MULTIVIEW_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_MULTIVIEW_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_MULTIVIEW_EXTENSION_NAME

type VK_KHR_MULTIVIEW_EXTENSION_NAME = "VK_KHR_multiview"

pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHR =
        VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHR =
        VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHR
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES

pattern VK_DEPENDENCY_VIEW_LOCAL_BIT_KHR =
        VK_DEPENDENCY_VIEW_LOCAL_BIT
