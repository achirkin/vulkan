{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_separate_depth_stencil_layouts
       (-- * Vulkan extension: @VK_KHR_separate_depth_stencil_layouts@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Piers Daniell @pdaniell-nv@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @242@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_create_renderpass2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_create_renderpass2'.
        VkAttachmentDescriptionStencilLayoutKHR,
        VkAttachmentReferenceStencilLayoutKHR,
        VkPhysicalDeviceSeparateDepthStencilLayoutsFeaturesKHR,
        VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_SPEC_VERSION,
        pattern VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_SPEC_VERSION,
        VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME,
        pattern VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES_KHR,
        pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT_KHR,
        pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT_KHR,
        pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR,
        pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL_KHR,
        pattern VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL_KHR,
        pattern VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL_KHR)
       where
import GHC.Ptr                                     (Ptr (..))
import Graphics.Vulkan.Core_1_2                    (pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL,
                                                    pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL,
                                                    pattern VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL,
                                                    pattern VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL,
                                                    pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT,
                                                    pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT,
                                                    pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Struct.Attachment     (VkAttachmentDescriptionStencilLayoutKHR,
                                                    VkAttachmentReferenceStencilLayoutKHR)
import Graphics.Vulkan.Types.Struct.PhysicalDevice (VkPhysicalDeviceSeparateDepthStencilLayoutsFeaturesKHR)

pattern VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_SPEC_VERSION = 1

type VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_SPEC_VERSION = 1

pattern VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME ::
        CString

pattern VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME <-
        (is_VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME -> True)
  where
    VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME
      = _VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME

{-# INLINE _VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME
           #-}

_VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME :: CString
_VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME
  = Ptr "VK_KHR_separate_depth_stencil_layouts\NUL"#

{-# INLINE is_VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME
           #-}

is_VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME ::
                                                        CString -> Bool
is_VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME

type VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME =
     "VK_KHR_separate_depth_stencil_layouts"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES_KHR
        =
        VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES

pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT_KHR =
        VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT

pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT_KHR
        = VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT

pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR =
        VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL

pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL_KHR =
        VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL

pattern VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL_KHR =
        VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL

pattern VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL_KHR =
        VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL
