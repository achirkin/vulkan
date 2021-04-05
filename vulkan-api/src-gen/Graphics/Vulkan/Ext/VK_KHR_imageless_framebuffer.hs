{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_imageless_framebuffer
       (-- * Vulkan extension: @VK_KHR_imageless_framebuffer@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Tobias Hector @tobias@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @109@
        --
        -- Required extensions: 'VK_KHR_maintenance2', 'VK_KHR_image_format_list'.
        --

        -- ** Required extensions: 'VK_KHR_maintenance2', 'VK_KHR_image_format_list'.
        VkFramebufferAttachmentImageInfoKHR,
        VkFramebufferAttachmentsCreateInfoKHR,
        VkPhysicalDeviceImagelessFramebufferFeaturesKHR,
        VkRenderPassAttachmentBeginInfoKHR,
        VK_KHR_IMAGELESS_FRAMEBUFFER_SPEC_VERSION,
        pattern VK_KHR_IMAGELESS_FRAMEBUFFER_SPEC_VERSION,
        VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME,
        pattern VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES_KHR,
        pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO_KHR,
        pattern VK_FRAMEBUFFER_CREATE_IMAGELESS_BIT_KHR)
       where
import GHC.Ptr                                     (Ptr (..))
import Graphics.Vulkan.Core_1_2                    (pattern VK_FRAMEBUFFER_CREATE_IMAGELESS_BIT,
                                                    pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO,
                                                    pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO,
                                                    pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES,
                                                    pattern VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Struct.Framebuffer    (VkFramebufferAttachmentImageInfoKHR,
                                                    VkFramebufferAttachmentsCreateInfoKHR)
import Graphics.Vulkan.Types.Struct.PhysicalDevice (VkPhysicalDeviceImagelessFramebufferFeaturesKHR)
import Graphics.Vulkan.Types.Struct.RenderPass     (VkRenderPassAttachmentBeginInfoKHR)

pattern VK_KHR_IMAGELESS_FRAMEBUFFER_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_IMAGELESS_FRAMEBUFFER_SPEC_VERSION = 1

type VK_KHR_IMAGELESS_FRAMEBUFFER_SPEC_VERSION = 1

pattern VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME :: CString

pattern VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME <-
        (is_VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME -> True)
  where
    VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME
      = _VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME

{-# INLINE _VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME #-}

_VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME :: CString
_VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME
  = Ptr "VK_KHR_imageless_framebuffer\NUL"#

{-# INLINE is_VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME #-}

is_VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME

type VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME =
     "VK_KHR_imageless_framebuffer"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES_KHR
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES

pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO_KHR =
        VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO

pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO_KHR =
        VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO

pattern VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO_KHR =
        VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO

pattern VK_FRAMEBUFFER_CREATE_IMAGELESS_BIT_KHR =
        VK_FRAMEBUFFER_CREATE_IMAGELESS_BIT
