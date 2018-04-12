{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_maintenance2
       (-- * Vulkan extension: @VK_KHR_maintenance2@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Michael Worcester @michaelworcester@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @118@
        module Graphics.Vulkan.Types.Struct.Image,
        module Graphics.Vulkan.Types.Struct.InputAttachmentAspectReference,
        module Graphics.Vulkan.Types.Struct.PhysicalDevice,
        module Graphics.Vulkan.Types.Struct.Pipeline,
        module Graphics.Vulkan.Types.Enum.PointClippingBehavior,
        module Graphics.Vulkan.Types.Struct.RenderPass,
        module Graphics.Vulkan.Types.Enum.TessellationDomainOrigin,
        VK_KHR_MAINTENANCE2_SPEC_VERSION,
        pattern VK_KHR_MAINTENANCE2_SPEC_VERSION,
        VK_KHR_MAINTENANCE2_EXTENSION_NAME,
        pattern VK_KHR_MAINTENANCE2_EXTENSION_NAME,
        pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR,
        pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR,
        pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR,
        pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR,
        pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR,
        pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR,
        pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR,
        pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR)
       where
import           GHC.Ptr                                                     (Ptr (..))
import           Graphics.Vulkan.Core_1_1                                    (pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT,
                                                                              pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT,
                                                                              pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL,
                                                                              pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL,
                                                                              pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO,
                                                                              pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES,
                                                                              pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO,
                                                                              pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Enum.PointClippingBehavior
import           Graphics.Vulkan.Types.Enum.TessellationDomainOrigin
import           Graphics.Vulkan.Types.Struct.Image
import           Graphics.Vulkan.Types.Struct.InputAttachmentAspectReference
import           Graphics.Vulkan.Types.Struct.PhysicalDevice
import           Graphics.Vulkan.Types.Struct.Pipeline
import           Graphics.Vulkan.Types.Struct.RenderPass

pattern VK_KHR_MAINTENANCE2_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_MAINTENANCE2_SPEC_VERSION = 1

type VK_KHR_MAINTENANCE2_SPEC_VERSION = 1

pattern VK_KHR_MAINTENANCE2_EXTENSION_NAME :: CString

pattern VK_KHR_MAINTENANCE2_EXTENSION_NAME <-
        (is_VK_KHR_MAINTENANCE2_EXTENSION_NAME -> True)
  where VK_KHR_MAINTENANCE2_EXTENSION_NAME
          = _VK_KHR_MAINTENANCE2_EXTENSION_NAME

{-# INLINE _VK_KHR_MAINTENANCE2_EXTENSION_NAME #-}

_VK_KHR_MAINTENANCE2_EXTENSION_NAME :: CString
_VK_KHR_MAINTENANCE2_EXTENSION_NAME
  = Ptr "VK_KHR_maintenance2\NUL"#

{-# INLINE is_VK_KHR_MAINTENANCE2_EXTENSION_NAME #-}

is_VK_KHR_MAINTENANCE2_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_MAINTENANCE2_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_MAINTENANCE2_EXTENSION_NAME

type VK_KHR_MAINTENANCE2_EXTENSION_NAME = "VK_KHR_maintenance2"

pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR =
        VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT

pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR =
        VK_IMAGE_CREATE_EXTENDED_USAGE_BIT

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES

pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR
        = VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR =
        VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO

pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR
        =
        VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO

pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR
        = VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL

pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR
        = VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL

pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR =
        VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES

pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR =
        VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY

pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR =
        VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT

pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR =
        VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT
