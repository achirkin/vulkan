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
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkAccessFlags,
        module Graphics.Vulkan.Types.Struct.VkAttachmentDescription,
        module Graphics.Vulkan.Types.Enum.VkAttachmentDescriptionFlags,
        module Graphics.Vulkan.Types.Enum.VkAttachmentLoadOp,
        module Graphics.Vulkan.Types.Struct.VkAttachmentReference,
        module Graphics.Vulkan.Types.Enum.VkAttachmentStoreOp,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkComponentMapping,
        module Graphics.Vulkan.Types.Enum.VkComponentSwizzle,
        module Graphics.Vulkan.Types.Enum.VkDependencyFlags,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkImageAspectFlags,
        module Graphics.Vulkan.Types.Enum.VkImageLayout,
        module Graphics.Vulkan.Types.Struct.VkImageSubresourceRange,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkImageViewCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkImageViewType,
        module Graphics.Vulkan.Types.Struct.VkImageViewUsageCreateInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkInputAttachmentAspectReferenceKHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDevicePointClippingPropertiesKHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties,
        module Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType,
        module Graphics.Vulkan.Types.Enum.VkPipelineBindPoint,
        module Graphics.Vulkan.Types.Enum.VkPipelineStageFlags,
        module Graphics.Vulkan.Types.Struct.VkPipelineTessellationDomainOriginStateCreateInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkPipelineTessellationStateCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkPointClippingBehaviorKHR,
        module Graphics.Vulkan.Types.Struct.VkRenderPassCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkRenderPassInputAttachmentAspectCreateInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Struct.VkSubpassDependency,
        module Graphics.Vulkan.Types.Struct.VkSubpassDescription,
        module Graphics.Vulkan.Types.Enum.VkSubpassDescriptionFlags,
        module Graphics.Vulkan.Types.Enum.VkTessellationDomainOriginKHR,
        -- > #include "vk_platform.h"
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
        pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR)
       where
import           GHC.Ptr
                                                                                                    (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkAccessFlags
import           Graphics.Vulkan.Types.Enum.VkAttachmentDescriptionFlags
import           Graphics.Vulkan.Types.Enum.VkAttachmentLoadOp
import           Graphics.Vulkan.Types.Enum.VkAttachmentStoreOp
import           Graphics.Vulkan.Types.Enum.VkComponentSwizzle
import           Graphics.Vulkan.Types.Enum.VkDependencyFlags
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags
                                                                                                    (VkImageCreateBitmask (..),
                                                                                                    VkImageCreateFlagBits)
import           Graphics.Vulkan.Types.Enum.VkImageLayout
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkImageViewType
import           Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.VkPipelineBindPoint
import           Graphics.Vulkan.Types.Enum.VkPipelineStageFlags
import           Graphics.Vulkan.Types.Enum.VkPointClippingBehaviorKHR
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSubpassDescriptionFlags
import           Graphics.Vulkan.Types.Enum.VkTessellationDomainOriginKHR
import           Graphics.Vulkan.Types.Struct.VkAttachmentDescription
import           Graphics.Vulkan.Types.Struct.VkAttachmentReference
import           Graphics.Vulkan.Types.Struct.VkComponentMapping
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceRange
import           Graphics.Vulkan.Types.Struct.VkImageViewCreateInfo
import           Graphics.Vulkan.Types.Struct.VkImageViewUsageCreateInfoKHR
import           Graphics.Vulkan.Types.Struct.VkInputAttachmentAspectReferenceKHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits
import           Graphics.Vulkan.Types.Struct.VkPhysicalDevicePointClippingPropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties
import           Graphics.Vulkan.Types.Struct.VkPipelineTessellationDomainOriginStateCreateInfoKHR
import           Graphics.Vulkan.Types.Struct.VkPipelineTessellationStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkRenderPassCreateInfo
import           Graphics.Vulkan.Types.Struct.VkRenderPassInputAttachmentAspectCreateInfoKHR
import           Graphics.Vulkan.Types.Struct.VkSubpassDependency
import           Graphics.Vulkan.Types.Struct.VkSubpassDescription

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

-- | bitpos = @7@
pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR ::
        VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR =
        VkImageCreateFlagBits 128

-- | bitpos = @8@
pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR ::
        VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR =
        VkImageCreateFlagBits 256

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR
        = VkStructureType 1000117000

pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR
        = VkStructureType 1000117001

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR =
        VkStructureType 1000117002

pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR
        = VkStructureType 1000117003

pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR
        :: VkImageLayout

pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR
        = VkImageLayout 1000117000

pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR
        :: VkImageLayout

pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR
        = VkImageLayout 1000117001
