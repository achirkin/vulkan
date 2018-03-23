{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
module Graphics.Vulkan.Core_1_1
       (-- * Vulkan 1.1 core API interface definitions.
        -- |
        --
        -- @api = vulkan@
        --
        -- @name = VK_VERSION_1_1@
        --
        -- @number = 1.1@
        --

        -- ** Device Initialization
        vkEnumerateInstanceVersion, vkEnumerateInstanceVersionSafe,
        module Graphics.Vulkan.Types.Enum.VkResult,
        -- ** Promoted from VK_KHR_relaxed_block_layout, which has no API
        --

        -- ** Promoted from VK_KHR_storage_buffer_storage_class, which has no API
        --

        -- ** Originally based on VK_KHR_subgroup (extension 94), but the actual enum block used was, incorrectly, that of extension 95
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSubgroupProperties,
        module Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkShaderStageFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Enum.VkSubgroupFeatureFlags,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES,
        -- ** Promoted from VK_KHR_bind_memory2
        module Graphics.Vulkan.Types.Struct.VkBindBufferMemoryInfo,
        module Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfo,
        -- > #include "vk_platform.h"
        vkBindBufferMemory2, vkBindBufferMemory2Safe, vkBindImageMemory2,
        vkBindImageMemory2Safe, module Graphics.Vulkan.Types.Handles,
        pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO,
        pattern VK_IMAGE_CREATE_ALIAS_BIT,
        -- ** Promoted from VK_KHR_16bit_storage
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkDeviceQueueCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDevice16BitStorageFeatures,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES,
        -- ** Promoted from VK_KHR_dedicated_allocation
        module Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo,
        module Graphics.Vulkan.Types.Struct.VkMemoryDedicatedAllocateInfo,
        module Graphics.Vulkan.Types.Struct.VkMemoryDedicatedRequirements,
        module Graphics.Vulkan.Types.Struct.VkMemoryRequirements,
        module Graphics.Vulkan.Types.Struct.VkMemoryRequirements2,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS,
        pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO,
        -- ** Promoted from VK_KHR_device_group
        module Graphics.Vulkan.Types.Struct.VkBindSparseInfo,
        module Graphics.Vulkan.Types.Struct.VkClearColorValue,
        module Graphics.Vulkan.Types.Struct.VkClearDepthStencilValue,
        module Graphics.Vulkan.Types.Struct.VkClearValue,
        module Graphics.Vulkan.Types.Struct.VkCommandBufferBeginInfo,
        module Graphics.Vulkan.Types.Struct.VkCommandBufferInheritanceInfo,
        module Graphics.Vulkan.Types.Enum.VkCommandBufferUsageFlags,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupBindSparseInfo,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupCommandBufferBeginInfo,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupRenderPassBeginInfo,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupSubmitInfo,
        module Graphics.Vulkan.Types.Struct.VkExtent2D,
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.Enum.VkImageAspectFlags,
        module Graphics.Vulkan.Types.Struct.VkImageSubresource,
        module Graphics.Vulkan.Types.Enum.VkMemoryAllocateFlags,
        module Graphics.Vulkan.Types.Struct.VkMemoryAllocateFlagsInfo,
        module Graphics.Vulkan.Types.Struct.VkOffset2D,
        module Graphics.Vulkan.Types.Struct.VkOffset3D,
        module Graphics.Vulkan.Types.Enum.VkPeerMemoryFeatureFlags,
        module Graphics.Vulkan.Types.Enum.VkPipelineStageFlags,
        module Graphics.Vulkan.Types.Enum.VkQueryControlFlags,
        module Graphics.Vulkan.Types.Enum.VkQueryPipelineStatisticFlags,
        module Graphics.Vulkan.Types.Struct.VkRect2D,
        module Graphics.Vulkan.Types.Struct.VkRenderPassBeginInfo,
        module Graphics.Vulkan.Types.Struct.VkSparseBufferMemoryBindInfo,
        module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBind,
        module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBindInfo,
        module Graphics.Vulkan.Types.Struct.VkSparseImageOpaqueMemoryBindInfo,
        module Graphics.Vulkan.Types.Struct.VkSparseMemoryBind,
        module Graphics.Vulkan.Types.Enum.VkSparseMemoryBindFlags,
        module Graphics.Vulkan.Types.Struct.VkSubmitInfo,
        -- > #include "vk_platform.h"
        vkGetDeviceGroupPeerMemoryFeatures,
        vkGetDeviceGroupPeerMemoryFeaturesSafe, vkCmdSetDeviceMask,
        vkCmdSetDeviceMaskSafe, vkCmdDispatchBase, vkCmdDispatchBaseSafe,
        pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO,
        pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT,
        pattern VK_PIPELINE_CREATE_DISPATCH_BASE,
        pattern VK_DEPENDENCY_DEVICE_GROUP_BIT,
        -- ** Promoted from VK_KHR_device_group + VK_KHR_bind_memory2
        module Graphics.Vulkan.Types.Struct.VkBindBufferMemoryDeviceGroupInfo,
        module Graphics.Vulkan.Types.Struct.VkBindImageMemoryDeviceGroupInfo,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO,
        pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT,
        -- ** Promoted from VK_KHR_device_group_creation
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupDeviceCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceGroupProperties,
        -- > #include "vk_platform.h"
        vkEnumeratePhysicalDeviceGroups,
        vkEnumeratePhysicalDeviceGroupsSafe,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO,
        pattern VK_MAX_DEVICE_GROUP_SIZE,
        pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT,
        -- ** Promoted from VK_KHR_get_memory_requirements2
        module Graphics.Vulkan.Types.Struct.VkBufferMemoryRequirementsInfo2,
        module Graphics.Vulkan.Types.Struct.VkImageMemoryRequirementsInfo2,
        module Graphics.Vulkan.Types.Struct.VkImageSparseMemoryRequirementsInfo2,
        module Graphics.Vulkan.Types.Enum.VkSparseImageFormatFlags,
        module Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements,
        module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements2,
        -- > #include "vk_platform.h"
        vkGetImageMemoryRequirements2, vkGetImageMemoryRequirements2Safe,
        vkGetBufferMemoryRequirements2, vkGetBufferMemoryRequirements2Safe,
        vkGetImageSparseMemoryRequirements2,
        vkGetImageSparseMemoryRequirements2Safe,
        pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2,
        pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2,
        pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2,
        pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2,
        pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2,
        -- ** Promoted from VK_KHR_get_physical_device_properties2
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkFormatFeatureFlags,
        module Graphics.Vulkan.Types.Struct.VkFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkFormatProperties2,
        module Graphics.Vulkan.Types.Enum.VkImageCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties2,
        module Graphics.Vulkan.Types.Enum.VkImageTiling,
        module Graphics.Vulkan.Types.Enum.VkImageType,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Struct.VkMemoryHeap,
        module Graphics.Vulkan.Types.Enum.VkMemoryHeapFlags,
        module Graphics.Vulkan.Types.Enum.VkMemoryPropertyFlags,
        module Graphics.Vulkan.Types.Struct.VkMemoryType,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceImageFormatInfo2,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties2,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseImageFormatInfo2,
        module Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties,
        module Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties2,
        module Graphics.Vulkan.Types.Enum.VkQueueFlags,
        module Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties2,
        -- > #include "vk_platform.h"
        vkGetPhysicalDeviceFeatures2, vkGetPhysicalDeviceFeatures2Safe,
        vkGetPhysicalDeviceProperties2, vkGetPhysicalDeviceProperties2Safe,
        vkGetPhysicalDeviceFormatProperties2,
        vkGetPhysicalDeviceFormatProperties2Safe,
        vkGetPhysicalDeviceImageFormatProperties2,
        vkGetPhysicalDeviceImageFormatProperties2Safe,
        vkGetPhysicalDeviceQueueFamilyProperties2,
        vkGetPhysicalDeviceQueueFamilyProperties2Safe,
        vkGetPhysicalDeviceMemoryProperties2,
        vkGetPhysicalDeviceMemoryProperties2Safe,
        vkGetPhysicalDeviceSparseImageFormatProperties2,
        vkGetPhysicalDeviceSparseImageFormatProperties2Safe,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2,
        pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2,
        pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2,
        pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2,
        pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2,
        -- ** Promoted from VK_KHR_maintenance1
        --
        -- |
        -- > #include "vk_platform.h"
        vkTrimCommandPool, vkTrimCommandPoolSafe,
        pattern VK_ERROR_OUT_OF_POOL_MEMORY,
        pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT,
        pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT,
        pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT,
        -- ** Promoted from VK_KHR_maintenance2
        module Graphics.Vulkan.Types.Enum.VkAccessFlags,
        module Graphics.Vulkan.Types.Struct.VkAttachmentDescription,
        module Graphics.Vulkan.Types.Enum.VkAttachmentDescriptionFlags,
        module Graphics.Vulkan.Types.Enum.VkAttachmentLoadOp,
        module Graphics.Vulkan.Types.Struct.VkAttachmentReference,
        module Graphics.Vulkan.Types.Enum.VkAttachmentStoreOp,
        module Graphics.Vulkan.Types.Struct.VkComponentMapping,
        module Graphics.Vulkan.Types.Enum.VkComponentSwizzle,
        module Graphics.Vulkan.Types.Enum.VkDependencyFlags,
        module Graphics.Vulkan.Types.Enum.VkImageLayout,
        module Graphics.Vulkan.Types.Struct.VkImageSubresourceRange,
        module Graphics.Vulkan.Types.Struct.VkImageViewCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkImageViewType,
        module Graphics.Vulkan.Types.Struct.VkImageViewUsageCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkInputAttachmentAspectReference,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDevicePointClippingProperties,
        module Graphics.Vulkan.Types.Enum.VkPipelineBindPoint,
        module Graphics.Vulkan.Types.Struct.VkPipelineTessellationDomainOriginStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineTessellationStateCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkPointClippingBehavior,
        module Graphics.Vulkan.Types.Struct.VkRenderPassCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkRenderPassInputAttachmentAspectCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkSubpassDependency,
        module Graphics.Vulkan.Types.Struct.VkSubpassDescription,
        module Graphics.Vulkan.Types.Enum.VkSubpassDescriptionFlags,
        module Graphics.Vulkan.Types.Enum.VkTessellationDomainOrigin,
        -- > #include "vk_platform.h"
        pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT,
        pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO,
        pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL,
        pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL,
        -- ** Promoted from VK_KHR_multiview
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMultiviewFeatures,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMultiviewProperties,
        module Graphics.Vulkan.Types.Struct.VkRenderPassMultiviewCreateInfo,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES,
        pattern VK_DEPENDENCY_VIEW_LOCAL_BIT,
        -- ** Promoted from VK_KHR_variable_pointers
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceVariablePointerFeatures,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES,
        -- ** Originally based on VK_KHR_protected_memory (extension 146), which was never published; thus the mystifying large value= numbers below. These are not aliased since they weren't actually promoted from an extension.
        module Graphics.Vulkan.Types.Struct.VkDeviceQueueInfo2,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProtectedMemoryFeatures,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProtectedMemoryProperties,
        module Graphics.Vulkan.Types.Struct.VkProtectedSubmitInfo,
        -- > #include "vk_platform.h"
        vkGetDeviceQueue2, vkGetDeviceQueue2Safe,
        pattern VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2,
        pattern VK_QUEUE_PROTECTED_BIT,
        pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT,
        pattern VK_MEMORY_PROPERTY_PROTECTED_BIT,
        pattern VK_BUFFER_CREATE_PROTECTED_BIT,
        pattern VK_IMAGE_CREATE_PROTECTED_BIT,
        pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT,
        -- ** Promoted from VK_KHR_sampler_ycbcr_conversion
        module Graphics.Vulkan.Types.Struct.VkBindImagePlaneMemoryInfo,
        module Graphics.Vulkan.Types.Enum.VkBorderColor,
        module Graphics.Vulkan.Types.Enum.VkChromaLocation,
        module Graphics.Vulkan.Types.Enum.VkCompareOp,
        module Graphics.Vulkan.Types.Enum.VkFilter,
        module Graphics.Vulkan.Types.Struct.VkImagePlaneMemoryRequirementsInfo,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSamplerYcbcrConversionFeatures,
        module Graphics.Vulkan.Types.Enum.VkSamplerAddressMode,
        module Graphics.Vulkan.Types.Struct.VkSamplerCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkSamplerMipmapMode,
        module Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionImageFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionInfo,
        module Graphics.Vulkan.Types.Enum.VkSamplerYcbcrModelConversion,
        module Graphics.Vulkan.Types.Enum.VkSamplerYcbcrRange,
        -- > #include "vk_platform.h"
        vkCreateSamplerYcbcrConversion, vkCreateSamplerYcbcrConversionSafe,
        vkDestroySamplerYcbcrConversion,
        vkDestroySamplerYcbcrConversionSafe,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO,
        pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES,
        pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES,
        pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION,
        pattern VK_FORMAT_G8B8G8R8_422_UNORM,
        pattern VK_FORMAT_B8G8R8G8_422_UNORM,
        pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM,
        pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM,
        pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM,
        pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM,
        pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM,
        pattern VK_FORMAT_R10X6_UNORM_PACK16,
        pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16,
        pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16,
        pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16,
        pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16,
        pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16,
        pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16,
        pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16,
        pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16,
        pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16,
        pattern VK_FORMAT_R12X4_UNORM_PACK16,
        pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16,
        pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16,
        pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16,
        pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16,
        pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16,
        pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16,
        pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16,
        pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16,
        pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16,
        pattern VK_FORMAT_G16B16G16R16_422_UNORM,
        pattern VK_FORMAT_B16G16R16G16_422_UNORM,
        pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM,
        pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM,
        pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM,
        pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM,
        pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM,
        pattern VK_IMAGE_ASPECT_PLANE_0_BIT,
        pattern VK_IMAGE_ASPECT_PLANE_1_BIT,
        pattern VK_IMAGE_ASPECT_PLANE_2_BIT,
        pattern VK_IMAGE_CREATE_DISJOINT_BIT,
        pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT,
        pattern VK_FORMAT_FEATURE_DISJOINT_BIT,
        pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT,
        -- ** Promoted from VK_KHR_descriptor_update_template
        module Graphics.Vulkan.Types.Enum.VkDescriptorType,
        module Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateEntry,
        module Graphics.Vulkan.Types.Enum.VkDescriptorUpdateTemplateType,
        -- > #include "vk_platform.h"
        vkCreateDescriptorUpdateTemplate,
        vkCreateDescriptorUpdateTemplateSafe,
        vkDestroyDescriptorUpdateTemplate,
        vkDestroyDescriptorUpdateTemplateSafe,
        vkUpdateDescriptorSetWithTemplate,
        vkUpdateDescriptorSetWithTemplateSafe,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO,
        pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE,
        -- ** Promoted from VK_KHR_external_memory_capabilities
        module Graphics.Vulkan.Types.Enum.VkBufferCreateFlags,
        module Graphics.Vulkan.Types.Enum.VkBufferUsageFlags,
        module Graphics.Vulkan.Types.Struct.VkExternalBufferProperties,
        module Graphics.Vulkan.Types.Struct.VkExternalImageFormatProperties,
        module Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlags,
        module Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlags,
        module Graphics.Vulkan.Types.Struct.VkExternalMemoryProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalBufferInfo,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalImageFormatInfo,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceIDProperties,
        -- > #include "vk_platform.h"
        vkGetPhysicalDeviceExternalBufferProperties,
        vkGetPhysicalDeviceExternalBufferPropertiesSafe,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES,
        pattern VK_LUID_SIZE,
        -- ** Promoted from VK_KHR_external_memory
        module Graphics.Vulkan.Types.Struct.VkBufferCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkExportMemoryAllocateInfo,
        module Graphics.Vulkan.Types.Struct.VkExternalMemoryBufferCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkExternalMemoryImageCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkImageCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkSharingMode,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO,
        pattern VK_ERROR_INVALID_EXTERNAL_HANDLE,
        pattern VK_QUEUE_FAMILY_EXTERNAL,
        -- ** Promoted from VK_KHR_external_fence_capabilities
        module Graphics.Vulkan.Types.Enum.VkExternalFenceFeatureFlags,
        module Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlags,
        module Graphics.Vulkan.Types.Struct.VkExternalFenceProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalFenceInfo,
        -- > #include "vk_platform.h"
        vkGetPhysicalDeviceExternalFenceProperties,
        vkGetPhysicalDeviceExternalFencePropertiesSafe,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES,
        -- ** Promoted from VK_KHR_external_fence
        module Graphics.Vulkan.Types.Struct.VkExportFenceCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkFenceCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkFenceCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkFenceImportFlags,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO,
        -- ** Promoted from VK_KHR_external_semaphore
        module Graphics.Vulkan.Types.Struct.VkExportSemaphoreCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkExternalSemaphoreHandleTypeFlags,
        module Graphics.Vulkan.Types.Struct.VkSemaphoreCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkSemaphoreImportFlags,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO,
        -- ** Promoted from VK_KHR_external_semaphore_capabilities
        module Graphics.Vulkan.Types.Enum.VkExternalSemaphoreFeatureFlags,
        module Graphics.Vulkan.Types.Struct.VkExternalSemaphoreProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalSemaphoreInfo,
        -- > #include "vk_platform.h"
        vkGetPhysicalDeviceExternalSemaphoreProperties,
        vkGetPhysicalDeviceExternalSemaphorePropertiesSafe,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES,
        -- ** Promoted from VK_KHR_maintenance3
        module Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutSupport,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMaintenance3Properties,
        -- > #include "vk_platform.h"
        vkGetDescriptorSetLayoutSupport,
        vkGetDescriptorSetLayoutSupportSafe,
        module Graphics.Vulkan.Types.Enum.VkDescriptorSetLayoutCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutBinding,
        module Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutCreateInfo,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT,
        -- ** Promoted from VK_KHR_shader_draw_parameters, with a feature support query added
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceShaderDrawParameterFeatures,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES)
       where
import           Graphics.Vulkan.Constants
                                                                                                 (pattern VK_LUID_SIZE,
                                                                                                 pattern VK_MAX_DEVICE_GROUP_SIZE,
                                                                                                 pattern VK_QUEUE_FAMILY_EXTERNAL)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkAccessFlags
import           Graphics.Vulkan.Types.Enum.VkAttachmentDescriptionFlags
import           Graphics.Vulkan.Types.Enum.VkAttachmentLoadOp
import           Graphics.Vulkan.Types.Enum.VkAttachmentStoreOp
import           Graphics.Vulkan.Types.Enum.VkBorderColor
import           Graphics.Vulkan.Types.Enum.VkBufferCreateFlags
import           Graphics.Vulkan.Types.Enum.VkBufferUsageFlags
import           Graphics.Vulkan.Types.Enum.VkChromaLocation
import           Graphics.Vulkan.Types.Enum.VkCommandBufferUsageFlags
import           Graphics.Vulkan.Types.Enum.VkCommandPoolCreateFlags
                                                                                                 (VkCommandPoolCreateBitmask (..),
                                                                                                 VkCommandPoolCreateFlagBits)
import           Graphics.Vulkan.Types.Enum.VkCompareOp
import           Graphics.Vulkan.Types.Enum.VkComponentSwizzle
import           Graphics.Vulkan.Types.Enum.VkDependencyFlags
import           Graphics.Vulkan.Types.Enum.VkDescriptorSetLayoutCreateFlags
import           Graphics.Vulkan.Types.Enum.VkDescriptorType
import           Graphics.Vulkan.Types.Enum.VkDescriptorUpdateTemplateType
import           Graphics.Vulkan.Types.Enum.VkDeviceQueueCreateFlags
import           Graphics.Vulkan.Types.Enum.VkExternalFenceFeatureFlags
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlags
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlags
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlags
import           Graphics.Vulkan.Types.Enum.VkExternalSemaphoreFeatureFlags
import           Graphics.Vulkan.Types.Enum.VkExternalSemaphoreHandleTypeFlags
import           Graphics.Vulkan.Types.Enum.VkFenceCreateFlags
import           Graphics.Vulkan.Types.Enum.VkFenceImportFlags
import           Graphics.Vulkan.Types.Enum.VkFilter
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkFormatFeatureFlags
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags
import           Graphics.Vulkan.Types.Enum.VkImageLayout
import           Graphics.Vulkan.Types.Enum.VkImageTiling
import           Graphics.Vulkan.Types.Enum.VkImageType
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkImageViewType
import           Graphics.Vulkan.Types.Enum.VkInternalAllocationType
import           Graphics.Vulkan.Types.Enum.VkMemoryAllocateFlags
import           Graphics.Vulkan.Types.Enum.VkMemoryHeapFlags
import           Graphics.Vulkan.Types.Enum.VkMemoryPropertyFlags
import           Graphics.Vulkan.Types.Enum.VkObjectType
                                                                                                 (VkObjectType (..))
import           Graphics.Vulkan.Types.Enum.VkPeerMemoryFeatureFlags
import           Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.VkPipelineBindPoint
import           Graphics.Vulkan.Types.Enum.VkPipelineCreateFlags
                                                                                                 (VkPipelineCreateBitmask (..),
                                                                                                 VkPipelineCreateFlagBits)
import           Graphics.Vulkan.Types.Enum.VkPipelineStageFlags
import           Graphics.Vulkan.Types.Enum.VkPointClippingBehavior
import           Graphics.Vulkan.Types.Enum.VkQueryControlFlags
import           Graphics.Vulkan.Types.Enum.VkQueryPipelineStatisticFlags
import           Graphics.Vulkan.Types.Enum.VkQueueFlags
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkSamplerAddressMode
import           Graphics.Vulkan.Types.Enum.VkSamplerMipmapMode
import           Graphics.Vulkan.Types.Enum.VkSamplerYcbcrModelConversion
import           Graphics.Vulkan.Types.Enum.VkSamplerYcbcrRange
import           Graphics.Vulkan.Types.Enum.VkSemaphoreImportFlags
import           Graphics.Vulkan.Types.Enum.VkShaderStageFlags
import           Graphics.Vulkan.Types.Enum.VkSharingMode
import           Graphics.Vulkan.Types.Enum.VkSparseImageFormatFlags
import           Graphics.Vulkan.Types.Enum.VkSparseMemoryBindFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSubgroupFeatureFlags
import           Graphics.Vulkan.Types.Enum.VkSubpassDescriptionFlags
import           Graphics.Vulkan.Types.Enum.VkSystemAllocationScope
import           Graphics.Vulkan.Types.Enum.VkTessellationDomainOrigin
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
import           Graphics.Vulkan.Types.Struct.VkAttachmentDescription
import           Graphics.Vulkan.Types.Struct.VkAttachmentReference
import           Graphics.Vulkan.Types.Struct.VkBindBufferMemoryDeviceGroupInfo
import           Graphics.Vulkan.Types.Struct.VkBindBufferMemoryInfo
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryDeviceGroupInfo
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfo
import           Graphics.Vulkan.Types.Struct.VkBindImagePlaneMemoryInfo
import           Graphics.Vulkan.Types.Struct.VkBindSparseInfo
import           Graphics.Vulkan.Types.Struct.VkBufferCreateInfo
import           Graphics.Vulkan.Types.Struct.VkBufferMemoryRequirementsInfo2
import           Graphics.Vulkan.Types.Struct.VkClearColorValue
import           Graphics.Vulkan.Types.Struct.VkClearDepthStencilValue
import           Graphics.Vulkan.Types.Struct.VkClearValue
import           Graphics.Vulkan.Types.Struct.VkCommandBufferBeginInfo
import           Graphics.Vulkan.Types.Struct.VkCommandBufferInheritanceInfo
import           Graphics.Vulkan.Types.Struct.VkComponentMapping
import           Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutBinding
import           Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutCreateInfo
import           Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutSupport
import           Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateEntry
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupBindSparseInfo
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupCommandBufferBeginInfo
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupDeviceCreateInfo
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupRenderPassBeginInfo
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupSubmitInfo
import           Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo
import           Graphics.Vulkan.Types.Struct.VkDeviceQueueInfo2
import           Graphics.Vulkan.Types.Struct.VkExportFenceCreateInfo
import           Graphics.Vulkan.Types.Struct.VkExportMemoryAllocateInfo
import           Graphics.Vulkan.Types.Struct.VkExportSemaphoreCreateInfo
import           Graphics.Vulkan.Types.Struct.VkExtent2D
import           Graphics.Vulkan.Types.Struct.VkExtent3D
import           Graphics.Vulkan.Types.Struct.VkExternalBufferProperties
import           Graphics.Vulkan.Types.Struct.VkExternalFenceProperties
import           Graphics.Vulkan.Types.Struct.VkExternalImageFormatProperties
import           Graphics.Vulkan.Types.Struct.VkExternalMemoryBufferCreateInfo
import           Graphics.Vulkan.Types.Struct.VkExternalMemoryImageCreateInfo
import           Graphics.Vulkan.Types.Struct.VkExternalMemoryProperties
import           Graphics.Vulkan.Types.Struct.VkExternalSemaphoreProperties
import           Graphics.Vulkan.Types.Struct.VkFenceCreateInfo
import           Graphics.Vulkan.Types.Struct.VkFormatProperties
import           Graphics.Vulkan.Types.Struct.VkFormatProperties2
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties2
import           Graphics.Vulkan.Types.Struct.VkImageMemoryRequirementsInfo2
import           Graphics.Vulkan.Types.Struct.VkImagePlaneMemoryRequirementsInfo
import           Graphics.Vulkan.Types.Struct.VkImageSparseMemoryRequirementsInfo2
import           Graphics.Vulkan.Types.Struct.VkImageSubresource
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceRange
import           Graphics.Vulkan.Types.Struct.VkImageViewCreateInfo
import           Graphics.Vulkan.Types.Struct.VkImageViewUsageCreateInfo
import           Graphics.Vulkan.Types.Struct.VkInputAttachmentAspectReference
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateFlagsInfo
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo
import           Graphics.Vulkan.Types.Struct.VkMemoryDedicatedAllocateInfo
import           Graphics.Vulkan.Types.Struct.VkMemoryDedicatedRequirements
import           Graphics.Vulkan.Types.Struct.VkMemoryHeap
import           Graphics.Vulkan.Types.Struct.VkMemoryRequirements
import           Graphics.Vulkan.Types.Struct.VkMemoryRequirements2
import           Graphics.Vulkan.Types.Struct.VkMemoryType
import           Graphics.Vulkan.Types.Struct.VkOffset2D
import           Graphics.Vulkan.Types.Struct.VkOffset3D
import           Graphics.Vulkan.Types.Struct.VkPhysicalDevice16BitStorageFeatures
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalBufferInfo
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalFenceInfo
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalImageFormatInfo
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalSemaphoreInfo
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceGroupProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceIDProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceImageFormatInfo2
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMaintenance3Properties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties2
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMultiviewFeatures
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMultiviewProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDevicePointClippingProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProtectedMemoryFeatures
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProtectedMemoryProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSamplerYcbcrConversionFeatures
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceShaderDrawParameterFeatures
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseImageFormatInfo2
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSubgroupProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceVariablePointerFeatures
import           Graphics.Vulkan.Types.Struct.VkPipelineTessellationDomainOriginStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineTessellationStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkProtectedSubmitInfo
import           Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties
import           Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties2
import           Graphics.Vulkan.Types.Struct.VkRect2D
import           Graphics.Vulkan.Types.Struct.VkRenderPassBeginInfo
import           Graphics.Vulkan.Types.Struct.VkRenderPassCreateInfo
import           Graphics.Vulkan.Types.Struct.VkRenderPassInputAttachmentAspectCreateInfo
import           Graphics.Vulkan.Types.Struct.VkRenderPassMultiviewCreateInfo
import           Graphics.Vulkan.Types.Struct.VkSamplerCreateInfo
import           Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionCreateInfo
import           Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionImageFormatProperties
import           Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionInfo
import           Graphics.Vulkan.Types.Struct.VkSemaphoreCreateInfo
import           Graphics.Vulkan.Types.Struct.VkSparseBufferMemoryBindInfo
import           Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties
import           Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties2
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBind
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBindInfo
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements2
import           Graphics.Vulkan.Types.Struct.VkSparseImageOpaqueMemoryBindInfo
import           Graphics.Vulkan.Types.Struct.VkSparseMemoryBind
import           Graphics.Vulkan.Types.Struct.VkSubmitInfo
import           Graphics.Vulkan.Types.Struct.VkSubpassDependency
import           Graphics.Vulkan.Types.Struct.VkSubpassDescription

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkEnumerateInstanceVersion
--   >     ( uint32_t* pApiVersion
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumerateInstanceVersion.html vkEnumerateInstanceVersion registry at www.khronos.org>
foreign import ccall unsafe "vkEnumerateInstanceVersion"
               vkEnumerateInstanceVersion :: Ptr Word32 -- ^ pApiVersion
                                                        -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkEnumerateInstanceVersion
--   >     ( uint32_t* pApiVersion
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumerateInstanceVersion.html vkEnumerateInstanceVersion registry at www.khronos.org>
foreign import ccall safe "vkEnumerateInstanceVersion"
               vkEnumerateInstanceVersionSafe :: Ptr Word32 -- ^ pApiVersion
                                                            -> IO VkResult

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES =
        VkStructureType 1000094000

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindBufferMemory2
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindBufferMemoryInfo* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkBindBufferMemory2.html vkBindBufferMemory2 registry at www.khronos.org>
foreign import ccall unsafe "vkBindBufferMemory2"
               vkBindBufferMemory2 ::
               VkDevice -- ^ device
                        -> Word32 -- ^ bindInfoCount
                                  -> Ptr VkBindBufferMemoryInfo -- ^ pBindInfos
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindBufferMemory2
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindBufferMemoryInfo* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkBindBufferMemory2.html vkBindBufferMemory2 registry at www.khronos.org>
foreign import ccall safe "vkBindBufferMemory2"
               vkBindBufferMemory2Safe ::
               VkDevice -- ^ device
                        -> Word32 -- ^ bindInfoCount
                                  -> Ptr VkBindBufferMemoryInfo -- ^ pBindInfos
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindImageMemory2
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindImageMemoryInfo* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkBindImageMemory2.html vkBindImageMemory2 registry at www.khronos.org>
foreign import ccall unsafe "vkBindImageMemory2" vkBindImageMemory2
               :: VkDevice -- ^ device
                           -> Word32 -- ^ bindInfoCount
                                     -> Ptr VkBindImageMemoryInfo -- ^ pBindInfos
                                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindImageMemory2
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindImageMemoryInfo* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkBindImageMemory2.html vkBindImageMemory2 registry at www.khronos.org>
foreign import ccall safe "vkBindImageMemory2"
               vkBindImageMemory2Safe ::
               VkDevice -- ^ device
                        -> Word32 -- ^ bindInfoCount
                                  -> Ptr VkBindImageMemoryInfo -- ^ pBindInfos
                                                               -> IO VkResult

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO =
        VkStructureType 1000157000

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO =
        VkStructureType 1000157001

-- | bitpos = @10@
pattern VK_IMAGE_CREATE_ALIAS_BIT :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_ALIAS_BIT = VkImageCreateFlagBits 1024

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES =
        VkStructureType 1000083000

pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS =
        VkStructureType 1000127000

pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO =
        VkStructureType 1000127001

-- | > () vkGetDeviceGroupPeerMemoryFeatures
--   >     ( VkDevice device
--   >     , uint32_t heapIndex
--   >     , uint32_t localDeviceIndex
--   >     , uint32_t remoteDeviceIndex
--   >     , VkPeerMemoryFeatureFlags* pPeerMemoryFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceGroupPeerMemoryFeatures.html vkGetDeviceGroupPeerMemoryFeatures registry at www.khronos.org>
foreign import ccall unsafe "vkGetDeviceGroupPeerMemoryFeatures"
               vkGetDeviceGroupPeerMemoryFeatures ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ heapIndex
                        -> Word32 -- ^ localDeviceIndex
                                  -> Word32 -- ^ remoteDeviceIndex
                                            -> Ptr VkPeerMemoryFeatureFlags -- ^ pPeerMemoryFeatures
                                                                            -> IO ()

-- | > () vkGetDeviceGroupPeerMemoryFeatures
--   >     ( VkDevice device
--   >     , uint32_t heapIndex
--   >     , uint32_t localDeviceIndex
--   >     , uint32_t remoteDeviceIndex
--   >     , VkPeerMemoryFeatureFlags* pPeerMemoryFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceGroupPeerMemoryFeatures.html vkGetDeviceGroupPeerMemoryFeatures registry at www.khronos.org>
foreign import ccall safe "vkGetDeviceGroupPeerMemoryFeatures"
               vkGetDeviceGroupPeerMemoryFeaturesSafe ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ heapIndex
                        -> Word32 -- ^ localDeviceIndex
                                  -> Word32 -- ^ remoteDeviceIndex
                                            -> Ptr VkPeerMemoryFeatureFlags -- ^ pPeerMemoryFeatures
                                                                            -> IO ()

-- | queues: 'graphics', 'compute', 'transfer'.
--
--   renderpass: @both@
--
--   > () vkCmdSetDeviceMask
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t deviceMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetDeviceMask.html vkCmdSetDeviceMask registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetDeviceMask" vkCmdSetDeviceMask
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> Word32 -- ^ deviceMask
                                            -> IO ()

-- | queues: 'graphics', 'compute', 'transfer'.
--
--   renderpass: @both@
--
--   > () vkCmdSetDeviceMask
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t deviceMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetDeviceMask.html vkCmdSetDeviceMask registry at www.khronos.org>
foreign import ccall safe "vkCmdSetDeviceMask"
               vkCmdSetDeviceMaskSafe :: VkCommandBuffer -- ^ commandBuffer
                                                         -> Word32 -- ^ deviceMask
                                                                   -> IO ()

-- | queues: 'compute'.
--
--   renderpass: @outside@
--
--   > () vkCmdDispatchBase
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t baseGroupX
--   >     , uint32_t baseGroupY
--   >     , uint32_t baseGroupZ
--   >     , uint32_t groupCountX
--   >     , uint32_t groupCountY
--   >     , uint32_t groupCountZ
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDispatchBase.html vkCmdDispatchBase registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDispatchBase" vkCmdDispatchBase
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ baseGroupX
                        -> Word32 -- ^ baseGroupY
                                  -> Word32 -- ^ baseGroupZ
                                            -> Word32 -- ^ groupCountX
                                                      -> Word32 -- ^ groupCountY
                                                                -> Word32 -- ^ groupCountZ
                                                                          -> IO ()

-- | queues: 'compute'.
--
--   renderpass: @outside@
--
--   > () vkCmdDispatchBase
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t baseGroupX
--   >     , uint32_t baseGroupY
--   >     , uint32_t baseGroupZ
--   >     , uint32_t groupCountX
--   >     , uint32_t groupCountY
--   >     , uint32_t groupCountZ
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDispatchBase.html vkCmdDispatchBase registry at www.khronos.org>
foreign import ccall safe "vkCmdDispatchBase" vkCmdDispatchBaseSafe
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ baseGroupX
                        -> Word32 -- ^ baseGroupY
                                  -> Word32 -- ^ baseGroupZ
                                            -> Word32 -- ^ groupCountX
                                                      -> Word32 -- ^ groupCountY
                                                                -> Word32 -- ^ groupCountZ
                                                                          -> IO ()

pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO =
        VkStructureType 1000060000

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO =
        VkStructureType 1000060003

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO =
        VkStructureType 1000060004

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO =
        VkStructureType 1000060005

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO =
        VkStructureType 1000060006

-- | bitpos = @3@
pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT ::
        VkPipelineCreateFlagBits

pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT =
        VkPipelineCreateFlagBits 8

-- | bitpos = @4@
pattern VK_PIPELINE_CREATE_DISPATCH_BASE ::
        VkPipelineCreateFlagBits

pattern VK_PIPELINE_CREATE_DISPATCH_BASE =
        VkPipelineCreateFlagBits 16

-- | Dependency is across devices
--
--   bitpos = @2@
pattern VK_DEPENDENCY_DEVICE_GROUP_BIT :: VkDependencyFlagBits

pattern VK_DEPENDENCY_DEVICE_GROUP_BIT = VkDependencyFlagBits 4

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO =
        VkStructureType 1000060013

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO =
        VkStructureType 1000060014

-- | Allows using VkBindImageMemoryDeviceGroupInfo::pSplitInstanceBindRegions when binding memory to the image
--
--   bitpos = @6@
pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT ::
        VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT =
        VkImageCreateFlagBits 64

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkEnumeratePhysicalDeviceGroups
--   >     ( VkInstance instance
--   >     , uint32_t* pPhysicalDeviceGroupCount
--   >     , VkPhysicalDeviceGroupProperties* pPhysicalDeviceGroupProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumeratePhysicalDeviceGroups.html vkEnumeratePhysicalDeviceGroups registry at www.khronos.org>
foreign import ccall unsafe "vkEnumeratePhysicalDeviceGroups"
               vkEnumeratePhysicalDeviceGroups ::
               VkInstance -- ^ instance
                          ->
                 Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                            -> Ptr VkPhysicalDeviceGroupProperties -- ^ pPhysicalDeviceGroupProperties
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkEnumeratePhysicalDeviceGroups
--   >     ( VkInstance instance
--   >     , uint32_t* pPhysicalDeviceGroupCount
--   >     , VkPhysicalDeviceGroupProperties* pPhysicalDeviceGroupProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumeratePhysicalDeviceGroups.html vkEnumeratePhysicalDeviceGroups registry at www.khronos.org>
foreign import ccall safe "vkEnumeratePhysicalDeviceGroups"
               vkEnumeratePhysicalDeviceGroupsSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                            -> Ptr VkPhysicalDeviceGroupProperties -- ^ pPhysicalDeviceGroupProperties
                                                                   -> IO VkResult

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES =
        VkStructureType 1000070000

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO =
        VkStructureType 1000070001

-- | If set, heap allocations allocate multiple instances by default
--
--   bitpos = @1@
pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT :: VkMemoryHeapFlagBits

pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT = VkMemoryHeapFlagBits 2

-- | > () vkGetImageMemoryRequirements2
--   >     ( VkDevice device
--   >     , const VkImageMemoryRequirementsInfo2* pInfo
--   >     , VkMemoryRequirements2* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageMemoryRequirements2.html vkGetImageMemoryRequirements2 registry at www.khronos.org>
foreign import ccall unsafe "vkGetImageMemoryRequirements2"
               vkGetImageMemoryRequirements2 ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageMemoryRequirementsInfo2 -- ^ pInfo
                                                    ->
                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                             -> IO ()

-- | > () vkGetImageMemoryRequirements2
--   >     ( VkDevice device
--   >     , const VkImageMemoryRequirementsInfo2* pInfo
--   >     , VkMemoryRequirements2* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageMemoryRequirements2.html vkGetImageMemoryRequirements2 registry at www.khronos.org>
foreign import ccall safe "vkGetImageMemoryRequirements2"
               vkGetImageMemoryRequirements2Safe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageMemoryRequirementsInfo2 -- ^ pInfo
                                                    ->
                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                             -> IO ()

-- | > () vkGetBufferMemoryRequirements2
--   >     ( VkDevice device
--   >     , const VkBufferMemoryRequirementsInfo2* pInfo
--   >     , VkMemoryRequirements2* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetBufferMemoryRequirements2.html vkGetBufferMemoryRequirements2 registry at www.khronos.org>
foreign import ccall unsafe "vkGetBufferMemoryRequirements2"
               vkGetBufferMemoryRequirements2 ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferMemoryRequirementsInfo2 -- ^ pInfo
                                                     ->
                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                             -> IO ()

-- | > () vkGetBufferMemoryRequirements2
--   >     ( VkDevice device
--   >     , const VkBufferMemoryRequirementsInfo2* pInfo
--   >     , VkMemoryRequirements2* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetBufferMemoryRequirements2.html vkGetBufferMemoryRequirements2 registry at www.khronos.org>
foreign import ccall safe "vkGetBufferMemoryRequirements2"
               vkGetBufferMemoryRequirements2Safe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferMemoryRequirementsInfo2 -- ^ pInfo
                                                     ->
                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                             -> IO ()

-- | > () vkGetImageSparseMemoryRequirements2
--   >     ( VkDevice device
--   >     , const VkImageSparseMemoryRequirementsInfo2* pInfo
--   >     , uint32_t* pSparseMemoryRequirementCount
--   >     , VkSparseImageMemoryRequirements2* pSparseMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageSparseMemoryRequirements2.html vkGetImageSparseMemoryRequirements2 registry at www.khronos.org>
foreign import ccall unsafe "vkGetImageSparseMemoryRequirements2"
               vkGetImageSparseMemoryRequirements2 ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageSparseMemoryRequirementsInfo2 -- ^ pInfo
                                                          ->
                   Ptr Word32 -- ^ pSparseMemoryRequirementCount
                              -> Ptr VkSparseImageMemoryRequirements2 -- ^ pSparseMemoryRequirements
                                                                      -> IO ()

-- | > () vkGetImageSparseMemoryRequirements2
--   >     ( VkDevice device
--   >     , const VkImageSparseMemoryRequirementsInfo2* pInfo
--   >     , uint32_t* pSparseMemoryRequirementCount
--   >     , VkSparseImageMemoryRequirements2* pSparseMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageSparseMemoryRequirements2.html vkGetImageSparseMemoryRequirements2 registry at www.khronos.org>
foreign import ccall safe "vkGetImageSparseMemoryRequirements2"
               vkGetImageSparseMemoryRequirements2Safe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageSparseMemoryRequirementsInfo2 -- ^ pInfo
                                                          ->
                   Ptr Word32 -- ^ pSparseMemoryRequirementCount
                              -> Ptr VkSparseImageMemoryRequirements2 -- ^ pSparseMemoryRequirements
                                                                      -> IO ()

pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2 ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2 =
        VkStructureType 1000146000

pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2 ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2 =
        VkStructureType 1000146001

pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2 =
        VkStructureType 1000146002

pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2 :: VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2 =
        VkStructureType 1000146003

pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2 ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2 =
        VkStructureType 1000146004

-- | > () vkGetPhysicalDeviceFeatures2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures2* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFeatures2.html vkGetPhysicalDeviceFeatures2 registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceFeatures2"
               vkGetPhysicalDeviceFeatures2 ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceFeatures2 -- ^ pFeatures
                                                                 -> IO ()

-- | > () vkGetPhysicalDeviceFeatures2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures2* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFeatures2.html vkGetPhysicalDeviceFeatures2 registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceFeatures2"
               vkGetPhysicalDeviceFeatures2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceFeatures2 -- ^ pFeatures
                                                                 -> IO ()

-- | > () vkGetPhysicalDeviceProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties2* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceProperties2.html vkGetPhysicalDeviceProperties2 registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceProperties2"
               vkGetPhysicalDeviceProperties2 ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceProperties2 -- ^ pProperties
                                                                   -> IO ()

-- | > () vkGetPhysicalDeviceProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties2* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceProperties2.html vkGetPhysicalDeviceProperties2 registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceProperties2"
               vkGetPhysicalDeviceProperties2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceProperties2 -- ^ pProperties
                                                                   -> IO ()

-- | > () vkGetPhysicalDeviceFormatProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties2* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFormatProperties2.html vkGetPhysicalDeviceFormatProperties2 registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceFormatProperties2"
               vkGetPhysicalDeviceFormatProperties2 ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkFormat -- ^ format
                                            -> Ptr VkFormatProperties2 -- ^ pFormatProperties
                                                                       -> IO ()

-- | > () vkGetPhysicalDeviceFormatProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties2* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFormatProperties2.html vkGetPhysicalDeviceFormatProperties2 registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceFormatProperties2"
               vkGetPhysicalDeviceFormatProperties2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkFormat -- ^ format
                                            -> Ptr VkFormatProperties2 -- ^ pFormatProperties
                                                                       -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceImageFormatProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceImageFormatInfo2* pImageFormatInfo
--   >     , VkImageFormatProperties2* pImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceImageFormatProperties2.html vkGetPhysicalDeviceImageFormatProperties2 registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceImageFormatProperties2"
               vkGetPhysicalDeviceImageFormatProperties2 ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceImageFormatInfo2 -- ^ pImageFormatInfo
                                                      ->
                   Ptr VkImageFormatProperties2 -- ^ pImageFormatProperties
                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceImageFormatProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceImageFormatInfo2* pImageFormatInfo
--   >     , VkImageFormatProperties2* pImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceImageFormatProperties2.html vkGetPhysicalDeviceImageFormatProperties2 registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceImageFormatProperties2"
               vkGetPhysicalDeviceImageFormatProperties2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceImageFormatInfo2 -- ^ pImageFormatInfo
                                                      ->
                   Ptr VkImageFormatProperties2 -- ^ pImageFormatProperties
                                                -> IO VkResult

-- | > () vkGetPhysicalDeviceQueueFamilyProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties2* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceQueueFamilyProperties2.html vkGetPhysicalDeviceQueueFamilyProperties2 registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceQueueFamilyProperties2"
               vkGetPhysicalDeviceQueueFamilyProperties2 ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pQueueFamilyPropertyCount
                            -> Ptr VkQueueFamilyProperties2 -- ^ pQueueFamilyProperties
                                                            -> IO ()

-- | > () vkGetPhysicalDeviceQueueFamilyProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties2* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceQueueFamilyProperties2.html vkGetPhysicalDeviceQueueFamilyProperties2 registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceQueueFamilyProperties2"
               vkGetPhysicalDeviceQueueFamilyProperties2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pQueueFamilyPropertyCount
                            -> Ptr VkQueueFamilyProperties2 -- ^ pQueueFamilyProperties
                                                            -> IO ()

-- | > () vkGetPhysicalDeviceMemoryProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties2* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceMemoryProperties2.html vkGetPhysicalDeviceMemoryProperties2 registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceMemoryProperties2"
               vkGetPhysicalDeviceMemoryProperties2 ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceMemoryProperties2 -- ^ pMemoryProperties
                                                                         -> IO ()

-- | > () vkGetPhysicalDeviceMemoryProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties2* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceMemoryProperties2.html vkGetPhysicalDeviceMemoryProperties2 registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceMemoryProperties2"
               vkGetPhysicalDeviceMemoryProperties2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceMemoryProperties2 -- ^ pMemoryProperties
                                                                         -> IO ()

-- | > () vkGetPhysicalDeviceSparseImageFormatProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSparseImageFormatInfo2* pFormatInfo
--   >     , uint32_t* pPropertyCount
--   >     , VkSparseImageFormatProperties2* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceSparseImageFormatProperties2.html vkGetPhysicalDeviceSparseImageFormatProperties2 registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceSparseImageFormatProperties2"
               vkGetPhysicalDeviceSparseImageFormatProperties2 ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceSparseImageFormatInfo2 -- ^ pFormatInfo
                                                            ->
                   Ptr Word32 -- ^ pPropertyCount
                              -> Ptr VkSparseImageFormatProperties2 -- ^ pProperties
                                                                    -> IO ()

-- | > () vkGetPhysicalDeviceSparseImageFormatProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSparseImageFormatInfo2* pFormatInfo
--   >     , uint32_t* pPropertyCount
--   >     , VkSparseImageFormatProperties2* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceSparseImageFormatProperties2.html vkGetPhysicalDeviceSparseImageFormatProperties2 registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceSparseImageFormatProperties2"
               vkGetPhysicalDeviceSparseImageFormatProperties2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceSparseImageFormatInfo2 -- ^ pFormatInfo
                                                            ->
                   Ptr Word32 -- ^ pPropertyCount
                              -> Ptr VkSparseImageFormatProperties2 -- ^ pProperties
                                                                    -> IO ()

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 =
        VkStructureType 1000059000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 =
        VkStructureType 1000059001

pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2 :: VkStructureType

pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2 =
        VkStructureType 1000059002

pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 =
        VkStructureType 1000059003

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2 ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2 =
        VkStructureType 1000059004

pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2 ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2 =
        VkStructureType 1000059005

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 =
        VkStructureType 1000059006

pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2 ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2 =
        VkStructureType 1000059007

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
        = VkStructureType 1000059008

-- | > () vkTrimCommandPool
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , VkCommandPoolTrimFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkTrimCommandPool.html vkTrimCommandPool registry at www.khronos.org>
foreign import ccall unsafe "vkTrimCommandPool" vkTrimCommandPool
               :: VkDevice -- ^ device
                           -> VkCommandPool -- ^ commandPool
                                            -> VkCommandPoolTrimFlags -- ^ flags
                                                                      -> IO ()

-- | > () vkTrimCommandPool
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , VkCommandPoolTrimFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkTrimCommandPool.html vkTrimCommandPool registry at www.khronos.org>
foreign import ccall safe "vkTrimCommandPool" vkTrimCommandPoolSafe
               :: VkDevice -- ^ device
                           -> VkCommandPool -- ^ commandPool
                                            -> VkCommandPoolTrimFlags -- ^ flags
                                                                      -> IO ()

pattern VK_ERROR_OUT_OF_POOL_MEMORY :: VkResult

pattern VK_ERROR_OUT_OF_POOL_MEMORY = VkResult (-1000069000)

-- | Format can be used as the source image of image transfer commands
--
--   bitpos = @14@
pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT =
        VkFormatFeatureFlagBits 16384

-- | Format can be used as the destination image of image transfer commands
--
--   bitpos = @15@
pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT =
        VkFormatFeatureFlagBits 32768

-- | The 3D image can be viewed as a 2D or 2D array image
--
--   bitpos = @5@
pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT ::
        VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT =
        VkImageCreateFlagBits 32

-- | bitpos = @7@
pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT ::
        VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT =
        VkImageCreateFlagBits 128

-- | bitpos = @8@
pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT =
        VkImageCreateFlagBits 256

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES
        = VkStructureType 1000117000

pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO
        = VkStructureType 1000117001

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO =
        VkStructureType 1000117002

pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO
        = VkStructureType 1000117003

pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
        :: VkImageLayout

pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
        = VkImageLayout 1000117000

pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
        :: VkImageLayout

pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
        = VkImageLayout 1000117001

pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO =
        VkStructureType 1000053000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES =
        VkStructureType 1000053001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES =
        VkStructureType 1000053002

-- | bitpos = @1@
pattern VK_DEPENDENCY_VIEW_LOCAL_BIT :: VkDependencyFlagBits

pattern VK_DEPENDENCY_VIEW_LOCAL_BIT = VkDependencyFlagBits 2

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
        = VkStructureType 1000120000

-- | > () vkGetDeviceQueue2
--   >     ( VkDevice device
--   >     , const VkDeviceQueueInfo2* pQueueInfo
--   >     , VkQueue* pQueue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceQueue2.html vkGetDeviceQueue2 registry at www.khronos.org>
foreign import ccall unsafe "vkGetDeviceQueue2" vkGetDeviceQueue2
               :: VkDevice -- ^ device
                           -> Ptr VkDeviceQueueInfo2 -- ^ pQueueInfo
                                                     -> Ptr VkQueue -- ^ pQueue
                                                                    -> IO ()

-- | > () vkGetDeviceQueue2
--   >     ( VkDevice device
--   >     , const VkDeviceQueueInfo2* pQueueInfo
--   >     , VkQueue* pQueue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceQueue2.html vkGetDeviceQueue2 registry at www.khronos.org>
foreign import ccall safe "vkGetDeviceQueue2" vkGetDeviceQueue2Safe
               :: VkDevice -- ^ device
                           -> Ptr VkDeviceQueueInfo2 -- ^ pQueueInfo
                                                     -> Ptr VkQueue -- ^ pQueue
                                                                    -> IO ()

pattern VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO =
        VkStructureType 1000145000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES
        = VkStructureType 1000145001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES
        = VkStructureType 1000145002

pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2 :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2 =
        VkStructureType 1000145003

-- | Queues may support protected operations
--
--   bitpos = @4@
pattern VK_QUEUE_PROTECTED_BIT :: VkQueueFlagBits

pattern VK_QUEUE_PROTECTED_BIT = VkQueueFlagBits 16

-- | Queue is a protected-capable device queue
--
--   bitpos = @0@
pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT ::
        VkDeviceQueueCreateFlagBits

pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT =
        VkDeviceQueueCreateFlagBits 1

-- | Memory is protected
--
--   bitpos = @5@
pattern VK_MEMORY_PROPERTY_PROTECTED_BIT ::
        VkMemoryPropertyFlagBits

pattern VK_MEMORY_PROPERTY_PROTECTED_BIT =
        VkMemoryPropertyFlagBits 32

-- | Buffer requires protected memory
--
--   bitpos = @3@
pattern VK_BUFFER_CREATE_PROTECTED_BIT :: VkBufferCreateFlagBits

pattern VK_BUFFER_CREATE_PROTECTED_BIT = VkBufferCreateFlagBits 8

-- | Image requires protected memory
--
--   bitpos = @11@
pattern VK_IMAGE_CREATE_PROTECTED_BIT :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_PROTECTED_BIT = VkImageCreateFlagBits 2048

-- | Command buffers allocated from pool are protected command buffers
--
--   bitpos = @2@
pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT ::
        VkCommandPoolCreateFlagBits

pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT =
        VkCommandPoolCreateFlagBits 4

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateSamplerYcbcrConversion
--   >     ( VkDevice device
--   >     , const VkSamplerYcbcrConversionCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSamplerYcbcrConversion* pYcbcrConversion
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateSamplerYcbcrConversion.html vkCreateSamplerYcbcrConversion registry at www.khronos.org>
foreign import ccall unsafe "vkCreateSamplerYcbcrConversion"
               vkCreateSamplerYcbcrConversion ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSamplerYcbcrConversionCreateInfo -- ^ pCreateInfo
                                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkSamplerYcbcrConversion -- ^ pYcbcrConversion
                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateSamplerYcbcrConversion
--   >     ( VkDevice device
--   >     , const VkSamplerYcbcrConversionCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSamplerYcbcrConversion* pYcbcrConversion
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateSamplerYcbcrConversion.html vkCreateSamplerYcbcrConversion registry at www.khronos.org>
foreign import ccall safe "vkCreateSamplerYcbcrConversion"
               vkCreateSamplerYcbcrConversionSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSamplerYcbcrConversionCreateInfo -- ^ pCreateInfo
                                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkSamplerYcbcrConversion -- ^ pYcbcrConversion
                                                  -> IO VkResult

-- | > () vkDestroySamplerYcbcrConversion
--   >     ( VkDevice device
--   >     , VkSamplerYcbcrConversion ycbcrConversion
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroySamplerYcbcrConversion.html vkDestroySamplerYcbcrConversion registry at www.khronos.org>
foreign import ccall unsafe "vkDestroySamplerYcbcrConversion"
               vkDestroySamplerYcbcrConversion ::
               VkDevice -- ^ device
                        ->
                 VkSamplerYcbcrConversion -- ^ ycbcrConversion
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

-- | > () vkDestroySamplerYcbcrConversion
--   >     ( VkDevice device
--   >     , VkSamplerYcbcrConversion ycbcrConversion
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroySamplerYcbcrConversion.html vkDestroySamplerYcbcrConversion registry at www.khronos.org>
foreign import ccall safe "vkDestroySamplerYcbcrConversion"
               vkDestroySamplerYcbcrConversionSafe ::
               VkDevice -- ^ device
                        ->
                 VkSamplerYcbcrConversion -- ^ ycbcrConversion
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO =
        VkStructureType 1000156000

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO =
        VkStructureType 1000156001

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO =
        VkStructureType 1000156002

pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO =
        VkStructureType 1000156003

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES
        = VkStructureType 1000156004

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES
        = VkStructureType 1000156005

pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION :: VkObjectType

pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION =
        VkObjectType 1000156000

pattern VK_FORMAT_G8B8G8R8_422_UNORM :: VkFormat

pattern VK_FORMAT_G8B8G8R8_422_UNORM = VkFormat 1000156000

pattern VK_FORMAT_B8G8R8G8_422_UNORM :: VkFormat

pattern VK_FORMAT_B8G8R8G8_422_UNORM = VkFormat 1000156001

pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM :: VkFormat

pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM = VkFormat 1000156002

pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM :: VkFormat

pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM = VkFormat 1000156003

pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM :: VkFormat

pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM = VkFormat 1000156004

pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM :: VkFormat

pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM = VkFormat 1000156005

pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM :: VkFormat

pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM = VkFormat 1000156006

pattern VK_FORMAT_R10X6_UNORM_PACK16 :: VkFormat

pattern VK_FORMAT_R10X6_UNORM_PACK16 = VkFormat 1000156007

pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16 :: VkFormat

pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16 = VkFormat 1000156008

pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16 :: VkFormat

pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16 =
        VkFormat 1000156009

pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16 ::
        VkFormat

pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16 =
        VkFormat 1000156010

pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16 ::
        VkFormat

pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16 =
        VkFormat 1000156011

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16 ::
        VkFormat

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16 =
        VkFormat 1000156012

pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 ::
        VkFormat

pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 =
        VkFormat 1000156013

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16 ::
        VkFormat

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16 =
        VkFormat 1000156014

pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16 ::
        VkFormat

pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16 =
        VkFormat 1000156015

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16 ::
        VkFormat

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16 =
        VkFormat 1000156016

pattern VK_FORMAT_R12X4_UNORM_PACK16 :: VkFormat

pattern VK_FORMAT_R12X4_UNORM_PACK16 = VkFormat 1000156017

pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16 :: VkFormat

pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16 = VkFormat 1000156018

pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16 :: VkFormat

pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16 =
        VkFormat 1000156019

pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16 ::
        VkFormat

pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16 =
        VkFormat 1000156020

pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16 ::
        VkFormat

pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16 =
        VkFormat 1000156021

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16 ::
        VkFormat

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16 =
        VkFormat 1000156022

pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16 ::
        VkFormat

pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16 =
        VkFormat 1000156023

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16 ::
        VkFormat

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16 =
        VkFormat 1000156024

pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16 ::
        VkFormat

pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16 =
        VkFormat 1000156025

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16 ::
        VkFormat

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16 =
        VkFormat 1000156026

pattern VK_FORMAT_G16B16G16R16_422_UNORM :: VkFormat

pattern VK_FORMAT_G16B16G16R16_422_UNORM = VkFormat 1000156027

pattern VK_FORMAT_B16G16R16G16_422_UNORM :: VkFormat

pattern VK_FORMAT_B16G16R16G16_422_UNORM = VkFormat 1000156028

pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM :: VkFormat

pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM =
        VkFormat 1000156029

pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM :: VkFormat

pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM = VkFormat 1000156030

pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM :: VkFormat

pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM =
        VkFormat 1000156031

pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM :: VkFormat

pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM = VkFormat 1000156032

pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM :: VkFormat

pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM =
        VkFormat 1000156033

-- | bitpos = @4@
pattern VK_IMAGE_ASPECT_PLANE_0_BIT :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_PLANE_0_BIT = VkImageAspectFlagBits 16

-- | bitpos = @5@
pattern VK_IMAGE_ASPECT_PLANE_1_BIT :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_PLANE_1_BIT = VkImageAspectFlagBits 32

-- | bitpos = @6@
pattern VK_IMAGE_ASPECT_PLANE_2_BIT :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_PLANE_2_BIT = VkImageAspectFlagBits 64

-- | bitpos = @9@
pattern VK_IMAGE_CREATE_DISJOINT_BIT :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_DISJOINT_BIT = VkImageCreateFlagBits 512

-- | Format can have midpoint rather than cosited chroma samples
--
--   bitpos = @17@
pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT =
        VkFormatFeatureFlagBits 131072

-- | Format can be used with linear filtering whilst color conversion is enabled
--
--   bitpos = @18@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
        :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
        = VkFormatFeatureFlagBits 262144

-- | Format can have different chroma, min and mag filters
--
--   bitpos = @19@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
        :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
        = VkFormatFeatureFlagBits 524288

-- | bitpos = @20@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
        :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
        = VkFormatFeatureFlagBits 1048576

-- | bitpos = @21@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
        :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
        = VkFormatFeatureFlagBits 2097152

-- | Format supports disjoint planes
--
--   bitpos = @22@
pattern VK_FORMAT_FEATURE_DISJOINT_BIT :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_DISJOINT_BIT =
        VkFormatFeatureFlagBits 4194304

-- | Format can have cosited rather than midpoint chroma samples
--
--   bitpos = @23@
pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT =
        VkFormatFeatureFlagBits 8388608

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDescriptorUpdateTemplate
--   >     ( VkDevice device
--   >     , const VkDescriptorUpdateTemplateCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDescriptorUpdateTemplate* pDescriptorUpdateTemplate
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateDescriptorUpdateTemplate.html vkCreateDescriptorUpdateTemplate registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDescriptorUpdateTemplate"
               vkCreateDescriptorUpdateTemplate ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorUpdateTemplateCreateInfo -- ^ pCreateInfo
                                                          ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkDescriptorUpdateTemplate -- ^ pDescriptorUpdateTemplate
                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDescriptorUpdateTemplate
--   >     ( VkDevice device
--   >     , const VkDescriptorUpdateTemplateCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDescriptorUpdateTemplate* pDescriptorUpdateTemplate
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateDescriptorUpdateTemplate.html vkCreateDescriptorUpdateTemplate registry at www.khronos.org>
foreign import ccall safe "vkCreateDescriptorUpdateTemplate"
               vkCreateDescriptorUpdateTemplateSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorUpdateTemplateCreateInfo -- ^ pCreateInfo
                                                          ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkDescriptorUpdateTemplate -- ^ pDescriptorUpdateTemplate
                                                    -> IO VkResult

-- | > () vkDestroyDescriptorUpdateTemplate
--   >     ( VkDevice device
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDescriptorUpdateTemplate.html vkDestroyDescriptorUpdateTemplate registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyDescriptorUpdateTemplate"
               vkDestroyDescriptorUpdateTemplate ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

-- | > () vkDestroyDescriptorUpdateTemplate
--   >     ( VkDevice device
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDescriptorUpdateTemplate.html vkDestroyDescriptorUpdateTemplate registry at www.khronos.org>
foreign import ccall safe "vkDestroyDescriptorUpdateTemplate"
               vkDestroyDescriptorUpdateTemplateSafe ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

-- | > () vkUpdateDescriptorSetWithTemplate
--   >     ( VkDevice device
--   >     , VkDescriptorSet descriptorSet
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkUpdateDescriptorSetWithTemplate.html vkUpdateDescriptorSetWithTemplate registry at www.khronos.org>
foreign import ccall unsafe "vkUpdateDescriptorSetWithTemplate"
               vkUpdateDescriptorSetWithTemplate ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorSet -- ^ descriptorSet
                                 -> VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                               -> Ptr Void -- ^ pData
                                                                           -> IO ()

-- | > () vkUpdateDescriptorSetWithTemplate
--   >     ( VkDevice device
--   >     , VkDescriptorSet descriptorSet
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkUpdateDescriptorSetWithTemplate.html vkUpdateDescriptorSetWithTemplate registry at www.khronos.org>
foreign import ccall safe "vkUpdateDescriptorSetWithTemplate"
               vkUpdateDescriptorSetWithTemplateSafe ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorSet -- ^ descriptorSet
                                 -> VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                               -> Ptr Void -- ^ pData
                                                                           -> IO ()

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO =
        VkStructureType 1000085000

pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE :: VkObjectType

pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE =
        VkObjectType 1000085000

-- | > () vkGetPhysicalDeviceExternalBufferProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalBufferInfo* pExternalBufferInfo
--   >     , VkExternalBufferProperties* pExternalBufferProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceExternalBufferProperties.html vkGetPhysicalDeviceExternalBufferProperties registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalBufferProperties"
               vkGetPhysicalDeviceExternalBufferProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalBufferInfo -- ^ pExternalBufferInfo
                                                        ->
                   Ptr VkExternalBufferProperties -- ^ pExternalBufferProperties
                                                  -> IO ()

-- | > () vkGetPhysicalDeviceExternalBufferProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalBufferInfo* pExternalBufferInfo
--   >     , VkExternalBufferProperties* pExternalBufferProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceExternalBufferProperties.html vkGetPhysicalDeviceExternalBufferProperties registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceExternalBufferProperties"
               vkGetPhysicalDeviceExternalBufferPropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalBufferInfo -- ^ pExternalBufferInfo
                                                        ->
                   Ptr VkExternalBufferProperties -- ^ pExternalBufferProperties
                                                  -> IO ()

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
        = VkStructureType 1000071000

pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES =
        VkStructureType 1000071001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO =
        VkStructureType 1000071002

pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES =
        VkStructureType 1000071003

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES =
        VkStructureType 1000071004

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO =
        VkStructureType 1000072000

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO =
        VkStructureType 1000072001

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO =
        VkStructureType 1000072002

pattern VK_ERROR_INVALID_EXTERNAL_HANDLE :: VkResult

pattern VK_ERROR_INVALID_EXTERNAL_HANDLE = VkResult (-1000072003)

-- | > () vkGetPhysicalDeviceExternalFenceProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalFenceInfo* pExternalFenceInfo
--   >     , VkExternalFenceProperties* pExternalFenceProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceExternalFenceProperties.html vkGetPhysicalDeviceExternalFenceProperties registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalFenceProperties"
               vkGetPhysicalDeviceExternalFenceProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalFenceInfo -- ^ pExternalFenceInfo
                                                       ->
                   Ptr VkExternalFenceProperties -- ^ pExternalFenceProperties
                                                 -> IO ()

-- | > () vkGetPhysicalDeviceExternalFenceProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalFenceInfo* pExternalFenceInfo
--   >     , VkExternalFenceProperties* pExternalFenceProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceExternalFenceProperties.html vkGetPhysicalDeviceExternalFenceProperties registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceExternalFenceProperties"
               vkGetPhysicalDeviceExternalFencePropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalFenceInfo -- ^ pExternalFenceInfo
                                                       ->
                   Ptr VkExternalFenceProperties -- ^ pExternalFenceProperties
                                                 -> IO ()

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO =
        VkStructureType 1000112000

pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES =
        VkStructureType 1000112001

pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO =
        VkStructureType 1000113000

pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO =
        VkStructureType 1000077000

-- | > () vkGetPhysicalDeviceExternalSemaphoreProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalSemaphoreInfo* pExternalSemaphoreInfo
--   >     , VkExternalSemaphoreProperties* pExternalSemaphoreProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceExternalSemaphoreProperties.html vkGetPhysicalDeviceExternalSemaphoreProperties registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalSemaphoreProperties"
               vkGetPhysicalDeviceExternalSemaphoreProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalSemaphoreInfo -- ^ pExternalSemaphoreInfo
                                                           ->
                   Ptr VkExternalSemaphoreProperties -- ^ pExternalSemaphoreProperties
                                                     -> IO ()

-- | > () vkGetPhysicalDeviceExternalSemaphoreProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalSemaphoreInfo* pExternalSemaphoreInfo
--   >     , VkExternalSemaphoreProperties* pExternalSemaphoreProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceExternalSemaphoreProperties.html vkGetPhysicalDeviceExternalSemaphoreProperties registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceExternalSemaphoreProperties"
               vkGetPhysicalDeviceExternalSemaphorePropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalSemaphoreInfo -- ^ pExternalSemaphoreInfo
                                                           ->
                   Ptr VkExternalSemaphoreProperties -- ^ pExternalSemaphoreProperties
                                                     -> IO ()

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO =
        VkStructureType 1000076000

pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES =
        VkStructureType 1000076001

-- | > () vkGetDescriptorSetLayoutSupport
--   >     ( VkDevice device
--   >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
--   >     , VkDescriptorSetLayoutSupport* pSupport
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDescriptorSetLayoutSupport.html vkGetDescriptorSetLayoutSupport registry at www.khronos.org>
foreign import ccall unsafe "vkGetDescriptorSetLayoutSupport"
               vkGetDescriptorSetLayoutSupport ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                     ->
                   Ptr VkDescriptorSetLayoutSupport -- ^ pSupport
                                                    -> IO ()

-- | > () vkGetDescriptorSetLayoutSupport
--   >     ( VkDevice device
--   >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
--   >     , VkDescriptorSetLayoutSupport* pSupport
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDescriptorSetLayoutSupport.html vkGetDescriptorSetLayoutSupport registry at www.khronos.org>
foreign import ccall safe "vkGetDescriptorSetLayoutSupport"
               vkGetDescriptorSetLayoutSupportSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                     ->
                   Ptr VkDescriptorSetLayoutSupport -- ^ pSupport
                                                    -> IO ()

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
        = VkStructureType 1000168000

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT =
        VkStructureType 1000168001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES
        = VkStructureType 1000063000
