{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
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
        VkEnumerateInstanceVersion, pattern VkEnumerateInstanceVersion,
        HS_vkEnumerateInstanceVersion, PFN_vkEnumerateInstanceVersion,
        unwrapVkEnumerateInstanceVersion, vkEnumerateInstanceVersion,
        vkEnumerateInstanceVersionSafe,
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
        VkBindBufferMemory2, pattern VkBindBufferMemory2,
        HS_vkBindBufferMemory2, PFN_vkBindBufferMemory2,
        unwrapVkBindBufferMemory2, vkBindBufferMemory2,
        vkBindBufferMemory2Safe, VkBindImageMemory2,
        pattern VkBindImageMemory2, HS_vkBindImageMemory2,
        PFN_vkBindImageMemory2, unwrapVkBindImageMemory2,
        vkBindImageMemory2, vkBindImageMemory2Safe,
        module Graphics.Vulkan.Types.Handles,
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
        VkGetDeviceGroupPeerMemoryFeatures,
        pattern VkGetDeviceGroupPeerMemoryFeatures,
        HS_vkGetDeviceGroupPeerMemoryFeatures,
        PFN_vkGetDeviceGroupPeerMemoryFeatures,
        unwrapVkGetDeviceGroupPeerMemoryFeatures,
        vkGetDeviceGroupPeerMemoryFeatures,
        vkGetDeviceGroupPeerMemoryFeaturesSafe, VkCmdSetDeviceMask,
        pattern VkCmdSetDeviceMask, HS_vkCmdSetDeviceMask,
        PFN_vkCmdSetDeviceMask, unwrapVkCmdSetDeviceMask,
        vkCmdSetDeviceMask, vkCmdSetDeviceMaskSafe, VkCmdDispatchBase,
        pattern VkCmdDispatchBase, HS_vkCmdDispatchBase,
        PFN_vkCmdDispatchBase, unwrapVkCmdDispatchBase, vkCmdDispatchBase,
        vkCmdDispatchBaseSafe,
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
        VkEnumeratePhysicalDeviceGroups,
        pattern VkEnumeratePhysicalDeviceGroups,
        HS_vkEnumeratePhysicalDeviceGroups,
        PFN_vkEnumeratePhysicalDeviceGroups,
        unwrapVkEnumeratePhysicalDeviceGroups,
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
        VkGetImageMemoryRequirements2,
        pattern VkGetImageMemoryRequirements2,
        HS_vkGetImageMemoryRequirements2,
        PFN_vkGetImageMemoryRequirements2,
        unwrapVkGetImageMemoryRequirements2, vkGetImageMemoryRequirements2,
        vkGetImageMemoryRequirements2Safe, VkGetBufferMemoryRequirements2,
        pattern VkGetBufferMemoryRequirements2,
        HS_vkGetBufferMemoryRequirements2,
        PFN_vkGetBufferMemoryRequirements2,
        unwrapVkGetBufferMemoryRequirements2,
        vkGetBufferMemoryRequirements2, vkGetBufferMemoryRequirements2Safe,
        VkGetImageSparseMemoryRequirements2,
        pattern VkGetImageSparseMemoryRequirements2,
        HS_vkGetImageSparseMemoryRequirements2,
        PFN_vkGetImageSparseMemoryRequirements2,
        unwrapVkGetImageSparseMemoryRequirements2,
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
        VkGetPhysicalDeviceFeatures2, pattern VkGetPhysicalDeviceFeatures2,
        HS_vkGetPhysicalDeviceFeatures2, PFN_vkGetPhysicalDeviceFeatures2,
        unwrapVkGetPhysicalDeviceFeatures2, vkGetPhysicalDeviceFeatures2,
        vkGetPhysicalDeviceFeatures2Safe, VkGetPhysicalDeviceProperties2,
        pattern VkGetPhysicalDeviceProperties2,
        HS_vkGetPhysicalDeviceProperties2,
        PFN_vkGetPhysicalDeviceProperties2,
        unwrapVkGetPhysicalDeviceProperties2,
        vkGetPhysicalDeviceProperties2, vkGetPhysicalDeviceProperties2Safe,
        VkGetPhysicalDeviceFormatProperties2,
        pattern VkGetPhysicalDeviceFormatProperties2,
        HS_vkGetPhysicalDeviceFormatProperties2,
        PFN_vkGetPhysicalDeviceFormatProperties2,
        unwrapVkGetPhysicalDeviceFormatProperties2,
        vkGetPhysicalDeviceFormatProperties2,
        vkGetPhysicalDeviceFormatProperties2Safe,
        VkGetPhysicalDeviceImageFormatProperties2,
        pattern VkGetPhysicalDeviceImageFormatProperties2,
        HS_vkGetPhysicalDeviceImageFormatProperties2,
        PFN_vkGetPhysicalDeviceImageFormatProperties2,
        unwrapVkGetPhysicalDeviceImageFormatProperties2,
        vkGetPhysicalDeviceImageFormatProperties2,
        vkGetPhysicalDeviceImageFormatProperties2Safe,
        VkGetPhysicalDeviceQueueFamilyProperties2,
        pattern VkGetPhysicalDeviceQueueFamilyProperties2,
        HS_vkGetPhysicalDeviceQueueFamilyProperties2,
        PFN_vkGetPhysicalDeviceQueueFamilyProperties2,
        unwrapVkGetPhysicalDeviceQueueFamilyProperties2,
        vkGetPhysicalDeviceQueueFamilyProperties2,
        vkGetPhysicalDeviceQueueFamilyProperties2Safe,
        VkGetPhysicalDeviceMemoryProperties2,
        pattern VkGetPhysicalDeviceMemoryProperties2,
        HS_vkGetPhysicalDeviceMemoryProperties2,
        PFN_vkGetPhysicalDeviceMemoryProperties2,
        unwrapVkGetPhysicalDeviceMemoryProperties2,
        vkGetPhysicalDeviceMemoryProperties2,
        vkGetPhysicalDeviceMemoryProperties2Safe,
        VkGetPhysicalDeviceSparseImageFormatProperties2,
        pattern VkGetPhysicalDeviceSparseImageFormatProperties2,
        HS_vkGetPhysicalDeviceSparseImageFormatProperties2,
        PFN_vkGetPhysicalDeviceSparseImageFormatProperties2,
        unwrapVkGetPhysicalDeviceSparseImageFormatProperties2,
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
        VkTrimCommandPool, pattern VkTrimCommandPool, HS_vkTrimCommandPool,
        PFN_vkTrimCommandPool, unwrapVkTrimCommandPool, vkTrimCommandPool,
        vkTrimCommandPoolSafe, pattern VK_ERROR_OUT_OF_POOL_MEMORY,
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
        VkGetDeviceQueue2, pattern VkGetDeviceQueue2, HS_vkGetDeviceQueue2,
        PFN_vkGetDeviceQueue2, unwrapVkGetDeviceQueue2, vkGetDeviceQueue2,
        vkGetDeviceQueue2Safe,
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
        VkCreateSamplerYcbcrConversion,
        pattern VkCreateSamplerYcbcrConversion,
        HS_vkCreateSamplerYcbcrConversion,
        PFN_vkCreateSamplerYcbcrConversion,
        unwrapVkCreateSamplerYcbcrConversion,
        vkCreateSamplerYcbcrConversion, vkCreateSamplerYcbcrConversionSafe,
        VkDestroySamplerYcbcrConversion,
        pattern VkDestroySamplerYcbcrConversion,
        HS_vkDestroySamplerYcbcrConversion,
        PFN_vkDestroySamplerYcbcrConversion,
        unwrapVkDestroySamplerYcbcrConversion,
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
        VkCreateDescriptorUpdateTemplate,
        pattern VkCreateDescriptorUpdateTemplate,
        HS_vkCreateDescriptorUpdateTemplate,
        PFN_vkCreateDescriptorUpdateTemplate,
        unwrapVkCreateDescriptorUpdateTemplate,
        vkCreateDescriptorUpdateTemplate,
        vkCreateDescriptorUpdateTemplateSafe,
        VkDestroyDescriptorUpdateTemplate,
        pattern VkDestroyDescriptorUpdateTemplate,
        HS_vkDestroyDescriptorUpdateTemplate,
        PFN_vkDestroyDescriptorUpdateTemplate,
        unwrapVkDestroyDescriptorUpdateTemplate,
        vkDestroyDescriptorUpdateTemplate,
        vkDestroyDescriptorUpdateTemplateSafe,
        VkUpdateDescriptorSetWithTemplate,
        pattern VkUpdateDescriptorSetWithTemplate,
        HS_vkUpdateDescriptorSetWithTemplate,
        PFN_vkUpdateDescriptorSetWithTemplate,
        unwrapVkUpdateDescriptorSetWithTemplate,
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
        VkGetPhysicalDeviceExternalBufferProperties,
        pattern VkGetPhysicalDeviceExternalBufferProperties,
        HS_vkGetPhysicalDeviceExternalBufferProperties,
        PFN_vkGetPhysicalDeviceExternalBufferProperties,
        unwrapVkGetPhysicalDeviceExternalBufferProperties,
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
        VkGetPhysicalDeviceExternalFenceProperties,
        pattern VkGetPhysicalDeviceExternalFenceProperties,
        HS_vkGetPhysicalDeviceExternalFenceProperties,
        PFN_vkGetPhysicalDeviceExternalFenceProperties,
        unwrapVkGetPhysicalDeviceExternalFenceProperties,
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
        VkGetPhysicalDeviceExternalSemaphoreProperties,
        pattern VkGetPhysicalDeviceExternalSemaphoreProperties,
        HS_vkGetPhysicalDeviceExternalSemaphoreProperties,
        PFN_vkGetPhysicalDeviceExternalSemaphoreProperties,
        unwrapVkGetPhysicalDeviceExternalSemaphoreProperties,
        vkGetPhysicalDeviceExternalSemaphoreProperties,
        vkGetPhysicalDeviceExternalSemaphorePropertiesSafe,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES,
        -- ** Promoted from VK_KHR_maintenance3
        module Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutSupport,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMaintenance3Properties,
        -- > #include "vk_platform.h"
        VkGetDescriptorSetLayoutSupport,
        pattern VkGetDescriptorSetLayoutSupport,
        HS_vkGetDescriptorSetLayoutSupport,
        PFN_vkGetDescriptorSetLayoutSupport,
        unwrapVkGetDescriptorSetLayoutSupport,
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
import           GHC.Ptr
                                                                                                 (Ptr (..))
import           Graphics.Vulkan.Constants
                                                                                                 (pattern VK_LUID_SIZE,
                                                                                                 pattern VK_MAX_DEVICE_GROUP_SIZE,
                                                                                                 pattern VK_QUEUE_FAMILY_EXTERNAL)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
                                                                                                 (VulkanProc (..))
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

pattern VkEnumerateInstanceVersion :: CString

pattern VkEnumerateInstanceVersion <-
        (is_VkEnumerateInstanceVersion -> True)
  where VkEnumerateInstanceVersion = _VkEnumerateInstanceVersion

{-# INLINE _VkEnumerateInstanceVersion #-}

_VkEnumerateInstanceVersion :: CString
_VkEnumerateInstanceVersion = Ptr "vkEnumerateInstanceVersion\NUL"#

{-# INLINE is_VkEnumerateInstanceVersion #-}

is_VkEnumerateInstanceVersion :: CString -> Bool
is_VkEnumerateInstanceVersion
  = (EQ ==) . cmpCStrings _VkEnumerateInstanceVersion

type VkEnumerateInstanceVersion = "vkEnumerateInstanceVersion"

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

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkEnumerateInstanceVersion
--   >     ( uint32_t* pApiVersion
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumerateInstanceVersion.html vkEnumerateInstanceVersion registry at www.khronos.org>
type HS_vkEnumerateInstanceVersion = Ptr Word32 -- ^ pApiVersion
                                                -> IO VkResult

type PFN_vkEnumerateInstanceVersion =
     FunPtr HS_vkEnumerateInstanceVersion

foreign import ccall "dynamic" unwrapVkEnumerateInstanceVersion ::
               PFN_vkEnumerateInstanceVersion -> HS_vkEnumerateInstanceVersion

instance VulkanProc "vkEnumerateInstanceVersion" where
        type VkProcType "vkEnumerateInstanceVersion" =
             HS_vkEnumerateInstanceVersion
        vkProcSymbol = _VkEnumerateInstanceVersion

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkEnumerateInstanceVersion

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES =
        VkStructureType 1000094000

pattern VkBindBufferMemory2 :: CString

pattern VkBindBufferMemory2 <- (is_VkBindBufferMemory2 -> True)
  where VkBindBufferMemory2 = _VkBindBufferMemory2

{-# INLINE _VkBindBufferMemory2 #-}

_VkBindBufferMemory2 :: CString
_VkBindBufferMemory2 = Ptr "vkBindBufferMemory2\NUL"#

{-# INLINE is_VkBindBufferMemory2 #-}

is_VkBindBufferMemory2 :: CString -> Bool
is_VkBindBufferMemory2 = (EQ ==) . cmpCStrings _VkBindBufferMemory2

type VkBindBufferMemory2 = "vkBindBufferMemory2"

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
--   > VkResult vkBindBufferMemory2
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindBufferMemoryInfo* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkBindBufferMemory2.html vkBindBufferMemory2 registry at www.khronos.org>
type HS_vkBindBufferMemory2 =
     VkDevice -- ^ device
              -> Word32 -- ^ bindInfoCount
                        -> Ptr VkBindBufferMemoryInfo -- ^ pBindInfos
                                                      -> IO VkResult

type PFN_vkBindBufferMemory2 = FunPtr HS_vkBindBufferMemory2

foreign import ccall "dynamic" unwrapVkBindBufferMemory2 ::
               PFN_vkBindBufferMemory2 -> HS_vkBindBufferMemory2

instance VulkanProc "vkBindBufferMemory2" where
        type VkProcType "vkBindBufferMemory2" = HS_vkBindBufferMemory2
        vkProcSymbol = _VkBindBufferMemory2

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkBindBufferMemory2

        {-# INLINE unwrapVkProcPtr #-}

pattern VkBindImageMemory2 :: CString

pattern VkBindImageMemory2 <- (is_VkBindImageMemory2 -> True)
  where VkBindImageMemory2 = _VkBindImageMemory2

{-# INLINE _VkBindImageMemory2 #-}

_VkBindImageMemory2 :: CString
_VkBindImageMemory2 = Ptr "vkBindImageMemory2\NUL"#

{-# INLINE is_VkBindImageMemory2 #-}

is_VkBindImageMemory2 :: CString -> Bool
is_VkBindImageMemory2 = (EQ ==) . cmpCStrings _VkBindImageMemory2

type VkBindImageMemory2 = "vkBindImageMemory2"

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
type HS_vkBindImageMemory2 =
     VkDevice -- ^ device
              -> Word32 -- ^ bindInfoCount
                        -> Ptr VkBindImageMemoryInfo -- ^ pBindInfos
                                                     -> IO VkResult

type PFN_vkBindImageMemory2 = FunPtr HS_vkBindImageMemory2

foreign import ccall "dynamic" unwrapVkBindImageMemory2 ::
               PFN_vkBindImageMemory2 -> HS_vkBindImageMemory2

instance VulkanProc "vkBindImageMemory2" where
        type VkProcType "vkBindImageMemory2" = HS_vkBindImageMemory2
        vkProcSymbol = _VkBindImageMemory2

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkBindImageMemory2

        {-# INLINE unwrapVkProcPtr #-}

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

pattern VkGetDeviceGroupPeerMemoryFeatures :: CString

pattern VkGetDeviceGroupPeerMemoryFeatures <-
        (is_VkGetDeviceGroupPeerMemoryFeatures -> True)
  where VkGetDeviceGroupPeerMemoryFeatures
          = _VkGetDeviceGroupPeerMemoryFeatures

{-# INLINE _VkGetDeviceGroupPeerMemoryFeatures #-}

_VkGetDeviceGroupPeerMemoryFeatures :: CString
_VkGetDeviceGroupPeerMemoryFeatures
  = Ptr "vkGetDeviceGroupPeerMemoryFeatures\NUL"#

{-# INLINE is_VkGetDeviceGroupPeerMemoryFeatures #-}

is_VkGetDeviceGroupPeerMemoryFeatures :: CString -> Bool
is_VkGetDeviceGroupPeerMemoryFeatures
  = (EQ ==) . cmpCStrings _VkGetDeviceGroupPeerMemoryFeatures

type VkGetDeviceGroupPeerMemoryFeatures =
     "vkGetDeviceGroupPeerMemoryFeatures"

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

-- | > () vkGetDeviceGroupPeerMemoryFeatures
--   >     ( VkDevice device
--   >     , uint32_t heapIndex
--   >     , uint32_t localDeviceIndex
--   >     , uint32_t remoteDeviceIndex
--   >     , VkPeerMemoryFeatureFlags* pPeerMemoryFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceGroupPeerMemoryFeatures.html vkGetDeviceGroupPeerMemoryFeatures registry at www.khronos.org>
type HS_vkGetDeviceGroupPeerMemoryFeatures =
     VkDevice -- ^ device
              ->
       Word32 -- ^ heapIndex
              -> Word32 -- ^ localDeviceIndex
                        -> Word32 -- ^ remoteDeviceIndex
                                  -> Ptr VkPeerMemoryFeatureFlags -- ^ pPeerMemoryFeatures
                                                                  -> IO ()

type PFN_vkGetDeviceGroupPeerMemoryFeatures =
     FunPtr HS_vkGetDeviceGroupPeerMemoryFeatures

foreign import ccall "dynamic"
               unwrapVkGetDeviceGroupPeerMemoryFeatures ::
               PFN_vkGetDeviceGroupPeerMemoryFeatures ->
                 HS_vkGetDeviceGroupPeerMemoryFeatures

instance VulkanProc "vkGetDeviceGroupPeerMemoryFeatures" where
        type VkProcType "vkGetDeviceGroupPeerMemoryFeatures" =
             HS_vkGetDeviceGroupPeerMemoryFeatures
        vkProcSymbol = _VkGetDeviceGroupPeerMemoryFeatures

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetDeviceGroupPeerMemoryFeatures

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdSetDeviceMask :: CString

pattern VkCmdSetDeviceMask <- (is_VkCmdSetDeviceMask -> True)
  where VkCmdSetDeviceMask = _VkCmdSetDeviceMask

{-# INLINE _VkCmdSetDeviceMask #-}

_VkCmdSetDeviceMask :: CString
_VkCmdSetDeviceMask = Ptr "vkCmdSetDeviceMask\NUL"#

{-# INLINE is_VkCmdSetDeviceMask #-}

is_VkCmdSetDeviceMask :: CString -> Bool
is_VkCmdSetDeviceMask = (EQ ==) . cmpCStrings _VkCmdSetDeviceMask

type VkCmdSetDeviceMask = "vkCmdSetDeviceMask"

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
type HS_vkCmdSetDeviceMask = VkCommandBuffer -- ^ commandBuffer
                                             -> Word32 -- ^ deviceMask
                                                       -> IO ()

type PFN_vkCmdSetDeviceMask = FunPtr HS_vkCmdSetDeviceMask

foreign import ccall "dynamic" unwrapVkCmdSetDeviceMask ::
               PFN_vkCmdSetDeviceMask -> HS_vkCmdSetDeviceMask

instance VulkanProc "vkCmdSetDeviceMask" where
        type VkProcType "vkCmdSetDeviceMask" = HS_vkCmdSetDeviceMask
        vkProcSymbol = _VkCmdSetDeviceMask

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetDeviceMask

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdDispatchBase :: CString

pattern VkCmdDispatchBase <- (is_VkCmdDispatchBase -> True)
  where VkCmdDispatchBase = _VkCmdDispatchBase

{-# INLINE _VkCmdDispatchBase #-}

_VkCmdDispatchBase :: CString
_VkCmdDispatchBase = Ptr "vkCmdDispatchBase\NUL"#

{-# INLINE is_VkCmdDispatchBase #-}

is_VkCmdDispatchBase :: CString -> Bool
is_VkCmdDispatchBase = (EQ ==) . cmpCStrings _VkCmdDispatchBase

type VkCmdDispatchBase = "vkCmdDispatchBase"

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
type HS_vkCmdDispatchBase =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ baseGroupX
              -> Word32 -- ^ baseGroupY
                        -> Word32 -- ^ baseGroupZ
                                  -> Word32 -- ^ groupCountX
                                            -> Word32 -- ^ groupCountY
                                                      -> Word32 -- ^ groupCountZ
                                                                -> IO ()

type PFN_vkCmdDispatchBase = FunPtr HS_vkCmdDispatchBase

foreign import ccall "dynamic" unwrapVkCmdDispatchBase ::
               PFN_vkCmdDispatchBase -> HS_vkCmdDispatchBase

instance VulkanProc "vkCmdDispatchBase" where
        type VkProcType "vkCmdDispatchBase" = HS_vkCmdDispatchBase
        vkProcSymbol = _VkCmdDispatchBase

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdDispatchBase

        {-# INLINE unwrapVkProcPtr #-}

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

pattern VkEnumeratePhysicalDeviceGroups :: CString

pattern VkEnumeratePhysicalDeviceGroups <-
        (is_VkEnumeratePhysicalDeviceGroups -> True)
  where VkEnumeratePhysicalDeviceGroups
          = _VkEnumeratePhysicalDeviceGroups

{-# INLINE _VkEnumeratePhysicalDeviceGroups #-}

_VkEnumeratePhysicalDeviceGroups :: CString
_VkEnumeratePhysicalDeviceGroups
  = Ptr "vkEnumeratePhysicalDeviceGroups\NUL"#

{-# INLINE is_VkEnumeratePhysicalDeviceGroups #-}

is_VkEnumeratePhysicalDeviceGroups :: CString -> Bool
is_VkEnumeratePhysicalDeviceGroups
  = (EQ ==) . cmpCStrings _VkEnumeratePhysicalDeviceGroups

type VkEnumeratePhysicalDeviceGroups =
     "vkEnumeratePhysicalDeviceGroups"

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
type HS_vkEnumeratePhysicalDeviceGroups =
     VkInstance -- ^ instance
                ->
       Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                  -> Ptr VkPhysicalDeviceGroupProperties -- ^ pPhysicalDeviceGroupProperties
                                                         -> IO VkResult

type PFN_vkEnumeratePhysicalDeviceGroups =
     FunPtr HS_vkEnumeratePhysicalDeviceGroups

foreign import ccall "dynamic"
               unwrapVkEnumeratePhysicalDeviceGroups ::
               PFN_vkEnumeratePhysicalDeviceGroups ->
                 HS_vkEnumeratePhysicalDeviceGroups

instance VulkanProc "vkEnumeratePhysicalDeviceGroups" where
        type VkProcType "vkEnumeratePhysicalDeviceGroups" =
             HS_vkEnumeratePhysicalDeviceGroups
        vkProcSymbol = _VkEnumeratePhysicalDeviceGroups

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkEnumeratePhysicalDeviceGroups

        {-# INLINE unwrapVkProcPtr #-}

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

pattern VkGetImageMemoryRequirements2 :: CString

pattern VkGetImageMemoryRequirements2 <-
        (is_VkGetImageMemoryRequirements2 -> True)
  where VkGetImageMemoryRequirements2
          = _VkGetImageMemoryRequirements2

{-# INLINE _VkGetImageMemoryRequirements2 #-}

_VkGetImageMemoryRequirements2 :: CString
_VkGetImageMemoryRequirements2
  = Ptr "vkGetImageMemoryRequirements2\NUL"#

{-# INLINE is_VkGetImageMemoryRequirements2 #-}

is_VkGetImageMemoryRequirements2 :: CString -> Bool
is_VkGetImageMemoryRequirements2
  = (EQ ==) . cmpCStrings _VkGetImageMemoryRequirements2

type VkGetImageMemoryRequirements2 =
     "vkGetImageMemoryRequirements2"

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

-- | > () vkGetImageMemoryRequirements2
--   >     ( VkDevice device
--   >     , const VkImageMemoryRequirementsInfo2* pInfo
--   >     , VkMemoryRequirements2* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageMemoryRequirements2.html vkGetImageMemoryRequirements2 registry at www.khronos.org>
type HS_vkGetImageMemoryRequirements2 =
     VkDevice -- ^ device
              ->
       Ptr VkImageMemoryRequirementsInfo2 -- ^ pInfo
                                          ->
         Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                   -> IO ()

type PFN_vkGetImageMemoryRequirements2 =
     FunPtr HS_vkGetImageMemoryRequirements2

foreign import ccall "dynamic" unwrapVkGetImageMemoryRequirements2
               ::
               PFN_vkGetImageMemoryRequirements2 ->
                 HS_vkGetImageMemoryRequirements2

instance VulkanProc "vkGetImageMemoryRequirements2" where
        type VkProcType "vkGetImageMemoryRequirements2" =
             HS_vkGetImageMemoryRequirements2
        vkProcSymbol = _VkGetImageMemoryRequirements2

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetImageMemoryRequirements2

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetBufferMemoryRequirements2 :: CString

pattern VkGetBufferMemoryRequirements2 <-
        (is_VkGetBufferMemoryRequirements2 -> True)
  where VkGetBufferMemoryRequirements2
          = _VkGetBufferMemoryRequirements2

{-# INLINE _VkGetBufferMemoryRequirements2 #-}

_VkGetBufferMemoryRequirements2 :: CString
_VkGetBufferMemoryRequirements2
  = Ptr "vkGetBufferMemoryRequirements2\NUL"#

{-# INLINE is_VkGetBufferMemoryRequirements2 #-}

is_VkGetBufferMemoryRequirements2 :: CString -> Bool
is_VkGetBufferMemoryRequirements2
  = (EQ ==) . cmpCStrings _VkGetBufferMemoryRequirements2

type VkGetBufferMemoryRequirements2 =
     "vkGetBufferMemoryRequirements2"

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

-- | > () vkGetBufferMemoryRequirements2
--   >     ( VkDevice device
--   >     , const VkBufferMemoryRequirementsInfo2* pInfo
--   >     , VkMemoryRequirements2* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetBufferMemoryRequirements2.html vkGetBufferMemoryRequirements2 registry at www.khronos.org>
type HS_vkGetBufferMemoryRequirements2 =
     VkDevice -- ^ device
              ->
       Ptr VkBufferMemoryRequirementsInfo2 -- ^ pInfo
                                           ->
         Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                   -> IO ()

type PFN_vkGetBufferMemoryRequirements2 =
     FunPtr HS_vkGetBufferMemoryRequirements2

foreign import ccall "dynamic" unwrapVkGetBufferMemoryRequirements2
               ::
               PFN_vkGetBufferMemoryRequirements2 ->
                 HS_vkGetBufferMemoryRequirements2

instance VulkanProc "vkGetBufferMemoryRequirements2" where
        type VkProcType "vkGetBufferMemoryRequirements2" =
             HS_vkGetBufferMemoryRequirements2
        vkProcSymbol = _VkGetBufferMemoryRequirements2

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetBufferMemoryRequirements2

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetImageSparseMemoryRequirements2 :: CString

pattern VkGetImageSparseMemoryRequirements2 <-
        (is_VkGetImageSparseMemoryRequirements2 -> True)
  where VkGetImageSparseMemoryRequirements2
          = _VkGetImageSparseMemoryRequirements2

{-# INLINE _VkGetImageSparseMemoryRequirements2 #-}

_VkGetImageSparseMemoryRequirements2 :: CString
_VkGetImageSparseMemoryRequirements2
  = Ptr "vkGetImageSparseMemoryRequirements2\NUL"#

{-# INLINE is_VkGetImageSparseMemoryRequirements2 #-}

is_VkGetImageSparseMemoryRequirements2 :: CString -> Bool
is_VkGetImageSparseMemoryRequirements2
  = (EQ ==) . cmpCStrings _VkGetImageSparseMemoryRequirements2

type VkGetImageSparseMemoryRequirements2 =
     "vkGetImageSparseMemoryRequirements2"

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

-- | > () vkGetImageSparseMemoryRequirements2
--   >     ( VkDevice device
--   >     , const VkImageSparseMemoryRequirementsInfo2* pInfo
--   >     , uint32_t* pSparseMemoryRequirementCount
--   >     , VkSparseImageMemoryRequirements2* pSparseMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageSparseMemoryRequirements2.html vkGetImageSparseMemoryRequirements2 registry at www.khronos.org>
type HS_vkGetImageSparseMemoryRequirements2 =
     VkDevice -- ^ device
              ->
       Ptr VkImageSparseMemoryRequirementsInfo2 -- ^ pInfo
                                                ->
         Ptr Word32 -- ^ pSparseMemoryRequirementCount
                    -> Ptr VkSparseImageMemoryRequirements2 -- ^ pSparseMemoryRequirements
                                                            -> IO ()

type PFN_vkGetImageSparseMemoryRequirements2 =
     FunPtr HS_vkGetImageSparseMemoryRequirements2

foreign import ccall "dynamic"
               unwrapVkGetImageSparseMemoryRequirements2 ::
               PFN_vkGetImageSparseMemoryRequirements2 ->
                 HS_vkGetImageSparseMemoryRequirements2

instance VulkanProc "vkGetImageSparseMemoryRequirements2" where
        type VkProcType "vkGetImageSparseMemoryRequirements2" =
             HS_vkGetImageSparseMemoryRequirements2
        vkProcSymbol = _VkGetImageSparseMemoryRequirements2

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetImageSparseMemoryRequirements2

        {-# INLINE unwrapVkProcPtr #-}

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

pattern VkGetPhysicalDeviceFeatures2 :: CString

pattern VkGetPhysicalDeviceFeatures2 <-
        (is_VkGetPhysicalDeviceFeatures2 -> True)
  where VkGetPhysicalDeviceFeatures2 = _VkGetPhysicalDeviceFeatures2

{-# INLINE _VkGetPhysicalDeviceFeatures2 #-}

_VkGetPhysicalDeviceFeatures2 :: CString
_VkGetPhysicalDeviceFeatures2
  = Ptr "vkGetPhysicalDeviceFeatures2\NUL"#

{-# INLINE is_VkGetPhysicalDeviceFeatures2 #-}

is_VkGetPhysicalDeviceFeatures2 :: CString -> Bool
is_VkGetPhysicalDeviceFeatures2
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceFeatures2

type VkGetPhysicalDeviceFeatures2 = "vkGetPhysicalDeviceFeatures2"

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

-- | > () vkGetPhysicalDeviceFeatures2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures2* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFeatures2.html vkGetPhysicalDeviceFeatures2 registry at www.khronos.org>
type HS_vkGetPhysicalDeviceFeatures2 =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Ptr VkPhysicalDeviceFeatures2 -- ^ pFeatures
                                                       -> IO ()

type PFN_vkGetPhysicalDeviceFeatures2 =
     FunPtr HS_vkGetPhysicalDeviceFeatures2

foreign import ccall "dynamic" unwrapVkGetPhysicalDeviceFeatures2
               ::
               PFN_vkGetPhysicalDeviceFeatures2 -> HS_vkGetPhysicalDeviceFeatures2

instance VulkanProc "vkGetPhysicalDeviceFeatures2" where
        type VkProcType "vkGetPhysicalDeviceFeatures2" =
             HS_vkGetPhysicalDeviceFeatures2
        vkProcSymbol = _VkGetPhysicalDeviceFeatures2

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceFeatures2

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceProperties2 :: CString

pattern VkGetPhysicalDeviceProperties2 <-
        (is_VkGetPhysicalDeviceProperties2 -> True)
  where VkGetPhysicalDeviceProperties2
          = _VkGetPhysicalDeviceProperties2

{-# INLINE _VkGetPhysicalDeviceProperties2 #-}

_VkGetPhysicalDeviceProperties2 :: CString
_VkGetPhysicalDeviceProperties2
  = Ptr "vkGetPhysicalDeviceProperties2\NUL"#

{-# INLINE is_VkGetPhysicalDeviceProperties2 #-}

is_VkGetPhysicalDeviceProperties2 :: CString -> Bool
is_VkGetPhysicalDeviceProperties2
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceProperties2

type VkGetPhysicalDeviceProperties2 =
     "vkGetPhysicalDeviceProperties2"

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

-- | > () vkGetPhysicalDeviceProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties2* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceProperties2.html vkGetPhysicalDeviceProperties2 registry at www.khronos.org>
type HS_vkGetPhysicalDeviceProperties2 =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Ptr VkPhysicalDeviceProperties2 -- ^ pProperties
                                                         -> IO ()

type PFN_vkGetPhysicalDeviceProperties2 =
     FunPtr HS_vkGetPhysicalDeviceProperties2

foreign import ccall "dynamic" unwrapVkGetPhysicalDeviceProperties2
               ::
               PFN_vkGetPhysicalDeviceProperties2 ->
                 HS_vkGetPhysicalDeviceProperties2

instance VulkanProc "vkGetPhysicalDeviceProperties2" where
        type VkProcType "vkGetPhysicalDeviceProperties2" =
             HS_vkGetPhysicalDeviceProperties2
        vkProcSymbol = _VkGetPhysicalDeviceProperties2

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceProperties2

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceFormatProperties2 :: CString

pattern VkGetPhysicalDeviceFormatProperties2 <-
        (is_VkGetPhysicalDeviceFormatProperties2 -> True)
  where VkGetPhysicalDeviceFormatProperties2
          = _VkGetPhysicalDeviceFormatProperties2

{-# INLINE _VkGetPhysicalDeviceFormatProperties2 #-}

_VkGetPhysicalDeviceFormatProperties2 :: CString
_VkGetPhysicalDeviceFormatProperties2
  = Ptr "vkGetPhysicalDeviceFormatProperties2\NUL"#

{-# INLINE is_VkGetPhysicalDeviceFormatProperties2 #-}

is_VkGetPhysicalDeviceFormatProperties2 :: CString -> Bool
is_VkGetPhysicalDeviceFormatProperties2
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceFormatProperties2

type VkGetPhysicalDeviceFormatProperties2 =
     "vkGetPhysicalDeviceFormatProperties2"

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

-- | > () vkGetPhysicalDeviceFormatProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties2* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFormatProperties2.html vkGetPhysicalDeviceFormatProperties2 registry at www.khronos.org>
type HS_vkGetPhysicalDeviceFormatProperties2 =
     VkPhysicalDevice -- ^ physicalDevice
                      -> VkFormat -- ^ format
                                  -> Ptr VkFormatProperties2 -- ^ pFormatProperties
                                                             -> IO ()

type PFN_vkGetPhysicalDeviceFormatProperties2 =
     FunPtr HS_vkGetPhysicalDeviceFormatProperties2

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceFormatProperties2 ::
               PFN_vkGetPhysicalDeviceFormatProperties2 ->
                 HS_vkGetPhysicalDeviceFormatProperties2

instance VulkanProc "vkGetPhysicalDeviceFormatProperties2" where
        type VkProcType "vkGetPhysicalDeviceFormatProperties2" =
             HS_vkGetPhysicalDeviceFormatProperties2
        vkProcSymbol = _VkGetPhysicalDeviceFormatProperties2

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceFormatProperties2

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceImageFormatProperties2 :: CString

pattern VkGetPhysicalDeviceImageFormatProperties2 <-
        (is_VkGetPhysicalDeviceImageFormatProperties2 -> True)
  where VkGetPhysicalDeviceImageFormatProperties2
          = _VkGetPhysicalDeviceImageFormatProperties2

{-# INLINE _VkGetPhysicalDeviceImageFormatProperties2 #-}

_VkGetPhysicalDeviceImageFormatProperties2 :: CString
_VkGetPhysicalDeviceImageFormatProperties2
  = Ptr "vkGetPhysicalDeviceImageFormatProperties2\NUL"#

{-# INLINE is_VkGetPhysicalDeviceImageFormatProperties2 #-}

is_VkGetPhysicalDeviceImageFormatProperties2 :: CString -> Bool
is_VkGetPhysicalDeviceImageFormatProperties2
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceImageFormatProperties2

type VkGetPhysicalDeviceImageFormatProperties2 =
     "vkGetPhysicalDeviceImageFormatProperties2"

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
type HS_vkGetPhysicalDeviceImageFormatProperties2 =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkPhysicalDeviceImageFormatInfo2 -- ^ pImageFormatInfo
                                            ->
         Ptr VkImageFormatProperties2 -- ^ pImageFormatProperties
                                      -> IO VkResult

type PFN_vkGetPhysicalDeviceImageFormatProperties2 =
     FunPtr HS_vkGetPhysicalDeviceImageFormatProperties2

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceImageFormatProperties2 ::
               PFN_vkGetPhysicalDeviceImageFormatProperties2 ->
                 HS_vkGetPhysicalDeviceImageFormatProperties2

instance VulkanProc "vkGetPhysicalDeviceImageFormatProperties2"
         where
        type VkProcType "vkGetPhysicalDeviceImageFormatProperties2" =
             HS_vkGetPhysicalDeviceImageFormatProperties2
        vkProcSymbol = _VkGetPhysicalDeviceImageFormatProperties2

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceImageFormatProperties2

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceQueueFamilyProperties2 :: CString

pattern VkGetPhysicalDeviceQueueFamilyProperties2 <-
        (is_VkGetPhysicalDeviceQueueFamilyProperties2 -> True)
  where VkGetPhysicalDeviceQueueFamilyProperties2
          = _VkGetPhysicalDeviceQueueFamilyProperties2

{-# INLINE _VkGetPhysicalDeviceQueueFamilyProperties2 #-}

_VkGetPhysicalDeviceQueueFamilyProperties2 :: CString
_VkGetPhysicalDeviceQueueFamilyProperties2
  = Ptr "vkGetPhysicalDeviceQueueFamilyProperties2\NUL"#

{-# INLINE is_VkGetPhysicalDeviceQueueFamilyProperties2 #-}

is_VkGetPhysicalDeviceQueueFamilyProperties2 :: CString -> Bool
is_VkGetPhysicalDeviceQueueFamilyProperties2
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceQueueFamilyProperties2

type VkGetPhysicalDeviceQueueFamilyProperties2 =
     "vkGetPhysicalDeviceQueueFamilyProperties2"

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

-- | > () vkGetPhysicalDeviceQueueFamilyProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties2* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceQueueFamilyProperties2.html vkGetPhysicalDeviceQueueFamilyProperties2 registry at www.khronos.org>
type HS_vkGetPhysicalDeviceQueueFamilyProperties2 =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr Word32 -- ^ pQueueFamilyPropertyCount
                  -> Ptr VkQueueFamilyProperties2 -- ^ pQueueFamilyProperties
                                                  -> IO ()

type PFN_vkGetPhysicalDeviceQueueFamilyProperties2 =
     FunPtr HS_vkGetPhysicalDeviceQueueFamilyProperties2

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceQueueFamilyProperties2 ::
               PFN_vkGetPhysicalDeviceQueueFamilyProperties2 ->
                 HS_vkGetPhysicalDeviceQueueFamilyProperties2

instance VulkanProc "vkGetPhysicalDeviceQueueFamilyProperties2"
         where
        type VkProcType "vkGetPhysicalDeviceQueueFamilyProperties2" =
             HS_vkGetPhysicalDeviceQueueFamilyProperties2
        vkProcSymbol = _VkGetPhysicalDeviceQueueFamilyProperties2

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceQueueFamilyProperties2

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceMemoryProperties2 :: CString

pattern VkGetPhysicalDeviceMemoryProperties2 <-
        (is_VkGetPhysicalDeviceMemoryProperties2 -> True)
  where VkGetPhysicalDeviceMemoryProperties2
          = _VkGetPhysicalDeviceMemoryProperties2

{-# INLINE _VkGetPhysicalDeviceMemoryProperties2 #-}

_VkGetPhysicalDeviceMemoryProperties2 :: CString
_VkGetPhysicalDeviceMemoryProperties2
  = Ptr "vkGetPhysicalDeviceMemoryProperties2\NUL"#

{-# INLINE is_VkGetPhysicalDeviceMemoryProperties2 #-}

is_VkGetPhysicalDeviceMemoryProperties2 :: CString -> Bool
is_VkGetPhysicalDeviceMemoryProperties2
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceMemoryProperties2

type VkGetPhysicalDeviceMemoryProperties2 =
     "vkGetPhysicalDeviceMemoryProperties2"

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

-- | > () vkGetPhysicalDeviceMemoryProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties2* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceMemoryProperties2.html vkGetPhysicalDeviceMemoryProperties2 registry at www.khronos.org>
type HS_vkGetPhysicalDeviceMemoryProperties2 =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Ptr VkPhysicalDeviceMemoryProperties2 -- ^ pMemoryProperties
                                                               -> IO ()

type PFN_vkGetPhysicalDeviceMemoryProperties2 =
     FunPtr HS_vkGetPhysicalDeviceMemoryProperties2

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceMemoryProperties2 ::
               PFN_vkGetPhysicalDeviceMemoryProperties2 ->
                 HS_vkGetPhysicalDeviceMemoryProperties2

instance VulkanProc "vkGetPhysicalDeviceMemoryProperties2" where
        type VkProcType "vkGetPhysicalDeviceMemoryProperties2" =
             HS_vkGetPhysicalDeviceMemoryProperties2
        vkProcSymbol = _VkGetPhysicalDeviceMemoryProperties2

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceMemoryProperties2

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceSparseImageFormatProperties2 :: CString

pattern VkGetPhysicalDeviceSparseImageFormatProperties2 <-
        (is_VkGetPhysicalDeviceSparseImageFormatProperties2 -> True)
  where VkGetPhysicalDeviceSparseImageFormatProperties2
          = _VkGetPhysicalDeviceSparseImageFormatProperties2

{-# INLINE _VkGetPhysicalDeviceSparseImageFormatProperties2 #-}

_VkGetPhysicalDeviceSparseImageFormatProperties2 :: CString
_VkGetPhysicalDeviceSparseImageFormatProperties2
  = Ptr "vkGetPhysicalDeviceSparseImageFormatProperties2\NUL"#

{-# INLINE is_VkGetPhysicalDeviceSparseImageFormatProperties2 #-}

is_VkGetPhysicalDeviceSparseImageFormatProperties2 ::
                                                   CString -> Bool
is_VkGetPhysicalDeviceSparseImageFormatProperties2
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceSparseImageFormatProperties2

type VkGetPhysicalDeviceSparseImageFormatProperties2 =
     "vkGetPhysicalDeviceSparseImageFormatProperties2"

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

-- | > () vkGetPhysicalDeviceSparseImageFormatProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSparseImageFormatInfo2* pFormatInfo
--   >     , uint32_t* pPropertyCount
--   >     , VkSparseImageFormatProperties2* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceSparseImageFormatProperties2.html vkGetPhysicalDeviceSparseImageFormatProperties2 registry at www.khronos.org>
type HS_vkGetPhysicalDeviceSparseImageFormatProperties2 =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkPhysicalDeviceSparseImageFormatInfo2 -- ^ pFormatInfo
                                                  ->
         Ptr Word32 -- ^ pPropertyCount
                    -> Ptr VkSparseImageFormatProperties2 -- ^ pProperties
                                                          -> IO ()

type PFN_vkGetPhysicalDeviceSparseImageFormatProperties2 =
     FunPtr HS_vkGetPhysicalDeviceSparseImageFormatProperties2

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceSparseImageFormatProperties2 ::
               PFN_vkGetPhysicalDeviceSparseImageFormatProperties2 ->
                 HS_vkGetPhysicalDeviceSparseImageFormatProperties2

instance VulkanProc
           "vkGetPhysicalDeviceSparseImageFormatProperties2"
         where
        type VkProcType "vkGetPhysicalDeviceSparseImageFormatProperties2" =
             HS_vkGetPhysicalDeviceSparseImageFormatProperties2
        vkProcSymbol = _VkGetPhysicalDeviceSparseImageFormatProperties2

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr
          = unwrapVkGetPhysicalDeviceSparseImageFormatProperties2

        {-# INLINE unwrapVkProcPtr #-}

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

pattern VkTrimCommandPool :: CString

pattern VkTrimCommandPool <- (is_VkTrimCommandPool -> True)
  where VkTrimCommandPool = _VkTrimCommandPool

{-# INLINE _VkTrimCommandPool #-}

_VkTrimCommandPool :: CString
_VkTrimCommandPool = Ptr "vkTrimCommandPool\NUL"#

{-# INLINE is_VkTrimCommandPool #-}

is_VkTrimCommandPool :: CString -> Bool
is_VkTrimCommandPool = (EQ ==) . cmpCStrings _VkTrimCommandPool

type VkTrimCommandPool = "vkTrimCommandPool"

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

-- | > () vkTrimCommandPool
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , VkCommandPoolTrimFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkTrimCommandPool.html vkTrimCommandPool registry at www.khronos.org>
type HS_vkTrimCommandPool =
     VkDevice -- ^ device
              -> VkCommandPool -- ^ commandPool
                               -> VkCommandPoolTrimFlags -- ^ flags
                                                         -> IO ()

type PFN_vkTrimCommandPool = FunPtr HS_vkTrimCommandPool

foreign import ccall "dynamic" unwrapVkTrimCommandPool ::
               PFN_vkTrimCommandPool -> HS_vkTrimCommandPool

instance VulkanProc "vkTrimCommandPool" where
        type VkProcType "vkTrimCommandPool" = HS_vkTrimCommandPool
        vkProcSymbol = _VkTrimCommandPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkTrimCommandPool

        {-# INLINE unwrapVkProcPtr #-}

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

pattern VkGetDeviceQueue2 :: CString

pattern VkGetDeviceQueue2 <- (is_VkGetDeviceQueue2 -> True)
  where VkGetDeviceQueue2 = _VkGetDeviceQueue2

{-# INLINE _VkGetDeviceQueue2 #-}

_VkGetDeviceQueue2 :: CString
_VkGetDeviceQueue2 = Ptr "vkGetDeviceQueue2\NUL"#

{-# INLINE is_VkGetDeviceQueue2 #-}

is_VkGetDeviceQueue2 :: CString -> Bool
is_VkGetDeviceQueue2 = (EQ ==) . cmpCStrings _VkGetDeviceQueue2

type VkGetDeviceQueue2 = "vkGetDeviceQueue2"

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

-- | > () vkGetDeviceQueue2
--   >     ( VkDevice device
--   >     , const VkDeviceQueueInfo2* pQueueInfo
--   >     , VkQueue* pQueue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceQueue2.html vkGetDeviceQueue2 registry at www.khronos.org>
type HS_vkGetDeviceQueue2 =
     VkDevice -- ^ device
              -> Ptr VkDeviceQueueInfo2 -- ^ pQueueInfo
                                        -> Ptr VkQueue -- ^ pQueue
                                                       -> IO ()

type PFN_vkGetDeviceQueue2 = FunPtr HS_vkGetDeviceQueue2

foreign import ccall "dynamic" unwrapVkGetDeviceQueue2 ::
               PFN_vkGetDeviceQueue2 -> HS_vkGetDeviceQueue2

instance VulkanProc "vkGetDeviceQueue2" where
        type VkProcType "vkGetDeviceQueue2" = HS_vkGetDeviceQueue2
        vkProcSymbol = _VkGetDeviceQueue2

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetDeviceQueue2

        {-# INLINE unwrapVkProcPtr #-}

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

pattern VkCreateSamplerYcbcrConversion :: CString

pattern VkCreateSamplerYcbcrConversion <-
        (is_VkCreateSamplerYcbcrConversion -> True)
  where VkCreateSamplerYcbcrConversion
          = _VkCreateSamplerYcbcrConversion

{-# INLINE _VkCreateSamplerYcbcrConversion #-}

_VkCreateSamplerYcbcrConversion :: CString
_VkCreateSamplerYcbcrConversion
  = Ptr "vkCreateSamplerYcbcrConversion\NUL"#

{-# INLINE is_VkCreateSamplerYcbcrConversion #-}

is_VkCreateSamplerYcbcrConversion :: CString -> Bool
is_VkCreateSamplerYcbcrConversion
  = (EQ ==) . cmpCStrings _VkCreateSamplerYcbcrConversion

type VkCreateSamplerYcbcrConversion =
     "vkCreateSamplerYcbcrConversion"

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
type HS_vkCreateSamplerYcbcrConversion =
     VkDevice -- ^ device
              ->
       Ptr VkSamplerYcbcrConversionCreateInfo -- ^ pCreateInfo
                                              ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   ->
           Ptr VkSamplerYcbcrConversion -- ^ pYcbcrConversion
                                        -> IO VkResult

type PFN_vkCreateSamplerYcbcrConversion =
     FunPtr HS_vkCreateSamplerYcbcrConversion

foreign import ccall "dynamic" unwrapVkCreateSamplerYcbcrConversion
               ::
               PFN_vkCreateSamplerYcbcrConversion ->
                 HS_vkCreateSamplerYcbcrConversion

instance VulkanProc "vkCreateSamplerYcbcrConversion" where
        type VkProcType "vkCreateSamplerYcbcrConversion" =
             HS_vkCreateSamplerYcbcrConversion
        vkProcSymbol = _VkCreateSamplerYcbcrConversion

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateSamplerYcbcrConversion

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroySamplerYcbcrConversion :: CString

pattern VkDestroySamplerYcbcrConversion <-
        (is_VkDestroySamplerYcbcrConversion -> True)
  where VkDestroySamplerYcbcrConversion
          = _VkDestroySamplerYcbcrConversion

{-# INLINE _VkDestroySamplerYcbcrConversion #-}

_VkDestroySamplerYcbcrConversion :: CString
_VkDestroySamplerYcbcrConversion
  = Ptr "vkDestroySamplerYcbcrConversion\NUL"#

{-# INLINE is_VkDestroySamplerYcbcrConversion #-}

is_VkDestroySamplerYcbcrConversion :: CString -> Bool
is_VkDestroySamplerYcbcrConversion
  = (EQ ==) . cmpCStrings _VkDestroySamplerYcbcrConversion

type VkDestroySamplerYcbcrConversion =
     "vkDestroySamplerYcbcrConversion"

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

-- | > () vkDestroySamplerYcbcrConversion
--   >     ( VkDevice device
--   >     , VkSamplerYcbcrConversion ycbcrConversion
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroySamplerYcbcrConversion.html vkDestroySamplerYcbcrConversion registry at www.khronos.org>
type HS_vkDestroySamplerYcbcrConversion =
     VkDevice -- ^ device
              ->
       VkSamplerYcbcrConversion -- ^ ycbcrConversion
                                -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                             -> IO ()

type PFN_vkDestroySamplerYcbcrConversion =
     FunPtr HS_vkDestroySamplerYcbcrConversion

foreign import ccall "dynamic"
               unwrapVkDestroySamplerYcbcrConversion ::
               PFN_vkDestroySamplerYcbcrConversion ->
                 HS_vkDestroySamplerYcbcrConversion

instance VulkanProc "vkDestroySamplerYcbcrConversion" where
        type VkProcType "vkDestroySamplerYcbcrConversion" =
             HS_vkDestroySamplerYcbcrConversion
        vkProcSymbol = _VkDestroySamplerYcbcrConversion

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroySamplerYcbcrConversion

        {-# INLINE unwrapVkProcPtr #-}

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

pattern VkCreateDescriptorUpdateTemplate :: CString

pattern VkCreateDescriptorUpdateTemplate <-
        (is_VkCreateDescriptorUpdateTemplate -> True)
  where VkCreateDescriptorUpdateTemplate
          = _VkCreateDescriptorUpdateTemplate

{-# INLINE _VkCreateDescriptorUpdateTemplate #-}

_VkCreateDescriptorUpdateTemplate :: CString
_VkCreateDescriptorUpdateTemplate
  = Ptr "vkCreateDescriptorUpdateTemplate\NUL"#

{-# INLINE is_VkCreateDescriptorUpdateTemplate #-}

is_VkCreateDescriptorUpdateTemplate :: CString -> Bool
is_VkCreateDescriptorUpdateTemplate
  = (EQ ==) . cmpCStrings _VkCreateDescriptorUpdateTemplate

type VkCreateDescriptorUpdateTemplate =
     "vkCreateDescriptorUpdateTemplate"

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
type HS_vkCreateDescriptorUpdateTemplate =
     VkDevice -- ^ device
              ->
       Ptr VkDescriptorUpdateTemplateCreateInfo -- ^ pCreateInfo
                                                ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   ->
           Ptr VkDescriptorUpdateTemplate -- ^ pDescriptorUpdateTemplate
                                          -> IO VkResult

type PFN_vkCreateDescriptorUpdateTemplate =
     FunPtr HS_vkCreateDescriptorUpdateTemplate

foreign import ccall "dynamic"
               unwrapVkCreateDescriptorUpdateTemplate ::
               PFN_vkCreateDescriptorUpdateTemplate ->
                 HS_vkCreateDescriptorUpdateTemplate

instance VulkanProc "vkCreateDescriptorUpdateTemplate" where
        type VkProcType "vkCreateDescriptorUpdateTemplate" =
             HS_vkCreateDescriptorUpdateTemplate
        vkProcSymbol = _VkCreateDescriptorUpdateTemplate

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateDescriptorUpdateTemplate

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyDescriptorUpdateTemplate :: CString

pattern VkDestroyDescriptorUpdateTemplate <-
        (is_VkDestroyDescriptorUpdateTemplate -> True)
  where VkDestroyDescriptorUpdateTemplate
          = _VkDestroyDescriptorUpdateTemplate

{-# INLINE _VkDestroyDescriptorUpdateTemplate #-}

_VkDestroyDescriptorUpdateTemplate :: CString
_VkDestroyDescriptorUpdateTemplate
  = Ptr "vkDestroyDescriptorUpdateTemplate\NUL"#

{-# INLINE is_VkDestroyDescriptorUpdateTemplate #-}

is_VkDestroyDescriptorUpdateTemplate :: CString -> Bool
is_VkDestroyDescriptorUpdateTemplate
  = (EQ ==) . cmpCStrings _VkDestroyDescriptorUpdateTemplate

type VkDestroyDescriptorUpdateTemplate =
     "vkDestroyDescriptorUpdateTemplate"

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

-- | > () vkDestroyDescriptorUpdateTemplate
--   >     ( VkDevice device
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDescriptorUpdateTemplate.html vkDestroyDescriptorUpdateTemplate registry at www.khronos.org>
type HS_vkDestroyDescriptorUpdateTemplate =
     VkDevice -- ^ device
              ->
       VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                  -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                               -> IO ()

type PFN_vkDestroyDescriptorUpdateTemplate =
     FunPtr HS_vkDestroyDescriptorUpdateTemplate

foreign import ccall "dynamic"
               unwrapVkDestroyDescriptorUpdateTemplate ::
               PFN_vkDestroyDescriptorUpdateTemplate ->
                 HS_vkDestroyDescriptorUpdateTemplate

instance VulkanProc "vkDestroyDescriptorUpdateTemplate" where
        type VkProcType "vkDestroyDescriptorUpdateTemplate" =
             HS_vkDestroyDescriptorUpdateTemplate
        vkProcSymbol = _VkDestroyDescriptorUpdateTemplate

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyDescriptorUpdateTemplate

        {-# INLINE unwrapVkProcPtr #-}

pattern VkUpdateDescriptorSetWithTemplate :: CString

pattern VkUpdateDescriptorSetWithTemplate <-
        (is_VkUpdateDescriptorSetWithTemplate -> True)
  where VkUpdateDescriptorSetWithTemplate
          = _VkUpdateDescriptorSetWithTemplate

{-# INLINE _VkUpdateDescriptorSetWithTemplate #-}

_VkUpdateDescriptorSetWithTemplate :: CString
_VkUpdateDescriptorSetWithTemplate
  = Ptr "vkUpdateDescriptorSetWithTemplate\NUL"#

{-# INLINE is_VkUpdateDescriptorSetWithTemplate #-}

is_VkUpdateDescriptorSetWithTemplate :: CString -> Bool
is_VkUpdateDescriptorSetWithTemplate
  = (EQ ==) . cmpCStrings _VkUpdateDescriptorSetWithTemplate

type VkUpdateDescriptorSetWithTemplate =
     "vkUpdateDescriptorSetWithTemplate"

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

-- | > () vkUpdateDescriptorSetWithTemplate
--   >     ( VkDevice device
--   >     , VkDescriptorSet descriptorSet
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkUpdateDescriptorSetWithTemplate.html vkUpdateDescriptorSetWithTemplate registry at www.khronos.org>
type HS_vkUpdateDescriptorSetWithTemplate =
     VkDevice -- ^ device
              ->
       VkDescriptorSet -- ^ descriptorSet
                       -> VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                     -> Ptr Void -- ^ pData
                                                                 -> IO ()

type PFN_vkUpdateDescriptorSetWithTemplate =
     FunPtr HS_vkUpdateDescriptorSetWithTemplate

foreign import ccall "dynamic"
               unwrapVkUpdateDescriptorSetWithTemplate ::
               PFN_vkUpdateDescriptorSetWithTemplate ->
                 HS_vkUpdateDescriptorSetWithTemplate

instance VulkanProc "vkUpdateDescriptorSetWithTemplate" where
        type VkProcType "vkUpdateDescriptorSetWithTemplate" =
             HS_vkUpdateDescriptorSetWithTemplate
        vkProcSymbol = _VkUpdateDescriptorSetWithTemplate

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkUpdateDescriptorSetWithTemplate

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO =
        VkStructureType 1000085000

pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE :: VkObjectType

pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE =
        VkObjectType 1000085000

pattern VkGetPhysicalDeviceExternalBufferProperties :: CString

pattern VkGetPhysicalDeviceExternalBufferProperties <-
        (is_VkGetPhysicalDeviceExternalBufferProperties -> True)
  where VkGetPhysicalDeviceExternalBufferProperties
          = _VkGetPhysicalDeviceExternalBufferProperties

{-# INLINE _VkGetPhysicalDeviceExternalBufferProperties #-}

_VkGetPhysicalDeviceExternalBufferProperties :: CString
_VkGetPhysicalDeviceExternalBufferProperties
  = Ptr "vkGetPhysicalDeviceExternalBufferProperties\NUL"#

{-# INLINE is_VkGetPhysicalDeviceExternalBufferProperties #-}

is_VkGetPhysicalDeviceExternalBufferProperties :: CString -> Bool
is_VkGetPhysicalDeviceExternalBufferProperties
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceExternalBufferProperties

type VkGetPhysicalDeviceExternalBufferProperties =
     "vkGetPhysicalDeviceExternalBufferProperties"

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

-- | > () vkGetPhysicalDeviceExternalBufferProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalBufferInfo* pExternalBufferInfo
--   >     , VkExternalBufferProperties* pExternalBufferProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceExternalBufferProperties.html vkGetPhysicalDeviceExternalBufferProperties registry at www.khronos.org>
type HS_vkGetPhysicalDeviceExternalBufferProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkPhysicalDeviceExternalBufferInfo -- ^ pExternalBufferInfo
                                              ->
         Ptr VkExternalBufferProperties -- ^ pExternalBufferProperties
                                        -> IO ()

type PFN_vkGetPhysicalDeviceExternalBufferProperties =
     FunPtr HS_vkGetPhysicalDeviceExternalBufferProperties

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceExternalBufferProperties ::
               PFN_vkGetPhysicalDeviceExternalBufferProperties ->
                 HS_vkGetPhysicalDeviceExternalBufferProperties

instance VulkanProc "vkGetPhysicalDeviceExternalBufferProperties"
         where
        type VkProcType "vkGetPhysicalDeviceExternalBufferProperties" =
             HS_vkGetPhysicalDeviceExternalBufferProperties
        vkProcSymbol = _VkGetPhysicalDeviceExternalBufferProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceExternalBufferProperties

        {-# INLINE unwrapVkProcPtr #-}

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

pattern VkGetPhysicalDeviceExternalFenceProperties :: CString

pattern VkGetPhysicalDeviceExternalFenceProperties <-
        (is_VkGetPhysicalDeviceExternalFenceProperties -> True)
  where VkGetPhysicalDeviceExternalFenceProperties
          = _VkGetPhysicalDeviceExternalFenceProperties

{-# INLINE _VkGetPhysicalDeviceExternalFenceProperties #-}

_VkGetPhysicalDeviceExternalFenceProperties :: CString
_VkGetPhysicalDeviceExternalFenceProperties
  = Ptr "vkGetPhysicalDeviceExternalFenceProperties\NUL"#

{-# INLINE is_VkGetPhysicalDeviceExternalFenceProperties #-}

is_VkGetPhysicalDeviceExternalFenceProperties :: CString -> Bool
is_VkGetPhysicalDeviceExternalFenceProperties
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceExternalFenceProperties

type VkGetPhysicalDeviceExternalFenceProperties =
     "vkGetPhysicalDeviceExternalFenceProperties"

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

-- | > () vkGetPhysicalDeviceExternalFenceProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalFenceInfo* pExternalFenceInfo
--   >     , VkExternalFenceProperties* pExternalFenceProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceExternalFenceProperties.html vkGetPhysicalDeviceExternalFenceProperties registry at www.khronos.org>
type HS_vkGetPhysicalDeviceExternalFenceProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkPhysicalDeviceExternalFenceInfo -- ^ pExternalFenceInfo
                                             ->
         Ptr VkExternalFenceProperties -- ^ pExternalFenceProperties
                                       -> IO ()

type PFN_vkGetPhysicalDeviceExternalFenceProperties =
     FunPtr HS_vkGetPhysicalDeviceExternalFenceProperties

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceExternalFenceProperties ::
               PFN_vkGetPhysicalDeviceExternalFenceProperties ->
                 HS_vkGetPhysicalDeviceExternalFenceProperties

instance VulkanProc "vkGetPhysicalDeviceExternalFenceProperties"
         where
        type VkProcType "vkGetPhysicalDeviceExternalFenceProperties" =
             HS_vkGetPhysicalDeviceExternalFenceProperties
        vkProcSymbol = _VkGetPhysicalDeviceExternalFenceProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceExternalFenceProperties

        {-# INLINE unwrapVkProcPtr #-}

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

pattern VkGetPhysicalDeviceExternalSemaphoreProperties :: CString

pattern VkGetPhysicalDeviceExternalSemaphoreProperties <-
        (is_VkGetPhysicalDeviceExternalSemaphoreProperties -> True)
  where VkGetPhysicalDeviceExternalSemaphoreProperties
          = _VkGetPhysicalDeviceExternalSemaphoreProperties

{-# INLINE _VkGetPhysicalDeviceExternalSemaphoreProperties #-}

_VkGetPhysicalDeviceExternalSemaphoreProperties :: CString
_VkGetPhysicalDeviceExternalSemaphoreProperties
  = Ptr "vkGetPhysicalDeviceExternalSemaphoreProperties\NUL"#

{-# INLINE is_VkGetPhysicalDeviceExternalSemaphoreProperties #-}

is_VkGetPhysicalDeviceExternalSemaphoreProperties ::
                                                  CString -> Bool
is_VkGetPhysicalDeviceExternalSemaphoreProperties
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceExternalSemaphoreProperties

type VkGetPhysicalDeviceExternalSemaphoreProperties =
     "vkGetPhysicalDeviceExternalSemaphoreProperties"

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

-- | > () vkGetPhysicalDeviceExternalSemaphoreProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalSemaphoreInfo* pExternalSemaphoreInfo
--   >     , VkExternalSemaphoreProperties* pExternalSemaphoreProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceExternalSemaphoreProperties.html vkGetPhysicalDeviceExternalSemaphoreProperties registry at www.khronos.org>
type HS_vkGetPhysicalDeviceExternalSemaphoreProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkPhysicalDeviceExternalSemaphoreInfo -- ^ pExternalSemaphoreInfo
                                                 ->
         Ptr VkExternalSemaphoreProperties -- ^ pExternalSemaphoreProperties
                                           -> IO ()

type PFN_vkGetPhysicalDeviceExternalSemaphoreProperties =
     FunPtr HS_vkGetPhysicalDeviceExternalSemaphoreProperties

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceExternalSemaphoreProperties ::
               PFN_vkGetPhysicalDeviceExternalSemaphoreProperties ->
                 HS_vkGetPhysicalDeviceExternalSemaphoreProperties

instance VulkanProc
           "vkGetPhysicalDeviceExternalSemaphoreProperties"
         where
        type VkProcType "vkGetPhysicalDeviceExternalSemaphoreProperties" =
             HS_vkGetPhysicalDeviceExternalSemaphoreProperties
        vkProcSymbol = _VkGetPhysicalDeviceExternalSemaphoreProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr
          = unwrapVkGetPhysicalDeviceExternalSemaphoreProperties

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO =
        VkStructureType 1000076000

pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES =
        VkStructureType 1000076001

pattern VkGetDescriptorSetLayoutSupport :: CString

pattern VkGetDescriptorSetLayoutSupport <-
        (is_VkGetDescriptorSetLayoutSupport -> True)
  where VkGetDescriptorSetLayoutSupport
          = _VkGetDescriptorSetLayoutSupport

{-# INLINE _VkGetDescriptorSetLayoutSupport #-}

_VkGetDescriptorSetLayoutSupport :: CString
_VkGetDescriptorSetLayoutSupport
  = Ptr "vkGetDescriptorSetLayoutSupport\NUL"#

{-# INLINE is_VkGetDescriptorSetLayoutSupport #-}

is_VkGetDescriptorSetLayoutSupport :: CString -> Bool
is_VkGetDescriptorSetLayoutSupport
  = (EQ ==) . cmpCStrings _VkGetDescriptorSetLayoutSupport

type VkGetDescriptorSetLayoutSupport =
     "vkGetDescriptorSetLayoutSupport"

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

-- | > () vkGetDescriptorSetLayoutSupport
--   >     ( VkDevice device
--   >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
--   >     , VkDescriptorSetLayoutSupport* pSupport
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDescriptorSetLayoutSupport.html vkGetDescriptorSetLayoutSupport registry at www.khronos.org>
type HS_vkGetDescriptorSetLayoutSupport =
     VkDevice -- ^ device
              ->
       Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                           ->
         Ptr VkDescriptorSetLayoutSupport -- ^ pSupport
                                          -> IO ()

type PFN_vkGetDescriptorSetLayoutSupport =
     FunPtr HS_vkGetDescriptorSetLayoutSupport

foreign import ccall "dynamic"
               unwrapVkGetDescriptorSetLayoutSupport ::
               PFN_vkGetDescriptorSetLayoutSupport ->
                 HS_vkGetDescriptorSetLayoutSupport

instance VulkanProc "vkGetDescriptorSetLayoutSupport" where
        type VkProcType "vkGetDescriptorSetLayoutSupport" =
             HS_vkGetDescriptorSetLayoutSupport
        vkProcSymbol = _VkGetDescriptorSetLayoutSupport

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetDescriptorSetLayoutSupport

        {-# INLINE unwrapVkProcPtr #-}

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
