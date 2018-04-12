{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeApplications         #-}
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
        vkEnumerateInstanceVersion, vkEnumerateInstanceVersionSafe,
        module Graphics.Vulkan.Types.Enum.Result,
        -- ** Promoted from VK_KHR_relaxed_block_layout, which has no API
        --

        -- ** Promoted from VK_KHR_storage_buffer_storage_class, which has no API
        --

        -- ** Originally based on VK_KHR_subgroup (extension 94), but the actual enum block used was, incorrectly, that of extension 95
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.PhysicalDevice,
        module Graphics.Vulkan.Types.Enum.PhysicalDeviceType,
        module Graphics.Vulkan.Types.Enum.SampleCountFlags,
        module Graphics.Vulkan.Types.Enum.Shader,
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Enum.SubgroupFeatureFlags,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES,
        -- ** Promoted from VK_KHR_bind_memory2
        module Graphics.Vulkan.Types.Struct.Bind, -- > #include "vk_platform.h"
                                                  VkBindBufferMemory2,
        pattern VkBindBufferMemory2, HS_vkBindBufferMemory2,
        PFN_vkBindBufferMemory2, vkBindBufferMemory2,
        vkBindBufferMemory2Safe, VkBindImageMemory2,
        pattern VkBindImageMemory2, HS_vkBindImageMemory2,
        PFN_vkBindImageMemory2, vkBindImageMemory2, vkBindImageMemory2Safe,
        module Graphics.Vulkan.Types.Handles,
        pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO,
        pattern VK_IMAGE_CREATE_ALIAS_BIT,
        -- ** Promoted from VK_KHR_16bit_storage
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.Device,
        module Graphics.Vulkan.Types.Enum.Device,
        module Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES,
        -- ** Promoted from VK_KHR_dedicated_allocation
        module Graphics.Vulkan.Types.Struct.Memory,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS,
        pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO,
        -- ** Promoted from VK_KHR_device_group
        module Graphics.Vulkan.Types.Struct.Clear,
        module Graphics.Vulkan.Types.Struct.Command,
        module Graphics.Vulkan.Types.Enum.Command,
        module Graphics.Vulkan.Types.Struct.Extent,
        module Graphics.Vulkan.Types.Enum.Image,
        module Graphics.Vulkan.Types.Struct.Image,
        module Graphics.Vulkan.Types.Enum.Memory,
        module Graphics.Vulkan.Types.Struct.Offset,
        module Graphics.Vulkan.Types.Enum.PeerMemoryFeatureFlag,
        module Graphics.Vulkan.Types.Enum.Pipeline,
        module Graphics.Vulkan.Types.Enum.Query,
        module Graphics.Vulkan.Types.Struct.Rect,
        module Graphics.Vulkan.Types.Struct.RenderPass,
        module Graphics.Vulkan.Types.Struct.Sparse,
        module Graphics.Vulkan.Types.Enum.Sparse,
        module Graphics.Vulkan.Types.Struct.SubmitInfo,
        -- > #include "vk_platform.h"
        VkGetDeviceGroupPeerMemoryFeatures,
        pattern VkGetDeviceGroupPeerMemoryFeatures,
        HS_vkGetDeviceGroupPeerMemoryFeatures,
        PFN_vkGetDeviceGroupPeerMemoryFeatures,
        vkGetDeviceGroupPeerMemoryFeatures,
        vkGetDeviceGroupPeerMemoryFeaturesSafe, VkCmdSetDeviceMask,
        pattern VkCmdSetDeviceMask, HS_vkCmdSetDeviceMask,
        PFN_vkCmdSetDeviceMask, vkCmdSetDeviceMask, vkCmdSetDeviceMaskSafe,
        VkCmdDispatchBase, pattern VkCmdDispatchBase, HS_vkCmdDispatchBase,
        PFN_vkCmdDispatchBase, vkCmdDispatchBase, vkCmdDispatchBaseSafe,
        pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO,
        pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT,
        pattern VK_PIPELINE_CREATE_DISPATCH_BASE,
        pattern VK_DEPENDENCY_DEVICE_GROUP_BIT,
        -- ** Promoted from VK_KHR_device_group + VK_KHR_bind_memory2
        --
        -- |
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO,
        pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT,
        -- ** Promoted from VK_KHR_device_group_creation
        --
        -- |
        -- > #include "vk_platform.h"
        VkEnumeratePhysicalDeviceGroups,
        pattern VkEnumeratePhysicalDeviceGroups,
        HS_vkEnumeratePhysicalDeviceGroups,
        PFN_vkEnumeratePhysicalDeviceGroups,
        vkEnumeratePhysicalDeviceGroups,
        vkEnumeratePhysicalDeviceGroupsSafe,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO,
        pattern VK_MAX_DEVICE_GROUP_SIZE,
        pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT,
        -- ** Promoted from VK_KHR_get_memory_requirements2
        module Graphics.Vulkan.Types.Struct.Buffer,
        -- > #include "vk_platform.h"
        VkGetImageMemoryRequirements2,
        pattern VkGetImageMemoryRequirements2,
        HS_vkGetImageMemoryRequirements2,
        PFN_vkGetImageMemoryRequirements2, vkGetImageMemoryRequirements2,
        vkGetImageMemoryRequirements2Safe, VkGetBufferMemoryRequirements2,
        pattern VkGetBufferMemoryRequirements2,
        HS_vkGetBufferMemoryRequirements2,
        PFN_vkGetBufferMemoryRequirements2, vkGetBufferMemoryRequirements2,
        vkGetBufferMemoryRequirements2Safe,
        VkGetImageSparseMemoryRequirements2,
        pattern VkGetImageSparseMemoryRequirements2,
        HS_vkGetImageSparseMemoryRequirements2,
        PFN_vkGetImageSparseMemoryRequirements2,
        vkGetImageSparseMemoryRequirements2,
        vkGetImageSparseMemoryRequirements2Safe,
        pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2,
        pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2,
        pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2,
        pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2,
        pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2,
        -- ** Promoted from VK_KHR_get_physical_device_properties2
        module Graphics.Vulkan.Types.Enum.Format,
        module Graphics.Vulkan.Types.Struct.FormatProperties,
        module Graphics.Vulkan.Types.Struct.QueueFamilyProperties,
        module Graphics.Vulkan.Types.Enum.Queue,
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceFeatures2, pattern VkGetPhysicalDeviceFeatures2,
        HS_vkGetPhysicalDeviceFeatures2, PFN_vkGetPhysicalDeviceFeatures2,
        vkGetPhysicalDeviceFeatures2, vkGetPhysicalDeviceFeatures2Safe,
        VkGetPhysicalDeviceProperties2,
        pattern VkGetPhysicalDeviceProperties2,
        HS_vkGetPhysicalDeviceProperties2,
        PFN_vkGetPhysicalDeviceProperties2, vkGetPhysicalDeviceProperties2,
        vkGetPhysicalDeviceProperties2Safe,
        VkGetPhysicalDeviceFormatProperties2,
        pattern VkGetPhysicalDeviceFormatProperties2,
        HS_vkGetPhysicalDeviceFormatProperties2,
        PFN_vkGetPhysicalDeviceFormatProperties2,
        vkGetPhysicalDeviceFormatProperties2,
        vkGetPhysicalDeviceFormatProperties2Safe,
        VkGetPhysicalDeviceImageFormatProperties2,
        pattern VkGetPhysicalDeviceImageFormatProperties2,
        HS_vkGetPhysicalDeviceImageFormatProperties2,
        PFN_vkGetPhysicalDeviceImageFormatProperties2,
        vkGetPhysicalDeviceImageFormatProperties2,
        vkGetPhysicalDeviceImageFormatProperties2Safe,
        VkGetPhysicalDeviceQueueFamilyProperties2,
        pattern VkGetPhysicalDeviceQueueFamilyProperties2,
        HS_vkGetPhysicalDeviceQueueFamilyProperties2,
        PFN_vkGetPhysicalDeviceQueueFamilyProperties2,
        vkGetPhysicalDeviceQueueFamilyProperties2,
        vkGetPhysicalDeviceQueueFamilyProperties2Safe,
        VkGetPhysicalDeviceMemoryProperties2,
        pattern VkGetPhysicalDeviceMemoryProperties2,
        HS_vkGetPhysicalDeviceMemoryProperties2,
        PFN_vkGetPhysicalDeviceMemoryProperties2,
        vkGetPhysicalDeviceMemoryProperties2,
        vkGetPhysicalDeviceMemoryProperties2Safe,
        VkGetPhysicalDeviceSparseImageFormatProperties2,
        pattern VkGetPhysicalDeviceSparseImageFormatProperties2,
        HS_vkGetPhysicalDeviceSparseImageFormatProperties2,
        PFN_vkGetPhysicalDeviceSparseImageFormatProperties2,
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
        PFN_vkTrimCommandPool, vkTrimCommandPool, vkTrimCommandPoolSafe,
        pattern VK_ERROR_OUT_OF_POOL_MEMORY,
        pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT,
        pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT,
        pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT,
        -- ** Promoted from VK_KHR_maintenance2
        module Graphics.Vulkan.Types.Enum.AccessFlags,
        module Graphics.Vulkan.Types.Struct.Attachment,
        module Graphics.Vulkan.Types.Enum.Attachment,
        module Graphics.Vulkan.Types.Struct.ComponentMapping,
        module Graphics.Vulkan.Types.Enum.ComponentSwizzle,
        module Graphics.Vulkan.Types.Enum.DependencyFlags,
        module Graphics.Vulkan.Types.Struct.InputAttachmentAspectReference,
        module Graphics.Vulkan.Types.Struct.Pipeline,
        module Graphics.Vulkan.Types.Enum.PointClippingBehavior,
        module Graphics.Vulkan.Types.Struct.Subpass,
        module Graphics.Vulkan.Types.Enum.Subpass,
        module Graphics.Vulkan.Types.Enum.TessellationDomainOrigin,
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
        --
        -- |
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES,
        pattern VK_DEPENDENCY_VIEW_LOCAL_BIT,
        -- ** Promoted from VK_KHR_variable_pointers
        --
        -- |
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES,
        -- ** Originally based on VK_KHR_protected_memory (extension 146), which was never published; thus the mystifying large value= numbers below. These are not aliased since they weren't actually promoted from an extension.
        module Graphics.Vulkan.Types.Struct.ProtectedSubmitInfo,
        -- > #include "vk_platform.h"
        VkGetDeviceQueue2, pattern VkGetDeviceQueue2, HS_vkGetDeviceQueue2,
        PFN_vkGetDeviceQueue2, vkGetDeviceQueue2, vkGetDeviceQueue2Safe,
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
        module Graphics.Vulkan.Types.Enum.BorderColor,
        module Graphics.Vulkan.Types.Enum.ChromaLocation,
        module Graphics.Vulkan.Types.Enum.CompareOp,
        module Graphics.Vulkan.Types.Enum.Filter,
        module Graphics.Vulkan.Types.Enum.Sampler,
        module Graphics.Vulkan.Types.Struct.Sampler,
        -- > #include "vk_platform.h"
        VkCreateSamplerYcbcrConversion,
        pattern VkCreateSamplerYcbcrConversion,
        HS_vkCreateSamplerYcbcrConversion,
        PFN_vkCreateSamplerYcbcrConversion, vkCreateSamplerYcbcrConversion,
        vkCreateSamplerYcbcrConversionSafe,
        VkDestroySamplerYcbcrConversion,
        pattern VkDestroySamplerYcbcrConversion,
        HS_vkDestroySamplerYcbcrConversion,
        PFN_vkDestroySamplerYcbcrConversion,
        vkDestroySamplerYcbcrConversion,
        vkDestroySamplerYcbcrConversionSafe,
        module Graphics.Vulkan.Types.Enum.InternalAllocationType,
        module Graphics.Vulkan.Types.Enum.SystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Struct.AllocationCallbacks,
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
        module Graphics.Vulkan.Types.Enum.Descriptor,
        module Graphics.Vulkan.Types.Struct.Descriptor,
        -- > #include "vk_platform.h"
        VkCreateDescriptorUpdateTemplate,
        pattern VkCreateDescriptorUpdateTemplate,
        HS_vkCreateDescriptorUpdateTemplate,
        PFN_vkCreateDescriptorUpdateTemplate,
        vkCreateDescriptorUpdateTemplate,
        vkCreateDescriptorUpdateTemplateSafe,
        VkDestroyDescriptorUpdateTemplate,
        pattern VkDestroyDescriptorUpdateTemplate,
        HS_vkDestroyDescriptorUpdateTemplate,
        PFN_vkDestroyDescriptorUpdateTemplate,
        vkDestroyDescriptorUpdateTemplate,
        vkDestroyDescriptorUpdateTemplateSafe,
        VkUpdateDescriptorSetWithTemplate,
        pattern VkUpdateDescriptorSetWithTemplate,
        HS_vkUpdateDescriptorSetWithTemplate,
        PFN_vkUpdateDescriptorSetWithTemplate,
        vkUpdateDescriptorSetWithTemplate,
        vkUpdateDescriptorSetWithTemplateSafe,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO,
        pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE,
        -- ** Promoted from VK_KHR_external_memory_capabilities
        module Graphics.Vulkan.Types.Enum.Buffer,
        module Graphics.Vulkan.Types.Struct.External,
        module Graphics.Vulkan.Types.Enum.External,
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceExternalBufferProperties,
        pattern VkGetPhysicalDeviceExternalBufferProperties,
        HS_vkGetPhysicalDeviceExternalBufferProperties,
        PFN_vkGetPhysicalDeviceExternalBufferProperties,
        vkGetPhysicalDeviceExternalBufferProperties,
        vkGetPhysicalDeviceExternalBufferPropertiesSafe,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES,
        pattern VK_LUID_SIZE, -- ** Promoted from VK_KHR_external_memory
                              module Graphics.Vulkan.Types.Struct.Export,
        module Graphics.Vulkan.Types.Enum.SharingMode,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO,
        pattern VK_ERROR_INVALID_EXTERNAL_HANDLE,
        pattern VK_QUEUE_FAMILY_EXTERNAL,
        -- ** Promoted from VK_KHR_external_fence_capabilities
        --
        -- |
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceExternalFenceProperties,
        pattern VkGetPhysicalDeviceExternalFenceProperties,
        HS_vkGetPhysicalDeviceExternalFenceProperties,
        PFN_vkGetPhysicalDeviceExternalFenceProperties,
        vkGetPhysicalDeviceExternalFenceProperties,
        vkGetPhysicalDeviceExternalFencePropertiesSafe,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES,
        -- ** Promoted from VK_KHR_external_fence
        module Graphics.Vulkan.Types.Enum.Fence,
        module Graphics.Vulkan.Types.Struct.Fence,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO,
        -- ** Promoted from VK_KHR_external_semaphore
        module Graphics.Vulkan.Types.Struct.Semaphore,
        module Graphics.Vulkan.Types.Enum.SemaphoreImportFlag,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO,
        -- ** Promoted from VK_KHR_external_semaphore_capabilities
        --
        -- |
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceExternalSemaphoreProperties,
        pattern VkGetPhysicalDeviceExternalSemaphoreProperties,
        HS_vkGetPhysicalDeviceExternalSemaphoreProperties,
        PFN_vkGetPhysicalDeviceExternalSemaphoreProperties,
        vkGetPhysicalDeviceExternalSemaphoreProperties,
        vkGetPhysicalDeviceExternalSemaphorePropertiesSafe,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES,
        -- ** Promoted from VK_KHR_maintenance3
        --
        -- |
        -- > #include "vk_platform.h"
        VkGetDescriptorSetLayoutSupport,
        pattern VkGetDescriptorSetLayoutSupport,
        HS_vkGetDescriptorSetLayoutSupport,
        PFN_vkGetDescriptorSetLayoutSupport,
        vkGetDescriptorSetLayoutSupport,
        vkGetDescriptorSetLayoutSupportSafe,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT,
        -- ** Promoted from VK_KHR_shader_draw_parameters, with a feature support query added
        --
        -- |
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES)
       where
import           GHC.Ptr                                                     (Ptr (..))
import           Graphics.Vulkan.Constants                                   (pattern VK_LUID_SIZE,
                                                                              pattern VK_MAX_DEVICE_GROUP_SIZE,
                                                                              pattern VK_QUEUE_FAMILY_EXTERNAL)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.AccessFlags
import           Graphics.Vulkan.Types.Enum.Attachment
import           Graphics.Vulkan.Types.Enum.BorderColor
import           Graphics.Vulkan.Types.Enum.Buffer
import           Graphics.Vulkan.Types.Enum.ChromaLocation
import           Graphics.Vulkan.Types.Enum.Command
import           Graphics.Vulkan.Types.Enum.CompareOp
import           Graphics.Vulkan.Types.Enum.ComponentSwizzle
import           Graphics.Vulkan.Types.Enum.DependencyFlags
import           Graphics.Vulkan.Types.Enum.Descriptor
import           Graphics.Vulkan.Types.Enum.Device
import           Graphics.Vulkan.Types.Enum.External
import           Graphics.Vulkan.Types.Enum.Fence
import           Graphics.Vulkan.Types.Enum.Filter
import           Graphics.Vulkan.Types.Enum.Format
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.InternalAllocationType
import           Graphics.Vulkan.Types.Enum.Memory
import           Graphics.Vulkan.Types.Enum.Object                           (VkObjectType (..))
import           Graphics.Vulkan.Types.Enum.PeerMemoryFeatureFlag
import           Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.Pipeline
import           Graphics.Vulkan.Types.Enum.PointClippingBehavior
import           Graphics.Vulkan.Types.Enum.Query
import           Graphics.Vulkan.Types.Enum.Queue
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.Sampler
import           Graphics.Vulkan.Types.Enum.SemaphoreImportFlag
import           Graphics.Vulkan.Types.Enum.Shader
import           Graphics.Vulkan.Types.Enum.SharingMode
import           Graphics.Vulkan.Types.Enum.Sparse
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.SubgroupFeatureFlags
import           Graphics.Vulkan.Types.Enum.Subpass
import           Graphics.Vulkan.Types.Enum.SystemAllocationScope
import           Graphics.Vulkan.Types.Enum.TessellationDomainOrigin
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.AllocationCallbacks
import           Graphics.Vulkan.Types.Struct.Attachment
import           Graphics.Vulkan.Types.Struct.Bind
import           Graphics.Vulkan.Types.Struct.Buffer
import           Graphics.Vulkan.Types.Struct.Clear
import           Graphics.Vulkan.Types.Struct.Command
import           Graphics.Vulkan.Types.Struct.ComponentMapping
import           Graphics.Vulkan.Types.Struct.Descriptor
import           Graphics.Vulkan.Types.Struct.Device
import           Graphics.Vulkan.Types.Struct.Export
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.External
import           Graphics.Vulkan.Types.Struct.Fence
import           Graphics.Vulkan.Types.Struct.FormatProperties
import           Graphics.Vulkan.Types.Struct.Image
import           Graphics.Vulkan.Types.Struct.InputAttachmentAspectReference
import           Graphics.Vulkan.Types.Struct.Memory
import           Graphics.Vulkan.Types.Struct.Offset
import           Graphics.Vulkan.Types.Struct.PhysicalDevice
import           Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures
import           Graphics.Vulkan.Types.Struct.Pipeline
import           Graphics.Vulkan.Types.Struct.ProtectedSubmitInfo
import           Graphics.Vulkan.Types.Struct.QueueFamilyProperties
import           Graphics.Vulkan.Types.Struct.Rect
import           Graphics.Vulkan.Types.Struct.RenderPass
import           Graphics.Vulkan.Types.Struct.Sampler
import           Graphics.Vulkan.Types.Struct.Semaphore
import           Graphics.Vulkan.Types.Struct.Sparse
import           Graphics.Vulkan.Types.Struct.SubmitInfo
import           Graphics.Vulkan.Types.Struct.Subpass
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- > VkResult vkEnumerateInstanceVersion
-- >     ( uint32_t* pApiVersion
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateInstanceVersion vkEnumerateInstanceVersion registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkEnumerateInstanceVersion"
               vkEnumerateInstanceVersion :: Ptr Word32 -- ^ pApiVersion
                                                        -> IO VkResult

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- > VkResult vkEnumerateInstanceVersion
-- >     ( uint32_t* pApiVersion
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateInstanceVersion vkEnumerateInstanceVersion registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumerateInstanceVersion <- vkGetInstanceProc @VkEnumerateInstanceVersion vkInstance
--
vkEnumerateInstanceVersion :: Ptr Word32 -- ^ pApiVersion
                                         -> IO VkResult
vkEnumerateInstanceVersion
  = error $
      "Cannot lookup C symbol \"vkEnumerateInstanceVersion\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-1 cabal flag."

{-# WARNING
vkEnumerateInstanceVersion"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- > VkResult vkEnumerateInstanceVersion
-- >     ( uint32_t* pApiVersion
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateInstanceVersion vkEnumerateInstanceVersion registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkEnumerateInstanceVersion"
               vkEnumerateInstanceVersionSafe :: Ptr Word32 -- ^ pApiVersion
                                                            -> IO VkResult

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- > VkResult vkEnumerateInstanceVersion
-- >     ( uint32_t* pApiVersion
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateInstanceVersion vkEnumerateInstanceVersion registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumerateInstanceVersion <- vkGetInstanceProc @VkEnumerateInstanceVersion vkInstance
--
vkEnumerateInstanceVersionSafe :: Ptr Word32 -- ^ pApiVersion
                                             -> IO VkResult
vkEnumerateInstanceVersionSafe = vkEnumerateInstanceVersion

{-# INLINE vkEnumerateInstanceVersionSafe #-}

{-# WARNING
vkEnumerateInstanceVersionSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkEnumerateInstanceVersion
--   >     ( uint32_t* pApiVersion
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateInstanceVersion vkEnumerateInstanceVersion registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkBindBufferMemory2
-- >     ( VkDevice device
-- >     , uint32_t bindInfoCount
-- >     , const VkBindBufferMemoryInfo* pBindInfos
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindBufferMemory2 vkBindBufferMemory2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkBindBufferMemory2"
               vkBindBufferMemory2 ::
               VkDevice -- ^ device
                        -> Word32 -- ^ bindInfoCount
                                  -> Ptr VkBindBufferMemoryInfo -- ^ pBindInfos
                                                                -> IO VkResult

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkBindBufferMemory2
-- >     ( VkDevice device
-- >     , uint32_t bindInfoCount
-- >     , const VkBindBufferMemoryInfo* pBindInfos
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindBufferMemory2 vkBindBufferMemory2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myBindBufferMemory2 <- vkGetDeviceProc @VkBindBufferMemory2 vkDevice
--
vkBindBufferMemory2 ::
                    VkDevice -- ^ device
                             -> Word32 -- ^ bindInfoCount
                                       -> Ptr VkBindBufferMemoryInfo -- ^ pBindInfos
                                                                     -> IO VkResult
vkBindBufferMemory2 d
  = unsafeDupablePerformIO (vkGetDeviceProc @VkBindBufferMemory2 d) d

{-# INLINE vkBindBufferMemory2 #-}

{-# WARNING
vkBindBufferMemory2"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkBindBufferMemory2
-- >     ( VkDevice device
-- >     , uint32_t bindInfoCount
-- >     , const VkBindBufferMemoryInfo* pBindInfos
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindBufferMemory2 vkBindBufferMemory2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkBindBufferMemory2"
               vkBindBufferMemory2Safe ::
               VkDevice -- ^ device
                        -> Word32 -- ^ bindInfoCount
                                  -> Ptr VkBindBufferMemoryInfo -- ^ pBindInfos
                                                                -> IO VkResult

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkBindBufferMemory2
-- >     ( VkDevice device
-- >     , uint32_t bindInfoCount
-- >     , const VkBindBufferMemoryInfo* pBindInfos
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindBufferMemory2 vkBindBufferMemory2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myBindBufferMemory2 <- vkGetDeviceProc @VkBindBufferMemory2 vkDevice
--
vkBindBufferMemory2Safe ::
                        VkDevice -- ^ device
                                 -> Word32 -- ^ bindInfoCount
                                           -> Ptr VkBindBufferMemoryInfo -- ^ pBindInfos
                                                                         -> IO VkResult
vkBindBufferMemory2Safe = vkBindBufferMemory2

{-# INLINE vkBindBufferMemory2Safe #-}

{-# WARNING
vkBindBufferMemory2Safe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindBufferMemory2 vkBindBufferMemory2 registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkBindImageMemory2
-- >     ( VkDevice device
-- >     , uint32_t bindInfoCount
-- >     , const VkBindImageMemoryInfo* pBindInfos
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindImageMemory2 vkBindImageMemory2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkBindImageMemory2" vkBindImageMemory2
               :: VkDevice -- ^ device
                           -> Word32 -- ^ bindInfoCount
                                     -> Ptr VkBindImageMemoryInfo -- ^ pBindInfos
                                                                  -> IO VkResult

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkBindImageMemory2
-- >     ( VkDevice device
-- >     , uint32_t bindInfoCount
-- >     , const VkBindImageMemoryInfo* pBindInfos
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindImageMemory2 vkBindImageMemory2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myBindImageMemory2 <- vkGetDeviceProc @VkBindImageMemory2 vkDevice
--
vkBindImageMemory2 ::
                   VkDevice -- ^ device
                            -> Word32 -- ^ bindInfoCount
                                      -> Ptr VkBindImageMemoryInfo -- ^ pBindInfos
                                                                   -> IO VkResult
vkBindImageMemory2 d
  = unsafeDupablePerformIO (vkGetDeviceProc @VkBindImageMemory2 d) d

{-# INLINE vkBindImageMemory2 #-}

{-# WARNING
vkBindImageMemory2"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkBindImageMemory2
-- >     ( VkDevice device
-- >     , uint32_t bindInfoCount
-- >     , const VkBindImageMemoryInfo* pBindInfos
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindImageMemory2 vkBindImageMemory2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkBindImageMemory2"
               vkBindImageMemory2Safe ::
               VkDevice -- ^ device
                        -> Word32 -- ^ bindInfoCount
                                  -> Ptr VkBindImageMemoryInfo -- ^ pBindInfos
                                                               -> IO VkResult

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkBindImageMemory2
-- >     ( VkDevice device
-- >     , uint32_t bindInfoCount
-- >     , const VkBindImageMemoryInfo* pBindInfos
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindImageMemory2 vkBindImageMemory2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myBindImageMemory2 <- vkGetDeviceProc @VkBindImageMemory2 vkDevice
--
vkBindImageMemory2Safe ::
                       VkDevice -- ^ device
                                -> Word32 -- ^ bindInfoCount
                                          -> Ptr VkBindImageMemoryInfo -- ^ pBindInfos
                                                                       -> IO VkResult
vkBindImageMemory2Safe = vkBindImageMemory2

{-# INLINE vkBindImageMemory2Safe #-}

{-# WARNING
vkBindImageMemory2Safe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindImageMemory2 vkBindImageMemory2 registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetDeviceGroupPeerMemoryFeatures
-- >     ( VkDevice device
-- >     , uint32_t heapIndex
-- >     , uint32_t localDeviceIndex
-- >     , uint32_t remoteDeviceIndex
-- >     , VkPeerMemoryFeatureFlags* pPeerMemoryFeatures
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceGroupPeerMemoryFeatures vkGetDeviceGroupPeerMemoryFeatures registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkGetDeviceGroupPeerMemoryFeatures"
               vkGetDeviceGroupPeerMemoryFeatures ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ heapIndex
                        -> Word32 -- ^ localDeviceIndex
                                  -> Word32 -- ^ remoteDeviceIndex
                                            -> Ptr VkPeerMemoryFeatureFlags -- ^ pPeerMemoryFeatures
                                                                            -> IO ()

#else
-- |
-- > void vkGetDeviceGroupPeerMemoryFeatures
-- >     ( VkDevice device
-- >     , uint32_t heapIndex
-- >     , uint32_t localDeviceIndex
-- >     , uint32_t remoteDeviceIndex
-- >     , VkPeerMemoryFeatureFlags* pPeerMemoryFeatures
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceGroupPeerMemoryFeatures vkGetDeviceGroupPeerMemoryFeatures registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceGroupPeerMemoryFeatures <- vkGetDeviceProc @VkGetDeviceGroupPeerMemoryFeatures vkDevice
--
vkGetDeviceGroupPeerMemoryFeatures ::
                                   VkDevice -- ^ device
                                            ->
                                     Word32 -- ^ heapIndex
                                            ->
                                       Word32 -- ^ localDeviceIndex
                                              -> Word32 -- ^ remoteDeviceIndex
                                                        -> Ptr VkPeerMemoryFeatureFlags -- ^ pPeerMemoryFeatures
                                                                                        -> IO ()
vkGetDeviceGroupPeerMemoryFeatures d
  = unsafeDupablePerformIO
      (vkGetDeviceProc @VkGetDeviceGroupPeerMemoryFeatures d)
      d

{-# INLINE vkGetDeviceGroupPeerMemoryFeatures #-}

{-# WARNING
vkGetDeviceGroupPeerMemoryFeatures"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetDeviceGroupPeerMemoryFeatures
-- >     ( VkDevice device
-- >     , uint32_t heapIndex
-- >     , uint32_t localDeviceIndex
-- >     , uint32_t remoteDeviceIndex
-- >     , VkPeerMemoryFeatureFlags* pPeerMemoryFeatures
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceGroupPeerMemoryFeatures vkGetDeviceGroupPeerMemoryFeatures registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkGetDeviceGroupPeerMemoryFeatures"
               vkGetDeviceGroupPeerMemoryFeaturesSafe ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ heapIndex
                        -> Word32 -- ^ localDeviceIndex
                                  -> Word32 -- ^ remoteDeviceIndex
                                            -> Ptr VkPeerMemoryFeatureFlags -- ^ pPeerMemoryFeatures
                                                                            -> IO ()

#else
-- |
-- > void vkGetDeviceGroupPeerMemoryFeatures
-- >     ( VkDevice device
-- >     , uint32_t heapIndex
-- >     , uint32_t localDeviceIndex
-- >     , uint32_t remoteDeviceIndex
-- >     , VkPeerMemoryFeatureFlags* pPeerMemoryFeatures
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceGroupPeerMemoryFeatures vkGetDeviceGroupPeerMemoryFeatures registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceGroupPeerMemoryFeatures <- vkGetDeviceProc @VkGetDeviceGroupPeerMemoryFeatures vkDevice
--
vkGetDeviceGroupPeerMemoryFeaturesSafe ::
                                       VkDevice -- ^ device
                                                ->
                                         Word32 -- ^ heapIndex
                                                ->
                                           Word32 -- ^ localDeviceIndex
                                                  -> Word32 -- ^ remoteDeviceIndex
                                                            -> Ptr VkPeerMemoryFeatureFlags -- ^ pPeerMemoryFeatures
                                                                                            -> IO ()
vkGetDeviceGroupPeerMemoryFeaturesSafe
  = vkGetDeviceGroupPeerMemoryFeatures

{-# INLINE vkGetDeviceGroupPeerMemoryFeaturesSafe #-}

{-# WARNING
vkGetDeviceGroupPeerMemoryFeaturesSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkGetDeviceGroupPeerMemoryFeatures
--   >     ( VkDevice device
--   >     , uint32_t heapIndex
--   >     , uint32_t localDeviceIndex
--   >     , uint32_t remoteDeviceIndex
--   >     , VkPeerMemoryFeatureFlags* pPeerMemoryFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceGroupPeerMemoryFeatures vkGetDeviceGroupPeerMemoryFeatures registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Queues: 'graphics', 'compute', 'transfer'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetDeviceMask
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t deviceMask
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetDeviceMask vkCmdSetDeviceMask registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkCmdSetDeviceMask" vkCmdSetDeviceMask
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> Word32 -- ^ deviceMask
                                            -> IO ()

#else
-- |
-- Queues: 'graphics', 'compute', 'transfer'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetDeviceMask
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t deviceMask
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetDeviceMask vkCmdSetDeviceMask registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetDeviceMask <- vkGetInstanceProc @VkCmdSetDeviceMask vkInstance
--
vkCmdSetDeviceMask :: VkCommandBuffer -- ^ commandBuffer
                                      -> Word32 -- ^ deviceMask
                                                -> IO ()
vkCmdSetDeviceMask
  = error $
      "Cannot lookup C symbol \"vkCmdSetDeviceMask\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-1 cabal flag."

{-# WARNING
vkCmdSetDeviceMask"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Queues: 'graphics', 'compute', 'transfer'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetDeviceMask
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t deviceMask
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetDeviceMask vkCmdSetDeviceMask registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkCmdSetDeviceMask"
               vkCmdSetDeviceMaskSafe :: VkCommandBuffer -- ^ commandBuffer
                                                         -> Word32 -- ^ deviceMask
                                                                   -> IO ()

#else
-- |
-- Queues: 'graphics', 'compute', 'transfer'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetDeviceMask
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t deviceMask
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetDeviceMask vkCmdSetDeviceMask registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetDeviceMask <- vkGetInstanceProc @VkCmdSetDeviceMask vkInstance
--
vkCmdSetDeviceMaskSafe :: VkCommandBuffer -- ^ commandBuffer
                                          -> Word32 -- ^ deviceMask
                                                    -> IO ()
vkCmdSetDeviceMaskSafe = vkCmdSetDeviceMask

{-# INLINE vkCmdSetDeviceMaskSafe #-}

{-# WARNING
vkCmdSetDeviceMaskSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | Queues: 'graphics', 'compute', 'transfer'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetDeviceMask
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t deviceMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetDeviceMask vkCmdSetDeviceMask registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Queues: 'compute'.
--
-- Renderpass: @outside@
--
-- > void vkCmdDispatchBase
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t baseGroupX
-- >     , uint32_t baseGroupY
-- >     , uint32_t baseGroupZ
-- >     , uint32_t groupCountX
-- >     , uint32_t groupCountY
-- >     , uint32_t groupCountZ
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDispatchBase vkCmdDispatchBase registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
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

#else
-- |
-- Queues: 'compute'.
--
-- Renderpass: @outside@
--
-- > void vkCmdDispatchBase
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t baseGroupX
-- >     , uint32_t baseGroupY
-- >     , uint32_t baseGroupZ
-- >     , uint32_t groupCountX
-- >     , uint32_t groupCountY
-- >     , uint32_t groupCountZ
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDispatchBase vkCmdDispatchBase registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDispatchBase <- vkGetInstanceProc @VkCmdDispatchBase vkInstance
--
vkCmdDispatchBase ::
                  VkCommandBuffer -- ^ commandBuffer
                                  ->
                    Word32 -- ^ baseGroupX
                           -> Word32 -- ^ baseGroupY
                                     -> Word32 -- ^ baseGroupZ
                                               -> Word32 -- ^ groupCountX
                                                         -> Word32 -- ^ groupCountY
                                                                   -> Word32 -- ^ groupCountZ
                                                                             -> IO ()
vkCmdDispatchBase
  = error $
      "Cannot lookup C symbol \"vkCmdDispatchBase\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-1 cabal flag."

{-# WARNING
vkCmdDispatchBase"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Queues: 'compute'.
--
-- Renderpass: @outside@
--
-- > void vkCmdDispatchBase
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t baseGroupX
-- >     , uint32_t baseGroupY
-- >     , uint32_t baseGroupZ
-- >     , uint32_t groupCountX
-- >     , uint32_t groupCountY
-- >     , uint32_t groupCountZ
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDispatchBase vkCmdDispatchBase registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
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

#else
-- |
-- Queues: 'compute'.
--
-- Renderpass: @outside@
--
-- > void vkCmdDispatchBase
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t baseGroupX
-- >     , uint32_t baseGroupY
-- >     , uint32_t baseGroupZ
-- >     , uint32_t groupCountX
-- >     , uint32_t groupCountY
-- >     , uint32_t groupCountZ
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDispatchBase vkCmdDispatchBase registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDispatchBase <- vkGetInstanceProc @VkCmdDispatchBase vkInstance
--
vkCmdDispatchBaseSafe ::
                      VkCommandBuffer -- ^ commandBuffer
                                      ->
                        Word32 -- ^ baseGroupX
                               -> Word32 -- ^ baseGroupY
                                         -> Word32 -- ^ baseGroupZ
                                                   -> Word32 -- ^ groupCountX
                                                             -> Word32 -- ^ groupCountY
                                                                       -> Word32 -- ^ groupCountZ
                                                                                 -> IO ()
vkCmdDispatchBaseSafe = vkCmdDispatchBase

{-# INLINE vkCmdDispatchBaseSafe #-}

{-# WARNING
vkCmdDispatchBaseSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdDispatchBase
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t baseGroupX
--   >     , uint32_t baseGroupY
--   >     , uint32_t baseGroupZ
--   >     , uint32_t groupCountX
--   >     , uint32_t groupCountY
--   >     , uint32_t groupCountZ
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDispatchBase vkCmdDispatchBase registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
-- > VkResult vkEnumeratePhysicalDeviceGroups
-- >     ( VkInstance instance
-- >     , uint32_t* pPhysicalDeviceGroupCount
-- >     , VkPhysicalDeviceGroupProperties* pPhysicalDeviceGroupProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumeratePhysicalDeviceGroups vkEnumeratePhysicalDeviceGroups registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkEnumeratePhysicalDeviceGroups"
               vkEnumeratePhysicalDeviceGroups ::
               VkInstance -- ^ instance
                          ->
                 Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                            -> Ptr VkPhysicalDeviceGroupProperties -- ^ pPhysicalDeviceGroupProperties
                                                                   -> IO VkResult

#else
-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
-- > VkResult vkEnumeratePhysicalDeviceGroups
-- >     ( VkInstance instance
-- >     , uint32_t* pPhysicalDeviceGroupCount
-- >     , VkPhysicalDeviceGroupProperties* pPhysicalDeviceGroupProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumeratePhysicalDeviceGroups vkEnumeratePhysicalDeviceGroups registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumeratePhysicalDeviceGroups <- vkGetInstanceProc @VkEnumeratePhysicalDeviceGroups vkInstance
--
vkEnumeratePhysicalDeviceGroups ::
                                VkInstance -- ^ instance
                                           ->
                                  Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                                             -> Ptr VkPhysicalDeviceGroupProperties -- ^ pPhysicalDeviceGroupProperties
                                                                                    -> IO VkResult
vkEnumeratePhysicalDeviceGroups d
  = unsafeDupablePerformIO
      (vkGetInstanceProc @VkEnumeratePhysicalDeviceGroups d)
      d

{-# INLINE vkEnumeratePhysicalDeviceGroups #-}

{-# WARNING
vkEnumeratePhysicalDeviceGroups"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetInstanceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
-- > VkResult vkEnumeratePhysicalDeviceGroups
-- >     ( VkInstance instance
-- >     , uint32_t* pPhysicalDeviceGroupCount
-- >     , VkPhysicalDeviceGroupProperties* pPhysicalDeviceGroupProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumeratePhysicalDeviceGroups vkEnumeratePhysicalDeviceGroups registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkEnumeratePhysicalDeviceGroups"
               vkEnumeratePhysicalDeviceGroupsSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                            -> Ptr VkPhysicalDeviceGroupProperties -- ^ pPhysicalDeviceGroupProperties
                                                                   -> IO VkResult

#else
-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
-- > VkResult vkEnumeratePhysicalDeviceGroups
-- >     ( VkInstance instance
-- >     , uint32_t* pPhysicalDeviceGroupCount
-- >     , VkPhysicalDeviceGroupProperties* pPhysicalDeviceGroupProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumeratePhysicalDeviceGroups vkEnumeratePhysicalDeviceGroups registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumeratePhysicalDeviceGroups <- vkGetInstanceProc @VkEnumeratePhysicalDeviceGroups vkInstance
--
vkEnumeratePhysicalDeviceGroupsSafe ::
                                    VkInstance -- ^ instance
                                               ->
                                      Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                                                 ->
                                        Ptr VkPhysicalDeviceGroupProperties -- ^ pPhysicalDeviceGroupProperties
                                                                            -> IO VkResult
vkEnumeratePhysicalDeviceGroupsSafe
  = vkEnumeratePhysicalDeviceGroups

{-# INLINE vkEnumeratePhysicalDeviceGroupsSafe #-}

{-# WARNING
vkEnumeratePhysicalDeviceGroupsSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetInstanceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumeratePhysicalDeviceGroups vkEnumeratePhysicalDeviceGroups registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetImageMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkImageMemoryRequirementsInfo2* pInfo
-- >     , VkMemoryRequirements2* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageMemoryRequirements2 vkGetImageMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkGetImageMemoryRequirements2"
               vkGetImageMemoryRequirements2 ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageMemoryRequirementsInfo2 -- ^ pInfo
                                                    ->
                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                             -> IO ()

#else
-- |
-- > void vkGetImageMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkImageMemoryRequirementsInfo2* pInfo
-- >     , VkMemoryRequirements2* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageMemoryRequirements2 vkGetImageMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetImageMemoryRequirements2 <- vkGetDeviceProc @VkGetImageMemoryRequirements2 vkDevice
--
vkGetImageMemoryRequirements2 ::
                              VkDevice -- ^ device
                                       ->
                                Ptr VkImageMemoryRequirementsInfo2 -- ^ pInfo
                                                                   ->
                                  Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                                            -> IO ()
vkGetImageMemoryRequirements2 d
  = unsafeDupablePerformIO
      (vkGetDeviceProc @VkGetImageMemoryRequirements2 d)
      d

{-# INLINE vkGetImageMemoryRequirements2 #-}

{-# WARNING
vkGetImageMemoryRequirements2"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetImageMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkImageMemoryRequirementsInfo2* pInfo
-- >     , VkMemoryRequirements2* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageMemoryRequirements2 vkGetImageMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkGetImageMemoryRequirements2"
               vkGetImageMemoryRequirements2Safe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageMemoryRequirementsInfo2 -- ^ pInfo
                                                    ->
                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                             -> IO ()

#else
-- |
-- > void vkGetImageMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkImageMemoryRequirementsInfo2* pInfo
-- >     , VkMemoryRequirements2* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageMemoryRequirements2 vkGetImageMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetImageMemoryRequirements2 <- vkGetDeviceProc @VkGetImageMemoryRequirements2 vkDevice
--
vkGetImageMemoryRequirements2Safe ::
                                  VkDevice -- ^ device
                                           ->
                                    Ptr VkImageMemoryRequirementsInfo2 -- ^ pInfo
                                                                       ->
                                      Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                                                -> IO ()
vkGetImageMemoryRequirements2Safe = vkGetImageMemoryRequirements2

{-# INLINE vkGetImageMemoryRequirements2Safe #-}

{-# WARNING
vkGetImageMemoryRequirements2Safe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkGetImageMemoryRequirements2
--   >     ( VkDevice device
--   >     , const VkImageMemoryRequirementsInfo2* pInfo
--   >     , VkMemoryRequirements2* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageMemoryRequirements2 vkGetImageMemoryRequirements2 registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetBufferMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkBufferMemoryRequirementsInfo2* pInfo
-- >     , VkMemoryRequirements2* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetBufferMemoryRequirements2 vkGetBufferMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkGetBufferMemoryRequirements2"
               vkGetBufferMemoryRequirements2 ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferMemoryRequirementsInfo2 -- ^ pInfo
                                                     ->
                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                             -> IO ()

#else
-- |
-- > void vkGetBufferMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkBufferMemoryRequirementsInfo2* pInfo
-- >     , VkMemoryRequirements2* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetBufferMemoryRequirements2 vkGetBufferMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetBufferMemoryRequirements2 <- vkGetDeviceProc @VkGetBufferMemoryRequirements2 vkDevice
--
vkGetBufferMemoryRequirements2 ::
                               VkDevice -- ^ device
                                        ->
                                 Ptr VkBufferMemoryRequirementsInfo2 -- ^ pInfo
                                                                     ->
                                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                                             -> IO ()
vkGetBufferMemoryRequirements2 d
  = unsafeDupablePerformIO
      (vkGetDeviceProc @VkGetBufferMemoryRequirements2 d)
      d

{-# INLINE vkGetBufferMemoryRequirements2 #-}

{-# WARNING
vkGetBufferMemoryRequirements2"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetBufferMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkBufferMemoryRequirementsInfo2* pInfo
-- >     , VkMemoryRequirements2* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetBufferMemoryRequirements2 vkGetBufferMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkGetBufferMemoryRequirements2"
               vkGetBufferMemoryRequirements2Safe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferMemoryRequirementsInfo2 -- ^ pInfo
                                                     ->
                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                             -> IO ()

#else
-- |
-- > void vkGetBufferMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkBufferMemoryRequirementsInfo2* pInfo
-- >     , VkMemoryRequirements2* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetBufferMemoryRequirements2 vkGetBufferMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetBufferMemoryRequirements2 <- vkGetDeviceProc @VkGetBufferMemoryRequirements2 vkDevice
--
vkGetBufferMemoryRequirements2Safe ::
                                   VkDevice -- ^ device
                                            ->
                                     Ptr VkBufferMemoryRequirementsInfo2 -- ^ pInfo
                                                                         ->
                                       Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                                                 -> IO ()
vkGetBufferMemoryRequirements2Safe = vkGetBufferMemoryRequirements2

{-# INLINE vkGetBufferMemoryRequirements2Safe #-}

{-# WARNING
vkGetBufferMemoryRequirements2Safe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkGetBufferMemoryRequirements2
--   >     ( VkDevice device
--   >     , const VkBufferMemoryRequirementsInfo2* pInfo
--   >     , VkMemoryRequirements2* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetBufferMemoryRequirements2 vkGetBufferMemoryRequirements2 registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetImageSparseMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkImageSparseMemoryRequirementsInfo2* pInfo
-- >     , uint32_t* pSparseMemoryRequirementCount
-- >     , VkSparseImageMemoryRequirements2* pSparseMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageSparseMemoryRequirements2 vkGetImageSparseMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkGetImageSparseMemoryRequirements2"
               vkGetImageSparseMemoryRequirements2 ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageSparseMemoryRequirementsInfo2 -- ^ pInfo
                                                          ->
                   Ptr Word32 -- ^ pSparseMemoryRequirementCount
                              -> Ptr VkSparseImageMemoryRequirements2 -- ^ pSparseMemoryRequirements
                                                                      -> IO ()

#else
-- |
-- > void vkGetImageSparseMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkImageSparseMemoryRequirementsInfo2* pInfo
-- >     , uint32_t* pSparseMemoryRequirementCount
-- >     , VkSparseImageMemoryRequirements2* pSparseMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageSparseMemoryRequirements2 vkGetImageSparseMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetImageSparseMemoryRequirements2 <- vkGetDeviceProc @VkGetImageSparseMemoryRequirements2 vkDevice
--
vkGetImageSparseMemoryRequirements2 ::
                                    VkDevice -- ^ device
                                             ->
                                      Ptr VkImageSparseMemoryRequirementsInfo2 -- ^ pInfo
                                                                               ->
                                        Ptr Word32 -- ^ pSparseMemoryRequirementCount
                                                   -> Ptr VkSparseImageMemoryRequirements2 -- ^ pSparseMemoryRequirements
                                                                                           -> IO ()
vkGetImageSparseMemoryRequirements2 d
  = unsafeDupablePerformIO
      (vkGetDeviceProc @VkGetImageSparseMemoryRequirements2 d)
      d

{-# INLINE vkGetImageSparseMemoryRequirements2 #-}

{-# WARNING
vkGetImageSparseMemoryRequirements2"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetImageSparseMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkImageSparseMemoryRequirementsInfo2* pInfo
-- >     , uint32_t* pSparseMemoryRequirementCount
-- >     , VkSparseImageMemoryRequirements2* pSparseMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageSparseMemoryRequirements2 vkGetImageSparseMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkGetImageSparseMemoryRequirements2"
               vkGetImageSparseMemoryRequirements2Safe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageSparseMemoryRequirementsInfo2 -- ^ pInfo
                                                          ->
                   Ptr Word32 -- ^ pSparseMemoryRequirementCount
                              -> Ptr VkSparseImageMemoryRequirements2 -- ^ pSparseMemoryRequirements
                                                                      -> IO ()

#else
-- |
-- > void vkGetImageSparseMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkImageSparseMemoryRequirementsInfo2* pInfo
-- >     , uint32_t* pSparseMemoryRequirementCount
-- >     , VkSparseImageMemoryRequirements2* pSparseMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageSparseMemoryRequirements2 vkGetImageSparseMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetImageSparseMemoryRequirements2 <- vkGetDeviceProc @VkGetImageSparseMemoryRequirements2 vkDevice
--
vkGetImageSparseMemoryRequirements2Safe ::
                                        VkDevice -- ^ device
                                                 ->
                                          Ptr VkImageSparseMemoryRequirementsInfo2 -- ^ pInfo
                                                                                   ->
                                            Ptr Word32 -- ^ pSparseMemoryRequirementCount
                                                       ->
                                              Ptr VkSparseImageMemoryRequirements2 -- ^ pSparseMemoryRequirements
                                                                                   -> IO ()
vkGetImageSparseMemoryRequirements2Safe
  = vkGetImageSparseMemoryRequirements2

{-# INLINE vkGetImageSparseMemoryRequirements2Safe #-}

{-# WARNING
vkGetImageSparseMemoryRequirements2Safe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkGetImageSparseMemoryRequirements2
--   >     ( VkDevice device
--   >     , const VkImageSparseMemoryRequirementsInfo2* pInfo
--   >     , uint32_t* pSparseMemoryRequirementCount
--   >     , VkSparseImageMemoryRequirements2* pSparseMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageSparseMemoryRequirements2 vkGetImageSparseMemoryRequirements2 registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceFeatures2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceFeatures2* pFeatures
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFeatures2 vkGetPhysicalDeviceFeatures2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkGetPhysicalDeviceFeatures2"
               vkGetPhysicalDeviceFeatures2 ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceFeatures2 -- ^ pFeatures
                                                                 -> IO ()

#else
-- |
-- > void vkGetPhysicalDeviceFeatures2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceFeatures2* pFeatures
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFeatures2 vkGetPhysicalDeviceFeatures2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceFeatures2 <- vkGetInstanceProc @VkGetPhysicalDeviceFeatures2 vkInstance
--
vkGetPhysicalDeviceFeatures2 ::
                             VkPhysicalDevice -- ^ physicalDevice
                                              -> Ptr VkPhysicalDeviceFeatures2 -- ^ pFeatures
                                                                               -> IO ()
vkGetPhysicalDeviceFeatures2
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDeviceFeatures2\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-1 cabal flag."

{-# WARNING
vkGetPhysicalDeviceFeatures2"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceFeatures2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceFeatures2* pFeatures
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFeatures2 vkGetPhysicalDeviceFeatures2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkGetPhysicalDeviceFeatures2"
               vkGetPhysicalDeviceFeatures2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceFeatures2 -- ^ pFeatures
                                                                 -> IO ()

#else
-- |
-- > void vkGetPhysicalDeviceFeatures2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceFeatures2* pFeatures
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFeatures2 vkGetPhysicalDeviceFeatures2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceFeatures2 <- vkGetInstanceProc @VkGetPhysicalDeviceFeatures2 vkInstance
--
vkGetPhysicalDeviceFeatures2Safe ::
                                 VkPhysicalDevice -- ^ physicalDevice
                                                  -> Ptr VkPhysicalDeviceFeatures2 -- ^ pFeatures
                                                                                   -> IO ()
vkGetPhysicalDeviceFeatures2Safe = vkGetPhysicalDeviceFeatures2

{-# INLINE vkGetPhysicalDeviceFeatures2Safe #-}

{-# WARNING
vkGetPhysicalDeviceFeatures2Safe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkGetPhysicalDeviceFeatures2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures2* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFeatures2 vkGetPhysicalDeviceFeatures2 registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceProperties2* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceProperties2 vkGetPhysicalDeviceProperties2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkGetPhysicalDeviceProperties2"
               vkGetPhysicalDeviceProperties2 ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceProperties2 -- ^ pProperties
                                                                   -> IO ()

#else
-- |
-- > void vkGetPhysicalDeviceProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceProperties2* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceProperties2 vkGetPhysicalDeviceProperties2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceProperties2 vkInstance
--
vkGetPhysicalDeviceProperties2 ::
                               VkPhysicalDevice -- ^ physicalDevice
                                                -> Ptr VkPhysicalDeviceProperties2 -- ^ pProperties
                                                                                   -> IO ()
vkGetPhysicalDeviceProperties2
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDeviceProperties2\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-1 cabal flag."

{-# WARNING
vkGetPhysicalDeviceProperties2"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceProperties2* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceProperties2 vkGetPhysicalDeviceProperties2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkGetPhysicalDeviceProperties2"
               vkGetPhysicalDeviceProperties2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceProperties2 -- ^ pProperties
                                                                   -> IO ()

#else
-- |
-- > void vkGetPhysicalDeviceProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceProperties2* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceProperties2 vkGetPhysicalDeviceProperties2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceProperties2 vkInstance
--
vkGetPhysicalDeviceProperties2Safe ::
                                   VkPhysicalDevice -- ^ physicalDevice
                                                    -> Ptr VkPhysicalDeviceProperties2 -- ^ pProperties
                                                                                       -> IO ()
vkGetPhysicalDeviceProperties2Safe = vkGetPhysicalDeviceProperties2

{-# INLINE vkGetPhysicalDeviceProperties2Safe #-}

{-# WARNING
vkGetPhysicalDeviceProperties2Safe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkGetPhysicalDeviceProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties2* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceProperties2 vkGetPhysicalDeviceProperties2 registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceFormatProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkFormat format
-- >     , VkFormatProperties2* pFormatProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFormatProperties2 vkGetPhysicalDeviceFormatProperties2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkGetPhysicalDeviceFormatProperties2"
               vkGetPhysicalDeviceFormatProperties2 ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkFormat -- ^ format
                                            -> Ptr VkFormatProperties2 -- ^ pFormatProperties
                                                                       -> IO ()

#else
-- |
-- > void vkGetPhysicalDeviceFormatProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkFormat format
-- >     , VkFormatProperties2* pFormatProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFormatProperties2 vkGetPhysicalDeviceFormatProperties2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceFormatProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceFormatProperties2 vkInstance
--
vkGetPhysicalDeviceFormatProperties2 ::
                                     VkPhysicalDevice -- ^ physicalDevice
                                                      ->
                                       VkFormat -- ^ format
                                                -> Ptr VkFormatProperties2 -- ^ pFormatProperties
                                                                           -> IO ()
vkGetPhysicalDeviceFormatProperties2
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDeviceFormatProperties2\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-1 cabal flag."

{-# WARNING
vkGetPhysicalDeviceFormatProperties2"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceFormatProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkFormat format
-- >     , VkFormatProperties2* pFormatProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFormatProperties2 vkGetPhysicalDeviceFormatProperties2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkGetPhysicalDeviceFormatProperties2"
               vkGetPhysicalDeviceFormatProperties2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkFormat -- ^ format
                                            -> Ptr VkFormatProperties2 -- ^ pFormatProperties
                                                                       -> IO ()

#else
-- |
-- > void vkGetPhysicalDeviceFormatProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkFormat format
-- >     , VkFormatProperties2* pFormatProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFormatProperties2 vkGetPhysicalDeviceFormatProperties2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceFormatProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceFormatProperties2 vkInstance
--
vkGetPhysicalDeviceFormatProperties2Safe ::
                                         VkPhysicalDevice -- ^ physicalDevice
                                                          ->
                                           VkFormat -- ^ format
                                                    -> Ptr VkFormatProperties2 -- ^ pFormatProperties
                                                                               -> IO ()
vkGetPhysicalDeviceFormatProperties2Safe
  = vkGetPhysicalDeviceFormatProperties2

{-# INLINE vkGetPhysicalDeviceFormatProperties2Safe #-}

{-# WARNING
vkGetPhysicalDeviceFormatProperties2Safe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkGetPhysicalDeviceFormatProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties2* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFormatProperties2 vkGetPhysicalDeviceFormatProperties2 registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
-- > VkResult vkGetPhysicalDeviceImageFormatProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceImageFormatInfo2* pImageFormatInfo
-- >     , VkImageFormatProperties2* pImageFormatProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceImageFormatProperties2 vkGetPhysicalDeviceImageFormatProperties2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe
               "vkGetPhysicalDeviceImageFormatProperties2"
               vkGetPhysicalDeviceImageFormatProperties2 ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceImageFormatInfo2 -- ^ pImageFormatInfo
                                                      ->
                   Ptr VkImageFormatProperties2 -- ^ pImageFormatProperties
                                                -> IO VkResult

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
-- > VkResult vkGetPhysicalDeviceImageFormatProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceImageFormatInfo2* pImageFormatInfo
-- >     , VkImageFormatProperties2* pImageFormatProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceImageFormatProperties2 vkGetPhysicalDeviceImageFormatProperties2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceImageFormatProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceImageFormatProperties2 vkInstance
--
vkGetPhysicalDeviceImageFormatProperties2 ::
                                          VkPhysicalDevice -- ^ physicalDevice
                                                           ->
                                            Ptr VkPhysicalDeviceImageFormatInfo2 -- ^ pImageFormatInfo
                                                                                 ->
                                              Ptr VkImageFormatProperties2 -- ^ pImageFormatProperties
                                                                           -> IO VkResult
vkGetPhysicalDeviceImageFormatProperties2
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDeviceImageFormatProperties2\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-1 cabal flag."

{-# WARNING
vkGetPhysicalDeviceImageFormatProperties2"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
-- > VkResult vkGetPhysicalDeviceImageFormatProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceImageFormatInfo2* pImageFormatInfo
-- >     , VkImageFormatProperties2* pImageFormatProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceImageFormatProperties2 vkGetPhysicalDeviceImageFormatProperties2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe
               "vkGetPhysicalDeviceImageFormatProperties2"
               vkGetPhysicalDeviceImageFormatProperties2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceImageFormatInfo2 -- ^ pImageFormatInfo
                                                      ->
                   Ptr VkImageFormatProperties2 -- ^ pImageFormatProperties
                                                -> IO VkResult

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
-- > VkResult vkGetPhysicalDeviceImageFormatProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceImageFormatInfo2* pImageFormatInfo
-- >     , VkImageFormatProperties2* pImageFormatProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceImageFormatProperties2 vkGetPhysicalDeviceImageFormatProperties2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceImageFormatProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceImageFormatProperties2 vkInstance
--
vkGetPhysicalDeviceImageFormatProperties2Safe ::
                                              VkPhysicalDevice -- ^ physicalDevice
                                                               ->
                                                Ptr VkPhysicalDeviceImageFormatInfo2 -- ^ pImageFormatInfo
                                                                                     ->
                                                  Ptr VkImageFormatProperties2 -- ^ pImageFormatProperties
                                                                               -> IO VkResult
vkGetPhysicalDeviceImageFormatProperties2Safe
  = vkGetPhysicalDeviceImageFormatProperties2

{-# INLINE vkGetPhysicalDeviceImageFormatProperties2Safe #-}

{-# WARNING
vkGetPhysicalDeviceImageFormatProperties2Safe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceImageFormatProperties2 vkGetPhysicalDeviceImageFormatProperties2 registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceQueueFamilyProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t* pQueueFamilyPropertyCount
-- >     , VkQueueFamilyProperties2* pQueueFamilyProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceQueueFamilyProperties2 vkGetPhysicalDeviceQueueFamilyProperties2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe
               "vkGetPhysicalDeviceQueueFamilyProperties2"
               vkGetPhysicalDeviceQueueFamilyProperties2 ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pQueueFamilyPropertyCount
                            -> Ptr VkQueueFamilyProperties2 -- ^ pQueueFamilyProperties
                                                            -> IO ()

#else
-- |
-- > void vkGetPhysicalDeviceQueueFamilyProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t* pQueueFamilyPropertyCount
-- >     , VkQueueFamilyProperties2* pQueueFamilyProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceQueueFamilyProperties2 vkGetPhysicalDeviceQueueFamilyProperties2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceQueueFamilyProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceQueueFamilyProperties2 vkInstance
--
vkGetPhysicalDeviceQueueFamilyProperties2 ::
                                          VkPhysicalDevice -- ^ physicalDevice
                                                           ->
                                            Ptr Word32 -- ^ pQueueFamilyPropertyCount
                                                       -> Ptr VkQueueFamilyProperties2 -- ^ pQueueFamilyProperties
                                                                                       -> IO ()
vkGetPhysicalDeviceQueueFamilyProperties2
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDeviceQueueFamilyProperties2\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-1 cabal flag."

{-# WARNING
vkGetPhysicalDeviceQueueFamilyProperties2"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceQueueFamilyProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t* pQueueFamilyPropertyCount
-- >     , VkQueueFamilyProperties2* pQueueFamilyProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceQueueFamilyProperties2 vkGetPhysicalDeviceQueueFamilyProperties2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe
               "vkGetPhysicalDeviceQueueFamilyProperties2"
               vkGetPhysicalDeviceQueueFamilyProperties2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pQueueFamilyPropertyCount
                            -> Ptr VkQueueFamilyProperties2 -- ^ pQueueFamilyProperties
                                                            -> IO ()

#else
-- |
-- > void vkGetPhysicalDeviceQueueFamilyProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t* pQueueFamilyPropertyCount
-- >     , VkQueueFamilyProperties2* pQueueFamilyProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceQueueFamilyProperties2 vkGetPhysicalDeviceQueueFamilyProperties2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceQueueFamilyProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceQueueFamilyProperties2 vkInstance
--
vkGetPhysicalDeviceQueueFamilyProperties2Safe ::
                                              VkPhysicalDevice -- ^ physicalDevice
                                                               ->
                                                Ptr Word32 -- ^ pQueueFamilyPropertyCount
                                                           -> Ptr VkQueueFamilyProperties2 -- ^ pQueueFamilyProperties
                                                                                           -> IO ()
vkGetPhysicalDeviceQueueFamilyProperties2Safe
  = vkGetPhysicalDeviceQueueFamilyProperties2

{-# INLINE vkGetPhysicalDeviceQueueFamilyProperties2Safe #-}

{-# WARNING
vkGetPhysicalDeviceQueueFamilyProperties2Safe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkGetPhysicalDeviceQueueFamilyProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties2* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceQueueFamilyProperties2 vkGetPhysicalDeviceQueueFamilyProperties2 registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceMemoryProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceMemoryProperties2* pMemoryProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMemoryProperties2 vkGetPhysicalDeviceMemoryProperties2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkGetPhysicalDeviceMemoryProperties2"
               vkGetPhysicalDeviceMemoryProperties2 ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceMemoryProperties2 -- ^ pMemoryProperties
                                                                         -> IO ()

#else
-- |
-- > void vkGetPhysicalDeviceMemoryProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceMemoryProperties2* pMemoryProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMemoryProperties2 vkGetPhysicalDeviceMemoryProperties2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceMemoryProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceMemoryProperties2 vkInstance
--
vkGetPhysicalDeviceMemoryProperties2 ::
                                     VkPhysicalDevice -- ^ physicalDevice
                                                      ->
                                       Ptr VkPhysicalDeviceMemoryProperties2 -- ^ pMemoryProperties
                                                                             -> IO ()
vkGetPhysicalDeviceMemoryProperties2
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDeviceMemoryProperties2\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-1 cabal flag."

{-# WARNING
vkGetPhysicalDeviceMemoryProperties2"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceMemoryProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceMemoryProperties2* pMemoryProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMemoryProperties2 vkGetPhysicalDeviceMemoryProperties2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkGetPhysicalDeviceMemoryProperties2"
               vkGetPhysicalDeviceMemoryProperties2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceMemoryProperties2 -- ^ pMemoryProperties
                                                                         -> IO ()

#else
-- |
-- > void vkGetPhysicalDeviceMemoryProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceMemoryProperties2* pMemoryProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMemoryProperties2 vkGetPhysicalDeviceMemoryProperties2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceMemoryProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceMemoryProperties2 vkInstance
--
vkGetPhysicalDeviceMemoryProperties2Safe ::
                                         VkPhysicalDevice -- ^ physicalDevice
                                                          ->
                                           Ptr VkPhysicalDeviceMemoryProperties2 -- ^ pMemoryProperties
                                                                                 -> IO ()
vkGetPhysicalDeviceMemoryProperties2Safe
  = vkGetPhysicalDeviceMemoryProperties2

{-# INLINE vkGetPhysicalDeviceMemoryProperties2Safe #-}

{-# WARNING
vkGetPhysicalDeviceMemoryProperties2Safe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkGetPhysicalDeviceMemoryProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties2* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMemoryProperties2 vkGetPhysicalDeviceMemoryProperties2 registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceSparseImageFormatProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceSparseImageFormatInfo2* pFormatInfo
-- >     , uint32_t* pPropertyCount
-- >     , VkSparseImageFormatProperties2* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSparseImageFormatProperties2 vkGetPhysicalDeviceSparseImageFormatProperties2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
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

#else
-- |
-- > void vkGetPhysicalDeviceSparseImageFormatProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceSparseImageFormatInfo2* pFormatInfo
-- >     , uint32_t* pPropertyCount
-- >     , VkSparseImageFormatProperties2* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSparseImageFormatProperties2 vkGetPhysicalDeviceSparseImageFormatProperties2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceSparseImageFormatProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceSparseImageFormatProperties2 vkInstance
--
vkGetPhysicalDeviceSparseImageFormatProperties2 ::
                                                VkPhysicalDevice -- ^ physicalDevice
                                                                 ->
                                                  Ptr VkPhysicalDeviceSparseImageFormatInfo2 -- ^ pFormatInfo
                                                                                             ->
                                                    Ptr Word32 -- ^ pPropertyCount
                                                               ->
                                                      Ptr VkSparseImageFormatProperties2 -- ^ pProperties
                                                                                         -> IO ()
vkGetPhysicalDeviceSparseImageFormatProperties2
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDeviceSparseImageFormatProperties2\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-1 cabal flag."

{-# WARNING
vkGetPhysicalDeviceSparseImageFormatProperties2"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceSparseImageFormatProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceSparseImageFormatInfo2* pFormatInfo
-- >     , uint32_t* pPropertyCount
-- >     , VkSparseImageFormatProperties2* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSparseImageFormatProperties2 vkGetPhysicalDeviceSparseImageFormatProperties2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
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

#else
-- |
-- > void vkGetPhysicalDeviceSparseImageFormatProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceSparseImageFormatInfo2* pFormatInfo
-- >     , uint32_t* pPropertyCount
-- >     , VkSparseImageFormatProperties2* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSparseImageFormatProperties2 vkGetPhysicalDeviceSparseImageFormatProperties2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceSparseImageFormatProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceSparseImageFormatProperties2 vkInstance
--
vkGetPhysicalDeviceSparseImageFormatProperties2Safe ::
                                                    VkPhysicalDevice -- ^ physicalDevice
                                                                     ->
                                                      Ptr VkPhysicalDeviceSparseImageFormatInfo2 -- ^ pFormatInfo
                                                                                                 ->
                                                        Ptr Word32 -- ^ pPropertyCount
                                                                   ->
                                                          Ptr VkSparseImageFormatProperties2 -- ^ pProperties
                                                                                             ->
                                                            IO ()
vkGetPhysicalDeviceSparseImageFormatProperties2Safe
  = vkGetPhysicalDeviceSparseImageFormatProperties2

{-# INLINE vkGetPhysicalDeviceSparseImageFormatProperties2Safe #-}

{-# WARNING
vkGetPhysicalDeviceSparseImageFormatProperties2Safe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkGetPhysicalDeviceSparseImageFormatProperties2
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSparseImageFormatInfo2* pFormatInfo
--   >     , uint32_t* pPropertyCount
--   >     , VkSparseImageFormatProperties2* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSparseImageFormatProperties2 vkGetPhysicalDeviceSparseImageFormatProperties2 registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkTrimCommandPool
-- >     ( VkDevice device
-- >     , VkCommandPool commandPool
-- >     , VkCommandPoolTrimFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkTrimCommandPool vkTrimCommandPool registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkTrimCommandPool" vkTrimCommandPool
               :: VkDevice -- ^ device
                           -> VkCommandPool -- ^ commandPool
                                            -> VkCommandPoolTrimFlags -- ^ flags
                                                                      -> IO ()

#else
-- |
-- > void vkTrimCommandPool
-- >     ( VkDevice device
-- >     , VkCommandPool commandPool
-- >     , VkCommandPoolTrimFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkTrimCommandPool vkTrimCommandPool registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myTrimCommandPool <- vkGetDeviceProc @VkTrimCommandPool vkDevice
--
vkTrimCommandPool ::
                  VkDevice -- ^ device
                           -> VkCommandPool -- ^ commandPool
                                            -> VkCommandPoolTrimFlags -- ^ flags
                                                                      -> IO ()
vkTrimCommandPool d
  = unsafeDupablePerformIO (vkGetDeviceProc @VkTrimCommandPool d) d

{-# INLINE vkTrimCommandPool #-}

{-# WARNING
vkTrimCommandPool"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkTrimCommandPool
-- >     ( VkDevice device
-- >     , VkCommandPool commandPool
-- >     , VkCommandPoolTrimFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkTrimCommandPool vkTrimCommandPool registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkTrimCommandPool" vkTrimCommandPoolSafe
               :: VkDevice -- ^ device
                           -> VkCommandPool -- ^ commandPool
                                            -> VkCommandPoolTrimFlags -- ^ flags
                                                                      -> IO ()

#else
-- |
-- > void vkTrimCommandPool
-- >     ( VkDevice device
-- >     , VkCommandPool commandPool
-- >     , VkCommandPoolTrimFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkTrimCommandPool vkTrimCommandPool registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myTrimCommandPool <- vkGetDeviceProc @VkTrimCommandPool vkDevice
--
vkTrimCommandPoolSafe ::
                      VkDevice -- ^ device
                               -> VkCommandPool -- ^ commandPool
                                                -> VkCommandPoolTrimFlags -- ^ flags
                                                                          -> IO ()
vkTrimCommandPoolSafe = vkTrimCommandPool

{-# INLINE vkTrimCommandPoolSafe #-}

{-# WARNING
vkTrimCommandPoolSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkTrimCommandPool
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , VkCommandPoolTrimFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkTrimCommandPool vkTrimCommandPool registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetDeviceQueue2
-- >     ( VkDevice device
-- >     , const VkDeviceQueueInfo2* pQueueInfo
-- >     , VkQueue* pQueue
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceQueue2 vkGetDeviceQueue2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkGetDeviceQueue2" vkGetDeviceQueue2
               :: VkDevice -- ^ device
                           -> Ptr VkDeviceQueueInfo2 -- ^ pQueueInfo
                                                     -> Ptr VkQueue -- ^ pQueue
                                                                    -> IO ()

#else
-- |
-- > void vkGetDeviceQueue2
-- >     ( VkDevice device
-- >     , const VkDeviceQueueInfo2* pQueueInfo
-- >     , VkQueue* pQueue
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceQueue2 vkGetDeviceQueue2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceQueue2 <- vkGetDeviceProc @VkGetDeviceQueue2 vkDevice
--
vkGetDeviceQueue2 ::
                  VkDevice -- ^ device
                           -> Ptr VkDeviceQueueInfo2 -- ^ pQueueInfo
                                                     -> Ptr VkQueue -- ^ pQueue
                                                                    -> IO ()
vkGetDeviceQueue2 d
  = unsafeDupablePerformIO (vkGetDeviceProc @VkGetDeviceQueue2 d) d

{-# INLINE vkGetDeviceQueue2 #-}

{-# WARNING
vkGetDeviceQueue2"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetDeviceQueue2
-- >     ( VkDevice device
-- >     , const VkDeviceQueueInfo2* pQueueInfo
-- >     , VkQueue* pQueue
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceQueue2 vkGetDeviceQueue2 registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkGetDeviceQueue2" vkGetDeviceQueue2Safe
               :: VkDevice -- ^ device
                           -> Ptr VkDeviceQueueInfo2 -- ^ pQueueInfo
                                                     -> Ptr VkQueue -- ^ pQueue
                                                                    -> IO ()

#else
-- |
-- > void vkGetDeviceQueue2
-- >     ( VkDevice device
-- >     , const VkDeviceQueueInfo2* pQueueInfo
-- >     , VkQueue* pQueue
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceQueue2 vkGetDeviceQueue2 registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceQueue2 <- vkGetDeviceProc @VkGetDeviceQueue2 vkDevice
--
vkGetDeviceQueue2Safe ::
                      VkDevice -- ^ device
                               -> Ptr VkDeviceQueueInfo2 -- ^ pQueueInfo
                                                         -> Ptr VkQueue -- ^ pQueue
                                                                        -> IO ()
vkGetDeviceQueue2Safe = vkGetDeviceQueue2

{-# INLINE vkGetDeviceQueue2Safe #-}

{-# WARNING
vkGetDeviceQueue2Safe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkGetDeviceQueue2
--   >     ( VkDevice device
--   >     , const VkDeviceQueueInfo2* pQueueInfo
--   >     , VkQueue* pQueue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceQueue2 vkGetDeviceQueue2 registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateSamplerYcbcrConversion
-- >     ( VkDevice device
-- >     , const VkSamplerYcbcrConversionCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSamplerYcbcrConversion* pYcbcrConversion
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSamplerYcbcrConversion vkCreateSamplerYcbcrConversion registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
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

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateSamplerYcbcrConversion
-- >     ( VkDevice device
-- >     , const VkSamplerYcbcrConversionCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSamplerYcbcrConversion* pYcbcrConversion
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSamplerYcbcrConversion vkCreateSamplerYcbcrConversion registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSamplerYcbcrConversion <- vkGetDeviceProc @VkCreateSamplerYcbcrConversion vkDevice
--
vkCreateSamplerYcbcrConversion ::
                               VkDevice -- ^ device
                                        ->
                                 Ptr VkSamplerYcbcrConversionCreateInfo -- ^ pCreateInfo
                                                                        ->
                                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                                             ->
                                     Ptr VkSamplerYcbcrConversion -- ^ pYcbcrConversion
                                                                  -> IO VkResult
vkCreateSamplerYcbcrConversion d
  = unsafeDupablePerformIO
      (vkGetDeviceProc @VkCreateSamplerYcbcrConversion d)
      d

{-# INLINE vkCreateSamplerYcbcrConversion #-}

{-# WARNING
vkCreateSamplerYcbcrConversion"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateSamplerYcbcrConversion
-- >     ( VkDevice device
-- >     , const VkSamplerYcbcrConversionCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSamplerYcbcrConversion* pYcbcrConversion
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSamplerYcbcrConversion vkCreateSamplerYcbcrConversion registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
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

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateSamplerYcbcrConversion
-- >     ( VkDevice device
-- >     , const VkSamplerYcbcrConversionCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSamplerYcbcrConversion* pYcbcrConversion
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSamplerYcbcrConversion vkCreateSamplerYcbcrConversion registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSamplerYcbcrConversion <- vkGetDeviceProc @VkCreateSamplerYcbcrConversion vkDevice
--
vkCreateSamplerYcbcrConversionSafe ::
                                   VkDevice -- ^ device
                                            ->
                                     Ptr VkSamplerYcbcrConversionCreateInfo -- ^ pCreateInfo
                                                                            ->
                                       Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                 ->
                                         Ptr VkSamplerYcbcrConversion -- ^ pYcbcrConversion
                                                                      -> IO VkResult
vkCreateSamplerYcbcrConversionSafe = vkCreateSamplerYcbcrConversion

{-# INLINE vkCreateSamplerYcbcrConversionSafe #-}

{-# WARNING
vkCreateSamplerYcbcrConversionSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSamplerYcbcrConversion vkCreateSamplerYcbcrConversion registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkDestroySamplerYcbcrConversion
-- >     ( VkDevice device
-- >     , VkSamplerYcbcrConversion ycbcrConversion
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySamplerYcbcrConversion vkDestroySamplerYcbcrConversion registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkDestroySamplerYcbcrConversion"
               vkDestroySamplerYcbcrConversion ::
               VkDevice -- ^ device
                        ->
                 VkSamplerYcbcrConversion -- ^ ycbcrConversion
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

#else
-- |
-- > void vkDestroySamplerYcbcrConversion
-- >     ( VkDevice device
-- >     , VkSamplerYcbcrConversion ycbcrConversion
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySamplerYcbcrConversion vkDestroySamplerYcbcrConversion registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroySamplerYcbcrConversion <- vkGetDeviceProc @VkDestroySamplerYcbcrConversion vkDevice
--
vkDestroySamplerYcbcrConversion ::
                                VkDevice -- ^ device
                                         ->
                                  VkSamplerYcbcrConversion -- ^ ycbcrConversion
                                                           -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                        -> IO ()
vkDestroySamplerYcbcrConversion d
  = unsafeDupablePerformIO
      (vkGetDeviceProc @VkDestroySamplerYcbcrConversion d)
      d

{-# INLINE vkDestroySamplerYcbcrConversion #-}

{-# WARNING
vkDestroySamplerYcbcrConversion"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkDestroySamplerYcbcrConversion
-- >     ( VkDevice device
-- >     , VkSamplerYcbcrConversion ycbcrConversion
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySamplerYcbcrConversion vkDestroySamplerYcbcrConversion registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkDestroySamplerYcbcrConversion"
               vkDestroySamplerYcbcrConversionSafe ::
               VkDevice -- ^ device
                        ->
                 VkSamplerYcbcrConversion -- ^ ycbcrConversion
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

#else
-- |
-- > void vkDestroySamplerYcbcrConversion
-- >     ( VkDevice device
-- >     , VkSamplerYcbcrConversion ycbcrConversion
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySamplerYcbcrConversion vkDestroySamplerYcbcrConversion registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroySamplerYcbcrConversion <- vkGetDeviceProc @VkDestroySamplerYcbcrConversion vkDevice
--
vkDestroySamplerYcbcrConversionSafe ::
                                    VkDevice -- ^ device
                                             ->
                                      VkSamplerYcbcrConversion -- ^ ycbcrConversion
                                                               -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                            -> IO ()
vkDestroySamplerYcbcrConversionSafe
  = vkDestroySamplerYcbcrConversion

{-# INLINE vkDestroySamplerYcbcrConversionSafe #-}

{-# WARNING
vkDestroySamplerYcbcrConversionSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkDestroySamplerYcbcrConversion
--   >     ( VkDevice device
--   >     , VkSamplerYcbcrConversion ycbcrConversion
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySamplerYcbcrConversion vkDestroySamplerYcbcrConversion registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateDescriptorUpdateTemplate
-- >     ( VkDevice device
-- >     , const VkDescriptorUpdateTemplateCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkDescriptorUpdateTemplate* pDescriptorUpdateTemplate
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDescriptorUpdateTemplate vkCreateDescriptorUpdateTemplate registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
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

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateDescriptorUpdateTemplate
-- >     ( VkDevice device
-- >     , const VkDescriptorUpdateTemplateCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkDescriptorUpdateTemplate* pDescriptorUpdateTemplate
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDescriptorUpdateTemplate vkCreateDescriptorUpdateTemplate registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateDescriptorUpdateTemplate <- vkGetDeviceProc @VkCreateDescriptorUpdateTemplate vkDevice
--
vkCreateDescriptorUpdateTemplate ::
                                 VkDevice -- ^ device
                                          ->
                                   Ptr VkDescriptorUpdateTemplateCreateInfo -- ^ pCreateInfo
                                                                            ->
                                     Ptr VkAllocationCallbacks -- ^ pAllocator
                                                               ->
                                       Ptr VkDescriptorUpdateTemplate -- ^ pDescriptorUpdateTemplate
                                                                      -> IO VkResult
vkCreateDescriptorUpdateTemplate d
  = unsafeDupablePerformIO
      (vkGetDeviceProc @VkCreateDescriptorUpdateTemplate d)
      d

{-# INLINE vkCreateDescriptorUpdateTemplate #-}

{-# WARNING
vkCreateDescriptorUpdateTemplate"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateDescriptorUpdateTemplate
-- >     ( VkDevice device
-- >     , const VkDescriptorUpdateTemplateCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkDescriptorUpdateTemplate* pDescriptorUpdateTemplate
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDescriptorUpdateTemplate vkCreateDescriptorUpdateTemplate registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
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

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateDescriptorUpdateTemplate
-- >     ( VkDevice device
-- >     , const VkDescriptorUpdateTemplateCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkDescriptorUpdateTemplate* pDescriptorUpdateTemplate
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDescriptorUpdateTemplate vkCreateDescriptorUpdateTemplate registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateDescriptorUpdateTemplate <- vkGetDeviceProc @VkCreateDescriptorUpdateTemplate vkDevice
--
vkCreateDescriptorUpdateTemplateSafe ::
                                     VkDevice -- ^ device
                                              ->
                                       Ptr VkDescriptorUpdateTemplateCreateInfo -- ^ pCreateInfo
                                                                                ->
                                         Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                   ->
                                           Ptr VkDescriptorUpdateTemplate -- ^ pDescriptorUpdateTemplate
                                                                          -> IO VkResult
vkCreateDescriptorUpdateTemplateSafe
  = vkCreateDescriptorUpdateTemplate

{-# INLINE vkCreateDescriptorUpdateTemplateSafe #-}

{-# WARNING
vkCreateDescriptorUpdateTemplateSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDescriptorUpdateTemplate vkCreateDescriptorUpdateTemplate registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkDestroyDescriptorUpdateTemplate
-- >     ( VkDevice device
-- >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDescriptorUpdateTemplate vkDestroyDescriptorUpdateTemplate registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkDestroyDescriptorUpdateTemplate"
               vkDestroyDescriptorUpdateTemplate ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

#else
-- |
-- > void vkDestroyDescriptorUpdateTemplate
-- >     ( VkDevice device
-- >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDescriptorUpdateTemplate vkDestroyDescriptorUpdateTemplate registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyDescriptorUpdateTemplate <- vkGetDeviceProc @VkDestroyDescriptorUpdateTemplate vkDevice
--
vkDestroyDescriptorUpdateTemplate ::
                                  VkDevice -- ^ device
                                           ->
                                    VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                               -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                            -> IO ()
vkDestroyDescriptorUpdateTemplate d
  = unsafeDupablePerformIO
      (vkGetDeviceProc @VkDestroyDescriptorUpdateTemplate d)
      d

{-# INLINE vkDestroyDescriptorUpdateTemplate #-}

{-# WARNING
vkDestroyDescriptorUpdateTemplate"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkDestroyDescriptorUpdateTemplate
-- >     ( VkDevice device
-- >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDescriptorUpdateTemplate vkDestroyDescriptorUpdateTemplate registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkDestroyDescriptorUpdateTemplate"
               vkDestroyDescriptorUpdateTemplateSafe ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

#else
-- |
-- > void vkDestroyDescriptorUpdateTemplate
-- >     ( VkDevice device
-- >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDescriptorUpdateTemplate vkDestroyDescriptorUpdateTemplate registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyDescriptorUpdateTemplate <- vkGetDeviceProc @VkDestroyDescriptorUpdateTemplate vkDevice
--
vkDestroyDescriptorUpdateTemplateSafe ::
                                      VkDevice -- ^ device
                                               ->
                                        VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                                   ->
                                          Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()
vkDestroyDescriptorUpdateTemplateSafe
  = vkDestroyDescriptorUpdateTemplate

{-# INLINE vkDestroyDescriptorUpdateTemplateSafe #-}

{-# WARNING
vkDestroyDescriptorUpdateTemplateSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkDestroyDescriptorUpdateTemplate
--   >     ( VkDevice device
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDescriptorUpdateTemplate vkDestroyDescriptorUpdateTemplate registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkUpdateDescriptorSetWithTemplate
-- >     ( VkDevice device
-- >     , VkDescriptorSet descriptorSet
-- >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
-- >     , const void* pData
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkUpdateDescriptorSetWithTemplate vkUpdateDescriptorSetWithTemplate registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkUpdateDescriptorSetWithTemplate"
               vkUpdateDescriptorSetWithTemplate ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorSet -- ^ descriptorSet
                                 -> VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                               -> Ptr Void -- ^ pData
                                                                           -> IO ()

#else
-- |
-- > void vkUpdateDescriptorSetWithTemplate
-- >     ( VkDevice device
-- >     , VkDescriptorSet descriptorSet
-- >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
-- >     , const void* pData
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkUpdateDescriptorSetWithTemplate vkUpdateDescriptorSetWithTemplate registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myUpdateDescriptorSetWithTemplate <- vkGetDeviceProc @VkUpdateDescriptorSetWithTemplate vkDevice
--
vkUpdateDescriptorSetWithTemplate ::
                                  VkDevice -- ^ device
                                           ->
                                    VkDescriptorSet -- ^ descriptorSet
                                                    ->
                                      VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                                 -> Ptr Void -- ^ pData
                                                                             -> IO ()
vkUpdateDescriptorSetWithTemplate d
  = unsafeDupablePerformIO
      (vkGetDeviceProc @VkUpdateDescriptorSetWithTemplate d)
      d

{-# INLINE vkUpdateDescriptorSetWithTemplate #-}

{-# WARNING
vkUpdateDescriptorSetWithTemplate"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkUpdateDescriptorSetWithTemplate
-- >     ( VkDevice device
-- >     , VkDescriptorSet descriptorSet
-- >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
-- >     , const void* pData
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkUpdateDescriptorSetWithTemplate vkUpdateDescriptorSetWithTemplate registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkUpdateDescriptorSetWithTemplate"
               vkUpdateDescriptorSetWithTemplateSafe ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorSet -- ^ descriptorSet
                                 -> VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                               -> Ptr Void -- ^ pData
                                                                           -> IO ()

#else
-- |
-- > void vkUpdateDescriptorSetWithTemplate
-- >     ( VkDevice device
-- >     , VkDescriptorSet descriptorSet
-- >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
-- >     , const void* pData
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkUpdateDescriptorSetWithTemplate vkUpdateDescriptorSetWithTemplate registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myUpdateDescriptorSetWithTemplate <- vkGetDeviceProc @VkUpdateDescriptorSetWithTemplate vkDevice
--
vkUpdateDescriptorSetWithTemplateSafe ::
                                      VkDevice -- ^ device
                                               ->
                                        VkDescriptorSet -- ^ descriptorSet
                                                        ->
                                          VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                                     -> Ptr Void -- ^ pData
                                                                                 -> IO ()
vkUpdateDescriptorSetWithTemplateSafe
  = vkUpdateDescriptorSetWithTemplate

{-# INLINE vkUpdateDescriptorSetWithTemplateSafe #-}

{-# WARNING
vkUpdateDescriptorSetWithTemplateSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkUpdateDescriptorSetWithTemplate
--   >     ( VkDevice device
--   >     , VkDescriptorSet descriptorSet
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkUpdateDescriptorSetWithTemplate vkUpdateDescriptorSetWithTemplate registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceExternalBufferProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalBufferInfo* pExternalBufferInfo
-- >     , VkExternalBufferProperties* pExternalBufferProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalBufferProperties vkGetPhysicalDeviceExternalBufferProperties registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalBufferProperties"
               vkGetPhysicalDeviceExternalBufferProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalBufferInfo -- ^ pExternalBufferInfo
                                                        ->
                   Ptr VkExternalBufferProperties -- ^ pExternalBufferProperties
                                                  -> IO ()

#else
-- |
-- > void vkGetPhysicalDeviceExternalBufferProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalBufferInfo* pExternalBufferInfo
-- >     , VkExternalBufferProperties* pExternalBufferProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalBufferProperties vkGetPhysicalDeviceExternalBufferProperties registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceExternalBufferProperties <- vkGetInstanceProc @VkGetPhysicalDeviceExternalBufferProperties vkInstance
--
vkGetPhysicalDeviceExternalBufferProperties ::
                                            VkPhysicalDevice -- ^ physicalDevice
                                                             ->
                                              Ptr VkPhysicalDeviceExternalBufferInfo -- ^ pExternalBufferInfo
                                                                                     ->
                                                Ptr VkExternalBufferProperties -- ^ pExternalBufferProperties
                                                                               -> IO ()
vkGetPhysicalDeviceExternalBufferProperties
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDeviceExternalBufferProperties\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-1 cabal flag."

{-# WARNING
vkGetPhysicalDeviceExternalBufferProperties"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceExternalBufferProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalBufferInfo* pExternalBufferInfo
-- >     , VkExternalBufferProperties* pExternalBufferProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalBufferProperties vkGetPhysicalDeviceExternalBufferProperties registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe
               "vkGetPhysicalDeviceExternalBufferProperties"
               vkGetPhysicalDeviceExternalBufferPropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalBufferInfo -- ^ pExternalBufferInfo
                                                        ->
                   Ptr VkExternalBufferProperties -- ^ pExternalBufferProperties
                                                  -> IO ()

#else
-- |
-- > void vkGetPhysicalDeviceExternalBufferProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalBufferInfo* pExternalBufferInfo
-- >     , VkExternalBufferProperties* pExternalBufferProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalBufferProperties vkGetPhysicalDeviceExternalBufferProperties registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceExternalBufferProperties <- vkGetInstanceProc @VkGetPhysicalDeviceExternalBufferProperties vkInstance
--
vkGetPhysicalDeviceExternalBufferPropertiesSafe ::
                                                VkPhysicalDevice -- ^ physicalDevice
                                                                 ->
                                                  Ptr VkPhysicalDeviceExternalBufferInfo -- ^ pExternalBufferInfo
                                                                                         ->
                                                    Ptr VkExternalBufferProperties -- ^ pExternalBufferProperties
                                                                                   -> IO ()
vkGetPhysicalDeviceExternalBufferPropertiesSafe
  = vkGetPhysicalDeviceExternalBufferProperties

{-# INLINE vkGetPhysicalDeviceExternalBufferPropertiesSafe #-}

{-# WARNING
vkGetPhysicalDeviceExternalBufferPropertiesSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkGetPhysicalDeviceExternalBufferProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalBufferInfo* pExternalBufferInfo
--   >     , VkExternalBufferProperties* pExternalBufferProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalBufferProperties vkGetPhysicalDeviceExternalBufferProperties registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceExternalFenceProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalFenceInfo* pExternalFenceInfo
-- >     , VkExternalFenceProperties* pExternalFenceProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalFenceProperties vkGetPhysicalDeviceExternalFenceProperties registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalFenceProperties"
               vkGetPhysicalDeviceExternalFenceProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalFenceInfo -- ^ pExternalFenceInfo
                                                       ->
                   Ptr VkExternalFenceProperties -- ^ pExternalFenceProperties
                                                 -> IO ()

#else
-- |
-- > void vkGetPhysicalDeviceExternalFenceProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalFenceInfo* pExternalFenceInfo
-- >     , VkExternalFenceProperties* pExternalFenceProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalFenceProperties vkGetPhysicalDeviceExternalFenceProperties registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceExternalFenceProperties <- vkGetInstanceProc @VkGetPhysicalDeviceExternalFenceProperties vkInstance
--
vkGetPhysicalDeviceExternalFenceProperties ::
                                           VkPhysicalDevice -- ^ physicalDevice
                                                            ->
                                             Ptr VkPhysicalDeviceExternalFenceInfo -- ^ pExternalFenceInfo
                                                                                   ->
                                               Ptr VkExternalFenceProperties -- ^ pExternalFenceProperties
                                                                             -> IO ()
vkGetPhysicalDeviceExternalFenceProperties
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDeviceExternalFenceProperties\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-1 cabal flag."

{-# WARNING
vkGetPhysicalDeviceExternalFenceProperties"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceExternalFenceProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalFenceInfo* pExternalFenceInfo
-- >     , VkExternalFenceProperties* pExternalFenceProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalFenceProperties vkGetPhysicalDeviceExternalFenceProperties registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe
               "vkGetPhysicalDeviceExternalFenceProperties"
               vkGetPhysicalDeviceExternalFencePropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalFenceInfo -- ^ pExternalFenceInfo
                                                       ->
                   Ptr VkExternalFenceProperties -- ^ pExternalFenceProperties
                                                 -> IO ()

#else
-- |
-- > void vkGetPhysicalDeviceExternalFenceProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalFenceInfo* pExternalFenceInfo
-- >     , VkExternalFenceProperties* pExternalFenceProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalFenceProperties vkGetPhysicalDeviceExternalFenceProperties registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceExternalFenceProperties <- vkGetInstanceProc @VkGetPhysicalDeviceExternalFenceProperties vkInstance
--
vkGetPhysicalDeviceExternalFencePropertiesSafe ::
                                               VkPhysicalDevice -- ^ physicalDevice
                                                                ->
                                                 Ptr VkPhysicalDeviceExternalFenceInfo -- ^ pExternalFenceInfo
                                                                                       ->
                                                   Ptr VkExternalFenceProperties -- ^ pExternalFenceProperties
                                                                                 -> IO ()
vkGetPhysicalDeviceExternalFencePropertiesSafe
  = vkGetPhysicalDeviceExternalFenceProperties

{-# INLINE vkGetPhysicalDeviceExternalFencePropertiesSafe #-}

{-# WARNING
vkGetPhysicalDeviceExternalFencePropertiesSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkGetPhysicalDeviceExternalFenceProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalFenceInfo* pExternalFenceInfo
--   >     , VkExternalFenceProperties* pExternalFenceProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalFenceProperties vkGetPhysicalDeviceExternalFenceProperties registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceExternalSemaphoreProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalSemaphoreInfo* pExternalSemaphoreInfo
-- >     , VkExternalSemaphoreProperties* pExternalSemaphoreProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalSemaphoreProperties vkGetPhysicalDeviceExternalSemaphoreProperties registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalSemaphoreProperties"
               vkGetPhysicalDeviceExternalSemaphoreProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalSemaphoreInfo -- ^ pExternalSemaphoreInfo
                                                           ->
                   Ptr VkExternalSemaphoreProperties -- ^ pExternalSemaphoreProperties
                                                     -> IO ()

#else
-- |
-- > void vkGetPhysicalDeviceExternalSemaphoreProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalSemaphoreInfo* pExternalSemaphoreInfo
-- >     , VkExternalSemaphoreProperties* pExternalSemaphoreProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalSemaphoreProperties vkGetPhysicalDeviceExternalSemaphoreProperties registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceExternalSemaphoreProperties <- vkGetInstanceProc @VkGetPhysicalDeviceExternalSemaphoreProperties vkInstance
--
vkGetPhysicalDeviceExternalSemaphoreProperties ::
                                               VkPhysicalDevice -- ^ physicalDevice
                                                                ->
                                                 Ptr VkPhysicalDeviceExternalSemaphoreInfo -- ^ pExternalSemaphoreInfo
                                                                                           ->
                                                   Ptr VkExternalSemaphoreProperties -- ^ pExternalSemaphoreProperties
                                                                                     -> IO ()
vkGetPhysicalDeviceExternalSemaphoreProperties
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDeviceExternalSemaphoreProperties\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-1 cabal flag."

{-# WARNING
vkGetPhysicalDeviceExternalSemaphoreProperties"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetPhysicalDeviceExternalSemaphoreProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalSemaphoreInfo* pExternalSemaphoreInfo
-- >     , VkExternalSemaphoreProperties* pExternalSemaphoreProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalSemaphoreProperties vkGetPhysicalDeviceExternalSemaphoreProperties registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe
               "vkGetPhysicalDeviceExternalSemaphoreProperties"
               vkGetPhysicalDeviceExternalSemaphorePropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalSemaphoreInfo -- ^ pExternalSemaphoreInfo
                                                           ->
                   Ptr VkExternalSemaphoreProperties -- ^ pExternalSemaphoreProperties
                                                     -> IO ()

#else
-- |
-- > void vkGetPhysicalDeviceExternalSemaphoreProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalSemaphoreInfo* pExternalSemaphoreInfo
-- >     , VkExternalSemaphoreProperties* pExternalSemaphoreProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalSemaphoreProperties vkGetPhysicalDeviceExternalSemaphoreProperties registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceExternalSemaphoreProperties <- vkGetInstanceProc @VkGetPhysicalDeviceExternalSemaphoreProperties vkInstance
--
vkGetPhysicalDeviceExternalSemaphorePropertiesSafe ::
                                                   VkPhysicalDevice -- ^ physicalDevice
                                                                    ->
                                                     Ptr VkPhysicalDeviceExternalSemaphoreInfo -- ^ pExternalSemaphoreInfo
                                                                                               ->
                                                       Ptr VkExternalSemaphoreProperties -- ^ pExternalSemaphoreProperties
                                                                                         -> IO ()
vkGetPhysicalDeviceExternalSemaphorePropertiesSafe
  = vkGetPhysicalDeviceExternalSemaphoreProperties

{-# INLINE vkGetPhysicalDeviceExternalSemaphorePropertiesSafe #-}

{-# WARNING
vkGetPhysicalDeviceExternalSemaphorePropertiesSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkGetPhysicalDeviceExternalSemaphoreProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalSemaphoreInfo* pExternalSemaphoreInfo
--   >     , VkExternalSemaphoreProperties* pExternalSemaphoreProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalSemaphoreProperties vkGetPhysicalDeviceExternalSemaphoreProperties registry at www.khronos.org>
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetDescriptorSetLayoutSupport
-- >     ( VkDevice device
-- >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
-- >     , VkDescriptorSetLayoutSupport* pSupport
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDescriptorSetLayoutSupport vkGetDescriptorSetLayoutSupport registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkGetDescriptorSetLayoutSupport"
               vkGetDescriptorSetLayoutSupport ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                     ->
                   Ptr VkDescriptorSetLayoutSupport -- ^ pSupport
                                                    -> IO ()

#else
-- |
-- > void vkGetDescriptorSetLayoutSupport
-- >     ( VkDevice device
-- >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
-- >     , VkDescriptorSetLayoutSupport* pSupport
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDescriptorSetLayoutSupport vkGetDescriptorSetLayoutSupport registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDescriptorSetLayoutSupport <- vkGetDeviceProc @VkGetDescriptorSetLayoutSupport vkDevice
--
vkGetDescriptorSetLayoutSupport ::
                                VkDevice -- ^ device
                                         ->
                                  Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                                      ->
                                    Ptr VkDescriptorSetLayoutSupport -- ^ pSupport
                                                                     -> IO ()
vkGetDescriptorSetLayoutSupport d
  = unsafeDupablePerformIO
      (vkGetDeviceProc @VkGetDescriptorSetLayoutSupport d)
      d

{-# INLINE vkGetDescriptorSetLayoutSupport #-}

{-# WARNING
vkGetDescriptorSetLayoutSupport"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
-- |
-- > void vkGetDescriptorSetLayoutSupport
-- >     ( VkDevice device
-- >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
-- >     , VkDescriptorSetLayoutSupport* pSupport
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDescriptorSetLayoutSupport vkGetDescriptorSetLayoutSupport registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkGetDescriptorSetLayoutSupport"
               vkGetDescriptorSetLayoutSupportSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                     ->
                   Ptr VkDescriptorSetLayoutSupport -- ^ pSupport
                                                    -> IO ()

#else
-- |
-- > void vkGetDescriptorSetLayoutSupport
-- >     ( VkDevice device
-- >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
-- >     , VkDescriptorSetLayoutSupport* pSupport
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDescriptorSetLayoutSupport vkGetDescriptorSetLayoutSupport registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDescriptorSetLayoutSupport <- vkGetDeviceProc @VkGetDescriptorSetLayoutSupport vkDevice
--
vkGetDescriptorSetLayoutSupportSafe ::
                                    VkDevice -- ^ device
                                             ->
                                      Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                                          ->
                                        Ptr VkDescriptorSetLayoutSupport -- ^ pSupport
                                                                         -> IO ()
vkGetDescriptorSetLayoutSupportSafe
  = vkGetDescriptorSetLayoutSupport

{-# INLINE vkGetDescriptorSetLayoutSupportSafe #-}

{-# WARNING
vkGetDescriptorSetLayoutSupportSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

-- | > void vkGetDescriptorSetLayoutSupport
--   >     ( VkDevice device
--   >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
--   >     , VkDescriptorSetLayoutSupport* pSupport
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDescriptorSetLayoutSupport vkGetDescriptorSetLayoutSupport registry at www.khronos.org>
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
