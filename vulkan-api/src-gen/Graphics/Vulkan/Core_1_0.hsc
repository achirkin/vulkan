#include "vulkan/vulkan.h"

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
module Graphics.Vulkan.Core_1_0
       (-- * Vulkan core API interface definitions
        -- |
        --
        -- @api = vulkan@
        --
        -- @name = VK_VERSION_1_0@
        --
        -- @number = 1.0@
        --

        -- ** Header boilerplate
        --
        -- |
        -- > ##include "vk_platform.h"
        --

        -- ** API version
        --

        -- ** API constants
        module Graphics.Vulkan.Types.Enum.VkPipelineCacheHeaderVersion,
        pattern VK_LOD_CLAMP_NONE, pattern VK_REMAINING_MIP_LEVELS,
        pattern VK_REMAINING_ARRAY_LAYERS, pattern VK_WHOLE_SIZE,
        pattern VK_ATTACHMENT_UNUSED, pattern VK_TRUE, pattern VK_FALSE,
        pattern VK_QUEUE_FAMILY_IGNORED, pattern VK_SUBPASS_EXTERNAL,
        -- ** Device initialization
        VkCreateInstance, pattern VkCreateInstance, HS_vkCreateInstance,
        PFN_vkCreateInstance, unwrapVkCreateInstance, vkCreateInstance,
        vkCreateInstanceSafe, VkDestroyInstance, pattern VkDestroyInstance,
        HS_vkDestroyInstance, PFN_vkDestroyInstance,
        unwrapVkDestroyInstance, vkDestroyInstance, vkDestroyInstanceSafe,
        VkEnumeratePhysicalDevices, pattern VkEnumeratePhysicalDevices,
        HS_vkEnumeratePhysicalDevices, PFN_vkEnumeratePhysicalDevices,
        unwrapVkEnumeratePhysicalDevices, vkEnumeratePhysicalDevices,
        vkEnumeratePhysicalDevicesSafe, VkGetPhysicalDeviceFeatures,
        pattern VkGetPhysicalDeviceFeatures,
        HS_vkGetPhysicalDeviceFeatures, PFN_vkGetPhysicalDeviceFeatures,
        unwrapVkGetPhysicalDeviceFeatures, vkGetPhysicalDeviceFeatures,
        vkGetPhysicalDeviceFeaturesSafe,
        VkGetPhysicalDeviceFormatProperties,
        pattern VkGetPhysicalDeviceFormatProperties,
        HS_vkGetPhysicalDeviceFormatProperties,
        PFN_vkGetPhysicalDeviceFormatProperties,
        unwrapVkGetPhysicalDeviceFormatProperties,
        vkGetPhysicalDeviceFormatProperties,
        vkGetPhysicalDeviceFormatPropertiesSafe,
        VkGetPhysicalDeviceImageFormatProperties,
        pattern VkGetPhysicalDeviceImageFormatProperties,
        HS_vkGetPhysicalDeviceImageFormatProperties,
        PFN_vkGetPhysicalDeviceImageFormatProperties,
        unwrapVkGetPhysicalDeviceImageFormatProperties,
        vkGetPhysicalDeviceImageFormatProperties,
        vkGetPhysicalDeviceImageFormatPropertiesSafe,
        VkGetPhysicalDeviceProperties,
        pattern VkGetPhysicalDeviceProperties,
        HS_vkGetPhysicalDeviceProperties,
        PFN_vkGetPhysicalDeviceProperties,
        unwrapVkGetPhysicalDeviceProperties, vkGetPhysicalDeviceProperties,
        vkGetPhysicalDevicePropertiesSafe,
        VkGetPhysicalDeviceQueueFamilyProperties,
        pattern VkGetPhysicalDeviceQueueFamilyProperties,
        HS_vkGetPhysicalDeviceQueueFamilyProperties,
        PFN_vkGetPhysicalDeviceQueueFamilyProperties,
        unwrapVkGetPhysicalDeviceQueueFamilyProperties,
        vkGetPhysicalDeviceQueueFamilyProperties,
        vkGetPhysicalDeviceQueueFamilyPropertiesSafe,
        VkGetPhysicalDeviceMemoryProperties,
        pattern VkGetPhysicalDeviceMemoryProperties,
        HS_vkGetPhysicalDeviceMemoryProperties,
        PFN_vkGetPhysicalDeviceMemoryProperties,
        unwrapVkGetPhysicalDeviceMemoryProperties,
        vkGetPhysicalDeviceMemoryProperties,
        vkGetPhysicalDeviceMemoryPropertiesSafe, VkGetInstanceProcAddr,
        pattern VkGetInstanceProcAddr, HS_vkGetInstanceProcAddr,
        PFN_vkGetInstanceProcAddr, unwrapVkGetInstanceProcAddr,
        vkGetInstanceProcAddr, vkGetInstanceProcAddrSafe,
        VkGetDeviceProcAddr, pattern VkGetDeviceProcAddr,
        HS_vkGetDeviceProcAddr, PFN_vkGetDeviceProcAddr,
        unwrapVkGetDeviceProcAddr, vkGetDeviceProcAddr,
        vkGetDeviceProcAddrSafe, module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkFormatFeatureFlags,
        module Graphics.Vulkan.Types.Enum.VkImageCreateFlags,
        module Graphics.Vulkan.Types.Enum.VkImageTiling,
        module Graphics.Vulkan.Types.Enum.VkImageType,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkMemoryHeapFlags,
        module Graphics.Vulkan.Types.Enum.VkMemoryPropertyFlags,
        module Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType,
        module Graphics.Vulkan.Types.Enum.VkQueueFlags,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        module Graphics.Vulkan.Types.Struct.VkApplicationInfo,
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.Struct.VkFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkInstanceCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkMemoryHeap,
        module Graphics.Vulkan.Types.Struct.VkMemoryType,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties,
        module Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties,
        -- ** Device commands
        VkCreateDevice, pattern VkCreateDevice, HS_vkCreateDevice,
        PFN_vkCreateDevice, unwrapVkCreateDevice, vkCreateDevice,
        vkCreateDeviceSafe, VkDestroyDevice, pattern VkDestroyDevice,
        HS_vkDestroyDevice, PFN_vkDestroyDevice, unwrapVkDestroyDevice,
        vkDestroyDevice, vkDestroyDeviceSafe,
        module Graphics.Vulkan.Types.Enum.VkDeviceQueueCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo,
        -- ** Extension discovery commands
        VkEnumerateInstanceExtensionProperties,
        pattern VkEnumerateInstanceExtensionProperties,
        HS_vkEnumerateInstanceExtensionProperties,
        PFN_vkEnumerateInstanceExtensionProperties,
        unwrapVkEnumerateInstanceExtensionProperties,
        vkEnumerateInstanceExtensionProperties,
        vkEnumerateInstanceExtensionPropertiesSafe,
        VkEnumerateDeviceExtensionProperties,
        pattern VkEnumerateDeviceExtensionProperties,
        HS_vkEnumerateDeviceExtensionProperties,
        PFN_vkEnumerateDeviceExtensionProperties,
        unwrapVkEnumerateDeviceExtensionProperties,
        vkEnumerateDeviceExtensionProperties,
        vkEnumerateDeviceExtensionPropertiesSafe,
        module Graphics.Vulkan.Types.Struct.VkExtensionProperties,
        -- ** Layer discovery commands
        VkEnumerateInstanceLayerProperties,
        pattern VkEnumerateInstanceLayerProperties,
        HS_vkEnumerateInstanceLayerProperties,
        PFN_vkEnumerateInstanceLayerProperties,
        unwrapVkEnumerateInstanceLayerProperties,
        vkEnumerateInstanceLayerProperties,
        vkEnumerateInstanceLayerPropertiesSafe,
        VkEnumerateDeviceLayerProperties,
        pattern VkEnumerateDeviceLayerProperties,
        HS_vkEnumerateDeviceLayerProperties,
        PFN_vkEnumerateDeviceLayerProperties,
        unwrapVkEnumerateDeviceLayerProperties,
        vkEnumerateDeviceLayerProperties,
        vkEnumerateDeviceLayerPropertiesSafe,
        module Graphics.Vulkan.Types.Struct.VkLayerProperties,
        -- ** queue commands
        VkGetDeviceQueue, pattern VkGetDeviceQueue, HS_vkGetDeviceQueue,
        PFN_vkGetDeviceQueue, unwrapVkGetDeviceQueue, vkGetDeviceQueue,
        vkGetDeviceQueueSafe, VkQueueSubmit, pattern VkQueueSubmit,
        HS_vkQueueSubmit, PFN_vkQueueSubmit, unwrapVkQueueSubmit,
        vkQueueSubmit, vkQueueSubmitSafe, VkQueueWaitIdle,
        pattern VkQueueWaitIdle, HS_vkQueueWaitIdle, PFN_vkQueueWaitIdle,
        unwrapVkQueueWaitIdle, vkQueueWaitIdle, vkQueueWaitIdleSafe,
        VkDeviceWaitIdle, pattern VkDeviceWaitIdle, HS_vkDeviceWaitIdle,
        PFN_vkDeviceWaitIdle, unwrapVkDeviceWaitIdle, vkDeviceWaitIdle,
        vkDeviceWaitIdleSafe,
        module Graphics.Vulkan.Types.Enum.VkPipelineStageFlags,
        module Graphics.Vulkan.Types.Struct.VkSubmitInfo, -- ** Memory commands
                                                          VkAllocateMemory,
        pattern VkAllocateMemory, HS_vkAllocateMemory,
        PFN_vkAllocateMemory, unwrapVkAllocateMemory, vkAllocateMemory,
        vkAllocateMemorySafe, VkFreeMemory, pattern VkFreeMemory,
        HS_vkFreeMemory, PFN_vkFreeMemory, unwrapVkFreeMemory,
        vkFreeMemory, vkFreeMemorySafe, VkMapMemory, pattern VkMapMemory,
        HS_vkMapMemory, PFN_vkMapMemory, unwrapVkMapMemory, vkMapMemory,
        vkMapMemorySafe, VkUnmapMemory, pattern VkUnmapMemory,
        HS_vkUnmapMemory, PFN_vkUnmapMemory, unwrapVkUnmapMemory,
        vkUnmapMemory, vkUnmapMemorySafe, VkFlushMappedMemoryRanges,
        pattern VkFlushMappedMemoryRanges, HS_vkFlushMappedMemoryRanges,
        PFN_vkFlushMappedMemoryRanges, unwrapVkFlushMappedMemoryRanges,
        vkFlushMappedMemoryRanges, vkFlushMappedMemoryRangesSafe,
        VkInvalidateMappedMemoryRanges,
        pattern VkInvalidateMappedMemoryRanges,
        HS_vkInvalidateMappedMemoryRanges,
        PFN_vkInvalidateMappedMemoryRanges,
        unwrapVkInvalidateMappedMemoryRanges,
        vkInvalidateMappedMemoryRanges, vkInvalidateMappedMemoryRangesSafe,
        VkGetDeviceMemoryCommitment, pattern VkGetDeviceMemoryCommitment,
        HS_vkGetDeviceMemoryCommitment, PFN_vkGetDeviceMemoryCommitment,
        unwrapVkGetDeviceMemoryCommitment, vkGetDeviceMemoryCommitment,
        vkGetDeviceMemoryCommitmentSafe,
        module Graphics.Vulkan.Types.Struct.VkMappedMemoryRange,
        module Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo,
        -- ** Memory management API commands
        VkBindBufferMemory, pattern VkBindBufferMemory,
        HS_vkBindBufferMemory, PFN_vkBindBufferMemory,
        unwrapVkBindBufferMemory, vkBindBufferMemory,
        vkBindBufferMemorySafe, VkBindImageMemory,
        pattern VkBindImageMemory, HS_vkBindImageMemory,
        PFN_vkBindImageMemory, unwrapVkBindImageMemory, vkBindImageMemory,
        vkBindImageMemorySafe, VkGetBufferMemoryRequirements,
        pattern VkGetBufferMemoryRequirements,
        HS_vkGetBufferMemoryRequirements,
        PFN_vkGetBufferMemoryRequirements,
        unwrapVkGetBufferMemoryRequirements, vkGetBufferMemoryRequirements,
        vkGetBufferMemoryRequirementsSafe, VkGetImageMemoryRequirements,
        pattern VkGetImageMemoryRequirements,
        HS_vkGetImageMemoryRequirements, PFN_vkGetImageMemoryRequirements,
        unwrapVkGetImageMemoryRequirements, vkGetImageMemoryRequirements,
        vkGetImageMemoryRequirementsSafe,
        module Graphics.Vulkan.Types.Struct.VkMemoryRequirements,
        -- ** Sparse resource memory management API commands
        VkGetImageSparseMemoryRequirements,
        pattern VkGetImageSparseMemoryRequirements,
        HS_vkGetImageSparseMemoryRequirements,
        PFN_vkGetImageSparseMemoryRequirements,
        unwrapVkGetImageSparseMemoryRequirements,
        vkGetImageSparseMemoryRequirements,
        vkGetImageSparseMemoryRequirementsSafe,
        VkGetPhysicalDeviceSparseImageFormatProperties,
        pattern VkGetPhysicalDeviceSparseImageFormatProperties,
        HS_vkGetPhysicalDeviceSparseImageFormatProperties,
        PFN_vkGetPhysicalDeviceSparseImageFormatProperties,
        unwrapVkGetPhysicalDeviceSparseImageFormatProperties,
        vkGetPhysicalDeviceSparseImageFormatProperties,
        vkGetPhysicalDeviceSparseImageFormatPropertiesSafe,
        VkQueueBindSparse, pattern VkQueueBindSparse, HS_vkQueueBindSparse,
        PFN_vkQueueBindSparse, unwrapVkQueueBindSparse, vkQueueBindSparse,
        vkQueueBindSparseSafe,
        module Graphics.Vulkan.Types.Enum.VkImageAspectFlags,
        module Graphics.Vulkan.Types.Enum.VkSparseImageFormatFlags,
        module Graphics.Vulkan.Types.Enum.VkSparseMemoryBindFlags,
        module Graphics.Vulkan.Types.Struct.VkBindSparseInfo,
        module Graphics.Vulkan.Types.Struct.VkImageSubresource,
        module Graphics.Vulkan.Types.Struct.VkOffset3D,
        module Graphics.Vulkan.Types.Struct.VkSparseBufferMemoryBindInfo,
        module Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBind,
        module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBindInfo,
        module Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements,
        module Graphics.Vulkan.Types.Struct.VkSparseImageOpaqueMemoryBindInfo,
        module Graphics.Vulkan.Types.Struct.VkSparseMemoryBind,
        -- ** Fence commands
        VkCreateFence, pattern VkCreateFence, HS_vkCreateFence,
        PFN_vkCreateFence, unwrapVkCreateFence, vkCreateFence,
        vkCreateFenceSafe, VkDestroyFence, pattern VkDestroyFence,
        HS_vkDestroyFence, PFN_vkDestroyFence, unwrapVkDestroyFence,
        vkDestroyFence, vkDestroyFenceSafe, VkResetFences,
        pattern VkResetFences, HS_vkResetFences, PFN_vkResetFences,
        unwrapVkResetFences, vkResetFences, vkResetFencesSafe,
        VkGetFenceStatus, pattern VkGetFenceStatus, HS_vkGetFenceStatus,
        PFN_vkGetFenceStatus, unwrapVkGetFenceStatus, vkGetFenceStatus,
        vkGetFenceStatusSafe, VkWaitForFences, pattern VkWaitForFences,
        HS_vkWaitForFences, PFN_vkWaitForFences, unwrapVkWaitForFences,
        vkWaitForFences, vkWaitForFencesSafe,
        module Graphics.Vulkan.Types.Enum.VkFenceCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkFenceCreateInfo,
        -- ** Queue semaphore commands
        VkCreateSemaphore, pattern VkCreateSemaphore, HS_vkCreateSemaphore,
        PFN_vkCreateSemaphore, unwrapVkCreateSemaphore, vkCreateSemaphore,
        vkCreateSemaphoreSafe, VkDestroySemaphore,
        pattern VkDestroySemaphore, HS_vkDestroySemaphore,
        PFN_vkDestroySemaphore, unwrapVkDestroySemaphore,
        vkDestroySemaphore, vkDestroySemaphoreSafe,
        module Graphics.Vulkan.Types.Struct.VkSemaphoreCreateInfo,
        -- ** Event commands
        VkCreateEvent, pattern VkCreateEvent, HS_vkCreateEvent,
        PFN_vkCreateEvent, unwrapVkCreateEvent, vkCreateEvent,
        vkCreateEventSafe, VkDestroyEvent, pattern VkDestroyEvent,
        HS_vkDestroyEvent, PFN_vkDestroyEvent, unwrapVkDestroyEvent,
        vkDestroyEvent, vkDestroyEventSafe, VkGetEventStatus,
        pattern VkGetEventStatus, HS_vkGetEventStatus,
        PFN_vkGetEventStatus, unwrapVkGetEventStatus, vkGetEventStatus,
        vkGetEventStatusSafe, VkSetEvent, pattern VkSetEvent,
        HS_vkSetEvent, PFN_vkSetEvent, unwrapVkSetEvent, vkSetEvent,
        vkSetEventSafe, VkResetEvent, pattern VkResetEvent,
        HS_vkResetEvent, PFN_vkResetEvent, unwrapVkResetEvent,
        vkResetEvent, vkResetEventSafe,
        module Graphics.Vulkan.Types.Struct.VkEventCreateInfo,
        -- ** Query commands
        VkCreateQueryPool, pattern VkCreateQueryPool, HS_vkCreateQueryPool,
        PFN_vkCreateQueryPool, unwrapVkCreateQueryPool, vkCreateQueryPool,
        vkCreateQueryPoolSafe, VkDestroyQueryPool,
        pattern VkDestroyQueryPool, HS_vkDestroyQueryPool,
        PFN_vkDestroyQueryPool, unwrapVkDestroyQueryPool,
        vkDestroyQueryPool, vkDestroyQueryPoolSafe, VkGetQueryPoolResults,
        pattern VkGetQueryPoolResults, HS_vkGetQueryPoolResults,
        PFN_vkGetQueryPoolResults, unwrapVkGetQueryPoolResults,
        vkGetQueryPoolResults, vkGetQueryPoolResultsSafe,
        module Graphics.Vulkan.Types.Enum.VkQueryPipelineStatisticFlags,
        module Graphics.Vulkan.Types.Enum.VkQueryResultFlags,
        module Graphics.Vulkan.Types.Enum.VkQueryType,
        module Graphics.Vulkan.Types.Struct.VkQueryPoolCreateInfo,
        -- ** Buffer commands
        VkCreateBuffer, pattern VkCreateBuffer, HS_vkCreateBuffer,
        PFN_vkCreateBuffer, unwrapVkCreateBuffer, vkCreateBuffer,
        vkCreateBufferSafe, VkDestroyBuffer, pattern VkDestroyBuffer,
        HS_vkDestroyBuffer, PFN_vkDestroyBuffer, unwrapVkDestroyBuffer,
        vkDestroyBuffer, vkDestroyBufferSafe,
        module Graphics.Vulkan.Types.Enum.VkBufferCreateFlags,
        module Graphics.Vulkan.Types.Enum.VkBufferUsageFlags,
        module Graphics.Vulkan.Types.Enum.VkSharingMode,
        module Graphics.Vulkan.Types.Struct.VkBufferCreateInfo,
        -- ** Buffer view commands
        VkCreateBufferView, pattern VkCreateBufferView,
        HS_vkCreateBufferView, PFN_vkCreateBufferView,
        unwrapVkCreateBufferView, vkCreateBufferView,
        vkCreateBufferViewSafe, VkDestroyBufferView,
        pattern VkDestroyBufferView, HS_vkDestroyBufferView,
        PFN_vkDestroyBufferView, unwrapVkDestroyBufferView,
        vkDestroyBufferView, vkDestroyBufferViewSafe,
        module Graphics.Vulkan.Types.Struct.VkBufferViewCreateInfo,
        -- ** Image commands
        VkCreateImage, pattern VkCreateImage, HS_vkCreateImage,
        PFN_vkCreateImage, unwrapVkCreateImage, vkCreateImage,
        vkCreateImageSafe, VkDestroyImage, pattern VkDestroyImage,
        HS_vkDestroyImage, PFN_vkDestroyImage, unwrapVkDestroyImage,
        vkDestroyImage, vkDestroyImageSafe, VkGetImageSubresourceLayout,
        pattern VkGetImageSubresourceLayout,
        HS_vkGetImageSubresourceLayout, PFN_vkGetImageSubresourceLayout,
        unwrapVkGetImageSubresourceLayout, vkGetImageSubresourceLayout,
        vkGetImageSubresourceLayoutSafe,
        module Graphics.Vulkan.Types.Enum.VkImageLayout,
        module Graphics.Vulkan.Types.Struct.VkImageCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkSubresourceLayout,
        -- ** Image view commands
        VkCreateImageView, pattern VkCreateImageView, HS_vkCreateImageView,
        PFN_vkCreateImageView, unwrapVkCreateImageView, vkCreateImageView,
        vkCreateImageViewSafe, VkDestroyImageView,
        pattern VkDestroyImageView, HS_vkDestroyImageView,
        PFN_vkDestroyImageView, unwrapVkDestroyImageView,
        vkDestroyImageView, vkDestroyImageViewSafe,
        module Graphics.Vulkan.Types.Enum.VkComponentSwizzle,
        module Graphics.Vulkan.Types.Enum.VkImageViewType,
        module Graphics.Vulkan.Types.Struct.VkComponentMapping,
        module Graphics.Vulkan.Types.Struct.VkImageSubresourceRange,
        module Graphics.Vulkan.Types.Struct.VkImageViewCreateInfo,
        -- ** Shader commands
        VkCreateShaderModule, pattern VkCreateShaderModule,
        HS_vkCreateShaderModule, PFN_vkCreateShaderModule,
        unwrapVkCreateShaderModule, vkCreateShaderModule,
        vkCreateShaderModuleSafe, VkDestroyShaderModule,
        pattern VkDestroyShaderModule, HS_vkDestroyShaderModule,
        PFN_vkDestroyShaderModule, unwrapVkDestroyShaderModule,
        vkDestroyShaderModule, vkDestroyShaderModuleSafe,
        module Graphics.Vulkan.Types.Struct.VkShaderModuleCreateInfo,
        -- ** Pipeline Cache commands
        VkCreatePipelineCache, pattern VkCreatePipelineCache,
        HS_vkCreatePipelineCache, PFN_vkCreatePipelineCache,
        unwrapVkCreatePipelineCache, vkCreatePipelineCache,
        vkCreatePipelineCacheSafe, VkDestroyPipelineCache,
        pattern VkDestroyPipelineCache, HS_vkDestroyPipelineCache,
        PFN_vkDestroyPipelineCache, unwrapVkDestroyPipelineCache,
        vkDestroyPipelineCache, vkDestroyPipelineCacheSafe,
        VkGetPipelineCacheData, pattern VkGetPipelineCacheData,
        HS_vkGetPipelineCacheData, PFN_vkGetPipelineCacheData,
        unwrapVkGetPipelineCacheData, vkGetPipelineCacheData,
        vkGetPipelineCacheDataSafe, VkMergePipelineCaches,
        pattern VkMergePipelineCaches, HS_vkMergePipelineCaches,
        PFN_vkMergePipelineCaches, unwrapVkMergePipelineCaches,
        vkMergePipelineCaches, vkMergePipelineCachesSafe,
        module Graphics.Vulkan.Types.Struct.VkPipelineCacheCreateInfo,
        -- ** Pipeline commands
        VkCreateGraphicsPipelines, pattern VkCreateGraphicsPipelines,
        HS_vkCreateGraphicsPipelines, PFN_vkCreateGraphicsPipelines,
        unwrapVkCreateGraphicsPipelines, vkCreateGraphicsPipelines,
        vkCreateGraphicsPipelinesSafe, VkCreateComputePipelines,
        pattern VkCreateComputePipelines, HS_vkCreateComputePipelines,
        PFN_vkCreateComputePipelines, unwrapVkCreateComputePipelines,
        vkCreateComputePipelines, vkCreateComputePipelinesSafe,
        VkDestroyPipeline, pattern VkDestroyPipeline, HS_vkDestroyPipeline,
        PFN_vkDestroyPipeline, unwrapVkDestroyPipeline, vkDestroyPipeline,
        vkDestroyPipelineSafe,
        module Graphics.Vulkan.Types.Enum.VkBlendFactor,
        module Graphics.Vulkan.Types.Enum.VkBlendOp,
        module Graphics.Vulkan.Types.Enum.VkColorComponentFlags,
        module Graphics.Vulkan.Types.Enum.VkCompareOp,
        module Graphics.Vulkan.Types.Enum.VkCullModeFlags,
        module Graphics.Vulkan.Types.Enum.VkDynamicState,
        module Graphics.Vulkan.Types.Enum.VkFrontFace,
        module Graphics.Vulkan.Types.Enum.VkLogicOp,
        module Graphics.Vulkan.Types.Enum.VkPipelineCreateFlags,
        module Graphics.Vulkan.Types.Enum.VkPolygonMode,
        module Graphics.Vulkan.Types.Enum.VkPrimitiveTopology,
        module Graphics.Vulkan.Types.Enum.VkShaderStageFlags,
        module Graphics.Vulkan.Types.Enum.VkStencilOp,
        module Graphics.Vulkan.Types.Enum.VkVertexInputRate,
        module Graphics.Vulkan.Types.Struct.VkComputePipelineCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkExtent2D,
        module Graphics.Vulkan.Types.Struct.VkGraphicsPipelineCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkOffset2D,
        module Graphics.Vulkan.Types.Struct.VkPipelineColorBlendAttachmentState,
        module Graphics.Vulkan.Types.Struct.VkPipelineColorBlendStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineDepthStencilStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineDynamicStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineInputAssemblyStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineMultisampleStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineRasterizationStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineShaderStageCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineTessellationStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineVertexInputStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPipelineViewportStateCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkRect2D,
        module Graphics.Vulkan.Types.Struct.VkSpecializationInfo,
        module Graphics.Vulkan.Types.Struct.VkSpecializationMapEntry,
        module Graphics.Vulkan.Types.Struct.VkStencilOpState,
        module Graphics.Vulkan.Types.Struct.VkVertexInputAttributeDescription,
        module Graphics.Vulkan.Types.Struct.VkVertexInputBindingDescription,
        module Graphics.Vulkan.Types.Struct.VkViewport,
        -- ** Pipeline layout commands
        VkCreatePipelineLayout, pattern VkCreatePipelineLayout,
        HS_vkCreatePipelineLayout, PFN_vkCreatePipelineLayout,
        unwrapVkCreatePipelineLayout, vkCreatePipelineLayout,
        vkCreatePipelineLayoutSafe, VkDestroyPipelineLayout,
        pattern VkDestroyPipelineLayout, HS_vkDestroyPipelineLayout,
        PFN_vkDestroyPipelineLayout, unwrapVkDestroyPipelineLayout,
        vkDestroyPipelineLayout, vkDestroyPipelineLayoutSafe,
        module Graphics.Vulkan.Types.Struct.VkPipelineLayoutCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPushConstantRange,
        -- ** Sampler commands
        VkCreateSampler, pattern VkCreateSampler, HS_vkCreateSampler,
        PFN_vkCreateSampler, unwrapVkCreateSampler, vkCreateSampler,
        vkCreateSamplerSafe, VkDestroySampler, pattern VkDestroySampler,
        HS_vkDestroySampler, PFN_vkDestroySampler, unwrapVkDestroySampler,
        vkDestroySampler, vkDestroySamplerSafe,
        module Graphics.Vulkan.Types.Enum.VkBorderColor,
        module Graphics.Vulkan.Types.Enum.VkFilter,
        module Graphics.Vulkan.Types.Enum.VkSamplerAddressMode,
        module Graphics.Vulkan.Types.Enum.VkSamplerMipmapMode,
        module Graphics.Vulkan.Types.Struct.VkSamplerCreateInfo,
        -- ** Descriptor set commands
        VkCreateDescriptorSetLayout, pattern VkCreateDescriptorSetLayout,
        HS_vkCreateDescriptorSetLayout, PFN_vkCreateDescriptorSetLayout,
        unwrapVkCreateDescriptorSetLayout, vkCreateDescriptorSetLayout,
        vkCreateDescriptorSetLayoutSafe, VkDestroyDescriptorSetLayout,
        pattern VkDestroyDescriptorSetLayout,
        HS_vkDestroyDescriptorSetLayout, PFN_vkDestroyDescriptorSetLayout,
        unwrapVkDestroyDescriptorSetLayout, vkDestroyDescriptorSetLayout,
        vkDestroyDescriptorSetLayoutSafe, VkCreateDescriptorPool,
        pattern VkCreateDescriptorPool, HS_vkCreateDescriptorPool,
        PFN_vkCreateDescriptorPool, unwrapVkCreateDescriptorPool,
        vkCreateDescriptorPool, vkCreateDescriptorPoolSafe,
        VkDestroyDescriptorPool, pattern VkDestroyDescriptorPool,
        HS_vkDestroyDescriptorPool, PFN_vkDestroyDescriptorPool,
        unwrapVkDestroyDescriptorPool, vkDestroyDescriptorPool,
        vkDestroyDescriptorPoolSafe, VkResetDescriptorPool,
        pattern VkResetDescriptorPool, HS_vkResetDescriptorPool,
        PFN_vkResetDescriptorPool, unwrapVkResetDescriptorPool,
        vkResetDescriptorPool, vkResetDescriptorPoolSafe,
        VkAllocateDescriptorSets, pattern VkAllocateDescriptorSets,
        HS_vkAllocateDescriptorSets, PFN_vkAllocateDescriptorSets,
        unwrapVkAllocateDescriptorSets, vkAllocateDescriptorSets,
        vkAllocateDescriptorSetsSafe, VkFreeDescriptorSets,
        pattern VkFreeDescriptorSets, HS_vkFreeDescriptorSets,
        PFN_vkFreeDescriptorSets, unwrapVkFreeDescriptorSets,
        vkFreeDescriptorSets, vkFreeDescriptorSetsSafe,
        VkUpdateDescriptorSets, pattern VkUpdateDescriptorSets,
        HS_vkUpdateDescriptorSets, PFN_vkUpdateDescriptorSets,
        unwrapVkUpdateDescriptorSets, vkUpdateDescriptorSets,
        vkUpdateDescriptorSetsSafe,
        module Graphics.Vulkan.Types.Enum.VkDescriptorPoolCreateFlags,
        module Graphics.Vulkan.Types.Enum.VkDescriptorSetLayoutCreateFlags,
        module Graphics.Vulkan.Types.Enum.VkDescriptorType,
        module Graphics.Vulkan.Types.Struct.VkCopyDescriptorSet,
        module Graphics.Vulkan.Types.Struct.VkDescriptorBufferInfo,
        module Graphics.Vulkan.Types.Struct.VkDescriptorImageInfo,
        module Graphics.Vulkan.Types.Struct.VkDescriptorPoolCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkDescriptorPoolSize,
        module Graphics.Vulkan.Types.Struct.VkDescriptorSetAllocateInfo,
        module Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutBinding,
        module Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkWriteDescriptorSet,
        -- ** Pass commands
        VkCreateFramebuffer, pattern VkCreateFramebuffer,
        HS_vkCreateFramebuffer, PFN_vkCreateFramebuffer,
        unwrapVkCreateFramebuffer, vkCreateFramebuffer,
        vkCreateFramebufferSafe, VkDestroyFramebuffer,
        pattern VkDestroyFramebuffer, HS_vkDestroyFramebuffer,
        PFN_vkDestroyFramebuffer, unwrapVkDestroyFramebuffer,
        vkDestroyFramebuffer, vkDestroyFramebufferSafe, VkCreateRenderPass,
        pattern VkCreateRenderPass, HS_vkCreateRenderPass,
        PFN_vkCreateRenderPass, unwrapVkCreateRenderPass,
        vkCreateRenderPass, vkCreateRenderPassSafe, VkDestroyRenderPass,
        pattern VkDestroyRenderPass, HS_vkDestroyRenderPass,
        PFN_vkDestroyRenderPass, unwrapVkDestroyRenderPass,
        vkDestroyRenderPass, vkDestroyRenderPassSafe,
        VkGetRenderAreaGranularity, pattern VkGetRenderAreaGranularity,
        HS_vkGetRenderAreaGranularity, PFN_vkGetRenderAreaGranularity,
        unwrapVkGetRenderAreaGranularity, vkGetRenderAreaGranularity,
        vkGetRenderAreaGranularitySafe,
        module Graphics.Vulkan.Types.Enum.VkAccessFlags,
        module Graphics.Vulkan.Types.Enum.VkAttachmentDescriptionFlags,
        module Graphics.Vulkan.Types.Enum.VkAttachmentLoadOp,
        module Graphics.Vulkan.Types.Enum.VkAttachmentStoreOp,
        module Graphics.Vulkan.Types.Enum.VkDependencyFlags,
        module Graphics.Vulkan.Types.Enum.VkPipelineBindPoint,
        module Graphics.Vulkan.Types.Enum.VkSubpassDescriptionFlags,
        module Graphics.Vulkan.Types.Struct.VkAttachmentDescription,
        module Graphics.Vulkan.Types.Struct.VkAttachmentReference,
        module Graphics.Vulkan.Types.Struct.VkFramebufferCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkRenderPassCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkSubpassDependency,
        module Graphics.Vulkan.Types.Struct.VkSubpassDescription,
        -- ** Command pool commands
        VkCreateCommandPool, pattern VkCreateCommandPool,
        HS_vkCreateCommandPool, PFN_vkCreateCommandPool,
        unwrapVkCreateCommandPool, vkCreateCommandPool,
        vkCreateCommandPoolSafe, VkDestroyCommandPool,
        pattern VkDestroyCommandPool, HS_vkDestroyCommandPool,
        PFN_vkDestroyCommandPool, unwrapVkDestroyCommandPool,
        vkDestroyCommandPool, vkDestroyCommandPoolSafe, VkResetCommandPool,
        pattern VkResetCommandPool, HS_vkResetCommandPool,
        PFN_vkResetCommandPool, unwrapVkResetCommandPool,
        vkResetCommandPool, vkResetCommandPoolSafe,
        module Graphics.Vulkan.Types.Enum.VkCommandPoolCreateFlags,
        module Graphics.Vulkan.Types.Enum.VkCommandPoolResetFlags,
        module Graphics.Vulkan.Types.Struct.VkCommandPoolCreateInfo,
        -- ** Command buffer commands
        VkAllocateCommandBuffers, pattern VkAllocateCommandBuffers,
        HS_vkAllocateCommandBuffers, PFN_vkAllocateCommandBuffers,
        unwrapVkAllocateCommandBuffers, vkAllocateCommandBuffers,
        vkAllocateCommandBuffersSafe, VkFreeCommandBuffers,
        pattern VkFreeCommandBuffers, HS_vkFreeCommandBuffers,
        PFN_vkFreeCommandBuffers, unwrapVkFreeCommandBuffers,
        vkFreeCommandBuffers, vkFreeCommandBuffersSafe,
        VkBeginCommandBuffer, pattern VkBeginCommandBuffer,
        HS_vkBeginCommandBuffer, PFN_vkBeginCommandBuffer,
        unwrapVkBeginCommandBuffer, vkBeginCommandBuffer,
        vkBeginCommandBufferSafe, VkEndCommandBuffer,
        pattern VkEndCommandBuffer, HS_vkEndCommandBuffer,
        PFN_vkEndCommandBuffer, unwrapVkEndCommandBuffer,
        vkEndCommandBuffer, vkEndCommandBufferSafe, VkResetCommandBuffer,
        pattern VkResetCommandBuffer, HS_vkResetCommandBuffer,
        PFN_vkResetCommandBuffer, unwrapVkResetCommandBuffer,
        vkResetCommandBuffer, vkResetCommandBufferSafe,
        module Graphics.Vulkan.Types.Enum.VkCommandBufferLevel,
        module Graphics.Vulkan.Types.Enum.VkCommandBufferResetFlags,
        module Graphics.Vulkan.Types.Enum.VkCommandBufferUsageFlags,
        module Graphics.Vulkan.Types.Enum.VkQueryControlFlags,
        module Graphics.Vulkan.Types.Struct.VkCommandBufferAllocateInfo,
        module Graphics.Vulkan.Types.Struct.VkCommandBufferBeginInfo,
        module Graphics.Vulkan.Types.Struct.VkCommandBufferInheritanceInfo,
        -- ** Command buffer building commands
        VkCmdBindPipeline, pattern VkCmdBindPipeline, HS_vkCmdBindPipeline,
        PFN_vkCmdBindPipeline, unwrapVkCmdBindPipeline, vkCmdBindPipeline,
        vkCmdBindPipelineSafe, VkCmdSetViewport, pattern VkCmdSetViewport,
        HS_vkCmdSetViewport, PFN_vkCmdSetViewport, unwrapVkCmdSetViewport,
        vkCmdSetViewport, vkCmdSetViewportSafe, VkCmdSetScissor,
        pattern VkCmdSetScissor, HS_vkCmdSetScissor, PFN_vkCmdSetScissor,
        unwrapVkCmdSetScissor, vkCmdSetScissor, vkCmdSetScissorSafe,
        VkCmdSetLineWidth, pattern VkCmdSetLineWidth, HS_vkCmdSetLineWidth,
        PFN_vkCmdSetLineWidth, unwrapVkCmdSetLineWidth, vkCmdSetLineWidth,
        vkCmdSetLineWidthSafe, VkCmdSetDepthBias,
        pattern VkCmdSetDepthBias, HS_vkCmdSetDepthBias,
        PFN_vkCmdSetDepthBias, unwrapVkCmdSetDepthBias, vkCmdSetDepthBias,
        vkCmdSetDepthBiasSafe, VkCmdSetBlendConstants,
        pattern VkCmdSetBlendConstants, HS_vkCmdSetBlendConstants,
        PFN_vkCmdSetBlendConstants, unwrapVkCmdSetBlendConstants,
        vkCmdSetBlendConstants, vkCmdSetBlendConstantsSafe,
        VkCmdSetDepthBounds, pattern VkCmdSetDepthBounds,
        HS_vkCmdSetDepthBounds, PFN_vkCmdSetDepthBounds,
        unwrapVkCmdSetDepthBounds, vkCmdSetDepthBounds,
        vkCmdSetDepthBoundsSafe, VkCmdSetStencilCompareMask,
        pattern VkCmdSetStencilCompareMask, HS_vkCmdSetStencilCompareMask,
        PFN_vkCmdSetStencilCompareMask, unwrapVkCmdSetStencilCompareMask,
        vkCmdSetStencilCompareMask, vkCmdSetStencilCompareMaskSafe,
        VkCmdSetStencilWriteMask, pattern VkCmdSetStencilWriteMask,
        HS_vkCmdSetStencilWriteMask, PFN_vkCmdSetStencilWriteMask,
        unwrapVkCmdSetStencilWriteMask, vkCmdSetStencilWriteMask,
        vkCmdSetStencilWriteMaskSafe, VkCmdSetStencilReference,
        pattern VkCmdSetStencilReference, HS_vkCmdSetStencilReference,
        PFN_vkCmdSetStencilReference, unwrapVkCmdSetStencilReference,
        vkCmdSetStencilReference, vkCmdSetStencilReferenceSafe,
        VkCmdBindDescriptorSets, pattern VkCmdBindDescriptorSets,
        HS_vkCmdBindDescriptorSets, PFN_vkCmdBindDescriptorSets,
        unwrapVkCmdBindDescriptorSets, vkCmdBindDescriptorSets,
        vkCmdBindDescriptorSetsSafe, VkCmdBindIndexBuffer,
        pattern VkCmdBindIndexBuffer, HS_vkCmdBindIndexBuffer,
        PFN_vkCmdBindIndexBuffer, unwrapVkCmdBindIndexBuffer,
        vkCmdBindIndexBuffer, vkCmdBindIndexBufferSafe,
        VkCmdBindVertexBuffers, pattern VkCmdBindVertexBuffers,
        HS_vkCmdBindVertexBuffers, PFN_vkCmdBindVertexBuffers,
        unwrapVkCmdBindVertexBuffers, vkCmdBindVertexBuffers,
        vkCmdBindVertexBuffersSafe, VkCmdDraw, pattern VkCmdDraw,
        HS_vkCmdDraw, PFN_vkCmdDraw, unwrapVkCmdDraw, vkCmdDraw,
        vkCmdDrawSafe, VkCmdDrawIndexed, pattern VkCmdDrawIndexed,
        HS_vkCmdDrawIndexed, PFN_vkCmdDrawIndexed, unwrapVkCmdDrawIndexed,
        vkCmdDrawIndexed, vkCmdDrawIndexedSafe, VkCmdDrawIndirect,
        pattern VkCmdDrawIndirect, HS_vkCmdDrawIndirect,
        PFN_vkCmdDrawIndirect, unwrapVkCmdDrawIndirect, vkCmdDrawIndirect,
        vkCmdDrawIndirectSafe, VkCmdDrawIndexedIndirect,
        pattern VkCmdDrawIndexedIndirect, HS_vkCmdDrawIndexedIndirect,
        PFN_vkCmdDrawIndexedIndirect, unwrapVkCmdDrawIndexedIndirect,
        vkCmdDrawIndexedIndirect, vkCmdDrawIndexedIndirectSafe,
        VkCmdDispatch, pattern VkCmdDispatch, HS_vkCmdDispatch,
        PFN_vkCmdDispatch, unwrapVkCmdDispatch, vkCmdDispatch,
        vkCmdDispatchSafe, VkCmdDispatchIndirect,
        pattern VkCmdDispatchIndirect, HS_vkCmdDispatchIndirect,
        PFN_vkCmdDispatchIndirect, unwrapVkCmdDispatchIndirect,
        vkCmdDispatchIndirect, vkCmdDispatchIndirectSafe, VkCmdCopyBuffer,
        pattern VkCmdCopyBuffer, HS_vkCmdCopyBuffer, PFN_vkCmdCopyBuffer,
        unwrapVkCmdCopyBuffer, vkCmdCopyBuffer, vkCmdCopyBufferSafe,
        VkCmdCopyImage, pattern VkCmdCopyImage, HS_vkCmdCopyImage,
        PFN_vkCmdCopyImage, unwrapVkCmdCopyImage, vkCmdCopyImage,
        vkCmdCopyImageSafe, VkCmdBlitImage, pattern VkCmdBlitImage,
        HS_vkCmdBlitImage, PFN_vkCmdBlitImage, unwrapVkCmdBlitImage,
        vkCmdBlitImage, vkCmdBlitImageSafe, VkCmdCopyBufferToImage,
        pattern VkCmdCopyBufferToImage, HS_vkCmdCopyBufferToImage,
        PFN_vkCmdCopyBufferToImage, unwrapVkCmdCopyBufferToImage,
        vkCmdCopyBufferToImage, vkCmdCopyBufferToImageSafe,
        VkCmdCopyImageToBuffer, pattern VkCmdCopyImageToBuffer,
        HS_vkCmdCopyImageToBuffer, PFN_vkCmdCopyImageToBuffer,
        unwrapVkCmdCopyImageToBuffer, vkCmdCopyImageToBuffer,
        vkCmdCopyImageToBufferSafe, VkCmdUpdateBuffer,
        pattern VkCmdUpdateBuffer, HS_vkCmdUpdateBuffer,
        PFN_vkCmdUpdateBuffer, unwrapVkCmdUpdateBuffer, vkCmdUpdateBuffer,
        vkCmdUpdateBufferSafe, VkCmdFillBuffer, pattern VkCmdFillBuffer,
        HS_vkCmdFillBuffer, PFN_vkCmdFillBuffer, unwrapVkCmdFillBuffer,
        vkCmdFillBuffer, vkCmdFillBufferSafe, VkCmdClearColorImage,
        pattern VkCmdClearColorImage, HS_vkCmdClearColorImage,
        PFN_vkCmdClearColorImage, unwrapVkCmdClearColorImage,
        vkCmdClearColorImage, vkCmdClearColorImageSafe,
        VkCmdClearDepthStencilImage, pattern VkCmdClearDepthStencilImage,
        HS_vkCmdClearDepthStencilImage, PFN_vkCmdClearDepthStencilImage,
        unwrapVkCmdClearDepthStencilImage, vkCmdClearDepthStencilImage,
        vkCmdClearDepthStencilImageSafe, VkCmdClearAttachments,
        pattern VkCmdClearAttachments, HS_vkCmdClearAttachments,
        PFN_vkCmdClearAttachments, unwrapVkCmdClearAttachments,
        vkCmdClearAttachments, vkCmdClearAttachmentsSafe,
        VkCmdResolveImage, pattern VkCmdResolveImage, HS_vkCmdResolveImage,
        PFN_vkCmdResolveImage, unwrapVkCmdResolveImage, vkCmdResolveImage,
        vkCmdResolveImageSafe, VkCmdSetEvent, pattern VkCmdSetEvent,
        HS_vkCmdSetEvent, PFN_vkCmdSetEvent, unwrapVkCmdSetEvent,
        vkCmdSetEvent, vkCmdSetEventSafe, VkCmdResetEvent,
        pattern VkCmdResetEvent, HS_vkCmdResetEvent, PFN_vkCmdResetEvent,
        unwrapVkCmdResetEvent, vkCmdResetEvent, vkCmdResetEventSafe,
        VkCmdWaitEvents, pattern VkCmdWaitEvents, HS_vkCmdWaitEvents,
        PFN_vkCmdWaitEvents, unwrapVkCmdWaitEvents, vkCmdWaitEvents,
        vkCmdWaitEventsSafe, VkCmdPipelineBarrier,
        pattern VkCmdPipelineBarrier, HS_vkCmdPipelineBarrier,
        PFN_vkCmdPipelineBarrier, unwrapVkCmdPipelineBarrier,
        vkCmdPipelineBarrier, vkCmdPipelineBarrierSafe, VkCmdBeginQuery,
        pattern VkCmdBeginQuery, HS_vkCmdBeginQuery, PFN_vkCmdBeginQuery,
        unwrapVkCmdBeginQuery, vkCmdBeginQuery, vkCmdBeginQuerySafe,
        VkCmdEndQuery, pattern VkCmdEndQuery, HS_vkCmdEndQuery,
        PFN_vkCmdEndQuery, unwrapVkCmdEndQuery, vkCmdEndQuery,
        vkCmdEndQuerySafe, VkCmdResetQueryPool,
        pattern VkCmdResetQueryPool, HS_vkCmdResetQueryPool,
        PFN_vkCmdResetQueryPool, unwrapVkCmdResetQueryPool,
        vkCmdResetQueryPool, vkCmdResetQueryPoolSafe, VkCmdWriteTimestamp,
        pattern VkCmdWriteTimestamp, HS_vkCmdWriteTimestamp,
        PFN_vkCmdWriteTimestamp, unwrapVkCmdWriteTimestamp,
        vkCmdWriteTimestamp, vkCmdWriteTimestampSafe,
        VkCmdCopyQueryPoolResults, pattern VkCmdCopyQueryPoolResults,
        HS_vkCmdCopyQueryPoolResults, PFN_vkCmdCopyQueryPoolResults,
        unwrapVkCmdCopyQueryPoolResults, vkCmdCopyQueryPoolResults,
        vkCmdCopyQueryPoolResultsSafe, VkCmdPushConstants,
        pattern VkCmdPushConstants, HS_vkCmdPushConstants,
        PFN_vkCmdPushConstants, unwrapVkCmdPushConstants,
        vkCmdPushConstants, vkCmdPushConstantsSafe, VkCmdBeginRenderPass,
        pattern VkCmdBeginRenderPass, HS_vkCmdBeginRenderPass,
        PFN_vkCmdBeginRenderPass, unwrapVkCmdBeginRenderPass,
        vkCmdBeginRenderPass, vkCmdBeginRenderPassSafe, VkCmdNextSubpass,
        pattern VkCmdNextSubpass, HS_vkCmdNextSubpass,
        PFN_vkCmdNextSubpass, unwrapVkCmdNextSubpass, vkCmdNextSubpass,
        vkCmdNextSubpassSafe, VkCmdEndRenderPass,
        pattern VkCmdEndRenderPass, HS_vkCmdEndRenderPass,
        PFN_vkCmdEndRenderPass, unwrapVkCmdEndRenderPass,
        vkCmdEndRenderPass, vkCmdEndRenderPassSafe, VkCmdExecuteCommands,
        pattern VkCmdExecuteCommands, HS_vkCmdExecuteCommands,
        PFN_vkCmdExecuteCommands, unwrapVkCmdExecuteCommands,
        vkCmdExecuteCommands, vkCmdExecuteCommandsSafe,
        module Graphics.Vulkan.Types.Enum.VkIndexType,
        module Graphics.Vulkan.Types.Enum.VkStencilFaceFlags,
        module Graphics.Vulkan.Types.Enum.VkSubpassContents,
        module Graphics.Vulkan.Types.Struct.VkBufferCopy,
        module Graphics.Vulkan.Types.Struct.VkBufferImageCopy,
        module Graphics.Vulkan.Types.Struct.VkBufferMemoryBarrier,
        module Graphics.Vulkan.Types.Struct.VkClearAttachment,
        module Graphics.Vulkan.Types.Struct.VkClearColorValue,
        module Graphics.Vulkan.Types.Struct.VkClearDepthStencilValue,
        module Graphics.Vulkan.Types.Struct.VkClearRect,
        module Graphics.Vulkan.Types.Struct.VkClearValue,
        module Graphics.Vulkan.Types.Struct.VkImageBlit,
        module Graphics.Vulkan.Types.Struct.VkImageCopy,
        module Graphics.Vulkan.Types.Struct.VkImageMemoryBarrier,
        module Graphics.Vulkan.Types.Struct.VkImageResolve,
        module Graphics.Vulkan.Types.Struct.VkImageSubresourceLayers,
        module Graphics.Vulkan.Types.Struct.VkMemoryBarrier,
        module Graphics.Vulkan.Types.Struct.VkRenderPassBeginInfo,
        -- ** Types not directly used by the API. Include e.g. structs that are not parameter types of commands, but still defined by the API.
        module Graphics.Vulkan.Types.Struct.VkDispatchIndirectCommand,
        module Graphics.Vulkan.Types.Struct.VkDrawIndexedIndirectCommand,
        module Graphics.Vulkan.Types.Struct.VkDrawIndirectCommand,
        module Graphics.Vulkan.Types.Enum.VkObjectType)
       where
import           GHC.Ptr
                                                                                      (Ptr (..))
import           Graphics.Vulkan.Constants
                                                                                      (pattern VK_ATTACHMENT_UNUSED,
                                                                                      pattern VK_FALSE,
                                                                                      pattern VK_LOD_CLAMP_NONE,
                                                                                      pattern VK_QUEUE_FAMILY_IGNORED,
                                                                                      pattern VK_REMAINING_ARRAY_LAYERS,
                                                                                      pattern VK_REMAINING_MIP_LEVELS,
                                                                                      pattern VK_SUBPASS_EXTERNAL,
                                                                                      pattern VK_TRUE,
                                                                                      pattern VK_WHOLE_SIZE)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
                                                                                      (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkAccessFlags
import           Graphics.Vulkan.Types.Enum.VkAttachmentDescriptionFlags
import           Graphics.Vulkan.Types.Enum.VkAttachmentLoadOp
import           Graphics.Vulkan.Types.Enum.VkAttachmentStoreOp
import           Graphics.Vulkan.Types.Enum.VkBlendFactor
import           Graphics.Vulkan.Types.Enum.VkBlendOp
import           Graphics.Vulkan.Types.Enum.VkBorderColor
import           Graphics.Vulkan.Types.Enum.VkBufferCreateFlags
import           Graphics.Vulkan.Types.Enum.VkBufferUsageFlags
import           Graphics.Vulkan.Types.Enum.VkColorComponentFlags
import           Graphics.Vulkan.Types.Enum.VkCommandBufferLevel
import           Graphics.Vulkan.Types.Enum.VkCommandBufferResetFlags
import           Graphics.Vulkan.Types.Enum.VkCommandBufferUsageFlags
import           Graphics.Vulkan.Types.Enum.VkCommandPoolCreateFlags
import           Graphics.Vulkan.Types.Enum.VkCommandPoolResetFlags
import           Graphics.Vulkan.Types.Enum.VkCompareOp
import           Graphics.Vulkan.Types.Enum.VkComponentSwizzle
import           Graphics.Vulkan.Types.Enum.VkCullModeFlags
import           Graphics.Vulkan.Types.Enum.VkDependencyFlags
import           Graphics.Vulkan.Types.Enum.VkDescriptorPoolCreateFlags
import           Graphics.Vulkan.Types.Enum.VkDescriptorSetLayoutCreateFlags
import           Graphics.Vulkan.Types.Enum.VkDescriptorType
import           Graphics.Vulkan.Types.Enum.VkDeviceQueueCreateFlags
import           Graphics.Vulkan.Types.Enum.VkDynamicState
import           Graphics.Vulkan.Types.Enum.VkFenceCreateFlags
import           Graphics.Vulkan.Types.Enum.VkFilter
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkFormatFeatureFlags
import           Graphics.Vulkan.Types.Enum.VkFrontFace
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags
import           Graphics.Vulkan.Types.Enum.VkImageLayout
import           Graphics.Vulkan.Types.Enum.VkImageTiling
import           Graphics.Vulkan.Types.Enum.VkImageType
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkImageViewType
import           Graphics.Vulkan.Types.Enum.VkIndexType
import           Graphics.Vulkan.Types.Enum.VkInternalAllocationType
import           Graphics.Vulkan.Types.Enum.VkLogicOp
import           Graphics.Vulkan.Types.Enum.VkMemoryHeapFlags
import           Graphics.Vulkan.Types.Enum.VkMemoryPropertyFlags
import           Graphics.Vulkan.Types.Enum.VkObjectType
import           Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.VkPipelineBindPoint
import           Graphics.Vulkan.Types.Enum.VkPipelineCacheHeaderVersion
import           Graphics.Vulkan.Types.Enum.VkPipelineCreateFlags
import           Graphics.Vulkan.Types.Enum.VkPipelineStageFlags
import           Graphics.Vulkan.Types.Enum.VkPolygonMode
import           Graphics.Vulkan.Types.Enum.VkPrimitiveTopology
import           Graphics.Vulkan.Types.Enum.VkQueryControlFlags
import           Graphics.Vulkan.Types.Enum.VkQueryPipelineStatisticFlags
import           Graphics.Vulkan.Types.Enum.VkQueryResultFlags
import           Graphics.Vulkan.Types.Enum.VkQueryType
import           Graphics.Vulkan.Types.Enum.VkQueueFlags
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkSamplerAddressMode
import           Graphics.Vulkan.Types.Enum.VkSamplerMipmapMode
import           Graphics.Vulkan.Types.Enum.VkShaderStageFlags
import           Graphics.Vulkan.Types.Enum.VkSharingMode
import           Graphics.Vulkan.Types.Enum.VkSparseImageFormatFlags
import           Graphics.Vulkan.Types.Enum.VkSparseMemoryBindFlags
import           Graphics.Vulkan.Types.Enum.VkStencilFaceFlags
import           Graphics.Vulkan.Types.Enum.VkStencilOp
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSubpassContents
import           Graphics.Vulkan.Types.Enum.VkSubpassDescriptionFlags
import           Graphics.Vulkan.Types.Enum.VkSystemAllocationScope
import           Graphics.Vulkan.Types.Enum.VkVertexInputRate
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
import           Graphics.Vulkan.Types.Struct.VkApplicationInfo
import           Graphics.Vulkan.Types.Struct.VkAttachmentDescription
import           Graphics.Vulkan.Types.Struct.VkAttachmentReference
import           Graphics.Vulkan.Types.Struct.VkBindSparseInfo
import           Graphics.Vulkan.Types.Struct.VkBufferCopy
import           Graphics.Vulkan.Types.Struct.VkBufferCreateInfo
import           Graphics.Vulkan.Types.Struct.VkBufferImageCopy
import           Graphics.Vulkan.Types.Struct.VkBufferMemoryBarrier
import           Graphics.Vulkan.Types.Struct.VkBufferViewCreateInfo
import           Graphics.Vulkan.Types.Struct.VkClearAttachment
import           Graphics.Vulkan.Types.Struct.VkClearColorValue
import           Graphics.Vulkan.Types.Struct.VkClearDepthStencilValue
import           Graphics.Vulkan.Types.Struct.VkClearRect
import           Graphics.Vulkan.Types.Struct.VkClearValue
import           Graphics.Vulkan.Types.Struct.VkCommandBufferAllocateInfo
import           Graphics.Vulkan.Types.Struct.VkCommandBufferBeginInfo
import           Graphics.Vulkan.Types.Struct.VkCommandBufferInheritanceInfo
import           Graphics.Vulkan.Types.Struct.VkCommandPoolCreateInfo
import           Graphics.Vulkan.Types.Struct.VkComponentMapping
import           Graphics.Vulkan.Types.Struct.VkComputePipelineCreateInfo
import           Graphics.Vulkan.Types.Struct.VkCopyDescriptorSet
import           Graphics.Vulkan.Types.Struct.VkDescriptorBufferInfo
import           Graphics.Vulkan.Types.Struct.VkDescriptorImageInfo
import           Graphics.Vulkan.Types.Struct.VkDescriptorPoolCreateInfo
import           Graphics.Vulkan.Types.Struct.VkDescriptorPoolSize
import           Graphics.Vulkan.Types.Struct.VkDescriptorSetAllocateInfo
import           Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutBinding
import           Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutCreateInfo
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo
import           Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo
import           Graphics.Vulkan.Types.Struct.VkDispatchIndirectCommand
import           Graphics.Vulkan.Types.Struct.VkDrawIndexedIndirectCommand
import           Graphics.Vulkan.Types.Struct.VkDrawIndirectCommand
import           Graphics.Vulkan.Types.Struct.VkEventCreateInfo
import           Graphics.Vulkan.Types.Struct.VkExtensionProperties
import           Graphics.Vulkan.Types.Struct.VkExtent2D
import           Graphics.Vulkan.Types.Struct.VkExtent3D
import           Graphics.Vulkan.Types.Struct.VkFenceCreateInfo
import           Graphics.Vulkan.Types.Struct.VkFormatProperties
import           Graphics.Vulkan.Types.Struct.VkFramebufferCreateInfo
import           Graphics.Vulkan.Types.Struct.VkGraphicsPipelineCreateInfo
import           Graphics.Vulkan.Types.Struct.VkImageBlit
import           Graphics.Vulkan.Types.Struct.VkImageCopy
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties
import           Graphics.Vulkan.Types.Struct.VkImageMemoryBarrier
import           Graphics.Vulkan.Types.Struct.VkImageResolve
import           Graphics.Vulkan.Types.Struct.VkImageSubresource
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceLayers
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceRange
import           Graphics.Vulkan.Types.Struct.VkImageViewCreateInfo
import           Graphics.Vulkan.Types.Struct.VkInstanceCreateInfo
import           Graphics.Vulkan.Types.Struct.VkLayerProperties
import           Graphics.Vulkan.Types.Struct.VkMappedMemoryRange
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo
import           Graphics.Vulkan.Types.Struct.VkMemoryBarrier
import           Graphics.Vulkan.Types.Struct.VkMemoryHeap
import           Graphics.Vulkan.Types.Struct.VkMemoryRequirements
import           Graphics.Vulkan.Types.Struct.VkMemoryType
import           Graphics.Vulkan.Types.Struct.VkOffset2D
import           Graphics.Vulkan.Types.Struct.VkOffset3D
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMemoryProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties
import           Graphics.Vulkan.Types.Struct.VkPipelineCacheCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineColorBlendAttachmentState
import           Graphics.Vulkan.Types.Struct.VkPipelineColorBlendStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineDepthStencilStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineDynamicStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineInputAssemblyStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineLayoutCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineMultisampleStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineRasterizationStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineShaderStageCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineTessellationStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineVertexInputStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPipelineViewportStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPushConstantRange
import           Graphics.Vulkan.Types.Struct.VkQueryPoolCreateInfo
import           Graphics.Vulkan.Types.Struct.VkQueueFamilyProperties
import           Graphics.Vulkan.Types.Struct.VkRect2D
import           Graphics.Vulkan.Types.Struct.VkRenderPassBeginInfo
import           Graphics.Vulkan.Types.Struct.VkRenderPassCreateInfo
import           Graphics.Vulkan.Types.Struct.VkSamplerCreateInfo
import           Graphics.Vulkan.Types.Struct.VkSemaphoreCreateInfo
import           Graphics.Vulkan.Types.Struct.VkShaderModuleCreateInfo
import           Graphics.Vulkan.Types.Struct.VkSparseBufferMemoryBindInfo
import           Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBind
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBindInfo
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryRequirements
import           Graphics.Vulkan.Types.Struct.VkSparseImageOpaqueMemoryBindInfo
import           Graphics.Vulkan.Types.Struct.VkSparseMemoryBind
import           Graphics.Vulkan.Types.Struct.VkSpecializationInfo
import           Graphics.Vulkan.Types.Struct.VkSpecializationMapEntry
import           Graphics.Vulkan.Types.Struct.VkStencilOpState
import           Graphics.Vulkan.Types.Struct.VkSubmitInfo
import           Graphics.Vulkan.Types.Struct.VkSubpassDependency
import           Graphics.Vulkan.Types.Struct.VkSubpassDescription
import           Graphics.Vulkan.Types.Struct.VkSubresourceLayout
import           Graphics.Vulkan.Types.Struct.VkVertexInputAttributeDescription
import           Graphics.Vulkan.Types.Struct.VkVertexInputBindingDescription
import           Graphics.Vulkan.Types.Struct.VkViewport
import           Graphics.Vulkan.Types.Struct.VkWriteDescriptorSet

pattern VkCreateInstance :: CString

pattern VkCreateInstance <- (is_VkCreateInstance -> True)
  where VkCreateInstance = _VkCreateInstance

{-# INLINE _VkCreateInstance #-}

_VkCreateInstance :: CString
_VkCreateInstance = Ptr "vkCreateInstance\NUL"##

{-# INLINE is_VkCreateInstance #-}

is_VkCreateInstance :: CString -> Bool
is_VkCreateInstance = (EQ ==) . cmpCStrings _VkCreateInstance

type VkCreateInstance = "vkCreateInstance"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED', 'VK_ERROR_LAYER_NOT_PRESENT', 'VK_ERROR_EXTENSION_NOT_PRESENT', 'VK_ERROR_INCOMPATIBLE_DRIVER'.
--
--   > VkResult vkCreateInstance
--   >     ( const VkInstanceCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkInstance* pInstance
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateInstance.html vkCreateInstance registry at www.khronos.org>
foreign import ccall unsafe "vkCreateInstance" vkCreateInstance ::
               Ptr VkInstanceCreateInfo -- ^ pCreateInfo
                                        ->
                 Ptr VkAllocationCallbacks -- ^ pAllocator
                                           -> Ptr VkInstance -- ^ pInstance
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED', 'VK_ERROR_LAYER_NOT_PRESENT', 'VK_ERROR_EXTENSION_NOT_PRESENT', 'VK_ERROR_INCOMPATIBLE_DRIVER'.
--
--   > VkResult vkCreateInstance
--   >     ( const VkInstanceCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkInstance* pInstance
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateInstance.html vkCreateInstance registry at www.khronos.org>
foreign import ccall safe "vkCreateInstance" vkCreateInstanceSafe
               ::
               Ptr VkInstanceCreateInfo -- ^ pCreateInfo
                                        ->
                 Ptr VkAllocationCallbacks -- ^ pAllocator
                                           -> Ptr VkInstance -- ^ pInstance
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED', 'VK_ERROR_LAYER_NOT_PRESENT', 'VK_ERROR_EXTENSION_NOT_PRESENT', 'VK_ERROR_INCOMPATIBLE_DRIVER'.
--
--   > VkResult vkCreateInstance
--   >     ( const VkInstanceCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkInstance* pInstance
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateInstance.html vkCreateInstance registry at www.khronos.org>
type HS_vkCreateInstance =
     Ptr VkInstanceCreateInfo -- ^ pCreateInfo
                              ->
       Ptr VkAllocationCallbacks -- ^ pAllocator
                                 -> Ptr VkInstance -- ^ pInstance
                                                   -> IO VkResult

type PFN_vkCreateInstance = FunPtr HS_vkCreateInstance

foreign import ccall "dynamic" unwrapVkCreateInstance ::
               PFN_vkCreateInstance -> HS_vkCreateInstance

instance VulkanProc "vkCreateInstance" where
        type VkProcType "vkCreateInstance" = HS_vkCreateInstance
        vkProcSymbol = _VkCreateInstance

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateInstance

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyInstance :: CString

pattern VkDestroyInstance <- (is_VkDestroyInstance -> True)
  where VkDestroyInstance = _VkDestroyInstance

{-# INLINE _VkDestroyInstance #-}

_VkDestroyInstance :: CString
_VkDestroyInstance = Ptr "vkDestroyInstance\NUL"##

{-# INLINE is_VkDestroyInstance #-}

is_VkDestroyInstance :: CString -> Bool
is_VkDestroyInstance = (EQ ==) . cmpCStrings _VkDestroyInstance

type VkDestroyInstance = "vkDestroyInstance"

-- | > () vkDestroyInstance
--   >     ( VkInstance instance
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyInstance.html vkDestroyInstance registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyInstance" vkDestroyInstance
               :: VkInstance -- ^ instance
                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          -> IO ()

-- | > () vkDestroyInstance
--   >     ( VkInstance instance
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyInstance.html vkDestroyInstance registry at www.khronos.org>
foreign import ccall safe "vkDestroyInstance" vkDestroyInstanceSafe
               :: VkInstance -- ^ instance
                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          -> IO ()

-- | > () vkDestroyInstance
--   >     ( VkInstance instance
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyInstance.html vkDestroyInstance registry at www.khronos.org>
type HS_vkDestroyInstance =
     VkInstance -- ^ instance
                -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> IO ()

type PFN_vkDestroyInstance = FunPtr HS_vkDestroyInstance

foreign import ccall "dynamic" unwrapVkDestroyInstance ::
               PFN_vkDestroyInstance -> HS_vkDestroyInstance

instance VulkanProc "vkDestroyInstance" where
        type VkProcType "vkDestroyInstance" = HS_vkDestroyInstance
        vkProcSymbol = _VkDestroyInstance

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyInstance

        {-# INLINE unwrapVkProcPtr #-}

pattern VkEnumeratePhysicalDevices :: CString

pattern VkEnumeratePhysicalDevices <-
        (is_VkEnumeratePhysicalDevices -> True)
  where VkEnumeratePhysicalDevices = _VkEnumeratePhysicalDevices

{-# INLINE _VkEnumeratePhysicalDevices #-}

_VkEnumeratePhysicalDevices :: CString
_VkEnumeratePhysicalDevices = Ptr "vkEnumeratePhysicalDevices\NUL"##

{-# INLINE is_VkEnumeratePhysicalDevices #-}

is_VkEnumeratePhysicalDevices :: CString -> Bool
is_VkEnumeratePhysicalDevices
  = (EQ ==) . cmpCStrings _VkEnumeratePhysicalDevices

type VkEnumeratePhysicalDevices = "vkEnumeratePhysicalDevices"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkEnumeratePhysicalDevices
--   >     ( VkInstance instance
--   >     , uint32_t* pPhysicalDeviceCount
--   >     , VkPhysicalDevice* pPhysicalDevices
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumeratePhysicalDevices.html vkEnumeratePhysicalDevices registry at www.khronos.org>
foreign import ccall unsafe "vkEnumeratePhysicalDevices"
               vkEnumeratePhysicalDevices ::
               VkInstance -- ^ instance
                          -> Ptr Word32 -- ^ pPhysicalDeviceCount
                                        -> Ptr VkPhysicalDevice -- ^ pPhysicalDevices
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkEnumeratePhysicalDevices
--   >     ( VkInstance instance
--   >     , uint32_t* pPhysicalDeviceCount
--   >     , VkPhysicalDevice* pPhysicalDevices
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumeratePhysicalDevices.html vkEnumeratePhysicalDevices registry at www.khronos.org>
foreign import ccall safe "vkEnumeratePhysicalDevices"
               vkEnumeratePhysicalDevicesSafe ::
               VkInstance -- ^ instance
                          -> Ptr Word32 -- ^ pPhysicalDeviceCount
                                        -> Ptr VkPhysicalDevice -- ^ pPhysicalDevices
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkEnumeratePhysicalDevices
--   >     ( VkInstance instance
--   >     , uint32_t* pPhysicalDeviceCount
--   >     , VkPhysicalDevice* pPhysicalDevices
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumeratePhysicalDevices.html vkEnumeratePhysicalDevices registry at www.khronos.org>
type HS_vkEnumeratePhysicalDevices =
     VkInstance -- ^ instance
                -> Ptr Word32 -- ^ pPhysicalDeviceCount
                              -> Ptr VkPhysicalDevice -- ^ pPhysicalDevices
                                                      -> IO VkResult

type PFN_vkEnumeratePhysicalDevices =
     FunPtr HS_vkEnumeratePhysicalDevices

foreign import ccall "dynamic" unwrapVkEnumeratePhysicalDevices ::
               PFN_vkEnumeratePhysicalDevices -> HS_vkEnumeratePhysicalDevices

instance VulkanProc "vkEnumeratePhysicalDevices" where
        type VkProcType "vkEnumeratePhysicalDevices" =
             HS_vkEnumeratePhysicalDevices
        vkProcSymbol = _VkEnumeratePhysicalDevices

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkEnumeratePhysicalDevices

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceFeatures :: CString

pattern VkGetPhysicalDeviceFeatures <-
        (is_VkGetPhysicalDeviceFeatures -> True)
  where VkGetPhysicalDeviceFeatures = _VkGetPhysicalDeviceFeatures

{-# INLINE _VkGetPhysicalDeviceFeatures #-}

_VkGetPhysicalDeviceFeatures :: CString
_VkGetPhysicalDeviceFeatures
  = Ptr "vkGetPhysicalDeviceFeatures\NUL"##

{-# INLINE is_VkGetPhysicalDeviceFeatures #-}

is_VkGetPhysicalDeviceFeatures :: CString -> Bool
is_VkGetPhysicalDeviceFeatures
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceFeatures

type VkGetPhysicalDeviceFeatures = "vkGetPhysicalDeviceFeatures"

-- | > () vkGetPhysicalDeviceFeatures
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFeatures.html vkGetPhysicalDeviceFeatures registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceFeatures"
               vkGetPhysicalDeviceFeatures ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceFeatures -- ^ pFeatures
                                                                -> IO ()

-- | > () vkGetPhysicalDeviceFeatures
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFeatures.html vkGetPhysicalDeviceFeatures registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceFeatures"
               vkGetPhysicalDeviceFeaturesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceFeatures -- ^ pFeatures
                                                                -> IO ()

-- | > () vkGetPhysicalDeviceFeatures
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFeatures.html vkGetPhysicalDeviceFeatures registry at www.khronos.org>
type HS_vkGetPhysicalDeviceFeatures =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Ptr VkPhysicalDeviceFeatures -- ^ pFeatures
                                                      -> IO ()

type PFN_vkGetPhysicalDeviceFeatures =
     FunPtr HS_vkGetPhysicalDeviceFeatures

foreign import ccall "dynamic" unwrapVkGetPhysicalDeviceFeatures ::
               PFN_vkGetPhysicalDeviceFeatures -> HS_vkGetPhysicalDeviceFeatures

instance VulkanProc "vkGetPhysicalDeviceFeatures" where
        type VkProcType "vkGetPhysicalDeviceFeatures" =
             HS_vkGetPhysicalDeviceFeatures
        vkProcSymbol = _VkGetPhysicalDeviceFeatures

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceFeatures

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceFormatProperties :: CString

pattern VkGetPhysicalDeviceFormatProperties <-
        (is_VkGetPhysicalDeviceFormatProperties -> True)
  where VkGetPhysicalDeviceFormatProperties
          = _VkGetPhysicalDeviceFormatProperties

{-# INLINE _VkGetPhysicalDeviceFormatProperties #-}

_VkGetPhysicalDeviceFormatProperties :: CString
_VkGetPhysicalDeviceFormatProperties
  = Ptr "vkGetPhysicalDeviceFormatProperties\NUL"##

{-# INLINE is_VkGetPhysicalDeviceFormatProperties #-}

is_VkGetPhysicalDeviceFormatProperties :: CString -> Bool
is_VkGetPhysicalDeviceFormatProperties
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceFormatProperties

type VkGetPhysicalDeviceFormatProperties =
     "vkGetPhysicalDeviceFormatProperties"

-- | > () vkGetPhysicalDeviceFormatProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFormatProperties.html vkGetPhysicalDeviceFormatProperties registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceFormatProperties"
               vkGetPhysicalDeviceFormatProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkFormat -- ^ format
                                            -> Ptr VkFormatProperties -- ^ pFormatProperties
                                                                      -> IO ()

-- | > () vkGetPhysicalDeviceFormatProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFormatProperties.html vkGetPhysicalDeviceFormatProperties registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceFormatProperties"
               vkGetPhysicalDeviceFormatPropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkFormat -- ^ format
                                            -> Ptr VkFormatProperties -- ^ pFormatProperties
                                                                      -> IO ()

-- | > () vkGetPhysicalDeviceFormatProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceFormatProperties.html vkGetPhysicalDeviceFormatProperties registry at www.khronos.org>
type HS_vkGetPhysicalDeviceFormatProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      -> VkFormat -- ^ format
                                  -> Ptr VkFormatProperties -- ^ pFormatProperties
                                                            -> IO ()

type PFN_vkGetPhysicalDeviceFormatProperties =
     FunPtr HS_vkGetPhysicalDeviceFormatProperties

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceFormatProperties ::
               PFN_vkGetPhysicalDeviceFormatProperties ->
                 HS_vkGetPhysicalDeviceFormatProperties

instance VulkanProc "vkGetPhysicalDeviceFormatProperties" where
        type VkProcType "vkGetPhysicalDeviceFormatProperties" =
             HS_vkGetPhysicalDeviceFormatProperties
        vkProcSymbol = _VkGetPhysicalDeviceFormatProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceFormatProperties

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceImageFormatProperties :: CString

pattern VkGetPhysicalDeviceImageFormatProperties <-
        (is_VkGetPhysicalDeviceImageFormatProperties -> True)
  where VkGetPhysicalDeviceImageFormatProperties
          = _VkGetPhysicalDeviceImageFormatProperties

{-# INLINE _VkGetPhysicalDeviceImageFormatProperties #-}

_VkGetPhysicalDeviceImageFormatProperties :: CString
_VkGetPhysicalDeviceImageFormatProperties
  = Ptr "vkGetPhysicalDeviceImageFormatProperties\NUL"##

{-# INLINE is_VkGetPhysicalDeviceImageFormatProperties #-}

is_VkGetPhysicalDeviceImageFormatProperties :: CString -> Bool
is_VkGetPhysicalDeviceImageFormatProperties
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceImageFormatProperties

type VkGetPhysicalDeviceImageFormatProperties =
     "vkGetPhysicalDeviceImageFormatProperties"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceImageFormatProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkImageType type
--   >     , VkImageTiling tiling
--   >     , VkImageUsageFlags usage
--   >     , VkImageCreateFlags flags
--   >     , VkImageFormatProperties* pImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceImageFormatProperties.html vkGetPhysicalDeviceImageFormatProperties registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceImageFormatProperties"
               vkGetPhysicalDeviceImageFormatProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkFormat -- ^ format
                          ->
                   VkImageType -- ^ type
                               ->
                     VkImageTiling -- ^ tiling
                                   ->
                       VkImageUsageFlags -- ^ usage
                                         ->
                         VkImageCreateFlags -- ^ flags
                                            -> Ptr VkImageFormatProperties -- ^ pImageFormatProperties
                                                                           -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceImageFormatProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkImageType type
--   >     , VkImageTiling tiling
--   >     , VkImageUsageFlags usage
--   >     , VkImageCreateFlags flags
--   >     , VkImageFormatProperties* pImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceImageFormatProperties.html vkGetPhysicalDeviceImageFormatProperties registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceImageFormatProperties"
               vkGetPhysicalDeviceImageFormatPropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkFormat -- ^ format
                          ->
                   VkImageType -- ^ type
                               ->
                     VkImageTiling -- ^ tiling
                                   ->
                       VkImageUsageFlags -- ^ usage
                                         ->
                         VkImageCreateFlags -- ^ flags
                                            -> Ptr VkImageFormatProperties -- ^ pImageFormatProperties
                                                                           -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceImageFormatProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkImageType type
--   >     , VkImageTiling tiling
--   >     , VkImageUsageFlags usage
--   >     , VkImageCreateFlags flags
--   >     , VkImageFormatProperties* pImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceImageFormatProperties.html vkGetPhysicalDeviceImageFormatProperties registry at www.khronos.org>
type HS_vkGetPhysicalDeviceImageFormatProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       VkFormat -- ^ format
                ->
         VkImageType -- ^ type
                     ->
           VkImageTiling -- ^ tiling
                         ->
             VkImageUsageFlags -- ^ usage
                               ->
               VkImageCreateFlags -- ^ flags
                                  -> Ptr VkImageFormatProperties -- ^ pImageFormatProperties
                                                                 -> IO VkResult

type PFN_vkGetPhysicalDeviceImageFormatProperties =
     FunPtr HS_vkGetPhysicalDeviceImageFormatProperties

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceImageFormatProperties ::
               PFN_vkGetPhysicalDeviceImageFormatProperties ->
                 HS_vkGetPhysicalDeviceImageFormatProperties

instance VulkanProc "vkGetPhysicalDeviceImageFormatProperties"
         where
        type VkProcType "vkGetPhysicalDeviceImageFormatProperties" =
             HS_vkGetPhysicalDeviceImageFormatProperties
        vkProcSymbol = _VkGetPhysicalDeviceImageFormatProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceImageFormatProperties

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceProperties :: CString

pattern VkGetPhysicalDeviceProperties <-
        (is_VkGetPhysicalDeviceProperties -> True)
  where VkGetPhysicalDeviceProperties
          = _VkGetPhysicalDeviceProperties

{-# INLINE _VkGetPhysicalDeviceProperties #-}

_VkGetPhysicalDeviceProperties :: CString
_VkGetPhysicalDeviceProperties
  = Ptr "vkGetPhysicalDeviceProperties\NUL"##

{-# INLINE is_VkGetPhysicalDeviceProperties #-}

is_VkGetPhysicalDeviceProperties :: CString -> Bool
is_VkGetPhysicalDeviceProperties
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceProperties

type VkGetPhysicalDeviceProperties =
     "vkGetPhysicalDeviceProperties"

-- | > () vkGetPhysicalDeviceProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceProperties.html vkGetPhysicalDeviceProperties registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceProperties"
               vkGetPhysicalDeviceProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceProperties -- ^ pProperties
                                                                  -> IO ()

-- | > () vkGetPhysicalDeviceProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceProperties.html vkGetPhysicalDeviceProperties registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceProperties"
               vkGetPhysicalDevicePropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceProperties -- ^ pProperties
                                                                  -> IO ()

-- | > () vkGetPhysicalDeviceProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceProperties.html vkGetPhysicalDeviceProperties registry at www.khronos.org>
type HS_vkGetPhysicalDeviceProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Ptr VkPhysicalDeviceProperties -- ^ pProperties
                                                        -> IO ()

type PFN_vkGetPhysicalDeviceProperties =
     FunPtr HS_vkGetPhysicalDeviceProperties

foreign import ccall "dynamic" unwrapVkGetPhysicalDeviceProperties
               ::
               PFN_vkGetPhysicalDeviceProperties ->
                 HS_vkGetPhysicalDeviceProperties

instance VulkanProc "vkGetPhysicalDeviceProperties" where
        type VkProcType "vkGetPhysicalDeviceProperties" =
             HS_vkGetPhysicalDeviceProperties
        vkProcSymbol = _VkGetPhysicalDeviceProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceProperties

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceQueueFamilyProperties :: CString

pattern VkGetPhysicalDeviceQueueFamilyProperties <-
        (is_VkGetPhysicalDeviceQueueFamilyProperties -> True)
  where VkGetPhysicalDeviceQueueFamilyProperties
          = _VkGetPhysicalDeviceQueueFamilyProperties

{-# INLINE _VkGetPhysicalDeviceQueueFamilyProperties #-}

_VkGetPhysicalDeviceQueueFamilyProperties :: CString
_VkGetPhysicalDeviceQueueFamilyProperties
  = Ptr "vkGetPhysicalDeviceQueueFamilyProperties\NUL"##

{-# INLINE is_VkGetPhysicalDeviceQueueFamilyProperties #-}

is_VkGetPhysicalDeviceQueueFamilyProperties :: CString -> Bool
is_VkGetPhysicalDeviceQueueFamilyProperties
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceQueueFamilyProperties

type VkGetPhysicalDeviceQueueFamilyProperties =
     "vkGetPhysicalDeviceQueueFamilyProperties"

-- | > () vkGetPhysicalDeviceQueueFamilyProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceQueueFamilyProperties.html vkGetPhysicalDeviceQueueFamilyProperties registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceQueueFamilyProperties"
               vkGetPhysicalDeviceQueueFamilyProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pQueueFamilyPropertyCount
                            -> Ptr VkQueueFamilyProperties -- ^ pQueueFamilyProperties
                                                           -> IO ()

-- | > () vkGetPhysicalDeviceQueueFamilyProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceQueueFamilyProperties.html vkGetPhysicalDeviceQueueFamilyProperties registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceQueueFamilyProperties"
               vkGetPhysicalDeviceQueueFamilyPropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pQueueFamilyPropertyCount
                            -> Ptr VkQueueFamilyProperties -- ^ pQueueFamilyProperties
                                                           -> IO ()

-- | > () vkGetPhysicalDeviceQueueFamilyProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceQueueFamilyProperties.html vkGetPhysicalDeviceQueueFamilyProperties registry at www.khronos.org>
type HS_vkGetPhysicalDeviceQueueFamilyProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr Word32 -- ^ pQueueFamilyPropertyCount
                  -> Ptr VkQueueFamilyProperties -- ^ pQueueFamilyProperties
                                                 -> IO ()

type PFN_vkGetPhysicalDeviceQueueFamilyProperties =
     FunPtr HS_vkGetPhysicalDeviceQueueFamilyProperties

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceQueueFamilyProperties ::
               PFN_vkGetPhysicalDeviceQueueFamilyProperties ->
                 HS_vkGetPhysicalDeviceQueueFamilyProperties

instance VulkanProc "vkGetPhysicalDeviceQueueFamilyProperties"
         where
        type VkProcType "vkGetPhysicalDeviceQueueFamilyProperties" =
             HS_vkGetPhysicalDeviceQueueFamilyProperties
        vkProcSymbol = _VkGetPhysicalDeviceQueueFamilyProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceQueueFamilyProperties

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceMemoryProperties :: CString

pattern VkGetPhysicalDeviceMemoryProperties <-
        (is_VkGetPhysicalDeviceMemoryProperties -> True)
  where VkGetPhysicalDeviceMemoryProperties
          = _VkGetPhysicalDeviceMemoryProperties

{-# INLINE _VkGetPhysicalDeviceMemoryProperties #-}

_VkGetPhysicalDeviceMemoryProperties :: CString
_VkGetPhysicalDeviceMemoryProperties
  = Ptr "vkGetPhysicalDeviceMemoryProperties\NUL"##

{-# INLINE is_VkGetPhysicalDeviceMemoryProperties #-}

is_VkGetPhysicalDeviceMemoryProperties :: CString -> Bool
is_VkGetPhysicalDeviceMemoryProperties
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceMemoryProperties

type VkGetPhysicalDeviceMemoryProperties =
     "vkGetPhysicalDeviceMemoryProperties"

-- | > () vkGetPhysicalDeviceMemoryProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceMemoryProperties.html vkGetPhysicalDeviceMemoryProperties registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceMemoryProperties"
               vkGetPhysicalDeviceMemoryProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceMemoryProperties -- ^ pMemoryProperties
                                                                        -> IO ()

-- | > () vkGetPhysicalDeviceMemoryProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceMemoryProperties.html vkGetPhysicalDeviceMemoryProperties registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceMemoryProperties"
               vkGetPhysicalDeviceMemoryPropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceMemoryProperties -- ^ pMemoryProperties
                                                                        -> IO ()

-- | > () vkGetPhysicalDeviceMemoryProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceMemoryProperties.html vkGetPhysicalDeviceMemoryProperties registry at www.khronos.org>
type HS_vkGetPhysicalDeviceMemoryProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Ptr VkPhysicalDeviceMemoryProperties -- ^ pMemoryProperties
                                                              -> IO ()

type PFN_vkGetPhysicalDeviceMemoryProperties =
     FunPtr HS_vkGetPhysicalDeviceMemoryProperties

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceMemoryProperties ::
               PFN_vkGetPhysicalDeviceMemoryProperties ->
                 HS_vkGetPhysicalDeviceMemoryProperties

instance VulkanProc "vkGetPhysicalDeviceMemoryProperties" where
        type VkProcType "vkGetPhysicalDeviceMemoryProperties" =
             HS_vkGetPhysicalDeviceMemoryProperties
        vkProcSymbol = _VkGetPhysicalDeviceMemoryProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceMemoryProperties

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetInstanceProcAddr :: CString

pattern VkGetInstanceProcAddr <- (is_VkGetInstanceProcAddr -> True)
  where VkGetInstanceProcAddr = _VkGetInstanceProcAddr

{-# INLINE _VkGetInstanceProcAddr #-}

_VkGetInstanceProcAddr :: CString
_VkGetInstanceProcAddr = Ptr "vkGetInstanceProcAddr\NUL"##

{-# INLINE is_VkGetInstanceProcAddr #-}

is_VkGetInstanceProcAddr :: CString -> Bool
is_VkGetInstanceProcAddr
  = (EQ ==) . cmpCStrings _VkGetInstanceProcAddr

type VkGetInstanceProcAddr = "vkGetInstanceProcAddr"

-- | > PFN_vkVoidFunction vkGetInstanceProcAddr
--   >     ( VkInstance instance
--   >     , const char* pName
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetInstanceProcAddr.html vkGetInstanceProcAddr registry at www.khronos.org>
foreign import ccall unsafe "vkGetInstanceProcAddr"
               vkGetInstanceProcAddr ::
               VkInstance -- ^ instance
                          -> CString -- ^ pName
                                     -> IO PFN_vkVoidFunction

-- | > PFN_vkVoidFunction vkGetInstanceProcAddr
--   >     ( VkInstance instance
--   >     , const char* pName
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetInstanceProcAddr.html vkGetInstanceProcAddr registry at www.khronos.org>
foreign import ccall safe "vkGetInstanceProcAddr"
               vkGetInstanceProcAddrSafe ::
               VkInstance -- ^ instance
                          -> CString -- ^ pName
                                     -> IO PFN_vkVoidFunction

-- | > PFN_vkVoidFunction vkGetInstanceProcAddr
--   >     ( VkInstance instance
--   >     , const char* pName
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetInstanceProcAddr.html vkGetInstanceProcAddr registry at www.khronos.org>
type HS_vkGetInstanceProcAddr =
     VkInstance -- ^ instance
                -> CString -- ^ pName
                           -> IO PFN_vkVoidFunction

type PFN_vkGetInstanceProcAddr = FunPtr HS_vkGetInstanceProcAddr

foreign import ccall "dynamic" unwrapVkGetInstanceProcAddr ::
               PFN_vkGetInstanceProcAddr -> HS_vkGetInstanceProcAddr

instance VulkanProc "vkGetInstanceProcAddr" where
        type VkProcType "vkGetInstanceProcAddr" = HS_vkGetInstanceProcAddr
        vkProcSymbol = _VkGetInstanceProcAddr

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetInstanceProcAddr

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetDeviceProcAddr :: CString

pattern VkGetDeviceProcAddr <- (is_VkGetDeviceProcAddr -> True)
  where VkGetDeviceProcAddr = _VkGetDeviceProcAddr

{-# INLINE _VkGetDeviceProcAddr #-}

_VkGetDeviceProcAddr :: CString
_VkGetDeviceProcAddr = Ptr "vkGetDeviceProcAddr\NUL"##

{-# INLINE is_VkGetDeviceProcAddr #-}

is_VkGetDeviceProcAddr :: CString -> Bool
is_VkGetDeviceProcAddr = (EQ ==) . cmpCStrings _VkGetDeviceProcAddr

type VkGetDeviceProcAddr = "vkGetDeviceProcAddr"

-- | > PFN_vkVoidFunction vkGetDeviceProcAddr
--   >     ( VkDevice device
--   >     , const char* pName
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceProcAddr.html vkGetDeviceProcAddr registry at www.khronos.org>
foreign import ccall unsafe "vkGetDeviceProcAddr"
               vkGetDeviceProcAddr :: VkDevice -- ^ device
                                               -> CString -- ^ pName
                                                          -> IO PFN_vkVoidFunction

-- | > PFN_vkVoidFunction vkGetDeviceProcAddr
--   >     ( VkDevice device
--   >     , const char* pName
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceProcAddr.html vkGetDeviceProcAddr registry at www.khronos.org>
foreign import ccall safe "vkGetDeviceProcAddr"
               vkGetDeviceProcAddrSafe ::
               VkDevice -- ^ device
                        -> CString -- ^ pName
                                   -> IO PFN_vkVoidFunction

-- | > PFN_vkVoidFunction vkGetDeviceProcAddr
--   >     ( VkDevice device
--   >     , const char* pName
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceProcAddr.html vkGetDeviceProcAddr registry at www.khronos.org>
type HS_vkGetDeviceProcAddr =
     VkDevice -- ^ device
              -> CString -- ^ pName
                         -> IO PFN_vkVoidFunction

type PFN_vkGetDeviceProcAddr = FunPtr HS_vkGetDeviceProcAddr

foreign import ccall "dynamic" unwrapVkGetDeviceProcAddr ::
               PFN_vkGetDeviceProcAddr -> HS_vkGetDeviceProcAddr

instance VulkanProc "vkGetDeviceProcAddr" where
        type VkProcType "vkGetDeviceProcAddr" = HS_vkGetDeviceProcAddr
        vkProcSymbol = _VkGetDeviceProcAddr

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetDeviceProcAddr

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateDevice :: CString

pattern VkCreateDevice <- (is_VkCreateDevice -> True)
  where VkCreateDevice = _VkCreateDevice

{-# INLINE _VkCreateDevice #-}

_VkCreateDevice :: CString
_VkCreateDevice = Ptr "vkCreateDevice\NUL"##

{-# INLINE is_VkCreateDevice #-}

is_VkCreateDevice :: CString -> Bool
is_VkCreateDevice = (EQ ==) . cmpCStrings _VkCreateDevice

type VkCreateDevice = "vkCreateDevice"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED', 'VK_ERROR_EXTENSION_NOT_PRESENT', 'VK_ERROR_FEATURE_NOT_PRESENT', 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkCreateDevice
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkDeviceCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDevice* pDevice
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateDevice.html vkCreateDevice registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDevice" vkCreateDevice ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkDeviceCreateInfo -- ^ pCreateInfo
                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkDevice -- ^ pDevice
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED', 'VK_ERROR_EXTENSION_NOT_PRESENT', 'VK_ERROR_FEATURE_NOT_PRESENT', 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkCreateDevice
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkDeviceCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDevice* pDevice
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateDevice.html vkCreateDevice registry at www.khronos.org>
foreign import ccall safe "vkCreateDevice" vkCreateDeviceSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkDeviceCreateInfo -- ^ pCreateInfo
                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkDevice -- ^ pDevice
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED', 'VK_ERROR_EXTENSION_NOT_PRESENT', 'VK_ERROR_FEATURE_NOT_PRESENT', 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkCreateDevice
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkDeviceCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDevice* pDevice
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateDevice.html vkCreateDevice registry at www.khronos.org>
type HS_vkCreateDevice =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkDeviceCreateInfo -- ^ pCreateInfo
                              ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkDevice -- ^ pDevice
                                                   -> IO VkResult

type PFN_vkCreateDevice = FunPtr HS_vkCreateDevice

foreign import ccall "dynamic" unwrapVkCreateDevice ::
               PFN_vkCreateDevice -> HS_vkCreateDevice

instance VulkanProc "vkCreateDevice" where
        type VkProcType "vkCreateDevice" = HS_vkCreateDevice
        vkProcSymbol = _VkCreateDevice

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateDevice

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyDevice :: CString

pattern VkDestroyDevice <- (is_VkDestroyDevice -> True)
  where VkDestroyDevice = _VkDestroyDevice

{-# INLINE _VkDestroyDevice #-}

_VkDestroyDevice :: CString
_VkDestroyDevice = Ptr "vkDestroyDevice\NUL"##

{-# INLINE is_VkDestroyDevice #-}

is_VkDestroyDevice :: CString -> Bool
is_VkDestroyDevice = (EQ ==) . cmpCStrings _VkDestroyDevice

type VkDestroyDevice = "vkDestroyDevice"

-- | > () vkDestroyDevice
--   >     ( VkDevice device
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDevice.html vkDestroyDevice registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyDevice" vkDestroyDevice ::
               VkDevice -- ^ device
                        -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                     -> IO ()

-- | > () vkDestroyDevice
--   >     ( VkDevice device
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDevice.html vkDestroyDevice registry at www.khronos.org>
foreign import ccall safe "vkDestroyDevice" vkDestroyDeviceSafe ::
               VkDevice -- ^ device
                        -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                     -> IO ()

-- | > () vkDestroyDevice
--   >     ( VkDevice device
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDevice.html vkDestroyDevice registry at www.khronos.org>
type HS_vkDestroyDevice =
     VkDevice -- ^ device
              -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                           -> IO ()

type PFN_vkDestroyDevice = FunPtr HS_vkDestroyDevice

foreign import ccall "dynamic" unwrapVkDestroyDevice ::
               PFN_vkDestroyDevice -> HS_vkDestroyDevice

instance VulkanProc "vkDestroyDevice" where
        type VkProcType "vkDestroyDevice" = HS_vkDestroyDevice
        vkProcSymbol = _VkDestroyDevice

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyDevice

        {-# INLINE unwrapVkProcPtr #-}

pattern VkEnumerateInstanceExtensionProperties :: CString

pattern VkEnumerateInstanceExtensionProperties <-
        (is_VkEnumerateInstanceExtensionProperties -> True)
  where VkEnumerateInstanceExtensionProperties
          = _VkEnumerateInstanceExtensionProperties

{-# INLINE _VkEnumerateInstanceExtensionProperties #-}

_VkEnumerateInstanceExtensionProperties :: CString
_VkEnumerateInstanceExtensionProperties
  = Ptr "vkEnumerateInstanceExtensionProperties\NUL"##

{-# INLINE is_VkEnumerateInstanceExtensionProperties #-}

is_VkEnumerateInstanceExtensionProperties :: CString -> Bool
is_VkEnumerateInstanceExtensionProperties
  = (EQ ==) . cmpCStrings _VkEnumerateInstanceExtensionProperties

type VkEnumerateInstanceExtensionProperties =
     "vkEnumerateInstanceExtensionProperties"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_LAYER_NOT_PRESENT'.
--
--   > VkResult vkEnumerateInstanceExtensionProperties
--   >     ( const char* pLayerName
--   >     , uint32_t* pPropertyCount
--   >     , VkExtensionProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumerateInstanceExtensionProperties.html vkEnumerateInstanceExtensionProperties registry at www.khronos.org>
foreign import ccall unsafe
               "vkEnumerateInstanceExtensionProperties"
               vkEnumerateInstanceExtensionProperties ::
               CString -- ^ pLayerName
                       -> Ptr Word32 -- ^ pPropertyCount
                                     -> Ptr VkExtensionProperties -- ^ pProperties
                                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_LAYER_NOT_PRESENT'.
--
--   > VkResult vkEnumerateInstanceExtensionProperties
--   >     ( const char* pLayerName
--   >     , uint32_t* pPropertyCount
--   >     , VkExtensionProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumerateInstanceExtensionProperties.html vkEnumerateInstanceExtensionProperties registry at www.khronos.org>
foreign import ccall safe "vkEnumerateInstanceExtensionProperties"
               vkEnumerateInstanceExtensionPropertiesSafe ::
               CString -- ^ pLayerName
                       -> Ptr Word32 -- ^ pPropertyCount
                                     -> Ptr VkExtensionProperties -- ^ pProperties
                                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_LAYER_NOT_PRESENT'.
--
--   > VkResult vkEnumerateInstanceExtensionProperties
--   >     ( const char* pLayerName
--   >     , uint32_t* pPropertyCount
--   >     , VkExtensionProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumerateInstanceExtensionProperties.html vkEnumerateInstanceExtensionProperties registry at www.khronos.org>
type HS_vkEnumerateInstanceExtensionProperties =
     CString -- ^ pLayerName
             -> Ptr Word32 -- ^ pPropertyCount
                           -> Ptr VkExtensionProperties -- ^ pProperties
                                                        -> IO VkResult

type PFN_vkEnumerateInstanceExtensionProperties =
     FunPtr HS_vkEnumerateInstanceExtensionProperties

foreign import ccall "dynamic"
               unwrapVkEnumerateInstanceExtensionProperties ::
               PFN_vkEnumerateInstanceExtensionProperties ->
                 HS_vkEnumerateInstanceExtensionProperties

instance VulkanProc "vkEnumerateInstanceExtensionProperties" where
        type VkProcType "vkEnumerateInstanceExtensionProperties" =
             HS_vkEnumerateInstanceExtensionProperties
        vkProcSymbol = _VkEnumerateInstanceExtensionProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkEnumerateInstanceExtensionProperties

        {-# INLINE unwrapVkProcPtr #-}

pattern VkEnumerateDeviceExtensionProperties :: CString

pattern VkEnumerateDeviceExtensionProperties <-
        (is_VkEnumerateDeviceExtensionProperties -> True)
  where VkEnumerateDeviceExtensionProperties
          = _VkEnumerateDeviceExtensionProperties

{-# INLINE _VkEnumerateDeviceExtensionProperties #-}

_VkEnumerateDeviceExtensionProperties :: CString
_VkEnumerateDeviceExtensionProperties
  = Ptr "vkEnumerateDeviceExtensionProperties\NUL"##

{-# INLINE is_VkEnumerateDeviceExtensionProperties #-}

is_VkEnumerateDeviceExtensionProperties :: CString -> Bool
is_VkEnumerateDeviceExtensionProperties
  = (EQ ==) . cmpCStrings _VkEnumerateDeviceExtensionProperties

type VkEnumerateDeviceExtensionProperties =
     "vkEnumerateDeviceExtensionProperties"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_LAYER_NOT_PRESENT'.
--
--   > VkResult vkEnumerateDeviceExtensionProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const char* pLayerName
--   >     , uint32_t* pPropertyCount
--   >     , VkExtensionProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumerateDeviceExtensionProperties.html vkEnumerateDeviceExtensionProperties registry at www.khronos.org>
foreign import ccall unsafe "vkEnumerateDeviceExtensionProperties"
               vkEnumerateDeviceExtensionProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 CString -- ^ pLayerName
                         -> Ptr Word32 -- ^ pPropertyCount
                                       -> Ptr VkExtensionProperties -- ^ pProperties
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_LAYER_NOT_PRESENT'.
--
--   > VkResult vkEnumerateDeviceExtensionProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const char* pLayerName
--   >     , uint32_t* pPropertyCount
--   >     , VkExtensionProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumerateDeviceExtensionProperties.html vkEnumerateDeviceExtensionProperties registry at www.khronos.org>
foreign import ccall safe "vkEnumerateDeviceExtensionProperties"
               vkEnumerateDeviceExtensionPropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 CString -- ^ pLayerName
                         -> Ptr Word32 -- ^ pPropertyCount
                                       -> Ptr VkExtensionProperties -- ^ pProperties
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_LAYER_NOT_PRESENT'.
--
--   > VkResult vkEnumerateDeviceExtensionProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const char* pLayerName
--   >     , uint32_t* pPropertyCount
--   >     , VkExtensionProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumerateDeviceExtensionProperties.html vkEnumerateDeviceExtensionProperties registry at www.khronos.org>
type HS_vkEnumerateDeviceExtensionProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       CString -- ^ pLayerName
               -> Ptr Word32 -- ^ pPropertyCount
                             -> Ptr VkExtensionProperties -- ^ pProperties
                                                          -> IO VkResult

type PFN_vkEnumerateDeviceExtensionProperties =
     FunPtr HS_vkEnumerateDeviceExtensionProperties

foreign import ccall "dynamic"
               unwrapVkEnumerateDeviceExtensionProperties ::
               PFN_vkEnumerateDeviceExtensionProperties ->
                 HS_vkEnumerateDeviceExtensionProperties

instance VulkanProc "vkEnumerateDeviceExtensionProperties" where
        type VkProcType "vkEnumerateDeviceExtensionProperties" =
             HS_vkEnumerateDeviceExtensionProperties
        vkProcSymbol = _VkEnumerateDeviceExtensionProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkEnumerateDeviceExtensionProperties

        {-# INLINE unwrapVkProcPtr #-}

pattern VkEnumerateInstanceLayerProperties :: CString

pattern VkEnumerateInstanceLayerProperties <-
        (is_VkEnumerateInstanceLayerProperties -> True)
  where VkEnumerateInstanceLayerProperties
          = _VkEnumerateInstanceLayerProperties

{-# INLINE _VkEnumerateInstanceLayerProperties #-}

_VkEnumerateInstanceLayerProperties :: CString
_VkEnumerateInstanceLayerProperties
  = Ptr "vkEnumerateInstanceLayerProperties\NUL"##

{-# INLINE is_VkEnumerateInstanceLayerProperties #-}

is_VkEnumerateInstanceLayerProperties :: CString -> Bool
is_VkEnumerateInstanceLayerProperties
  = (EQ ==) . cmpCStrings _VkEnumerateInstanceLayerProperties

type VkEnumerateInstanceLayerProperties =
     "vkEnumerateInstanceLayerProperties"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkEnumerateInstanceLayerProperties
--   >     ( uint32_t* pPropertyCount
--   >     , VkLayerProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumerateInstanceLayerProperties.html vkEnumerateInstanceLayerProperties registry at www.khronos.org>
foreign import ccall unsafe "vkEnumerateInstanceLayerProperties"
               vkEnumerateInstanceLayerProperties ::
               Ptr Word32 -- ^ pPropertyCount
                          -> Ptr VkLayerProperties -- ^ pProperties
                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkEnumerateInstanceLayerProperties
--   >     ( uint32_t* pPropertyCount
--   >     , VkLayerProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumerateInstanceLayerProperties.html vkEnumerateInstanceLayerProperties registry at www.khronos.org>
foreign import ccall safe "vkEnumerateInstanceLayerProperties"
               vkEnumerateInstanceLayerPropertiesSafe ::
               Ptr Word32 -- ^ pPropertyCount
                          -> Ptr VkLayerProperties -- ^ pProperties
                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkEnumerateInstanceLayerProperties
--   >     ( uint32_t* pPropertyCount
--   >     , VkLayerProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumerateInstanceLayerProperties.html vkEnumerateInstanceLayerProperties registry at www.khronos.org>
type HS_vkEnumerateInstanceLayerProperties =
     Ptr Word32 -- ^ pPropertyCount
                -> Ptr VkLayerProperties -- ^ pProperties
                                         -> IO VkResult

type PFN_vkEnumerateInstanceLayerProperties =
     FunPtr HS_vkEnumerateInstanceLayerProperties

foreign import ccall "dynamic"
               unwrapVkEnumerateInstanceLayerProperties ::
               PFN_vkEnumerateInstanceLayerProperties ->
                 HS_vkEnumerateInstanceLayerProperties

instance VulkanProc "vkEnumerateInstanceLayerProperties" where
        type VkProcType "vkEnumerateInstanceLayerProperties" =
             HS_vkEnumerateInstanceLayerProperties
        vkProcSymbol = _VkEnumerateInstanceLayerProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkEnumerateInstanceLayerProperties

        {-# INLINE unwrapVkProcPtr #-}

pattern VkEnumerateDeviceLayerProperties :: CString

pattern VkEnumerateDeviceLayerProperties <-
        (is_VkEnumerateDeviceLayerProperties -> True)
  where VkEnumerateDeviceLayerProperties
          = _VkEnumerateDeviceLayerProperties

{-# INLINE _VkEnumerateDeviceLayerProperties #-}

_VkEnumerateDeviceLayerProperties :: CString
_VkEnumerateDeviceLayerProperties
  = Ptr "vkEnumerateDeviceLayerProperties\NUL"##

{-# INLINE is_VkEnumerateDeviceLayerProperties #-}

is_VkEnumerateDeviceLayerProperties :: CString -> Bool
is_VkEnumerateDeviceLayerProperties
  = (EQ ==) . cmpCStrings _VkEnumerateDeviceLayerProperties

type VkEnumerateDeviceLayerProperties =
     "vkEnumerateDeviceLayerProperties"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkEnumerateDeviceLayerProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkLayerProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumerateDeviceLayerProperties.html vkEnumerateDeviceLayerProperties registry at www.khronos.org>
foreign import ccall unsafe "vkEnumerateDeviceLayerProperties"
               vkEnumerateDeviceLayerProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pPropertyCount
                            -> Ptr VkLayerProperties -- ^ pProperties
                                                     -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkEnumerateDeviceLayerProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkLayerProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumerateDeviceLayerProperties.html vkEnumerateDeviceLayerProperties registry at www.khronos.org>
foreign import ccall safe "vkEnumerateDeviceLayerProperties"
               vkEnumerateDeviceLayerPropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pPropertyCount
                            -> Ptr VkLayerProperties -- ^ pProperties
                                                     -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkEnumerateDeviceLayerProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkLayerProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEnumerateDeviceLayerProperties.html vkEnumerateDeviceLayerProperties registry at www.khronos.org>
type HS_vkEnumerateDeviceLayerProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr Word32 -- ^ pPropertyCount
                  -> Ptr VkLayerProperties -- ^ pProperties
                                           -> IO VkResult

type PFN_vkEnumerateDeviceLayerProperties =
     FunPtr HS_vkEnumerateDeviceLayerProperties

foreign import ccall "dynamic"
               unwrapVkEnumerateDeviceLayerProperties ::
               PFN_vkEnumerateDeviceLayerProperties ->
                 HS_vkEnumerateDeviceLayerProperties

instance VulkanProc "vkEnumerateDeviceLayerProperties" where
        type VkProcType "vkEnumerateDeviceLayerProperties" =
             HS_vkEnumerateDeviceLayerProperties
        vkProcSymbol = _VkEnumerateDeviceLayerProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkEnumerateDeviceLayerProperties

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetDeviceQueue :: CString

pattern VkGetDeviceQueue <- (is_VkGetDeviceQueue -> True)
  where VkGetDeviceQueue = _VkGetDeviceQueue

{-# INLINE _VkGetDeviceQueue #-}

_VkGetDeviceQueue :: CString
_VkGetDeviceQueue = Ptr "vkGetDeviceQueue\NUL"##

{-# INLINE is_VkGetDeviceQueue #-}

is_VkGetDeviceQueue :: CString -> Bool
is_VkGetDeviceQueue = (EQ ==) . cmpCStrings _VkGetDeviceQueue

type VkGetDeviceQueue = "vkGetDeviceQueue"

-- | > () vkGetDeviceQueue
--   >     ( VkDevice device
--   >     , uint32_t queueFamilyIndex
--   >     , uint32_t queueIndex
--   >     , VkQueue* pQueue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceQueue.html vkGetDeviceQueue registry at www.khronos.org>
foreign import ccall unsafe "vkGetDeviceQueue" vkGetDeviceQueue ::
               VkDevice -- ^ device
                        -> Word32 -- ^ queueFamilyIndex
                                  -> Word32 -- ^ queueIndex
                                            -> Ptr VkQueue -- ^ pQueue
                                                           -> IO ()

-- | > () vkGetDeviceQueue
--   >     ( VkDevice device
--   >     , uint32_t queueFamilyIndex
--   >     , uint32_t queueIndex
--   >     , VkQueue* pQueue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceQueue.html vkGetDeviceQueue registry at www.khronos.org>
foreign import ccall safe "vkGetDeviceQueue" vkGetDeviceQueueSafe
               :: VkDevice -- ^ device
                           -> Word32 -- ^ queueFamilyIndex
                                     -> Word32 -- ^ queueIndex
                                               -> Ptr VkQueue -- ^ pQueue
                                                              -> IO ()

-- | > () vkGetDeviceQueue
--   >     ( VkDevice device
--   >     , uint32_t queueFamilyIndex
--   >     , uint32_t queueIndex
--   >     , VkQueue* pQueue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceQueue.html vkGetDeviceQueue registry at www.khronos.org>
type HS_vkGetDeviceQueue =
     VkDevice -- ^ device
              -> Word32 -- ^ queueFamilyIndex
                        -> Word32 -- ^ queueIndex
                                  -> Ptr VkQueue -- ^ pQueue
                                                 -> IO ()

type PFN_vkGetDeviceQueue = FunPtr HS_vkGetDeviceQueue

foreign import ccall "dynamic" unwrapVkGetDeviceQueue ::
               PFN_vkGetDeviceQueue -> HS_vkGetDeviceQueue

instance VulkanProc "vkGetDeviceQueue" where
        type VkProcType "vkGetDeviceQueue" = HS_vkGetDeviceQueue
        vkProcSymbol = _VkGetDeviceQueue

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetDeviceQueue

        {-# INLINE unwrapVkProcPtr #-}

pattern VkQueueSubmit :: CString

pattern VkQueueSubmit <- (is_VkQueueSubmit -> True)
  where VkQueueSubmit = _VkQueueSubmit

{-# INLINE _VkQueueSubmit #-}

_VkQueueSubmit :: CString
_VkQueueSubmit = Ptr "vkQueueSubmit\NUL"##

{-# INLINE is_VkQueueSubmit #-}

is_VkQueueSubmit :: CString -> Bool
is_VkQueueSubmit = (EQ ==) . cmpCStrings _VkQueueSubmit

type VkQueueSubmit = "vkQueueSubmit"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkQueueSubmit
--   >     ( VkQueue queue
--   >     , uint32_t submitCount
--   >     , const VkSubmitInfo* pSubmits
--   >     , VkFence fence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueueSubmit.html vkQueueSubmit registry at www.khronos.org>
foreign import ccall unsafe "vkQueueSubmit" vkQueueSubmit ::
               VkQueue -- ^ queue
                       -> Word32 -- ^ submitCount
                                 -> Ptr VkSubmitInfo -- ^ pSubmits
                                                     -> VkFence -- ^ fence
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkQueueSubmit
--   >     ( VkQueue queue
--   >     , uint32_t submitCount
--   >     , const VkSubmitInfo* pSubmits
--   >     , VkFence fence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueueSubmit.html vkQueueSubmit registry at www.khronos.org>
foreign import ccall safe "vkQueueSubmit" vkQueueSubmitSafe ::
               VkQueue -- ^ queue
                       -> Word32 -- ^ submitCount
                                 -> Ptr VkSubmitInfo -- ^ pSubmits
                                                     -> VkFence -- ^ fence
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkQueueSubmit
--   >     ( VkQueue queue
--   >     , uint32_t submitCount
--   >     , const VkSubmitInfo* pSubmits
--   >     , VkFence fence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueueSubmit.html vkQueueSubmit registry at www.khronos.org>
type HS_vkQueueSubmit =
     VkQueue -- ^ queue
             -> Word32 -- ^ submitCount
                       -> Ptr VkSubmitInfo -- ^ pSubmits
                                           -> VkFence -- ^ fence
                                                      -> IO VkResult

type PFN_vkQueueSubmit = FunPtr HS_vkQueueSubmit

foreign import ccall "dynamic" unwrapVkQueueSubmit ::
               PFN_vkQueueSubmit -> HS_vkQueueSubmit

instance VulkanProc "vkQueueSubmit" where
        type VkProcType "vkQueueSubmit" = HS_vkQueueSubmit
        vkProcSymbol = _VkQueueSubmit

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkQueueSubmit

        {-# INLINE unwrapVkProcPtr #-}

pattern VkQueueWaitIdle :: CString

pattern VkQueueWaitIdle <- (is_VkQueueWaitIdle -> True)
  where VkQueueWaitIdle = _VkQueueWaitIdle

{-# INLINE _VkQueueWaitIdle #-}

_VkQueueWaitIdle :: CString
_VkQueueWaitIdle = Ptr "vkQueueWaitIdle\NUL"##

{-# INLINE is_VkQueueWaitIdle #-}

is_VkQueueWaitIdle :: CString -> Bool
is_VkQueueWaitIdle = (EQ ==) . cmpCStrings _VkQueueWaitIdle

type VkQueueWaitIdle = "vkQueueWaitIdle"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkQueueWaitIdle
--   >     ( VkQueue queue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueueWaitIdle.html vkQueueWaitIdle registry at www.khronos.org>
foreign import ccall unsafe "vkQueueWaitIdle" vkQueueWaitIdle ::
               VkQueue -- ^ queue
                       -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkQueueWaitIdle
--   >     ( VkQueue queue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueueWaitIdle.html vkQueueWaitIdle registry at www.khronos.org>
foreign import ccall safe "vkQueueWaitIdle" vkQueueWaitIdleSafe ::
               VkQueue -- ^ queue
                       -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkQueueWaitIdle
--   >     ( VkQueue queue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueueWaitIdle.html vkQueueWaitIdle registry at www.khronos.org>
type HS_vkQueueWaitIdle = VkQueue -- ^ queue
                                  -> IO VkResult

type PFN_vkQueueWaitIdle = FunPtr HS_vkQueueWaitIdle

foreign import ccall "dynamic" unwrapVkQueueWaitIdle ::
               PFN_vkQueueWaitIdle -> HS_vkQueueWaitIdle

instance VulkanProc "vkQueueWaitIdle" where
        type VkProcType "vkQueueWaitIdle" = HS_vkQueueWaitIdle
        vkProcSymbol = _VkQueueWaitIdle

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkQueueWaitIdle

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDeviceWaitIdle :: CString

pattern VkDeviceWaitIdle <- (is_VkDeviceWaitIdle -> True)
  where VkDeviceWaitIdle = _VkDeviceWaitIdle

{-# INLINE _VkDeviceWaitIdle #-}

_VkDeviceWaitIdle :: CString
_VkDeviceWaitIdle = Ptr "vkDeviceWaitIdle\NUL"##

{-# INLINE is_VkDeviceWaitIdle #-}

is_VkDeviceWaitIdle :: CString -> Bool
is_VkDeviceWaitIdle = (EQ ==) . cmpCStrings _VkDeviceWaitIdle

type VkDeviceWaitIdle = "vkDeviceWaitIdle"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkDeviceWaitIdle
--   >     ( VkDevice device
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDeviceWaitIdle.html vkDeviceWaitIdle registry at www.khronos.org>
foreign import ccall unsafe "vkDeviceWaitIdle" vkDeviceWaitIdle ::
               VkDevice -- ^ device
                        -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkDeviceWaitIdle
--   >     ( VkDevice device
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDeviceWaitIdle.html vkDeviceWaitIdle registry at www.khronos.org>
foreign import ccall safe "vkDeviceWaitIdle" vkDeviceWaitIdleSafe
               :: VkDevice -- ^ device
                           -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkDeviceWaitIdle
--   >     ( VkDevice device
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDeviceWaitIdle.html vkDeviceWaitIdle registry at www.khronos.org>
type HS_vkDeviceWaitIdle = VkDevice -- ^ device
                                    -> IO VkResult

type PFN_vkDeviceWaitIdle = FunPtr HS_vkDeviceWaitIdle

foreign import ccall "dynamic" unwrapVkDeviceWaitIdle ::
               PFN_vkDeviceWaitIdle -> HS_vkDeviceWaitIdle

instance VulkanProc "vkDeviceWaitIdle" where
        type VkProcType "vkDeviceWaitIdle" = HS_vkDeviceWaitIdle
        vkProcSymbol = _VkDeviceWaitIdle

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDeviceWaitIdle

        {-# INLINE unwrapVkProcPtr #-}

pattern VkAllocateMemory :: CString

pattern VkAllocateMemory <- (is_VkAllocateMemory -> True)
  where VkAllocateMemory = _VkAllocateMemory

{-# INLINE _VkAllocateMemory #-}

_VkAllocateMemory :: CString
_VkAllocateMemory = Ptr "vkAllocateMemory\NUL"##

{-# INLINE is_VkAllocateMemory #-}

is_VkAllocateMemory :: CString -> Bool
is_VkAllocateMemory = (EQ ==) . cmpCStrings _VkAllocateMemory

type VkAllocateMemory = "vkAllocateMemory"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_INVALID_EXTERNAL_HANDLE'.
--
--   > VkResult vkAllocateMemory
--   >     ( VkDevice device
--   >     , const VkMemoryAllocateInfo* pAllocateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDeviceMemory* pMemory
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkAllocateMemory.html vkAllocateMemory registry at www.khronos.org>
foreign import ccall unsafe "vkAllocateMemory" vkAllocateMemory ::
               VkDevice -- ^ device
                        ->
                 Ptr VkMemoryAllocateInfo -- ^ pAllocateInfo
                                          ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkDeviceMemory -- ^ pMemory
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_INVALID_EXTERNAL_HANDLE'.
--
--   > VkResult vkAllocateMemory
--   >     ( VkDevice device
--   >     , const VkMemoryAllocateInfo* pAllocateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDeviceMemory* pMemory
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkAllocateMemory.html vkAllocateMemory registry at www.khronos.org>
foreign import ccall safe "vkAllocateMemory" vkAllocateMemorySafe
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkMemoryAllocateInfo -- ^ pAllocateInfo
                                          ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkDeviceMemory -- ^ pMemory
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_INVALID_EXTERNAL_HANDLE'.
--
--   > VkResult vkAllocateMemory
--   >     ( VkDevice device
--   >     , const VkMemoryAllocateInfo* pAllocateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDeviceMemory* pMemory
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkAllocateMemory.html vkAllocateMemory registry at www.khronos.org>
type HS_vkAllocateMemory =
     VkDevice -- ^ device
              ->
       Ptr VkMemoryAllocateInfo -- ^ pAllocateInfo
                                ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkDeviceMemory -- ^ pMemory
                                                         -> IO VkResult

type PFN_vkAllocateMemory = FunPtr HS_vkAllocateMemory

foreign import ccall "dynamic" unwrapVkAllocateMemory ::
               PFN_vkAllocateMemory -> HS_vkAllocateMemory

instance VulkanProc "vkAllocateMemory" where
        type VkProcType "vkAllocateMemory" = HS_vkAllocateMemory
        vkProcSymbol = _VkAllocateMemory

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkAllocateMemory

        {-# INLINE unwrapVkProcPtr #-}

pattern VkFreeMemory :: CString

pattern VkFreeMemory <- (is_VkFreeMemory -> True)
  where VkFreeMemory = _VkFreeMemory

{-# INLINE _VkFreeMemory #-}

_VkFreeMemory :: CString
_VkFreeMemory = Ptr "vkFreeMemory\NUL"##

{-# INLINE is_VkFreeMemory #-}

is_VkFreeMemory :: CString -> Bool
is_VkFreeMemory = (EQ ==) . cmpCStrings _VkFreeMemory

type VkFreeMemory = "vkFreeMemory"

-- | > () vkFreeMemory
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkFreeMemory.html vkFreeMemory registry at www.khronos.org>
foreign import ccall unsafe "vkFreeMemory" vkFreeMemory ::
               VkDevice -- ^ device
                        -> VkDeviceMemory -- ^ memory
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

-- | > () vkFreeMemory
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkFreeMemory.html vkFreeMemory registry at www.khronos.org>
foreign import ccall safe "vkFreeMemory" vkFreeMemorySafe ::
               VkDevice -- ^ device
                        -> VkDeviceMemory -- ^ memory
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

-- | > () vkFreeMemory
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkFreeMemory.html vkFreeMemory registry at www.khronos.org>
type HS_vkFreeMemory =
     VkDevice -- ^ device
              -> VkDeviceMemory -- ^ memory
                                -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                             -> IO ()

type PFN_vkFreeMemory = FunPtr HS_vkFreeMemory

foreign import ccall "dynamic" unwrapVkFreeMemory ::
               PFN_vkFreeMemory -> HS_vkFreeMemory

instance VulkanProc "vkFreeMemory" where
        type VkProcType "vkFreeMemory" = HS_vkFreeMemory
        vkProcSymbol = _VkFreeMemory

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkFreeMemory

        {-# INLINE unwrapVkProcPtr #-}

pattern VkMapMemory :: CString

pattern VkMapMemory <- (is_VkMapMemory -> True)
  where VkMapMemory = _VkMapMemory

{-# INLINE _VkMapMemory #-}

_VkMapMemory :: CString
_VkMapMemory = Ptr "vkMapMemory\NUL"##

{-# INLINE is_VkMapMemory #-}

is_VkMapMemory :: CString -> Bool
is_VkMapMemory = (EQ ==) . cmpCStrings _VkMapMemory

type VkMapMemory = "vkMapMemory"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_MEMORY_MAP_FAILED'.
--
--   > VkResult vkMapMemory
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize offset
--   >     , VkDeviceSize size
--   >     , VkMemoryMapFlags flags
--   >     , void** ppData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkMapMemory.html vkMapMemory registry at www.khronos.org>
foreign import ccall unsafe "vkMapMemory" vkMapMemory ::
               VkDevice -- ^ device
                        ->
                 VkDeviceMemory -- ^ memory
                                ->
                   VkDeviceSize -- ^ offset
                                ->
                     VkDeviceSize -- ^ size
                                  -> VkMemoryMapFlags -- ^ flags
                                                      -> Ptr (Ptr Void) -- ^ ppData
                                                                        -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_MEMORY_MAP_FAILED'.
--
--   > VkResult vkMapMemory
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize offset
--   >     , VkDeviceSize size
--   >     , VkMemoryMapFlags flags
--   >     , void** ppData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkMapMemory.html vkMapMemory registry at www.khronos.org>
foreign import ccall safe "vkMapMemory" vkMapMemorySafe ::
               VkDevice -- ^ device
                        ->
                 VkDeviceMemory -- ^ memory
                                ->
                   VkDeviceSize -- ^ offset
                                ->
                     VkDeviceSize -- ^ size
                                  -> VkMemoryMapFlags -- ^ flags
                                                      -> Ptr (Ptr Void) -- ^ ppData
                                                                        -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_MEMORY_MAP_FAILED'.
--
--   > VkResult vkMapMemory
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize offset
--   >     , VkDeviceSize size
--   >     , VkMemoryMapFlags flags
--   >     , void** ppData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkMapMemory.html vkMapMemory registry at www.khronos.org>
type HS_vkMapMemory =
     VkDevice -- ^ device
              ->
       VkDeviceMemory -- ^ memory
                      ->
         VkDeviceSize -- ^ offset
                      ->
           VkDeviceSize -- ^ size
                        -> VkMemoryMapFlags -- ^ flags
                                            -> Ptr (Ptr Void) -- ^ ppData
                                                              -> IO VkResult

type PFN_vkMapMemory = FunPtr HS_vkMapMemory

foreign import ccall "dynamic" unwrapVkMapMemory ::
               PFN_vkMapMemory -> HS_vkMapMemory

instance VulkanProc "vkMapMemory" where
        type VkProcType "vkMapMemory" = HS_vkMapMemory
        vkProcSymbol = _VkMapMemory

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkMapMemory

        {-# INLINE unwrapVkProcPtr #-}

pattern VkUnmapMemory :: CString

pattern VkUnmapMemory <- (is_VkUnmapMemory -> True)
  where VkUnmapMemory = _VkUnmapMemory

{-# INLINE _VkUnmapMemory #-}

_VkUnmapMemory :: CString
_VkUnmapMemory = Ptr "vkUnmapMemory\NUL"##

{-# INLINE is_VkUnmapMemory #-}

is_VkUnmapMemory :: CString -> Bool
is_VkUnmapMemory = (EQ ==) . cmpCStrings _VkUnmapMemory

type VkUnmapMemory = "vkUnmapMemory"

-- | > () vkUnmapMemory
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkUnmapMemory.html vkUnmapMemory registry at www.khronos.org>
foreign import ccall unsafe "vkUnmapMemory" vkUnmapMemory ::
               VkDevice -- ^ device
                        -> VkDeviceMemory -- ^ memory
                                          -> IO ()

-- | > () vkUnmapMemory
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkUnmapMemory.html vkUnmapMemory registry at www.khronos.org>
foreign import ccall safe "vkUnmapMemory" vkUnmapMemorySafe ::
               VkDevice -- ^ device
                        -> VkDeviceMemory -- ^ memory
                                          -> IO ()

-- | > () vkUnmapMemory
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkUnmapMemory.html vkUnmapMemory registry at www.khronos.org>
type HS_vkUnmapMemory = VkDevice -- ^ device
                                 -> VkDeviceMemory -- ^ memory
                                                   -> IO ()

type PFN_vkUnmapMemory = FunPtr HS_vkUnmapMemory

foreign import ccall "dynamic" unwrapVkUnmapMemory ::
               PFN_vkUnmapMemory -> HS_vkUnmapMemory

instance VulkanProc "vkUnmapMemory" where
        type VkProcType "vkUnmapMemory" = HS_vkUnmapMemory
        vkProcSymbol = _VkUnmapMemory

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkUnmapMemory

        {-# INLINE unwrapVkProcPtr #-}

pattern VkFlushMappedMemoryRanges :: CString

pattern VkFlushMappedMemoryRanges <-
        (is_VkFlushMappedMemoryRanges -> True)
  where VkFlushMappedMemoryRanges = _VkFlushMappedMemoryRanges

{-# INLINE _VkFlushMappedMemoryRanges #-}

_VkFlushMappedMemoryRanges :: CString
_VkFlushMappedMemoryRanges = Ptr "vkFlushMappedMemoryRanges\NUL"##

{-# INLINE is_VkFlushMappedMemoryRanges #-}

is_VkFlushMappedMemoryRanges :: CString -> Bool
is_VkFlushMappedMemoryRanges
  = (EQ ==) . cmpCStrings _VkFlushMappedMemoryRanges

type VkFlushMappedMemoryRanges = "vkFlushMappedMemoryRanges"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkFlushMappedMemoryRanges
--   >     ( VkDevice device
--   >     , uint32_t memoryRangeCount
--   >     , const VkMappedMemoryRange* pMemoryRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkFlushMappedMemoryRanges.html vkFlushMappedMemoryRanges registry at www.khronos.org>
foreign import ccall unsafe "vkFlushMappedMemoryRanges"
               vkFlushMappedMemoryRanges ::
               VkDevice -- ^ device
                        -> Word32 -- ^ memoryRangeCount
                                  -> Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkFlushMappedMemoryRanges
--   >     ( VkDevice device
--   >     , uint32_t memoryRangeCount
--   >     , const VkMappedMemoryRange* pMemoryRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkFlushMappedMemoryRanges.html vkFlushMappedMemoryRanges registry at www.khronos.org>
foreign import ccall safe "vkFlushMappedMemoryRanges"
               vkFlushMappedMemoryRangesSafe ::
               VkDevice -- ^ device
                        -> Word32 -- ^ memoryRangeCount
                                  -> Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkFlushMappedMemoryRanges
--   >     ( VkDevice device
--   >     , uint32_t memoryRangeCount
--   >     , const VkMappedMemoryRange* pMemoryRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkFlushMappedMemoryRanges.html vkFlushMappedMemoryRanges registry at www.khronos.org>
type HS_vkFlushMappedMemoryRanges =
     VkDevice -- ^ device
              -> Word32 -- ^ memoryRangeCount
                        -> Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                   -> IO VkResult

type PFN_vkFlushMappedMemoryRanges =
     FunPtr HS_vkFlushMappedMemoryRanges

foreign import ccall "dynamic" unwrapVkFlushMappedMemoryRanges ::
               PFN_vkFlushMappedMemoryRanges -> HS_vkFlushMappedMemoryRanges

instance VulkanProc "vkFlushMappedMemoryRanges" where
        type VkProcType "vkFlushMappedMemoryRanges" =
             HS_vkFlushMappedMemoryRanges
        vkProcSymbol = _VkFlushMappedMemoryRanges

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkFlushMappedMemoryRanges

        {-# INLINE unwrapVkProcPtr #-}

pattern VkInvalidateMappedMemoryRanges :: CString

pattern VkInvalidateMappedMemoryRanges <-
        (is_VkInvalidateMappedMemoryRanges -> True)
  where VkInvalidateMappedMemoryRanges
          = _VkInvalidateMappedMemoryRanges

{-# INLINE _VkInvalidateMappedMemoryRanges #-}

_VkInvalidateMappedMemoryRanges :: CString
_VkInvalidateMappedMemoryRanges
  = Ptr "vkInvalidateMappedMemoryRanges\NUL"##

{-# INLINE is_VkInvalidateMappedMemoryRanges #-}

is_VkInvalidateMappedMemoryRanges :: CString -> Bool
is_VkInvalidateMappedMemoryRanges
  = (EQ ==) . cmpCStrings _VkInvalidateMappedMemoryRanges

type VkInvalidateMappedMemoryRanges =
     "vkInvalidateMappedMemoryRanges"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkInvalidateMappedMemoryRanges
--   >     ( VkDevice device
--   >     , uint32_t memoryRangeCount
--   >     , const VkMappedMemoryRange* pMemoryRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkInvalidateMappedMemoryRanges.html vkInvalidateMappedMemoryRanges registry at www.khronos.org>
foreign import ccall unsafe "vkInvalidateMappedMemoryRanges"
               vkInvalidateMappedMemoryRanges ::
               VkDevice -- ^ device
                        -> Word32 -- ^ memoryRangeCount
                                  -> Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkInvalidateMappedMemoryRanges
--   >     ( VkDevice device
--   >     , uint32_t memoryRangeCount
--   >     , const VkMappedMemoryRange* pMemoryRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkInvalidateMappedMemoryRanges.html vkInvalidateMappedMemoryRanges registry at www.khronos.org>
foreign import ccall safe "vkInvalidateMappedMemoryRanges"
               vkInvalidateMappedMemoryRangesSafe ::
               VkDevice -- ^ device
                        -> Word32 -- ^ memoryRangeCount
                                  -> Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkInvalidateMappedMemoryRanges
--   >     ( VkDevice device
--   >     , uint32_t memoryRangeCount
--   >     , const VkMappedMemoryRange* pMemoryRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkInvalidateMappedMemoryRanges.html vkInvalidateMappedMemoryRanges registry at www.khronos.org>
type HS_vkInvalidateMappedMemoryRanges =
     VkDevice -- ^ device
              -> Word32 -- ^ memoryRangeCount
                        -> Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                   -> IO VkResult

type PFN_vkInvalidateMappedMemoryRanges =
     FunPtr HS_vkInvalidateMappedMemoryRanges

foreign import ccall "dynamic" unwrapVkInvalidateMappedMemoryRanges
               ::
               PFN_vkInvalidateMappedMemoryRanges ->
                 HS_vkInvalidateMappedMemoryRanges

instance VulkanProc "vkInvalidateMappedMemoryRanges" where
        type VkProcType "vkInvalidateMappedMemoryRanges" =
             HS_vkInvalidateMappedMemoryRanges
        vkProcSymbol = _VkInvalidateMappedMemoryRanges

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkInvalidateMappedMemoryRanges

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetDeviceMemoryCommitment :: CString

pattern VkGetDeviceMemoryCommitment <-
        (is_VkGetDeviceMemoryCommitment -> True)
  where VkGetDeviceMemoryCommitment = _VkGetDeviceMemoryCommitment

{-# INLINE _VkGetDeviceMemoryCommitment #-}

_VkGetDeviceMemoryCommitment :: CString
_VkGetDeviceMemoryCommitment
  = Ptr "vkGetDeviceMemoryCommitment\NUL"##

{-# INLINE is_VkGetDeviceMemoryCommitment #-}

is_VkGetDeviceMemoryCommitment :: CString -> Bool
is_VkGetDeviceMemoryCommitment
  = (EQ ==) . cmpCStrings _VkGetDeviceMemoryCommitment

type VkGetDeviceMemoryCommitment = "vkGetDeviceMemoryCommitment"

-- | > () vkGetDeviceMemoryCommitment
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize* pCommittedMemoryInBytes
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceMemoryCommitment.html vkGetDeviceMemoryCommitment registry at www.khronos.org>
foreign import ccall unsafe "vkGetDeviceMemoryCommitment"
               vkGetDeviceMemoryCommitment ::
               VkDevice -- ^ device
                        -> VkDeviceMemory -- ^ memory
                                          -> Ptr VkDeviceSize -- ^ pCommittedMemoryInBytes
                                                              -> IO ()

-- | > () vkGetDeviceMemoryCommitment
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize* pCommittedMemoryInBytes
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceMemoryCommitment.html vkGetDeviceMemoryCommitment registry at www.khronos.org>
foreign import ccall safe "vkGetDeviceMemoryCommitment"
               vkGetDeviceMemoryCommitmentSafe ::
               VkDevice -- ^ device
                        -> VkDeviceMemory -- ^ memory
                                          -> Ptr VkDeviceSize -- ^ pCommittedMemoryInBytes
                                                              -> IO ()

-- | > () vkGetDeviceMemoryCommitment
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize* pCommittedMemoryInBytes
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceMemoryCommitment.html vkGetDeviceMemoryCommitment registry at www.khronos.org>
type HS_vkGetDeviceMemoryCommitment =
     VkDevice -- ^ device
              -> VkDeviceMemory -- ^ memory
                                -> Ptr VkDeviceSize -- ^ pCommittedMemoryInBytes
                                                    -> IO ()

type PFN_vkGetDeviceMemoryCommitment =
     FunPtr HS_vkGetDeviceMemoryCommitment

foreign import ccall "dynamic" unwrapVkGetDeviceMemoryCommitment ::
               PFN_vkGetDeviceMemoryCommitment -> HS_vkGetDeviceMemoryCommitment

instance VulkanProc "vkGetDeviceMemoryCommitment" where
        type VkProcType "vkGetDeviceMemoryCommitment" =
             HS_vkGetDeviceMemoryCommitment
        vkProcSymbol = _VkGetDeviceMemoryCommitment

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetDeviceMemoryCommitment

        {-# INLINE unwrapVkProcPtr #-}

pattern VkBindBufferMemory :: CString

pattern VkBindBufferMemory <- (is_VkBindBufferMemory -> True)
  where VkBindBufferMemory = _VkBindBufferMemory

{-# INLINE _VkBindBufferMemory #-}

_VkBindBufferMemory :: CString
_VkBindBufferMemory = Ptr "vkBindBufferMemory\NUL"##

{-# INLINE is_VkBindBufferMemory #-}

is_VkBindBufferMemory :: CString -> Bool
is_VkBindBufferMemory = (EQ ==) . cmpCStrings _VkBindBufferMemory

type VkBindBufferMemory = "vkBindBufferMemory"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindBufferMemory
--   >     ( VkDevice device
--   >     , VkBuffer buffer
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize memoryOffset
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkBindBufferMemory.html vkBindBufferMemory registry at www.khronos.org>
foreign import ccall unsafe "vkBindBufferMemory" vkBindBufferMemory
               ::
               VkDevice -- ^ device
                        ->
                 VkBuffer -- ^ buffer
                          -> VkDeviceMemory -- ^ memory
                                            -> VkDeviceSize -- ^ memoryOffset
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindBufferMemory
--   >     ( VkDevice device
--   >     , VkBuffer buffer
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize memoryOffset
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkBindBufferMemory.html vkBindBufferMemory registry at www.khronos.org>
foreign import ccall safe "vkBindBufferMemory"
               vkBindBufferMemorySafe ::
               VkDevice -- ^ device
                        ->
                 VkBuffer -- ^ buffer
                          -> VkDeviceMemory -- ^ memory
                                            -> VkDeviceSize -- ^ memoryOffset
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindBufferMemory
--   >     ( VkDevice device
--   >     , VkBuffer buffer
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize memoryOffset
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkBindBufferMemory.html vkBindBufferMemory registry at www.khronos.org>
type HS_vkBindBufferMemory =
     VkDevice -- ^ device
              ->
       VkBuffer -- ^ buffer
                -> VkDeviceMemory -- ^ memory
                                  -> VkDeviceSize -- ^ memoryOffset
                                                  -> IO VkResult

type PFN_vkBindBufferMemory = FunPtr HS_vkBindBufferMemory

foreign import ccall "dynamic" unwrapVkBindBufferMemory ::
               PFN_vkBindBufferMemory -> HS_vkBindBufferMemory

instance VulkanProc "vkBindBufferMemory" where
        type VkProcType "vkBindBufferMemory" = HS_vkBindBufferMemory
        vkProcSymbol = _VkBindBufferMemory

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkBindBufferMemory

        {-# INLINE unwrapVkProcPtr #-}

pattern VkBindImageMemory :: CString

pattern VkBindImageMemory <- (is_VkBindImageMemory -> True)
  where VkBindImageMemory = _VkBindImageMemory

{-# INLINE _VkBindImageMemory #-}

_VkBindImageMemory :: CString
_VkBindImageMemory = Ptr "vkBindImageMemory\NUL"##

{-# INLINE is_VkBindImageMemory #-}

is_VkBindImageMemory :: CString -> Bool
is_VkBindImageMemory = (EQ ==) . cmpCStrings _VkBindImageMemory

type VkBindImageMemory = "vkBindImageMemory"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindImageMemory
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize memoryOffset
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkBindImageMemory.html vkBindImageMemory registry at www.khronos.org>
foreign import ccall unsafe "vkBindImageMemory" vkBindImageMemory
               ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         -> VkDeviceMemory -- ^ memory
                                           -> VkDeviceSize -- ^ memoryOffset
                                                           -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindImageMemory
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize memoryOffset
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkBindImageMemory.html vkBindImageMemory registry at www.khronos.org>
foreign import ccall safe "vkBindImageMemory" vkBindImageMemorySafe
               ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         -> VkDeviceMemory -- ^ memory
                                           -> VkDeviceSize -- ^ memoryOffset
                                                           -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindImageMemory
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize memoryOffset
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkBindImageMemory.html vkBindImageMemory registry at www.khronos.org>
type HS_vkBindImageMemory =
     VkDevice -- ^ device
              ->
       VkImage -- ^ image
               -> VkDeviceMemory -- ^ memory
                                 -> VkDeviceSize -- ^ memoryOffset
                                                 -> IO VkResult

type PFN_vkBindImageMemory = FunPtr HS_vkBindImageMemory

foreign import ccall "dynamic" unwrapVkBindImageMemory ::
               PFN_vkBindImageMemory -> HS_vkBindImageMemory

instance VulkanProc "vkBindImageMemory" where
        type VkProcType "vkBindImageMemory" = HS_vkBindImageMemory
        vkProcSymbol = _VkBindImageMemory

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkBindImageMemory

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetBufferMemoryRequirements :: CString

pattern VkGetBufferMemoryRequirements <-
        (is_VkGetBufferMemoryRequirements -> True)
  where VkGetBufferMemoryRequirements
          = _VkGetBufferMemoryRequirements

{-# INLINE _VkGetBufferMemoryRequirements #-}

_VkGetBufferMemoryRequirements :: CString
_VkGetBufferMemoryRequirements
  = Ptr "vkGetBufferMemoryRequirements\NUL"##

{-# INLINE is_VkGetBufferMemoryRequirements #-}

is_VkGetBufferMemoryRequirements :: CString -> Bool
is_VkGetBufferMemoryRequirements
  = (EQ ==) . cmpCStrings _VkGetBufferMemoryRequirements

type VkGetBufferMemoryRequirements =
     "vkGetBufferMemoryRequirements"

-- | > () vkGetBufferMemoryRequirements
--   >     ( VkDevice device
--   >     , VkBuffer buffer
--   >     , VkMemoryRequirements* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetBufferMemoryRequirements.html vkGetBufferMemoryRequirements registry at www.khronos.org>
foreign import ccall unsafe "vkGetBufferMemoryRequirements"
               vkGetBufferMemoryRequirements ::
               VkDevice -- ^ device
                        -> VkBuffer -- ^ buffer
                                    -> Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                                -> IO ()

-- | > () vkGetBufferMemoryRequirements
--   >     ( VkDevice device
--   >     , VkBuffer buffer
--   >     , VkMemoryRequirements* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetBufferMemoryRequirements.html vkGetBufferMemoryRequirements registry at www.khronos.org>
foreign import ccall safe "vkGetBufferMemoryRequirements"
               vkGetBufferMemoryRequirementsSafe ::
               VkDevice -- ^ device
                        -> VkBuffer -- ^ buffer
                                    -> Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                                -> IO ()

-- | > () vkGetBufferMemoryRequirements
--   >     ( VkDevice device
--   >     , VkBuffer buffer
--   >     , VkMemoryRequirements* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetBufferMemoryRequirements.html vkGetBufferMemoryRequirements registry at www.khronos.org>
type HS_vkGetBufferMemoryRequirements =
     VkDevice -- ^ device
              -> VkBuffer -- ^ buffer
                          -> Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                      -> IO ()

type PFN_vkGetBufferMemoryRequirements =
     FunPtr HS_vkGetBufferMemoryRequirements

foreign import ccall "dynamic" unwrapVkGetBufferMemoryRequirements
               ::
               PFN_vkGetBufferMemoryRequirements ->
                 HS_vkGetBufferMemoryRequirements

instance VulkanProc "vkGetBufferMemoryRequirements" where
        type VkProcType "vkGetBufferMemoryRequirements" =
             HS_vkGetBufferMemoryRequirements
        vkProcSymbol = _VkGetBufferMemoryRequirements

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetBufferMemoryRequirements

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetImageMemoryRequirements :: CString

pattern VkGetImageMemoryRequirements <-
        (is_VkGetImageMemoryRequirements -> True)
  where VkGetImageMemoryRequirements = _VkGetImageMemoryRequirements

{-# INLINE _VkGetImageMemoryRequirements #-}

_VkGetImageMemoryRequirements :: CString
_VkGetImageMemoryRequirements
  = Ptr "vkGetImageMemoryRequirements\NUL"##

{-# INLINE is_VkGetImageMemoryRequirements #-}

is_VkGetImageMemoryRequirements :: CString -> Bool
is_VkGetImageMemoryRequirements
  = (EQ ==) . cmpCStrings _VkGetImageMemoryRequirements

type VkGetImageMemoryRequirements = "vkGetImageMemoryRequirements"

-- | > () vkGetImageMemoryRequirements
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , VkMemoryRequirements* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageMemoryRequirements.html vkGetImageMemoryRequirements registry at www.khronos.org>
foreign import ccall unsafe "vkGetImageMemoryRequirements"
               vkGetImageMemoryRequirements ::
               VkDevice -- ^ device
                        -> VkImage -- ^ image
                                   -> Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                               -> IO ()

-- | > () vkGetImageMemoryRequirements
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , VkMemoryRequirements* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageMemoryRequirements.html vkGetImageMemoryRequirements registry at www.khronos.org>
foreign import ccall safe "vkGetImageMemoryRequirements"
               vkGetImageMemoryRequirementsSafe ::
               VkDevice -- ^ device
                        -> VkImage -- ^ image
                                   -> Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                               -> IO ()

-- | > () vkGetImageMemoryRequirements
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , VkMemoryRequirements* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageMemoryRequirements.html vkGetImageMemoryRequirements registry at www.khronos.org>
type HS_vkGetImageMemoryRequirements =
     VkDevice -- ^ device
              -> VkImage -- ^ image
                         -> Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                     -> IO ()

type PFN_vkGetImageMemoryRequirements =
     FunPtr HS_vkGetImageMemoryRequirements

foreign import ccall "dynamic" unwrapVkGetImageMemoryRequirements
               ::
               PFN_vkGetImageMemoryRequirements -> HS_vkGetImageMemoryRequirements

instance VulkanProc "vkGetImageMemoryRequirements" where
        type VkProcType "vkGetImageMemoryRequirements" =
             HS_vkGetImageMemoryRequirements
        vkProcSymbol = _VkGetImageMemoryRequirements

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetImageMemoryRequirements

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetImageSparseMemoryRequirements :: CString

pattern VkGetImageSparseMemoryRequirements <-
        (is_VkGetImageSparseMemoryRequirements -> True)
  where VkGetImageSparseMemoryRequirements
          = _VkGetImageSparseMemoryRequirements

{-# INLINE _VkGetImageSparseMemoryRequirements #-}

_VkGetImageSparseMemoryRequirements :: CString
_VkGetImageSparseMemoryRequirements
  = Ptr "vkGetImageSparseMemoryRequirements\NUL"##

{-# INLINE is_VkGetImageSparseMemoryRequirements #-}

is_VkGetImageSparseMemoryRequirements :: CString -> Bool
is_VkGetImageSparseMemoryRequirements
  = (EQ ==) . cmpCStrings _VkGetImageSparseMemoryRequirements

type VkGetImageSparseMemoryRequirements =
     "vkGetImageSparseMemoryRequirements"

-- | > () vkGetImageSparseMemoryRequirements
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , uint32_t* pSparseMemoryRequirementCount
--   >     , VkSparseImageMemoryRequirements* pSparseMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageSparseMemoryRequirements.html vkGetImageSparseMemoryRequirements registry at www.khronos.org>
foreign import ccall unsafe "vkGetImageSparseMemoryRequirements"
               vkGetImageSparseMemoryRequirements ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         ->
                   Ptr Word32 -- ^ pSparseMemoryRequirementCount
                              -> Ptr VkSparseImageMemoryRequirements -- ^ pSparseMemoryRequirements
                                                                     -> IO ()

-- | > () vkGetImageSparseMemoryRequirements
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , uint32_t* pSparseMemoryRequirementCount
--   >     , VkSparseImageMemoryRequirements* pSparseMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageSparseMemoryRequirements.html vkGetImageSparseMemoryRequirements registry at www.khronos.org>
foreign import ccall safe "vkGetImageSparseMemoryRequirements"
               vkGetImageSparseMemoryRequirementsSafe ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         ->
                   Ptr Word32 -- ^ pSparseMemoryRequirementCount
                              -> Ptr VkSparseImageMemoryRequirements -- ^ pSparseMemoryRequirements
                                                                     -> IO ()

-- | > () vkGetImageSparseMemoryRequirements
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , uint32_t* pSparseMemoryRequirementCount
--   >     , VkSparseImageMemoryRequirements* pSparseMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageSparseMemoryRequirements.html vkGetImageSparseMemoryRequirements registry at www.khronos.org>
type HS_vkGetImageSparseMemoryRequirements =
     VkDevice -- ^ device
              ->
       VkImage -- ^ image
               ->
         Ptr Word32 -- ^ pSparseMemoryRequirementCount
                    -> Ptr VkSparseImageMemoryRequirements -- ^ pSparseMemoryRequirements
                                                           -> IO ()

type PFN_vkGetImageSparseMemoryRequirements =
     FunPtr HS_vkGetImageSparseMemoryRequirements

foreign import ccall "dynamic"
               unwrapVkGetImageSparseMemoryRequirements ::
               PFN_vkGetImageSparseMemoryRequirements ->
                 HS_vkGetImageSparseMemoryRequirements

instance VulkanProc "vkGetImageSparseMemoryRequirements" where
        type VkProcType "vkGetImageSparseMemoryRequirements" =
             HS_vkGetImageSparseMemoryRequirements
        vkProcSymbol = _VkGetImageSparseMemoryRequirements

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetImageSparseMemoryRequirements

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceSparseImageFormatProperties :: CString

pattern VkGetPhysicalDeviceSparseImageFormatProperties <-
        (is_VkGetPhysicalDeviceSparseImageFormatProperties -> True)
  where VkGetPhysicalDeviceSparseImageFormatProperties
          = _VkGetPhysicalDeviceSparseImageFormatProperties

{-# INLINE _VkGetPhysicalDeviceSparseImageFormatProperties #-}

_VkGetPhysicalDeviceSparseImageFormatProperties :: CString
_VkGetPhysicalDeviceSparseImageFormatProperties
  = Ptr "vkGetPhysicalDeviceSparseImageFormatProperties\NUL"##

{-# INLINE is_VkGetPhysicalDeviceSparseImageFormatProperties #-}

is_VkGetPhysicalDeviceSparseImageFormatProperties ::
                                                  CString -> Bool
is_VkGetPhysicalDeviceSparseImageFormatProperties
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceSparseImageFormatProperties

type VkGetPhysicalDeviceSparseImageFormatProperties =
     "vkGetPhysicalDeviceSparseImageFormatProperties"

-- | > () vkGetPhysicalDeviceSparseImageFormatProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkImageType type
--   >     , VkSampleCountFlagBits samples
--   >     , VkImageUsageFlags usage
--   >     , VkImageTiling tiling
--   >     , uint32_t* pPropertyCount
--   >     , VkSparseImageFormatProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceSparseImageFormatProperties.html vkGetPhysicalDeviceSparseImageFormatProperties registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceSparseImageFormatProperties"
               vkGetPhysicalDeviceSparseImageFormatProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkFormat -- ^ format
                          ->
                   VkImageType -- ^ type
                               ->
                     VkSampleCountFlagBits -- ^ samples
                                           ->
                       VkImageUsageFlags -- ^ usage
                                         ->
                         VkImageTiling -- ^ tiling
                                       ->
                           Ptr Word32 -- ^ pPropertyCount
                                      -> Ptr VkSparseImageFormatProperties -- ^ pProperties
                                                                           -> IO ()

-- | > () vkGetPhysicalDeviceSparseImageFormatProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkImageType type
--   >     , VkSampleCountFlagBits samples
--   >     , VkImageUsageFlags usage
--   >     , VkImageTiling tiling
--   >     , uint32_t* pPropertyCount
--   >     , VkSparseImageFormatProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceSparseImageFormatProperties.html vkGetPhysicalDeviceSparseImageFormatProperties registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceSparseImageFormatProperties"
               vkGetPhysicalDeviceSparseImageFormatPropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkFormat -- ^ format
                          ->
                   VkImageType -- ^ type
                               ->
                     VkSampleCountFlagBits -- ^ samples
                                           ->
                       VkImageUsageFlags -- ^ usage
                                         ->
                         VkImageTiling -- ^ tiling
                                       ->
                           Ptr Word32 -- ^ pPropertyCount
                                      -> Ptr VkSparseImageFormatProperties -- ^ pProperties
                                                                           -> IO ()

-- | > () vkGetPhysicalDeviceSparseImageFormatProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkImageType type
--   >     , VkSampleCountFlagBits samples
--   >     , VkImageUsageFlags usage
--   >     , VkImageTiling tiling
--   >     , uint32_t* pPropertyCount
--   >     , VkSparseImageFormatProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceSparseImageFormatProperties.html vkGetPhysicalDeviceSparseImageFormatProperties registry at www.khronos.org>
type HS_vkGetPhysicalDeviceSparseImageFormatProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       VkFormat -- ^ format
                ->
         VkImageType -- ^ type
                     ->
           VkSampleCountFlagBits -- ^ samples
                                 ->
             VkImageUsageFlags -- ^ usage
                               ->
               VkImageTiling -- ^ tiling
                             ->
                 Ptr Word32 -- ^ pPropertyCount
                            -> Ptr VkSparseImageFormatProperties -- ^ pProperties
                                                                 -> IO ()

type PFN_vkGetPhysicalDeviceSparseImageFormatProperties =
     FunPtr HS_vkGetPhysicalDeviceSparseImageFormatProperties

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceSparseImageFormatProperties ::
               PFN_vkGetPhysicalDeviceSparseImageFormatProperties ->
                 HS_vkGetPhysicalDeviceSparseImageFormatProperties

instance VulkanProc
           "vkGetPhysicalDeviceSparseImageFormatProperties"
         where
        type VkProcType "vkGetPhysicalDeviceSparseImageFormatProperties" =
             HS_vkGetPhysicalDeviceSparseImageFormatProperties
        vkProcSymbol = _VkGetPhysicalDeviceSparseImageFormatProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr
          = unwrapVkGetPhysicalDeviceSparseImageFormatProperties

        {-# INLINE unwrapVkProcPtr #-}

pattern VkQueueBindSparse :: CString

pattern VkQueueBindSparse <- (is_VkQueueBindSparse -> True)
  where VkQueueBindSparse = _VkQueueBindSparse

{-# INLINE _VkQueueBindSparse #-}

_VkQueueBindSparse :: CString
_VkQueueBindSparse = Ptr "vkQueueBindSparse\NUL"##

{-# INLINE is_VkQueueBindSparse #-}

is_VkQueueBindSparse :: CString -> Bool
is_VkQueueBindSparse = (EQ ==) . cmpCStrings _VkQueueBindSparse

type VkQueueBindSparse = "vkQueueBindSparse"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   queues: 'sparse_binding'.
--
--   > VkResult vkQueueBindSparse
--   >     ( VkQueue queue
--   >     , uint32_t bindInfoCount
--   >     , const VkBindSparseInfo* pBindInfo
--   >     , VkFence fence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueueBindSparse.html vkQueueBindSparse registry at www.khronos.org>
foreign import ccall unsafe "vkQueueBindSparse" vkQueueBindSparse
               ::
               VkQueue -- ^ queue
                       -> Word32 -- ^ bindInfoCount
                                 -> Ptr VkBindSparseInfo -- ^ pBindInfo
                                                         -> VkFence -- ^ fence
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   queues: 'sparse_binding'.
--
--   > VkResult vkQueueBindSparse
--   >     ( VkQueue queue
--   >     , uint32_t bindInfoCount
--   >     , const VkBindSparseInfo* pBindInfo
--   >     , VkFence fence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueueBindSparse.html vkQueueBindSparse registry at www.khronos.org>
foreign import ccall safe "vkQueueBindSparse" vkQueueBindSparseSafe
               ::
               VkQueue -- ^ queue
                       -> Word32 -- ^ bindInfoCount
                                 -> Ptr VkBindSparseInfo -- ^ pBindInfo
                                                         -> VkFence -- ^ fence
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   queues: 'sparse_binding'.
--
--   > VkResult vkQueueBindSparse
--   >     ( VkQueue queue
--   >     , uint32_t bindInfoCount
--   >     , const VkBindSparseInfo* pBindInfo
--   >     , VkFence fence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueueBindSparse.html vkQueueBindSparse registry at www.khronos.org>
type HS_vkQueueBindSparse =
     VkQueue -- ^ queue
             -> Word32 -- ^ bindInfoCount
                       -> Ptr VkBindSparseInfo -- ^ pBindInfo
                                               -> VkFence -- ^ fence
                                                          -> IO VkResult

type PFN_vkQueueBindSparse = FunPtr HS_vkQueueBindSparse

foreign import ccall "dynamic" unwrapVkQueueBindSparse ::
               PFN_vkQueueBindSparse -> HS_vkQueueBindSparse

instance VulkanProc "vkQueueBindSparse" where
        type VkProcType "vkQueueBindSparse" = HS_vkQueueBindSparse
        vkProcSymbol = _VkQueueBindSparse

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkQueueBindSparse

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateFence :: CString

pattern VkCreateFence <- (is_VkCreateFence -> True)
  where VkCreateFence = _VkCreateFence

{-# INLINE _VkCreateFence #-}

_VkCreateFence :: CString
_VkCreateFence = Ptr "vkCreateFence\NUL"##

{-# INLINE is_VkCreateFence #-}

is_VkCreateFence :: CString -> Bool
is_VkCreateFence = (EQ ==) . cmpCStrings _VkCreateFence

type VkCreateFence = "vkCreateFence"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateFence
--   >     ( VkDevice device
--   >     , const VkFenceCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFence* pFence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateFence.html vkCreateFence registry at www.khronos.org>
foreign import ccall unsafe "vkCreateFence" vkCreateFence ::
               VkDevice -- ^ device
                        ->
                 Ptr VkFenceCreateInfo -- ^ pCreateInfo
                                       ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkFence -- ^ pFence
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateFence
--   >     ( VkDevice device
--   >     , const VkFenceCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFence* pFence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateFence.html vkCreateFence registry at www.khronos.org>
foreign import ccall safe "vkCreateFence" vkCreateFenceSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkFenceCreateInfo -- ^ pCreateInfo
                                       ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkFence -- ^ pFence
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateFence
--   >     ( VkDevice device
--   >     , const VkFenceCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFence* pFence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateFence.html vkCreateFence registry at www.khronos.org>
type HS_vkCreateFence =
     VkDevice -- ^ device
              ->
       Ptr VkFenceCreateInfo -- ^ pCreateInfo
                             ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkFence -- ^ pFence
                                                  -> IO VkResult

type PFN_vkCreateFence = FunPtr HS_vkCreateFence

foreign import ccall "dynamic" unwrapVkCreateFence ::
               PFN_vkCreateFence -> HS_vkCreateFence

instance VulkanProc "vkCreateFence" where
        type VkProcType "vkCreateFence" = HS_vkCreateFence
        vkProcSymbol = _VkCreateFence

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateFence

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyFence :: CString

pattern VkDestroyFence <- (is_VkDestroyFence -> True)
  where VkDestroyFence = _VkDestroyFence

{-# INLINE _VkDestroyFence #-}

_VkDestroyFence :: CString
_VkDestroyFence = Ptr "vkDestroyFence\NUL"##

{-# INLINE is_VkDestroyFence #-}

is_VkDestroyFence :: CString -> Bool
is_VkDestroyFence = (EQ ==) . cmpCStrings _VkDestroyFence

type VkDestroyFence = "vkDestroyFence"

-- | > () vkDestroyFence
--   >     ( VkDevice device
--   >     , VkFence fence
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyFence.html vkDestroyFence registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyFence" vkDestroyFence ::
               VkDevice -- ^ device
                        -> VkFence -- ^ fence
                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> IO ()

-- | > () vkDestroyFence
--   >     ( VkDevice device
--   >     , VkFence fence
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyFence.html vkDestroyFence registry at www.khronos.org>
foreign import ccall safe "vkDestroyFence" vkDestroyFenceSafe ::
               VkDevice -- ^ device
                        -> VkFence -- ^ fence
                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> IO ()

-- | > () vkDestroyFence
--   >     ( VkDevice device
--   >     , VkFence fence
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyFence.html vkDestroyFence registry at www.khronos.org>
type HS_vkDestroyFence =
     VkDevice -- ^ device
              -> VkFence -- ^ fence
                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                      -> IO ()

type PFN_vkDestroyFence = FunPtr HS_vkDestroyFence

foreign import ccall "dynamic" unwrapVkDestroyFence ::
               PFN_vkDestroyFence -> HS_vkDestroyFence

instance VulkanProc "vkDestroyFence" where
        type VkProcType "vkDestroyFence" = HS_vkDestroyFence
        vkProcSymbol = _VkDestroyFence

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyFence

        {-# INLINE unwrapVkProcPtr #-}

pattern VkResetFences :: CString

pattern VkResetFences <- (is_VkResetFences -> True)
  where VkResetFences = _VkResetFences

{-# INLINE _VkResetFences #-}

_VkResetFences :: CString
_VkResetFences = Ptr "vkResetFences\NUL"##

{-# INLINE is_VkResetFences #-}

is_VkResetFences :: CString -> Bool
is_VkResetFences = (EQ ==) . cmpCStrings _VkResetFences

type VkResetFences = "vkResetFences"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetFences
--   >     ( VkDevice device
--   >     , uint32_t fenceCount
--   >     , const VkFence* pFences
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkResetFences.html vkResetFences registry at www.khronos.org>
foreign import ccall unsafe "vkResetFences" vkResetFences ::
               VkDevice -- ^ device
                        -> Word32 -- ^ fenceCount
                                  -> Ptr VkFence -- ^ pFences
                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetFences
--   >     ( VkDevice device
--   >     , uint32_t fenceCount
--   >     , const VkFence* pFences
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkResetFences.html vkResetFences registry at www.khronos.org>
foreign import ccall safe "vkResetFences" vkResetFencesSafe ::
               VkDevice -- ^ device
                        -> Word32 -- ^ fenceCount
                                  -> Ptr VkFence -- ^ pFences
                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetFences
--   >     ( VkDevice device
--   >     , uint32_t fenceCount
--   >     , const VkFence* pFences
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkResetFences.html vkResetFences registry at www.khronos.org>
type HS_vkResetFences =
     VkDevice -- ^ device
              -> Word32 -- ^ fenceCount
                        -> Ptr VkFence -- ^ pFences
                                       -> IO VkResult

type PFN_vkResetFences = FunPtr HS_vkResetFences

foreign import ccall "dynamic" unwrapVkResetFences ::
               PFN_vkResetFences -> HS_vkResetFences

instance VulkanProc "vkResetFences" where
        type VkProcType "vkResetFences" = HS_vkResetFences
        vkProcSymbol = _VkResetFences

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkResetFences

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetFenceStatus :: CString

pattern VkGetFenceStatus <- (is_VkGetFenceStatus -> True)
  where VkGetFenceStatus = _VkGetFenceStatus

{-# INLINE _VkGetFenceStatus #-}

_VkGetFenceStatus :: CString
_VkGetFenceStatus = Ptr "vkGetFenceStatus\NUL"##

{-# INLINE is_VkGetFenceStatus #-}

is_VkGetFenceStatus :: CString -> Bool
is_VkGetFenceStatus = (EQ ==) . cmpCStrings _VkGetFenceStatus

type VkGetFenceStatus = "vkGetFenceStatus"

-- | Success codes: 'VK_SUCCESS', 'VK_NOT_READY'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkGetFenceStatus
--   >     ( VkDevice device
--   >     , VkFence fence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetFenceStatus.html vkGetFenceStatus registry at www.khronos.org>
foreign import ccall unsafe "vkGetFenceStatus" vkGetFenceStatus ::
               VkDevice -- ^ device
                        -> VkFence -- ^ fence
                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_NOT_READY'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkGetFenceStatus
--   >     ( VkDevice device
--   >     , VkFence fence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetFenceStatus.html vkGetFenceStatus registry at www.khronos.org>
foreign import ccall safe "vkGetFenceStatus" vkGetFenceStatusSafe
               :: VkDevice -- ^ device
                           -> VkFence -- ^ fence
                                      -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_NOT_READY'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkGetFenceStatus
--   >     ( VkDevice device
--   >     , VkFence fence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetFenceStatus.html vkGetFenceStatus registry at www.khronos.org>
type HS_vkGetFenceStatus = VkDevice -- ^ device
                                    -> VkFence -- ^ fence
                                               -> IO VkResult

type PFN_vkGetFenceStatus = FunPtr HS_vkGetFenceStatus

foreign import ccall "dynamic" unwrapVkGetFenceStatus ::
               PFN_vkGetFenceStatus -> HS_vkGetFenceStatus

instance VulkanProc "vkGetFenceStatus" where
        type VkProcType "vkGetFenceStatus" = HS_vkGetFenceStatus
        vkProcSymbol = _VkGetFenceStatus

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetFenceStatus

        {-# INLINE unwrapVkProcPtr #-}

pattern VkWaitForFences :: CString

pattern VkWaitForFences <- (is_VkWaitForFences -> True)
  where VkWaitForFences = _VkWaitForFences

{-# INLINE _VkWaitForFences #-}

_VkWaitForFences :: CString
_VkWaitForFences = Ptr "vkWaitForFences\NUL"##

{-# INLINE is_VkWaitForFences #-}

is_VkWaitForFences :: CString -> Bool
is_VkWaitForFences = (EQ ==) . cmpCStrings _VkWaitForFences

type VkWaitForFences = "vkWaitForFences"

-- | Success codes: 'VK_SUCCESS', 'VK_TIMEOUT'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkWaitForFences
--   >     ( VkDevice device
--   >     , uint32_t fenceCount
--   >     , const VkFence* pFences
--   >     , VkBool32 waitAll
--   >     , uint64_t timeout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkWaitForFences.html vkWaitForFences registry at www.khronos.org>
foreign import ccall unsafe "vkWaitForFences" vkWaitForFences ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ fenceCount
                        -> Ptr VkFence -- ^ pFences
                                       -> VkBool32 -- ^ waitAll
                                                   -> Word64 -- ^ timeout
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_TIMEOUT'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkWaitForFences
--   >     ( VkDevice device
--   >     , uint32_t fenceCount
--   >     , const VkFence* pFences
--   >     , VkBool32 waitAll
--   >     , uint64_t timeout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkWaitForFences.html vkWaitForFences registry at www.khronos.org>
foreign import ccall safe "vkWaitForFences" vkWaitForFencesSafe ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ fenceCount
                        -> Ptr VkFence -- ^ pFences
                                       -> VkBool32 -- ^ waitAll
                                                   -> Word64 -- ^ timeout
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_TIMEOUT'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkWaitForFences
--   >     ( VkDevice device
--   >     , uint32_t fenceCount
--   >     , const VkFence* pFences
--   >     , VkBool32 waitAll
--   >     , uint64_t timeout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkWaitForFences.html vkWaitForFences registry at www.khronos.org>
type HS_vkWaitForFences =
     VkDevice -- ^ device
              ->
       Word32 -- ^ fenceCount
              -> Ptr VkFence -- ^ pFences
                             -> VkBool32 -- ^ waitAll
                                         -> Word64 -- ^ timeout
                                                   -> IO VkResult

type PFN_vkWaitForFences = FunPtr HS_vkWaitForFences

foreign import ccall "dynamic" unwrapVkWaitForFences ::
               PFN_vkWaitForFences -> HS_vkWaitForFences

instance VulkanProc "vkWaitForFences" where
        type VkProcType "vkWaitForFences" = HS_vkWaitForFences
        vkProcSymbol = _VkWaitForFences

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkWaitForFences

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateSemaphore :: CString

pattern VkCreateSemaphore <- (is_VkCreateSemaphore -> True)
  where VkCreateSemaphore = _VkCreateSemaphore

{-# INLINE _VkCreateSemaphore #-}

_VkCreateSemaphore :: CString
_VkCreateSemaphore = Ptr "vkCreateSemaphore\NUL"##

{-# INLINE is_VkCreateSemaphore #-}

is_VkCreateSemaphore :: CString -> Bool
is_VkCreateSemaphore = (EQ ==) . cmpCStrings _VkCreateSemaphore

type VkCreateSemaphore = "vkCreateSemaphore"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateSemaphore
--   >     ( VkDevice device
--   >     , const VkSemaphoreCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSemaphore* pSemaphore
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateSemaphore.html vkCreateSemaphore registry at www.khronos.org>
foreign import ccall unsafe "vkCreateSemaphore" vkCreateSemaphore
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSemaphoreCreateInfo -- ^ pCreateInfo
                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSemaphore -- ^ pSemaphore
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateSemaphore
--   >     ( VkDevice device
--   >     , const VkSemaphoreCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSemaphore* pSemaphore
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateSemaphore.html vkCreateSemaphore registry at www.khronos.org>
foreign import ccall safe "vkCreateSemaphore" vkCreateSemaphoreSafe
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSemaphoreCreateInfo -- ^ pCreateInfo
                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSemaphore -- ^ pSemaphore
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateSemaphore
--   >     ( VkDevice device
--   >     , const VkSemaphoreCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSemaphore* pSemaphore
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateSemaphore.html vkCreateSemaphore registry at www.khronos.org>
type HS_vkCreateSemaphore =
     VkDevice -- ^ device
              ->
       Ptr VkSemaphoreCreateInfo -- ^ pCreateInfo
                                 ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSemaphore -- ^ pSemaphore
                                                      -> IO VkResult

type PFN_vkCreateSemaphore = FunPtr HS_vkCreateSemaphore

foreign import ccall "dynamic" unwrapVkCreateSemaphore ::
               PFN_vkCreateSemaphore -> HS_vkCreateSemaphore

instance VulkanProc "vkCreateSemaphore" where
        type VkProcType "vkCreateSemaphore" = HS_vkCreateSemaphore
        vkProcSymbol = _VkCreateSemaphore

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateSemaphore

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroySemaphore :: CString

pattern VkDestroySemaphore <- (is_VkDestroySemaphore -> True)
  where VkDestroySemaphore = _VkDestroySemaphore

{-# INLINE _VkDestroySemaphore #-}

_VkDestroySemaphore :: CString
_VkDestroySemaphore = Ptr "vkDestroySemaphore\NUL"##

{-# INLINE is_VkDestroySemaphore #-}

is_VkDestroySemaphore :: CString -> Bool
is_VkDestroySemaphore = (EQ ==) . cmpCStrings _VkDestroySemaphore

type VkDestroySemaphore = "vkDestroySemaphore"

-- | > () vkDestroySemaphore
--   >     ( VkDevice device
--   >     , VkSemaphore semaphore
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroySemaphore.html vkDestroySemaphore registry at www.khronos.org>
foreign import ccall unsafe "vkDestroySemaphore" vkDestroySemaphore
               :: VkDevice -- ^ device
                           -> VkSemaphore -- ^ semaphore
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

-- | > () vkDestroySemaphore
--   >     ( VkDevice device
--   >     , VkSemaphore semaphore
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroySemaphore.html vkDestroySemaphore registry at www.khronos.org>
foreign import ccall safe "vkDestroySemaphore"
               vkDestroySemaphoreSafe ::
               VkDevice -- ^ device
                        -> VkSemaphore -- ^ semaphore
                                       -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()

-- | > () vkDestroySemaphore
--   >     ( VkDevice device
--   >     , VkSemaphore semaphore
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroySemaphore.html vkDestroySemaphore registry at www.khronos.org>
type HS_vkDestroySemaphore =
     VkDevice -- ^ device
              -> VkSemaphore -- ^ semaphore
                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          -> IO ()

type PFN_vkDestroySemaphore = FunPtr HS_vkDestroySemaphore

foreign import ccall "dynamic" unwrapVkDestroySemaphore ::
               PFN_vkDestroySemaphore -> HS_vkDestroySemaphore

instance VulkanProc "vkDestroySemaphore" where
        type VkProcType "vkDestroySemaphore" = HS_vkDestroySemaphore
        vkProcSymbol = _VkDestroySemaphore

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroySemaphore

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateEvent :: CString

pattern VkCreateEvent <- (is_VkCreateEvent -> True)
  where VkCreateEvent = _VkCreateEvent

{-# INLINE _VkCreateEvent #-}

_VkCreateEvent :: CString
_VkCreateEvent = Ptr "vkCreateEvent\NUL"##

{-# INLINE is_VkCreateEvent #-}

is_VkCreateEvent :: CString -> Bool
is_VkCreateEvent = (EQ ==) . cmpCStrings _VkCreateEvent

type VkCreateEvent = "vkCreateEvent"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateEvent
--   >     ( VkDevice device
--   >     , const VkEventCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkEvent* pEvent
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateEvent.html vkCreateEvent registry at www.khronos.org>
foreign import ccall unsafe "vkCreateEvent" vkCreateEvent ::
               VkDevice -- ^ device
                        ->
                 Ptr VkEventCreateInfo -- ^ pCreateInfo
                                       ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkEvent -- ^ pEvent
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateEvent
--   >     ( VkDevice device
--   >     , const VkEventCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkEvent* pEvent
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateEvent.html vkCreateEvent registry at www.khronos.org>
foreign import ccall safe "vkCreateEvent" vkCreateEventSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkEventCreateInfo -- ^ pCreateInfo
                                       ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkEvent -- ^ pEvent
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateEvent
--   >     ( VkDevice device
--   >     , const VkEventCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkEvent* pEvent
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateEvent.html vkCreateEvent registry at www.khronos.org>
type HS_vkCreateEvent =
     VkDevice -- ^ device
              ->
       Ptr VkEventCreateInfo -- ^ pCreateInfo
                             ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkEvent -- ^ pEvent
                                                  -> IO VkResult

type PFN_vkCreateEvent = FunPtr HS_vkCreateEvent

foreign import ccall "dynamic" unwrapVkCreateEvent ::
               PFN_vkCreateEvent -> HS_vkCreateEvent

instance VulkanProc "vkCreateEvent" where
        type VkProcType "vkCreateEvent" = HS_vkCreateEvent
        vkProcSymbol = _VkCreateEvent

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateEvent

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyEvent :: CString

pattern VkDestroyEvent <- (is_VkDestroyEvent -> True)
  where VkDestroyEvent = _VkDestroyEvent

{-# INLINE _VkDestroyEvent #-}

_VkDestroyEvent :: CString
_VkDestroyEvent = Ptr "vkDestroyEvent\NUL"##

{-# INLINE is_VkDestroyEvent #-}

is_VkDestroyEvent :: CString -> Bool
is_VkDestroyEvent = (EQ ==) . cmpCStrings _VkDestroyEvent

type VkDestroyEvent = "vkDestroyEvent"

-- | > () vkDestroyEvent
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyEvent.html vkDestroyEvent registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyEvent" vkDestroyEvent ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> IO ()

-- | > () vkDestroyEvent
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyEvent.html vkDestroyEvent registry at www.khronos.org>
foreign import ccall safe "vkDestroyEvent" vkDestroyEventSafe ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> IO ()

-- | > () vkDestroyEvent
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyEvent.html vkDestroyEvent registry at www.khronos.org>
type HS_vkDestroyEvent =
     VkDevice -- ^ device
              -> VkEvent -- ^ event
                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                      -> IO ()

type PFN_vkDestroyEvent = FunPtr HS_vkDestroyEvent

foreign import ccall "dynamic" unwrapVkDestroyEvent ::
               PFN_vkDestroyEvent -> HS_vkDestroyEvent

instance VulkanProc "vkDestroyEvent" where
        type VkProcType "vkDestroyEvent" = HS_vkDestroyEvent
        vkProcSymbol = _VkDestroyEvent

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyEvent

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetEventStatus :: CString

pattern VkGetEventStatus <- (is_VkGetEventStatus -> True)
  where VkGetEventStatus = _VkGetEventStatus

{-# INLINE _VkGetEventStatus #-}

_VkGetEventStatus :: CString
_VkGetEventStatus = Ptr "vkGetEventStatus\NUL"##

{-# INLINE is_VkGetEventStatus #-}

is_VkGetEventStatus :: CString -> Bool
is_VkGetEventStatus = (EQ ==) . cmpCStrings _VkGetEventStatus

type VkGetEventStatus = "vkGetEventStatus"

-- | Success codes: 'VK_EVENT_SET', 'VK_EVENT_RESET'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkGetEventStatus
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetEventStatus.html vkGetEventStatus registry at www.khronos.org>
foreign import ccall unsafe "vkGetEventStatus" vkGetEventStatus ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> IO VkResult

-- | Success codes: 'VK_EVENT_SET', 'VK_EVENT_RESET'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkGetEventStatus
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetEventStatus.html vkGetEventStatus registry at www.khronos.org>
foreign import ccall safe "vkGetEventStatus" vkGetEventStatusSafe
               :: VkDevice -- ^ device
                           -> VkEvent -- ^ event
                                      -> IO VkResult

-- | Success codes: 'VK_EVENT_SET', 'VK_EVENT_RESET'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkGetEventStatus
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetEventStatus.html vkGetEventStatus registry at www.khronos.org>
type HS_vkGetEventStatus = VkDevice -- ^ device
                                    -> VkEvent -- ^ event
                                               -> IO VkResult

type PFN_vkGetEventStatus = FunPtr HS_vkGetEventStatus

foreign import ccall "dynamic" unwrapVkGetEventStatus ::
               PFN_vkGetEventStatus -> HS_vkGetEventStatus

instance VulkanProc "vkGetEventStatus" where
        type VkProcType "vkGetEventStatus" = HS_vkGetEventStatus
        vkProcSymbol = _VkGetEventStatus

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetEventStatus

        {-# INLINE unwrapVkProcPtr #-}

pattern VkSetEvent :: CString

pattern VkSetEvent <- (is_VkSetEvent -> True)
  where VkSetEvent = _VkSetEvent

{-# INLINE _VkSetEvent #-}

_VkSetEvent :: CString
_VkSetEvent = Ptr "vkSetEvent\NUL"##

{-# INLINE is_VkSetEvent #-}

is_VkSetEvent :: CString -> Bool
is_VkSetEvent = (EQ ==) . cmpCStrings _VkSetEvent

type VkSetEvent = "vkSetEvent"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkSetEvent
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkSetEvent.html vkSetEvent registry at www.khronos.org>
foreign import ccall unsafe "vkSetEvent" vkSetEvent ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkSetEvent
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkSetEvent.html vkSetEvent registry at www.khronos.org>
foreign import ccall safe "vkSetEvent" vkSetEventSafe ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkSetEvent
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkSetEvent.html vkSetEvent registry at www.khronos.org>
type HS_vkSetEvent = VkDevice -- ^ device
                              -> VkEvent -- ^ event
                                         -> IO VkResult

type PFN_vkSetEvent = FunPtr HS_vkSetEvent

foreign import ccall "dynamic" unwrapVkSetEvent ::
               PFN_vkSetEvent -> HS_vkSetEvent

instance VulkanProc "vkSetEvent" where
        type VkProcType "vkSetEvent" = HS_vkSetEvent
        vkProcSymbol = _VkSetEvent

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkSetEvent

        {-# INLINE unwrapVkProcPtr #-}

pattern VkResetEvent :: CString

pattern VkResetEvent <- (is_VkResetEvent -> True)
  where VkResetEvent = _VkResetEvent

{-# INLINE _VkResetEvent #-}

_VkResetEvent :: CString
_VkResetEvent = Ptr "vkResetEvent\NUL"##

{-# INLINE is_VkResetEvent #-}

is_VkResetEvent :: CString -> Bool
is_VkResetEvent = (EQ ==) . cmpCStrings _VkResetEvent

type VkResetEvent = "vkResetEvent"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetEvent
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkResetEvent.html vkResetEvent registry at www.khronos.org>
foreign import ccall unsafe "vkResetEvent" vkResetEvent ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetEvent
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkResetEvent.html vkResetEvent registry at www.khronos.org>
foreign import ccall safe "vkResetEvent" vkResetEventSafe ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetEvent
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkResetEvent.html vkResetEvent registry at www.khronos.org>
type HS_vkResetEvent = VkDevice -- ^ device
                                -> VkEvent -- ^ event
                                           -> IO VkResult

type PFN_vkResetEvent = FunPtr HS_vkResetEvent

foreign import ccall "dynamic" unwrapVkResetEvent ::
               PFN_vkResetEvent -> HS_vkResetEvent

instance VulkanProc "vkResetEvent" where
        type VkProcType "vkResetEvent" = HS_vkResetEvent
        vkProcSymbol = _VkResetEvent

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkResetEvent

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateQueryPool :: CString

pattern VkCreateQueryPool <- (is_VkCreateQueryPool -> True)
  where VkCreateQueryPool = _VkCreateQueryPool

{-# INLINE _VkCreateQueryPool #-}

_VkCreateQueryPool :: CString
_VkCreateQueryPool = Ptr "vkCreateQueryPool\NUL"##

{-# INLINE is_VkCreateQueryPool #-}

is_VkCreateQueryPool :: CString -> Bool
is_VkCreateQueryPool = (EQ ==) . cmpCStrings _VkCreateQueryPool

type VkCreateQueryPool = "vkCreateQueryPool"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateQueryPool
--   >     ( VkDevice device
--   >     , const VkQueryPoolCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkQueryPool* pQueryPool
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateQueryPool.html vkCreateQueryPool registry at www.khronos.org>
foreign import ccall unsafe "vkCreateQueryPool" vkCreateQueryPool
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkQueryPoolCreateInfo -- ^ pCreateInfo
                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkQueryPool -- ^ pQueryPool
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateQueryPool
--   >     ( VkDevice device
--   >     , const VkQueryPoolCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkQueryPool* pQueryPool
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateQueryPool.html vkCreateQueryPool registry at www.khronos.org>
foreign import ccall safe "vkCreateQueryPool" vkCreateQueryPoolSafe
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkQueryPoolCreateInfo -- ^ pCreateInfo
                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkQueryPool -- ^ pQueryPool
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateQueryPool
--   >     ( VkDevice device
--   >     , const VkQueryPoolCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkQueryPool* pQueryPool
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateQueryPool.html vkCreateQueryPool registry at www.khronos.org>
type HS_vkCreateQueryPool =
     VkDevice -- ^ device
              ->
       Ptr VkQueryPoolCreateInfo -- ^ pCreateInfo
                                 ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkQueryPool -- ^ pQueryPool
                                                      -> IO VkResult

type PFN_vkCreateQueryPool = FunPtr HS_vkCreateQueryPool

foreign import ccall "dynamic" unwrapVkCreateQueryPool ::
               PFN_vkCreateQueryPool -> HS_vkCreateQueryPool

instance VulkanProc "vkCreateQueryPool" where
        type VkProcType "vkCreateQueryPool" = HS_vkCreateQueryPool
        vkProcSymbol = _VkCreateQueryPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateQueryPool

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyQueryPool :: CString

pattern VkDestroyQueryPool <- (is_VkDestroyQueryPool -> True)
  where VkDestroyQueryPool = _VkDestroyQueryPool

{-# INLINE _VkDestroyQueryPool #-}

_VkDestroyQueryPool :: CString
_VkDestroyQueryPool = Ptr "vkDestroyQueryPool\NUL"##

{-# INLINE is_VkDestroyQueryPool #-}

is_VkDestroyQueryPool :: CString -> Bool
is_VkDestroyQueryPool = (EQ ==) . cmpCStrings _VkDestroyQueryPool

type VkDestroyQueryPool = "vkDestroyQueryPool"

-- | > () vkDestroyQueryPool
--   >     ( VkDevice device
--   >     , VkQueryPool queryPool
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyQueryPool.html vkDestroyQueryPool registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyQueryPool" vkDestroyQueryPool
               :: VkDevice -- ^ device
                           -> VkQueryPool -- ^ queryPool
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

-- | > () vkDestroyQueryPool
--   >     ( VkDevice device
--   >     , VkQueryPool queryPool
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyQueryPool.html vkDestroyQueryPool registry at www.khronos.org>
foreign import ccall safe "vkDestroyQueryPool"
               vkDestroyQueryPoolSafe ::
               VkDevice -- ^ device
                        -> VkQueryPool -- ^ queryPool
                                       -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()

-- | > () vkDestroyQueryPool
--   >     ( VkDevice device
--   >     , VkQueryPool queryPool
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyQueryPool.html vkDestroyQueryPool registry at www.khronos.org>
type HS_vkDestroyQueryPool =
     VkDevice -- ^ device
              -> VkQueryPool -- ^ queryPool
                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          -> IO ()

type PFN_vkDestroyQueryPool = FunPtr HS_vkDestroyQueryPool

foreign import ccall "dynamic" unwrapVkDestroyQueryPool ::
               PFN_vkDestroyQueryPool -> HS_vkDestroyQueryPool

instance VulkanProc "vkDestroyQueryPool" where
        type VkProcType "vkDestroyQueryPool" = HS_vkDestroyQueryPool
        vkProcSymbol = _VkDestroyQueryPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyQueryPool

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetQueryPoolResults :: CString

pattern VkGetQueryPoolResults <- (is_VkGetQueryPoolResults -> True)
  where VkGetQueryPoolResults = _VkGetQueryPoolResults

{-# INLINE _VkGetQueryPoolResults #-}

_VkGetQueryPoolResults :: CString
_VkGetQueryPoolResults = Ptr "vkGetQueryPoolResults\NUL"##

{-# INLINE is_VkGetQueryPoolResults #-}

is_VkGetQueryPoolResults :: CString -> Bool
is_VkGetQueryPoolResults
  = (EQ ==) . cmpCStrings _VkGetQueryPoolResults

type VkGetQueryPoolResults = "vkGetQueryPoolResults"

-- | Success codes: 'VK_SUCCESS', 'VK_NOT_READY'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkGetQueryPoolResults
--   >     ( VkDevice device
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     , uint32_t queryCount
--   >     , size_t dataSize
--   >     , void* pData
--   >     , VkDeviceSize stride
--   >     , VkQueryResultFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetQueryPoolResults.html vkGetQueryPoolResults registry at www.khronos.org>
foreign import ccall unsafe "vkGetQueryPoolResults"
               vkGetQueryPoolResults ::
               VkDevice -- ^ device
                        ->
                 VkQueryPool -- ^ queryPool
                             ->
                   Word32 -- ^ firstQuery
                          ->
                     Word32 -- ^ queryCount
                            ->
                       CSize -- ^ dataSize
                             ->
                         Ptr Void -- ^ pData
                                  -> VkDeviceSize -- ^ stride
                                                  -> VkQueryResultFlags -- ^ flags
                                                                        -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_NOT_READY'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkGetQueryPoolResults
--   >     ( VkDevice device
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     , uint32_t queryCount
--   >     , size_t dataSize
--   >     , void* pData
--   >     , VkDeviceSize stride
--   >     , VkQueryResultFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetQueryPoolResults.html vkGetQueryPoolResults registry at www.khronos.org>
foreign import ccall safe "vkGetQueryPoolResults"
               vkGetQueryPoolResultsSafe ::
               VkDevice -- ^ device
                        ->
                 VkQueryPool -- ^ queryPool
                             ->
                   Word32 -- ^ firstQuery
                          ->
                     Word32 -- ^ queryCount
                            ->
                       CSize -- ^ dataSize
                             ->
                         Ptr Void -- ^ pData
                                  -> VkDeviceSize -- ^ stride
                                                  -> VkQueryResultFlags -- ^ flags
                                                                        -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_NOT_READY'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkGetQueryPoolResults
--   >     ( VkDevice device
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     , uint32_t queryCount
--   >     , size_t dataSize
--   >     , void* pData
--   >     , VkDeviceSize stride
--   >     , VkQueryResultFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetQueryPoolResults.html vkGetQueryPoolResults registry at www.khronos.org>
type HS_vkGetQueryPoolResults =
     VkDevice -- ^ device
              ->
       VkQueryPool -- ^ queryPool
                   ->
         Word32 -- ^ firstQuery
                ->
           Word32 -- ^ queryCount
                  ->
             CSize -- ^ dataSize
                   ->
               Ptr Void -- ^ pData
                        -> VkDeviceSize -- ^ stride
                                        -> VkQueryResultFlags -- ^ flags
                                                              -> IO VkResult

type PFN_vkGetQueryPoolResults = FunPtr HS_vkGetQueryPoolResults

foreign import ccall "dynamic" unwrapVkGetQueryPoolResults ::
               PFN_vkGetQueryPoolResults -> HS_vkGetQueryPoolResults

instance VulkanProc "vkGetQueryPoolResults" where
        type VkProcType "vkGetQueryPoolResults" = HS_vkGetQueryPoolResults
        vkProcSymbol = _VkGetQueryPoolResults

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetQueryPoolResults

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateBuffer :: CString

pattern VkCreateBuffer <- (is_VkCreateBuffer -> True)
  where VkCreateBuffer = _VkCreateBuffer

{-# INLINE _VkCreateBuffer #-}

_VkCreateBuffer :: CString
_VkCreateBuffer = Ptr "vkCreateBuffer\NUL"##

{-# INLINE is_VkCreateBuffer #-}

is_VkCreateBuffer :: CString -> Bool
is_VkCreateBuffer = (EQ ==) . cmpCStrings _VkCreateBuffer

type VkCreateBuffer = "vkCreateBuffer"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateBuffer
--   >     ( VkDevice device
--   >     , const VkBufferCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkBuffer* pBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateBuffer.html vkCreateBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkCreateBuffer" vkCreateBuffer ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferCreateInfo -- ^ pCreateInfo
                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkBuffer -- ^ pBuffer
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateBuffer
--   >     ( VkDevice device
--   >     , const VkBufferCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkBuffer* pBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateBuffer.html vkCreateBuffer registry at www.khronos.org>
foreign import ccall safe "vkCreateBuffer" vkCreateBufferSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferCreateInfo -- ^ pCreateInfo
                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkBuffer -- ^ pBuffer
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateBuffer
--   >     ( VkDevice device
--   >     , const VkBufferCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkBuffer* pBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateBuffer.html vkCreateBuffer registry at www.khronos.org>
type HS_vkCreateBuffer =
     VkDevice -- ^ device
              ->
       Ptr VkBufferCreateInfo -- ^ pCreateInfo
                              ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkBuffer -- ^ pBuffer
                                                   -> IO VkResult

type PFN_vkCreateBuffer = FunPtr HS_vkCreateBuffer

foreign import ccall "dynamic" unwrapVkCreateBuffer ::
               PFN_vkCreateBuffer -> HS_vkCreateBuffer

instance VulkanProc "vkCreateBuffer" where
        type VkProcType "vkCreateBuffer" = HS_vkCreateBuffer
        vkProcSymbol = _VkCreateBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateBuffer

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyBuffer :: CString

pattern VkDestroyBuffer <- (is_VkDestroyBuffer -> True)
  where VkDestroyBuffer = _VkDestroyBuffer

{-# INLINE _VkDestroyBuffer #-}

_VkDestroyBuffer :: CString
_VkDestroyBuffer = Ptr "vkDestroyBuffer\NUL"##

{-# INLINE is_VkDestroyBuffer #-}

is_VkDestroyBuffer :: CString -> Bool
is_VkDestroyBuffer = (EQ ==) . cmpCStrings _VkDestroyBuffer

type VkDestroyBuffer = "vkDestroyBuffer"

-- | > () vkDestroyBuffer
--   >     ( VkDevice device
--   >     , VkBuffer buffer
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyBuffer.html vkDestroyBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyBuffer" vkDestroyBuffer ::
               VkDevice -- ^ device
                        -> VkBuffer -- ^ buffer
                                    -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                 -> IO ()

-- | > () vkDestroyBuffer
--   >     ( VkDevice device
--   >     , VkBuffer buffer
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyBuffer.html vkDestroyBuffer registry at www.khronos.org>
foreign import ccall safe "vkDestroyBuffer" vkDestroyBufferSafe ::
               VkDevice -- ^ device
                        -> VkBuffer -- ^ buffer
                                    -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                 -> IO ()

-- | > () vkDestroyBuffer
--   >     ( VkDevice device
--   >     , VkBuffer buffer
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyBuffer.html vkDestroyBuffer registry at www.khronos.org>
type HS_vkDestroyBuffer =
     VkDevice -- ^ device
              -> VkBuffer -- ^ buffer
                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                       -> IO ()

type PFN_vkDestroyBuffer = FunPtr HS_vkDestroyBuffer

foreign import ccall "dynamic" unwrapVkDestroyBuffer ::
               PFN_vkDestroyBuffer -> HS_vkDestroyBuffer

instance VulkanProc "vkDestroyBuffer" where
        type VkProcType "vkDestroyBuffer" = HS_vkDestroyBuffer
        vkProcSymbol = _VkDestroyBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyBuffer

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateBufferView :: CString

pattern VkCreateBufferView <- (is_VkCreateBufferView -> True)
  where VkCreateBufferView = _VkCreateBufferView

{-# INLINE _VkCreateBufferView #-}

_VkCreateBufferView :: CString
_VkCreateBufferView = Ptr "vkCreateBufferView\NUL"##

{-# INLINE is_VkCreateBufferView #-}

is_VkCreateBufferView :: CString -> Bool
is_VkCreateBufferView = (EQ ==) . cmpCStrings _VkCreateBufferView

type VkCreateBufferView = "vkCreateBufferView"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateBufferView
--   >     ( VkDevice device
--   >     , const VkBufferViewCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkBufferView* pView
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateBufferView.html vkCreateBufferView registry at www.khronos.org>
foreign import ccall unsafe "vkCreateBufferView" vkCreateBufferView
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferViewCreateInfo -- ^ pCreateInfo
                                            ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkBufferView -- ^ pView
                                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateBufferView
--   >     ( VkDevice device
--   >     , const VkBufferViewCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkBufferView* pView
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateBufferView.html vkCreateBufferView registry at www.khronos.org>
foreign import ccall safe "vkCreateBufferView"
               vkCreateBufferViewSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferViewCreateInfo -- ^ pCreateInfo
                                            ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkBufferView -- ^ pView
                                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateBufferView
--   >     ( VkDevice device
--   >     , const VkBufferViewCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkBufferView* pView
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateBufferView.html vkCreateBufferView registry at www.khronos.org>
type HS_vkCreateBufferView =
     VkDevice -- ^ device
              ->
       Ptr VkBufferViewCreateInfo -- ^ pCreateInfo
                                  ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkBufferView -- ^ pView
                                                       -> IO VkResult

type PFN_vkCreateBufferView = FunPtr HS_vkCreateBufferView

foreign import ccall "dynamic" unwrapVkCreateBufferView ::
               PFN_vkCreateBufferView -> HS_vkCreateBufferView

instance VulkanProc "vkCreateBufferView" where
        type VkProcType "vkCreateBufferView" = HS_vkCreateBufferView
        vkProcSymbol = _VkCreateBufferView

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateBufferView

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyBufferView :: CString

pattern VkDestroyBufferView <- (is_VkDestroyBufferView -> True)
  where VkDestroyBufferView = _VkDestroyBufferView

{-# INLINE _VkDestroyBufferView #-}

_VkDestroyBufferView :: CString
_VkDestroyBufferView = Ptr "vkDestroyBufferView\NUL"##

{-# INLINE is_VkDestroyBufferView #-}

is_VkDestroyBufferView :: CString -> Bool
is_VkDestroyBufferView = (EQ ==) . cmpCStrings _VkDestroyBufferView

type VkDestroyBufferView = "vkDestroyBufferView"

-- | > () vkDestroyBufferView
--   >     ( VkDevice device
--   >     , VkBufferView bufferView
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyBufferView.html vkDestroyBufferView registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyBufferView"
               vkDestroyBufferView ::
               VkDevice -- ^ device
                        -> VkBufferView -- ^ bufferView
                                        -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                     -> IO ()

-- | > () vkDestroyBufferView
--   >     ( VkDevice device
--   >     , VkBufferView bufferView
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyBufferView.html vkDestroyBufferView registry at www.khronos.org>
foreign import ccall safe "vkDestroyBufferView"
               vkDestroyBufferViewSafe ::
               VkDevice -- ^ device
                        -> VkBufferView -- ^ bufferView
                                        -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                     -> IO ()

-- | > () vkDestroyBufferView
--   >     ( VkDevice device
--   >     , VkBufferView bufferView
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyBufferView.html vkDestroyBufferView registry at www.khronos.org>
type HS_vkDestroyBufferView =
     VkDevice -- ^ device
              -> VkBufferView -- ^ bufferView
                              -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                           -> IO ()

type PFN_vkDestroyBufferView = FunPtr HS_vkDestroyBufferView

foreign import ccall "dynamic" unwrapVkDestroyBufferView ::
               PFN_vkDestroyBufferView -> HS_vkDestroyBufferView

instance VulkanProc "vkDestroyBufferView" where
        type VkProcType "vkDestroyBufferView" = HS_vkDestroyBufferView
        vkProcSymbol = _VkDestroyBufferView

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyBufferView

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateImage :: CString

pattern VkCreateImage <- (is_VkCreateImage -> True)
  where VkCreateImage = _VkCreateImage

{-# INLINE _VkCreateImage #-}

_VkCreateImage :: CString
_VkCreateImage = Ptr "vkCreateImage\NUL"##

{-# INLINE is_VkCreateImage #-}

is_VkCreateImage :: CString -> Bool
is_VkCreateImage = (EQ ==) . cmpCStrings _VkCreateImage

type VkCreateImage = "vkCreateImage"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateImage
--   >     ( VkDevice device
--   >     , const VkImageCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkImage* pImage
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateImage.html vkCreateImage registry at www.khronos.org>
foreign import ccall unsafe "vkCreateImage" vkCreateImage ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageCreateInfo -- ^ pCreateInfo
                                       ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkImage -- ^ pImage
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateImage
--   >     ( VkDevice device
--   >     , const VkImageCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkImage* pImage
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateImage.html vkCreateImage registry at www.khronos.org>
foreign import ccall safe "vkCreateImage" vkCreateImageSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageCreateInfo -- ^ pCreateInfo
                                       ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkImage -- ^ pImage
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateImage
--   >     ( VkDevice device
--   >     , const VkImageCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkImage* pImage
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateImage.html vkCreateImage registry at www.khronos.org>
type HS_vkCreateImage =
     VkDevice -- ^ device
              ->
       Ptr VkImageCreateInfo -- ^ pCreateInfo
                             ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkImage -- ^ pImage
                                                  -> IO VkResult

type PFN_vkCreateImage = FunPtr HS_vkCreateImage

foreign import ccall "dynamic" unwrapVkCreateImage ::
               PFN_vkCreateImage -> HS_vkCreateImage

instance VulkanProc "vkCreateImage" where
        type VkProcType "vkCreateImage" = HS_vkCreateImage
        vkProcSymbol = _VkCreateImage

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateImage

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyImage :: CString

pattern VkDestroyImage <- (is_VkDestroyImage -> True)
  where VkDestroyImage = _VkDestroyImage

{-# INLINE _VkDestroyImage #-}

_VkDestroyImage :: CString
_VkDestroyImage = Ptr "vkDestroyImage\NUL"##

{-# INLINE is_VkDestroyImage #-}

is_VkDestroyImage :: CString -> Bool
is_VkDestroyImage = (EQ ==) . cmpCStrings _VkDestroyImage

type VkDestroyImage = "vkDestroyImage"

-- | > () vkDestroyImage
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyImage.html vkDestroyImage registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyImage" vkDestroyImage ::
               VkDevice -- ^ device
                        -> VkImage -- ^ image
                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> IO ()

-- | > () vkDestroyImage
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyImage.html vkDestroyImage registry at www.khronos.org>
foreign import ccall safe "vkDestroyImage" vkDestroyImageSafe ::
               VkDevice -- ^ device
                        -> VkImage -- ^ image
                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> IO ()

-- | > () vkDestroyImage
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyImage.html vkDestroyImage registry at www.khronos.org>
type HS_vkDestroyImage =
     VkDevice -- ^ device
              -> VkImage -- ^ image
                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                      -> IO ()

type PFN_vkDestroyImage = FunPtr HS_vkDestroyImage

foreign import ccall "dynamic" unwrapVkDestroyImage ::
               PFN_vkDestroyImage -> HS_vkDestroyImage

instance VulkanProc "vkDestroyImage" where
        type VkProcType "vkDestroyImage" = HS_vkDestroyImage
        vkProcSymbol = _VkDestroyImage

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyImage

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetImageSubresourceLayout :: CString

pattern VkGetImageSubresourceLayout <-
        (is_VkGetImageSubresourceLayout -> True)
  where VkGetImageSubresourceLayout = _VkGetImageSubresourceLayout

{-# INLINE _VkGetImageSubresourceLayout #-}

_VkGetImageSubresourceLayout :: CString
_VkGetImageSubresourceLayout
  = Ptr "vkGetImageSubresourceLayout\NUL"##

{-# INLINE is_VkGetImageSubresourceLayout #-}

is_VkGetImageSubresourceLayout :: CString -> Bool
is_VkGetImageSubresourceLayout
  = (EQ ==) . cmpCStrings _VkGetImageSubresourceLayout

type VkGetImageSubresourceLayout = "vkGetImageSubresourceLayout"

-- | > () vkGetImageSubresourceLayout
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , const VkImageSubresource* pSubresource
--   >     , VkSubresourceLayout* pLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageSubresourceLayout.html vkGetImageSubresourceLayout registry at www.khronos.org>
foreign import ccall unsafe "vkGetImageSubresourceLayout"
               vkGetImageSubresourceLayout ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         ->
                   Ptr VkImageSubresource -- ^ pSubresource
                                          -> Ptr VkSubresourceLayout -- ^ pLayout
                                                                     -> IO ()

-- | > () vkGetImageSubresourceLayout
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , const VkImageSubresource* pSubresource
--   >     , VkSubresourceLayout* pLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageSubresourceLayout.html vkGetImageSubresourceLayout registry at www.khronos.org>
foreign import ccall safe "vkGetImageSubresourceLayout"
               vkGetImageSubresourceLayoutSafe ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         ->
                   Ptr VkImageSubresource -- ^ pSubresource
                                          -> Ptr VkSubresourceLayout -- ^ pLayout
                                                                     -> IO ()

-- | > () vkGetImageSubresourceLayout
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , const VkImageSubresource* pSubresource
--   >     , VkSubresourceLayout* pLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetImageSubresourceLayout.html vkGetImageSubresourceLayout registry at www.khronos.org>
type HS_vkGetImageSubresourceLayout =
     VkDevice -- ^ device
              ->
       VkImage -- ^ image
               ->
         Ptr VkImageSubresource -- ^ pSubresource
                                -> Ptr VkSubresourceLayout -- ^ pLayout
                                                           -> IO ()

type PFN_vkGetImageSubresourceLayout =
     FunPtr HS_vkGetImageSubresourceLayout

foreign import ccall "dynamic" unwrapVkGetImageSubresourceLayout ::
               PFN_vkGetImageSubresourceLayout -> HS_vkGetImageSubresourceLayout

instance VulkanProc "vkGetImageSubresourceLayout" where
        type VkProcType "vkGetImageSubresourceLayout" =
             HS_vkGetImageSubresourceLayout
        vkProcSymbol = _VkGetImageSubresourceLayout

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetImageSubresourceLayout

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateImageView :: CString

pattern VkCreateImageView <- (is_VkCreateImageView -> True)
  where VkCreateImageView = _VkCreateImageView

{-# INLINE _VkCreateImageView #-}

_VkCreateImageView :: CString
_VkCreateImageView = Ptr "vkCreateImageView\NUL"##

{-# INLINE is_VkCreateImageView #-}

is_VkCreateImageView :: CString -> Bool
is_VkCreateImageView = (EQ ==) . cmpCStrings _VkCreateImageView

type VkCreateImageView = "vkCreateImageView"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateImageView
--   >     ( VkDevice device
--   >     , const VkImageViewCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkImageView* pView
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateImageView.html vkCreateImageView registry at www.khronos.org>
foreign import ccall unsafe "vkCreateImageView" vkCreateImageView
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageViewCreateInfo -- ^ pCreateInfo
                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkImageView -- ^ pView
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateImageView
--   >     ( VkDevice device
--   >     , const VkImageViewCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkImageView* pView
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateImageView.html vkCreateImageView registry at www.khronos.org>
foreign import ccall safe "vkCreateImageView" vkCreateImageViewSafe
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageViewCreateInfo -- ^ pCreateInfo
                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkImageView -- ^ pView
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateImageView
--   >     ( VkDevice device
--   >     , const VkImageViewCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkImageView* pView
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateImageView.html vkCreateImageView registry at www.khronos.org>
type HS_vkCreateImageView =
     VkDevice -- ^ device
              ->
       Ptr VkImageViewCreateInfo -- ^ pCreateInfo
                                 ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkImageView -- ^ pView
                                                      -> IO VkResult

type PFN_vkCreateImageView = FunPtr HS_vkCreateImageView

foreign import ccall "dynamic" unwrapVkCreateImageView ::
               PFN_vkCreateImageView -> HS_vkCreateImageView

instance VulkanProc "vkCreateImageView" where
        type VkProcType "vkCreateImageView" = HS_vkCreateImageView
        vkProcSymbol = _VkCreateImageView

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateImageView

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyImageView :: CString

pattern VkDestroyImageView <- (is_VkDestroyImageView -> True)
  where VkDestroyImageView = _VkDestroyImageView

{-# INLINE _VkDestroyImageView #-}

_VkDestroyImageView :: CString
_VkDestroyImageView = Ptr "vkDestroyImageView\NUL"##

{-# INLINE is_VkDestroyImageView #-}

is_VkDestroyImageView :: CString -> Bool
is_VkDestroyImageView = (EQ ==) . cmpCStrings _VkDestroyImageView

type VkDestroyImageView = "vkDestroyImageView"

-- | > () vkDestroyImageView
--   >     ( VkDevice device
--   >     , VkImageView imageView
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyImageView.html vkDestroyImageView registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyImageView" vkDestroyImageView
               :: VkDevice -- ^ device
                           -> VkImageView -- ^ imageView
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

-- | > () vkDestroyImageView
--   >     ( VkDevice device
--   >     , VkImageView imageView
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyImageView.html vkDestroyImageView registry at www.khronos.org>
foreign import ccall safe "vkDestroyImageView"
               vkDestroyImageViewSafe ::
               VkDevice -- ^ device
                        -> VkImageView -- ^ imageView
                                       -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()

-- | > () vkDestroyImageView
--   >     ( VkDevice device
--   >     , VkImageView imageView
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyImageView.html vkDestroyImageView registry at www.khronos.org>
type HS_vkDestroyImageView =
     VkDevice -- ^ device
              -> VkImageView -- ^ imageView
                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          -> IO ()

type PFN_vkDestroyImageView = FunPtr HS_vkDestroyImageView

foreign import ccall "dynamic" unwrapVkDestroyImageView ::
               PFN_vkDestroyImageView -> HS_vkDestroyImageView

instance VulkanProc "vkDestroyImageView" where
        type VkProcType "vkDestroyImageView" = HS_vkDestroyImageView
        vkProcSymbol = _VkDestroyImageView

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyImageView

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateShaderModule :: CString

pattern VkCreateShaderModule <- (is_VkCreateShaderModule -> True)
  where VkCreateShaderModule = _VkCreateShaderModule

{-# INLINE _VkCreateShaderModule #-}

_VkCreateShaderModule :: CString
_VkCreateShaderModule = Ptr "vkCreateShaderModule\NUL"##

{-# INLINE is_VkCreateShaderModule #-}

is_VkCreateShaderModule :: CString -> Bool
is_VkCreateShaderModule
  = (EQ ==) . cmpCStrings _VkCreateShaderModule

type VkCreateShaderModule = "vkCreateShaderModule"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
--   > VkResult vkCreateShaderModule
--   >     ( VkDevice device
--   >     , const VkShaderModuleCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkShaderModule* pShaderModule
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateShaderModule.html vkCreateShaderModule registry at www.khronos.org>
foreign import ccall unsafe "vkCreateShaderModule"
               vkCreateShaderModule ::
               VkDevice -- ^ device
                        ->
                 Ptr VkShaderModuleCreateInfo -- ^ pCreateInfo
                                              ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkShaderModule -- ^ pShaderModule
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
--   > VkResult vkCreateShaderModule
--   >     ( VkDevice device
--   >     , const VkShaderModuleCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkShaderModule* pShaderModule
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateShaderModule.html vkCreateShaderModule registry at www.khronos.org>
foreign import ccall safe "vkCreateShaderModule"
               vkCreateShaderModuleSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkShaderModuleCreateInfo -- ^ pCreateInfo
                                              ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkShaderModule -- ^ pShaderModule
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
--   > VkResult vkCreateShaderModule
--   >     ( VkDevice device
--   >     , const VkShaderModuleCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkShaderModule* pShaderModule
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateShaderModule.html vkCreateShaderModule registry at www.khronos.org>
type HS_vkCreateShaderModule =
     VkDevice -- ^ device
              ->
       Ptr VkShaderModuleCreateInfo -- ^ pCreateInfo
                                    ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkShaderModule -- ^ pShaderModule
                                                         -> IO VkResult

type PFN_vkCreateShaderModule = FunPtr HS_vkCreateShaderModule

foreign import ccall "dynamic" unwrapVkCreateShaderModule ::
               PFN_vkCreateShaderModule -> HS_vkCreateShaderModule

instance VulkanProc "vkCreateShaderModule" where
        type VkProcType "vkCreateShaderModule" = HS_vkCreateShaderModule
        vkProcSymbol = _VkCreateShaderModule

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateShaderModule

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyShaderModule :: CString

pattern VkDestroyShaderModule <- (is_VkDestroyShaderModule -> True)
  where VkDestroyShaderModule = _VkDestroyShaderModule

{-# INLINE _VkDestroyShaderModule #-}

_VkDestroyShaderModule :: CString
_VkDestroyShaderModule = Ptr "vkDestroyShaderModule\NUL"##

{-# INLINE is_VkDestroyShaderModule #-}

is_VkDestroyShaderModule :: CString -> Bool
is_VkDestroyShaderModule
  = (EQ ==) . cmpCStrings _VkDestroyShaderModule

type VkDestroyShaderModule = "vkDestroyShaderModule"

-- | > () vkDestroyShaderModule
--   >     ( VkDevice device
--   >     , VkShaderModule shaderModule
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyShaderModule.html vkDestroyShaderModule registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyShaderModule"
               vkDestroyShaderModule ::
               VkDevice -- ^ device
                        -> VkShaderModule -- ^ shaderModule
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

-- | > () vkDestroyShaderModule
--   >     ( VkDevice device
--   >     , VkShaderModule shaderModule
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyShaderModule.html vkDestroyShaderModule registry at www.khronos.org>
foreign import ccall safe "vkDestroyShaderModule"
               vkDestroyShaderModuleSafe ::
               VkDevice -- ^ device
                        -> VkShaderModule -- ^ shaderModule
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

-- | > () vkDestroyShaderModule
--   >     ( VkDevice device
--   >     , VkShaderModule shaderModule
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyShaderModule.html vkDestroyShaderModule registry at www.khronos.org>
type HS_vkDestroyShaderModule =
     VkDevice -- ^ device
              -> VkShaderModule -- ^ shaderModule
                                -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                             -> IO ()

type PFN_vkDestroyShaderModule = FunPtr HS_vkDestroyShaderModule

foreign import ccall "dynamic" unwrapVkDestroyShaderModule ::
               PFN_vkDestroyShaderModule -> HS_vkDestroyShaderModule

instance VulkanProc "vkDestroyShaderModule" where
        type VkProcType "vkDestroyShaderModule" = HS_vkDestroyShaderModule
        vkProcSymbol = _VkDestroyShaderModule

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyShaderModule

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreatePipelineCache :: CString

pattern VkCreatePipelineCache <- (is_VkCreatePipelineCache -> True)
  where VkCreatePipelineCache = _VkCreatePipelineCache

{-# INLINE _VkCreatePipelineCache #-}

_VkCreatePipelineCache :: CString
_VkCreatePipelineCache = Ptr "vkCreatePipelineCache\NUL"##

{-# INLINE is_VkCreatePipelineCache #-}

is_VkCreatePipelineCache :: CString -> Bool
is_VkCreatePipelineCache
  = (EQ ==) . cmpCStrings _VkCreatePipelineCache

type VkCreatePipelineCache = "vkCreatePipelineCache"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreatePipelineCache
--   >     ( VkDevice device
--   >     , const VkPipelineCacheCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipelineCache* pPipelineCache
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreatePipelineCache.html vkCreatePipelineCache registry at www.khronos.org>
foreign import ccall unsafe "vkCreatePipelineCache"
               vkCreatePipelineCache ::
               VkDevice -- ^ device
                        ->
                 Ptr VkPipelineCacheCreateInfo -- ^ pCreateInfo
                                               ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkPipelineCache -- ^ pPipelineCache
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreatePipelineCache
--   >     ( VkDevice device
--   >     , const VkPipelineCacheCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipelineCache* pPipelineCache
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreatePipelineCache.html vkCreatePipelineCache registry at www.khronos.org>
foreign import ccall safe "vkCreatePipelineCache"
               vkCreatePipelineCacheSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkPipelineCacheCreateInfo -- ^ pCreateInfo
                                               ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkPipelineCache -- ^ pPipelineCache
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreatePipelineCache
--   >     ( VkDevice device
--   >     , const VkPipelineCacheCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipelineCache* pPipelineCache
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreatePipelineCache.html vkCreatePipelineCache registry at www.khronos.org>
type HS_vkCreatePipelineCache =
     VkDevice -- ^ device
              ->
       Ptr VkPipelineCacheCreateInfo -- ^ pCreateInfo
                                     ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkPipelineCache -- ^ pPipelineCache
                                                          -> IO VkResult

type PFN_vkCreatePipelineCache = FunPtr HS_vkCreatePipelineCache

foreign import ccall "dynamic" unwrapVkCreatePipelineCache ::
               PFN_vkCreatePipelineCache -> HS_vkCreatePipelineCache

instance VulkanProc "vkCreatePipelineCache" where
        type VkProcType "vkCreatePipelineCache" = HS_vkCreatePipelineCache
        vkProcSymbol = _VkCreatePipelineCache

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreatePipelineCache

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyPipelineCache :: CString

pattern VkDestroyPipelineCache <-
        (is_VkDestroyPipelineCache -> True)
  where VkDestroyPipelineCache = _VkDestroyPipelineCache

{-# INLINE _VkDestroyPipelineCache #-}

_VkDestroyPipelineCache :: CString
_VkDestroyPipelineCache = Ptr "vkDestroyPipelineCache\NUL"##

{-# INLINE is_VkDestroyPipelineCache #-}

is_VkDestroyPipelineCache :: CString -> Bool
is_VkDestroyPipelineCache
  = (EQ ==) . cmpCStrings _VkDestroyPipelineCache

type VkDestroyPipelineCache = "vkDestroyPipelineCache"

-- | > () vkDestroyPipelineCache
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyPipelineCache.html vkDestroyPipelineCache registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyPipelineCache"
               vkDestroyPipelineCache ::
               VkDevice -- ^ device
                        -> VkPipelineCache -- ^ pipelineCache
                                           -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                        -> IO ()

-- | > () vkDestroyPipelineCache
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyPipelineCache.html vkDestroyPipelineCache registry at www.khronos.org>
foreign import ccall safe "vkDestroyPipelineCache"
               vkDestroyPipelineCacheSafe ::
               VkDevice -- ^ device
                        -> VkPipelineCache -- ^ pipelineCache
                                           -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                        -> IO ()

-- | > () vkDestroyPipelineCache
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyPipelineCache.html vkDestroyPipelineCache registry at www.khronos.org>
type HS_vkDestroyPipelineCache =
     VkDevice -- ^ device
              -> VkPipelineCache -- ^ pipelineCache
                                 -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                              -> IO ()

type PFN_vkDestroyPipelineCache = FunPtr HS_vkDestroyPipelineCache

foreign import ccall "dynamic" unwrapVkDestroyPipelineCache ::
               PFN_vkDestroyPipelineCache -> HS_vkDestroyPipelineCache

instance VulkanProc "vkDestroyPipelineCache" where
        type VkProcType "vkDestroyPipelineCache" =
             HS_vkDestroyPipelineCache
        vkProcSymbol = _VkDestroyPipelineCache

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyPipelineCache

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPipelineCacheData :: CString

pattern VkGetPipelineCacheData <-
        (is_VkGetPipelineCacheData -> True)
  where VkGetPipelineCacheData = _VkGetPipelineCacheData

{-# INLINE _VkGetPipelineCacheData #-}

_VkGetPipelineCacheData :: CString
_VkGetPipelineCacheData = Ptr "vkGetPipelineCacheData\NUL"##

{-# INLINE is_VkGetPipelineCacheData #-}

is_VkGetPipelineCacheData :: CString -> Bool
is_VkGetPipelineCacheData
  = (EQ ==) . cmpCStrings _VkGetPipelineCacheData

type VkGetPipelineCacheData = "vkGetPipelineCacheData"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPipelineCacheData
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , size_t* pDataSize
--   >     , void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPipelineCacheData.html vkGetPipelineCacheData registry at www.khronos.org>
foreign import ccall unsafe "vkGetPipelineCacheData"
               vkGetPipelineCacheData ::
               VkDevice -- ^ device
                        -> VkPipelineCache -- ^ pipelineCache
                                           -> Ptr CSize -- ^ pDataSize
                                                        -> Ptr Void -- ^ pData
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPipelineCacheData
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , size_t* pDataSize
--   >     , void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPipelineCacheData.html vkGetPipelineCacheData registry at www.khronos.org>
foreign import ccall safe "vkGetPipelineCacheData"
               vkGetPipelineCacheDataSafe ::
               VkDevice -- ^ device
                        -> VkPipelineCache -- ^ pipelineCache
                                           -> Ptr CSize -- ^ pDataSize
                                                        -> Ptr Void -- ^ pData
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPipelineCacheData
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , size_t* pDataSize
--   >     , void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPipelineCacheData.html vkGetPipelineCacheData registry at www.khronos.org>
type HS_vkGetPipelineCacheData =
     VkDevice -- ^ device
              -> VkPipelineCache -- ^ pipelineCache
                                 -> Ptr CSize -- ^ pDataSize
                                              -> Ptr Void -- ^ pData
                                                          -> IO VkResult

type PFN_vkGetPipelineCacheData = FunPtr HS_vkGetPipelineCacheData

foreign import ccall "dynamic" unwrapVkGetPipelineCacheData ::
               PFN_vkGetPipelineCacheData -> HS_vkGetPipelineCacheData

instance VulkanProc "vkGetPipelineCacheData" where
        type VkProcType "vkGetPipelineCacheData" =
             HS_vkGetPipelineCacheData
        vkProcSymbol = _VkGetPipelineCacheData

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPipelineCacheData

        {-# INLINE unwrapVkProcPtr #-}

pattern VkMergePipelineCaches :: CString

pattern VkMergePipelineCaches <- (is_VkMergePipelineCaches -> True)
  where VkMergePipelineCaches = _VkMergePipelineCaches

{-# INLINE _VkMergePipelineCaches #-}

_VkMergePipelineCaches :: CString
_VkMergePipelineCaches = Ptr "vkMergePipelineCaches\NUL"##

{-# INLINE is_VkMergePipelineCaches #-}

is_VkMergePipelineCaches :: CString -> Bool
is_VkMergePipelineCaches
  = (EQ ==) . cmpCStrings _VkMergePipelineCaches

type VkMergePipelineCaches = "vkMergePipelineCaches"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkMergePipelineCaches
--   >     ( VkDevice device
--   >     , VkPipelineCache dstCache
--   >     , uint32_t srcCacheCount
--   >     , const VkPipelineCache* pSrcCaches
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkMergePipelineCaches.html vkMergePipelineCaches registry at www.khronos.org>
foreign import ccall unsafe "vkMergePipelineCaches"
               vkMergePipelineCaches ::
               VkDevice -- ^ device
                        ->
                 VkPipelineCache -- ^ dstCache
                                 -> Word32 -- ^ srcCacheCount
                                           -> Ptr VkPipelineCache -- ^ pSrcCaches
                                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkMergePipelineCaches
--   >     ( VkDevice device
--   >     , VkPipelineCache dstCache
--   >     , uint32_t srcCacheCount
--   >     , const VkPipelineCache* pSrcCaches
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkMergePipelineCaches.html vkMergePipelineCaches registry at www.khronos.org>
foreign import ccall safe "vkMergePipelineCaches"
               vkMergePipelineCachesSafe ::
               VkDevice -- ^ device
                        ->
                 VkPipelineCache -- ^ dstCache
                                 -> Word32 -- ^ srcCacheCount
                                           -> Ptr VkPipelineCache -- ^ pSrcCaches
                                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkMergePipelineCaches
--   >     ( VkDevice device
--   >     , VkPipelineCache dstCache
--   >     , uint32_t srcCacheCount
--   >     , const VkPipelineCache* pSrcCaches
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkMergePipelineCaches.html vkMergePipelineCaches registry at www.khronos.org>
type HS_vkMergePipelineCaches =
     VkDevice -- ^ device
              ->
       VkPipelineCache -- ^ dstCache
                       -> Word32 -- ^ srcCacheCount
                                 -> Ptr VkPipelineCache -- ^ pSrcCaches
                                                        -> IO VkResult

type PFN_vkMergePipelineCaches = FunPtr HS_vkMergePipelineCaches

foreign import ccall "dynamic" unwrapVkMergePipelineCaches ::
               PFN_vkMergePipelineCaches -> HS_vkMergePipelineCaches

instance VulkanProc "vkMergePipelineCaches" where
        type VkProcType "vkMergePipelineCaches" = HS_vkMergePipelineCaches
        vkProcSymbol = _VkMergePipelineCaches

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkMergePipelineCaches

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateGraphicsPipelines :: CString

pattern VkCreateGraphicsPipelines <-
        (is_VkCreateGraphicsPipelines -> True)
  where VkCreateGraphicsPipelines = _VkCreateGraphicsPipelines

{-# INLINE _VkCreateGraphicsPipelines #-}

_VkCreateGraphicsPipelines :: CString
_VkCreateGraphicsPipelines = Ptr "vkCreateGraphicsPipelines\NUL"##

{-# INLINE is_VkCreateGraphicsPipelines #-}

is_VkCreateGraphicsPipelines :: CString -> Bool
is_VkCreateGraphicsPipelines
  = (EQ ==) . cmpCStrings _VkCreateGraphicsPipelines

type VkCreateGraphicsPipelines = "vkCreateGraphicsPipelines"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
--   > VkResult vkCreateGraphicsPipelines
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , uint32_t createInfoCount
--   >     , const VkGraphicsPipelineCreateInfo* pCreateInfos
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipeline* pPipelines
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateGraphicsPipelines.html vkCreateGraphicsPipelines registry at www.khronos.org>
foreign import ccall unsafe "vkCreateGraphicsPipelines"
               vkCreateGraphicsPipelines ::
               VkDevice -- ^ device
                        ->
                 VkPipelineCache -- ^ pipelineCache
                                 ->
                   Word32 -- ^ createInfoCount
                          ->
                     Ptr VkGraphicsPipelineCreateInfo -- ^ pCreateInfos
                                                      ->
                       Ptr VkAllocationCallbacks -- ^ pAllocator
                                                 -> Ptr VkPipeline -- ^ pPipelines
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
--   > VkResult vkCreateGraphicsPipelines
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , uint32_t createInfoCount
--   >     , const VkGraphicsPipelineCreateInfo* pCreateInfos
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipeline* pPipelines
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateGraphicsPipelines.html vkCreateGraphicsPipelines registry at www.khronos.org>
foreign import ccall safe "vkCreateGraphicsPipelines"
               vkCreateGraphicsPipelinesSafe ::
               VkDevice -- ^ device
                        ->
                 VkPipelineCache -- ^ pipelineCache
                                 ->
                   Word32 -- ^ createInfoCount
                          ->
                     Ptr VkGraphicsPipelineCreateInfo -- ^ pCreateInfos
                                                      ->
                       Ptr VkAllocationCallbacks -- ^ pAllocator
                                                 -> Ptr VkPipeline -- ^ pPipelines
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
--   > VkResult vkCreateGraphicsPipelines
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , uint32_t createInfoCount
--   >     , const VkGraphicsPipelineCreateInfo* pCreateInfos
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipeline* pPipelines
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateGraphicsPipelines.html vkCreateGraphicsPipelines registry at www.khronos.org>
type HS_vkCreateGraphicsPipelines =
     VkDevice -- ^ device
              ->
       VkPipelineCache -- ^ pipelineCache
                       ->
         Word32 -- ^ createInfoCount
                ->
           Ptr VkGraphicsPipelineCreateInfo -- ^ pCreateInfos
                                            ->
             Ptr VkAllocationCallbacks -- ^ pAllocator
                                       -> Ptr VkPipeline -- ^ pPipelines
                                                         -> IO VkResult

type PFN_vkCreateGraphicsPipelines =
     FunPtr HS_vkCreateGraphicsPipelines

foreign import ccall "dynamic" unwrapVkCreateGraphicsPipelines ::
               PFN_vkCreateGraphicsPipelines -> HS_vkCreateGraphicsPipelines

instance VulkanProc "vkCreateGraphicsPipelines" where
        type VkProcType "vkCreateGraphicsPipelines" =
             HS_vkCreateGraphicsPipelines
        vkProcSymbol = _VkCreateGraphicsPipelines

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateGraphicsPipelines

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateComputePipelines :: CString

pattern VkCreateComputePipelines <-
        (is_VkCreateComputePipelines -> True)
  where VkCreateComputePipelines = _VkCreateComputePipelines

{-# INLINE _VkCreateComputePipelines #-}

_VkCreateComputePipelines :: CString
_VkCreateComputePipelines = Ptr "vkCreateComputePipelines\NUL"##

{-# INLINE is_VkCreateComputePipelines #-}

is_VkCreateComputePipelines :: CString -> Bool
is_VkCreateComputePipelines
  = (EQ ==) . cmpCStrings _VkCreateComputePipelines

type VkCreateComputePipelines = "vkCreateComputePipelines"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
--   > VkResult vkCreateComputePipelines
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , uint32_t createInfoCount
--   >     , const VkComputePipelineCreateInfo* pCreateInfos
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipeline* pPipelines
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateComputePipelines.html vkCreateComputePipelines registry at www.khronos.org>
foreign import ccall unsafe "vkCreateComputePipelines"
               vkCreateComputePipelines ::
               VkDevice -- ^ device
                        ->
                 VkPipelineCache -- ^ pipelineCache
                                 ->
                   Word32 -- ^ createInfoCount
                          ->
                     Ptr VkComputePipelineCreateInfo -- ^ pCreateInfos
                                                     ->
                       Ptr VkAllocationCallbacks -- ^ pAllocator
                                                 -> Ptr VkPipeline -- ^ pPipelines
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
--   > VkResult vkCreateComputePipelines
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , uint32_t createInfoCount
--   >     , const VkComputePipelineCreateInfo* pCreateInfos
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipeline* pPipelines
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateComputePipelines.html vkCreateComputePipelines registry at www.khronos.org>
foreign import ccall safe "vkCreateComputePipelines"
               vkCreateComputePipelinesSafe ::
               VkDevice -- ^ device
                        ->
                 VkPipelineCache -- ^ pipelineCache
                                 ->
                   Word32 -- ^ createInfoCount
                          ->
                     Ptr VkComputePipelineCreateInfo -- ^ pCreateInfos
                                                     ->
                       Ptr VkAllocationCallbacks -- ^ pAllocator
                                                 -> Ptr VkPipeline -- ^ pPipelines
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
--   > VkResult vkCreateComputePipelines
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , uint32_t createInfoCount
--   >     , const VkComputePipelineCreateInfo* pCreateInfos
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipeline* pPipelines
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateComputePipelines.html vkCreateComputePipelines registry at www.khronos.org>
type HS_vkCreateComputePipelines =
     VkDevice -- ^ device
              ->
       VkPipelineCache -- ^ pipelineCache
                       ->
         Word32 -- ^ createInfoCount
                ->
           Ptr VkComputePipelineCreateInfo -- ^ pCreateInfos
                                           ->
             Ptr VkAllocationCallbacks -- ^ pAllocator
                                       -> Ptr VkPipeline -- ^ pPipelines
                                                         -> IO VkResult

type PFN_vkCreateComputePipelines =
     FunPtr HS_vkCreateComputePipelines

foreign import ccall "dynamic" unwrapVkCreateComputePipelines ::
               PFN_vkCreateComputePipelines -> HS_vkCreateComputePipelines

instance VulkanProc "vkCreateComputePipelines" where
        type VkProcType "vkCreateComputePipelines" =
             HS_vkCreateComputePipelines
        vkProcSymbol = _VkCreateComputePipelines

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateComputePipelines

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyPipeline :: CString

pattern VkDestroyPipeline <- (is_VkDestroyPipeline -> True)
  where VkDestroyPipeline = _VkDestroyPipeline

{-# INLINE _VkDestroyPipeline #-}

_VkDestroyPipeline :: CString
_VkDestroyPipeline = Ptr "vkDestroyPipeline\NUL"##

{-# INLINE is_VkDestroyPipeline #-}

is_VkDestroyPipeline :: CString -> Bool
is_VkDestroyPipeline = (EQ ==) . cmpCStrings _VkDestroyPipeline

type VkDestroyPipeline = "vkDestroyPipeline"

-- | > () vkDestroyPipeline
--   >     ( VkDevice device
--   >     , VkPipeline pipeline
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyPipeline.html vkDestroyPipeline registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyPipeline" vkDestroyPipeline
               :: VkDevice -- ^ device
                           -> VkPipeline -- ^ pipeline
                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()

-- | > () vkDestroyPipeline
--   >     ( VkDevice device
--   >     , VkPipeline pipeline
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyPipeline.html vkDestroyPipeline registry at www.khronos.org>
foreign import ccall safe "vkDestroyPipeline" vkDestroyPipelineSafe
               :: VkDevice -- ^ device
                           -> VkPipeline -- ^ pipeline
                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()

-- | > () vkDestroyPipeline
--   >     ( VkDevice device
--   >     , VkPipeline pipeline
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyPipeline.html vkDestroyPipeline registry at www.khronos.org>
type HS_vkDestroyPipeline =
     VkDevice -- ^ device
              -> VkPipeline -- ^ pipeline
                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         -> IO ()

type PFN_vkDestroyPipeline = FunPtr HS_vkDestroyPipeline

foreign import ccall "dynamic" unwrapVkDestroyPipeline ::
               PFN_vkDestroyPipeline -> HS_vkDestroyPipeline

instance VulkanProc "vkDestroyPipeline" where
        type VkProcType "vkDestroyPipeline" = HS_vkDestroyPipeline
        vkProcSymbol = _VkDestroyPipeline

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyPipeline

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreatePipelineLayout :: CString

pattern VkCreatePipelineLayout <-
        (is_VkCreatePipelineLayout -> True)
  where VkCreatePipelineLayout = _VkCreatePipelineLayout

{-# INLINE _VkCreatePipelineLayout #-}

_VkCreatePipelineLayout :: CString
_VkCreatePipelineLayout = Ptr "vkCreatePipelineLayout\NUL"##

{-# INLINE is_VkCreatePipelineLayout #-}

is_VkCreatePipelineLayout :: CString -> Bool
is_VkCreatePipelineLayout
  = (EQ ==) . cmpCStrings _VkCreatePipelineLayout

type VkCreatePipelineLayout = "vkCreatePipelineLayout"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreatePipelineLayout
--   >     ( VkDevice device
--   >     , const VkPipelineLayoutCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipelineLayout* pPipelineLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreatePipelineLayout.html vkCreatePipelineLayout registry at www.khronos.org>
foreign import ccall unsafe "vkCreatePipelineLayout"
               vkCreatePipelineLayout ::
               VkDevice -- ^ device
                        ->
                 Ptr VkPipelineLayoutCreateInfo -- ^ pCreateInfo
                                                ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkPipelineLayout -- ^ pPipelineLayout
                                                                     -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreatePipelineLayout
--   >     ( VkDevice device
--   >     , const VkPipelineLayoutCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipelineLayout* pPipelineLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreatePipelineLayout.html vkCreatePipelineLayout registry at www.khronos.org>
foreign import ccall safe "vkCreatePipelineLayout"
               vkCreatePipelineLayoutSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkPipelineLayoutCreateInfo -- ^ pCreateInfo
                                                ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkPipelineLayout -- ^ pPipelineLayout
                                                                     -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreatePipelineLayout
--   >     ( VkDevice device
--   >     , const VkPipelineLayoutCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipelineLayout* pPipelineLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreatePipelineLayout.html vkCreatePipelineLayout registry at www.khronos.org>
type HS_vkCreatePipelineLayout =
     VkDevice -- ^ device
              ->
       Ptr VkPipelineLayoutCreateInfo -- ^ pCreateInfo
                                      ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkPipelineLayout -- ^ pPipelineLayout
                                                           -> IO VkResult

type PFN_vkCreatePipelineLayout = FunPtr HS_vkCreatePipelineLayout

foreign import ccall "dynamic" unwrapVkCreatePipelineLayout ::
               PFN_vkCreatePipelineLayout -> HS_vkCreatePipelineLayout

instance VulkanProc "vkCreatePipelineLayout" where
        type VkProcType "vkCreatePipelineLayout" =
             HS_vkCreatePipelineLayout
        vkProcSymbol = _VkCreatePipelineLayout

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreatePipelineLayout

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyPipelineLayout :: CString

pattern VkDestroyPipelineLayout <-
        (is_VkDestroyPipelineLayout -> True)
  where VkDestroyPipelineLayout = _VkDestroyPipelineLayout

{-# INLINE _VkDestroyPipelineLayout #-}

_VkDestroyPipelineLayout :: CString
_VkDestroyPipelineLayout = Ptr "vkDestroyPipelineLayout\NUL"##

{-# INLINE is_VkDestroyPipelineLayout #-}

is_VkDestroyPipelineLayout :: CString -> Bool
is_VkDestroyPipelineLayout
  = (EQ ==) . cmpCStrings _VkDestroyPipelineLayout

type VkDestroyPipelineLayout = "vkDestroyPipelineLayout"

-- | > () vkDestroyPipelineLayout
--   >     ( VkDevice device
--   >     , VkPipelineLayout pipelineLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyPipelineLayout.html vkDestroyPipelineLayout registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyPipelineLayout"
               vkDestroyPipelineLayout ::
               VkDevice -- ^ device
                        -> VkPipelineLayout -- ^ pipelineLayout
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

-- | > () vkDestroyPipelineLayout
--   >     ( VkDevice device
--   >     , VkPipelineLayout pipelineLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyPipelineLayout.html vkDestroyPipelineLayout registry at www.khronos.org>
foreign import ccall safe "vkDestroyPipelineLayout"
               vkDestroyPipelineLayoutSafe ::
               VkDevice -- ^ device
                        -> VkPipelineLayout -- ^ pipelineLayout
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

-- | > () vkDestroyPipelineLayout
--   >     ( VkDevice device
--   >     , VkPipelineLayout pipelineLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyPipelineLayout.html vkDestroyPipelineLayout registry at www.khronos.org>
type HS_vkDestroyPipelineLayout =
     VkDevice -- ^ device
              -> VkPipelineLayout -- ^ pipelineLayout
                                  -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                               -> IO ()

type PFN_vkDestroyPipelineLayout =
     FunPtr HS_vkDestroyPipelineLayout

foreign import ccall "dynamic" unwrapVkDestroyPipelineLayout ::
               PFN_vkDestroyPipelineLayout -> HS_vkDestroyPipelineLayout

instance VulkanProc "vkDestroyPipelineLayout" where
        type VkProcType "vkDestroyPipelineLayout" =
             HS_vkDestroyPipelineLayout
        vkProcSymbol = _VkDestroyPipelineLayout

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyPipelineLayout

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateSampler :: CString

pattern VkCreateSampler <- (is_VkCreateSampler -> True)
  where VkCreateSampler = _VkCreateSampler

{-# INLINE _VkCreateSampler #-}

_VkCreateSampler :: CString
_VkCreateSampler = Ptr "vkCreateSampler\NUL"##

{-# INLINE is_VkCreateSampler #-}

is_VkCreateSampler :: CString -> Bool
is_VkCreateSampler = (EQ ==) . cmpCStrings _VkCreateSampler

type VkCreateSampler = "vkCreateSampler"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_TOO_MANY_OBJECTS'.
--
--   > VkResult vkCreateSampler
--   >     ( VkDevice device
--   >     , const VkSamplerCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSampler* pSampler
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateSampler.html vkCreateSampler registry at www.khronos.org>
foreign import ccall unsafe "vkCreateSampler" vkCreateSampler ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSamplerCreateInfo -- ^ pCreateInfo
                                         ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSampler -- ^ pSampler
                                                              -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_TOO_MANY_OBJECTS'.
--
--   > VkResult vkCreateSampler
--   >     ( VkDevice device
--   >     , const VkSamplerCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSampler* pSampler
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateSampler.html vkCreateSampler registry at www.khronos.org>
foreign import ccall safe "vkCreateSampler" vkCreateSamplerSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSamplerCreateInfo -- ^ pCreateInfo
                                         ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSampler -- ^ pSampler
                                                              -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_TOO_MANY_OBJECTS'.
--
--   > VkResult vkCreateSampler
--   >     ( VkDevice device
--   >     , const VkSamplerCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSampler* pSampler
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateSampler.html vkCreateSampler registry at www.khronos.org>
type HS_vkCreateSampler =
     VkDevice -- ^ device
              ->
       Ptr VkSamplerCreateInfo -- ^ pCreateInfo
                               ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSampler -- ^ pSampler
                                                    -> IO VkResult

type PFN_vkCreateSampler = FunPtr HS_vkCreateSampler

foreign import ccall "dynamic" unwrapVkCreateSampler ::
               PFN_vkCreateSampler -> HS_vkCreateSampler

instance VulkanProc "vkCreateSampler" where
        type VkProcType "vkCreateSampler" = HS_vkCreateSampler
        vkProcSymbol = _VkCreateSampler

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateSampler

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroySampler :: CString

pattern VkDestroySampler <- (is_VkDestroySampler -> True)
  where VkDestroySampler = _VkDestroySampler

{-# INLINE _VkDestroySampler #-}

_VkDestroySampler :: CString
_VkDestroySampler = Ptr "vkDestroySampler\NUL"##

{-# INLINE is_VkDestroySampler #-}

is_VkDestroySampler :: CString -> Bool
is_VkDestroySampler = (EQ ==) . cmpCStrings _VkDestroySampler

type VkDestroySampler = "vkDestroySampler"

-- | > () vkDestroySampler
--   >     ( VkDevice device
--   >     , VkSampler sampler
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroySampler.html vkDestroySampler registry at www.khronos.org>
foreign import ccall unsafe "vkDestroySampler" vkDestroySampler ::
               VkDevice -- ^ device
                        -> VkSampler -- ^ sampler
                                     -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                  -> IO ()

-- | > () vkDestroySampler
--   >     ( VkDevice device
--   >     , VkSampler sampler
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroySampler.html vkDestroySampler registry at www.khronos.org>
foreign import ccall safe "vkDestroySampler" vkDestroySamplerSafe
               :: VkDevice -- ^ device
                           -> VkSampler -- ^ sampler
                                        -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                     -> IO ()

-- | > () vkDestroySampler
--   >     ( VkDevice device
--   >     , VkSampler sampler
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroySampler.html vkDestroySampler registry at www.khronos.org>
type HS_vkDestroySampler =
     VkDevice -- ^ device
              -> VkSampler -- ^ sampler
                           -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                        -> IO ()

type PFN_vkDestroySampler = FunPtr HS_vkDestroySampler

foreign import ccall "dynamic" unwrapVkDestroySampler ::
               PFN_vkDestroySampler -> HS_vkDestroySampler

instance VulkanProc "vkDestroySampler" where
        type VkProcType "vkDestroySampler" = HS_vkDestroySampler
        vkProcSymbol = _VkDestroySampler

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroySampler

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateDescriptorSetLayout :: CString

pattern VkCreateDescriptorSetLayout <-
        (is_VkCreateDescriptorSetLayout -> True)
  where VkCreateDescriptorSetLayout = _VkCreateDescriptorSetLayout

{-# INLINE _VkCreateDescriptorSetLayout #-}

_VkCreateDescriptorSetLayout :: CString
_VkCreateDescriptorSetLayout
  = Ptr "vkCreateDescriptorSetLayout\NUL"##

{-# INLINE is_VkCreateDescriptorSetLayout #-}

is_VkCreateDescriptorSetLayout :: CString -> Bool
is_VkCreateDescriptorSetLayout
  = (EQ ==) . cmpCStrings _VkCreateDescriptorSetLayout

type VkCreateDescriptorSetLayout = "vkCreateDescriptorSetLayout"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDescriptorSetLayout
--   >     ( VkDevice device
--   >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDescriptorSetLayout* pSetLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateDescriptorSetLayout.html vkCreateDescriptorSetLayout registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDescriptorSetLayout"
               vkCreateDescriptorSetLayout ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                     ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkDescriptorSetLayout -- ^ pSetLayout
                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDescriptorSetLayout
--   >     ( VkDevice device
--   >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDescriptorSetLayout* pSetLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateDescriptorSetLayout.html vkCreateDescriptorSetLayout registry at www.khronos.org>
foreign import ccall safe "vkCreateDescriptorSetLayout"
               vkCreateDescriptorSetLayoutSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                     ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkDescriptorSetLayout -- ^ pSetLayout
                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDescriptorSetLayout
--   >     ( VkDevice device
--   >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDescriptorSetLayout* pSetLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateDescriptorSetLayout.html vkCreateDescriptorSetLayout registry at www.khronos.org>
type HS_vkCreateDescriptorSetLayout =
     VkDevice -- ^ device
              ->
       Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                           ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   ->
           Ptr VkDescriptorSetLayout -- ^ pSetLayout
                                     -> IO VkResult

type PFN_vkCreateDescriptorSetLayout =
     FunPtr HS_vkCreateDescriptorSetLayout

foreign import ccall "dynamic" unwrapVkCreateDescriptorSetLayout ::
               PFN_vkCreateDescriptorSetLayout -> HS_vkCreateDescriptorSetLayout

instance VulkanProc "vkCreateDescriptorSetLayout" where
        type VkProcType "vkCreateDescriptorSetLayout" =
             HS_vkCreateDescriptorSetLayout
        vkProcSymbol = _VkCreateDescriptorSetLayout

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateDescriptorSetLayout

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyDescriptorSetLayout :: CString

pattern VkDestroyDescriptorSetLayout <-
        (is_VkDestroyDescriptorSetLayout -> True)
  where VkDestroyDescriptorSetLayout = _VkDestroyDescriptorSetLayout

{-# INLINE _VkDestroyDescriptorSetLayout #-}

_VkDestroyDescriptorSetLayout :: CString
_VkDestroyDescriptorSetLayout
  = Ptr "vkDestroyDescriptorSetLayout\NUL"##

{-# INLINE is_VkDestroyDescriptorSetLayout #-}

is_VkDestroyDescriptorSetLayout :: CString -> Bool
is_VkDestroyDescriptorSetLayout
  = (EQ ==) . cmpCStrings _VkDestroyDescriptorSetLayout

type VkDestroyDescriptorSetLayout = "vkDestroyDescriptorSetLayout"

-- | > () vkDestroyDescriptorSetLayout
--   >     ( VkDevice device
--   >     , VkDescriptorSetLayout descriptorSetLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDescriptorSetLayout.html vkDestroyDescriptorSetLayout registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyDescriptorSetLayout"
               vkDestroyDescriptorSetLayout ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorSetLayout -- ^ descriptorSetLayout
                                       -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()

-- | > () vkDestroyDescriptorSetLayout
--   >     ( VkDevice device
--   >     , VkDescriptorSetLayout descriptorSetLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDescriptorSetLayout.html vkDestroyDescriptorSetLayout registry at www.khronos.org>
foreign import ccall safe "vkDestroyDescriptorSetLayout"
               vkDestroyDescriptorSetLayoutSafe ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorSetLayout -- ^ descriptorSetLayout
                                       -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()

-- | > () vkDestroyDescriptorSetLayout
--   >     ( VkDevice device
--   >     , VkDescriptorSetLayout descriptorSetLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDescriptorSetLayout.html vkDestroyDescriptorSetLayout registry at www.khronos.org>
type HS_vkDestroyDescriptorSetLayout =
     VkDevice -- ^ device
              ->
       VkDescriptorSetLayout -- ^ descriptorSetLayout
                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          -> IO ()

type PFN_vkDestroyDescriptorSetLayout =
     FunPtr HS_vkDestroyDescriptorSetLayout

foreign import ccall "dynamic" unwrapVkDestroyDescriptorSetLayout
               ::
               PFN_vkDestroyDescriptorSetLayout -> HS_vkDestroyDescriptorSetLayout

instance VulkanProc "vkDestroyDescriptorSetLayout" where
        type VkProcType "vkDestroyDescriptorSetLayout" =
             HS_vkDestroyDescriptorSetLayout
        vkProcSymbol = _VkDestroyDescriptorSetLayout

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyDescriptorSetLayout

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateDescriptorPool :: CString

pattern VkCreateDescriptorPool <-
        (is_VkCreateDescriptorPool -> True)
  where VkCreateDescriptorPool = _VkCreateDescriptorPool

{-# INLINE _VkCreateDescriptorPool #-}

_VkCreateDescriptorPool :: CString
_VkCreateDescriptorPool = Ptr "vkCreateDescriptorPool\NUL"##

{-# INLINE is_VkCreateDescriptorPool #-}

is_VkCreateDescriptorPool :: CString -> Bool
is_VkCreateDescriptorPool
  = (EQ ==) . cmpCStrings _VkCreateDescriptorPool

type VkCreateDescriptorPool = "vkCreateDescriptorPool"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FRAGMENTATION_EXT'.
--
--   > VkResult vkCreateDescriptorPool
--   >     ( VkDevice device
--   >     , const VkDescriptorPoolCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDescriptorPool* pDescriptorPool
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateDescriptorPool.html vkCreateDescriptorPool registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDescriptorPool"
               vkCreateDescriptorPool ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorPoolCreateInfo -- ^ pCreateInfo
                                                ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkDescriptorPool -- ^ pDescriptorPool
                                                                     -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FRAGMENTATION_EXT'.
--
--   > VkResult vkCreateDescriptorPool
--   >     ( VkDevice device
--   >     , const VkDescriptorPoolCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDescriptorPool* pDescriptorPool
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateDescriptorPool.html vkCreateDescriptorPool registry at www.khronos.org>
foreign import ccall safe "vkCreateDescriptorPool"
               vkCreateDescriptorPoolSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorPoolCreateInfo -- ^ pCreateInfo
                                                ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkDescriptorPool -- ^ pDescriptorPool
                                                                     -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FRAGMENTATION_EXT'.
--
--   > VkResult vkCreateDescriptorPool
--   >     ( VkDevice device
--   >     , const VkDescriptorPoolCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDescriptorPool* pDescriptorPool
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateDescriptorPool.html vkCreateDescriptorPool registry at www.khronos.org>
type HS_vkCreateDescriptorPool =
     VkDevice -- ^ device
              ->
       Ptr VkDescriptorPoolCreateInfo -- ^ pCreateInfo
                                      ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkDescriptorPool -- ^ pDescriptorPool
                                                           -> IO VkResult

type PFN_vkCreateDescriptorPool = FunPtr HS_vkCreateDescriptorPool

foreign import ccall "dynamic" unwrapVkCreateDescriptorPool ::
               PFN_vkCreateDescriptorPool -> HS_vkCreateDescriptorPool

instance VulkanProc "vkCreateDescriptorPool" where
        type VkProcType "vkCreateDescriptorPool" =
             HS_vkCreateDescriptorPool
        vkProcSymbol = _VkCreateDescriptorPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateDescriptorPool

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyDescriptorPool :: CString

pattern VkDestroyDescriptorPool <-
        (is_VkDestroyDescriptorPool -> True)
  where VkDestroyDescriptorPool = _VkDestroyDescriptorPool

{-# INLINE _VkDestroyDescriptorPool #-}

_VkDestroyDescriptorPool :: CString
_VkDestroyDescriptorPool = Ptr "vkDestroyDescriptorPool\NUL"##

{-# INLINE is_VkDestroyDescriptorPool #-}

is_VkDestroyDescriptorPool :: CString -> Bool
is_VkDestroyDescriptorPool
  = (EQ ==) . cmpCStrings _VkDestroyDescriptorPool

type VkDestroyDescriptorPool = "vkDestroyDescriptorPool"

-- | > () vkDestroyDescriptorPool
--   >     ( VkDevice device
--   >     , VkDescriptorPool descriptorPool
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDescriptorPool.html vkDestroyDescriptorPool registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyDescriptorPool"
               vkDestroyDescriptorPool ::
               VkDevice -- ^ device
                        -> VkDescriptorPool -- ^ descriptorPool
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

-- | > () vkDestroyDescriptorPool
--   >     ( VkDevice device
--   >     , VkDescriptorPool descriptorPool
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDescriptorPool.html vkDestroyDescriptorPool registry at www.khronos.org>
foreign import ccall safe "vkDestroyDescriptorPool"
               vkDestroyDescriptorPoolSafe ::
               VkDevice -- ^ device
                        -> VkDescriptorPool -- ^ descriptorPool
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

-- | > () vkDestroyDescriptorPool
--   >     ( VkDevice device
--   >     , VkDescriptorPool descriptorPool
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyDescriptorPool.html vkDestroyDescriptorPool registry at www.khronos.org>
type HS_vkDestroyDescriptorPool =
     VkDevice -- ^ device
              -> VkDescriptorPool -- ^ descriptorPool
                                  -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                               -> IO ()

type PFN_vkDestroyDescriptorPool =
     FunPtr HS_vkDestroyDescriptorPool

foreign import ccall "dynamic" unwrapVkDestroyDescriptorPool ::
               PFN_vkDestroyDescriptorPool -> HS_vkDestroyDescriptorPool

instance VulkanProc "vkDestroyDescriptorPool" where
        type VkProcType "vkDestroyDescriptorPool" =
             HS_vkDestroyDescriptorPool
        vkProcSymbol = _VkDestroyDescriptorPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyDescriptorPool

        {-# INLINE unwrapVkProcPtr #-}

pattern VkResetDescriptorPool :: CString

pattern VkResetDescriptorPool <- (is_VkResetDescriptorPool -> True)
  where VkResetDescriptorPool = _VkResetDescriptorPool

{-# INLINE _VkResetDescriptorPool #-}

_VkResetDescriptorPool :: CString
_VkResetDescriptorPool = Ptr "vkResetDescriptorPool\NUL"##

{-# INLINE is_VkResetDescriptorPool #-}

is_VkResetDescriptorPool :: CString -> Bool
is_VkResetDescriptorPool
  = (EQ ==) . cmpCStrings _VkResetDescriptorPool

type VkResetDescriptorPool = "vkResetDescriptorPool"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetDescriptorPool
--   >     ( VkDevice device
--   >     , VkDescriptorPool descriptorPool
--   >     , VkDescriptorPoolResetFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkResetDescriptorPool.html vkResetDescriptorPool registry at www.khronos.org>
foreign import ccall unsafe "vkResetDescriptorPool"
               vkResetDescriptorPool ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorPool -- ^ descriptorPool
                                  -> VkDescriptorPoolResetFlags -- ^ flags
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetDescriptorPool
--   >     ( VkDevice device
--   >     , VkDescriptorPool descriptorPool
--   >     , VkDescriptorPoolResetFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkResetDescriptorPool.html vkResetDescriptorPool registry at www.khronos.org>
foreign import ccall safe "vkResetDescriptorPool"
               vkResetDescriptorPoolSafe ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorPool -- ^ descriptorPool
                                  -> VkDescriptorPoolResetFlags -- ^ flags
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetDescriptorPool
--   >     ( VkDevice device
--   >     , VkDescriptorPool descriptorPool
--   >     , VkDescriptorPoolResetFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkResetDescriptorPool.html vkResetDescriptorPool registry at www.khronos.org>
type HS_vkResetDescriptorPool =
     VkDevice -- ^ device
              ->
       VkDescriptorPool -- ^ descriptorPool
                        -> VkDescriptorPoolResetFlags -- ^ flags
                                                      -> IO VkResult

type PFN_vkResetDescriptorPool = FunPtr HS_vkResetDescriptorPool

foreign import ccall "dynamic" unwrapVkResetDescriptorPool ::
               PFN_vkResetDescriptorPool -> HS_vkResetDescriptorPool

instance VulkanProc "vkResetDescriptorPool" where
        type VkProcType "vkResetDescriptorPool" = HS_vkResetDescriptorPool
        vkProcSymbol = _VkResetDescriptorPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkResetDescriptorPool

        {-# INLINE unwrapVkProcPtr #-}

pattern VkAllocateDescriptorSets :: CString

pattern VkAllocateDescriptorSets <-
        (is_VkAllocateDescriptorSets -> True)
  where VkAllocateDescriptorSets = _VkAllocateDescriptorSets

{-# INLINE _VkAllocateDescriptorSets #-}

_VkAllocateDescriptorSets :: CString
_VkAllocateDescriptorSets = Ptr "vkAllocateDescriptorSets\NUL"##

{-# INLINE is_VkAllocateDescriptorSets #-}

is_VkAllocateDescriptorSets :: CString -> Bool
is_VkAllocateDescriptorSets
  = (EQ ==) . cmpCStrings _VkAllocateDescriptorSets

type VkAllocateDescriptorSets = "vkAllocateDescriptorSets"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FRAGMENTED_POOL', 'VK_ERROR_OUT_OF_POOL_MEMORY'.
--
--   > VkResult vkAllocateDescriptorSets
--   >     ( VkDevice device
--   >     , const VkDescriptorSetAllocateInfo* pAllocateInfo
--   >     , VkDescriptorSet* pDescriptorSets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkAllocateDescriptorSets.html vkAllocateDescriptorSets registry at www.khronos.org>
foreign import ccall unsafe "vkAllocateDescriptorSets"
               vkAllocateDescriptorSets ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorSetAllocateInfo -- ^ pAllocateInfo
                                                 ->
                   Ptr VkDescriptorSet -- ^ pDescriptorSets
                                       -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FRAGMENTED_POOL', 'VK_ERROR_OUT_OF_POOL_MEMORY'.
--
--   > VkResult vkAllocateDescriptorSets
--   >     ( VkDevice device
--   >     , const VkDescriptorSetAllocateInfo* pAllocateInfo
--   >     , VkDescriptorSet* pDescriptorSets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkAllocateDescriptorSets.html vkAllocateDescriptorSets registry at www.khronos.org>
foreign import ccall safe "vkAllocateDescriptorSets"
               vkAllocateDescriptorSetsSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorSetAllocateInfo -- ^ pAllocateInfo
                                                 ->
                   Ptr VkDescriptorSet -- ^ pDescriptorSets
                                       -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FRAGMENTED_POOL', 'VK_ERROR_OUT_OF_POOL_MEMORY'.
--
--   > VkResult vkAllocateDescriptorSets
--   >     ( VkDevice device
--   >     , const VkDescriptorSetAllocateInfo* pAllocateInfo
--   >     , VkDescriptorSet* pDescriptorSets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkAllocateDescriptorSets.html vkAllocateDescriptorSets registry at www.khronos.org>
type HS_vkAllocateDescriptorSets =
     VkDevice -- ^ device
              ->
       Ptr VkDescriptorSetAllocateInfo -- ^ pAllocateInfo
                                       ->
         Ptr VkDescriptorSet -- ^ pDescriptorSets
                             -> IO VkResult

type PFN_vkAllocateDescriptorSets =
     FunPtr HS_vkAllocateDescriptorSets

foreign import ccall "dynamic" unwrapVkAllocateDescriptorSets ::
               PFN_vkAllocateDescriptorSets -> HS_vkAllocateDescriptorSets

instance VulkanProc "vkAllocateDescriptorSets" where
        type VkProcType "vkAllocateDescriptorSets" =
             HS_vkAllocateDescriptorSets
        vkProcSymbol = _VkAllocateDescriptorSets

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkAllocateDescriptorSets

        {-# INLINE unwrapVkProcPtr #-}

pattern VkFreeDescriptorSets :: CString

pattern VkFreeDescriptorSets <- (is_VkFreeDescriptorSets -> True)
  where VkFreeDescriptorSets = _VkFreeDescriptorSets

{-# INLINE _VkFreeDescriptorSets #-}

_VkFreeDescriptorSets :: CString
_VkFreeDescriptorSets = Ptr "vkFreeDescriptorSets\NUL"##

{-# INLINE is_VkFreeDescriptorSets #-}

is_VkFreeDescriptorSets :: CString -> Bool
is_VkFreeDescriptorSets
  = (EQ ==) . cmpCStrings _VkFreeDescriptorSets

type VkFreeDescriptorSets = "vkFreeDescriptorSets"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkFreeDescriptorSets
--   >     ( VkDevice device
--   >     , VkDescriptorPool descriptorPool
--   >     , uint32_t descriptorSetCount
--   >     , const VkDescriptorSet* pDescriptorSets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkFreeDescriptorSets.html vkFreeDescriptorSets registry at www.khronos.org>
foreign import ccall unsafe "vkFreeDescriptorSets"
               vkFreeDescriptorSets ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorPool -- ^ descriptorPool
                                  -> Word32 -- ^ descriptorSetCount
                                            -> Ptr VkDescriptorSet -- ^ pDescriptorSets
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkFreeDescriptorSets
--   >     ( VkDevice device
--   >     , VkDescriptorPool descriptorPool
--   >     , uint32_t descriptorSetCount
--   >     , const VkDescriptorSet* pDescriptorSets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkFreeDescriptorSets.html vkFreeDescriptorSets registry at www.khronos.org>
foreign import ccall safe "vkFreeDescriptorSets"
               vkFreeDescriptorSetsSafe ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorPool -- ^ descriptorPool
                                  -> Word32 -- ^ descriptorSetCount
                                            -> Ptr VkDescriptorSet -- ^ pDescriptorSets
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkFreeDescriptorSets
--   >     ( VkDevice device
--   >     , VkDescriptorPool descriptorPool
--   >     , uint32_t descriptorSetCount
--   >     , const VkDescriptorSet* pDescriptorSets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkFreeDescriptorSets.html vkFreeDescriptorSets registry at www.khronos.org>
type HS_vkFreeDescriptorSets =
     VkDevice -- ^ device
              ->
       VkDescriptorPool -- ^ descriptorPool
                        -> Word32 -- ^ descriptorSetCount
                                  -> Ptr VkDescriptorSet -- ^ pDescriptorSets
                                                         -> IO VkResult

type PFN_vkFreeDescriptorSets = FunPtr HS_vkFreeDescriptorSets

foreign import ccall "dynamic" unwrapVkFreeDescriptorSets ::
               PFN_vkFreeDescriptorSets -> HS_vkFreeDescriptorSets

instance VulkanProc "vkFreeDescriptorSets" where
        type VkProcType "vkFreeDescriptorSets" = HS_vkFreeDescriptorSets
        vkProcSymbol = _VkFreeDescriptorSets

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkFreeDescriptorSets

        {-# INLINE unwrapVkProcPtr #-}

pattern VkUpdateDescriptorSets :: CString

pattern VkUpdateDescriptorSets <-
        (is_VkUpdateDescriptorSets -> True)
  where VkUpdateDescriptorSets = _VkUpdateDescriptorSets

{-# INLINE _VkUpdateDescriptorSets #-}

_VkUpdateDescriptorSets :: CString
_VkUpdateDescriptorSets = Ptr "vkUpdateDescriptorSets\NUL"##

{-# INLINE is_VkUpdateDescriptorSets #-}

is_VkUpdateDescriptorSets :: CString -> Bool
is_VkUpdateDescriptorSets
  = (EQ ==) . cmpCStrings _VkUpdateDescriptorSets

type VkUpdateDescriptorSets = "vkUpdateDescriptorSets"

-- | > () vkUpdateDescriptorSets
--   >     ( VkDevice device
--   >     , uint32_t descriptorWriteCount
--   >     , const VkWriteDescriptorSet* pDescriptorWrites
--   >     , uint32_t descriptorCopyCount
--   >     , const VkCopyDescriptorSet* pDescriptorCopies
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkUpdateDescriptorSets.html vkUpdateDescriptorSets registry at www.khronos.org>
foreign import ccall unsafe "vkUpdateDescriptorSets"
               vkUpdateDescriptorSets ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ descriptorWriteCount
                        ->
                   Ptr VkWriteDescriptorSet -- ^ pDescriptorWrites
                                            ->
                     Word32 -- ^ descriptorCopyCount
                            -> Ptr VkCopyDescriptorSet -- ^ pDescriptorCopies
                                                       -> IO ()

-- | > () vkUpdateDescriptorSets
--   >     ( VkDevice device
--   >     , uint32_t descriptorWriteCount
--   >     , const VkWriteDescriptorSet* pDescriptorWrites
--   >     , uint32_t descriptorCopyCount
--   >     , const VkCopyDescriptorSet* pDescriptorCopies
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkUpdateDescriptorSets.html vkUpdateDescriptorSets registry at www.khronos.org>
foreign import ccall safe "vkUpdateDescriptorSets"
               vkUpdateDescriptorSetsSafe ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ descriptorWriteCount
                        ->
                   Ptr VkWriteDescriptorSet -- ^ pDescriptorWrites
                                            ->
                     Word32 -- ^ descriptorCopyCount
                            -> Ptr VkCopyDescriptorSet -- ^ pDescriptorCopies
                                                       -> IO ()

-- | > () vkUpdateDescriptorSets
--   >     ( VkDevice device
--   >     , uint32_t descriptorWriteCount
--   >     , const VkWriteDescriptorSet* pDescriptorWrites
--   >     , uint32_t descriptorCopyCount
--   >     , const VkCopyDescriptorSet* pDescriptorCopies
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkUpdateDescriptorSets.html vkUpdateDescriptorSets registry at www.khronos.org>
type HS_vkUpdateDescriptorSets =
     VkDevice -- ^ device
              ->
       Word32 -- ^ descriptorWriteCount
              ->
         Ptr VkWriteDescriptorSet -- ^ pDescriptorWrites
                                  ->
           Word32 -- ^ descriptorCopyCount
                  -> Ptr VkCopyDescriptorSet -- ^ pDescriptorCopies
                                             -> IO ()

type PFN_vkUpdateDescriptorSets = FunPtr HS_vkUpdateDescriptorSets

foreign import ccall "dynamic" unwrapVkUpdateDescriptorSets ::
               PFN_vkUpdateDescriptorSets -> HS_vkUpdateDescriptorSets

instance VulkanProc "vkUpdateDescriptorSets" where
        type VkProcType "vkUpdateDescriptorSets" =
             HS_vkUpdateDescriptorSets
        vkProcSymbol = _VkUpdateDescriptorSets

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkUpdateDescriptorSets

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateFramebuffer :: CString

pattern VkCreateFramebuffer <- (is_VkCreateFramebuffer -> True)
  where VkCreateFramebuffer = _VkCreateFramebuffer

{-# INLINE _VkCreateFramebuffer #-}

_VkCreateFramebuffer :: CString
_VkCreateFramebuffer = Ptr "vkCreateFramebuffer\NUL"##

{-# INLINE is_VkCreateFramebuffer #-}

is_VkCreateFramebuffer :: CString -> Bool
is_VkCreateFramebuffer = (EQ ==) . cmpCStrings _VkCreateFramebuffer

type VkCreateFramebuffer = "vkCreateFramebuffer"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateFramebuffer
--   >     ( VkDevice device
--   >     , const VkFramebufferCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFramebuffer* pFramebuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateFramebuffer.html vkCreateFramebuffer registry at www.khronos.org>
foreign import ccall unsafe "vkCreateFramebuffer"
               vkCreateFramebuffer ::
               VkDevice -- ^ device
                        ->
                 Ptr VkFramebufferCreateInfo -- ^ pCreateInfo
                                             ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkFramebuffer -- ^ pFramebuffer
                                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateFramebuffer
--   >     ( VkDevice device
--   >     , const VkFramebufferCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFramebuffer* pFramebuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateFramebuffer.html vkCreateFramebuffer registry at www.khronos.org>
foreign import ccall safe "vkCreateFramebuffer"
               vkCreateFramebufferSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkFramebufferCreateInfo -- ^ pCreateInfo
                                             ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkFramebuffer -- ^ pFramebuffer
                                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateFramebuffer
--   >     ( VkDevice device
--   >     , const VkFramebufferCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFramebuffer* pFramebuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateFramebuffer.html vkCreateFramebuffer registry at www.khronos.org>
type HS_vkCreateFramebuffer =
     VkDevice -- ^ device
              ->
       Ptr VkFramebufferCreateInfo -- ^ pCreateInfo
                                   ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkFramebuffer -- ^ pFramebuffer
                                                        -> IO VkResult

type PFN_vkCreateFramebuffer = FunPtr HS_vkCreateFramebuffer

foreign import ccall "dynamic" unwrapVkCreateFramebuffer ::
               PFN_vkCreateFramebuffer -> HS_vkCreateFramebuffer

instance VulkanProc "vkCreateFramebuffer" where
        type VkProcType "vkCreateFramebuffer" = HS_vkCreateFramebuffer
        vkProcSymbol = _VkCreateFramebuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateFramebuffer

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyFramebuffer :: CString

pattern VkDestroyFramebuffer <- (is_VkDestroyFramebuffer -> True)
  where VkDestroyFramebuffer = _VkDestroyFramebuffer

{-# INLINE _VkDestroyFramebuffer #-}

_VkDestroyFramebuffer :: CString
_VkDestroyFramebuffer = Ptr "vkDestroyFramebuffer\NUL"##

{-# INLINE is_VkDestroyFramebuffer #-}

is_VkDestroyFramebuffer :: CString -> Bool
is_VkDestroyFramebuffer
  = (EQ ==) . cmpCStrings _VkDestroyFramebuffer

type VkDestroyFramebuffer = "vkDestroyFramebuffer"

-- | > () vkDestroyFramebuffer
--   >     ( VkDevice device
--   >     , VkFramebuffer framebuffer
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyFramebuffer.html vkDestroyFramebuffer registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyFramebuffer"
               vkDestroyFramebuffer ::
               VkDevice -- ^ device
                        -> VkFramebuffer -- ^ framebuffer
                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()

-- | > () vkDestroyFramebuffer
--   >     ( VkDevice device
--   >     , VkFramebuffer framebuffer
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyFramebuffer.html vkDestroyFramebuffer registry at www.khronos.org>
foreign import ccall safe "vkDestroyFramebuffer"
               vkDestroyFramebufferSafe ::
               VkDevice -- ^ device
                        -> VkFramebuffer -- ^ framebuffer
                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()

-- | > () vkDestroyFramebuffer
--   >     ( VkDevice device
--   >     , VkFramebuffer framebuffer
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyFramebuffer.html vkDestroyFramebuffer registry at www.khronos.org>
type HS_vkDestroyFramebuffer =
     VkDevice -- ^ device
              -> VkFramebuffer -- ^ framebuffer
                               -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                            -> IO ()

type PFN_vkDestroyFramebuffer = FunPtr HS_vkDestroyFramebuffer

foreign import ccall "dynamic" unwrapVkDestroyFramebuffer ::
               PFN_vkDestroyFramebuffer -> HS_vkDestroyFramebuffer

instance VulkanProc "vkDestroyFramebuffer" where
        type VkProcType "vkDestroyFramebuffer" = HS_vkDestroyFramebuffer
        vkProcSymbol = _VkDestroyFramebuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyFramebuffer

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateRenderPass :: CString

pattern VkCreateRenderPass <- (is_VkCreateRenderPass -> True)
  where VkCreateRenderPass = _VkCreateRenderPass

{-# INLINE _VkCreateRenderPass #-}

_VkCreateRenderPass :: CString
_VkCreateRenderPass = Ptr "vkCreateRenderPass\NUL"##

{-# INLINE is_VkCreateRenderPass #-}

is_VkCreateRenderPass :: CString -> Bool
is_VkCreateRenderPass = (EQ ==) . cmpCStrings _VkCreateRenderPass

type VkCreateRenderPass = "vkCreateRenderPass"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateRenderPass
--   >     ( VkDevice device
--   >     , const VkRenderPassCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkRenderPass* pRenderPass
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateRenderPass.html vkCreateRenderPass registry at www.khronos.org>
foreign import ccall unsafe "vkCreateRenderPass" vkCreateRenderPass
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkRenderPassCreateInfo -- ^ pCreateInfo
                                            ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkRenderPass -- ^ pRenderPass
                                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateRenderPass
--   >     ( VkDevice device
--   >     , const VkRenderPassCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkRenderPass* pRenderPass
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateRenderPass.html vkCreateRenderPass registry at www.khronos.org>
foreign import ccall safe "vkCreateRenderPass"
               vkCreateRenderPassSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkRenderPassCreateInfo -- ^ pCreateInfo
                                            ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkRenderPass -- ^ pRenderPass
                                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateRenderPass
--   >     ( VkDevice device
--   >     , const VkRenderPassCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkRenderPass* pRenderPass
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateRenderPass.html vkCreateRenderPass registry at www.khronos.org>
type HS_vkCreateRenderPass =
     VkDevice -- ^ device
              ->
       Ptr VkRenderPassCreateInfo -- ^ pCreateInfo
                                  ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkRenderPass -- ^ pRenderPass
                                                       -> IO VkResult

type PFN_vkCreateRenderPass = FunPtr HS_vkCreateRenderPass

foreign import ccall "dynamic" unwrapVkCreateRenderPass ::
               PFN_vkCreateRenderPass -> HS_vkCreateRenderPass

instance VulkanProc "vkCreateRenderPass" where
        type VkProcType "vkCreateRenderPass" = HS_vkCreateRenderPass
        vkProcSymbol = _VkCreateRenderPass

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateRenderPass

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyRenderPass :: CString

pattern VkDestroyRenderPass <- (is_VkDestroyRenderPass -> True)
  where VkDestroyRenderPass = _VkDestroyRenderPass

{-# INLINE _VkDestroyRenderPass #-}

_VkDestroyRenderPass :: CString
_VkDestroyRenderPass = Ptr "vkDestroyRenderPass\NUL"##

{-# INLINE is_VkDestroyRenderPass #-}

is_VkDestroyRenderPass :: CString -> Bool
is_VkDestroyRenderPass = (EQ ==) . cmpCStrings _VkDestroyRenderPass

type VkDestroyRenderPass = "vkDestroyRenderPass"

-- | > () vkDestroyRenderPass
--   >     ( VkDevice device
--   >     , VkRenderPass renderPass
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyRenderPass.html vkDestroyRenderPass registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyRenderPass"
               vkDestroyRenderPass ::
               VkDevice -- ^ device
                        -> VkRenderPass -- ^ renderPass
                                        -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                     -> IO ()

-- | > () vkDestroyRenderPass
--   >     ( VkDevice device
--   >     , VkRenderPass renderPass
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyRenderPass.html vkDestroyRenderPass registry at www.khronos.org>
foreign import ccall safe "vkDestroyRenderPass"
               vkDestroyRenderPassSafe ::
               VkDevice -- ^ device
                        -> VkRenderPass -- ^ renderPass
                                        -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                     -> IO ()

-- | > () vkDestroyRenderPass
--   >     ( VkDevice device
--   >     , VkRenderPass renderPass
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyRenderPass.html vkDestroyRenderPass registry at www.khronos.org>
type HS_vkDestroyRenderPass =
     VkDevice -- ^ device
              -> VkRenderPass -- ^ renderPass
                              -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                           -> IO ()

type PFN_vkDestroyRenderPass = FunPtr HS_vkDestroyRenderPass

foreign import ccall "dynamic" unwrapVkDestroyRenderPass ::
               PFN_vkDestroyRenderPass -> HS_vkDestroyRenderPass

instance VulkanProc "vkDestroyRenderPass" where
        type VkProcType "vkDestroyRenderPass" = HS_vkDestroyRenderPass
        vkProcSymbol = _VkDestroyRenderPass

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyRenderPass

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetRenderAreaGranularity :: CString

pattern VkGetRenderAreaGranularity <-
        (is_VkGetRenderAreaGranularity -> True)
  where VkGetRenderAreaGranularity = _VkGetRenderAreaGranularity

{-# INLINE _VkGetRenderAreaGranularity #-}

_VkGetRenderAreaGranularity :: CString
_VkGetRenderAreaGranularity = Ptr "vkGetRenderAreaGranularity\NUL"##

{-# INLINE is_VkGetRenderAreaGranularity #-}

is_VkGetRenderAreaGranularity :: CString -> Bool
is_VkGetRenderAreaGranularity
  = (EQ ==) . cmpCStrings _VkGetRenderAreaGranularity

type VkGetRenderAreaGranularity = "vkGetRenderAreaGranularity"

-- | > () vkGetRenderAreaGranularity
--   >     ( VkDevice device
--   >     , VkRenderPass renderPass
--   >     , VkExtent2D* pGranularity
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetRenderAreaGranularity.html vkGetRenderAreaGranularity registry at www.khronos.org>
foreign import ccall unsafe "vkGetRenderAreaGranularity"
               vkGetRenderAreaGranularity ::
               VkDevice -- ^ device
                        -> VkRenderPass -- ^ renderPass
                                        -> Ptr VkExtent2D -- ^ pGranularity
                                                          -> IO ()

-- | > () vkGetRenderAreaGranularity
--   >     ( VkDevice device
--   >     , VkRenderPass renderPass
--   >     , VkExtent2D* pGranularity
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetRenderAreaGranularity.html vkGetRenderAreaGranularity registry at www.khronos.org>
foreign import ccall safe "vkGetRenderAreaGranularity"
               vkGetRenderAreaGranularitySafe ::
               VkDevice -- ^ device
                        -> VkRenderPass -- ^ renderPass
                                        -> Ptr VkExtent2D -- ^ pGranularity
                                                          -> IO ()

-- | > () vkGetRenderAreaGranularity
--   >     ( VkDevice device
--   >     , VkRenderPass renderPass
--   >     , VkExtent2D* pGranularity
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetRenderAreaGranularity.html vkGetRenderAreaGranularity registry at www.khronos.org>
type HS_vkGetRenderAreaGranularity =
     VkDevice -- ^ device
              -> VkRenderPass -- ^ renderPass
                              -> Ptr VkExtent2D -- ^ pGranularity
                                                -> IO ()

type PFN_vkGetRenderAreaGranularity =
     FunPtr HS_vkGetRenderAreaGranularity

foreign import ccall "dynamic" unwrapVkGetRenderAreaGranularity ::
               PFN_vkGetRenderAreaGranularity -> HS_vkGetRenderAreaGranularity

instance VulkanProc "vkGetRenderAreaGranularity" where
        type VkProcType "vkGetRenderAreaGranularity" =
             HS_vkGetRenderAreaGranularity
        vkProcSymbol = _VkGetRenderAreaGranularity

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetRenderAreaGranularity

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateCommandPool :: CString

pattern VkCreateCommandPool <- (is_VkCreateCommandPool -> True)
  where VkCreateCommandPool = _VkCreateCommandPool

{-# INLINE _VkCreateCommandPool #-}

_VkCreateCommandPool :: CString
_VkCreateCommandPool = Ptr "vkCreateCommandPool\NUL"##

{-# INLINE is_VkCreateCommandPool #-}

is_VkCreateCommandPool :: CString -> Bool
is_VkCreateCommandPool = (EQ ==) . cmpCStrings _VkCreateCommandPool

type VkCreateCommandPool = "vkCreateCommandPool"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateCommandPool
--   >     ( VkDevice device
--   >     , const VkCommandPoolCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkCommandPool* pCommandPool
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateCommandPool.html vkCreateCommandPool registry at www.khronos.org>
foreign import ccall unsafe "vkCreateCommandPool"
               vkCreateCommandPool ::
               VkDevice -- ^ device
                        ->
                 Ptr VkCommandPoolCreateInfo -- ^ pCreateInfo
                                             ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkCommandPool -- ^ pCommandPool
                                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateCommandPool
--   >     ( VkDevice device
--   >     , const VkCommandPoolCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkCommandPool* pCommandPool
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateCommandPool.html vkCreateCommandPool registry at www.khronos.org>
foreign import ccall safe "vkCreateCommandPool"
               vkCreateCommandPoolSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkCommandPoolCreateInfo -- ^ pCreateInfo
                                             ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkCommandPool -- ^ pCommandPool
                                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateCommandPool
--   >     ( VkDevice device
--   >     , const VkCommandPoolCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkCommandPool* pCommandPool
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateCommandPool.html vkCreateCommandPool registry at www.khronos.org>
type HS_vkCreateCommandPool =
     VkDevice -- ^ device
              ->
       Ptr VkCommandPoolCreateInfo -- ^ pCreateInfo
                                   ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkCommandPool -- ^ pCommandPool
                                                        -> IO VkResult

type PFN_vkCreateCommandPool = FunPtr HS_vkCreateCommandPool

foreign import ccall "dynamic" unwrapVkCreateCommandPool ::
               PFN_vkCreateCommandPool -> HS_vkCreateCommandPool

instance VulkanProc "vkCreateCommandPool" where
        type VkProcType "vkCreateCommandPool" = HS_vkCreateCommandPool
        vkProcSymbol = _VkCreateCommandPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateCommandPool

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroyCommandPool :: CString

pattern VkDestroyCommandPool <- (is_VkDestroyCommandPool -> True)
  where VkDestroyCommandPool = _VkDestroyCommandPool

{-# INLINE _VkDestroyCommandPool #-}

_VkDestroyCommandPool :: CString
_VkDestroyCommandPool = Ptr "vkDestroyCommandPool\NUL"##

{-# INLINE is_VkDestroyCommandPool #-}

is_VkDestroyCommandPool :: CString -> Bool
is_VkDestroyCommandPool
  = (EQ ==) . cmpCStrings _VkDestroyCommandPool

type VkDestroyCommandPool = "vkDestroyCommandPool"

-- | > () vkDestroyCommandPool
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyCommandPool.html vkDestroyCommandPool registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyCommandPool"
               vkDestroyCommandPool ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()

-- | > () vkDestroyCommandPool
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyCommandPool.html vkDestroyCommandPool registry at www.khronos.org>
foreign import ccall safe "vkDestroyCommandPool"
               vkDestroyCommandPoolSafe ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()

-- | > () vkDestroyCommandPool
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroyCommandPool.html vkDestroyCommandPool registry at www.khronos.org>
type HS_vkDestroyCommandPool =
     VkDevice -- ^ device
              -> VkCommandPool -- ^ commandPool
                               -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                            -> IO ()

type PFN_vkDestroyCommandPool = FunPtr HS_vkDestroyCommandPool

foreign import ccall "dynamic" unwrapVkDestroyCommandPool ::
               PFN_vkDestroyCommandPool -> HS_vkDestroyCommandPool

instance VulkanProc "vkDestroyCommandPool" where
        type VkProcType "vkDestroyCommandPool" = HS_vkDestroyCommandPool
        vkProcSymbol = _VkDestroyCommandPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyCommandPool

        {-# INLINE unwrapVkProcPtr #-}

pattern VkResetCommandPool :: CString

pattern VkResetCommandPool <- (is_VkResetCommandPool -> True)
  where VkResetCommandPool = _VkResetCommandPool

{-# INLINE _VkResetCommandPool #-}

_VkResetCommandPool :: CString
_VkResetCommandPool = Ptr "vkResetCommandPool\NUL"##

{-# INLINE is_VkResetCommandPool #-}

is_VkResetCommandPool :: CString -> Bool
is_VkResetCommandPool = (EQ ==) . cmpCStrings _VkResetCommandPool

type VkResetCommandPool = "vkResetCommandPool"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetCommandPool
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , VkCommandPoolResetFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkResetCommandPool.html vkResetCommandPool registry at www.khronos.org>
foreign import ccall unsafe "vkResetCommandPool" vkResetCommandPool
               ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> VkCommandPoolResetFlags -- ^ flags
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetCommandPool
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , VkCommandPoolResetFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkResetCommandPool.html vkResetCommandPool registry at www.khronos.org>
foreign import ccall safe "vkResetCommandPool"
               vkResetCommandPoolSafe ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> VkCommandPoolResetFlags -- ^ flags
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetCommandPool
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , VkCommandPoolResetFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkResetCommandPool.html vkResetCommandPool registry at www.khronos.org>
type HS_vkResetCommandPool =
     VkDevice -- ^ device
              -> VkCommandPool -- ^ commandPool
                               -> VkCommandPoolResetFlags -- ^ flags
                                                          -> IO VkResult

type PFN_vkResetCommandPool = FunPtr HS_vkResetCommandPool

foreign import ccall "dynamic" unwrapVkResetCommandPool ::
               PFN_vkResetCommandPool -> HS_vkResetCommandPool

instance VulkanProc "vkResetCommandPool" where
        type VkProcType "vkResetCommandPool" = HS_vkResetCommandPool
        vkProcSymbol = _VkResetCommandPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkResetCommandPool

        {-# INLINE unwrapVkProcPtr #-}

pattern VkAllocateCommandBuffers :: CString

pattern VkAllocateCommandBuffers <-
        (is_VkAllocateCommandBuffers -> True)
  where VkAllocateCommandBuffers = _VkAllocateCommandBuffers

{-# INLINE _VkAllocateCommandBuffers #-}

_VkAllocateCommandBuffers :: CString
_VkAllocateCommandBuffers = Ptr "vkAllocateCommandBuffers\NUL"##

{-# INLINE is_VkAllocateCommandBuffers #-}

is_VkAllocateCommandBuffers :: CString -> Bool
is_VkAllocateCommandBuffers
  = (EQ ==) . cmpCStrings _VkAllocateCommandBuffers

type VkAllocateCommandBuffers = "vkAllocateCommandBuffers"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkAllocateCommandBuffers
--   >     ( VkDevice device
--   >     , const VkCommandBufferAllocateInfo* pAllocateInfo
--   >     , VkCommandBuffer* pCommandBuffers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkAllocateCommandBuffers.html vkAllocateCommandBuffers registry at www.khronos.org>
foreign import ccall unsafe "vkAllocateCommandBuffers"
               vkAllocateCommandBuffers ::
               VkDevice -- ^ device
                        ->
                 Ptr VkCommandBufferAllocateInfo -- ^ pAllocateInfo
                                                 ->
                   Ptr VkCommandBuffer -- ^ pCommandBuffers
                                       -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkAllocateCommandBuffers
--   >     ( VkDevice device
--   >     , const VkCommandBufferAllocateInfo* pAllocateInfo
--   >     , VkCommandBuffer* pCommandBuffers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkAllocateCommandBuffers.html vkAllocateCommandBuffers registry at www.khronos.org>
foreign import ccall safe "vkAllocateCommandBuffers"
               vkAllocateCommandBuffersSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkCommandBufferAllocateInfo -- ^ pAllocateInfo
                                                 ->
                   Ptr VkCommandBuffer -- ^ pCommandBuffers
                                       -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkAllocateCommandBuffers
--   >     ( VkDevice device
--   >     , const VkCommandBufferAllocateInfo* pAllocateInfo
--   >     , VkCommandBuffer* pCommandBuffers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkAllocateCommandBuffers.html vkAllocateCommandBuffers registry at www.khronos.org>
type HS_vkAllocateCommandBuffers =
     VkDevice -- ^ device
              ->
       Ptr VkCommandBufferAllocateInfo -- ^ pAllocateInfo
                                       ->
         Ptr VkCommandBuffer -- ^ pCommandBuffers
                             -> IO VkResult

type PFN_vkAllocateCommandBuffers =
     FunPtr HS_vkAllocateCommandBuffers

foreign import ccall "dynamic" unwrapVkAllocateCommandBuffers ::
               PFN_vkAllocateCommandBuffers -> HS_vkAllocateCommandBuffers

instance VulkanProc "vkAllocateCommandBuffers" where
        type VkProcType "vkAllocateCommandBuffers" =
             HS_vkAllocateCommandBuffers
        vkProcSymbol = _VkAllocateCommandBuffers

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkAllocateCommandBuffers

        {-# INLINE unwrapVkProcPtr #-}

pattern VkFreeCommandBuffers :: CString

pattern VkFreeCommandBuffers <- (is_VkFreeCommandBuffers -> True)
  where VkFreeCommandBuffers = _VkFreeCommandBuffers

{-# INLINE _VkFreeCommandBuffers #-}

_VkFreeCommandBuffers :: CString
_VkFreeCommandBuffers = Ptr "vkFreeCommandBuffers\NUL"##

{-# INLINE is_VkFreeCommandBuffers #-}

is_VkFreeCommandBuffers :: CString -> Bool
is_VkFreeCommandBuffers
  = (EQ ==) . cmpCStrings _VkFreeCommandBuffers

type VkFreeCommandBuffers = "vkFreeCommandBuffers"

-- | > () vkFreeCommandBuffers
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , uint32_t commandBufferCount
--   >     , const VkCommandBuffer* pCommandBuffers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkFreeCommandBuffers.html vkFreeCommandBuffers registry at www.khronos.org>
foreign import ccall unsafe "vkFreeCommandBuffers"
               vkFreeCommandBuffers ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> Word32 -- ^ commandBufferCount
                                                   -> Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                                          -> IO ()

-- | > () vkFreeCommandBuffers
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , uint32_t commandBufferCount
--   >     , const VkCommandBuffer* pCommandBuffers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkFreeCommandBuffers.html vkFreeCommandBuffers registry at www.khronos.org>
foreign import ccall safe "vkFreeCommandBuffers"
               vkFreeCommandBuffersSafe ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> Word32 -- ^ commandBufferCount
                                                   -> Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                                          -> IO ()

-- | > () vkFreeCommandBuffers
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , uint32_t commandBufferCount
--   >     , const VkCommandBuffer* pCommandBuffers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkFreeCommandBuffers.html vkFreeCommandBuffers registry at www.khronos.org>
type HS_vkFreeCommandBuffers =
     VkDevice -- ^ device
              -> VkCommandPool -- ^ commandPool
                               -> Word32 -- ^ commandBufferCount
                                         -> Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                                -> IO ()

type PFN_vkFreeCommandBuffers = FunPtr HS_vkFreeCommandBuffers

foreign import ccall "dynamic" unwrapVkFreeCommandBuffers ::
               PFN_vkFreeCommandBuffers -> HS_vkFreeCommandBuffers

instance VulkanProc "vkFreeCommandBuffers" where
        type VkProcType "vkFreeCommandBuffers" = HS_vkFreeCommandBuffers
        vkProcSymbol = _VkFreeCommandBuffers

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkFreeCommandBuffers

        {-# INLINE unwrapVkProcPtr #-}

pattern VkBeginCommandBuffer :: CString

pattern VkBeginCommandBuffer <- (is_VkBeginCommandBuffer -> True)
  where VkBeginCommandBuffer = _VkBeginCommandBuffer

{-# INLINE _VkBeginCommandBuffer #-}

_VkBeginCommandBuffer :: CString
_VkBeginCommandBuffer = Ptr "vkBeginCommandBuffer\NUL"##

{-# INLINE is_VkBeginCommandBuffer #-}

is_VkBeginCommandBuffer :: CString -> Bool
is_VkBeginCommandBuffer
  = (EQ ==) . cmpCStrings _VkBeginCommandBuffer

type VkBeginCommandBuffer = "vkBeginCommandBuffer"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBeginCommandBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCommandBufferBeginInfo* pBeginInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkBeginCommandBuffer.html vkBeginCommandBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkBeginCommandBuffer"
               vkBeginCommandBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkCommandBufferBeginInfo -- ^ pBeginInfo
                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBeginCommandBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCommandBufferBeginInfo* pBeginInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkBeginCommandBuffer.html vkBeginCommandBuffer registry at www.khronos.org>
foreign import ccall safe "vkBeginCommandBuffer"
               vkBeginCommandBufferSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkCommandBufferBeginInfo -- ^ pBeginInfo
                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBeginCommandBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCommandBufferBeginInfo* pBeginInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkBeginCommandBuffer.html vkBeginCommandBuffer registry at www.khronos.org>
type HS_vkBeginCommandBuffer =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr VkCommandBufferBeginInfo -- ^ pBeginInfo
                                                     -> IO VkResult

type PFN_vkBeginCommandBuffer = FunPtr HS_vkBeginCommandBuffer

foreign import ccall "dynamic" unwrapVkBeginCommandBuffer ::
               PFN_vkBeginCommandBuffer -> HS_vkBeginCommandBuffer

instance VulkanProc "vkBeginCommandBuffer" where
        type VkProcType "vkBeginCommandBuffer" = HS_vkBeginCommandBuffer
        vkProcSymbol = _VkBeginCommandBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkBeginCommandBuffer

        {-# INLINE unwrapVkProcPtr #-}

pattern VkEndCommandBuffer :: CString

pattern VkEndCommandBuffer <- (is_VkEndCommandBuffer -> True)
  where VkEndCommandBuffer = _VkEndCommandBuffer

{-# INLINE _VkEndCommandBuffer #-}

_VkEndCommandBuffer :: CString
_VkEndCommandBuffer = Ptr "vkEndCommandBuffer\NUL"##

{-# INLINE is_VkEndCommandBuffer #-}

is_VkEndCommandBuffer :: CString -> Bool
is_VkEndCommandBuffer = (EQ ==) . cmpCStrings _VkEndCommandBuffer

type VkEndCommandBuffer = "vkEndCommandBuffer"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkEndCommandBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEndCommandBuffer.html vkEndCommandBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkEndCommandBuffer" vkEndCommandBuffer
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkEndCommandBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEndCommandBuffer.html vkEndCommandBuffer registry at www.khronos.org>
foreign import ccall safe "vkEndCommandBuffer"
               vkEndCommandBufferSafe :: VkCommandBuffer -- ^ commandBuffer
                                                         -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkEndCommandBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkEndCommandBuffer.html vkEndCommandBuffer registry at www.khronos.org>
type HS_vkEndCommandBuffer = VkCommandBuffer -- ^ commandBuffer
                                             -> IO VkResult

type PFN_vkEndCommandBuffer = FunPtr HS_vkEndCommandBuffer

foreign import ccall "dynamic" unwrapVkEndCommandBuffer ::
               PFN_vkEndCommandBuffer -> HS_vkEndCommandBuffer

instance VulkanProc "vkEndCommandBuffer" where
        type VkProcType "vkEndCommandBuffer" = HS_vkEndCommandBuffer
        vkProcSymbol = _VkEndCommandBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkEndCommandBuffer

        {-# INLINE unwrapVkProcPtr #-}

pattern VkResetCommandBuffer :: CString

pattern VkResetCommandBuffer <- (is_VkResetCommandBuffer -> True)
  where VkResetCommandBuffer = _VkResetCommandBuffer

{-# INLINE _VkResetCommandBuffer #-}

_VkResetCommandBuffer :: CString
_VkResetCommandBuffer = Ptr "vkResetCommandBuffer\NUL"##

{-# INLINE is_VkResetCommandBuffer #-}

is_VkResetCommandBuffer :: CString -> Bool
is_VkResetCommandBuffer
  = (EQ ==) . cmpCStrings _VkResetCommandBuffer

type VkResetCommandBuffer = "vkResetCommandBuffer"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetCommandBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkCommandBufferResetFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkResetCommandBuffer.html vkResetCommandBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkResetCommandBuffer"
               vkResetCommandBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkCommandBufferResetFlags -- ^ flags
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetCommandBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkCommandBufferResetFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkResetCommandBuffer.html vkResetCommandBuffer registry at www.khronos.org>
foreign import ccall safe "vkResetCommandBuffer"
               vkResetCommandBufferSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkCommandBufferResetFlags -- ^ flags
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetCommandBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkCommandBufferResetFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkResetCommandBuffer.html vkResetCommandBuffer registry at www.khronos.org>
type HS_vkResetCommandBuffer =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkCommandBufferResetFlags -- ^ flags
                                                  -> IO VkResult

type PFN_vkResetCommandBuffer = FunPtr HS_vkResetCommandBuffer

foreign import ccall "dynamic" unwrapVkResetCommandBuffer ::
               PFN_vkResetCommandBuffer -> HS_vkResetCommandBuffer

instance VulkanProc "vkResetCommandBuffer" where
        type VkProcType "vkResetCommandBuffer" = HS_vkResetCommandBuffer
        vkProcSymbol = _VkResetCommandBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkResetCommandBuffer

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdBindPipeline :: CString

pattern VkCmdBindPipeline <- (is_VkCmdBindPipeline -> True)
  where VkCmdBindPipeline = _VkCmdBindPipeline

{-# INLINE _VkCmdBindPipeline #-}

_VkCmdBindPipeline :: CString
_VkCmdBindPipeline = Ptr "vkCmdBindPipeline\NUL"##

{-# INLINE is_VkCmdBindPipeline #-}

is_VkCmdBindPipeline :: CString -> Bool
is_VkCmdBindPipeline = (EQ ==) . cmpCStrings _VkCmdBindPipeline

type VkCmdBindPipeline = "vkCmdBindPipeline"

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdBindPipeline
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineBindPoint pipelineBindPoint
--   >     , VkPipeline pipeline
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBindPipeline.html vkCmdBindPipeline registry at www.khronos.org>
foreign import ccall unsafe "vkCmdBindPipeline" vkCmdBindPipeline
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> VkPipelineBindPoint -- ^ pipelineBindPoint
                                                         -> VkPipeline -- ^ pipeline
                                                                       -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdBindPipeline
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineBindPoint pipelineBindPoint
--   >     , VkPipeline pipeline
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBindPipeline.html vkCmdBindPipeline registry at www.khronos.org>
foreign import ccall safe "vkCmdBindPipeline" vkCmdBindPipelineSafe
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> VkPipelineBindPoint -- ^ pipelineBindPoint
                                                         -> VkPipeline -- ^ pipeline
                                                                       -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdBindPipeline
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineBindPoint pipelineBindPoint
--   >     , VkPipeline pipeline
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBindPipeline.html vkCmdBindPipeline registry at www.khronos.org>
type HS_vkCmdBindPipeline =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkPipelineBindPoint -- ^ pipelineBindPoint
                                            -> VkPipeline -- ^ pipeline
                                                          -> IO ()

type PFN_vkCmdBindPipeline = FunPtr HS_vkCmdBindPipeline

foreign import ccall "dynamic" unwrapVkCmdBindPipeline ::
               PFN_vkCmdBindPipeline -> HS_vkCmdBindPipeline

instance VulkanProc "vkCmdBindPipeline" where
        type VkProcType "vkCmdBindPipeline" = HS_vkCmdBindPipeline
        vkProcSymbol = _VkCmdBindPipeline

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdBindPipeline

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdSetViewport :: CString

pattern VkCmdSetViewport <- (is_VkCmdSetViewport -> True)
  where VkCmdSetViewport = _VkCmdSetViewport

{-# INLINE _VkCmdSetViewport #-}

_VkCmdSetViewport :: CString
_VkCmdSetViewport = Ptr "vkCmdSetViewport\NUL"##

{-# INLINE is_VkCmdSetViewport #-}

is_VkCmdSetViewport :: CString -> Bool
is_VkCmdSetViewport = (EQ ==) . cmpCStrings _VkCmdSetViewport

type VkCmdSetViewport = "vkCmdSetViewport"

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetViewport
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstViewport
--   >     , uint32_t viewportCount
--   >     , const VkViewport* pViewports
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetViewport.html vkCmdSetViewport registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetViewport" vkCmdSetViewport ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ firstViewport
                                         -> Word32 -- ^ viewportCount
                                                   -> Ptr VkViewport -- ^ pViewports
                                                                     -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetViewport
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstViewport
--   >     , uint32_t viewportCount
--   >     , const VkViewport* pViewports
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetViewport.html vkCmdSetViewport registry at www.khronos.org>
foreign import ccall safe "vkCmdSetViewport" vkCmdSetViewportSafe
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> Word32 -- ^ firstViewport
                                            -> Word32 -- ^ viewportCount
                                                      -> Ptr VkViewport -- ^ pViewports
                                                                        -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetViewport
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstViewport
--   >     , uint32_t viewportCount
--   >     , const VkViewport* pViewports
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetViewport.html vkCmdSetViewport registry at www.khronos.org>
type HS_vkCmdSetViewport =
     VkCommandBuffer -- ^ commandBuffer
                     -> Word32 -- ^ firstViewport
                               -> Word32 -- ^ viewportCount
                                         -> Ptr VkViewport -- ^ pViewports
                                                           -> IO ()

type PFN_vkCmdSetViewport = FunPtr HS_vkCmdSetViewport

foreign import ccall "dynamic" unwrapVkCmdSetViewport ::
               PFN_vkCmdSetViewport -> HS_vkCmdSetViewport

instance VulkanProc "vkCmdSetViewport" where
        type VkProcType "vkCmdSetViewport" = HS_vkCmdSetViewport
        vkProcSymbol = _VkCmdSetViewport

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetViewport

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdSetScissor :: CString

pattern VkCmdSetScissor <- (is_VkCmdSetScissor -> True)
  where VkCmdSetScissor = _VkCmdSetScissor

{-# INLINE _VkCmdSetScissor #-}

_VkCmdSetScissor :: CString
_VkCmdSetScissor = Ptr "vkCmdSetScissor\NUL"##

{-# INLINE is_VkCmdSetScissor #-}

is_VkCmdSetScissor :: CString -> Bool
is_VkCmdSetScissor = (EQ ==) . cmpCStrings _VkCmdSetScissor

type VkCmdSetScissor = "vkCmdSetScissor"

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetScissor
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstScissor
--   >     , uint32_t scissorCount
--   >     , const VkRect2D* pScissors
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetScissor.html vkCmdSetScissor registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetScissor" vkCmdSetScissor ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ firstScissor
                                         -> Word32 -- ^ scissorCount
                                                   -> Ptr VkRect2D -- ^ pScissors
                                                                   -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetScissor
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstScissor
--   >     , uint32_t scissorCount
--   >     , const VkRect2D* pScissors
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetScissor.html vkCmdSetScissor registry at www.khronos.org>
foreign import ccall safe "vkCmdSetScissor" vkCmdSetScissorSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ firstScissor
                                         -> Word32 -- ^ scissorCount
                                                   -> Ptr VkRect2D -- ^ pScissors
                                                                   -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetScissor
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstScissor
--   >     , uint32_t scissorCount
--   >     , const VkRect2D* pScissors
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetScissor.html vkCmdSetScissor registry at www.khronos.org>
type HS_vkCmdSetScissor =
     VkCommandBuffer -- ^ commandBuffer
                     -> Word32 -- ^ firstScissor
                               -> Word32 -- ^ scissorCount
                                         -> Ptr VkRect2D -- ^ pScissors
                                                         -> IO ()

type PFN_vkCmdSetScissor = FunPtr HS_vkCmdSetScissor

foreign import ccall "dynamic" unwrapVkCmdSetScissor ::
               PFN_vkCmdSetScissor -> HS_vkCmdSetScissor

instance VulkanProc "vkCmdSetScissor" where
        type VkProcType "vkCmdSetScissor" = HS_vkCmdSetScissor
        vkProcSymbol = _VkCmdSetScissor

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetScissor

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdSetLineWidth :: CString

pattern VkCmdSetLineWidth <- (is_VkCmdSetLineWidth -> True)
  where VkCmdSetLineWidth = _VkCmdSetLineWidth

{-# INLINE _VkCmdSetLineWidth #-}

_VkCmdSetLineWidth :: CString
_VkCmdSetLineWidth = Ptr "vkCmdSetLineWidth\NUL"##

{-# INLINE is_VkCmdSetLineWidth #-}

is_VkCmdSetLineWidth :: CString -> Bool
is_VkCmdSetLineWidth = (EQ ==) . cmpCStrings _VkCmdSetLineWidth

type VkCmdSetLineWidth = "vkCmdSetLineWidth"

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetLineWidth
--   >     ( VkCommandBuffer commandBuffer
--   >     , float lineWidth
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetLineWidth.html vkCmdSetLineWidth registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetLineWidth" vkCmdSetLineWidth
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> #{type float} -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetLineWidth
--   >     ( VkCommandBuffer commandBuffer
--   >     , float lineWidth
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetLineWidth.html vkCmdSetLineWidth registry at www.khronos.org>
foreign import ccall safe "vkCmdSetLineWidth" vkCmdSetLineWidthSafe
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> #{type float} -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetLineWidth
--   >     ( VkCommandBuffer commandBuffer
--   >     , float lineWidth
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetLineWidth.html vkCmdSetLineWidth registry at www.khronos.org>
type HS_vkCmdSetLineWidth =
     VkCommandBuffer -- ^ commandBuffer
                     -> #{type float} -> IO ()

type PFN_vkCmdSetLineWidth = FunPtr HS_vkCmdSetLineWidth

foreign import ccall "dynamic" unwrapVkCmdSetLineWidth ::
               PFN_vkCmdSetLineWidth -> HS_vkCmdSetLineWidth

instance VulkanProc "vkCmdSetLineWidth" where
        type VkProcType "vkCmdSetLineWidth" = HS_vkCmdSetLineWidth
        vkProcSymbol = _VkCmdSetLineWidth

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetLineWidth

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdSetDepthBias :: CString

pattern VkCmdSetDepthBias <- (is_VkCmdSetDepthBias -> True)
  where VkCmdSetDepthBias = _VkCmdSetDepthBias

{-# INLINE _VkCmdSetDepthBias #-}

_VkCmdSetDepthBias :: CString
_VkCmdSetDepthBias = Ptr "vkCmdSetDepthBias\NUL"##

{-# INLINE is_VkCmdSetDepthBias #-}

is_VkCmdSetDepthBias :: CString -> Bool
is_VkCmdSetDepthBias = (EQ ==) . cmpCStrings _VkCmdSetDepthBias

type VkCmdSetDepthBias = "vkCmdSetDepthBias"

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetDepthBias
--   >     ( VkCommandBuffer commandBuffer
--   >     , float depthBiasConstantFactor
--   >     , float depthBiasClamp
--   >     , float depthBiasSlopeFactor
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetDepthBias.html vkCmdSetDepthBias registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetDepthBias" vkCmdSetDepthBias
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 #{type float} ->
                   #{type float} -> #{type float} -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetDepthBias
--   >     ( VkCommandBuffer commandBuffer
--   >     , float depthBiasConstantFactor
--   >     , float depthBiasClamp
--   >     , float depthBiasSlopeFactor
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetDepthBias.html vkCmdSetDepthBias registry at www.khronos.org>
foreign import ccall safe "vkCmdSetDepthBias" vkCmdSetDepthBiasSafe
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 #{type float} ->
                   #{type float} -> #{type float} -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetDepthBias
--   >     ( VkCommandBuffer commandBuffer
--   >     , float depthBiasConstantFactor
--   >     , float depthBiasClamp
--   >     , float depthBiasSlopeFactor
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetDepthBias.html vkCmdSetDepthBias registry at www.khronos.org>
type HS_vkCmdSetDepthBias =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       #{type float} ->
         #{type float} -> #{type float} -> IO ()

type PFN_vkCmdSetDepthBias = FunPtr HS_vkCmdSetDepthBias

foreign import ccall "dynamic" unwrapVkCmdSetDepthBias ::
               PFN_vkCmdSetDepthBias -> HS_vkCmdSetDepthBias

instance VulkanProc "vkCmdSetDepthBias" where
        type VkProcType "vkCmdSetDepthBias" = HS_vkCmdSetDepthBias
        vkProcSymbol = _VkCmdSetDepthBias

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetDepthBias

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdSetBlendConstants :: CString

pattern VkCmdSetBlendConstants <-
        (is_VkCmdSetBlendConstants -> True)
  where VkCmdSetBlendConstants = _VkCmdSetBlendConstants

{-# INLINE _VkCmdSetBlendConstants #-}

_VkCmdSetBlendConstants :: CString
_VkCmdSetBlendConstants = Ptr "vkCmdSetBlendConstants\NUL"##

{-# INLINE is_VkCmdSetBlendConstants #-}

is_VkCmdSetBlendConstants :: CString -> Bool
is_VkCmdSetBlendConstants
  = (EQ ==) . cmpCStrings _VkCmdSetBlendConstants

type VkCmdSetBlendConstants = "vkCmdSetBlendConstants"

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetBlendConstants
--   >     ( VkCommandBuffer commandBuffer
--   >     , const float blendConstants[4]
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetBlendConstants.html vkCmdSetBlendConstants registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetBlendConstants"
               vkCmdSetBlendConstants ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr #{type float} -- ^ blendConstants
                                                                -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetBlendConstants
--   >     ( VkCommandBuffer commandBuffer
--   >     , const float blendConstants[4]
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetBlendConstants.html vkCmdSetBlendConstants registry at www.khronos.org>
foreign import ccall safe "vkCmdSetBlendConstants"
               vkCmdSetBlendConstantsSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr #{type float} -- ^ blendConstants
                                                                -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetBlendConstants
--   >     ( VkCommandBuffer commandBuffer
--   >     , const float blendConstants[4]
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetBlendConstants.html vkCmdSetBlendConstants registry at www.khronos.org>
type HS_vkCmdSetBlendConstants =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr #{type float} -- ^ blendConstants
                                                      -> IO ()

type PFN_vkCmdSetBlendConstants = FunPtr HS_vkCmdSetBlendConstants

foreign import ccall "dynamic" unwrapVkCmdSetBlendConstants ::
               PFN_vkCmdSetBlendConstants -> HS_vkCmdSetBlendConstants

instance VulkanProc "vkCmdSetBlendConstants" where
        type VkProcType "vkCmdSetBlendConstants" =
             HS_vkCmdSetBlendConstants
        vkProcSymbol = _VkCmdSetBlendConstants

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetBlendConstants

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdSetDepthBounds :: CString

pattern VkCmdSetDepthBounds <- (is_VkCmdSetDepthBounds -> True)
  where VkCmdSetDepthBounds = _VkCmdSetDepthBounds

{-# INLINE _VkCmdSetDepthBounds #-}

_VkCmdSetDepthBounds :: CString
_VkCmdSetDepthBounds = Ptr "vkCmdSetDepthBounds\NUL"##

{-# INLINE is_VkCmdSetDepthBounds #-}

is_VkCmdSetDepthBounds :: CString -> Bool
is_VkCmdSetDepthBounds = (EQ ==) . cmpCStrings _VkCmdSetDepthBounds

type VkCmdSetDepthBounds = "vkCmdSetDepthBounds"

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetDepthBounds
--   >     ( VkCommandBuffer commandBuffer
--   >     , float minDepthBounds
--   >     , float maxDepthBounds
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetDepthBounds.html vkCmdSetDepthBounds registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetDepthBounds"
               vkCmdSetDepthBounds ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 #{type float} -> #{type float} -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetDepthBounds
--   >     ( VkCommandBuffer commandBuffer
--   >     , float minDepthBounds
--   >     , float maxDepthBounds
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetDepthBounds.html vkCmdSetDepthBounds registry at www.khronos.org>
foreign import ccall safe "vkCmdSetDepthBounds"
               vkCmdSetDepthBoundsSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 #{type float} -> #{type float} -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetDepthBounds
--   >     ( VkCommandBuffer commandBuffer
--   >     , float minDepthBounds
--   >     , float maxDepthBounds
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetDepthBounds.html vkCmdSetDepthBounds registry at www.khronos.org>
type HS_vkCmdSetDepthBounds =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       #{type float} -> #{type float} -> IO ()

type PFN_vkCmdSetDepthBounds = FunPtr HS_vkCmdSetDepthBounds

foreign import ccall "dynamic" unwrapVkCmdSetDepthBounds ::
               PFN_vkCmdSetDepthBounds -> HS_vkCmdSetDepthBounds

instance VulkanProc "vkCmdSetDepthBounds" where
        type VkProcType "vkCmdSetDepthBounds" = HS_vkCmdSetDepthBounds
        vkProcSymbol = _VkCmdSetDepthBounds

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetDepthBounds

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdSetStencilCompareMask :: CString

pattern VkCmdSetStencilCompareMask <-
        (is_VkCmdSetStencilCompareMask -> True)
  where VkCmdSetStencilCompareMask = _VkCmdSetStencilCompareMask

{-# INLINE _VkCmdSetStencilCompareMask #-}

_VkCmdSetStencilCompareMask :: CString
_VkCmdSetStencilCompareMask = Ptr "vkCmdSetStencilCompareMask\NUL"##

{-# INLINE is_VkCmdSetStencilCompareMask #-}

is_VkCmdSetStencilCompareMask :: CString -> Bool
is_VkCmdSetStencilCompareMask
  = (EQ ==) . cmpCStrings _VkCmdSetStencilCompareMask

type VkCmdSetStencilCompareMask = "vkCmdSetStencilCompareMask"

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetStencilCompareMask
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkStencilFaceFlags faceMask
--   >     , uint32_t compareMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetStencilCompareMask.html vkCmdSetStencilCompareMask registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetStencilCompareMask"
               vkCmdSetStencilCompareMask ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkStencilFaceFlags -- ^ faceMask
                                                     -> Word32 -- ^ compareMask
                                                               -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetStencilCompareMask
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkStencilFaceFlags faceMask
--   >     , uint32_t compareMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetStencilCompareMask.html vkCmdSetStencilCompareMask registry at www.khronos.org>
foreign import ccall safe "vkCmdSetStencilCompareMask"
               vkCmdSetStencilCompareMaskSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkStencilFaceFlags -- ^ faceMask
                                                     -> Word32 -- ^ compareMask
                                                               -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetStencilCompareMask
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkStencilFaceFlags faceMask
--   >     , uint32_t compareMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetStencilCompareMask.html vkCmdSetStencilCompareMask registry at www.khronos.org>
type HS_vkCmdSetStencilCompareMask =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkStencilFaceFlags -- ^ faceMask
                                           -> Word32 -- ^ compareMask
                                                     -> IO ()

type PFN_vkCmdSetStencilCompareMask =
     FunPtr HS_vkCmdSetStencilCompareMask

foreign import ccall "dynamic" unwrapVkCmdSetStencilCompareMask ::
               PFN_vkCmdSetStencilCompareMask -> HS_vkCmdSetStencilCompareMask

instance VulkanProc "vkCmdSetStencilCompareMask" where
        type VkProcType "vkCmdSetStencilCompareMask" =
             HS_vkCmdSetStencilCompareMask
        vkProcSymbol = _VkCmdSetStencilCompareMask

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetStencilCompareMask

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdSetStencilWriteMask :: CString

pattern VkCmdSetStencilWriteMask <-
        (is_VkCmdSetStencilWriteMask -> True)
  where VkCmdSetStencilWriteMask = _VkCmdSetStencilWriteMask

{-# INLINE _VkCmdSetStencilWriteMask #-}

_VkCmdSetStencilWriteMask :: CString
_VkCmdSetStencilWriteMask = Ptr "vkCmdSetStencilWriteMask\NUL"##

{-# INLINE is_VkCmdSetStencilWriteMask #-}

is_VkCmdSetStencilWriteMask :: CString -> Bool
is_VkCmdSetStencilWriteMask
  = (EQ ==) . cmpCStrings _VkCmdSetStencilWriteMask

type VkCmdSetStencilWriteMask = "vkCmdSetStencilWriteMask"

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetStencilWriteMask
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkStencilFaceFlags faceMask
--   >     , uint32_t writeMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetStencilWriteMask.html vkCmdSetStencilWriteMask registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetStencilWriteMask"
               vkCmdSetStencilWriteMask ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkStencilFaceFlags -- ^ faceMask
                                                     -> Word32 -- ^ writeMask
                                                               -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetStencilWriteMask
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkStencilFaceFlags faceMask
--   >     , uint32_t writeMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetStencilWriteMask.html vkCmdSetStencilWriteMask registry at www.khronos.org>
foreign import ccall safe "vkCmdSetStencilWriteMask"
               vkCmdSetStencilWriteMaskSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkStencilFaceFlags -- ^ faceMask
                                                     -> Word32 -- ^ writeMask
                                                               -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetStencilWriteMask
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkStencilFaceFlags faceMask
--   >     , uint32_t writeMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetStencilWriteMask.html vkCmdSetStencilWriteMask registry at www.khronos.org>
type HS_vkCmdSetStencilWriteMask =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkStencilFaceFlags -- ^ faceMask
                                           -> Word32 -- ^ writeMask
                                                     -> IO ()

type PFN_vkCmdSetStencilWriteMask =
     FunPtr HS_vkCmdSetStencilWriteMask

foreign import ccall "dynamic" unwrapVkCmdSetStencilWriteMask ::
               PFN_vkCmdSetStencilWriteMask -> HS_vkCmdSetStencilWriteMask

instance VulkanProc "vkCmdSetStencilWriteMask" where
        type VkProcType "vkCmdSetStencilWriteMask" =
             HS_vkCmdSetStencilWriteMask
        vkProcSymbol = _VkCmdSetStencilWriteMask

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetStencilWriteMask

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdSetStencilReference :: CString

pattern VkCmdSetStencilReference <-
        (is_VkCmdSetStencilReference -> True)
  where VkCmdSetStencilReference = _VkCmdSetStencilReference

{-# INLINE _VkCmdSetStencilReference #-}

_VkCmdSetStencilReference :: CString
_VkCmdSetStencilReference = Ptr "vkCmdSetStencilReference\NUL"##

{-# INLINE is_VkCmdSetStencilReference #-}

is_VkCmdSetStencilReference :: CString -> Bool
is_VkCmdSetStencilReference
  = (EQ ==) . cmpCStrings _VkCmdSetStencilReference

type VkCmdSetStencilReference = "vkCmdSetStencilReference"

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetStencilReference
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkStencilFaceFlags faceMask
--   >     , uint32_t reference
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetStencilReference.html vkCmdSetStencilReference registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetStencilReference"
               vkCmdSetStencilReference ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkStencilFaceFlags -- ^ faceMask
                                                     -> Word32 -- ^ reference
                                                               -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetStencilReference
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkStencilFaceFlags faceMask
--   >     , uint32_t reference
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetStencilReference.html vkCmdSetStencilReference registry at www.khronos.org>
foreign import ccall safe "vkCmdSetStencilReference"
               vkCmdSetStencilReferenceSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkStencilFaceFlags -- ^ faceMask
                                                     -> Word32 -- ^ reference
                                                               -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdSetStencilReference
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkStencilFaceFlags faceMask
--   >     , uint32_t reference
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetStencilReference.html vkCmdSetStencilReference registry at www.khronos.org>
type HS_vkCmdSetStencilReference =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkStencilFaceFlags -- ^ faceMask
                                           -> Word32 -- ^ reference
                                                     -> IO ()

type PFN_vkCmdSetStencilReference =
     FunPtr HS_vkCmdSetStencilReference

foreign import ccall "dynamic" unwrapVkCmdSetStencilReference ::
               PFN_vkCmdSetStencilReference -> HS_vkCmdSetStencilReference

instance VulkanProc "vkCmdSetStencilReference" where
        type VkProcType "vkCmdSetStencilReference" =
             HS_vkCmdSetStencilReference
        vkProcSymbol = _VkCmdSetStencilReference

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetStencilReference

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdBindDescriptorSets :: CString

pattern VkCmdBindDescriptorSets <-
        (is_VkCmdBindDescriptorSets -> True)
  where VkCmdBindDescriptorSets = _VkCmdBindDescriptorSets

{-# INLINE _VkCmdBindDescriptorSets #-}

_VkCmdBindDescriptorSets :: CString
_VkCmdBindDescriptorSets = Ptr "vkCmdBindDescriptorSets\NUL"##

{-# INLINE is_VkCmdBindDescriptorSets #-}

is_VkCmdBindDescriptorSets :: CString -> Bool
is_VkCmdBindDescriptorSets
  = (EQ ==) . cmpCStrings _VkCmdBindDescriptorSets

type VkCmdBindDescriptorSets = "vkCmdBindDescriptorSets"

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdBindDescriptorSets
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineBindPoint pipelineBindPoint
--   >     , VkPipelineLayout layout
--   >     , uint32_t firstSet
--   >     , uint32_t descriptorSetCount
--   >     , const VkDescriptorSet* pDescriptorSets
--   >     , uint32_t dynamicOffsetCount
--   >     , const uint32_t* pDynamicOffsets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBindDescriptorSets.html vkCmdBindDescriptorSets registry at www.khronos.org>
foreign import ccall unsafe "vkCmdBindDescriptorSets"
               vkCmdBindDescriptorSets ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineBindPoint -- ^ pipelineBindPoint
                                     ->
                   VkPipelineLayout -- ^ layout
                                    ->
                     Word32 -- ^ firstSet
                            ->
                       Word32 -- ^ descriptorSetCount
                              -> Ptr VkDescriptorSet -- ^ pDescriptorSets
                                                     -> Word32 -- ^ dynamicOffsetCount
                                                               -> Ptr Word32 -- ^ pDynamicOffsets
                                                                             -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdBindDescriptorSets
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineBindPoint pipelineBindPoint
--   >     , VkPipelineLayout layout
--   >     , uint32_t firstSet
--   >     , uint32_t descriptorSetCount
--   >     , const VkDescriptorSet* pDescriptorSets
--   >     , uint32_t dynamicOffsetCount
--   >     , const uint32_t* pDynamicOffsets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBindDescriptorSets.html vkCmdBindDescriptorSets registry at www.khronos.org>
foreign import ccall safe "vkCmdBindDescriptorSets"
               vkCmdBindDescriptorSetsSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineBindPoint -- ^ pipelineBindPoint
                                     ->
                   VkPipelineLayout -- ^ layout
                                    ->
                     Word32 -- ^ firstSet
                            ->
                       Word32 -- ^ descriptorSetCount
                              -> Ptr VkDescriptorSet -- ^ pDescriptorSets
                                                     -> Word32 -- ^ dynamicOffsetCount
                                                               -> Ptr Word32 -- ^ pDynamicOffsets
                                                                             -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdBindDescriptorSets
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineBindPoint pipelineBindPoint
--   >     , VkPipelineLayout layout
--   >     , uint32_t firstSet
--   >     , uint32_t descriptorSetCount
--   >     , const VkDescriptorSet* pDescriptorSets
--   >     , uint32_t dynamicOffsetCount
--   >     , const uint32_t* pDynamicOffsets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBindDescriptorSets.html vkCmdBindDescriptorSets registry at www.khronos.org>
type HS_vkCmdBindDescriptorSets =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkPipelineBindPoint -- ^ pipelineBindPoint
                           ->
         VkPipelineLayout -- ^ layout
                          ->
           Word32 -- ^ firstSet
                  ->
             Word32 -- ^ descriptorSetCount
                    -> Ptr VkDescriptorSet -- ^ pDescriptorSets
                                           -> Word32 -- ^ dynamicOffsetCount
                                                     -> Ptr Word32 -- ^ pDynamicOffsets
                                                                   -> IO ()

type PFN_vkCmdBindDescriptorSets =
     FunPtr HS_vkCmdBindDescriptorSets

foreign import ccall "dynamic" unwrapVkCmdBindDescriptorSets ::
               PFN_vkCmdBindDescriptorSets -> HS_vkCmdBindDescriptorSets

instance VulkanProc "vkCmdBindDescriptorSets" where
        type VkProcType "vkCmdBindDescriptorSets" =
             HS_vkCmdBindDescriptorSets
        vkProcSymbol = _VkCmdBindDescriptorSets

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdBindDescriptorSets

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdBindIndexBuffer :: CString

pattern VkCmdBindIndexBuffer <- (is_VkCmdBindIndexBuffer -> True)
  where VkCmdBindIndexBuffer = _VkCmdBindIndexBuffer

{-# INLINE _VkCmdBindIndexBuffer #-}

_VkCmdBindIndexBuffer :: CString
_VkCmdBindIndexBuffer = Ptr "vkCmdBindIndexBuffer\NUL"##

{-# INLINE is_VkCmdBindIndexBuffer #-}

is_VkCmdBindIndexBuffer :: CString -> Bool
is_VkCmdBindIndexBuffer
  = (EQ ==) . cmpCStrings _VkCmdBindIndexBuffer

type VkCmdBindIndexBuffer = "vkCmdBindIndexBuffer"

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdBindIndexBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkIndexType indexType
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBindIndexBuffer.html vkCmdBindIndexBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkCmdBindIndexBuffer"
               vkCmdBindIndexBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkBuffer -- ^ buffer
                                           -> VkDeviceSize -- ^ offset
                                                           -> VkIndexType -- ^ indexType
                                                                          -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdBindIndexBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkIndexType indexType
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBindIndexBuffer.html vkCmdBindIndexBuffer registry at www.khronos.org>
foreign import ccall safe "vkCmdBindIndexBuffer"
               vkCmdBindIndexBufferSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkBuffer -- ^ buffer
                                           -> VkDeviceSize -- ^ offset
                                                           -> VkIndexType -- ^ indexType
                                                                          -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdBindIndexBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkIndexType indexType
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBindIndexBuffer.html vkCmdBindIndexBuffer registry at www.khronos.org>
type HS_vkCmdBindIndexBuffer =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkBuffer -- ^ buffer
                                 -> VkDeviceSize -- ^ offset
                                                 -> VkIndexType -- ^ indexType
                                                                -> IO ()

type PFN_vkCmdBindIndexBuffer = FunPtr HS_vkCmdBindIndexBuffer

foreign import ccall "dynamic" unwrapVkCmdBindIndexBuffer ::
               PFN_vkCmdBindIndexBuffer -> HS_vkCmdBindIndexBuffer

instance VulkanProc "vkCmdBindIndexBuffer" where
        type VkProcType "vkCmdBindIndexBuffer" = HS_vkCmdBindIndexBuffer
        vkProcSymbol = _VkCmdBindIndexBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdBindIndexBuffer

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdBindVertexBuffers :: CString

pattern VkCmdBindVertexBuffers <-
        (is_VkCmdBindVertexBuffers -> True)
  where VkCmdBindVertexBuffers = _VkCmdBindVertexBuffers

{-# INLINE _VkCmdBindVertexBuffers #-}

_VkCmdBindVertexBuffers :: CString
_VkCmdBindVertexBuffers = Ptr "vkCmdBindVertexBuffers\NUL"##

{-# INLINE is_VkCmdBindVertexBuffers #-}

is_VkCmdBindVertexBuffers :: CString -> Bool
is_VkCmdBindVertexBuffers
  = (EQ ==) . cmpCStrings _VkCmdBindVertexBuffers

type VkCmdBindVertexBuffers = "vkCmdBindVertexBuffers"

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdBindVertexBuffers
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstBinding
--   >     , uint32_t bindingCount
--   >     , const VkBuffer* pBuffers
--   >     , const VkDeviceSize* pOffsets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBindVertexBuffers.html vkCmdBindVertexBuffers registry at www.khronos.org>
foreign import ccall unsafe "vkCmdBindVertexBuffers"
               vkCmdBindVertexBuffers ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ firstBinding
                        -> Word32 -- ^ bindingCount
                                  -> Ptr VkBuffer -- ^ pBuffers
                                                  -> Ptr VkDeviceSize -- ^ pOffsets
                                                                      -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdBindVertexBuffers
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstBinding
--   >     , uint32_t bindingCount
--   >     , const VkBuffer* pBuffers
--   >     , const VkDeviceSize* pOffsets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBindVertexBuffers.html vkCmdBindVertexBuffers registry at www.khronos.org>
foreign import ccall safe "vkCmdBindVertexBuffers"
               vkCmdBindVertexBuffersSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ firstBinding
                        -> Word32 -- ^ bindingCount
                                  -> Ptr VkBuffer -- ^ pBuffers
                                                  -> Ptr VkDeviceSize -- ^ pOffsets
                                                                      -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @both@
--
--   > () vkCmdBindVertexBuffers
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstBinding
--   >     , uint32_t bindingCount
--   >     , const VkBuffer* pBuffers
--   >     , const VkDeviceSize* pOffsets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBindVertexBuffers.html vkCmdBindVertexBuffers registry at www.khronos.org>
type HS_vkCmdBindVertexBuffers =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ firstBinding
              -> Word32 -- ^ bindingCount
                        -> Ptr VkBuffer -- ^ pBuffers
                                        -> Ptr VkDeviceSize -- ^ pOffsets
                                                            -> IO ()

type PFN_vkCmdBindVertexBuffers = FunPtr HS_vkCmdBindVertexBuffers

foreign import ccall "dynamic" unwrapVkCmdBindVertexBuffers ::
               PFN_vkCmdBindVertexBuffers -> HS_vkCmdBindVertexBuffers

instance VulkanProc "vkCmdBindVertexBuffers" where
        type VkProcType "vkCmdBindVertexBuffers" =
             HS_vkCmdBindVertexBuffers
        vkProcSymbol = _VkCmdBindVertexBuffers

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdBindVertexBuffers

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdDraw :: CString

pattern VkCmdDraw <- (is_VkCmdDraw -> True)
  where VkCmdDraw = _VkCmdDraw

{-# INLINE _VkCmdDraw #-}

_VkCmdDraw :: CString
_VkCmdDraw = Ptr "vkCmdDraw\NUL"##

{-# INLINE is_VkCmdDraw #-}

is_VkCmdDraw :: CString -> Bool
is_VkCmdDraw = (EQ ==) . cmpCStrings _VkCmdDraw

type VkCmdDraw = "vkCmdDraw"

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDraw
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t vertexCount
--   >     , uint32_t instanceCount
--   >     , uint32_t firstVertex
--   >     , uint32_t firstInstance
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDraw.html vkCmdDraw registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDraw" vkCmdDraw ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ vertexCount
                                         -> Word32 -- ^ instanceCount
                                                   -> Word32 -- ^ firstVertex
                                                             -> Word32 -- ^ firstInstance
                                                                       -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDraw
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t vertexCount
--   >     , uint32_t instanceCount
--   >     , uint32_t firstVertex
--   >     , uint32_t firstInstance
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDraw.html vkCmdDraw registry at www.khronos.org>
foreign import ccall safe "vkCmdDraw" vkCmdDrawSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ vertexCount
                                         -> Word32 -- ^ instanceCount
                                                   -> Word32 -- ^ firstVertex
                                                             -> Word32 -- ^ firstInstance
                                                                       -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDraw
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t vertexCount
--   >     , uint32_t instanceCount
--   >     , uint32_t firstVertex
--   >     , uint32_t firstInstance
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDraw.html vkCmdDraw registry at www.khronos.org>
type HS_vkCmdDraw =
     VkCommandBuffer -- ^ commandBuffer
                     -> Word32 -- ^ vertexCount
                               -> Word32 -- ^ instanceCount
                                         -> Word32 -- ^ firstVertex
                                                   -> Word32 -- ^ firstInstance
                                                             -> IO ()

type PFN_vkCmdDraw = FunPtr HS_vkCmdDraw

foreign import ccall "dynamic" unwrapVkCmdDraw ::
               PFN_vkCmdDraw -> HS_vkCmdDraw

instance VulkanProc "vkCmdDraw" where
        type VkProcType "vkCmdDraw" = HS_vkCmdDraw
        vkProcSymbol = _VkCmdDraw

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdDraw

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdDrawIndexed :: CString

pattern VkCmdDrawIndexed <- (is_VkCmdDrawIndexed -> True)
  where VkCmdDrawIndexed = _VkCmdDrawIndexed

{-# INLINE _VkCmdDrawIndexed #-}

_VkCmdDrawIndexed :: CString
_VkCmdDrawIndexed = Ptr "vkCmdDrawIndexed\NUL"##

{-# INLINE is_VkCmdDrawIndexed #-}

is_VkCmdDrawIndexed :: CString -> Bool
is_VkCmdDrawIndexed = (EQ ==) . cmpCStrings _VkCmdDrawIndexed

type VkCmdDrawIndexed = "vkCmdDrawIndexed"

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDrawIndexed
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t indexCount
--   >     , uint32_t instanceCount
--   >     , uint32_t firstIndex
--   >     , int32_t vertexOffset
--   >     , uint32_t firstInstance
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDrawIndexed.html vkCmdDrawIndexed registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDrawIndexed" vkCmdDrawIndexed ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ indexCount
                        -> Word32 -- ^ instanceCount
                                  -> Word32 -- ^ firstIndex
                                            -> Int32 -- ^ vertexOffset
                                                     -> Word32 -- ^ firstInstance
                                                               -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDrawIndexed
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t indexCount
--   >     , uint32_t instanceCount
--   >     , uint32_t firstIndex
--   >     , int32_t vertexOffset
--   >     , uint32_t firstInstance
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDrawIndexed.html vkCmdDrawIndexed registry at www.khronos.org>
foreign import ccall safe "vkCmdDrawIndexed" vkCmdDrawIndexedSafe
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ indexCount
                        -> Word32 -- ^ instanceCount
                                  -> Word32 -- ^ firstIndex
                                            -> Int32 -- ^ vertexOffset
                                                     -> Word32 -- ^ firstInstance
                                                               -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDrawIndexed
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t indexCount
--   >     , uint32_t instanceCount
--   >     , uint32_t firstIndex
--   >     , int32_t vertexOffset
--   >     , uint32_t firstInstance
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDrawIndexed.html vkCmdDrawIndexed registry at www.khronos.org>
type HS_vkCmdDrawIndexed =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ indexCount
              -> Word32 -- ^ instanceCount
                        -> Word32 -- ^ firstIndex
                                  -> Int32 -- ^ vertexOffset
                                           -> Word32 -- ^ firstInstance
                                                     -> IO ()

type PFN_vkCmdDrawIndexed = FunPtr HS_vkCmdDrawIndexed

foreign import ccall "dynamic" unwrapVkCmdDrawIndexed ::
               PFN_vkCmdDrawIndexed -> HS_vkCmdDrawIndexed

instance VulkanProc "vkCmdDrawIndexed" where
        type VkProcType "vkCmdDrawIndexed" = HS_vkCmdDrawIndexed
        vkProcSymbol = _VkCmdDrawIndexed

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdDrawIndexed

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdDrawIndirect :: CString

pattern VkCmdDrawIndirect <- (is_VkCmdDrawIndirect -> True)
  where VkCmdDrawIndirect = _VkCmdDrawIndirect

{-# INLINE _VkCmdDrawIndirect #-}

_VkCmdDrawIndirect :: CString
_VkCmdDrawIndirect = Ptr "vkCmdDrawIndirect\NUL"##

{-# INLINE is_VkCmdDrawIndirect #-}

is_VkCmdDrawIndirect :: CString -> Bool
is_VkCmdDrawIndirect = (EQ ==) . cmpCStrings _VkCmdDrawIndirect

type VkCmdDrawIndirect = "vkCmdDrawIndirect"

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDrawIndirect
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , uint32_t drawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDrawIndirect.html vkCmdDrawIndirect registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDrawIndirect" vkCmdDrawIndirect
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          -> VkDeviceSize -- ^ offset
                                          -> Word32 -- ^ drawCount
                                                    -> Word32 -- ^ stride
                                                              -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDrawIndirect
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , uint32_t drawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDrawIndirect.html vkCmdDrawIndirect registry at www.khronos.org>
foreign import ccall safe "vkCmdDrawIndirect" vkCmdDrawIndirectSafe
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          -> VkDeviceSize -- ^ offset
                                          -> Word32 -- ^ drawCount
                                                    -> Word32 -- ^ stride
                                                              -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDrawIndirect
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , uint32_t drawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDrawIndirect.html vkCmdDrawIndirect registry at www.khronos.org>
type HS_vkCmdDrawIndirect =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkBuffer -- ^ buffer
                -> VkDeviceSize -- ^ offset
                                -> Word32 -- ^ drawCount
                                          -> Word32 -- ^ stride
                                                    -> IO ()

type PFN_vkCmdDrawIndirect = FunPtr HS_vkCmdDrawIndirect

foreign import ccall "dynamic" unwrapVkCmdDrawIndirect ::
               PFN_vkCmdDrawIndirect -> HS_vkCmdDrawIndirect

instance VulkanProc "vkCmdDrawIndirect" where
        type VkProcType "vkCmdDrawIndirect" = HS_vkCmdDrawIndirect
        vkProcSymbol = _VkCmdDrawIndirect

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdDrawIndirect

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdDrawIndexedIndirect :: CString

pattern VkCmdDrawIndexedIndirect <-
        (is_VkCmdDrawIndexedIndirect -> True)
  where VkCmdDrawIndexedIndirect = _VkCmdDrawIndexedIndirect

{-# INLINE _VkCmdDrawIndexedIndirect #-}

_VkCmdDrawIndexedIndirect :: CString
_VkCmdDrawIndexedIndirect = Ptr "vkCmdDrawIndexedIndirect\NUL"##

{-# INLINE is_VkCmdDrawIndexedIndirect #-}

is_VkCmdDrawIndexedIndirect :: CString -> Bool
is_VkCmdDrawIndexedIndirect
  = (EQ ==) . cmpCStrings _VkCmdDrawIndexedIndirect

type VkCmdDrawIndexedIndirect = "vkCmdDrawIndexedIndirect"

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDrawIndexedIndirect
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , uint32_t drawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDrawIndexedIndirect.html vkCmdDrawIndexedIndirect registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDrawIndexedIndirect"
               vkCmdDrawIndexedIndirect ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          -> VkDeviceSize -- ^ offset
                                          -> Word32 -- ^ drawCount
                                                    -> Word32 -- ^ stride
                                                              -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDrawIndexedIndirect
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , uint32_t drawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDrawIndexedIndirect.html vkCmdDrawIndexedIndirect registry at www.khronos.org>
foreign import ccall safe "vkCmdDrawIndexedIndirect"
               vkCmdDrawIndexedIndirectSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          -> VkDeviceSize -- ^ offset
                                          -> Word32 -- ^ drawCount
                                                    -> Word32 -- ^ stride
                                                              -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdDrawIndexedIndirect
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , uint32_t drawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDrawIndexedIndirect.html vkCmdDrawIndexedIndirect registry at www.khronos.org>
type HS_vkCmdDrawIndexedIndirect =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkBuffer -- ^ buffer
                -> VkDeviceSize -- ^ offset
                                -> Word32 -- ^ drawCount
                                          -> Word32 -- ^ stride
                                                    -> IO ()

type PFN_vkCmdDrawIndexedIndirect =
     FunPtr HS_vkCmdDrawIndexedIndirect

foreign import ccall "dynamic" unwrapVkCmdDrawIndexedIndirect ::
               PFN_vkCmdDrawIndexedIndirect -> HS_vkCmdDrawIndexedIndirect

instance VulkanProc "vkCmdDrawIndexedIndirect" where
        type VkProcType "vkCmdDrawIndexedIndirect" =
             HS_vkCmdDrawIndexedIndirect
        vkProcSymbol = _VkCmdDrawIndexedIndirect

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdDrawIndexedIndirect

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdDispatch :: CString

pattern VkCmdDispatch <- (is_VkCmdDispatch -> True)
  where VkCmdDispatch = _VkCmdDispatch

{-# INLINE _VkCmdDispatch #-}

_VkCmdDispatch :: CString
_VkCmdDispatch = Ptr "vkCmdDispatch\NUL"##

{-# INLINE is_VkCmdDispatch #-}

is_VkCmdDispatch :: CString -> Bool
is_VkCmdDispatch = (EQ ==) . cmpCStrings _VkCmdDispatch

type VkCmdDispatch = "vkCmdDispatch"

-- | queues: 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @compute@
--
--   > () vkCmdDispatch
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t groupCountX
--   >     , uint32_t groupCountY
--   >     , uint32_t groupCountZ
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDispatch.html vkCmdDispatch registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDispatch" vkCmdDispatch ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ groupCountX
                                         -> Word32 -- ^ groupCountY
                                                   -> Word32 -- ^ groupCountZ
                                                             -> IO ()

-- | queues: 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @compute@
--
--   > () vkCmdDispatch
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t groupCountX
--   >     , uint32_t groupCountY
--   >     , uint32_t groupCountZ
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDispatch.html vkCmdDispatch registry at www.khronos.org>
foreign import ccall safe "vkCmdDispatch" vkCmdDispatchSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ groupCountX
                                         -> Word32 -- ^ groupCountY
                                                   -> Word32 -- ^ groupCountZ
                                                             -> IO ()

-- | queues: 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @compute@
--
--   > () vkCmdDispatch
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t groupCountX
--   >     , uint32_t groupCountY
--   >     , uint32_t groupCountZ
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDispatch.html vkCmdDispatch registry at www.khronos.org>
type HS_vkCmdDispatch =
     VkCommandBuffer -- ^ commandBuffer
                     -> Word32 -- ^ groupCountX
                               -> Word32 -- ^ groupCountY
                                         -> Word32 -- ^ groupCountZ
                                                   -> IO ()

type PFN_vkCmdDispatch = FunPtr HS_vkCmdDispatch

foreign import ccall "dynamic" unwrapVkCmdDispatch ::
               PFN_vkCmdDispatch -> HS_vkCmdDispatch

instance VulkanProc "vkCmdDispatch" where
        type VkProcType "vkCmdDispatch" = HS_vkCmdDispatch
        vkProcSymbol = _VkCmdDispatch

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdDispatch

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdDispatchIndirect :: CString

pattern VkCmdDispatchIndirect <- (is_VkCmdDispatchIndirect -> True)
  where VkCmdDispatchIndirect = _VkCmdDispatchIndirect

{-# INLINE _VkCmdDispatchIndirect #-}

_VkCmdDispatchIndirect :: CString
_VkCmdDispatchIndirect = Ptr "vkCmdDispatchIndirect\NUL"##

{-# INLINE is_VkCmdDispatchIndirect #-}

is_VkCmdDispatchIndirect :: CString -> Bool
is_VkCmdDispatchIndirect
  = (EQ ==) . cmpCStrings _VkCmdDispatchIndirect

type VkCmdDispatchIndirect = "vkCmdDispatchIndirect"

-- | queues: 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @compute@
--
--   > () vkCmdDispatchIndirect
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDispatchIndirect.html vkCmdDispatchIndirect registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDispatchIndirect"
               vkCmdDispatchIndirect ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkBuffer -- ^ buffer
                                           -> VkDeviceSize -- ^ offset
                                                           -> IO ()

-- | queues: 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @compute@
--
--   > () vkCmdDispatchIndirect
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDispatchIndirect.html vkCmdDispatchIndirect registry at www.khronos.org>
foreign import ccall safe "vkCmdDispatchIndirect"
               vkCmdDispatchIndirectSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkBuffer -- ^ buffer
                                           -> VkDeviceSize -- ^ offset
                                                           -> IO ()

-- | queues: 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @compute@
--
--   > () vkCmdDispatchIndirect
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdDispatchIndirect.html vkCmdDispatchIndirect registry at www.khronos.org>
type HS_vkCmdDispatchIndirect =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkBuffer -- ^ buffer
                                 -> VkDeviceSize -- ^ offset
                                                 -> IO ()

type PFN_vkCmdDispatchIndirect = FunPtr HS_vkCmdDispatchIndirect

foreign import ccall "dynamic" unwrapVkCmdDispatchIndirect ::
               PFN_vkCmdDispatchIndirect -> HS_vkCmdDispatchIndirect

instance VulkanProc "vkCmdDispatchIndirect" where
        type VkProcType "vkCmdDispatchIndirect" = HS_vkCmdDispatchIndirect
        vkProcSymbol = _VkCmdDispatchIndirect

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdDispatchIndirect

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdCopyBuffer :: CString

pattern VkCmdCopyBuffer <- (is_VkCmdCopyBuffer -> True)
  where VkCmdCopyBuffer = _VkCmdCopyBuffer

{-# INLINE _VkCmdCopyBuffer #-}

_VkCmdCopyBuffer :: CString
_VkCmdCopyBuffer = Ptr "vkCmdCopyBuffer\NUL"##

{-# INLINE is_VkCmdCopyBuffer #-}

is_VkCmdCopyBuffer :: CString -> Bool
is_VkCmdCopyBuffer = (EQ ==) . cmpCStrings _VkCmdCopyBuffer

type VkCmdCopyBuffer = "vkCmdCopyBuffer"

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdCopyBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer srcBuffer
--   >     , VkBuffer dstBuffer
--   >     , uint32_t regionCount
--   >     , const VkBufferCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdCopyBuffer.html vkCmdCopyBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkCmdCopyBuffer" vkCmdCopyBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ srcBuffer
                          -> VkBuffer -- ^ dstBuffer
                                      -> Word32 -- ^ regionCount
                                                -> Ptr VkBufferCopy -- ^ pRegions
                                                                    -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdCopyBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer srcBuffer
--   >     , VkBuffer dstBuffer
--   >     , uint32_t regionCount
--   >     , const VkBufferCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdCopyBuffer.html vkCmdCopyBuffer registry at www.khronos.org>
foreign import ccall safe "vkCmdCopyBuffer" vkCmdCopyBufferSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ srcBuffer
                          -> VkBuffer -- ^ dstBuffer
                                      -> Word32 -- ^ regionCount
                                                -> Ptr VkBufferCopy -- ^ pRegions
                                                                    -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdCopyBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer srcBuffer
--   >     , VkBuffer dstBuffer
--   >     , uint32_t regionCount
--   >     , const VkBufferCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdCopyBuffer.html vkCmdCopyBuffer registry at www.khronos.org>
type HS_vkCmdCopyBuffer =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkBuffer -- ^ srcBuffer
                -> VkBuffer -- ^ dstBuffer
                            -> Word32 -- ^ regionCount
                                      -> Ptr VkBufferCopy -- ^ pRegions
                                                          -> IO ()

type PFN_vkCmdCopyBuffer = FunPtr HS_vkCmdCopyBuffer

foreign import ccall "dynamic" unwrapVkCmdCopyBuffer ::
               PFN_vkCmdCopyBuffer -> HS_vkCmdCopyBuffer

instance VulkanProc "vkCmdCopyBuffer" where
        type VkProcType "vkCmdCopyBuffer" = HS_vkCmdCopyBuffer
        vkProcSymbol = _VkCmdCopyBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdCopyBuffer

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdCopyImage :: CString

pattern VkCmdCopyImage <- (is_VkCmdCopyImage -> True)
  where VkCmdCopyImage = _VkCmdCopyImage

{-# INLINE _VkCmdCopyImage #-}

_VkCmdCopyImage :: CString
_VkCmdCopyImage = Ptr "vkCmdCopyImage\NUL"##

{-# INLINE is_VkCmdCopyImage #-}

is_VkCmdCopyImage :: CString -> Bool
is_VkCmdCopyImage = (EQ ==) . cmpCStrings _VkCmdCopyImage

type VkCmdCopyImage = "vkCmdCopyImage"

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdCopyImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkImageCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdCopyImage.html vkCmdCopyImage registry at www.khronos.org>
foreign import ccall unsafe "vkCmdCopyImage" vkCmdCopyImage ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ srcImage
                         ->
                   VkImageLayout -- ^ srcImageLayout
                                 ->
                     VkImage -- ^ dstImage
                             -> VkImageLayout -- ^ dstImageLayout
                                              -> Word32 -- ^ regionCount
                                                        -> Ptr VkImageCopy -- ^ pRegions
                                                                           -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdCopyImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkImageCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdCopyImage.html vkCmdCopyImage registry at www.khronos.org>
foreign import ccall safe "vkCmdCopyImage" vkCmdCopyImageSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ srcImage
                         ->
                   VkImageLayout -- ^ srcImageLayout
                                 ->
                     VkImage -- ^ dstImage
                             -> VkImageLayout -- ^ dstImageLayout
                                              -> Word32 -- ^ regionCount
                                                        -> Ptr VkImageCopy -- ^ pRegions
                                                                           -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdCopyImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkImageCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdCopyImage.html vkCmdCopyImage registry at www.khronos.org>
type HS_vkCmdCopyImage =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkImage -- ^ srcImage
               ->
         VkImageLayout -- ^ srcImageLayout
                       ->
           VkImage -- ^ dstImage
                   -> VkImageLayout -- ^ dstImageLayout
                                    -> Word32 -- ^ regionCount
                                              -> Ptr VkImageCopy -- ^ pRegions
                                                                 -> IO ()

type PFN_vkCmdCopyImage = FunPtr HS_vkCmdCopyImage

foreign import ccall "dynamic" unwrapVkCmdCopyImage ::
               PFN_vkCmdCopyImage -> HS_vkCmdCopyImage

instance VulkanProc "vkCmdCopyImage" where
        type VkProcType "vkCmdCopyImage" = HS_vkCmdCopyImage
        vkProcSymbol = _VkCmdCopyImage

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdCopyImage

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdBlitImage :: CString

pattern VkCmdBlitImage <- (is_VkCmdBlitImage -> True)
  where VkCmdBlitImage = _VkCmdBlitImage

{-# INLINE _VkCmdBlitImage #-}

_VkCmdBlitImage :: CString
_VkCmdBlitImage = Ptr "vkCmdBlitImage\NUL"##

{-# INLINE is_VkCmdBlitImage #-}

is_VkCmdBlitImage :: CString -> Bool
is_VkCmdBlitImage = (EQ ==) . cmpCStrings _VkCmdBlitImage

type VkCmdBlitImage = "vkCmdBlitImage"

-- | queues: 'graphics'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdBlitImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkImageBlit* pRegions
--   >     , VkFilter filter
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBlitImage.html vkCmdBlitImage registry at www.khronos.org>
foreign import ccall unsafe "vkCmdBlitImage" vkCmdBlitImage ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ srcImage
                         ->
                   VkImageLayout -- ^ srcImageLayout
                                 ->
                     VkImage -- ^ dstImage
                             ->
                       VkImageLayout -- ^ dstImageLayout
                                     -> Word32 -- ^ regionCount
                                               -> Ptr VkImageBlit -- ^ pRegions
                                                                  -> VkFilter -- ^ filter
                                                                              -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdBlitImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkImageBlit* pRegions
--   >     , VkFilter filter
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBlitImage.html vkCmdBlitImage registry at www.khronos.org>
foreign import ccall safe "vkCmdBlitImage" vkCmdBlitImageSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ srcImage
                         ->
                   VkImageLayout -- ^ srcImageLayout
                                 ->
                     VkImage -- ^ dstImage
                             ->
                       VkImageLayout -- ^ dstImageLayout
                                     -> Word32 -- ^ regionCount
                                               -> Ptr VkImageBlit -- ^ pRegions
                                                                  -> VkFilter -- ^ filter
                                                                              -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdBlitImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkImageBlit* pRegions
--   >     , VkFilter filter
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBlitImage.html vkCmdBlitImage registry at www.khronos.org>
type HS_vkCmdBlitImage =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkImage -- ^ srcImage
               ->
         VkImageLayout -- ^ srcImageLayout
                       ->
           VkImage -- ^ dstImage
                   ->
             VkImageLayout -- ^ dstImageLayout
                           -> Word32 -- ^ regionCount
                                     -> Ptr VkImageBlit -- ^ pRegions
                                                        -> VkFilter -- ^ filter
                                                                    -> IO ()

type PFN_vkCmdBlitImage = FunPtr HS_vkCmdBlitImage

foreign import ccall "dynamic" unwrapVkCmdBlitImage ::
               PFN_vkCmdBlitImage -> HS_vkCmdBlitImage

instance VulkanProc "vkCmdBlitImage" where
        type VkProcType "vkCmdBlitImage" = HS_vkCmdBlitImage
        vkProcSymbol = _VkCmdBlitImage

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdBlitImage

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdCopyBufferToImage :: CString

pattern VkCmdCopyBufferToImage <-
        (is_VkCmdCopyBufferToImage -> True)
  where VkCmdCopyBufferToImage = _VkCmdCopyBufferToImage

{-# INLINE _VkCmdCopyBufferToImage #-}

_VkCmdCopyBufferToImage :: CString
_VkCmdCopyBufferToImage = Ptr "vkCmdCopyBufferToImage\NUL"##

{-# INLINE is_VkCmdCopyBufferToImage #-}

is_VkCmdCopyBufferToImage :: CString -> Bool
is_VkCmdCopyBufferToImage
  = (EQ ==) . cmpCStrings _VkCmdCopyBufferToImage

type VkCmdCopyBufferToImage = "vkCmdCopyBufferToImage"

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdCopyBufferToImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer srcBuffer
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkBufferImageCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdCopyBufferToImage.html vkCmdCopyBufferToImage registry at www.khronos.org>
foreign import ccall unsafe "vkCmdCopyBufferToImage"
               vkCmdCopyBufferToImage ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ srcBuffer
                          ->
                   VkImage -- ^ dstImage
                           ->
                     VkImageLayout -- ^ dstImageLayout
                                   -> Word32 -- ^ regionCount
                                             -> Ptr VkBufferImageCopy -- ^ pRegions
                                                                      -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdCopyBufferToImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer srcBuffer
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkBufferImageCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdCopyBufferToImage.html vkCmdCopyBufferToImage registry at www.khronos.org>
foreign import ccall safe "vkCmdCopyBufferToImage"
               vkCmdCopyBufferToImageSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ srcBuffer
                          ->
                   VkImage -- ^ dstImage
                           ->
                     VkImageLayout -- ^ dstImageLayout
                                   -> Word32 -- ^ regionCount
                                             -> Ptr VkBufferImageCopy -- ^ pRegions
                                                                      -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdCopyBufferToImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer srcBuffer
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkBufferImageCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdCopyBufferToImage.html vkCmdCopyBufferToImage registry at www.khronos.org>
type HS_vkCmdCopyBufferToImage =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkBuffer -- ^ srcBuffer
                ->
         VkImage -- ^ dstImage
                 ->
           VkImageLayout -- ^ dstImageLayout
                         -> Word32 -- ^ regionCount
                                   -> Ptr VkBufferImageCopy -- ^ pRegions
                                                            -> IO ()

type PFN_vkCmdCopyBufferToImage = FunPtr HS_vkCmdCopyBufferToImage

foreign import ccall "dynamic" unwrapVkCmdCopyBufferToImage ::
               PFN_vkCmdCopyBufferToImage -> HS_vkCmdCopyBufferToImage

instance VulkanProc "vkCmdCopyBufferToImage" where
        type VkProcType "vkCmdCopyBufferToImage" =
             HS_vkCmdCopyBufferToImage
        vkProcSymbol = _VkCmdCopyBufferToImage

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdCopyBufferToImage

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdCopyImageToBuffer :: CString

pattern VkCmdCopyImageToBuffer <-
        (is_VkCmdCopyImageToBuffer -> True)
  where VkCmdCopyImageToBuffer = _VkCmdCopyImageToBuffer

{-# INLINE _VkCmdCopyImageToBuffer #-}

_VkCmdCopyImageToBuffer :: CString
_VkCmdCopyImageToBuffer = Ptr "vkCmdCopyImageToBuffer\NUL"##

{-# INLINE is_VkCmdCopyImageToBuffer #-}

is_VkCmdCopyImageToBuffer :: CString -> Bool
is_VkCmdCopyImageToBuffer
  = (EQ ==) . cmpCStrings _VkCmdCopyImageToBuffer

type VkCmdCopyImageToBuffer = "vkCmdCopyImageToBuffer"

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdCopyImageToBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkBuffer dstBuffer
--   >     , uint32_t regionCount
--   >     , const VkBufferImageCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdCopyImageToBuffer.html vkCmdCopyImageToBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkCmdCopyImageToBuffer"
               vkCmdCopyImageToBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ srcImage
                         ->
                   VkImageLayout -- ^ srcImageLayout
                                 ->
                     VkBuffer -- ^ dstBuffer
                              -> Word32 -- ^ regionCount
                                        -> Ptr VkBufferImageCopy -- ^ pRegions
                                                                 -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdCopyImageToBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkBuffer dstBuffer
--   >     , uint32_t regionCount
--   >     , const VkBufferImageCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdCopyImageToBuffer.html vkCmdCopyImageToBuffer registry at www.khronos.org>
foreign import ccall safe "vkCmdCopyImageToBuffer"
               vkCmdCopyImageToBufferSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ srcImage
                         ->
                   VkImageLayout -- ^ srcImageLayout
                                 ->
                     VkBuffer -- ^ dstBuffer
                              -> Word32 -- ^ regionCount
                                        -> Ptr VkBufferImageCopy -- ^ pRegions
                                                                 -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdCopyImageToBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkBuffer dstBuffer
--   >     , uint32_t regionCount
--   >     , const VkBufferImageCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdCopyImageToBuffer.html vkCmdCopyImageToBuffer registry at www.khronos.org>
type HS_vkCmdCopyImageToBuffer =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkImage -- ^ srcImage
               ->
         VkImageLayout -- ^ srcImageLayout
                       ->
           VkBuffer -- ^ dstBuffer
                    -> Word32 -- ^ regionCount
                              -> Ptr VkBufferImageCopy -- ^ pRegions
                                                       -> IO ()

type PFN_vkCmdCopyImageToBuffer = FunPtr HS_vkCmdCopyImageToBuffer

foreign import ccall "dynamic" unwrapVkCmdCopyImageToBuffer ::
               PFN_vkCmdCopyImageToBuffer -> HS_vkCmdCopyImageToBuffer

instance VulkanProc "vkCmdCopyImageToBuffer" where
        type VkProcType "vkCmdCopyImageToBuffer" =
             HS_vkCmdCopyImageToBuffer
        vkProcSymbol = _VkCmdCopyImageToBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdCopyImageToBuffer

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdUpdateBuffer :: CString

pattern VkCmdUpdateBuffer <- (is_VkCmdUpdateBuffer -> True)
  where VkCmdUpdateBuffer = _VkCmdUpdateBuffer

{-# INLINE _VkCmdUpdateBuffer #-}

_VkCmdUpdateBuffer :: CString
_VkCmdUpdateBuffer = Ptr "vkCmdUpdateBuffer\NUL"##

{-# INLINE is_VkCmdUpdateBuffer #-}

is_VkCmdUpdateBuffer :: CString -> Bool
is_VkCmdUpdateBuffer = (EQ ==) . cmpCStrings _VkCmdUpdateBuffer

type VkCmdUpdateBuffer = "vkCmdUpdateBuffer"

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdUpdateBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , VkDeviceSize dataSize
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdUpdateBuffer.html vkCmdUpdateBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkCmdUpdateBuffer" vkCmdUpdateBuffer
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ dstBuffer
                          -> VkDeviceSize -- ^ dstOffset
                                          -> VkDeviceSize -- ^ dataSize
                                                          -> Ptr Void -- ^ pData
                                                                      -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdUpdateBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , VkDeviceSize dataSize
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdUpdateBuffer.html vkCmdUpdateBuffer registry at www.khronos.org>
foreign import ccall safe "vkCmdUpdateBuffer" vkCmdUpdateBufferSafe
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ dstBuffer
                          -> VkDeviceSize -- ^ dstOffset
                                          -> VkDeviceSize -- ^ dataSize
                                                          -> Ptr Void -- ^ pData
                                                                      -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdUpdateBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , VkDeviceSize dataSize
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdUpdateBuffer.html vkCmdUpdateBuffer registry at www.khronos.org>
type HS_vkCmdUpdateBuffer =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkBuffer -- ^ dstBuffer
                -> VkDeviceSize -- ^ dstOffset
                                -> VkDeviceSize -- ^ dataSize
                                                -> Ptr Void -- ^ pData
                                                            -> IO ()

type PFN_vkCmdUpdateBuffer = FunPtr HS_vkCmdUpdateBuffer

foreign import ccall "dynamic" unwrapVkCmdUpdateBuffer ::
               PFN_vkCmdUpdateBuffer -> HS_vkCmdUpdateBuffer

instance VulkanProc "vkCmdUpdateBuffer" where
        type VkProcType "vkCmdUpdateBuffer" = HS_vkCmdUpdateBuffer
        vkProcSymbol = _VkCmdUpdateBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdUpdateBuffer

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdFillBuffer :: CString

pattern VkCmdFillBuffer <- (is_VkCmdFillBuffer -> True)
  where VkCmdFillBuffer = _VkCmdFillBuffer

{-# INLINE _VkCmdFillBuffer #-}

_VkCmdFillBuffer :: CString
_VkCmdFillBuffer = Ptr "vkCmdFillBuffer\NUL"##

{-# INLINE is_VkCmdFillBuffer #-}

is_VkCmdFillBuffer :: CString -> Bool
is_VkCmdFillBuffer = (EQ ==) . cmpCStrings _VkCmdFillBuffer

type VkCmdFillBuffer = "vkCmdFillBuffer"

-- | transfer support is only available when VK_KHR_maintenance1 is enabled, as documented in valid usage language in the specification
--
--   queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdFillBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , VkDeviceSize size
--   >     , uint32_t data
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdFillBuffer.html vkCmdFillBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkCmdFillBuffer" vkCmdFillBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ dstBuffer
                          -> VkDeviceSize -- ^ dstOffset
                                          -> VkDeviceSize -- ^ size
                                                          -> Word32 -- ^ data
                                                                    -> IO ()

-- | transfer support is only available when VK_KHR_maintenance1 is enabled, as documented in valid usage language in the specification
--
--   queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdFillBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , VkDeviceSize size
--   >     , uint32_t data
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdFillBuffer.html vkCmdFillBuffer registry at www.khronos.org>
foreign import ccall safe "vkCmdFillBuffer" vkCmdFillBufferSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ dstBuffer
                          -> VkDeviceSize -- ^ dstOffset
                                          -> VkDeviceSize -- ^ size
                                                          -> Word32 -- ^ data
                                                                    -> IO ()

-- | transfer support is only available when VK_KHR_maintenance1 is enabled, as documented in valid usage language in the specification
--
--   queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdFillBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , VkDeviceSize size
--   >     , uint32_t data
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdFillBuffer.html vkCmdFillBuffer registry at www.khronos.org>
type HS_vkCmdFillBuffer =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkBuffer -- ^ dstBuffer
                -> VkDeviceSize -- ^ dstOffset
                                -> VkDeviceSize -- ^ size
                                                -> Word32 -- ^ data
                                                          -> IO ()

type PFN_vkCmdFillBuffer = FunPtr HS_vkCmdFillBuffer

foreign import ccall "dynamic" unwrapVkCmdFillBuffer ::
               PFN_vkCmdFillBuffer -> HS_vkCmdFillBuffer

instance VulkanProc "vkCmdFillBuffer" where
        type VkProcType "vkCmdFillBuffer" = HS_vkCmdFillBuffer
        vkProcSymbol = _VkCmdFillBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdFillBuffer

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdClearColorImage :: CString

pattern VkCmdClearColorImage <- (is_VkCmdClearColorImage -> True)
  where VkCmdClearColorImage = _VkCmdClearColorImage

{-# INLINE _VkCmdClearColorImage #-}

_VkCmdClearColorImage :: CString
_VkCmdClearColorImage = Ptr "vkCmdClearColorImage\NUL"##

{-# INLINE is_VkCmdClearColorImage #-}

is_VkCmdClearColorImage :: CString -> Bool
is_VkCmdClearColorImage
  = (EQ ==) . cmpCStrings _VkCmdClearColorImage

type VkCmdClearColorImage = "vkCmdClearColorImage"

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdClearColorImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage image
--   >     , VkImageLayout imageLayout
--   >     , const VkClearColorValue* pColor
--   >     , uint32_t rangeCount
--   >     , const VkImageSubresourceRange* pRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdClearColorImage.html vkCmdClearColorImage registry at www.khronos.org>
foreign import ccall unsafe "vkCmdClearColorImage"
               vkCmdClearColorImage ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ image
                         ->
                   VkImageLayout -- ^ imageLayout
                                 ->
                     Ptr VkClearColorValue -- ^ pColor
                                           ->
                       Word32 -- ^ rangeCount
                              -> Ptr VkImageSubresourceRange -- ^ pRanges
                                                             -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdClearColorImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage image
--   >     , VkImageLayout imageLayout
--   >     , const VkClearColorValue* pColor
--   >     , uint32_t rangeCount
--   >     , const VkImageSubresourceRange* pRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdClearColorImage.html vkCmdClearColorImage registry at www.khronos.org>
foreign import ccall safe "vkCmdClearColorImage"
               vkCmdClearColorImageSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ image
                         ->
                   VkImageLayout -- ^ imageLayout
                                 ->
                     Ptr VkClearColorValue -- ^ pColor
                                           ->
                       Word32 -- ^ rangeCount
                              -> Ptr VkImageSubresourceRange -- ^ pRanges
                                                             -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdClearColorImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage image
--   >     , VkImageLayout imageLayout
--   >     , const VkClearColorValue* pColor
--   >     , uint32_t rangeCount
--   >     , const VkImageSubresourceRange* pRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdClearColorImage.html vkCmdClearColorImage registry at www.khronos.org>
type HS_vkCmdClearColorImage =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkImage -- ^ image
               ->
         VkImageLayout -- ^ imageLayout
                       ->
           Ptr VkClearColorValue -- ^ pColor
                                 ->
             Word32 -- ^ rangeCount
                    -> Ptr VkImageSubresourceRange -- ^ pRanges
                                                   -> IO ()

type PFN_vkCmdClearColorImage = FunPtr HS_vkCmdClearColorImage

foreign import ccall "dynamic" unwrapVkCmdClearColorImage ::
               PFN_vkCmdClearColorImage -> HS_vkCmdClearColorImage

instance VulkanProc "vkCmdClearColorImage" where
        type VkProcType "vkCmdClearColorImage" = HS_vkCmdClearColorImage
        vkProcSymbol = _VkCmdClearColorImage

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdClearColorImage

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdClearDepthStencilImage :: CString

pattern VkCmdClearDepthStencilImage <-
        (is_VkCmdClearDepthStencilImage -> True)
  where VkCmdClearDepthStencilImage = _VkCmdClearDepthStencilImage

{-# INLINE _VkCmdClearDepthStencilImage #-}

_VkCmdClearDepthStencilImage :: CString
_VkCmdClearDepthStencilImage
  = Ptr "vkCmdClearDepthStencilImage\NUL"##

{-# INLINE is_VkCmdClearDepthStencilImage #-}

is_VkCmdClearDepthStencilImage :: CString -> Bool
is_VkCmdClearDepthStencilImage
  = (EQ ==) . cmpCStrings _VkCmdClearDepthStencilImage

type VkCmdClearDepthStencilImage = "vkCmdClearDepthStencilImage"

-- | queues: 'graphics'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdClearDepthStencilImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage image
--   >     , VkImageLayout imageLayout
--   >     , const VkClearDepthStencilValue* pDepthStencil
--   >     , uint32_t rangeCount
--   >     , const VkImageSubresourceRange* pRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdClearDepthStencilImage.html vkCmdClearDepthStencilImage registry at www.khronos.org>
foreign import ccall unsafe "vkCmdClearDepthStencilImage"
               vkCmdClearDepthStencilImage ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ image
                         ->
                   VkImageLayout -- ^ imageLayout
                                 ->
                     Ptr VkClearDepthStencilValue -- ^ pDepthStencil
                                                  ->
                       Word32 -- ^ rangeCount
                              -> Ptr VkImageSubresourceRange -- ^ pRanges
                                                             -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdClearDepthStencilImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage image
--   >     , VkImageLayout imageLayout
--   >     , const VkClearDepthStencilValue* pDepthStencil
--   >     , uint32_t rangeCount
--   >     , const VkImageSubresourceRange* pRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdClearDepthStencilImage.html vkCmdClearDepthStencilImage registry at www.khronos.org>
foreign import ccall safe "vkCmdClearDepthStencilImage"
               vkCmdClearDepthStencilImageSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ image
                         ->
                   VkImageLayout -- ^ imageLayout
                                 ->
                     Ptr VkClearDepthStencilValue -- ^ pDepthStencil
                                                  ->
                       Word32 -- ^ rangeCount
                              -> Ptr VkImageSubresourceRange -- ^ pRanges
                                                             -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdClearDepthStencilImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage image
--   >     , VkImageLayout imageLayout
--   >     , const VkClearDepthStencilValue* pDepthStencil
--   >     , uint32_t rangeCount
--   >     , const VkImageSubresourceRange* pRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdClearDepthStencilImage.html vkCmdClearDepthStencilImage registry at www.khronos.org>
type HS_vkCmdClearDepthStencilImage =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkImage -- ^ image
               ->
         VkImageLayout -- ^ imageLayout
                       ->
           Ptr VkClearDepthStencilValue -- ^ pDepthStencil
                                        ->
             Word32 -- ^ rangeCount
                    -> Ptr VkImageSubresourceRange -- ^ pRanges
                                                   -> IO ()

type PFN_vkCmdClearDepthStencilImage =
     FunPtr HS_vkCmdClearDepthStencilImage

foreign import ccall "dynamic" unwrapVkCmdClearDepthStencilImage ::
               PFN_vkCmdClearDepthStencilImage -> HS_vkCmdClearDepthStencilImage

instance VulkanProc "vkCmdClearDepthStencilImage" where
        type VkProcType "vkCmdClearDepthStencilImage" =
             HS_vkCmdClearDepthStencilImage
        vkProcSymbol = _VkCmdClearDepthStencilImage

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdClearDepthStencilImage

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdClearAttachments :: CString

pattern VkCmdClearAttachments <- (is_VkCmdClearAttachments -> True)
  where VkCmdClearAttachments = _VkCmdClearAttachments

{-# INLINE _VkCmdClearAttachments #-}

_VkCmdClearAttachments :: CString
_VkCmdClearAttachments = Ptr "vkCmdClearAttachments\NUL"##

{-# INLINE is_VkCmdClearAttachments #-}

is_VkCmdClearAttachments :: CString -> Bool
is_VkCmdClearAttachments
  = (EQ ==) . cmpCStrings _VkCmdClearAttachments

type VkCmdClearAttachments = "vkCmdClearAttachments"

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdClearAttachments
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t attachmentCount
--   >     , const VkClearAttachment* pAttachments
--   >     , uint32_t rectCount
--   >     , const VkClearRect* pRects
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdClearAttachments.html vkCmdClearAttachments registry at www.khronos.org>
foreign import ccall unsafe "vkCmdClearAttachments"
               vkCmdClearAttachments ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ attachmentCount
                        ->
                   Ptr VkClearAttachment -- ^ pAttachments
                                         -> Word32 -- ^ rectCount
                                                   -> Ptr VkClearRect -- ^ pRects
                                                                      -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdClearAttachments
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t attachmentCount
--   >     , const VkClearAttachment* pAttachments
--   >     , uint32_t rectCount
--   >     , const VkClearRect* pRects
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdClearAttachments.html vkCmdClearAttachments registry at www.khronos.org>
foreign import ccall safe "vkCmdClearAttachments"
               vkCmdClearAttachmentsSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ attachmentCount
                        ->
                   Ptr VkClearAttachment -- ^ pAttachments
                                         -> Word32 -- ^ rectCount
                                                   -> Ptr VkClearRect -- ^ pRects
                                                                      -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdClearAttachments
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t attachmentCount
--   >     , const VkClearAttachment* pAttachments
--   >     , uint32_t rectCount
--   >     , const VkClearRect* pRects
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdClearAttachments.html vkCmdClearAttachments registry at www.khronos.org>
type HS_vkCmdClearAttachments =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ attachmentCount
              ->
         Ptr VkClearAttachment -- ^ pAttachments
                               -> Word32 -- ^ rectCount
                                         -> Ptr VkClearRect -- ^ pRects
                                                            -> IO ()

type PFN_vkCmdClearAttachments = FunPtr HS_vkCmdClearAttachments

foreign import ccall "dynamic" unwrapVkCmdClearAttachments ::
               PFN_vkCmdClearAttachments -> HS_vkCmdClearAttachments

instance VulkanProc "vkCmdClearAttachments" where
        type VkProcType "vkCmdClearAttachments" = HS_vkCmdClearAttachments
        vkProcSymbol = _VkCmdClearAttachments

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdClearAttachments

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdResolveImage :: CString

pattern VkCmdResolveImage <- (is_VkCmdResolveImage -> True)
  where VkCmdResolveImage = _VkCmdResolveImage

{-# INLINE _VkCmdResolveImage #-}

_VkCmdResolveImage :: CString
_VkCmdResolveImage = Ptr "vkCmdResolveImage\NUL"##

{-# INLINE is_VkCmdResolveImage #-}

is_VkCmdResolveImage :: CString -> Bool
is_VkCmdResolveImage = (EQ ==) . cmpCStrings _VkCmdResolveImage

type VkCmdResolveImage = "vkCmdResolveImage"

-- | queues: 'graphics'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdResolveImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkImageResolve* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdResolveImage.html vkCmdResolveImage registry at www.khronos.org>
foreign import ccall unsafe "vkCmdResolveImage" vkCmdResolveImage
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ srcImage
                         ->
                   VkImageLayout -- ^ srcImageLayout
                                 ->
                     VkImage -- ^ dstImage
                             -> VkImageLayout -- ^ dstImageLayout
                                              -> Word32 -- ^ regionCount
                                                        -> Ptr VkImageResolve -- ^ pRegions
                                                                              -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdResolveImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkImageResolve* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdResolveImage.html vkCmdResolveImage registry at www.khronos.org>
foreign import ccall safe "vkCmdResolveImage" vkCmdResolveImageSafe
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ srcImage
                         ->
                   VkImageLayout -- ^ srcImageLayout
                                 ->
                     VkImage -- ^ dstImage
                             -> VkImageLayout -- ^ dstImageLayout
                                              -> Word32 -- ^ regionCount
                                                        -> Ptr VkImageResolve -- ^ pRegions
                                                                              -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdResolveImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkImageResolve* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdResolveImage.html vkCmdResolveImage registry at www.khronos.org>
type HS_vkCmdResolveImage =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkImage -- ^ srcImage
               ->
         VkImageLayout -- ^ srcImageLayout
                       ->
           VkImage -- ^ dstImage
                   -> VkImageLayout -- ^ dstImageLayout
                                    -> Word32 -- ^ regionCount
                                              -> Ptr VkImageResolve -- ^ pRegions
                                                                    -> IO ()

type PFN_vkCmdResolveImage = FunPtr HS_vkCmdResolveImage

foreign import ccall "dynamic" unwrapVkCmdResolveImage ::
               PFN_vkCmdResolveImage -> HS_vkCmdResolveImage

instance VulkanProc "vkCmdResolveImage" where
        type VkProcType "vkCmdResolveImage" = HS_vkCmdResolveImage
        vkProcSymbol = _VkCmdResolveImage

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdResolveImage

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdSetEvent :: CString

pattern VkCmdSetEvent <- (is_VkCmdSetEvent -> True)
  where VkCmdSetEvent = _VkCmdSetEvent

{-# INLINE _VkCmdSetEvent #-}

_VkCmdSetEvent :: CString
_VkCmdSetEvent = Ptr "vkCmdSetEvent\NUL"##

{-# INLINE is_VkCmdSetEvent #-}

is_VkCmdSetEvent :: CString -> Bool
is_VkCmdSetEvent = (EQ ==) . cmpCStrings _VkCmdSetEvent

type VkCmdSetEvent = "vkCmdSetEvent"

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   > () vkCmdSetEvent
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkEvent event
--   >     , VkPipelineStageFlags stageMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetEvent.html vkCmdSetEvent registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetEvent" vkCmdSetEvent ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkEvent -- ^ event
                                          -> VkPipelineStageFlags -- ^ stageMask
                                                                  -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   > () vkCmdSetEvent
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkEvent event
--   >     , VkPipelineStageFlags stageMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetEvent.html vkCmdSetEvent registry at www.khronos.org>
foreign import ccall safe "vkCmdSetEvent" vkCmdSetEventSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkEvent -- ^ event
                                          -> VkPipelineStageFlags -- ^ stageMask
                                                                  -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   > () vkCmdSetEvent
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkEvent event
--   >     , VkPipelineStageFlags stageMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdSetEvent.html vkCmdSetEvent registry at www.khronos.org>
type HS_vkCmdSetEvent =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkEvent -- ^ event
                                -> VkPipelineStageFlags -- ^ stageMask
                                                        -> IO ()

type PFN_vkCmdSetEvent = FunPtr HS_vkCmdSetEvent

foreign import ccall "dynamic" unwrapVkCmdSetEvent ::
               PFN_vkCmdSetEvent -> HS_vkCmdSetEvent

instance VulkanProc "vkCmdSetEvent" where
        type VkProcType "vkCmdSetEvent" = HS_vkCmdSetEvent
        vkProcSymbol = _VkCmdSetEvent

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetEvent

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdResetEvent :: CString

pattern VkCmdResetEvent <- (is_VkCmdResetEvent -> True)
  where VkCmdResetEvent = _VkCmdResetEvent

{-# INLINE _VkCmdResetEvent #-}

_VkCmdResetEvent :: CString
_VkCmdResetEvent = Ptr "vkCmdResetEvent\NUL"##

{-# INLINE is_VkCmdResetEvent #-}

is_VkCmdResetEvent :: CString -> Bool
is_VkCmdResetEvent = (EQ ==) . cmpCStrings _VkCmdResetEvent

type VkCmdResetEvent = "vkCmdResetEvent"

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   > () vkCmdResetEvent
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkEvent event
--   >     , VkPipelineStageFlags stageMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdResetEvent.html vkCmdResetEvent registry at www.khronos.org>
foreign import ccall unsafe "vkCmdResetEvent" vkCmdResetEvent ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkEvent -- ^ event
                                          -> VkPipelineStageFlags -- ^ stageMask
                                                                  -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   > () vkCmdResetEvent
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkEvent event
--   >     , VkPipelineStageFlags stageMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdResetEvent.html vkCmdResetEvent registry at www.khronos.org>
foreign import ccall safe "vkCmdResetEvent" vkCmdResetEventSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkEvent -- ^ event
                                          -> VkPipelineStageFlags -- ^ stageMask
                                                                  -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   > () vkCmdResetEvent
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkEvent event
--   >     , VkPipelineStageFlags stageMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdResetEvent.html vkCmdResetEvent registry at www.khronos.org>
type HS_vkCmdResetEvent =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkEvent -- ^ event
                                -> VkPipelineStageFlags -- ^ stageMask
                                                        -> IO ()

type PFN_vkCmdResetEvent = FunPtr HS_vkCmdResetEvent

foreign import ccall "dynamic" unwrapVkCmdResetEvent ::
               PFN_vkCmdResetEvent -> HS_vkCmdResetEvent

instance VulkanProc "vkCmdResetEvent" where
        type VkProcType "vkCmdResetEvent" = HS_vkCmdResetEvent
        vkProcSymbol = _VkCmdResetEvent

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdResetEvent

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdWaitEvents :: CString

pattern VkCmdWaitEvents <- (is_VkCmdWaitEvents -> True)
  where VkCmdWaitEvents = _VkCmdWaitEvents

{-# INLINE _VkCmdWaitEvents #-}

_VkCmdWaitEvents :: CString
_VkCmdWaitEvents = Ptr "vkCmdWaitEvents\NUL"##

{-# INLINE is_VkCmdWaitEvents #-}

is_VkCmdWaitEvents :: CString -> Bool
is_VkCmdWaitEvents = (EQ ==) . cmpCStrings _VkCmdWaitEvents

type VkCmdWaitEvents = "vkCmdWaitEvents"

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdWaitEvents
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t eventCount
--   >     , const VkEvent* pEvents
--   >     , VkPipelineStageFlags srcStageMask
--   >     , VkPipelineStageFlags dstStageMask
--   >     , uint32_t memoryBarrierCount
--   >     , const VkMemoryBarrier* pMemoryBarriers
--   >     , uint32_t bufferMemoryBarrierCount
--   >     , const VkBufferMemoryBarrier* pBufferMemoryBarriers
--   >     , uint32_t imageMemoryBarrierCount
--   >     , const VkImageMemoryBarrier* pImageMemoryBarriers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdWaitEvents.html vkCmdWaitEvents registry at www.khronos.org>
foreign import ccall unsafe "vkCmdWaitEvents" vkCmdWaitEvents ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ eventCount
                        ->
                   Ptr VkEvent -- ^ pEvents
                               ->
                     VkPipelineStageFlags -- ^ srcStageMask
                                          ->
                       VkPipelineStageFlags -- ^ dstStageMask
                                            ->
                         Word32 -- ^ memoryBarrierCount
                                ->
                           Ptr VkMemoryBarrier -- ^ pMemoryBarriers
                                               ->
                             Word32 -- ^ bufferMemoryBarrierCount
                                    ->
                               Ptr VkBufferMemoryBarrier -- ^ pBufferMemoryBarriers
                                                         ->
                                 Word32 -- ^ imageMemoryBarrierCount
                                        -> Ptr VkImageMemoryBarrier -- ^ pImageMemoryBarriers
                                                                    -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdWaitEvents
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t eventCount
--   >     , const VkEvent* pEvents
--   >     , VkPipelineStageFlags srcStageMask
--   >     , VkPipelineStageFlags dstStageMask
--   >     , uint32_t memoryBarrierCount
--   >     , const VkMemoryBarrier* pMemoryBarriers
--   >     , uint32_t bufferMemoryBarrierCount
--   >     , const VkBufferMemoryBarrier* pBufferMemoryBarriers
--   >     , uint32_t imageMemoryBarrierCount
--   >     , const VkImageMemoryBarrier* pImageMemoryBarriers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdWaitEvents.html vkCmdWaitEvents registry at www.khronos.org>
foreign import ccall safe "vkCmdWaitEvents" vkCmdWaitEventsSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ eventCount
                        ->
                   Ptr VkEvent -- ^ pEvents
                               ->
                     VkPipelineStageFlags -- ^ srcStageMask
                                          ->
                       VkPipelineStageFlags -- ^ dstStageMask
                                            ->
                         Word32 -- ^ memoryBarrierCount
                                ->
                           Ptr VkMemoryBarrier -- ^ pMemoryBarriers
                                               ->
                             Word32 -- ^ bufferMemoryBarrierCount
                                    ->
                               Ptr VkBufferMemoryBarrier -- ^ pBufferMemoryBarriers
                                                         ->
                                 Word32 -- ^ imageMemoryBarrierCount
                                        -> Ptr VkImageMemoryBarrier -- ^ pImageMemoryBarriers
                                                                    -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdWaitEvents
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t eventCount
--   >     , const VkEvent* pEvents
--   >     , VkPipelineStageFlags srcStageMask
--   >     , VkPipelineStageFlags dstStageMask
--   >     , uint32_t memoryBarrierCount
--   >     , const VkMemoryBarrier* pMemoryBarriers
--   >     , uint32_t bufferMemoryBarrierCount
--   >     , const VkBufferMemoryBarrier* pBufferMemoryBarriers
--   >     , uint32_t imageMemoryBarrierCount
--   >     , const VkImageMemoryBarrier* pImageMemoryBarriers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdWaitEvents.html vkCmdWaitEvents registry at www.khronos.org>
type HS_vkCmdWaitEvents =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ eventCount
              ->
         Ptr VkEvent -- ^ pEvents
                     ->
           VkPipelineStageFlags -- ^ srcStageMask
                                ->
             VkPipelineStageFlags -- ^ dstStageMask
                                  ->
               Word32 -- ^ memoryBarrierCount
                      ->
                 Ptr VkMemoryBarrier -- ^ pMemoryBarriers
                                     ->
                   Word32 -- ^ bufferMemoryBarrierCount
                          ->
                     Ptr VkBufferMemoryBarrier -- ^ pBufferMemoryBarriers
                                               ->
                       Word32 -- ^ imageMemoryBarrierCount
                              -> Ptr VkImageMemoryBarrier -- ^ pImageMemoryBarriers
                                                          -> IO ()

type PFN_vkCmdWaitEvents = FunPtr HS_vkCmdWaitEvents

foreign import ccall "dynamic" unwrapVkCmdWaitEvents ::
               PFN_vkCmdWaitEvents -> HS_vkCmdWaitEvents

instance VulkanProc "vkCmdWaitEvents" where
        type VkProcType "vkCmdWaitEvents" = HS_vkCmdWaitEvents
        vkProcSymbol = _VkCmdWaitEvents

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdWaitEvents

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdPipelineBarrier :: CString

pattern VkCmdPipelineBarrier <- (is_VkCmdPipelineBarrier -> True)
  where VkCmdPipelineBarrier = _VkCmdPipelineBarrier

{-# INLINE _VkCmdPipelineBarrier #-}

_VkCmdPipelineBarrier :: CString
_VkCmdPipelineBarrier = Ptr "vkCmdPipelineBarrier\NUL"##

{-# INLINE is_VkCmdPipelineBarrier #-}

is_VkCmdPipelineBarrier :: CString -> Bool
is_VkCmdPipelineBarrier
  = (EQ ==) . cmpCStrings _VkCmdPipelineBarrier

type VkCmdPipelineBarrier = "vkCmdPipelineBarrier"

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdPipelineBarrier
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineStageFlags srcStageMask
--   >     , VkPipelineStageFlags dstStageMask
--   >     , VkDependencyFlags dependencyFlags
--   >     , uint32_t memoryBarrierCount
--   >     , const VkMemoryBarrier* pMemoryBarriers
--   >     , uint32_t bufferMemoryBarrierCount
--   >     , const VkBufferMemoryBarrier* pBufferMemoryBarriers
--   >     , uint32_t imageMemoryBarrierCount
--   >     , const VkImageMemoryBarrier* pImageMemoryBarriers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdPipelineBarrier.html vkCmdPipelineBarrier registry at www.khronos.org>
foreign import ccall unsafe "vkCmdPipelineBarrier"
               vkCmdPipelineBarrier ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineStageFlags -- ^ srcStageMask
                                      ->
                   VkPipelineStageFlags -- ^ dstStageMask
                                        ->
                     VkDependencyFlags -- ^ dependencyFlags
                                       ->
                       Word32 -- ^ memoryBarrierCount
                              ->
                         Ptr VkMemoryBarrier -- ^ pMemoryBarriers
                                             ->
                           Word32 -- ^ bufferMemoryBarrierCount
                                  ->
                             Ptr VkBufferMemoryBarrier -- ^ pBufferMemoryBarriers
                                                       ->
                               Word32 -- ^ imageMemoryBarrierCount
                                      -> Ptr VkImageMemoryBarrier -- ^ pImageMemoryBarriers
                                                                  -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdPipelineBarrier
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineStageFlags srcStageMask
--   >     , VkPipelineStageFlags dstStageMask
--   >     , VkDependencyFlags dependencyFlags
--   >     , uint32_t memoryBarrierCount
--   >     , const VkMemoryBarrier* pMemoryBarriers
--   >     , uint32_t bufferMemoryBarrierCount
--   >     , const VkBufferMemoryBarrier* pBufferMemoryBarriers
--   >     , uint32_t imageMemoryBarrierCount
--   >     , const VkImageMemoryBarrier* pImageMemoryBarriers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdPipelineBarrier.html vkCmdPipelineBarrier registry at www.khronos.org>
foreign import ccall safe "vkCmdPipelineBarrier"
               vkCmdPipelineBarrierSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineStageFlags -- ^ srcStageMask
                                      ->
                   VkPipelineStageFlags -- ^ dstStageMask
                                        ->
                     VkDependencyFlags -- ^ dependencyFlags
                                       ->
                       Word32 -- ^ memoryBarrierCount
                              ->
                         Ptr VkMemoryBarrier -- ^ pMemoryBarriers
                                             ->
                           Word32 -- ^ bufferMemoryBarrierCount
                                  ->
                             Ptr VkBufferMemoryBarrier -- ^ pBufferMemoryBarriers
                                                       ->
                               Word32 -- ^ imageMemoryBarrierCount
                                      -> Ptr VkImageMemoryBarrier -- ^ pImageMemoryBarriers
                                                                  -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdPipelineBarrier
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineStageFlags srcStageMask
--   >     , VkPipelineStageFlags dstStageMask
--   >     , VkDependencyFlags dependencyFlags
--   >     , uint32_t memoryBarrierCount
--   >     , const VkMemoryBarrier* pMemoryBarriers
--   >     , uint32_t bufferMemoryBarrierCount
--   >     , const VkBufferMemoryBarrier* pBufferMemoryBarriers
--   >     , uint32_t imageMemoryBarrierCount
--   >     , const VkImageMemoryBarrier* pImageMemoryBarriers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdPipelineBarrier.html vkCmdPipelineBarrier registry at www.khronos.org>
type HS_vkCmdPipelineBarrier =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkPipelineStageFlags -- ^ srcStageMask
                            ->
         VkPipelineStageFlags -- ^ dstStageMask
                              ->
           VkDependencyFlags -- ^ dependencyFlags
                             ->
             Word32 -- ^ memoryBarrierCount
                    ->
               Ptr VkMemoryBarrier -- ^ pMemoryBarriers
                                   ->
                 Word32 -- ^ bufferMemoryBarrierCount
                        ->
                   Ptr VkBufferMemoryBarrier -- ^ pBufferMemoryBarriers
                                             ->
                     Word32 -- ^ imageMemoryBarrierCount
                            -> Ptr VkImageMemoryBarrier -- ^ pImageMemoryBarriers
                                                        -> IO ()

type PFN_vkCmdPipelineBarrier = FunPtr HS_vkCmdPipelineBarrier

foreign import ccall "dynamic" unwrapVkCmdPipelineBarrier ::
               PFN_vkCmdPipelineBarrier -> HS_vkCmdPipelineBarrier

instance VulkanProc "vkCmdPipelineBarrier" where
        type VkProcType "vkCmdPipelineBarrier" = HS_vkCmdPipelineBarrier
        vkProcSymbol = _VkCmdPipelineBarrier

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdPipelineBarrier

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdBeginQuery :: CString

pattern VkCmdBeginQuery <- (is_VkCmdBeginQuery -> True)
  where VkCmdBeginQuery = _VkCmdBeginQuery

{-# INLINE _VkCmdBeginQuery #-}

_VkCmdBeginQuery :: CString
_VkCmdBeginQuery = Ptr "vkCmdBeginQuery\NUL"##

{-# INLINE is_VkCmdBeginQuery #-}

is_VkCmdBeginQuery :: CString -> Bool
is_VkCmdBeginQuery = (EQ ==) . cmpCStrings _VkCmdBeginQuery

type VkCmdBeginQuery = "vkCmdBeginQuery"

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdBeginQuery
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     , VkQueryControlFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBeginQuery.html vkCmdBeginQuery registry at www.khronos.org>
foreign import ccall unsafe "vkCmdBeginQuery" vkCmdBeginQuery ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkQueryPool -- ^ queryPool
                             -> Word32 -- ^ query
                                       -> VkQueryControlFlags -- ^ flags
                                                              -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdBeginQuery
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     , VkQueryControlFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBeginQuery.html vkCmdBeginQuery registry at www.khronos.org>
foreign import ccall safe "vkCmdBeginQuery" vkCmdBeginQuerySafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkQueryPool -- ^ queryPool
                             -> Word32 -- ^ query
                                       -> VkQueryControlFlags -- ^ flags
                                                              -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdBeginQuery
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     , VkQueryControlFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBeginQuery.html vkCmdBeginQuery registry at www.khronos.org>
type HS_vkCmdBeginQuery =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkQueryPool -- ^ queryPool
                   -> Word32 -- ^ query
                             -> VkQueryControlFlags -- ^ flags
                                                    -> IO ()

type PFN_vkCmdBeginQuery = FunPtr HS_vkCmdBeginQuery

foreign import ccall "dynamic" unwrapVkCmdBeginQuery ::
               PFN_vkCmdBeginQuery -> HS_vkCmdBeginQuery

instance VulkanProc "vkCmdBeginQuery" where
        type VkProcType "vkCmdBeginQuery" = HS_vkCmdBeginQuery
        vkProcSymbol = _VkCmdBeginQuery

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdBeginQuery

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdEndQuery :: CString

pattern VkCmdEndQuery <- (is_VkCmdEndQuery -> True)
  where VkCmdEndQuery = _VkCmdEndQuery

{-# INLINE _VkCmdEndQuery #-}

_VkCmdEndQuery :: CString
_VkCmdEndQuery = Ptr "vkCmdEndQuery\NUL"##

{-# INLINE is_VkCmdEndQuery #-}

is_VkCmdEndQuery :: CString -> Bool
is_VkCmdEndQuery = (EQ ==) . cmpCStrings _VkCmdEndQuery

type VkCmdEndQuery = "vkCmdEndQuery"

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdEndQuery
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdEndQuery.html vkCmdEndQuery registry at www.khronos.org>
foreign import ccall unsafe "vkCmdEndQuery" vkCmdEndQuery ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkQueryPool -- ^ queryPool
                                              -> Word32 -- ^ query
                                                        -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdEndQuery
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdEndQuery.html vkCmdEndQuery registry at www.khronos.org>
foreign import ccall safe "vkCmdEndQuery" vkCmdEndQuerySafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkQueryPool -- ^ queryPool
                                              -> Word32 -- ^ query
                                                        -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdEndQuery
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdEndQuery.html vkCmdEndQuery registry at www.khronos.org>
type HS_vkCmdEndQuery =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkQueryPool -- ^ queryPool
                                    -> Word32 -- ^ query
                                              -> IO ()

type PFN_vkCmdEndQuery = FunPtr HS_vkCmdEndQuery

foreign import ccall "dynamic" unwrapVkCmdEndQuery ::
               PFN_vkCmdEndQuery -> HS_vkCmdEndQuery

instance VulkanProc "vkCmdEndQuery" where
        type VkProcType "vkCmdEndQuery" = HS_vkCmdEndQuery
        vkProcSymbol = _VkCmdEndQuery

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdEndQuery

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdResetQueryPool :: CString

pattern VkCmdResetQueryPool <- (is_VkCmdResetQueryPool -> True)
  where VkCmdResetQueryPool = _VkCmdResetQueryPool

{-# INLINE _VkCmdResetQueryPool #-}

_VkCmdResetQueryPool :: CString
_VkCmdResetQueryPool = Ptr "vkCmdResetQueryPool\NUL"##

{-# INLINE is_VkCmdResetQueryPool #-}

is_VkCmdResetQueryPool :: CString -> Bool
is_VkCmdResetQueryPool = (EQ ==) . cmpCStrings _VkCmdResetQueryPool

type VkCmdResetQueryPool = "vkCmdResetQueryPool"

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   > () vkCmdResetQueryPool
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     , uint32_t queryCount
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdResetQueryPool.html vkCmdResetQueryPool registry at www.khronos.org>
foreign import ccall unsafe "vkCmdResetQueryPool"
               vkCmdResetQueryPool ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkQueryPool -- ^ queryPool
                                              -> Word32 -- ^ firstQuery
                                                        -> Word32 -- ^ queryCount
                                                                  -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   > () vkCmdResetQueryPool
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     , uint32_t queryCount
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdResetQueryPool.html vkCmdResetQueryPool registry at www.khronos.org>
foreign import ccall safe "vkCmdResetQueryPool"
               vkCmdResetQueryPoolSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkQueryPool -- ^ queryPool
                                              -> Word32 -- ^ firstQuery
                                                        -> Word32 -- ^ queryCount
                                                                  -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   > () vkCmdResetQueryPool
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     , uint32_t queryCount
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdResetQueryPool.html vkCmdResetQueryPool registry at www.khronos.org>
type HS_vkCmdResetQueryPool =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkQueryPool -- ^ queryPool
                                    -> Word32 -- ^ firstQuery
                                              -> Word32 -- ^ queryCount
                                                        -> IO ()

type PFN_vkCmdResetQueryPool = FunPtr HS_vkCmdResetQueryPool

foreign import ccall "dynamic" unwrapVkCmdResetQueryPool ::
               PFN_vkCmdResetQueryPool -> HS_vkCmdResetQueryPool

instance VulkanProc "vkCmdResetQueryPool" where
        type VkProcType "vkCmdResetQueryPool" = HS_vkCmdResetQueryPool
        vkProcSymbol = _VkCmdResetQueryPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdResetQueryPool

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdWriteTimestamp :: CString

pattern VkCmdWriteTimestamp <- (is_VkCmdWriteTimestamp -> True)
  where VkCmdWriteTimestamp = _VkCmdWriteTimestamp

{-# INLINE _VkCmdWriteTimestamp #-}

_VkCmdWriteTimestamp :: CString
_VkCmdWriteTimestamp = Ptr "vkCmdWriteTimestamp\NUL"##

{-# INLINE is_VkCmdWriteTimestamp #-}

is_VkCmdWriteTimestamp :: CString -> Bool
is_VkCmdWriteTimestamp = (EQ ==) . cmpCStrings _VkCmdWriteTimestamp

type VkCmdWriteTimestamp = "vkCmdWriteTimestamp"

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   pipeline: @transfer@
--
--   > () vkCmdWriteTimestamp
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineStageFlagBits pipelineStage
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdWriteTimestamp.html vkCmdWriteTimestamp registry at www.khronos.org>
foreign import ccall unsafe "vkCmdWriteTimestamp"
               vkCmdWriteTimestamp ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineStageFlagBits -- ^ pipelineStage
                                         -> VkQueryPool -- ^ queryPool
                                                        -> Word32 -- ^ query
                                                                  -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   pipeline: @transfer@
--
--   > () vkCmdWriteTimestamp
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineStageFlagBits pipelineStage
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdWriteTimestamp.html vkCmdWriteTimestamp registry at www.khronos.org>
foreign import ccall safe "vkCmdWriteTimestamp"
               vkCmdWriteTimestampSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineStageFlagBits -- ^ pipelineStage
                                         -> VkQueryPool -- ^ queryPool
                                                        -> Word32 -- ^ query
                                                                  -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   pipeline: @transfer@
--
--   > () vkCmdWriteTimestamp
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineStageFlagBits pipelineStage
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdWriteTimestamp.html vkCmdWriteTimestamp registry at www.khronos.org>
type HS_vkCmdWriteTimestamp =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkPipelineStageFlagBits -- ^ pipelineStage
                               -> VkQueryPool -- ^ queryPool
                                              -> Word32 -- ^ query
                                                        -> IO ()

type PFN_vkCmdWriteTimestamp = FunPtr HS_vkCmdWriteTimestamp

foreign import ccall "dynamic" unwrapVkCmdWriteTimestamp ::
               PFN_vkCmdWriteTimestamp -> HS_vkCmdWriteTimestamp

instance VulkanProc "vkCmdWriteTimestamp" where
        type VkProcType "vkCmdWriteTimestamp" = HS_vkCmdWriteTimestamp
        vkProcSymbol = _VkCmdWriteTimestamp

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdWriteTimestamp

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdCopyQueryPoolResults :: CString

pattern VkCmdCopyQueryPoolResults <-
        (is_VkCmdCopyQueryPoolResults -> True)
  where VkCmdCopyQueryPoolResults = _VkCmdCopyQueryPoolResults

{-# INLINE _VkCmdCopyQueryPoolResults #-}

_VkCmdCopyQueryPoolResults :: CString
_VkCmdCopyQueryPoolResults = Ptr "vkCmdCopyQueryPoolResults\NUL"##

{-# INLINE is_VkCmdCopyQueryPoolResults #-}

is_VkCmdCopyQueryPoolResults :: CString -> Bool
is_VkCmdCopyQueryPoolResults
  = (EQ ==) . cmpCStrings _VkCmdCopyQueryPoolResults

type VkCmdCopyQueryPoolResults = "vkCmdCopyQueryPoolResults"

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdCopyQueryPoolResults
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     , uint32_t queryCount
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , VkDeviceSize stride
--   >     , VkQueryResultFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdCopyQueryPoolResults.html vkCmdCopyQueryPoolResults registry at www.khronos.org>
foreign import ccall unsafe "vkCmdCopyQueryPoolResults"
               vkCmdCopyQueryPoolResults ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkQueryPool -- ^ queryPool
                             ->
                   Word32 -- ^ firstQuery
                          ->
                     Word32 -- ^ queryCount
                            ->
                       VkBuffer -- ^ dstBuffer
                                ->
                         VkDeviceSize -- ^ dstOffset
                                      -> VkDeviceSize -- ^ stride
                                                      -> VkQueryResultFlags -- ^ flags
                                                                            -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdCopyQueryPoolResults
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     , uint32_t queryCount
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , VkDeviceSize stride
--   >     , VkQueryResultFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdCopyQueryPoolResults.html vkCmdCopyQueryPoolResults registry at www.khronos.org>
foreign import ccall safe "vkCmdCopyQueryPoolResults"
               vkCmdCopyQueryPoolResultsSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkQueryPool -- ^ queryPool
                             ->
                   Word32 -- ^ firstQuery
                          ->
                     Word32 -- ^ queryCount
                            ->
                       VkBuffer -- ^ dstBuffer
                                ->
                         VkDeviceSize -- ^ dstOffset
                                      -> VkDeviceSize -- ^ stride
                                                      -> VkQueryResultFlags -- ^ flags
                                                                            -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > () vkCmdCopyQueryPoolResults
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     , uint32_t queryCount
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , VkDeviceSize stride
--   >     , VkQueryResultFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdCopyQueryPoolResults.html vkCmdCopyQueryPoolResults registry at www.khronos.org>
type HS_vkCmdCopyQueryPoolResults =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkQueryPool -- ^ queryPool
                   ->
         Word32 -- ^ firstQuery
                ->
           Word32 -- ^ queryCount
                  ->
             VkBuffer -- ^ dstBuffer
                      ->
               VkDeviceSize -- ^ dstOffset
                            -> VkDeviceSize -- ^ stride
                                            -> VkQueryResultFlags -- ^ flags
                                                                  -> IO ()

type PFN_vkCmdCopyQueryPoolResults =
     FunPtr HS_vkCmdCopyQueryPoolResults

foreign import ccall "dynamic" unwrapVkCmdCopyQueryPoolResults ::
               PFN_vkCmdCopyQueryPoolResults -> HS_vkCmdCopyQueryPoolResults

instance VulkanProc "vkCmdCopyQueryPoolResults" where
        type VkProcType "vkCmdCopyQueryPoolResults" =
             HS_vkCmdCopyQueryPoolResults
        vkProcSymbol = _VkCmdCopyQueryPoolResults

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdCopyQueryPoolResults

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdPushConstants :: CString

pattern VkCmdPushConstants <- (is_VkCmdPushConstants -> True)
  where VkCmdPushConstants = _VkCmdPushConstants

{-# INLINE _VkCmdPushConstants #-}

_VkCmdPushConstants :: CString
_VkCmdPushConstants = Ptr "vkCmdPushConstants\NUL"##

{-# INLINE is_VkCmdPushConstants #-}

is_VkCmdPushConstants :: CString -> Bool
is_VkCmdPushConstants = (EQ ==) . cmpCStrings _VkCmdPushConstants

type VkCmdPushConstants = "vkCmdPushConstants"

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdPushConstants
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineLayout layout
--   >     , VkShaderStageFlags stageFlags
--   >     , uint32_t offset
--   >     , uint32_t size
--   >     , const void* pValues
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdPushConstants.html vkCmdPushConstants registry at www.khronos.org>
foreign import ccall unsafe "vkCmdPushConstants" vkCmdPushConstants
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineLayout -- ^ layout
                                  ->
                   VkShaderStageFlags -- ^ stageFlags
                                      -> Word32 -- ^ offset
                                                -> Word32 -- ^ size
                                                          -> Ptr Void -- ^ pValues
                                                                      -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdPushConstants
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineLayout layout
--   >     , VkShaderStageFlags stageFlags
--   >     , uint32_t offset
--   >     , uint32_t size
--   >     , const void* pValues
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdPushConstants.html vkCmdPushConstants registry at www.khronos.org>
foreign import ccall safe "vkCmdPushConstants"
               vkCmdPushConstantsSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineLayout -- ^ layout
                                  ->
                   VkShaderStageFlags -- ^ stageFlags
                                      -> Word32 -- ^ offset
                                                -> Word32 -- ^ size
                                                          -> Ptr Void -- ^ pValues
                                                                      -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdPushConstants
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineLayout layout
--   >     , VkShaderStageFlags stageFlags
--   >     , uint32_t offset
--   >     , uint32_t size
--   >     , const void* pValues
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdPushConstants.html vkCmdPushConstants registry at www.khronos.org>
type HS_vkCmdPushConstants =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkPipelineLayout -- ^ layout
                        ->
         VkShaderStageFlags -- ^ stageFlags
                            -> Word32 -- ^ offset
                                      -> Word32 -- ^ size
                                                -> Ptr Void -- ^ pValues
                                                            -> IO ()

type PFN_vkCmdPushConstants = FunPtr HS_vkCmdPushConstants

foreign import ccall "dynamic" unwrapVkCmdPushConstants ::
               PFN_vkCmdPushConstants -> HS_vkCmdPushConstants

instance VulkanProc "vkCmdPushConstants" where
        type VkProcType "vkCmdPushConstants" = HS_vkCmdPushConstants
        vkProcSymbol = _VkCmdPushConstants

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdPushConstants

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdBeginRenderPass :: CString

pattern VkCmdBeginRenderPass <- (is_VkCmdBeginRenderPass -> True)
  where VkCmdBeginRenderPass = _VkCmdBeginRenderPass

{-# INLINE _VkCmdBeginRenderPass #-}

_VkCmdBeginRenderPass :: CString
_VkCmdBeginRenderPass = Ptr "vkCmdBeginRenderPass\NUL"##

{-# INLINE is_VkCmdBeginRenderPass #-}

is_VkCmdBeginRenderPass :: CString -> Bool
is_VkCmdBeginRenderPass
  = (EQ ==) . cmpCStrings _VkCmdBeginRenderPass

type VkCmdBeginRenderPass = "vkCmdBeginRenderPass"

-- | queues: 'graphics'.
--
--   renderpass: @outside@
--
--   pipeline: @graphics@
--
--   > () vkCmdBeginRenderPass
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkRenderPassBeginInfo* pRenderPassBegin
--   >     , VkSubpassContents contents
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBeginRenderPass.html vkCmdBeginRenderPass registry at www.khronos.org>
foreign import ccall unsafe "vkCmdBeginRenderPass"
               vkCmdBeginRenderPass ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Ptr VkRenderPassBeginInfo -- ^ pRenderPassBegin
                                           -> VkSubpassContents -- ^ contents
                                                                -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @outside@
--
--   pipeline: @graphics@
--
--   > () vkCmdBeginRenderPass
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkRenderPassBeginInfo* pRenderPassBegin
--   >     , VkSubpassContents contents
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBeginRenderPass.html vkCmdBeginRenderPass registry at www.khronos.org>
foreign import ccall safe "vkCmdBeginRenderPass"
               vkCmdBeginRenderPassSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Ptr VkRenderPassBeginInfo -- ^ pRenderPassBegin
                                           -> VkSubpassContents -- ^ contents
                                                                -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @outside@
--
--   pipeline: @graphics@
--
--   > () vkCmdBeginRenderPass
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkRenderPassBeginInfo* pRenderPassBegin
--   >     , VkSubpassContents contents
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdBeginRenderPass.html vkCmdBeginRenderPass registry at www.khronos.org>
type HS_vkCmdBeginRenderPass =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Ptr VkRenderPassBeginInfo -- ^ pRenderPassBegin
                                 -> VkSubpassContents -- ^ contents
                                                      -> IO ()

type PFN_vkCmdBeginRenderPass = FunPtr HS_vkCmdBeginRenderPass

foreign import ccall "dynamic" unwrapVkCmdBeginRenderPass ::
               PFN_vkCmdBeginRenderPass -> HS_vkCmdBeginRenderPass

instance VulkanProc "vkCmdBeginRenderPass" where
        type VkProcType "vkCmdBeginRenderPass" = HS_vkCmdBeginRenderPass
        vkProcSymbol = _VkCmdBeginRenderPass

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdBeginRenderPass

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdNextSubpass :: CString

pattern VkCmdNextSubpass <- (is_VkCmdNextSubpass -> True)
  where VkCmdNextSubpass = _VkCmdNextSubpass

{-# INLINE _VkCmdNextSubpass #-}

_VkCmdNextSubpass :: CString
_VkCmdNextSubpass = Ptr "vkCmdNextSubpass\NUL"##

{-# INLINE is_VkCmdNextSubpass #-}

is_VkCmdNextSubpass :: CString -> Bool
is_VkCmdNextSubpass = (EQ ==) . cmpCStrings _VkCmdNextSubpass

type VkCmdNextSubpass = "vkCmdNextSubpass"

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdNextSubpass
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkSubpassContents contents
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdNextSubpass.html vkCmdNextSubpass registry at www.khronos.org>
foreign import ccall unsafe "vkCmdNextSubpass" vkCmdNextSubpass ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkSubpassContents -- ^ contents
                                                    -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdNextSubpass
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkSubpassContents contents
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdNextSubpass.html vkCmdNextSubpass registry at www.khronos.org>
foreign import ccall safe "vkCmdNextSubpass" vkCmdNextSubpassSafe
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> VkSubpassContents -- ^ contents
                                                       -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdNextSubpass
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkSubpassContents contents
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdNextSubpass.html vkCmdNextSubpass registry at www.khronos.org>
type HS_vkCmdNextSubpass =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkSubpassContents -- ^ contents
                                          -> IO ()

type PFN_vkCmdNextSubpass = FunPtr HS_vkCmdNextSubpass

foreign import ccall "dynamic" unwrapVkCmdNextSubpass ::
               PFN_vkCmdNextSubpass -> HS_vkCmdNextSubpass

instance VulkanProc "vkCmdNextSubpass" where
        type VkProcType "vkCmdNextSubpass" = HS_vkCmdNextSubpass
        vkProcSymbol = _VkCmdNextSubpass

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdNextSubpass

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdEndRenderPass :: CString

pattern VkCmdEndRenderPass <- (is_VkCmdEndRenderPass -> True)
  where VkCmdEndRenderPass = _VkCmdEndRenderPass

{-# INLINE _VkCmdEndRenderPass #-}

_VkCmdEndRenderPass :: CString
_VkCmdEndRenderPass = Ptr "vkCmdEndRenderPass\NUL"##

{-# INLINE is_VkCmdEndRenderPass #-}

is_VkCmdEndRenderPass :: CString -> Bool
is_VkCmdEndRenderPass = (EQ ==) . cmpCStrings _VkCmdEndRenderPass

type VkCmdEndRenderPass = "vkCmdEndRenderPass"

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdEndRenderPass
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdEndRenderPass.html vkCmdEndRenderPass registry at www.khronos.org>
foreign import ccall unsafe "vkCmdEndRenderPass" vkCmdEndRenderPass
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdEndRenderPass
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdEndRenderPass.html vkCmdEndRenderPass registry at www.khronos.org>
foreign import ccall safe "vkCmdEndRenderPass"
               vkCmdEndRenderPassSafe :: VkCommandBuffer -- ^ commandBuffer
                                                         -> IO ()

-- | queues: 'graphics'.
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > () vkCmdEndRenderPass
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdEndRenderPass.html vkCmdEndRenderPass registry at www.khronos.org>
type HS_vkCmdEndRenderPass = VkCommandBuffer -- ^ commandBuffer
                                             -> IO ()

type PFN_vkCmdEndRenderPass = FunPtr HS_vkCmdEndRenderPass

foreign import ccall "dynamic" unwrapVkCmdEndRenderPass ::
               PFN_vkCmdEndRenderPass -> HS_vkCmdEndRenderPass

instance VulkanProc "vkCmdEndRenderPass" where
        type VkProcType "vkCmdEndRenderPass" = HS_vkCmdEndRenderPass
        vkProcSymbol = _VkCmdEndRenderPass

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdEndRenderPass

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCmdExecuteCommands :: CString

pattern VkCmdExecuteCommands <- (is_VkCmdExecuteCommands -> True)
  where VkCmdExecuteCommands = _VkCmdExecuteCommands

{-# INLINE _VkCmdExecuteCommands #-}

_VkCmdExecuteCommands :: CString
_VkCmdExecuteCommands = Ptr "vkCmdExecuteCommands\NUL"##

{-# INLINE is_VkCmdExecuteCommands #-}

is_VkCmdExecuteCommands :: CString -> Bool
is_VkCmdExecuteCommands
  = (EQ ==) . cmpCStrings _VkCmdExecuteCommands

type VkCmdExecuteCommands = "vkCmdExecuteCommands"

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdExecuteCommands
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t commandBufferCount
--   >     , const VkCommandBuffer* pCommandBuffers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdExecuteCommands.html vkCmdExecuteCommands registry at www.khronos.org>
foreign import ccall unsafe "vkCmdExecuteCommands"
               vkCmdExecuteCommands ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ commandBufferCount
                                         -> Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                                -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdExecuteCommands
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t commandBufferCount
--   >     , const VkCommandBuffer* pCommandBuffers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdExecuteCommands.html vkCmdExecuteCommands registry at www.khronos.org>
foreign import ccall safe "vkCmdExecuteCommands"
               vkCmdExecuteCommandsSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ commandBufferCount
                                         -> Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                                -> IO ()

-- | queues: 'transfer', 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdExecuteCommands
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t commandBufferCount
--   >     , const VkCommandBuffer* pCommandBuffers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdExecuteCommands.html vkCmdExecuteCommands registry at www.khronos.org>
type HS_vkCmdExecuteCommands =
     VkCommandBuffer -- ^ commandBuffer
                     -> Word32 -- ^ commandBufferCount
                               -> Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                      -> IO ()

type PFN_vkCmdExecuteCommands = FunPtr HS_vkCmdExecuteCommands

foreign import ccall "dynamic" unwrapVkCmdExecuteCommands ::
               PFN_vkCmdExecuteCommands -> HS_vkCmdExecuteCommands

instance VulkanProc "vkCmdExecuteCommands" where
        type VkProcType "vkCmdExecuteCommands" = HS_vkCmdExecuteCommands
        vkProcSymbol = _VkCmdExecuteCommands

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdExecuteCommands

        {-# INLINE unwrapVkProcPtr #-}
