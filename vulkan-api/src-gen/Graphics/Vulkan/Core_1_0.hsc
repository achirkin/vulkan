#include "vulkan/vulkan.h"

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
        module Graphics.Vulkan.Types.Enum.Pipeline,
        pattern VK_LOD_CLAMP_NONE, pattern VK_REMAINING_MIP_LEVELS,
        pattern VK_REMAINING_ARRAY_LAYERS, pattern VK_WHOLE_SIZE,
        pattern VK_ATTACHMENT_UNUSED, pattern VK_TRUE, pattern VK_FALSE,
        pattern VK_QUEUE_FAMILY_IGNORED, pattern VK_SUBPASS_EXTERNAL,
        -- ** Device initialization
        VkCreateInstance, pattern VkCreateInstance, HS_vkCreateInstance,
        PFN_vkCreateInstance, vkCreateInstance, vkCreateInstanceSafe,
        VkDestroyInstance, pattern VkDestroyInstance, HS_vkDestroyInstance,
        PFN_vkDestroyInstance, vkDestroyInstance, vkDestroyInstanceSafe,
        VkEnumeratePhysicalDevices, pattern VkEnumeratePhysicalDevices,
        HS_vkEnumeratePhysicalDevices, PFN_vkEnumeratePhysicalDevices,
        vkEnumeratePhysicalDevices, vkEnumeratePhysicalDevicesSafe,
        VkGetPhysicalDeviceFeatures, pattern VkGetPhysicalDeviceFeatures,
        HS_vkGetPhysicalDeviceFeatures, PFN_vkGetPhysicalDeviceFeatures,
        vkGetPhysicalDeviceFeatures, vkGetPhysicalDeviceFeaturesSafe,
        VkGetPhysicalDeviceFormatProperties,
        pattern VkGetPhysicalDeviceFormatProperties,
        HS_vkGetPhysicalDeviceFormatProperties,
        PFN_vkGetPhysicalDeviceFormatProperties,
        vkGetPhysicalDeviceFormatProperties,
        vkGetPhysicalDeviceFormatPropertiesSafe,
        VkGetPhysicalDeviceImageFormatProperties,
        pattern VkGetPhysicalDeviceImageFormatProperties,
        HS_vkGetPhysicalDeviceImageFormatProperties,
        PFN_vkGetPhysicalDeviceImageFormatProperties,
        vkGetPhysicalDeviceImageFormatProperties,
        vkGetPhysicalDeviceImageFormatPropertiesSafe,
        VkGetPhysicalDeviceProperties,
        pattern VkGetPhysicalDeviceProperties,
        HS_vkGetPhysicalDeviceProperties,
        PFN_vkGetPhysicalDeviceProperties, vkGetPhysicalDeviceProperties,
        vkGetPhysicalDevicePropertiesSafe,
        VkGetPhysicalDeviceQueueFamilyProperties,
        pattern VkGetPhysicalDeviceQueueFamilyProperties,
        HS_vkGetPhysicalDeviceQueueFamilyProperties,
        PFN_vkGetPhysicalDeviceQueueFamilyProperties,
        vkGetPhysicalDeviceQueueFamilyProperties,
        vkGetPhysicalDeviceQueueFamilyPropertiesSafe,
        VkGetPhysicalDeviceMemoryProperties,
        pattern VkGetPhysicalDeviceMemoryProperties,
        HS_vkGetPhysicalDeviceMemoryProperties,
        PFN_vkGetPhysicalDeviceMemoryProperties,
        vkGetPhysicalDeviceMemoryProperties,
        vkGetPhysicalDeviceMemoryPropertiesSafe, VkGetInstanceProcAddr,
        pattern VkGetInstanceProcAddr, HS_vkGetInstanceProcAddr,
        PFN_vkGetInstanceProcAddr, vkGetInstanceProcAddr,
        vkGetInstanceProcAddrSafe, VkGetDeviceProcAddr,
        pattern VkGetDeviceProcAddr, HS_vkGetDeviceProcAddr,
        PFN_vkGetDeviceProcAddr, vkGetDeviceProcAddr,
        vkGetDeviceProcAddrSafe, module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Enum.Format,
        module Graphics.Vulkan.Types.Enum.Image,
        module Graphics.Vulkan.Types.Enum.InternalAllocationType,
        module Graphics.Vulkan.Types.Enum.Memory,
        module Graphics.Vulkan.Types.Enum.PhysicalDeviceType,
        module Graphics.Vulkan.Types.Enum.Queue,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Enum.SampleCountFlags,
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Enum.SystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.AllocationCallbacks,
        module Graphics.Vulkan.Types.Struct.ApplicationInfo,
        module Graphics.Vulkan.Types.Struct.Extent,
        module Graphics.Vulkan.Types.Struct.FormatProperties,
        module Graphics.Vulkan.Types.Struct.Image,
        module Graphics.Vulkan.Types.Struct.InstanceCreateInfo,
        module Graphics.Vulkan.Types.Struct.Memory,
        module Graphics.Vulkan.Types.Struct.PhysicalDevice,
        module Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures,
        module Graphics.Vulkan.Types.Struct.QueueFamilyProperties,
        -- ** Device commands
        VkCreateDevice, pattern VkCreateDevice, HS_vkCreateDevice,
        PFN_vkCreateDevice, vkCreateDevice, vkCreateDeviceSafe,
        VkDestroyDevice, pattern VkDestroyDevice, HS_vkDestroyDevice,
        PFN_vkDestroyDevice, vkDestroyDevice, vkDestroyDeviceSafe,
        module Graphics.Vulkan.Types.Enum.Device,
        module Graphics.Vulkan.Types.Struct.Device,
        -- ** Extension discovery commands
        VkEnumerateInstanceExtensionProperties,
        pattern VkEnumerateInstanceExtensionProperties,
        HS_vkEnumerateInstanceExtensionProperties,
        PFN_vkEnumerateInstanceExtensionProperties,
        vkEnumerateInstanceExtensionProperties,
        vkEnumerateInstanceExtensionPropertiesSafe,
        VkEnumerateDeviceExtensionProperties,
        pattern VkEnumerateDeviceExtensionProperties,
        HS_vkEnumerateDeviceExtensionProperties,
        PFN_vkEnumerateDeviceExtensionProperties,
        vkEnumerateDeviceExtensionProperties,
        vkEnumerateDeviceExtensionPropertiesSafe,
        module Graphics.Vulkan.Types.Struct.ExtensionProperties,
        -- ** Layer discovery commands
        VkEnumerateInstanceLayerProperties,
        pattern VkEnumerateInstanceLayerProperties,
        HS_vkEnumerateInstanceLayerProperties,
        PFN_vkEnumerateInstanceLayerProperties,
        vkEnumerateInstanceLayerProperties,
        vkEnumerateInstanceLayerPropertiesSafe,
        VkEnumerateDeviceLayerProperties,
        pattern VkEnumerateDeviceLayerProperties,
        HS_vkEnumerateDeviceLayerProperties,
        PFN_vkEnumerateDeviceLayerProperties,
        vkEnumerateDeviceLayerProperties,
        vkEnumerateDeviceLayerPropertiesSafe,
        module Graphics.Vulkan.Types.Struct.LayerProperties,
        -- ** queue commands
        VkGetDeviceQueue, pattern VkGetDeviceQueue, HS_vkGetDeviceQueue,
        PFN_vkGetDeviceQueue, vkGetDeviceQueue, vkGetDeviceQueueSafe,
        VkQueueSubmit, pattern VkQueueSubmit, HS_vkQueueSubmit,
        PFN_vkQueueSubmit, vkQueueSubmit, vkQueueSubmitSafe,
        VkQueueWaitIdle, pattern VkQueueWaitIdle, HS_vkQueueWaitIdle,
        PFN_vkQueueWaitIdle, vkQueueWaitIdle, vkQueueWaitIdleSafe,
        VkDeviceWaitIdle, pattern VkDeviceWaitIdle, HS_vkDeviceWaitIdle,
        PFN_vkDeviceWaitIdle, vkDeviceWaitIdle, vkDeviceWaitIdleSafe,
        module Graphics.Vulkan.Types.Struct.SubmitInfo, -- ** Memory commands
                                                        VkAllocateMemory,
        pattern VkAllocateMemory, HS_vkAllocateMemory,
        PFN_vkAllocateMemory, vkAllocateMemory, vkAllocateMemorySafe,
        VkFreeMemory, pattern VkFreeMemory, HS_vkFreeMemory,
        PFN_vkFreeMemory, vkFreeMemory, vkFreeMemorySafe, VkMapMemory,
        pattern VkMapMemory, HS_vkMapMemory, PFN_vkMapMemory, vkMapMemory,
        vkMapMemorySafe, VkUnmapMemory, pattern VkUnmapMemory,
        HS_vkUnmapMemory, PFN_vkUnmapMemory, vkUnmapMemory,
        vkUnmapMemorySafe, VkFlushMappedMemoryRanges,
        pattern VkFlushMappedMemoryRanges, HS_vkFlushMappedMemoryRanges,
        PFN_vkFlushMappedMemoryRanges, vkFlushMappedMemoryRanges,
        vkFlushMappedMemoryRangesSafe, VkInvalidateMappedMemoryRanges,
        pattern VkInvalidateMappedMemoryRanges,
        HS_vkInvalidateMappedMemoryRanges,
        PFN_vkInvalidateMappedMemoryRanges, vkInvalidateMappedMemoryRanges,
        vkInvalidateMappedMemoryRangesSafe, VkGetDeviceMemoryCommitment,
        pattern VkGetDeviceMemoryCommitment,
        HS_vkGetDeviceMemoryCommitment, PFN_vkGetDeviceMemoryCommitment,
        vkGetDeviceMemoryCommitment, vkGetDeviceMemoryCommitmentSafe,
        module Graphics.Vulkan.Types.Struct.MappedMemoryRange,
        -- ** Memory management API commands
        VkBindBufferMemory, pattern VkBindBufferMemory,
        HS_vkBindBufferMemory, PFN_vkBindBufferMemory, vkBindBufferMemory,
        vkBindBufferMemorySafe, VkBindImageMemory,
        pattern VkBindImageMemory, HS_vkBindImageMemory,
        PFN_vkBindImageMemory, vkBindImageMemory, vkBindImageMemorySafe,
        VkGetBufferMemoryRequirements,
        pattern VkGetBufferMemoryRequirements,
        HS_vkGetBufferMemoryRequirements,
        PFN_vkGetBufferMemoryRequirements, vkGetBufferMemoryRequirements,
        vkGetBufferMemoryRequirementsSafe, VkGetImageMemoryRequirements,
        pattern VkGetImageMemoryRequirements,
        HS_vkGetImageMemoryRequirements, PFN_vkGetImageMemoryRequirements,
        vkGetImageMemoryRequirements, vkGetImageMemoryRequirementsSafe,
        -- ** Sparse resource memory management API commands
        VkGetImageSparseMemoryRequirements,
        pattern VkGetImageSparseMemoryRequirements,
        HS_vkGetImageSparseMemoryRequirements,
        PFN_vkGetImageSparseMemoryRequirements,
        vkGetImageSparseMemoryRequirements,
        vkGetImageSparseMemoryRequirementsSafe,
        VkGetPhysicalDeviceSparseImageFormatProperties,
        pattern VkGetPhysicalDeviceSparseImageFormatProperties,
        HS_vkGetPhysicalDeviceSparseImageFormatProperties,
        PFN_vkGetPhysicalDeviceSparseImageFormatProperties,
        vkGetPhysicalDeviceSparseImageFormatProperties,
        vkGetPhysicalDeviceSparseImageFormatPropertiesSafe,
        VkQueueBindSparse, pattern VkQueueBindSparse, HS_vkQueueBindSparse,
        PFN_vkQueueBindSparse, vkQueueBindSparse, vkQueueBindSparseSafe,
        module Graphics.Vulkan.Types.Enum.Sparse,
        module Graphics.Vulkan.Types.Struct.Bind,
        module Graphics.Vulkan.Types.Struct.Offset,
        module Graphics.Vulkan.Types.Struct.Sparse, -- ** Fence commands
                                                    VkCreateFence,
        pattern VkCreateFence, HS_vkCreateFence, PFN_vkCreateFence,
        vkCreateFence, vkCreateFenceSafe, VkDestroyFence,
        pattern VkDestroyFence, HS_vkDestroyFence, PFN_vkDestroyFence,
        vkDestroyFence, vkDestroyFenceSafe, VkResetFences,
        pattern VkResetFences, HS_vkResetFences, PFN_vkResetFences,
        vkResetFences, vkResetFencesSafe, VkGetFenceStatus,
        pattern VkGetFenceStatus, HS_vkGetFenceStatus,
        PFN_vkGetFenceStatus, vkGetFenceStatus, vkGetFenceStatusSafe,
        VkWaitForFences, pattern VkWaitForFences, HS_vkWaitForFences,
        PFN_vkWaitForFences, vkWaitForFences, vkWaitForFencesSafe,
        module Graphics.Vulkan.Types.Enum.Fence,
        module Graphics.Vulkan.Types.Struct.Fence, -- ** Queue semaphore commands
                                                   VkCreateSemaphore,
        pattern VkCreateSemaphore, HS_vkCreateSemaphore,
        PFN_vkCreateSemaphore, vkCreateSemaphore, vkCreateSemaphoreSafe,
        VkDestroySemaphore, pattern VkDestroySemaphore,
        HS_vkDestroySemaphore, PFN_vkDestroySemaphore, vkDestroySemaphore,
        vkDestroySemaphoreSafe,
        module Graphics.Vulkan.Types.Struct.Semaphore, -- ** Event commands
                                                       VkCreateEvent,
        pattern VkCreateEvent, HS_vkCreateEvent, PFN_vkCreateEvent,
        vkCreateEvent, vkCreateEventSafe, VkDestroyEvent,
        pattern VkDestroyEvent, HS_vkDestroyEvent, PFN_vkDestroyEvent,
        vkDestroyEvent, vkDestroyEventSafe, VkGetEventStatus,
        pattern VkGetEventStatus, HS_vkGetEventStatus,
        PFN_vkGetEventStatus, vkGetEventStatus, vkGetEventStatusSafe,
        VkSetEvent, pattern VkSetEvent, HS_vkSetEvent, PFN_vkSetEvent,
        vkSetEvent, vkSetEventSafe, VkResetEvent, pattern VkResetEvent,
        HS_vkResetEvent, PFN_vkResetEvent, vkResetEvent, vkResetEventSafe,
        module Graphics.Vulkan.Types.Struct.EventCreateInfo,
        -- ** Query commands
        VkCreateQueryPool, pattern VkCreateQueryPool, HS_vkCreateQueryPool,
        PFN_vkCreateQueryPool, vkCreateQueryPool, vkCreateQueryPoolSafe,
        VkDestroyQueryPool, pattern VkDestroyQueryPool,
        HS_vkDestroyQueryPool, PFN_vkDestroyQueryPool, vkDestroyQueryPool,
        vkDestroyQueryPoolSafe, VkGetQueryPoolResults,
        pattern VkGetQueryPoolResults, HS_vkGetQueryPoolResults,
        PFN_vkGetQueryPoolResults, vkGetQueryPoolResults,
        vkGetQueryPoolResultsSafe, module Graphics.Vulkan.Types.Enum.Query,
        module Graphics.Vulkan.Types.Struct.QueryPoolCreateInfo,
        -- ** Buffer commands
        VkCreateBuffer, pattern VkCreateBuffer, HS_vkCreateBuffer,
        PFN_vkCreateBuffer, vkCreateBuffer, vkCreateBufferSafe,
        VkDestroyBuffer, pattern VkDestroyBuffer, HS_vkDestroyBuffer,
        PFN_vkDestroyBuffer, vkDestroyBuffer, vkDestroyBufferSafe,
        module Graphics.Vulkan.Types.Enum.Buffer,
        module Graphics.Vulkan.Types.Enum.SharingMode,
        module Graphics.Vulkan.Types.Struct.Buffer, -- ** Buffer view commands
                                                    VkCreateBufferView,
        pattern VkCreateBufferView, HS_vkCreateBufferView,
        PFN_vkCreateBufferView, vkCreateBufferView, vkCreateBufferViewSafe,
        VkDestroyBufferView, pattern VkDestroyBufferView,
        HS_vkDestroyBufferView, PFN_vkDestroyBufferView,
        vkDestroyBufferView, vkDestroyBufferViewSafe, -- ** Image commands
                                                      VkCreateImage,
        pattern VkCreateImage, HS_vkCreateImage, PFN_vkCreateImage,
        vkCreateImage, vkCreateImageSafe, VkDestroyImage,
        pattern VkDestroyImage, HS_vkDestroyImage, PFN_vkDestroyImage,
        vkDestroyImage, vkDestroyImageSafe, VkGetImageSubresourceLayout,
        pattern VkGetImageSubresourceLayout,
        HS_vkGetImageSubresourceLayout, PFN_vkGetImageSubresourceLayout,
        vkGetImageSubresourceLayout, vkGetImageSubresourceLayoutSafe,
        module Graphics.Vulkan.Types.Struct.SubresourceLayout,
        -- ** Image view commands
        VkCreateImageView, pattern VkCreateImageView, HS_vkCreateImageView,
        PFN_vkCreateImageView, vkCreateImageView, vkCreateImageViewSafe,
        VkDestroyImageView, pattern VkDestroyImageView,
        HS_vkDestroyImageView, PFN_vkDestroyImageView, vkDestroyImageView,
        vkDestroyImageViewSafe,
        module Graphics.Vulkan.Types.Enum.ComponentSwizzle,
        module Graphics.Vulkan.Types.Struct.ComponentMapping,
        -- ** Shader commands
        VkCreateShaderModule, pattern VkCreateShaderModule,
        HS_vkCreateShaderModule, PFN_vkCreateShaderModule,
        vkCreateShaderModule, vkCreateShaderModuleSafe,
        VkDestroyShaderModule, pattern VkDestroyShaderModule,
        HS_vkDestroyShaderModule, PFN_vkDestroyShaderModule,
        vkDestroyShaderModule, vkDestroyShaderModuleSafe,
        module Graphics.Vulkan.Types.Struct.Shader, -- ** Pipeline Cache commands
                                                    VkCreatePipelineCache,
        pattern VkCreatePipelineCache, HS_vkCreatePipelineCache,
        PFN_vkCreatePipelineCache, vkCreatePipelineCache,
        vkCreatePipelineCacheSafe, VkDestroyPipelineCache,
        pattern VkDestroyPipelineCache, HS_vkDestroyPipelineCache,
        PFN_vkDestroyPipelineCache, vkDestroyPipelineCache,
        vkDestroyPipelineCacheSafe, VkGetPipelineCacheData,
        pattern VkGetPipelineCacheData, HS_vkGetPipelineCacheData,
        PFN_vkGetPipelineCacheData, vkGetPipelineCacheData,
        vkGetPipelineCacheDataSafe, VkMergePipelineCaches,
        pattern VkMergePipelineCaches, HS_vkMergePipelineCaches,
        PFN_vkMergePipelineCaches, vkMergePipelineCaches,
        vkMergePipelineCachesSafe,
        module Graphics.Vulkan.Types.Struct.Pipeline,
        -- ** Pipeline commands
        VkCreateGraphicsPipelines, pattern VkCreateGraphicsPipelines,
        HS_vkCreateGraphicsPipelines, PFN_vkCreateGraphicsPipelines,
        vkCreateGraphicsPipelines, vkCreateGraphicsPipelinesSafe,
        VkCreateComputePipelines, pattern VkCreateComputePipelines,
        HS_vkCreateComputePipelines, PFN_vkCreateComputePipelines,
        vkCreateComputePipelines, vkCreateComputePipelinesSafe,
        VkDestroyPipeline, pattern VkDestroyPipeline, HS_vkDestroyPipeline,
        PFN_vkDestroyPipeline, vkDestroyPipeline, vkDestroyPipelineSafe,
        module Graphics.Vulkan.Types.Enum.Blend,
        module Graphics.Vulkan.Types.Enum.Color,
        module Graphics.Vulkan.Types.Enum.CompareOp,
        module Graphics.Vulkan.Types.Enum.CullModeFlags,
        module Graphics.Vulkan.Types.Enum.DynamicState,
        module Graphics.Vulkan.Types.Enum.FrontFace,
        module Graphics.Vulkan.Types.Enum.LogicOp,
        module Graphics.Vulkan.Types.Enum.PolygonMode,
        module Graphics.Vulkan.Types.Enum.PrimitiveTopology,
        module Graphics.Vulkan.Types.Enum.Shader,
        module Graphics.Vulkan.Types.Enum.Stencil,
        module Graphics.Vulkan.Types.Enum.VertexInputRate,
        module Graphics.Vulkan.Types.Struct.ComputePipelineCreateInfo,
        module Graphics.Vulkan.Types.Struct.Rect,
        module Graphics.Vulkan.Types.Struct.Specialization,
        module Graphics.Vulkan.Types.Struct.StencilOpState,
        module Graphics.Vulkan.Types.Struct.VertexInput,
        module Graphics.Vulkan.Types.Struct.Viewport,
        -- ** Pipeline layout commands
        VkCreatePipelineLayout, pattern VkCreatePipelineLayout,
        HS_vkCreatePipelineLayout, PFN_vkCreatePipelineLayout,
        vkCreatePipelineLayout, vkCreatePipelineLayoutSafe,
        VkDestroyPipelineLayout, pattern VkDestroyPipelineLayout,
        HS_vkDestroyPipelineLayout, PFN_vkDestroyPipelineLayout,
        vkDestroyPipelineLayout, vkDestroyPipelineLayoutSafe,
        module Graphics.Vulkan.Types.Struct.PushConstantRange,
        -- ** Sampler commands
        VkCreateSampler, pattern VkCreateSampler, HS_vkCreateSampler,
        PFN_vkCreateSampler, vkCreateSampler, vkCreateSamplerSafe,
        VkDestroySampler, pattern VkDestroySampler, HS_vkDestroySampler,
        PFN_vkDestroySampler, vkDestroySampler, vkDestroySamplerSafe,
        module Graphics.Vulkan.Types.Enum.BorderColor,
        module Graphics.Vulkan.Types.Enum.Filter,
        module Graphics.Vulkan.Types.Enum.Sampler,
        module Graphics.Vulkan.Types.Struct.Sampler,
        -- ** Descriptor set commands
        VkCreateDescriptorSetLayout, pattern VkCreateDescriptorSetLayout,
        HS_vkCreateDescriptorSetLayout, PFN_vkCreateDescriptorSetLayout,
        vkCreateDescriptorSetLayout, vkCreateDescriptorSetLayoutSafe,
        VkDestroyDescriptorSetLayout, pattern VkDestroyDescriptorSetLayout,
        HS_vkDestroyDescriptorSetLayout, PFN_vkDestroyDescriptorSetLayout,
        vkDestroyDescriptorSetLayout, vkDestroyDescriptorSetLayoutSafe,
        VkCreateDescriptorPool, pattern VkCreateDescriptorPool,
        HS_vkCreateDescriptorPool, PFN_vkCreateDescriptorPool,
        vkCreateDescriptorPool, vkCreateDescriptorPoolSafe,
        VkDestroyDescriptorPool, pattern VkDestroyDescriptorPool,
        HS_vkDestroyDescriptorPool, PFN_vkDestroyDescriptorPool,
        vkDestroyDescriptorPool, vkDestroyDescriptorPoolSafe,
        VkResetDescriptorPool, pattern VkResetDescriptorPool,
        HS_vkResetDescriptorPool, PFN_vkResetDescriptorPool,
        vkResetDescriptorPool, vkResetDescriptorPoolSafe,
        VkAllocateDescriptorSets, pattern VkAllocateDescriptorSets,
        HS_vkAllocateDescriptorSets, PFN_vkAllocateDescriptorSets,
        vkAllocateDescriptorSets, vkAllocateDescriptorSetsSafe,
        VkFreeDescriptorSets, pattern VkFreeDescriptorSets,
        HS_vkFreeDescriptorSets, PFN_vkFreeDescriptorSets,
        vkFreeDescriptorSets, vkFreeDescriptorSetsSafe,
        VkUpdateDescriptorSets, pattern VkUpdateDescriptorSets,
        HS_vkUpdateDescriptorSets, PFN_vkUpdateDescriptorSets,
        vkUpdateDescriptorSets, vkUpdateDescriptorSetsSafe,
        module Graphics.Vulkan.Types.Enum.Descriptor,
        module Graphics.Vulkan.Types.Struct.CopyDescriptorSet,
        module Graphics.Vulkan.Types.Struct.Descriptor,
        module Graphics.Vulkan.Types.Struct.WriteDescriptorSet,
        -- ** Pass commands
        VkCreateFramebuffer, pattern VkCreateFramebuffer,
        HS_vkCreateFramebuffer, PFN_vkCreateFramebuffer,
        vkCreateFramebuffer, vkCreateFramebufferSafe, VkDestroyFramebuffer,
        pattern VkDestroyFramebuffer, HS_vkDestroyFramebuffer,
        PFN_vkDestroyFramebuffer, vkDestroyFramebuffer,
        vkDestroyFramebufferSafe, VkCreateRenderPass,
        pattern VkCreateRenderPass, HS_vkCreateRenderPass,
        PFN_vkCreateRenderPass, vkCreateRenderPass, vkCreateRenderPassSafe,
        VkDestroyRenderPass, pattern VkDestroyRenderPass,
        HS_vkDestroyRenderPass, PFN_vkDestroyRenderPass,
        vkDestroyRenderPass, vkDestroyRenderPassSafe,
        VkGetRenderAreaGranularity, pattern VkGetRenderAreaGranularity,
        HS_vkGetRenderAreaGranularity, PFN_vkGetRenderAreaGranularity,
        vkGetRenderAreaGranularity, vkGetRenderAreaGranularitySafe,
        module Graphics.Vulkan.Types.Enum.AccessFlags,
        module Graphics.Vulkan.Types.Enum.Attachment,
        module Graphics.Vulkan.Types.Enum.DependencyFlags,
        module Graphics.Vulkan.Types.Enum.Subpass,
        module Graphics.Vulkan.Types.Struct.Attachment,
        module Graphics.Vulkan.Types.Struct.FramebufferCreateInfo,
        module Graphics.Vulkan.Types.Struct.RenderPass,
        module Graphics.Vulkan.Types.Struct.Subpass, -- ** Command pool commands
                                                     VkCreateCommandPool,
        pattern VkCreateCommandPool, HS_vkCreateCommandPool,
        PFN_vkCreateCommandPool, vkCreateCommandPool,
        vkCreateCommandPoolSafe, VkDestroyCommandPool,
        pattern VkDestroyCommandPool, HS_vkDestroyCommandPool,
        PFN_vkDestroyCommandPool, vkDestroyCommandPool,
        vkDestroyCommandPoolSafe, VkResetCommandPool,
        pattern VkResetCommandPool, HS_vkResetCommandPool,
        PFN_vkResetCommandPool, vkResetCommandPool, vkResetCommandPoolSafe,
        module Graphics.Vulkan.Types.Enum.Command,
        module Graphics.Vulkan.Types.Struct.Command,
        -- ** Command buffer commands
        VkAllocateCommandBuffers, pattern VkAllocateCommandBuffers,
        HS_vkAllocateCommandBuffers, PFN_vkAllocateCommandBuffers,
        vkAllocateCommandBuffers, vkAllocateCommandBuffersSafe,
        VkFreeCommandBuffers, pattern VkFreeCommandBuffers,
        HS_vkFreeCommandBuffers, PFN_vkFreeCommandBuffers,
        vkFreeCommandBuffers, vkFreeCommandBuffersSafe,
        VkBeginCommandBuffer, pattern VkBeginCommandBuffer,
        HS_vkBeginCommandBuffer, PFN_vkBeginCommandBuffer,
        vkBeginCommandBuffer, vkBeginCommandBufferSafe, VkEndCommandBuffer,
        pattern VkEndCommandBuffer, HS_vkEndCommandBuffer,
        PFN_vkEndCommandBuffer, vkEndCommandBuffer, vkEndCommandBufferSafe,
        VkResetCommandBuffer, pattern VkResetCommandBuffer,
        HS_vkResetCommandBuffer, PFN_vkResetCommandBuffer,
        vkResetCommandBuffer, vkResetCommandBufferSafe, -- ** Command buffer building commands
                                                        VkCmdBindPipeline,
        pattern VkCmdBindPipeline, HS_vkCmdBindPipeline,
        PFN_vkCmdBindPipeline, vkCmdBindPipeline, vkCmdBindPipelineSafe,
        VkCmdSetViewport, pattern VkCmdSetViewport, HS_vkCmdSetViewport,
        PFN_vkCmdSetViewport, vkCmdSetViewport, vkCmdSetViewportSafe,
        VkCmdSetScissor, pattern VkCmdSetScissor, HS_vkCmdSetScissor,
        PFN_vkCmdSetScissor, vkCmdSetScissor, vkCmdSetScissorSafe,
        VkCmdSetLineWidth, pattern VkCmdSetLineWidth, HS_vkCmdSetLineWidth,
        PFN_vkCmdSetLineWidth, vkCmdSetLineWidth, vkCmdSetLineWidthSafe,
        VkCmdSetDepthBias, pattern VkCmdSetDepthBias, HS_vkCmdSetDepthBias,
        PFN_vkCmdSetDepthBias, vkCmdSetDepthBias, vkCmdSetDepthBiasSafe,
        VkCmdSetBlendConstants, pattern VkCmdSetBlendConstants,
        HS_vkCmdSetBlendConstants, PFN_vkCmdSetBlendConstants,
        vkCmdSetBlendConstants, vkCmdSetBlendConstantsSafe,
        VkCmdSetDepthBounds, pattern VkCmdSetDepthBounds,
        HS_vkCmdSetDepthBounds, PFN_vkCmdSetDepthBounds,
        vkCmdSetDepthBounds, vkCmdSetDepthBoundsSafe,
        VkCmdSetStencilCompareMask, pattern VkCmdSetStencilCompareMask,
        HS_vkCmdSetStencilCompareMask, PFN_vkCmdSetStencilCompareMask,
        vkCmdSetStencilCompareMask, vkCmdSetStencilCompareMaskSafe,
        VkCmdSetStencilWriteMask, pattern VkCmdSetStencilWriteMask,
        HS_vkCmdSetStencilWriteMask, PFN_vkCmdSetStencilWriteMask,
        vkCmdSetStencilWriteMask, vkCmdSetStencilWriteMaskSafe,
        VkCmdSetStencilReference, pattern VkCmdSetStencilReference,
        HS_vkCmdSetStencilReference, PFN_vkCmdSetStencilReference,
        vkCmdSetStencilReference, vkCmdSetStencilReferenceSafe,
        VkCmdBindDescriptorSets, pattern VkCmdBindDescriptorSets,
        HS_vkCmdBindDescriptorSets, PFN_vkCmdBindDescriptorSets,
        vkCmdBindDescriptorSets, vkCmdBindDescriptorSetsSafe,
        VkCmdBindIndexBuffer, pattern VkCmdBindIndexBuffer,
        HS_vkCmdBindIndexBuffer, PFN_vkCmdBindIndexBuffer,
        vkCmdBindIndexBuffer, vkCmdBindIndexBufferSafe,
        VkCmdBindVertexBuffers, pattern VkCmdBindVertexBuffers,
        HS_vkCmdBindVertexBuffers, PFN_vkCmdBindVertexBuffers,
        vkCmdBindVertexBuffers, vkCmdBindVertexBuffersSafe, VkCmdDraw,
        pattern VkCmdDraw, HS_vkCmdDraw, PFN_vkCmdDraw, vkCmdDraw,
        vkCmdDrawSafe, VkCmdDrawIndexed, pattern VkCmdDrawIndexed,
        HS_vkCmdDrawIndexed, PFN_vkCmdDrawIndexed, vkCmdDrawIndexed,
        vkCmdDrawIndexedSafe, VkCmdDrawIndirect, pattern VkCmdDrawIndirect,
        HS_vkCmdDrawIndirect, PFN_vkCmdDrawIndirect, vkCmdDrawIndirect,
        vkCmdDrawIndirectSafe, VkCmdDrawIndexedIndirect,
        pattern VkCmdDrawIndexedIndirect, HS_vkCmdDrawIndexedIndirect,
        PFN_vkCmdDrawIndexedIndirect, vkCmdDrawIndexedIndirect,
        vkCmdDrawIndexedIndirectSafe, VkCmdDispatch, pattern VkCmdDispatch,
        HS_vkCmdDispatch, PFN_vkCmdDispatch, vkCmdDispatch,
        vkCmdDispatchSafe, VkCmdDispatchIndirect,
        pattern VkCmdDispatchIndirect, HS_vkCmdDispatchIndirect,
        PFN_vkCmdDispatchIndirect, vkCmdDispatchIndirect,
        vkCmdDispatchIndirectSafe, VkCmdCopyBuffer,
        pattern VkCmdCopyBuffer, HS_vkCmdCopyBuffer, PFN_vkCmdCopyBuffer,
        vkCmdCopyBuffer, vkCmdCopyBufferSafe, VkCmdCopyImage,
        pattern VkCmdCopyImage, HS_vkCmdCopyImage, PFN_vkCmdCopyImage,
        vkCmdCopyImage, vkCmdCopyImageSafe, VkCmdBlitImage,
        pattern VkCmdBlitImage, HS_vkCmdBlitImage, PFN_vkCmdBlitImage,
        vkCmdBlitImage, vkCmdBlitImageSafe, VkCmdCopyBufferToImage,
        pattern VkCmdCopyBufferToImage, HS_vkCmdCopyBufferToImage,
        PFN_vkCmdCopyBufferToImage, vkCmdCopyBufferToImage,
        vkCmdCopyBufferToImageSafe, VkCmdCopyImageToBuffer,
        pattern VkCmdCopyImageToBuffer, HS_vkCmdCopyImageToBuffer,
        PFN_vkCmdCopyImageToBuffer, vkCmdCopyImageToBuffer,
        vkCmdCopyImageToBufferSafe, VkCmdUpdateBuffer,
        pattern VkCmdUpdateBuffer, HS_vkCmdUpdateBuffer,
        PFN_vkCmdUpdateBuffer, vkCmdUpdateBuffer, vkCmdUpdateBufferSafe,
        VkCmdFillBuffer, pattern VkCmdFillBuffer, HS_vkCmdFillBuffer,
        PFN_vkCmdFillBuffer, vkCmdFillBuffer, vkCmdFillBufferSafe,
        VkCmdClearColorImage, pattern VkCmdClearColorImage,
        HS_vkCmdClearColorImage, PFN_vkCmdClearColorImage,
        vkCmdClearColorImage, vkCmdClearColorImageSafe,
        VkCmdClearDepthStencilImage, pattern VkCmdClearDepthStencilImage,
        HS_vkCmdClearDepthStencilImage, PFN_vkCmdClearDepthStencilImage,
        vkCmdClearDepthStencilImage, vkCmdClearDepthStencilImageSafe,
        VkCmdClearAttachments, pattern VkCmdClearAttachments,
        HS_vkCmdClearAttachments, PFN_vkCmdClearAttachments,
        vkCmdClearAttachments, vkCmdClearAttachmentsSafe,
        VkCmdResolveImage, pattern VkCmdResolveImage, HS_vkCmdResolveImage,
        PFN_vkCmdResolveImage, vkCmdResolveImage, vkCmdResolveImageSafe,
        VkCmdSetEvent, pattern VkCmdSetEvent, HS_vkCmdSetEvent,
        PFN_vkCmdSetEvent, vkCmdSetEvent, vkCmdSetEventSafe,
        VkCmdResetEvent, pattern VkCmdResetEvent, HS_vkCmdResetEvent,
        PFN_vkCmdResetEvent, vkCmdResetEvent, vkCmdResetEventSafe,
        VkCmdWaitEvents, pattern VkCmdWaitEvents, HS_vkCmdWaitEvents,
        PFN_vkCmdWaitEvents, vkCmdWaitEvents, vkCmdWaitEventsSafe,
        VkCmdPipelineBarrier, pattern VkCmdPipelineBarrier,
        HS_vkCmdPipelineBarrier, PFN_vkCmdPipelineBarrier,
        vkCmdPipelineBarrier, vkCmdPipelineBarrierSafe, VkCmdBeginQuery,
        pattern VkCmdBeginQuery, HS_vkCmdBeginQuery, PFN_vkCmdBeginQuery,
        vkCmdBeginQuery, vkCmdBeginQuerySafe, VkCmdEndQuery,
        pattern VkCmdEndQuery, HS_vkCmdEndQuery, PFN_vkCmdEndQuery,
        vkCmdEndQuery, vkCmdEndQuerySafe, VkCmdResetQueryPool,
        pattern VkCmdResetQueryPool, HS_vkCmdResetQueryPool,
        PFN_vkCmdResetQueryPool, vkCmdResetQueryPool,
        vkCmdResetQueryPoolSafe, VkCmdWriteTimestamp,
        pattern VkCmdWriteTimestamp, HS_vkCmdWriteTimestamp,
        PFN_vkCmdWriteTimestamp, vkCmdWriteTimestamp,
        vkCmdWriteTimestampSafe, VkCmdCopyQueryPoolResults,
        pattern VkCmdCopyQueryPoolResults, HS_vkCmdCopyQueryPoolResults,
        PFN_vkCmdCopyQueryPoolResults, vkCmdCopyQueryPoolResults,
        vkCmdCopyQueryPoolResultsSafe, VkCmdPushConstants,
        pattern VkCmdPushConstants, HS_vkCmdPushConstants,
        PFN_vkCmdPushConstants, vkCmdPushConstants, vkCmdPushConstantsSafe,
        VkCmdBeginRenderPass, pattern VkCmdBeginRenderPass,
        HS_vkCmdBeginRenderPass, PFN_vkCmdBeginRenderPass,
        vkCmdBeginRenderPass, vkCmdBeginRenderPassSafe, VkCmdNextSubpass,
        pattern VkCmdNextSubpass, HS_vkCmdNextSubpass,
        PFN_vkCmdNextSubpass, vkCmdNextSubpass, vkCmdNextSubpassSafe,
        VkCmdEndRenderPass, pattern VkCmdEndRenderPass,
        HS_vkCmdEndRenderPass, PFN_vkCmdEndRenderPass, vkCmdEndRenderPass,
        vkCmdEndRenderPassSafe, VkCmdExecuteCommands,
        pattern VkCmdExecuteCommands, HS_vkCmdExecuteCommands,
        PFN_vkCmdExecuteCommands, vkCmdExecuteCommands,
        vkCmdExecuteCommandsSafe,
        module Graphics.Vulkan.Types.Enum.IndexType,
        module Graphics.Vulkan.Types.Struct.Clear,
        -- ** Types not directly used by the API. Include e.g. structs that are not parameter types of commands, but still defined by the API.
        module Graphics.Vulkan.Types.Struct.DispatchIndirectCommand,
        module Graphics.Vulkan.Types.Struct.DrawInd,
        module Graphics.Vulkan.Types.Enum.Object)
       where
import           GHC.Ptr                                                (Ptr (..))
import           Graphics.Vulkan.Constants                              (pattern VK_ATTACHMENT_UNUSED,
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
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.AccessFlags
import           Graphics.Vulkan.Types.Enum.Attachment
import           Graphics.Vulkan.Types.Enum.Blend
import           Graphics.Vulkan.Types.Enum.BorderColor
import           Graphics.Vulkan.Types.Enum.Buffer
import           Graphics.Vulkan.Types.Enum.Color
import           Graphics.Vulkan.Types.Enum.Command
import           Graphics.Vulkan.Types.Enum.CompareOp
import           Graphics.Vulkan.Types.Enum.ComponentSwizzle
import           Graphics.Vulkan.Types.Enum.CullModeFlags
import           Graphics.Vulkan.Types.Enum.DependencyFlags
import           Graphics.Vulkan.Types.Enum.Descriptor
import           Graphics.Vulkan.Types.Enum.Device
import           Graphics.Vulkan.Types.Enum.DynamicState
import           Graphics.Vulkan.Types.Enum.Fence
import           Graphics.Vulkan.Types.Enum.Filter
import           Graphics.Vulkan.Types.Enum.Format
import           Graphics.Vulkan.Types.Enum.FrontFace
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.IndexType
import           Graphics.Vulkan.Types.Enum.InternalAllocationType
import           Graphics.Vulkan.Types.Enum.LogicOp
import           Graphics.Vulkan.Types.Enum.Memory
import           Graphics.Vulkan.Types.Enum.Object
import           Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.Pipeline
import           Graphics.Vulkan.Types.Enum.PolygonMode
import           Graphics.Vulkan.Types.Enum.PrimitiveTopology
import           Graphics.Vulkan.Types.Enum.Query
import           Graphics.Vulkan.Types.Enum.Queue
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.Sampler
import           Graphics.Vulkan.Types.Enum.Shader
import           Graphics.Vulkan.Types.Enum.SharingMode
import           Graphics.Vulkan.Types.Enum.Sparse
import           Graphics.Vulkan.Types.Enum.Stencil
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.Subpass
import           Graphics.Vulkan.Types.Enum.SystemAllocationScope
import           Graphics.Vulkan.Types.Enum.VertexInputRate
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.AllocationCallbacks
import           Graphics.Vulkan.Types.Struct.ApplicationInfo
import           Graphics.Vulkan.Types.Struct.Attachment
import           Graphics.Vulkan.Types.Struct.Bind
import           Graphics.Vulkan.Types.Struct.Buffer
import           Graphics.Vulkan.Types.Struct.Clear
import           Graphics.Vulkan.Types.Struct.Command
import           Graphics.Vulkan.Types.Struct.ComponentMapping
import           Graphics.Vulkan.Types.Struct.ComputePipelineCreateInfo
import           Graphics.Vulkan.Types.Struct.CopyDescriptorSet
import           Graphics.Vulkan.Types.Struct.Descriptor
import           Graphics.Vulkan.Types.Struct.Device
import           Graphics.Vulkan.Types.Struct.DispatchIndirectCommand
import           Graphics.Vulkan.Types.Struct.DrawInd
import           Graphics.Vulkan.Types.Struct.EventCreateInfo
import           Graphics.Vulkan.Types.Struct.ExtensionProperties
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.Fence
import           Graphics.Vulkan.Types.Struct.FormatProperties
import           Graphics.Vulkan.Types.Struct.FramebufferCreateInfo
import           Graphics.Vulkan.Types.Struct.Image
import           Graphics.Vulkan.Types.Struct.InstanceCreateInfo
import           Graphics.Vulkan.Types.Struct.LayerProperties
import           Graphics.Vulkan.Types.Struct.MappedMemoryRange
import           Graphics.Vulkan.Types.Struct.Memory
import           Graphics.Vulkan.Types.Struct.Offset
import           Graphics.Vulkan.Types.Struct.PhysicalDevice
import           Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures
import           Graphics.Vulkan.Types.Struct.Pipeline
import           Graphics.Vulkan.Types.Struct.PushConstantRange
import           Graphics.Vulkan.Types.Struct.QueryPoolCreateInfo
import           Graphics.Vulkan.Types.Struct.QueueFamilyProperties
import           Graphics.Vulkan.Types.Struct.Rect
import           Graphics.Vulkan.Types.Struct.RenderPass
import           Graphics.Vulkan.Types.Struct.Sampler
import           Graphics.Vulkan.Types.Struct.Semaphore
import           Graphics.Vulkan.Types.Struct.Shader
import           Graphics.Vulkan.Types.Struct.Sparse
import           Graphics.Vulkan.Types.Struct.Specialization
import           Graphics.Vulkan.Types.Struct.StencilOpState
import           Graphics.Vulkan.Types.Struct.SubmitInfo
import           Graphics.Vulkan.Types.Struct.Subpass
import           Graphics.Vulkan.Types.Struct.SubresourceLayout
import           Graphics.Vulkan.Types.Struct.VertexInput
import           Graphics.Vulkan.Types.Struct.Viewport
import           Graphics.Vulkan.Types.Struct.WriteDescriptorSet
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED', 'VK_ERROR_LAYER_NOT_PRESENT', 'VK_ERROR_EXTENSION_NOT_PRESENT', 'VK_ERROR_INCOMPATIBLE_DRIVER'.
--
-- > VkResult vkCreateInstance
-- >     ( const VkInstanceCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkInstance* pInstance
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateInstance vkCreateInstance registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateInstance <- vkGetInstanceProc @VkCreateInstance VK_NULL
--
-- or less efficient:
--
-- > myCreateInstance <- vkGetProc @VkCreateInstance
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateInstance" vkCreateInstance ::
               Ptr VkInstanceCreateInfo -- ^ pCreateInfo
                                        ->
                 Ptr VkAllocationCallbacks -- ^ pAllocator
                                           -> Ptr VkInstance -- ^ pInstance
                                                             -> IO VkResult

##else
vkCreateInstance ::
                 Ptr VkInstanceCreateInfo -- ^ pCreateInfo
                                          ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkInstance -- ^ pInstance
                                                               -> IO VkResult
vkCreateInstance
  = unsafeDupablePerformIO (vkGetProc @VkCreateInstance)

{-# NOINLINE vkCreateInstance #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED', 'VK_ERROR_LAYER_NOT_PRESENT', 'VK_ERROR_EXTENSION_NOT_PRESENT', 'VK_ERROR_INCOMPATIBLE_DRIVER'.
--
-- > VkResult vkCreateInstance
-- >     ( const VkInstanceCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkInstance* pInstance
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateInstance vkCreateInstance registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateInstance <- vkGetInstanceProc @VkCreateInstance VK_NULL
--
-- or less efficient:
--
-- > myCreateInstance <- vkGetProc @VkCreateInstance
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateInstance" vkCreateInstanceSafe
               ::
               Ptr VkInstanceCreateInfo -- ^ pCreateInfo
                                        ->
                 Ptr VkAllocationCallbacks -- ^ pAllocator
                                           -> Ptr VkInstance -- ^ pInstance
                                                             -> IO VkResult

##else
vkCreateInstanceSafe ::
                     Ptr VkInstanceCreateInfo -- ^ pCreateInfo
                                              ->
                       Ptr VkAllocationCallbacks -- ^ pAllocator
                                                 -> Ptr VkInstance -- ^ pInstance
                                                                   -> IO VkResult
vkCreateInstanceSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateInstance)

{-# NOINLINE vkCreateInstanceSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateInstance vkCreateInstance registry at www.khronos.org>
type HS_vkCreateInstance =
     Ptr VkInstanceCreateInfo -- ^ pCreateInfo
                              ->
       Ptr VkAllocationCallbacks -- ^ pAllocator
                                 -> Ptr VkInstance -- ^ pInstance
                                                   -> IO VkResult

type PFN_vkCreateInstance = FunPtr HS_vkCreateInstance

foreign import ccall unsafe "dynamic" unwrapVkCreateInstance ::
               PFN_vkCreateInstance -> HS_vkCreateInstance

foreign import ccall safe "dynamic" unwrapVkCreateInstanceSafe ::
               PFN_vkCreateInstance -> HS_vkCreateInstance

instance VulkanProc "vkCreateInstance" where
        type VkProcType "vkCreateInstance" = HS_vkCreateInstance
        vkProcSymbol = _VkCreateInstance

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateInstance

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateInstanceSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyInstance
-- >     ( VkInstance instance
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyInstance vkDestroyInstance registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyInstance <- vkGetInstanceProc @VkDestroyInstance vkInstance
--
-- or less efficient:
--
-- > myDestroyInstance <- vkGetProc @VkDestroyInstance
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyInstance" vkDestroyInstance
               :: VkInstance -- ^ instance
                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          -> IO ()

##else
vkDestroyInstance ::
                  VkInstance -- ^ instance
                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          -> IO ()
vkDestroyInstance
  = unsafeDupablePerformIO (vkGetProc @VkDestroyInstance)

{-# NOINLINE vkDestroyInstance #-}
##endif

-- |
-- > void vkDestroyInstance
-- >     ( VkInstance instance
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyInstance vkDestroyInstance registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyInstance <- vkGetInstanceProc @VkDestroyInstance vkInstance
--
-- or less efficient:
--
-- > myDestroyInstance <- vkGetProc @VkDestroyInstance
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyInstance" vkDestroyInstanceSafe
               :: VkInstance -- ^ instance
                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          -> IO ()

##else
vkDestroyInstanceSafe ::
                      VkInstance -- ^ instance
                                 -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                              -> IO ()
vkDestroyInstanceSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyInstance)

{-# NOINLINE vkDestroyInstanceSafe #-}
##endif

-- | > void vkDestroyInstance
--   >     ( VkInstance instance
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyInstance vkDestroyInstance registry at www.khronos.org>
type HS_vkDestroyInstance =
     VkInstance -- ^ instance
                -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> IO ()

type PFN_vkDestroyInstance = FunPtr HS_vkDestroyInstance

foreign import ccall unsafe "dynamic" unwrapVkDestroyInstance ::
               PFN_vkDestroyInstance -> HS_vkDestroyInstance

foreign import ccall safe "dynamic" unwrapVkDestroyInstanceSafe ::
               PFN_vkDestroyInstance -> HS_vkDestroyInstance

instance VulkanProc "vkDestroyInstance" where
        type VkProcType "vkDestroyInstance" = HS_vkDestroyInstance
        vkProcSymbol = _VkDestroyInstance

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyInstance

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyInstanceSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
-- > VkResult vkEnumeratePhysicalDevices
-- >     ( VkInstance instance
-- >     , uint32_t* pPhysicalDeviceCount
-- >     , VkPhysicalDevice* pPhysicalDevices
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumeratePhysicalDevices vkEnumeratePhysicalDevices registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumeratePhysicalDevices <- vkGetInstanceProc @VkEnumeratePhysicalDevices vkInstance
--
-- or less efficient:
--
-- > myEnumeratePhysicalDevices <- vkGetProc @VkEnumeratePhysicalDevices
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkEnumeratePhysicalDevices"
               vkEnumeratePhysicalDevices ::
               VkInstance -- ^ instance
                          -> Ptr Word32 -- ^ pPhysicalDeviceCount
                                        -> Ptr VkPhysicalDevice -- ^ pPhysicalDevices
                                                                -> IO VkResult

##else
vkEnumeratePhysicalDevices ::
                           VkInstance -- ^ instance
                                      -> Ptr Word32 -- ^ pPhysicalDeviceCount
                                                    -> Ptr VkPhysicalDevice -- ^ pPhysicalDevices
                                                                            -> IO VkResult
vkEnumeratePhysicalDevices
  = unsafeDupablePerformIO (vkGetProc @VkEnumeratePhysicalDevices)

{-# NOINLINE vkEnumeratePhysicalDevices #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
-- > VkResult vkEnumeratePhysicalDevices
-- >     ( VkInstance instance
-- >     , uint32_t* pPhysicalDeviceCount
-- >     , VkPhysicalDevice* pPhysicalDevices
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumeratePhysicalDevices vkEnumeratePhysicalDevices registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumeratePhysicalDevices <- vkGetInstanceProc @VkEnumeratePhysicalDevices vkInstance
--
-- or less efficient:
--
-- > myEnumeratePhysicalDevices <- vkGetProc @VkEnumeratePhysicalDevices
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkEnumeratePhysicalDevices"
               vkEnumeratePhysicalDevicesSafe ::
               VkInstance -- ^ instance
                          -> Ptr Word32 -- ^ pPhysicalDeviceCount
                                        -> Ptr VkPhysicalDevice -- ^ pPhysicalDevices
                                                                -> IO VkResult

##else
vkEnumeratePhysicalDevicesSafe ::
                               VkInstance -- ^ instance
                                          -> Ptr Word32 -- ^ pPhysicalDeviceCount
                                                        -> Ptr VkPhysicalDevice -- ^ pPhysicalDevices
                                                                                -> IO VkResult
vkEnumeratePhysicalDevicesSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkEnumeratePhysicalDevices)

{-# NOINLINE vkEnumeratePhysicalDevicesSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumeratePhysicalDevices vkEnumeratePhysicalDevices registry at www.khronos.org>
type HS_vkEnumeratePhysicalDevices =
     VkInstance -- ^ instance
                -> Ptr Word32 -- ^ pPhysicalDeviceCount
                              -> Ptr VkPhysicalDevice -- ^ pPhysicalDevices
                                                      -> IO VkResult

type PFN_vkEnumeratePhysicalDevices =
     FunPtr HS_vkEnumeratePhysicalDevices

foreign import ccall unsafe "dynamic"
               unwrapVkEnumeratePhysicalDevices ::
               PFN_vkEnumeratePhysicalDevices -> HS_vkEnumeratePhysicalDevices

foreign import ccall safe "dynamic"
               unwrapVkEnumeratePhysicalDevicesSafe ::
               PFN_vkEnumeratePhysicalDevices -> HS_vkEnumeratePhysicalDevices

instance VulkanProc "vkEnumeratePhysicalDevices" where
        type VkProcType "vkEnumeratePhysicalDevices" =
             HS_vkEnumeratePhysicalDevices
        vkProcSymbol = _VkEnumeratePhysicalDevices

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkEnumeratePhysicalDevices

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkEnumeratePhysicalDevicesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkGetPhysicalDeviceFeatures
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceFeatures* pFeatures
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFeatures vkGetPhysicalDeviceFeatures registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceFeatures <- vkGetInstanceProc @VkGetPhysicalDeviceFeatures vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceFeatures <- vkGetProc @VkGetPhysicalDeviceFeatures
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetPhysicalDeviceFeatures"
               vkGetPhysicalDeviceFeatures ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceFeatures -- ^ pFeatures
                                                                -> IO ()

##else
vkGetPhysicalDeviceFeatures ::
                            VkPhysicalDevice -- ^ physicalDevice
                                             -> Ptr VkPhysicalDeviceFeatures -- ^ pFeatures
                                                                             -> IO ()
vkGetPhysicalDeviceFeatures
  = unsafeDupablePerformIO (vkGetProc @VkGetPhysicalDeviceFeatures)

{-# NOINLINE vkGetPhysicalDeviceFeatures #-}
##endif

-- |
-- > void vkGetPhysicalDeviceFeatures
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceFeatures* pFeatures
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFeatures vkGetPhysicalDeviceFeatures registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceFeatures <- vkGetInstanceProc @VkGetPhysicalDeviceFeatures vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceFeatures <- vkGetProc @VkGetPhysicalDeviceFeatures
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetPhysicalDeviceFeatures"
               vkGetPhysicalDeviceFeaturesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceFeatures -- ^ pFeatures
                                                                -> IO ()

##else
vkGetPhysicalDeviceFeaturesSafe ::
                                VkPhysicalDevice -- ^ physicalDevice
                                                 -> Ptr VkPhysicalDeviceFeatures -- ^ pFeatures
                                                                                 -> IO ()
vkGetPhysicalDeviceFeaturesSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceFeatures)

{-# NOINLINE vkGetPhysicalDeviceFeaturesSafe #-}
##endif

-- | > void vkGetPhysicalDeviceFeatures
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFeatures vkGetPhysicalDeviceFeatures registry at www.khronos.org>
type HS_vkGetPhysicalDeviceFeatures =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Ptr VkPhysicalDeviceFeatures -- ^ pFeatures
                                                      -> IO ()

type PFN_vkGetPhysicalDeviceFeatures =
     FunPtr HS_vkGetPhysicalDeviceFeatures

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceFeatures ::
               PFN_vkGetPhysicalDeviceFeatures -> HS_vkGetPhysicalDeviceFeatures

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceFeaturesSafe ::
               PFN_vkGetPhysicalDeviceFeatures -> HS_vkGetPhysicalDeviceFeatures

instance VulkanProc "vkGetPhysicalDeviceFeatures" where
        type VkProcType "vkGetPhysicalDeviceFeatures" =
             HS_vkGetPhysicalDeviceFeatures
        vkProcSymbol = _VkGetPhysicalDeviceFeatures

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceFeatures

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetPhysicalDeviceFeaturesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkGetPhysicalDeviceFormatProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkFormat format
-- >     , VkFormatProperties* pFormatProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFormatProperties vkGetPhysicalDeviceFormatProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceFormatProperties <- vkGetInstanceProc @VkGetPhysicalDeviceFormatProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceFormatProperties <- vkGetProc @VkGetPhysicalDeviceFormatProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetPhysicalDeviceFormatProperties"
               vkGetPhysicalDeviceFormatProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkFormat -- ^ format
                                            -> Ptr VkFormatProperties -- ^ pFormatProperties
                                                                      -> IO ()

##else
vkGetPhysicalDeviceFormatProperties ::
                                    VkPhysicalDevice -- ^ physicalDevice
                                                     -> VkFormat -- ^ format
                                                                 -> Ptr VkFormatProperties -- ^ pFormatProperties
                                                                                           -> IO ()
vkGetPhysicalDeviceFormatProperties
  = unsafeDupablePerformIO
      (vkGetProc @VkGetPhysicalDeviceFormatProperties)

{-# NOINLINE vkGetPhysicalDeviceFormatProperties #-}
##endif

-- |
-- > void vkGetPhysicalDeviceFormatProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkFormat format
-- >     , VkFormatProperties* pFormatProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFormatProperties vkGetPhysicalDeviceFormatProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceFormatProperties <- vkGetInstanceProc @VkGetPhysicalDeviceFormatProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceFormatProperties <- vkGetProc @VkGetPhysicalDeviceFormatProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetPhysicalDeviceFormatProperties"
               vkGetPhysicalDeviceFormatPropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkFormat -- ^ format
                                            -> Ptr VkFormatProperties -- ^ pFormatProperties
                                                                      -> IO ()

##else
vkGetPhysicalDeviceFormatPropertiesSafe ::
                                        VkPhysicalDevice -- ^ physicalDevice
                                                         ->
                                          VkFormat -- ^ format
                                                   -> Ptr VkFormatProperties -- ^ pFormatProperties
                                                                             -> IO ()
vkGetPhysicalDeviceFormatPropertiesSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceFormatProperties)

{-# NOINLINE vkGetPhysicalDeviceFormatPropertiesSafe #-}
##endif

-- | > void vkGetPhysicalDeviceFormatProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFormatProperties vkGetPhysicalDeviceFormatProperties registry at www.khronos.org>
type HS_vkGetPhysicalDeviceFormatProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      -> VkFormat -- ^ format
                                  -> Ptr VkFormatProperties -- ^ pFormatProperties
                                                            -> IO ()

type PFN_vkGetPhysicalDeviceFormatProperties =
     FunPtr HS_vkGetPhysicalDeviceFormatProperties

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceFormatProperties ::
               PFN_vkGetPhysicalDeviceFormatProperties ->
                 HS_vkGetPhysicalDeviceFormatProperties

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceFormatPropertiesSafe ::
               PFN_vkGetPhysicalDeviceFormatProperties ->
                 HS_vkGetPhysicalDeviceFormatProperties

instance VulkanProc "vkGetPhysicalDeviceFormatProperties" where
        type VkProcType "vkGetPhysicalDeviceFormatProperties" =
             HS_vkGetPhysicalDeviceFormatProperties
        vkProcSymbol = _VkGetPhysicalDeviceFormatProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceFormatProperties

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetPhysicalDeviceFormatPropertiesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
-- > VkResult vkGetPhysicalDeviceImageFormatProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkFormat format
-- >     , VkImageType type
-- >     , VkImageTiling tiling
-- >     , VkImageUsageFlags usage
-- >     , VkImageCreateFlags flags
-- >     , VkImageFormatProperties* pImageFormatProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceImageFormatProperties vkGetPhysicalDeviceImageFormatProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceImageFormatProperties <- vkGetInstanceProc @VkGetPhysicalDeviceImageFormatProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceImageFormatProperties <- vkGetProc @VkGetPhysicalDeviceImageFormatProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
                                                                      ->
                                                     Ptr VkImageFormatProperties -- ^ pImageFormatProperties
                                                                                 -> IO VkResult
vkGetPhysicalDeviceImageFormatProperties
  = unsafeDupablePerformIO
      (vkGetProc @VkGetPhysicalDeviceImageFormatProperties)

{-# NOINLINE vkGetPhysicalDeviceImageFormatProperties #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
-- > VkResult vkGetPhysicalDeviceImageFormatProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkFormat format
-- >     , VkImageType type
-- >     , VkImageTiling tiling
-- >     , VkImageUsageFlags usage
-- >     , VkImageCreateFlags flags
-- >     , VkImageFormatProperties* pImageFormatProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceImageFormatProperties vkGetPhysicalDeviceImageFormatProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceImageFormatProperties <- vkGetInstanceProc @VkGetPhysicalDeviceImageFormatProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceImageFormatProperties <- vkGetProc @VkGetPhysicalDeviceImageFormatProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
                                                                          ->
                                                         Ptr VkImageFormatProperties -- ^ pImageFormatProperties
                                                                                     -> IO VkResult
vkGetPhysicalDeviceImageFormatPropertiesSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceImageFormatProperties)

{-# NOINLINE vkGetPhysicalDeviceImageFormatPropertiesSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceImageFormatProperties vkGetPhysicalDeviceImageFormatProperties registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceImageFormatProperties ::
               PFN_vkGetPhysicalDeviceImageFormatProperties ->
                 HS_vkGetPhysicalDeviceImageFormatProperties

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceImageFormatPropertiesSafe ::
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
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceImageFormatPropertiesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkGetPhysicalDeviceProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceProperties* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceProperties vkGetPhysicalDeviceProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceProperties <- vkGetInstanceProc @VkGetPhysicalDeviceProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceProperties <- vkGetProc @VkGetPhysicalDeviceProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetPhysicalDeviceProperties"
               vkGetPhysicalDeviceProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceProperties -- ^ pProperties
                                                                  -> IO ()

##else
vkGetPhysicalDeviceProperties ::
                              VkPhysicalDevice -- ^ physicalDevice
                                               -> Ptr VkPhysicalDeviceProperties -- ^ pProperties
                                                                                 -> IO ()
vkGetPhysicalDeviceProperties
  = unsafeDupablePerformIO (vkGetProc @VkGetPhysicalDeviceProperties)

{-# NOINLINE vkGetPhysicalDeviceProperties #-}
##endif

-- |
-- > void vkGetPhysicalDeviceProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceProperties* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceProperties vkGetPhysicalDeviceProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceProperties <- vkGetInstanceProc @VkGetPhysicalDeviceProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceProperties <- vkGetProc @VkGetPhysicalDeviceProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetPhysicalDeviceProperties"
               vkGetPhysicalDevicePropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceProperties -- ^ pProperties
                                                                  -> IO ()

##else
vkGetPhysicalDevicePropertiesSafe ::
                                  VkPhysicalDevice -- ^ physicalDevice
                                                   -> Ptr VkPhysicalDeviceProperties -- ^ pProperties
                                                                                     -> IO ()
vkGetPhysicalDevicePropertiesSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceProperties)

{-# NOINLINE vkGetPhysicalDevicePropertiesSafe #-}
##endif

-- | > void vkGetPhysicalDeviceProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceProperties vkGetPhysicalDeviceProperties registry at www.khronos.org>
type HS_vkGetPhysicalDeviceProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Ptr VkPhysicalDeviceProperties -- ^ pProperties
                                                        -> IO ()

type PFN_vkGetPhysicalDeviceProperties =
     FunPtr HS_vkGetPhysicalDeviceProperties

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceProperties ::
               PFN_vkGetPhysicalDeviceProperties ->
                 HS_vkGetPhysicalDeviceProperties

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDevicePropertiesSafe ::
               PFN_vkGetPhysicalDeviceProperties ->
                 HS_vkGetPhysicalDeviceProperties

instance VulkanProc "vkGetPhysicalDeviceProperties" where
        type VkProcType "vkGetPhysicalDeviceProperties" =
             HS_vkGetPhysicalDeviceProperties
        vkProcSymbol = _VkGetPhysicalDeviceProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceProperties

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetPhysicalDevicePropertiesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkGetPhysicalDeviceQueueFamilyProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t* pQueueFamilyPropertyCount
-- >     , VkQueueFamilyProperties* pQueueFamilyProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceQueueFamilyProperties vkGetPhysicalDeviceQueueFamilyProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceQueueFamilyProperties <- vkGetInstanceProc @VkGetPhysicalDeviceQueueFamilyProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceQueueFamilyProperties <- vkGetProc @VkGetPhysicalDeviceQueueFamilyProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe
               "vkGetPhysicalDeviceQueueFamilyProperties"
               vkGetPhysicalDeviceQueueFamilyProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pQueueFamilyPropertyCount
                            -> Ptr VkQueueFamilyProperties -- ^ pQueueFamilyProperties
                                                           -> IO ()

##else
vkGetPhysicalDeviceQueueFamilyProperties ::
                                         VkPhysicalDevice -- ^ physicalDevice
                                                          ->
                                           Ptr Word32 -- ^ pQueueFamilyPropertyCount
                                                      -> Ptr VkQueueFamilyProperties -- ^ pQueueFamilyProperties
                                                                                     -> IO ()
vkGetPhysicalDeviceQueueFamilyProperties
  = unsafeDupablePerformIO
      (vkGetProc @VkGetPhysicalDeviceQueueFamilyProperties)

{-# NOINLINE vkGetPhysicalDeviceQueueFamilyProperties #-}
##endif

-- |
-- > void vkGetPhysicalDeviceQueueFamilyProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t* pQueueFamilyPropertyCount
-- >     , VkQueueFamilyProperties* pQueueFamilyProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceQueueFamilyProperties vkGetPhysicalDeviceQueueFamilyProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceQueueFamilyProperties <- vkGetInstanceProc @VkGetPhysicalDeviceQueueFamilyProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceQueueFamilyProperties <- vkGetProc @VkGetPhysicalDeviceQueueFamilyProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe
               "vkGetPhysicalDeviceQueueFamilyProperties"
               vkGetPhysicalDeviceQueueFamilyPropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pQueueFamilyPropertyCount
                            -> Ptr VkQueueFamilyProperties -- ^ pQueueFamilyProperties
                                                           -> IO ()

##else
vkGetPhysicalDeviceQueueFamilyPropertiesSafe ::
                                             VkPhysicalDevice -- ^ physicalDevice
                                                              ->
                                               Ptr Word32 -- ^ pQueueFamilyPropertyCount
                                                          -> Ptr VkQueueFamilyProperties -- ^ pQueueFamilyProperties
                                                                                         -> IO ()
vkGetPhysicalDeviceQueueFamilyPropertiesSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceQueueFamilyProperties)

{-# NOINLINE vkGetPhysicalDeviceQueueFamilyPropertiesSafe #-}
##endif

-- | > void vkGetPhysicalDeviceQueueFamilyProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceQueueFamilyProperties vkGetPhysicalDeviceQueueFamilyProperties registry at www.khronos.org>
type HS_vkGetPhysicalDeviceQueueFamilyProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr Word32 -- ^ pQueueFamilyPropertyCount
                  -> Ptr VkQueueFamilyProperties -- ^ pQueueFamilyProperties
                                                 -> IO ()

type PFN_vkGetPhysicalDeviceQueueFamilyProperties =
     FunPtr HS_vkGetPhysicalDeviceQueueFamilyProperties

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceQueueFamilyProperties ::
               PFN_vkGetPhysicalDeviceQueueFamilyProperties ->
                 HS_vkGetPhysicalDeviceQueueFamilyProperties

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceQueueFamilyPropertiesSafe ::
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
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceQueueFamilyPropertiesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkGetPhysicalDeviceMemoryProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceMemoryProperties* pMemoryProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMemoryProperties vkGetPhysicalDeviceMemoryProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceMemoryProperties <- vkGetInstanceProc @VkGetPhysicalDeviceMemoryProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceMemoryProperties <- vkGetProc @VkGetPhysicalDeviceMemoryProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetPhysicalDeviceMemoryProperties"
               vkGetPhysicalDeviceMemoryProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceMemoryProperties -- ^ pMemoryProperties
                                                                        -> IO ()

##else
vkGetPhysicalDeviceMemoryProperties ::
                                    VkPhysicalDevice -- ^ physicalDevice
                                                     ->
                                      Ptr VkPhysicalDeviceMemoryProperties -- ^ pMemoryProperties
                                                                           -> IO ()
vkGetPhysicalDeviceMemoryProperties
  = unsafeDupablePerformIO
      (vkGetProc @VkGetPhysicalDeviceMemoryProperties)

{-# NOINLINE vkGetPhysicalDeviceMemoryProperties #-}
##endif

-- |
-- > void vkGetPhysicalDeviceMemoryProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceMemoryProperties* pMemoryProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMemoryProperties vkGetPhysicalDeviceMemoryProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceMemoryProperties <- vkGetInstanceProc @VkGetPhysicalDeviceMemoryProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceMemoryProperties <- vkGetProc @VkGetPhysicalDeviceMemoryProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetPhysicalDeviceMemoryProperties"
               vkGetPhysicalDeviceMemoryPropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceMemoryProperties -- ^ pMemoryProperties
                                                                        -> IO ()

##else
vkGetPhysicalDeviceMemoryPropertiesSafe ::
                                        VkPhysicalDevice -- ^ physicalDevice
                                                         ->
                                          Ptr VkPhysicalDeviceMemoryProperties -- ^ pMemoryProperties
                                                                               -> IO ()
vkGetPhysicalDeviceMemoryPropertiesSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceMemoryProperties)

{-# NOINLINE vkGetPhysicalDeviceMemoryPropertiesSafe #-}
##endif

-- | > void vkGetPhysicalDeviceMemoryProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMemoryProperties vkGetPhysicalDeviceMemoryProperties registry at www.khronos.org>
type HS_vkGetPhysicalDeviceMemoryProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Ptr VkPhysicalDeviceMemoryProperties -- ^ pMemoryProperties
                                                              -> IO ()

type PFN_vkGetPhysicalDeviceMemoryProperties =
     FunPtr HS_vkGetPhysicalDeviceMemoryProperties

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceMemoryProperties ::
               PFN_vkGetPhysicalDeviceMemoryProperties ->
                 HS_vkGetPhysicalDeviceMemoryProperties

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceMemoryPropertiesSafe ::
               PFN_vkGetPhysicalDeviceMemoryProperties ->
                 HS_vkGetPhysicalDeviceMemoryProperties

instance VulkanProc "vkGetPhysicalDeviceMemoryProperties" where
        type VkProcType "vkGetPhysicalDeviceMemoryProperties" =
             HS_vkGetPhysicalDeviceMemoryProperties
        vkProcSymbol = _VkGetPhysicalDeviceMemoryProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceMemoryProperties

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetPhysicalDeviceMemoryPropertiesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > PFN_vkVoidFunction vkGetInstanceProcAddr
-- >     ( VkInstance instance
-- >     , const char* pName
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetInstanceProcAddr vkGetInstanceProcAddr registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetInstanceProcAddr <- vkGetInstanceProc @VkGetInstanceProcAddr vkInstance
--
-- or less efficient:
--
-- > myGetInstanceProcAddr <- vkGetProc @VkGetInstanceProcAddr
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetInstanceProcAddr"
               vkGetInstanceProcAddr ::
               VkInstance -- ^ instance
                          -> CString -- ^ pName
                                     -> IO PFN_vkVoidFunction

##else
vkGetInstanceProcAddr ::
                      VkInstance -- ^ instance
                                 -> CString -- ^ pName
                                            -> IO PFN_vkVoidFunction
vkGetInstanceProcAddr
  = unsafeDupablePerformIO (vkGetProc @VkGetInstanceProcAddr)

{-# NOINLINE vkGetInstanceProcAddr #-}
##endif

-- |
-- > PFN_vkVoidFunction vkGetInstanceProcAddr
-- >     ( VkInstance instance
-- >     , const char* pName
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetInstanceProcAddr vkGetInstanceProcAddr registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetInstanceProcAddr <- vkGetInstanceProc @VkGetInstanceProcAddr vkInstance
--
-- or less efficient:
--
-- > myGetInstanceProcAddr <- vkGetProc @VkGetInstanceProcAddr
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetInstanceProcAddr"
               vkGetInstanceProcAddrSafe ::
               VkInstance -- ^ instance
                          -> CString -- ^ pName
                                     -> IO PFN_vkVoidFunction

##else
vkGetInstanceProcAddrSafe ::
                          VkInstance -- ^ instance
                                     -> CString -- ^ pName
                                                -> IO PFN_vkVoidFunction
vkGetInstanceProcAddrSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkGetInstanceProcAddr)

{-# NOINLINE vkGetInstanceProcAddrSafe #-}
##endif

-- | > PFN_vkVoidFunction vkGetInstanceProcAddr
--   >     ( VkInstance instance
--   >     , const char* pName
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetInstanceProcAddr vkGetInstanceProcAddr registry at www.khronos.org>
type HS_vkGetInstanceProcAddr =
     VkInstance -- ^ instance
                -> CString -- ^ pName
                           -> IO PFN_vkVoidFunction

type PFN_vkGetInstanceProcAddr = FunPtr HS_vkGetInstanceProcAddr

foreign import ccall unsafe "dynamic" unwrapVkGetInstanceProcAddr
               :: PFN_vkGetInstanceProcAddr -> HS_vkGetInstanceProcAddr

foreign import ccall safe "dynamic" unwrapVkGetInstanceProcAddrSafe
               :: PFN_vkGetInstanceProcAddr -> HS_vkGetInstanceProcAddr

instance VulkanProc "vkGetInstanceProcAddr" where
        type VkProcType "vkGetInstanceProcAddr" = HS_vkGetInstanceProcAddr
        vkProcSymbol = _VkGetInstanceProcAddr

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetInstanceProcAddr

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetInstanceProcAddrSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > PFN_vkVoidFunction vkGetDeviceProcAddr
-- >     ( VkDevice device
-- >     , const char* pName
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceProcAddr vkGetDeviceProcAddr registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceProcAddr <- vkGetDeviceProc @VkGetDeviceProcAddr vkDevice
--
-- or less efficient:
--
-- > myGetDeviceProcAddr <- vkGetProc @VkGetDeviceProcAddr
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetDeviceProcAddr"
               vkGetDeviceProcAddr :: VkDevice -- ^ device
                                               -> CString -- ^ pName
                                                          -> IO PFN_vkVoidFunction

##else
vkGetDeviceProcAddr :: VkDevice -- ^ device
                                -> CString -- ^ pName
                                           -> IO PFN_vkVoidFunction
vkGetDeviceProcAddr
  = unsafeDupablePerformIO (vkGetProc @VkGetDeviceProcAddr)

{-# NOINLINE vkGetDeviceProcAddr #-}
##endif

-- |
-- > PFN_vkVoidFunction vkGetDeviceProcAddr
-- >     ( VkDevice device
-- >     , const char* pName
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceProcAddr vkGetDeviceProcAddr registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceProcAddr <- vkGetDeviceProc @VkGetDeviceProcAddr vkDevice
--
-- or less efficient:
--
-- > myGetDeviceProcAddr <- vkGetProc @VkGetDeviceProcAddr
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetDeviceProcAddr"
               vkGetDeviceProcAddrSafe ::
               VkDevice -- ^ device
                        -> CString -- ^ pName
                                   -> IO PFN_vkVoidFunction

##else
vkGetDeviceProcAddrSafe ::
                        VkDevice -- ^ device
                                 -> CString -- ^ pName
                                            -> IO PFN_vkVoidFunction
vkGetDeviceProcAddrSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkGetDeviceProcAddr)

{-# NOINLINE vkGetDeviceProcAddrSafe #-}
##endif

-- | > PFN_vkVoidFunction vkGetDeviceProcAddr
--   >     ( VkDevice device
--   >     , const char* pName
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceProcAddr vkGetDeviceProcAddr registry at www.khronos.org>
type HS_vkGetDeviceProcAddr =
     VkDevice -- ^ device
              -> CString -- ^ pName
                         -> IO PFN_vkVoidFunction

type PFN_vkGetDeviceProcAddr = FunPtr HS_vkGetDeviceProcAddr

foreign import ccall unsafe "dynamic" unwrapVkGetDeviceProcAddr ::
               PFN_vkGetDeviceProcAddr -> HS_vkGetDeviceProcAddr

foreign import ccall safe "dynamic" unwrapVkGetDeviceProcAddrSafe
               :: PFN_vkGetDeviceProcAddr -> HS_vkGetDeviceProcAddr

instance VulkanProc "vkGetDeviceProcAddr" where
        type VkProcType "vkGetDeviceProcAddr" = HS_vkGetDeviceProcAddr
        vkProcSymbol = _VkGetDeviceProcAddr

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetDeviceProcAddr

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetDeviceProcAddrSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED', 'VK_ERROR_EXTENSION_NOT_PRESENT', 'VK_ERROR_FEATURE_NOT_PRESENT', 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkCreateDevice
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkDeviceCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkDevice* pDevice
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDevice vkCreateDevice registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateDevice <- vkGetInstanceProc @VkCreateDevice vkInstance
--
-- or less efficient:
--
-- > myCreateDevice <- vkGetProc @VkCreateDevice
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateDevice" vkCreateDevice ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkDeviceCreateInfo -- ^ pCreateInfo
                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkDevice -- ^ pDevice
                                                             -> IO VkResult

##else
vkCreateDevice ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkDeviceCreateInfo -- ^ pCreateInfo
                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkDevice -- ^ pDevice
                                                             -> IO VkResult
vkCreateDevice = unsafeDupablePerformIO (vkGetProc @VkCreateDevice)

{-# NOINLINE vkCreateDevice #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED', 'VK_ERROR_EXTENSION_NOT_PRESENT', 'VK_ERROR_FEATURE_NOT_PRESENT', 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkCreateDevice
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkDeviceCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkDevice* pDevice
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDevice vkCreateDevice registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateDevice <- vkGetInstanceProc @VkCreateDevice vkInstance
--
-- or less efficient:
--
-- > myCreateDevice <- vkGetProc @VkCreateDevice
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateDevice" vkCreateDeviceSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkDeviceCreateInfo -- ^ pCreateInfo
                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkDevice -- ^ pDevice
                                                             -> IO VkResult

##else
vkCreateDeviceSafe ::
                   VkPhysicalDevice -- ^ physicalDevice
                                    ->
                     Ptr VkDeviceCreateInfo -- ^ pCreateInfo
                                            ->
                       Ptr VkAllocationCallbacks -- ^ pAllocator
                                                 -> Ptr VkDevice -- ^ pDevice
                                                                 -> IO VkResult
vkCreateDeviceSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateDevice)

{-# NOINLINE vkCreateDeviceSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDevice vkCreateDevice registry at www.khronos.org>
type HS_vkCreateDevice =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkDeviceCreateInfo -- ^ pCreateInfo
                              ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkDevice -- ^ pDevice
                                                   -> IO VkResult

type PFN_vkCreateDevice = FunPtr HS_vkCreateDevice

foreign import ccall unsafe "dynamic" unwrapVkCreateDevice ::
               PFN_vkCreateDevice -> HS_vkCreateDevice

foreign import ccall safe "dynamic" unwrapVkCreateDeviceSafe ::
               PFN_vkCreateDevice -> HS_vkCreateDevice

instance VulkanProc "vkCreateDevice" where
        type VkProcType "vkCreateDevice" = HS_vkCreateDevice
        vkProcSymbol = _VkCreateDevice

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateDevice

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateDeviceSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyDevice
-- >     ( VkDevice device
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDevice vkDestroyDevice registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyDevice <- vkGetDeviceProc @VkDestroyDevice vkDevice
--
-- or less efficient:
--
-- > myDestroyDevice <- vkGetProc @VkDestroyDevice
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyDevice" vkDestroyDevice ::
               VkDevice -- ^ device
                        -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                     -> IO ()

##else
vkDestroyDevice :: VkDevice -- ^ device
                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         -> IO ()
vkDestroyDevice
  = unsafeDupablePerformIO (vkGetProc @VkDestroyDevice)

{-# NOINLINE vkDestroyDevice #-}
##endif

-- |
-- > void vkDestroyDevice
-- >     ( VkDevice device
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDevice vkDestroyDevice registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyDevice <- vkGetDeviceProc @VkDestroyDevice vkDevice
--
-- or less efficient:
--
-- > myDestroyDevice <- vkGetProc @VkDestroyDevice
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyDevice" vkDestroyDeviceSafe ::
               VkDevice -- ^ device
                        -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                     -> IO ()

##else
vkDestroyDeviceSafe ::
                    VkDevice -- ^ device
                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          -> IO ()
vkDestroyDeviceSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyDevice)

{-# NOINLINE vkDestroyDeviceSafe #-}
##endif

-- | > void vkDestroyDevice
--   >     ( VkDevice device
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDevice vkDestroyDevice registry at www.khronos.org>
type HS_vkDestroyDevice =
     VkDevice -- ^ device
              -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                           -> IO ()

type PFN_vkDestroyDevice = FunPtr HS_vkDestroyDevice

foreign import ccall unsafe "dynamic" unwrapVkDestroyDevice ::
               PFN_vkDestroyDevice -> HS_vkDestroyDevice

foreign import ccall safe "dynamic" unwrapVkDestroyDeviceSafe ::
               PFN_vkDestroyDevice -> HS_vkDestroyDevice

instance VulkanProc "vkDestroyDevice" where
        type VkProcType "vkDestroyDevice" = HS_vkDestroyDevice
        vkProcSymbol = _VkDestroyDevice

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyDevice

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyDeviceSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_LAYER_NOT_PRESENT'.
--
-- > VkResult vkEnumerateInstanceExtensionProperties
-- >     ( const char* pLayerName
-- >     , uint32_t* pPropertyCount
-- >     , VkExtensionProperties* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateInstanceExtensionProperties vkEnumerateInstanceExtensionProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumerateInstanceExtensionProperties <- vkGetInstanceProc @VkEnumerateInstanceExtensionProperties VK_NULL
--
-- or less efficient:
--
-- > myEnumerateInstanceExtensionProperties <- vkGetProc @VkEnumerateInstanceExtensionProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe
               "vkEnumerateInstanceExtensionProperties"
               vkEnumerateInstanceExtensionProperties ::
               CString -- ^ pLayerName
                       -> Ptr Word32 -- ^ pPropertyCount
                                     -> Ptr VkExtensionProperties -- ^ pProperties
                                                                  -> IO VkResult

##else
vkEnumerateInstanceExtensionProperties ::
                                       CString -- ^ pLayerName
                                               ->
                                         Ptr Word32 -- ^ pPropertyCount
                                                    -> Ptr VkExtensionProperties -- ^ pProperties
                                                                                 -> IO VkResult
vkEnumerateInstanceExtensionProperties
  = unsafeDupablePerformIO
      (vkGetProc @VkEnumerateInstanceExtensionProperties)

{-# NOINLINE vkEnumerateInstanceExtensionProperties #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_LAYER_NOT_PRESENT'.
--
-- > VkResult vkEnumerateInstanceExtensionProperties
-- >     ( const char* pLayerName
-- >     , uint32_t* pPropertyCount
-- >     , VkExtensionProperties* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateInstanceExtensionProperties vkEnumerateInstanceExtensionProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumerateInstanceExtensionProperties <- vkGetInstanceProc @VkEnumerateInstanceExtensionProperties VK_NULL
--
-- or less efficient:
--
-- > myEnumerateInstanceExtensionProperties <- vkGetProc @VkEnumerateInstanceExtensionProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkEnumerateInstanceExtensionProperties"
               vkEnumerateInstanceExtensionPropertiesSafe ::
               CString -- ^ pLayerName
                       -> Ptr Word32 -- ^ pPropertyCount
                                     -> Ptr VkExtensionProperties -- ^ pProperties
                                                                  -> IO VkResult

##else
vkEnumerateInstanceExtensionPropertiesSafe ::
                                           CString -- ^ pLayerName
                                                   ->
                                             Ptr Word32 -- ^ pPropertyCount
                                                        -> Ptr VkExtensionProperties -- ^ pProperties
                                                                                     -> IO VkResult
vkEnumerateInstanceExtensionPropertiesSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkEnumerateInstanceExtensionProperties)

{-# NOINLINE vkEnumerateInstanceExtensionPropertiesSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateInstanceExtensionProperties vkEnumerateInstanceExtensionProperties registry at www.khronos.org>
type HS_vkEnumerateInstanceExtensionProperties =
     CString -- ^ pLayerName
             -> Ptr Word32 -- ^ pPropertyCount
                           -> Ptr VkExtensionProperties -- ^ pProperties
                                                        -> IO VkResult

type PFN_vkEnumerateInstanceExtensionProperties =
     FunPtr HS_vkEnumerateInstanceExtensionProperties

foreign import ccall unsafe "dynamic"
               unwrapVkEnumerateInstanceExtensionProperties ::
               PFN_vkEnumerateInstanceExtensionProperties ->
                 HS_vkEnumerateInstanceExtensionProperties

foreign import ccall safe "dynamic"
               unwrapVkEnumerateInstanceExtensionPropertiesSafe ::
               PFN_vkEnumerateInstanceExtensionProperties ->
                 HS_vkEnumerateInstanceExtensionProperties

instance VulkanProc "vkEnumerateInstanceExtensionProperties" where
        type VkProcType "vkEnumerateInstanceExtensionProperties" =
             HS_vkEnumerateInstanceExtensionProperties
        vkProcSymbol = _VkEnumerateInstanceExtensionProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkEnumerateInstanceExtensionProperties

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe
          = unwrapVkEnumerateInstanceExtensionPropertiesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_LAYER_NOT_PRESENT'.
--
-- > VkResult vkEnumerateDeviceExtensionProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const char* pLayerName
-- >     , uint32_t* pPropertyCount
-- >     , VkExtensionProperties* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateDeviceExtensionProperties vkEnumerateDeviceExtensionProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumerateDeviceExtensionProperties <- vkGetInstanceProc @VkEnumerateDeviceExtensionProperties vkInstance
--
-- or less efficient:
--
-- > myEnumerateDeviceExtensionProperties <- vkGetProc @VkEnumerateDeviceExtensionProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkEnumerateDeviceExtensionProperties"
               vkEnumerateDeviceExtensionProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 CString -- ^ pLayerName
                         -> Ptr Word32 -- ^ pPropertyCount
                                       -> Ptr VkExtensionProperties -- ^ pProperties
                                                                    -> IO VkResult

##else
vkEnumerateDeviceExtensionProperties ::
                                     VkPhysicalDevice -- ^ physicalDevice
                                                      ->
                                       CString -- ^ pLayerName
                                               ->
                                         Ptr Word32 -- ^ pPropertyCount
                                                    -> Ptr VkExtensionProperties -- ^ pProperties
                                                                                 -> IO VkResult
vkEnumerateDeviceExtensionProperties
  = unsafeDupablePerformIO
      (vkGetProc @VkEnumerateDeviceExtensionProperties)

{-# NOINLINE vkEnumerateDeviceExtensionProperties #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_LAYER_NOT_PRESENT'.
--
-- > VkResult vkEnumerateDeviceExtensionProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const char* pLayerName
-- >     , uint32_t* pPropertyCount
-- >     , VkExtensionProperties* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateDeviceExtensionProperties vkEnumerateDeviceExtensionProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumerateDeviceExtensionProperties <- vkGetInstanceProc @VkEnumerateDeviceExtensionProperties vkInstance
--
-- or less efficient:
--
-- > myEnumerateDeviceExtensionProperties <- vkGetProc @VkEnumerateDeviceExtensionProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkEnumerateDeviceExtensionProperties"
               vkEnumerateDeviceExtensionPropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 CString -- ^ pLayerName
                         -> Ptr Word32 -- ^ pPropertyCount
                                       -> Ptr VkExtensionProperties -- ^ pProperties
                                                                    -> IO VkResult

##else
vkEnumerateDeviceExtensionPropertiesSafe ::
                                         VkPhysicalDevice -- ^ physicalDevice
                                                          ->
                                           CString -- ^ pLayerName
                                                   ->
                                             Ptr Word32 -- ^ pPropertyCount
                                                        -> Ptr VkExtensionProperties -- ^ pProperties
                                                                                     -> IO VkResult
vkEnumerateDeviceExtensionPropertiesSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkEnumerateDeviceExtensionProperties)

{-# NOINLINE vkEnumerateDeviceExtensionPropertiesSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateDeviceExtensionProperties vkEnumerateDeviceExtensionProperties registry at www.khronos.org>
type HS_vkEnumerateDeviceExtensionProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       CString -- ^ pLayerName
               -> Ptr Word32 -- ^ pPropertyCount
                             -> Ptr VkExtensionProperties -- ^ pProperties
                                                          -> IO VkResult

type PFN_vkEnumerateDeviceExtensionProperties =
     FunPtr HS_vkEnumerateDeviceExtensionProperties

foreign import ccall unsafe "dynamic"
               unwrapVkEnumerateDeviceExtensionProperties ::
               PFN_vkEnumerateDeviceExtensionProperties ->
                 HS_vkEnumerateDeviceExtensionProperties

foreign import ccall safe "dynamic"
               unwrapVkEnumerateDeviceExtensionPropertiesSafe ::
               PFN_vkEnumerateDeviceExtensionProperties ->
                 HS_vkEnumerateDeviceExtensionProperties

instance VulkanProc "vkEnumerateDeviceExtensionProperties" where
        type VkProcType "vkEnumerateDeviceExtensionProperties" =
             HS_vkEnumerateDeviceExtensionProperties
        vkProcSymbol = _VkEnumerateDeviceExtensionProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkEnumerateDeviceExtensionProperties

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe
          = unwrapVkEnumerateDeviceExtensionPropertiesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkEnumerateInstanceLayerProperties
-- >     ( uint32_t* pPropertyCount
-- >     , VkLayerProperties* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateInstanceLayerProperties vkEnumerateInstanceLayerProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumerateInstanceLayerProperties <- vkGetInstanceProc @VkEnumerateInstanceLayerProperties VK_NULL
--
-- or less efficient:
--
-- > myEnumerateInstanceLayerProperties <- vkGetProc @VkEnumerateInstanceLayerProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkEnumerateInstanceLayerProperties"
               vkEnumerateInstanceLayerProperties ::
               Ptr Word32 -- ^ pPropertyCount
                          -> Ptr VkLayerProperties -- ^ pProperties
                                                   -> IO VkResult

##else
vkEnumerateInstanceLayerProperties ::
                                   Ptr Word32 -- ^ pPropertyCount
                                              -> Ptr VkLayerProperties -- ^ pProperties
                                                                       -> IO VkResult
vkEnumerateInstanceLayerProperties
  = unsafeDupablePerformIO
      (vkGetProc @VkEnumerateInstanceLayerProperties)

{-# NOINLINE vkEnumerateInstanceLayerProperties #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkEnumerateInstanceLayerProperties
-- >     ( uint32_t* pPropertyCount
-- >     , VkLayerProperties* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateInstanceLayerProperties vkEnumerateInstanceLayerProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumerateInstanceLayerProperties <- vkGetInstanceProc @VkEnumerateInstanceLayerProperties VK_NULL
--
-- or less efficient:
--
-- > myEnumerateInstanceLayerProperties <- vkGetProc @VkEnumerateInstanceLayerProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkEnumerateInstanceLayerProperties"
               vkEnumerateInstanceLayerPropertiesSafe ::
               Ptr Word32 -- ^ pPropertyCount
                          -> Ptr VkLayerProperties -- ^ pProperties
                                                   -> IO VkResult

##else
vkEnumerateInstanceLayerPropertiesSafe ::
                                       Ptr Word32 -- ^ pPropertyCount
                                                  -> Ptr VkLayerProperties -- ^ pProperties
                                                                           -> IO VkResult
vkEnumerateInstanceLayerPropertiesSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkEnumerateInstanceLayerProperties)

{-# NOINLINE vkEnumerateInstanceLayerPropertiesSafe #-}
##endif

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkEnumerateInstanceLayerProperties
--   >     ( uint32_t* pPropertyCount
--   >     , VkLayerProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateInstanceLayerProperties vkEnumerateInstanceLayerProperties registry at www.khronos.org>
type HS_vkEnumerateInstanceLayerProperties =
     Ptr Word32 -- ^ pPropertyCount
                -> Ptr VkLayerProperties -- ^ pProperties
                                         -> IO VkResult

type PFN_vkEnumerateInstanceLayerProperties =
     FunPtr HS_vkEnumerateInstanceLayerProperties

foreign import ccall unsafe "dynamic"
               unwrapVkEnumerateInstanceLayerProperties ::
               PFN_vkEnumerateInstanceLayerProperties ->
                 HS_vkEnumerateInstanceLayerProperties

foreign import ccall safe "dynamic"
               unwrapVkEnumerateInstanceLayerPropertiesSafe ::
               PFN_vkEnumerateInstanceLayerProperties ->
                 HS_vkEnumerateInstanceLayerProperties

instance VulkanProc "vkEnumerateInstanceLayerProperties" where
        type VkProcType "vkEnumerateInstanceLayerProperties" =
             HS_vkEnumerateInstanceLayerProperties
        vkProcSymbol = _VkEnumerateInstanceLayerProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkEnumerateInstanceLayerProperties

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkEnumerateInstanceLayerPropertiesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkEnumerateDeviceLayerProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t* pPropertyCount
-- >     , VkLayerProperties* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateDeviceLayerProperties vkEnumerateDeviceLayerProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumerateDeviceLayerProperties <- vkGetInstanceProc @VkEnumerateDeviceLayerProperties vkInstance
--
-- or less efficient:
--
-- > myEnumerateDeviceLayerProperties <- vkGetProc @VkEnumerateDeviceLayerProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkEnumerateDeviceLayerProperties"
               vkEnumerateDeviceLayerProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pPropertyCount
                            -> Ptr VkLayerProperties -- ^ pProperties
                                                     -> IO VkResult

##else
vkEnumerateDeviceLayerProperties ::
                                 VkPhysicalDevice -- ^ physicalDevice
                                                  ->
                                   Ptr Word32 -- ^ pPropertyCount
                                              -> Ptr VkLayerProperties -- ^ pProperties
                                                                       -> IO VkResult
vkEnumerateDeviceLayerProperties
  = unsafeDupablePerformIO
      (vkGetProc @VkEnumerateDeviceLayerProperties)

{-# NOINLINE vkEnumerateDeviceLayerProperties #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkEnumerateDeviceLayerProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t* pPropertyCount
-- >     , VkLayerProperties* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateDeviceLayerProperties vkEnumerateDeviceLayerProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumerateDeviceLayerProperties <- vkGetInstanceProc @VkEnumerateDeviceLayerProperties vkInstance
--
-- or less efficient:
--
-- > myEnumerateDeviceLayerProperties <- vkGetProc @VkEnumerateDeviceLayerProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkEnumerateDeviceLayerProperties"
               vkEnumerateDeviceLayerPropertiesSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pPropertyCount
                            -> Ptr VkLayerProperties -- ^ pProperties
                                                     -> IO VkResult

##else
vkEnumerateDeviceLayerPropertiesSafe ::
                                     VkPhysicalDevice -- ^ physicalDevice
                                                      ->
                                       Ptr Word32 -- ^ pPropertyCount
                                                  -> Ptr VkLayerProperties -- ^ pProperties
                                                                           -> IO VkResult
vkEnumerateDeviceLayerPropertiesSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkEnumerateDeviceLayerProperties)

{-# NOINLINE vkEnumerateDeviceLayerPropertiesSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateDeviceLayerProperties vkEnumerateDeviceLayerProperties registry at www.khronos.org>
type HS_vkEnumerateDeviceLayerProperties =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr Word32 -- ^ pPropertyCount
                  -> Ptr VkLayerProperties -- ^ pProperties
                                           -> IO VkResult

type PFN_vkEnumerateDeviceLayerProperties =
     FunPtr HS_vkEnumerateDeviceLayerProperties

foreign import ccall unsafe "dynamic"
               unwrapVkEnumerateDeviceLayerProperties ::
               PFN_vkEnumerateDeviceLayerProperties ->
                 HS_vkEnumerateDeviceLayerProperties

foreign import ccall safe "dynamic"
               unwrapVkEnumerateDeviceLayerPropertiesSafe ::
               PFN_vkEnumerateDeviceLayerProperties ->
                 HS_vkEnumerateDeviceLayerProperties

instance VulkanProc "vkEnumerateDeviceLayerProperties" where
        type VkProcType "vkEnumerateDeviceLayerProperties" =
             HS_vkEnumerateDeviceLayerProperties
        vkProcSymbol = _VkEnumerateDeviceLayerProperties

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkEnumerateDeviceLayerProperties

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkEnumerateDeviceLayerPropertiesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkGetDeviceQueue
-- >     ( VkDevice device
-- >     , uint32_t queueFamilyIndex
-- >     , uint32_t queueIndex
-- >     , VkQueue* pQueue
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceQueue vkGetDeviceQueue registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceQueue <- vkGetDeviceProc @VkGetDeviceQueue vkDevice
--
-- or less efficient:
--
-- > myGetDeviceQueue <- vkGetProc @VkGetDeviceQueue
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetDeviceQueue" vkGetDeviceQueue ::
               VkDevice -- ^ device
                        -> Word32 -- ^ queueFamilyIndex
                                  -> Word32 -- ^ queueIndex
                                            -> Ptr VkQueue -- ^ pQueue
                                                           -> IO ()

##else
vkGetDeviceQueue ::
                 VkDevice -- ^ device
                          -> Word32 -- ^ queueFamilyIndex
                                    -> Word32 -- ^ queueIndex
                                              -> Ptr VkQueue -- ^ pQueue
                                                             -> IO ()
vkGetDeviceQueue
  = unsafeDupablePerformIO (vkGetProc @VkGetDeviceQueue)

{-# NOINLINE vkGetDeviceQueue #-}
##endif

-- |
-- > void vkGetDeviceQueue
-- >     ( VkDevice device
-- >     , uint32_t queueFamilyIndex
-- >     , uint32_t queueIndex
-- >     , VkQueue* pQueue
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceQueue vkGetDeviceQueue registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceQueue <- vkGetDeviceProc @VkGetDeviceQueue vkDevice
--
-- or less efficient:
--
-- > myGetDeviceQueue <- vkGetProc @VkGetDeviceQueue
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetDeviceQueue" vkGetDeviceQueueSafe
               :: VkDevice -- ^ device
                           -> Word32 -- ^ queueFamilyIndex
                                     -> Word32 -- ^ queueIndex
                                               -> Ptr VkQueue -- ^ pQueue
                                                              -> IO ()

##else
vkGetDeviceQueueSafe ::
                     VkDevice -- ^ device
                              -> Word32 -- ^ queueFamilyIndex
                                        -> Word32 -- ^ queueIndex
                                                  -> Ptr VkQueue -- ^ pQueue
                                                                 -> IO ()
vkGetDeviceQueueSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkGetDeviceQueue)

{-# NOINLINE vkGetDeviceQueueSafe #-}
##endif

-- | > void vkGetDeviceQueue
--   >     ( VkDevice device
--   >     , uint32_t queueFamilyIndex
--   >     , uint32_t queueIndex
--   >     , VkQueue* pQueue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceQueue vkGetDeviceQueue registry at www.khronos.org>
type HS_vkGetDeviceQueue =
     VkDevice -- ^ device
              -> Word32 -- ^ queueFamilyIndex
                        -> Word32 -- ^ queueIndex
                                  -> Ptr VkQueue -- ^ pQueue
                                                 -> IO ()

type PFN_vkGetDeviceQueue = FunPtr HS_vkGetDeviceQueue

foreign import ccall unsafe "dynamic" unwrapVkGetDeviceQueue ::
               PFN_vkGetDeviceQueue -> HS_vkGetDeviceQueue

foreign import ccall safe "dynamic" unwrapVkGetDeviceQueueSafe ::
               PFN_vkGetDeviceQueue -> HS_vkGetDeviceQueue

instance VulkanProc "vkGetDeviceQueue" where
        type VkProcType "vkGetDeviceQueue" = HS_vkGetDeviceQueue
        vkProcSymbol = _VkGetDeviceQueue

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetDeviceQueue

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetDeviceQueueSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkQueueSubmit
-- >     ( VkQueue queue
-- >     , uint32_t submitCount
-- >     , const VkSubmitInfo* pSubmits
-- >     , VkFence fence
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkQueueSubmit vkQueueSubmit registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myQueueSubmit <- vkGetInstanceProc @VkQueueSubmit vkInstance
--
-- or less efficient:
--
-- > myQueueSubmit <- vkGetProc @VkQueueSubmit
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkQueueSubmit" vkQueueSubmit ::
               VkQueue -- ^ queue
                       -> Word32 -- ^ submitCount
                                 -> Ptr VkSubmitInfo -- ^ pSubmits
                                                     -> VkFence -- ^ fence
                                                                -> IO VkResult

##else
vkQueueSubmit ::
              VkQueue -- ^ queue
                      -> Word32 -- ^ submitCount
                                -> Ptr VkSubmitInfo -- ^ pSubmits
                                                    -> VkFence -- ^ fence
                                                               -> IO VkResult
vkQueueSubmit = unsafeDupablePerformIO (vkGetProc @VkQueueSubmit)

{-# NOINLINE vkQueueSubmit #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkQueueSubmit
-- >     ( VkQueue queue
-- >     , uint32_t submitCount
-- >     , const VkSubmitInfo* pSubmits
-- >     , VkFence fence
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkQueueSubmit vkQueueSubmit registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myQueueSubmit <- vkGetInstanceProc @VkQueueSubmit vkInstance
--
-- or less efficient:
--
-- > myQueueSubmit <- vkGetProc @VkQueueSubmit
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkQueueSubmit" vkQueueSubmitSafe ::
               VkQueue -- ^ queue
                       -> Word32 -- ^ submitCount
                                 -> Ptr VkSubmitInfo -- ^ pSubmits
                                                     -> VkFence -- ^ fence
                                                                -> IO VkResult

##else
vkQueueSubmitSafe ::
                  VkQueue -- ^ queue
                          -> Word32 -- ^ submitCount
                                    -> Ptr VkSubmitInfo -- ^ pSubmits
                                                        -> VkFence -- ^ fence
                                                                   -> IO VkResult
vkQueueSubmitSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkQueueSubmit)

{-# NOINLINE vkQueueSubmitSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkQueueSubmit vkQueueSubmit registry at www.khronos.org>
type HS_vkQueueSubmit =
     VkQueue -- ^ queue
             -> Word32 -- ^ submitCount
                       -> Ptr VkSubmitInfo -- ^ pSubmits
                                           -> VkFence -- ^ fence
                                                      -> IO VkResult

type PFN_vkQueueSubmit = FunPtr HS_vkQueueSubmit

foreign import ccall unsafe "dynamic" unwrapVkQueueSubmit ::
               PFN_vkQueueSubmit -> HS_vkQueueSubmit

foreign import ccall safe "dynamic" unwrapVkQueueSubmitSafe ::
               PFN_vkQueueSubmit -> HS_vkQueueSubmit

instance VulkanProc "vkQueueSubmit" where
        type VkProcType "vkQueueSubmit" = HS_vkQueueSubmit
        vkProcSymbol = _VkQueueSubmit

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkQueueSubmit

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkQueueSubmitSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkQueueWaitIdle
-- >     ( VkQueue queue
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkQueueWaitIdle vkQueueWaitIdle registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myQueueWaitIdle <- vkGetInstanceProc @VkQueueWaitIdle vkInstance
--
-- or less efficient:
--
-- > myQueueWaitIdle <- vkGetProc @VkQueueWaitIdle
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkQueueWaitIdle" vkQueueWaitIdle ::
               VkQueue -- ^ queue
                       -> IO VkResult

##else
vkQueueWaitIdle :: VkQueue -- ^ queue
                           -> IO VkResult
vkQueueWaitIdle
  = unsafeDupablePerformIO (vkGetProc @VkQueueWaitIdle)

{-# NOINLINE vkQueueWaitIdle #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkQueueWaitIdle
-- >     ( VkQueue queue
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkQueueWaitIdle vkQueueWaitIdle registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myQueueWaitIdle <- vkGetInstanceProc @VkQueueWaitIdle vkInstance
--
-- or less efficient:
--
-- > myQueueWaitIdle <- vkGetProc @VkQueueWaitIdle
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkQueueWaitIdle" vkQueueWaitIdleSafe ::
               VkQueue -- ^ queue
                       -> IO VkResult

##else
vkQueueWaitIdleSafe :: VkQueue -- ^ queue
                               -> IO VkResult
vkQueueWaitIdleSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkQueueWaitIdle)

{-# NOINLINE vkQueueWaitIdleSafe #-}
##endif

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkQueueWaitIdle
--   >     ( VkQueue queue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkQueueWaitIdle vkQueueWaitIdle registry at www.khronos.org>
type HS_vkQueueWaitIdle = VkQueue -- ^ queue
                                  -> IO VkResult

type PFN_vkQueueWaitIdle = FunPtr HS_vkQueueWaitIdle

foreign import ccall unsafe "dynamic" unwrapVkQueueWaitIdle ::
               PFN_vkQueueWaitIdle -> HS_vkQueueWaitIdle

foreign import ccall safe "dynamic" unwrapVkQueueWaitIdleSafe ::
               PFN_vkQueueWaitIdle -> HS_vkQueueWaitIdle

instance VulkanProc "vkQueueWaitIdle" where
        type VkProcType "vkQueueWaitIdle" = HS_vkQueueWaitIdle
        vkProcSymbol = _VkQueueWaitIdle

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkQueueWaitIdle

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkQueueWaitIdleSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkDeviceWaitIdle
-- >     ( VkDevice device
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDeviceWaitIdle vkDeviceWaitIdle registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDeviceWaitIdle <- vkGetDeviceProc @VkDeviceWaitIdle vkDevice
--
-- or less efficient:
--
-- > myDeviceWaitIdle <- vkGetProc @VkDeviceWaitIdle
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDeviceWaitIdle" vkDeviceWaitIdle ::
               VkDevice -- ^ device
                        -> IO VkResult

##else
vkDeviceWaitIdle :: VkDevice -- ^ device
                             -> IO VkResult
vkDeviceWaitIdle
  = unsafeDupablePerformIO (vkGetProc @VkDeviceWaitIdle)

{-# NOINLINE vkDeviceWaitIdle #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkDeviceWaitIdle
-- >     ( VkDevice device
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDeviceWaitIdle vkDeviceWaitIdle registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDeviceWaitIdle <- vkGetDeviceProc @VkDeviceWaitIdle vkDevice
--
-- or less efficient:
--
-- > myDeviceWaitIdle <- vkGetProc @VkDeviceWaitIdle
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDeviceWaitIdle" vkDeviceWaitIdleSafe
               :: VkDevice -- ^ device
                           -> IO VkResult

##else
vkDeviceWaitIdleSafe :: VkDevice -- ^ device
                                 -> IO VkResult
vkDeviceWaitIdleSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDeviceWaitIdle)

{-# NOINLINE vkDeviceWaitIdleSafe #-}
##endif

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkDeviceWaitIdle
--   >     ( VkDevice device
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDeviceWaitIdle vkDeviceWaitIdle registry at www.khronos.org>
type HS_vkDeviceWaitIdle = VkDevice -- ^ device
                                    -> IO VkResult

type PFN_vkDeviceWaitIdle = FunPtr HS_vkDeviceWaitIdle

foreign import ccall unsafe "dynamic" unwrapVkDeviceWaitIdle ::
               PFN_vkDeviceWaitIdle -> HS_vkDeviceWaitIdle

foreign import ccall safe "dynamic" unwrapVkDeviceWaitIdleSafe ::
               PFN_vkDeviceWaitIdle -> HS_vkDeviceWaitIdle

instance VulkanProc "vkDeviceWaitIdle" where
        type VkProcType "vkDeviceWaitIdle" = HS_vkDeviceWaitIdle
        vkProcSymbol = _VkDeviceWaitIdle

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDeviceWaitIdle

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDeviceWaitIdleSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_INVALID_EXTERNAL_HANDLE'.
--
-- > VkResult vkAllocateMemory
-- >     ( VkDevice device
-- >     , const VkMemoryAllocateInfo* pAllocateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkDeviceMemory* pMemory
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAllocateMemory vkAllocateMemory registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myAllocateMemory <- vkGetDeviceProc @VkAllocateMemory vkDevice
--
-- or less efficient:
--
-- > myAllocateMemory <- vkGetProc @VkAllocateMemory
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkAllocateMemory" vkAllocateMemory ::
               VkDevice -- ^ device
                        ->
                 Ptr VkMemoryAllocateInfo -- ^ pAllocateInfo
                                          ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkDeviceMemory -- ^ pMemory
                                                                   -> IO VkResult

##else
vkAllocateMemory ::
                 VkDevice -- ^ device
                          ->
                   Ptr VkMemoryAllocateInfo -- ^ pAllocateInfo
                                            ->
                     Ptr VkAllocationCallbacks -- ^ pAllocator
                                               -> Ptr VkDeviceMemory -- ^ pMemory
                                                                     -> IO VkResult
vkAllocateMemory
  = unsafeDupablePerformIO (vkGetProc @VkAllocateMemory)

{-# NOINLINE vkAllocateMemory #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_INVALID_EXTERNAL_HANDLE'.
--
-- > VkResult vkAllocateMemory
-- >     ( VkDevice device
-- >     , const VkMemoryAllocateInfo* pAllocateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkDeviceMemory* pMemory
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAllocateMemory vkAllocateMemory registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myAllocateMemory <- vkGetDeviceProc @VkAllocateMemory vkDevice
--
-- or less efficient:
--
-- > myAllocateMemory <- vkGetProc @VkAllocateMemory
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkAllocateMemory" vkAllocateMemorySafe
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkMemoryAllocateInfo -- ^ pAllocateInfo
                                          ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkDeviceMemory -- ^ pMemory
                                                                   -> IO VkResult

##else
vkAllocateMemorySafe ::
                     VkDevice -- ^ device
                              ->
                       Ptr VkMemoryAllocateInfo -- ^ pAllocateInfo
                                                ->
                         Ptr VkAllocationCallbacks -- ^ pAllocator
                                                   -> Ptr VkDeviceMemory -- ^ pMemory
                                                                         -> IO VkResult
vkAllocateMemorySafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkAllocateMemory)

{-# NOINLINE vkAllocateMemorySafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAllocateMemory vkAllocateMemory registry at www.khronos.org>
type HS_vkAllocateMemory =
     VkDevice -- ^ device
              ->
       Ptr VkMemoryAllocateInfo -- ^ pAllocateInfo
                                ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkDeviceMemory -- ^ pMemory
                                                         -> IO VkResult

type PFN_vkAllocateMemory = FunPtr HS_vkAllocateMemory

foreign import ccall unsafe "dynamic" unwrapVkAllocateMemory ::
               PFN_vkAllocateMemory -> HS_vkAllocateMemory

foreign import ccall safe "dynamic" unwrapVkAllocateMemorySafe ::
               PFN_vkAllocateMemory -> HS_vkAllocateMemory

instance VulkanProc "vkAllocateMemory" where
        type VkProcType "vkAllocateMemory" = HS_vkAllocateMemory
        vkProcSymbol = _VkAllocateMemory

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkAllocateMemory

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkAllocateMemorySafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkFreeMemory
-- >     ( VkDevice device
-- >     , VkDeviceMemory memory
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkFreeMemory vkFreeMemory registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myFreeMemory <- vkGetDeviceProc @VkFreeMemory vkDevice
--
-- or less efficient:
--
-- > myFreeMemory <- vkGetProc @VkFreeMemory
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkFreeMemory" vkFreeMemory ::
               VkDevice -- ^ device
                        -> VkDeviceMemory -- ^ memory
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

##else
vkFreeMemory ::
             VkDevice -- ^ device
                      -> VkDeviceMemory -- ^ memory
                                        -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                     -> IO ()
vkFreeMemory = unsafeDupablePerformIO (vkGetProc @VkFreeMemory)

{-# NOINLINE vkFreeMemory #-}
##endif

-- |
-- > void vkFreeMemory
-- >     ( VkDevice device
-- >     , VkDeviceMemory memory
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkFreeMemory vkFreeMemory registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myFreeMemory <- vkGetDeviceProc @VkFreeMemory vkDevice
--
-- or less efficient:
--
-- > myFreeMemory <- vkGetProc @VkFreeMemory
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkFreeMemory" vkFreeMemorySafe ::
               VkDevice -- ^ device
                        -> VkDeviceMemory -- ^ memory
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

##else
vkFreeMemorySafe ::
                 VkDevice -- ^ device
                          -> VkDeviceMemory -- ^ memory
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()
vkFreeMemorySafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkFreeMemory)

{-# NOINLINE vkFreeMemorySafe #-}
##endif

-- | > void vkFreeMemory
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkFreeMemory vkFreeMemory registry at www.khronos.org>
type HS_vkFreeMemory =
     VkDevice -- ^ device
              -> VkDeviceMemory -- ^ memory
                                -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                             -> IO ()

type PFN_vkFreeMemory = FunPtr HS_vkFreeMemory

foreign import ccall unsafe "dynamic" unwrapVkFreeMemory ::
               PFN_vkFreeMemory -> HS_vkFreeMemory

foreign import ccall safe "dynamic" unwrapVkFreeMemorySafe ::
               PFN_vkFreeMemory -> HS_vkFreeMemory

instance VulkanProc "vkFreeMemory" where
        type VkProcType "vkFreeMemory" = HS_vkFreeMemory
        vkProcSymbol = _VkFreeMemory

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkFreeMemory

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkFreeMemorySafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_MEMORY_MAP_FAILED'.
--
-- > VkResult vkMapMemory
-- >     ( VkDevice device
-- >     , VkDeviceMemory memory
-- >     , VkDeviceSize offset
-- >     , VkDeviceSize size
-- >     , VkMemoryMapFlags flags
-- >     , void** ppData
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkMapMemory vkMapMemory registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myMapMemory <- vkGetDeviceProc @VkMapMemory vkDevice
--
-- or less efficient:
--
-- > myMapMemory <- vkGetProc @VkMapMemory
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
vkMapMemory ::
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
vkMapMemory = unsafeDupablePerformIO (vkGetProc @VkMapMemory)

{-# NOINLINE vkMapMemory #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_MEMORY_MAP_FAILED'.
--
-- > VkResult vkMapMemory
-- >     ( VkDevice device
-- >     , VkDeviceMemory memory
-- >     , VkDeviceSize offset
-- >     , VkDeviceSize size
-- >     , VkMemoryMapFlags flags
-- >     , void** ppData
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkMapMemory vkMapMemory registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myMapMemory <- vkGetDeviceProc @VkMapMemory vkDevice
--
-- or less efficient:
--
-- > myMapMemory <- vkGetProc @VkMapMemory
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
vkMapMemorySafe ::
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
vkMapMemorySafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkMapMemory)

{-# NOINLINE vkMapMemorySafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkMapMemory vkMapMemory registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic" unwrapVkMapMemory ::
               PFN_vkMapMemory -> HS_vkMapMemory

foreign import ccall safe "dynamic" unwrapVkMapMemorySafe ::
               PFN_vkMapMemory -> HS_vkMapMemory

instance VulkanProc "vkMapMemory" where
        type VkProcType "vkMapMemory" = HS_vkMapMemory
        vkProcSymbol = _VkMapMemory

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkMapMemory

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkMapMemorySafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkUnmapMemory
-- >     ( VkDevice device
-- >     , VkDeviceMemory memory
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkUnmapMemory vkUnmapMemory registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myUnmapMemory <- vkGetDeviceProc @VkUnmapMemory vkDevice
--
-- or less efficient:
--
-- > myUnmapMemory <- vkGetProc @VkUnmapMemory
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkUnmapMemory" vkUnmapMemory ::
               VkDevice -- ^ device
                        -> VkDeviceMemory -- ^ memory
                                          -> IO ()

##else
vkUnmapMemory :: VkDevice -- ^ device
                          -> VkDeviceMemory -- ^ memory
                                            -> IO ()
vkUnmapMemory = unsafeDupablePerformIO (vkGetProc @VkUnmapMemory)

{-# NOINLINE vkUnmapMemory #-}
##endif

-- |
-- > void vkUnmapMemory
-- >     ( VkDevice device
-- >     , VkDeviceMemory memory
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkUnmapMemory vkUnmapMemory registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myUnmapMemory <- vkGetDeviceProc @VkUnmapMemory vkDevice
--
-- or less efficient:
--
-- > myUnmapMemory <- vkGetProc @VkUnmapMemory
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkUnmapMemory" vkUnmapMemorySafe ::
               VkDevice -- ^ device
                        -> VkDeviceMemory -- ^ memory
                                          -> IO ()

##else
vkUnmapMemorySafe :: VkDevice -- ^ device
                              -> VkDeviceMemory -- ^ memory
                                                -> IO ()
vkUnmapMemorySafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkUnmapMemory)

{-# NOINLINE vkUnmapMemorySafe #-}
##endif

-- | > void vkUnmapMemory
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkUnmapMemory vkUnmapMemory registry at www.khronos.org>
type HS_vkUnmapMemory = VkDevice -- ^ device
                                 -> VkDeviceMemory -- ^ memory
                                                   -> IO ()

type PFN_vkUnmapMemory = FunPtr HS_vkUnmapMemory

foreign import ccall unsafe "dynamic" unwrapVkUnmapMemory ::
               PFN_vkUnmapMemory -> HS_vkUnmapMemory

foreign import ccall safe "dynamic" unwrapVkUnmapMemorySafe ::
               PFN_vkUnmapMemory -> HS_vkUnmapMemory

instance VulkanProc "vkUnmapMemory" where
        type VkProcType "vkUnmapMemory" = HS_vkUnmapMemory
        vkProcSymbol = _VkUnmapMemory

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkUnmapMemory

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkUnmapMemorySafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkFlushMappedMemoryRanges
-- >     ( VkDevice device
-- >     , uint32_t memoryRangeCount
-- >     , const VkMappedMemoryRange* pMemoryRanges
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkFlushMappedMemoryRanges vkFlushMappedMemoryRanges registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myFlushMappedMemoryRanges <- vkGetDeviceProc @VkFlushMappedMemoryRanges vkDevice
--
-- or less efficient:
--
-- > myFlushMappedMemoryRanges <- vkGetProc @VkFlushMappedMemoryRanges
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkFlushMappedMemoryRanges"
               vkFlushMappedMemoryRanges ::
               VkDevice -- ^ device
                        -> Word32 -- ^ memoryRangeCount
                                  -> Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                             -> IO VkResult

##else
vkFlushMappedMemoryRanges ::
                          VkDevice -- ^ device
                                   -> Word32 -- ^ memoryRangeCount
                                             -> Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                                        -> IO VkResult
vkFlushMappedMemoryRanges
  = unsafeDupablePerformIO (vkGetProc @VkFlushMappedMemoryRanges)

{-# NOINLINE vkFlushMappedMemoryRanges #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkFlushMappedMemoryRanges
-- >     ( VkDevice device
-- >     , uint32_t memoryRangeCount
-- >     , const VkMappedMemoryRange* pMemoryRanges
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkFlushMappedMemoryRanges vkFlushMappedMemoryRanges registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myFlushMappedMemoryRanges <- vkGetDeviceProc @VkFlushMappedMemoryRanges vkDevice
--
-- or less efficient:
--
-- > myFlushMappedMemoryRanges <- vkGetProc @VkFlushMappedMemoryRanges
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkFlushMappedMemoryRanges"
               vkFlushMappedMemoryRangesSafe ::
               VkDevice -- ^ device
                        -> Word32 -- ^ memoryRangeCount
                                  -> Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                             -> IO VkResult

##else
vkFlushMappedMemoryRangesSafe ::
                              VkDevice -- ^ device
                                       -> Word32 -- ^ memoryRangeCount
                                                 -> Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                                            -> IO VkResult
vkFlushMappedMemoryRangesSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkFlushMappedMemoryRanges)

{-# NOINLINE vkFlushMappedMemoryRangesSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkFlushMappedMemoryRanges vkFlushMappedMemoryRanges registry at www.khronos.org>
type HS_vkFlushMappedMemoryRanges =
     VkDevice -- ^ device
              -> Word32 -- ^ memoryRangeCount
                        -> Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                   -> IO VkResult

type PFN_vkFlushMappedMemoryRanges =
     FunPtr HS_vkFlushMappedMemoryRanges

foreign import ccall unsafe "dynamic"
               unwrapVkFlushMappedMemoryRanges ::
               PFN_vkFlushMappedMemoryRanges -> HS_vkFlushMappedMemoryRanges

foreign import ccall safe "dynamic"
               unwrapVkFlushMappedMemoryRangesSafe ::
               PFN_vkFlushMappedMemoryRanges -> HS_vkFlushMappedMemoryRanges

instance VulkanProc "vkFlushMappedMemoryRanges" where
        type VkProcType "vkFlushMappedMemoryRanges" =
             HS_vkFlushMappedMemoryRanges
        vkProcSymbol = _VkFlushMappedMemoryRanges

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkFlushMappedMemoryRanges

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkFlushMappedMemoryRangesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkInvalidateMappedMemoryRanges
-- >     ( VkDevice device
-- >     , uint32_t memoryRangeCount
-- >     , const VkMappedMemoryRange* pMemoryRanges
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkInvalidateMappedMemoryRanges vkInvalidateMappedMemoryRanges registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myInvalidateMappedMemoryRanges <- vkGetDeviceProc @VkInvalidateMappedMemoryRanges vkDevice
--
-- or less efficient:
--
-- > myInvalidateMappedMemoryRanges <- vkGetProc @VkInvalidateMappedMemoryRanges
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkInvalidateMappedMemoryRanges"
               vkInvalidateMappedMemoryRanges ::
               VkDevice -- ^ device
                        -> Word32 -- ^ memoryRangeCount
                                  -> Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                             -> IO VkResult

##else
vkInvalidateMappedMemoryRanges ::
                               VkDevice -- ^ device
                                        -> Word32 -- ^ memoryRangeCount
                                                  -> Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                                             -> IO VkResult
vkInvalidateMappedMemoryRanges
  = unsafeDupablePerformIO
      (vkGetProc @VkInvalidateMappedMemoryRanges)

{-# NOINLINE vkInvalidateMappedMemoryRanges #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkInvalidateMappedMemoryRanges
-- >     ( VkDevice device
-- >     , uint32_t memoryRangeCount
-- >     , const VkMappedMemoryRange* pMemoryRanges
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkInvalidateMappedMemoryRanges vkInvalidateMappedMemoryRanges registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myInvalidateMappedMemoryRanges <- vkGetDeviceProc @VkInvalidateMappedMemoryRanges vkDevice
--
-- or less efficient:
--
-- > myInvalidateMappedMemoryRanges <- vkGetProc @VkInvalidateMappedMemoryRanges
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkInvalidateMappedMemoryRanges"
               vkInvalidateMappedMemoryRangesSafe ::
               VkDevice -- ^ device
                        -> Word32 -- ^ memoryRangeCount
                                  -> Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                             -> IO VkResult

##else
vkInvalidateMappedMemoryRangesSafe ::
                                   VkDevice -- ^ device
                                            -> Word32 -- ^ memoryRangeCount
                                                      -> Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                                                 -> IO VkResult
vkInvalidateMappedMemoryRangesSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkInvalidateMappedMemoryRanges)

{-# NOINLINE vkInvalidateMappedMemoryRangesSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkInvalidateMappedMemoryRanges vkInvalidateMappedMemoryRanges registry at www.khronos.org>
type HS_vkInvalidateMappedMemoryRanges =
     VkDevice -- ^ device
              -> Word32 -- ^ memoryRangeCount
                        -> Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                   -> IO VkResult

type PFN_vkInvalidateMappedMemoryRanges =
     FunPtr HS_vkInvalidateMappedMemoryRanges

foreign import ccall unsafe "dynamic"
               unwrapVkInvalidateMappedMemoryRanges ::
               PFN_vkInvalidateMappedMemoryRanges ->
                 HS_vkInvalidateMappedMemoryRanges

foreign import ccall safe "dynamic"
               unwrapVkInvalidateMappedMemoryRangesSafe ::
               PFN_vkInvalidateMappedMemoryRanges ->
                 HS_vkInvalidateMappedMemoryRanges

instance VulkanProc "vkInvalidateMappedMemoryRanges" where
        type VkProcType "vkInvalidateMappedMemoryRanges" =
             HS_vkInvalidateMappedMemoryRanges
        vkProcSymbol = _VkInvalidateMappedMemoryRanges

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkInvalidateMappedMemoryRanges

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkInvalidateMappedMemoryRangesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkGetDeviceMemoryCommitment
-- >     ( VkDevice device
-- >     , VkDeviceMemory memory
-- >     , VkDeviceSize* pCommittedMemoryInBytes
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceMemoryCommitment vkGetDeviceMemoryCommitment registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceMemoryCommitment <- vkGetDeviceProc @VkGetDeviceMemoryCommitment vkDevice
--
-- or less efficient:
--
-- > myGetDeviceMemoryCommitment <- vkGetProc @VkGetDeviceMemoryCommitment
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetDeviceMemoryCommitment"
               vkGetDeviceMemoryCommitment ::
               VkDevice -- ^ device
                        -> VkDeviceMemory -- ^ memory
                                          -> Ptr VkDeviceSize -- ^ pCommittedMemoryInBytes
                                                              -> IO ()

##else
vkGetDeviceMemoryCommitment ::
                            VkDevice -- ^ device
                                     -> VkDeviceMemory -- ^ memory
                                                       -> Ptr VkDeviceSize -- ^ pCommittedMemoryInBytes
                                                                           -> IO ()
vkGetDeviceMemoryCommitment
  = unsafeDupablePerformIO (vkGetProc @VkGetDeviceMemoryCommitment)

{-# NOINLINE vkGetDeviceMemoryCommitment #-}
##endif

-- |
-- > void vkGetDeviceMemoryCommitment
-- >     ( VkDevice device
-- >     , VkDeviceMemory memory
-- >     , VkDeviceSize* pCommittedMemoryInBytes
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceMemoryCommitment vkGetDeviceMemoryCommitment registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceMemoryCommitment <- vkGetDeviceProc @VkGetDeviceMemoryCommitment vkDevice
--
-- or less efficient:
--
-- > myGetDeviceMemoryCommitment <- vkGetProc @VkGetDeviceMemoryCommitment
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetDeviceMemoryCommitment"
               vkGetDeviceMemoryCommitmentSafe ::
               VkDevice -- ^ device
                        -> VkDeviceMemory -- ^ memory
                                          -> Ptr VkDeviceSize -- ^ pCommittedMemoryInBytes
                                                              -> IO ()

##else
vkGetDeviceMemoryCommitmentSafe ::
                                VkDevice -- ^ device
                                         -> VkDeviceMemory -- ^ memory
                                                           -> Ptr VkDeviceSize -- ^ pCommittedMemoryInBytes
                                                                               -> IO ()
vkGetDeviceMemoryCommitmentSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetDeviceMemoryCommitment)

{-# NOINLINE vkGetDeviceMemoryCommitmentSafe #-}
##endif

-- | > void vkGetDeviceMemoryCommitment
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize* pCommittedMemoryInBytes
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceMemoryCommitment vkGetDeviceMemoryCommitment registry at www.khronos.org>
type HS_vkGetDeviceMemoryCommitment =
     VkDevice -- ^ device
              -> VkDeviceMemory -- ^ memory
                                -> Ptr VkDeviceSize -- ^ pCommittedMemoryInBytes
                                                    -> IO ()

type PFN_vkGetDeviceMemoryCommitment =
     FunPtr HS_vkGetDeviceMemoryCommitment

foreign import ccall unsafe "dynamic"
               unwrapVkGetDeviceMemoryCommitment ::
               PFN_vkGetDeviceMemoryCommitment -> HS_vkGetDeviceMemoryCommitment

foreign import ccall safe "dynamic"
               unwrapVkGetDeviceMemoryCommitmentSafe ::
               PFN_vkGetDeviceMemoryCommitment -> HS_vkGetDeviceMemoryCommitment

instance VulkanProc "vkGetDeviceMemoryCommitment" where
        type VkProcType "vkGetDeviceMemoryCommitment" =
             HS_vkGetDeviceMemoryCommitment
        vkProcSymbol = _VkGetDeviceMemoryCommitment

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetDeviceMemoryCommitment

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetDeviceMemoryCommitmentSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkBindBufferMemory
-- >     ( VkDevice device
-- >     , VkBuffer buffer
-- >     , VkDeviceMemory memory
-- >     , VkDeviceSize memoryOffset
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindBufferMemory vkBindBufferMemory registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myBindBufferMemory <- vkGetDeviceProc @VkBindBufferMemory vkDevice
--
-- or less efficient:
--
-- > myBindBufferMemory <- vkGetProc @VkBindBufferMemory
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkBindBufferMemory" vkBindBufferMemory
               ::
               VkDevice -- ^ device
                        ->
                 VkBuffer -- ^ buffer
                          -> VkDeviceMemory -- ^ memory
                                            -> VkDeviceSize -- ^ memoryOffset
                                                            -> IO VkResult

##else
vkBindBufferMemory ::
                   VkDevice -- ^ device
                            ->
                     VkBuffer -- ^ buffer
                              -> VkDeviceMemory -- ^ memory
                                                -> VkDeviceSize -- ^ memoryOffset
                                                                -> IO VkResult
vkBindBufferMemory
  = unsafeDupablePerformIO (vkGetProc @VkBindBufferMemory)

{-# NOINLINE vkBindBufferMemory #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkBindBufferMemory
-- >     ( VkDevice device
-- >     , VkBuffer buffer
-- >     , VkDeviceMemory memory
-- >     , VkDeviceSize memoryOffset
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindBufferMemory vkBindBufferMemory registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myBindBufferMemory <- vkGetDeviceProc @VkBindBufferMemory vkDevice
--
-- or less efficient:
--
-- > myBindBufferMemory <- vkGetProc @VkBindBufferMemory
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkBindBufferMemory"
               vkBindBufferMemorySafe ::
               VkDevice -- ^ device
                        ->
                 VkBuffer -- ^ buffer
                          -> VkDeviceMemory -- ^ memory
                                            -> VkDeviceSize -- ^ memoryOffset
                                                            -> IO VkResult

##else
vkBindBufferMemorySafe ::
                       VkDevice -- ^ device
                                ->
                         VkBuffer -- ^ buffer
                                  -> VkDeviceMemory -- ^ memory
                                                    -> VkDeviceSize -- ^ memoryOffset
                                                                    -> IO VkResult
vkBindBufferMemorySafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkBindBufferMemory)

{-# NOINLINE vkBindBufferMemorySafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindBufferMemory vkBindBufferMemory registry at www.khronos.org>
type HS_vkBindBufferMemory =
     VkDevice -- ^ device
              ->
       VkBuffer -- ^ buffer
                -> VkDeviceMemory -- ^ memory
                                  -> VkDeviceSize -- ^ memoryOffset
                                                  -> IO VkResult

type PFN_vkBindBufferMemory = FunPtr HS_vkBindBufferMemory

foreign import ccall unsafe "dynamic" unwrapVkBindBufferMemory ::
               PFN_vkBindBufferMemory -> HS_vkBindBufferMemory

foreign import ccall safe "dynamic" unwrapVkBindBufferMemorySafe ::
               PFN_vkBindBufferMemory -> HS_vkBindBufferMemory

instance VulkanProc "vkBindBufferMemory" where
        type VkProcType "vkBindBufferMemory" = HS_vkBindBufferMemory
        vkProcSymbol = _VkBindBufferMemory

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkBindBufferMemory

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkBindBufferMemorySafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkBindImageMemory
-- >     ( VkDevice device
-- >     , VkImage image
-- >     , VkDeviceMemory memory
-- >     , VkDeviceSize memoryOffset
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindImageMemory vkBindImageMemory registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myBindImageMemory <- vkGetDeviceProc @VkBindImageMemory vkDevice
--
-- or less efficient:
--
-- > myBindImageMemory <- vkGetProc @VkBindImageMemory
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkBindImageMemory" vkBindImageMemory
               ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         -> VkDeviceMemory -- ^ memory
                                           -> VkDeviceSize -- ^ memoryOffset
                                                           -> IO VkResult

##else
vkBindImageMemory ::
                  VkDevice -- ^ device
                           ->
                    VkImage -- ^ image
                            -> VkDeviceMemory -- ^ memory
                                              -> VkDeviceSize -- ^ memoryOffset
                                                              -> IO VkResult
vkBindImageMemory
  = unsafeDupablePerformIO (vkGetProc @VkBindImageMemory)

{-# NOINLINE vkBindImageMemory #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkBindImageMemory
-- >     ( VkDevice device
-- >     , VkImage image
-- >     , VkDeviceMemory memory
-- >     , VkDeviceSize memoryOffset
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindImageMemory vkBindImageMemory registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myBindImageMemory <- vkGetDeviceProc @VkBindImageMemory vkDevice
--
-- or less efficient:
--
-- > myBindImageMemory <- vkGetProc @VkBindImageMemory
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkBindImageMemory" vkBindImageMemorySafe
               ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         -> VkDeviceMemory -- ^ memory
                                           -> VkDeviceSize -- ^ memoryOffset
                                                           -> IO VkResult

##else
vkBindImageMemorySafe ::
                      VkDevice -- ^ device
                               ->
                        VkImage -- ^ image
                                -> VkDeviceMemory -- ^ memory
                                                  -> VkDeviceSize -- ^ memoryOffset
                                                                  -> IO VkResult
vkBindImageMemorySafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkBindImageMemory)

{-# NOINLINE vkBindImageMemorySafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBindImageMemory vkBindImageMemory registry at www.khronos.org>
type HS_vkBindImageMemory =
     VkDevice -- ^ device
              ->
       VkImage -- ^ image
               -> VkDeviceMemory -- ^ memory
                                 -> VkDeviceSize -- ^ memoryOffset
                                                 -> IO VkResult

type PFN_vkBindImageMemory = FunPtr HS_vkBindImageMemory

foreign import ccall unsafe "dynamic" unwrapVkBindImageMemory ::
               PFN_vkBindImageMemory -> HS_vkBindImageMemory

foreign import ccall safe "dynamic" unwrapVkBindImageMemorySafe ::
               PFN_vkBindImageMemory -> HS_vkBindImageMemory

instance VulkanProc "vkBindImageMemory" where
        type VkProcType "vkBindImageMemory" = HS_vkBindImageMemory
        vkProcSymbol = _VkBindImageMemory

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkBindImageMemory

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkBindImageMemorySafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkGetBufferMemoryRequirements
-- >     ( VkDevice device
-- >     , VkBuffer buffer
-- >     , VkMemoryRequirements* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetBufferMemoryRequirements vkGetBufferMemoryRequirements registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetBufferMemoryRequirements <- vkGetDeviceProc @VkGetBufferMemoryRequirements vkDevice
--
-- or less efficient:
--
-- > myGetBufferMemoryRequirements <- vkGetProc @VkGetBufferMemoryRequirements
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetBufferMemoryRequirements"
               vkGetBufferMemoryRequirements ::
               VkDevice -- ^ device
                        -> VkBuffer -- ^ buffer
                                    -> Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                                -> IO ()

##else
vkGetBufferMemoryRequirements ::
                              VkDevice -- ^ device
                                       -> VkBuffer -- ^ buffer
                                                   -> Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                                               -> IO ()
vkGetBufferMemoryRequirements
  = unsafeDupablePerformIO (vkGetProc @VkGetBufferMemoryRequirements)

{-# NOINLINE vkGetBufferMemoryRequirements #-}
##endif

-- |
-- > void vkGetBufferMemoryRequirements
-- >     ( VkDevice device
-- >     , VkBuffer buffer
-- >     , VkMemoryRequirements* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetBufferMemoryRequirements vkGetBufferMemoryRequirements registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetBufferMemoryRequirements <- vkGetDeviceProc @VkGetBufferMemoryRequirements vkDevice
--
-- or less efficient:
--
-- > myGetBufferMemoryRequirements <- vkGetProc @VkGetBufferMemoryRequirements
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetBufferMemoryRequirements"
               vkGetBufferMemoryRequirementsSafe ::
               VkDevice -- ^ device
                        -> VkBuffer -- ^ buffer
                                    -> Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                                -> IO ()

##else
vkGetBufferMemoryRequirementsSafe ::
                                  VkDevice -- ^ device
                                           -> VkBuffer -- ^ buffer
                                                       -> Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                                                   -> IO ()
vkGetBufferMemoryRequirementsSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetBufferMemoryRequirements)

{-# NOINLINE vkGetBufferMemoryRequirementsSafe #-}
##endif

-- | > void vkGetBufferMemoryRequirements
--   >     ( VkDevice device
--   >     , VkBuffer buffer
--   >     , VkMemoryRequirements* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetBufferMemoryRequirements vkGetBufferMemoryRequirements registry at www.khronos.org>
type HS_vkGetBufferMemoryRequirements =
     VkDevice -- ^ device
              -> VkBuffer -- ^ buffer
                          -> Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                      -> IO ()

type PFN_vkGetBufferMemoryRequirements =
     FunPtr HS_vkGetBufferMemoryRequirements

foreign import ccall unsafe "dynamic"
               unwrapVkGetBufferMemoryRequirements ::
               PFN_vkGetBufferMemoryRequirements ->
                 HS_vkGetBufferMemoryRequirements

foreign import ccall safe "dynamic"
               unwrapVkGetBufferMemoryRequirementsSafe ::
               PFN_vkGetBufferMemoryRequirements ->
                 HS_vkGetBufferMemoryRequirements

instance VulkanProc "vkGetBufferMemoryRequirements" where
        type VkProcType "vkGetBufferMemoryRequirements" =
             HS_vkGetBufferMemoryRequirements
        vkProcSymbol = _VkGetBufferMemoryRequirements

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetBufferMemoryRequirements

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetBufferMemoryRequirementsSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkGetImageMemoryRequirements
-- >     ( VkDevice device
-- >     , VkImage image
-- >     , VkMemoryRequirements* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageMemoryRequirements vkGetImageMemoryRequirements registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetImageMemoryRequirements <- vkGetDeviceProc @VkGetImageMemoryRequirements vkDevice
--
-- or less efficient:
--
-- > myGetImageMemoryRequirements <- vkGetProc @VkGetImageMemoryRequirements
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetImageMemoryRequirements"
               vkGetImageMemoryRequirements ::
               VkDevice -- ^ device
                        -> VkImage -- ^ image
                                   -> Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                               -> IO ()

##else
vkGetImageMemoryRequirements ::
                             VkDevice -- ^ device
                                      -> VkImage -- ^ image
                                                 -> Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                                             -> IO ()
vkGetImageMemoryRequirements
  = unsafeDupablePerformIO (vkGetProc @VkGetImageMemoryRequirements)

{-# NOINLINE vkGetImageMemoryRequirements #-}
##endif

-- |
-- > void vkGetImageMemoryRequirements
-- >     ( VkDevice device
-- >     , VkImage image
-- >     , VkMemoryRequirements* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageMemoryRequirements vkGetImageMemoryRequirements registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetImageMemoryRequirements <- vkGetDeviceProc @VkGetImageMemoryRequirements vkDevice
--
-- or less efficient:
--
-- > myGetImageMemoryRequirements <- vkGetProc @VkGetImageMemoryRequirements
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetImageMemoryRequirements"
               vkGetImageMemoryRequirementsSafe ::
               VkDevice -- ^ device
                        -> VkImage -- ^ image
                                   -> Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                               -> IO ()

##else
vkGetImageMemoryRequirementsSafe ::
                                 VkDevice -- ^ device
                                          -> VkImage -- ^ image
                                                     -> Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                                                 -> IO ()
vkGetImageMemoryRequirementsSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetImageMemoryRequirements)

{-# NOINLINE vkGetImageMemoryRequirementsSafe #-}
##endif

-- | > void vkGetImageMemoryRequirements
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , VkMemoryRequirements* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageMemoryRequirements vkGetImageMemoryRequirements registry at www.khronos.org>
type HS_vkGetImageMemoryRequirements =
     VkDevice -- ^ device
              -> VkImage -- ^ image
                         -> Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                     -> IO ()

type PFN_vkGetImageMemoryRequirements =
     FunPtr HS_vkGetImageMemoryRequirements

foreign import ccall unsafe "dynamic"
               unwrapVkGetImageMemoryRequirements ::
               PFN_vkGetImageMemoryRequirements -> HS_vkGetImageMemoryRequirements

foreign import ccall safe "dynamic"
               unwrapVkGetImageMemoryRequirementsSafe ::
               PFN_vkGetImageMemoryRequirements -> HS_vkGetImageMemoryRequirements

instance VulkanProc "vkGetImageMemoryRequirements" where
        type VkProcType "vkGetImageMemoryRequirements" =
             HS_vkGetImageMemoryRequirements
        vkProcSymbol = _VkGetImageMemoryRequirements

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetImageMemoryRequirements

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetImageMemoryRequirementsSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkGetImageSparseMemoryRequirements
-- >     ( VkDevice device
-- >     , VkImage image
-- >     , uint32_t* pSparseMemoryRequirementCount
-- >     , VkSparseImageMemoryRequirements* pSparseMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageSparseMemoryRequirements vkGetImageSparseMemoryRequirements registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetImageSparseMemoryRequirements <- vkGetDeviceProc @VkGetImageSparseMemoryRequirements vkDevice
--
-- or less efficient:
--
-- > myGetImageSparseMemoryRequirements <- vkGetProc @VkGetImageSparseMemoryRequirements
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetImageSparseMemoryRequirements"
               vkGetImageSparseMemoryRequirements ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         ->
                   Ptr Word32 -- ^ pSparseMemoryRequirementCount
                              -> Ptr VkSparseImageMemoryRequirements -- ^ pSparseMemoryRequirements
                                                                     -> IO ()

##else
vkGetImageSparseMemoryRequirements ::
                                   VkDevice -- ^ device
                                            ->
                                     VkImage -- ^ image
                                             ->
                                       Ptr Word32 -- ^ pSparseMemoryRequirementCount
                                                  -> Ptr VkSparseImageMemoryRequirements -- ^ pSparseMemoryRequirements
                                                                                         -> IO ()
vkGetImageSparseMemoryRequirements
  = unsafeDupablePerformIO
      (vkGetProc @VkGetImageSparseMemoryRequirements)

{-# NOINLINE vkGetImageSparseMemoryRequirements #-}
##endif

-- |
-- > void vkGetImageSparseMemoryRequirements
-- >     ( VkDevice device
-- >     , VkImage image
-- >     , uint32_t* pSparseMemoryRequirementCount
-- >     , VkSparseImageMemoryRequirements* pSparseMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageSparseMemoryRequirements vkGetImageSparseMemoryRequirements registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetImageSparseMemoryRequirements <- vkGetDeviceProc @VkGetImageSparseMemoryRequirements vkDevice
--
-- or less efficient:
--
-- > myGetImageSparseMemoryRequirements <- vkGetProc @VkGetImageSparseMemoryRequirements
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetImageSparseMemoryRequirements"
               vkGetImageSparseMemoryRequirementsSafe ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         ->
                   Ptr Word32 -- ^ pSparseMemoryRequirementCount
                              -> Ptr VkSparseImageMemoryRequirements -- ^ pSparseMemoryRequirements
                                                                     -> IO ()

##else
vkGetImageSparseMemoryRequirementsSafe ::
                                       VkDevice -- ^ device
                                                ->
                                         VkImage -- ^ image
                                                 ->
                                           Ptr Word32 -- ^ pSparseMemoryRequirementCount
                                                      ->
                                             Ptr VkSparseImageMemoryRequirements -- ^ pSparseMemoryRequirements
                                                                                 -> IO ()
vkGetImageSparseMemoryRequirementsSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetImageSparseMemoryRequirements)

{-# NOINLINE vkGetImageSparseMemoryRequirementsSafe #-}
##endif

-- | > void vkGetImageSparseMemoryRequirements
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , uint32_t* pSparseMemoryRequirementCount
--   >     , VkSparseImageMemoryRequirements* pSparseMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageSparseMemoryRequirements vkGetImageSparseMemoryRequirements registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic"
               unwrapVkGetImageSparseMemoryRequirements ::
               PFN_vkGetImageSparseMemoryRequirements ->
                 HS_vkGetImageSparseMemoryRequirements

foreign import ccall safe "dynamic"
               unwrapVkGetImageSparseMemoryRequirementsSafe ::
               PFN_vkGetImageSparseMemoryRequirements ->
                 HS_vkGetImageSparseMemoryRequirements

instance VulkanProc "vkGetImageSparseMemoryRequirements" where
        type VkProcType "vkGetImageSparseMemoryRequirements" =
             HS_vkGetImageSparseMemoryRequirements
        vkProcSymbol = _VkGetImageSparseMemoryRequirements

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetImageSparseMemoryRequirements

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetImageSparseMemoryRequirementsSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkGetPhysicalDeviceSparseImageFormatProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkFormat format
-- >     , VkImageType type
-- >     , VkSampleCountFlagBits samples
-- >     , VkImageUsageFlags usage
-- >     , VkImageTiling tiling
-- >     , uint32_t* pPropertyCount
-- >     , VkSparseImageFormatProperties* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSparseImageFormatProperties vkGetPhysicalDeviceSparseImageFormatProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceSparseImageFormatProperties <- vkGetInstanceProc @VkGetPhysicalDeviceSparseImageFormatProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceSparseImageFormatProperties <- vkGetProc @VkGetPhysicalDeviceSparseImageFormatProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
                                                                      ->
                                                             Ptr VkSparseImageFormatProperties -- ^ pProperties
                                                                                               ->
                                                               IO ()
vkGetPhysicalDeviceSparseImageFormatProperties
  = unsafeDupablePerformIO
      (vkGetProc @VkGetPhysicalDeviceSparseImageFormatProperties)

{-# NOINLINE vkGetPhysicalDeviceSparseImageFormatProperties #-}
##endif

-- |
-- > void vkGetPhysicalDeviceSparseImageFormatProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkFormat format
-- >     , VkImageType type
-- >     , VkSampleCountFlagBits samples
-- >     , VkImageUsageFlags usage
-- >     , VkImageTiling tiling
-- >     , uint32_t* pPropertyCount
-- >     , VkSparseImageFormatProperties* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSparseImageFormatProperties vkGetPhysicalDeviceSparseImageFormatProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceSparseImageFormatProperties <- vkGetInstanceProc @VkGetPhysicalDeviceSparseImageFormatProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceSparseImageFormatProperties <- vkGetProc @VkGetPhysicalDeviceSparseImageFormatProperties
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
                                                                          ->
                                                                 Ptr VkSparseImageFormatProperties -- ^ pProperties
                                                                   -> IO ()
vkGetPhysicalDeviceSparseImageFormatPropertiesSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceSparseImageFormatProperties)

{-# NOINLINE vkGetPhysicalDeviceSparseImageFormatPropertiesSafe #-}
##endif

-- | > void vkGetPhysicalDeviceSparseImageFormatProperties
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSparseImageFormatProperties vkGetPhysicalDeviceSparseImageFormatProperties registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceSparseImageFormatProperties ::
               PFN_vkGetPhysicalDeviceSparseImageFormatProperties ->
                 HS_vkGetPhysicalDeviceSparseImageFormatProperties

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceSparseImageFormatPropertiesSafe ::
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
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceSparseImageFormatPropertiesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- Queues: 'sparse_binding'.
--
-- > VkResult vkQueueBindSparse
-- >     ( VkQueue queue
-- >     , uint32_t bindInfoCount
-- >     , const VkBindSparseInfo* pBindInfo
-- >     , VkFence fence
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkQueueBindSparse vkQueueBindSparse registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myQueueBindSparse <- vkGetInstanceProc @VkQueueBindSparse vkInstance
--
-- or less efficient:
--
-- > myQueueBindSparse <- vkGetProc @VkQueueBindSparse
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkQueueBindSparse" vkQueueBindSparse
               ::
               VkQueue -- ^ queue
                       -> Word32 -- ^ bindInfoCount
                                 -> Ptr VkBindSparseInfo -- ^ pBindInfo
                                                         -> VkFence -- ^ fence
                                                                    -> IO VkResult

##else
vkQueueBindSparse ::
                  VkQueue -- ^ queue
                          -> Word32 -- ^ bindInfoCount
                                    -> Ptr VkBindSparseInfo -- ^ pBindInfo
                                                            -> VkFence -- ^ fence
                                                                       -> IO VkResult
vkQueueBindSparse
  = unsafeDupablePerformIO (vkGetProc @VkQueueBindSparse)

{-# NOINLINE vkQueueBindSparse #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- Queues: 'sparse_binding'.
--
-- > VkResult vkQueueBindSparse
-- >     ( VkQueue queue
-- >     , uint32_t bindInfoCount
-- >     , const VkBindSparseInfo* pBindInfo
-- >     , VkFence fence
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkQueueBindSparse vkQueueBindSparse registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myQueueBindSparse <- vkGetInstanceProc @VkQueueBindSparse vkInstance
--
-- or less efficient:
--
-- > myQueueBindSparse <- vkGetProc @VkQueueBindSparse
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkQueueBindSparse" vkQueueBindSparseSafe
               ::
               VkQueue -- ^ queue
                       -> Word32 -- ^ bindInfoCount
                                 -> Ptr VkBindSparseInfo -- ^ pBindInfo
                                                         -> VkFence -- ^ fence
                                                                    -> IO VkResult

##else
vkQueueBindSparseSafe ::
                      VkQueue -- ^ queue
                              -> Word32 -- ^ bindInfoCount
                                        -> Ptr VkBindSparseInfo -- ^ pBindInfo
                                                                -> VkFence -- ^ fence
                                                                           -> IO VkResult
vkQueueBindSparseSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkQueueBindSparse)

{-# NOINLINE vkQueueBindSparseSafe #-}
##endif

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   Queues: 'sparse_binding'.
--
--   > VkResult vkQueueBindSparse
--   >     ( VkQueue queue
--   >     , uint32_t bindInfoCount
--   >     , const VkBindSparseInfo* pBindInfo
--   >     , VkFence fence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkQueueBindSparse vkQueueBindSparse registry at www.khronos.org>
type HS_vkQueueBindSparse =
     VkQueue -- ^ queue
             -> Word32 -- ^ bindInfoCount
                       -> Ptr VkBindSparseInfo -- ^ pBindInfo
                                               -> VkFence -- ^ fence
                                                          -> IO VkResult

type PFN_vkQueueBindSparse = FunPtr HS_vkQueueBindSparse

foreign import ccall unsafe "dynamic" unwrapVkQueueBindSparse ::
               PFN_vkQueueBindSparse -> HS_vkQueueBindSparse

foreign import ccall safe "dynamic" unwrapVkQueueBindSparseSafe ::
               PFN_vkQueueBindSparse -> HS_vkQueueBindSparse

instance VulkanProc "vkQueueBindSparse" where
        type VkProcType "vkQueueBindSparse" = HS_vkQueueBindSparse
        vkProcSymbol = _VkQueueBindSparse

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkQueueBindSparse

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkQueueBindSparseSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateFence
-- >     ( VkDevice device
-- >     , const VkFenceCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkFence* pFence
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateFence vkCreateFence registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateFence <- vkGetDeviceProc @VkCreateFence vkDevice
--
-- or less efficient:
--
-- > myCreateFence <- vkGetProc @VkCreateFence
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateFence" vkCreateFence ::
               VkDevice -- ^ device
                        ->
                 Ptr VkFenceCreateInfo -- ^ pCreateInfo
                                       ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkFence -- ^ pFence
                                                            -> IO VkResult

##else
vkCreateFence ::
              VkDevice -- ^ device
                       ->
                Ptr VkFenceCreateInfo -- ^ pCreateInfo
                                      ->
                  Ptr VkAllocationCallbacks -- ^ pAllocator
                                            -> Ptr VkFence -- ^ pFence
                                                           -> IO VkResult
vkCreateFence = unsafeDupablePerformIO (vkGetProc @VkCreateFence)

{-# NOINLINE vkCreateFence #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateFence
-- >     ( VkDevice device
-- >     , const VkFenceCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkFence* pFence
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateFence vkCreateFence registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateFence <- vkGetDeviceProc @VkCreateFence vkDevice
--
-- or less efficient:
--
-- > myCreateFence <- vkGetProc @VkCreateFence
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateFence" vkCreateFenceSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkFenceCreateInfo -- ^ pCreateInfo
                                       ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkFence -- ^ pFence
                                                            -> IO VkResult

##else
vkCreateFenceSafe ::
                  VkDevice -- ^ device
                           ->
                    Ptr VkFenceCreateInfo -- ^ pCreateInfo
                                          ->
                      Ptr VkAllocationCallbacks -- ^ pAllocator
                                                -> Ptr VkFence -- ^ pFence
                                                               -> IO VkResult
vkCreateFenceSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateFence)

{-# NOINLINE vkCreateFenceSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateFence vkCreateFence registry at www.khronos.org>
type HS_vkCreateFence =
     VkDevice -- ^ device
              ->
       Ptr VkFenceCreateInfo -- ^ pCreateInfo
                             ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkFence -- ^ pFence
                                                  -> IO VkResult

type PFN_vkCreateFence = FunPtr HS_vkCreateFence

foreign import ccall unsafe "dynamic" unwrapVkCreateFence ::
               PFN_vkCreateFence -> HS_vkCreateFence

foreign import ccall safe "dynamic" unwrapVkCreateFenceSafe ::
               PFN_vkCreateFence -> HS_vkCreateFence

instance VulkanProc "vkCreateFence" where
        type VkProcType "vkCreateFence" = HS_vkCreateFence
        vkProcSymbol = _VkCreateFence

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateFence

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateFenceSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyFence
-- >     ( VkDevice device
-- >     , VkFence fence
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyFence vkDestroyFence registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyFence <- vkGetDeviceProc @VkDestroyFence vkDevice
--
-- or less efficient:
--
-- > myDestroyFence <- vkGetProc @VkDestroyFence
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyFence" vkDestroyFence ::
               VkDevice -- ^ device
                        -> VkFence -- ^ fence
                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> IO ()

##else
vkDestroyFence ::
               VkDevice -- ^ device
                        -> VkFence -- ^ fence
                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> IO ()
vkDestroyFence = unsafeDupablePerformIO (vkGetProc @VkDestroyFence)

{-# NOINLINE vkDestroyFence #-}
##endif

-- |
-- > void vkDestroyFence
-- >     ( VkDevice device
-- >     , VkFence fence
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyFence vkDestroyFence registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyFence <- vkGetDeviceProc @VkDestroyFence vkDevice
--
-- or less efficient:
--
-- > myDestroyFence <- vkGetProc @VkDestroyFence
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyFence" vkDestroyFenceSafe ::
               VkDevice -- ^ device
                        -> VkFence -- ^ fence
                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> IO ()

##else
vkDestroyFenceSafe ::
                   VkDevice -- ^ device
                            -> VkFence -- ^ fence
                                       -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()
vkDestroyFenceSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyFence)

{-# NOINLINE vkDestroyFenceSafe #-}
##endif

-- | > void vkDestroyFence
--   >     ( VkDevice device
--   >     , VkFence fence
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyFence vkDestroyFence registry at www.khronos.org>
type HS_vkDestroyFence =
     VkDevice -- ^ device
              -> VkFence -- ^ fence
                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                      -> IO ()

type PFN_vkDestroyFence = FunPtr HS_vkDestroyFence

foreign import ccall unsafe "dynamic" unwrapVkDestroyFence ::
               PFN_vkDestroyFence -> HS_vkDestroyFence

foreign import ccall safe "dynamic" unwrapVkDestroyFenceSafe ::
               PFN_vkDestroyFence -> HS_vkDestroyFence

instance VulkanProc "vkDestroyFence" where
        type VkProcType "vkDestroyFence" = HS_vkDestroyFence
        vkProcSymbol = _VkDestroyFence

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyFence

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyFenceSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkResetFences
-- >     ( VkDevice device
-- >     , uint32_t fenceCount
-- >     , const VkFence* pFences
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkResetFences vkResetFences registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myResetFences <- vkGetDeviceProc @VkResetFences vkDevice
--
-- or less efficient:
--
-- > myResetFences <- vkGetProc @VkResetFences
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkResetFences" vkResetFences ::
               VkDevice -- ^ device
                        -> Word32 -- ^ fenceCount
                                  -> Ptr VkFence -- ^ pFences
                                                 -> IO VkResult

##else
vkResetFences :: VkDevice -- ^ device
                          -> Word32 -- ^ fenceCount
                                    -> Ptr VkFence -- ^ pFences
                                                   -> IO VkResult
vkResetFences = unsafeDupablePerformIO (vkGetProc @VkResetFences)

{-# NOINLINE vkResetFences #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkResetFences
-- >     ( VkDevice device
-- >     , uint32_t fenceCount
-- >     , const VkFence* pFences
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkResetFences vkResetFences registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myResetFences <- vkGetDeviceProc @VkResetFences vkDevice
--
-- or less efficient:
--
-- > myResetFences <- vkGetProc @VkResetFences
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkResetFences" vkResetFencesSafe ::
               VkDevice -- ^ device
                        -> Word32 -- ^ fenceCount
                                  -> Ptr VkFence -- ^ pFences
                                                 -> IO VkResult

##else
vkResetFencesSafe ::
                  VkDevice -- ^ device
                           -> Word32 -- ^ fenceCount
                                     -> Ptr VkFence -- ^ pFences
                                                    -> IO VkResult
vkResetFencesSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkResetFences)

{-# NOINLINE vkResetFencesSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkResetFences vkResetFences registry at www.khronos.org>
type HS_vkResetFences =
     VkDevice -- ^ device
              -> Word32 -- ^ fenceCount
                        -> Ptr VkFence -- ^ pFences
                                       -> IO VkResult

type PFN_vkResetFences = FunPtr HS_vkResetFences

foreign import ccall unsafe "dynamic" unwrapVkResetFences ::
               PFN_vkResetFences -> HS_vkResetFences

foreign import ccall safe "dynamic" unwrapVkResetFencesSafe ::
               PFN_vkResetFences -> HS_vkResetFences

instance VulkanProc "vkResetFences" where
        type VkProcType "vkResetFences" = HS_vkResetFences
        vkProcSymbol = _VkResetFences

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkResetFences

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkResetFencesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS', 'VK_NOT_READY'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkGetFenceStatus
-- >     ( VkDevice device
-- >     , VkFence fence
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetFenceStatus vkGetFenceStatus registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetFenceStatus <- vkGetDeviceProc @VkGetFenceStatus vkDevice
--
-- or less efficient:
--
-- > myGetFenceStatus <- vkGetProc @VkGetFenceStatus
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetFenceStatus" vkGetFenceStatus ::
               VkDevice -- ^ device
                        -> VkFence -- ^ fence
                                   -> IO VkResult

##else
vkGetFenceStatus :: VkDevice -- ^ device
                             -> VkFence -- ^ fence
                                        -> IO VkResult
vkGetFenceStatus
  = unsafeDupablePerformIO (vkGetProc @VkGetFenceStatus)

{-# NOINLINE vkGetFenceStatus #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_NOT_READY'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkGetFenceStatus
-- >     ( VkDevice device
-- >     , VkFence fence
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetFenceStatus vkGetFenceStatus registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetFenceStatus <- vkGetDeviceProc @VkGetFenceStatus vkDevice
--
-- or less efficient:
--
-- > myGetFenceStatus <- vkGetProc @VkGetFenceStatus
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetFenceStatus" vkGetFenceStatusSafe
               :: VkDevice -- ^ device
                           -> VkFence -- ^ fence
                                      -> IO VkResult

##else
vkGetFenceStatusSafe :: VkDevice -- ^ device
                                 -> VkFence -- ^ fence
                                            -> IO VkResult
vkGetFenceStatusSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkGetFenceStatus)

{-# NOINLINE vkGetFenceStatusSafe #-}
##endif

-- | Success codes: 'VK_SUCCESS', 'VK_NOT_READY'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkGetFenceStatus
--   >     ( VkDevice device
--   >     , VkFence fence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetFenceStatus vkGetFenceStatus registry at www.khronos.org>
type HS_vkGetFenceStatus = VkDevice -- ^ device
                                    -> VkFence -- ^ fence
                                               -> IO VkResult

type PFN_vkGetFenceStatus = FunPtr HS_vkGetFenceStatus

foreign import ccall unsafe "dynamic" unwrapVkGetFenceStatus ::
               PFN_vkGetFenceStatus -> HS_vkGetFenceStatus

foreign import ccall safe "dynamic" unwrapVkGetFenceStatusSafe ::
               PFN_vkGetFenceStatus -> HS_vkGetFenceStatus

instance VulkanProc "vkGetFenceStatus" where
        type VkProcType "vkGetFenceStatus" = HS_vkGetFenceStatus
        vkProcSymbol = _VkGetFenceStatus

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetFenceStatus

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetFenceStatusSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS', 'VK_TIMEOUT'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkWaitForFences
-- >     ( VkDevice device
-- >     , uint32_t fenceCount
-- >     , const VkFence* pFences
-- >     , VkBool32 waitAll
-- >     , uint64_t timeout
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkWaitForFences vkWaitForFences registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myWaitForFences <- vkGetDeviceProc @VkWaitForFences vkDevice
--
-- or less efficient:
--
-- > myWaitForFences <- vkGetProc @VkWaitForFences
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkWaitForFences" vkWaitForFences ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ fenceCount
                        -> Ptr VkFence -- ^ pFences
                                       -> VkBool32 -- ^ waitAll
                                                   -> Word64 -- ^ timeout
                                                             -> IO VkResult

##else
vkWaitForFences ::
                VkDevice -- ^ device
                         ->
                  Word32 -- ^ fenceCount
                         -> Ptr VkFence -- ^ pFences
                                        -> VkBool32 -- ^ waitAll
                                                    -> Word64 -- ^ timeout
                                                              -> IO VkResult
vkWaitForFences
  = unsafeDupablePerformIO (vkGetProc @VkWaitForFences)

{-# NOINLINE vkWaitForFences #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_TIMEOUT'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkWaitForFences
-- >     ( VkDevice device
-- >     , uint32_t fenceCount
-- >     , const VkFence* pFences
-- >     , VkBool32 waitAll
-- >     , uint64_t timeout
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkWaitForFences vkWaitForFences registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myWaitForFences <- vkGetDeviceProc @VkWaitForFences vkDevice
--
-- or less efficient:
--
-- > myWaitForFences <- vkGetProc @VkWaitForFences
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkWaitForFences" vkWaitForFencesSafe ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ fenceCount
                        -> Ptr VkFence -- ^ pFences
                                       -> VkBool32 -- ^ waitAll
                                                   -> Word64 -- ^ timeout
                                                             -> IO VkResult

##else
vkWaitForFencesSafe ::
                    VkDevice -- ^ device
                             ->
                      Word32 -- ^ fenceCount
                             -> Ptr VkFence -- ^ pFences
                                            -> VkBool32 -- ^ waitAll
                                                        -> Word64 -- ^ timeout
                                                                  -> IO VkResult
vkWaitForFencesSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkWaitForFences)

{-# NOINLINE vkWaitForFencesSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkWaitForFences vkWaitForFences registry at www.khronos.org>
type HS_vkWaitForFences =
     VkDevice -- ^ device
              ->
       Word32 -- ^ fenceCount
              -> Ptr VkFence -- ^ pFences
                             -> VkBool32 -- ^ waitAll
                                         -> Word64 -- ^ timeout
                                                   -> IO VkResult

type PFN_vkWaitForFences = FunPtr HS_vkWaitForFences

foreign import ccall unsafe "dynamic" unwrapVkWaitForFences ::
               PFN_vkWaitForFences -> HS_vkWaitForFences

foreign import ccall safe "dynamic" unwrapVkWaitForFencesSafe ::
               PFN_vkWaitForFences -> HS_vkWaitForFences

instance VulkanProc "vkWaitForFences" where
        type VkProcType "vkWaitForFences" = HS_vkWaitForFences
        vkProcSymbol = _VkWaitForFences

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkWaitForFences

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkWaitForFencesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateSemaphore
-- >     ( VkDevice device
-- >     , const VkSemaphoreCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSemaphore* pSemaphore
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSemaphore vkCreateSemaphore registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSemaphore <- vkGetDeviceProc @VkCreateSemaphore vkDevice
--
-- or less efficient:
--
-- > myCreateSemaphore <- vkGetProc @VkCreateSemaphore
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateSemaphore" vkCreateSemaphore
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSemaphoreCreateInfo -- ^ pCreateInfo
                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSemaphore -- ^ pSemaphore
                                                                -> IO VkResult

##else
vkCreateSemaphore ::
                  VkDevice -- ^ device
                           ->
                    Ptr VkSemaphoreCreateInfo -- ^ pCreateInfo
                                              ->
                      Ptr VkAllocationCallbacks -- ^ pAllocator
                                                -> Ptr VkSemaphore -- ^ pSemaphore
                                                                   -> IO VkResult
vkCreateSemaphore
  = unsafeDupablePerformIO (vkGetProc @VkCreateSemaphore)

{-# NOINLINE vkCreateSemaphore #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateSemaphore
-- >     ( VkDevice device
-- >     , const VkSemaphoreCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSemaphore* pSemaphore
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSemaphore vkCreateSemaphore registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSemaphore <- vkGetDeviceProc @VkCreateSemaphore vkDevice
--
-- or less efficient:
--
-- > myCreateSemaphore <- vkGetProc @VkCreateSemaphore
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateSemaphore" vkCreateSemaphoreSafe
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSemaphoreCreateInfo -- ^ pCreateInfo
                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSemaphore -- ^ pSemaphore
                                                                -> IO VkResult

##else
vkCreateSemaphoreSafe ::
                      VkDevice -- ^ device
                               ->
                        Ptr VkSemaphoreCreateInfo -- ^ pCreateInfo
                                                  ->
                          Ptr VkAllocationCallbacks -- ^ pAllocator
                                                    -> Ptr VkSemaphore -- ^ pSemaphore
                                                                       -> IO VkResult
vkCreateSemaphoreSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateSemaphore)

{-# NOINLINE vkCreateSemaphoreSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSemaphore vkCreateSemaphore registry at www.khronos.org>
type HS_vkCreateSemaphore =
     VkDevice -- ^ device
              ->
       Ptr VkSemaphoreCreateInfo -- ^ pCreateInfo
                                 ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSemaphore -- ^ pSemaphore
                                                      -> IO VkResult

type PFN_vkCreateSemaphore = FunPtr HS_vkCreateSemaphore

foreign import ccall unsafe "dynamic" unwrapVkCreateSemaphore ::
               PFN_vkCreateSemaphore -> HS_vkCreateSemaphore

foreign import ccall safe "dynamic" unwrapVkCreateSemaphoreSafe ::
               PFN_vkCreateSemaphore -> HS_vkCreateSemaphore

instance VulkanProc "vkCreateSemaphore" where
        type VkProcType "vkCreateSemaphore" = HS_vkCreateSemaphore
        vkProcSymbol = _VkCreateSemaphore

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateSemaphore

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateSemaphoreSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroySemaphore
-- >     ( VkDevice device
-- >     , VkSemaphore semaphore
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySemaphore vkDestroySemaphore registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroySemaphore <- vkGetDeviceProc @VkDestroySemaphore vkDevice
--
-- or less efficient:
--
-- > myDestroySemaphore <- vkGetProc @VkDestroySemaphore
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroySemaphore" vkDestroySemaphore
               :: VkDevice -- ^ device
                           -> VkSemaphore -- ^ semaphore
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

##else
vkDestroySemaphore ::
                   VkDevice -- ^ device
                            -> VkSemaphore -- ^ semaphore
                                           -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                        -> IO ()
vkDestroySemaphore
  = unsafeDupablePerformIO (vkGetProc @VkDestroySemaphore)

{-# NOINLINE vkDestroySemaphore #-}
##endif

-- |
-- > void vkDestroySemaphore
-- >     ( VkDevice device
-- >     , VkSemaphore semaphore
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySemaphore vkDestroySemaphore registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroySemaphore <- vkGetDeviceProc @VkDestroySemaphore vkDevice
--
-- or less efficient:
--
-- > myDestroySemaphore <- vkGetProc @VkDestroySemaphore
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroySemaphore"
               vkDestroySemaphoreSafe ::
               VkDevice -- ^ device
                        -> VkSemaphore -- ^ semaphore
                                       -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()

##else
vkDestroySemaphoreSafe ::
                       VkDevice -- ^ device
                                -> VkSemaphore -- ^ semaphore
                                               -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                            -> IO ()
vkDestroySemaphoreSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroySemaphore)

{-# NOINLINE vkDestroySemaphoreSafe #-}
##endif

-- | > void vkDestroySemaphore
--   >     ( VkDevice device
--   >     , VkSemaphore semaphore
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySemaphore vkDestroySemaphore registry at www.khronos.org>
type HS_vkDestroySemaphore =
     VkDevice -- ^ device
              -> VkSemaphore -- ^ semaphore
                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          -> IO ()

type PFN_vkDestroySemaphore = FunPtr HS_vkDestroySemaphore

foreign import ccall unsafe "dynamic" unwrapVkDestroySemaphore ::
               PFN_vkDestroySemaphore -> HS_vkDestroySemaphore

foreign import ccall safe "dynamic" unwrapVkDestroySemaphoreSafe ::
               PFN_vkDestroySemaphore -> HS_vkDestroySemaphore

instance VulkanProc "vkDestroySemaphore" where
        type VkProcType "vkDestroySemaphore" = HS_vkDestroySemaphore
        vkProcSymbol = _VkDestroySemaphore

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroySemaphore

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroySemaphoreSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateEvent
-- >     ( VkDevice device
-- >     , const VkEventCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkEvent* pEvent
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateEvent vkCreateEvent registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateEvent <- vkGetDeviceProc @VkCreateEvent vkDevice
--
-- or less efficient:
--
-- > myCreateEvent <- vkGetProc @VkCreateEvent
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateEvent" vkCreateEvent ::
               VkDevice -- ^ device
                        ->
                 Ptr VkEventCreateInfo -- ^ pCreateInfo
                                       ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkEvent -- ^ pEvent
                                                            -> IO VkResult

##else
vkCreateEvent ::
              VkDevice -- ^ device
                       ->
                Ptr VkEventCreateInfo -- ^ pCreateInfo
                                      ->
                  Ptr VkAllocationCallbacks -- ^ pAllocator
                                            -> Ptr VkEvent -- ^ pEvent
                                                           -> IO VkResult
vkCreateEvent = unsafeDupablePerformIO (vkGetProc @VkCreateEvent)

{-# NOINLINE vkCreateEvent #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateEvent
-- >     ( VkDevice device
-- >     , const VkEventCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkEvent* pEvent
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateEvent vkCreateEvent registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateEvent <- vkGetDeviceProc @VkCreateEvent vkDevice
--
-- or less efficient:
--
-- > myCreateEvent <- vkGetProc @VkCreateEvent
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateEvent" vkCreateEventSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkEventCreateInfo -- ^ pCreateInfo
                                       ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkEvent -- ^ pEvent
                                                            -> IO VkResult

##else
vkCreateEventSafe ::
                  VkDevice -- ^ device
                           ->
                    Ptr VkEventCreateInfo -- ^ pCreateInfo
                                          ->
                      Ptr VkAllocationCallbacks -- ^ pAllocator
                                                -> Ptr VkEvent -- ^ pEvent
                                                               -> IO VkResult
vkCreateEventSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateEvent)

{-# NOINLINE vkCreateEventSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateEvent vkCreateEvent registry at www.khronos.org>
type HS_vkCreateEvent =
     VkDevice -- ^ device
              ->
       Ptr VkEventCreateInfo -- ^ pCreateInfo
                             ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkEvent -- ^ pEvent
                                                  -> IO VkResult

type PFN_vkCreateEvent = FunPtr HS_vkCreateEvent

foreign import ccall unsafe "dynamic" unwrapVkCreateEvent ::
               PFN_vkCreateEvent -> HS_vkCreateEvent

foreign import ccall safe "dynamic" unwrapVkCreateEventSafe ::
               PFN_vkCreateEvent -> HS_vkCreateEvent

instance VulkanProc "vkCreateEvent" where
        type VkProcType "vkCreateEvent" = HS_vkCreateEvent
        vkProcSymbol = _VkCreateEvent

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateEvent

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateEventSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyEvent
-- >     ( VkDevice device
-- >     , VkEvent event
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyEvent vkDestroyEvent registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyEvent <- vkGetDeviceProc @VkDestroyEvent vkDevice
--
-- or less efficient:
--
-- > myDestroyEvent <- vkGetProc @VkDestroyEvent
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyEvent" vkDestroyEvent ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> IO ()

##else
vkDestroyEvent ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> IO ()
vkDestroyEvent = unsafeDupablePerformIO (vkGetProc @VkDestroyEvent)

{-# NOINLINE vkDestroyEvent #-}
##endif

-- |
-- > void vkDestroyEvent
-- >     ( VkDevice device
-- >     , VkEvent event
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyEvent vkDestroyEvent registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyEvent <- vkGetDeviceProc @VkDestroyEvent vkDevice
--
-- or less efficient:
--
-- > myDestroyEvent <- vkGetProc @VkDestroyEvent
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyEvent" vkDestroyEventSafe ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> IO ()

##else
vkDestroyEventSafe ::
                   VkDevice -- ^ device
                            -> VkEvent -- ^ event
                                       -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()
vkDestroyEventSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyEvent)

{-# NOINLINE vkDestroyEventSafe #-}
##endif

-- | > void vkDestroyEvent
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyEvent vkDestroyEvent registry at www.khronos.org>
type HS_vkDestroyEvent =
     VkDevice -- ^ device
              -> VkEvent -- ^ event
                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                      -> IO ()

type PFN_vkDestroyEvent = FunPtr HS_vkDestroyEvent

foreign import ccall unsafe "dynamic" unwrapVkDestroyEvent ::
               PFN_vkDestroyEvent -> HS_vkDestroyEvent

foreign import ccall safe "dynamic" unwrapVkDestroyEventSafe ::
               PFN_vkDestroyEvent -> HS_vkDestroyEvent

instance VulkanProc "vkDestroyEvent" where
        type VkProcType "vkDestroyEvent" = HS_vkDestroyEvent
        vkProcSymbol = _VkDestroyEvent

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyEvent

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyEventSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_EVENT_SET', 'VK_EVENT_RESET'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkGetEventStatus
-- >     ( VkDevice device
-- >     , VkEvent event
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetEventStatus vkGetEventStatus registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetEventStatus <- vkGetDeviceProc @VkGetEventStatus vkDevice
--
-- or less efficient:
--
-- > myGetEventStatus <- vkGetProc @VkGetEventStatus
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetEventStatus" vkGetEventStatus ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> IO VkResult

##else
vkGetEventStatus :: VkDevice -- ^ device
                             -> VkEvent -- ^ event
                                        -> IO VkResult
vkGetEventStatus
  = unsafeDupablePerformIO (vkGetProc @VkGetEventStatus)

{-# NOINLINE vkGetEventStatus #-}
##endif

-- |
-- Success codes: 'VK_EVENT_SET', 'VK_EVENT_RESET'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkGetEventStatus
-- >     ( VkDevice device
-- >     , VkEvent event
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetEventStatus vkGetEventStatus registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetEventStatus <- vkGetDeviceProc @VkGetEventStatus vkDevice
--
-- or less efficient:
--
-- > myGetEventStatus <- vkGetProc @VkGetEventStatus
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetEventStatus" vkGetEventStatusSafe
               :: VkDevice -- ^ device
                           -> VkEvent -- ^ event
                                      -> IO VkResult

##else
vkGetEventStatusSafe :: VkDevice -- ^ device
                                 -> VkEvent -- ^ event
                                            -> IO VkResult
vkGetEventStatusSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkGetEventStatus)

{-# NOINLINE vkGetEventStatusSafe #-}
##endif

-- | Success codes: 'VK_EVENT_SET', 'VK_EVENT_RESET'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkGetEventStatus
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetEventStatus vkGetEventStatus registry at www.khronos.org>
type HS_vkGetEventStatus = VkDevice -- ^ device
                                    -> VkEvent -- ^ event
                                               -> IO VkResult

type PFN_vkGetEventStatus = FunPtr HS_vkGetEventStatus

foreign import ccall unsafe "dynamic" unwrapVkGetEventStatus ::
               PFN_vkGetEventStatus -> HS_vkGetEventStatus

foreign import ccall safe "dynamic" unwrapVkGetEventStatusSafe ::
               PFN_vkGetEventStatus -> HS_vkGetEventStatus

instance VulkanProc "vkGetEventStatus" where
        type VkProcType "vkGetEventStatus" = HS_vkGetEventStatus
        vkProcSymbol = _VkGetEventStatus

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetEventStatus

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetEventStatusSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkSetEvent
-- >     ( VkDevice device
-- >     , VkEvent event
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkSetEvent vkSetEvent registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > mySetEvent <- vkGetDeviceProc @VkSetEvent vkDevice
--
-- or less efficient:
--
-- > mySetEvent <- vkGetProc @VkSetEvent
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkSetEvent" vkSetEvent ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> IO VkResult

##else
vkSetEvent :: VkDevice -- ^ device
                       -> VkEvent -- ^ event
                                  -> IO VkResult
vkSetEvent = unsafeDupablePerformIO (vkGetProc @VkSetEvent)

{-# NOINLINE vkSetEvent #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkSetEvent
-- >     ( VkDevice device
-- >     , VkEvent event
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkSetEvent vkSetEvent registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > mySetEvent <- vkGetDeviceProc @VkSetEvent vkDevice
--
-- or less efficient:
--
-- > mySetEvent <- vkGetProc @VkSetEvent
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkSetEvent" vkSetEventSafe ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> IO VkResult

##else
vkSetEventSafe :: VkDevice -- ^ device
                           -> VkEvent -- ^ event
                                      -> IO VkResult
vkSetEventSafe = unsafeDupablePerformIO (vkGetProcSafe @VkSetEvent)

{-# NOINLINE vkSetEventSafe #-}
##endif

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkSetEvent
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkSetEvent vkSetEvent registry at www.khronos.org>
type HS_vkSetEvent = VkDevice -- ^ device
                              -> VkEvent -- ^ event
                                         -> IO VkResult

type PFN_vkSetEvent = FunPtr HS_vkSetEvent

foreign import ccall unsafe "dynamic" unwrapVkSetEvent ::
               PFN_vkSetEvent -> HS_vkSetEvent

foreign import ccall safe "dynamic" unwrapVkSetEventSafe ::
               PFN_vkSetEvent -> HS_vkSetEvent

instance VulkanProc "vkSetEvent" where
        type VkProcType "vkSetEvent" = HS_vkSetEvent
        vkProcSymbol = _VkSetEvent

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkSetEvent

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkSetEventSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkResetEvent
-- >     ( VkDevice device
-- >     , VkEvent event
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkResetEvent vkResetEvent registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myResetEvent <- vkGetDeviceProc @VkResetEvent vkDevice
--
-- or less efficient:
--
-- > myResetEvent <- vkGetProc @VkResetEvent
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkResetEvent" vkResetEvent ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> IO VkResult

##else
vkResetEvent :: VkDevice -- ^ device
                         -> VkEvent -- ^ event
                                    -> IO VkResult
vkResetEvent = unsafeDupablePerformIO (vkGetProc @VkResetEvent)

{-# NOINLINE vkResetEvent #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkResetEvent
-- >     ( VkDevice device
-- >     , VkEvent event
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkResetEvent vkResetEvent registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myResetEvent <- vkGetDeviceProc @VkResetEvent vkDevice
--
-- or less efficient:
--
-- > myResetEvent <- vkGetProc @VkResetEvent
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkResetEvent" vkResetEventSafe ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> IO VkResult

##else
vkResetEventSafe :: VkDevice -- ^ device
                             -> VkEvent -- ^ event
                                        -> IO VkResult
vkResetEventSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkResetEvent)

{-# NOINLINE vkResetEventSafe #-}
##endif

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetEvent
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkResetEvent vkResetEvent registry at www.khronos.org>
type HS_vkResetEvent = VkDevice -- ^ device
                                -> VkEvent -- ^ event
                                           -> IO VkResult

type PFN_vkResetEvent = FunPtr HS_vkResetEvent

foreign import ccall unsafe "dynamic" unwrapVkResetEvent ::
               PFN_vkResetEvent -> HS_vkResetEvent

foreign import ccall safe "dynamic" unwrapVkResetEventSafe ::
               PFN_vkResetEvent -> HS_vkResetEvent

instance VulkanProc "vkResetEvent" where
        type VkProcType "vkResetEvent" = HS_vkResetEvent
        vkProcSymbol = _VkResetEvent

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkResetEvent

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkResetEventSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateQueryPool
-- >     ( VkDevice device
-- >     , const VkQueryPoolCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkQueryPool* pQueryPool
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateQueryPool vkCreateQueryPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateQueryPool <- vkGetDeviceProc @VkCreateQueryPool vkDevice
--
-- or less efficient:
--
-- > myCreateQueryPool <- vkGetProc @VkCreateQueryPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateQueryPool" vkCreateQueryPool
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkQueryPoolCreateInfo -- ^ pCreateInfo
                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkQueryPool -- ^ pQueryPool
                                                                -> IO VkResult

##else
vkCreateQueryPool ::
                  VkDevice -- ^ device
                           ->
                    Ptr VkQueryPoolCreateInfo -- ^ pCreateInfo
                                              ->
                      Ptr VkAllocationCallbacks -- ^ pAllocator
                                                -> Ptr VkQueryPool -- ^ pQueryPool
                                                                   -> IO VkResult
vkCreateQueryPool
  = unsafeDupablePerformIO (vkGetProc @VkCreateQueryPool)

{-# NOINLINE vkCreateQueryPool #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateQueryPool
-- >     ( VkDevice device
-- >     , const VkQueryPoolCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkQueryPool* pQueryPool
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateQueryPool vkCreateQueryPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateQueryPool <- vkGetDeviceProc @VkCreateQueryPool vkDevice
--
-- or less efficient:
--
-- > myCreateQueryPool <- vkGetProc @VkCreateQueryPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateQueryPool" vkCreateQueryPoolSafe
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkQueryPoolCreateInfo -- ^ pCreateInfo
                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkQueryPool -- ^ pQueryPool
                                                                -> IO VkResult

##else
vkCreateQueryPoolSafe ::
                      VkDevice -- ^ device
                               ->
                        Ptr VkQueryPoolCreateInfo -- ^ pCreateInfo
                                                  ->
                          Ptr VkAllocationCallbacks -- ^ pAllocator
                                                    -> Ptr VkQueryPool -- ^ pQueryPool
                                                                       -> IO VkResult
vkCreateQueryPoolSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateQueryPool)

{-# NOINLINE vkCreateQueryPoolSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateQueryPool vkCreateQueryPool registry at www.khronos.org>
type HS_vkCreateQueryPool =
     VkDevice -- ^ device
              ->
       Ptr VkQueryPoolCreateInfo -- ^ pCreateInfo
                                 ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkQueryPool -- ^ pQueryPool
                                                      -> IO VkResult

type PFN_vkCreateQueryPool = FunPtr HS_vkCreateQueryPool

foreign import ccall unsafe "dynamic" unwrapVkCreateQueryPool ::
               PFN_vkCreateQueryPool -> HS_vkCreateQueryPool

foreign import ccall safe "dynamic" unwrapVkCreateQueryPoolSafe ::
               PFN_vkCreateQueryPool -> HS_vkCreateQueryPool

instance VulkanProc "vkCreateQueryPool" where
        type VkProcType "vkCreateQueryPool" = HS_vkCreateQueryPool
        vkProcSymbol = _VkCreateQueryPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateQueryPool

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateQueryPoolSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyQueryPool
-- >     ( VkDevice device
-- >     , VkQueryPool queryPool
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyQueryPool vkDestroyQueryPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyQueryPool <- vkGetDeviceProc @VkDestroyQueryPool vkDevice
--
-- or less efficient:
--
-- > myDestroyQueryPool <- vkGetProc @VkDestroyQueryPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyQueryPool" vkDestroyQueryPool
               :: VkDevice -- ^ device
                           -> VkQueryPool -- ^ queryPool
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

##else
vkDestroyQueryPool ::
                   VkDevice -- ^ device
                            -> VkQueryPool -- ^ queryPool
                                           -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                        -> IO ()
vkDestroyQueryPool
  = unsafeDupablePerformIO (vkGetProc @VkDestroyQueryPool)

{-# NOINLINE vkDestroyQueryPool #-}
##endif

-- |
-- > void vkDestroyQueryPool
-- >     ( VkDevice device
-- >     , VkQueryPool queryPool
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyQueryPool vkDestroyQueryPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyQueryPool <- vkGetDeviceProc @VkDestroyQueryPool vkDevice
--
-- or less efficient:
--
-- > myDestroyQueryPool <- vkGetProc @VkDestroyQueryPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyQueryPool"
               vkDestroyQueryPoolSafe ::
               VkDevice -- ^ device
                        -> VkQueryPool -- ^ queryPool
                                       -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()

##else
vkDestroyQueryPoolSafe ::
                       VkDevice -- ^ device
                                -> VkQueryPool -- ^ queryPool
                                               -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                            -> IO ()
vkDestroyQueryPoolSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyQueryPool)

{-# NOINLINE vkDestroyQueryPoolSafe #-}
##endif

-- | > void vkDestroyQueryPool
--   >     ( VkDevice device
--   >     , VkQueryPool queryPool
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyQueryPool vkDestroyQueryPool registry at www.khronos.org>
type HS_vkDestroyQueryPool =
     VkDevice -- ^ device
              -> VkQueryPool -- ^ queryPool
                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          -> IO ()

type PFN_vkDestroyQueryPool = FunPtr HS_vkDestroyQueryPool

foreign import ccall unsafe "dynamic" unwrapVkDestroyQueryPool ::
               PFN_vkDestroyQueryPool -> HS_vkDestroyQueryPool

foreign import ccall safe "dynamic" unwrapVkDestroyQueryPoolSafe ::
               PFN_vkDestroyQueryPool -> HS_vkDestroyQueryPool

instance VulkanProc "vkDestroyQueryPool" where
        type VkProcType "vkDestroyQueryPool" = HS_vkDestroyQueryPool
        vkProcSymbol = _VkDestroyQueryPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyQueryPool

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyQueryPoolSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS', 'VK_NOT_READY'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkGetQueryPoolResults
-- >     ( VkDevice device
-- >     , VkQueryPool queryPool
-- >     , uint32_t firstQuery
-- >     , uint32_t queryCount
-- >     , size_t dataSize
-- >     , void* pData
-- >     , VkDeviceSize stride
-- >     , VkQueryResultFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetQueryPoolResults vkGetQueryPoolResults registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetQueryPoolResults <- vkGetDeviceProc @VkGetQueryPoolResults vkDevice
--
-- or less efficient:
--
-- > myGetQueryPoolResults <- vkGetProc @VkGetQueryPoolResults
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkGetQueryPoolResults
  = unsafeDupablePerformIO (vkGetProc @VkGetQueryPoolResults)

{-# NOINLINE vkGetQueryPoolResults #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_NOT_READY'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkGetQueryPoolResults
-- >     ( VkDevice device
-- >     , VkQueryPool queryPool
-- >     , uint32_t firstQuery
-- >     , uint32_t queryCount
-- >     , size_t dataSize
-- >     , void* pData
-- >     , VkDeviceSize stride
-- >     , VkQueryResultFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetQueryPoolResults vkGetQueryPoolResults registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetQueryPoolResults <- vkGetDeviceProc @VkGetQueryPoolResults vkDevice
--
-- or less efficient:
--
-- > myGetQueryPoolResults <- vkGetProc @VkGetQueryPoolResults
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkGetQueryPoolResultsSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkGetQueryPoolResults)

{-# NOINLINE vkGetQueryPoolResultsSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetQueryPoolResults vkGetQueryPoolResults registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic" unwrapVkGetQueryPoolResults
               :: PFN_vkGetQueryPoolResults -> HS_vkGetQueryPoolResults

foreign import ccall safe "dynamic" unwrapVkGetQueryPoolResultsSafe
               :: PFN_vkGetQueryPoolResults -> HS_vkGetQueryPoolResults

instance VulkanProc "vkGetQueryPoolResults" where
        type VkProcType "vkGetQueryPoolResults" = HS_vkGetQueryPoolResults
        vkProcSymbol = _VkGetQueryPoolResults

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetQueryPoolResults

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetQueryPoolResultsSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateBuffer
-- >     ( VkDevice device
-- >     , const VkBufferCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkBuffer* pBuffer
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateBuffer vkCreateBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateBuffer <- vkGetDeviceProc @VkCreateBuffer vkDevice
--
-- or less efficient:
--
-- > myCreateBuffer <- vkGetProc @VkCreateBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateBuffer" vkCreateBuffer ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferCreateInfo -- ^ pCreateInfo
                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkBuffer -- ^ pBuffer
                                                             -> IO VkResult

##else
vkCreateBuffer ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferCreateInfo -- ^ pCreateInfo
                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkBuffer -- ^ pBuffer
                                                             -> IO VkResult
vkCreateBuffer = unsafeDupablePerformIO (vkGetProc @VkCreateBuffer)

{-# NOINLINE vkCreateBuffer #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateBuffer
-- >     ( VkDevice device
-- >     , const VkBufferCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkBuffer* pBuffer
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateBuffer vkCreateBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateBuffer <- vkGetDeviceProc @VkCreateBuffer vkDevice
--
-- or less efficient:
--
-- > myCreateBuffer <- vkGetProc @VkCreateBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateBuffer" vkCreateBufferSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferCreateInfo -- ^ pCreateInfo
                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkBuffer -- ^ pBuffer
                                                             -> IO VkResult

##else
vkCreateBufferSafe ::
                   VkDevice -- ^ device
                            ->
                     Ptr VkBufferCreateInfo -- ^ pCreateInfo
                                            ->
                       Ptr VkAllocationCallbacks -- ^ pAllocator
                                                 -> Ptr VkBuffer -- ^ pBuffer
                                                                 -> IO VkResult
vkCreateBufferSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateBuffer)

{-# NOINLINE vkCreateBufferSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateBuffer vkCreateBuffer registry at www.khronos.org>
type HS_vkCreateBuffer =
     VkDevice -- ^ device
              ->
       Ptr VkBufferCreateInfo -- ^ pCreateInfo
                              ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkBuffer -- ^ pBuffer
                                                   -> IO VkResult

type PFN_vkCreateBuffer = FunPtr HS_vkCreateBuffer

foreign import ccall unsafe "dynamic" unwrapVkCreateBuffer ::
               PFN_vkCreateBuffer -> HS_vkCreateBuffer

foreign import ccall safe "dynamic" unwrapVkCreateBufferSafe ::
               PFN_vkCreateBuffer -> HS_vkCreateBuffer

instance VulkanProc "vkCreateBuffer" where
        type VkProcType "vkCreateBuffer" = HS_vkCreateBuffer
        vkProcSymbol = _VkCreateBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateBuffer

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateBufferSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyBuffer
-- >     ( VkDevice device
-- >     , VkBuffer buffer
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyBuffer vkDestroyBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyBuffer <- vkGetDeviceProc @VkDestroyBuffer vkDevice
--
-- or less efficient:
--
-- > myDestroyBuffer <- vkGetProc @VkDestroyBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyBuffer" vkDestroyBuffer ::
               VkDevice -- ^ device
                        -> VkBuffer -- ^ buffer
                                    -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                 -> IO ()

##else
vkDestroyBuffer ::
                VkDevice -- ^ device
                         -> VkBuffer -- ^ buffer
                                     -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                  -> IO ()
vkDestroyBuffer
  = unsafeDupablePerformIO (vkGetProc @VkDestroyBuffer)

{-# NOINLINE vkDestroyBuffer #-}
##endif

-- |
-- > void vkDestroyBuffer
-- >     ( VkDevice device
-- >     , VkBuffer buffer
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyBuffer vkDestroyBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyBuffer <- vkGetDeviceProc @VkDestroyBuffer vkDevice
--
-- or less efficient:
--
-- > myDestroyBuffer <- vkGetProc @VkDestroyBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyBuffer" vkDestroyBufferSafe ::
               VkDevice -- ^ device
                        -> VkBuffer -- ^ buffer
                                    -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                 -> IO ()

##else
vkDestroyBufferSafe ::
                    VkDevice -- ^ device
                             -> VkBuffer -- ^ buffer
                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()
vkDestroyBufferSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyBuffer)

{-# NOINLINE vkDestroyBufferSafe #-}
##endif

-- | > void vkDestroyBuffer
--   >     ( VkDevice device
--   >     , VkBuffer buffer
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyBuffer vkDestroyBuffer registry at www.khronos.org>
type HS_vkDestroyBuffer =
     VkDevice -- ^ device
              -> VkBuffer -- ^ buffer
                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                       -> IO ()

type PFN_vkDestroyBuffer = FunPtr HS_vkDestroyBuffer

foreign import ccall unsafe "dynamic" unwrapVkDestroyBuffer ::
               PFN_vkDestroyBuffer -> HS_vkDestroyBuffer

foreign import ccall safe "dynamic" unwrapVkDestroyBufferSafe ::
               PFN_vkDestroyBuffer -> HS_vkDestroyBuffer

instance VulkanProc "vkDestroyBuffer" where
        type VkProcType "vkDestroyBuffer" = HS_vkDestroyBuffer
        vkProcSymbol = _VkDestroyBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyBuffer

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyBufferSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateBufferView
-- >     ( VkDevice device
-- >     , const VkBufferViewCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkBufferView* pView
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateBufferView vkCreateBufferView registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateBufferView <- vkGetDeviceProc @VkCreateBufferView vkDevice
--
-- or less efficient:
--
-- > myCreateBufferView <- vkGetProc @VkCreateBufferView
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateBufferView" vkCreateBufferView
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferViewCreateInfo -- ^ pCreateInfo
                                            ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkBufferView -- ^ pView
                                                                 -> IO VkResult

##else
vkCreateBufferView ::
                   VkDevice -- ^ device
                            ->
                     Ptr VkBufferViewCreateInfo -- ^ pCreateInfo
                                                ->
                       Ptr VkAllocationCallbacks -- ^ pAllocator
                                                 -> Ptr VkBufferView -- ^ pView
                                                                     -> IO VkResult
vkCreateBufferView
  = unsafeDupablePerformIO (vkGetProc @VkCreateBufferView)

{-# NOINLINE vkCreateBufferView #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateBufferView
-- >     ( VkDevice device
-- >     , const VkBufferViewCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkBufferView* pView
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateBufferView vkCreateBufferView registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateBufferView <- vkGetDeviceProc @VkCreateBufferView vkDevice
--
-- or less efficient:
--
-- > myCreateBufferView <- vkGetProc @VkCreateBufferView
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateBufferView"
               vkCreateBufferViewSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferViewCreateInfo -- ^ pCreateInfo
                                            ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkBufferView -- ^ pView
                                                                 -> IO VkResult

##else
vkCreateBufferViewSafe ::
                       VkDevice -- ^ device
                                ->
                         Ptr VkBufferViewCreateInfo -- ^ pCreateInfo
                                                    ->
                           Ptr VkAllocationCallbacks -- ^ pAllocator
                                                     -> Ptr VkBufferView -- ^ pView
                                                                         -> IO VkResult
vkCreateBufferViewSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateBufferView)

{-# NOINLINE vkCreateBufferViewSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateBufferView vkCreateBufferView registry at www.khronos.org>
type HS_vkCreateBufferView =
     VkDevice -- ^ device
              ->
       Ptr VkBufferViewCreateInfo -- ^ pCreateInfo
                                  ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkBufferView -- ^ pView
                                                       -> IO VkResult

type PFN_vkCreateBufferView = FunPtr HS_vkCreateBufferView

foreign import ccall unsafe "dynamic" unwrapVkCreateBufferView ::
               PFN_vkCreateBufferView -> HS_vkCreateBufferView

foreign import ccall safe "dynamic" unwrapVkCreateBufferViewSafe ::
               PFN_vkCreateBufferView -> HS_vkCreateBufferView

instance VulkanProc "vkCreateBufferView" where
        type VkProcType "vkCreateBufferView" = HS_vkCreateBufferView
        vkProcSymbol = _VkCreateBufferView

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateBufferView

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateBufferViewSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyBufferView
-- >     ( VkDevice device
-- >     , VkBufferView bufferView
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyBufferView vkDestroyBufferView registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyBufferView <- vkGetDeviceProc @VkDestroyBufferView vkDevice
--
-- or less efficient:
--
-- > myDestroyBufferView <- vkGetProc @VkDestroyBufferView
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyBufferView"
               vkDestroyBufferView ::
               VkDevice -- ^ device
                        -> VkBufferView -- ^ bufferView
                                        -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                     -> IO ()

##else
vkDestroyBufferView ::
                    VkDevice -- ^ device
                             -> VkBufferView -- ^ bufferView
                                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                          -> IO ()
vkDestroyBufferView
  = unsafeDupablePerformIO (vkGetProc @VkDestroyBufferView)

{-# NOINLINE vkDestroyBufferView #-}
##endif

-- |
-- > void vkDestroyBufferView
-- >     ( VkDevice device
-- >     , VkBufferView bufferView
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyBufferView vkDestroyBufferView registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyBufferView <- vkGetDeviceProc @VkDestroyBufferView vkDevice
--
-- or less efficient:
--
-- > myDestroyBufferView <- vkGetProc @VkDestroyBufferView
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyBufferView"
               vkDestroyBufferViewSafe ::
               VkDevice -- ^ device
                        -> VkBufferView -- ^ bufferView
                                        -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                     -> IO ()

##else
vkDestroyBufferViewSafe ::
                        VkDevice -- ^ device
                                 -> VkBufferView -- ^ bufferView
                                                 -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                              -> IO ()
vkDestroyBufferViewSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyBufferView)

{-# NOINLINE vkDestroyBufferViewSafe #-}
##endif

-- | > void vkDestroyBufferView
--   >     ( VkDevice device
--   >     , VkBufferView bufferView
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyBufferView vkDestroyBufferView registry at www.khronos.org>
type HS_vkDestroyBufferView =
     VkDevice -- ^ device
              -> VkBufferView -- ^ bufferView
                              -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                           -> IO ()

type PFN_vkDestroyBufferView = FunPtr HS_vkDestroyBufferView

foreign import ccall unsafe "dynamic" unwrapVkDestroyBufferView ::
               PFN_vkDestroyBufferView -> HS_vkDestroyBufferView

foreign import ccall safe "dynamic" unwrapVkDestroyBufferViewSafe
               :: PFN_vkDestroyBufferView -> HS_vkDestroyBufferView

instance VulkanProc "vkDestroyBufferView" where
        type VkProcType "vkDestroyBufferView" = HS_vkDestroyBufferView
        vkProcSymbol = _VkDestroyBufferView

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyBufferView

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyBufferViewSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateImage
-- >     ( VkDevice device
-- >     , const VkImageCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkImage* pImage
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateImage vkCreateImage registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateImage <- vkGetDeviceProc @VkCreateImage vkDevice
--
-- or less efficient:
--
-- > myCreateImage <- vkGetProc @VkCreateImage
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateImage" vkCreateImage ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageCreateInfo -- ^ pCreateInfo
                                       ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkImage -- ^ pImage
                                                            -> IO VkResult

##else
vkCreateImage ::
              VkDevice -- ^ device
                       ->
                Ptr VkImageCreateInfo -- ^ pCreateInfo
                                      ->
                  Ptr VkAllocationCallbacks -- ^ pAllocator
                                            -> Ptr VkImage -- ^ pImage
                                                           -> IO VkResult
vkCreateImage = unsafeDupablePerformIO (vkGetProc @VkCreateImage)

{-# NOINLINE vkCreateImage #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateImage
-- >     ( VkDevice device
-- >     , const VkImageCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkImage* pImage
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateImage vkCreateImage registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateImage <- vkGetDeviceProc @VkCreateImage vkDevice
--
-- or less efficient:
--
-- > myCreateImage <- vkGetProc @VkCreateImage
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateImage" vkCreateImageSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageCreateInfo -- ^ pCreateInfo
                                       ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkImage -- ^ pImage
                                                            -> IO VkResult

##else
vkCreateImageSafe ::
                  VkDevice -- ^ device
                           ->
                    Ptr VkImageCreateInfo -- ^ pCreateInfo
                                          ->
                      Ptr VkAllocationCallbacks -- ^ pAllocator
                                                -> Ptr VkImage -- ^ pImage
                                                               -> IO VkResult
vkCreateImageSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateImage)

{-# NOINLINE vkCreateImageSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateImage vkCreateImage registry at www.khronos.org>
type HS_vkCreateImage =
     VkDevice -- ^ device
              ->
       Ptr VkImageCreateInfo -- ^ pCreateInfo
                             ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkImage -- ^ pImage
                                                  -> IO VkResult

type PFN_vkCreateImage = FunPtr HS_vkCreateImage

foreign import ccall unsafe "dynamic" unwrapVkCreateImage ::
               PFN_vkCreateImage -> HS_vkCreateImage

foreign import ccall safe "dynamic" unwrapVkCreateImageSafe ::
               PFN_vkCreateImage -> HS_vkCreateImage

instance VulkanProc "vkCreateImage" where
        type VkProcType "vkCreateImage" = HS_vkCreateImage
        vkProcSymbol = _VkCreateImage

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateImage

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateImageSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyImage
-- >     ( VkDevice device
-- >     , VkImage image
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyImage vkDestroyImage registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyImage <- vkGetDeviceProc @VkDestroyImage vkDevice
--
-- or less efficient:
--
-- > myDestroyImage <- vkGetProc @VkDestroyImage
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyImage" vkDestroyImage ::
               VkDevice -- ^ device
                        -> VkImage -- ^ image
                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> IO ()

##else
vkDestroyImage ::
               VkDevice -- ^ device
                        -> VkImage -- ^ image
                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> IO ()
vkDestroyImage = unsafeDupablePerformIO (vkGetProc @VkDestroyImage)

{-# NOINLINE vkDestroyImage #-}
##endif

-- |
-- > void vkDestroyImage
-- >     ( VkDevice device
-- >     , VkImage image
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyImage vkDestroyImage registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyImage <- vkGetDeviceProc @VkDestroyImage vkDevice
--
-- or less efficient:
--
-- > myDestroyImage <- vkGetProc @VkDestroyImage
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyImage" vkDestroyImageSafe ::
               VkDevice -- ^ device
                        -> VkImage -- ^ image
                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> IO ()

##else
vkDestroyImageSafe ::
                   VkDevice -- ^ device
                            -> VkImage -- ^ image
                                       -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()
vkDestroyImageSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyImage)

{-# NOINLINE vkDestroyImageSafe #-}
##endif

-- | > void vkDestroyImage
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyImage vkDestroyImage registry at www.khronos.org>
type HS_vkDestroyImage =
     VkDevice -- ^ device
              -> VkImage -- ^ image
                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                      -> IO ()

type PFN_vkDestroyImage = FunPtr HS_vkDestroyImage

foreign import ccall unsafe "dynamic" unwrapVkDestroyImage ::
               PFN_vkDestroyImage -> HS_vkDestroyImage

foreign import ccall safe "dynamic" unwrapVkDestroyImageSafe ::
               PFN_vkDestroyImage -> HS_vkDestroyImage

instance VulkanProc "vkDestroyImage" where
        type VkProcType "vkDestroyImage" = HS_vkDestroyImage
        vkProcSymbol = _VkDestroyImage

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyImage

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyImageSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkGetImageSubresourceLayout
-- >     ( VkDevice device
-- >     , VkImage image
-- >     , const VkImageSubresource* pSubresource
-- >     , VkSubresourceLayout* pLayout
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageSubresourceLayout vkGetImageSubresourceLayout registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetImageSubresourceLayout <- vkGetDeviceProc @VkGetImageSubresourceLayout vkDevice
--
-- or less efficient:
--
-- > myGetImageSubresourceLayout <- vkGetProc @VkGetImageSubresourceLayout
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetImageSubresourceLayout"
               vkGetImageSubresourceLayout ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         ->
                   Ptr VkImageSubresource -- ^ pSubresource
                                          -> Ptr VkSubresourceLayout -- ^ pLayout
                                                                     -> IO ()

##else
vkGetImageSubresourceLayout ::
                            VkDevice -- ^ device
                                     ->
                              VkImage -- ^ image
                                      ->
                                Ptr VkImageSubresource -- ^ pSubresource
                                                       -> Ptr VkSubresourceLayout -- ^ pLayout
                                                                                  -> IO ()
vkGetImageSubresourceLayout
  = unsafeDupablePerformIO (vkGetProc @VkGetImageSubresourceLayout)

{-# NOINLINE vkGetImageSubresourceLayout #-}
##endif

-- |
-- > void vkGetImageSubresourceLayout
-- >     ( VkDevice device
-- >     , VkImage image
-- >     , const VkImageSubresource* pSubresource
-- >     , VkSubresourceLayout* pLayout
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageSubresourceLayout vkGetImageSubresourceLayout registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetImageSubresourceLayout <- vkGetDeviceProc @VkGetImageSubresourceLayout vkDevice
--
-- or less efficient:
--
-- > myGetImageSubresourceLayout <- vkGetProc @VkGetImageSubresourceLayout
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetImageSubresourceLayout"
               vkGetImageSubresourceLayoutSafe ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         ->
                   Ptr VkImageSubresource -- ^ pSubresource
                                          -> Ptr VkSubresourceLayout -- ^ pLayout
                                                                     -> IO ()

##else
vkGetImageSubresourceLayoutSafe ::
                                VkDevice -- ^ device
                                         ->
                                  VkImage -- ^ image
                                          ->
                                    Ptr VkImageSubresource -- ^ pSubresource
                                                           -> Ptr VkSubresourceLayout -- ^ pLayout
                                                                                      -> IO ()
vkGetImageSubresourceLayoutSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetImageSubresourceLayout)

{-# NOINLINE vkGetImageSubresourceLayoutSafe #-}
##endif

-- | > void vkGetImageSubresourceLayout
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , const VkImageSubresource* pSubresource
--   >     , VkSubresourceLayout* pLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageSubresourceLayout vkGetImageSubresourceLayout registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic"
               unwrapVkGetImageSubresourceLayout ::
               PFN_vkGetImageSubresourceLayout -> HS_vkGetImageSubresourceLayout

foreign import ccall safe "dynamic"
               unwrapVkGetImageSubresourceLayoutSafe ::
               PFN_vkGetImageSubresourceLayout -> HS_vkGetImageSubresourceLayout

instance VulkanProc "vkGetImageSubresourceLayout" where
        type VkProcType "vkGetImageSubresourceLayout" =
             HS_vkGetImageSubresourceLayout
        vkProcSymbol = _VkGetImageSubresourceLayout

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetImageSubresourceLayout

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetImageSubresourceLayoutSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateImageView
-- >     ( VkDevice device
-- >     , const VkImageViewCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkImageView* pView
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateImageView vkCreateImageView registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateImageView <- vkGetDeviceProc @VkCreateImageView vkDevice
--
-- or less efficient:
--
-- > myCreateImageView <- vkGetProc @VkCreateImageView
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateImageView" vkCreateImageView
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageViewCreateInfo -- ^ pCreateInfo
                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkImageView -- ^ pView
                                                                -> IO VkResult

##else
vkCreateImageView ::
                  VkDevice -- ^ device
                           ->
                    Ptr VkImageViewCreateInfo -- ^ pCreateInfo
                                              ->
                      Ptr VkAllocationCallbacks -- ^ pAllocator
                                                -> Ptr VkImageView -- ^ pView
                                                                   -> IO VkResult
vkCreateImageView
  = unsafeDupablePerformIO (vkGetProc @VkCreateImageView)

{-# NOINLINE vkCreateImageView #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateImageView
-- >     ( VkDevice device
-- >     , const VkImageViewCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkImageView* pView
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateImageView vkCreateImageView registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateImageView <- vkGetDeviceProc @VkCreateImageView vkDevice
--
-- or less efficient:
--
-- > myCreateImageView <- vkGetProc @VkCreateImageView
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateImageView" vkCreateImageViewSafe
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageViewCreateInfo -- ^ pCreateInfo
                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkImageView -- ^ pView
                                                                -> IO VkResult

##else
vkCreateImageViewSafe ::
                      VkDevice -- ^ device
                               ->
                        Ptr VkImageViewCreateInfo -- ^ pCreateInfo
                                                  ->
                          Ptr VkAllocationCallbacks -- ^ pAllocator
                                                    -> Ptr VkImageView -- ^ pView
                                                                       -> IO VkResult
vkCreateImageViewSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateImageView)

{-# NOINLINE vkCreateImageViewSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateImageView vkCreateImageView registry at www.khronos.org>
type HS_vkCreateImageView =
     VkDevice -- ^ device
              ->
       Ptr VkImageViewCreateInfo -- ^ pCreateInfo
                                 ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkImageView -- ^ pView
                                                      -> IO VkResult

type PFN_vkCreateImageView = FunPtr HS_vkCreateImageView

foreign import ccall unsafe "dynamic" unwrapVkCreateImageView ::
               PFN_vkCreateImageView -> HS_vkCreateImageView

foreign import ccall safe "dynamic" unwrapVkCreateImageViewSafe ::
               PFN_vkCreateImageView -> HS_vkCreateImageView

instance VulkanProc "vkCreateImageView" where
        type VkProcType "vkCreateImageView" = HS_vkCreateImageView
        vkProcSymbol = _VkCreateImageView

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateImageView

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateImageViewSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyImageView
-- >     ( VkDevice device
-- >     , VkImageView imageView
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyImageView vkDestroyImageView registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyImageView <- vkGetDeviceProc @VkDestroyImageView vkDevice
--
-- or less efficient:
--
-- > myDestroyImageView <- vkGetProc @VkDestroyImageView
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyImageView" vkDestroyImageView
               :: VkDevice -- ^ device
                           -> VkImageView -- ^ imageView
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

##else
vkDestroyImageView ::
                   VkDevice -- ^ device
                            -> VkImageView -- ^ imageView
                                           -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                        -> IO ()
vkDestroyImageView
  = unsafeDupablePerformIO (vkGetProc @VkDestroyImageView)

{-# NOINLINE vkDestroyImageView #-}
##endif

-- |
-- > void vkDestroyImageView
-- >     ( VkDevice device
-- >     , VkImageView imageView
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyImageView vkDestroyImageView registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyImageView <- vkGetDeviceProc @VkDestroyImageView vkDevice
--
-- or less efficient:
--
-- > myDestroyImageView <- vkGetProc @VkDestroyImageView
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyImageView"
               vkDestroyImageViewSafe ::
               VkDevice -- ^ device
                        -> VkImageView -- ^ imageView
                                       -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()

##else
vkDestroyImageViewSafe ::
                       VkDevice -- ^ device
                                -> VkImageView -- ^ imageView
                                               -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                            -> IO ()
vkDestroyImageViewSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyImageView)

{-# NOINLINE vkDestroyImageViewSafe #-}
##endif

-- | > void vkDestroyImageView
--   >     ( VkDevice device
--   >     , VkImageView imageView
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyImageView vkDestroyImageView registry at www.khronos.org>
type HS_vkDestroyImageView =
     VkDevice -- ^ device
              -> VkImageView -- ^ imageView
                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          -> IO ()

type PFN_vkDestroyImageView = FunPtr HS_vkDestroyImageView

foreign import ccall unsafe "dynamic" unwrapVkDestroyImageView ::
               PFN_vkDestroyImageView -> HS_vkDestroyImageView

foreign import ccall safe "dynamic" unwrapVkDestroyImageViewSafe ::
               PFN_vkDestroyImageView -> HS_vkDestroyImageView

instance VulkanProc "vkDestroyImageView" where
        type VkProcType "vkDestroyImageView" = HS_vkDestroyImageView
        vkProcSymbol = _VkDestroyImageView

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyImageView

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyImageViewSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
-- > VkResult vkCreateShaderModule
-- >     ( VkDevice device
-- >     , const VkShaderModuleCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkShaderModule* pShaderModule
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateShaderModule vkCreateShaderModule registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateShaderModule <- vkGetDeviceProc @VkCreateShaderModule vkDevice
--
-- or less efficient:
--
-- > myCreateShaderModule <- vkGetProc @VkCreateShaderModule
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateShaderModule"
               vkCreateShaderModule ::
               VkDevice -- ^ device
                        ->
                 Ptr VkShaderModuleCreateInfo -- ^ pCreateInfo
                                              ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkShaderModule -- ^ pShaderModule
                                                                   -> IO VkResult

##else
vkCreateShaderModule ::
                     VkDevice -- ^ device
                              ->
                       Ptr VkShaderModuleCreateInfo -- ^ pCreateInfo
                                                    ->
                         Ptr VkAllocationCallbacks -- ^ pAllocator
                                                   -> Ptr VkShaderModule -- ^ pShaderModule
                                                                         -> IO VkResult
vkCreateShaderModule
  = unsafeDupablePerformIO (vkGetProc @VkCreateShaderModule)

{-# NOINLINE vkCreateShaderModule #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
-- > VkResult vkCreateShaderModule
-- >     ( VkDevice device
-- >     , const VkShaderModuleCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkShaderModule* pShaderModule
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateShaderModule vkCreateShaderModule registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateShaderModule <- vkGetDeviceProc @VkCreateShaderModule vkDevice
--
-- or less efficient:
--
-- > myCreateShaderModule <- vkGetProc @VkCreateShaderModule
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateShaderModule"
               vkCreateShaderModuleSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkShaderModuleCreateInfo -- ^ pCreateInfo
                                              ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkShaderModule -- ^ pShaderModule
                                                                   -> IO VkResult

##else
vkCreateShaderModuleSafe ::
                         VkDevice -- ^ device
                                  ->
                           Ptr VkShaderModuleCreateInfo -- ^ pCreateInfo
                                                        ->
                             Ptr VkAllocationCallbacks -- ^ pAllocator
                                                       -> Ptr VkShaderModule -- ^ pShaderModule
                                                                             -> IO VkResult
vkCreateShaderModuleSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateShaderModule)

{-# NOINLINE vkCreateShaderModuleSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateShaderModule vkCreateShaderModule registry at www.khronos.org>
type HS_vkCreateShaderModule =
     VkDevice -- ^ device
              ->
       Ptr VkShaderModuleCreateInfo -- ^ pCreateInfo
                                    ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkShaderModule -- ^ pShaderModule
                                                         -> IO VkResult

type PFN_vkCreateShaderModule = FunPtr HS_vkCreateShaderModule

foreign import ccall unsafe "dynamic" unwrapVkCreateShaderModule ::
               PFN_vkCreateShaderModule -> HS_vkCreateShaderModule

foreign import ccall safe "dynamic" unwrapVkCreateShaderModuleSafe
               :: PFN_vkCreateShaderModule -> HS_vkCreateShaderModule

instance VulkanProc "vkCreateShaderModule" where
        type VkProcType "vkCreateShaderModule" = HS_vkCreateShaderModule
        vkProcSymbol = _VkCreateShaderModule

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateShaderModule

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateShaderModuleSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyShaderModule
-- >     ( VkDevice device
-- >     , VkShaderModule shaderModule
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyShaderModule vkDestroyShaderModule registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyShaderModule <- vkGetDeviceProc @VkDestroyShaderModule vkDevice
--
-- or less efficient:
--
-- > myDestroyShaderModule <- vkGetProc @VkDestroyShaderModule
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyShaderModule"
               vkDestroyShaderModule ::
               VkDevice -- ^ device
                        -> VkShaderModule -- ^ shaderModule
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

##else
vkDestroyShaderModule ::
                      VkDevice -- ^ device
                               -> VkShaderModule -- ^ shaderModule
                                                 -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                              -> IO ()
vkDestroyShaderModule
  = unsafeDupablePerformIO (vkGetProc @VkDestroyShaderModule)

{-# NOINLINE vkDestroyShaderModule #-}
##endif

-- |
-- > void vkDestroyShaderModule
-- >     ( VkDevice device
-- >     , VkShaderModule shaderModule
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyShaderModule vkDestroyShaderModule registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyShaderModule <- vkGetDeviceProc @VkDestroyShaderModule vkDevice
--
-- or less efficient:
--
-- > myDestroyShaderModule <- vkGetProc @VkDestroyShaderModule
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyShaderModule"
               vkDestroyShaderModuleSafe ::
               VkDevice -- ^ device
                        -> VkShaderModule -- ^ shaderModule
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

##else
vkDestroyShaderModuleSafe ::
                          VkDevice -- ^ device
                                   -> VkShaderModule -- ^ shaderModule
                                                     -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                  -> IO ()
vkDestroyShaderModuleSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyShaderModule)

{-# NOINLINE vkDestroyShaderModuleSafe #-}
##endif

-- | > void vkDestroyShaderModule
--   >     ( VkDevice device
--   >     , VkShaderModule shaderModule
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyShaderModule vkDestroyShaderModule registry at www.khronos.org>
type HS_vkDestroyShaderModule =
     VkDevice -- ^ device
              -> VkShaderModule -- ^ shaderModule
                                -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                             -> IO ()

type PFN_vkDestroyShaderModule = FunPtr HS_vkDestroyShaderModule

foreign import ccall unsafe "dynamic" unwrapVkDestroyShaderModule
               :: PFN_vkDestroyShaderModule -> HS_vkDestroyShaderModule

foreign import ccall safe "dynamic" unwrapVkDestroyShaderModuleSafe
               :: PFN_vkDestroyShaderModule -> HS_vkDestroyShaderModule

instance VulkanProc "vkDestroyShaderModule" where
        type VkProcType "vkDestroyShaderModule" = HS_vkDestroyShaderModule
        vkProcSymbol = _VkDestroyShaderModule

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyShaderModule

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyShaderModuleSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreatePipelineCache
-- >     ( VkDevice device
-- >     , const VkPipelineCacheCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkPipelineCache* pPipelineCache
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreatePipelineCache vkCreatePipelineCache registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreatePipelineCache <- vkGetDeviceProc @VkCreatePipelineCache vkDevice
--
-- or less efficient:
--
-- > myCreatePipelineCache <- vkGetProc @VkCreatePipelineCache
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreatePipelineCache"
               vkCreatePipelineCache ::
               VkDevice -- ^ device
                        ->
                 Ptr VkPipelineCacheCreateInfo -- ^ pCreateInfo
                                               ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkPipelineCache -- ^ pPipelineCache
                                                                    -> IO VkResult

##else
vkCreatePipelineCache ::
                      VkDevice -- ^ device
                               ->
                        Ptr VkPipelineCacheCreateInfo -- ^ pCreateInfo
                                                      ->
                          Ptr VkAllocationCallbacks -- ^ pAllocator
                                                    -> Ptr VkPipelineCache -- ^ pPipelineCache
                                                                           -> IO VkResult
vkCreatePipelineCache
  = unsafeDupablePerformIO (vkGetProc @VkCreatePipelineCache)

{-# NOINLINE vkCreatePipelineCache #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreatePipelineCache
-- >     ( VkDevice device
-- >     , const VkPipelineCacheCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkPipelineCache* pPipelineCache
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreatePipelineCache vkCreatePipelineCache registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreatePipelineCache <- vkGetDeviceProc @VkCreatePipelineCache vkDevice
--
-- or less efficient:
--
-- > myCreatePipelineCache <- vkGetProc @VkCreatePipelineCache
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreatePipelineCache"
               vkCreatePipelineCacheSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkPipelineCacheCreateInfo -- ^ pCreateInfo
                                               ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkPipelineCache -- ^ pPipelineCache
                                                                    -> IO VkResult

##else
vkCreatePipelineCacheSafe ::
                          VkDevice -- ^ device
                                   ->
                            Ptr VkPipelineCacheCreateInfo -- ^ pCreateInfo
                                                          ->
                              Ptr VkAllocationCallbacks -- ^ pAllocator
                                                        -> Ptr VkPipelineCache -- ^ pPipelineCache
                                                                               -> IO VkResult
vkCreatePipelineCacheSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreatePipelineCache)

{-# NOINLINE vkCreatePipelineCacheSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreatePipelineCache vkCreatePipelineCache registry at www.khronos.org>
type HS_vkCreatePipelineCache =
     VkDevice -- ^ device
              ->
       Ptr VkPipelineCacheCreateInfo -- ^ pCreateInfo
                                     ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkPipelineCache -- ^ pPipelineCache
                                                          -> IO VkResult

type PFN_vkCreatePipelineCache = FunPtr HS_vkCreatePipelineCache

foreign import ccall unsafe "dynamic" unwrapVkCreatePipelineCache
               :: PFN_vkCreatePipelineCache -> HS_vkCreatePipelineCache

foreign import ccall safe "dynamic" unwrapVkCreatePipelineCacheSafe
               :: PFN_vkCreatePipelineCache -> HS_vkCreatePipelineCache

instance VulkanProc "vkCreatePipelineCache" where
        type VkProcType "vkCreatePipelineCache" = HS_vkCreatePipelineCache
        vkProcSymbol = _VkCreatePipelineCache

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreatePipelineCache

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreatePipelineCacheSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyPipelineCache
-- >     ( VkDevice device
-- >     , VkPipelineCache pipelineCache
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyPipelineCache vkDestroyPipelineCache registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyPipelineCache <- vkGetDeviceProc @VkDestroyPipelineCache vkDevice
--
-- or less efficient:
--
-- > myDestroyPipelineCache <- vkGetProc @VkDestroyPipelineCache
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyPipelineCache"
               vkDestroyPipelineCache ::
               VkDevice -- ^ device
                        -> VkPipelineCache -- ^ pipelineCache
                                           -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                        -> IO ()

##else
vkDestroyPipelineCache ::
                       VkDevice -- ^ device
                                -> VkPipelineCache -- ^ pipelineCache
                                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                -> IO ()
vkDestroyPipelineCache
  = unsafeDupablePerformIO (vkGetProc @VkDestroyPipelineCache)

{-# NOINLINE vkDestroyPipelineCache #-}
##endif

-- |
-- > void vkDestroyPipelineCache
-- >     ( VkDevice device
-- >     , VkPipelineCache pipelineCache
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyPipelineCache vkDestroyPipelineCache registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyPipelineCache <- vkGetDeviceProc @VkDestroyPipelineCache vkDevice
--
-- or less efficient:
--
-- > myDestroyPipelineCache <- vkGetProc @VkDestroyPipelineCache
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyPipelineCache"
               vkDestroyPipelineCacheSafe ::
               VkDevice -- ^ device
                        -> VkPipelineCache -- ^ pipelineCache
                                           -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                        -> IO ()

##else
vkDestroyPipelineCacheSafe ::
                           VkDevice -- ^ device
                                    -> VkPipelineCache -- ^ pipelineCache
                                                       -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                    -> IO ()
vkDestroyPipelineCacheSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyPipelineCache)

{-# NOINLINE vkDestroyPipelineCacheSafe #-}
##endif

-- | > void vkDestroyPipelineCache
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyPipelineCache vkDestroyPipelineCache registry at www.khronos.org>
type HS_vkDestroyPipelineCache =
     VkDevice -- ^ device
              -> VkPipelineCache -- ^ pipelineCache
                                 -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                              -> IO ()

type PFN_vkDestroyPipelineCache = FunPtr HS_vkDestroyPipelineCache

foreign import ccall unsafe "dynamic" unwrapVkDestroyPipelineCache
               :: PFN_vkDestroyPipelineCache -> HS_vkDestroyPipelineCache

foreign import ccall safe "dynamic"
               unwrapVkDestroyPipelineCacheSafe ::
               PFN_vkDestroyPipelineCache -> HS_vkDestroyPipelineCache

instance VulkanProc "vkDestroyPipelineCache" where
        type VkProcType "vkDestroyPipelineCache" =
             HS_vkDestroyPipelineCache
        vkProcSymbol = _VkDestroyPipelineCache

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyPipelineCache

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyPipelineCacheSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkGetPipelineCacheData
-- >     ( VkDevice device
-- >     , VkPipelineCache pipelineCache
-- >     , size_t* pDataSize
-- >     , void* pData
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPipelineCacheData vkGetPipelineCacheData registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPipelineCacheData <- vkGetDeviceProc @VkGetPipelineCacheData vkDevice
--
-- or less efficient:
--
-- > myGetPipelineCacheData <- vkGetProc @VkGetPipelineCacheData
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetPipelineCacheData"
               vkGetPipelineCacheData ::
               VkDevice -- ^ device
                        -> VkPipelineCache -- ^ pipelineCache
                                           -> Ptr CSize -- ^ pDataSize
                                                        -> Ptr Void -- ^ pData
                                                                    -> IO VkResult

##else
vkGetPipelineCacheData ::
                       VkDevice -- ^ device
                                -> VkPipelineCache -- ^ pipelineCache
                                                   -> Ptr CSize -- ^ pDataSize
                                                                -> Ptr Void -- ^ pData
                                                                            -> IO VkResult
vkGetPipelineCacheData
  = unsafeDupablePerformIO (vkGetProc @VkGetPipelineCacheData)

{-# NOINLINE vkGetPipelineCacheData #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkGetPipelineCacheData
-- >     ( VkDevice device
-- >     , VkPipelineCache pipelineCache
-- >     , size_t* pDataSize
-- >     , void* pData
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPipelineCacheData vkGetPipelineCacheData registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPipelineCacheData <- vkGetDeviceProc @VkGetPipelineCacheData vkDevice
--
-- or less efficient:
--
-- > myGetPipelineCacheData <- vkGetProc @VkGetPipelineCacheData
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetPipelineCacheData"
               vkGetPipelineCacheDataSafe ::
               VkDevice -- ^ device
                        -> VkPipelineCache -- ^ pipelineCache
                                           -> Ptr CSize -- ^ pDataSize
                                                        -> Ptr Void -- ^ pData
                                                                    -> IO VkResult

##else
vkGetPipelineCacheDataSafe ::
                           VkDevice -- ^ device
                                    -> VkPipelineCache -- ^ pipelineCache
                                                       -> Ptr CSize -- ^ pDataSize
                                                                    -> Ptr Void -- ^ pData
                                                                                -> IO VkResult
vkGetPipelineCacheDataSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkGetPipelineCacheData)

{-# NOINLINE vkGetPipelineCacheDataSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPipelineCacheData vkGetPipelineCacheData registry at www.khronos.org>
type HS_vkGetPipelineCacheData =
     VkDevice -- ^ device
              -> VkPipelineCache -- ^ pipelineCache
                                 -> Ptr CSize -- ^ pDataSize
                                              -> Ptr Void -- ^ pData
                                                          -> IO VkResult

type PFN_vkGetPipelineCacheData = FunPtr HS_vkGetPipelineCacheData

foreign import ccall unsafe "dynamic" unwrapVkGetPipelineCacheData
               :: PFN_vkGetPipelineCacheData -> HS_vkGetPipelineCacheData

foreign import ccall safe "dynamic"
               unwrapVkGetPipelineCacheDataSafe ::
               PFN_vkGetPipelineCacheData -> HS_vkGetPipelineCacheData

instance VulkanProc "vkGetPipelineCacheData" where
        type VkProcType "vkGetPipelineCacheData" =
             HS_vkGetPipelineCacheData
        vkProcSymbol = _VkGetPipelineCacheData

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPipelineCacheData

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetPipelineCacheDataSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkMergePipelineCaches
-- >     ( VkDevice device
-- >     , VkPipelineCache dstCache
-- >     , uint32_t srcCacheCount
-- >     , const VkPipelineCache* pSrcCaches
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkMergePipelineCaches vkMergePipelineCaches registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myMergePipelineCaches <- vkGetDeviceProc @VkMergePipelineCaches vkDevice
--
-- or less efficient:
--
-- > myMergePipelineCaches <- vkGetProc @VkMergePipelineCaches
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkMergePipelineCaches"
               vkMergePipelineCaches ::
               VkDevice -- ^ device
                        ->
                 VkPipelineCache -- ^ dstCache
                                 -> Word32 -- ^ srcCacheCount
                                           -> Ptr VkPipelineCache -- ^ pSrcCaches
                                                                  -> IO VkResult

##else
vkMergePipelineCaches ::
                      VkDevice -- ^ device
                               ->
                        VkPipelineCache -- ^ dstCache
                                        -> Word32 -- ^ srcCacheCount
                                                  -> Ptr VkPipelineCache -- ^ pSrcCaches
                                                                         -> IO VkResult
vkMergePipelineCaches
  = unsafeDupablePerformIO (vkGetProc @VkMergePipelineCaches)

{-# NOINLINE vkMergePipelineCaches #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkMergePipelineCaches
-- >     ( VkDevice device
-- >     , VkPipelineCache dstCache
-- >     , uint32_t srcCacheCount
-- >     , const VkPipelineCache* pSrcCaches
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkMergePipelineCaches vkMergePipelineCaches registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myMergePipelineCaches <- vkGetDeviceProc @VkMergePipelineCaches vkDevice
--
-- or less efficient:
--
-- > myMergePipelineCaches <- vkGetProc @VkMergePipelineCaches
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkMergePipelineCaches"
               vkMergePipelineCachesSafe ::
               VkDevice -- ^ device
                        ->
                 VkPipelineCache -- ^ dstCache
                                 -> Word32 -- ^ srcCacheCount
                                           -> Ptr VkPipelineCache -- ^ pSrcCaches
                                                                  -> IO VkResult

##else
vkMergePipelineCachesSafe ::
                          VkDevice -- ^ device
                                   ->
                            VkPipelineCache -- ^ dstCache
                                            -> Word32 -- ^ srcCacheCount
                                                      -> Ptr VkPipelineCache -- ^ pSrcCaches
                                                                             -> IO VkResult
vkMergePipelineCachesSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkMergePipelineCaches)

{-# NOINLINE vkMergePipelineCachesSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkMergePipelineCaches vkMergePipelineCaches registry at www.khronos.org>
type HS_vkMergePipelineCaches =
     VkDevice -- ^ device
              ->
       VkPipelineCache -- ^ dstCache
                       -> Word32 -- ^ srcCacheCount
                                 -> Ptr VkPipelineCache -- ^ pSrcCaches
                                                        -> IO VkResult

type PFN_vkMergePipelineCaches = FunPtr HS_vkMergePipelineCaches

foreign import ccall unsafe "dynamic" unwrapVkMergePipelineCaches
               :: PFN_vkMergePipelineCaches -> HS_vkMergePipelineCaches

foreign import ccall safe "dynamic" unwrapVkMergePipelineCachesSafe
               :: PFN_vkMergePipelineCaches -> HS_vkMergePipelineCaches

instance VulkanProc "vkMergePipelineCaches" where
        type VkProcType "vkMergePipelineCaches" = HS_vkMergePipelineCaches
        vkProcSymbol = _VkMergePipelineCaches

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkMergePipelineCaches

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkMergePipelineCachesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
-- > VkResult vkCreateGraphicsPipelines
-- >     ( VkDevice device
-- >     , VkPipelineCache pipelineCache
-- >     , uint32_t createInfoCount
-- >     , const VkGraphicsPipelineCreateInfo* pCreateInfos
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkPipeline* pPipelines
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateGraphicsPipelines vkCreateGraphicsPipelines registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateGraphicsPipelines <- vkGetDeviceProc @VkCreateGraphicsPipelines vkDevice
--
-- or less efficient:
--
-- > myCreateGraphicsPipelines <- vkGetProc @VkCreateGraphicsPipelines
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCreateGraphicsPipelines
  = unsafeDupablePerformIO (vkGetProc @VkCreateGraphicsPipelines)

{-# NOINLINE vkCreateGraphicsPipelines #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
-- > VkResult vkCreateGraphicsPipelines
-- >     ( VkDevice device
-- >     , VkPipelineCache pipelineCache
-- >     , uint32_t createInfoCount
-- >     , const VkGraphicsPipelineCreateInfo* pCreateInfos
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkPipeline* pPipelines
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateGraphicsPipelines vkCreateGraphicsPipelines registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateGraphicsPipelines <- vkGetDeviceProc @VkCreateGraphicsPipelines vkDevice
--
-- or less efficient:
--
-- > myCreateGraphicsPipelines <- vkGetProc @VkCreateGraphicsPipelines
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCreateGraphicsPipelinesSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateGraphicsPipelines)

{-# NOINLINE vkCreateGraphicsPipelinesSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateGraphicsPipelines vkCreateGraphicsPipelines registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic"
               unwrapVkCreateGraphicsPipelines ::
               PFN_vkCreateGraphicsPipelines -> HS_vkCreateGraphicsPipelines

foreign import ccall safe "dynamic"
               unwrapVkCreateGraphicsPipelinesSafe ::
               PFN_vkCreateGraphicsPipelines -> HS_vkCreateGraphicsPipelines

instance VulkanProc "vkCreateGraphicsPipelines" where
        type VkProcType "vkCreateGraphicsPipelines" =
             HS_vkCreateGraphicsPipelines
        vkProcSymbol = _VkCreateGraphicsPipelines

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateGraphicsPipelines

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateGraphicsPipelinesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
-- > VkResult vkCreateComputePipelines
-- >     ( VkDevice device
-- >     , VkPipelineCache pipelineCache
-- >     , uint32_t createInfoCount
-- >     , const VkComputePipelineCreateInfo* pCreateInfos
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkPipeline* pPipelines
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateComputePipelines vkCreateComputePipelines registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateComputePipelines <- vkGetDeviceProc @VkCreateComputePipelines vkDevice
--
-- or less efficient:
--
-- > myCreateComputePipelines <- vkGetProc @VkCreateComputePipelines
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCreateComputePipelines
  = unsafeDupablePerformIO (vkGetProc @VkCreateComputePipelines)

{-# NOINLINE vkCreateComputePipelines #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
-- > VkResult vkCreateComputePipelines
-- >     ( VkDevice device
-- >     , VkPipelineCache pipelineCache
-- >     , uint32_t createInfoCount
-- >     , const VkComputePipelineCreateInfo* pCreateInfos
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkPipeline* pPipelines
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateComputePipelines vkCreateComputePipelines registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateComputePipelines <- vkGetDeviceProc @VkCreateComputePipelines vkDevice
--
-- or less efficient:
--
-- > myCreateComputePipelines <- vkGetProc @VkCreateComputePipelines
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCreateComputePipelinesSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateComputePipelines)

{-# NOINLINE vkCreateComputePipelinesSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateComputePipelines vkCreateComputePipelines registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic"
               unwrapVkCreateComputePipelines ::
               PFN_vkCreateComputePipelines -> HS_vkCreateComputePipelines

foreign import ccall safe "dynamic"
               unwrapVkCreateComputePipelinesSafe ::
               PFN_vkCreateComputePipelines -> HS_vkCreateComputePipelines

instance VulkanProc "vkCreateComputePipelines" where
        type VkProcType "vkCreateComputePipelines" =
             HS_vkCreateComputePipelines
        vkProcSymbol = _VkCreateComputePipelines

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateComputePipelines

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateComputePipelinesSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyPipeline
-- >     ( VkDevice device
-- >     , VkPipeline pipeline
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyPipeline vkDestroyPipeline registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyPipeline <- vkGetDeviceProc @VkDestroyPipeline vkDevice
--
-- or less efficient:
--
-- > myDestroyPipeline <- vkGetProc @VkDestroyPipeline
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyPipeline" vkDestroyPipeline
               :: VkDevice -- ^ device
                           -> VkPipeline -- ^ pipeline
                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()

##else
vkDestroyPipeline ::
                  VkDevice -- ^ device
                           -> VkPipeline -- ^ pipeline
                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()
vkDestroyPipeline
  = unsafeDupablePerformIO (vkGetProc @VkDestroyPipeline)

{-# NOINLINE vkDestroyPipeline #-}
##endif

-- |
-- > void vkDestroyPipeline
-- >     ( VkDevice device
-- >     , VkPipeline pipeline
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyPipeline vkDestroyPipeline registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyPipeline <- vkGetDeviceProc @VkDestroyPipeline vkDevice
--
-- or less efficient:
--
-- > myDestroyPipeline <- vkGetProc @VkDestroyPipeline
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyPipeline" vkDestroyPipelineSafe
               :: VkDevice -- ^ device
                           -> VkPipeline -- ^ pipeline
                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()

##else
vkDestroyPipelineSafe ::
                      VkDevice -- ^ device
                               -> VkPipeline -- ^ pipeline
                                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                          -> IO ()
vkDestroyPipelineSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyPipeline)

{-# NOINLINE vkDestroyPipelineSafe #-}
##endif

-- | > void vkDestroyPipeline
--   >     ( VkDevice device
--   >     , VkPipeline pipeline
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyPipeline vkDestroyPipeline registry at www.khronos.org>
type HS_vkDestroyPipeline =
     VkDevice -- ^ device
              -> VkPipeline -- ^ pipeline
                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         -> IO ()

type PFN_vkDestroyPipeline = FunPtr HS_vkDestroyPipeline

foreign import ccall unsafe "dynamic" unwrapVkDestroyPipeline ::
               PFN_vkDestroyPipeline -> HS_vkDestroyPipeline

foreign import ccall safe "dynamic" unwrapVkDestroyPipelineSafe ::
               PFN_vkDestroyPipeline -> HS_vkDestroyPipeline

instance VulkanProc "vkDestroyPipeline" where
        type VkProcType "vkDestroyPipeline" = HS_vkDestroyPipeline
        vkProcSymbol = _VkDestroyPipeline

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyPipeline

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyPipelineSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreatePipelineLayout
-- >     ( VkDevice device
-- >     , const VkPipelineLayoutCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkPipelineLayout* pPipelineLayout
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreatePipelineLayout vkCreatePipelineLayout registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreatePipelineLayout <- vkGetDeviceProc @VkCreatePipelineLayout vkDevice
--
-- or less efficient:
--
-- > myCreatePipelineLayout <- vkGetProc @VkCreatePipelineLayout
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreatePipelineLayout"
               vkCreatePipelineLayout ::
               VkDevice -- ^ device
                        ->
                 Ptr VkPipelineLayoutCreateInfo -- ^ pCreateInfo
                                                ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkPipelineLayout -- ^ pPipelineLayout
                                                                     -> IO VkResult

##else
vkCreatePipelineLayout ::
                       VkDevice -- ^ device
                                ->
                         Ptr VkPipelineLayoutCreateInfo -- ^ pCreateInfo
                                                        ->
                           Ptr VkAllocationCallbacks -- ^ pAllocator
                                                     -> Ptr VkPipelineLayout -- ^ pPipelineLayout
                                                                             -> IO VkResult
vkCreatePipelineLayout
  = unsafeDupablePerformIO (vkGetProc @VkCreatePipelineLayout)

{-# NOINLINE vkCreatePipelineLayout #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreatePipelineLayout
-- >     ( VkDevice device
-- >     , const VkPipelineLayoutCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkPipelineLayout* pPipelineLayout
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreatePipelineLayout vkCreatePipelineLayout registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreatePipelineLayout <- vkGetDeviceProc @VkCreatePipelineLayout vkDevice
--
-- or less efficient:
--
-- > myCreatePipelineLayout <- vkGetProc @VkCreatePipelineLayout
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreatePipelineLayout"
               vkCreatePipelineLayoutSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkPipelineLayoutCreateInfo -- ^ pCreateInfo
                                                ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkPipelineLayout -- ^ pPipelineLayout
                                                                     -> IO VkResult

##else
vkCreatePipelineLayoutSafe ::
                           VkDevice -- ^ device
                                    ->
                             Ptr VkPipelineLayoutCreateInfo -- ^ pCreateInfo
                                                            ->
                               Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         -> Ptr VkPipelineLayout -- ^ pPipelineLayout
                                                                                 -> IO VkResult
vkCreatePipelineLayoutSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreatePipelineLayout)

{-# NOINLINE vkCreatePipelineLayoutSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreatePipelineLayout vkCreatePipelineLayout registry at www.khronos.org>
type HS_vkCreatePipelineLayout =
     VkDevice -- ^ device
              ->
       Ptr VkPipelineLayoutCreateInfo -- ^ pCreateInfo
                                      ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkPipelineLayout -- ^ pPipelineLayout
                                                           -> IO VkResult

type PFN_vkCreatePipelineLayout = FunPtr HS_vkCreatePipelineLayout

foreign import ccall unsafe "dynamic" unwrapVkCreatePipelineLayout
               :: PFN_vkCreatePipelineLayout -> HS_vkCreatePipelineLayout

foreign import ccall safe "dynamic"
               unwrapVkCreatePipelineLayoutSafe ::
               PFN_vkCreatePipelineLayout -> HS_vkCreatePipelineLayout

instance VulkanProc "vkCreatePipelineLayout" where
        type VkProcType "vkCreatePipelineLayout" =
             HS_vkCreatePipelineLayout
        vkProcSymbol = _VkCreatePipelineLayout

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreatePipelineLayout

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreatePipelineLayoutSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyPipelineLayout
-- >     ( VkDevice device
-- >     , VkPipelineLayout pipelineLayout
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyPipelineLayout vkDestroyPipelineLayout registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyPipelineLayout <- vkGetDeviceProc @VkDestroyPipelineLayout vkDevice
--
-- or less efficient:
--
-- > myDestroyPipelineLayout <- vkGetProc @VkDestroyPipelineLayout
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyPipelineLayout"
               vkDestroyPipelineLayout ::
               VkDevice -- ^ device
                        -> VkPipelineLayout -- ^ pipelineLayout
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

##else
vkDestroyPipelineLayout ::
                        VkDevice -- ^ device
                                 -> VkPipelineLayout -- ^ pipelineLayout
                                                     -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                  -> IO ()
vkDestroyPipelineLayout
  = unsafeDupablePerformIO (vkGetProc @VkDestroyPipelineLayout)

{-# NOINLINE vkDestroyPipelineLayout #-}
##endif

-- |
-- > void vkDestroyPipelineLayout
-- >     ( VkDevice device
-- >     , VkPipelineLayout pipelineLayout
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyPipelineLayout vkDestroyPipelineLayout registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyPipelineLayout <- vkGetDeviceProc @VkDestroyPipelineLayout vkDevice
--
-- or less efficient:
--
-- > myDestroyPipelineLayout <- vkGetProc @VkDestroyPipelineLayout
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyPipelineLayout"
               vkDestroyPipelineLayoutSafe ::
               VkDevice -- ^ device
                        -> VkPipelineLayout -- ^ pipelineLayout
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

##else
vkDestroyPipelineLayoutSafe ::
                            VkDevice -- ^ device
                                     -> VkPipelineLayout -- ^ pipelineLayout
                                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                      -> IO ()
vkDestroyPipelineLayoutSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyPipelineLayout)

{-# NOINLINE vkDestroyPipelineLayoutSafe #-}
##endif

-- | > void vkDestroyPipelineLayout
--   >     ( VkDevice device
--   >     , VkPipelineLayout pipelineLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyPipelineLayout vkDestroyPipelineLayout registry at www.khronos.org>
type HS_vkDestroyPipelineLayout =
     VkDevice -- ^ device
              -> VkPipelineLayout -- ^ pipelineLayout
                                  -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                               -> IO ()

type PFN_vkDestroyPipelineLayout =
     FunPtr HS_vkDestroyPipelineLayout

foreign import ccall unsafe "dynamic" unwrapVkDestroyPipelineLayout
               :: PFN_vkDestroyPipelineLayout -> HS_vkDestroyPipelineLayout

foreign import ccall safe "dynamic"
               unwrapVkDestroyPipelineLayoutSafe ::
               PFN_vkDestroyPipelineLayout -> HS_vkDestroyPipelineLayout

instance VulkanProc "vkDestroyPipelineLayout" where
        type VkProcType "vkDestroyPipelineLayout" =
             HS_vkDestroyPipelineLayout
        vkProcSymbol = _VkDestroyPipelineLayout

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyPipelineLayout

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyPipelineLayoutSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_TOO_MANY_OBJECTS'.
--
-- > VkResult vkCreateSampler
-- >     ( VkDevice device
-- >     , const VkSamplerCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSampler* pSampler
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSampler vkCreateSampler registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSampler <- vkGetDeviceProc @VkCreateSampler vkDevice
--
-- or less efficient:
--
-- > myCreateSampler <- vkGetProc @VkCreateSampler
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateSampler" vkCreateSampler ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSamplerCreateInfo -- ^ pCreateInfo
                                         ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSampler -- ^ pSampler
                                                              -> IO VkResult

##else
vkCreateSampler ::
                VkDevice -- ^ device
                         ->
                  Ptr VkSamplerCreateInfo -- ^ pCreateInfo
                                          ->
                    Ptr VkAllocationCallbacks -- ^ pAllocator
                                              -> Ptr VkSampler -- ^ pSampler
                                                               -> IO VkResult
vkCreateSampler
  = unsafeDupablePerformIO (vkGetProc @VkCreateSampler)

{-# NOINLINE vkCreateSampler #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_TOO_MANY_OBJECTS'.
--
-- > VkResult vkCreateSampler
-- >     ( VkDevice device
-- >     , const VkSamplerCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSampler* pSampler
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSampler vkCreateSampler registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSampler <- vkGetDeviceProc @VkCreateSampler vkDevice
--
-- or less efficient:
--
-- > myCreateSampler <- vkGetProc @VkCreateSampler
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateSampler" vkCreateSamplerSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSamplerCreateInfo -- ^ pCreateInfo
                                         ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSampler -- ^ pSampler
                                                              -> IO VkResult

##else
vkCreateSamplerSafe ::
                    VkDevice -- ^ device
                             ->
                      Ptr VkSamplerCreateInfo -- ^ pCreateInfo
                                              ->
                        Ptr VkAllocationCallbacks -- ^ pAllocator
                                                  -> Ptr VkSampler -- ^ pSampler
                                                                   -> IO VkResult
vkCreateSamplerSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateSampler)

{-# NOINLINE vkCreateSamplerSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSampler vkCreateSampler registry at www.khronos.org>
type HS_vkCreateSampler =
     VkDevice -- ^ device
              ->
       Ptr VkSamplerCreateInfo -- ^ pCreateInfo
                               ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSampler -- ^ pSampler
                                                    -> IO VkResult

type PFN_vkCreateSampler = FunPtr HS_vkCreateSampler

foreign import ccall unsafe "dynamic" unwrapVkCreateSampler ::
               PFN_vkCreateSampler -> HS_vkCreateSampler

foreign import ccall safe "dynamic" unwrapVkCreateSamplerSafe ::
               PFN_vkCreateSampler -> HS_vkCreateSampler

instance VulkanProc "vkCreateSampler" where
        type VkProcType "vkCreateSampler" = HS_vkCreateSampler
        vkProcSymbol = _VkCreateSampler

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateSampler

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateSamplerSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroySampler
-- >     ( VkDevice device
-- >     , VkSampler sampler
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySampler vkDestroySampler registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroySampler <- vkGetDeviceProc @VkDestroySampler vkDevice
--
-- or less efficient:
--
-- > myDestroySampler <- vkGetProc @VkDestroySampler
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroySampler" vkDestroySampler ::
               VkDevice -- ^ device
                        -> VkSampler -- ^ sampler
                                     -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                  -> IO ()

##else
vkDestroySampler ::
                 VkDevice -- ^ device
                          -> VkSampler -- ^ sampler
                                       -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()
vkDestroySampler
  = unsafeDupablePerformIO (vkGetProc @VkDestroySampler)

{-# NOINLINE vkDestroySampler #-}
##endif

-- |
-- > void vkDestroySampler
-- >     ( VkDevice device
-- >     , VkSampler sampler
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySampler vkDestroySampler registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroySampler <- vkGetDeviceProc @VkDestroySampler vkDevice
--
-- or less efficient:
--
-- > myDestroySampler <- vkGetProc @VkDestroySampler
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroySampler" vkDestroySamplerSafe
               :: VkDevice -- ^ device
                           -> VkSampler -- ^ sampler
                                        -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                     -> IO ()

##else
vkDestroySamplerSafe ::
                     VkDevice -- ^ device
                              -> VkSampler -- ^ sampler
                                           -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                        -> IO ()
vkDestroySamplerSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroySampler)

{-# NOINLINE vkDestroySamplerSafe #-}
##endif

-- | > void vkDestroySampler
--   >     ( VkDevice device
--   >     , VkSampler sampler
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySampler vkDestroySampler registry at www.khronos.org>
type HS_vkDestroySampler =
     VkDevice -- ^ device
              -> VkSampler -- ^ sampler
                           -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                        -> IO ()

type PFN_vkDestroySampler = FunPtr HS_vkDestroySampler

foreign import ccall unsafe "dynamic" unwrapVkDestroySampler ::
               PFN_vkDestroySampler -> HS_vkDestroySampler

foreign import ccall safe "dynamic" unwrapVkDestroySamplerSafe ::
               PFN_vkDestroySampler -> HS_vkDestroySampler

instance VulkanProc "vkDestroySampler" where
        type VkProcType "vkDestroySampler" = HS_vkDestroySampler
        vkProcSymbol = _VkDestroySampler

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroySampler

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroySamplerSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateDescriptorSetLayout
-- >     ( VkDevice device
-- >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkDescriptorSetLayout* pSetLayout
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDescriptorSetLayout vkCreateDescriptorSetLayout registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateDescriptorSetLayout <- vkGetDeviceProc @VkCreateDescriptorSetLayout vkDevice
--
-- or less efficient:
--
-- > myCreateDescriptorSetLayout <- vkGetProc @VkCreateDescriptorSetLayout
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
vkCreateDescriptorSetLayout ::
                            VkDevice -- ^ device
                                     ->
                              Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                                  ->
                                Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          ->
                                  Ptr VkDescriptorSetLayout -- ^ pSetLayout
                                                            -> IO VkResult
vkCreateDescriptorSetLayout
  = unsafeDupablePerformIO (vkGetProc @VkCreateDescriptorSetLayout)

{-# NOINLINE vkCreateDescriptorSetLayout #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateDescriptorSetLayout
-- >     ( VkDevice device
-- >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkDescriptorSetLayout* pSetLayout
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDescriptorSetLayout vkCreateDescriptorSetLayout registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateDescriptorSetLayout <- vkGetDeviceProc @VkCreateDescriptorSetLayout vkDevice
--
-- or less efficient:
--
-- > myCreateDescriptorSetLayout <- vkGetProc @VkCreateDescriptorSetLayout
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
vkCreateDescriptorSetLayoutSafe ::
                                VkDevice -- ^ device
                                         ->
                                  Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                                      ->
                                    Ptr VkAllocationCallbacks -- ^ pAllocator
                                                              ->
                                      Ptr VkDescriptorSetLayout -- ^ pSetLayout
                                                                -> IO VkResult
vkCreateDescriptorSetLayoutSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkCreateDescriptorSetLayout)

{-# NOINLINE vkCreateDescriptorSetLayoutSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDescriptorSetLayout vkCreateDescriptorSetLayout registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic"
               unwrapVkCreateDescriptorSetLayout ::
               PFN_vkCreateDescriptorSetLayout -> HS_vkCreateDescriptorSetLayout

foreign import ccall safe "dynamic"
               unwrapVkCreateDescriptorSetLayoutSafe ::
               PFN_vkCreateDescriptorSetLayout -> HS_vkCreateDescriptorSetLayout

instance VulkanProc "vkCreateDescriptorSetLayout" where
        type VkProcType "vkCreateDescriptorSetLayout" =
             HS_vkCreateDescriptorSetLayout
        vkProcSymbol = _VkCreateDescriptorSetLayout

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateDescriptorSetLayout

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateDescriptorSetLayoutSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyDescriptorSetLayout
-- >     ( VkDevice device
-- >     , VkDescriptorSetLayout descriptorSetLayout
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDescriptorSetLayout vkDestroyDescriptorSetLayout registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyDescriptorSetLayout <- vkGetDeviceProc @VkDestroyDescriptorSetLayout vkDevice
--
-- or less efficient:
--
-- > myDestroyDescriptorSetLayout <- vkGetProc @VkDestroyDescriptorSetLayout
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyDescriptorSetLayout"
               vkDestroyDescriptorSetLayout ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorSetLayout -- ^ descriptorSetLayout
                                       -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()

##else
vkDestroyDescriptorSetLayout ::
                             VkDevice -- ^ device
                                      ->
                               VkDescriptorSetLayout -- ^ descriptorSetLayout
                                                     -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                  -> IO ()
vkDestroyDescriptorSetLayout
  = unsafeDupablePerformIO (vkGetProc @VkDestroyDescriptorSetLayout)

{-# NOINLINE vkDestroyDescriptorSetLayout #-}
##endif

-- |
-- > void vkDestroyDescriptorSetLayout
-- >     ( VkDevice device
-- >     , VkDescriptorSetLayout descriptorSetLayout
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDescriptorSetLayout vkDestroyDescriptorSetLayout registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyDescriptorSetLayout <- vkGetDeviceProc @VkDestroyDescriptorSetLayout vkDevice
--
-- or less efficient:
--
-- > myDestroyDescriptorSetLayout <- vkGetProc @VkDestroyDescriptorSetLayout
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyDescriptorSetLayout"
               vkDestroyDescriptorSetLayoutSafe ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorSetLayout -- ^ descriptorSetLayout
                                       -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()

##else
vkDestroyDescriptorSetLayoutSafe ::
                                 VkDevice -- ^ device
                                          ->
                                   VkDescriptorSetLayout -- ^ descriptorSetLayout
                                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                      -> IO ()
vkDestroyDescriptorSetLayoutSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkDestroyDescriptorSetLayout)

{-# NOINLINE vkDestroyDescriptorSetLayoutSafe #-}
##endif

-- | > void vkDestroyDescriptorSetLayout
--   >     ( VkDevice device
--   >     , VkDescriptorSetLayout descriptorSetLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDescriptorSetLayout vkDestroyDescriptorSetLayout registry at www.khronos.org>
type HS_vkDestroyDescriptorSetLayout =
     VkDevice -- ^ device
              ->
       VkDescriptorSetLayout -- ^ descriptorSetLayout
                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          -> IO ()

type PFN_vkDestroyDescriptorSetLayout =
     FunPtr HS_vkDestroyDescriptorSetLayout

foreign import ccall unsafe "dynamic"
               unwrapVkDestroyDescriptorSetLayout ::
               PFN_vkDestroyDescriptorSetLayout -> HS_vkDestroyDescriptorSetLayout

foreign import ccall safe "dynamic"
               unwrapVkDestroyDescriptorSetLayoutSafe ::
               PFN_vkDestroyDescriptorSetLayout -> HS_vkDestroyDescriptorSetLayout

instance VulkanProc "vkDestroyDescriptorSetLayout" where
        type VkProcType "vkDestroyDescriptorSetLayout" =
             HS_vkDestroyDescriptorSetLayout
        vkProcSymbol = _VkDestroyDescriptorSetLayout

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyDescriptorSetLayout

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyDescriptorSetLayoutSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FRAGMENTATION_EXT'.
--
-- > VkResult vkCreateDescriptorPool
-- >     ( VkDevice device
-- >     , const VkDescriptorPoolCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkDescriptorPool* pDescriptorPool
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDescriptorPool vkCreateDescriptorPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateDescriptorPool <- vkGetDeviceProc @VkCreateDescriptorPool vkDevice
--
-- or less efficient:
--
-- > myCreateDescriptorPool <- vkGetProc @VkCreateDescriptorPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateDescriptorPool"
               vkCreateDescriptorPool ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorPoolCreateInfo -- ^ pCreateInfo
                                                ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkDescriptorPool -- ^ pDescriptorPool
                                                                     -> IO VkResult

##else
vkCreateDescriptorPool ::
                       VkDevice -- ^ device
                                ->
                         Ptr VkDescriptorPoolCreateInfo -- ^ pCreateInfo
                                                        ->
                           Ptr VkAllocationCallbacks -- ^ pAllocator
                                                     -> Ptr VkDescriptorPool -- ^ pDescriptorPool
                                                                             -> IO VkResult
vkCreateDescriptorPool
  = unsafeDupablePerformIO (vkGetProc @VkCreateDescriptorPool)

{-# NOINLINE vkCreateDescriptorPool #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FRAGMENTATION_EXT'.
--
-- > VkResult vkCreateDescriptorPool
-- >     ( VkDevice device
-- >     , const VkDescriptorPoolCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkDescriptorPool* pDescriptorPool
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDescriptorPool vkCreateDescriptorPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateDescriptorPool <- vkGetDeviceProc @VkCreateDescriptorPool vkDevice
--
-- or less efficient:
--
-- > myCreateDescriptorPool <- vkGetProc @VkCreateDescriptorPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateDescriptorPool"
               vkCreateDescriptorPoolSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorPoolCreateInfo -- ^ pCreateInfo
                                                ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkDescriptorPool -- ^ pDescriptorPool
                                                                     -> IO VkResult

##else
vkCreateDescriptorPoolSafe ::
                           VkDevice -- ^ device
                                    ->
                             Ptr VkDescriptorPoolCreateInfo -- ^ pCreateInfo
                                                            ->
                               Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         -> Ptr VkDescriptorPool -- ^ pDescriptorPool
                                                                                 -> IO VkResult
vkCreateDescriptorPoolSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateDescriptorPool)

{-# NOINLINE vkCreateDescriptorPoolSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDescriptorPool vkCreateDescriptorPool registry at www.khronos.org>
type HS_vkCreateDescriptorPool =
     VkDevice -- ^ device
              ->
       Ptr VkDescriptorPoolCreateInfo -- ^ pCreateInfo
                                      ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkDescriptorPool -- ^ pDescriptorPool
                                                           -> IO VkResult

type PFN_vkCreateDescriptorPool = FunPtr HS_vkCreateDescriptorPool

foreign import ccall unsafe "dynamic" unwrapVkCreateDescriptorPool
               :: PFN_vkCreateDescriptorPool -> HS_vkCreateDescriptorPool

foreign import ccall safe "dynamic"
               unwrapVkCreateDescriptorPoolSafe ::
               PFN_vkCreateDescriptorPool -> HS_vkCreateDescriptorPool

instance VulkanProc "vkCreateDescriptorPool" where
        type VkProcType "vkCreateDescriptorPool" =
             HS_vkCreateDescriptorPool
        vkProcSymbol = _VkCreateDescriptorPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateDescriptorPool

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateDescriptorPoolSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyDescriptorPool
-- >     ( VkDevice device
-- >     , VkDescriptorPool descriptorPool
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDescriptorPool vkDestroyDescriptorPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyDescriptorPool <- vkGetDeviceProc @VkDestroyDescriptorPool vkDevice
--
-- or less efficient:
--
-- > myDestroyDescriptorPool <- vkGetProc @VkDestroyDescriptorPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyDescriptorPool"
               vkDestroyDescriptorPool ::
               VkDevice -- ^ device
                        -> VkDescriptorPool -- ^ descriptorPool
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

##else
vkDestroyDescriptorPool ::
                        VkDevice -- ^ device
                                 -> VkDescriptorPool -- ^ descriptorPool
                                                     -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                  -> IO ()
vkDestroyDescriptorPool
  = unsafeDupablePerformIO (vkGetProc @VkDestroyDescriptorPool)

{-# NOINLINE vkDestroyDescriptorPool #-}
##endif

-- |
-- > void vkDestroyDescriptorPool
-- >     ( VkDevice device
-- >     , VkDescriptorPool descriptorPool
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDescriptorPool vkDestroyDescriptorPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyDescriptorPool <- vkGetDeviceProc @VkDestroyDescriptorPool vkDevice
--
-- or less efficient:
--
-- > myDestroyDescriptorPool <- vkGetProc @VkDestroyDescriptorPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyDescriptorPool"
               vkDestroyDescriptorPoolSafe ::
               VkDevice -- ^ device
                        -> VkDescriptorPool -- ^ descriptorPool
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

##else
vkDestroyDescriptorPoolSafe ::
                            VkDevice -- ^ device
                                     -> VkDescriptorPool -- ^ descriptorPool
                                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                      -> IO ()
vkDestroyDescriptorPoolSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyDescriptorPool)

{-# NOINLINE vkDestroyDescriptorPoolSafe #-}
##endif

-- | > void vkDestroyDescriptorPool
--   >     ( VkDevice device
--   >     , VkDescriptorPool descriptorPool
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDescriptorPool vkDestroyDescriptorPool registry at www.khronos.org>
type HS_vkDestroyDescriptorPool =
     VkDevice -- ^ device
              -> VkDescriptorPool -- ^ descriptorPool
                                  -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                               -> IO ()

type PFN_vkDestroyDescriptorPool =
     FunPtr HS_vkDestroyDescriptorPool

foreign import ccall unsafe "dynamic" unwrapVkDestroyDescriptorPool
               :: PFN_vkDestroyDescriptorPool -> HS_vkDestroyDescriptorPool

foreign import ccall safe "dynamic"
               unwrapVkDestroyDescriptorPoolSafe ::
               PFN_vkDestroyDescriptorPool -> HS_vkDestroyDescriptorPool

instance VulkanProc "vkDestroyDescriptorPool" where
        type VkProcType "vkDestroyDescriptorPool" =
             HS_vkDestroyDescriptorPool
        vkProcSymbol = _VkDestroyDescriptorPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyDescriptorPool

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyDescriptorPoolSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkResetDescriptorPool
-- >     ( VkDevice device
-- >     , VkDescriptorPool descriptorPool
-- >     , VkDescriptorPoolResetFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkResetDescriptorPool vkResetDescriptorPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myResetDescriptorPool <- vkGetDeviceProc @VkResetDescriptorPool vkDevice
--
-- or less efficient:
--
-- > myResetDescriptorPool <- vkGetProc @VkResetDescriptorPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkResetDescriptorPool"
               vkResetDescriptorPool ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorPool -- ^ descriptorPool
                                  -> VkDescriptorPoolResetFlags -- ^ flags
                                                                -> IO VkResult

##else
vkResetDescriptorPool ::
                      VkDevice -- ^ device
                               ->
                        VkDescriptorPool -- ^ descriptorPool
                                         -> VkDescriptorPoolResetFlags -- ^ flags
                                                                       -> IO VkResult
vkResetDescriptorPool
  = unsafeDupablePerformIO (vkGetProc @VkResetDescriptorPool)

{-# NOINLINE vkResetDescriptorPool #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkResetDescriptorPool
-- >     ( VkDevice device
-- >     , VkDescriptorPool descriptorPool
-- >     , VkDescriptorPoolResetFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkResetDescriptorPool vkResetDescriptorPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myResetDescriptorPool <- vkGetDeviceProc @VkResetDescriptorPool vkDevice
--
-- or less efficient:
--
-- > myResetDescriptorPool <- vkGetProc @VkResetDescriptorPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkResetDescriptorPool"
               vkResetDescriptorPoolSafe ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorPool -- ^ descriptorPool
                                  -> VkDescriptorPoolResetFlags -- ^ flags
                                                                -> IO VkResult

##else
vkResetDescriptorPoolSafe ::
                          VkDevice -- ^ device
                                   ->
                            VkDescriptorPool -- ^ descriptorPool
                                             -> VkDescriptorPoolResetFlags -- ^ flags
                                                                           -> IO VkResult
vkResetDescriptorPoolSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkResetDescriptorPool)

{-# NOINLINE vkResetDescriptorPoolSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkResetDescriptorPool vkResetDescriptorPool registry at www.khronos.org>
type HS_vkResetDescriptorPool =
     VkDevice -- ^ device
              ->
       VkDescriptorPool -- ^ descriptorPool
                        -> VkDescriptorPoolResetFlags -- ^ flags
                                                      -> IO VkResult

type PFN_vkResetDescriptorPool = FunPtr HS_vkResetDescriptorPool

foreign import ccall unsafe "dynamic" unwrapVkResetDescriptorPool
               :: PFN_vkResetDescriptorPool -> HS_vkResetDescriptorPool

foreign import ccall safe "dynamic" unwrapVkResetDescriptorPoolSafe
               :: PFN_vkResetDescriptorPool -> HS_vkResetDescriptorPool

instance VulkanProc "vkResetDescriptorPool" where
        type VkProcType "vkResetDescriptorPool" = HS_vkResetDescriptorPool
        vkProcSymbol = _VkResetDescriptorPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkResetDescriptorPool

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkResetDescriptorPoolSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FRAGMENTED_POOL', 'VK_ERROR_OUT_OF_POOL_MEMORY'.
--
-- > VkResult vkAllocateDescriptorSets
-- >     ( VkDevice device
-- >     , const VkDescriptorSetAllocateInfo* pAllocateInfo
-- >     , VkDescriptorSet* pDescriptorSets
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAllocateDescriptorSets vkAllocateDescriptorSets registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myAllocateDescriptorSets <- vkGetDeviceProc @VkAllocateDescriptorSets vkDevice
--
-- or less efficient:
--
-- > myAllocateDescriptorSets <- vkGetProc @VkAllocateDescriptorSets
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkAllocateDescriptorSets"
               vkAllocateDescriptorSets ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorSetAllocateInfo -- ^ pAllocateInfo
                                                 ->
                   Ptr VkDescriptorSet -- ^ pDescriptorSets
                                       -> IO VkResult

##else
vkAllocateDescriptorSets ::
                         VkDevice -- ^ device
                                  ->
                           Ptr VkDescriptorSetAllocateInfo -- ^ pAllocateInfo
                                                           ->
                             Ptr VkDescriptorSet -- ^ pDescriptorSets
                                                 -> IO VkResult
vkAllocateDescriptorSets
  = unsafeDupablePerformIO (vkGetProc @VkAllocateDescriptorSets)

{-# NOINLINE vkAllocateDescriptorSets #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FRAGMENTED_POOL', 'VK_ERROR_OUT_OF_POOL_MEMORY'.
--
-- > VkResult vkAllocateDescriptorSets
-- >     ( VkDevice device
-- >     , const VkDescriptorSetAllocateInfo* pAllocateInfo
-- >     , VkDescriptorSet* pDescriptorSets
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAllocateDescriptorSets vkAllocateDescriptorSets registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myAllocateDescriptorSets <- vkGetDeviceProc @VkAllocateDescriptorSets vkDevice
--
-- or less efficient:
--
-- > myAllocateDescriptorSets <- vkGetProc @VkAllocateDescriptorSets
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkAllocateDescriptorSets"
               vkAllocateDescriptorSetsSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorSetAllocateInfo -- ^ pAllocateInfo
                                                 ->
                   Ptr VkDescriptorSet -- ^ pDescriptorSets
                                       -> IO VkResult

##else
vkAllocateDescriptorSetsSafe ::
                             VkDevice -- ^ device
                                      ->
                               Ptr VkDescriptorSetAllocateInfo -- ^ pAllocateInfo
                                                               ->
                                 Ptr VkDescriptorSet -- ^ pDescriptorSets
                                                     -> IO VkResult
vkAllocateDescriptorSetsSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkAllocateDescriptorSets)

{-# NOINLINE vkAllocateDescriptorSetsSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAllocateDescriptorSets vkAllocateDescriptorSets registry at www.khronos.org>
type HS_vkAllocateDescriptorSets =
     VkDevice -- ^ device
              ->
       Ptr VkDescriptorSetAllocateInfo -- ^ pAllocateInfo
                                       ->
         Ptr VkDescriptorSet -- ^ pDescriptorSets
                             -> IO VkResult

type PFN_vkAllocateDescriptorSets =
     FunPtr HS_vkAllocateDescriptorSets

foreign import ccall unsafe "dynamic"
               unwrapVkAllocateDescriptorSets ::
               PFN_vkAllocateDescriptorSets -> HS_vkAllocateDescriptorSets

foreign import ccall safe "dynamic"
               unwrapVkAllocateDescriptorSetsSafe ::
               PFN_vkAllocateDescriptorSets -> HS_vkAllocateDescriptorSets

instance VulkanProc "vkAllocateDescriptorSets" where
        type VkProcType "vkAllocateDescriptorSets" =
             HS_vkAllocateDescriptorSets
        vkProcSymbol = _VkAllocateDescriptorSets

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkAllocateDescriptorSets

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkAllocateDescriptorSetsSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkFreeDescriptorSets
-- >     ( VkDevice device
-- >     , VkDescriptorPool descriptorPool
-- >     , uint32_t descriptorSetCount
-- >     , const VkDescriptorSet* pDescriptorSets
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkFreeDescriptorSets vkFreeDescriptorSets registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myFreeDescriptorSets <- vkGetDeviceProc @VkFreeDescriptorSets vkDevice
--
-- or less efficient:
--
-- > myFreeDescriptorSets <- vkGetProc @VkFreeDescriptorSets
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkFreeDescriptorSets"
               vkFreeDescriptorSets ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorPool -- ^ descriptorPool
                                  -> Word32 -- ^ descriptorSetCount
                                            -> Ptr VkDescriptorSet -- ^ pDescriptorSets
                                                                   -> IO VkResult

##else
vkFreeDescriptorSets ::
                     VkDevice -- ^ device
                              ->
                       VkDescriptorPool -- ^ descriptorPool
                                        -> Word32 -- ^ descriptorSetCount
                                                  -> Ptr VkDescriptorSet -- ^ pDescriptorSets
                                                                         -> IO VkResult
vkFreeDescriptorSets
  = unsafeDupablePerformIO (vkGetProc @VkFreeDescriptorSets)

{-# NOINLINE vkFreeDescriptorSets #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkFreeDescriptorSets
-- >     ( VkDevice device
-- >     , VkDescriptorPool descriptorPool
-- >     , uint32_t descriptorSetCount
-- >     , const VkDescriptorSet* pDescriptorSets
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkFreeDescriptorSets vkFreeDescriptorSets registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myFreeDescriptorSets <- vkGetDeviceProc @VkFreeDescriptorSets vkDevice
--
-- or less efficient:
--
-- > myFreeDescriptorSets <- vkGetProc @VkFreeDescriptorSets
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkFreeDescriptorSets"
               vkFreeDescriptorSetsSafe ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorPool -- ^ descriptorPool
                                  -> Word32 -- ^ descriptorSetCount
                                            -> Ptr VkDescriptorSet -- ^ pDescriptorSets
                                                                   -> IO VkResult

##else
vkFreeDescriptorSetsSafe ::
                         VkDevice -- ^ device
                                  ->
                           VkDescriptorPool -- ^ descriptorPool
                                            -> Word32 -- ^ descriptorSetCount
                                                      -> Ptr VkDescriptorSet -- ^ pDescriptorSets
                                                                             -> IO VkResult
vkFreeDescriptorSetsSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkFreeDescriptorSets)

{-# NOINLINE vkFreeDescriptorSetsSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkFreeDescriptorSets vkFreeDescriptorSets registry at www.khronos.org>
type HS_vkFreeDescriptorSets =
     VkDevice -- ^ device
              ->
       VkDescriptorPool -- ^ descriptorPool
                        -> Word32 -- ^ descriptorSetCount
                                  -> Ptr VkDescriptorSet -- ^ pDescriptorSets
                                                         -> IO VkResult

type PFN_vkFreeDescriptorSets = FunPtr HS_vkFreeDescriptorSets

foreign import ccall unsafe "dynamic" unwrapVkFreeDescriptorSets ::
               PFN_vkFreeDescriptorSets -> HS_vkFreeDescriptorSets

foreign import ccall safe "dynamic" unwrapVkFreeDescriptorSetsSafe
               :: PFN_vkFreeDescriptorSets -> HS_vkFreeDescriptorSets

instance VulkanProc "vkFreeDescriptorSets" where
        type VkProcType "vkFreeDescriptorSets" = HS_vkFreeDescriptorSets
        vkProcSymbol = _VkFreeDescriptorSets

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkFreeDescriptorSets

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkFreeDescriptorSetsSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkUpdateDescriptorSets
-- >     ( VkDevice device
-- >     , uint32_t descriptorWriteCount
-- >     , const VkWriteDescriptorSet* pDescriptorWrites
-- >     , uint32_t descriptorCopyCount
-- >     , const VkCopyDescriptorSet* pDescriptorCopies
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkUpdateDescriptorSets vkUpdateDescriptorSets registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myUpdateDescriptorSets <- vkGetDeviceProc @VkUpdateDescriptorSets vkDevice
--
-- or less efficient:
--
-- > myUpdateDescriptorSets <- vkGetProc @VkUpdateDescriptorSets
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkUpdateDescriptorSets
  = unsafeDupablePerformIO (vkGetProc @VkUpdateDescriptorSets)

{-# NOINLINE vkUpdateDescriptorSets #-}
##endif

-- |
-- > void vkUpdateDescriptorSets
-- >     ( VkDevice device
-- >     , uint32_t descriptorWriteCount
-- >     , const VkWriteDescriptorSet* pDescriptorWrites
-- >     , uint32_t descriptorCopyCount
-- >     , const VkCopyDescriptorSet* pDescriptorCopies
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkUpdateDescriptorSets vkUpdateDescriptorSets registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myUpdateDescriptorSets <- vkGetDeviceProc @VkUpdateDescriptorSets vkDevice
--
-- or less efficient:
--
-- > myUpdateDescriptorSets <- vkGetProc @VkUpdateDescriptorSets
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkUpdateDescriptorSetsSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkUpdateDescriptorSets)

{-# NOINLINE vkUpdateDescriptorSetsSafe #-}
##endif

-- | > void vkUpdateDescriptorSets
--   >     ( VkDevice device
--   >     , uint32_t descriptorWriteCount
--   >     , const VkWriteDescriptorSet* pDescriptorWrites
--   >     , uint32_t descriptorCopyCount
--   >     , const VkCopyDescriptorSet* pDescriptorCopies
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkUpdateDescriptorSets vkUpdateDescriptorSets registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic" unwrapVkUpdateDescriptorSets
               :: PFN_vkUpdateDescriptorSets -> HS_vkUpdateDescriptorSets

foreign import ccall safe "dynamic"
               unwrapVkUpdateDescriptorSetsSafe ::
               PFN_vkUpdateDescriptorSets -> HS_vkUpdateDescriptorSets

instance VulkanProc "vkUpdateDescriptorSets" where
        type VkProcType "vkUpdateDescriptorSets" =
             HS_vkUpdateDescriptorSets
        vkProcSymbol = _VkUpdateDescriptorSets

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkUpdateDescriptorSets

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkUpdateDescriptorSetsSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateFramebuffer
-- >     ( VkDevice device
-- >     , const VkFramebufferCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkFramebuffer* pFramebuffer
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateFramebuffer vkCreateFramebuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateFramebuffer <- vkGetDeviceProc @VkCreateFramebuffer vkDevice
--
-- or less efficient:
--
-- > myCreateFramebuffer <- vkGetProc @VkCreateFramebuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateFramebuffer"
               vkCreateFramebuffer ::
               VkDevice -- ^ device
                        ->
                 Ptr VkFramebufferCreateInfo -- ^ pCreateInfo
                                             ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkFramebuffer -- ^ pFramebuffer
                                                                  -> IO VkResult

##else
vkCreateFramebuffer ::
                    VkDevice -- ^ device
                             ->
                      Ptr VkFramebufferCreateInfo -- ^ pCreateInfo
                                                  ->
                        Ptr VkAllocationCallbacks -- ^ pAllocator
                                                  -> Ptr VkFramebuffer -- ^ pFramebuffer
                                                                       -> IO VkResult
vkCreateFramebuffer
  = unsafeDupablePerformIO (vkGetProc @VkCreateFramebuffer)

{-# NOINLINE vkCreateFramebuffer #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateFramebuffer
-- >     ( VkDevice device
-- >     , const VkFramebufferCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkFramebuffer* pFramebuffer
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateFramebuffer vkCreateFramebuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateFramebuffer <- vkGetDeviceProc @VkCreateFramebuffer vkDevice
--
-- or less efficient:
--
-- > myCreateFramebuffer <- vkGetProc @VkCreateFramebuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateFramebuffer"
               vkCreateFramebufferSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkFramebufferCreateInfo -- ^ pCreateInfo
                                             ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkFramebuffer -- ^ pFramebuffer
                                                                  -> IO VkResult

##else
vkCreateFramebufferSafe ::
                        VkDevice -- ^ device
                                 ->
                          Ptr VkFramebufferCreateInfo -- ^ pCreateInfo
                                                      ->
                            Ptr VkAllocationCallbacks -- ^ pAllocator
                                                      -> Ptr VkFramebuffer -- ^ pFramebuffer
                                                                           -> IO VkResult
vkCreateFramebufferSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateFramebuffer)

{-# NOINLINE vkCreateFramebufferSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateFramebuffer vkCreateFramebuffer registry at www.khronos.org>
type HS_vkCreateFramebuffer =
     VkDevice -- ^ device
              ->
       Ptr VkFramebufferCreateInfo -- ^ pCreateInfo
                                   ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkFramebuffer -- ^ pFramebuffer
                                                        -> IO VkResult

type PFN_vkCreateFramebuffer = FunPtr HS_vkCreateFramebuffer

foreign import ccall unsafe "dynamic" unwrapVkCreateFramebuffer ::
               PFN_vkCreateFramebuffer -> HS_vkCreateFramebuffer

foreign import ccall safe "dynamic" unwrapVkCreateFramebufferSafe
               :: PFN_vkCreateFramebuffer -> HS_vkCreateFramebuffer

instance VulkanProc "vkCreateFramebuffer" where
        type VkProcType "vkCreateFramebuffer" = HS_vkCreateFramebuffer
        vkProcSymbol = _VkCreateFramebuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateFramebuffer

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateFramebufferSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyFramebuffer
-- >     ( VkDevice device
-- >     , VkFramebuffer framebuffer
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyFramebuffer vkDestroyFramebuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyFramebuffer <- vkGetDeviceProc @VkDestroyFramebuffer vkDevice
--
-- or less efficient:
--
-- > myDestroyFramebuffer <- vkGetProc @VkDestroyFramebuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyFramebuffer"
               vkDestroyFramebuffer ::
               VkDevice -- ^ device
                        -> VkFramebuffer -- ^ framebuffer
                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()

##else
vkDestroyFramebuffer ::
                     VkDevice -- ^ device
                              -> VkFramebuffer -- ^ framebuffer
                                               -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                            -> IO ()
vkDestroyFramebuffer
  = unsafeDupablePerformIO (vkGetProc @VkDestroyFramebuffer)

{-# NOINLINE vkDestroyFramebuffer #-}
##endif

-- |
-- > void vkDestroyFramebuffer
-- >     ( VkDevice device
-- >     , VkFramebuffer framebuffer
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyFramebuffer vkDestroyFramebuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyFramebuffer <- vkGetDeviceProc @VkDestroyFramebuffer vkDevice
--
-- or less efficient:
--
-- > myDestroyFramebuffer <- vkGetProc @VkDestroyFramebuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyFramebuffer"
               vkDestroyFramebufferSafe ::
               VkDevice -- ^ device
                        -> VkFramebuffer -- ^ framebuffer
                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()

##else
vkDestroyFramebufferSafe ::
                         VkDevice -- ^ device
                                  -> VkFramebuffer -- ^ framebuffer
                                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                -> IO ()
vkDestroyFramebufferSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyFramebuffer)

{-# NOINLINE vkDestroyFramebufferSafe #-}
##endif

-- | > void vkDestroyFramebuffer
--   >     ( VkDevice device
--   >     , VkFramebuffer framebuffer
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyFramebuffer vkDestroyFramebuffer registry at www.khronos.org>
type HS_vkDestroyFramebuffer =
     VkDevice -- ^ device
              -> VkFramebuffer -- ^ framebuffer
                               -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                            -> IO ()

type PFN_vkDestroyFramebuffer = FunPtr HS_vkDestroyFramebuffer

foreign import ccall unsafe "dynamic" unwrapVkDestroyFramebuffer ::
               PFN_vkDestroyFramebuffer -> HS_vkDestroyFramebuffer

foreign import ccall safe "dynamic" unwrapVkDestroyFramebufferSafe
               :: PFN_vkDestroyFramebuffer -> HS_vkDestroyFramebuffer

instance VulkanProc "vkDestroyFramebuffer" where
        type VkProcType "vkDestroyFramebuffer" = HS_vkDestroyFramebuffer
        vkProcSymbol = _VkDestroyFramebuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyFramebuffer

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyFramebufferSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateRenderPass
-- >     ( VkDevice device
-- >     , const VkRenderPassCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkRenderPass* pRenderPass
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateRenderPass vkCreateRenderPass registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateRenderPass <- vkGetDeviceProc @VkCreateRenderPass vkDevice
--
-- or less efficient:
--
-- > myCreateRenderPass <- vkGetProc @VkCreateRenderPass
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateRenderPass" vkCreateRenderPass
               ::
               VkDevice -- ^ device
                        ->
                 Ptr VkRenderPassCreateInfo -- ^ pCreateInfo
                                            ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkRenderPass -- ^ pRenderPass
                                                                 -> IO VkResult

##else
vkCreateRenderPass ::
                   VkDevice -- ^ device
                            ->
                     Ptr VkRenderPassCreateInfo -- ^ pCreateInfo
                                                ->
                       Ptr VkAllocationCallbacks -- ^ pAllocator
                                                 -> Ptr VkRenderPass -- ^ pRenderPass
                                                                     -> IO VkResult
vkCreateRenderPass
  = unsafeDupablePerformIO (vkGetProc @VkCreateRenderPass)

{-# NOINLINE vkCreateRenderPass #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateRenderPass
-- >     ( VkDevice device
-- >     , const VkRenderPassCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkRenderPass* pRenderPass
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateRenderPass vkCreateRenderPass registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateRenderPass <- vkGetDeviceProc @VkCreateRenderPass vkDevice
--
-- or less efficient:
--
-- > myCreateRenderPass <- vkGetProc @VkCreateRenderPass
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateRenderPass"
               vkCreateRenderPassSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkRenderPassCreateInfo -- ^ pCreateInfo
                                            ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkRenderPass -- ^ pRenderPass
                                                                 -> IO VkResult

##else
vkCreateRenderPassSafe ::
                       VkDevice -- ^ device
                                ->
                         Ptr VkRenderPassCreateInfo -- ^ pCreateInfo
                                                    ->
                           Ptr VkAllocationCallbacks -- ^ pAllocator
                                                     -> Ptr VkRenderPass -- ^ pRenderPass
                                                                         -> IO VkResult
vkCreateRenderPassSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateRenderPass)

{-# NOINLINE vkCreateRenderPassSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateRenderPass vkCreateRenderPass registry at www.khronos.org>
type HS_vkCreateRenderPass =
     VkDevice -- ^ device
              ->
       Ptr VkRenderPassCreateInfo -- ^ pCreateInfo
                                  ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkRenderPass -- ^ pRenderPass
                                                       -> IO VkResult

type PFN_vkCreateRenderPass = FunPtr HS_vkCreateRenderPass

foreign import ccall unsafe "dynamic" unwrapVkCreateRenderPass ::
               PFN_vkCreateRenderPass -> HS_vkCreateRenderPass

foreign import ccall safe "dynamic" unwrapVkCreateRenderPassSafe ::
               PFN_vkCreateRenderPass -> HS_vkCreateRenderPass

instance VulkanProc "vkCreateRenderPass" where
        type VkProcType "vkCreateRenderPass" = HS_vkCreateRenderPass
        vkProcSymbol = _VkCreateRenderPass

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateRenderPass

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateRenderPassSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyRenderPass
-- >     ( VkDevice device
-- >     , VkRenderPass renderPass
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyRenderPass vkDestroyRenderPass registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyRenderPass <- vkGetDeviceProc @VkDestroyRenderPass vkDevice
--
-- or less efficient:
--
-- > myDestroyRenderPass <- vkGetProc @VkDestroyRenderPass
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyRenderPass"
               vkDestroyRenderPass ::
               VkDevice -- ^ device
                        -> VkRenderPass -- ^ renderPass
                                        -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                     -> IO ()

##else
vkDestroyRenderPass ::
                    VkDevice -- ^ device
                             -> VkRenderPass -- ^ renderPass
                                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                          -> IO ()
vkDestroyRenderPass
  = unsafeDupablePerformIO (vkGetProc @VkDestroyRenderPass)

{-# NOINLINE vkDestroyRenderPass #-}
##endif

-- |
-- > void vkDestroyRenderPass
-- >     ( VkDevice device
-- >     , VkRenderPass renderPass
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyRenderPass vkDestroyRenderPass registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyRenderPass <- vkGetDeviceProc @VkDestroyRenderPass vkDevice
--
-- or less efficient:
--
-- > myDestroyRenderPass <- vkGetProc @VkDestroyRenderPass
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyRenderPass"
               vkDestroyRenderPassSafe ::
               VkDevice -- ^ device
                        -> VkRenderPass -- ^ renderPass
                                        -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                     -> IO ()

##else
vkDestroyRenderPassSafe ::
                        VkDevice -- ^ device
                                 -> VkRenderPass -- ^ renderPass
                                                 -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                              -> IO ()
vkDestroyRenderPassSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyRenderPass)

{-# NOINLINE vkDestroyRenderPassSafe #-}
##endif

-- | > void vkDestroyRenderPass
--   >     ( VkDevice device
--   >     , VkRenderPass renderPass
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyRenderPass vkDestroyRenderPass registry at www.khronos.org>
type HS_vkDestroyRenderPass =
     VkDevice -- ^ device
              -> VkRenderPass -- ^ renderPass
                              -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                           -> IO ()

type PFN_vkDestroyRenderPass = FunPtr HS_vkDestroyRenderPass

foreign import ccall unsafe "dynamic" unwrapVkDestroyRenderPass ::
               PFN_vkDestroyRenderPass -> HS_vkDestroyRenderPass

foreign import ccall safe "dynamic" unwrapVkDestroyRenderPassSafe
               :: PFN_vkDestroyRenderPass -> HS_vkDestroyRenderPass

instance VulkanProc "vkDestroyRenderPass" where
        type VkProcType "vkDestroyRenderPass" = HS_vkDestroyRenderPass
        vkProcSymbol = _VkDestroyRenderPass

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyRenderPass

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyRenderPassSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkGetRenderAreaGranularity
-- >     ( VkDevice device
-- >     , VkRenderPass renderPass
-- >     , VkExtent2D* pGranularity
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetRenderAreaGranularity vkGetRenderAreaGranularity registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetRenderAreaGranularity <- vkGetDeviceProc @VkGetRenderAreaGranularity vkDevice
--
-- or less efficient:
--
-- > myGetRenderAreaGranularity <- vkGetProc @VkGetRenderAreaGranularity
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetRenderAreaGranularity"
               vkGetRenderAreaGranularity ::
               VkDevice -- ^ device
                        -> VkRenderPass -- ^ renderPass
                                        -> Ptr VkExtent2D -- ^ pGranularity
                                                          -> IO ()

##else
vkGetRenderAreaGranularity ::
                           VkDevice -- ^ device
                                    -> VkRenderPass -- ^ renderPass
                                                    -> Ptr VkExtent2D -- ^ pGranularity
                                                                      -> IO ()
vkGetRenderAreaGranularity
  = unsafeDupablePerformIO (vkGetProc @VkGetRenderAreaGranularity)

{-# NOINLINE vkGetRenderAreaGranularity #-}
##endif

-- |
-- > void vkGetRenderAreaGranularity
-- >     ( VkDevice device
-- >     , VkRenderPass renderPass
-- >     , VkExtent2D* pGranularity
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetRenderAreaGranularity vkGetRenderAreaGranularity registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetRenderAreaGranularity <- vkGetDeviceProc @VkGetRenderAreaGranularity vkDevice
--
-- or less efficient:
--
-- > myGetRenderAreaGranularity <- vkGetProc @VkGetRenderAreaGranularity
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetRenderAreaGranularity"
               vkGetRenderAreaGranularitySafe ::
               VkDevice -- ^ device
                        -> VkRenderPass -- ^ renderPass
                                        -> Ptr VkExtent2D -- ^ pGranularity
                                                          -> IO ()

##else
vkGetRenderAreaGranularitySafe ::
                               VkDevice -- ^ device
                                        -> VkRenderPass -- ^ renderPass
                                                        -> Ptr VkExtent2D -- ^ pGranularity
                                                                          -> IO ()
vkGetRenderAreaGranularitySafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetRenderAreaGranularity)

{-# NOINLINE vkGetRenderAreaGranularitySafe #-}
##endif

-- | > void vkGetRenderAreaGranularity
--   >     ( VkDevice device
--   >     , VkRenderPass renderPass
--   >     , VkExtent2D* pGranularity
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetRenderAreaGranularity vkGetRenderAreaGranularity registry at www.khronos.org>
type HS_vkGetRenderAreaGranularity =
     VkDevice -- ^ device
              -> VkRenderPass -- ^ renderPass
                              -> Ptr VkExtent2D -- ^ pGranularity
                                                -> IO ()

type PFN_vkGetRenderAreaGranularity =
     FunPtr HS_vkGetRenderAreaGranularity

foreign import ccall unsafe "dynamic"
               unwrapVkGetRenderAreaGranularity ::
               PFN_vkGetRenderAreaGranularity -> HS_vkGetRenderAreaGranularity

foreign import ccall safe "dynamic"
               unwrapVkGetRenderAreaGranularitySafe ::
               PFN_vkGetRenderAreaGranularity -> HS_vkGetRenderAreaGranularity

instance VulkanProc "vkGetRenderAreaGranularity" where
        type VkProcType "vkGetRenderAreaGranularity" =
             HS_vkGetRenderAreaGranularity
        vkProcSymbol = _VkGetRenderAreaGranularity

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetRenderAreaGranularity

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkGetRenderAreaGranularitySafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateCommandPool
-- >     ( VkDevice device
-- >     , const VkCommandPoolCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkCommandPool* pCommandPool
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateCommandPool vkCreateCommandPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateCommandPool <- vkGetDeviceProc @VkCreateCommandPool vkDevice
--
-- or less efficient:
--
-- > myCreateCommandPool <- vkGetProc @VkCreateCommandPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateCommandPool"
               vkCreateCommandPool ::
               VkDevice -- ^ device
                        ->
                 Ptr VkCommandPoolCreateInfo -- ^ pCreateInfo
                                             ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkCommandPool -- ^ pCommandPool
                                                                  -> IO VkResult

##else
vkCreateCommandPool ::
                    VkDevice -- ^ device
                             ->
                      Ptr VkCommandPoolCreateInfo -- ^ pCreateInfo
                                                  ->
                        Ptr VkAllocationCallbacks -- ^ pAllocator
                                                  -> Ptr VkCommandPool -- ^ pCommandPool
                                                                       -> IO VkResult
vkCreateCommandPool
  = unsafeDupablePerformIO (vkGetProc @VkCreateCommandPool)

{-# NOINLINE vkCreateCommandPool #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateCommandPool
-- >     ( VkDevice device
-- >     , const VkCommandPoolCreateInfo* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkCommandPool* pCommandPool
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateCommandPool vkCreateCommandPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateCommandPool <- vkGetDeviceProc @VkCreateCommandPool vkDevice
--
-- or less efficient:
--
-- > myCreateCommandPool <- vkGetProc @VkCreateCommandPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateCommandPool"
               vkCreateCommandPoolSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkCommandPoolCreateInfo -- ^ pCreateInfo
                                             ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkCommandPool -- ^ pCommandPool
                                                                  -> IO VkResult

##else
vkCreateCommandPoolSafe ::
                        VkDevice -- ^ device
                                 ->
                          Ptr VkCommandPoolCreateInfo -- ^ pCreateInfo
                                                      ->
                            Ptr VkAllocationCallbacks -- ^ pAllocator
                                                      -> Ptr VkCommandPool -- ^ pCommandPool
                                                                           -> IO VkResult
vkCreateCommandPoolSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateCommandPool)

{-# NOINLINE vkCreateCommandPoolSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateCommandPool vkCreateCommandPool registry at www.khronos.org>
type HS_vkCreateCommandPool =
     VkDevice -- ^ device
              ->
       Ptr VkCommandPoolCreateInfo -- ^ pCreateInfo
                                   ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkCommandPool -- ^ pCommandPool
                                                        -> IO VkResult

type PFN_vkCreateCommandPool = FunPtr HS_vkCreateCommandPool

foreign import ccall unsafe "dynamic" unwrapVkCreateCommandPool ::
               PFN_vkCreateCommandPool -> HS_vkCreateCommandPool

foreign import ccall safe "dynamic" unwrapVkCreateCommandPoolSafe
               :: PFN_vkCreateCommandPool -> HS_vkCreateCommandPool

instance VulkanProc "vkCreateCommandPool" where
        type VkProcType "vkCreateCommandPool" = HS_vkCreateCommandPool
        vkProcSymbol = _VkCreateCommandPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateCommandPool

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCreateCommandPoolSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkDestroyCommandPool
-- >     ( VkDevice device
-- >     , VkCommandPool commandPool
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyCommandPool vkDestroyCommandPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyCommandPool <- vkGetDeviceProc @VkDestroyCommandPool vkDevice
--
-- or less efficient:
--
-- > myDestroyCommandPool <- vkGetProc @VkDestroyCommandPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroyCommandPool"
               vkDestroyCommandPool ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()

##else
vkDestroyCommandPool ::
                     VkDevice -- ^ device
                              -> VkCommandPool -- ^ commandPool
                                               -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                            -> IO ()
vkDestroyCommandPool
  = unsafeDupablePerformIO (vkGetProc @VkDestroyCommandPool)

{-# NOINLINE vkDestroyCommandPool #-}
##endif

-- |
-- > void vkDestroyCommandPool
-- >     ( VkDevice device
-- >     , VkCommandPool commandPool
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyCommandPool vkDestroyCommandPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyCommandPool <- vkGetDeviceProc @VkDestroyCommandPool vkDevice
--
-- or less efficient:
--
-- > myDestroyCommandPool <- vkGetProc @VkDestroyCommandPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroyCommandPool"
               vkDestroyCommandPoolSafe ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()

##else
vkDestroyCommandPoolSafe ::
                         VkDevice -- ^ device
                                  -> VkCommandPool -- ^ commandPool
                                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                -> IO ()
vkDestroyCommandPoolSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroyCommandPool)

{-# NOINLINE vkDestroyCommandPoolSafe #-}
##endif

-- | > void vkDestroyCommandPool
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyCommandPool vkDestroyCommandPool registry at www.khronos.org>
type HS_vkDestroyCommandPool =
     VkDevice -- ^ device
              -> VkCommandPool -- ^ commandPool
                               -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                            -> IO ()

type PFN_vkDestroyCommandPool = FunPtr HS_vkDestroyCommandPool

foreign import ccall unsafe "dynamic" unwrapVkDestroyCommandPool ::
               PFN_vkDestroyCommandPool -> HS_vkDestroyCommandPool

foreign import ccall safe "dynamic" unwrapVkDestroyCommandPoolSafe
               :: PFN_vkDestroyCommandPool -> HS_vkDestroyCommandPool

instance VulkanProc "vkDestroyCommandPool" where
        type VkProcType "vkDestroyCommandPool" = HS_vkDestroyCommandPool
        vkProcSymbol = _VkDestroyCommandPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroyCommandPool

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroyCommandPoolSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkResetCommandPool
-- >     ( VkDevice device
-- >     , VkCommandPool commandPool
-- >     , VkCommandPoolResetFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkResetCommandPool vkResetCommandPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myResetCommandPool <- vkGetDeviceProc @VkResetCommandPool vkDevice
--
-- or less efficient:
--
-- > myResetCommandPool <- vkGetProc @VkResetCommandPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkResetCommandPool" vkResetCommandPool
               ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> VkCommandPoolResetFlags -- ^ flags
                                                                    -> IO VkResult

##else
vkResetCommandPool ::
                   VkDevice -- ^ device
                            -> VkCommandPool -- ^ commandPool
                                             -> VkCommandPoolResetFlags -- ^ flags
                                                                        -> IO VkResult
vkResetCommandPool
  = unsafeDupablePerformIO (vkGetProc @VkResetCommandPool)

{-# NOINLINE vkResetCommandPool #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkResetCommandPool
-- >     ( VkDevice device
-- >     , VkCommandPool commandPool
-- >     , VkCommandPoolResetFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkResetCommandPool vkResetCommandPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myResetCommandPool <- vkGetDeviceProc @VkResetCommandPool vkDevice
--
-- or less efficient:
--
-- > myResetCommandPool <- vkGetProc @VkResetCommandPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkResetCommandPool"
               vkResetCommandPoolSafe ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> VkCommandPoolResetFlags -- ^ flags
                                                                    -> IO VkResult

##else
vkResetCommandPoolSafe ::
                       VkDevice -- ^ device
                                -> VkCommandPool -- ^ commandPool
                                                 -> VkCommandPoolResetFlags -- ^ flags
                                                                            -> IO VkResult
vkResetCommandPoolSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkResetCommandPool)

{-# NOINLINE vkResetCommandPoolSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkResetCommandPool vkResetCommandPool registry at www.khronos.org>
type HS_vkResetCommandPool =
     VkDevice -- ^ device
              -> VkCommandPool -- ^ commandPool
                               -> VkCommandPoolResetFlags -- ^ flags
                                                          -> IO VkResult

type PFN_vkResetCommandPool = FunPtr HS_vkResetCommandPool

foreign import ccall unsafe "dynamic" unwrapVkResetCommandPool ::
               PFN_vkResetCommandPool -> HS_vkResetCommandPool

foreign import ccall safe "dynamic" unwrapVkResetCommandPoolSafe ::
               PFN_vkResetCommandPool -> HS_vkResetCommandPool

instance VulkanProc "vkResetCommandPool" where
        type VkProcType "vkResetCommandPool" = HS_vkResetCommandPool
        vkProcSymbol = _VkResetCommandPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkResetCommandPool

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkResetCommandPoolSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkAllocateCommandBuffers
-- >     ( VkDevice device
-- >     , const VkCommandBufferAllocateInfo* pAllocateInfo
-- >     , VkCommandBuffer* pCommandBuffers
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAllocateCommandBuffers vkAllocateCommandBuffers registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myAllocateCommandBuffers <- vkGetDeviceProc @VkAllocateCommandBuffers vkDevice
--
-- or less efficient:
--
-- > myAllocateCommandBuffers <- vkGetProc @VkAllocateCommandBuffers
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkAllocateCommandBuffers"
               vkAllocateCommandBuffers ::
               VkDevice -- ^ device
                        ->
                 Ptr VkCommandBufferAllocateInfo -- ^ pAllocateInfo
                                                 ->
                   Ptr VkCommandBuffer -- ^ pCommandBuffers
                                       -> IO VkResult

##else
vkAllocateCommandBuffers ::
                         VkDevice -- ^ device
                                  ->
                           Ptr VkCommandBufferAllocateInfo -- ^ pAllocateInfo
                                                           ->
                             Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                 -> IO VkResult
vkAllocateCommandBuffers
  = unsafeDupablePerformIO (vkGetProc @VkAllocateCommandBuffers)

{-# NOINLINE vkAllocateCommandBuffers #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkAllocateCommandBuffers
-- >     ( VkDevice device
-- >     , const VkCommandBufferAllocateInfo* pAllocateInfo
-- >     , VkCommandBuffer* pCommandBuffers
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAllocateCommandBuffers vkAllocateCommandBuffers registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myAllocateCommandBuffers <- vkGetDeviceProc @VkAllocateCommandBuffers vkDevice
--
-- or less efficient:
--
-- > myAllocateCommandBuffers <- vkGetProc @VkAllocateCommandBuffers
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkAllocateCommandBuffers"
               vkAllocateCommandBuffersSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkCommandBufferAllocateInfo -- ^ pAllocateInfo
                                                 ->
                   Ptr VkCommandBuffer -- ^ pCommandBuffers
                                       -> IO VkResult

##else
vkAllocateCommandBuffersSafe ::
                             VkDevice -- ^ device
                                      ->
                               Ptr VkCommandBufferAllocateInfo -- ^ pAllocateInfo
                                                               ->
                                 Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                     -> IO VkResult
vkAllocateCommandBuffersSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkAllocateCommandBuffers)

{-# NOINLINE vkAllocateCommandBuffersSafe #-}
##endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAllocateCommandBuffers vkAllocateCommandBuffers registry at www.khronos.org>
type HS_vkAllocateCommandBuffers =
     VkDevice -- ^ device
              ->
       Ptr VkCommandBufferAllocateInfo -- ^ pAllocateInfo
                                       ->
         Ptr VkCommandBuffer -- ^ pCommandBuffers
                             -> IO VkResult

type PFN_vkAllocateCommandBuffers =
     FunPtr HS_vkAllocateCommandBuffers

foreign import ccall unsafe "dynamic"
               unwrapVkAllocateCommandBuffers ::
               PFN_vkAllocateCommandBuffers -> HS_vkAllocateCommandBuffers

foreign import ccall safe "dynamic"
               unwrapVkAllocateCommandBuffersSafe ::
               PFN_vkAllocateCommandBuffers -> HS_vkAllocateCommandBuffers

instance VulkanProc "vkAllocateCommandBuffers" where
        type VkProcType "vkAllocateCommandBuffers" =
             HS_vkAllocateCommandBuffers
        vkProcSymbol = _VkAllocateCommandBuffers

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkAllocateCommandBuffers

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkAllocateCommandBuffersSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- > void vkFreeCommandBuffers
-- >     ( VkDevice device
-- >     , VkCommandPool commandPool
-- >     , uint32_t commandBufferCount
-- >     , const VkCommandBuffer* pCommandBuffers
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkFreeCommandBuffers vkFreeCommandBuffers registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myFreeCommandBuffers <- vkGetDeviceProc @VkFreeCommandBuffers vkDevice
--
-- or less efficient:
--
-- > myFreeCommandBuffers <- vkGetProc @VkFreeCommandBuffers
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkFreeCommandBuffers"
               vkFreeCommandBuffers ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> Word32 -- ^ commandBufferCount
                                                   -> Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                                          -> IO ()

##else
vkFreeCommandBuffers ::
                     VkDevice -- ^ device
                              -> VkCommandPool -- ^ commandPool
                                               -> Word32 -- ^ commandBufferCount
                                                         -> Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                                                -> IO ()
vkFreeCommandBuffers
  = unsafeDupablePerformIO (vkGetProc @VkFreeCommandBuffers)

{-# NOINLINE vkFreeCommandBuffers #-}
##endif

-- |
-- > void vkFreeCommandBuffers
-- >     ( VkDevice device
-- >     , VkCommandPool commandPool
-- >     , uint32_t commandBufferCount
-- >     , const VkCommandBuffer* pCommandBuffers
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkFreeCommandBuffers vkFreeCommandBuffers registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myFreeCommandBuffers <- vkGetDeviceProc @VkFreeCommandBuffers vkDevice
--
-- or less efficient:
--
-- > myFreeCommandBuffers <- vkGetProc @VkFreeCommandBuffers
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkFreeCommandBuffers"
               vkFreeCommandBuffersSafe ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> Word32 -- ^ commandBufferCount
                                                   -> Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                                          -> IO ()

##else
vkFreeCommandBuffersSafe ::
                         VkDevice -- ^ device
                                  -> VkCommandPool -- ^ commandPool
                                                   -> Word32 -- ^ commandBufferCount
                                                             -> Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                                                    -> IO ()
vkFreeCommandBuffersSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkFreeCommandBuffers)

{-# NOINLINE vkFreeCommandBuffersSafe #-}
##endif

-- | > void vkFreeCommandBuffers
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , uint32_t commandBufferCount
--   >     , const VkCommandBuffer* pCommandBuffers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkFreeCommandBuffers vkFreeCommandBuffers registry at www.khronos.org>
type HS_vkFreeCommandBuffers =
     VkDevice -- ^ device
              -> VkCommandPool -- ^ commandPool
                               -> Word32 -- ^ commandBufferCount
                                         -> Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                                -> IO ()

type PFN_vkFreeCommandBuffers = FunPtr HS_vkFreeCommandBuffers

foreign import ccall unsafe "dynamic" unwrapVkFreeCommandBuffers ::
               PFN_vkFreeCommandBuffers -> HS_vkFreeCommandBuffers

foreign import ccall safe "dynamic" unwrapVkFreeCommandBuffersSafe
               :: PFN_vkFreeCommandBuffers -> HS_vkFreeCommandBuffers

instance VulkanProc "vkFreeCommandBuffers" where
        type VkProcType "vkFreeCommandBuffers" = HS_vkFreeCommandBuffers
        vkProcSymbol = _VkFreeCommandBuffers

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkFreeCommandBuffers

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkFreeCommandBuffersSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkBeginCommandBuffer
-- >     ( VkCommandBuffer commandBuffer
-- >     , const VkCommandBufferBeginInfo* pBeginInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBeginCommandBuffer vkBeginCommandBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myBeginCommandBuffer <- vkGetInstanceProc @VkBeginCommandBuffer vkInstance
--
-- or less efficient:
--
-- > myBeginCommandBuffer <- vkGetProc @VkBeginCommandBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkBeginCommandBuffer"
               vkBeginCommandBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkCommandBufferBeginInfo -- ^ pBeginInfo
                                                               -> IO VkResult

##else
vkBeginCommandBuffer ::
                     VkCommandBuffer -- ^ commandBuffer
                                     -> Ptr VkCommandBufferBeginInfo -- ^ pBeginInfo
                                                                     -> IO VkResult
vkBeginCommandBuffer
  = unsafeDupablePerformIO (vkGetProc @VkBeginCommandBuffer)

{-# NOINLINE vkBeginCommandBuffer #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkBeginCommandBuffer
-- >     ( VkCommandBuffer commandBuffer
-- >     , const VkCommandBufferBeginInfo* pBeginInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBeginCommandBuffer vkBeginCommandBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myBeginCommandBuffer <- vkGetInstanceProc @VkBeginCommandBuffer vkInstance
--
-- or less efficient:
--
-- > myBeginCommandBuffer <- vkGetProc @VkBeginCommandBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkBeginCommandBuffer"
               vkBeginCommandBufferSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkCommandBufferBeginInfo -- ^ pBeginInfo
                                                               -> IO VkResult

##else
vkBeginCommandBufferSafe ::
                         VkCommandBuffer -- ^ commandBuffer
                                         -> Ptr VkCommandBufferBeginInfo -- ^ pBeginInfo
                                                                         -> IO VkResult
vkBeginCommandBufferSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkBeginCommandBuffer)

{-# NOINLINE vkBeginCommandBufferSafe #-}
##endif

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBeginCommandBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCommandBufferBeginInfo* pBeginInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkBeginCommandBuffer vkBeginCommandBuffer registry at www.khronos.org>
type HS_vkBeginCommandBuffer =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr VkCommandBufferBeginInfo -- ^ pBeginInfo
                                                     -> IO VkResult

type PFN_vkBeginCommandBuffer = FunPtr HS_vkBeginCommandBuffer

foreign import ccall unsafe "dynamic" unwrapVkBeginCommandBuffer ::
               PFN_vkBeginCommandBuffer -> HS_vkBeginCommandBuffer

foreign import ccall safe "dynamic" unwrapVkBeginCommandBufferSafe
               :: PFN_vkBeginCommandBuffer -> HS_vkBeginCommandBuffer

instance VulkanProc "vkBeginCommandBuffer" where
        type VkProcType "vkBeginCommandBuffer" = HS_vkBeginCommandBuffer
        vkProcSymbol = _VkBeginCommandBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkBeginCommandBuffer

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkBeginCommandBufferSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkEndCommandBuffer
-- >     ( VkCommandBuffer commandBuffer
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEndCommandBuffer vkEndCommandBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEndCommandBuffer <- vkGetInstanceProc @VkEndCommandBuffer vkInstance
--
-- or less efficient:
--
-- > myEndCommandBuffer <- vkGetProc @VkEndCommandBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkEndCommandBuffer" vkEndCommandBuffer
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> IO VkResult

##else
vkEndCommandBuffer :: VkCommandBuffer -- ^ commandBuffer
                                      -> IO VkResult
vkEndCommandBuffer
  = unsafeDupablePerformIO (vkGetProc @VkEndCommandBuffer)

{-# NOINLINE vkEndCommandBuffer #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkEndCommandBuffer
-- >     ( VkCommandBuffer commandBuffer
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEndCommandBuffer vkEndCommandBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEndCommandBuffer <- vkGetInstanceProc @VkEndCommandBuffer vkInstance
--
-- or less efficient:
--
-- > myEndCommandBuffer <- vkGetProc @VkEndCommandBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkEndCommandBuffer"
               vkEndCommandBufferSafe :: VkCommandBuffer -- ^ commandBuffer
                                                         -> IO VkResult

##else
vkEndCommandBufferSafe :: VkCommandBuffer -- ^ commandBuffer
                                          -> IO VkResult
vkEndCommandBufferSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkEndCommandBuffer)

{-# NOINLINE vkEndCommandBufferSafe #-}
##endif

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkEndCommandBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEndCommandBuffer vkEndCommandBuffer registry at www.khronos.org>
type HS_vkEndCommandBuffer = VkCommandBuffer -- ^ commandBuffer
                                             -> IO VkResult

type PFN_vkEndCommandBuffer = FunPtr HS_vkEndCommandBuffer

foreign import ccall unsafe "dynamic" unwrapVkEndCommandBuffer ::
               PFN_vkEndCommandBuffer -> HS_vkEndCommandBuffer

foreign import ccall safe "dynamic" unwrapVkEndCommandBufferSafe ::
               PFN_vkEndCommandBuffer -> HS_vkEndCommandBuffer

instance VulkanProc "vkEndCommandBuffer" where
        type VkProcType "vkEndCommandBuffer" = HS_vkEndCommandBuffer
        vkProcSymbol = _VkEndCommandBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkEndCommandBuffer

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkEndCommandBufferSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkResetCommandBuffer
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkCommandBufferResetFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkResetCommandBuffer vkResetCommandBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myResetCommandBuffer <- vkGetInstanceProc @VkResetCommandBuffer vkInstance
--
-- or less efficient:
--
-- > myResetCommandBuffer <- vkGetProc @VkResetCommandBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkResetCommandBuffer"
               vkResetCommandBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkCommandBufferResetFlags -- ^ flags
                                                            -> IO VkResult

##else
vkResetCommandBuffer ::
                     VkCommandBuffer -- ^ commandBuffer
                                     -> VkCommandBufferResetFlags -- ^ flags
                                                                  -> IO VkResult
vkResetCommandBuffer
  = unsafeDupablePerformIO (vkGetProc @VkResetCommandBuffer)

{-# NOINLINE vkResetCommandBuffer #-}
##endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkResetCommandBuffer
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkCommandBufferResetFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkResetCommandBuffer vkResetCommandBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myResetCommandBuffer <- vkGetInstanceProc @VkResetCommandBuffer vkInstance
--
-- or less efficient:
--
-- > myResetCommandBuffer <- vkGetProc @VkResetCommandBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkResetCommandBuffer"
               vkResetCommandBufferSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkCommandBufferResetFlags -- ^ flags
                                                            -> IO VkResult

##else
vkResetCommandBufferSafe ::
                         VkCommandBuffer -- ^ commandBuffer
                                         -> VkCommandBufferResetFlags -- ^ flags
                                                                      -> IO VkResult
vkResetCommandBufferSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkResetCommandBuffer)

{-# NOINLINE vkResetCommandBufferSafe #-}
##endif

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetCommandBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkCommandBufferResetFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkResetCommandBuffer vkResetCommandBuffer registry at www.khronos.org>
type HS_vkResetCommandBuffer =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkCommandBufferResetFlags -- ^ flags
                                                  -> IO VkResult

type PFN_vkResetCommandBuffer = FunPtr HS_vkResetCommandBuffer

foreign import ccall unsafe "dynamic" unwrapVkResetCommandBuffer ::
               PFN_vkResetCommandBuffer -> HS_vkResetCommandBuffer

foreign import ccall safe "dynamic" unwrapVkResetCommandBufferSafe
               :: PFN_vkResetCommandBuffer -> HS_vkResetCommandBuffer

instance VulkanProc "vkResetCommandBuffer" where
        type VkProcType "vkResetCommandBuffer" = HS_vkResetCommandBuffer
        vkProcSymbol = _VkResetCommandBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkResetCommandBuffer

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkResetCommandBufferSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- > void vkCmdBindPipeline
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkPipelineBindPoint pipelineBindPoint
-- >     , VkPipeline pipeline
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBindPipeline vkCmdBindPipeline registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBindPipeline <- vkGetInstanceProc @VkCmdBindPipeline vkInstance
--
-- or less efficient:
--
-- > myCmdBindPipeline <- vkGetProc @VkCmdBindPipeline
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdBindPipeline" vkCmdBindPipeline
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> VkPipelineBindPoint -- ^ pipelineBindPoint
                                                         -> VkPipeline -- ^ pipeline
                                                                       -> IO ()

##else
vkCmdBindPipeline ::
                  VkCommandBuffer -- ^ commandBuffer
                                  -> VkPipelineBindPoint -- ^ pipelineBindPoint
                                                         -> VkPipeline -- ^ pipeline
                                                                       -> IO ()
vkCmdBindPipeline
  = unsafeDupablePerformIO (vkGetProc @VkCmdBindPipeline)

{-# NOINLINE vkCmdBindPipeline #-}
##endif

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- > void vkCmdBindPipeline
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkPipelineBindPoint pipelineBindPoint
-- >     , VkPipeline pipeline
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBindPipeline vkCmdBindPipeline registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBindPipeline <- vkGetInstanceProc @VkCmdBindPipeline vkInstance
--
-- or less efficient:
--
-- > myCmdBindPipeline <- vkGetProc @VkCmdBindPipeline
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdBindPipeline" vkCmdBindPipelineSafe
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> VkPipelineBindPoint -- ^ pipelineBindPoint
                                                         -> VkPipeline -- ^ pipeline
                                                                       -> IO ()

##else
vkCmdBindPipelineSafe ::
                      VkCommandBuffer -- ^ commandBuffer
                                      -> VkPipelineBindPoint -- ^ pipelineBindPoint
                                                             -> VkPipeline -- ^ pipeline
                                                                           -> IO ()
vkCmdBindPipelineSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdBindPipeline)

{-# NOINLINE vkCmdBindPipelineSafe #-}
##endif

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdBindPipeline
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineBindPoint pipelineBindPoint
--   >     , VkPipeline pipeline
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBindPipeline vkCmdBindPipeline registry at www.khronos.org>
type HS_vkCmdBindPipeline =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkPipelineBindPoint -- ^ pipelineBindPoint
                                            -> VkPipeline -- ^ pipeline
                                                          -> IO ()

type PFN_vkCmdBindPipeline = FunPtr HS_vkCmdBindPipeline

foreign import ccall unsafe "dynamic" unwrapVkCmdBindPipeline ::
               PFN_vkCmdBindPipeline -> HS_vkCmdBindPipeline

foreign import ccall safe "dynamic" unwrapVkCmdBindPipelineSafe ::
               PFN_vkCmdBindPipeline -> HS_vkCmdBindPipeline

instance VulkanProc "vkCmdBindPipeline" where
        type VkProcType "vkCmdBindPipeline" = HS_vkCmdBindPipeline
        vkProcSymbol = _VkCmdBindPipeline

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdBindPipeline

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdBindPipelineSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetViewport
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t firstViewport
-- >     , uint32_t viewportCount
-- >     , const VkViewport* pViewports
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetViewport vkCmdSetViewport registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetViewport <- vkGetInstanceProc @VkCmdSetViewport vkInstance
--
-- or less efficient:
--
-- > myCmdSetViewport <- vkGetProc @VkCmdSetViewport
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdSetViewport" vkCmdSetViewport ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ firstViewport
                                         -> Word32 -- ^ viewportCount
                                                   -> Ptr VkViewport -- ^ pViewports
                                                                     -> IO ()

##else
vkCmdSetViewport ::
                 VkCommandBuffer -- ^ commandBuffer
                                 -> Word32 -- ^ firstViewport
                                           -> Word32 -- ^ viewportCount
                                                     -> Ptr VkViewport -- ^ pViewports
                                                                       -> IO ()
vkCmdSetViewport
  = unsafeDupablePerformIO (vkGetProc @VkCmdSetViewport)

{-# NOINLINE vkCmdSetViewport #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetViewport
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t firstViewport
-- >     , uint32_t viewportCount
-- >     , const VkViewport* pViewports
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetViewport vkCmdSetViewport registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetViewport <- vkGetInstanceProc @VkCmdSetViewport vkInstance
--
-- or less efficient:
--
-- > myCmdSetViewport <- vkGetProc @VkCmdSetViewport
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdSetViewport" vkCmdSetViewportSafe
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> Word32 -- ^ firstViewport
                                            -> Word32 -- ^ viewportCount
                                                      -> Ptr VkViewport -- ^ pViewports
                                                                        -> IO ()

##else
vkCmdSetViewportSafe ::
                     VkCommandBuffer -- ^ commandBuffer
                                     -> Word32 -- ^ firstViewport
                                               -> Word32 -- ^ viewportCount
                                                         -> Ptr VkViewport -- ^ pViewports
                                                                           -> IO ()
vkCmdSetViewportSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdSetViewport)

{-# NOINLINE vkCmdSetViewportSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetViewport
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstViewport
--   >     , uint32_t viewportCount
--   >     , const VkViewport* pViewports
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetViewport vkCmdSetViewport registry at www.khronos.org>
type HS_vkCmdSetViewport =
     VkCommandBuffer -- ^ commandBuffer
                     -> Word32 -- ^ firstViewport
                               -> Word32 -- ^ viewportCount
                                         -> Ptr VkViewport -- ^ pViewports
                                                           -> IO ()

type PFN_vkCmdSetViewport = FunPtr HS_vkCmdSetViewport

foreign import ccall unsafe "dynamic" unwrapVkCmdSetViewport ::
               PFN_vkCmdSetViewport -> HS_vkCmdSetViewport

foreign import ccall safe "dynamic" unwrapVkCmdSetViewportSafe ::
               PFN_vkCmdSetViewport -> HS_vkCmdSetViewport

instance VulkanProc "vkCmdSetViewport" where
        type VkProcType "vkCmdSetViewport" = HS_vkCmdSetViewport
        vkProcSymbol = _VkCmdSetViewport

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetViewport

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdSetViewportSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetScissor
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t firstScissor
-- >     , uint32_t scissorCount
-- >     , const VkRect2D* pScissors
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetScissor vkCmdSetScissor registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetScissor <- vkGetInstanceProc @VkCmdSetScissor vkInstance
--
-- or less efficient:
--
-- > myCmdSetScissor <- vkGetProc @VkCmdSetScissor
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdSetScissor" vkCmdSetScissor ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ firstScissor
                                         -> Word32 -- ^ scissorCount
                                                   -> Ptr VkRect2D -- ^ pScissors
                                                                   -> IO ()

##else
vkCmdSetScissor ::
                VkCommandBuffer -- ^ commandBuffer
                                -> Word32 -- ^ firstScissor
                                          -> Word32 -- ^ scissorCount
                                                    -> Ptr VkRect2D -- ^ pScissors
                                                                    -> IO ()
vkCmdSetScissor
  = unsafeDupablePerformIO (vkGetProc @VkCmdSetScissor)

{-# NOINLINE vkCmdSetScissor #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetScissor
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t firstScissor
-- >     , uint32_t scissorCount
-- >     , const VkRect2D* pScissors
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetScissor vkCmdSetScissor registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetScissor <- vkGetInstanceProc @VkCmdSetScissor vkInstance
--
-- or less efficient:
--
-- > myCmdSetScissor <- vkGetProc @VkCmdSetScissor
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdSetScissor" vkCmdSetScissorSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ firstScissor
                                         -> Word32 -- ^ scissorCount
                                                   -> Ptr VkRect2D -- ^ pScissors
                                                                   -> IO ()

##else
vkCmdSetScissorSafe ::
                    VkCommandBuffer -- ^ commandBuffer
                                    -> Word32 -- ^ firstScissor
                                              -> Word32 -- ^ scissorCount
                                                        -> Ptr VkRect2D -- ^ pScissors
                                                                        -> IO ()
vkCmdSetScissorSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdSetScissor)

{-# NOINLINE vkCmdSetScissorSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetScissor
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstScissor
--   >     , uint32_t scissorCount
--   >     , const VkRect2D* pScissors
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetScissor vkCmdSetScissor registry at www.khronos.org>
type HS_vkCmdSetScissor =
     VkCommandBuffer -- ^ commandBuffer
                     -> Word32 -- ^ firstScissor
                               -> Word32 -- ^ scissorCount
                                         -> Ptr VkRect2D -- ^ pScissors
                                                         -> IO ()

type PFN_vkCmdSetScissor = FunPtr HS_vkCmdSetScissor

foreign import ccall unsafe "dynamic" unwrapVkCmdSetScissor ::
               PFN_vkCmdSetScissor -> HS_vkCmdSetScissor

foreign import ccall safe "dynamic" unwrapVkCmdSetScissorSafe ::
               PFN_vkCmdSetScissor -> HS_vkCmdSetScissor

instance VulkanProc "vkCmdSetScissor" where
        type VkProcType "vkCmdSetScissor" = HS_vkCmdSetScissor
        vkProcSymbol = _VkCmdSetScissor

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetScissor

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdSetScissorSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetLineWidth
-- >     ( VkCommandBuffer commandBuffer
-- >     , float lineWidth
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetLineWidth vkCmdSetLineWidth registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetLineWidth <- vkGetInstanceProc @VkCmdSetLineWidth vkInstance
--
-- or less efficient:
--
-- > myCmdSetLineWidth <- vkGetProc @VkCmdSetLineWidth
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdSetLineWidth" vkCmdSetLineWidth
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> #{type float} -> IO ()

##else
vkCmdSetLineWidth ::
                  VkCommandBuffer -- ^ commandBuffer
                                  -> #{type float} -> IO ()
vkCmdSetLineWidth
  = unsafeDupablePerformIO (vkGetProc @VkCmdSetLineWidth)

{-# NOINLINE vkCmdSetLineWidth #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetLineWidth
-- >     ( VkCommandBuffer commandBuffer
-- >     , float lineWidth
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetLineWidth vkCmdSetLineWidth registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetLineWidth <- vkGetInstanceProc @VkCmdSetLineWidth vkInstance
--
-- or less efficient:
--
-- > myCmdSetLineWidth <- vkGetProc @VkCmdSetLineWidth
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdSetLineWidth" vkCmdSetLineWidthSafe
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> #{type float} -> IO ()

##else
vkCmdSetLineWidthSafe ::
                      VkCommandBuffer -- ^ commandBuffer
                                      -> #{type float} -> IO ()
vkCmdSetLineWidthSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdSetLineWidth)

{-# NOINLINE vkCmdSetLineWidthSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetLineWidth
--   >     ( VkCommandBuffer commandBuffer
--   >     , float lineWidth
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetLineWidth vkCmdSetLineWidth registry at www.khronos.org>
type HS_vkCmdSetLineWidth =
     VkCommandBuffer -- ^ commandBuffer
                     -> #{type float} -> IO ()

type PFN_vkCmdSetLineWidth = FunPtr HS_vkCmdSetLineWidth

foreign import ccall unsafe "dynamic" unwrapVkCmdSetLineWidth ::
               PFN_vkCmdSetLineWidth -> HS_vkCmdSetLineWidth

foreign import ccall safe "dynamic" unwrapVkCmdSetLineWidthSafe ::
               PFN_vkCmdSetLineWidth -> HS_vkCmdSetLineWidth

instance VulkanProc "vkCmdSetLineWidth" where
        type VkProcType "vkCmdSetLineWidth" = HS_vkCmdSetLineWidth
        vkProcSymbol = _VkCmdSetLineWidth

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetLineWidth

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdSetLineWidthSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetDepthBias
-- >     ( VkCommandBuffer commandBuffer
-- >     , float depthBiasConstantFactor
-- >     , float depthBiasClamp
-- >     , float depthBiasSlopeFactor
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetDepthBias vkCmdSetDepthBias registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetDepthBias <- vkGetInstanceProc @VkCmdSetDepthBias vkInstance
--
-- or less efficient:
--
-- > myCmdSetDepthBias <- vkGetProc @VkCmdSetDepthBias
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdSetDepthBias" vkCmdSetDepthBias
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 #{type float} ->
                   #{type float} -> #{type float} -> IO ()

##else
vkCmdSetDepthBias ::
                  VkCommandBuffer -- ^ commandBuffer
                                  ->
                    #{type float} ->
                      #{type float} -> #{type float} -> IO ()
vkCmdSetDepthBias
  = unsafeDupablePerformIO (vkGetProc @VkCmdSetDepthBias)

{-# NOINLINE vkCmdSetDepthBias #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetDepthBias
-- >     ( VkCommandBuffer commandBuffer
-- >     , float depthBiasConstantFactor
-- >     , float depthBiasClamp
-- >     , float depthBiasSlopeFactor
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetDepthBias vkCmdSetDepthBias registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetDepthBias <- vkGetInstanceProc @VkCmdSetDepthBias vkInstance
--
-- or less efficient:
--
-- > myCmdSetDepthBias <- vkGetProc @VkCmdSetDepthBias
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdSetDepthBias" vkCmdSetDepthBiasSafe
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 #{type float} ->
                   #{type float} -> #{type float} -> IO ()

##else
vkCmdSetDepthBiasSafe ::
                      VkCommandBuffer -- ^ commandBuffer
                                      ->
                        #{type float} ->
                          #{type float} -> #{type float} -> IO ()
vkCmdSetDepthBiasSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdSetDepthBias)

{-# NOINLINE vkCmdSetDepthBiasSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetDepthBias
--   >     ( VkCommandBuffer commandBuffer
--   >     , float depthBiasConstantFactor
--   >     , float depthBiasClamp
--   >     , float depthBiasSlopeFactor
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetDepthBias vkCmdSetDepthBias registry at www.khronos.org>
type HS_vkCmdSetDepthBias =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       #{type float} ->
         #{type float} -> #{type float} -> IO ()

type PFN_vkCmdSetDepthBias = FunPtr HS_vkCmdSetDepthBias

foreign import ccall unsafe "dynamic" unwrapVkCmdSetDepthBias ::
               PFN_vkCmdSetDepthBias -> HS_vkCmdSetDepthBias

foreign import ccall safe "dynamic" unwrapVkCmdSetDepthBiasSafe ::
               PFN_vkCmdSetDepthBias -> HS_vkCmdSetDepthBias

instance VulkanProc "vkCmdSetDepthBias" where
        type VkProcType "vkCmdSetDepthBias" = HS_vkCmdSetDepthBias
        vkProcSymbol = _VkCmdSetDepthBias

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetDepthBias

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdSetDepthBiasSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetBlendConstants
-- >     ( VkCommandBuffer commandBuffer
-- >     , const float blendConstants[4]
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetBlendConstants vkCmdSetBlendConstants registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetBlendConstants <- vkGetInstanceProc @VkCmdSetBlendConstants vkInstance
--
-- or less efficient:
--
-- > myCmdSetBlendConstants <- vkGetProc @VkCmdSetBlendConstants
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdSetBlendConstants"
               vkCmdSetBlendConstants ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr #{type float} -- ^ blendConstants
                                                                -> IO ()

##else
vkCmdSetBlendConstants ::
                       VkCommandBuffer -- ^ commandBuffer
                                       -> Ptr #{type float} -- ^ blendConstants
                                                                        -> IO ()
vkCmdSetBlendConstants
  = unsafeDupablePerformIO (vkGetProc @VkCmdSetBlendConstants)

{-# NOINLINE vkCmdSetBlendConstants #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetBlendConstants
-- >     ( VkCommandBuffer commandBuffer
-- >     , const float blendConstants[4]
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetBlendConstants vkCmdSetBlendConstants registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetBlendConstants <- vkGetInstanceProc @VkCmdSetBlendConstants vkInstance
--
-- or less efficient:
--
-- > myCmdSetBlendConstants <- vkGetProc @VkCmdSetBlendConstants
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdSetBlendConstants"
               vkCmdSetBlendConstantsSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr #{type float} -- ^ blendConstants
                                                                -> IO ()

##else
vkCmdSetBlendConstantsSafe ::
                           VkCommandBuffer -- ^ commandBuffer
                                           -> Ptr #{type float} -- ^ blendConstants
                                                                            -> IO ()
vkCmdSetBlendConstantsSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdSetBlendConstants)

{-# NOINLINE vkCmdSetBlendConstantsSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetBlendConstants
--   >     ( VkCommandBuffer commandBuffer
--   >     , const float blendConstants[4]
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetBlendConstants vkCmdSetBlendConstants registry at www.khronos.org>
type HS_vkCmdSetBlendConstants =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr #{type float} -- ^ blendConstants
                                                      -> IO ()

type PFN_vkCmdSetBlendConstants = FunPtr HS_vkCmdSetBlendConstants

foreign import ccall unsafe "dynamic" unwrapVkCmdSetBlendConstants
               :: PFN_vkCmdSetBlendConstants -> HS_vkCmdSetBlendConstants

foreign import ccall safe "dynamic"
               unwrapVkCmdSetBlendConstantsSafe ::
               PFN_vkCmdSetBlendConstants -> HS_vkCmdSetBlendConstants

instance VulkanProc "vkCmdSetBlendConstants" where
        type VkProcType "vkCmdSetBlendConstants" =
             HS_vkCmdSetBlendConstants
        vkProcSymbol = _VkCmdSetBlendConstants

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetBlendConstants

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdSetBlendConstantsSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetDepthBounds
-- >     ( VkCommandBuffer commandBuffer
-- >     , float minDepthBounds
-- >     , float maxDepthBounds
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetDepthBounds vkCmdSetDepthBounds registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetDepthBounds <- vkGetInstanceProc @VkCmdSetDepthBounds vkInstance
--
-- or less efficient:
--
-- > myCmdSetDepthBounds <- vkGetProc @VkCmdSetDepthBounds
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdSetDepthBounds"
               vkCmdSetDepthBounds ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 #{type float} -> #{type float} -> IO ()

##else
vkCmdSetDepthBounds ::
                    VkCommandBuffer -- ^ commandBuffer
                                    ->
                      #{type float} -> #{type float} -> IO ()
vkCmdSetDepthBounds
  = unsafeDupablePerformIO (vkGetProc @VkCmdSetDepthBounds)

{-# NOINLINE vkCmdSetDepthBounds #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetDepthBounds
-- >     ( VkCommandBuffer commandBuffer
-- >     , float minDepthBounds
-- >     , float maxDepthBounds
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetDepthBounds vkCmdSetDepthBounds registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetDepthBounds <- vkGetInstanceProc @VkCmdSetDepthBounds vkInstance
--
-- or less efficient:
--
-- > myCmdSetDepthBounds <- vkGetProc @VkCmdSetDepthBounds
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdSetDepthBounds"
               vkCmdSetDepthBoundsSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 #{type float} -> #{type float} -> IO ()

##else
vkCmdSetDepthBoundsSafe ::
                        VkCommandBuffer -- ^ commandBuffer
                                        ->
                          #{type float} -> #{type float} -> IO ()
vkCmdSetDepthBoundsSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdSetDepthBounds)

{-# NOINLINE vkCmdSetDepthBoundsSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetDepthBounds
--   >     ( VkCommandBuffer commandBuffer
--   >     , float minDepthBounds
--   >     , float maxDepthBounds
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetDepthBounds vkCmdSetDepthBounds registry at www.khronos.org>
type HS_vkCmdSetDepthBounds =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       #{type float} -> #{type float} -> IO ()

type PFN_vkCmdSetDepthBounds = FunPtr HS_vkCmdSetDepthBounds

foreign import ccall unsafe "dynamic" unwrapVkCmdSetDepthBounds ::
               PFN_vkCmdSetDepthBounds -> HS_vkCmdSetDepthBounds

foreign import ccall safe "dynamic" unwrapVkCmdSetDepthBoundsSafe
               :: PFN_vkCmdSetDepthBounds -> HS_vkCmdSetDepthBounds

instance VulkanProc "vkCmdSetDepthBounds" where
        type VkProcType "vkCmdSetDepthBounds" = HS_vkCmdSetDepthBounds
        vkProcSymbol = _VkCmdSetDepthBounds

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetDepthBounds

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdSetDepthBoundsSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetStencilCompareMask
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkStencilFaceFlags faceMask
-- >     , uint32_t compareMask
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetStencilCompareMask vkCmdSetStencilCompareMask registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetStencilCompareMask <- vkGetInstanceProc @VkCmdSetStencilCompareMask vkInstance
--
-- or less efficient:
--
-- > myCmdSetStencilCompareMask <- vkGetProc @VkCmdSetStencilCompareMask
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdSetStencilCompareMask"
               vkCmdSetStencilCompareMask ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkStencilFaceFlags -- ^ faceMask
                                                     -> Word32 -- ^ compareMask
                                                               -> IO ()

##else
vkCmdSetStencilCompareMask ::
                           VkCommandBuffer -- ^ commandBuffer
                                           -> VkStencilFaceFlags -- ^ faceMask
                                                                 -> Word32 -- ^ compareMask
                                                                           -> IO ()
vkCmdSetStencilCompareMask
  = unsafeDupablePerformIO (vkGetProc @VkCmdSetStencilCompareMask)

{-# NOINLINE vkCmdSetStencilCompareMask #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetStencilCompareMask
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkStencilFaceFlags faceMask
-- >     , uint32_t compareMask
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetStencilCompareMask vkCmdSetStencilCompareMask registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetStencilCompareMask <- vkGetInstanceProc @VkCmdSetStencilCompareMask vkInstance
--
-- or less efficient:
--
-- > myCmdSetStencilCompareMask <- vkGetProc @VkCmdSetStencilCompareMask
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdSetStencilCompareMask"
               vkCmdSetStencilCompareMaskSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkStencilFaceFlags -- ^ faceMask
                                                     -> Word32 -- ^ compareMask
                                                               -> IO ()

##else
vkCmdSetStencilCompareMaskSafe ::
                               VkCommandBuffer -- ^ commandBuffer
                                               -> VkStencilFaceFlags -- ^ faceMask
                                                                     -> Word32 -- ^ compareMask
                                                                               -> IO ()
vkCmdSetStencilCompareMaskSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkCmdSetStencilCompareMask)

{-# NOINLINE vkCmdSetStencilCompareMaskSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetStencilCompareMask
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkStencilFaceFlags faceMask
--   >     , uint32_t compareMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetStencilCompareMask vkCmdSetStencilCompareMask registry at www.khronos.org>
type HS_vkCmdSetStencilCompareMask =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkStencilFaceFlags -- ^ faceMask
                                           -> Word32 -- ^ compareMask
                                                     -> IO ()

type PFN_vkCmdSetStencilCompareMask =
     FunPtr HS_vkCmdSetStencilCompareMask

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetStencilCompareMask ::
               PFN_vkCmdSetStencilCompareMask -> HS_vkCmdSetStencilCompareMask

foreign import ccall safe "dynamic"
               unwrapVkCmdSetStencilCompareMaskSafe ::
               PFN_vkCmdSetStencilCompareMask -> HS_vkCmdSetStencilCompareMask

instance VulkanProc "vkCmdSetStencilCompareMask" where
        type VkProcType "vkCmdSetStencilCompareMask" =
             HS_vkCmdSetStencilCompareMask
        vkProcSymbol = _VkCmdSetStencilCompareMask

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetStencilCompareMask

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdSetStencilCompareMaskSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetStencilWriteMask
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkStencilFaceFlags faceMask
-- >     , uint32_t writeMask
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetStencilWriteMask vkCmdSetStencilWriteMask registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetStencilWriteMask <- vkGetInstanceProc @VkCmdSetStencilWriteMask vkInstance
--
-- or less efficient:
--
-- > myCmdSetStencilWriteMask <- vkGetProc @VkCmdSetStencilWriteMask
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdSetStencilWriteMask"
               vkCmdSetStencilWriteMask ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkStencilFaceFlags -- ^ faceMask
                                                     -> Word32 -- ^ writeMask
                                                               -> IO ()

##else
vkCmdSetStencilWriteMask ::
                         VkCommandBuffer -- ^ commandBuffer
                                         -> VkStencilFaceFlags -- ^ faceMask
                                                               -> Word32 -- ^ writeMask
                                                                         -> IO ()
vkCmdSetStencilWriteMask
  = unsafeDupablePerformIO (vkGetProc @VkCmdSetStencilWriteMask)

{-# NOINLINE vkCmdSetStencilWriteMask #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetStencilWriteMask
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkStencilFaceFlags faceMask
-- >     , uint32_t writeMask
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetStencilWriteMask vkCmdSetStencilWriteMask registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetStencilWriteMask <- vkGetInstanceProc @VkCmdSetStencilWriteMask vkInstance
--
-- or less efficient:
--
-- > myCmdSetStencilWriteMask <- vkGetProc @VkCmdSetStencilWriteMask
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdSetStencilWriteMask"
               vkCmdSetStencilWriteMaskSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkStencilFaceFlags -- ^ faceMask
                                                     -> Word32 -- ^ writeMask
                                                               -> IO ()

##else
vkCmdSetStencilWriteMaskSafe ::
                             VkCommandBuffer -- ^ commandBuffer
                                             -> VkStencilFaceFlags -- ^ faceMask
                                                                   -> Word32 -- ^ writeMask
                                                                             -> IO ()
vkCmdSetStencilWriteMaskSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdSetStencilWriteMask)

{-# NOINLINE vkCmdSetStencilWriteMaskSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetStencilWriteMask
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkStencilFaceFlags faceMask
--   >     , uint32_t writeMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetStencilWriteMask vkCmdSetStencilWriteMask registry at www.khronos.org>
type HS_vkCmdSetStencilWriteMask =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkStencilFaceFlags -- ^ faceMask
                                           -> Word32 -- ^ writeMask
                                                     -> IO ()

type PFN_vkCmdSetStencilWriteMask =
     FunPtr HS_vkCmdSetStencilWriteMask

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetStencilWriteMask ::
               PFN_vkCmdSetStencilWriteMask -> HS_vkCmdSetStencilWriteMask

foreign import ccall safe "dynamic"
               unwrapVkCmdSetStencilWriteMaskSafe ::
               PFN_vkCmdSetStencilWriteMask -> HS_vkCmdSetStencilWriteMask

instance VulkanProc "vkCmdSetStencilWriteMask" where
        type VkProcType "vkCmdSetStencilWriteMask" =
             HS_vkCmdSetStencilWriteMask
        vkProcSymbol = _VkCmdSetStencilWriteMask

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetStencilWriteMask

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdSetStencilWriteMaskSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetStencilReference
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkStencilFaceFlags faceMask
-- >     , uint32_t reference
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetStencilReference vkCmdSetStencilReference registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetStencilReference <- vkGetInstanceProc @VkCmdSetStencilReference vkInstance
--
-- or less efficient:
--
-- > myCmdSetStencilReference <- vkGetProc @VkCmdSetStencilReference
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdSetStencilReference"
               vkCmdSetStencilReference ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkStencilFaceFlags -- ^ faceMask
                                                     -> Word32 -- ^ reference
                                                               -> IO ()

##else
vkCmdSetStencilReference ::
                         VkCommandBuffer -- ^ commandBuffer
                                         -> VkStencilFaceFlags -- ^ faceMask
                                                               -> Word32 -- ^ reference
                                                                         -> IO ()
vkCmdSetStencilReference
  = unsafeDupablePerformIO (vkGetProc @VkCmdSetStencilReference)

{-# NOINLINE vkCmdSetStencilReference #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdSetStencilReference
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkStencilFaceFlags faceMask
-- >     , uint32_t reference
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetStencilReference vkCmdSetStencilReference registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetStencilReference <- vkGetInstanceProc @VkCmdSetStencilReference vkInstance
--
-- or less efficient:
--
-- > myCmdSetStencilReference <- vkGetProc @VkCmdSetStencilReference
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdSetStencilReference"
               vkCmdSetStencilReferenceSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkStencilFaceFlags -- ^ faceMask
                                                     -> Word32 -- ^ reference
                                                               -> IO ()

##else
vkCmdSetStencilReferenceSafe ::
                             VkCommandBuffer -- ^ commandBuffer
                                             -> VkStencilFaceFlags -- ^ faceMask
                                                                   -> Word32 -- ^ reference
                                                                             -> IO ()
vkCmdSetStencilReferenceSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdSetStencilReference)

{-# NOINLINE vkCmdSetStencilReferenceSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetStencilReference
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkStencilFaceFlags faceMask
--   >     , uint32_t reference
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetStencilReference vkCmdSetStencilReference registry at www.khronos.org>
type HS_vkCmdSetStencilReference =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkStencilFaceFlags -- ^ faceMask
                                           -> Word32 -- ^ reference
                                                     -> IO ()

type PFN_vkCmdSetStencilReference =
     FunPtr HS_vkCmdSetStencilReference

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetStencilReference ::
               PFN_vkCmdSetStencilReference -> HS_vkCmdSetStencilReference

foreign import ccall safe "dynamic"
               unwrapVkCmdSetStencilReferenceSafe ::
               PFN_vkCmdSetStencilReference -> HS_vkCmdSetStencilReference

instance VulkanProc "vkCmdSetStencilReference" where
        type VkProcType "vkCmdSetStencilReference" =
             HS_vkCmdSetStencilReference
        vkProcSymbol = _VkCmdSetStencilReference

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetStencilReference

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdSetStencilReferenceSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- > void vkCmdBindDescriptorSets
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkPipelineBindPoint pipelineBindPoint
-- >     , VkPipelineLayout layout
-- >     , uint32_t firstSet
-- >     , uint32_t descriptorSetCount
-- >     , const VkDescriptorSet* pDescriptorSets
-- >     , uint32_t dynamicOffsetCount
-- >     , const uint32_t* pDynamicOffsets
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBindDescriptorSets vkCmdBindDescriptorSets registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBindDescriptorSets <- vkGetInstanceProc @VkCmdBindDescriptorSets vkInstance
--
-- or less efficient:
--
-- > myCmdBindDescriptorSets <- vkGetProc @VkCmdBindDescriptorSets
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCmdBindDescriptorSets
  = unsafeDupablePerformIO (vkGetProc @VkCmdBindDescriptorSets)

{-# NOINLINE vkCmdBindDescriptorSets #-}
##endif

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- > void vkCmdBindDescriptorSets
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkPipelineBindPoint pipelineBindPoint
-- >     , VkPipelineLayout layout
-- >     , uint32_t firstSet
-- >     , uint32_t descriptorSetCount
-- >     , const VkDescriptorSet* pDescriptorSets
-- >     , uint32_t dynamicOffsetCount
-- >     , const uint32_t* pDynamicOffsets
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBindDescriptorSets vkCmdBindDescriptorSets registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBindDescriptorSets <- vkGetInstanceProc @VkCmdBindDescriptorSets vkInstance
--
-- or less efficient:
--
-- > myCmdBindDescriptorSets <- vkGetProc @VkCmdBindDescriptorSets
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCmdBindDescriptorSetsSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdBindDescriptorSets)

{-# NOINLINE vkCmdBindDescriptorSetsSafe #-}
##endif

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdBindDescriptorSets
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBindDescriptorSets vkCmdBindDescriptorSets registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic" unwrapVkCmdBindDescriptorSets
               :: PFN_vkCmdBindDescriptorSets -> HS_vkCmdBindDescriptorSets

foreign import ccall safe "dynamic"
               unwrapVkCmdBindDescriptorSetsSafe ::
               PFN_vkCmdBindDescriptorSets -> HS_vkCmdBindDescriptorSets

instance VulkanProc "vkCmdBindDescriptorSets" where
        type VkProcType "vkCmdBindDescriptorSets" =
             HS_vkCmdBindDescriptorSets
        vkProcSymbol = _VkCmdBindDescriptorSets

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdBindDescriptorSets

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdBindDescriptorSetsSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdBindIndexBuffer
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer buffer
-- >     , VkDeviceSize offset
-- >     , VkIndexType indexType
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBindIndexBuffer vkCmdBindIndexBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBindIndexBuffer <- vkGetInstanceProc @VkCmdBindIndexBuffer vkInstance
--
-- or less efficient:
--
-- > myCmdBindIndexBuffer <- vkGetProc @VkCmdBindIndexBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdBindIndexBuffer"
               vkCmdBindIndexBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkBuffer -- ^ buffer
                                           -> VkDeviceSize -- ^ offset
                                                           -> VkIndexType -- ^ indexType
                                                                          -> IO ()

##else
vkCmdBindIndexBuffer ::
                     VkCommandBuffer -- ^ commandBuffer
                                     -> VkBuffer -- ^ buffer
                                                 -> VkDeviceSize -- ^ offset
                                                                 -> VkIndexType -- ^ indexType
                                                                                -> IO ()
vkCmdBindIndexBuffer
  = unsafeDupablePerformIO (vkGetProc @VkCmdBindIndexBuffer)

{-# NOINLINE vkCmdBindIndexBuffer #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdBindIndexBuffer
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer buffer
-- >     , VkDeviceSize offset
-- >     , VkIndexType indexType
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBindIndexBuffer vkCmdBindIndexBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBindIndexBuffer <- vkGetInstanceProc @VkCmdBindIndexBuffer vkInstance
--
-- or less efficient:
--
-- > myCmdBindIndexBuffer <- vkGetProc @VkCmdBindIndexBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdBindIndexBuffer"
               vkCmdBindIndexBufferSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkBuffer -- ^ buffer
                                           -> VkDeviceSize -- ^ offset
                                                           -> VkIndexType -- ^ indexType
                                                                          -> IO ()

##else
vkCmdBindIndexBufferSafe ::
                         VkCommandBuffer -- ^ commandBuffer
                                         -> VkBuffer -- ^ buffer
                                                     -> VkDeviceSize -- ^ offset
                                                                     -> VkIndexType -- ^ indexType
                                                                                    -> IO ()
vkCmdBindIndexBufferSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdBindIndexBuffer)

{-# NOINLINE vkCmdBindIndexBufferSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdBindIndexBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkIndexType indexType
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBindIndexBuffer vkCmdBindIndexBuffer registry at www.khronos.org>
type HS_vkCmdBindIndexBuffer =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkBuffer -- ^ buffer
                                 -> VkDeviceSize -- ^ offset
                                                 -> VkIndexType -- ^ indexType
                                                                -> IO ()

type PFN_vkCmdBindIndexBuffer = FunPtr HS_vkCmdBindIndexBuffer

foreign import ccall unsafe "dynamic" unwrapVkCmdBindIndexBuffer ::
               PFN_vkCmdBindIndexBuffer -> HS_vkCmdBindIndexBuffer

foreign import ccall safe "dynamic" unwrapVkCmdBindIndexBufferSafe
               :: PFN_vkCmdBindIndexBuffer -> HS_vkCmdBindIndexBuffer

instance VulkanProc "vkCmdBindIndexBuffer" where
        type VkProcType "vkCmdBindIndexBuffer" = HS_vkCmdBindIndexBuffer
        vkProcSymbol = _VkCmdBindIndexBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdBindIndexBuffer

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdBindIndexBufferSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdBindVertexBuffers
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t firstBinding
-- >     , uint32_t bindingCount
-- >     , const VkBuffer* pBuffers
-- >     , const VkDeviceSize* pOffsets
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBindVertexBuffers vkCmdBindVertexBuffers registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBindVertexBuffers <- vkGetInstanceProc @VkCmdBindVertexBuffers vkInstance
--
-- or less efficient:
--
-- > myCmdBindVertexBuffers <- vkGetProc @VkCmdBindVertexBuffers
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdBindVertexBuffers"
               vkCmdBindVertexBuffers ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ firstBinding
                        -> Word32 -- ^ bindingCount
                                  -> Ptr VkBuffer -- ^ pBuffers
                                                  -> Ptr VkDeviceSize -- ^ pOffsets
                                                                      -> IO ()

##else
vkCmdBindVertexBuffers ::
                       VkCommandBuffer -- ^ commandBuffer
                                       ->
                         Word32 -- ^ firstBinding
                                -> Word32 -- ^ bindingCount
                                          -> Ptr VkBuffer -- ^ pBuffers
                                                          -> Ptr VkDeviceSize -- ^ pOffsets
                                                                              -> IO ()
vkCmdBindVertexBuffers
  = unsafeDupablePerformIO (vkGetProc @VkCmdBindVertexBuffers)

{-# NOINLINE vkCmdBindVertexBuffers #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @both@
--
-- > void vkCmdBindVertexBuffers
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t firstBinding
-- >     , uint32_t bindingCount
-- >     , const VkBuffer* pBuffers
-- >     , const VkDeviceSize* pOffsets
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBindVertexBuffers vkCmdBindVertexBuffers registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBindVertexBuffers <- vkGetInstanceProc @VkCmdBindVertexBuffers vkInstance
--
-- or less efficient:
--
-- > myCmdBindVertexBuffers <- vkGetProc @VkCmdBindVertexBuffers
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdBindVertexBuffers"
               vkCmdBindVertexBuffersSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ firstBinding
                        -> Word32 -- ^ bindingCount
                                  -> Ptr VkBuffer -- ^ pBuffers
                                                  -> Ptr VkDeviceSize -- ^ pOffsets
                                                                      -> IO ()

##else
vkCmdBindVertexBuffersSafe ::
                           VkCommandBuffer -- ^ commandBuffer
                                           ->
                             Word32 -- ^ firstBinding
                                    -> Word32 -- ^ bindingCount
                                              -> Ptr VkBuffer -- ^ pBuffers
                                                              -> Ptr VkDeviceSize -- ^ pOffsets
                                                                                  -> IO ()
vkCmdBindVertexBuffersSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdBindVertexBuffers)

{-# NOINLINE vkCmdBindVertexBuffersSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdBindVertexBuffers
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstBinding
--   >     , uint32_t bindingCount
--   >     , const VkBuffer* pBuffers
--   >     , const VkDeviceSize* pOffsets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBindVertexBuffers vkCmdBindVertexBuffers registry at www.khronos.org>
type HS_vkCmdBindVertexBuffers =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ firstBinding
              -> Word32 -- ^ bindingCount
                        -> Ptr VkBuffer -- ^ pBuffers
                                        -> Ptr VkDeviceSize -- ^ pOffsets
                                                            -> IO ()

type PFN_vkCmdBindVertexBuffers = FunPtr HS_vkCmdBindVertexBuffers

foreign import ccall unsafe "dynamic" unwrapVkCmdBindVertexBuffers
               :: PFN_vkCmdBindVertexBuffers -> HS_vkCmdBindVertexBuffers

foreign import ccall safe "dynamic"
               unwrapVkCmdBindVertexBuffersSafe ::
               PFN_vkCmdBindVertexBuffers -> HS_vkCmdBindVertexBuffers

instance VulkanProc "vkCmdBindVertexBuffers" where
        type VkProcType "vkCmdBindVertexBuffers" =
             HS_vkCmdBindVertexBuffers
        vkProcSymbol = _VkCmdBindVertexBuffers

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdBindVertexBuffers

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdBindVertexBuffersSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdDraw
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t vertexCount
-- >     , uint32_t instanceCount
-- >     , uint32_t firstVertex
-- >     , uint32_t firstInstance
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDraw vkCmdDraw registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDraw <- vkGetInstanceProc @VkCmdDraw vkInstance
--
-- or less efficient:
--
-- > myCmdDraw <- vkGetProc @VkCmdDraw
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdDraw" vkCmdDraw ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ vertexCount
                                         -> Word32 -- ^ instanceCount
                                                   -> Word32 -- ^ firstVertex
                                                             -> Word32 -- ^ firstInstance
                                                                       -> IO ()

##else
vkCmdDraw ::
          VkCommandBuffer -- ^ commandBuffer
                          -> Word32 -- ^ vertexCount
                                    -> Word32 -- ^ instanceCount
                                              -> Word32 -- ^ firstVertex
                                                        -> Word32 -- ^ firstInstance
                                                                  -> IO ()
vkCmdDraw = unsafeDupablePerformIO (vkGetProc @VkCmdDraw)

{-# NOINLINE vkCmdDraw #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdDraw
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t vertexCount
-- >     , uint32_t instanceCount
-- >     , uint32_t firstVertex
-- >     , uint32_t firstInstance
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDraw vkCmdDraw registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDraw <- vkGetInstanceProc @VkCmdDraw vkInstance
--
-- or less efficient:
--
-- > myCmdDraw <- vkGetProc @VkCmdDraw
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdDraw" vkCmdDrawSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ vertexCount
                                         -> Word32 -- ^ instanceCount
                                                   -> Word32 -- ^ firstVertex
                                                             -> Word32 -- ^ firstInstance
                                                                       -> IO ()

##else
vkCmdDrawSafe ::
              VkCommandBuffer -- ^ commandBuffer
                              -> Word32 -- ^ vertexCount
                                        -> Word32 -- ^ instanceCount
                                                  -> Word32 -- ^ firstVertex
                                                            -> Word32 -- ^ firstInstance
                                                                      -> IO ()
vkCmdDrawSafe = unsafeDupablePerformIO (vkGetProcSafe @VkCmdDraw)

{-# NOINLINE vkCmdDrawSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdDraw
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t vertexCount
--   >     , uint32_t instanceCount
--   >     , uint32_t firstVertex
--   >     , uint32_t firstInstance
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDraw vkCmdDraw registry at www.khronos.org>
type HS_vkCmdDraw =
     VkCommandBuffer -- ^ commandBuffer
                     -> Word32 -- ^ vertexCount
                               -> Word32 -- ^ instanceCount
                                         -> Word32 -- ^ firstVertex
                                                   -> Word32 -- ^ firstInstance
                                                             -> IO ()

type PFN_vkCmdDraw = FunPtr HS_vkCmdDraw

foreign import ccall unsafe "dynamic" unwrapVkCmdDraw ::
               PFN_vkCmdDraw -> HS_vkCmdDraw

foreign import ccall safe "dynamic" unwrapVkCmdDrawSafe ::
               PFN_vkCmdDraw -> HS_vkCmdDraw

instance VulkanProc "vkCmdDraw" where
        type VkProcType "vkCmdDraw" = HS_vkCmdDraw
        vkProcSymbol = _VkCmdDraw

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdDraw

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdDrawSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdDrawIndexed
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t indexCount
-- >     , uint32_t instanceCount
-- >     , uint32_t firstIndex
-- >     , int32_t vertexOffset
-- >     , uint32_t firstInstance
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDrawIndexed vkCmdDrawIndexed registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDrawIndexed <- vkGetInstanceProc @VkCmdDrawIndexed vkInstance
--
-- or less efficient:
--
-- > myCmdDrawIndexed <- vkGetProc @VkCmdDrawIndexed
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdDrawIndexed" vkCmdDrawIndexed ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ indexCount
                        -> Word32 -- ^ instanceCount
                                  -> Word32 -- ^ firstIndex
                                            -> Int32 -- ^ vertexOffset
                                                     -> Word32 -- ^ firstInstance
                                                               -> IO ()

##else
vkCmdDrawIndexed ::
                 VkCommandBuffer -- ^ commandBuffer
                                 ->
                   Word32 -- ^ indexCount
                          -> Word32 -- ^ instanceCount
                                    -> Word32 -- ^ firstIndex
                                              -> Int32 -- ^ vertexOffset
                                                       -> Word32 -- ^ firstInstance
                                                                 -> IO ()
vkCmdDrawIndexed
  = unsafeDupablePerformIO (vkGetProc @VkCmdDrawIndexed)

{-# NOINLINE vkCmdDrawIndexed #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdDrawIndexed
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t indexCount
-- >     , uint32_t instanceCount
-- >     , uint32_t firstIndex
-- >     , int32_t vertexOffset
-- >     , uint32_t firstInstance
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDrawIndexed vkCmdDrawIndexed registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDrawIndexed <- vkGetInstanceProc @VkCmdDrawIndexed vkInstance
--
-- or less efficient:
--
-- > myCmdDrawIndexed <- vkGetProc @VkCmdDrawIndexed
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
vkCmdDrawIndexedSafe ::
                     VkCommandBuffer -- ^ commandBuffer
                                     ->
                       Word32 -- ^ indexCount
                              -> Word32 -- ^ instanceCount
                                        -> Word32 -- ^ firstIndex
                                                  -> Int32 -- ^ vertexOffset
                                                           -> Word32 -- ^ firstInstance
                                                                     -> IO ()
vkCmdDrawIndexedSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdDrawIndexed)

{-# NOINLINE vkCmdDrawIndexedSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdDrawIndexed
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t indexCount
--   >     , uint32_t instanceCount
--   >     , uint32_t firstIndex
--   >     , int32_t vertexOffset
--   >     , uint32_t firstInstance
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDrawIndexed vkCmdDrawIndexed registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic" unwrapVkCmdDrawIndexed ::
               PFN_vkCmdDrawIndexed -> HS_vkCmdDrawIndexed

foreign import ccall safe "dynamic" unwrapVkCmdDrawIndexedSafe ::
               PFN_vkCmdDrawIndexed -> HS_vkCmdDrawIndexed

instance VulkanProc "vkCmdDrawIndexed" where
        type VkProcType "vkCmdDrawIndexed" = HS_vkCmdDrawIndexed
        vkProcSymbol = _VkCmdDrawIndexed

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdDrawIndexed

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdDrawIndexedSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdDrawIndirect
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer buffer
-- >     , VkDeviceSize offset
-- >     , uint32_t drawCount
-- >     , uint32_t stride
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDrawIndirect vkCmdDrawIndirect registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDrawIndirect <- vkGetInstanceProc @VkCmdDrawIndirect vkInstance
--
-- or less efficient:
--
-- > myCmdDrawIndirect <- vkGetProc @VkCmdDrawIndirect
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdDrawIndirect" vkCmdDrawIndirect
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          -> VkDeviceSize -- ^ offset
                                          -> Word32 -- ^ drawCount
                                                    -> Word32 -- ^ stride
                                                              -> IO ()

##else
vkCmdDrawIndirect ::
                  VkCommandBuffer -- ^ commandBuffer
                                  ->
                    VkBuffer -- ^ buffer
                             -> VkDeviceSize -- ^ offset
                                             -> Word32 -- ^ drawCount
                                                       -> Word32 -- ^ stride
                                                                 -> IO ()
vkCmdDrawIndirect
  = unsafeDupablePerformIO (vkGetProc @VkCmdDrawIndirect)

{-# NOINLINE vkCmdDrawIndirect #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdDrawIndirect
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer buffer
-- >     , VkDeviceSize offset
-- >     , uint32_t drawCount
-- >     , uint32_t stride
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDrawIndirect vkCmdDrawIndirect registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDrawIndirect <- vkGetInstanceProc @VkCmdDrawIndirect vkInstance
--
-- or less efficient:
--
-- > myCmdDrawIndirect <- vkGetProc @VkCmdDrawIndirect
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdDrawIndirect" vkCmdDrawIndirectSafe
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          -> VkDeviceSize -- ^ offset
                                          -> Word32 -- ^ drawCount
                                                    -> Word32 -- ^ stride
                                                              -> IO ()

##else
vkCmdDrawIndirectSafe ::
                      VkCommandBuffer -- ^ commandBuffer
                                      ->
                        VkBuffer -- ^ buffer
                                 -> VkDeviceSize -- ^ offset
                                                 -> Word32 -- ^ drawCount
                                                           -> Word32 -- ^ stride
                                                                     -> IO ()
vkCmdDrawIndirectSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdDrawIndirect)

{-# NOINLINE vkCmdDrawIndirectSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdDrawIndirect
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , uint32_t drawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDrawIndirect vkCmdDrawIndirect registry at www.khronos.org>
type HS_vkCmdDrawIndirect =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkBuffer -- ^ buffer
                -> VkDeviceSize -- ^ offset
                                -> Word32 -- ^ drawCount
                                          -> Word32 -- ^ stride
                                                    -> IO ()

type PFN_vkCmdDrawIndirect = FunPtr HS_vkCmdDrawIndirect

foreign import ccall unsafe "dynamic" unwrapVkCmdDrawIndirect ::
               PFN_vkCmdDrawIndirect -> HS_vkCmdDrawIndirect

foreign import ccall safe "dynamic" unwrapVkCmdDrawIndirectSafe ::
               PFN_vkCmdDrawIndirect -> HS_vkCmdDrawIndirect

instance VulkanProc "vkCmdDrawIndirect" where
        type VkProcType "vkCmdDrawIndirect" = HS_vkCmdDrawIndirect
        vkProcSymbol = _VkCmdDrawIndirect

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdDrawIndirect

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdDrawIndirectSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdDrawIndexedIndirect
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer buffer
-- >     , VkDeviceSize offset
-- >     , uint32_t drawCount
-- >     , uint32_t stride
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDrawIndexedIndirect vkCmdDrawIndexedIndirect registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDrawIndexedIndirect <- vkGetInstanceProc @VkCmdDrawIndexedIndirect vkInstance
--
-- or less efficient:
--
-- > myCmdDrawIndexedIndirect <- vkGetProc @VkCmdDrawIndexedIndirect
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdDrawIndexedIndirect"
               vkCmdDrawIndexedIndirect ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          -> VkDeviceSize -- ^ offset
                                          -> Word32 -- ^ drawCount
                                                    -> Word32 -- ^ stride
                                                              -> IO ()

##else
vkCmdDrawIndexedIndirect ::
                         VkCommandBuffer -- ^ commandBuffer
                                         ->
                           VkBuffer -- ^ buffer
                                    -> VkDeviceSize -- ^ offset
                                                    -> Word32 -- ^ drawCount
                                                              -> Word32 -- ^ stride
                                                                        -> IO ()
vkCmdDrawIndexedIndirect
  = unsafeDupablePerformIO (vkGetProc @VkCmdDrawIndexedIndirect)

{-# NOINLINE vkCmdDrawIndexedIndirect #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdDrawIndexedIndirect
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer buffer
-- >     , VkDeviceSize offset
-- >     , uint32_t drawCount
-- >     , uint32_t stride
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDrawIndexedIndirect vkCmdDrawIndexedIndirect registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDrawIndexedIndirect <- vkGetInstanceProc @VkCmdDrawIndexedIndirect vkInstance
--
-- or less efficient:
--
-- > myCmdDrawIndexedIndirect <- vkGetProc @VkCmdDrawIndexedIndirect
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdDrawIndexedIndirect"
               vkCmdDrawIndexedIndirectSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          -> VkDeviceSize -- ^ offset
                                          -> Word32 -- ^ drawCount
                                                    -> Word32 -- ^ stride
                                                              -> IO ()

##else
vkCmdDrawIndexedIndirectSafe ::
                             VkCommandBuffer -- ^ commandBuffer
                                             ->
                               VkBuffer -- ^ buffer
                                        -> VkDeviceSize -- ^ offset
                                                        -> Word32 -- ^ drawCount
                                                                  -> Word32 -- ^ stride
                                                                            -> IO ()
vkCmdDrawIndexedIndirectSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdDrawIndexedIndirect)

{-# NOINLINE vkCmdDrawIndexedIndirectSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdDrawIndexedIndirect
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , uint32_t drawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDrawIndexedIndirect vkCmdDrawIndexedIndirect registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic"
               unwrapVkCmdDrawIndexedIndirect ::
               PFN_vkCmdDrawIndexedIndirect -> HS_vkCmdDrawIndexedIndirect

foreign import ccall safe "dynamic"
               unwrapVkCmdDrawIndexedIndirectSafe ::
               PFN_vkCmdDrawIndexedIndirect -> HS_vkCmdDrawIndexedIndirect

instance VulkanProc "vkCmdDrawIndexedIndirect" where
        type VkProcType "vkCmdDrawIndexedIndirect" =
             HS_vkCmdDrawIndexedIndirect
        vkProcSymbol = _VkCmdDrawIndexedIndirect

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdDrawIndexedIndirect

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdDrawIndexedIndirectSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @compute@
--
-- > void vkCmdDispatch
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t groupCountX
-- >     , uint32_t groupCountY
-- >     , uint32_t groupCountZ
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDispatch vkCmdDispatch registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDispatch <- vkGetInstanceProc @VkCmdDispatch vkInstance
--
-- or less efficient:
--
-- > myCmdDispatch <- vkGetProc @VkCmdDispatch
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdDispatch" vkCmdDispatch ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ groupCountX
                                         -> Word32 -- ^ groupCountY
                                                   -> Word32 -- ^ groupCountZ
                                                             -> IO ()

##else
vkCmdDispatch ::
              VkCommandBuffer -- ^ commandBuffer
                              -> Word32 -- ^ groupCountX
                                        -> Word32 -- ^ groupCountY
                                                  -> Word32 -- ^ groupCountZ
                                                            -> IO ()
vkCmdDispatch = unsafeDupablePerformIO (vkGetProc @VkCmdDispatch)

{-# NOINLINE vkCmdDispatch #-}
##endif

-- |
-- Queues: 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @compute@
--
-- > void vkCmdDispatch
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t groupCountX
-- >     , uint32_t groupCountY
-- >     , uint32_t groupCountZ
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDispatch vkCmdDispatch registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDispatch <- vkGetInstanceProc @VkCmdDispatch vkInstance
--
-- or less efficient:
--
-- > myCmdDispatch <- vkGetProc @VkCmdDispatch
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdDispatch" vkCmdDispatchSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ groupCountX
                                         -> Word32 -- ^ groupCountY
                                                   -> Word32 -- ^ groupCountZ
                                                             -> IO ()

##else
vkCmdDispatchSafe ::
                  VkCommandBuffer -- ^ commandBuffer
                                  -> Word32 -- ^ groupCountX
                                            -> Word32 -- ^ groupCountY
                                                      -> Word32 -- ^ groupCountZ
                                                                -> IO ()
vkCmdDispatchSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdDispatch)

{-# NOINLINE vkCmdDispatchSafe #-}
##endif

-- | Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   Pipeline: @compute@
--
--   > void vkCmdDispatch
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t groupCountX
--   >     , uint32_t groupCountY
--   >     , uint32_t groupCountZ
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDispatch vkCmdDispatch registry at www.khronos.org>
type HS_vkCmdDispatch =
     VkCommandBuffer -- ^ commandBuffer
                     -> Word32 -- ^ groupCountX
                               -> Word32 -- ^ groupCountY
                                         -> Word32 -- ^ groupCountZ
                                                   -> IO ()

type PFN_vkCmdDispatch = FunPtr HS_vkCmdDispatch

foreign import ccall unsafe "dynamic" unwrapVkCmdDispatch ::
               PFN_vkCmdDispatch -> HS_vkCmdDispatch

foreign import ccall safe "dynamic" unwrapVkCmdDispatchSafe ::
               PFN_vkCmdDispatch -> HS_vkCmdDispatch

instance VulkanProc "vkCmdDispatch" where
        type VkProcType "vkCmdDispatch" = HS_vkCmdDispatch
        vkProcSymbol = _VkCmdDispatch

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdDispatch

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdDispatchSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @compute@
--
-- > void vkCmdDispatchIndirect
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer buffer
-- >     , VkDeviceSize offset
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDispatchIndirect vkCmdDispatchIndirect registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDispatchIndirect <- vkGetInstanceProc @VkCmdDispatchIndirect vkInstance
--
-- or less efficient:
--
-- > myCmdDispatchIndirect <- vkGetProc @VkCmdDispatchIndirect
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdDispatchIndirect"
               vkCmdDispatchIndirect ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkBuffer -- ^ buffer
                                           -> VkDeviceSize -- ^ offset
                                                           -> IO ()

##else
vkCmdDispatchIndirect ::
                      VkCommandBuffer -- ^ commandBuffer
                                      -> VkBuffer -- ^ buffer
                                                  -> VkDeviceSize -- ^ offset
                                                                  -> IO ()
vkCmdDispatchIndirect
  = unsafeDupablePerformIO (vkGetProc @VkCmdDispatchIndirect)

{-# NOINLINE vkCmdDispatchIndirect #-}
##endif

-- |
-- Queues: 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @compute@
--
-- > void vkCmdDispatchIndirect
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer buffer
-- >     , VkDeviceSize offset
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDispatchIndirect vkCmdDispatchIndirect registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDispatchIndirect <- vkGetInstanceProc @VkCmdDispatchIndirect vkInstance
--
-- or less efficient:
--
-- > myCmdDispatchIndirect <- vkGetProc @VkCmdDispatchIndirect
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdDispatchIndirect"
               vkCmdDispatchIndirectSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkBuffer -- ^ buffer
                                           -> VkDeviceSize -- ^ offset
                                                           -> IO ()

##else
vkCmdDispatchIndirectSafe ::
                          VkCommandBuffer -- ^ commandBuffer
                                          -> VkBuffer -- ^ buffer
                                                      -> VkDeviceSize -- ^ offset
                                                                      -> IO ()
vkCmdDispatchIndirectSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdDispatchIndirect)

{-# NOINLINE vkCmdDispatchIndirectSafe #-}
##endif

-- | Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   Pipeline: @compute@
--
--   > void vkCmdDispatchIndirect
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDispatchIndirect vkCmdDispatchIndirect registry at www.khronos.org>
type HS_vkCmdDispatchIndirect =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkBuffer -- ^ buffer
                                 -> VkDeviceSize -- ^ offset
                                                 -> IO ()

type PFN_vkCmdDispatchIndirect = FunPtr HS_vkCmdDispatchIndirect

foreign import ccall unsafe "dynamic" unwrapVkCmdDispatchIndirect
               :: PFN_vkCmdDispatchIndirect -> HS_vkCmdDispatchIndirect

foreign import ccall safe "dynamic" unwrapVkCmdDispatchIndirectSafe
               :: PFN_vkCmdDispatchIndirect -> HS_vkCmdDispatchIndirect

instance VulkanProc "vkCmdDispatchIndirect" where
        type VkProcType "vkCmdDispatchIndirect" = HS_vkCmdDispatchIndirect
        vkProcSymbol = _VkCmdDispatchIndirect

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdDispatchIndirect

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdDispatchIndirectSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdCopyBuffer
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer srcBuffer
-- >     , VkBuffer dstBuffer
-- >     , uint32_t regionCount
-- >     , const VkBufferCopy* pRegions
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdCopyBuffer vkCmdCopyBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdCopyBuffer <- vkGetInstanceProc @VkCmdCopyBuffer vkInstance
--
-- or less efficient:
--
-- > myCmdCopyBuffer <- vkGetProc @VkCmdCopyBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdCopyBuffer" vkCmdCopyBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ srcBuffer
                          -> VkBuffer -- ^ dstBuffer
                                      -> Word32 -- ^ regionCount
                                                -> Ptr VkBufferCopy -- ^ pRegions
                                                                    -> IO ()

##else
vkCmdCopyBuffer ::
                VkCommandBuffer -- ^ commandBuffer
                                ->
                  VkBuffer -- ^ srcBuffer
                           -> VkBuffer -- ^ dstBuffer
                                       -> Word32 -- ^ regionCount
                                                 -> Ptr VkBufferCopy -- ^ pRegions
                                                                     -> IO ()
vkCmdCopyBuffer
  = unsafeDupablePerformIO (vkGetProc @VkCmdCopyBuffer)

{-# NOINLINE vkCmdCopyBuffer #-}
##endif

-- |
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdCopyBuffer
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer srcBuffer
-- >     , VkBuffer dstBuffer
-- >     , uint32_t regionCount
-- >     , const VkBufferCopy* pRegions
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdCopyBuffer vkCmdCopyBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdCopyBuffer <- vkGetInstanceProc @VkCmdCopyBuffer vkInstance
--
-- or less efficient:
--
-- > myCmdCopyBuffer <- vkGetProc @VkCmdCopyBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdCopyBuffer" vkCmdCopyBufferSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ srcBuffer
                          -> VkBuffer -- ^ dstBuffer
                                      -> Word32 -- ^ regionCount
                                                -> Ptr VkBufferCopy -- ^ pRegions
                                                                    -> IO ()

##else
vkCmdCopyBufferSafe ::
                    VkCommandBuffer -- ^ commandBuffer
                                    ->
                      VkBuffer -- ^ srcBuffer
                               -> VkBuffer -- ^ dstBuffer
                                           -> Word32 -- ^ regionCount
                                                     -> Ptr VkBufferCopy -- ^ pRegions
                                                                         -> IO ()
vkCmdCopyBufferSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdCopyBuffer)

{-# NOINLINE vkCmdCopyBufferSafe #-}
##endif

-- | Queues: 'transfer', 'graphics', 'compute'.
--
--   Renderpass: @outside@
--
--   Pipeline: @transfer@
--
--   > void vkCmdCopyBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer srcBuffer
--   >     , VkBuffer dstBuffer
--   >     , uint32_t regionCount
--   >     , const VkBufferCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdCopyBuffer vkCmdCopyBuffer registry at www.khronos.org>
type HS_vkCmdCopyBuffer =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkBuffer -- ^ srcBuffer
                -> VkBuffer -- ^ dstBuffer
                            -> Word32 -- ^ regionCount
                                      -> Ptr VkBufferCopy -- ^ pRegions
                                                          -> IO ()

type PFN_vkCmdCopyBuffer = FunPtr HS_vkCmdCopyBuffer

foreign import ccall unsafe "dynamic" unwrapVkCmdCopyBuffer ::
               PFN_vkCmdCopyBuffer -> HS_vkCmdCopyBuffer

foreign import ccall safe "dynamic" unwrapVkCmdCopyBufferSafe ::
               PFN_vkCmdCopyBuffer -> HS_vkCmdCopyBuffer

instance VulkanProc "vkCmdCopyBuffer" where
        type VkProcType "vkCmdCopyBuffer" = HS_vkCmdCopyBuffer
        vkProcSymbol = _VkCmdCopyBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdCopyBuffer

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdCopyBufferSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdCopyImage
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkImage srcImage
-- >     , VkImageLayout srcImageLayout
-- >     , VkImage dstImage
-- >     , VkImageLayout dstImageLayout
-- >     , uint32_t regionCount
-- >     , const VkImageCopy* pRegions
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdCopyImage vkCmdCopyImage registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdCopyImage <- vkGetInstanceProc @VkCmdCopyImage vkInstance
--
-- or less efficient:
--
-- > myCmdCopyImage <- vkGetProc @VkCmdCopyImage
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
vkCmdCopyImage ::
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
vkCmdCopyImage = unsafeDupablePerformIO (vkGetProc @VkCmdCopyImage)

{-# NOINLINE vkCmdCopyImage #-}
##endif

-- |
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdCopyImage
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkImage srcImage
-- >     , VkImageLayout srcImageLayout
-- >     , VkImage dstImage
-- >     , VkImageLayout dstImageLayout
-- >     , uint32_t regionCount
-- >     , const VkImageCopy* pRegions
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdCopyImage vkCmdCopyImage registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdCopyImage <- vkGetInstanceProc @VkCmdCopyImage vkInstance
--
-- or less efficient:
--
-- > myCmdCopyImage <- vkGetProc @VkCmdCopyImage
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
vkCmdCopyImageSafe ::
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
vkCmdCopyImageSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdCopyImage)

{-# NOINLINE vkCmdCopyImageSafe #-}
##endif

-- | Queues: 'transfer', 'graphics', 'compute'.
--
--   Renderpass: @outside@
--
--   Pipeline: @transfer@
--
--   > void vkCmdCopyImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkImageCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdCopyImage vkCmdCopyImage registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic" unwrapVkCmdCopyImage ::
               PFN_vkCmdCopyImage -> HS_vkCmdCopyImage

foreign import ccall safe "dynamic" unwrapVkCmdCopyImageSafe ::
               PFN_vkCmdCopyImage -> HS_vkCmdCopyImage

instance VulkanProc "vkCmdCopyImage" where
        type VkProcType "vkCmdCopyImage" = HS_vkCmdCopyImage
        vkProcSymbol = _VkCmdCopyImage

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdCopyImage

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdCopyImageSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdBlitImage
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkImage srcImage
-- >     , VkImageLayout srcImageLayout
-- >     , VkImage dstImage
-- >     , VkImageLayout dstImageLayout
-- >     , uint32_t regionCount
-- >     , const VkImageBlit* pRegions
-- >     , VkFilter filter
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBlitImage vkCmdBlitImage registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBlitImage <- vkGetInstanceProc @VkCmdBlitImage vkInstance
--
-- or less efficient:
--
-- > myCmdBlitImage <- vkGetProc @VkCmdBlitImage
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
vkCmdBlitImage ::
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
vkCmdBlitImage = unsafeDupablePerformIO (vkGetProc @VkCmdBlitImage)

{-# NOINLINE vkCmdBlitImage #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdBlitImage
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkImage srcImage
-- >     , VkImageLayout srcImageLayout
-- >     , VkImage dstImage
-- >     , VkImageLayout dstImageLayout
-- >     , uint32_t regionCount
-- >     , const VkImageBlit* pRegions
-- >     , VkFilter filter
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBlitImage vkCmdBlitImage registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBlitImage <- vkGetInstanceProc @VkCmdBlitImage vkInstance
--
-- or less efficient:
--
-- > myCmdBlitImage <- vkGetProc @VkCmdBlitImage
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
vkCmdBlitImageSafe ::
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
vkCmdBlitImageSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdBlitImage)

{-# NOINLINE vkCmdBlitImageSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @outside@
--
--   Pipeline: @transfer@
--
--   > void vkCmdBlitImage
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBlitImage vkCmdBlitImage registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic" unwrapVkCmdBlitImage ::
               PFN_vkCmdBlitImage -> HS_vkCmdBlitImage

foreign import ccall safe "dynamic" unwrapVkCmdBlitImageSafe ::
               PFN_vkCmdBlitImage -> HS_vkCmdBlitImage

instance VulkanProc "vkCmdBlitImage" where
        type VkProcType "vkCmdBlitImage" = HS_vkCmdBlitImage
        vkProcSymbol = _VkCmdBlitImage

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdBlitImage

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdBlitImageSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdCopyBufferToImage
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer srcBuffer
-- >     , VkImage dstImage
-- >     , VkImageLayout dstImageLayout
-- >     , uint32_t regionCount
-- >     , const VkBufferImageCopy* pRegions
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdCopyBufferToImage vkCmdCopyBufferToImage registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdCopyBufferToImage <- vkGetInstanceProc @VkCmdCopyBufferToImage vkInstance
--
-- or less efficient:
--
-- > myCmdCopyBufferToImage <- vkGetProc @VkCmdCopyBufferToImage
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCmdCopyBufferToImage
  = unsafeDupablePerformIO (vkGetProc @VkCmdCopyBufferToImage)

{-# NOINLINE vkCmdCopyBufferToImage #-}
##endif

-- |
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdCopyBufferToImage
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer srcBuffer
-- >     , VkImage dstImage
-- >     , VkImageLayout dstImageLayout
-- >     , uint32_t regionCount
-- >     , const VkBufferImageCopy* pRegions
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdCopyBufferToImage vkCmdCopyBufferToImage registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdCopyBufferToImage <- vkGetInstanceProc @VkCmdCopyBufferToImage vkInstance
--
-- or less efficient:
--
-- > myCmdCopyBufferToImage <- vkGetProc @VkCmdCopyBufferToImage
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCmdCopyBufferToImageSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdCopyBufferToImage)

{-# NOINLINE vkCmdCopyBufferToImageSafe #-}
##endif

-- | Queues: 'transfer', 'graphics', 'compute'.
--
--   Renderpass: @outside@
--
--   Pipeline: @transfer@
--
--   > void vkCmdCopyBufferToImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer srcBuffer
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkBufferImageCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdCopyBufferToImage vkCmdCopyBufferToImage registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic" unwrapVkCmdCopyBufferToImage
               :: PFN_vkCmdCopyBufferToImage -> HS_vkCmdCopyBufferToImage

foreign import ccall safe "dynamic"
               unwrapVkCmdCopyBufferToImageSafe ::
               PFN_vkCmdCopyBufferToImage -> HS_vkCmdCopyBufferToImage

instance VulkanProc "vkCmdCopyBufferToImage" where
        type VkProcType "vkCmdCopyBufferToImage" =
             HS_vkCmdCopyBufferToImage
        vkProcSymbol = _VkCmdCopyBufferToImage

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdCopyBufferToImage

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdCopyBufferToImageSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdCopyImageToBuffer
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkImage srcImage
-- >     , VkImageLayout srcImageLayout
-- >     , VkBuffer dstBuffer
-- >     , uint32_t regionCount
-- >     , const VkBufferImageCopy* pRegions
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdCopyImageToBuffer vkCmdCopyImageToBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdCopyImageToBuffer <- vkGetInstanceProc @VkCmdCopyImageToBuffer vkInstance
--
-- or less efficient:
--
-- > myCmdCopyImageToBuffer <- vkGetProc @VkCmdCopyImageToBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCmdCopyImageToBuffer
  = unsafeDupablePerformIO (vkGetProc @VkCmdCopyImageToBuffer)

{-# NOINLINE vkCmdCopyImageToBuffer #-}
##endif

-- |
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdCopyImageToBuffer
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkImage srcImage
-- >     , VkImageLayout srcImageLayout
-- >     , VkBuffer dstBuffer
-- >     , uint32_t regionCount
-- >     , const VkBufferImageCopy* pRegions
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdCopyImageToBuffer vkCmdCopyImageToBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdCopyImageToBuffer <- vkGetInstanceProc @VkCmdCopyImageToBuffer vkInstance
--
-- or less efficient:
--
-- > myCmdCopyImageToBuffer <- vkGetProc @VkCmdCopyImageToBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCmdCopyImageToBufferSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdCopyImageToBuffer)

{-# NOINLINE vkCmdCopyImageToBufferSafe #-}
##endif

-- | Queues: 'transfer', 'graphics', 'compute'.
--
--   Renderpass: @outside@
--
--   Pipeline: @transfer@
--
--   > void vkCmdCopyImageToBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkBuffer dstBuffer
--   >     , uint32_t regionCount
--   >     , const VkBufferImageCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdCopyImageToBuffer vkCmdCopyImageToBuffer registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic" unwrapVkCmdCopyImageToBuffer
               :: PFN_vkCmdCopyImageToBuffer -> HS_vkCmdCopyImageToBuffer

foreign import ccall safe "dynamic"
               unwrapVkCmdCopyImageToBufferSafe ::
               PFN_vkCmdCopyImageToBuffer -> HS_vkCmdCopyImageToBuffer

instance VulkanProc "vkCmdCopyImageToBuffer" where
        type VkProcType "vkCmdCopyImageToBuffer" =
             HS_vkCmdCopyImageToBuffer
        vkProcSymbol = _VkCmdCopyImageToBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdCopyImageToBuffer

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdCopyImageToBufferSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdUpdateBuffer
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer dstBuffer
-- >     , VkDeviceSize dstOffset
-- >     , VkDeviceSize dataSize
-- >     , const void* pData
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdUpdateBuffer vkCmdUpdateBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdUpdateBuffer <- vkGetInstanceProc @VkCmdUpdateBuffer vkInstance
--
-- or less efficient:
--
-- > myCmdUpdateBuffer <- vkGetProc @VkCmdUpdateBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdUpdateBuffer" vkCmdUpdateBuffer
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ dstBuffer
                          -> VkDeviceSize -- ^ dstOffset
                                          -> VkDeviceSize -- ^ dataSize
                                                          -> Ptr Void -- ^ pData
                                                                      -> IO ()

##else
vkCmdUpdateBuffer ::
                  VkCommandBuffer -- ^ commandBuffer
                                  ->
                    VkBuffer -- ^ dstBuffer
                             -> VkDeviceSize -- ^ dstOffset
                                             -> VkDeviceSize -- ^ dataSize
                                                             -> Ptr Void -- ^ pData
                                                                         -> IO ()
vkCmdUpdateBuffer
  = unsafeDupablePerformIO (vkGetProc @VkCmdUpdateBuffer)

{-# NOINLINE vkCmdUpdateBuffer #-}
##endif

-- |
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdUpdateBuffer
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer dstBuffer
-- >     , VkDeviceSize dstOffset
-- >     , VkDeviceSize dataSize
-- >     , const void* pData
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdUpdateBuffer vkCmdUpdateBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdUpdateBuffer <- vkGetInstanceProc @VkCmdUpdateBuffer vkInstance
--
-- or less efficient:
--
-- > myCmdUpdateBuffer <- vkGetProc @VkCmdUpdateBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdUpdateBuffer" vkCmdUpdateBufferSafe
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ dstBuffer
                          -> VkDeviceSize -- ^ dstOffset
                                          -> VkDeviceSize -- ^ dataSize
                                                          -> Ptr Void -- ^ pData
                                                                      -> IO ()

##else
vkCmdUpdateBufferSafe ::
                      VkCommandBuffer -- ^ commandBuffer
                                      ->
                        VkBuffer -- ^ dstBuffer
                                 -> VkDeviceSize -- ^ dstOffset
                                                 -> VkDeviceSize -- ^ dataSize
                                                                 -> Ptr Void -- ^ pData
                                                                             -> IO ()
vkCmdUpdateBufferSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdUpdateBuffer)

{-# NOINLINE vkCmdUpdateBufferSafe #-}
##endif

-- | Queues: 'transfer', 'graphics', 'compute'.
--
--   Renderpass: @outside@
--
--   Pipeline: @transfer@
--
--   > void vkCmdUpdateBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , VkDeviceSize dataSize
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdUpdateBuffer vkCmdUpdateBuffer registry at www.khronos.org>
type HS_vkCmdUpdateBuffer =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkBuffer -- ^ dstBuffer
                -> VkDeviceSize -- ^ dstOffset
                                -> VkDeviceSize -- ^ dataSize
                                                -> Ptr Void -- ^ pData
                                                            -> IO ()

type PFN_vkCmdUpdateBuffer = FunPtr HS_vkCmdUpdateBuffer

foreign import ccall unsafe "dynamic" unwrapVkCmdUpdateBuffer ::
               PFN_vkCmdUpdateBuffer -> HS_vkCmdUpdateBuffer

foreign import ccall safe "dynamic" unwrapVkCmdUpdateBufferSafe ::
               PFN_vkCmdUpdateBuffer -> HS_vkCmdUpdateBuffer

instance VulkanProc "vkCmdUpdateBuffer" where
        type VkProcType "vkCmdUpdateBuffer" = HS_vkCmdUpdateBuffer
        vkProcSymbol = _VkCmdUpdateBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdUpdateBuffer

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdUpdateBufferSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- transfer support is only available when VK_KHR_maintenance1 is enabled, as documented in valid usage language in the specification
--
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdFillBuffer
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer dstBuffer
-- >     , VkDeviceSize dstOffset
-- >     , VkDeviceSize size
-- >     , uint32_t data
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdFillBuffer vkCmdFillBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdFillBuffer <- vkGetInstanceProc @VkCmdFillBuffer vkInstance
--
-- or less efficient:
--
-- > myCmdFillBuffer <- vkGetProc @VkCmdFillBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdFillBuffer" vkCmdFillBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ dstBuffer
                          -> VkDeviceSize -- ^ dstOffset
                                          -> VkDeviceSize -- ^ size
                                                          -> Word32 -- ^ data
                                                                    -> IO ()

##else
vkCmdFillBuffer ::
                VkCommandBuffer -- ^ commandBuffer
                                ->
                  VkBuffer -- ^ dstBuffer
                           -> VkDeviceSize -- ^ dstOffset
                                           -> VkDeviceSize -- ^ size
                                                           -> Word32 -- ^ data
                                                                     -> IO ()
vkCmdFillBuffer
  = unsafeDupablePerformIO (vkGetProc @VkCmdFillBuffer)

{-# NOINLINE vkCmdFillBuffer #-}
##endif

-- |
-- transfer support is only available when VK_KHR_maintenance1 is enabled, as documented in valid usage language in the specification
--
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdFillBuffer
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer dstBuffer
-- >     , VkDeviceSize dstOffset
-- >     , VkDeviceSize size
-- >     , uint32_t data
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdFillBuffer vkCmdFillBuffer registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdFillBuffer <- vkGetInstanceProc @VkCmdFillBuffer vkInstance
--
-- or less efficient:
--
-- > myCmdFillBuffer <- vkGetProc @VkCmdFillBuffer
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdFillBuffer" vkCmdFillBufferSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ dstBuffer
                          -> VkDeviceSize -- ^ dstOffset
                                          -> VkDeviceSize -- ^ size
                                                          -> Word32 -- ^ data
                                                                    -> IO ()

##else
vkCmdFillBufferSafe ::
                    VkCommandBuffer -- ^ commandBuffer
                                    ->
                      VkBuffer -- ^ dstBuffer
                               -> VkDeviceSize -- ^ dstOffset
                                               -> VkDeviceSize -- ^ size
                                                               -> Word32 -- ^ data
                                                                         -> IO ()
vkCmdFillBufferSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdFillBuffer)

{-# NOINLINE vkCmdFillBufferSafe #-}
##endif

-- | transfer support is only available when VK_KHR_maintenance1 is enabled, as documented in valid usage language in the specification
--
--   Queues: 'transfer', 'graphics', 'compute'.
--
--   Renderpass: @outside@
--
--   Pipeline: @transfer@
--
--   > void vkCmdFillBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , VkDeviceSize size
--   >     , uint32_t data
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdFillBuffer vkCmdFillBuffer registry at www.khronos.org>
type HS_vkCmdFillBuffer =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkBuffer -- ^ dstBuffer
                -> VkDeviceSize -- ^ dstOffset
                                -> VkDeviceSize -- ^ size
                                                -> Word32 -- ^ data
                                                          -> IO ()

type PFN_vkCmdFillBuffer = FunPtr HS_vkCmdFillBuffer

foreign import ccall unsafe "dynamic" unwrapVkCmdFillBuffer ::
               PFN_vkCmdFillBuffer -> HS_vkCmdFillBuffer

foreign import ccall safe "dynamic" unwrapVkCmdFillBufferSafe ::
               PFN_vkCmdFillBuffer -> HS_vkCmdFillBuffer

instance VulkanProc "vkCmdFillBuffer" where
        type VkProcType "vkCmdFillBuffer" = HS_vkCmdFillBuffer
        vkProcSymbol = _VkCmdFillBuffer

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdFillBuffer

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdFillBufferSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdClearColorImage
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkImage image
-- >     , VkImageLayout imageLayout
-- >     , const VkClearColorValue* pColor
-- >     , uint32_t rangeCount
-- >     , const VkImageSubresourceRange* pRanges
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdClearColorImage vkCmdClearColorImage registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdClearColorImage <- vkGetInstanceProc @VkCmdClearColorImage vkInstance
--
-- or less efficient:
--
-- > myCmdClearColorImage <- vkGetProc @VkCmdClearColorImage
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCmdClearColorImage
  = unsafeDupablePerformIO (vkGetProc @VkCmdClearColorImage)

{-# NOINLINE vkCmdClearColorImage #-}
##endif

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdClearColorImage
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkImage image
-- >     , VkImageLayout imageLayout
-- >     , const VkClearColorValue* pColor
-- >     , uint32_t rangeCount
-- >     , const VkImageSubresourceRange* pRanges
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdClearColorImage vkCmdClearColorImage registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdClearColorImage <- vkGetInstanceProc @VkCmdClearColorImage vkInstance
--
-- or less efficient:
--
-- > myCmdClearColorImage <- vkGetProc @VkCmdClearColorImage
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCmdClearColorImageSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdClearColorImage)

{-# NOINLINE vkCmdClearColorImageSafe #-}
##endif

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @outside@
--
--   Pipeline: @transfer@
--
--   > void vkCmdClearColorImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage image
--   >     , VkImageLayout imageLayout
--   >     , const VkClearColorValue* pColor
--   >     , uint32_t rangeCount
--   >     , const VkImageSubresourceRange* pRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdClearColorImage vkCmdClearColorImage registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic" unwrapVkCmdClearColorImage ::
               PFN_vkCmdClearColorImage -> HS_vkCmdClearColorImage

foreign import ccall safe "dynamic" unwrapVkCmdClearColorImageSafe
               :: PFN_vkCmdClearColorImage -> HS_vkCmdClearColorImage

instance VulkanProc "vkCmdClearColorImage" where
        type VkProcType "vkCmdClearColorImage" = HS_vkCmdClearColorImage
        vkProcSymbol = _VkCmdClearColorImage

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdClearColorImage

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdClearColorImageSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdClearDepthStencilImage
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkImage image
-- >     , VkImageLayout imageLayout
-- >     , const VkClearDepthStencilValue* pDepthStencil
-- >     , uint32_t rangeCount
-- >     , const VkImageSubresourceRange* pRanges
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdClearDepthStencilImage vkCmdClearDepthStencilImage registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdClearDepthStencilImage <- vkGetInstanceProc @VkCmdClearDepthStencilImage vkInstance
--
-- or less efficient:
--
-- > myCmdClearDepthStencilImage <- vkGetProc @VkCmdClearDepthStencilImage
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCmdClearDepthStencilImage
  = unsafeDupablePerformIO (vkGetProc @VkCmdClearDepthStencilImage)

{-# NOINLINE vkCmdClearDepthStencilImage #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdClearDepthStencilImage
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkImage image
-- >     , VkImageLayout imageLayout
-- >     , const VkClearDepthStencilValue* pDepthStencil
-- >     , uint32_t rangeCount
-- >     , const VkImageSubresourceRange* pRanges
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdClearDepthStencilImage vkCmdClearDepthStencilImage registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdClearDepthStencilImage <- vkGetInstanceProc @VkCmdClearDepthStencilImage vkInstance
--
-- or less efficient:
--
-- > myCmdClearDepthStencilImage <- vkGetProc @VkCmdClearDepthStencilImage
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCmdClearDepthStencilImageSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkCmdClearDepthStencilImage)

{-# NOINLINE vkCmdClearDepthStencilImageSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @outside@
--
--   Pipeline: @transfer@
--
--   > void vkCmdClearDepthStencilImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage image
--   >     , VkImageLayout imageLayout
--   >     , const VkClearDepthStencilValue* pDepthStencil
--   >     , uint32_t rangeCount
--   >     , const VkImageSubresourceRange* pRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdClearDepthStencilImage vkCmdClearDepthStencilImage registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic"
               unwrapVkCmdClearDepthStencilImage ::
               PFN_vkCmdClearDepthStencilImage -> HS_vkCmdClearDepthStencilImage

foreign import ccall safe "dynamic"
               unwrapVkCmdClearDepthStencilImageSafe ::
               PFN_vkCmdClearDepthStencilImage -> HS_vkCmdClearDepthStencilImage

instance VulkanProc "vkCmdClearDepthStencilImage" where
        type VkProcType "vkCmdClearDepthStencilImage" =
             HS_vkCmdClearDepthStencilImage
        vkProcSymbol = _VkCmdClearDepthStencilImage

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdClearDepthStencilImage

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdClearDepthStencilImageSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdClearAttachments
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t attachmentCount
-- >     , const VkClearAttachment* pAttachments
-- >     , uint32_t rectCount
-- >     , const VkClearRect* pRects
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdClearAttachments vkCmdClearAttachments registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdClearAttachments <- vkGetInstanceProc @VkCmdClearAttachments vkInstance
--
-- or less efficient:
--
-- > myCmdClearAttachments <- vkGetProc @VkCmdClearAttachments
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
vkCmdClearAttachments ::
                      VkCommandBuffer -- ^ commandBuffer
                                      ->
                        Word32 -- ^ attachmentCount
                               ->
                          Ptr VkClearAttachment -- ^ pAttachments
                                                -> Word32 -- ^ rectCount
                                                          -> Ptr VkClearRect -- ^ pRects
                                                                             -> IO ()
vkCmdClearAttachments
  = unsafeDupablePerformIO (vkGetProc @VkCmdClearAttachments)

{-# NOINLINE vkCmdClearAttachments #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdClearAttachments
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t attachmentCount
-- >     , const VkClearAttachment* pAttachments
-- >     , uint32_t rectCount
-- >     , const VkClearRect* pRects
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdClearAttachments vkCmdClearAttachments registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdClearAttachments <- vkGetInstanceProc @VkCmdClearAttachments vkInstance
--
-- or less efficient:
--
-- > myCmdClearAttachments <- vkGetProc @VkCmdClearAttachments
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
vkCmdClearAttachmentsSafe ::
                          VkCommandBuffer -- ^ commandBuffer
                                          ->
                            Word32 -- ^ attachmentCount
                                   ->
                              Ptr VkClearAttachment -- ^ pAttachments
                                                    -> Word32 -- ^ rectCount
                                                              -> Ptr VkClearRect -- ^ pRects
                                                                                 -> IO ()
vkCmdClearAttachmentsSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdClearAttachments)

{-# NOINLINE vkCmdClearAttachmentsSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdClearAttachments
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t attachmentCount
--   >     , const VkClearAttachment* pAttachments
--   >     , uint32_t rectCount
--   >     , const VkClearRect* pRects
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdClearAttachments vkCmdClearAttachments registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic" unwrapVkCmdClearAttachments
               :: PFN_vkCmdClearAttachments -> HS_vkCmdClearAttachments

foreign import ccall safe "dynamic" unwrapVkCmdClearAttachmentsSafe
               :: PFN_vkCmdClearAttachments -> HS_vkCmdClearAttachments

instance VulkanProc "vkCmdClearAttachments" where
        type VkProcType "vkCmdClearAttachments" = HS_vkCmdClearAttachments
        vkProcSymbol = _VkCmdClearAttachments

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdClearAttachments

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdClearAttachmentsSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdResolveImage
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkImage srcImage
-- >     , VkImageLayout srcImageLayout
-- >     , VkImage dstImage
-- >     , VkImageLayout dstImageLayout
-- >     , uint32_t regionCount
-- >     , const VkImageResolve* pRegions
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdResolveImage vkCmdResolveImage registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdResolveImage <- vkGetInstanceProc @VkCmdResolveImage vkInstance
--
-- or less efficient:
--
-- > myCmdResolveImage <- vkGetProc @VkCmdResolveImage
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
vkCmdResolveImage ::
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
vkCmdResolveImage
  = unsafeDupablePerformIO (vkGetProc @VkCmdResolveImage)

{-# NOINLINE vkCmdResolveImage #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdResolveImage
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkImage srcImage
-- >     , VkImageLayout srcImageLayout
-- >     , VkImage dstImage
-- >     , VkImageLayout dstImageLayout
-- >     , uint32_t regionCount
-- >     , const VkImageResolve* pRegions
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdResolveImage vkCmdResolveImage registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdResolveImage <- vkGetInstanceProc @VkCmdResolveImage vkInstance
--
-- or less efficient:
--
-- > myCmdResolveImage <- vkGetProc @VkCmdResolveImage
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
vkCmdResolveImageSafe ::
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
vkCmdResolveImageSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdResolveImage)

{-# NOINLINE vkCmdResolveImageSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @outside@
--
--   Pipeline: @transfer@
--
--   > void vkCmdResolveImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkImageResolve* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdResolveImage vkCmdResolveImage registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic" unwrapVkCmdResolveImage ::
               PFN_vkCmdResolveImage -> HS_vkCmdResolveImage

foreign import ccall safe "dynamic" unwrapVkCmdResolveImageSafe ::
               PFN_vkCmdResolveImage -> HS_vkCmdResolveImage

instance VulkanProc "vkCmdResolveImage" where
        type VkProcType "vkCmdResolveImage" = HS_vkCmdResolveImage
        vkProcSymbol = _VkCmdResolveImage

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdResolveImage

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdResolveImageSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- > void vkCmdSetEvent
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkEvent event
-- >     , VkPipelineStageFlags stageMask
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetEvent vkCmdSetEvent registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetEvent <- vkGetInstanceProc @VkCmdSetEvent vkInstance
--
-- or less efficient:
--
-- > myCmdSetEvent <- vkGetProc @VkCmdSetEvent
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdSetEvent" vkCmdSetEvent ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkEvent -- ^ event
                                          -> VkPipelineStageFlags -- ^ stageMask
                                                                  -> IO ()

##else
vkCmdSetEvent ::
              VkCommandBuffer -- ^ commandBuffer
                              -> VkEvent -- ^ event
                                         -> VkPipelineStageFlags -- ^ stageMask
                                                                 -> IO ()
vkCmdSetEvent = unsafeDupablePerformIO (vkGetProc @VkCmdSetEvent)

{-# NOINLINE vkCmdSetEvent #-}
##endif

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- > void vkCmdSetEvent
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkEvent event
-- >     , VkPipelineStageFlags stageMask
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetEvent vkCmdSetEvent registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetEvent <- vkGetInstanceProc @VkCmdSetEvent vkInstance
--
-- or less efficient:
--
-- > myCmdSetEvent <- vkGetProc @VkCmdSetEvent
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdSetEvent" vkCmdSetEventSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkEvent -- ^ event
                                          -> VkPipelineStageFlags -- ^ stageMask
                                                                  -> IO ()

##else
vkCmdSetEventSafe ::
                  VkCommandBuffer -- ^ commandBuffer
                                  -> VkEvent -- ^ event
                                             -> VkPipelineStageFlags -- ^ stageMask
                                                                     -> IO ()
vkCmdSetEventSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdSetEvent)

{-# NOINLINE vkCmdSetEventSafe #-}
##endif

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdSetEvent
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkEvent event
--   >     , VkPipelineStageFlags stageMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetEvent vkCmdSetEvent registry at www.khronos.org>
type HS_vkCmdSetEvent =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkEvent -- ^ event
                                -> VkPipelineStageFlags -- ^ stageMask
                                                        -> IO ()

type PFN_vkCmdSetEvent = FunPtr HS_vkCmdSetEvent

foreign import ccall unsafe "dynamic" unwrapVkCmdSetEvent ::
               PFN_vkCmdSetEvent -> HS_vkCmdSetEvent

foreign import ccall safe "dynamic" unwrapVkCmdSetEventSafe ::
               PFN_vkCmdSetEvent -> HS_vkCmdSetEvent

instance VulkanProc "vkCmdSetEvent" where
        type VkProcType "vkCmdSetEvent" = HS_vkCmdSetEvent
        vkProcSymbol = _VkCmdSetEvent

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdSetEvent

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdSetEventSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- > void vkCmdResetEvent
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkEvent event
-- >     , VkPipelineStageFlags stageMask
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdResetEvent vkCmdResetEvent registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdResetEvent <- vkGetInstanceProc @VkCmdResetEvent vkInstance
--
-- or less efficient:
--
-- > myCmdResetEvent <- vkGetProc @VkCmdResetEvent
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdResetEvent" vkCmdResetEvent ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkEvent -- ^ event
                                          -> VkPipelineStageFlags -- ^ stageMask
                                                                  -> IO ()

##else
vkCmdResetEvent ::
                VkCommandBuffer -- ^ commandBuffer
                                -> VkEvent -- ^ event
                                           -> VkPipelineStageFlags -- ^ stageMask
                                                                   -> IO ()
vkCmdResetEvent
  = unsafeDupablePerformIO (vkGetProc @VkCmdResetEvent)

{-# NOINLINE vkCmdResetEvent #-}
##endif

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- > void vkCmdResetEvent
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkEvent event
-- >     , VkPipelineStageFlags stageMask
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdResetEvent vkCmdResetEvent registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdResetEvent <- vkGetInstanceProc @VkCmdResetEvent vkInstance
--
-- or less efficient:
--
-- > myCmdResetEvent <- vkGetProc @VkCmdResetEvent
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdResetEvent" vkCmdResetEventSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkEvent -- ^ event
                                          -> VkPipelineStageFlags -- ^ stageMask
                                                                  -> IO ()

##else
vkCmdResetEventSafe ::
                    VkCommandBuffer -- ^ commandBuffer
                                    -> VkEvent -- ^ event
                                               -> VkPipelineStageFlags -- ^ stageMask
                                                                       -> IO ()
vkCmdResetEventSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdResetEvent)

{-# NOINLINE vkCmdResetEventSafe #-}
##endif

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdResetEvent
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkEvent event
--   >     , VkPipelineStageFlags stageMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdResetEvent vkCmdResetEvent registry at www.khronos.org>
type HS_vkCmdResetEvent =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkEvent -- ^ event
                                -> VkPipelineStageFlags -- ^ stageMask
                                                        -> IO ()

type PFN_vkCmdResetEvent = FunPtr HS_vkCmdResetEvent

foreign import ccall unsafe "dynamic" unwrapVkCmdResetEvent ::
               PFN_vkCmdResetEvent -> HS_vkCmdResetEvent

foreign import ccall safe "dynamic" unwrapVkCmdResetEventSafe ::
               PFN_vkCmdResetEvent -> HS_vkCmdResetEvent

instance VulkanProc "vkCmdResetEvent" where
        type VkProcType "vkCmdResetEvent" = HS_vkCmdResetEvent
        vkProcSymbol = _VkCmdResetEvent

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdResetEvent

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdResetEventSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- > void vkCmdWaitEvents
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t eventCount
-- >     , const VkEvent* pEvents
-- >     , VkPipelineStageFlags srcStageMask
-- >     , VkPipelineStageFlags dstStageMask
-- >     , uint32_t memoryBarrierCount
-- >     , const VkMemoryBarrier* pMemoryBarriers
-- >     , uint32_t bufferMemoryBarrierCount
-- >     , const VkBufferMemoryBarrier* pBufferMemoryBarriers
-- >     , uint32_t imageMemoryBarrierCount
-- >     , const VkImageMemoryBarrier* pImageMemoryBarriers
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdWaitEvents vkCmdWaitEvents registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdWaitEvents <- vkGetInstanceProc @VkCmdWaitEvents vkInstance
--
-- or less efficient:
--
-- > myCmdWaitEvents <- vkGetProc @VkCmdWaitEvents
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
vkCmdWaitEvents ::
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
vkCmdWaitEvents
  = unsafeDupablePerformIO (vkGetProc @VkCmdWaitEvents)

{-# NOINLINE vkCmdWaitEvents #-}
##endif

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- > void vkCmdWaitEvents
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t eventCount
-- >     , const VkEvent* pEvents
-- >     , VkPipelineStageFlags srcStageMask
-- >     , VkPipelineStageFlags dstStageMask
-- >     , uint32_t memoryBarrierCount
-- >     , const VkMemoryBarrier* pMemoryBarriers
-- >     , uint32_t bufferMemoryBarrierCount
-- >     , const VkBufferMemoryBarrier* pBufferMemoryBarriers
-- >     , uint32_t imageMemoryBarrierCount
-- >     , const VkImageMemoryBarrier* pImageMemoryBarriers
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdWaitEvents vkCmdWaitEvents registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdWaitEvents <- vkGetInstanceProc @VkCmdWaitEvents vkInstance
--
-- or less efficient:
--
-- > myCmdWaitEvents <- vkGetProc @VkCmdWaitEvents
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
vkCmdWaitEventsSafe ::
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
vkCmdWaitEventsSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdWaitEvents)

{-# NOINLINE vkCmdWaitEventsSafe #-}
##endif

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdWaitEvents
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdWaitEvents vkCmdWaitEvents registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic" unwrapVkCmdWaitEvents ::
               PFN_vkCmdWaitEvents -> HS_vkCmdWaitEvents

foreign import ccall safe "dynamic" unwrapVkCmdWaitEventsSafe ::
               PFN_vkCmdWaitEvents -> HS_vkCmdWaitEvents

instance VulkanProc "vkCmdWaitEvents" where
        type VkProcType "vkCmdWaitEvents" = HS_vkCmdWaitEvents
        vkProcSymbol = _VkCmdWaitEvents

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdWaitEvents

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdWaitEventsSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- > void vkCmdPipelineBarrier
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkPipelineStageFlags srcStageMask
-- >     , VkPipelineStageFlags dstStageMask
-- >     , VkDependencyFlags dependencyFlags
-- >     , uint32_t memoryBarrierCount
-- >     , const VkMemoryBarrier* pMemoryBarriers
-- >     , uint32_t bufferMemoryBarrierCount
-- >     , const VkBufferMemoryBarrier* pBufferMemoryBarriers
-- >     , uint32_t imageMemoryBarrierCount
-- >     , const VkImageMemoryBarrier* pImageMemoryBarriers
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdPipelineBarrier vkCmdPipelineBarrier registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdPipelineBarrier <- vkGetInstanceProc @VkCmdPipelineBarrier vkInstance
--
-- or less efficient:
--
-- > myCmdPipelineBarrier <- vkGetProc @VkCmdPipelineBarrier
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCmdPipelineBarrier
  = unsafeDupablePerformIO (vkGetProc @VkCmdPipelineBarrier)

{-# NOINLINE vkCmdPipelineBarrier #-}
##endif

-- |
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- > void vkCmdPipelineBarrier
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkPipelineStageFlags srcStageMask
-- >     , VkPipelineStageFlags dstStageMask
-- >     , VkDependencyFlags dependencyFlags
-- >     , uint32_t memoryBarrierCount
-- >     , const VkMemoryBarrier* pMemoryBarriers
-- >     , uint32_t bufferMemoryBarrierCount
-- >     , const VkBufferMemoryBarrier* pBufferMemoryBarriers
-- >     , uint32_t imageMemoryBarrierCount
-- >     , const VkImageMemoryBarrier* pImageMemoryBarriers
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdPipelineBarrier vkCmdPipelineBarrier registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdPipelineBarrier <- vkGetInstanceProc @VkCmdPipelineBarrier vkInstance
--
-- or less efficient:
--
-- > myCmdPipelineBarrier <- vkGetProc @VkCmdPipelineBarrier
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCmdPipelineBarrierSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdPipelineBarrier)

{-# NOINLINE vkCmdPipelineBarrierSafe #-}
##endif

-- | Queues: 'transfer', 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdPipelineBarrier
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdPipelineBarrier vkCmdPipelineBarrier registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic" unwrapVkCmdPipelineBarrier ::
               PFN_vkCmdPipelineBarrier -> HS_vkCmdPipelineBarrier

foreign import ccall safe "dynamic" unwrapVkCmdPipelineBarrierSafe
               :: PFN_vkCmdPipelineBarrier -> HS_vkCmdPipelineBarrier

instance VulkanProc "vkCmdPipelineBarrier" where
        type VkProcType "vkCmdPipelineBarrier" = HS_vkCmdPipelineBarrier
        vkProcSymbol = _VkCmdPipelineBarrier

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdPipelineBarrier

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdPipelineBarrierSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- > void vkCmdBeginQuery
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkQueryPool queryPool
-- >     , uint32_t query
-- >     , VkQueryControlFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBeginQuery vkCmdBeginQuery registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBeginQuery <- vkGetInstanceProc @VkCmdBeginQuery vkInstance
--
-- or less efficient:
--
-- > myCmdBeginQuery <- vkGetProc @VkCmdBeginQuery
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdBeginQuery" vkCmdBeginQuery ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkQueryPool -- ^ queryPool
                             -> Word32 -- ^ query
                                       -> VkQueryControlFlags -- ^ flags
                                                              -> IO ()

##else
vkCmdBeginQuery ::
                VkCommandBuffer -- ^ commandBuffer
                                ->
                  VkQueryPool -- ^ queryPool
                              -> Word32 -- ^ query
                                        -> VkQueryControlFlags -- ^ flags
                                                               -> IO ()
vkCmdBeginQuery
  = unsafeDupablePerformIO (vkGetProc @VkCmdBeginQuery)

{-# NOINLINE vkCmdBeginQuery #-}
##endif

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- > void vkCmdBeginQuery
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkQueryPool queryPool
-- >     , uint32_t query
-- >     , VkQueryControlFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBeginQuery vkCmdBeginQuery registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBeginQuery <- vkGetInstanceProc @VkCmdBeginQuery vkInstance
--
-- or less efficient:
--
-- > myCmdBeginQuery <- vkGetProc @VkCmdBeginQuery
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdBeginQuery" vkCmdBeginQuerySafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkQueryPool -- ^ queryPool
                             -> Word32 -- ^ query
                                       -> VkQueryControlFlags -- ^ flags
                                                              -> IO ()

##else
vkCmdBeginQuerySafe ::
                    VkCommandBuffer -- ^ commandBuffer
                                    ->
                      VkQueryPool -- ^ queryPool
                                  -> Word32 -- ^ query
                                            -> VkQueryControlFlags -- ^ flags
                                                                   -> IO ()
vkCmdBeginQuerySafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdBeginQuery)

{-# NOINLINE vkCmdBeginQuerySafe #-}
##endif

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdBeginQuery
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     , VkQueryControlFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBeginQuery vkCmdBeginQuery registry at www.khronos.org>
type HS_vkCmdBeginQuery =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkQueryPool -- ^ queryPool
                   -> Word32 -- ^ query
                             -> VkQueryControlFlags -- ^ flags
                                                    -> IO ()

type PFN_vkCmdBeginQuery = FunPtr HS_vkCmdBeginQuery

foreign import ccall unsafe "dynamic" unwrapVkCmdBeginQuery ::
               PFN_vkCmdBeginQuery -> HS_vkCmdBeginQuery

foreign import ccall safe "dynamic" unwrapVkCmdBeginQuerySafe ::
               PFN_vkCmdBeginQuery -> HS_vkCmdBeginQuery

instance VulkanProc "vkCmdBeginQuery" where
        type VkProcType "vkCmdBeginQuery" = HS_vkCmdBeginQuery
        vkProcSymbol = _VkCmdBeginQuery

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdBeginQuery

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdBeginQuerySafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- > void vkCmdEndQuery
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkQueryPool queryPool
-- >     , uint32_t query
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdEndQuery vkCmdEndQuery registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdEndQuery <- vkGetInstanceProc @VkCmdEndQuery vkInstance
--
-- or less efficient:
--
-- > myCmdEndQuery <- vkGetProc @VkCmdEndQuery
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdEndQuery" vkCmdEndQuery ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkQueryPool -- ^ queryPool
                                              -> Word32 -- ^ query
                                                        -> IO ()

##else
vkCmdEndQuery :: VkCommandBuffer -- ^ commandBuffer
                                 -> VkQueryPool -- ^ queryPool
                                                -> Word32 -- ^ query
                                                          -> IO ()
vkCmdEndQuery = unsafeDupablePerformIO (vkGetProc @VkCmdEndQuery)

{-# NOINLINE vkCmdEndQuery #-}
##endif

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- > void vkCmdEndQuery
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkQueryPool queryPool
-- >     , uint32_t query
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdEndQuery vkCmdEndQuery registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdEndQuery <- vkGetInstanceProc @VkCmdEndQuery vkInstance
--
-- or less efficient:
--
-- > myCmdEndQuery <- vkGetProc @VkCmdEndQuery
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdEndQuery" vkCmdEndQuerySafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkQueryPool -- ^ queryPool
                                              -> Word32 -- ^ query
                                                        -> IO ()

##else
vkCmdEndQuerySafe ::
                  VkCommandBuffer -- ^ commandBuffer
                                  -> VkQueryPool -- ^ queryPool
                                                 -> Word32 -- ^ query
                                                           -> IO ()
vkCmdEndQuerySafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdEndQuery)

{-# NOINLINE vkCmdEndQuerySafe #-}
##endif

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdEndQuery
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdEndQuery vkCmdEndQuery registry at www.khronos.org>
type HS_vkCmdEndQuery =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkQueryPool -- ^ queryPool
                                    -> Word32 -- ^ query
                                              -> IO ()

type PFN_vkCmdEndQuery = FunPtr HS_vkCmdEndQuery

foreign import ccall unsafe "dynamic" unwrapVkCmdEndQuery ::
               PFN_vkCmdEndQuery -> HS_vkCmdEndQuery

foreign import ccall safe "dynamic" unwrapVkCmdEndQuerySafe ::
               PFN_vkCmdEndQuery -> HS_vkCmdEndQuery

instance VulkanProc "vkCmdEndQuery" where
        type VkProcType "vkCmdEndQuery" = HS_vkCmdEndQuery
        vkProcSymbol = _VkCmdEndQuery

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdEndQuery

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdEndQuerySafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- > void vkCmdResetQueryPool
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkQueryPool queryPool
-- >     , uint32_t firstQuery
-- >     , uint32_t queryCount
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdResetQueryPool vkCmdResetQueryPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdResetQueryPool <- vkGetInstanceProc @VkCmdResetQueryPool vkInstance
--
-- or less efficient:
--
-- > myCmdResetQueryPool <- vkGetProc @VkCmdResetQueryPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdResetQueryPool"
               vkCmdResetQueryPool ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkQueryPool -- ^ queryPool
                                              -> Word32 -- ^ firstQuery
                                                        -> Word32 -- ^ queryCount
                                                                  -> IO ()

##else
vkCmdResetQueryPool ::
                    VkCommandBuffer -- ^ commandBuffer
                                    -> VkQueryPool -- ^ queryPool
                                                   -> Word32 -- ^ firstQuery
                                                             -> Word32 -- ^ queryCount
                                                                       -> IO ()
vkCmdResetQueryPool
  = unsafeDupablePerformIO (vkGetProc @VkCmdResetQueryPool)

{-# NOINLINE vkCmdResetQueryPool #-}
##endif

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- > void vkCmdResetQueryPool
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkQueryPool queryPool
-- >     , uint32_t firstQuery
-- >     , uint32_t queryCount
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdResetQueryPool vkCmdResetQueryPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdResetQueryPool <- vkGetInstanceProc @VkCmdResetQueryPool vkInstance
--
-- or less efficient:
--
-- > myCmdResetQueryPool <- vkGetProc @VkCmdResetQueryPool
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdResetQueryPool"
               vkCmdResetQueryPoolSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkQueryPool -- ^ queryPool
                                              -> Word32 -- ^ firstQuery
                                                        -> Word32 -- ^ queryCount
                                                                  -> IO ()

##else
vkCmdResetQueryPoolSafe ::
                        VkCommandBuffer -- ^ commandBuffer
                                        -> VkQueryPool -- ^ queryPool
                                                       -> Word32 -- ^ firstQuery
                                                                 -> Word32 -- ^ queryCount
                                                                           -> IO ()
vkCmdResetQueryPoolSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdResetQueryPool)

{-# NOINLINE vkCmdResetQueryPoolSafe #-}
##endif

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdResetQueryPool
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     , uint32_t queryCount
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdResetQueryPool vkCmdResetQueryPool registry at www.khronos.org>
type HS_vkCmdResetQueryPool =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkQueryPool -- ^ queryPool
                                    -> Word32 -- ^ firstQuery
                                              -> Word32 -- ^ queryCount
                                                        -> IO ()

type PFN_vkCmdResetQueryPool = FunPtr HS_vkCmdResetQueryPool

foreign import ccall unsafe "dynamic" unwrapVkCmdResetQueryPool ::
               PFN_vkCmdResetQueryPool -> HS_vkCmdResetQueryPool

foreign import ccall safe "dynamic" unwrapVkCmdResetQueryPoolSafe
               :: PFN_vkCmdResetQueryPool -> HS_vkCmdResetQueryPool

instance VulkanProc "vkCmdResetQueryPool" where
        type VkProcType "vkCmdResetQueryPool" = HS_vkCmdResetQueryPool
        vkProcSymbol = _VkCmdResetQueryPool

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdResetQueryPool

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdResetQueryPoolSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- Pipeline: @transfer@
--
-- > void vkCmdWriteTimestamp
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkPipelineStageFlagBits pipelineStage
-- >     , VkQueryPool queryPool
-- >     , uint32_t query
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdWriteTimestamp vkCmdWriteTimestamp registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdWriteTimestamp <- vkGetInstanceProc @VkCmdWriteTimestamp vkInstance
--
-- or less efficient:
--
-- > myCmdWriteTimestamp <- vkGetProc @VkCmdWriteTimestamp
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdWriteTimestamp"
               vkCmdWriteTimestamp ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineStageFlagBits -- ^ pipelineStage
                                         -> VkQueryPool -- ^ queryPool
                                                        -> Word32 -- ^ query
                                                                  -> IO ()

##else
vkCmdWriteTimestamp ::
                    VkCommandBuffer -- ^ commandBuffer
                                    ->
                      VkPipelineStageFlagBits -- ^ pipelineStage
                                              -> VkQueryPool -- ^ queryPool
                                                             -> Word32 -- ^ query
                                                                       -> IO ()
vkCmdWriteTimestamp
  = unsafeDupablePerformIO (vkGetProc @VkCmdWriteTimestamp)

{-# NOINLINE vkCmdWriteTimestamp #-}
##endif

-- |
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- Pipeline: @transfer@
--
-- > void vkCmdWriteTimestamp
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkPipelineStageFlagBits pipelineStage
-- >     , VkQueryPool queryPool
-- >     , uint32_t query
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdWriteTimestamp vkCmdWriteTimestamp registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdWriteTimestamp <- vkGetInstanceProc @VkCmdWriteTimestamp vkInstance
--
-- or less efficient:
--
-- > myCmdWriteTimestamp <- vkGetProc @VkCmdWriteTimestamp
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdWriteTimestamp"
               vkCmdWriteTimestampSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineStageFlagBits -- ^ pipelineStage
                                         -> VkQueryPool -- ^ queryPool
                                                        -> Word32 -- ^ query
                                                                  -> IO ()

##else
vkCmdWriteTimestampSafe ::
                        VkCommandBuffer -- ^ commandBuffer
                                        ->
                          VkPipelineStageFlagBits -- ^ pipelineStage
                                                  -> VkQueryPool -- ^ queryPool
                                                                 -> Word32 -- ^ query
                                                                           -> IO ()
vkCmdWriteTimestampSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdWriteTimestamp)

{-# NOINLINE vkCmdWriteTimestampSafe #-}
##endif

-- | Queues: 'transfer', 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   Pipeline: @transfer@
--
--   > void vkCmdWriteTimestamp
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineStageFlagBits pipelineStage
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdWriteTimestamp vkCmdWriteTimestamp registry at www.khronos.org>
type HS_vkCmdWriteTimestamp =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkPipelineStageFlagBits -- ^ pipelineStage
                               -> VkQueryPool -- ^ queryPool
                                              -> Word32 -- ^ query
                                                        -> IO ()

type PFN_vkCmdWriteTimestamp = FunPtr HS_vkCmdWriteTimestamp

foreign import ccall unsafe "dynamic" unwrapVkCmdWriteTimestamp ::
               PFN_vkCmdWriteTimestamp -> HS_vkCmdWriteTimestamp

foreign import ccall safe "dynamic" unwrapVkCmdWriteTimestampSafe
               :: PFN_vkCmdWriteTimestamp -> HS_vkCmdWriteTimestamp

instance VulkanProc "vkCmdWriteTimestamp" where
        type VkProcType "vkCmdWriteTimestamp" = HS_vkCmdWriteTimestamp
        vkProcSymbol = _VkCmdWriteTimestamp

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdWriteTimestamp

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdWriteTimestampSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdCopyQueryPoolResults
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkQueryPool queryPool
-- >     , uint32_t firstQuery
-- >     , uint32_t queryCount
-- >     , VkBuffer dstBuffer
-- >     , VkDeviceSize dstOffset
-- >     , VkDeviceSize stride
-- >     , VkQueryResultFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdCopyQueryPoolResults vkCmdCopyQueryPoolResults registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdCopyQueryPoolResults <- vkGetInstanceProc @VkCmdCopyQueryPoolResults vkInstance
--
-- or less efficient:
--
-- > myCmdCopyQueryPoolResults <- vkGetProc @VkCmdCopyQueryPoolResults
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCmdCopyQueryPoolResults
  = unsafeDupablePerformIO (vkGetProc @VkCmdCopyQueryPoolResults)

{-# NOINLINE vkCmdCopyQueryPoolResults #-}
##endif

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @outside@
--
-- Pipeline: @transfer@
--
-- > void vkCmdCopyQueryPoolResults
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkQueryPool queryPool
-- >     , uint32_t firstQuery
-- >     , uint32_t queryCount
-- >     , VkBuffer dstBuffer
-- >     , VkDeviceSize dstOffset
-- >     , VkDeviceSize stride
-- >     , VkQueryResultFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdCopyQueryPoolResults vkCmdCopyQueryPoolResults registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdCopyQueryPoolResults <- vkGetInstanceProc @VkCmdCopyQueryPoolResults vkInstance
--
-- or less efficient:
--
-- > myCmdCopyQueryPoolResults <- vkGetProc @VkCmdCopyQueryPoolResults
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCmdCopyQueryPoolResultsSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdCopyQueryPoolResults)

{-# NOINLINE vkCmdCopyQueryPoolResultsSafe #-}
##endif

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @outside@
--
--   Pipeline: @transfer@
--
--   > void vkCmdCopyQueryPoolResults
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdCopyQueryPoolResults vkCmdCopyQueryPoolResults registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic"
               unwrapVkCmdCopyQueryPoolResults ::
               PFN_vkCmdCopyQueryPoolResults -> HS_vkCmdCopyQueryPoolResults

foreign import ccall safe "dynamic"
               unwrapVkCmdCopyQueryPoolResultsSafe ::
               PFN_vkCmdCopyQueryPoolResults -> HS_vkCmdCopyQueryPoolResults

instance VulkanProc "vkCmdCopyQueryPoolResults" where
        type VkProcType "vkCmdCopyQueryPoolResults" =
             HS_vkCmdCopyQueryPoolResults
        vkProcSymbol = _VkCmdCopyQueryPoolResults

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdCopyQueryPoolResults

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdCopyQueryPoolResultsSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- > void vkCmdPushConstants
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkPipelineLayout layout
-- >     , VkShaderStageFlags stageFlags
-- >     , uint32_t offset
-- >     , uint32_t size
-- >     , const void* pValues
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdPushConstants vkCmdPushConstants registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdPushConstants <- vkGetInstanceProc @VkCmdPushConstants vkInstance
--
-- or less efficient:
--
-- > myCmdPushConstants <- vkGetProc @VkCmdPushConstants
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
vkCmdPushConstants ::
                   VkCommandBuffer -- ^ commandBuffer
                                   ->
                     VkPipelineLayout -- ^ layout
                                      ->
                       VkShaderStageFlags -- ^ stageFlags
                                          -> Word32 -- ^ offset
                                                    -> Word32 -- ^ size
                                                              -> Ptr Void -- ^ pValues
                                                                          -> IO ()
vkCmdPushConstants
  = unsafeDupablePerformIO (vkGetProc @VkCmdPushConstants)

{-# NOINLINE vkCmdPushConstants #-}
##endif

-- |
-- Queues: 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- > void vkCmdPushConstants
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkPipelineLayout layout
-- >     , VkShaderStageFlags stageFlags
-- >     , uint32_t offset
-- >     , uint32_t size
-- >     , const void* pValues
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdPushConstants vkCmdPushConstants registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdPushConstants <- vkGetInstanceProc @VkCmdPushConstants vkInstance
--
-- or less efficient:
--
-- > myCmdPushConstants <- vkGetProc @VkCmdPushConstants
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
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

##else
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
vkCmdPushConstantsSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdPushConstants)

{-# NOINLINE vkCmdPushConstantsSafe #-}
##endif

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdPushConstants
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineLayout layout
--   >     , VkShaderStageFlags stageFlags
--   >     , uint32_t offset
--   >     , uint32_t size
--   >     , const void* pValues
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdPushConstants vkCmdPushConstants registry at www.khronos.org>
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

foreign import ccall unsafe "dynamic" unwrapVkCmdPushConstants ::
               PFN_vkCmdPushConstants -> HS_vkCmdPushConstants

foreign import ccall safe "dynamic" unwrapVkCmdPushConstantsSafe ::
               PFN_vkCmdPushConstants -> HS_vkCmdPushConstants

instance VulkanProc "vkCmdPushConstants" where
        type VkProcType "vkCmdPushConstants" = HS_vkCmdPushConstants
        vkProcSymbol = _VkCmdPushConstants

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdPushConstants

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdPushConstantsSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @outside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdBeginRenderPass
-- >     ( VkCommandBuffer commandBuffer
-- >     , const VkRenderPassBeginInfo* pRenderPassBegin
-- >     , VkSubpassContents contents
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBeginRenderPass vkCmdBeginRenderPass registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBeginRenderPass <- vkGetInstanceProc @VkCmdBeginRenderPass vkInstance
--
-- or less efficient:
--
-- > myCmdBeginRenderPass <- vkGetProc @VkCmdBeginRenderPass
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdBeginRenderPass"
               vkCmdBeginRenderPass ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Ptr VkRenderPassBeginInfo -- ^ pRenderPassBegin
                                           -> VkSubpassContents -- ^ contents
                                                                -> IO ()

##else
vkCmdBeginRenderPass ::
                     VkCommandBuffer -- ^ commandBuffer
                                     ->
                       Ptr VkRenderPassBeginInfo -- ^ pRenderPassBegin
                                                 -> VkSubpassContents -- ^ contents
                                                                      -> IO ()
vkCmdBeginRenderPass
  = unsafeDupablePerformIO (vkGetProc @VkCmdBeginRenderPass)

{-# NOINLINE vkCmdBeginRenderPass #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @outside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdBeginRenderPass
-- >     ( VkCommandBuffer commandBuffer
-- >     , const VkRenderPassBeginInfo* pRenderPassBegin
-- >     , VkSubpassContents contents
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBeginRenderPass vkCmdBeginRenderPass registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBeginRenderPass <- vkGetInstanceProc @VkCmdBeginRenderPass vkInstance
--
-- or less efficient:
--
-- > myCmdBeginRenderPass <- vkGetProc @VkCmdBeginRenderPass
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdBeginRenderPass"
               vkCmdBeginRenderPassSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Ptr VkRenderPassBeginInfo -- ^ pRenderPassBegin
                                           -> VkSubpassContents -- ^ contents
                                                                -> IO ()

##else
vkCmdBeginRenderPassSafe ::
                         VkCommandBuffer -- ^ commandBuffer
                                         ->
                           Ptr VkRenderPassBeginInfo -- ^ pRenderPassBegin
                                                     -> VkSubpassContents -- ^ contents
                                                                          -> IO ()
vkCmdBeginRenderPassSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdBeginRenderPass)

{-# NOINLINE vkCmdBeginRenderPassSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @outside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdBeginRenderPass
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkRenderPassBeginInfo* pRenderPassBegin
--   >     , VkSubpassContents contents
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBeginRenderPass vkCmdBeginRenderPass registry at www.khronos.org>
type HS_vkCmdBeginRenderPass =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Ptr VkRenderPassBeginInfo -- ^ pRenderPassBegin
                                 -> VkSubpassContents -- ^ contents
                                                      -> IO ()

type PFN_vkCmdBeginRenderPass = FunPtr HS_vkCmdBeginRenderPass

foreign import ccall unsafe "dynamic" unwrapVkCmdBeginRenderPass ::
               PFN_vkCmdBeginRenderPass -> HS_vkCmdBeginRenderPass

foreign import ccall safe "dynamic" unwrapVkCmdBeginRenderPassSafe
               :: PFN_vkCmdBeginRenderPass -> HS_vkCmdBeginRenderPass

instance VulkanProc "vkCmdBeginRenderPass" where
        type VkProcType "vkCmdBeginRenderPass" = HS_vkCmdBeginRenderPass
        vkProcSymbol = _VkCmdBeginRenderPass

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdBeginRenderPass

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdBeginRenderPassSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdNextSubpass
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkSubpassContents contents
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdNextSubpass vkCmdNextSubpass registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdNextSubpass <- vkGetInstanceProc @VkCmdNextSubpass vkInstance
--
-- or less efficient:
--
-- > myCmdNextSubpass <- vkGetProc @VkCmdNextSubpass
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdNextSubpass" vkCmdNextSubpass ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkSubpassContents -- ^ contents
                                                    -> IO ()

##else
vkCmdNextSubpass :: VkCommandBuffer -- ^ commandBuffer
                                    -> VkSubpassContents -- ^ contents
                                                         -> IO ()
vkCmdNextSubpass
  = unsafeDupablePerformIO (vkGetProc @VkCmdNextSubpass)

{-# NOINLINE vkCmdNextSubpass #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdNextSubpass
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkSubpassContents contents
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdNextSubpass vkCmdNextSubpass registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdNextSubpass <- vkGetInstanceProc @VkCmdNextSubpass vkInstance
--
-- or less efficient:
--
-- > myCmdNextSubpass <- vkGetProc @VkCmdNextSubpass
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdNextSubpass" vkCmdNextSubpassSafe
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> VkSubpassContents -- ^ contents
                                                       -> IO ()

##else
vkCmdNextSubpassSafe ::
                     VkCommandBuffer -- ^ commandBuffer
                                     -> VkSubpassContents -- ^ contents
                                                          -> IO ()
vkCmdNextSubpassSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdNextSubpass)

{-# NOINLINE vkCmdNextSubpassSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdNextSubpass
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkSubpassContents contents
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdNextSubpass vkCmdNextSubpass registry at www.khronos.org>
type HS_vkCmdNextSubpass =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkSubpassContents -- ^ contents
                                          -> IO ()

type PFN_vkCmdNextSubpass = FunPtr HS_vkCmdNextSubpass

foreign import ccall unsafe "dynamic" unwrapVkCmdNextSubpass ::
               PFN_vkCmdNextSubpass -> HS_vkCmdNextSubpass

foreign import ccall safe "dynamic" unwrapVkCmdNextSubpassSafe ::
               PFN_vkCmdNextSubpass -> HS_vkCmdNextSubpass

instance VulkanProc "vkCmdNextSubpass" where
        type VkProcType "vkCmdNextSubpass" = HS_vkCmdNextSubpass
        vkProcSymbol = _VkCmdNextSubpass

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdNextSubpass

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdNextSubpassSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdEndRenderPass
-- >     ( VkCommandBuffer commandBuffer
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdEndRenderPass vkCmdEndRenderPass registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdEndRenderPass <- vkGetInstanceProc @VkCmdEndRenderPass vkInstance
--
-- or less efficient:
--
-- > myCmdEndRenderPass <- vkGetProc @VkCmdEndRenderPass
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdEndRenderPass" vkCmdEndRenderPass
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> IO ()

##else
vkCmdEndRenderPass :: VkCommandBuffer -- ^ commandBuffer
                                      -> IO ()
vkCmdEndRenderPass
  = unsafeDupablePerformIO (vkGetProc @VkCmdEndRenderPass)

{-# NOINLINE vkCmdEndRenderPass #-}
##endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdEndRenderPass
-- >     ( VkCommandBuffer commandBuffer
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdEndRenderPass vkCmdEndRenderPass registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdEndRenderPass <- vkGetInstanceProc @VkCmdEndRenderPass vkInstance
--
-- or less efficient:
--
-- > myCmdEndRenderPass <- vkGetProc @VkCmdEndRenderPass
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdEndRenderPass"
               vkCmdEndRenderPassSafe :: VkCommandBuffer -- ^ commandBuffer
                                                         -> IO ()

##else
vkCmdEndRenderPassSafe :: VkCommandBuffer -- ^ commandBuffer
                                          -> IO ()
vkCmdEndRenderPassSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdEndRenderPass)

{-# NOINLINE vkCmdEndRenderPassSafe #-}
##endif

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdEndRenderPass
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdEndRenderPass vkCmdEndRenderPass registry at www.khronos.org>
type HS_vkCmdEndRenderPass = VkCommandBuffer -- ^ commandBuffer
                                             -> IO ()

type PFN_vkCmdEndRenderPass = FunPtr HS_vkCmdEndRenderPass

foreign import ccall unsafe "dynamic" unwrapVkCmdEndRenderPass ::
               PFN_vkCmdEndRenderPass -> HS_vkCmdEndRenderPass

foreign import ccall safe "dynamic" unwrapVkCmdEndRenderPassSafe ::
               PFN_vkCmdEndRenderPass -> HS_vkCmdEndRenderPass

instance VulkanProc "vkCmdEndRenderPass" where
        type VkProcType "vkCmdEndRenderPass" = HS_vkCmdEndRenderPass
        vkProcSymbol = _VkCmdEndRenderPass

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdEndRenderPass

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdEndRenderPassSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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

-- |
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- > void vkCmdExecuteCommands
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t commandBufferCount
-- >     , const VkCommandBuffer* pCommandBuffers
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdExecuteCommands vkCmdExecuteCommands registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdExecuteCommands <- vkGetInstanceProc @VkCmdExecuteCommands vkInstance
--
-- or less efficient:
--
-- > myCmdExecuteCommands <- vkGetProc @VkCmdExecuteCommands
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCmdExecuteCommands"
               vkCmdExecuteCommands ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ commandBufferCount
                                         -> Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                                -> IO ()

##else
vkCmdExecuteCommands ::
                     VkCommandBuffer -- ^ commandBuffer
                                     -> Word32 -- ^ commandBufferCount
                                               -> Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                                      -> IO ()
vkCmdExecuteCommands
  = unsafeDupablePerformIO (vkGetProc @VkCmdExecuteCommands)

{-# NOINLINE vkCmdExecuteCommands #-}
##endif

-- |
-- Queues: 'transfer', 'graphics', 'compute'.
--
-- Renderpass: @both@
--
-- > void vkCmdExecuteCommands
-- >     ( VkCommandBuffer commandBuffer
-- >     , uint32_t commandBufferCount
-- >     , const VkCommandBuffer* pCommandBuffers
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdExecuteCommands vkCmdExecuteCommands registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdExecuteCommands <- vkGetInstanceProc @VkCmdExecuteCommands vkInstance
--
-- or less efficient:
--
-- > myCmdExecuteCommands <- vkGetProc @VkCmdExecuteCommands
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
##ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCmdExecuteCommands"
               vkCmdExecuteCommandsSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Word32 -- ^ commandBufferCount
                                         -> Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                                -> IO ()

##else
vkCmdExecuteCommandsSafe ::
                         VkCommandBuffer -- ^ commandBuffer
                                         -> Word32 -- ^ commandBufferCount
                                                   -> Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                                          -> IO ()
vkCmdExecuteCommandsSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdExecuteCommands)

{-# NOINLINE vkCmdExecuteCommandsSafe #-}
##endif

-- | Queues: 'transfer', 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdExecuteCommands
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t commandBufferCount
--   >     , const VkCommandBuffer* pCommandBuffers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdExecuteCommands vkCmdExecuteCommands registry at www.khronos.org>
type HS_vkCmdExecuteCommands =
     VkCommandBuffer -- ^ commandBuffer
                     -> Word32 -- ^ commandBufferCount
                               -> Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                      -> IO ()

type PFN_vkCmdExecuteCommands = FunPtr HS_vkCmdExecuteCommands

foreign import ccall unsafe "dynamic" unwrapVkCmdExecuteCommands ::
               PFN_vkCmdExecuteCommands -> HS_vkCmdExecuteCommands

foreign import ccall safe "dynamic" unwrapVkCmdExecuteCommandsSafe
               :: PFN_vkCmdExecuteCommands -> HS_vkCmdExecuteCommands

instance VulkanProc "vkCmdExecuteCommands" where
        type VkProcType "vkCmdExecuteCommands" = HS_vkCmdExecuteCommands
        vkProcSymbol = _VkCmdExecuteCommands

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdExecuteCommands

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkCmdExecuteCommandsSafe

        {-# INLINE unwrapVkProcPtrSafe #-}
