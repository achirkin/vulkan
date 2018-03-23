#include "vulkan/vulkan.h"

{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
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
        vkCreateInstance, vkCreateInstanceSafe, vkDestroyInstance,
        vkDestroyInstanceSafe, vkEnumeratePhysicalDevices,
        vkEnumeratePhysicalDevicesSafe, vkGetPhysicalDeviceFeatures,
        vkGetPhysicalDeviceFeaturesSafe,
        vkGetPhysicalDeviceFormatProperties,
        vkGetPhysicalDeviceFormatPropertiesSafe,
        vkGetPhysicalDeviceImageFormatProperties,
        vkGetPhysicalDeviceImageFormatPropertiesSafe,
        vkGetPhysicalDeviceProperties, vkGetPhysicalDevicePropertiesSafe,
        vkGetPhysicalDeviceQueueFamilyProperties,
        vkGetPhysicalDeviceQueueFamilyPropertiesSafe,
        vkGetPhysicalDeviceMemoryProperties,
        vkGetPhysicalDeviceMemoryPropertiesSafe, vkGetInstanceProcAddr,
        vkGetInstanceProcAddrSafe, vkGetDeviceProcAddr,
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
        vkCreateDevice, vkCreateDeviceSafe, vkDestroyDevice,
        vkDestroyDeviceSafe,
        module Graphics.Vulkan.Types.Enum.VkDeviceQueueCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo,
        -- ** Extension discovery commands
        vkEnumerateInstanceExtensionProperties,
        vkEnumerateInstanceExtensionPropertiesSafe,
        vkEnumerateDeviceExtensionProperties,
        vkEnumerateDeviceExtensionPropertiesSafe,
        module Graphics.Vulkan.Types.Struct.VkExtensionProperties,
        -- ** Layer discovery commands
        vkEnumerateInstanceLayerProperties,
        vkEnumerateInstanceLayerPropertiesSafe,
        vkEnumerateDeviceLayerProperties,
        vkEnumerateDeviceLayerPropertiesSafe,
        module Graphics.Vulkan.Types.Struct.VkLayerProperties,
        -- ** queue commands
        vkGetDeviceQueue, vkGetDeviceQueueSafe, vkQueueSubmit,
        vkQueueSubmitSafe, vkQueueWaitIdle, vkQueueWaitIdleSafe,
        vkDeviceWaitIdle, vkDeviceWaitIdleSafe,
        module Graphics.Vulkan.Types.Enum.VkPipelineStageFlags,
        module Graphics.Vulkan.Types.Struct.VkSubmitInfo, -- ** Memory commands
                                                          vkAllocateMemory,
        vkAllocateMemorySafe, vkFreeMemory, vkFreeMemorySafe, vkMapMemory,
        vkMapMemorySafe, vkUnmapMemory, vkUnmapMemorySafe,
        vkFlushMappedMemoryRanges, vkFlushMappedMemoryRangesSafe,
        vkInvalidateMappedMemoryRanges, vkInvalidateMappedMemoryRangesSafe,
        vkGetDeviceMemoryCommitment, vkGetDeviceMemoryCommitmentSafe,
        module Graphics.Vulkan.Types.Struct.VkMappedMemoryRange,
        module Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo,
        -- ** Memory management API commands
        vkBindBufferMemory, vkBindBufferMemorySafe, vkBindImageMemory,
        vkBindImageMemorySafe, vkGetBufferMemoryRequirements,
        vkGetBufferMemoryRequirementsSafe, vkGetImageMemoryRequirements,
        vkGetImageMemoryRequirementsSafe,
        module Graphics.Vulkan.Types.Struct.VkMemoryRequirements,
        -- ** Sparse resource memory management API commands
        vkGetImageSparseMemoryRequirements,
        vkGetImageSparseMemoryRequirementsSafe,
        vkGetPhysicalDeviceSparseImageFormatProperties,
        vkGetPhysicalDeviceSparseImageFormatPropertiesSafe,
        vkQueueBindSparse, vkQueueBindSparseSafe,
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
        vkCreateFence, vkCreateFenceSafe, vkDestroyFence,
        vkDestroyFenceSafe, vkResetFences, vkResetFencesSafe,
        vkGetFenceStatus, vkGetFenceStatusSafe, vkWaitForFences,
        vkWaitForFencesSafe,
        module Graphics.Vulkan.Types.Enum.VkFenceCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkFenceCreateInfo,
        -- ** Queue semaphore commands
        vkCreateSemaphore, vkCreateSemaphoreSafe, vkDestroySemaphore,
        vkDestroySemaphoreSafe,
        module Graphics.Vulkan.Types.Struct.VkSemaphoreCreateInfo,
        -- ** Event commands
        vkCreateEvent, vkCreateEventSafe, vkDestroyEvent,
        vkDestroyEventSafe, vkGetEventStatus, vkGetEventStatusSafe,
        vkSetEvent, vkSetEventSafe, vkResetEvent, vkResetEventSafe,
        module Graphics.Vulkan.Types.Struct.VkEventCreateInfo,
        -- ** Query commands
        vkCreateQueryPool, vkCreateQueryPoolSafe, vkDestroyQueryPool,
        vkDestroyQueryPoolSafe, vkGetQueryPoolResults,
        vkGetQueryPoolResultsSafe,
        module Graphics.Vulkan.Types.Enum.VkQueryPipelineStatisticFlags,
        module Graphics.Vulkan.Types.Enum.VkQueryResultFlags,
        module Graphics.Vulkan.Types.Enum.VkQueryType,
        module Graphics.Vulkan.Types.Struct.VkQueryPoolCreateInfo,
        -- ** Buffer commands
        vkCreateBuffer, vkCreateBufferSafe, vkDestroyBuffer,
        vkDestroyBufferSafe,
        module Graphics.Vulkan.Types.Enum.VkBufferCreateFlags,
        module Graphics.Vulkan.Types.Enum.VkBufferUsageFlags,
        module Graphics.Vulkan.Types.Enum.VkSharingMode,
        module Graphics.Vulkan.Types.Struct.VkBufferCreateInfo,
        -- ** Buffer view commands
        vkCreateBufferView, vkCreateBufferViewSafe, vkDestroyBufferView,
        vkDestroyBufferViewSafe,
        module Graphics.Vulkan.Types.Struct.VkBufferViewCreateInfo,
        -- ** Image commands
        vkCreateImage, vkCreateImageSafe, vkDestroyImage,
        vkDestroyImageSafe, vkGetImageSubresourceLayout,
        vkGetImageSubresourceLayoutSafe,
        module Graphics.Vulkan.Types.Enum.VkImageLayout,
        module Graphics.Vulkan.Types.Struct.VkImageCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkSubresourceLayout,
        -- ** Image view commands
        vkCreateImageView, vkCreateImageViewSafe, vkDestroyImageView,
        vkDestroyImageViewSafe,
        module Graphics.Vulkan.Types.Enum.VkComponentSwizzle,
        module Graphics.Vulkan.Types.Enum.VkImageViewType,
        module Graphics.Vulkan.Types.Struct.VkComponentMapping,
        module Graphics.Vulkan.Types.Struct.VkImageSubresourceRange,
        module Graphics.Vulkan.Types.Struct.VkImageViewCreateInfo,
        -- ** Shader commands
        vkCreateShaderModule, vkCreateShaderModuleSafe,
        vkDestroyShaderModule, vkDestroyShaderModuleSafe,
        module Graphics.Vulkan.Types.Struct.VkShaderModuleCreateInfo,
        -- ** Pipeline Cache commands
        vkCreatePipelineCache, vkCreatePipelineCacheSafe,
        vkDestroyPipelineCache, vkDestroyPipelineCacheSafe,
        vkGetPipelineCacheData, vkGetPipelineCacheDataSafe,
        vkMergePipelineCaches, vkMergePipelineCachesSafe,
        module Graphics.Vulkan.Types.Struct.VkPipelineCacheCreateInfo,
        -- ** Pipeline commands
        vkCreateGraphicsPipelines, vkCreateGraphicsPipelinesSafe,
        vkCreateComputePipelines, vkCreateComputePipelinesSafe,
        vkDestroyPipeline, vkDestroyPipelineSafe,
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
        vkCreatePipelineLayout, vkCreatePipelineLayoutSafe,
        vkDestroyPipelineLayout, vkDestroyPipelineLayoutSafe,
        module Graphics.Vulkan.Types.Struct.VkPipelineLayoutCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPushConstantRange,
        -- ** Sampler commands
        vkCreateSampler, vkCreateSamplerSafe, vkDestroySampler,
        vkDestroySamplerSafe,
        module Graphics.Vulkan.Types.Enum.VkBorderColor,
        module Graphics.Vulkan.Types.Enum.VkFilter,
        module Graphics.Vulkan.Types.Enum.VkSamplerAddressMode,
        module Graphics.Vulkan.Types.Enum.VkSamplerMipmapMode,
        module Graphics.Vulkan.Types.Struct.VkSamplerCreateInfo,
        -- ** Descriptor set commands
        vkCreateDescriptorSetLayout, vkCreateDescriptorSetLayoutSafe,
        vkDestroyDescriptorSetLayout, vkDestroyDescriptorSetLayoutSafe,
        vkCreateDescriptorPool, vkCreateDescriptorPoolSafe,
        vkDestroyDescriptorPool, vkDestroyDescriptorPoolSafe,
        vkResetDescriptorPool, vkResetDescriptorPoolSafe,
        vkAllocateDescriptorSets, vkAllocateDescriptorSetsSafe,
        vkFreeDescriptorSets, vkFreeDescriptorSetsSafe,
        vkUpdateDescriptorSets, vkUpdateDescriptorSetsSafe,
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
        vkCreateFramebuffer, vkCreateFramebufferSafe, vkDestroyFramebuffer,
        vkDestroyFramebufferSafe, vkCreateRenderPass,
        vkCreateRenderPassSafe, vkDestroyRenderPass,
        vkDestroyRenderPassSafe, vkGetRenderAreaGranularity,
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
        vkCreateCommandPool, vkCreateCommandPoolSafe, vkDestroyCommandPool,
        vkDestroyCommandPoolSafe, vkResetCommandPool,
        vkResetCommandPoolSafe,
        module Graphics.Vulkan.Types.Enum.VkCommandPoolCreateFlags,
        module Graphics.Vulkan.Types.Enum.VkCommandPoolResetFlags,
        module Graphics.Vulkan.Types.Struct.VkCommandPoolCreateInfo,
        -- ** Command buffer commands
        vkAllocateCommandBuffers, vkAllocateCommandBuffersSafe,
        vkFreeCommandBuffers, vkFreeCommandBuffersSafe,
        vkBeginCommandBuffer, vkBeginCommandBufferSafe, vkEndCommandBuffer,
        vkEndCommandBufferSafe, vkResetCommandBuffer,
        vkResetCommandBufferSafe,
        module Graphics.Vulkan.Types.Enum.VkCommandBufferLevel,
        module Graphics.Vulkan.Types.Enum.VkCommandBufferResetFlags,
        module Graphics.Vulkan.Types.Enum.VkCommandBufferUsageFlags,
        module Graphics.Vulkan.Types.Enum.VkQueryControlFlags,
        module Graphics.Vulkan.Types.Struct.VkCommandBufferAllocateInfo,
        module Graphics.Vulkan.Types.Struct.VkCommandBufferBeginInfo,
        module Graphics.Vulkan.Types.Struct.VkCommandBufferInheritanceInfo,
        -- ** Command buffer building commands
        vkCmdBindPipeline, vkCmdBindPipelineSafe, vkCmdSetViewport,
        vkCmdSetViewportSafe, vkCmdSetScissor, vkCmdSetScissorSafe,
        vkCmdSetLineWidth, vkCmdSetLineWidthSafe, vkCmdSetDepthBias,
        vkCmdSetDepthBiasSafe, vkCmdSetBlendConstants,
        vkCmdSetBlendConstantsSafe, vkCmdSetDepthBounds,
        vkCmdSetDepthBoundsSafe, vkCmdSetStencilCompareMask,
        vkCmdSetStencilCompareMaskSafe, vkCmdSetStencilWriteMask,
        vkCmdSetStencilWriteMaskSafe, vkCmdSetStencilReference,
        vkCmdSetStencilReferenceSafe, vkCmdBindDescriptorSets,
        vkCmdBindDescriptorSetsSafe, vkCmdBindIndexBuffer,
        vkCmdBindIndexBufferSafe, vkCmdBindVertexBuffers,
        vkCmdBindVertexBuffersSafe, vkCmdDraw, vkCmdDrawSafe,
        vkCmdDrawIndexed, vkCmdDrawIndexedSafe, vkCmdDrawIndirect,
        vkCmdDrawIndirectSafe, vkCmdDrawIndexedIndirect,
        vkCmdDrawIndexedIndirectSafe, vkCmdDispatch, vkCmdDispatchSafe,
        vkCmdDispatchIndirect, vkCmdDispatchIndirectSafe, vkCmdCopyBuffer,
        vkCmdCopyBufferSafe, vkCmdCopyImage, vkCmdCopyImageSafe,
        vkCmdBlitImage, vkCmdBlitImageSafe, vkCmdCopyBufferToImage,
        vkCmdCopyBufferToImageSafe, vkCmdCopyImageToBuffer,
        vkCmdCopyImageToBufferSafe, vkCmdUpdateBuffer,
        vkCmdUpdateBufferSafe, vkCmdFillBuffer, vkCmdFillBufferSafe,
        vkCmdClearColorImage, vkCmdClearColorImageSafe,
        vkCmdClearDepthStencilImage, vkCmdClearDepthStencilImageSafe,
        vkCmdClearAttachments, vkCmdClearAttachmentsSafe,
        vkCmdResolveImage, vkCmdResolveImageSafe, vkCmdSetEvent,
        vkCmdSetEventSafe, vkCmdResetEvent, vkCmdResetEventSafe,
        vkCmdWaitEvents, vkCmdWaitEventsSafe, vkCmdPipelineBarrier,
        vkCmdPipelineBarrierSafe, vkCmdBeginQuery, vkCmdBeginQuerySafe,
        vkCmdEndQuery, vkCmdEndQuerySafe, vkCmdResetQueryPool,
        vkCmdResetQueryPoolSafe, vkCmdWriteTimestamp,
        vkCmdWriteTimestampSafe, vkCmdCopyQueryPoolResults,
        vkCmdCopyQueryPoolResultsSafe, vkCmdPushConstants,
        vkCmdPushConstantsSafe, vkCmdBeginRenderPass,
        vkCmdBeginRenderPassSafe, vkCmdNextSubpass, vkCmdNextSubpassSafe,
        vkCmdEndRenderPass, vkCmdEndRenderPassSafe, vkCmdExecuteCommands,
        vkCmdExecuteCommandsSafe,
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

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
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
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
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
