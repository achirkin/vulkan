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
module Graphics.Vulkan.Core_1_2
       (-- * Vulkan 1.2 core API interface definitions.
        -- |
        --
        -- @api = vulkan@
        --
        -- @name = VK_VERSION_1_2@
        --
        -- @number = 1.2@
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkConformanceVersion, VkAndroidSurfaceCreateFlagsKHR(..),
        VkBufferViewCreateFlags(..),
        VkBuildAccelerationStructureFlagsNV(..),
        VkCommandPoolTrimFlags(..), VkCommandPoolTrimFlagsKHR(..),
        VkDebugUtilsMessengerCallbackDataFlagsEXT(..),
        VkDebugUtilsMessengerCreateFlagsEXT(..),
        VkDescriptorBindingFlagsEXT(..), VkDescriptorPoolResetFlags(..),
        VkDescriptorUpdateTemplateCreateFlags(..),
        VkDescriptorUpdateTemplateCreateFlagsKHR(..),
        VkDeviceCreateFlags(..), VkDirectFBSurfaceCreateFlagsEXT(..),
        VkDisplayModeCreateFlagsKHR(..),
        VkDisplaySurfaceCreateFlagsKHR(..), VkEventCreateFlags(..),
        VkExternalFenceFeatureFlagsKHR(..),
        VkExternalFenceHandleTypeFlagsKHR(..),
        VkExternalMemoryFeatureFlagsKHR(..),
        VkExternalMemoryHandleTypeFlagsKHR(..),
        VkExternalSemaphoreFeatureFlagsKHR(..),
        VkExternalSemaphoreHandleTypeFlagsKHR(..),
        VkFenceImportFlagsKHR(..), VkGeometryFlagsNV(..),
        VkGeometryInstanceFlagsNV(..), VkHeadlessSurfaceCreateFlagsEXT(..),
        VkIOSSurfaceCreateFlagsMVK(..),
        VkImagePipeSurfaceCreateFlagsFUCHSIA(..),
        VkInstanceCreateFlags(..), VkMacOSSurfaceCreateFlagsMVK(..),
        VkMemoryAllocateFlagsKHR(..), VkMemoryMapFlags(..),
        VkMetalSurfaceCreateFlagsEXT(..), VkPeerMemoryFeatureFlagsKHR(..),
        VkPipelineColorBlendStateCreateFlags(..),
        VkPipelineCoverageModulationStateCreateFlagsNV(..),
        VkPipelineCoverageReductionStateCreateFlagsNV(..),
        VkPipelineCoverageToColorStateCreateFlagsNV(..),
        VkPipelineDepthStencilStateCreateFlags(..),
        VkPipelineDiscardRectangleStateCreateFlagsEXT(..),
        VkPipelineDynamicStateCreateFlags(..),
        VkPipelineInputAssemblyStateCreateFlags(..),
        VkPipelineLayoutCreateFlags(..),
        VkPipelineMultisampleStateCreateFlags(..),
        VkPipelineRasterizationConservativeStateCreateFlagsEXT(..),
        VkPipelineRasterizationDepthClipStateCreateFlagsEXT(..),
        VkPipelineRasterizationStateCreateFlags(..),
        VkPipelineRasterizationStateStreamCreateFlagsEXT(..),
        VkPipelineTessellationStateCreateFlags(..),
        VkPipelineVertexInputStateCreateFlags(..),
        VkPipelineViewportStateCreateFlags(..),
        VkPipelineViewportSwizzleStateCreateFlagsNV(..),
        VkQueryPoolCreateFlags(..), VkResolveModeFlagsKHR(..),
        VkSemaphoreCreateFlags(..), VkSemaphoreImportFlagsKHR(..),
        VkSemaphoreWaitFlagsKHR(..),
        VkStreamDescriptorSurfaceCreateFlagsGGP(..),
        VkValidationCacheCreateFlagsEXT(..), VkViSurfaceCreateFlagsNN(..),
        VkWaylandSurfaceCreateFlagsKHR(..),
        VkWin32SurfaceCreateFlagsKHR(..), VkXcbSurfaceCreateFlagsKHR(..),
        VkXlibSurfaceCreateFlagsKHR(..), VkDeviceCreateInfo,
        VkDeviceDiagnosticsConfigBitmaskNV(..), VkDeviceEventTypeEXT(..),
        VkDeviceGroupPresentModeBitmaskKHR(..), VkDeviceCreateFlagBits(..),
        VkDeviceDiagnosticsConfigFlagBitsNV(),
        VkDeviceDiagnosticsConfigFlagsNV(),
        VkDeviceGroupPresentModeFlagBitsKHR(),
        VkDeviceGroupPresentModeFlagsKHR(), VkDeviceQueueCreateBitmask(..),
        VkDeviceQueueCreateFlagBits(), VkDeviceQueueCreateFlags(),
        VkDeviceQueueCreateInfo, VkDriverId(..), VkDriverIdKHR(..),
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        VkPhysicalDeviceLimits, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2, VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceType(..), VkPhysicalDeviceVulkan11Features,
        VkPhysicalDeviceVulkan11Properties,
        VkPhysicalDeviceVulkan12Features,
        VkPhysicalDeviceVulkan12Properties, VkPointClippingBehavior(..),
        VkPointClippingBehaviorKHR(..), VkResolveModeBitmask(..),
        VkResolveModeFlagBits(), VkResolveModeFlagBitsKHR(..),
        VkResolveModeFlags(), VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(),
        VkShaderFloatControlsIndependence(..), VkShaderInfoTypeAMD(..),
        VkShaderStageBitmask(..), VkShaderCorePropertiesBitmaskAMD(..),
        VkShaderCorePropertiesFlagBitsAMD(),
        VkShaderCorePropertiesFlagsAMD(),
        VkShaderFloatControlsIndependenceKHR(..),
        VkShaderModuleCreateBitmask(..), VkShaderModuleCreateFlagBits(),
        VkShaderModuleCreateFlags(), VkShaderStageFlagBits(),
        VkShaderStageFlags(), VkStructureType(..),
        VkSubgroupFeatureBitmask(..), VkSubgroupFeatureFlagBits(),
        VkSubgroupFeatureFlags(),
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES,
        -- ** Promoted from VK_KHR_image_format_list (extension 148)
        pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR,
        VkColorComponentBitmask(..), VkColorSpaceKHR(..),
        VkColorComponentFlagBits(), VkColorComponentFlags(),
        VkCompositeAlphaBitmaskKHR(..), VkCompositeAlphaFlagBitsKHR(),
        VkCompositeAlphaFlagsKHR(), VkExtent2D, VkExtent3D, VkFormat(..),
        VkFormatFeatureBitmask(..), VkFormatFeatureFlagBits(),
        VkFormatFeatureFlags(), VkImageAspectBitmask(..),
        VkImageCreateBitmask(..), VkImageLayout(..), VkImageTiling(..),
        VkImageType(..), VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(),
        VkImageViewCreateBitmask(..), VkImageViewCreateFlagBits(),
        VkImageViewCreateFlags(), VkImageCreateInfo,
        VkImageFormatListCreateInfo, VkPhysicalDeviceImageFormatInfo2,
        VkPresentModeKHR(..), VkSharingMode(..),
        VkSurfaceCounterBitmaskEXT(..), VkSurfaceTransformBitmaskKHR(..),
        VkSurfaceCounterFlagBitsEXT(), VkSurfaceCounterFlagsEXT(),
        VkSurfaceTransformFlagBitsKHR(), VkSurfaceTransformFlagsKHR(),
        VkSwapchainImageUsageBitmaskANDROID(..),
        VkSwapchainCreateBitmaskKHR(..), VkSwapchainCreateFlagBitsKHR(),
        VkSwapchainCreateFlagsKHR(),
        VkSwapchainImageUsageFlagBitsANDROID(),
        VkSwapchainImageUsageFlagsANDROID(), VkSwapchainCreateInfoKHR,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO,
        -- ** Promoted from VK_KHR_sampler_mirror_clamp_to_edge (extension 15)
        pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE,
        -- ** Promoted from VK_KHR_draw_indirect_count (extension 170)
        VkCmdDrawIndirectCount, pattern VkCmdDrawIndirectCount,
        HS_vkCmdDrawIndirectCount, PFN_vkCmdDrawIndirectCount,
        vkCmdDrawIndirectCount, vkCmdDrawIndirectCountUnsafe,
        vkCmdDrawIndirectCountSafe, VkCmdDrawIndexedIndirectCount,
        pattern VkCmdDrawIndexedIndirectCount,
        HS_vkCmdDrawIndexedIndirectCount,
        PFN_vkCmdDrawIndexedIndirectCount, vkCmdDrawIndexedIndirectCount,
        vkCmdDrawIndexedIndirectCountUnsafe,
        vkCmdDrawIndexedIndirectCountSafe, VkAccelerationStructureKHR,
        VkAccelerationStructureKHR_T(), VkAccelerationStructureNV,
        VkAccelerationStructureNV_T(), VkBuffer, VkBufferView,
        VkBufferView_T(), VkBuffer_T(), VkCommandBuffer,
        VkCommandBuffer_T(), VkCommandPool, VkCommandPool_T(),
        VkDebugReportCallbackEXT, VkDebugReportCallbackEXT_T(),
        VkDebugUtilsMessengerEXT, VkDebugUtilsMessengerEXT_T(),
        VkDeferredOperationKHR, VkDeferredOperationKHR_T(),
        VkDescriptorPool, VkDescriptorPool_T(), VkDescriptorSet,
        VkDescriptorSetLayout, VkDescriptorSetLayout_T(),
        VkDescriptorSet_T(), VkDescriptorUpdateTemplate,
        VkDescriptorUpdateTemplateKHR, VkDescriptorUpdateTemplateKHR_T(),
        VkDescriptorUpdateTemplate_T(), VkDevice, VkDeviceMemory,
        VkDeviceMemory_T(), VkDevice_T(), VkDisplayKHR, VkDisplayKHR_T(),
        VkDisplayModeKHR, VkDisplayModeKHR_T(), VkEvent, VkEvent_T(),
        VkFence, VkFence_T(), VkFramebuffer, VkFramebuffer_T(), VkImage,
        VkImageView, VkImageView_T(), VkImage_T(),
        VkIndirectCommandsLayoutNV, VkIndirectCommandsLayoutNV_T(),
        VkInstance, VkInstance_T(), VkPerformanceConfigurationINTEL,
        VkPerformanceConfigurationINTEL_T(), VkPhysicalDevice,
        VkPhysicalDevice_T(), VkPipeline, VkPipelineCache,
        VkPipelineCache_T(), VkPipelineLayout, VkPipelineLayout_T(),
        VkPipeline_T(), VkPrivateDataSlotEXT, VkPrivateDataSlotEXT_T(),
        VkQueryPool, VkQueryPool_T(), VkQueue, VkQueue_T(), VkRenderPass,
        VkRenderPass_T(), VkSampler, VkSamplerYcbcrConversion,
        VkSamplerYcbcrConversionKHR, VkSamplerYcbcrConversionKHR_T(),
        VkSamplerYcbcrConversion_T(), VkSampler_T(), VkSemaphore,
        VkSemaphore_T(), VkShaderModule, VkShaderModule_T(), VkSurfaceKHR,
        VkSurfaceKHR_T(), VkSwapchainKHR, VkSwapchainKHR_T(),
        VkValidationCacheEXT, VkValidationCacheEXT_T(),
        -- ** Promoted from VK_KHR_create_renderpass2 (extension 110)
        VkAccessBitmask(..), VkAccessFlagBits(), VkAccessFlags(),
        VkAttachmentDescription2, VkAttachmentDescriptionBitmask(..),
        VkAttachmentLoadOp(..), VkAttachmentStoreOp(..),
        VkAttachmentDescriptionFlagBits(), VkAttachmentDescriptionFlags(),
        VkAttachmentReference2, VkDependencyBitmask(..),
        VkDependencyFlagBits(), VkDependencyFlags(),
        VkPipelineBindPoint(..), VkPipelineCacheHeaderVersion(..),
        VkPipelineCreateBitmask(..),
        VkPipelineCreationFeedbackBitmaskEXT(..),
        VkPipelineExecutableStatisticFormatKHR(..),
        VkPipelineStageBitmask(..), VkPipelineCacheCreateBitmask(..),
        VkPipelineCacheCreateFlagBits(), VkPipelineCacheCreateFlags(),
        VkPipelineCompilerControlBitmaskAMD(..),
        VkPipelineCompilerControlFlagBitsAMD(),
        VkPipelineCompilerControlFlagsAMD(), VkPipelineCreateFlagBits(),
        VkPipelineCreateFlags(), VkPipelineCreationFeedbackFlagBitsEXT(),
        VkPipelineCreationFeedbackFlagsEXT(),
        VkPipelineShaderStageCreateBitmask(..),
        VkPipelineShaderStageCreateFlagBits(),
        VkPipelineShaderStageCreateFlags(), VkPipelineStageFlagBits(),
        VkPipelineStageFlags(), VkRenderPassCreateBitmask(..),
        VkRenderPassCreateFlagBits(), VkRenderPassCreateFlags(),
        VkRenderPassCreateInfo2, VkSubpassBeginInfo, VkSubpassContents(..),
        VkSubpassDescriptionBitmask(..), VkSubpassDescriptionFlagBits(),
        VkSubpassDescriptionFlags(), VkSubpassDependency2,
        VkSubpassDescription2, VkSubpassEndInfo, -- > #include "vk_platform.h"
                                                 VkCreateRenderPass2,
        pattern VkCreateRenderPass2, HS_vkCreateRenderPass2,
        PFN_vkCreateRenderPass2, vkCreateRenderPass2,
        vkCreateRenderPass2Unsafe, vkCreateRenderPass2Safe,
        VkCmdBeginRenderPass2, pattern VkCmdBeginRenderPass2,
        HS_vkCmdBeginRenderPass2, PFN_vkCmdBeginRenderPass2,
        vkCmdBeginRenderPass2, vkCmdBeginRenderPass2Unsafe,
        vkCmdBeginRenderPass2Safe, VkCmdNextSubpass2,
        pattern VkCmdNextSubpass2, HS_vkCmdNextSubpass2,
        PFN_vkCmdNextSubpass2, vkCmdNextSubpass2, vkCmdNextSubpass2Unsafe,
        vkCmdNextSubpass2Safe, VkCmdEndRenderPass2,
        pattern VkCmdEndRenderPass2, HS_vkCmdEndRenderPass2,
        PFN_vkCmdEndRenderPass2, vkCmdEndRenderPass2,
        vkCmdEndRenderPass2Unsafe, vkCmdEndRenderPass2Safe,
        VkInternalAllocationType(..), VkResult(..),
        VkSystemAllocationScope(..), newVkAllocationFunction,
        newVkDebugReportCallbackEXT, newVkDebugUtilsMessengerCallbackEXT,
        newVkFreeFunction, newVkInternalAllocationNotification,
        newVkInternalFreeNotification, newVkReallocationFunction,
        newVkVoidFunction, unwrapVkAllocationFunction,
        unwrapVkDebugReportCallbackEXT,
        unwrapVkDebugUtilsMessengerCallbackEXT, unwrapVkFreeFunction,
        unwrapVkInternalAllocationNotification,
        unwrapVkInternalFreeNotification, unwrapVkReallocationFunction,
        unwrapVkVoidFunction, HS_vkAllocationFunction,
        HS_vkDebugReportCallbackEXT, HS_vkDebugUtilsMessengerCallbackEXT,
        HS_vkFreeFunction, HS_vkInternalAllocationNotification,
        HS_vkInternalFreeNotification, HS_vkReallocationFunction,
        HS_vkVoidFunction, PFN_vkAllocationFunction,
        PFN_vkDebugReportCallbackEXT, PFN_vkDebugUtilsMessengerCallbackEXT,
        PFN_vkFreeFunction, PFN_vkInternalAllocationNotification,
        PFN_vkInternalFreeNotification, PFN_vkReallocationFunction,
        PFN_vkVoidFunction, VkAllocationCallbacks, VkAttachmentDescription,
        VkAttachmentDescription2KHR, VkAttachmentDescriptionStencilLayout,
        VkAttachmentDescriptionStencilLayoutKHR, VkAttachmentReference,
        VkAttachmentReference2KHR, VkAttachmentReferenceStencilLayout,
        VkAttachmentReferenceStencilLayoutKHR,
        VkAttachmentSampleLocationsEXT, VkClearAttachment,
        VkClearColorValue, VkClearDepthStencilValue, VkClearRect,
        VkClearValue, VkOffset2D, VkOffset3D, VkRect2D, VkRectLayerKHR,
        VkRenderPassAttachmentBeginInfo,
        VkRenderPassAttachmentBeginInfoKHR, VkRenderPassBeginInfo,
        VkRenderPassCreateInfo, VkRenderPassCreateInfo2KHR,
        VkRenderPassFragmentDensityMapCreateInfoEXT,
        VkRenderPassInputAttachmentAspectCreateInfo,
        VkRenderPassInputAttachmentAspectCreateInfoKHR,
        VkRenderPassMultiviewCreateInfo,
        VkRenderPassMultiviewCreateInfoKHR,
        VkRenderPassSampleLocationsBeginInfoEXT,
        VkRenderPassTransformBeginInfoQCOM, VkSubpassBeginInfoKHR,
        VkSubpassDependency, VkSubpassDependency2KHR, VkSubpassDescription,
        VkSubpassDescription2KHR, VkSubpassDescriptionDepthStencilResolve,
        VkSubpassDescriptionDepthStencilResolveKHR, VkSubpassEndInfoKHR,
        VkSubpassSampleLocationsEXT,
        pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2,
        pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2,
        pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2,
        pattern VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2,
        pattern VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO,
        pattern VK_STRUCTURE_TYPE_SUBPASS_END_INFO,
        VkPhysicalDevice8BitStorageFeatures,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES,
        VkPhysicalDeviceDriverProperties,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES,
        pattern VK_MAX_DRIVER_NAME_SIZE, pattern VK_MAX_DRIVER_INFO_SIZE,
        VkPhysicalDeviceShaderAtomicInt64Features,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES,
        VkPhysicalDeviceShaderFloat16Int8Features,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES,
        -- ** Promoted from VK_KHR_shader_float_controls (extension 198)
        VkPhysicalDeviceFloatControlsProperties,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES,
        -- ** Promoted from VK_EXT_descriptor_indexing (extension 162)
        VkDescriptorBindingBitmask(..), VkDescriptorPoolCreateBitmask(..),
        VkDescriptorType(..), VkDescriptorUpdateTemplateType(..),
        VkDescriptorBindingFlagBits(), VkDescriptorBindingFlagBitsEXT(..),
        VkDescriptorBindingFlags(), VkDescriptorPoolCreateFlagBits(),
        VkDescriptorPoolCreateFlags(),
        VkDescriptorSetLayoutCreateBitmask(..),
        VkDescriptorSetLayoutCreateFlagBits(),
        VkDescriptorSetLayoutCreateFlags(),
        VkDescriptorUpdateTemplateTypeKHR(..), VkDescriptorSetAllocateInfo,
        VkDescriptorSetLayoutBinding,
        VkDescriptorSetLayoutBindingFlagsCreateInfo,
        VkDescriptorSetLayoutCreateInfo, VkDescriptorSetLayoutSupport,
        VkDescriptorSetVariableDescriptorCountAllocateInfo,
        VkDescriptorSetVariableDescriptorCountLayoutSupport,
        VkPhysicalDeviceDescriptorIndexingFeatures,
        VkPhysicalDeviceDescriptorIndexingProperties,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT,
        pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT,
        pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT,
        pattern VK_ERROR_FRAGMENTATION,
        VkPhysicalDeviceDepthStencilResolveProperties,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE,
        VkPhysicalDeviceScalarBlockLayoutFeatures,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES,
        VkImageStencilUsageCreateInfo,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO,
        -- ** Promoted from VK_EXT_sampler_filter_minmax (extension 131)
        VkBorderColor(..), VkCompareOp(..), VkFilter(..),
        VkPhysicalDeviceSamplerFilterMinmaxProperties,
        VkSamplerAddressMode(..), VkSamplerMipmapMode(..),
        VkSamplerReductionMode(..), VkSamplerYcbcrModelConversion(..),
        VkSamplerYcbcrRange(..), VkSamplerCreateBitmask(..),
        VkSamplerCreateFlagBits(), VkSamplerCreateFlags(),
        VkSamplerReductionModeEXT(..),
        VkSamplerYcbcrModelConversionKHR(..), VkSamplerYcbcrRangeKHR(..),
        VkSamplerCreateInfo, VkSamplerReductionModeCreateInfo,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT,
        VkPhysicalDeviceVulkanMemoryModelFeatures,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES,
        VkFramebufferAttachmentImageInfo,
        VkFramebufferAttachmentsCreateInfo, VkFramebufferCreateBitmask(..),
        VkFramebufferCreateFlagBits(), VkFramebufferCreateFlags(),
        VkFramebufferCreateInfo,
        VkPhysicalDeviceImagelessFramebufferFeatures,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES,
        pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO,
        pattern VK_FRAMEBUFFER_CREATE_IMAGELESS_BIT,
        VkPhysicalDeviceUniformBufferStandardLayoutFeatures,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES,
        VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES,
        VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES,
        pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT,
        pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT,
        pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL,
        pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL,
        pattern VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL,
        pattern VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL,
        VkPhysicalDeviceHostQueryResetFeatures, -- > #include "vk_platform.h"
                                                VkResetQueryPool,
        pattern VkResetQueryPool, HS_vkResetQueryPool,
        PFN_vkResetQueryPool, vkResetQueryPool, vkResetQueryPoolUnsafe,
        vkResetQueryPoolSafe,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES,
        -- ** Promoted from VK_KHR_timeline_semaphore (extension 208)
        VkBindSparseInfo, VkExternalFenceFeatureBitmask(..),
        VkExternalFenceHandleTypeBitmask(..),
        VkExternalMemoryFeatureBitmask(..),
        VkExternalMemoryFeatureBitmaskNV(..),
        VkExternalMemoryHandleTypeBitmaskNV(..),
        VkExternalMemoryHandleTypeBitmask(..),
        VkExternalSemaphoreFeatureBitmask(..),
        VkExternalSemaphoreHandleTypeBitmask(..),
        VkExternalFenceFeatureFlagBits(),
        VkExternalFenceFeatureFlagBitsKHR(..),
        VkExternalFenceFeatureFlags(), VkExternalFenceHandleTypeFlagBits(),
        VkExternalFenceHandleTypeFlagBitsKHR(..),
        VkExternalFenceHandleTypeFlags(),
        VkExternalMemoryFeatureFlagBits(),
        VkExternalMemoryFeatureFlagBitsKHR(..),
        VkExternalMemoryFeatureFlagBitsNV(),
        VkExternalMemoryFeatureFlags(), VkExternalMemoryFeatureFlagsNV(),
        VkExternalMemoryHandleTypeFlagBits(),
        VkExternalMemoryHandleTypeFlagBitsKHR(..),
        VkExternalMemoryHandleTypeFlagBitsNV(),
        VkExternalMemoryHandleTypeFlags(),
        VkExternalMemoryHandleTypeFlagsNV(),
        VkExternalSemaphoreFeatureFlagBits(),
        VkExternalSemaphoreFeatureFlagBitsKHR(..),
        VkExternalSemaphoreFeatureFlags(),
        VkExternalSemaphoreHandleTypeFlagBits(),
        VkExternalSemaphoreHandleTypeFlagBitsKHR(..),
        VkExternalSemaphoreHandleTypeFlags(), VkImageSubresource,
        VkPhysicalDeviceExternalSemaphoreInfo,
        VkPhysicalDeviceTimelineSemaphoreFeatures,
        VkPhysicalDeviceTimelineSemaphoreProperties, VkSemaphoreCreateInfo,
        VkSemaphoreSignalInfo, VkSemaphoreImportBitmask(..),
        VkSemaphoreType(..), VkSemaphoreWaitBitmask(..),
        VkSemaphoreImportFlagBits(), VkSemaphoreImportFlagBitsKHR(..),
        VkSemaphoreImportFlags(), VkSemaphoreTypeKHR(..),
        VkSemaphoreWaitFlagBits(), VkSemaphoreWaitFlagBitsKHR(..),
        VkSemaphoreWaitFlags(), VkSemaphoreTypeCreateInfo,
        VkSemaphoreWaitInfo, VkSparseBufferMemoryBindInfo,
        VkSparseImageMemoryBind, VkSparseImageMemoryBindInfo,
        VkSparseImageOpaqueMemoryBindInfo, VkSparseMemoryBind,
        VkSparseImageFormatBitmask(..), VkSparseMemoryBindBitmask(..),
        VkSparseImageFormatFlagBits(), VkSparseImageFormatFlags(),
        VkSparseMemoryBindFlagBits(), VkSparseMemoryBindFlags(),
        VkSubmitInfo, VkTimelineSemaphoreSubmitInfo,
        -- > #include "vk_platform.h"
        VkGetSemaphoreCounterValue, pattern VkGetSemaphoreCounterValue,
        HS_vkGetSemaphoreCounterValue, PFN_vkGetSemaphoreCounterValue,
        vkGetSemaphoreCounterValue, vkGetSemaphoreCounterValueUnsafe,
        vkGetSemaphoreCounterValueSafe, VkWaitSemaphores,
        pattern VkWaitSemaphores, HS_vkWaitSemaphores,
        PFN_vkWaitSemaphores, vkWaitSemaphores, vkWaitSemaphoresUnsafe,
        vkWaitSemaphoresSafe, VkSignalSemaphore, pattern VkSignalSemaphore,
        HS_vkSignalSemaphore, PFN_vkSignalSemaphore, vkSignalSemaphore,
        vkSignalSemaphoreUnsafe, vkSignalSemaphoreSafe,
        VkSemaphoreGetFdInfoKHR, VkSemaphoreSignalInfoKHR,
        VkSemaphoreTypeCreateInfoKHR, VkSemaphoreWaitInfoKHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO,
        pattern VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO,
        pattern VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO,
        -- ** Promoted from VK_KHR_buffer_device_address (extension 258)
        VkBufferCreateBitmask(..), VkBufferUsageBitmask(..),
        VkBufferCreateFlagBits(), VkBufferCreateFlags(),
        VkBufferUsageFlagBits(), VkBufferUsageFlags(), VkBufferCreateInfo,
        VkBufferDeviceAddressInfo, VkBufferOpaqueCaptureAddressCreateInfo,
        VkDeviceMemoryOpaqueCaptureAddressInfo, VkMemoryAllocateInfo,
        VkMemoryOpaqueCaptureAddressAllocateInfo,
        VkPhysicalDeviceBufferDeviceAddressFeatures,
        -- > #include "vk_platform.h"
        VkGetBufferDeviceAddress, pattern VkGetBufferDeviceAddress,
        HS_vkGetBufferDeviceAddress, PFN_vkGetBufferDeviceAddress,
        vkGetBufferDeviceAddress, vkGetBufferDeviceAddressUnsafe,
        vkGetBufferDeviceAddressSafe, VkGetBufferOpaqueCaptureAddress,
        pattern VkGetBufferOpaqueCaptureAddress,
        HS_vkGetBufferOpaqueCaptureAddress,
        PFN_vkGetBufferOpaqueCaptureAddress,
        vkGetBufferOpaqueCaptureAddress,
        vkGetBufferOpaqueCaptureAddressUnsafe,
        vkGetBufferOpaqueCaptureAddressSafe,
        VkGetDeviceMemoryOpaqueCaptureAddress,
        pattern VkGetDeviceMemoryOpaqueCaptureAddress,
        HS_vkGetDeviceMemoryOpaqueCaptureAddress,
        PFN_vkGetDeviceMemoryOpaqueCaptureAddress,
        vkGetDeviceMemoryOpaqueCaptureAddress,
        vkGetDeviceMemoryOpaqueCaptureAddressUnsafe,
        vkGetDeviceMemoryOpaqueCaptureAddressSafe, VkBufferCopy,
        VkBufferDeviceAddressCreateInfoEXT, VkBufferDeviceAddressInfoEXT,
        VkBufferDeviceAddressInfoKHR, VkBufferImageCopy,
        VkBufferMemoryBarrier, VkBufferMemoryRequirementsInfo2,
        VkBufferMemoryRequirementsInfo2KHR,
        VkBufferOpaqueCaptureAddressCreateInfoKHR, VkBufferViewCreateInfo,
        VkDeviceDiagnosticsConfigCreateInfoNV, VkDeviceEventInfoEXT,
        VkDeviceGroupBindSparseInfo, VkDeviceGroupBindSparseInfoKHR,
        VkDeviceGroupCommandBufferBeginInfo,
        VkDeviceGroupCommandBufferBeginInfoKHR,
        VkDeviceGroupDeviceCreateInfo, VkDeviceGroupDeviceCreateInfoKHR,
        VkDeviceGroupPresentCapabilitiesKHR, VkDeviceGroupPresentInfoKHR,
        VkDeviceGroupRenderPassBeginInfo,
        VkDeviceGroupRenderPassBeginInfoKHR, VkDeviceGroupSubmitInfo,
        VkDeviceGroupSubmitInfoKHR, VkDeviceGroupSwapchainCreateInfoKHR,
        VkDeviceMemoryOpaqueCaptureAddressInfoKHR,
        VkDeviceMemoryOverallocationCreateInfoAMD,
        VkDevicePrivateDataCreateInfoEXT,
        VkDeviceQueueGlobalPriorityCreateInfoEXT, VkDeviceQueueInfo2,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES,
        pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO,
        pattern VK_STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO,
        pattern VK_STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO,
        pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT,
        pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT,
        pattern VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT,
        pattern VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT,
        pattern VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS)
       where
import GHC.Ptr                                                  (Ptr (..))
import Graphics.Vulkan.Constants                                (pattern VK_MAX_DRIVER_INFO_SIZE,
                                                                 pattern VK_MAX_DRIVER_NAME_SIZE)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.AccessFlags
import Graphics.Vulkan.Types.Enum.Attachment
import Graphics.Vulkan.Types.Enum.BorderColor
import Graphics.Vulkan.Types.Enum.Buffer
import Graphics.Vulkan.Types.Enum.Color
import Graphics.Vulkan.Types.Enum.CompareOp
import Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR
import Graphics.Vulkan.Types.Enum.DependencyFlags
import Graphics.Vulkan.Types.Enum.Descriptor
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.DriverId
import Graphics.Vulkan.Types.Enum.External
import Graphics.Vulkan.Types.Enum.Filter
import Graphics.Vulkan.Types.Enum.Format
import Graphics.Vulkan.Types.Enum.FramebufferCreateFlags
import Graphics.Vulkan.Types.Enum.Image
import Graphics.Vulkan.Types.Enum.InternalAllocationType
import Graphics.Vulkan.Types.Enum.Memory                        (VkMemoryAllocateBitmask (..))
import Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import Graphics.Vulkan.Types.Enum.Pipeline
import Graphics.Vulkan.Types.Enum.PointClippingBehavior
import Graphics.Vulkan.Types.Enum.PresentModeKHR
import Graphics.Vulkan.Types.Enum.RenderPassCreateFlags
import Graphics.Vulkan.Types.Enum.ResolveModeFlag
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.Sampler
import Graphics.Vulkan.Types.Enum.Semaphore
import Graphics.Vulkan.Types.Enum.Shader
import Graphics.Vulkan.Types.Enum.SharingMode
import Graphics.Vulkan.Types.Enum.Sparse
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Enum.SubgroupFeatureFlags
import Graphics.Vulkan.Types.Enum.Subpass
import Graphics.Vulkan.Types.Enum.Surface
import Graphics.Vulkan.Types.Enum.Swapchain
import Graphics.Vulkan.Types.Enum.SystemAllocationScope
import Graphics.Vulkan.Types.Funcpointers
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.AllocationCallbacks
import Graphics.Vulkan.Types.Struct.Attachment
import Graphics.Vulkan.Types.Struct.Bind                        (VkBindSparseInfo)
import Graphics.Vulkan.Types.Struct.Buffer
import Graphics.Vulkan.Types.Struct.Clear
import Graphics.Vulkan.Types.Struct.ConformanceVersion          (VkConformanceVersion)
import Graphics.Vulkan.Types.Struct.Descriptor                  (VkDescriptorSetAllocateInfo,
                                                                 VkDescriptorSetLayoutBinding,
                                                                 VkDescriptorSetLayoutBindingFlagsCreateInfo,
                                                                 VkDescriptorSetLayoutCreateInfo,
                                                                 VkDescriptorSetLayoutSupport,
                                                                 VkDescriptorSetVariableDescriptorCountAllocateInfo,
                                                                 VkDescriptorSetVariableDescriptorCountLayoutSupport)
import Graphics.Vulkan.Types.Struct.Device
import Graphics.Vulkan.Types.Struct.Extent
import Graphics.Vulkan.Types.Struct.Framebuffer                 (VkFramebufferAttachmentImageInfo,
                                                                 VkFramebufferAttachmentsCreateInfo,
                                                                 VkFramebufferCreateInfo)
import Graphics.Vulkan.Types.Struct.Image                       (VkImageCreateInfo,
                                                                 VkImageFormatListCreateInfo,
                                                                 VkImageStencilUsageCreateInfo,
                                                                 VkImageSubresource)
import Graphics.Vulkan.Types.Struct.Memory                      (VkMemoryAllocateInfo,
                                                                 VkMemoryOpaqueCaptureAddressAllocateInfo)
import Graphics.Vulkan.Types.Struct.Offset
import Graphics.Vulkan.Types.Struct.PhysicalDevice              (VkPhysicalDevice8BitStorageFeatures,
                                                                 VkPhysicalDeviceBufferDeviceAddressFeatures,
                                                                 VkPhysicalDeviceDepthStencilResolveProperties,
                                                                 VkPhysicalDeviceDescriptorIndexingFeatures,
                                                                 VkPhysicalDeviceDescriptorIndexingProperties,
                                                                 VkPhysicalDeviceDriverProperties,
                                                                 VkPhysicalDeviceExternalSemaphoreInfo,
                                                                 VkPhysicalDeviceFeatures2,
                                                                 VkPhysicalDeviceFloatControlsProperties,
                                                                 VkPhysicalDeviceHostQueryResetFeatures,
                                                                 VkPhysicalDeviceImageFormatInfo2,
                                                                 VkPhysicalDeviceImagelessFramebufferFeatures,
                                                                 VkPhysicalDeviceLimits,
                                                                 VkPhysicalDeviceProperties,
                                                                 VkPhysicalDeviceProperties2,
                                                                 VkPhysicalDeviceSamplerFilterMinmaxProperties,
                                                                 VkPhysicalDeviceScalarBlockLayoutFeatures,
                                                                 VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures,
                                                                 VkPhysicalDeviceShaderAtomicInt64Features,
                                                                 VkPhysicalDeviceShaderFloat16Int8Features,
                                                                 VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures,
                                                                 VkPhysicalDeviceSparseProperties,
                                                                 VkPhysicalDeviceTimelineSemaphoreFeatures,
                                                                 VkPhysicalDeviceTimelineSemaphoreProperties,
                                                                 VkPhysicalDeviceUniformBufferStandardLayoutFeatures,
                                                                 VkPhysicalDeviceVulkan11Features,
                                                                 VkPhysicalDeviceVulkan11Properties,
                                                                 VkPhysicalDeviceVulkan12Features,
                                                                 VkPhysicalDeviceVulkan12Properties,
                                                                 VkPhysicalDeviceVulkanMemoryModelFeatures)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures      (VkPhysicalDeviceFeatures)
import Graphics.Vulkan.Types.Struct.Rect
import Graphics.Vulkan.Types.Struct.RenderPass
import Graphics.Vulkan.Types.Struct.Sampler                     (VkSamplerCreateInfo,
                                                                 VkSamplerReductionModeCreateInfo)
import Graphics.Vulkan.Types.Struct.Semaphore
import Graphics.Vulkan.Types.Struct.Sparse                      (VkSparseBufferMemoryBindInfo,
                                                                 VkSparseImageMemoryBind,
                                                                 VkSparseImageMemoryBindInfo,
                                                                 VkSparseImageOpaqueMemoryBindInfo,
                                                                 VkSparseMemoryBind)
import Graphics.Vulkan.Types.Struct.SubmitInfo                  (VkSubmitInfo)
import Graphics.Vulkan.Types.Struct.Subpass
import Graphics.Vulkan.Types.Struct.Swapchain                   (VkSwapchainCreateInfoKHR)
import Graphics.Vulkan.Types.Struct.TimelineSemaphoreSubmitInfo (VkTimelineSemaphoreSubmitInfo)
import System.IO.Unsafe                                         (unsafeDupablePerformIO)

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES =
        VkStructureType 49

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES =
        VkStructureType 50

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES =
        VkStructureType 51

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES =
        VkStructureType 52

pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO =
        VkStructureType 1000147000

-- | No need to add an extnumber attribute, since this uses a core enum value
pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE ::
        VkSamplerAddressMode

pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE =
        VkSamplerAddressMode 4

pattern VkCmdDrawIndirectCount :: CString

pattern VkCmdDrawIndirectCount <-
        (is_VkCmdDrawIndirectCount -> True)
  where
    VkCmdDrawIndirectCount = _VkCmdDrawIndirectCount

{-# INLINE _VkCmdDrawIndirectCount #-}

_VkCmdDrawIndirectCount :: CString
_VkCmdDrawIndirectCount = Ptr "vkCmdDrawIndirectCount\NUL"#

{-# INLINE is_VkCmdDrawIndirectCount #-}

is_VkCmdDrawIndirectCount :: CString -> Bool
is_VkCmdDrawIndirectCount
  = (EQ ==) . cmpCStrings _VkCmdDrawIndirectCount

type VkCmdDrawIndirectCount = "vkCmdDrawIndirectCount"

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdDrawIndirectCount
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer buffer
-- >     , VkDeviceSize offset
-- >     , VkBuffer countBuffer
-- >     , VkDeviceSize countBufferOffset
-- >     , uint32_t maxDrawCount
-- >     , uint32_t stride
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdDrawIndirectCount vkCmdDrawIndirectCount registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDrawIndirectCount <- vkGetInstanceProc @VkCmdDrawIndirectCount vkInstance
--
-- or less efficient:
--
-- > myCmdDrawIndirectCount <- vkGetProc @VkCmdDrawIndirectCount
--
-- __Note:__ @vkCmdDrawIndirectCountUnsafe@ and @vkCmdDrawIndirectCountSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdDrawIndirectCount@ is an alias
--           of @vkCmdDrawIndirectCountUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdDrawIndirectCountSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall unsafe "vkCmdDrawIndirectCount"
               vkCmdDrawIndirectCountUnsafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          ->
                   VkDeviceSize -- ^ offset
                                ->
                     VkBuffer -- ^ countBuffer
                              -> VkDeviceSize -- ^ countBufferOffset
                                              -> Word32 -- ^ maxDrawCount
                                                        -> Word32 -- ^ stride
                                                                  -> IO ()

#else
vkCmdDrawIndirectCountUnsafe ::
                             VkCommandBuffer -- ^ commandBuffer
                                             ->
                               VkBuffer -- ^ buffer
                                        ->
                                 VkDeviceSize -- ^ offset
                                              ->
                                   VkBuffer -- ^ countBuffer
                                            -> VkDeviceSize -- ^ countBufferOffset
                                                            -> Word32 -- ^ maxDrawCount
                                                                      -> Word32 -- ^ stride
                                                                                -> IO ()
vkCmdDrawIndirectCountUnsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkCmdDrawIndirectCount)

{-# NOINLINE vkCmdDrawIndirectCountUnsafe #-}
#endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdDrawIndirectCount
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer buffer
-- >     , VkDeviceSize offset
-- >     , VkBuffer countBuffer
-- >     , VkDeviceSize countBufferOffset
-- >     , uint32_t maxDrawCount
-- >     , uint32_t stride
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdDrawIndirectCount vkCmdDrawIndirectCount registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDrawIndirectCount <- vkGetInstanceProc @VkCmdDrawIndirectCount vkInstance
--
-- or less efficient:
--
-- > myCmdDrawIndirectCount <- vkGetProc @VkCmdDrawIndirectCount
--
-- __Note:__ @vkCmdDrawIndirectCountUnsafe@ and @vkCmdDrawIndirectCountSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdDrawIndirectCount@ is an alias
--           of @vkCmdDrawIndirectCountUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdDrawIndirectCountSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall safe "vkCmdDrawIndirectCount"
               vkCmdDrawIndirectCountSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          ->
                   VkDeviceSize -- ^ offset
                                ->
                     VkBuffer -- ^ countBuffer
                              -> VkDeviceSize -- ^ countBufferOffset
                                              -> Word32 -- ^ maxDrawCount
                                                        -> Word32 -- ^ stride
                                                                  -> IO ()

#else
vkCmdDrawIndirectCountSafe ::
                           VkCommandBuffer -- ^ commandBuffer
                                           ->
                             VkBuffer -- ^ buffer
                                      ->
                               VkDeviceSize -- ^ offset
                                            ->
                                 VkBuffer -- ^ countBuffer
                                          -> VkDeviceSize -- ^ countBufferOffset
                                                          -> Word32 -- ^ maxDrawCount
                                                                    -> Word32 -- ^ stride
                                                                              -> IO ()
vkCmdDrawIndirectCountSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdDrawIndirectCount)

{-# NOINLINE vkCmdDrawIndirectCountSafe #-}
#endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdDrawIndirectCount
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer buffer
-- >     , VkDeviceSize offset
-- >     , VkBuffer countBuffer
-- >     , VkDeviceSize countBufferOffset
-- >     , uint32_t maxDrawCount
-- >     , uint32_t stride
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdDrawIndirectCount vkCmdDrawIndirectCount registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDrawIndirectCount <- vkGetInstanceProc @VkCmdDrawIndirectCount vkInstance
--
-- or less efficient:
--
-- > myCmdDrawIndirectCount <- vkGetProc @VkCmdDrawIndirectCount
--
-- __Note:__ @vkCmdDrawIndirectCountUnsafe@ and @vkCmdDrawIndirectCountSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdDrawIndirectCount@ is an alias
--           of @vkCmdDrawIndirectCountUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdDrawIndirectCountSafe@.
--
vkCmdDrawIndirectCount ::
                       VkCommandBuffer -- ^ commandBuffer
                                       ->
                         VkBuffer -- ^ buffer
                                  ->
                           VkDeviceSize -- ^ offset
                                        ->
                             VkBuffer -- ^ countBuffer
                                      -> VkDeviceSize -- ^ countBufferOffset
                                                      -> Word32 -- ^ maxDrawCount
                                                                -> Word32 -- ^ stride
                                                                          -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkCmdDrawIndirectCount = vkCmdDrawIndirectCountUnsafe
#else
vkCmdDrawIndirectCount = vkCmdDrawIndirectCountSafe

#endif
{-# INLINE vkCmdDrawIndirectCount #-}

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdDrawIndirectCount
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkBuffer countBuffer
--   >     , VkDeviceSize countBufferOffset
--   >     , uint32_t maxDrawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdDrawIndirectCount vkCmdDrawIndirectCount registry at www.khronos.org>
type HS_vkCmdDrawIndirectCount =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkBuffer -- ^ buffer
                ->
         VkDeviceSize -- ^ offset
                      ->
           VkBuffer -- ^ countBuffer
                    -> VkDeviceSize -- ^ countBufferOffset
                                    -> Word32 -- ^ maxDrawCount
                                              -> Word32 -- ^ stride
                                                        -> IO ()

type PFN_vkCmdDrawIndirectCount = FunPtr HS_vkCmdDrawIndirectCount

foreign import ccall unsafe "dynamic"
               unwrapVkCmdDrawIndirectCountUnsafe ::
               PFN_vkCmdDrawIndirectCount -> HS_vkCmdDrawIndirectCount

foreign import ccall safe "dynamic"
               unwrapVkCmdDrawIndirectCountSafe ::
               PFN_vkCmdDrawIndirectCount -> HS_vkCmdDrawIndirectCount

instance VulkanProc "vkCmdDrawIndirectCount" where
    type VkProcType "vkCmdDrawIndirectCount" =
         HS_vkCmdDrawIndirectCount
    vkProcSymbol = _VkCmdDrawIndirectCount

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdDrawIndirectCountUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdDrawIndirectCountSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdDrawIndexedIndirectCount :: CString

pattern VkCmdDrawIndexedIndirectCount <-
        (is_VkCmdDrawIndexedIndirectCount -> True)
  where
    VkCmdDrawIndexedIndirectCount = _VkCmdDrawIndexedIndirectCount

{-# INLINE _VkCmdDrawIndexedIndirectCount #-}

_VkCmdDrawIndexedIndirectCount :: CString
_VkCmdDrawIndexedIndirectCount
  = Ptr "vkCmdDrawIndexedIndirectCount\NUL"#

{-# INLINE is_VkCmdDrawIndexedIndirectCount #-}

is_VkCmdDrawIndexedIndirectCount :: CString -> Bool
is_VkCmdDrawIndexedIndirectCount
  = (EQ ==) . cmpCStrings _VkCmdDrawIndexedIndirectCount

type VkCmdDrawIndexedIndirectCount =
     "vkCmdDrawIndexedIndirectCount"

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdDrawIndexedIndirectCount
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer buffer
-- >     , VkDeviceSize offset
-- >     , VkBuffer countBuffer
-- >     , VkDeviceSize countBufferOffset
-- >     , uint32_t maxDrawCount
-- >     , uint32_t stride
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdDrawIndexedIndirectCount vkCmdDrawIndexedIndirectCount registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDrawIndexedIndirectCount <- vkGetInstanceProc @VkCmdDrawIndexedIndirectCount vkInstance
--
-- or less efficient:
--
-- > myCmdDrawIndexedIndirectCount <- vkGetProc @VkCmdDrawIndexedIndirectCount
--
-- __Note:__ @vkCmdDrawIndexedIndirectCountUnsafe@ and @vkCmdDrawIndexedIndirectCountSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdDrawIndexedIndirectCount@ is an alias
--           of @vkCmdDrawIndexedIndirectCountUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdDrawIndexedIndirectCountSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall unsafe "vkCmdDrawIndexedIndirectCount"
               vkCmdDrawIndexedIndirectCountUnsafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          ->
                   VkDeviceSize -- ^ offset
                                ->
                     VkBuffer -- ^ countBuffer
                              -> VkDeviceSize -- ^ countBufferOffset
                                              -> Word32 -- ^ maxDrawCount
                                                        -> Word32 -- ^ stride
                                                                  -> IO ()

#else
vkCmdDrawIndexedIndirectCountUnsafe ::
                                    VkCommandBuffer -- ^ commandBuffer
                                                    ->
                                      VkBuffer -- ^ buffer
                                               ->
                                        VkDeviceSize -- ^ offset
                                                     ->
                                          VkBuffer -- ^ countBuffer
                                                   -> VkDeviceSize -- ^ countBufferOffset
                                                                   -> Word32 -- ^ maxDrawCount
                                                                             -> Word32 -- ^ stride
                                                                                       -> IO ()
vkCmdDrawIndexedIndirectCountUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkCmdDrawIndexedIndirectCount)

{-# NOINLINE vkCmdDrawIndexedIndirectCountUnsafe #-}
#endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdDrawIndexedIndirectCount
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer buffer
-- >     , VkDeviceSize offset
-- >     , VkBuffer countBuffer
-- >     , VkDeviceSize countBufferOffset
-- >     , uint32_t maxDrawCount
-- >     , uint32_t stride
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdDrawIndexedIndirectCount vkCmdDrawIndexedIndirectCount registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDrawIndexedIndirectCount <- vkGetInstanceProc @VkCmdDrawIndexedIndirectCount vkInstance
--
-- or less efficient:
--
-- > myCmdDrawIndexedIndirectCount <- vkGetProc @VkCmdDrawIndexedIndirectCount
--
-- __Note:__ @vkCmdDrawIndexedIndirectCountUnsafe@ and @vkCmdDrawIndexedIndirectCountSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdDrawIndexedIndirectCount@ is an alias
--           of @vkCmdDrawIndexedIndirectCountUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdDrawIndexedIndirectCountSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall safe "vkCmdDrawIndexedIndirectCount"
               vkCmdDrawIndexedIndirectCountSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          ->
                   VkDeviceSize -- ^ offset
                                ->
                     VkBuffer -- ^ countBuffer
                              -> VkDeviceSize -- ^ countBufferOffset
                                              -> Word32 -- ^ maxDrawCount
                                                        -> Word32 -- ^ stride
                                                                  -> IO ()

#else
vkCmdDrawIndexedIndirectCountSafe ::
                                  VkCommandBuffer -- ^ commandBuffer
                                                  ->
                                    VkBuffer -- ^ buffer
                                             ->
                                      VkDeviceSize -- ^ offset
                                                   ->
                                        VkBuffer -- ^ countBuffer
                                                 -> VkDeviceSize -- ^ countBufferOffset
                                                                 -> Word32 -- ^ maxDrawCount
                                                                           -> Word32 -- ^ stride
                                                                                     -> IO ()
vkCmdDrawIndexedIndirectCountSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkCmdDrawIndexedIndirectCount)

{-# NOINLINE vkCmdDrawIndexedIndirectCountSafe #-}
#endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdDrawIndexedIndirectCount
-- >     ( VkCommandBuffer commandBuffer
-- >     , VkBuffer buffer
-- >     , VkDeviceSize offset
-- >     , VkBuffer countBuffer
-- >     , VkDeviceSize countBufferOffset
-- >     , uint32_t maxDrawCount
-- >     , uint32_t stride
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdDrawIndexedIndirectCount vkCmdDrawIndexedIndirectCount registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDrawIndexedIndirectCount <- vkGetInstanceProc @VkCmdDrawIndexedIndirectCount vkInstance
--
-- or less efficient:
--
-- > myCmdDrawIndexedIndirectCount <- vkGetProc @VkCmdDrawIndexedIndirectCount
--
-- __Note:__ @vkCmdDrawIndexedIndirectCountUnsafe@ and @vkCmdDrawIndexedIndirectCountSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdDrawIndexedIndirectCount@ is an alias
--           of @vkCmdDrawIndexedIndirectCountUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdDrawIndexedIndirectCountSafe@.
--
vkCmdDrawIndexedIndirectCount ::
                              VkCommandBuffer -- ^ commandBuffer
                                              ->
                                VkBuffer -- ^ buffer
                                         ->
                                  VkDeviceSize -- ^ offset
                                               ->
                                    VkBuffer -- ^ countBuffer
                                             -> VkDeviceSize -- ^ countBufferOffset
                                                             -> Word32 -- ^ maxDrawCount
                                                                       -> Word32 -- ^ stride
                                                                                 -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkCmdDrawIndexedIndirectCount = vkCmdDrawIndexedIndirectCountUnsafe
#else
vkCmdDrawIndexedIndirectCount = vkCmdDrawIndexedIndirectCountSafe

#endif
{-# INLINE vkCmdDrawIndexedIndirectCount #-}

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdDrawIndexedIndirectCount
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkBuffer countBuffer
--   >     , VkDeviceSize countBufferOffset
--   >     , uint32_t maxDrawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdDrawIndexedIndirectCount vkCmdDrawIndexedIndirectCount registry at www.khronos.org>
type HS_vkCmdDrawIndexedIndirectCount =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkBuffer -- ^ buffer
                ->
         VkDeviceSize -- ^ offset
                      ->
           VkBuffer -- ^ countBuffer
                    -> VkDeviceSize -- ^ countBufferOffset
                                    -> Word32 -- ^ maxDrawCount
                                              -> Word32 -- ^ stride
                                                        -> IO ()

type PFN_vkCmdDrawIndexedIndirectCount =
     FunPtr HS_vkCmdDrawIndexedIndirectCount

foreign import ccall unsafe "dynamic"
               unwrapVkCmdDrawIndexedIndirectCountUnsafe ::
               PFN_vkCmdDrawIndexedIndirectCount ->
                 HS_vkCmdDrawIndexedIndirectCount

foreign import ccall safe "dynamic"
               unwrapVkCmdDrawIndexedIndirectCountSafe ::
               PFN_vkCmdDrawIndexedIndirectCount ->
                 HS_vkCmdDrawIndexedIndirectCount

instance VulkanProc "vkCmdDrawIndexedIndirectCount" where
    type VkProcType "vkCmdDrawIndexedIndirectCount" =
         HS_vkCmdDrawIndexedIndirectCount
    vkProcSymbol = _VkCmdDrawIndexedIndirectCount

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdDrawIndexedIndirectCountUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdDrawIndexedIndirectCountSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCreateRenderPass2 :: CString

pattern VkCreateRenderPass2 <- (is_VkCreateRenderPass2 -> True)
  where
    VkCreateRenderPass2 = _VkCreateRenderPass2

{-# INLINE _VkCreateRenderPass2 #-}

_VkCreateRenderPass2 :: CString
_VkCreateRenderPass2 = Ptr "vkCreateRenderPass2\NUL"#

{-# INLINE is_VkCreateRenderPass2 #-}

is_VkCreateRenderPass2 :: CString -> Bool
is_VkCreateRenderPass2 = (EQ ==) . cmpCStrings _VkCreateRenderPass2

type VkCreateRenderPass2 = "vkCreateRenderPass2"

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateRenderPass2
-- >     ( VkDevice device
-- >     , const VkRenderPassCreateInfo2* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkRenderPass* pRenderPass
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCreateRenderPass2 vkCreateRenderPass2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateRenderPass2 <- vkGetDeviceProc @VkCreateRenderPass2 vkDevice
--
-- or less efficient:
--
-- > myCreateRenderPass2 <- vkGetProc @VkCreateRenderPass2
--
-- __Note:__ @vkCreateRenderPass2Unsafe@ and @vkCreateRenderPass2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateRenderPass2@ is an alias
--           of @vkCreateRenderPass2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateRenderPass2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall unsafe "vkCreateRenderPass2"
               vkCreateRenderPass2Unsafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkRenderPassCreateInfo2 -- ^ pCreateInfo
                                             ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkRenderPass -- ^ pRenderPass
                                                                 -> IO VkResult

#else
vkCreateRenderPass2Unsafe ::
                          VkDevice -- ^ device
                                   ->
                            Ptr VkRenderPassCreateInfo2 -- ^ pCreateInfo
                                                        ->
                              Ptr VkAllocationCallbacks -- ^ pAllocator
                                                        -> Ptr VkRenderPass -- ^ pRenderPass
                                                                            -> IO VkResult
vkCreateRenderPass2Unsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkCreateRenderPass2)

{-# NOINLINE vkCreateRenderPass2Unsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateRenderPass2
-- >     ( VkDevice device
-- >     , const VkRenderPassCreateInfo2* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkRenderPass* pRenderPass
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCreateRenderPass2 vkCreateRenderPass2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateRenderPass2 <- vkGetDeviceProc @VkCreateRenderPass2 vkDevice
--
-- or less efficient:
--
-- > myCreateRenderPass2 <- vkGetProc @VkCreateRenderPass2
--
-- __Note:__ @vkCreateRenderPass2Unsafe@ and @vkCreateRenderPass2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateRenderPass2@ is an alias
--           of @vkCreateRenderPass2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateRenderPass2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall safe "vkCreateRenderPass2"
               vkCreateRenderPass2Safe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkRenderPassCreateInfo2 -- ^ pCreateInfo
                                             ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkRenderPass -- ^ pRenderPass
                                                                 -> IO VkResult

#else
vkCreateRenderPass2Safe ::
                        VkDevice -- ^ device
                                 ->
                          Ptr VkRenderPassCreateInfo2 -- ^ pCreateInfo
                                                      ->
                            Ptr VkAllocationCallbacks -- ^ pAllocator
                                                      -> Ptr VkRenderPass -- ^ pRenderPass
                                                                          -> IO VkResult
vkCreateRenderPass2Safe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateRenderPass2)

{-# NOINLINE vkCreateRenderPass2Safe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateRenderPass2
-- >     ( VkDevice device
-- >     , const VkRenderPassCreateInfo2* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkRenderPass* pRenderPass
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCreateRenderPass2 vkCreateRenderPass2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateRenderPass2 <- vkGetDeviceProc @VkCreateRenderPass2 vkDevice
--
-- or less efficient:
--
-- > myCreateRenderPass2 <- vkGetProc @VkCreateRenderPass2
--
-- __Note:__ @vkCreateRenderPass2Unsafe@ and @vkCreateRenderPass2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateRenderPass2@ is an alias
--           of @vkCreateRenderPass2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateRenderPass2Safe@.
--
vkCreateRenderPass2 ::
                    VkDevice -- ^ device
                             ->
                      Ptr VkRenderPassCreateInfo2 -- ^ pCreateInfo
                                                  ->
                        Ptr VkAllocationCallbacks -- ^ pAllocator
                                                  -> Ptr VkRenderPass -- ^ pRenderPass
                                                                      -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkCreateRenderPass2 = vkCreateRenderPass2Unsafe
#else
vkCreateRenderPass2 = vkCreateRenderPass2Safe

#endif
{-# INLINE vkCreateRenderPass2 #-}

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateRenderPass2
--   >     ( VkDevice device
--   >     , const VkRenderPassCreateInfo2* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkRenderPass* pRenderPass
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCreateRenderPass2 vkCreateRenderPass2 registry at www.khronos.org>
type HS_vkCreateRenderPass2 =
     VkDevice -- ^ device
              ->
       Ptr VkRenderPassCreateInfo2 -- ^ pCreateInfo
                                   ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkRenderPass -- ^ pRenderPass
                                                       -> IO VkResult

type PFN_vkCreateRenderPass2 = FunPtr HS_vkCreateRenderPass2

foreign import ccall unsafe "dynamic"
               unwrapVkCreateRenderPass2Unsafe ::
               PFN_vkCreateRenderPass2 -> HS_vkCreateRenderPass2

foreign import ccall safe "dynamic" unwrapVkCreateRenderPass2Safe
               :: PFN_vkCreateRenderPass2 -> HS_vkCreateRenderPass2

instance VulkanProc "vkCreateRenderPass2" where
    type VkProcType "vkCreateRenderPass2" = HS_vkCreateRenderPass2
    vkProcSymbol = _VkCreateRenderPass2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateRenderPass2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateRenderPass2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdBeginRenderPass2 :: CString

pattern VkCmdBeginRenderPass2 <- (is_VkCmdBeginRenderPass2 -> True)
  where
    VkCmdBeginRenderPass2 = _VkCmdBeginRenderPass2

{-# INLINE _VkCmdBeginRenderPass2 #-}

_VkCmdBeginRenderPass2 :: CString
_VkCmdBeginRenderPass2 = Ptr "vkCmdBeginRenderPass2\NUL"#

{-# INLINE is_VkCmdBeginRenderPass2 #-}

is_VkCmdBeginRenderPass2 :: CString -> Bool
is_VkCmdBeginRenderPass2
  = (EQ ==) . cmpCStrings _VkCmdBeginRenderPass2

type VkCmdBeginRenderPass2 = "vkCmdBeginRenderPass2"

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @outside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdBeginRenderPass2
-- >     ( VkCommandBuffer commandBuffer
-- >     , const VkRenderPassBeginInfo*      pRenderPassBegin
-- >     , const VkSubpassBeginInfo*      pSubpassBeginInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass2 vkCmdBeginRenderPass2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBeginRenderPass2 <- vkGetInstanceProc @VkCmdBeginRenderPass2 vkInstance
--
-- or less efficient:
--
-- > myCmdBeginRenderPass2 <- vkGetProc @VkCmdBeginRenderPass2
--
-- __Note:__ @vkCmdBeginRenderPass2Unsafe@ and @vkCmdBeginRenderPass2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdBeginRenderPass2@ is an alias
--           of @vkCmdBeginRenderPass2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdBeginRenderPass2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall unsafe "vkCmdBeginRenderPass2"
               vkCmdBeginRenderPass2Unsafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Ptr VkRenderPassBeginInfo -- ^ pRenderPassBegin
                                           -> Ptr VkSubpassBeginInfo -- ^ pSubpassBeginInfo
                                                                     -> IO ()

#else
vkCmdBeginRenderPass2Unsafe ::
                            VkCommandBuffer -- ^ commandBuffer
                                            ->
                              Ptr VkRenderPassBeginInfo -- ^ pRenderPassBegin
                                                        -> Ptr VkSubpassBeginInfo -- ^ pSubpassBeginInfo
                                                                                  -> IO ()
vkCmdBeginRenderPass2Unsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkCmdBeginRenderPass2)

{-# NOINLINE vkCmdBeginRenderPass2Unsafe #-}
#endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @outside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdBeginRenderPass2
-- >     ( VkCommandBuffer commandBuffer
-- >     , const VkRenderPassBeginInfo*      pRenderPassBegin
-- >     , const VkSubpassBeginInfo*      pSubpassBeginInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass2 vkCmdBeginRenderPass2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBeginRenderPass2 <- vkGetInstanceProc @VkCmdBeginRenderPass2 vkInstance
--
-- or less efficient:
--
-- > myCmdBeginRenderPass2 <- vkGetProc @VkCmdBeginRenderPass2
--
-- __Note:__ @vkCmdBeginRenderPass2Unsafe@ and @vkCmdBeginRenderPass2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdBeginRenderPass2@ is an alias
--           of @vkCmdBeginRenderPass2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdBeginRenderPass2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall safe "vkCmdBeginRenderPass2"
               vkCmdBeginRenderPass2Safe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Ptr VkRenderPassBeginInfo -- ^ pRenderPassBegin
                                           -> Ptr VkSubpassBeginInfo -- ^ pSubpassBeginInfo
                                                                     -> IO ()

#else
vkCmdBeginRenderPass2Safe ::
                          VkCommandBuffer -- ^ commandBuffer
                                          ->
                            Ptr VkRenderPassBeginInfo -- ^ pRenderPassBegin
                                                      -> Ptr VkSubpassBeginInfo -- ^ pSubpassBeginInfo
                                                                                -> IO ()
vkCmdBeginRenderPass2Safe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdBeginRenderPass2)

{-# NOINLINE vkCmdBeginRenderPass2Safe #-}
#endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @outside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdBeginRenderPass2
-- >     ( VkCommandBuffer commandBuffer
-- >     , const VkRenderPassBeginInfo*      pRenderPassBegin
-- >     , const VkSubpassBeginInfo*      pSubpassBeginInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass2 vkCmdBeginRenderPass2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdBeginRenderPass2 <- vkGetInstanceProc @VkCmdBeginRenderPass2 vkInstance
--
-- or less efficient:
--
-- > myCmdBeginRenderPass2 <- vkGetProc @VkCmdBeginRenderPass2
--
-- __Note:__ @vkCmdBeginRenderPass2Unsafe@ and @vkCmdBeginRenderPass2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdBeginRenderPass2@ is an alias
--           of @vkCmdBeginRenderPass2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdBeginRenderPass2Safe@.
--
vkCmdBeginRenderPass2 ::
                      VkCommandBuffer -- ^ commandBuffer
                                      ->
                        Ptr VkRenderPassBeginInfo -- ^ pRenderPassBegin
                                                  -> Ptr VkSubpassBeginInfo -- ^ pSubpassBeginInfo
                                                                            -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkCmdBeginRenderPass2 = vkCmdBeginRenderPass2Unsafe
#else
vkCmdBeginRenderPass2 = vkCmdBeginRenderPass2Safe

#endif
{-# INLINE vkCmdBeginRenderPass2 #-}

-- | Queues: 'graphics'.
--
--   Renderpass: @outside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdBeginRenderPass2
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkRenderPassBeginInfo*      pRenderPassBegin
--   >     , const VkSubpassBeginInfo*      pSubpassBeginInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass2 vkCmdBeginRenderPass2 registry at www.khronos.org>
type HS_vkCmdBeginRenderPass2 =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Ptr VkRenderPassBeginInfo -- ^ pRenderPassBegin
                                 -> Ptr VkSubpassBeginInfo -- ^ pSubpassBeginInfo
                                                           -> IO ()

type PFN_vkCmdBeginRenderPass2 = FunPtr HS_vkCmdBeginRenderPass2

foreign import ccall unsafe "dynamic"
               unwrapVkCmdBeginRenderPass2Unsafe ::
               PFN_vkCmdBeginRenderPass2 -> HS_vkCmdBeginRenderPass2

foreign import ccall safe "dynamic" unwrapVkCmdBeginRenderPass2Safe
               :: PFN_vkCmdBeginRenderPass2 -> HS_vkCmdBeginRenderPass2

instance VulkanProc "vkCmdBeginRenderPass2" where
    type VkProcType "vkCmdBeginRenderPass2" = HS_vkCmdBeginRenderPass2
    vkProcSymbol = _VkCmdBeginRenderPass2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdBeginRenderPass2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdBeginRenderPass2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdNextSubpass2 :: CString

pattern VkCmdNextSubpass2 <- (is_VkCmdNextSubpass2 -> True)
  where
    VkCmdNextSubpass2 = _VkCmdNextSubpass2

{-# INLINE _VkCmdNextSubpass2 #-}

_VkCmdNextSubpass2 :: CString
_VkCmdNextSubpass2 = Ptr "vkCmdNextSubpass2\NUL"#

{-# INLINE is_VkCmdNextSubpass2 #-}

is_VkCmdNextSubpass2 :: CString -> Bool
is_VkCmdNextSubpass2 = (EQ ==) . cmpCStrings _VkCmdNextSubpass2

type VkCmdNextSubpass2 = "vkCmdNextSubpass2"

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdNextSubpass2
-- >     ( VkCommandBuffer commandBuffer
-- >     , const VkSubpassBeginInfo*      pSubpassBeginInfo
-- >     , const VkSubpassEndInfo*        pSubpassEndInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdNextSubpass2 vkCmdNextSubpass2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdNextSubpass2 <- vkGetInstanceProc @VkCmdNextSubpass2 vkInstance
--
-- or less efficient:
--
-- > myCmdNextSubpass2 <- vkGetProc @VkCmdNextSubpass2
--
-- __Note:__ @vkCmdNextSubpass2Unsafe@ and @vkCmdNextSubpass2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdNextSubpass2@ is an alias
--           of @vkCmdNextSubpass2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdNextSubpass2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall unsafe "vkCmdNextSubpass2"
               vkCmdNextSubpass2Unsafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Ptr VkSubpassBeginInfo -- ^ pSubpassBeginInfo
                                        -> Ptr VkSubpassEndInfo -- ^ pSubpassEndInfo
                                                                -> IO ()

#else
vkCmdNextSubpass2Unsafe ::
                        VkCommandBuffer -- ^ commandBuffer
                                        ->
                          Ptr VkSubpassBeginInfo -- ^ pSubpassBeginInfo
                                                 -> Ptr VkSubpassEndInfo -- ^ pSubpassEndInfo
                                                                         -> IO ()
vkCmdNextSubpass2Unsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkCmdNextSubpass2)

{-# NOINLINE vkCmdNextSubpass2Unsafe #-}
#endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdNextSubpass2
-- >     ( VkCommandBuffer commandBuffer
-- >     , const VkSubpassBeginInfo*      pSubpassBeginInfo
-- >     , const VkSubpassEndInfo*        pSubpassEndInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdNextSubpass2 vkCmdNextSubpass2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdNextSubpass2 <- vkGetInstanceProc @VkCmdNextSubpass2 vkInstance
--
-- or less efficient:
--
-- > myCmdNextSubpass2 <- vkGetProc @VkCmdNextSubpass2
--
-- __Note:__ @vkCmdNextSubpass2Unsafe@ and @vkCmdNextSubpass2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdNextSubpass2@ is an alias
--           of @vkCmdNextSubpass2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdNextSubpass2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall safe "vkCmdNextSubpass2" vkCmdNextSubpass2Safe
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Ptr VkSubpassBeginInfo -- ^ pSubpassBeginInfo
                                        -> Ptr VkSubpassEndInfo -- ^ pSubpassEndInfo
                                                                -> IO ()

#else
vkCmdNextSubpass2Safe ::
                      VkCommandBuffer -- ^ commandBuffer
                                      ->
                        Ptr VkSubpassBeginInfo -- ^ pSubpassBeginInfo
                                               -> Ptr VkSubpassEndInfo -- ^ pSubpassEndInfo
                                                                       -> IO ()
vkCmdNextSubpass2Safe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdNextSubpass2)

{-# NOINLINE vkCmdNextSubpass2Safe #-}
#endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdNextSubpass2
-- >     ( VkCommandBuffer commandBuffer
-- >     , const VkSubpassBeginInfo*      pSubpassBeginInfo
-- >     , const VkSubpassEndInfo*        pSubpassEndInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdNextSubpass2 vkCmdNextSubpass2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdNextSubpass2 <- vkGetInstanceProc @VkCmdNextSubpass2 vkInstance
--
-- or less efficient:
--
-- > myCmdNextSubpass2 <- vkGetProc @VkCmdNextSubpass2
--
-- __Note:__ @vkCmdNextSubpass2Unsafe@ and @vkCmdNextSubpass2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdNextSubpass2@ is an alias
--           of @vkCmdNextSubpass2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdNextSubpass2Safe@.
--
vkCmdNextSubpass2 ::
                  VkCommandBuffer -- ^ commandBuffer
                                  ->
                    Ptr VkSubpassBeginInfo -- ^ pSubpassBeginInfo
                                           -> Ptr VkSubpassEndInfo -- ^ pSubpassEndInfo
                                                                   -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkCmdNextSubpass2 = vkCmdNextSubpass2Unsafe
#else
vkCmdNextSubpass2 = vkCmdNextSubpass2Safe

#endif
{-# INLINE vkCmdNextSubpass2 #-}

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdNextSubpass2
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkSubpassBeginInfo*      pSubpassBeginInfo
--   >     , const VkSubpassEndInfo*        pSubpassEndInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdNextSubpass2 vkCmdNextSubpass2 registry at www.khronos.org>
type HS_vkCmdNextSubpass2 =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Ptr VkSubpassBeginInfo -- ^ pSubpassBeginInfo
                              -> Ptr VkSubpassEndInfo -- ^ pSubpassEndInfo
                                                      -> IO ()

type PFN_vkCmdNextSubpass2 = FunPtr HS_vkCmdNextSubpass2

foreign import ccall unsafe "dynamic" unwrapVkCmdNextSubpass2Unsafe
               :: PFN_vkCmdNextSubpass2 -> HS_vkCmdNextSubpass2

foreign import ccall safe "dynamic" unwrapVkCmdNextSubpass2Safe ::
               PFN_vkCmdNextSubpass2 -> HS_vkCmdNextSubpass2

instance VulkanProc "vkCmdNextSubpass2" where
    type VkProcType "vkCmdNextSubpass2" = HS_vkCmdNextSubpass2
    vkProcSymbol = _VkCmdNextSubpass2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdNextSubpass2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdNextSubpass2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdEndRenderPass2 :: CString

pattern VkCmdEndRenderPass2 <- (is_VkCmdEndRenderPass2 -> True)
  where
    VkCmdEndRenderPass2 = _VkCmdEndRenderPass2

{-# INLINE _VkCmdEndRenderPass2 #-}

_VkCmdEndRenderPass2 :: CString
_VkCmdEndRenderPass2 = Ptr "vkCmdEndRenderPass2\NUL"#

{-# INLINE is_VkCmdEndRenderPass2 #-}

is_VkCmdEndRenderPass2 :: CString -> Bool
is_VkCmdEndRenderPass2 = (EQ ==) . cmpCStrings _VkCmdEndRenderPass2

type VkCmdEndRenderPass2 = "vkCmdEndRenderPass2"

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdEndRenderPass2
-- >     ( VkCommandBuffer commandBuffer
-- >     , const VkSubpassEndInfo*        pSubpassEndInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdEndRenderPass2 vkCmdEndRenderPass2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdEndRenderPass2 <- vkGetInstanceProc @VkCmdEndRenderPass2 vkInstance
--
-- or less efficient:
--
-- > myCmdEndRenderPass2 <- vkGetProc @VkCmdEndRenderPass2
--
-- __Note:__ @vkCmdEndRenderPass2Unsafe@ and @vkCmdEndRenderPass2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdEndRenderPass2@ is an alias
--           of @vkCmdEndRenderPass2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdEndRenderPass2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall unsafe "vkCmdEndRenderPass2"
               vkCmdEndRenderPass2Unsafe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkSubpassEndInfo -- ^ pSubpassEndInfo
                                                       -> IO ()

#else
vkCmdEndRenderPass2Unsafe ::
                          VkCommandBuffer -- ^ commandBuffer
                                          -> Ptr VkSubpassEndInfo -- ^ pSubpassEndInfo
                                                                  -> IO ()
vkCmdEndRenderPass2Unsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkCmdEndRenderPass2)

{-# NOINLINE vkCmdEndRenderPass2Unsafe #-}
#endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdEndRenderPass2
-- >     ( VkCommandBuffer commandBuffer
-- >     , const VkSubpassEndInfo*        pSubpassEndInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdEndRenderPass2 vkCmdEndRenderPass2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdEndRenderPass2 <- vkGetInstanceProc @VkCmdEndRenderPass2 vkInstance
--
-- or less efficient:
--
-- > myCmdEndRenderPass2 <- vkGetProc @VkCmdEndRenderPass2
--
-- __Note:__ @vkCmdEndRenderPass2Unsafe@ and @vkCmdEndRenderPass2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdEndRenderPass2@ is an alias
--           of @vkCmdEndRenderPass2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdEndRenderPass2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall safe "vkCmdEndRenderPass2"
               vkCmdEndRenderPass2Safe ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkSubpassEndInfo -- ^ pSubpassEndInfo
                                                       -> IO ()

#else
vkCmdEndRenderPass2Safe ::
                        VkCommandBuffer -- ^ commandBuffer
                                        -> Ptr VkSubpassEndInfo -- ^ pSubpassEndInfo
                                                                -> IO ()
vkCmdEndRenderPass2Safe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdEndRenderPass2)

{-# NOINLINE vkCmdEndRenderPass2Safe #-}
#endif

-- |
-- Queues: 'graphics'.
--
-- Renderpass: @inside@
--
-- Pipeline: @graphics@
--
-- > void vkCmdEndRenderPass2
-- >     ( VkCommandBuffer commandBuffer
-- >     , const VkSubpassEndInfo*        pSubpassEndInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdEndRenderPass2 vkCmdEndRenderPass2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdEndRenderPass2 <- vkGetInstanceProc @VkCmdEndRenderPass2 vkInstance
--
-- or less efficient:
--
-- > myCmdEndRenderPass2 <- vkGetProc @VkCmdEndRenderPass2
--
-- __Note:__ @vkCmdEndRenderPass2Unsafe@ and @vkCmdEndRenderPass2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdEndRenderPass2@ is an alias
--           of @vkCmdEndRenderPass2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdEndRenderPass2Safe@.
--
vkCmdEndRenderPass2 ::
                    VkCommandBuffer -- ^ commandBuffer
                                    -> Ptr VkSubpassEndInfo -- ^ pSubpassEndInfo
                                                            -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkCmdEndRenderPass2 = vkCmdEndRenderPass2Unsafe
#else
vkCmdEndRenderPass2 = vkCmdEndRenderPass2Safe

#endif
{-# INLINE vkCmdEndRenderPass2 #-}

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdEndRenderPass2
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkSubpassEndInfo*        pSubpassEndInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdEndRenderPass2 vkCmdEndRenderPass2 registry at www.khronos.org>
type HS_vkCmdEndRenderPass2 =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr VkSubpassEndInfo -- ^ pSubpassEndInfo
                                             -> IO ()

type PFN_vkCmdEndRenderPass2 = FunPtr HS_vkCmdEndRenderPass2

foreign import ccall unsafe "dynamic"
               unwrapVkCmdEndRenderPass2Unsafe ::
               PFN_vkCmdEndRenderPass2 -> HS_vkCmdEndRenderPass2

foreign import ccall safe "dynamic" unwrapVkCmdEndRenderPass2Safe
               :: PFN_vkCmdEndRenderPass2 -> HS_vkCmdEndRenderPass2

instance VulkanProc "vkCmdEndRenderPass2" where
    type VkProcType "vkCmdEndRenderPass2" = HS_vkCmdEndRenderPass2
    vkProcSymbol = _VkCmdEndRenderPass2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdEndRenderPass2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdEndRenderPass2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2 ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2 =
        VkStructureType 1000109000

pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2 :: VkStructureType

pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2 =
        VkStructureType 1000109001

pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2 :: VkStructureType

pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2 =
        VkStructureType 1000109002

pattern VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2 :: VkStructureType

pattern VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2 =
        VkStructureType 1000109003

pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2 ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2 =
        VkStructureType 1000109004

pattern VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO =
        VkStructureType 1000109005

pattern VK_STRUCTURE_TYPE_SUBPASS_END_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_SUBPASS_END_INFO =
        VkStructureType 1000109006

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES =
        VkStructureType 1000177000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES =
        VkStructureType 1000196000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES
        = VkStructureType 1000180000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES
        = VkStructureType 1000082000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES
        = VkStructureType 1000197000

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO
        = VkStructureType 1000161000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES
        = VkStructureType 1000161001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES
        = VkStructureType 1000161002

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO
        = VkStructureType 1000161003

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT
        = VkStructureType 1000161004

-- | bitpos = @1@
pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT ::
        VkDescriptorPoolCreateBitmask a

pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT =
        VkDescriptorPoolCreateBitmask 2

-- | bitpos = @1@
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT
        :: VkDescriptorSetLayoutCreateBitmask a

pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT
        = VkDescriptorSetLayoutCreateBitmask 2

pattern VK_ERROR_FRAGMENTATION :: VkResult

pattern VK_ERROR_FRAGMENTATION = VkResult (-1000161000)

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES
        = VkStructureType 1000199000

pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE
        = VkStructureType 1000199001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES
        = VkStructureType 1000221000

pattern VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO =
        VkStructureType 1000246000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES
        = VkStructureType 1000130000

pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO =
        VkStructureType 1000130001

-- | Format can be used with min/max reduction filtering
--
--   bitpos = @16@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT =
        VkFormatFeatureBitmask 65536

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES
        = VkStructureType 1000211000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES
        = VkStructureType 1000108000

pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO =
        VkStructureType 1000108001

pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO =
        VkStructureType 1000108002

pattern VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO =
        VkStructureType 1000108003

-- | bitpos = @0@
pattern VK_FRAMEBUFFER_CREATE_IMAGELESS_BIT ::
        VkFramebufferCreateBitmask a

pattern VK_FRAMEBUFFER_CREATE_IMAGELESS_BIT =
        VkFramebufferCreateBitmask 1

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES
        = VkStructureType 1000253000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES
        = VkStructureType 1000175000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES
        = VkStructureType 1000241000

pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT =
        VkStructureType 1000241001

pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT =
        VkStructureType 1000241002

pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL =
        VkImageLayout 1000241000

pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL =
        VkImageLayout 1000241001

pattern VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL =
        VkImageLayout 1000241002

pattern VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL :: VkImageLayout

pattern VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL =
        VkImageLayout 1000241003

pattern VkResetQueryPool :: CString

pattern VkResetQueryPool <- (is_VkResetQueryPool -> True)
  where
    VkResetQueryPool = _VkResetQueryPool

{-# INLINE _VkResetQueryPool #-}

_VkResetQueryPool :: CString
_VkResetQueryPool = Ptr "vkResetQueryPool\NUL"#

{-# INLINE is_VkResetQueryPool #-}

is_VkResetQueryPool :: CString -> Bool
is_VkResetQueryPool = (EQ ==) . cmpCStrings _VkResetQueryPool

type VkResetQueryPool = "vkResetQueryPool"

-- |
-- > void vkResetQueryPool
-- >     ( VkDevice device
-- >     , VkQueryPool queryPool
-- >     , uint32_t firstQuery
-- >     , uint32_t queryCount
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkResetQueryPool vkResetQueryPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myResetQueryPool <- vkGetDeviceProc @VkResetQueryPool vkDevice
--
-- or less efficient:
--
-- > myResetQueryPool <- vkGetProc @VkResetQueryPool
--
-- __Note:__ @vkResetQueryPoolUnsafe@ and @vkResetQueryPoolSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkResetQueryPool@ is an alias
--           of @vkResetQueryPoolUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkResetQueryPoolSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall unsafe "vkResetQueryPool"
               vkResetQueryPoolUnsafe ::
               VkDevice -- ^ device
                        -> VkQueryPool -- ^ queryPool
                                       -> Word32 -- ^ firstQuery
                                                 -> Word32 -- ^ queryCount
                                                           -> IO ()

#else
vkResetQueryPoolUnsafe ::
                       VkDevice -- ^ device
                                -> VkQueryPool -- ^ queryPool
                                               -> Word32 -- ^ firstQuery
                                                         -> Word32 -- ^ queryCount
                                                                   -> IO ()
vkResetQueryPoolUnsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkResetQueryPool)

{-# NOINLINE vkResetQueryPoolUnsafe #-}
#endif

-- |
-- > void vkResetQueryPool
-- >     ( VkDevice device
-- >     , VkQueryPool queryPool
-- >     , uint32_t firstQuery
-- >     , uint32_t queryCount
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkResetQueryPool vkResetQueryPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myResetQueryPool <- vkGetDeviceProc @VkResetQueryPool vkDevice
--
-- or less efficient:
--
-- > myResetQueryPool <- vkGetProc @VkResetQueryPool
--
-- __Note:__ @vkResetQueryPoolUnsafe@ and @vkResetQueryPoolSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkResetQueryPool@ is an alias
--           of @vkResetQueryPoolUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkResetQueryPoolSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall safe "vkResetQueryPool" vkResetQueryPoolSafe
               :: VkDevice -- ^ device
                           -> VkQueryPool -- ^ queryPool
                                          -> Word32 -- ^ firstQuery
                                                    -> Word32 -- ^ queryCount
                                                              -> IO ()

#else
vkResetQueryPoolSafe ::
                     VkDevice -- ^ device
                              -> VkQueryPool -- ^ queryPool
                                             -> Word32 -- ^ firstQuery
                                                       -> Word32 -- ^ queryCount
                                                                 -> IO ()
vkResetQueryPoolSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkResetQueryPool)

{-# NOINLINE vkResetQueryPoolSafe #-}
#endif

-- |
-- > void vkResetQueryPool
-- >     ( VkDevice device
-- >     , VkQueryPool queryPool
-- >     , uint32_t firstQuery
-- >     , uint32_t queryCount
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkResetQueryPool vkResetQueryPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myResetQueryPool <- vkGetDeviceProc @VkResetQueryPool vkDevice
--
-- or less efficient:
--
-- > myResetQueryPool <- vkGetProc @VkResetQueryPool
--
-- __Note:__ @vkResetQueryPoolUnsafe@ and @vkResetQueryPoolSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkResetQueryPool@ is an alias
--           of @vkResetQueryPoolUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkResetQueryPoolSafe@.
--
vkResetQueryPool ::
                 VkDevice -- ^ device
                          -> VkQueryPool -- ^ queryPool
                                         -> Word32 -- ^ firstQuery
                                                   -> Word32 -- ^ queryCount
                                                             -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkResetQueryPool = vkResetQueryPoolUnsafe
#else
vkResetQueryPool = vkResetQueryPoolSafe

#endif
{-# INLINE vkResetQueryPool #-}

-- | > void vkResetQueryPool
--   >     ( VkDevice device
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     , uint32_t queryCount
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkResetQueryPool vkResetQueryPool registry at www.khronos.org>
type HS_vkResetQueryPool =
     VkDevice -- ^ device
              -> VkQueryPool -- ^ queryPool
                             -> Word32 -- ^ firstQuery
                                       -> Word32 -- ^ queryCount
                                                 -> IO ()

type PFN_vkResetQueryPool = FunPtr HS_vkResetQueryPool

foreign import ccall unsafe "dynamic" unwrapVkResetQueryPoolUnsafe
               :: PFN_vkResetQueryPool -> HS_vkResetQueryPool

foreign import ccall safe "dynamic" unwrapVkResetQueryPoolSafe ::
               PFN_vkResetQueryPool -> HS_vkResetQueryPool

instance VulkanProc "vkResetQueryPool" where
    type VkProcType "vkResetQueryPool" = HS_vkResetQueryPool
    vkProcSymbol = _VkResetQueryPool

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkResetQueryPoolUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkResetQueryPoolSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES
        = VkStructureType 1000261000

pattern VkGetSemaphoreCounterValue :: CString

pattern VkGetSemaphoreCounterValue <-
        (is_VkGetSemaphoreCounterValue -> True)
  where
    VkGetSemaphoreCounterValue = _VkGetSemaphoreCounterValue

{-# INLINE _VkGetSemaphoreCounterValue #-}

_VkGetSemaphoreCounterValue :: CString
_VkGetSemaphoreCounterValue = Ptr "vkGetSemaphoreCounterValue\NUL"#

{-# INLINE is_VkGetSemaphoreCounterValue #-}

is_VkGetSemaphoreCounterValue :: CString -> Bool
is_VkGetSemaphoreCounterValue
  = (EQ ==) . cmpCStrings _VkGetSemaphoreCounterValue

type VkGetSemaphoreCounterValue = "vkGetSemaphoreCounterValue"

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkGetSemaphoreCounterValue
-- >     ( VkDevice device
-- >     , VkSemaphore semaphore
-- >     , uint64_t* pValue
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetSemaphoreCounterValue vkGetSemaphoreCounterValue registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetSemaphoreCounterValue <- vkGetDeviceProc @VkGetSemaphoreCounterValue vkDevice
--
-- or less efficient:
--
-- > myGetSemaphoreCounterValue <- vkGetProc @VkGetSemaphoreCounterValue
--
-- __Note:__ @vkGetSemaphoreCounterValueUnsafe@ and @vkGetSemaphoreCounterValueSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetSemaphoreCounterValue@ is an alias
--           of @vkGetSemaphoreCounterValueUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetSemaphoreCounterValueSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall unsafe "vkGetSemaphoreCounterValue"
               vkGetSemaphoreCounterValueUnsafe ::
               VkDevice -- ^ device
                        -> VkSemaphore -- ^ semaphore
                                       -> Ptr Word64 -- ^ pValue
                                                     -> IO VkResult

#else
vkGetSemaphoreCounterValueUnsafe ::
                                 VkDevice -- ^ device
                                          -> VkSemaphore -- ^ semaphore
                                                         -> Ptr Word64 -- ^ pValue
                                                                       -> IO VkResult
vkGetSemaphoreCounterValueUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetSemaphoreCounterValue)

{-# NOINLINE vkGetSemaphoreCounterValueUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkGetSemaphoreCounterValue
-- >     ( VkDevice device
-- >     , VkSemaphore semaphore
-- >     , uint64_t* pValue
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetSemaphoreCounterValue vkGetSemaphoreCounterValue registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetSemaphoreCounterValue <- vkGetDeviceProc @VkGetSemaphoreCounterValue vkDevice
--
-- or less efficient:
--
-- > myGetSemaphoreCounterValue <- vkGetProc @VkGetSemaphoreCounterValue
--
-- __Note:__ @vkGetSemaphoreCounterValueUnsafe@ and @vkGetSemaphoreCounterValueSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetSemaphoreCounterValue@ is an alias
--           of @vkGetSemaphoreCounterValueUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetSemaphoreCounterValueSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall safe "vkGetSemaphoreCounterValue"
               vkGetSemaphoreCounterValueSafe ::
               VkDevice -- ^ device
                        -> VkSemaphore -- ^ semaphore
                                       -> Ptr Word64 -- ^ pValue
                                                     -> IO VkResult

#else
vkGetSemaphoreCounterValueSafe ::
                               VkDevice -- ^ device
                                        -> VkSemaphore -- ^ semaphore
                                                       -> Ptr Word64 -- ^ pValue
                                                                     -> IO VkResult
vkGetSemaphoreCounterValueSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetSemaphoreCounterValue)

{-# NOINLINE vkGetSemaphoreCounterValueSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkGetSemaphoreCounterValue
-- >     ( VkDevice device
-- >     , VkSemaphore semaphore
-- >     , uint64_t* pValue
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetSemaphoreCounterValue vkGetSemaphoreCounterValue registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetSemaphoreCounterValue <- vkGetDeviceProc @VkGetSemaphoreCounterValue vkDevice
--
-- or less efficient:
--
-- > myGetSemaphoreCounterValue <- vkGetProc @VkGetSemaphoreCounterValue
--
-- __Note:__ @vkGetSemaphoreCounterValueUnsafe@ and @vkGetSemaphoreCounterValueSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetSemaphoreCounterValue@ is an alias
--           of @vkGetSemaphoreCounterValueUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetSemaphoreCounterValueSafe@.
--
vkGetSemaphoreCounterValue ::
                           VkDevice -- ^ device
                                    -> VkSemaphore -- ^ semaphore
                                                   -> Ptr Word64 -- ^ pValue
                                                                 -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkGetSemaphoreCounterValue = vkGetSemaphoreCounterValueUnsafe
#else
vkGetSemaphoreCounterValue = vkGetSemaphoreCounterValueSafe

#endif
{-# INLINE vkGetSemaphoreCounterValue #-}

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkGetSemaphoreCounterValue
--   >     ( VkDevice device
--   >     , VkSemaphore semaphore
--   >     , uint64_t* pValue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetSemaphoreCounterValue vkGetSemaphoreCounterValue registry at www.khronos.org>
type HS_vkGetSemaphoreCounterValue =
     VkDevice -- ^ device
              -> VkSemaphore -- ^ semaphore
                             -> Ptr Word64 -- ^ pValue
                                           -> IO VkResult

type PFN_vkGetSemaphoreCounterValue =
     FunPtr HS_vkGetSemaphoreCounterValue

foreign import ccall unsafe "dynamic"
               unwrapVkGetSemaphoreCounterValueUnsafe ::
               PFN_vkGetSemaphoreCounterValue -> HS_vkGetSemaphoreCounterValue

foreign import ccall safe "dynamic"
               unwrapVkGetSemaphoreCounterValueSafe ::
               PFN_vkGetSemaphoreCounterValue -> HS_vkGetSemaphoreCounterValue

instance VulkanProc "vkGetSemaphoreCounterValue" where
    type VkProcType "vkGetSemaphoreCounterValue" =
         HS_vkGetSemaphoreCounterValue
    vkProcSymbol = _VkGetSemaphoreCounterValue

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetSemaphoreCounterValueUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetSemaphoreCounterValueSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkWaitSemaphores :: CString

pattern VkWaitSemaphores <- (is_VkWaitSemaphores -> True)
  where
    VkWaitSemaphores = _VkWaitSemaphores

{-# INLINE _VkWaitSemaphores #-}

_VkWaitSemaphores :: CString
_VkWaitSemaphores = Ptr "vkWaitSemaphores\NUL"#

{-# INLINE is_VkWaitSemaphores #-}

is_VkWaitSemaphores :: CString -> Bool
is_VkWaitSemaphores = (EQ ==) . cmpCStrings _VkWaitSemaphores

type VkWaitSemaphores = "vkWaitSemaphores"

-- |
-- Success codes: 'VK_SUCCESS', 'VK_TIMEOUT'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkWaitSemaphores
-- >     ( VkDevice device
-- >     , const VkSemaphoreWaitInfo* pWaitInfo
-- >     , uint64_t timeout
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkWaitSemaphores vkWaitSemaphores registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myWaitSemaphores <- vkGetDeviceProc @VkWaitSemaphores vkDevice
--
-- or less efficient:
--
-- > myWaitSemaphores <- vkGetProc @VkWaitSemaphores
--
-- __Note:__ @vkWaitSemaphoresUnsafe@ and @vkWaitSemaphoresSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkWaitSemaphores@ is an alias
--           of @vkWaitSemaphoresUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkWaitSemaphoresSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall unsafe "vkWaitSemaphores"
               vkWaitSemaphoresUnsafe ::
               VkDevice -- ^ device
                        -> Ptr VkSemaphoreWaitInfo -- ^ pWaitInfo
                                                   -> Word64 -- ^ timeout
                                                             -> IO VkResult

#else
vkWaitSemaphoresUnsafe ::
                       VkDevice -- ^ device
                                -> Ptr VkSemaphoreWaitInfo -- ^ pWaitInfo
                                                           -> Word64 -- ^ timeout
                                                                     -> IO VkResult
vkWaitSemaphoresUnsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkWaitSemaphores)

{-# NOINLINE vkWaitSemaphoresUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_TIMEOUT'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkWaitSemaphores
-- >     ( VkDevice device
-- >     , const VkSemaphoreWaitInfo* pWaitInfo
-- >     , uint64_t timeout
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkWaitSemaphores vkWaitSemaphores registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myWaitSemaphores <- vkGetDeviceProc @VkWaitSemaphores vkDevice
--
-- or less efficient:
--
-- > myWaitSemaphores <- vkGetProc @VkWaitSemaphores
--
-- __Note:__ @vkWaitSemaphoresUnsafe@ and @vkWaitSemaphoresSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkWaitSemaphores@ is an alias
--           of @vkWaitSemaphoresUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkWaitSemaphoresSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall safe "vkWaitSemaphores" vkWaitSemaphoresSafe
               :: VkDevice -- ^ device
                           -> Ptr VkSemaphoreWaitInfo -- ^ pWaitInfo
                                                      -> Word64 -- ^ timeout
                                                                -> IO VkResult

#else
vkWaitSemaphoresSafe ::
                     VkDevice -- ^ device
                              -> Ptr VkSemaphoreWaitInfo -- ^ pWaitInfo
                                                         -> Word64 -- ^ timeout
                                                                   -> IO VkResult
vkWaitSemaphoresSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkWaitSemaphores)

{-# NOINLINE vkWaitSemaphoresSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_TIMEOUT'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
-- > VkResult vkWaitSemaphores
-- >     ( VkDevice device
-- >     , const VkSemaphoreWaitInfo* pWaitInfo
-- >     , uint64_t timeout
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkWaitSemaphores vkWaitSemaphores registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myWaitSemaphores <- vkGetDeviceProc @VkWaitSemaphores vkDevice
--
-- or less efficient:
--
-- > myWaitSemaphores <- vkGetProc @VkWaitSemaphores
--
-- __Note:__ @vkWaitSemaphoresUnsafe@ and @vkWaitSemaphoresSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkWaitSemaphores@ is an alias
--           of @vkWaitSemaphoresUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkWaitSemaphoresSafe@.
--
vkWaitSemaphores ::
                 VkDevice -- ^ device
                          -> Ptr VkSemaphoreWaitInfo -- ^ pWaitInfo
                                                     -> Word64 -- ^ timeout
                                                               -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkWaitSemaphores = vkWaitSemaphoresUnsafe
#else
vkWaitSemaphores = vkWaitSemaphoresSafe

#endif
{-# INLINE vkWaitSemaphores #-}

-- | Success codes: 'VK_SUCCESS', 'VK_TIMEOUT'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkWaitSemaphores
--   >     ( VkDevice device
--   >     , const VkSemaphoreWaitInfo* pWaitInfo
--   >     , uint64_t timeout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkWaitSemaphores vkWaitSemaphores registry at www.khronos.org>
type HS_vkWaitSemaphores =
     VkDevice -- ^ device
              -> Ptr VkSemaphoreWaitInfo -- ^ pWaitInfo
                                         -> Word64 -- ^ timeout
                                                   -> IO VkResult

type PFN_vkWaitSemaphores = FunPtr HS_vkWaitSemaphores

foreign import ccall unsafe "dynamic" unwrapVkWaitSemaphoresUnsafe
               :: PFN_vkWaitSemaphores -> HS_vkWaitSemaphores

foreign import ccall safe "dynamic" unwrapVkWaitSemaphoresSafe ::
               PFN_vkWaitSemaphores -> HS_vkWaitSemaphores

instance VulkanProc "vkWaitSemaphores" where
    type VkProcType "vkWaitSemaphores" = HS_vkWaitSemaphores
    vkProcSymbol = _VkWaitSemaphores

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkWaitSemaphoresUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkWaitSemaphoresSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkSignalSemaphore :: CString

pattern VkSignalSemaphore <- (is_VkSignalSemaphore -> True)
  where
    VkSignalSemaphore = _VkSignalSemaphore

{-# INLINE _VkSignalSemaphore #-}

_VkSignalSemaphore :: CString
_VkSignalSemaphore = Ptr "vkSignalSemaphore\NUL"#

{-# INLINE is_VkSignalSemaphore #-}

is_VkSignalSemaphore :: CString -> Bool
is_VkSignalSemaphore = (EQ ==) . cmpCStrings _VkSignalSemaphore

type VkSignalSemaphore = "vkSignalSemaphore"

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkSignalSemaphore
-- >     ( VkDevice device
-- >     , const VkSemaphoreSignalInfo* pSignalInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkSignalSemaphore vkSignalSemaphore registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > mySignalSemaphore <- vkGetDeviceProc @VkSignalSemaphore vkDevice
--
-- or less efficient:
--
-- > mySignalSemaphore <- vkGetProc @VkSignalSemaphore
--
-- __Note:__ @vkSignalSemaphoreUnsafe@ and @vkSignalSemaphoreSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkSignalSemaphore@ is an alias
--           of @vkSignalSemaphoreUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkSignalSemaphoreSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall unsafe "vkSignalSemaphore"
               vkSignalSemaphoreUnsafe ::
               VkDevice -- ^ device
                        -> Ptr VkSemaphoreSignalInfo -- ^ pSignalInfo
                                                     -> IO VkResult

#else
vkSignalSemaphoreUnsafe ::
                        VkDevice -- ^ device
                                 -> Ptr VkSemaphoreSignalInfo -- ^ pSignalInfo
                                                              -> IO VkResult
vkSignalSemaphoreUnsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkSignalSemaphore)

{-# NOINLINE vkSignalSemaphoreUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkSignalSemaphore
-- >     ( VkDevice device
-- >     , const VkSemaphoreSignalInfo* pSignalInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkSignalSemaphore vkSignalSemaphore registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > mySignalSemaphore <- vkGetDeviceProc @VkSignalSemaphore vkDevice
--
-- or less efficient:
--
-- > mySignalSemaphore <- vkGetProc @VkSignalSemaphore
--
-- __Note:__ @vkSignalSemaphoreUnsafe@ and @vkSignalSemaphoreSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkSignalSemaphore@ is an alias
--           of @vkSignalSemaphoreUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkSignalSemaphoreSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall safe "vkSignalSemaphore" vkSignalSemaphoreSafe
               :: VkDevice -- ^ device
                           -> Ptr VkSemaphoreSignalInfo -- ^ pSignalInfo
                                                        -> IO VkResult

#else
vkSignalSemaphoreSafe ::
                      VkDevice -- ^ device
                               -> Ptr VkSemaphoreSignalInfo -- ^ pSignalInfo
                                                            -> IO VkResult
vkSignalSemaphoreSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkSignalSemaphore)

{-# NOINLINE vkSignalSemaphoreSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkSignalSemaphore
-- >     ( VkDevice device
-- >     , const VkSemaphoreSignalInfo* pSignalInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkSignalSemaphore vkSignalSemaphore registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > mySignalSemaphore <- vkGetDeviceProc @VkSignalSemaphore vkDevice
--
-- or less efficient:
--
-- > mySignalSemaphore <- vkGetProc @VkSignalSemaphore
--
-- __Note:__ @vkSignalSemaphoreUnsafe@ and @vkSignalSemaphoreSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkSignalSemaphore@ is an alias
--           of @vkSignalSemaphoreUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkSignalSemaphoreSafe@.
--
vkSignalSemaphore ::
                  VkDevice -- ^ device
                           -> Ptr VkSemaphoreSignalInfo -- ^ pSignalInfo
                                                        -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkSignalSemaphore = vkSignalSemaphoreUnsafe
#else
vkSignalSemaphore = vkSignalSemaphoreSafe

#endif
{-# INLINE vkSignalSemaphore #-}

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkSignalSemaphore
--   >     ( VkDevice device
--   >     , const VkSemaphoreSignalInfo* pSignalInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkSignalSemaphore vkSignalSemaphore registry at www.khronos.org>
type HS_vkSignalSemaphore =
     VkDevice -- ^ device
              -> Ptr VkSemaphoreSignalInfo -- ^ pSignalInfo
                                           -> IO VkResult

type PFN_vkSignalSemaphore = FunPtr HS_vkSignalSemaphore

foreign import ccall unsafe "dynamic" unwrapVkSignalSemaphoreUnsafe
               :: PFN_vkSignalSemaphore -> HS_vkSignalSemaphore

foreign import ccall safe "dynamic" unwrapVkSignalSemaphoreSafe ::
               PFN_vkSignalSemaphore -> HS_vkSignalSemaphore

instance VulkanProc "vkSignalSemaphore" where
    type VkProcType "vkSignalSemaphore" = HS_vkSignalSemaphore
    vkProcSymbol = _VkSignalSemaphore

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkSignalSemaphoreUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkSignalSemaphoreSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES
        = VkStructureType 1000207000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES
        = VkStructureType 1000207001

pattern VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO =
        VkStructureType 1000207002

pattern VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO =
        VkStructureType 1000207003

pattern VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO =
        VkStructureType 1000207004

pattern VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO =
        VkStructureType 1000207005

pattern VkGetBufferDeviceAddress :: CString

pattern VkGetBufferDeviceAddress <-
        (is_VkGetBufferDeviceAddress -> True)
  where
    VkGetBufferDeviceAddress = _VkGetBufferDeviceAddress

{-# INLINE _VkGetBufferDeviceAddress #-}

_VkGetBufferDeviceAddress :: CString
_VkGetBufferDeviceAddress = Ptr "vkGetBufferDeviceAddress\NUL"#

{-# INLINE is_VkGetBufferDeviceAddress #-}

is_VkGetBufferDeviceAddress :: CString -> Bool
is_VkGetBufferDeviceAddress
  = (EQ ==) . cmpCStrings _VkGetBufferDeviceAddress

type VkGetBufferDeviceAddress = "vkGetBufferDeviceAddress"

-- |
-- > VkDeviceAddress vkGetBufferDeviceAddress
-- >     ( VkDevice device
-- >     , const VkBufferDeviceAddressInfo* pInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetBufferDeviceAddress vkGetBufferDeviceAddress registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetBufferDeviceAddress <- vkGetDeviceProc @VkGetBufferDeviceAddress vkDevice
--
-- or less efficient:
--
-- > myGetBufferDeviceAddress <- vkGetProc @VkGetBufferDeviceAddress
--
-- __Note:__ @vkGetBufferDeviceAddressUnsafe@ and @vkGetBufferDeviceAddressSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetBufferDeviceAddress@ is an alias
--           of @vkGetBufferDeviceAddressUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetBufferDeviceAddressSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall unsafe "vkGetBufferDeviceAddress"
               vkGetBufferDeviceAddressUnsafe ::
               VkDevice -- ^ device
                        -> Ptr VkBufferDeviceAddressInfo -- ^ pInfo
                                                         -> IO VkDeviceAddress

#else
vkGetBufferDeviceAddressUnsafe ::
                               VkDevice -- ^ device
                                        -> Ptr VkBufferDeviceAddressInfo -- ^ pInfo
                                                                         -> IO VkDeviceAddress
vkGetBufferDeviceAddressUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetBufferDeviceAddress)

{-# NOINLINE vkGetBufferDeviceAddressUnsafe #-}
#endif

-- |
-- > VkDeviceAddress vkGetBufferDeviceAddress
-- >     ( VkDevice device
-- >     , const VkBufferDeviceAddressInfo* pInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetBufferDeviceAddress vkGetBufferDeviceAddress registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetBufferDeviceAddress <- vkGetDeviceProc @VkGetBufferDeviceAddress vkDevice
--
-- or less efficient:
--
-- > myGetBufferDeviceAddress <- vkGetProc @VkGetBufferDeviceAddress
--
-- __Note:__ @vkGetBufferDeviceAddressUnsafe@ and @vkGetBufferDeviceAddressSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetBufferDeviceAddress@ is an alias
--           of @vkGetBufferDeviceAddressUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetBufferDeviceAddressSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall safe "vkGetBufferDeviceAddress"
               vkGetBufferDeviceAddressSafe ::
               VkDevice -- ^ device
                        -> Ptr VkBufferDeviceAddressInfo -- ^ pInfo
                                                         -> IO VkDeviceAddress

#else
vkGetBufferDeviceAddressSafe ::
                             VkDevice -- ^ device
                                      -> Ptr VkBufferDeviceAddressInfo -- ^ pInfo
                                                                       -> IO VkDeviceAddress
vkGetBufferDeviceAddressSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkGetBufferDeviceAddress)

{-# NOINLINE vkGetBufferDeviceAddressSafe #-}
#endif

-- |
-- > VkDeviceAddress vkGetBufferDeviceAddress
-- >     ( VkDevice device
-- >     , const VkBufferDeviceAddressInfo* pInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetBufferDeviceAddress vkGetBufferDeviceAddress registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetBufferDeviceAddress <- vkGetDeviceProc @VkGetBufferDeviceAddress vkDevice
--
-- or less efficient:
--
-- > myGetBufferDeviceAddress <- vkGetProc @VkGetBufferDeviceAddress
--
-- __Note:__ @vkGetBufferDeviceAddressUnsafe@ and @vkGetBufferDeviceAddressSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetBufferDeviceAddress@ is an alias
--           of @vkGetBufferDeviceAddressUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetBufferDeviceAddressSafe@.
--
vkGetBufferDeviceAddress ::
                         VkDevice -- ^ device
                                  -> Ptr VkBufferDeviceAddressInfo -- ^ pInfo
                                                                   -> IO VkDeviceAddress
#ifdef UNSAFE_FFI_DEFAULT
vkGetBufferDeviceAddress = vkGetBufferDeviceAddressUnsafe
#else
vkGetBufferDeviceAddress = vkGetBufferDeviceAddressSafe

#endif
{-# INLINE vkGetBufferDeviceAddress #-}

-- | > VkDeviceAddress vkGetBufferDeviceAddress
--   >     ( VkDevice device
--   >     , const VkBufferDeviceAddressInfo* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetBufferDeviceAddress vkGetBufferDeviceAddress registry at www.khronos.org>
type HS_vkGetBufferDeviceAddress =
     VkDevice -- ^ device
              -> Ptr VkBufferDeviceAddressInfo -- ^ pInfo
                                               -> IO VkDeviceAddress

type PFN_vkGetBufferDeviceAddress =
     FunPtr HS_vkGetBufferDeviceAddress

foreign import ccall unsafe "dynamic"
               unwrapVkGetBufferDeviceAddressUnsafe ::
               PFN_vkGetBufferDeviceAddress -> HS_vkGetBufferDeviceAddress

foreign import ccall safe "dynamic"
               unwrapVkGetBufferDeviceAddressSafe ::
               PFN_vkGetBufferDeviceAddress -> HS_vkGetBufferDeviceAddress

instance VulkanProc "vkGetBufferDeviceAddress" where
    type VkProcType "vkGetBufferDeviceAddress" =
         HS_vkGetBufferDeviceAddress
    vkProcSymbol = _VkGetBufferDeviceAddress

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetBufferDeviceAddressUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetBufferDeviceAddressSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetBufferOpaqueCaptureAddress :: CString

pattern VkGetBufferOpaqueCaptureAddress <-
        (is_VkGetBufferOpaqueCaptureAddress -> True)
  where
    VkGetBufferOpaqueCaptureAddress = _VkGetBufferOpaqueCaptureAddress

{-# INLINE _VkGetBufferOpaqueCaptureAddress #-}

_VkGetBufferOpaqueCaptureAddress :: CString
_VkGetBufferOpaqueCaptureAddress
  = Ptr "vkGetBufferOpaqueCaptureAddress\NUL"#

{-# INLINE is_VkGetBufferOpaqueCaptureAddress #-}

is_VkGetBufferOpaqueCaptureAddress :: CString -> Bool
is_VkGetBufferOpaqueCaptureAddress
  = (EQ ==) . cmpCStrings _VkGetBufferOpaqueCaptureAddress

type VkGetBufferOpaqueCaptureAddress =
     "vkGetBufferOpaqueCaptureAddress"

-- |
-- > uint64_t vkGetBufferOpaqueCaptureAddress
-- >     ( VkDevice device
-- >     , const VkBufferDeviceAddressInfo* pInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetBufferOpaqueCaptureAddress vkGetBufferOpaqueCaptureAddress registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetBufferOpaqueCaptureAddress <- vkGetDeviceProc @VkGetBufferOpaqueCaptureAddress vkDevice
--
-- or less efficient:
--
-- > myGetBufferOpaqueCaptureAddress <- vkGetProc @VkGetBufferOpaqueCaptureAddress
--
-- __Note:__ @vkGetBufferOpaqueCaptureAddressUnsafe@ and @vkGetBufferOpaqueCaptureAddressSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetBufferOpaqueCaptureAddress@ is an alias
--           of @vkGetBufferOpaqueCaptureAddressUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetBufferOpaqueCaptureAddressSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall unsafe "vkGetBufferOpaqueCaptureAddress"
               vkGetBufferOpaqueCaptureAddressUnsafe ::
               VkDevice -- ^ device
                        -> Ptr VkBufferDeviceAddressInfo -- ^ pInfo
                                                         -> IO Word64

#else
vkGetBufferOpaqueCaptureAddressUnsafe ::
                                      VkDevice -- ^ device
                                               -> Ptr VkBufferDeviceAddressInfo -- ^ pInfo
                                                                                -> IO Word64
vkGetBufferOpaqueCaptureAddressUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetBufferOpaqueCaptureAddress)

{-# NOINLINE vkGetBufferOpaqueCaptureAddressUnsafe #-}
#endif

-- |
-- > uint64_t vkGetBufferOpaqueCaptureAddress
-- >     ( VkDevice device
-- >     , const VkBufferDeviceAddressInfo* pInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetBufferOpaqueCaptureAddress vkGetBufferOpaqueCaptureAddress registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetBufferOpaqueCaptureAddress <- vkGetDeviceProc @VkGetBufferOpaqueCaptureAddress vkDevice
--
-- or less efficient:
--
-- > myGetBufferOpaqueCaptureAddress <- vkGetProc @VkGetBufferOpaqueCaptureAddress
--
-- __Note:__ @vkGetBufferOpaqueCaptureAddressUnsafe@ and @vkGetBufferOpaqueCaptureAddressSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetBufferOpaqueCaptureAddress@ is an alias
--           of @vkGetBufferOpaqueCaptureAddressUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetBufferOpaqueCaptureAddressSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall safe "vkGetBufferOpaqueCaptureAddress"
               vkGetBufferOpaqueCaptureAddressSafe ::
               VkDevice -- ^ device
                        -> Ptr VkBufferDeviceAddressInfo -- ^ pInfo
                                                         -> IO Word64

#else
vkGetBufferOpaqueCaptureAddressSafe ::
                                    VkDevice -- ^ device
                                             -> Ptr VkBufferDeviceAddressInfo -- ^ pInfo
                                                                              -> IO Word64
vkGetBufferOpaqueCaptureAddressSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetBufferOpaqueCaptureAddress)

{-# NOINLINE vkGetBufferOpaqueCaptureAddressSafe #-}
#endif

-- |
-- > uint64_t vkGetBufferOpaqueCaptureAddress
-- >     ( VkDevice device
-- >     , const VkBufferDeviceAddressInfo* pInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetBufferOpaqueCaptureAddress vkGetBufferOpaqueCaptureAddress registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetBufferOpaqueCaptureAddress <- vkGetDeviceProc @VkGetBufferOpaqueCaptureAddress vkDevice
--
-- or less efficient:
--
-- > myGetBufferOpaqueCaptureAddress <- vkGetProc @VkGetBufferOpaqueCaptureAddress
--
-- __Note:__ @vkGetBufferOpaqueCaptureAddressUnsafe@ and @vkGetBufferOpaqueCaptureAddressSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetBufferOpaqueCaptureAddress@ is an alias
--           of @vkGetBufferOpaqueCaptureAddressUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetBufferOpaqueCaptureAddressSafe@.
--
vkGetBufferOpaqueCaptureAddress ::
                                VkDevice -- ^ device
                                         -> Ptr VkBufferDeviceAddressInfo -- ^ pInfo
                                                                          -> IO Word64
#ifdef UNSAFE_FFI_DEFAULT
vkGetBufferOpaqueCaptureAddress
  = vkGetBufferOpaqueCaptureAddressUnsafe
#else
vkGetBufferOpaqueCaptureAddress
  = vkGetBufferOpaqueCaptureAddressSafe

#endif
{-# INLINE vkGetBufferOpaqueCaptureAddress #-}

-- | > uint64_t vkGetBufferOpaqueCaptureAddress
--   >     ( VkDevice device
--   >     , const VkBufferDeviceAddressInfo* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetBufferOpaqueCaptureAddress vkGetBufferOpaqueCaptureAddress registry at www.khronos.org>
type HS_vkGetBufferOpaqueCaptureAddress =
     VkDevice -- ^ device
              -> Ptr VkBufferDeviceAddressInfo -- ^ pInfo
                                               -> IO Word64

type PFN_vkGetBufferOpaqueCaptureAddress =
     FunPtr HS_vkGetBufferOpaqueCaptureAddress

foreign import ccall unsafe "dynamic"
               unwrapVkGetBufferOpaqueCaptureAddressUnsafe ::
               PFN_vkGetBufferOpaqueCaptureAddress ->
                 HS_vkGetBufferOpaqueCaptureAddress

foreign import ccall safe "dynamic"
               unwrapVkGetBufferOpaqueCaptureAddressSafe ::
               PFN_vkGetBufferOpaqueCaptureAddress ->
                 HS_vkGetBufferOpaqueCaptureAddress

instance VulkanProc "vkGetBufferOpaqueCaptureAddress" where
    type VkProcType "vkGetBufferOpaqueCaptureAddress" =
         HS_vkGetBufferOpaqueCaptureAddress
    vkProcSymbol = _VkGetBufferOpaqueCaptureAddress

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetBufferOpaqueCaptureAddressUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetBufferOpaqueCaptureAddressSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetDeviceMemoryOpaqueCaptureAddress :: CString

pattern VkGetDeviceMemoryOpaqueCaptureAddress <-
        (is_VkGetDeviceMemoryOpaqueCaptureAddress -> True)
  where
    VkGetDeviceMemoryOpaqueCaptureAddress
      = _VkGetDeviceMemoryOpaqueCaptureAddress

{-# INLINE _VkGetDeviceMemoryOpaqueCaptureAddress #-}

_VkGetDeviceMemoryOpaqueCaptureAddress :: CString
_VkGetDeviceMemoryOpaqueCaptureAddress
  = Ptr "vkGetDeviceMemoryOpaqueCaptureAddress\NUL"#

{-# INLINE is_VkGetDeviceMemoryOpaqueCaptureAddress #-}

is_VkGetDeviceMemoryOpaqueCaptureAddress :: CString -> Bool
is_VkGetDeviceMemoryOpaqueCaptureAddress
  = (EQ ==) . cmpCStrings _VkGetDeviceMemoryOpaqueCaptureAddress

type VkGetDeviceMemoryOpaqueCaptureAddress =
     "vkGetDeviceMemoryOpaqueCaptureAddress"

-- |
-- > uint64_t vkGetDeviceMemoryOpaqueCaptureAddress
-- >     ( VkDevice device
-- >     , const VkDeviceMemoryOpaqueCaptureAddressInfo* pInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetDeviceMemoryOpaqueCaptureAddress vkGetDeviceMemoryOpaqueCaptureAddress registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceMemoryOpaqueCaptureAddress <- vkGetDeviceProc @VkGetDeviceMemoryOpaqueCaptureAddress vkDevice
--
-- or less efficient:
--
-- > myGetDeviceMemoryOpaqueCaptureAddress <- vkGetProc @VkGetDeviceMemoryOpaqueCaptureAddress
--
-- __Note:__ @vkGetDeviceMemoryOpaqueCaptureAddressUnsafe@ and @vkGetDeviceMemoryOpaqueCaptureAddressSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDeviceMemoryOpaqueCaptureAddress@ is an alias
--           of @vkGetDeviceMemoryOpaqueCaptureAddressUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDeviceMemoryOpaqueCaptureAddressSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall unsafe "vkGetDeviceMemoryOpaqueCaptureAddress"
               vkGetDeviceMemoryOpaqueCaptureAddressUnsafe ::
               VkDevice -- ^ device
                        -> Ptr VkDeviceMemoryOpaqueCaptureAddressInfo -- ^ pInfo
                                                                      -> IO Word64

#else
vkGetDeviceMemoryOpaqueCaptureAddressUnsafe ::
                                            VkDevice -- ^ device
                                                     ->
                                              Ptr VkDeviceMemoryOpaqueCaptureAddressInfo -- ^ pInfo
                                                                                         ->
                                                IO Word64
vkGetDeviceMemoryOpaqueCaptureAddressUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetDeviceMemoryOpaqueCaptureAddress)

{-# NOINLINE vkGetDeviceMemoryOpaqueCaptureAddressUnsafe #-}
#endif

-- |
-- > uint64_t vkGetDeviceMemoryOpaqueCaptureAddress
-- >     ( VkDevice device
-- >     , const VkDeviceMemoryOpaqueCaptureAddressInfo* pInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetDeviceMemoryOpaqueCaptureAddress vkGetDeviceMemoryOpaqueCaptureAddress registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceMemoryOpaqueCaptureAddress <- vkGetDeviceProc @VkGetDeviceMemoryOpaqueCaptureAddress vkDevice
--
-- or less efficient:
--
-- > myGetDeviceMemoryOpaqueCaptureAddress <- vkGetProc @VkGetDeviceMemoryOpaqueCaptureAddress
--
-- __Note:__ @vkGetDeviceMemoryOpaqueCaptureAddressUnsafe@ and @vkGetDeviceMemoryOpaqueCaptureAddressSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDeviceMemoryOpaqueCaptureAddress@ is an alias
--           of @vkGetDeviceMemoryOpaqueCaptureAddressUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDeviceMemoryOpaqueCaptureAddressSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_2
foreign import ccall safe "vkGetDeviceMemoryOpaqueCaptureAddress"
               vkGetDeviceMemoryOpaqueCaptureAddressSafe ::
               VkDevice -- ^ device
                        -> Ptr VkDeviceMemoryOpaqueCaptureAddressInfo -- ^ pInfo
                                                                      -> IO Word64

#else
vkGetDeviceMemoryOpaqueCaptureAddressSafe ::
                                          VkDevice -- ^ device
                                                   ->
                                            Ptr VkDeviceMemoryOpaqueCaptureAddressInfo -- ^ pInfo
                                                                                       -> IO Word64
vkGetDeviceMemoryOpaqueCaptureAddressSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetDeviceMemoryOpaqueCaptureAddress)

{-# NOINLINE vkGetDeviceMemoryOpaqueCaptureAddressSafe #-}
#endif

-- |
-- > uint64_t vkGetDeviceMemoryOpaqueCaptureAddress
-- >     ( VkDevice device
-- >     , const VkDeviceMemoryOpaqueCaptureAddressInfo* pInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetDeviceMemoryOpaqueCaptureAddress vkGetDeviceMemoryOpaqueCaptureAddress registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-2@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceMemoryOpaqueCaptureAddress <- vkGetDeviceProc @VkGetDeviceMemoryOpaqueCaptureAddress vkDevice
--
-- or less efficient:
--
-- > myGetDeviceMemoryOpaqueCaptureAddress <- vkGetProc @VkGetDeviceMemoryOpaqueCaptureAddress
--
-- __Note:__ @vkGetDeviceMemoryOpaqueCaptureAddressUnsafe@ and @vkGetDeviceMemoryOpaqueCaptureAddressSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDeviceMemoryOpaqueCaptureAddress@ is an alias
--           of @vkGetDeviceMemoryOpaqueCaptureAddressUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDeviceMemoryOpaqueCaptureAddressSafe@.
--
vkGetDeviceMemoryOpaqueCaptureAddress ::
                                      VkDevice -- ^ device
                                               ->
                                        Ptr VkDeviceMemoryOpaqueCaptureAddressInfo -- ^ pInfo
                                                                                   -> IO Word64
#ifdef UNSAFE_FFI_DEFAULT
vkGetDeviceMemoryOpaqueCaptureAddress
  = vkGetDeviceMemoryOpaqueCaptureAddressUnsafe
#else
vkGetDeviceMemoryOpaqueCaptureAddress
  = vkGetDeviceMemoryOpaqueCaptureAddressSafe

#endif
{-# INLINE vkGetDeviceMemoryOpaqueCaptureAddress #-}

-- | > uint64_t vkGetDeviceMemoryOpaqueCaptureAddress
--   >     ( VkDevice device
--   >     , const VkDeviceMemoryOpaqueCaptureAddressInfo* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetDeviceMemoryOpaqueCaptureAddress vkGetDeviceMemoryOpaqueCaptureAddress registry at www.khronos.org>
type HS_vkGetDeviceMemoryOpaqueCaptureAddress =
     VkDevice -- ^ device
              -> Ptr VkDeviceMemoryOpaqueCaptureAddressInfo -- ^ pInfo
                                                            -> IO Word64

type PFN_vkGetDeviceMemoryOpaqueCaptureAddress =
     FunPtr HS_vkGetDeviceMemoryOpaqueCaptureAddress

foreign import ccall unsafe "dynamic"
               unwrapVkGetDeviceMemoryOpaqueCaptureAddressUnsafe ::
               PFN_vkGetDeviceMemoryOpaqueCaptureAddress ->
                 HS_vkGetDeviceMemoryOpaqueCaptureAddress

foreign import ccall safe "dynamic"
               unwrapVkGetDeviceMemoryOpaqueCaptureAddressSafe ::
               PFN_vkGetDeviceMemoryOpaqueCaptureAddress ->
                 HS_vkGetDeviceMemoryOpaqueCaptureAddress

instance VulkanProc "vkGetDeviceMemoryOpaqueCaptureAddress" where
    type VkProcType "vkGetDeviceMemoryOpaqueCaptureAddress" =
         HS_vkGetDeviceMemoryOpaqueCaptureAddress
    vkProcSymbol = _VkGetDeviceMemoryOpaqueCaptureAddress

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetDeviceMemoryOpaqueCaptureAddressUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetDeviceMemoryOpaqueCaptureAddressSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES
        = VkStructureType 1000257000

pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO =
        VkStructureType 1000244001

pattern VK_STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO
        = VkStructureType 1000257002

pattern VK_STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO
        = VkStructureType 1000257003

pattern VK_STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO
        = VkStructureType 1000257004

-- | bitpos = @17@
pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT ::
        VkBufferUsageBitmask a

pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT =
        VkBufferUsageBitmask 131072

-- | bitpos = @4@
pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT ::
        VkBufferCreateBitmask a

pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT =
        VkBufferCreateBitmask 16

-- | bitpos = @1@
pattern VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT ::
        VkMemoryAllocateBitmask a

pattern VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT =
        VkMemoryAllocateBitmask 2

-- | bitpos = @2@
pattern VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT ::
        VkMemoryAllocateBitmask a

pattern VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT =
        VkMemoryAllocateBitmask 4

pattern VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS :: VkResult

pattern VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS =
        VkResult (-1000257000)
