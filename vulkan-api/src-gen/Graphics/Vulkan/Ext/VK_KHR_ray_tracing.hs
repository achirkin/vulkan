{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_ray_tracing
       (-- * Vulkan extension: @VK_KHR_ray_tracing@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Koch @dgkoch@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- platform: @provisional@
        --
        -- Extension number: @151@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_get_memory_requirements2', 'VK_EXT_descriptor_indexing', 'VK_KHR_buffer_device_address', 'VK_KHR_deferred_host_operations', 'VK_KHR_pipeline_library'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_get_memory_requirements2', 'VK_EXT_descriptor_indexing', 'VK_KHR_buffer_device_address', 'VK_KHR_deferred_host_operations', 'VK_KHR_pipeline_library'.
        module Graphics.Vulkan.Marshal, VkAabbPositionsKHR,
        VkAccelerationStructureBuildGeometryInfoKHR,
        VkAccelerationStructureBuildOffsetInfoKHR,
        VkAccelerationStructureBuildTypeKHR(..),
        VkAccelerationStructureMemoryRequirementsTypeKHR(..),
        VkAccelerationStructureTypeKHR(..),
        VkAccelerationStructureMemoryRequirementsTypeNV(..),
        VkAccelerationStructureTypeNV(..),
        VkAccelerationStructureCreateGeometryTypeInfoKHR,
        VkAccelerationStructureCreateInfoKHR,
        VkAccelerationStructureDeviceAddressInfoKHR,
        VkAccelerationStructureGeometryAabbsDataKHR,
        VkAccelerationStructureGeometryDataKHR,
        VkAccelerationStructureGeometryInstancesDataKHR,
        VkAccelerationStructureGeometryKHR,
        VkAccelerationStructureGeometryTrianglesDataKHR,
        VkAccelerationStructureInstanceKHR,
        VkAccelerationStructureMemoryRequirementsInfoKHR,
        VkAccelerationStructureVersionKHR,
        VkBindAccelerationStructureMemoryInfoKHR, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkBuildAccelerationStructureBitmaskKHR(..),
        VkBuildAccelerationStructureFlagBitsKHR(),
        VkBuildAccelerationStructureFlagBitsNV(..),
        VkBuildAccelerationStructureFlagsKHR(),
        VkCopyAccelerationStructureInfoKHR,
        VkCopyAccelerationStructureModeKHR(..),
        VkCopyAccelerationStructureModeNV(..),
        VkCopyAccelerationStructureToMemoryInfoKHR,
        VkCopyMemoryToAccelerationStructureInfoKHR, VkDescriptorBufferInfo,
        VkDescriptorImageInfo, VkDescriptorBindingBitmask(..),
        VkDescriptorPoolCreateBitmask(..), VkDescriptorType(..),
        VkDescriptorUpdateTemplateType(..), VkDescriptorBindingFlagBits(),
        VkDescriptorBindingFlagBitsEXT(..), VkDescriptorBindingFlags(),
        VkDescriptorPoolCreateFlagBits(), VkDescriptorPoolCreateFlags(),
        VkDescriptorSetLayoutCreateBitmask(..),
        VkDescriptorSetLayoutCreateFlagBits(),
        VkDescriptorSetLayoutCreateFlags(),
        VkDescriptorUpdateTemplateTypeKHR(..),
        VkAndroidSurfaceCreateFlagsKHR(..), VkBufferViewCreateFlags(..),
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
        VkDeviceOrHostAddressConstKHR, VkDeviceOrHostAddressKHR,
        VkDeviceDiagnosticsConfigBitmaskNV(..), VkDeviceEventTypeEXT(..),
        VkDeviceGroupPresentModeBitmaskKHR(..), VkDeviceCreateFlagBits(..),
        VkDeviceDiagnosticsConfigFlagBitsNV(),
        VkDeviceDiagnosticsConfigFlagsNV(),
        VkDeviceGroupPresentModeFlagBitsKHR(),
        VkDeviceGroupPresentModeFlagsKHR(), VkDeviceQueueCreateBitmask(..),
        VkDeviceQueueCreateFlagBits(), VkDeviceQueueCreateFlags(),
        VkDeviceQueueCreateInfo, VkFormat(..), VkFormatFeatureBitmask(..),
        VkFormatFeatureFlagBits(), VkFormatFeatureFlags(),
        VkGeometryInstanceBitmaskKHR(..), VkGeometryBitmaskKHR(..),
        VkGeometryTypeKHR(..), VkGeometryFlagBitsKHR(),
        VkGeometryFlagBitsNV(..), VkGeometryFlagsKHR(),
        VkGeometryInstanceFlagBitsKHR(), VkGeometryInstanceFlagBitsNV(..),
        VkGeometryInstanceFlagsKHR(), VkGeometryTypeNV(..),
        VkImageAspectBitmask(..), VkImageCreateBitmask(..),
        VkImageLayout(..), VkImageTiling(..), VkImageType(..),
        VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(),
        VkImageViewCreateBitmask(..), VkImageViewCreateFlagBits(),
        VkImageViewCreateFlags(), VkIndexType(..),
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        VkPhysicalDeviceLimits, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2, VkPhysicalDeviceRayTracingFeaturesKHR,
        VkPhysicalDeviceRayTracingPropertiesKHR,
        VkPhysicalDeviceSparseProperties, VkPhysicalDeviceType(..),
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
        VkPipelineStageFlags(), VkPipelineLibraryCreateInfoKHR,
        VkPipelineShaderStageCreateInfo, VkRayTracingPipelineCreateInfoKHR,
        VkRayTracingPipelineInterfaceCreateInfoKHR,
        VkRayTracingShaderGroupCreateInfoKHR,
        VkRayTracingShaderGroupTypeKHR(..),
        VkRayTracingShaderGroupTypeNV(..), VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(),
        VkShaderFloatControlsIndependence(..), VkShaderInfoTypeAMD(..),
        VkShaderStageBitmask(..), VkShaderCorePropertiesBitmaskAMD(..),
        VkShaderCorePropertiesFlagBitsAMD(),
        VkShaderCorePropertiesFlagsAMD(),
        VkShaderFloatControlsIndependenceKHR(..),
        VkShaderModuleCreateBitmask(..), VkShaderModuleCreateFlagBits(),
        VkShaderModuleCreateFlags(), VkShaderStageFlagBits(),
        VkShaderStageFlags(), VkSpecializationInfo,
        VkSpecializationMapEntry, VkStridedBufferRegionKHR,
        VkStructureType(..), VkTraceRaysIndirectCommandKHR,
        VkTransformMatrixKHR, VkWriteDescriptorSet,
        VkWriteDescriptorSetAccelerationStructureKHR,
        -- > #include "vk_platform.h"
        VkCreateAccelerationStructureKHR,
        pattern VkCreateAccelerationStructureKHR,
        HS_vkCreateAccelerationStructureKHR,
        PFN_vkCreateAccelerationStructureKHR,
        VkDestroyAccelerationStructureKHR,
        pattern VkDestroyAccelerationStructureKHR,
        HS_vkDestroyAccelerationStructureKHR,
        PFN_vkDestroyAccelerationStructureKHR,
        VkGetAccelerationStructureMemoryRequirementsKHR,
        pattern VkGetAccelerationStructureMemoryRequirementsKHR,
        HS_vkGetAccelerationStructureMemoryRequirementsKHR,
        PFN_vkGetAccelerationStructureMemoryRequirementsKHR,
        VkBindAccelerationStructureMemoryKHR,
        pattern VkBindAccelerationStructureMemoryKHR,
        HS_vkBindAccelerationStructureMemoryKHR,
        PFN_vkBindAccelerationStructureMemoryKHR,
        VkCmdBuildAccelerationStructureKHR,
        pattern VkCmdBuildAccelerationStructureKHR,
        HS_vkCmdBuildAccelerationStructureKHR,
        PFN_vkCmdBuildAccelerationStructureKHR,
        VkCmdBuildAccelerationStructureIndirectKHR,
        pattern VkCmdBuildAccelerationStructureIndirectKHR,
        HS_vkCmdBuildAccelerationStructureIndirectKHR,
        PFN_vkCmdBuildAccelerationStructureIndirectKHR,
        VkBuildAccelerationStructureKHR,
        pattern VkBuildAccelerationStructureKHR,
        HS_vkBuildAccelerationStructureKHR,
        PFN_vkBuildAccelerationStructureKHR,
        VkCopyAccelerationStructureKHR,
        pattern VkCopyAccelerationStructureKHR,
        HS_vkCopyAccelerationStructureKHR,
        PFN_vkCopyAccelerationStructureKHR,
        VkCopyAccelerationStructureToMemoryKHR,
        pattern VkCopyAccelerationStructureToMemoryKHR,
        HS_vkCopyAccelerationStructureToMemoryKHR,
        PFN_vkCopyAccelerationStructureToMemoryKHR,
        VkCopyMemoryToAccelerationStructureKHR,
        pattern VkCopyMemoryToAccelerationStructureKHR,
        HS_vkCopyMemoryToAccelerationStructureKHR,
        PFN_vkCopyMemoryToAccelerationStructureKHR,
        VkWriteAccelerationStructuresPropertiesKHR,
        pattern VkWriteAccelerationStructuresPropertiesKHR,
        HS_vkWriteAccelerationStructuresPropertiesKHR,
        PFN_vkWriteAccelerationStructuresPropertiesKHR,
        VkCmdCopyAccelerationStructureKHR,
        pattern VkCmdCopyAccelerationStructureKHR,
        HS_vkCmdCopyAccelerationStructureKHR,
        PFN_vkCmdCopyAccelerationStructureKHR,
        VkCmdCopyAccelerationStructureToMemoryKHR,
        pattern VkCmdCopyAccelerationStructureToMemoryKHR,
        HS_vkCmdCopyAccelerationStructureToMemoryKHR,
        PFN_vkCmdCopyAccelerationStructureToMemoryKHR,
        VkCmdCopyMemoryToAccelerationStructureKHR,
        pattern VkCmdCopyMemoryToAccelerationStructureKHR,
        HS_vkCmdCopyMemoryToAccelerationStructureKHR,
        PFN_vkCmdCopyMemoryToAccelerationStructureKHR, VkCmdTraceRaysKHR,
        pattern VkCmdTraceRaysKHR, HS_vkCmdTraceRaysKHR,
        PFN_vkCmdTraceRaysKHR, VkCreateRayTracingPipelinesKHR,
        pattern VkCreateRayTracingPipelinesKHR,
        HS_vkCreateRayTracingPipelinesKHR,
        PFN_vkCreateRayTracingPipelinesKHR,
        VkGetRayTracingShaderGroupHandlesKHR,
        pattern VkGetRayTracingShaderGroupHandlesKHR,
        HS_vkGetRayTracingShaderGroupHandlesKHR,
        PFN_vkGetRayTracingShaderGroupHandlesKHR,
        VkGetAccelerationStructureDeviceAddressKHR,
        pattern VkGetAccelerationStructureDeviceAddressKHR,
        HS_vkGetAccelerationStructureDeviceAddressKHR,
        PFN_vkGetAccelerationStructureDeviceAddressKHR,
        VkGetRayTracingCaptureReplayShaderGroupHandlesKHR,
        pattern VkGetRayTracingCaptureReplayShaderGroupHandlesKHR,
        HS_vkGetRayTracingCaptureReplayShaderGroupHandlesKHR,
        PFN_vkGetRayTracingCaptureReplayShaderGroupHandlesKHR,
        VkCmdWriteAccelerationStructuresPropertiesKHR,
        pattern VkCmdWriteAccelerationStructuresPropertiesKHR,
        HS_vkCmdWriteAccelerationStructuresPropertiesKHR,
        PFN_vkCmdWriteAccelerationStructuresPropertiesKHR,
        VkCmdTraceRaysIndirectKHR, pattern VkCmdTraceRaysIndirectKHR,
        HS_vkCmdTraceRaysIndirectKHR, PFN_vkCmdTraceRaysIndirectKHR,
        VkGetDeviceAccelerationStructureCompatibilityKHR,
        pattern VkGetDeviceAccelerationStructureCompatibilityKHR,
        HS_vkGetDeviceAccelerationStructureCompatibilityKHR,
        PFN_vkGetDeviceAccelerationStructureCompatibilityKHR,
        VkInternalAllocationType(..), VkQueryControlBitmask(..),
        VkQueryPipelineStatisticBitmask(..),
        VkQueryPoolSamplingModeINTEL(..), VkQueryResultBitmask(..),
        VkQueryType(..), VkQueryControlFlagBits(), VkQueryControlFlags(),
        VkQueryPipelineStatisticFlagBits(),
        VkQueryPipelineStatisticFlags(), VkQueryPoolCreateFlagBits(..),
        VkQueryResultFlagBits(), VkQueryResultFlags(), VkResult(..),
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
        PFN_vkVoidFunction, VkAccelerationStructureKHR,
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
        VkAllocationCallbacks, VkDeferredOperationInfoKHR,
        VkMemoryAllocateFlagsInfo, VkMemoryAllocateFlagsInfoKHR,
        VkMemoryAllocateInfo, VkMemoryBarrier,
        VkMemoryDedicatedAllocateInfo, VkMemoryDedicatedAllocateInfoKHR,
        VkMemoryDedicatedRequirements, VkMemoryDedicatedRequirementsKHR,
        VkMemoryFdPropertiesKHR, VkMemoryGetFdInfoKHR, VkMemoryHeap,
        VkMemoryHostPointerPropertiesEXT,
        VkMemoryOpaqueCaptureAddressAllocateInfo,
        VkMemoryOpaqueCaptureAddressAllocateInfoKHR,
        VkMemoryPriorityAllocateInfoEXT, VkMemoryRequirements,
        VkMemoryRequirements2, VkMemoryRequirements2KHR, VkMemoryType,
        VkGraphicsPipelineCreateInfo, VkPipelineCacheCreateInfo,
        VkPipelineColorBlendAdvancedStateCreateInfoEXT,
        VkPipelineColorBlendAttachmentState,
        VkPipelineColorBlendStateCreateInfo,
        VkPipelineCompilerControlCreateInfoAMD,
        VkPipelineCoverageModulationStateCreateInfoNV,
        VkPipelineCoverageReductionStateCreateInfoNV,
        VkPipelineCoverageToColorStateCreateInfoNV,
        VkPipelineCreationFeedbackCreateInfoEXT,
        VkPipelineCreationFeedbackEXT,
        VkPipelineDepthStencilStateCreateInfo,
        VkPipelineDiscardRectangleStateCreateInfoEXT,
        VkPipelineDynamicStateCreateInfo, VkPipelineExecutableInfoKHR,
        VkPipelineExecutableInternalRepresentationKHR,
        VkPipelineExecutablePropertiesKHR,
        VkPipelineExecutableStatisticKHR,
        VkPipelineExecutableStatisticValueKHR, VkPipelineInfoKHR,
        VkPipelineInputAssemblyStateCreateInfo, VkPipelineLayoutCreateInfo,
        VkPipelineMultisampleStateCreateInfo,
        VkPipelineRasterizationConservativeStateCreateInfoEXT,
        VkPipelineRasterizationDepthClipStateCreateInfoEXT,
        VkPipelineRasterizationLineStateCreateInfoEXT,
        VkPipelineRasterizationStateCreateInfo,
        VkPipelineRasterizationStateRasterizationOrderAMD,
        VkPipelineRasterizationStateStreamCreateInfoEXT,
        VkPipelineRepresentativeFragmentTestStateCreateInfoNV,
        VkPipelineSampleLocationsStateCreateInfoEXT,
        VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT,
        VkPipelineTessellationDomainOriginStateCreateInfo,
        VkPipelineTessellationDomainOriginStateCreateInfoKHR,
        VkPipelineTessellationStateCreateInfo,
        VkPipelineVertexInputDivisorStateCreateInfoEXT,
        VkPipelineVertexInputStateCreateInfo,
        VkPipelineViewportCoarseSampleOrderStateCreateInfoNV,
        VkPipelineViewportExclusiveScissorStateCreateInfoNV,
        VkPipelineViewportShadingRateImageStateCreateInfoNV,
        VkPipelineViewportStateCreateInfo,
        VkPipelineViewportSwizzleStateCreateInfoNV,
        VkPipelineViewportWScalingStateCreateInfoNV,
        VK_KHR_RAY_TRACING_SPEC_VERSION,
        pattern VK_KHR_RAY_TRACING_SPEC_VERSION,
        VK_KHR_RAY_TRACING_EXTENSION_NAME,
        pattern VK_KHR_RAY_TRACING_EXTENSION_NAME,
        pattern VK_SHADER_UNUSED_KHR,
        pattern VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR,
        pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_GEOMETRY_TYPE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR,
        pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR,
        pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR,
        pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR,
        pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_KHR,
        pattern VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_FEATURES_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR,
        pattern VK_SHADER_STAGE_RAYGEN_BIT_KHR,
        pattern VK_SHADER_STAGE_ANY_HIT_BIT_KHR,
        pattern VK_SHADER_STAGE_CLOSEST_HIT_BIT_KHR,
        pattern VK_SHADER_STAGE_MISS_BIT_KHR,
        pattern VK_SHADER_STAGE_INTERSECTION_BIT_KHR,
        pattern VK_SHADER_STAGE_CALLABLE_BIT_KHR,
        pattern VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR,
        pattern VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR,
        pattern VK_BUFFER_USAGE_RAY_TRACING_BIT_KHR,
        pattern VK_PIPELINE_BIND_POINT_RAY_TRACING_KHR,
        pattern VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR,
        pattern VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR,
        pattern VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR,
        pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR,
        pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR,
        pattern VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT,
        pattern VK_INDEX_TYPE_NONE_KHR,
        pattern VK_GEOMETRY_TYPE_INSTANCES_KHR,
        pattern VK_ERROR_INCOMPATIBLE_VERSION_KHR,
        pattern VK_FORMAT_FEATURE_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR,
        pattern VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR,
        pattern VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR,
        pattern VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR,
        pattern VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR,
        pattern VK_PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR,
        pattern VK_PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR)
       where
import GHC.Ptr                                                   (Ptr (..))
import Graphics.Vulkan.Constants                                 (pattern VK_SHADER_UNUSED_KHR)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                              (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.AccelerationStructure
import Graphics.Vulkan.Types.Enum.AccessFlags                    (VkAccessBitmask (..))
import Graphics.Vulkan.Types.Enum.Buffer                         (VkBufferUsageBitmask (..))
import Graphics.Vulkan.Types.Enum.BuildAccelerationStructureFlag
import Graphics.Vulkan.Types.Enum.CopyAccelerationStructureMode
import Graphics.Vulkan.Types.Enum.Debug                          (VkDebugReportObjectTypeEXT (..))
import Graphics.Vulkan.Types.Enum.Descriptor
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.Format
import Graphics.Vulkan.Types.Enum.Geometry
import Graphics.Vulkan.Types.Enum.Image
import Graphics.Vulkan.Types.Enum.IndexType
import Graphics.Vulkan.Types.Enum.InternalAllocationType
import Graphics.Vulkan.Types.Enum.ObjectType                     (VkObjectType (..))
import Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import Graphics.Vulkan.Types.Enum.Pipeline
import Graphics.Vulkan.Types.Enum.Query
import Graphics.Vulkan.Types.Enum.RayTracingShaderGroupType
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.Shader
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Enum.SystemAllocationScope
import Graphics.Vulkan.Types.Funcpointers
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.AllocationCallbacks
import Graphics.Vulkan.Types.Struct.Descriptor                   (VkDescriptorBufferInfo,
                                                                  VkDescriptorImageInfo)
import Graphics.Vulkan.Types.Struct.Device                       (VkDeviceCreateInfo,
                                                                  VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.EnableBetaExtensions
import Graphics.Vulkan.Types.Struct.Memory
import Graphics.Vulkan.Types.Struct.PhysicalDevice               (VkPhysicalDeviceFeatures2,
                                                                  VkPhysicalDeviceLimits,
                                                                  VkPhysicalDeviceProperties,
                                                                  VkPhysicalDeviceProperties2,
                                                                  VkPhysicalDeviceSparseProperties)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures       (VkPhysicalDeviceFeatures)
import Graphics.Vulkan.Types.Struct.Pipeline
import Graphics.Vulkan.Types.Struct.Specialization
import Graphics.Vulkan.Types.Struct.WriteDescriptorSet           (VkWriteDescriptorSet)

pattern VkCreateAccelerationStructureKHR :: CString

pattern VkCreateAccelerationStructureKHR <-
        (is_VkCreateAccelerationStructureKHR -> True)
  where
    VkCreateAccelerationStructureKHR
      = _VkCreateAccelerationStructureKHR

{-# INLINE _VkCreateAccelerationStructureKHR #-}

_VkCreateAccelerationStructureKHR :: CString
_VkCreateAccelerationStructureKHR
  = Ptr "vkCreateAccelerationStructureKHR\NUL"#

{-# INLINE is_VkCreateAccelerationStructureKHR #-}

is_VkCreateAccelerationStructureKHR :: CString -> Bool
is_VkCreateAccelerationStructureKHR
  = (EQ ==) . cmpCStrings _VkCreateAccelerationStructureKHR

type VkCreateAccelerationStructureKHR =
     "vkCreateAccelerationStructureKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR'.
--
--   > VkResult vkCreateAccelerationStructureKHR
--   >     ( VkDevice                                           device
--   >     , const VkAccelerationStructureCreateInfoKHR*        pCreateInfo
--   >     , const VkAllocationCallbacks*       pAllocator
--   >     , VkAccelerationStructureKHR*                        pAccelerationStructure
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCreateAccelerationStructureKHR vkCreateAccelerationStructureKHR registry at www.khronos.org>
type HS_vkCreateAccelerationStructureKHR =
     VkDevice -- ^ device
              ->
       Ptr VkAccelerationStructureCreateInfoKHR -- ^ pCreateInfo
                                                ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   ->
           Ptr VkAccelerationStructureKHR -- ^ pAccelerationStructure
                                          -> IO VkResult

type PFN_vkCreateAccelerationStructureKHR =
     FunPtr HS_vkCreateAccelerationStructureKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCreateAccelerationStructureKHRUnsafe ::
               PFN_vkCreateAccelerationStructureKHR ->
                 HS_vkCreateAccelerationStructureKHR

foreign import ccall safe "dynamic"
               unwrapVkCreateAccelerationStructureKHRSafe ::
               PFN_vkCreateAccelerationStructureKHR ->
                 HS_vkCreateAccelerationStructureKHR

instance VulkanProc "vkCreateAccelerationStructureKHR" where
    type VkProcType "vkCreateAccelerationStructureKHR" =
         HS_vkCreateAccelerationStructureKHR
    vkProcSymbol = _VkCreateAccelerationStructureKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCreateAccelerationStructureKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateAccelerationStructureKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkDestroyAccelerationStructureKHR :: CString

pattern VkDestroyAccelerationStructureKHR <-
        (is_VkDestroyAccelerationStructureKHR -> True)
  where
    VkDestroyAccelerationStructureKHR
      = _VkDestroyAccelerationStructureKHR

{-# INLINE _VkDestroyAccelerationStructureKHR #-}

_VkDestroyAccelerationStructureKHR :: CString
_VkDestroyAccelerationStructureKHR
  = Ptr "vkDestroyAccelerationStructureKHR\NUL"#

{-# INLINE is_VkDestroyAccelerationStructureKHR #-}

is_VkDestroyAccelerationStructureKHR :: CString -> Bool
is_VkDestroyAccelerationStructureKHR
  = (EQ ==) . cmpCStrings _VkDestroyAccelerationStructureKHR

type VkDestroyAccelerationStructureKHR =
     "vkDestroyAccelerationStructureKHR"

-- | > void vkDestroyAccelerationStructureKHR
--   >     ( VkDevice device
--   >     , VkAccelerationStructureKHR accelerationStructure
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkDestroyAccelerationStructureKHR vkDestroyAccelerationStructureKHR registry at www.khronos.org>
type HS_vkDestroyAccelerationStructureKHR =
     VkDevice -- ^ device
              ->
       VkAccelerationStructureKHR -- ^ accelerationStructure
                                  -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                               -> IO ()

type PFN_vkDestroyAccelerationStructureKHR =
     FunPtr HS_vkDestroyAccelerationStructureKHR

foreign import ccall unsafe "dynamic"
               unwrapVkDestroyAccelerationStructureKHRUnsafe ::
               PFN_vkDestroyAccelerationStructureKHR ->
                 HS_vkDestroyAccelerationStructureKHR

foreign import ccall safe "dynamic"
               unwrapVkDestroyAccelerationStructureKHRSafe ::
               PFN_vkDestroyAccelerationStructureKHR ->
                 HS_vkDestroyAccelerationStructureKHR

instance VulkanProc "vkDestroyAccelerationStructureKHR" where
    type VkProcType "vkDestroyAccelerationStructureKHR" =
         HS_vkDestroyAccelerationStructureKHR
    vkProcSymbol = _VkDestroyAccelerationStructureKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkDestroyAccelerationStructureKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkDestroyAccelerationStructureKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetAccelerationStructureMemoryRequirementsKHR :: CString

pattern VkGetAccelerationStructureMemoryRequirementsKHR <-
        (is_VkGetAccelerationStructureMemoryRequirementsKHR -> True)
  where
    VkGetAccelerationStructureMemoryRequirementsKHR
      = _VkGetAccelerationStructureMemoryRequirementsKHR

{-# INLINE _VkGetAccelerationStructureMemoryRequirementsKHR #-}

_VkGetAccelerationStructureMemoryRequirementsKHR :: CString
_VkGetAccelerationStructureMemoryRequirementsKHR
  = Ptr "vkGetAccelerationStructureMemoryRequirementsKHR\NUL"#

{-# INLINE is_VkGetAccelerationStructureMemoryRequirementsKHR #-}

is_VkGetAccelerationStructureMemoryRequirementsKHR ::
                                                   CString -> Bool
is_VkGetAccelerationStructureMemoryRequirementsKHR
  = (EQ ==) .
      cmpCStrings _VkGetAccelerationStructureMemoryRequirementsKHR

type VkGetAccelerationStructureMemoryRequirementsKHR =
     "vkGetAccelerationStructureMemoryRequirementsKHR"

-- | > void vkGetAccelerationStructureMemoryRequirementsKHR
--   >     ( VkDevice device
--   >     , const VkAccelerationStructureMemoryRequirementsInfoKHR* pInfo
--   >     , VkMemoryRequirements2* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetAccelerationStructureMemoryRequirementsKHR vkGetAccelerationStructureMemoryRequirementsKHR registry at www.khronos.org>
type HS_vkGetAccelerationStructureMemoryRequirementsKHR =
     VkDevice -- ^ device
              ->
       Ptr VkAccelerationStructureMemoryRequirementsInfoKHR -- ^ pInfo
                                                            ->
         Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                   -> IO ()

type PFN_vkGetAccelerationStructureMemoryRequirementsKHR =
     FunPtr HS_vkGetAccelerationStructureMemoryRequirementsKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetAccelerationStructureMemoryRequirementsKHRUnsafe ::
               PFN_vkGetAccelerationStructureMemoryRequirementsKHR ->
                 HS_vkGetAccelerationStructureMemoryRequirementsKHR

foreign import ccall safe "dynamic"
               unwrapVkGetAccelerationStructureMemoryRequirementsKHRSafe ::
               PFN_vkGetAccelerationStructureMemoryRequirementsKHR ->
                 HS_vkGetAccelerationStructureMemoryRequirementsKHR

instance VulkanProc
           "vkGetAccelerationStructureMemoryRequirementsKHR"
         where
    type VkProcType "vkGetAccelerationStructureMemoryRequirementsKHR" =
         HS_vkGetAccelerationStructureMemoryRequirementsKHR
    vkProcSymbol = _VkGetAccelerationStructureMemoryRequirementsKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetAccelerationStructureMemoryRequirementsKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetAccelerationStructureMemoryRequirementsKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkBindAccelerationStructureMemoryKHR :: CString

pattern VkBindAccelerationStructureMemoryKHR <-
        (is_VkBindAccelerationStructureMemoryKHR -> True)
  where
    VkBindAccelerationStructureMemoryKHR
      = _VkBindAccelerationStructureMemoryKHR

{-# INLINE _VkBindAccelerationStructureMemoryKHR #-}

_VkBindAccelerationStructureMemoryKHR :: CString
_VkBindAccelerationStructureMemoryKHR
  = Ptr "vkBindAccelerationStructureMemoryKHR\NUL"#

{-# INLINE is_VkBindAccelerationStructureMemoryKHR #-}

is_VkBindAccelerationStructureMemoryKHR :: CString -> Bool
is_VkBindAccelerationStructureMemoryKHR
  = (EQ ==) . cmpCStrings _VkBindAccelerationStructureMemoryKHR

type VkBindAccelerationStructureMemoryKHR =
     "vkBindAccelerationStructureMemoryKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindAccelerationStructureMemoryKHR
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindAccelerationStructureMemoryInfoKHR* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkBindAccelerationStructureMemoryKHR vkBindAccelerationStructureMemoryKHR registry at www.khronos.org>
type HS_vkBindAccelerationStructureMemoryKHR =
     VkDevice -- ^ device
              ->
       Word32 -- ^ bindInfoCount
              ->
         Ptr VkBindAccelerationStructureMemoryInfoKHR -- ^ pBindInfos
                                                      -> IO VkResult

type PFN_vkBindAccelerationStructureMemoryKHR =
     FunPtr HS_vkBindAccelerationStructureMemoryKHR

foreign import ccall unsafe "dynamic"
               unwrapVkBindAccelerationStructureMemoryKHRUnsafe ::
               PFN_vkBindAccelerationStructureMemoryKHR ->
                 HS_vkBindAccelerationStructureMemoryKHR

foreign import ccall safe "dynamic"
               unwrapVkBindAccelerationStructureMemoryKHRSafe ::
               PFN_vkBindAccelerationStructureMemoryKHR ->
                 HS_vkBindAccelerationStructureMemoryKHR

instance VulkanProc "vkBindAccelerationStructureMemoryKHR" where
    type VkProcType "vkBindAccelerationStructureMemoryKHR" =
         HS_vkBindAccelerationStructureMemoryKHR
    vkProcSymbol = _VkBindAccelerationStructureMemoryKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkBindAccelerationStructureMemoryKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkBindAccelerationStructureMemoryKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdBuildAccelerationStructureKHR :: CString

pattern VkCmdBuildAccelerationStructureKHR <-
        (is_VkCmdBuildAccelerationStructureKHR -> True)
  where
    VkCmdBuildAccelerationStructureKHR
      = _VkCmdBuildAccelerationStructureKHR

{-# INLINE _VkCmdBuildAccelerationStructureKHR #-}

_VkCmdBuildAccelerationStructureKHR :: CString
_VkCmdBuildAccelerationStructureKHR
  = Ptr "vkCmdBuildAccelerationStructureKHR\NUL"#

{-# INLINE is_VkCmdBuildAccelerationStructureKHR #-}

is_VkCmdBuildAccelerationStructureKHR :: CString -> Bool
is_VkCmdBuildAccelerationStructureKHR
  = (EQ ==) . cmpCStrings _VkCmdBuildAccelerationStructureKHR

type VkCmdBuildAccelerationStructureKHR =
     "vkCmdBuildAccelerationStructureKHR"

-- | Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdBuildAccelerationStructureKHR
--   >     ( VkCommandBuffer                                    commandBuffer
--   >     , uint32_t infoCount
--   >     , const VkAccelerationStructureBuildGeometryInfoKHR* pInfos
--   >     , const VkAccelerationStructureBuildOffsetInfoKHR* const* ppOffsetInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBuildAccelerationStructureKHR vkCmdBuildAccelerationStructureKHR registry at www.khronos.org>
type HS_vkCmdBuildAccelerationStructureKHR =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ infoCount
              ->
         Ptr VkAccelerationStructureBuildGeometryInfoKHR -- ^ pInfos
                                                         ->
           Ptr (Ptr VkAccelerationStructureBuildOffsetInfoKHR) -- ^ ppOffsetInfos
                                                               -> IO ()

type PFN_vkCmdBuildAccelerationStructureKHR =
     FunPtr HS_vkCmdBuildAccelerationStructureKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCmdBuildAccelerationStructureKHRUnsafe ::
               PFN_vkCmdBuildAccelerationStructureKHR ->
                 HS_vkCmdBuildAccelerationStructureKHR

foreign import ccall safe "dynamic"
               unwrapVkCmdBuildAccelerationStructureKHRSafe ::
               PFN_vkCmdBuildAccelerationStructureKHR ->
                 HS_vkCmdBuildAccelerationStructureKHR

instance VulkanProc "vkCmdBuildAccelerationStructureKHR" where
    type VkProcType "vkCmdBuildAccelerationStructureKHR" =
         HS_vkCmdBuildAccelerationStructureKHR
    vkProcSymbol = _VkCmdBuildAccelerationStructureKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdBuildAccelerationStructureKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdBuildAccelerationStructureKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdBuildAccelerationStructureIndirectKHR :: CString

pattern VkCmdBuildAccelerationStructureIndirectKHR <-
        (is_VkCmdBuildAccelerationStructureIndirectKHR -> True)
  where
    VkCmdBuildAccelerationStructureIndirectKHR
      = _VkCmdBuildAccelerationStructureIndirectKHR

{-# INLINE _VkCmdBuildAccelerationStructureIndirectKHR #-}

_VkCmdBuildAccelerationStructureIndirectKHR :: CString
_VkCmdBuildAccelerationStructureIndirectKHR
  = Ptr "vkCmdBuildAccelerationStructureIndirectKHR\NUL"#

{-# INLINE is_VkCmdBuildAccelerationStructureIndirectKHR #-}

is_VkCmdBuildAccelerationStructureIndirectKHR :: CString -> Bool
is_VkCmdBuildAccelerationStructureIndirectKHR
  = (EQ ==) . cmpCStrings _VkCmdBuildAccelerationStructureIndirectKHR

type VkCmdBuildAccelerationStructureIndirectKHR =
     "vkCmdBuildAccelerationStructureIndirectKHR"

-- | Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdBuildAccelerationStructureIndirectKHR
--   >     ( VkCommandBuffer                  commandBuffer
--   >     , const VkAccelerationStructureBuildGeometryInfoKHR* pInfo
--   >     , VkBuffer                                           indirectBuffer
--   >     , VkDeviceSize                                       indirectOffset
--   >     , uint32_t                                           indirectStride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBuildAccelerationStructureIndirectKHR vkCmdBuildAccelerationStructureIndirectKHR registry at www.khronos.org>
type HS_vkCmdBuildAccelerationStructureIndirectKHR =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Ptr VkAccelerationStructureBuildGeometryInfoKHR -- ^ pInfo
                                                       ->
         VkBuffer -- ^ indirectBuffer
                  -> VkDeviceSize -- ^ indirectOffset
                                  -> Word32 -- ^ indirectStride
                                            -> IO ()

type PFN_vkCmdBuildAccelerationStructureIndirectKHR =
     FunPtr HS_vkCmdBuildAccelerationStructureIndirectKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCmdBuildAccelerationStructureIndirectKHRUnsafe ::
               PFN_vkCmdBuildAccelerationStructureIndirectKHR ->
                 HS_vkCmdBuildAccelerationStructureIndirectKHR

foreign import ccall safe "dynamic"
               unwrapVkCmdBuildAccelerationStructureIndirectKHRSafe ::
               PFN_vkCmdBuildAccelerationStructureIndirectKHR ->
                 HS_vkCmdBuildAccelerationStructureIndirectKHR

instance VulkanProc "vkCmdBuildAccelerationStructureIndirectKHR"
         where
    type VkProcType "vkCmdBuildAccelerationStructureIndirectKHR" =
         HS_vkCmdBuildAccelerationStructureIndirectKHR
    vkProcSymbol = _VkCmdBuildAccelerationStructureIndirectKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdBuildAccelerationStructureIndirectKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkCmdBuildAccelerationStructureIndirectKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkBuildAccelerationStructureKHR :: CString

pattern VkBuildAccelerationStructureKHR <-
        (is_VkBuildAccelerationStructureKHR -> True)
  where
    VkBuildAccelerationStructureKHR = _VkBuildAccelerationStructureKHR

{-# INLINE _VkBuildAccelerationStructureKHR #-}

_VkBuildAccelerationStructureKHR :: CString
_VkBuildAccelerationStructureKHR
  = Ptr "vkBuildAccelerationStructureKHR\NUL"#

{-# INLINE is_VkBuildAccelerationStructureKHR #-}

is_VkBuildAccelerationStructureKHR :: CString -> Bool
is_VkBuildAccelerationStructureKHR
  = (EQ ==) . cmpCStrings _VkBuildAccelerationStructureKHR

type VkBuildAccelerationStructureKHR =
     "vkBuildAccelerationStructureKHR"

-- | Success codes: 'VK_SUCCESS', 'VK_OPERATION_DEFERRED_KHR', 'VK_OPERATION_NOT_DEFERRED_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBuildAccelerationStructureKHR
--   >     ( VkDevice                                           device
--   >     , uint32_t infoCount
--   >     , const VkAccelerationStructureBuildGeometryInfoKHR* pInfos
--   >     , const VkAccelerationStructureBuildOffsetInfoKHR* const* ppOffsetInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkBuildAccelerationStructureKHR vkBuildAccelerationStructureKHR registry at www.khronos.org>
type HS_vkBuildAccelerationStructureKHR =
     VkDevice -- ^ device
              ->
       Word32 -- ^ infoCount
              ->
         Ptr VkAccelerationStructureBuildGeometryInfoKHR -- ^ pInfos
                                                         ->
           Ptr (Ptr VkAccelerationStructureBuildOffsetInfoKHR) -- ^ ppOffsetInfos
                                                               -> IO VkResult

type PFN_vkBuildAccelerationStructureKHR =
     FunPtr HS_vkBuildAccelerationStructureKHR

foreign import ccall unsafe "dynamic"
               unwrapVkBuildAccelerationStructureKHRUnsafe ::
               PFN_vkBuildAccelerationStructureKHR ->
                 HS_vkBuildAccelerationStructureKHR

foreign import ccall safe "dynamic"
               unwrapVkBuildAccelerationStructureKHRSafe ::
               PFN_vkBuildAccelerationStructureKHR ->
                 HS_vkBuildAccelerationStructureKHR

instance VulkanProc "vkBuildAccelerationStructureKHR" where
    type VkProcType "vkBuildAccelerationStructureKHR" =
         HS_vkBuildAccelerationStructureKHR
    vkProcSymbol = _VkBuildAccelerationStructureKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkBuildAccelerationStructureKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkBuildAccelerationStructureKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCopyAccelerationStructureKHR :: CString

pattern VkCopyAccelerationStructureKHR <-
        (is_VkCopyAccelerationStructureKHR -> True)
  where
    VkCopyAccelerationStructureKHR = _VkCopyAccelerationStructureKHR

{-# INLINE _VkCopyAccelerationStructureKHR #-}

_VkCopyAccelerationStructureKHR :: CString
_VkCopyAccelerationStructureKHR
  = Ptr "vkCopyAccelerationStructureKHR\NUL"#

{-# INLINE is_VkCopyAccelerationStructureKHR #-}

is_VkCopyAccelerationStructureKHR :: CString -> Bool
is_VkCopyAccelerationStructureKHR
  = (EQ ==) . cmpCStrings _VkCopyAccelerationStructureKHR

type VkCopyAccelerationStructureKHR =
     "vkCopyAccelerationStructureKHR"

-- | Success codes: 'VK_SUCCESS', 'VK_OPERATION_DEFERRED_KHR', 'VK_OPERATION_NOT_DEFERRED_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCopyAccelerationStructureKHR
--   >     ( VkDevice device
--   >     , const VkCopyAccelerationStructureInfoKHR* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCopyAccelerationStructureKHR vkCopyAccelerationStructureKHR registry at www.khronos.org>
type HS_vkCopyAccelerationStructureKHR =
     VkDevice -- ^ device
              -> Ptr VkCopyAccelerationStructureInfoKHR -- ^ pInfo
                                                        -> IO VkResult

type PFN_vkCopyAccelerationStructureKHR =
     FunPtr HS_vkCopyAccelerationStructureKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCopyAccelerationStructureKHRUnsafe ::
               PFN_vkCopyAccelerationStructureKHR ->
                 HS_vkCopyAccelerationStructureKHR

foreign import ccall safe "dynamic"
               unwrapVkCopyAccelerationStructureKHRSafe ::
               PFN_vkCopyAccelerationStructureKHR ->
                 HS_vkCopyAccelerationStructureKHR

instance VulkanProc "vkCopyAccelerationStructureKHR" where
    type VkProcType "vkCopyAccelerationStructureKHR" =
         HS_vkCopyAccelerationStructureKHR
    vkProcSymbol = _VkCopyAccelerationStructureKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCopyAccelerationStructureKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCopyAccelerationStructureKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCopyAccelerationStructureToMemoryKHR :: CString

pattern VkCopyAccelerationStructureToMemoryKHR <-
        (is_VkCopyAccelerationStructureToMemoryKHR -> True)
  where
    VkCopyAccelerationStructureToMemoryKHR
      = _VkCopyAccelerationStructureToMemoryKHR

{-# INLINE _VkCopyAccelerationStructureToMemoryKHR #-}

_VkCopyAccelerationStructureToMemoryKHR :: CString
_VkCopyAccelerationStructureToMemoryKHR
  = Ptr "vkCopyAccelerationStructureToMemoryKHR\NUL"#

{-# INLINE is_VkCopyAccelerationStructureToMemoryKHR #-}

is_VkCopyAccelerationStructureToMemoryKHR :: CString -> Bool
is_VkCopyAccelerationStructureToMemoryKHR
  = (EQ ==) . cmpCStrings _VkCopyAccelerationStructureToMemoryKHR

type VkCopyAccelerationStructureToMemoryKHR =
     "vkCopyAccelerationStructureToMemoryKHR"

-- | Success codes: 'VK_SUCCESS', 'VK_OPERATION_DEFERRED_KHR', 'VK_OPERATION_NOT_DEFERRED_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCopyAccelerationStructureToMemoryKHR
--   >     ( VkDevice device
--   >     , const VkCopyAccelerationStructureToMemoryInfoKHR* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCopyAccelerationStructureToMemoryKHR vkCopyAccelerationStructureToMemoryKHR registry at www.khronos.org>
type HS_vkCopyAccelerationStructureToMemoryKHR =
     VkDevice -- ^ device
              ->
       Ptr VkCopyAccelerationStructureToMemoryInfoKHR -- ^ pInfo
                                                      -> IO VkResult

type PFN_vkCopyAccelerationStructureToMemoryKHR =
     FunPtr HS_vkCopyAccelerationStructureToMemoryKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCopyAccelerationStructureToMemoryKHRUnsafe ::
               PFN_vkCopyAccelerationStructureToMemoryKHR ->
                 HS_vkCopyAccelerationStructureToMemoryKHR

foreign import ccall safe "dynamic"
               unwrapVkCopyAccelerationStructureToMemoryKHRSafe ::
               PFN_vkCopyAccelerationStructureToMemoryKHR ->
                 HS_vkCopyAccelerationStructureToMemoryKHR

instance VulkanProc "vkCopyAccelerationStructureToMemoryKHR" where
    type VkProcType "vkCopyAccelerationStructureToMemoryKHR" =
         HS_vkCopyAccelerationStructureToMemoryKHR
    vkProcSymbol = _VkCopyAccelerationStructureToMemoryKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCopyAccelerationStructureToMemoryKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkCopyAccelerationStructureToMemoryKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCopyMemoryToAccelerationStructureKHR :: CString

pattern VkCopyMemoryToAccelerationStructureKHR <-
        (is_VkCopyMemoryToAccelerationStructureKHR -> True)
  where
    VkCopyMemoryToAccelerationStructureKHR
      = _VkCopyMemoryToAccelerationStructureKHR

{-# INLINE _VkCopyMemoryToAccelerationStructureKHR #-}

_VkCopyMemoryToAccelerationStructureKHR :: CString
_VkCopyMemoryToAccelerationStructureKHR
  = Ptr "vkCopyMemoryToAccelerationStructureKHR\NUL"#

{-# INLINE is_VkCopyMemoryToAccelerationStructureKHR #-}

is_VkCopyMemoryToAccelerationStructureKHR :: CString -> Bool
is_VkCopyMemoryToAccelerationStructureKHR
  = (EQ ==) . cmpCStrings _VkCopyMemoryToAccelerationStructureKHR

type VkCopyMemoryToAccelerationStructureKHR =
     "vkCopyMemoryToAccelerationStructureKHR"

-- | Success codes: 'VK_SUCCESS', 'VK_OPERATION_DEFERRED_KHR', 'VK_OPERATION_NOT_DEFERRED_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCopyMemoryToAccelerationStructureKHR
--   >     ( VkDevice device
--   >     , const VkCopyMemoryToAccelerationStructureInfoKHR* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCopyMemoryToAccelerationStructureKHR vkCopyMemoryToAccelerationStructureKHR registry at www.khronos.org>
type HS_vkCopyMemoryToAccelerationStructureKHR =
     VkDevice -- ^ device
              ->
       Ptr VkCopyMemoryToAccelerationStructureInfoKHR -- ^ pInfo
                                                      -> IO VkResult

type PFN_vkCopyMemoryToAccelerationStructureKHR =
     FunPtr HS_vkCopyMemoryToAccelerationStructureKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCopyMemoryToAccelerationStructureKHRUnsafe ::
               PFN_vkCopyMemoryToAccelerationStructureKHR ->
                 HS_vkCopyMemoryToAccelerationStructureKHR

foreign import ccall safe "dynamic"
               unwrapVkCopyMemoryToAccelerationStructureKHRSafe ::
               PFN_vkCopyMemoryToAccelerationStructureKHR ->
                 HS_vkCopyMemoryToAccelerationStructureKHR

instance VulkanProc "vkCopyMemoryToAccelerationStructureKHR" where
    type VkProcType "vkCopyMemoryToAccelerationStructureKHR" =
         HS_vkCopyMemoryToAccelerationStructureKHR
    vkProcSymbol = _VkCopyMemoryToAccelerationStructureKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCopyMemoryToAccelerationStructureKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkCopyMemoryToAccelerationStructureKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkWriteAccelerationStructuresPropertiesKHR :: CString

pattern VkWriteAccelerationStructuresPropertiesKHR <-
        (is_VkWriteAccelerationStructuresPropertiesKHR -> True)
  where
    VkWriteAccelerationStructuresPropertiesKHR
      = _VkWriteAccelerationStructuresPropertiesKHR

{-# INLINE _VkWriteAccelerationStructuresPropertiesKHR #-}

_VkWriteAccelerationStructuresPropertiesKHR :: CString
_VkWriteAccelerationStructuresPropertiesKHR
  = Ptr "vkWriteAccelerationStructuresPropertiesKHR\NUL"#

{-# INLINE is_VkWriteAccelerationStructuresPropertiesKHR #-}

is_VkWriteAccelerationStructuresPropertiesKHR :: CString -> Bool
is_VkWriteAccelerationStructuresPropertiesKHR
  = (EQ ==) . cmpCStrings _VkWriteAccelerationStructuresPropertiesKHR

type VkWriteAccelerationStructuresPropertiesKHR =
     "vkWriteAccelerationStructuresPropertiesKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkWriteAccelerationStructuresPropertiesKHR
--   >     ( VkDevice device
--   >     , uint32_t accelerationStructureCount
--   >     , const VkAccelerationStructureKHR* pAccelerationStructures
--   >     , VkQueryType  queryType
--   >     , size_t       dataSize
--   >     , void* pData
--   >     , size_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkWriteAccelerationStructuresPropertiesKHR vkWriteAccelerationStructuresPropertiesKHR registry at www.khronos.org>
type HS_vkWriteAccelerationStructuresPropertiesKHR =
     VkDevice -- ^ device
              ->
       Word32 -- ^ accelerationStructureCount
              ->
         Ptr VkAccelerationStructureKHR -- ^ pAccelerationStructures
                                        ->
           VkQueryType -- ^ queryType
                       -> CSize -- ^ dataSize
                                -> Ptr Void -- ^ pData
                                            -> CSize -- ^ stride
                                                     -> IO VkResult

type PFN_vkWriteAccelerationStructuresPropertiesKHR =
     FunPtr HS_vkWriteAccelerationStructuresPropertiesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkWriteAccelerationStructuresPropertiesKHRUnsafe ::
               PFN_vkWriteAccelerationStructuresPropertiesKHR ->
                 HS_vkWriteAccelerationStructuresPropertiesKHR

foreign import ccall safe "dynamic"
               unwrapVkWriteAccelerationStructuresPropertiesKHRSafe ::
               PFN_vkWriteAccelerationStructuresPropertiesKHR ->
                 HS_vkWriteAccelerationStructuresPropertiesKHR

instance VulkanProc "vkWriteAccelerationStructuresPropertiesKHR"
         where
    type VkProcType "vkWriteAccelerationStructuresPropertiesKHR" =
         HS_vkWriteAccelerationStructuresPropertiesKHR
    vkProcSymbol = _VkWriteAccelerationStructuresPropertiesKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkWriteAccelerationStructuresPropertiesKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkWriteAccelerationStructuresPropertiesKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdCopyAccelerationStructureKHR :: CString

pattern VkCmdCopyAccelerationStructureKHR <-
        (is_VkCmdCopyAccelerationStructureKHR -> True)
  where
    VkCmdCopyAccelerationStructureKHR
      = _VkCmdCopyAccelerationStructureKHR

{-# INLINE _VkCmdCopyAccelerationStructureKHR #-}

_VkCmdCopyAccelerationStructureKHR :: CString
_VkCmdCopyAccelerationStructureKHR
  = Ptr "vkCmdCopyAccelerationStructureKHR\NUL"#

{-# INLINE is_VkCmdCopyAccelerationStructureKHR #-}

is_VkCmdCopyAccelerationStructureKHR :: CString -> Bool
is_VkCmdCopyAccelerationStructureKHR
  = (EQ ==) . cmpCStrings _VkCmdCopyAccelerationStructureKHR

type VkCmdCopyAccelerationStructureKHR =
     "vkCmdCopyAccelerationStructureKHR"

-- | Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdCopyAccelerationStructureKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCopyAccelerationStructureInfoKHR* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdCopyAccelerationStructureKHR vkCmdCopyAccelerationStructureKHR registry at www.khronos.org>
type HS_vkCmdCopyAccelerationStructureKHR =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr VkCopyAccelerationStructureInfoKHR -- ^ pInfo
                                                               -> IO ()

type PFN_vkCmdCopyAccelerationStructureKHR =
     FunPtr HS_vkCmdCopyAccelerationStructureKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCmdCopyAccelerationStructureKHRUnsafe ::
               PFN_vkCmdCopyAccelerationStructureKHR ->
                 HS_vkCmdCopyAccelerationStructureKHR

foreign import ccall safe "dynamic"
               unwrapVkCmdCopyAccelerationStructureKHRSafe ::
               PFN_vkCmdCopyAccelerationStructureKHR ->
                 HS_vkCmdCopyAccelerationStructureKHR

instance VulkanProc "vkCmdCopyAccelerationStructureKHR" where
    type VkProcType "vkCmdCopyAccelerationStructureKHR" =
         HS_vkCmdCopyAccelerationStructureKHR
    vkProcSymbol = _VkCmdCopyAccelerationStructureKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdCopyAccelerationStructureKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdCopyAccelerationStructureKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdCopyAccelerationStructureToMemoryKHR :: CString

pattern VkCmdCopyAccelerationStructureToMemoryKHR <-
        (is_VkCmdCopyAccelerationStructureToMemoryKHR -> True)
  where
    VkCmdCopyAccelerationStructureToMemoryKHR
      = _VkCmdCopyAccelerationStructureToMemoryKHR

{-# INLINE _VkCmdCopyAccelerationStructureToMemoryKHR #-}

_VkCmdCopyAccelerationStructureToMemoryKHR :: CString
_VkCmdCopyAccelerationStructureToMemoryKHR
  = Ptr "vkCmdCopyAccelerationStructureToMemoryKHR\NUL"#

{-# INLINE is_VkCmdCopyAccelerationStructureToMemoryKHR #-}

is_VkCmdCopyAccelerationStructureToMemoryKHR :: CString -> Bool
is_VkCmdCopyAccelerationStructureToMemoryKHR
  = (EQ ==) . cmpCStrings _VkCmdCopyAccelerationStructureToMemoryKHR

type VkCmdCopyAccelerationStructureToMemoryKHR =
     "vkCmdCopyAccelerationStructureToMemoryKHR"

-- | Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdCopyAccelerationStructureToMemoryKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCopyAccelerationStructureToMemoryInfoKHR* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdCopyAccelerationStructureToMemoryKHR vkCmdCopyAccelerationStructureToMemoryKHR registry at www.khronos.org>
type HS_vkCmdCopyAccelerationStructureToMemoryKHR =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Ptr VkCopyAccelerationStructureToMemoryInfoKHR -- ^ pInfo
                                                      -> IO ()

type PFN_vkCmdCopyAccelerationStructureToMemoryKHR =
     FunPtr HS_vkCmdCopyAccelerationStructureToMemoryKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCmdCopyAccelerationStructureToMemoryKHRUnsafe ::
               PFN_vkCmdCopyAccelerationStructureToMemoryKHR ->
                 HS_vkCmdCopyAccelerationStructureToMemoryKHR

foreign import ccall safe "dynamic"
               unwrapVkCmdCopyAccelerationStructureToMemoryKHRSafe ::
               PFN_vkCmdCopyAccelerationStructureToMemoryKHR ->
                 HS_vkCmdCopyAccelerationStructureToMemoryKHR

instance VulkanProc "vkCmdCopyAccelerationStructureToMemoryKHR"
         where
    type VkProcType "vkCmdCopyAccelerationStructureToMemoryKHR" =
         HS_vkCmdCopyAccelerationStructureToMemoryKHR
    vkProcSymbol = _VkCmdCopyAccelerationStructureToMemoryKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdCopyAccelerationStructureToMemoryKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkCmdCopyAccelerationStructureToMemoryKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdCopyMemoryToAccelerationStructureKHR :: CString

pattern VkCmdCopyMemoryToAccelerationStructureKHR <-
        (is_VkCmdCopyMemoryToAccelerationStructureKHR -> True)
  where
    VkCmdCopyMemoryToAccelerationStructureKHR
      = _VkCmdCopyMemoryToAccelerationStructureKHR

{-# INLINE _VkCmdCopyMemoryToAccelerationStructureKHR #-}

_VkCmdCopyMemoryToAccelerationStructureKHR :: CString
_VkCmdCopyMemoryToAccelerationStructureKHR
  = Ptr "vkCmdCopyMemoryToAccelerationStructureKHR\NUL"#

{-# INLINE is_VkCmdCopyMemoryToAccelerationStructureKHR #-}

is_VkCmdCopyMemoryToAccelerationStructureKHR :: CString -> Bool
is_VkCmdCopyMemoryToAccelerationStructureKHR
  = (EQ ==) . cmpCStrings _VkCmdCopyMemoryToAccelerationStructureKHR

type VkCmdCopyMemoryToAccelerationStructureKHR =
     "vkCmdCopyMemoryToAccelerationStructureKHR"

-- | Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdCopyMemoryToAccelerationStructureKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCopyMemoryToAccelerationStructureInfoKHR* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdCopyMemoryToAccelerationStructureKHR vkCmdCopyMemoryToAccelerationStructureKHR registry at www.khronos.org>
type HS_vkCmdCopyMemoryToAccelerationStructureKHR =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Ptr VkCopyMemoryToAccelerationStructureInfoKHR -- ^ pInfo
                                                      -> IO ()

type PFN_vkCmdCopyMemoryToAccelerationStructureKHR =
     FunPtr HS_vkCmdCopyMemoryToAccelerationStructureKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCmdCopyMemoryToAccelerationStructureKHRUnsafe ::
               PFN_vkCmdCopyMemoryToAccelerationStructureKHR ->
                 HS_vkCmdCopyMemoryToAccelerationStructureKHR

foreign import ccall safe "dynamic"
               unwrapVkCmdCopyMemoryToAccelerationStructureKHRSafe ::
               PFN_vkCmdCopyMemoryToAccelerationStructureKHR ->
                 HS_vkCmdCopyMemoryToAccelerationStructureKHR

instance VulkanProc "vkCmdCopyMemoryToAccelerationStructureKHR"
         where
    type VkProcType "vkCmdCopyMemoryToAccelerationStructureKHR" =
         HS_vkCmdCopyMemoryToAccelerationStructureKHR
    vkProcSymbol = _VkCmdCopyMemoryToAccelerationStructureKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdCopyMemoryToAccelerationStructureKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkCmdCopyMemoryToAccelerationStructureKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdTraceRaysKHR :: CString

pattern VkCmdTraceRaysKHR <- (is_VkCmdTraceRaysKHR -> True)
  where
    VkCmdTraceRaysKHR = _VkCmdTraceRaysKHR

{-# INLINE _VkCmdTraceRaysKHR #-}

_VkCmdTraceRaysKHR :: CString
_VkCmdTraceRaysKHR = Ptr "vkCmdTraceRaysKHR\NUL"#

{-# INLINE is_VkCmdTraceRaysKHR #-}

is_VkCmdTraceRaysKHR :: CString -> Bool
is_VkCmdTraceRaysKHR = (EQ ==) . cmpCStrings _VkCmdTraceRaysKHR

type VkCmdTraceRaysKHR = "vkCmdTraceRaysKHR"

-- | Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdTraceRaysKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkStridedBufferRegionKHR* pRaygenShaderBindingTable
--   >     , const VkStridedBufferRegionKHR* pMissShaderBindingTable
--   >     , const VkStridedBufferRegionKHR* pHitShaderBindingTable
--   >     , const VkStridedBufferRegionKHR* pCallableShaderBindingTable
--   >     , uint32_t width
--   >     , uint32_t height
--   >     , uint32_t depth
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdTraceRaysKHR vkCmdTraceRaysKHR registry at www.khronos.org>
type HS_vkCmdTraceRaysKHR =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Ptr VkStridedBufferRegionKHR -- ^ pRaygenShaderBindingTable
                                    ->
         Ptr VkStridedBufferRegionKHR -- ^ pMissShaderBindingTable
                                      ->
           Ptr VkStridedBufferRegionKHR -- ^ pHitShaderBindingTable
                                        ->
             Ptr VkStridedBufferRegionKHR -- ^ pCallableShaderBindingTable
                                          -> Word32 -- ^ width
                                                    -> Word32 -- ^ height
                                                              -> Word32 -- ^ depth
                                                                        -> IO ()

type PFN_vkCmdTraceRaysKHR = FunPtr HS_vkCmdTraceRaysKHR

foreign import ccall unsafe "dynamic" unwrapVkCmdTraceRaysKHRUnsafe
               :: PFN_vkCmdTraceRaysKHR -> HS_vkCmdTraceRaysKHR

foreign import ccall safe "dynamic" unwrapVkCmdTraceRaysKHRSafe ::
               PFN_vkCmdTraceRaysKHR -> HS_vkCmdTraceRaysKHR

instance VulkanProc "vkCmdTraceRaysKHR" where
    type VkProcType "vkCmdTraceRaysKHR" = HS_vkCmdTraceRaysKHR
    vkProcSymbol = _VkCmdTraceRaysKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdTraceRaysKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdTraceRaysKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCreateRayTracingPipelinesKHR :: CString

pattern VkCreateRayTracingPipelinesKHR <-
        (is_VkCreateRayTracingPipelinesKHR -> True)
  where
    VkCreateRayTracingPipelinesKHR = _VkCreateRayTracingPipelinesKHR

{-# INLINE _VkCreateRayTracingPipelinesKHR #-}

_VkCreateRayTracingPipelinesKHR :: CString
_VkCreateRayTracingPipelinesKHR
  = Ptr "vkCreateRayTracingPipelinesKHR\NUL"#

{-# INLINE is_VkCreateRayTracingPipelinesKHR #-}

is_VkCreateRayTracingPipelinesKHR :: CString -> Bool
is_VkCreateRayTracingPipelinesKHR
  = (EQ ==) . cmpCStrings _VkCreateRayTracingPipelinesKHR

type VkCreateRayTracingPipelinesKHR =
     "vkCreateRayTracingPipelinesKHR"

-- | Success codes: 'VK_SUCCESS', 'VK_OPERATION_DEFERRED_KHR', 'VK_OPERATION_NOT_DEFERRED_KHR', 'VK_PIPELINE_COMPILE_REQUIRED_EXT'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS'.
--
--   > VkResult vkCreateRayTracingPipelinesKHR
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , uint32_t createInfoCount
--   >     , const VkRayTracingPipelineCreateInfoKHR* pCreateInfos
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipeline* pPipelines
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCreateRayTracingPipelinesKHR vkCreateRayTracingPipelinesKHR registry at www.khronos.org>
type HS_vkCreateRayTracingPipelinesKHR =
     VkDevice -- ^ device
              ->
       VkPipelineCache -- ^ pipelineCache
                       ->
         Word32 -- ^ createInfoCount
                ->
           Ptr VkRayTracingPipelineCreateInfoKHR -- ^ pCreateInfos
                                                 ->
             Ptr VkAllocationCallbacks -- ^ pAllocator
                                       -> Ptr VkPipeline -- ^ pPipelines
                                                         -> IO VkResult

type PFN_vkCreateRayTracingPipelinesKHR =
     FunPtr HS_vkCreateRayTracingPipelinesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCreateRayTracingPipelinesKHRUnsafe ::
               PFN_vkCreateRayTracingPipelinesKHR ->
                 HS_vkCreateRayTracingPipelinesKHR

foreign import ccall safe "dynamic"
               unwrapVkCreateRayTracingPipelinesKHRSafe ::
               PFN_vkCreateRayTracingPipelinesKHR ->
                 HS_vkCreateRayTracingPipelinesKHR

instance VulkanProc "vkCreateRayTracingPipelinesKHR" where
    type VkProcType "vkCreateRayTracingPipelinesKHR" =
         HS_vkCreateRayTracingPipelinesKHR
    vkProcSymbol = _VkCreateRayTracingPipelinesKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateRayTracingPipelinesKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateRayTracingPipelinesKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetRayTracingShaderGroupHandlesKHR :: CString

pattern VkGetRayTracingShaderGroupHandlesKHR <-
        (is_VkGetRayTracingShaderGroupHandlesKHR -> True)
  where
    VkGetRayTracingShaderGroupHandlesKHR
      = _VkGetRayTracingShaderGroupHandlesKHR

{-# INLINE _VkGetRayTracingShaderGroupHandlesKHR #-}

_VkGetRayTracingShaderGroupHandlesKHR :: CString
_VkGetRayTracingShaderGroupHandlesKHR
  = Ptr "vkGetRayTracingShaderGroupHandlesKHR\NUL"#

{-# INLINE is_VkGetRayTracingShaderGroupHandlesKHR #-}

is_VkGetRayTracingShaderGroupHandlesKHR :: CString -> Bool
is_VkGetRayTracingShaderGroupHandlesKHR
  = (EQ ==) . cmpCStrings _VkGetRayTracingShaderGroupHandlesKHR

type VkGetRayTracingShaderGroupHandlesKHR =
     "vkGetRayTracingShaderGroupHandlesKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetRayTracingShaderGroupHandlesKHR
--   >     ( VkDevice device
--   >     , VkPipeline pipeline
--   >     , uint32_t firstGroup
--   >     , uint32_t groupCount
--   >     , size_t dataSize
--   >     , void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetRayTracingShaderGroupHandlesKHR vkGetRayTracingShaderGroupHandlesKHR registry at www.khronos.org>
type HS_vkGetRayTracingShaderGroupHandlesKHR =
     VkDevice -- ^ device
              ->
       VkPipeline -- ^ pipeline
                  -> Word32 -- ^ firstGroup
                            -> Word32 -- ^ groupCount
                                      -> CSize -- ^ dataSize
                                               -> Ptr Void -- ^ pData
                                                           -> IO VkResult

type PFN_vkGetRayTracingShaderGroupHandlesKHR =
     FunPtr HS_vkGetRayTracingShaderGroupHandlesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetRayTracingShaderGroupHandlesKHRUnsafe ::
               PFN_vkGetRayTracingShaderGroupHandlesKHR ->
                 HS_vkGetRayTracingShaderGroupHandlesKHR

foreign import ccall safe "dynamic"
               unwrapVkGetRayTracingShaderGroupHandlesKHRSafe ::
               PFN_vkGetRayTracingShaderGroupHandlesKHR ->
                 HS_vkGetRayTracingShaderGroupHandlesKHR

instance VulkanProc "vkGetRayTracingShaderGroupHandlesKHR" where
    type VkProcType "vkGetRayTracingShaderGroupHandlesKHR" =
         HS_vkGetRayTracingShaderGroupHandlesKHR
    vkProcSymbol = _VkGetRayTracingShaderGroupHandlesKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetRayTracingShaderGroupHandlesKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetRayTracingShaderGroupHandlesKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetAccelerationStructureDeviceAddressKHR :: CString

pattern VkGetAccelerationStructureDeviceAddressKHR <-
        (is_VkGetAccelerationStructureDeviceAddressKHR -> True)
  where
    VkGetAccelerationStructureDeviceAddressKHR
      = _VkGetAccelerationStructureDeviceAddressKHR

{-# INLINE _VkGetAccelerationStructureDeviceAddressKHR #-}

_VkGetAccelerationStructureDeviceAddressKHR :: CString
_VkGetAccelerationStructureDeviceAddressKHR
  = Ptr "vkGetAccelerationStructureDeviceAddressKHR\NUL"#

{-# INLINE is_VkGetAccelerationStructureDeviceAddressKHR #-}

is_VkGetAccelerationStructureDeviceAddressKHR :: CString -> Bool
is_VkGetAccelerationStructureDeviceAddressKHR
  = (EQ ==) . cmpCStrings _VkGetAccelerationStructureDeviceAddressKHR

type VkGetAccelerationStructureDeviceAddressKHR =
     "vkGetAccelerationStructureDeviceAddressKHR"

-- | > VkDeviceAddress vkGetAccelerationStructureDeviceAddressKHR
--   >     ( VkDevice device
--   >     , const VkAccelerationStructureDeviceAddressInfoKHR* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetAccelerationStructureDeviceAddressKHR vkGetAccelerationStructureDeviceAddressKHR registry at www.khronos.org>
type HS_vkGetAccelerationStructureDeviceAddressKHR =
     VkDevice -- ^ device
              ->
       Ptr VkAccelerationStructureDeviceAddressInfoKHR -- ^ pInfo
                                                       ->
         IO VkDeviceAddress

type PFN_vkGetAccelerationStructureDeviceAddressKHR =
     FunPtr HS_vkGetAccelerationStructureDeviceAddressKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetAccelerationStructureDeviceAddressKHRUnsafe ::
               PFN_vkGetAccelerationStructureDeviceAddressKHR ->
                 HS_vkGetAccelerationStructureDeviceAddressKHR

foreign import ccall safe "dynamic"
               unwrapVkGetAccelerationStructureDeviceAddressKHRSafe ::
               PFN_vkGetAccelerationStructureDeviceAddressKHR ->
                 HS_vkGetAccelerationStructureDeviceAddressKHR

instance VulkanProc "vkGetAccelerationStructureDeviceAddressKHR"
         where
    type VkProcType "vkGetAccelerationStructureDeviceAddressKHR" =
         HS_vkGetAccelerationStructureDeviceAddressKHR
    vkProcSymbol = _VkGetAccelerationStructureDeviceAddressKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetAccelerationStructureDeviceAddressKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetAccelerationStructureDeviceAddressKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetRayTracingCaptureReplayShaderGroupHandlesKHR ::
        CString

pattern VkGetRayTracingCaptureReplayShaderGroupHandlesKHR <-
        (is_VkGetRayTracingCaptureReplayShaderGroupHandlesKHR -> True)
  where
    VkGetRayTracingCaptureReplayShaderGroupHandlesKHR
      = _VkGetRayTracingCaptureReplayShaderGroupHandlesKHR

{-# INLINE _VkGetRayTracingCaptureReplayShaderGroupHandlesKHR #-}

_VkGetRayTracingCaptureReplayShaderGroupHandlesKHR :: CString
_VkGetRayTracingCaptureReplayShaderGroupHandlesKHR
  = Ptr "vkGetRayTracingCaptureReplayShaderGroupHandlesKHR\NUL"#

{-# INLINE is_VkGetRayTracingCaptureReplayShaderGroupHandlesKHR #-}

is_VkGetRayTracingCaptureReplayShaderGroupHandlesKHR ::
                                                     CString -> Bool
is_VkGetRayTracingCaptureReplayShaderGroupHandlesKHR
  = (EQ ==) .
      cmpCStrings _VkGetRayTracingCaptureReplayShaderGroupHandlesKHR

type VkGetRayTracingCaptureReplayShaderGroupHandlesKHR =
     "vkGetRayTracingCaptureReplayShaderGroupHandlesKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetRayTracingCaptureReplayShaderGroupHandlesKHR
--   >     ( VkDevice device
--   >     , VkPipeline pipeline
--   >     , uint32_t firstGroup
--   >     , uint32_t groupCount
--   >     , size_t dataSize
--   >     , void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetRayTracingCaptureReplayShaderGroupHandlesKHR vkGetRayTracingCaptureReplayShaderGroupHandlesKHR registry at www.khronos.org>
type HS_vkGetRayTracingCaptureReplayShaderGroupHandlesKHR =
     VkDevice -- ^ device
              ->
       VkPipeline -- ^ pipeline
                  -> Word32 -- ^ firstGroup
                            -> Word32 -- ^ groupCount
                                      -> CSize -- ^ dataSize
                                               -> Ptr Void -- ^ pData
                                                           -> IO VkResult

type PFN_vkGetRayTracingCaptureReplayShaderGroupHandlesKHR =
     FunPtr HS_vkGetRayTracingCaptureReplayShaderGroupHandlesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetRayTracingCaptureReplayShaderGroupHandlesKHRUnsafe ::
               PFN_vkGetRayTracingCaptureReplayShaderGroupHandlesKHR ->
                 HS_vkGetRayTracingCaptureReplayShaderGroupHandlesKHR

foreign import ccall safe "dynamic"
               unwrapVkGetRayTracingCaptureReplayShaderGroupHandlesKHRSafe ::
               PFN_vkGetRayTracingCaptureReplayShaderGroupHandlesKHR ->
                 HS_vkGetRayTracingCaptureReplayShaderGroupHandlesKHR

instance VulkanProc
           "vkGetRayTracingCaptureReplayShaderGroupHandlesKHR"
         where
    type VkProcType "vkGetRayTracingCaptureReplayShaderGroupHandlesKHR"
         = HS_vkGetRayTracingCaptureReplayShaderGroupHandlesKHR
    vkProcSymbol = _VkGetRayTracingCaptureReplayShaderGroupHandlesKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetRayTracingCaptureReplayShaderGroupHandlesKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetRayTracingCaptureReplayShaderGroupHandlesKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdWriteAccelerationStructuresPropertiesKHR :: CString

pattern VkCmdWriteAccelerationStructuresPropertiesKHR <-
        (is_VkCmdWriteAccelerationStructuresPropertiesKHR -> True)
  where
    VkCmdWriteAccelerationStructuresPropertiesKHR
      = _VkCmdWriteAccelerationStructuresPropertiesKHR

{-# INLINE _VkCmdWriteAccelerationStructuresPropertiesKHR #-}

_VkCmdWriteAccelerationStructuresPropertiesKHR :: CString
_VkCmdWriteAccelerationStructuresPropertiesKHR
  = Ptr "vkCmdWriteAccelerationStructuresPropertiesKHR\NUL"#

{-# INLINE is_VkCmdWriteAccelerationStructuresPropertiesKHR #-}

is_VkCmdWriteAccelerationStructuresPropertiesKHR :: CString -> Bool
is_VkCmdWriteAccelerationStructuresPropertiesKHR
  = (EQ ==) .
      cmpCStrings _VkCmdWriteAccelerationStructuresPropertiesKHR

type VkCmdWriteAccelerationStructuresPropertiesKHR =
     "vkCmdWriteAccelerationStructuresPropertiesKHR"

-- | Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdWriteAccelerationStructuresPropertiesKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t accelerationStructureCount
--   >     , const VkAccelerationStructureKHR* pAccelerationStructures
--   >     , VkQueryType queryType
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdWriteAccelerationStructuresPropertiesKHR vkCmdWriteAccelerationStructuresPropertiesKHR registry at www.khronos.org>
type HS_vkCmdWriteAccelerationStructuresPropertiesKHR =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ accelerationStructureCount
              ->
         Ptr VkAccelerationStructureKHR -- ^ pAccelerationStructures
                                        ->
           VkQueryType -- ^ queryType
                       -> VkQueryPool -- ^ queryPool
                                      -> Word32 -- ^ firstQuery
                                                -> IO ()

type PFN_vkCmdWriteAccelerationStructuresPropertiesKHR =
     FunPtr HS_vkCmdWriteAccelerationStructuresPropertiesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCmdWriteAccelerationStructuresPropertiesKHRUnsafe ::
               PFN_vkCmdWriteAccelerationStructuresPropertiesKHR ->
                 HS_vkCmdWriteAccelerationStructuresPropertiesKHR

foreign import ccall safe "dynamic"
               unwrapVkCmdWriteAccelerationStructuresPropertiesKHRSafe ::
               PFN_vkCmdWriteAccelerationStructuresPropertiesKHR ->
                 HS_vkCmdWriteAccelerationStructuresPropertiesKHR

instance VulkanProc "vkCmdWriteAccelerationStructuresPropertiesKHR"
         where
    type VkProcType "vkCmdWriteAccelerationStructuresPropertiesKHR" =
         HS_vkCmdWriteAccelerationStructuresPropertiesKHR
    vkProcSymbol = _VkCmdWriteAccelerationStructuresPropertiesKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdWriteAccelerationStructuresPropertiesKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkCmdWriteAccelerationStructuresPropertiesKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdTraceRaysIndirectKHR :: CString

pattern VkCmdTraceRaysIndirectKHR <-
        (is_VkCmdTraceRaysIndirectKHR -> True)
  where
    VkCmdTraceRaysIndirectKHR = _VkCmdTraceRaysIndirectKHR

{-# INLINE _VkCmdTraceRaysIndirectKHR #-}

_VkCmdTraceRaysIndirectKHR :: CString
_VkCmdTraceRaysIndirectKHR = Ptr "vkCmdTraceRaysIndirectKHR\NUL"#

{-# INLINE is_VkCmdTraceRaysIndirectKHR #-}

is_VkCmdTraceRaysIndirectKHR :: CString -> Bool
is_VkCmdTraceRaysIndirectKHR
  = (EQ ==) . cmpCStrings _VkCmdTraceRaysIndirectKHR

type VkCmdTraceRaysIndirectKHR = "vkCmdTraceRaysIndirectKHR"

-- | Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdTraceRaysIndirectKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkStridedBufferRegionKHR* pRaygenShaderBindingTable
--   >     , const VkStridedBufferRegionKHR* pMissShaderBindingTable
--   >     , const VkStridedBufferRegionKHR* pHitShaderBindingTable
--   >     , const VkStridedBufferRegionKHR* pCallableShaderBindingTable
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdTraceRaysIndirectKHR vkCmdTraceRaysIndirectKHR registry at www.khronos.org>
type HS_vkCmdTraceRaysIndirectKHR =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Ptr VkStridedBufferRegionKHR -- ^ pRaygenShaderBindingTable
                                    ->
         Ptr VkStridedBufferRegionKHR -- ^ pMissShaderBindingTable
                                      ->
           Ptr VkStridedBufferRegionKHR -- ^ pHitShaderBindingTable
                                        ->
             Ptr VkStridedBufferRegionKHR -- ^ pCallableShaderBindingTable
                                          -> VkBuffer -- ^ buffer
                                                      -> VkDeviceSize -- ^ offset
                                                                      -> IO ()

type PFN_vkCmdTraceRaysIndirectKHR =
     FunPtr HS_vkCmdTraceRaysIndirectKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCmdTraceRaysIndirectKHRUnsafe ::
               PFN_vkCmdTraceRaysIndirectKHR -> HS_vkCmdTraceRaysIndirectKHR

foreign import ccall safe "dynamic"
               unwrapVkCmdTraceRaysIndirectKHRSafe ::
               PFN_vkCmdTraceRaysIndirectKHR -> HS_vkCmdTraceRaysIndirectKHR

instance VulkanProc "vkCmdTraceRaysIndirectKHR" where
    type VkProcType "vkCmdTraceRaysIndirectKHR" =
         HS_vkCmdTraceRaysIndirectKHR
    vkProcSymbol = _VkCmdTraceRaysIndirectKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdTraceRaysIndirectKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdTraceRaysIndirectKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetDeviceAccelerationStructureCompatibilityKHR :: CString

pattern VkGetDeviceAccelerationStructureCompatibilityKHR <-
        (is_VkGetDeviceAccelerationStructureCompatibilityKHR -> True)
  where
    VkGetDeviceAccelerationStructureCompatibilityKHR
      = _VkGetDeviceAccelerationStructureCompatibilityKHR

{-# INLINE _VkGetDeviceAccelerationStructureCompatibilityKHR #-}

_VkGetDeviceAccelerationStructureCompatibilityKHR :: CString
_VkGetDeviceAccelerationStructureCompatibilityKHR
  = Ptr "vkGetDeviceAccelerationStructureCompatibilityKHR\NUL"#

{-# INLINE is_VkGetDeviceAccelerationStructureCompatibilityKHR #-}

is_VkGetDeviceAccelerationStructureCompatibilityKHR ::
                                                    CString -> Bool
is_VkGetDeviceAccelerationStructureCompatibilityKHR
  = (EQ ==) .
      cmpCStrings _VkGetDeviceAccelerationStructureCompatibilityKHR

type VkGetDeviceAccelerationStructureCompatibilityKHR =
     "vkGetDeviceAccelerationStructureCompatibilityKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_INCOMPATIBLE_VERSION_KHR'.
--
--   > VkResult vkGetDeviceAccelerationStructureCompatibilityKHR
--   >     ( VkDevice device
--   >     , const VkAccelerationStructureVersionKHR* version
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetDeviceAccelerationStructureCompatibilityKHR vkGetDeviceAccelerationStructureCompatibilityKHR registry at www.khronos.org>
type HS_vkGetDeviceAccelerationStructureCompatibilityKHR =
     VkDevice -- ^ device
              -> Ptr VkAccelerationStructureVersionKHR -- ^ version
                                                       -> IO VkResult

type PFN_vkGetDeviceAccelerationStructureCompatibilityKHR =
     FunPtr HS_vkGetDeviceAccelerationStructureCompatibilityKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetDeviceAccelerationStructureCompatibilityKHRUnsafe ::
               PFN_vkGetDeviceAccelerationStructureCompatibilityKHR ->
                 HS_vkGetDeviceAccelerationStructureCompatibilityKHR

foreign import ccall safe "dynamic"
               unwrapVkGetDeviceAccelerationStructureCompatibilityKHRSafe ::
               PFN_vkGetDeviceAccelerationStructureCompatibilityKHR ->
                 HS_vkGetDeviceAccelerationStructureCompatibilityKHR

instance VulkanProc
           "vkGetDeviceAccelerationStructureCompatibilityKHR"
         where
    type VkProcType "vkGetDeviceAccelerationStructureCompatibilityKHR"
         = HS_vkGetDeviceAccelerationStructureCompatibilityKHR
    vkProcSymbol = _VkGetDeviceAccelerationStructureCompatibilityKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetDeviceAccelerationStructureCompatibilityKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetDeviceAccelerationStructureCompatibilityKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_RAY_TRACING_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_RAY_TRACING_SPEC_VERSION = 8

type VK_KHR_RAY_TRACING_SPEC_VERSION = 8

pattern VK_KHR_RAY_TRACING_EXTENSION_NAME :: CString

pattern VK_KHR_RAY_TRACING_EXTENSION_NAME <-
        (is_VK_KHR_RAY_TRACING_EXTENSION_NAME -> True)
  where
    VK_KHR_RAY_TRACING_EXTENSION_NAME
      = _VK_KHR_RAY_TRACING_EXTENSION_NAME

{-# INLINE _VK_KHR_RAY_TRACING_EXTENSION_NAME #-}

_VK_KHR_RAY_TRACING_EXTENSION_NAME :: CString
_VK_KHR_RAY_TRACING_EXTENSION_NAME = Ptr "VK_KHR_ray_tracing\NUL"#

{-# INLINE is_VK_KHR_RAY_TRACING_EXTENSION_NAME #-}

is_VK_KHR_RAY_TRACING_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_RAY_TRACING_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_RAY_TRACING_EXTENSION_NAME

type VK_KHR_RAY_TRACING_EXTENSION_NAME = "VK_KHR_ray_tracing"

pattern VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR
        = VkStructureType 1000165006

pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR
        = VkStructureType 1000165007

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR
        = VkStructureType 1000150000

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_GEOMETRY_TYPE_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_GEOMETRY_TYPE_INFO_KHR
        = VkStructureType 1000150001

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR
        = VkStructureType 1000150002

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR
        = VkStructureType 1000150003

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR
        = VkStructureType 1000150004

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR
        = VkStructureType 1000150005

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR =
        VkStructureType 1000150006

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_KHR
        = VkStructureType 1000150008

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_KHR =
        VkStructureType 1000150009

pattern VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR =
        VkStructureType 1000150010

pattern VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR
        = VkStructureType 1000150011

pattern VK_STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR
        = VkStructureType 1000150012

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_FEATURES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_FEATURES_KHR
        = VkStructureType 1000150013

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_KHR
        = VkStructureType 1000150014

pattern VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR =
        VkStructureType 1000150015

pattern VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR
        = VkStructureType 1000150016

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR =
        VkStructureType 1000150017

pattern VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR
        = VkStructureType 1000150018

-- | bitpos = @8@
pattern VK_SHADER_STAGE_RAYGEN_BIT_KHR :: VkShaderStageBitmask a

pattern VK_SHADER_STAGE_RAYGEN_BIT_KHR = VkShaderStageBitmask 256

-- | bitpos = @9@
pattern VK_SHADER_STAGE_ANY_HIT_BIT_KHR :: VkShaderStageBitmask a

pattern VK_SHADER_STAGE_ANY_HIT_BIT_KHR = VkShaderStageBitmask 512

-- | bitpos = @10@
pattern VK_SHADER_STAGE_CLOSEST_HIT_BIT_KHR ::
        VkShaderStageBitmask a

pattern VK_SHADER_STAGE_CLOSEST_HIT_BIT_KHR =
        VkShaderStageBitmask 1024

-- | bitpos = @11@
pattern VK_SHADER_STAGE_MISS_BIT_KHR :: VkShaderStageBitmask a

pattern VK_SHADER_STAGE_MISS_BIT_KHR = VkShaderStageBitmask 2048

-- | bitpos = @12@
pattern VK_SHADER_STAGE_INTERSECTION_BIT_KHR ::
        VkShaderStageBitmask a

pattern VK_SHADER_STAGE_INTERSECTION_BIT_KHR =
        VkShaderStageBitmask 4096

-- | bitpos = @13@
pattern VK_SHADER_STAGE_CALLABLE_BIT_KHR :: VkShaderStageBitmask a

pattern VK_SHADER_STAGE_CALLABLE_BIT_KHR =
        VkShaderStageBitmask 8192

-- | bitpos = @21@
pattern VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR =
        VkPipelineStageBitmask 2097152

-- | bitpos = @25@
pattern VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR =
        VkPipelineStageBitmask 33554432

-- | bitpos = @10@
pattern VK_BUFFER_USAGE_RAY_TRACING_BIT_KHR ::
        VkBufferUsageBitmask a

pattern VK_BUFFER_USAGE_RAY_TRACING_BIT_KHR =
        VkBufferUsageBitmask 1024

pattern VK_PIPELINE_BIND_POINT_RAY_TRACING_KHR ::
        VkPipelineBindPoint

pattern VK_PIPELINE_BIND_POINT_RAY_TRACING_KHR =
        VkPipelineBindPoint 1000165000

pattern VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR ::
        VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR =
        VkDescriptorType 1000165000

-- | bitpos = @21@
pattern VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR ::
        VkAccessBitmask a

pattern VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR =
        VkAccessBitmask 2097152

-- | bitpos = @22@
pattern VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR ::
        VkAccessBitmask a

pattern VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR =
        VkAccessBitmask 4194304

pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR ::
        VkQueryType

pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR =
        VkQueryType 1000165000

pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR
        :: VkQueryType

pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR
        = VkQueryType 1000150000

pattern VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR :: VkObjectType

pattern VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR =
        VkObjectType 1000165000

pattern VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT
        :: VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT
        = VkDebugReportObjectTypeEXT 1000165000

pattern VK_INDEX_TYPE_NONE_KHR :: VkIndexType

pattern VK_INDEX_TYPE_NONE_KHR = VkIndexType 1000165000

pattern VK_GEOMETRY_TYPE_INSTANCES_KHR :: VkGeometryTypeKHR

pattern VK_GEOMETRY_TYPE_INSTANCES_KHR =
        VkGeometryTypeKHR 1000150000

pattern VK_ERROR_INCOMPATIBLE_VERSION_KHR :: VkResult

pattern VK_ERROR_INCOMPATIBLE_VERSION_KHR = VkResult (-1000150000)

-- | bitpos = @29@
pattern VK_FORMAT_FEATURE_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR
        :: VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR
        = VkFormatFeatureBitmask 536870912

-- | bitpos = @14@
pattern VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR
        :: VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR
        = VkPipelineCreateBitmask 16384

-- | bitpos = @15@
pattern VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR
        :: VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR
        = VkPipelineCreateBitmask 32768

-- | bitpos = @16@
pattern VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR
        :: VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR
        = VkPipelineCreateBitmask 65536

-- | bitpos = @17@
pattern VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR
        :: VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR
        = VkPipelineCreateBitmask 131072

-- | bitpos = @12@
pattern VK_PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR ::
        VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR =
        VkPipelineCreateBitmask 4096

-- | bitpos = @13@
pattern VK_PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR ::
        VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR =
        VkPipelineCreateBitmask 8192
