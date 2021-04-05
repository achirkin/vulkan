{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
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
module Graphics.Vulkan.Ext.VK_NV_ray_tracing
       (-- * Vulkan extension: @VK_NV_ray_tracing@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Eric Werness @ewerness@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @166@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_get_memory_requirements2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_get_memory_requirements2'.
        module Graphics.Vulkan.Marshal, VkAabbPositionsNV,
        VkAccelerationStructureCreateInfoNV, VkAccelerationStructureInfoNV,
        VkAccelerationStructureInstanceNV,
        VkAccelerationStructureMemoryRequirementsInfoNV,
        VkAccelerationStructureBuildTypeKHR(..),
        VkAccelerationStructureMemoryRequirementsTypeKHR(..),
        VkAccelerationStructureTypeKHR(..),
        VkAccelerationStructureMemoryRequirementsTypeNV(..),
        VkAccelerationStructureTypeNV(..),
        VkBindAccelerationStructureMemoryInfoNV, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkBuildAccelerationStructureBitmaskKHR(..),
        VkBuildAccelerationStructureFlagBitsKHR(),
        VkBuildAccelerationStructureFlagBitsNV(..),
        VkBuildAccelerationStructureFlagsKHR(),
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
        VkXlibSurfaceCreateFlagsKHR(..),
        VkCopyAccelerationStructureModeKHR(..),
        VkCopyAccelerationStructureModeNV(..), VkFormat(..),
        VkFormatFeatureBitmask(..), VkFormatFeatureFlagBits(),
        VkFormatFeatureFlags(), VkGeometryAABBNV, VkGeometryDataNV,
        VkGeometryInstanceBitmaskKHR(..), VkGeometryBitmaskKHR(..),
        VkGeometryTypeKHR(..), VkGeometryFlagBitsKHR(),
        VkGeometryFlagBitsNV(..), VkGeometryFlagsKHR(),
        VkGeometryInstanceFlagBitsKHR(), VkGeometryInstanceFlagBitsNV(..),
        VkGeometryInstanceFlagsKHR(), VkGeometryTypeNV(..), VkGeometryNV,
        VkGeometryTrianglesNV, VkIndexType(..), VkMemoryRequirements2KHR,
        VkPhysicalDeviceLimits, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2,
        VkPhysicalDeviceRayTracingPropertiesNV,
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
        VkPipelineStageFlags(), VkPipelineShaderStageCreateInfo,
        VkRayTracingPipelineCreateInfoNV,
        VkRayTracingShaderGroupCreateInfoNV,
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
        VkSpecializationMapEntry, VkStructureType(..), VkTransformMatrixNV,
        VkWriteDescriptorSetAccelerationStructureNV,
        -- > #include "vk_platform.h"
        VkCreateAccelerationStructureNV,
        pattern VkCreateAccelerationStructureNV,
        HS_vkCreateAccelerationStructureNV,
        PFN_vkCreateAccelerationStructureNV,
        VkDestroyAccelerationStructureNV,
        pattern VkDestroyAccelerationStructureNV,
        HS_vkDestroyAccelerationStructureNV,
        PFN_vkDestroyAccelerationStructureNV,
        VkGetAccelerationStructureMemoryRequirementsNV,
        pattern VkGetAccelerationStructureMemoryRequirementsNV,
        HS_vkGetAccelerationStructureMemoryRequirementsNV,
        PFN_vkGetAccelerationStructureMemoryRequirementsNV,
        VkBindAccelerationStructureMemoryNV,
        pattern VkBindAccelerationStructureMemoryNV,
        HS_vkBindAccelerationStructureMemoryNV,
        PFN_vkBindAccelerationStructureMemoryNV,
        VkCmdBuildAccelerationStructureNV,
        pattern VkCmdBuildAccelerationStructureNV,
        HS_vkCmdBuildAccelerationStructureNV,
        PFN_vkCmdBuildAccelerationStructureNV,
        VkCmdCopyAccelerationStructureNV,
        pattern VkCmdCopyAccelerationStructureNV,
        HS_vkCmdCopyAccelerationStructureNV,
        PFN_vkCmdCopyAccelerationStructureNV, VkCmdTraceRaysNV,
        pattern VkCmdTraceRaysNV, HS_vkCmdTraceRaysNV,
        PFN_vkCmdTraceRaysNV, VkCreateRayTracingPipelinesNV,
        pattern VkCreateRayTracingPipelinesNV,
        HS_vkCreateRayTracingPipelinesNV,
        PFN_vkCreateRayTracingPipelinesNV,
        VkGetRayTracingShaderGroupHandlesNV,
        pattern VkGetRayTracingShaderGroupHandlesNV,
        HS_vkGetRayTracingShaderGroupHandlesNV,
        PFN_vkGetRayTracingShaderGroupHandlesNV,
        VkGetAccelerationStructureHandleNV,
        pattern VkGetAccelerationStructureHandleNV,
        HS_vkGetAccelerationStructureHandleNV,
        PFN_vkGetAccelerationStructureHandleNV,
        VkCmdWriteAccelerationStructuresPropertiesNV,
        pattern VkCmdWriteAccelerationStructuresPropertiesNV,
        HS_vkCmdWriteAccelerationStructuresPropertiesNV,
        PFN_vkCmdWriteAccelerationStructuresPropertiesNV,
        VkCompileDeferredNV, pattern VkCompileDeferredNV,
        HS_vkCompileDeferredNV, PFN_vkCompileDeferredNV,
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
        VkAllocationCallbacks, VkAabbPositionsKHR,
        VkAccelerationStructureBuildGeometryInfoKHR,
        VkAccelerationStructureBuildOffsetInfoKHR,
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
        VkBindAccelerationStructureMemoryInfoKHR,
        VkCopyAccelerationStructureInfoKHR,
        VkCopyAccelerationStructureToMemoryInfoKHR,
        VkCopyMemoryToAccelerationStructureInfoKHR,
        VkDeferredOperationInfoKHR, VkDeviceOrHostAddressConstKHR,
        VkDeviceOrHostAddressKHR, VkPhysicalDeviceRayTracingFeaturesKHR,
        VkPhysicalDeviceRayTracingPropertiesKHR,
        VkPipelineLibraryCreateInfoKHR, VkRayTracingPipelineCreateInfoKHR,
        VkRayTracingPipelineInterfaceCreateInfoKHR,
        VkRayTracingShaderGroupCreateInfoKHR, VkStridedBufferRegionKHR,
        VkTraceRaysIndirectCommandKHR, VkTransformMatrixKHR,
        VkWriteDescriptorSetAccelerationStructureKHR,
        VkMemoryAllocateFlagsInfo, VkMemoryAllocateFlagsInfoKHR,
        VkMemoryAllocateInfo, VkMemoryBarrier,
        VkMemoryDedicatedAllocateInfo, VkMemoryDedicatedAllocateInfoKHR,
        VkMemoryDedicatedRequirements, VkMemoryDedicatedRequirementsKHR,
        VkMemoryFdPropertiesKHR, VkMemoryGetFdInfoKHR, VkMemoryHeap,
        VkMemoryHostPointerPropertiesEXT,
        VkMemoryOpaqueCaptureAddressAllocateInfo,
        VkMemoryOpaqueCaptureAddressAllocateInfoKHR,
        VkMemoryPriorityAllocateInfoEXT, VkMemoryRequirements,
        VkMemoryRequirements2, VkMemoryType, VkGraphicsPipelineCreateInfo,
        VkPipelineCacheCreateInfo,
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
        VK_NV_RAY_TRACING_SPEC_VERSION,
        pattern VK_NV_RAY_TRACING_SPEC_VERSION,
        VK_NV_RAY_TRACING_EXTENSION_NAME,
        pattern VK_NV_RAY_TRACING_EXTENSION_NAME,
        pattern VK_SHADER_UNUSED_NV,
        pattern VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_GEOMETRY_NV,
        pattern VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV,
        pattern VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV,
        pattern VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV,
        pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV,
        pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV,
        pattern VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV,
        pattern VK_SHADER_STAGE_RAYGEN_BIT_NV,
        pattern VK_SHADER_STAGE_ANY_HIT_BIT_NV,
        pattern VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV,
        pattern VK_SHADER_STAGE_MISS_BIT_NV,
        pattern VK_SHADER_STAGE_INTERSECTION_BIT_NV,
        pattern VK_SHADER_STAGE_CALLABLE_BIT_NV,
        pattern VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV,
        pattern VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV,
        pattern VK_BUFFER_USAGE_RAY_TRACING_BIT_NV,
        pattern VK_PIPELINE_BIND_POINT_RAY_TRACING_NV,
        pattern VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV,
        pattern VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV,
        pattern VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV,
        pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV,
        pattern VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV,
        pattern VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT,
        pattern VK_INDEX_TYPE_NONE_NV,
        pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV,
        pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV,
        pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV,
        pattern VK_GEOMETRY_TYPE_TRIANGLES_NV,
        pattern VK_GEOMETRY_TYPE_AABBS_NV,
        pattern VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV,
        pattern VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV,
        pattern VK_GEOMETRY_OPAQUE_BIT_NV,
        pattern VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV,
        pattern VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV,
        pattern VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV,
        pattern VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV,
        pattern VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV,
        pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV,
        pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV,
        pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV,
        pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV,
        pattern VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV,
        pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV,
        pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV,
        pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV,
        pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV,
        pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV)
       where
import GHC.Ptr                                                   (Ptr (..))
import Graphics.Vulkan.Constants                                 (pattern VK_SHADER_UNUSED_NV)
import Graphics.Vulkan.Ext.VK_KHR_ray_tracing                    (pattern VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR,
                                                                  pattern VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR,
                                                                  pattern VK_BUFFER_USAGE_RAY_TRACING_BIT_KHR,
                                                                  pattern VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT,
                                                                  pattern VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR,
                                                                  pattern VK_INDEX_TYPE_NONE_KHR,
                                                                  pattern VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR,
                                                                  pattern VK_PIPELINE_BIND_POINT_RAY_TRACING_KHR,
                                                                  pattern VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR,
                                                                  pattern VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR,
                                                                  pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR,
                                                                  pattern VK_SHADER_STAGE_ANY_HIT_BIT_KHR,
                                                                  pattern VK_SHADER_STAGE_CALLABLE_BIT_KHR,
                                                                  pattern VK_SHADER_STAGE_CLOSEST_HIT_BIT_KHR,
                                                                  pattern VK_SHADER_STAGE_INTERSECTION_BIT_KHR,
                                                                  pattern VK_SHADER_STAGE_MISS_BIT_KHR,
                                                                  pattern VK_SHADER_STAGE_RAYGEN_BIT_KHR,
                                                                  pattern VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR,
                                                                  pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                              (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.AccelerationStructure
import Graphics.Vulkan.Types.Enum.BuildAccelerationStructureFlag
import Graphics.Vulkan.Types.Enum.CopyAccelerationStructureMode
import Graphics.Vulkan.Types.Enum.Format
import Graphics.Vulkan.Types.Enum.Geometry
import Graphics.Vulkan.Types.Enum.IndexType
import Graphics.Vulkan.Types.Enum.InternalAllocationType
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
import Graphics.Vulkan.Types.Struct.AabbPositionsNV              (VkAabbPositionsNV)
import Graphics.Vulkan.Types.Struct.AccelerationStructure
import Graphics.Vulkan.Types.Struct.AllocationCallbacks
import Graphics.Vulkan.Types.Struct.Bind                         (VkBindAccelerationStructureMemoryInfoNV)
import Graphics.Vulkan.Types.Struct.EnableBetaExtensions
import Graphics.Vulkan.Types.Struct.Geometry
import Graphics.Vulkan.Types.Struct.Memory
import Graphics.Vulkan.Types.Struct.PhysicalDevice               (VkPhysicalDeviceLimits,
                                                                  VkPhysicalDeviceProperties,
                                                                  VkPhysicalDeviceProperties2,
                                                                  VkPhysicalDeviceRayTracingPropertiesNV,
                                                                  VkPhysicalDeviceSparseProperties)
import Graphics.Vulkan.Types.Struct.Pipeline
import Graphics.Vulkan.Types.Struct.RayTracing
import Graphics.Vulkan.Types.Struct.Specialization
import Graphics.Vulkan.Types.Struct.TransformMatrixNV            (VkTransformMatrixNV)
import Graphics.Vulkan.Types.Struct.WriteDescriptorSet           (VkWriteDescriptorSetAccelerationStructureNV)

pattern VkCreateAccelerationStructureNV :: CString

pattern VkCreateAccelerationStructureNV <-
        (is_VkCreateAccelerationStructureNV -> True)
  where
    VkCreateAccelerationStructureNV = _VkCreateAccelerationStructureNV

{-# INLINE _VkCreateAccelerationStructureNV #-}

_VkCreateAccelerationStructureNV :: CString
_VkCreateAccelerationStructureNV
  = Ptr "vkCreateAccelerationStructureNV\NUL"#

{-# INLINE is_VkCreateAccelerationStructureNV #-}

is_VkCreateAccelerationStructureNV :: CString -> Bool
is_VkCreateAccelerationStructureNV
  = (EQ ==) . cmpCStrings _VkCreateAccelerationStructureNV

type VkCreateAccelerationStructureNV =
     "vkCreateAccelerationStructureNV"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkCreateAccelerationStructureNV
--   >     ( VkDevice device
--   >     , const VkAccelerationStructureCreateInfoNV* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkAccelerationStructureNV* pAccelerationStructure
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCreateAccelerationStructureNV vkCreateAccelerationStructureNV registry at www.khronos.org>
type HS_vkCreateAccelerationStructureNV =
     VkDevice -- ^ device
              ->
       Ptr VkAccelerationStructureCreateInfoNV -- ^ pCreateInfo
                                               ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   ->
           Ptr VkAccelerationStructureNV -- ^ pAccelerationStructure
                                         -> IO VkResult

type PFN_vkCreateAccelerationStructureNV =
     FunPtr HS_vkCreateAccelerationStructureNV

foreign import ccall unsafe "dynamic"
               unwrapVkCreateAccelerationStructureNVUnsafe ::
               PFN_vkCreateAccelerationStructureNV ->
                 HS_vkCreateAccelerationStructureNV

foreign import ccall safe "dynamic"
               unwrapVkCreateAccelerationStructureNVSafe ::
               PFN_vkCreateAccelerationStructureNV ->
                 HS_vkCreateAccelerationStructureNV

instance VulkanProc "vkCreateAccelerationStructureNV" where
    type VkProcType "vkCreateAccelerationStructureNV" =
         HS_vkCreateAccelerationStructureNV
    vkProcSymbol = _VkCreateAccelerationStructureNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateAccelerationStructureNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateAccelerationStructureNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkDestroyAccelerationStructureNV :: CString

pattern VkDestroyAccelerationStructureNV <-
        (is_VkDestroyAccelerationStructureNV -> True)
  where
    VkDestroyAccelerationStructureNV
      = _VkDestroyAccelerationStructureNV

{-# INLINE _VkDestroyAccelerationStructureNV #-}

_VkDestroyAccelerationStructureNV :: CString
_VkDestroyAccelerationStructureNV
  = Ptr "vkDestroyAccelerationStructureNV\NUL"#

{-# INLINE is_VkDestroyAccelerationStructureNV #-}

is_VkDestroyAccelerationStructureNV :: CString -> Bool
is_VkDestroyAccelerationStructureNV
  = (EQ ==) . cmpCStrings _VkDestroyAccelerationStructureNV

type VkDestroyAccelerationStructureNV =
     "vkDestroyAccelerationStructureNV"

-- | This is an alias for `vkDestroyAccelerationStructureKHR`.
--
--   > void vkDestroyAccelerationStructureNV
--   >     ( VkDevice device
--   >     , VkAccelerationStructureKHR accelerationStructure
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkDestroyAccelerationStructureNV vkDestroyAccelerationStructureNV registry at www.khronos.org>
type HS_vkDestroyAccelerationStructureNV =
     VkDevice -- ^ device
              ->
       VkAccelerationStructureKHR -- ^ accelerationStructure
                                  -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                               -> IO ()

type PFN_vkDestroyAccelerationStructureNV =
     FunPtr HS_vkDestroyAccelerationStructureNV

foreign import ccall unsafe "dynamic"
               unwrapVkDestroyAccelerationStructureNVUnsafe ::
               PFN_vkDestroyAccelerationStructureNV ->
                 HS_vkDestroyAccelerationStructureNV

foreign import ccall safe "dynamic"
               unwrapVkDestroyAccelerationStructureNVSafe ::
               PFN_vkDestroyAccelerationStructureNV ->
                 HS_vkDestroyAccelerationStructureNV

instance VulkanProc "vkDestroyAccelerationStructureNV" where
    type VkProcType "vkDestroyAccelerationStructureNV" =
         HS_vkDestroyAccelerationStructureNV
    vkProcSymbol = _VkDestroyAccelerationStructureNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkDestroyAccelerationStructureNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkDestroyAccelerationStructureNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetAccelerationStructureMemoryRequirementsNV :: CString

pattern VkGetAccelerationStructureMemoryRequirementsNV <-
        (is_VkGetAccelerationStructureMemoryRequirementsNV -> True)
  where
    VkGetAccelerationStructureMemoryRequirementsNV
      = _VkGetAccelerationStructureMemoryRequirementsNV

{-# INLINE _VkGetAccelerationStructureMemoryRequirementsNV #-}

_VkGetAccelerationStructureMemoryRequirementsNV :: CString
_VkGetAccelerationStructureMemoryRequirementsNV
  = Ptr "vkGetAccelerationStructureMemoryRequirementsNV\NUL"#

{-# INLINE is_VkGetAccelerationStructureMemoryRequirementsNV #-}

is_VkGetAccelerationStructureMemoryRequirementsNV ::
                                                  CString -> Bool
is_VkGetAccelerationStructureMemoryRequirementsNV
  = (EQ ==) .
      cmpCStrings _VkGetAccelerationStructureMemoryRequirementsNV

type VkGetAccelerationStructureMemoryRequirementsNV =
     "vkGetAccelerationStructureMemoryRequirementsNV"

-- | > void vkGetAccelerationStructureMemoryRequirementsNV
--   >     ( VkDevice device
--   >     , const VkAccelerationStructureMemoryRequirementsInfoNV* pInfo
--   >     , VkMemoryRequirements2KHR* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetAccelerationStructureMemoryRequirementsNV vkGetAccelerationStructureMemoryRequirementsNV registry at www.khronos.org>
type HS_vkGetAccelerationStructureMemoryRequirementsNV =
     VkDevice -- ^ device
              ->
       Ptr VkAccelerationStructureMemoryRequirementsInfoNV -- ^ pInfo
                                                           ->
         Ptr VkMemoryRequirements2KHR -- ^ pMemoryRequirements
                                      -> IO ()

type PFN_vkGetAccelerationStructureMemoryRequirementsNV =
     FunPtr HS_vkGetAccelerationStructureMemoryRequirementsNV

foreign import ccall unsafe "dynamic"
               unwrapVkGetAccelerationStructureMemoryRequirementsNVUnsafe ::
               PFN_vkGetAccelerationStructureMemoryRequirementsNV ->
                 HS_vkGetAccelerationStructureMemoryRequirementsNV

foreign import ccall safe "dynamic"
               unwrapVkGetAccelerationStructureMemoryRequirementsNVSafe ::
               PFN_vkGetAccelerationStructureMemoryRequirementsNV ->
                 HS_vkGetAccelerationStructureMemoryRequirementsNV

instance VulkanProc
           "vkGetAccelerationStructureMemoryRequirementsNV"
         where
    type VkProcType "vkGetAccelerationStructureMemoryRequirementsNV" =
         HS_vkGetAccelerationStructureMemoryRequirementsNV
    vkProcSymbol = _VkGetAccelerationStructureMemoryRequirementsNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetAccelerationStructureMemoryRequirementsNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetAccelerationStructureMemoryRequirementsNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkBindAccelerationStructureMemoryNV :: CString

pattern VkBindAccelerationStructureMemoryNV <-
        (is_VkBindAccelerationStructureMemoryNV -> True)
  where
    VkBindAccelerationStructureMemoryNV
      = _VkBindAccelerationStructureMemoryNV

{-# INLINE _VkBindAccelerationStructureMemoryNV #-}

_VkBindAccelerationStructureMemoryNV :: CString
_VkBindAccelerationStructureMemoryNV
  = Ptr "vkBindAccelerationStructureMemoryNV\NUL"#

{-# INLINE is_VkBindAccelerationStructureMemoryNV #-}

is_VkBindAccelerationStructureMemoryNV :: CString -> Bool
is_VkBindAccelerationStructureMemoryNV
  = (EQ ==) . cmpCStrings _VkBindAccelerationStructureMemoryNV

type VkBindAccelerationStructureMemoryNV =
     "vkBindAccelerationStructureMemoryNV"

-- | This is an alias for `vkBindAccelerationStructureMemoryKHR`.
--
--   Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindAccelerationStructureMemoryNV
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindAccelerationStructureMemoryInfoKHR* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkBindAccelerationStructureMemoryNV vkBindAccelerationStructureMemoryNV registry at www.khronos.org>
type HS_vkBindAccelerationStructureMemoryNV =
     VkDevice -- ^ device
              ->
       Word32 -- ^ bindInfoCount
              ->
         Ptr VkBindAccelerationStructureMemoryInfoKHR -- ^ pBindInfos
                                                      -> IO VkResult

type PFN_vkBindAccelerationStructureMemoryNV =
     FunPtr HS_vkBindAccelerationStructureMemoryNV

foreign import ccall unsafe "dynamic"
               unwrapVkBindAccelerationStructureMemoryNVUnsafe ::
               PFN_vkBindAccelerationStructureMemoryNV ->
                 HS_vkBindAccelerationStructureMemoryNV

foreign import ccall safe "dynamic"
               unwrapVkBindAccelerationStructureMemoryNVSafe ::
               PFN_vkBindAccelerationStructureMemoryNV ->
                 HS_vkBindAccelerationStructureMemoryNV

instance VulkanProc "vkBindAccelerationStructureMemoryNV" where
    type VkProcType "vkBindAccelerationStructureMemoryNV" =
         HS_vkBindAccelerationStructureMemoryNV
    vkProcSymbol = _VkBindAccelerationStructureMemoryNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkBindAccelerationStructureMemoryNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkBindAccelerationStructureMemoryNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdBuildAccelerationStructureNV :: CString

pattern VkCmdBuildAccelerationStructureNV <-
        (is_VkCmdBuildAccelerationStructureNV -> True)
  where
    VkCmdBuildAccelerationStructureNV
      = _VkCmdBuildAccelerationStructureNV

{-# INLINE _VkCmdBuildAccelerationStructureNV #-}

_VkCmdBuildAccelerationStructureNV :: CString
_VkCmdBuildAccelerationStructureNV
  = Ptr "vkCmdBuildAccelerationStructureNV\NUL"#

{-# INLINE is_VkCmdBuildAccelerationStructureNV #-}

is_VkCmdBuildAccelerationStructureNV :: CString -> Bool
is_VkCmdBuildAccelerationStructureNV
  = (EQ ==) . cmpCStrings _VkCmdBuildAccelerationStructureNV

type VkCmdBuildAccelerationStructureNV =
     "vkCmdBuildAccelerationStructureNV"

-- | Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdBuildAccelerationStructureNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkAccelerationStructureInfoNV* pInfo
--   >     , VkBuffer instanceData
--   >     , VkDeviceSize instanceOffset
--   >     , VkBool32 update
--   >     , VkAccelerationStructureKHR dst
--   >     , VkAccelerationStructureKHR src
--   >     , VkBuffer scratch
--   >     , VkDeviceSize scratchOffset
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBuildAccelerationStructureNV vkCmdBuildAccelerationStructureNV registry at www.khronos.org>
type HS_vkCmdBuildAccelerationStructureNV =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Ptr VkAccelerationStructureInfoNV -- ^ pInfo
                                         ->
         VkBuffer -- ^ instanceData
                  ->
           VkDeviceSize -- ^ instanceOffset
                        ->
             VkBool32 -- ^ update
                      ->
               VkAccelerationStructureKHR -- ^ dst
                                          ->
                 VkAccelerationStructureKHR -- ^ src
                                            -> VkBuffer -- ^ scratch
                                                        -> VkDeviceSize -- ^ scratchOffset
                                                                        -> IO ()

type PFN_vkCmdBuildAccelerationStructureNV =
     FunPtr HS_vkCmdBuildAccelerationStructureNV

foreign import ccall unsafe "dynamic"
               unwrapVkCmdBuildAccelerationStructureNVUnsafe ::
               PFN_vkCmdBuildAccelerationStructureNV ->
                 HS_vkCmdBuildAccelerationStructureNV

foreign import ccall safe "dynamic"
               unwrapVkCmdBuildAccelerationStructureNVSafe ::
               PFN_vkCmdBuildAccelerationStructureNV ->
                 HS_vkCmdBuildAccelerationStructureNV

instance VulkanProc "vkCmdBuildAccelerationStructureNV" where
    type VkProcType "vkCmdBuildAccelerationStructureNV" =
         HS_vkCmdBuildAccelerationStructureNV
    vkProcSymbol = _VkCmdBuildAccelerationStructureNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdBuildAccelerationStructureNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdBuildAccelerationStructureNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdCopyAccelerationStructureNV :: CString

pattern VkCmdCopyAccelerationStructureNV <-
        (is_VkCmdCopyAccelerationStructureNV -> True)
  where
    VkCmdCopyAccelerationStructureNV
      = _VkCmdCopyAccelerationStructureNV

{-# INLINE _VkCmdCopyAccelerationStructureNV #-}

_VkCmdCopyAccelerationStructureNV :: CString
_VkCmdCopyAccelerationStructureNV
  = Ptr "vkCmdCopyAccelerationStructureNV\NUL"#

{-# INLINE is_VkCmdCopyAccelerationStructureNV #-}

is_VkCmdCopyAccelerationStructureNV :: CString -> Bool
is_VkCmdCopyAccelerationStructureNV
  = (EQ ==) . cmpCStrings _VkCmdCopyAccelerationStructureNV

type VkCmdCopyAccelerationStructureNV =
     "vkCmdCopyAccelerationStructureNV"

-- | Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdCopyAccelerationStructureNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkAccelerationStructureKHR dst
--   >     , VkAccelerationStructureKHR src
--   >     , VkCopyAccelerationStructureModeKHR mode
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdCopyAccelerationStructureNV vkCmdCopyAccelerationStructureNV registry at www.khronos.org>
type HS_vkCmdCopyAccelerationStructureNV =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkAccelerationStructureKHR -- ^ dst
                                  ->
         VkAccelerationStructureKHR -- ^ src
                                    ->
           VkCopyAccelerationStructureModeKHR -- ^ mode
                                              -> IO ()

type PFN_vkCmdCopyAccelerationStructureNV =
     FunPtr HS_vkCmdCopyAccelerationStructureNV

foreign import ccall unsafe "dynamic"
               unwrapVkCmdCopyAccelerationStructureNVUnsafe ::
               PFN_vkCmdCopyAccelerationStructureNV ->
                 HS_vkCmdCopyAccelerationStructureNV

foreign import ccall safe "dynamic"
               unwrapVkCmdCopyAccelerationStructureNVSafe ::
               PFN_vkCmdCopyAccelerationStructureNV ->
                 HS_vkCmdCopyAccelerationStructureNV

instance VulkanProc "vkCmdCopyAccelerationStructureNV" where
    type VkProcType "vkCmdCopyAccelerationStructureNV" =
         HS_vkCmdCopyAccelerationStructureNV
    vkProcSymbol = _VkCmdCopyAccelerationStructureNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdCopyAccelerationStructureNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdCopyAccelerationStructureNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdTraceRaysNV :: CString

pattern VkCmdTraceRaysNV <- (is_VkCmdTraceRaysNV -> True)
  where
    VkCmdTraceRaysNV = _VkCmdTraceRaysNV

{-# INLINE _VkCmdTraceRaysNV #-}

_VkCmdTraceRaysNV :: CString
_VkCmdTraceRaysNV = Ptr "vkCmdTraceRaysNV\NUL"#

{-# INLINE is_VkCmdTraceRaysNV #-}

is_VkCmdTraceRaysNV :: CString -> Bool
is_VkCmdTraceRaysNV = (EQ ==) . cmpCStrings _VkCmdTraceRaysNV

type VkCmdTraceRaysNV = "vkCmdTraceRaysNV"

-- | Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdTraceRaysNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer raygenShaderBindingTableBuffer
--   >     , VkDeviceSize raygenShaderBindingOffset
--   >     , VkBuffer missShaderBindingTableBuffer
--   >     , VkDeviceSize missShaderBindingOffset
--   >     , VkDeviceSize missShaderBindingStride
--   >     , VkBuffer hitShaderBindingTableBuffer
--   >     , VkDeviceSize hitShaderBindingOffset
--   >     , VkDeviceSize hitShaderBindingStride
--   >     , VkBuffer callableShaderBindingTableBuffer
--   >     , VkDeviceSize callableShaderBindingOffset
--   >     , VkDeviceSize callableShaderBindingStride
--   >     , uint32_t width
--   >     , uint32_t height
--   >     , uint32_t depth
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdTraceRaysNV vkCmdTraceRaysNV registry at www.khronos.org>
type HS_vkCmdTraceRaysNV =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkBuffer -- ^ raygenShaderBindingTableBuffer
                ->
         VkDeviceSize -- ^ raygenShaderBindingOffset
                      ->
           VkBuffer -- ^ missShaderBindingTableBuffer
                    ->
             VkDeviceSize -- ^ missShaderBindingOffset
                          ->
               VkDeviceSize -- ^ missShaderBindingStride
                            ->
                 VkBuffer -- ^ hitShaderBindingTableBuffer
                          ->
                   VkDeviceSize -- ^ hitShaderBindingOffset
                                ->
                     VkDeviceSize -- ^ hitShaderBindingStride
                                  ->
                       VkBuffer -- ^ callableShaderBindingTableBuffer
                                ->
                         VkDeviceSize -- ^ callableShaderBindingOffset
                                      -> VkDeviceSize -- ^ callableShaderBindingStride
                                                      -> Word32 -- ^ width
                                                                -> Word32 -- ^ height
                                                                          -> Word32 -- ^ depth
                                                                                    -> IO ()

type PFN_vkCmdTraceRaysNV = FunPtr HS_vkCmdTraceRaysNV

foreign import ccall unsafe "dynamic" unwrapVkCmdTraceRaysNVUnsafe
               :: PFN_vkCmdTraceRaysNV -> HS_vkCmdTraceRaysNV

foreign import ccall safe "dynamic" unwrapVkCmdTraceRaysNVSafe ::
               PFN_vkCmdTraceRaysNV -> HS_vkCmdTraceRaysNV

instance VulkanProc "vkCmdTraceRaysNV" where
    type VkProcType "vkCmdTraceRaysNV" = HS_vkCmdTraceRaysNV
    vkProcSymbol = _VkCmdTraceRaysNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdTraceRaysNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdTraceRaysNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCreateRayTracingPipelinesNV :: CString

pattern VkCreateRayTracingPipelinesNV <-
        (is_VkCreateRayTracingPipelinesNV -> True)
  where
    VkCreateRayTracingPipelinesNV = _VkCreateRayTracingPipelinesNV

{-# INLINE _VkCreateRayTracingPipelinesNV #-}

_VkCreateRayTracingPipelinesNV :: CString
_VkCreateRayTracingPipelinesNV
  = Ptr "vkCreateRayTracingPipelinesNV\NUL"#

{-# INLINE is_VkCreateRayTracingPipelinesNV #-}

is_VkCreateRayTracingPipelinesNV :: CString -> Bool
is_VkCreateRayTracingPipelinesNV
  = (EQ ==) . cmpCStrings _VkCreateRayTracingPipelinesNV

type VkCreateRayTracingPipelinesNV =
     "vkCreateRayTracingPipelinesNV"

-- | Success codes: 'VK_SUCCESS', 'VK_PIPELINE_COMPILE_REQUIRED_EXT'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
--   > VkResult vkCreateRayTracingPipelinesNV
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , uint32_t createInfoCount
--   >     , const VkRayTracingPipelineCreateInfoNV* pCreateInfos
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipeline* pPipelines
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCreateRayTracingPipelinesNV vkCreateRayTracingPipelinesNV registry at www.khronos.org>
type HS_vkCreateRayTracingPipelinesNV =
     VkDevice -- ^ device
              ->
       VkPipelineCache -- ^ pipelineCache
                       ->
         Word32 -- ^ createInfoCount
                ->
           Ptr VkRayTracingPipelineCreateInfoNV -- ^ pCreateInfos
                                                ->
             Ptr VkAllocationCallbacks -- ^ pAllocator
                                       -> Ptr VkPipeline -- ^ pPipelines
                                                         -> IO VkResult

type PFN_vkCreateRayTracingPipelinesNV =
     FunPtr HS_vkCreateRayTracingPipelinesNV

foreign import ccall unsafe "dynamic"
               unwrapVkCreateRayTracingPipelinesNVUnsafe ::
               PFN_vkCreateRayTracingPipelinesNV ->
                 HS_vkCreateRayTracingPipelinesNV

foreign import ccall safe "dynamic"
               unwrapVkCreateRayTracingPipelinesNVSafe ::
               PFN_vkCreateRayTracingPipelinesNV ->
                 HS_vkCreateRayTracingPipelinesNV

instance VulkanProc "vkCreateRayTracingPipelinesNV" where
    type VkProcType "vkCreateRayTracingPipelinesNV" =
         HS_vkCreateRayTracingPipelinesNV
    vkProcSymbol = _VkCreateRayTracingPipelinesNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateRayTracingPipelinesNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateRayTracingPipelinesNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetRayTracingShaderGroupHandlesNV :: CString

pattern VkGetRayTracingShaderGroupHandlesNV <-
        (is_VkGetRayTracingShaderGroupHandlesNV -> True)
  where
    VkGetRayTracingShaderGroupHandlesNV
      = _VkGetRayTracingShaderGroupHandlesNV

{-# INLINE _VkGetRayTracingShaderGroupHandlesNV #-}

_VkGetRayTracingShaderGroupHandlesNV :: CString
_VkGetRayTracingShaderGroupHandlesNV
  = Ptr "vkGetRayTracingShaderGroupHandlesNV\NUL"#

{-# INLINE is_VkGetRayTracingShaderGroupHandlesNV #-}

is_VkGetRayTracingShaderGroupHandlesNV :: CString -> Bool
is_VkGetRayTracingShaderGroupHandlesNV
  = (EQ ==) . cmpCStrings _VkGetRayTracingShaderGroupHandlesNV

type VkGetRayTracingShaderGroupHandlesNV =
     "vkGetRayTracingShaderGroupHandlesNV"

-- | This is an alias for `vkGetRayTracingShaderGroupHandlesKHR`.
--
--   Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetRayTracingShaderGroupHandlesNV
--   >     ( VkDevice device
--   >     , VkPipeline pipeline
--   >     , uint32_t firstGroup
--   >     , uint32_t groupCount
--   >     , size_t dataSize
--   >     , void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetRayTracingShaderGroupHandlesNV vkGetRayTracingShaderGroupHandlesNV registry at www.khronos.org>
type HS_vkGetRayTracingShaderGroupHandlesNV =
     VkDevice -- ^ device
              ->
       VkPipeline -- ^ pipeline
                  -> Word32 -- ^ firstGroup
                            -> Word32 -- ^ groupCount
                                      -> CSize -- ^ dataSize
                                               -> Ptr Void -- ^ pData
                                                           -> IO VkResult

type PFN_vkGetRayTracingShaderGroupHandlesNV =
     FunPtr HS_vkGetRayTracingShaderGroupHandlesNV

foreign import ccall unsafe "dynamic"
               unwrapVkGetRayTracingShaderGroupHandlesNVUnsafe ::
               PFN_vkGetRayTracingShaderGroupHandlesNV ->
                 HS_vkGetRayTracingShaderGroupHandlesNV

foreign import ccall safe "dynamic"
               unwrapVkGetRayTracingShaderGroupHandlesNVSafe ::
               PFN_vkGetRayTracingShaderGroupHandlesNV ->
                 HS_vkGetRayTracingShaderGroupHandlesNV

instance VulkanProc "vkGetRayTracingShaderGroupHandlesNV" where
    type VkProcType "vkGetRayTracingShaderGroupHandlesNV" =
         HS_vkGetRayTracingShaderGroupHandlesNV
    vkProcSymbol = _VkGetRayTracingShaderGroupHandlesNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetRayTracingShaderGroupHandlesNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetRayTracingShaderGroupHandlesNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetAccelerationStructureHandleNV :: CString

pattern VkGetAccelerationStructureHandleNV <-
        (is_VkGetAccelerationStructureHandleNV -> True)
  where
    VkGetAccelerationStructureHandleNV
      = _VkGetAccelerationStructureHandleNV

{-# INLINE _VkGetAccelerationStructureHandleNV #-}

_VkGetAccelerationStructureHandleNV :: CString
_VkGetAccelerationStructureHandleNV
  = Ptr "vkGetAccelerationStructureHandleNV\NUL"#

{-# INLINE is_VkGetAccelerationStructureHandleNV #-}

is_VkGetAccelerationStructureHandleNV :: CString -> Bool
is_VkGetAccelerationStructureHandleNV
  = (EQ ==) . cmpCStrings _VkGetAccelerationStructureHandleNV

type VkGetAccelerationStructureHandleNV =
     "vkGetAccelerationStructureHandleNV"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetAccelerationStructureHandleNV
--   >     ( VkDevice device
--   >     , VkAccelerationStructureKHR accelerationStructure
--   >     , size_t dataSize
--   >     , void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetAccelerationStructureHandleNV vkGetAccelerationStructureHandleNV registry at www.khronos.org>
type HS_vkGetAccelerationStructureHandleNV =
     VkDevice -- ^ device
              ->
       VkAccelerationStructureKHR -- ^ accelerationStructure
                                  -> CSize -- ^ dataSize
                                           -> Ptr Void -- ^ pData
                                                       -> IO VkResult

type PFN_vkGetAccelerationStructureHandleNV =
     FunPtr HS_vkGetAccelerationStructureHandleNV

foreign import ccall unsafe "dynamic"
               unwrapVkGetAccelerationStructureHandleNVUnsafe ::
               PFN_vkGetAccelerationStructureHandleNV ->
                 HS_vkGetAccelerationStructureHandleNV

foreign import ccall safe "dynamic"
               unwrapVkGetAccelerationStructureHandleNVSafe ::
               PFN_vkGetAccelerationStructureHandleNV ->
                 HS_vkGetAccelerationStructureHandleNV

instance VulkanProc "vkGetAccelerationStructureHandleNV" where
    type VkProcType "vkGetAccelerationStructureHandleNV" =
         HS_vkGetAccelerationStructureHandleNV
    vkProcSymbol = _VkGetAccelerationStructureHandleNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetAccelerationStructureHandleNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetAccelerationStructureHandleNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdWriteAccelerationStructuresPropertiesNV :: CString

pattern VkCmdWriteAccelerationStructuresPropertiesNV <-
        (is_VkCmdWriteAccelerationStructuresPropertiesNV -> True)
  where
    VkCmdWriteAccelerationStructuresPropertiesNV
      = _VkCmdWriteAccelerationStructuresPropertiesNV

{-# INLINE _VkCmdWriteAccelerationStructuresPropertiesNV #-}

_VkCmdWriteAccelerationStructuresPropertiesNV :: CString
_VkCmdWriteAccelerationStructuresPropertiesNV
  = Ptr "vkCmdWriteAccelerationStructuresPropertiesNV\NUL"#

{-# INLINE is_VkCmdWriteAccelerationStructuresPropertiesNV #-}

is_VkCmdWriteAccelerationStructuresPropertiesNV :: CString -> Bool
is_VkCmdWriteAccelerationStructuresPropertiesNV
  = (EQ ==) .
      cmpCStrings _VkCmdWriteAccelerationStructuresPropertiesNV

type VkCmdWriteAccelerationStructuresPropertiesNV =
     "vkCmdWriteAccelerationStructuresPropertiesNV"

-- | This is an alias for `vkCmdWriteAccelerationStructuresPropertiesKHR`.
--
--   Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdWriteAccelerationStructuresPropertiesNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t accelerationStructureCount
--   >     , const VkAccelerationStructureKHR* pAccelerationStructures
--   >     , VkQueryType queryType
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdWriteAccelerationStructuresPropertiesNV vkCmdWriteAccelerationStructuresPropertiesNV registry at www.khronos.org>
type HS_vkCmdWriteAccelerationStructuresPropertiesNV =
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

type PFN_vkCmdWriteAccelerationStructuresPropertiesNV =
     FunPtr HS_vkCmdWriteAccelerationStructuresPropertiesNV

foreign import ccall unsafe "dynamic"
               unwrapVkCmdWriteAccelerationStructuresPropertiesNVUnsafe ::
               PFN_vkCmdWriteAccelerationStructuresPropertiesNV ->
                 HS_vkCmdWriteAccelerationStructuresPropertiesNV

foreign import ccall safe "dynamic"
               unwrapVkCmdWriteAccelerationStructuresPropertiesNVSafe ::
               PFN_vkCmdWriteAccelerationStructuresPropertiesNV ->
                 HS_vkCmdWriteAccelerationStructuresPropertiesNV

instance VulkanProc "vkCmdWriteAccelerationStructuresPropertiesNV"
         where
    type VkProcType "vkCmdWriteAccelerationStructuresPropertiesNV" =
         HS_vkCmdWriteAccelerationStructuresPropertiesNV
    vkProcSymbol = _VkCmdWriteAccelerationStructuresPropertiesNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdWriteAccelerationStructuresPropertiesNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkCmdWriteAccelerationStructuresPropertiesNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCompileDeferredNV :: CString

pattern VkCompileDeferredNV <- (is_VkCompileDeferredNV -> True)
  where
    VkCompileDeferredNV = _VkCompileDeferredNV

{-# INLINE _VkCompileDeferredNV #-}

_VkCompileDeferredNV :: CString
_VkCompileDeferredNV = Ptr "vkCompileDeferredNV\NUL"#

{-# INLINE is_VkCompileDeferredNV #-}

is_VkCompileDeferredNV :: CString -> Bool
is_VkCompileDeferredNV = (EQ ==) . cmpCStrings _VkCompileDeferredNV

type VkCompileDeferredNV = "vkCompileDeferredNV"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCompileDeferredNV
--   >     ( VkDevice device
--   >     , VkPipeline pipeline
--   >     , uint32_t shader
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCompileDeferredNV vkCompileDeferredNV registry at www.khronos.org>
type HS_vkCompileDeferredNV =
     VkDevice -- ^ device
              -> VkPipeline -- ^ pipeline
                            -> Word32 -- ^ shader
                                      -> IO VkResult

type PFN_vkCompileDeferredNV = FunPtr HS_vkCompileDeferredNV

foreign import ccall unsafe "dynamic"
               unwrapVkCompileDeferredNVUnsafe ::
               PFN_vkCompileDeferredNV -> HS_vkCompileDeferredNV

foreign import ccall safe "dynamic" unwrapVkCompileDeferredNVSafe
               :: PFN_vkCompileDeferredNV -> HS_vkCompileDeferredNV

instance VulkanProc "vkCompileDeferredNV" where
    type VkProcType "vkCompileDeferredNV" = HS_vkCompileDeferredNV
    vkProcSymbol = _VkCompileDeferredNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCompileDeferredNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCompileDeferredNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_NV_RAY_TRACING_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_RAY_TRACING_SPEC_VERSION = 3

type VK_NV_RAY_TRACING_SPEC_VERSION = 3

pattern VK_NV_RAY_TRACING_EXTENSION_NAME :: CString

pattern VK_NV_RAY_TRACING_EXTENSION_NAME <-
        (is_VK_NV_RAY_TRACING_EXTENSION_NAME -> True)
  where
    VK_NV_RAY_TRACING_EXTENSION_NAME
      = _VK_NV_RAY_TRACING_EXTENSION_NAME

{-# INLINE _VK_NV_RAY_TRACING_EXTENSION_NAME #-}

_VK_NV_RAY_TRACING_EXTENSION_NAME :: CString
_VK_NV_RAY_TRACING_EXTENSION_NAME = Ptr "VK_NV_ray_tracing\NUL"#

{-# INLINE is_VK_NV_RAY_TRACING_EXTENSION_NAME #-}

is_VK_NV_RAY_TRACING_EXTENSION_NAME :: CString -> Bool
is_VK_NV_RAY_TRACING_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_NV_RAY_TRACING_EXTENSION_NAME

type VK_NV_RAY_TRACING_EXTENSION_NAME = "VK_NV_ray_tracing"

pattern VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV =
        VkStructureType 1000165000

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV =
        VkStructureType 1000165001

pattern VK_STRUCTURE_TYPE_GEOMETRY_NV :: VkStructureType

pattern VK_STRUCTURE_TYPE_GEOMETRY_NV = VkStructureType 1000165003

pattern VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV :: VkStructureType

pattern VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV =
        VkStructureType 1000165004

pattern VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV :: VkStructureType

pattern VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV =
        VkStructureType 1000165005

pattern VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV
        = VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR

pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV
        = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV
        = VkStructureType 1000165008

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV
        = VkStructureType 1000165009

pattern VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV =
        VkStructureType 1000165011

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV =
        VkStructureType 1000165012

pattern VK_SHADER_STAGE_RAYGEN_BIT_NV =
        VK_SHADER_STAGE_RAYGEN_BIT_KHR

pattern VK_SHADER_STAGE_ANY_HIT_BIT_NV =
        VK_SHADER_STAGE_ANY_HIT_BIT_KHR

pattern VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV =
        VK_SHADER_STAGE_CLOSEST_HIT_BIT_KHR

pattern VK_SHADER_STAGE_MISS_BIT_NV = VK_SHADER_STAGE_MISS_BIT_KHR

pattern VK_SHADER_STAGE_INTERSECTION_BIT_NV =
        VK_SHADER_STAGE_INTERSECTION_BIT_KHR

pattern VK_SHADER_STAGE_CALLABLE_BIT_NV =
        VK_SHADER_STAGE_CALLABLE_BIT_KHR

pattern VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV =
        VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR

pattern VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV =
        VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR

pattern VK_BUFFER_USAGE_RAY_TRACING_BIT_NV =
        VK_BUFFER_USAGE_RAY_TRACING_BIT_KHR

pattern VK_PIPELINE_BIND_POINT_RAY_TRACING_NV =
        VK_PIPELINE_BIND_POINT_RAY_TRACING_KHR

pattern VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV =
        VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR

pattern VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV =
        VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR

pattern VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV =
        VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR

pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV =
        VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR

-- | bitpos = @5@
pattern VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV ::
        VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV =
        VkPipelineCreateBitmask 32

pattern VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV =
        VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR

pattern VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT =
        VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT

pattern VK_INDEX_TYPE_NONE_NV = VK_INDEX_TYPE_NONE_KHR

pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV =
        VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR

pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV =
        VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR

pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV =
        VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR

pattern VK_GEOMETRY_TYPE_TRIANGLES_NV =
        VK_GEOMETRY_TYPE_TRIANGLES_KHR

pattern VK_GEOMETRY_TYPE_AABBS_NV = VK_GEOMETRY_TYPE_AABBS_KHR

pattern VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV =
        VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR

pattern VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV =
        VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR

pattern VK_GEOMETRY_OPAQUE_BIT_NV = VK_GEOMETRY_OPAQUE_BIT_KHR

pattern VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV =
        VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR

pattern VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV =
        VK_GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR

pattern VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV
        = VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR

pattern VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV =
        VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR

pattern VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV =
        VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR

pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV =
        VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR

pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV =
        VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR

pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV =
        VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR

pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV =
        VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR

pattern VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV =
        VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR

pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV =
        VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR

pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV =
        VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR

pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV
        = VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR

pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV
        =
        VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR

pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV
        =
        VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR
