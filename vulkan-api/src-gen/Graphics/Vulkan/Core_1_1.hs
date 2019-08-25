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
        vkEnumerateInstanceVersion, vkEnumerateInstanceVersionUnsafe,
        vkEnumerateInstanceVersionSafe, VkResult(..),
        -- ** Promoted from VK_KHR_relaxed_block_layout, which has no API
        --

        -- ** Promoted from VK_KHR_storage_buffer_storage_class, which has no API
        --

        -- ** Originally based on VK_KHR_subgroup (extension 94), but the actual enum block used was, incorrectly, that of extension 95
        module Graphics.Vulkan.Marshal, VkBool32(..), VkDeviceSize(..),
        VkFlags(..), VkSampleMask(..), VkPhysicalDeviceLimits,
        VkPhysicalDeviceProperties, VkPhysicalDeviceProperties2,
        VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceSubgroupProperties, VkPhysicalDeviceType(..),
        VkSampleCountBitmask(..), VkSampleCountFlagBits(),
        VkSampleCountFlags(), VkShaderInfoTypeAMD(..),
        VkShaderStageBitmask(..), VkShaderStageFlagBits(),
        VkShaderStageFlags(), VkStructureType(..),
        VkSubgroupFeatureBitmask(..), VkSubgroupFeatureFlagBits(),
        VkSubgroupFeatureFlags(),
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES,
        -- ** Promoted from VK_KHR_bind_memory2
        VkBindBufferMemoryInfo, VkBindImageMemoryInfo, -- > #include "vk_platform.h"
                                                       VkBindBufferMemory2,
        pattern VkBindBufferMemory2, HS_vkBindBufferMemory2,
        PFN_vkBindBufferMemory2, vkBindBufferMemory2,
        vkBindBufferMemory2Unsafe, vkBindBufferMemory2Safe,
        VkBindImageMemory2, pattern VkBindImageMemory2,
        HS_vkBindImageMemory2, PFN_vkBindImageMemory2, vkBindImageMemory2,
        vkBindImageMemory2Unsafe, vkBindImageMemory2Safe, VkBuffer,
        VkBufferView, VkBufferView_T(), VkBuffer_T(), VkCommandBuffer,
        VkCommandBuffer_T(), VkCommandPool, VkCommandPool_T(),
        VkDebugReportCallbackEXT, VkDebugReportCallbackEXT_T(),
        VkDebugUtilsMessengerEXT, VkDebugUtilsMessengerEXT_T(),
        VkDescriptorPool, VkDescriptorPool_T(), VkDescriptorSet,
        VkDescriptorSetLayout, VkDescriptorSetLayout_T(),
        VkDescriptorSet_T(), VkDescriptorUpdateTemplate,
        VkDescriptorUpdateTemplateKHR, VkDescriptorUpdateTemplateKHR_T(),
        VkDescriptorUpdateTemplate_T(), VkDevice, VkDeviceMemory,
        VkDeviceMemory_T(), VkDevice_T(), VkDisplayKHR, VkDisplayKHR_T(),
        VkDisplayModeKHR, VkDisplayModeKHR_T(), VkEvent, VkEvent_T(),
        VkFence, VkFence_T(), VkFramebuffer, VkFramebuffer_T(), VkImage,
        VkImageView, VkImageView_T(), VkImage_T(),
        VkIndirectCommandsLayoutNVX, VkIndirectCommandsLayoutNVX_T(),
        VkInstance, VkInstance_T(), VkObjectTableNVX, VkObjectTableNVX_T(),
        VkPhysicalDevice, VkPhysicalDevice_T(), VkPipeline,
        VkPipelineCache, VkPipelineCache_T(), VkPipelineLayout,
        VkPipelineLayout_T(), VkPipeline_T(), VkQueryPool, VkQueryPool_T(),
        VkQueue, VkQueue_T(), VkRenderPass, VkRenderPass_T(), VkSampler,
        VkSamplerYcbcrConversion, VkSamplerYcbcrConversionKHR,
        VkSamplerYcbcrConversionKHR_T(), VkSamplerYcbcrConversion_T(),
        VkSampler_T(), VkSemaphore, VkSemaphore_T(), VkShaderModule,
        VkShaderModule_T(), VkSurfaceKHR, VkSurfaceKHR_T(), VkSwapchainKHR,
        VkSwapchainKHR_T(), VkValidationCacheEXT, VkValidationCacheEXT_T(),
        VkBindBufferMemoryDeviceGroupInfo,
        VkBindBufferMemoryDeviceGroupInfoKHR, VkBindBufferMemoryInfoKHR,
        VkBindImageMemoryDeviceGroupInfo,
        VkBindImageMemoryDeviceGroupInfoKHR, VkBindImageMemoryInfoKHR,
        VkBindImageMemorySwapchainInfoKHR, VkBindImagePlaneMemoryInfo,
        VkBindImagePlaneMemoryInfoKHR, VkBindSparseInfo,
        pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO,
        pattern VK_IMAGE_CREATE_ALIAS_BIT,
        VkAndroidSurfaceCreateFlagsKHR(..), VkBufferViewCreateFlags(..),
        VkCommandPoolTrimFlags(..), VkCommandPoolTrimFlagsKHR(..),
        VkDebugUtilsMessengerCallbackDataFlagsEXT(..),
        VkDebugUtilsMessengerCreateFlagsEXT(..),
        VkDescriptorPoolResetFlags(..),
        VkDescriptorUpdateTemplateCreateFlags(..),
        VkDescriptorUpdateTemplateCreateFlagsKHR(..),
        VkDeviceCreateFlags(..), VkDisplayModeCreateFlagsKHR(..),
        VkDisplaySurfaceCreateFlagsKHR(..), VkEventCreateFlags(..),
        VkExternalFenceFeatureFlagsKHR(..),
        VkExternalFenceHandleTypeFlagsKHR(..),
        VkExternalMemoryFeatureFlagsKHR(..),
        VkExternalMemoryHandleTypeFlagsKHR(..),
        VkExternalSemaphoreFeatureFlagsKHR(..),
        VkExternalSemaphoreHandleTypeFlagsKHR(..),
        VkFenceImportFlagsKHR(..), VkFramebufferCreateFlags(..),
        VkIOSSurfaceCreateFlagsMVK(..), VkImageViewCreateFlags(..),
        VkInstanceCreateFlags(..), VkMacOSSurfaceCreateFlagsMVK(..),
        VkMemoryAllocateFlagsKHR(..), VkMemoryMapFlags(..),
        VkMirSurfaceCreateFlagsKHR(..), VkPeerMemoryFeatureFlagsKHR(..),
        VkPipelineCacheCreateFlags(..),
        VkPipelineColorBlendStateCreateFlags(..),
        VkPipelineCoverageModulationStateCreateFlagsNV(..),
        VkPipelineCoverageToColorStateCreateFlagsNV(..),
        VkPipelineDepthStencilStateCreateFlags(..),
        VkPipelineDiscardRectangleStateCreateFlagsEXT(..),
        VkPipelineDynamicStateCreateFlags(..),
        VkPipelineInputAssemblyStateCreateFlags(..),
        VkPipelineLayoutCreateFlags(..),
        VkPipelineMultisampleStateCreateFlags(..),
        VkPipelineRasterizationConservativeStateCreateFlagsEXT(..),
        VkPipelineRasterizationStateCreateFlags(..),
        VkPipelineShaderStageCreateFlags(..),
        VkPipelineTessellationStateCreateFlags(..),
        VkPipelineVertexInputStateCreateFlags(..),
        VkPipelineViewportStateCreateFlags(..),
        VkPipelineViewportSwizzleStateCreateFlagsNV(..),
        VkQueryPoolCreateFlags(..), VkRenderPassCreateFlags(..),
        VkSamplerCreateFlags(..), VkSemaphoreCreateFlags(..),
        VkSemaphoreImportFlagsKHR(..), VkShaderModuleCreateFlags(..),
        VkValidationCacheCreateFlagsEXT(..), VkViSurfaceCreateFlagsNN(..),
        VkWaylandSurfaceCreateFlagsKHR(..),
        VkWin32SurfaceCreateFlagsKHR(..), VkXcbSurfaceCreateFlagsKHR(..),
        VkXlibSurfaceCreateFlagsKHR(..), VkDeviceCreateInfo,
        VkDeviceEventTypeEXT(..), VkDeviceGroupPresentModeBitmaskKHR(..),
        VkDeviceCreateFlagBits(..), VkDeviceGroupPresentModeFlagBitsKHR(),
        VkDeviceGroupPresentModeFlagsKHR(), VkDeviceQueueCreateBitmask(..),
        VkDeviceQueueCreateFlagBits(), VkDeviceQueueCreateFlags(),
        VkDeviceQueueCreateInfo, VkPhysicalDevice16BitStorageFeatures,
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES,
        -- ** Promoted from VK_KHR_dedicated_allocation
        VkMemoryAllocateInfo, VkMemoryDedicatedAllocateInfo,
        VkMemoryDedicatedRequirements, VkMemoryRequirements,
        VkMemoryRequirements2,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS,
        pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO,
        VkClearColorValue, VkClearDepthStencilValue, VkClearValue,
        VkCommandBufferBeginInfo, VkCommandBufferInheritanceInfo,
        VkCommandBufferLevel(..), VkCommandBufferResetBitmask(..),
        VkCommandBufferUsageBitmask(..), VkCommandPoolCreateBitmask(..),
        VkCommandPoolResetBitmask(..), VkCommandBufferResetFlagBits(),
        VkCommandBufferResetFlags(), VkCommandBufferUsageFlagBits(),
        VkCommandBufferUsageFlags(), VkCommandPoolCreateFlagBits(),
        VkCommandPoolCreateFlags(), VkCommandPoolResetFlagBits(),
        VkCommandPoolResetFlags(), VkDeviceGroupBindSparseInfo,
        VkDeviceGroupCommandBufferBeginInfo,
        VkDeviceGroupRenderPassBeginInfo, VkDeviceGroupSubmitInfo,
        VkExtent2D, VkExtent3D, VkImageAspectBitmask(..),
        VkImageCreateBitmask(..), VkImageLayout(..), VkImageTiling(..),
        VkImageType(..), VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(), VkImageSubresource,
        VkMemoryAllocateBitmask(..), VkMemoryHeapBitmask(..),
        VkMemoryPropertyBitmask(..), VkMemoryAllocateFlagBits(),
        VkMemoryAllocateFlagBitsKHR(..), VkMemoryAllocateFlags(),
        VkMemoryHeapFlagBits(), VkMemoryHeapFlags(),
        VkMemoryPropertyFlagBits(), VkMemoryPropertyFlags(),
        VkMemoryAllocateFlagsInfo, VkOffset2D, VkOffset3D,
        VkPeerMemoryFeatureBitmask(..), VkPeerMemoryFeatureFlagBits(),
        VkPeerMemoryFeatureFlagBitsKHR(..), VkPeerMemoryFeatureFlags(),
        VkPipelineBindPoint(..), VkPipelineCacheHeaderVersion(..),
        VkPipelineCreateBitmask(..), VkPipelineStageBitmask(..),
        VkPipelineCacheCreateFlagBits(..),
        VkPipelineColorBlendStateCreateFlagBits(..),
        VkPipelineCreateFlagBits(), VkPipelineCreateFlags(),
        VkPipelineDepthStencilStateCreateFlagBits(..),
        VkPipelineDynamicStateCreateFlagBits(..),
        VkPipelineInputAssemblyStateCreateFlagBits(..),
        VkPipelineLayoutCreateFlagBits(..),
        VkPipelineMultisampleStateCreateFlagBits(..),
        VkPipelineRasterizationStateCreateFlagBits(..),
        VkPipelineShaderStageCreateFlagBits(..), VkPipelineStageFlagBits(),
        VkPipelineStageFlags(),
        VkPipelineTessellationStateCreateFlagBits(..),
        VkPipelineVertexInputStateCreateFlagBits(..),
        VkPipelineViewportStateCreateFlagBits(..),
        VkQueryControlBitmask(..), VkQueryPipelineStatisticBitmask(..),
        VkQueryResultBitmask(..), VkQueryType(..),
        VkQueryControlFlagBits(), VkQueryControlFlags(),
        VkQueryPipelineStatisticFlagBits(),
        VkQueryPipelineStatisticFlags(), VkQueryPoolCreateFlagBits(..),
        VkQueryResultFlagBits(), VkQueryResultFlags(), VkRect2D,
        VkRenderPassBeginInfo, VkSparseBufferMemoryBindInfo,
        VkSparseImageMemoryBind, VkSparseImageMemoryBindInfo,
        VkSparseImageOpaqueMemoryBindInfo, VkSparseMemoryBind,
        VkSparseImageFormatBitmask(..), VkSparseMemoryBindBitmask(..),
        VkSparseImageFormatFlagBits(), VkSparseImageFormatFlags(),
        VkSparseMemoryBindFlagBits(), VkSparseMemoryBindFlags(),
        VkSubmitInfo, -- > #include "vk_platform.h"
                      VkGetDeviceGroupPeerMemoryFeatures,
        pattern VkGetDeviceGroupPeerMemoryFeatures,
        HS_vkGetDeviceGroupPeerMemoryFeatures,
        PFN_vkGetDeviceGroupPeerMemoryFeatures,
        vkGetDeviceGroupPeerMemoryFeatures,
        vkGetDeviceGroupPeerMemoryFeaturesUnsafe,
        vkGetDeviceGroupPeerMemoryFeaturesSafe, VkCmdSetDeviceMask,
        pattern VkCmdSetDeviceMask, HS_vkCmdSetDeviceMask,
        PFN_vkCmdSetDeviceMask, vkCmdSetDeviceMask,
        vkCmdSetDeviceMaskUnsafe, vkCmdSetDeviceMaskSafe,
        VkCmdDispatchBase, pattern VkCmdDispatchBase, HS_vkCmdDispatchBase,
        PFN_vkCmdDispatchBase, vkCmdDispatchBase, vkCmdDispatchBaseUnsafe,
        vkCmdDispatchBaseSafe,
        pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO,
        pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT,
        pattern VK_PIPELINE_CREATE_DISPATCH_BASE,
        pattern VK_DEPENDENCY_DEVICE_GROUP_BIT,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO,
        pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT,
        VkDeviceGroupDeviceCreateInfo, VkPhysicalDeviceGroupProperties,
        -- > #include "vk_platform.h"
        VkEnumeratePhysicalDeviceGroups,
        pattern VkEnumeratePhysicalDeviceGroups,
        HS_vkEnumeratePhysicalDeviceGroups,
        PFN_vkEnumeratePhysicalDeviceGroups,
        vkEnumeratePhysicalDeviceGroups,
        vkEnumeratePhysicalDeviceGroupsUnsafe,
        vkEnumeratePhysicalDeviceGroupsSafe,
        VkPhysicalDevice16BitStorageFeaturesKHR,
        VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT,
        VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT,
        VkPhysicalDeviceConservativeRasterizationPropertiesEXT,
        VkPhysicalDeviceDescriptorIndexingFeaturesEXT,
        VkPhysicalDeviceDescriptorIndexingPropertiesEXT,
        VkPhysicalDeviceDiscardRectanglePropertiesEXT,
        VkPhysicalDeviceExternalBufferInfo,
        VkPhysicalDeviceExternalBufferInfoKHR,
        VkPhysicalDeviceExternalFenceInfo,
        VkPhysicalDeviceExternalFenceInfoKHR,
        VkPhysicalDeviceExternalImageFormatInfo,
        VkPhysicalDeviceExternalImageFormatInfoKHR,
        VkPhysicalDeviceExternalMemoryHostPropertiesEXT,
        VkPhysicalDeviceExternalSemaphoreInfo,
        VkPhysicalDeviceExternalSemaphoreInfoKHR,
        VkPhysicalDeviceFeatures2KHR, VkPhysicalDeviceGroupPropertiesKHR,
        VkPhysicalDeviceIDProperties, VkPhysicalDeviceIDPropertiesKHR,
        VkPhysicalDeviceImageFormatInfo2,
        VkPhysicalDeviceImageFormatInfo2KHR,
        VkPhysicalDeviceMaintenance3Properties,
        VkPhysicalDeviceMaintenance3PropertiesKHR,
        VkPhysicalDeviceMemoryProperties,
        VkPhysicalDeviceMemoryProperties2,
        VkPhysicalDeviceMemoryProperties2KHR,
        VkPhysicalDeviceMultiviewFeatures,
        VkPhysicalDeviceMultiviewFeaturesKHR,
        VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX,
        VkPhysicalDeviceMultiviewProperties,
        VkPhysicalDeviceMultiviewPropertiesKHR,
        VkPhysicalDevicePointClippingProperties,
        VkPhysicalDevicePointClippingPropertiesKHR,
        VkPhysicalDeviceProperties2KHR,
        VkPhysicalDeviceProtectedMemoryFeatures,
        VkPhysicalDeviceProtectedMemoryProperties,
        VkPhysicalDevicePushDescriptorPropertiesKHR,
        VkPhysicalDeviceSampleLocationsPropertiesEXT,
        VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT,
        VkPhysicalDeviceSamplerYcbcrConversionFeatures,
        VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR,
        VkPhysicalDeviceShaderCorePropertiesAMD,
        VkPhysicalDeviceShaderDrawParameterFeatures,
        VkPhysicalDeviceSparseImageFormatInfo2,
        VkPhysicalDeviceSparseImageFormatInfo2KHR,
        VkPhysicalDeviceSurfaceInfo2KHR,
        VkPhysicalDeviceVariablePointerFeatures,
        VkPhysicalDeviceVariablePointerFeaturesKHR,
        VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO,
        pattern VK_MAX_DEVICE_GROUP_SIZE,
        pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT,
        -- ** Promoted from VK_KHR_get_memory_requirements2
        VkBufferMemoryRequirementsInfo2, VkImageMemoryRequirementsInfo2,
        VkImageSparseMemoryRequirementsInfo2,
        VkSparseImageFormatProperties, VkSparseImageMemoryRequirements,
        VkSparseImageMemoryRequirements2, -- > #include "vk_platform.h"
                                          VkGetImageMemoryRequirements2,
        pattern VkGetImageMemoryRequirements2,
        HS_vkGetImageMemoryRequirements2,
        PFN_vkGetImageMemoryRequirements2, vkGetImageMemoryRequirements2,
        vkGetImageMemoryRequirements2Unsafe,
        vkGetImageMemoryRequirements2Safe, VkGetBufferMemoryRequirements2,
        pattern VkGetBufferMemoryRequirements2,
        HS_vkGetBufferMemoryRequirements2,
        PFN_vkGetBufferMemoryRequirements2, vkGetBufferMemoryRequirements2,
        vkGetBufferMemoryRequirements2Unsafe,
        vkGetBufferMemoryRequirements2Safe,
        VkGetImageSparseMemoryRequirements2,
        pattern VkGetImageSparseMemoryRequirements2,
        HS_vkGetImageSparseMemoryRequirements2,
        PFN_vkGetImageSparseMemoryRequirements2,
        vkGetImageSparseMemoryRequirements2,
        vkGetImageSparseMemoryRequirements2Unsafe,
        vkGetImageSparseMemoryRequirements2Safe, VkBufferCopy,
        VkBufferCreateInfo, VkBufferImageCopy, VkBufferMemoryBarrier,
        VkBufferMemoryRequirementsInfo2KHR, VkBufferViewCreateInfo,
        VkImageBlit, VkImageCopy, VkImageCreateInfo,
        VkImageFormatListCreateInfoKHR, VkImageFormatProperties,
        VkImageFormatProperties2, VkImageFormatProperties2KHR,
        VkImageMemoryBarrier, VkImageMemoryRequirementsInfo2KHR,
        VkImagePlaneMemoryRequirementsInfo,
        VkImagePlaneMemoryRequirementsInfoKHR, VkImageResolve,
        VkImageSparseMemoryRequirementsInfo2KHR, VkImageSubresourceLayers,
        VkImageSubresourceRange, VkImageSwapchainCreateInfoKHR,
        VkImageViewCreateInfo, VkImageViewUsageCreateInfo,
        VkImageViewUsageCreateInfoKHR, VkMemoryAllocateFlagsInfoKHR,
        VkMemoryBarrier, VkMemoryDedicatedAllocateInfoKHR,
        VkMemoryDedicatedRequirementsKHR, VkMemoryFdPropertiesKHR,
        VkMemoryGetFdInfoKHR, VkMemoryHeap,
        VkMemoryHostPointerPropertiesEXT, VkMemoryRequirements2KHR,
        VkMemoryType, VkSparseImageFormatProperties2,
        VkSparseImageFormatProperties2KHR,
        VkSparseImageMemoryRequirements2KHR,
        pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2,
        pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2,
        pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2,
        pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2,
        pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2,
        VkFormat(..), VkFormatFeatureBitmask(..),
        VkFormatFeatureFlagBits(), VkFormatFeatureFlags(),
        VkFormatProperties, VkFormatProperties2, VkQueueFamilyProperties,
        VkQueueFamilyProperties2, VkQueueBitmask(..),
        VkQueueGlobalPriorityEXT(..), VkQueueFlagBits(), VkQueueFlags(),
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceFeatures2, pattern VkGetPhysicalDeviceFeatures2,
        HS_vkGetPhysicalDeviceFeatures2, PFN_vkGetPhysicalDeviceFeatures2,
        vkGetPhysicalDeviceFeatures2, vkGetPhysicalDeviceFeatures2Unsafe,
        vkGetPhysicalDeviceFeatures2Safe, VkGetPhysicalDeviceProperties2,
        pattern VkGetPhysicalDeviceProperties2,
        HS_vkGetPhysicalDeviceProperties2,
        PFN_vkGetPhysicalDeviceProperties2, vkGetPhysicalDeviceProperties2,
        vkGetPhysicalDeviceProperties2Unsafe,
        vkGetPhysicalDeviceProperties2Safe,
        VkGetPhysicalDeviceFormatProperties2,
        pattern VkGetPhysicalDeviceFormatProperties2,
        HS_vkGetPhysicalDeviceFormatProperties2,
        PFN_vkGetPhysicalDeviceFormatProperties2,
        vkGetPhysicalDeviceFormatProperties2,
        vkGetPhysicalDeviceFormatProperties2Unsafe,
        vkGetPhysicalDeviceFormatProperties2Safe,
        VkGetPhysicalDeviceImageFormatProperties2,
        pattern VkGetPhysicalDeviceImageFormatProperties2,
        HS_vkGetPhysicalDeviceImageFormatProperties2,
        PFN_vkGetPhysicalDeviceImageFormatProperties2,
        vkGetPhysicalDeviceImageFormatProperties2,
        vkGetPhysicalDeviceImageFormatProperties2Unsafe,
        vkGetPhysicalDeviceImageFormatProperties2Safe,
        VkGetPhysicalDeviceQueueFamilyProperties2,
        pattern VkGetPhysicalDeviceQueueFamilyProperties2,
        HS_vkGetPhysicalDeviceQueueFamilyProperties2,
        PFN_vkGetPhysicalDeviceQueueFamilyProperties2,
        vkGetPhysicalDeviceQueueFamilyProperties2,
        vkGetPhysicalDeviceQueueFamilyProperties2Unsafe,
        vkGetPhysicalDeviceQueueFamilyProperties2Safe,
        VkGetPhysicalDeviceMemoryProperties2,
        pattern VkGetPhysicalDeviceMemoryProperties2,
        HS_vkGetPhysicalDeviceMemoryProperties2,
        PFN_vkGetPhysicalDeviceMemoryProperties2,
        vkGetPhysicalDeviceMemoryProperties2,
        vkGetPhysicalDeviceMemoryProperties2Unsafe,
        vkGetPhysicalDeviceMemoryProperties2Safe,
        VkGetPhysicalDeviceSparseImageFormatProperties2,
        pattern VkGetPhysicalDeviceSparseImageFormatProperties2,
        HS_vkGetPhysicalDeviceSparseImageFormatProperties2,
        PFN_vkGetPhysicalDeviceSparseImageFormatProperties2,
        vkGetPhysicalDeviceSparseImageFormatProperties2,
        vkGetPhysicalDeviceSparseImageFormatProperties2Unsafe,
        vkGetPhysicalDeviceSparseImageFormatProperties2Safe,
        VkDeviceEventInfoEXT, VkDeviceGeneratedCommandsFeaturesNVX,
        VkDeviceGeneratedCommandsLimitsNVX, VkDeviceGroupBindSparseInfoKHR,
        VkDeviceGroupCommandBufferBeginInfoKHR,
        VkDeviceGroupDeviceCreateInfoKHR,
        VkDeviceGroupPresentCapabilitiesKHR, VkDeviceGroupPresentInfoKHR,
        VkDeviceGroupRenderPassBeginInfoKHR, VkDeviceGroupSubmitInfoKHR,
        VkDeviceGroupSwapchainCreateInfoKHR,
        VkDeviceQueueGlobalPriorityCreateInfoEXT, VkDeviceQueueInfo2,
        VkFormatProperties2KHR, VkQueueFamilyProperties2KHR,
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
        PFN_vkTrimCommandPool, vkTrimCommandPool, vkTrimCommandPoolUnsafe,
        vkTrimCommandPoolSafe, pattern VK_ERROR_OUT_OF_POOL_MEMORY,
        pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT,
        pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT,
        pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT,
        -- ** Promoted from VK_KHR_maintenance2
        VkAccessBitmask(..), VkAccessFlagBits(), VkAccessFlags(),
        VkAttachmentDescription, VkAttachmentDescriptionBitmask(..),
        VkAttachmentLoadOp(..), VkAttachmentStoreOp(..),
        VkAttachmentDescriptionFlagBits(), VkAttachmentDescriptionFlags(),
        VkAttachmentReference, VkComponentMapping, VkComponentSwizzle(..),
        VkDependencyBitmask(..), VkDependencyFlagBits(),
        VkDependencyFlags(), VkInputAttachmentAspectReference,
        VkPipelineTessellationDomainOriginStateCreateInfo,
        VkPipelineTessellationStateCreateInfo, VkPointClippingBehavior(..),
        VkPointClippingBehaviorKHR(..), VkRenderPassCreateInfo,
        VkRenderPassInputAttachmentAspectCreateInfo, VkSubpassDependency,
        VkSubpassDescription, VkSubpassContents(..),
        VkSubpassDescriptionBitmask(..), VkSubpassDescriptionFlagBits(),
        VkSubpassDescriptionFlags(), VkTessellationDomainOrigin(..),
        VkTessellationDomainOriginKHR(..),
        -- > #include "vk_platform.h"
        pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT,
        pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO,
        pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL,
        pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL,
        VkRenderPassMultiviewCreateInfo,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES,
        pattern VK_DEPENDENCY_VIEW_LOCAL_BIT,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES,
        VkProtectedSubmitInfo, -- > #include "vk_platform.h"
                               VkGetDeviceQueue2,
        pattern VkGetDeviceQueue2, HS_vkGetDeviceQueue2,
        PFN_vkGetDeviceQueue2, vkGetDeviceQueue2, vkGetDeviceQueue2Unsafe,
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
        pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT, VkBorderColor(..),
        VkChromaLocation(..), VkChromaLocationKHR(..), VkCompareOp(..),
        VkFilter(..), VkSamplerAddressMode(..), VkSamplerMipmapMode(..),
        VkSamplerReductionModeEXT(..), VkSamplerYcbcrModelConversion(..),
        VkSamplerYcbcrRange(..), VkSamplerCreateFlagBits(..),
        VkSamplerYcbcrModelConversionKHR(..), VkSamplerYcbcrRangeKHR(..),
        VkSamplerCreateInfo, VkSamplerYcbcrConversionCreateInfo,
        VkSamplerYcbcrConversionImageFormatProperties,
        VkSamplerYcbcrConversionInfo, -- > #include "vk_platform.h"
                                      VkCreateSamplerYcbcrConversion,
        pattern VkCreateSamplerYcbcrConversion,
        HS_vkCreateSamplerYcbcrConversion,
        PFN_vkCreateSamplerYcbcrConversion, vkCreateSamplerYcbcrConversion,
        vkCreateSamplerYcbcrConversionUnsafe,
        vkCreateSamplerYcbcrConversionSafe,
        VkDestroySamplerYcbcrConversion,
        pattern VkDestroySamplerYcbcrConversion,
        HS_vkDestroySamplerYcbcrConversion,
        PFN_vkDestroySamplerYcbcrConversion,
        vkDestroySamplerYcbcrConversion,
        vkDestroySamplerYcbcrConversionUnsafe,
        vkDestroySamplerYcbcrConversionSafe, VkInternalAllocationType(..),
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
        PFN_vkVoidFunction, VkAllocationCallbacks,
        VkSamplerReductionModeCreateInfoEXT,
        VkSamplerYcbcrConversionCreateInfoKHR,
        VkSamplerYcbcrConversionImageFormatPropertiesKHR,
        VkSamplerYcbcrConversionInfoKHR,
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
        VkDescriptorBindingBitmaskEXT(..),
        VkDescriptorPoolCreateBitmask(..), VkDescriptorType(..),
        VkDescriptorUpdateTemplateType(..),
        VkDescriptorBindingFlagBitsEXT(), VkDescriptorBindingFlagsEXT(),
        VkDescriptorPoolCreateFlagBits(), VkDescriptorPoolCreateFlags(),
        VkDescriptorSetLayoutCreateBitmask(..),
        VkDescriptorSetLayoutCreateFlagBits(),
        VkDescriptorSetLayoutCreateFlags(),
        VkDescriptorUpdateTemplateTypeKHR(..),
        VkDescriptorUpdateTemplateCreateInfo,
        VkDescriptorUpdateTemplateEntry, -- > #include "vk_platform.h"
                                         VkCreateDescriptorUpdateTemplate,
        pattern VkCreateDescriptorUpdateTemplate,
        HS_vkCreateDescriptorUpdateTemplate,
        PFN_vkCreateDescriptorUpdateTemplate,
        vkCreateDescriptorUpdateTemplate,
        vkCreateDescriptorUpdateTemplateUnsafe,
        vkCreateDescriptorUpdateTemplateSafe,
        VkDestroyDescriptorUpdateTemplate,
        pattern VkDestroyDescriptorUpdateTemplate,
        HS_vkDestroyDescriptorUpdateTemplate,
        PFN_vkDestroyDescriptorUpdateTemplate,
        vkDestroyDescriptorUpdateTemplate,
        vkDestroyDescriptorUpdateTemplateUnsafe,
        vkDestroyDescriptorUpdateTemplateSafe,
        VkUpdateDescriptorSetWithTemplate,
        pattern VkUpdateDescriptorSetWithTemplate,
        HS_vkUpdateDescriptorSetWithTemplate,
        PFN_vkUpdateDescriptorSetWithTemplate,
        vkUpdateDescriptorSetWithTemplate,
        vkUpdateDescriptorSetWithTemplateUnsafe,
        vkUpdateDescriptorSetWithTemplateSafe, VkDescriptorBufferInfo,
        VkDescriptorImageInfo, VkDescriptorPoolCreateInfo,
        VkDescriptorPoolSize, VkDescriptorSetAllocateInfo,
        VkDescriptorSetLayoutBinding,
        VkDescriptorSetLayoutBindingFlagsCreateInfoEXT,
        VkDescriptorSetLayoutCreateInfo, VkDescriptorSetLayoutSupport,
        VkDescriptorSetLayoutSupportKHR,
        VkDescriptorSetVariableDescriptorCountAllocateInfoEXT,
        VkDescriptorSetVariableDescriptorCountLayoutSupportEXT,
        VkDescriptorUpdateTemplateCreateInfoKHR,
        VkDescriptorUpdateTemplateEntryKHR,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO,
        pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE,
        -- ** Promoted from VK_KHR_external_memory_capabilities
        VkBufferCreateBitmask(..), VkBufferUsageBitmask(..),
        VkBufferCreateFlagBits(), VkBufferCreateFlags(),
        VkBufferUsageFlagBits(), VkBufferUsageFlags(),
        VkBufferViewCreateFlagBits(..), VkExternalBufferProperties,
        VkExternalImageFormatProperties, VkExternalFenceFeatureBitmask(..),
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
        VkExternalSemaphoreHandleTypeFlags(), VkExternalMemoryProperties,
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceExternalBufferProperties,
        pattern VkGetPhysicalDeviceExternalBufferProperties,
        HS_vkGetPhysicalDeviceExternalBufferProperties,
        PFN_vkGetPhysicalDeviceExternalBufferProperties,
        vkGetPhysicalDeviceExternalBufferProperties,
        vkGetPhysicalDeviceExternalBufferPropertiesUnsafe,
        vkGetPhysicalDeviceExternalBufferPropertiesSafe,
        VkExternalBufferPropertiesKHR, VkExternalFenceProperties,
        VkExternalFencePropertiesKHR, VkExternalImageFormatPropertiesKHR,
        VkExternalImageFormatPropertiesNV,
        VkExternalMemoryBufferCreateInfo,
        VkExternalMemoryBufferCreateInfoKHR,
        VkExternalMemoryImageCreateInfo,
        VkExternalMemoryImageCreateInfoKHR,
        VkExternalMemoryImageCreateInfoNV, VkExternalMemoryPropertiesKHR,
        VkExternalSemaphoreProperties, VkExternalSemaphorePropertiesKHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES,
        pattern VK_LUID_SIZE, VkExportMemoryAllocateInfo,
        VkSharingMode(..),
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO,
        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO,
        pattern VK_ERROR_INVALID_EXTERNAL_HANDLE,
        pattern VK_QUEUE_FAMILY_EXTERNAL,
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceExternalFenceProperties,
        pattern VkGetPhysicalDeviceExternalFenceProperties,
        HS_vkGetPhysicalDeviceExternalFenceProperties,
        PFN_vkGetPhysicalDeviceExternalFenceProperties,
        vkGetPhysicalDeviceExternalFenceProperties,
        vkGetPhysicalDeviceExternalFencePropertiesUnsafe,
        vkGetPhysicalDeviceExternalFencePropertiesSafe,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES,
        -- ** Promoted from VK_KHR_external_fence
        VkExportFenceCreateInfo, VkFenceCreateBitmask(..),
        VkFenceImportBitmask(..), VkFenceCreateFlagBits(),
        VkFenceCreateFlags(), VkFenceImportFlagBits(),
        VkFenceImportFlagBitsKHR(..), VkFenceImportFlags(),
        VkFenceCreateInfo,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO,
        -- ** Promoted from VK_KHR_external_semaphore
        VkExportSemaphoreCreateInfo, VkSemaphoreCreateInfo,
        VkSemaphoreImportBitmask(..), VkSemaphoreImportFlagBits(),
        VkSemaphoreImportFlagBitsKHR(..), VkSemaphoreImportFlags(),
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO,
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceExternalSemaphoreProperties,
        pattern VkGetPhysicalDeviceExternalSemaphoreProperties,
        HS_vkGetPhysicalDeviceExternalSemaphoreProperties,
        PFN_vkGetPhysicalDeviceExternalSemaphoreProperties,
        vkGetPhysicalDeviceExternalSemaphoreProperties,
        vkGetPhysicalDeviceExternalSemaphorePropertiesUnsafe,
        vkGetPhysicalDeviceExternalSemaphorePropertiesSafe,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES,
        -- > #include "vk_platform.h"
        VkGetDescriptorSetLayoutSupport,
        pattern VkGetDescriptorSetLayoutSupport,
        HS_vkGetDescriptorSetLayoutSupport,
        PFN_vkGetDescriptorSetLayoutSupport,
        vkGetDescriptorSetLayoutSupport,
        vkGetDescriptorSetLayoutSupportUnsafe,
        vkGetDescriptorSetLayoutSupportSafe,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT,
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
import           Graphics.Vulkan.Types.Struct.Attachment                     (VkAttachmentDescription,
                                                                              VkAttachmentReference)
import           Graphics.Vulkan.Types.Struct.Bind
import           Graphics.Vulkan.Types.Struct.Buffer
import           Graphics.Vulkan.Types.Struct.Clear                          (VkClearColorValue,
                                                                              VkClearDepthStencilValue,
                                                                              VkClearValue)
import           Graphics.Vulkan.Types.Struct.Command                        (VkCommandBufferBeginInfo,
                                                                              VkCommandBufferInheritanceInfo)
import           Graphics.Vulkan.Types.Struct.ComponentMapping
import           Graphics.Vulkan.Types.Struct.Descriptor
import           Graphics.Vulkan.Types.Struct.Device
import           Graphics.Vulkan.Types.Struct.Export                         (VkExportFenceCreateInfo,
                                                                              VkExportMemoryAllocateInfo,
                                                                              VkExportSemaphoreCreateInfo)
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.External
import           Graphics.Vulkan.Types.Struct.Fence                          (VkFenceCreateInfo)
import           Graphics.Vulkan.Types.Struct.FormatProperties
import           Graphics.Vulkan.Types.Struct.Image
import           Graphics.Vulkan.Types.Struct.InputAttachmentAspectReference (VkInputAttachmentAspectReference)
import           Graphics.Vulkan.Types.Struct.Memory
import           Graphics.Vulkan.Types.Struct.Offset                         (VkOffset2D,
                                                                              VkOffset3D)
import           Graphics.Vulkan.Types.Struct.PhysicalDevice
import           Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures
import           Graphics.Vulkan.Types.Struct.Pipeline                       (VkPipelineTessellationDomainOriginStateCreateInfo,
                                                                              VkPipelineTessellationStateCreateInfo)
import           Graphics.Vulkan.Types.Struct.ProtectedSubmitInfo            (VkProtectedSubmitInfo)
import           Graphics.Vulkan.Types.Struct.QueueFamilyProperties
import           Graphics.Vulkan.Types.Struct.Rect                           (VkRect2D)
import           Graphics.Vulkan.Types.Struct.RenderPass                     (VkRenderPassBeginInfo,
                                                                              VkRenderPassCreateInfo,
                                                                              VkRenderPassInputAttachmentAspectCreateInfo,
                                                                              VkRenderPassMultiviewCreateInfo)
import           Graphics.Vulkan.Types.Struct.Sampler
import           Graphics.Vulkan.Types.Struct.Semaphore                      (VkSemaphoreCreateInfo)
import           Graphics.Vulkan.Types.Struct.Sparse
import           Graphics.Vulkan.Types.Struct.SubmitInfo                     (VkSubmitInfo)
import           Graphics.Vulkan.Types.Struct.Subpass                        (VkSubpassDependency,
                                                                              VkSubpassDescription)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

pattern VkEnumerateInstanceVersion :: CString

pattern VkEnumerateInstanceVersion <-
        (is_VkEnumerateInstanceVersion -> True)
  where
    VkEnumerateInstanceVersion = _VkEnumerateInstanceVersion

{-# INLINE _VkEnumerateInstanceVersion #-}

_VkEnumerateInstanceVersion :: CString
_VkEnumerateInstanceVersion = Ptr "vkEnumerateInstanceVersion\NUL"#

{-# INLINE is_VkEnumerateInstanceVersion #-}

is_VkEnumerateInstanceVersion :: CString -> Bool
is_VkEnumerateInstanceVersion
  = (EQ ==) . cmpCStrings _VkEnumerateInstanceVersion

type VkEnumerateInstanceVersion = "vkEnumerateInstanceVersion"

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- > VkResult vkEnumerateInstanceVersion
-- >     ( uint32_t* pApiVersion
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateInstanceVersion vkEnumerateInstanceVersion registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumerateInstanceVersion <- vkGetInstanceProc @VkEnumerateInstanceVersion vkInstance
--
-- or less efficient:
--
-- > myEnumerateInstanceVersion <- vkGetProc @VkEnumerateInstanceVersion
--
-- __Note:__ @vkEnumerateInstanceVersionUnsafe@ and @vkEnumerateInstanceVersionSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkEnumerateInstanceVersion@ is an alias
--           of @vkEnumerateInstanceVersionUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkEnumerateInstanceVersionSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkEnumerateInstanceVersion"
               vkEnumerateInstanceVersionUnsafe :: Ptr Word32 -- ^ pApiVersion
                                                              -> IO VkResult

#else
vkEnumerateInstanceVersionUnsafe :: Ptr Word32 -- ^ pApiVersion
                                               -> IO VkResult
vkEnumerateInstanceVersionUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkEnumerateInstanceVersion)

{-# NOINLINE vkEnumerateInstanceVersionUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- > VkResult vkEnumerateInstanceVersion
-- >     ( uint32_t* pApiVersion
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateInstanceVersion vkEnumerateInstanceVersion registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumerateInstanceVersion <- vkGetInstanceProc @VkEnumerateInstanceVersion vkInstance
--
-- or less efficient:
--
-- > myEnumerateInstanceVersion <- vkGetProc @VkEnumerateInstanceVersion
--
-- __Note:__ @vkEnumerateInstanceVersionUnsafe@ and @vkEnumerateInstanceVersionSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkEnumerateInstanceVersion@ is an alias
--           of @vkEnumerateInstanceVersionUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkEnumerateInstanceVersionSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkEnumerateInstanceVersion"
               vkEnumerateInstanceVersionSafe :: Ptr Word32 -- ^ pApiVersion
                                                            -> IO VkResult

#else
vkEnumerateInstanceVersionSafe :: Ptr Word32 -- ^ pApiVersion
                                             -> IO VkResult
vkEnumerateInstanceVersionSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkEnumerateInstanceVersion)

{-# NOINLINE vkEnumerateInstanceVersionSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- > VkResult vkEnumerateInstanceVersion
-- >     ( uint32_t* pApiVersion
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumerateInstanceVersion vkEnumerateInstanceVersion registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumerateInstanceVersion <- vkGetInstanceProc @VkEnumerateInstanceVersion vkInstance
--
-- or less efficient:
--
-- > myEnumerateInstanceVersion <- vkGetProc @VkEnumerateInstanceVersion
--
-- __Note:__ @vkEnumerateInstanceVersionUnsafe@ and @vkEnumerateInstanceVersionSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkEnumerateInstanceVersion@ is an alias
--           of @vkEnumerateInstanceVersionUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkEnumerateInstanceVersionSafe@.
--
vkEnumerateInstanceVersion :: Ptr Word32 -- ^ pApiVersion
                                         -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkEnumerateInstanceVersion = vkEnumerateInstanceVersionUnsafe
#else
vkEnumerateInstanceVersion = vkEnumerateInstanceVersionSafe

#endif
{-# INLINE vkEnumerateInstanceVersion #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkEnumerateInstanceVersionUnsafe ::
               PFN_vkEnumerateInstanceVersion -> HS_vkEnumerateInstanceVersion

foreign import ccall safe "dynamic"
               unwrapVkEnumerateInstanceVersionSafe ::
               PFN_vkEnumerateInstanceVersion -> HS_vkEnumerateInstanceVersion

instance VulkanProc "vkEnumerateInstanceVersion" where
    type VkProcType "vkEnumerateInstanceVersion" =
         HS_vkEnumerateInstanceVersion
    vkProcSymbol = _VkEnumerateInstanceVersion

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkEnumerateInstanceVersionUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkEnumerateInstanceVersionSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES =
        VkStructureType 1000094000

pattern VkBindBufferMemory2 :: CString

pattern VkBindBufferMemory2 <- (is_VkBindBufferMemory2 -> True)
  where
    VkBindBufferMemory2 = _VkBindBufferMemory2

{-# INLINE _VkBindBufferMemory2 #-}

_VkBindBufferMemory2 :: CString
_VkBindBufferMemory2 = Ptr "vkBindBufferMemory2\NUL"#

{-# INLINE is_VkBindBufferMemory2 #-}

is_VkBindBufferMemory2 :: CString -> Bool
is_VkBindBufferMemory2 = (EQ ==) . cmpCStrings _VkBindBufferMemory2

type VkBindBufferMemory2 = "vkBindBufferMemory2"

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myBindBufferMemory2 <- vkGetDeviceProc @VkBindBufferMemory2 vkDevice
--
-- or less efficient:
--
-- > myBindBufferMemory2 <- vkGetProc @VkBindBufferMemory2
--
-- __Note:__ @vkBindBufferMemory2Unsafe@ and @vkBindBufferMemory2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkBindBufferMemory2@ is an alias
--           of @vkBindBufferMemory2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkBindBufferMemory2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkBindBufferMemory2"
               vkBindBufferMemory2Unsafe ::
               VkDevice -- ^ device
                        -> Word32 -- ^ bindInfoCount
                                  -> Ptr VkBindBufferMemoryInfo -- ^ pBindInfos
                                                                -> IO VkResult

#else
vkBindBufferMemory2Unsafe ::
                          VkDevice -- ^ device
                                   -> Word32 -- ^ bindInfoCount
                                             -> Ptr VkBindBufferMemoryInfo -- ^ pBindInfos
                                                                           -> IO VkResult
vkBindBufferMemory2Unsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkBindBufferMemory2)

{-# NOINLINE vkBindBufferMemory2Unsafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myBindBufferMemory2 <- vkGetDeviceProc @VkBindBufferMemory2 vkDevice
--
-- or less efficient:
--
-- > myBindBufferMemory2 <- vkGetProc @VkBindBufferMemory2
--
-- __Note:__ @vkBindBufferMemory2Unsafe@ and @vkBindBufferMemory2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkBindBufferMemory2@ is an alias
--           of @vkBindBufferMemory2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkBindBufferMemory2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkBindBufferMemory2"
               vkBindBufferMemory2Safe ::
               VkDevice -- ^ device
                        -> Word32 -- ^ bindInfoCount
                                  -> Ptr VkBindBufferMemoryInfo -- ^ pBindInfos
                                                                -> IO VkResult

#else
vkBindBufferMemory2Safe ::
                        VkDevice -- ^ device
                                 -> Word32 -- ^ bindInfoCount
                                           -> Ptr VkBindBufferMemoryInfo -- ^ pBindInfos
                                                                         -> IO VkResult
vkBindBufferMemory2Safe
  = unsafeDupablePerformIO (vkGetProcSafe @VkBindBufferMemory2)

{-# NOINLINE vkBindBufferMemory2Safe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myBindBufferMemory2 <- vkGetDeviceProc @VkBindBufferMemory2 vkDevice
--
-- or less efficient:
--
-- > myBindBufferMemory2 <- vkGetProc @VkBindBufferMemory2
--
-- __Note:__ @vkBindBufferMemory2Unsafe@ and @vkBindBufferMemory2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkBindBufferMemory2@ is an alias
--           of @vkBindBufferMemory2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkBindBufferMemory2Safe@.
--
vkBindBufferMemory2 ::
                    VkDevice -- ^ device
                             -> Word32 -- ^ bindInfoCount
                                       -> Ptr VkBindBufferMemoryInfo -- ^ pBindInfos
                                                                     -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkBindBufferMemory2 = vkBindBufferMemory2Unsafe
#else
vkBindBufferMemory2 = vkBindBufferMemory2Safe

#endif
{-# INLINE vkBindBufferMemory2 #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkBindBufferMemory2Unsafe ::
               PFN_vkBindBufferMemory2 -> HS_vkBindBufferMemory2

foreign import ccall safe "dynamic" unwrapVkBindBufferMemory2Safe
               :: PFN_vkBindBufferMemory2 -> HS_vkBindBufferMemory2

instance VulkanProc "vkBindBufferMemory2" where
    type VkProcType "vkBindBufferMemory2" = HS_vkBindBufferMemory2
    vkProcSymbol = _VkBindBufferMemory2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkBindBufferMemory2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkBindBufferMemory2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkBindImageMemory2 :: CString

pattern VkBindImageMemory2 <- (is_VkBindImageMemory2 -> True)
  where
    VkBindImageMemory2 = _VkBindImageMemory2

{-# INLINE _VkBindImageMemory2 #-}

_VkBindImageMemory2 :: CString
_VkBindImageMemory2 = Ptr "vkBindImageMemory2\NUL"#

{-# INLINE is_VkBindImageMemory2 #-}

is_VkBindImageMemory2 :: CString -> Bool
is_VkBindImageMemory2 = (EQ ==) . cmpCStrings _VkBindImageMemory2

type VkBindImageMemory2 = "vkBindImageMemory2"

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myBindImageMemory2 <- vkGetDeviceProc @VkBindImageMemory2 vkDevice
--
-- or less efficient:
--
-- > myBindImageMemory2 <- vkGetProc @VkBindImageMemory2
--
-- __Note:__ @vkBindImageMemory2Unsafe@ and @vkBindImageMemory2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkBindImageMemory2@ is an alias
--           of @vkBindImageMemory2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkBindImageMemory2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkBindImageMemory2"
               vkBindImageMemory2Unsafe ::
               VkDevice -- ^ device
                        -> Word32 -- ^ bindInfoCount
                                  -> Ptr VkBindImageMemoryInfo -- ^ pBindInfos
                                                               -> IO VkResult

#else
vkBindImageMemory2Unsafe ::
                         VkDevice -- ^ device
                                  -> Word32 -- ^ bindInfoCount
                                            -> Ptr VkBindImageMemoryInfo -- ^ pBindInfos
                                                                         -> IO VkResult
vkBindImageMemory2Unsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkBindImageMemory2)

{-# NOINLINE vkBindImageMemory2Unsafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myBindImageMemory2 <- vkGetDeviceProc @VkBindImageMemory2 vkDevice
--
-- or less efficient:
--
-- > myBindImageMemory2 <- vkGetProc @VkBindImageMemory2
--
-- __Note:__ @vkBindImageMemory2Unsafe@ and @vkBindImageMemory2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkBindImageMemory2@ is an alias
--           of @vkBindImageMemory2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkBindImageMemory2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkBindImageMemory2"
               vkBindImageMemory2Safe ::
               VkDevice -- ^ device
                        -> Word32 -- ^ bindInfoCount
                                  -> Ptr VkBindImageMemoryInfo -- ^ pBindInfos
                                                               -> IO VkResult

#else
vkBindImageMemory2Safe ::
                       VkDevice -- ^ device
                                -> Word32 -- ^ bindInfoCount
                                          -> Ptr VkBindImageMemoryInfo -- ^ pBindInfos
                                                                       -> IO VkResult
vkBindImageMemory2Safe
  = unsafeDupablePerformIO (vkGetProcSafe @VkBindImageMemory2)

{-# NOINLINE vkBindImageMemory2Safe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myBindImageMemory2 <- vkGetDeviceProc @VkBindImageMemory2 vkDevice
--
-- or less efficient:
--
-- > myBindImageMemory2 <- vkGetProc @VkBindImageMemory2
--
-- __Note:__ @vkBindImageMemory2Unsafe@ and @vkBindImageMemory2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkBindImageMemory2@ is an alias
--           of @vkBindImageMemory2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkBindImageMemory2Safe@.
--
vkBindImageMemory2 ::
                   VkDevice -- ^ device
                            -> Word32 -- ^ bindInfoCount
                                      -> Ptr VkBindImageMemoryInfo -- ^ pBindInfos
                                                                   -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkBindImageMemory2 = vkBindImageMemory2Unsafe
#else
vkBindImageMemory2 = vkBindImageMemory2Safe

#endif
{-# INLINE vkBindImageMemory2 #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkBindImageMemory2Unsafe ::
               PFN_vkBindImageMemory2 -> HS_vkBindImageMemory2

foreign import ccall safe "dynamic" unwrapVkBindImageMemory2Safe ::
               PFN_vkBindImageMemory2 -> HS_vkBindImageMemory2

instance VulkanProc "vkBindImageMemory2" where
    type VkProcType "vkBindImageMemory2" = HS_vkBindImageMemory2
    vkProcSymbol = _VkBindImageMemory2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkBindImageMemory2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkBindImageMemory2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO =
        VkStructureType 1000157000

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO :: VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO =
        VkStructureType 1000157001

-- | bitpos = @10@
pattern VK_IMAGE_CREATE_ALIAS_BIT :: VkImageCreateBitmask a

pattern VK_IMAGE_CREATE_ALIAS_BIT = VkImageCreateBitmask 1024

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
  where
    VkGetDeviceGroupPeerMemoryFeatures
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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceGroupPeerMemoryFeatures <- vkGetDeviceProc @VkGetDeviceGroupPeerMemoryFeatures vkDevice
--
-- or less efficient:
--
-- > myGetDeviceGroupPeerMemoryFeatures <- vkGetProc @VkGetDeviceGroupPeerMemoryFeatures
--
-- __Note:__ @vkGetDeviceGroupPeerMemoryFeaturesUnsafe@ and @vkGetDeviceGroupPeerMemoryFeaturesSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDeviceGroupPeerMemoryFeatures@ is an alias
--           of @vkGetDeviceGroupPeerMemoryFeaturesUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDeviceGroupPeerMemoryFeaturesSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkGetDeviceGroupPeerMemoryFeatures"
               vkGetDeviceGroupPeerMemoryFeaturesUnsafe ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ heapIndex
                        -> Word32 -- ^ localDeviceIndex
                                  -> Word32 -- ^ remoteDeviceIndex
                                            -> Ptr VkPeerMemoryFeatureFlags -- ^ pPeerMemoryFeatures
                                                                            -> IO ()

#else
vkGetDeviceGroupPeerMemoryFeaturesUnsafe ::
                                         VkDevice -- ^ device
                                                  ->
                                           Word32 -- ^ heapIndex
                                                  ->
                                             Word32 -- ^ localDeviceIndex
                                                    ->
                                               Word32 -- ^ remoteDeviceIndex
                                                      -> Ptr VkPeerMemoryFeatureFlags -- ^ pPeerMemoryFeatures
                                                                                      -> IO ()
vkGetDeviceGroupPeerMemoryFeaturesUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetDeviceGroupPeerMemoryFeatures)

{-# NOINLINE vkGetDeviceGroupPeerMemoryFeaturesUnsafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceGroupPeerMemoryFeatures <- vkGetDeviceProc @VkGetDeviceGroupPeerMemoryFeatures vkDevice
--
-- or less efficient:
--
-- > myGetDeviceGroupPeerMemoryFeatures <- vkGetProc @VkGetDeviceGroupPeerMemoryFeatures
--
-- __Note:__ @vkGetDeviceGroupPeerMemoryFeaturesUnsafe@ and @vkGetDeviceGroupPeerMemoryFeaturesSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDeviceGroupPeerMemoryFeatures@ is an alias
--           of @vkGetDeviceGroupPeerMemoryFeaturesUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDeviceGroupPeerMemoryFeaturesSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
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
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetDeviceGroupPeerMemoryFeatures)

{-# NOINLINE vkGetDeviceGroupPeerMemoryFeaturesSafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceGroupPeerMemoryFeatures <- vkGetDeviceProc @VkGetDeviceGroupPeerMemoryFeatures vkDevice
--
-- or less efficient:
--
-- > myGetDeviceGroupPeerMemoryFeatures <- vkGetProc @VkGetDeviceGroupPeerMemoryFeatures
--
-- __Note:__ @vkGetDeviceGroupPeerMemoryFeaturesUnsafe@ and @vkGetDeviceGroupPeerMemoryFeaturesSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDeviceGroupPeerMemoryFeatures@ is an alias
--           of @vkGetDeviceGroupPeerMemoryFeaturesUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDeviceGroupPeerMemoryFeaturesSafe@.
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
#ifdef UNSAFE_FFI_DEFAULT
vkGetDeviceGroupPeerMemoryFeatures
  = vkGetDeviceGroupPeerMemoryFeaturesUnsafe
#else
vkGetDeviceGroupPeerMemoryFeatures
  = vkGetDeviceGroupPeerMemoryFeaturesSafe

#endif
{-# INLINE vkGetDeviceGroupPeerMemoryFeatures #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkGetDeviceGroupPeerMemoryFeaturesUnsafe ::
               PFN_vkGetDeviceGroupPeerMemoryFeatures ->
                 HS_vkGetDeviceGroupPeerMemoryFeatures

foreign import ccall safe "dynamic"
               unwrapVkGetDeviceGroupPeerMemoryFeaturesSafe ::
               PFN_vkGetDeviceGroupPeerMemoryFeatures ->
                 HS_vkGetDeviceGroupPeerMemoryFeatures

instance VulkanProc "vkGetDeviceGroupPeerMemoryFeatures" where
    type VkProcType "vkGetDeviceGroupPeerMemoryFeatures" =
         HS_vkGetDeviceGroupPeerMemoryFeatures
    vkProcSymbol = _VkGetDeviceGroupPeerMemoryFeatures

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetDeviceGroupPeerMemoryFeaturesUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetDeviceGroupPeerMemoryFeaturesSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetDeviceMask :: CString

pattern VkCmdSetDeviceMask <- (is_VkCmdSetDeviceMask -> True)
  where
    VkCmdSetDeviceMask = _VkCmdSetDeviceMask

{-# INLINE _VkCmdSetDeviceMask #-}

_VkCmdSetDeviceMask :: CString
_VkCmdSetDeviceMask = Ptr "vkCmdSetDeviceMask\NUL"#

{-# INLINE is_VkCmdSetDeviceMask #-}

is_VkCmdSetDeviceMask :: CString -> Bool
is_VkCmdSetDeviceMask = (EQ ==) . cmpCStrings _VkCmdSetDeviceMask

type VkCmdSetDeviceMask = "vkCmdSetDeviceMask"

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetDeviceMask <- vkGetInstanceProc @VkCmdSetDeviceMask vkInstance
--
-- or less efficient:
--
-- > myCmdSetDeviceMask <- vkGetProc @VkCmdSetDeviceMask
--
-- __Note:__ @vkCmdSetDeviceMaskUnsafe@ and @vkCmdSetDeviceMaskSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdSetDeviceMask@ is an alias
--           of @vkCmdSetDeviceMaskUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdSetDeviceMaskSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkCmdSetDeviceMask"
               vkCmdSetDeviceMaskUnsafe :: VkCommandBuffer -- ^ commandBuffer
                                                           -> Word32 -- ^ deviceMask
                                                                     -> IO ()

#else
vkCmdSetDeviceMaskUnsafe :: VkCommandBuffer -- ^ commandBuffer
                                            -> Word32 -- ^ deviceMask
                                                      -> IO ()
vkCmdSetDeviceMaskUnsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkCmdSetDeviceMask)

{-# NOINLINE vkCmdSetDeviceMaskUnsafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetDeviceMask <- vkGetInstanceProc @VkCmdSetDeviceMask vkInstance
--
-- or less efficient:
--
-- > myCmdSetDeviceMask <- vkGetProc @VkCmdSetDeviceMask
--
-- __Note:__ @vkCmdSetDeviceMaskUnsafe@ and @vkCmdSetDeviceMaskSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdSetDeviceMask@ is an alias
--           of @vkCmdSetDeviceMaskUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdSetDeviceMaskSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkCmdSetDeviceMask"
               vkCmdSetDeviceMaskSafe :: VkCommandBuffer -- ^ commandBuffer
                                                         -> Word32 -- ^ deviceMask
                                                                   -> IO ()

#else
vkCmdSetDeviceMaskSafe :: VkCommandBuffer -- ^ commandBuffer
                                          -> Word32 -- ^ deviceMask
                                                    -> IO ()
vkCmdSetDeviceMaskSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdSetDeviceMask)

{-# NOINLINE vkCmdSetDeviceMaskSafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdSetDeviceMask <- vkGetInstanceProc @VkCmdSetDeviceMask vkInstance
--
-- or less efficient:
--
-- > myCmdSetDeviceMask <- vkGetProc @VkCmdSetDeviceMask
--
-- __Note:__ @vkCmdSetDeviceMaskUnsafe@ and @vkCmdSetDeviceMaskSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdSetDeviceMask@ is an alias
--           of @vkCmdSetDeviceMaskUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdSetDeviceMaskSafe@.
--
vkCmdSetDeviceMask :: VkCommandBuffer -- ^ commandBuffer
                                      -> Word32 -- ^ deviceMask
                                                -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkCmdSetDeviceMask = vkCmdSetDeviceMaskUnsafe
#else
vkCmdSetDeviceMask = vkCmdSetDeviceMaskSafe

#endif
{-# INLINE vkCmdSetDeviceMask #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetDeviceMaskUnsafe ::
               PFN_vkCmdSetDeviceMask -> HS_vkCmdSetDeviceMask

foreign import ccall safe "dynamic" unwrapVkCmdSetDeviceMaskSafe ::
               PFN_vkCmdSetDeviceMask -> HS_vkCmdSetDeviceMask

instance VulkanProc "vkCmdSetDeviceMask" where
    type VkProcType "vkCmdSetDeviceMask" = HS_vkCmdSetDeviceMask
    vkProcSymbol = _VkCmdSetDeviceMask

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetDeviceMaskUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetDeviceMaskSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdDispatchBase :: CString

pattern VkCmdDispatchBase <- (is_VkCmdDispatchBase -> True)
  where
    VkCmdDispatchBase = _VkCmdDispatchBase

{-# INLINE _VkCmdDispatchBase #-}

_VkCmdDispatchBase :: CString
_VkCmdDispatchBase = Ptr "vkCmdDispatchBase\NUL"#

{-# INLINE is_VkCmdDispatchBase #-}

is_VkCmdDispatchBase :: CString -> Bool
is_VkCmdDispatchBase = (EQ ==) . cmpCStrings _VkCmdDispatchBase

type VkCmdDispatchBase = "vkCmdDispatchBase"

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDispatchBase <- vkGetInstanceProc @VkCmdDispatchBase vkInstance
--
-- or less efficient:
--
-- > myCmdDispatchBase <- vkGetProc @VkCmdDispatchBase
--
-- __Note:__ @vkCmdDispatchBaseUnsafe@ and @vkCmdDispatchBaseSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdDispatchBase@ is an alias
--           of @vkCmdDispatchBaseUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdDispatchBaseSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkCmdDispatchBase"
               vkCmdDispatchBaseUnsafe ::
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
vkCmdDispatchBaseUnsafe ::
                        VkCommandBuffer -- ^ commandBuffer
                                        ->
                          Word32 -- ^ baseGroupX
                                 -> Word32 -- ^ baseGroupY
                                           -> Word32 -- ^ baseGroupZ
                                                     -> Word32 -- ^ groupCountX
                                                               -> Word32 -- ^ groupCountY
                                                                         -> Word32 -- ^ groupCountZ
                                                                                   -> IO ()
vkCmdDispatchBaseUnsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkCmdDispatchBase)

{-# NOINLINE vkCmdDispatchBaseUnsafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDispatchBase <- vkGetInstanceProc @VkCmdDispatchBase vkInstance
--
-- or less efficient:
--
-- > myCmdDispatchBase <- vkGetProc @VkCmdDispatchBase
--
-- __Note:__ @vkCmdDispatchBaseUnsafe@ and @vkCmdDispatchBaseSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdDispatchBase@ is an alias
--           of @vkCmdDispatchBaseUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdDispatchBaseSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
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
vkCmdDispatchBaseSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCmdDispatchBase)

{-# NOINLINE vkCmdDispatchBaseSafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCmdDispatchBase <- vkGetInstanceProc @VkCmdDispatchBase vkInstance
--
-- or less efficient:
--
-- > myCmdDispatchBase <- vkGetProc @VkCmdDispatchBase
--
-- __Note:__ @vkCmdDispatchBaseUnsafe@ and @vkCmdDispatchBaseSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCmdDispatchBase@ is an alias
--           of @vkCmdDispatchBaseUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCmdDispatchBaseSafe@.
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
#ifdef UNSAFE_FFI_DEFAULT
vkCmdDispatchBase = vkCmdDispatchBaseUnsafe
#else
vkCmdDispatchBase = vkCmdDispatchBaseSafe

#endif
{-# INLINE vkCmdDispatchBase #-}

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

foreign import ccall unsafe "dynamic" unwrapVkCmdDispatchBaseUnsafe
               :: PFN_vkCmdDispatchBase -> HS_vkCmdDispatchBase

foreign import ccall safe "dynamic" unwrapVkCmdDispatchBaseSafe ::
               PFN_vkCmdDispatchBase -> HS_vkCmdDispatchBase

instance VulkanProc "vkCmdDispatchBase" where
    type VkProcType "vkCmdDispatchBase" = HS_vkCmdDispatchBase
    vkProcSymbol = _VkCmdDispatchBase

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdDispatchBaseUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdDispatchBaseSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

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
        VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT =
        VkPipelineCreateBitmask 8

-- | bitpos = @4@
pattern VK_PIPELINE_CREATE_DISPATCH_BASE ::
        VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_DISPATCH_BASE =
        VkPipelineCreateBitmask 16

-- | Dependency is across devices
--
--   bitpos = @2@
pattern VK_DEPENDENCY_DEVICE_GROUP_BIT :: VkDependencyBitmask a

pattern VK_DEPENDENCY_DEVICE_GROUP_BIT = VkDependencyBitmask 4

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
        VkImageCreateBitmask a

pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT =
        VkImageCreateBitmask 64

pattern VkEnumeratePhysicalDeviceGroups :: CString

pattern VkEnumeratePhysicalDeviceGroups <-
        (is_VkEnumeratePhysicalDeviceGroups -> True)
  where
    VkEnumeratePhysicalDeviceGroups = _VkEnumeratePhysicalDeviceGroups

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumeratePhysicalDeviceGroups <- vkGetInstanceProc @VkEnumeratePhysicalDeviceGroups vkInstance
--
-- or less efficient:
--
-- > myEnumeratePhysicalDeviceGroups <- vkGetProc @VkEnumeratePhysicalDeviceGroups
--
-- __Note:__ @vkEnumeratePhysicalDeviceGroupsUnsafe@ and @vkEnumeratePhysicalDeviceGroupsSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkEnumeratePhysicalDeviceGroups@ is an alias
--           of @vkEnumeratePhysicalDeviceGroupsUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkEnumeratePhysicalDeviceGroupsSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkEnumeratePhysicalDeviceGroups"
               vkEnumeratePhysicalDeviceGroupsUnsafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                            -> Ptr VkPhysicalDeviceGroupProperties -- ^ pPhysicalDeviceGroupProperties
                                                                   -> IO VkResult

#else
vkEnumeratePhysicalDeviceGroupsUnsafe ::
                                      VkInstance -- ^ instance
                                                 ->
                                        Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                                                   ->
                                          Ptr VkPhysicalDeviceGroupProperties -- ^ pPhysicalDeviceGroupProperties
                                                                              -> IO VkResult
vkEnumeratePhysicalDeviceGroupsUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkEnumeratePhysicalDeviceGroups)

{-# NOINLINE vkEnumeratePhysicalDeviceGroupsUnsafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumeratePhysicalDeviceGroups <- vkGetInstanceProc @VkEnumeratePhysicalDeviceGroups vkInstance
--
-- or less efficient:
--
-- > myEnumeratePhysicalDeviceGroups <- vkGetProc @VkEnumeratePhysicalDeviceGroups
--
-- __Note:__ @vkEnumeratePhysicalDeviceGroupsUnsafe@ and @vkEnumeratePhysicalDeviceGroupsSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkEnumeratePhysicalDeviceGroups@ is an alias
--           of @vkEnumeratePhysicalDeviceGroupsUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkEnumeratePhysicalDeviceGroupsSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkEnumeratePhysicalDeviceGroups"
               vkEnumeratePhysicalDeviceGroupsSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                            -> Ptr VkPhysicalDeviceGroupProperties -- ^ pPhysicalDeviceGroupProperties
                                                                   -> IO VkResult

#else
vkEnumeratePhysicalDeviceGroupsSafe ::
                                    VkInstance -- ^ instance
                                               ->
                                      Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                                                 ->
                                        Ptr VkPhysicalDeviceGroupProperties -- ^ pPhysicalDeviceGroupProperties
                                                                            -> IO VkResult
vkEnumeratePhysicalDeviceGroupsSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkEnumeratePhysicalDeviceGroups)

{-# NOINLINE vkEnumeratePhysicalDeviceGroupsSafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myEnumeratePhysicalDeviceGroups <- vkGetInstanceProc @VkEnumeratePhysicalDeviceGroups vkInstance
--
-- or less efficient:
--
-- > myEnumeratePhysicalDeviceGroups <- vkGetProc @VkEnumeratePhysicalDeviceGroups
--
-- __Note:__ @vkEnumeratePhysicalDeviceGroupsUnsafe@ and @vkEnumeratePhysicalDeviceGroupsSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkEnumeratePhysicalDeviceGroups@ is an alias
--           of @vkEnumeratePhysicalDeviceGroupsUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkEnumeratePhysicalDeviceGroupsSafe@.
--
vkEnumeratePhysicalDeviceGroups ::
                                VkInstance -- ^ instance
                                           ->
                                  Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                                             -> Ptr VkPhysicalDeviceGroupProperties -- ^ pPhysicalDeviceGroupProperties
                                                                                    -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkEnumeratePhysicalDeviceGroups
  = vkEnumeratePhysicalDeviceGroupsUnsafe
#else
vkEnumeratePhysicalDeviceGroups
  = vkEnumeratePhysicalDeviceGroupsSafe

#endif
{-# INLINE vkEnumeratePhysicalDeviceGroups #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkEnumeratePhysicalDeviceGroupsUnsafe ::
               PFN_vkEnumeratePhysicalDeviceGroups ->
                 HS_vkEnumeratePhysicalDeviceGroups

foreign import ccall safe "dynamic"
               unwrapVkEnumeratePhysicalDeviceGroupsSafe ::
               PFN_vkEnumeratePhysicalDeviceGroups ->
                 HS_vkEnumeratePhysicalDeviceGroups

instance VulkanProc "vkEnumeratePhysicalDeviceGroups" where
    type VkProcType "vkEnumeratePhysicalDeviceGroups" =
         HS_vkEnumeratePhysicalDeviceGroups
    vkProcSymbol = _VkEnumeratePhysicalDeviceGroups

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkEnumeratePhysicalDeviceGroupsUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkEnumeratePhysicalDeviceGroupsSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

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
pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT :: VkMemoryHeapBitmask a

pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT = VkMemoryHeapBitmask 2

pattern VkGetImageMemoryRequirements2 :: CString

pattern VkGetImageMemoryRequirements2 <-
        (is_VkGetImageMemoryRequirements2 -> True)
  where
    VkGetImageMemoryRequirements2 = _VkGetImageMemoryRequirements2

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

-- |
-- > void vkGetImageMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkImageMemoryRequirementsInfo2* pInfo
-- >     , VkMemoryRequirements2* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageMemoryRequirements2 vkGetImageMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetImageMemoryRequirements2 <- vkGetDeviceProc @VkGetImageMemoryRequirements2 vkDevice
--
-- or less efficient:
--
-- > myGetImageMemoryRequirements2 <- vkGetProc @VkGetImageMemoryRequirements2
--
-- __Note:__ @vkGetImageMemoryRequirements2Unsafe@ and @vkGetImageMemoryRequirements2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetImageMemoryRequirements2@ is an alias
--           of @vkGetImageMemoryRequirements2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetImageMemoryRequirements2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkGetImageMemoryRequirements2"
               vkGetImageMemoryRequirements2Unsafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageMemoryRequirementsInfo2 -- ^ pInfo
                                                    ->
                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                             -> IO ()

#else
vkGetImageMemoryRequirements2Unsafe ::
                                    VkDevice -- ^ device
                                             ->
                                      Ptr VkImageMemoryRequirementsInfo2 -- ^ pInfo
                                                                         ->
                                        Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                                                  -> IO ()
vkGetImageMemoryRequirements2Unsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetImageMemoryRequirements2)

{-# NOINLINE vkGetImageMemoryRequirements2Unsafe #-}
#endif

-- |
-- > void vkGetImageMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkImageMemoryRequirementsInfo2* pInfo
-- >     , VkMemoryRequirements2* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageMemoryRequirements2 vkGetImageMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetImageMemoryRequirements2 <- vkGetDeviceProc @VkGetImageMemoryRequirements2 vkDevice
--
-- or less efficient:
--
-- > myGetImageMemoryRequirements2 <- vkGetProc @VkGetImageMemoryRequirements2
--
-- __Note:__ @vkGetImageMemoryRequirements2Unsafe@ and @vkGetImageMemoryRequirements2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetImageMemoryRequirements2@ is an alias
--           of @vkGetImageMemoryRequirements2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetImageMemoryRequirements2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkGetImageMemoryRequirements2"
               vkGetImageMemoryRequirements2Safe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageMemoryRequirementsInfo2 -- ^ pInfo
                                                    ->
                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                             -> IO ()

#else
vkGetImageMemoryRequirements2Safe ::
                                  VkDevice -- ^ device
                                           ->
                                    Ptr VkImageMemoryRequirementsInfo2 -- ^ pInfo
                                                                       ->
                                      Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                                                -> IO ()
vkGetImageMemoryRequirements2Safe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetImageMemoryRequirements2)

{-# NOINLINE vkGetImageMemoryRequirements2Safe #-}
#endif

-- |
-- > void vkGetImageMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkImageMemoryRequirementsInfo2* pInfo
-- >     , VkMemoryRequirements2* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetImageMemoryRequirements2 vkGetImageMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetImageMemoryRequirements2 <- vkGetDeviceProc @VkGetImageMemoryRequirements2 vkDevice
--
-- or less efficient:
--
-- > myGetImageMemoryRequirements2 <- vkGetProc @VkGetImageMemoryRequirements2
--
-- __Note:__ @vkGetImageMemoryRequirements2Unsafe@ and @vkGetImageMemoryRequirements2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetImageMemoryRequirements2@ is an alias
--           of @vkGetImageMemoryRequirements2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetImageMemoryRequirements2Safe@.
--
vkGetImageMemoryRequirements2 ::
                              VkDevice -- ^ device
                                       ->
                                Ptr VkImageMemoryRequirementsInfo2 -- ^ pInfo
                                                                   ->
                                  Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                                            -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkGetImageMemoryRequirements2 = vkGetImageMemoryRequirements2Unsafe
#else
vkGetImageMemoryRequirements2 = vkGetImageMemoryRequirements2Safe

#endif
{-# INLINE vkGetImageMemoryRequirements2 #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkGetImageMemoryRequirements2Unsafe ::
               PFN_vkGetImageMemoryRequirements2 ->
                 HS_vkGetImageMemoryRequirements2

foreign import ccall safe "dynamic"
               unwrapVkGetImageMemoryRequirements2Safe ::
               PFN_vkGetImageMemoryRequirements2 ->
                 HS_vkGetImageMemoryRequirements2

instance VulkanProc "vkGetImageMemoryRequirements2" where
    type VkProcType "vkGetImageMemoryRequirements2" =
         HS_vkGetImageMemoryRequirements2
    vkProcSymbol = _VkGetImageMemoryRequirements2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetImageMemoryRequirements2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetImageMemoryRequirements2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetBufferMemoryRequirements2 :: CString

pattern VkGetBufferMemoryRequirements2 <-
        (is_VkGetBufferMemoryRequirements2 -> True)
  where
    VkGetBufferMemoryRequirements2 = _VkGetBufferMemoryRequirements2

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

-- |
-- > void vkGetBufferMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkBufferMemoryRequirementsInfo2* pInfo
-- >     , VkMemoryRequirements2* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetBufferMemoryRequirements2 vkGetBufferMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetBufferMemoryRequirements2 <- vkGetDeviceProc @VkGetBufferMemoryRequirements2 vkDevice
--
-- or less efficient:
--
-- > myGetBufferMemoryRequirements2 <- vkGetProc @VkGetBufferMemoryRequirements2
--
-- __Note:__ @vkGetBufferMemoryRequirements2Unsafe@ and @vkGetBufferMemoryRequirements2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetBufferMemoryRequirements2@ is an alias
--           of @vkGetBufferMemoryRequirements2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetBufferMemoryRequirements2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkGetBufferMemoryRequirements2"
               vkGetBufferMemoryRequirements2Unsafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferMemoryRequirementsInfo2 -- ^ pInfo
                                                     ->
                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                             -> IO ()

#else
vkGetBufferMemoryRequirements2Unsafe ::
                                     VkDevice -- ^ device
                                              ->
                                       Ptr VkBufferMemoryRequirementsInfo2 -- ^ pInfo
                                                                           ->
                                         Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                                                   -> IO ()
vkGetBufferMemoryRequirements2Unsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetBufferMemoryRequirements2)

{-# NOINLINE vkGetBufferMemoryRequirements2Unsafe #-}
#endif

-- |
-- > void vkGetBufferMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkBufferMemoryRequirementsInfo2* pInfo
-- >     , VkMemoryRequirements2* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetBufferMemoryRequirements2 vkGetBufferMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetBufferMemoryRequirements2 <- vkGetDeviceProc @VkGetBufferMemoryRequirements2 vkDevice
--
-- or less efficient:
--
-- > myGetBufferMemoryRequirements2 <- vkGetProc @VkGetBufferMemoryRequirements2
--
-- __Note:__ @vkGetBufferMemoryRequirements2Unsafe@ and @vkGetBufferMemoryRequirements2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetBufferMemoryRequirements2@ is an alias
--           of @vkGetBufferMemoryRequirements2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetBufferMemoryRequirements2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkGetBufferMemoryRequirements2"
               vkGetBufferMemoryRequirements2Safe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferMemoryRequirementsInfo2 -- ^ pInfo
                                                     ->
                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                             -> IO ()

#else
vkGetBufferMemoryRequirements2Safe ::
                                   VkDevice -- ^ device
                                            ->
                                     Ptr VkBufferMemoryRequirementsInfo2 -- ^ pInfo
                                                                         ->
                                       Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                                                 -> IO ()
vkGetBufferMemoryRequirements2Safe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetBufferMemoryRequirements2)

{-# NOINLINE vkGetBufferMemoryRequirements2Safe #-}
#endif

-- |
-- > void vkGetBufferMemoryRequirements2
-- >     ( VkDevice device
-- >     , const VkBufferMemoryRequirementsInfo2* pInfo
-- >     , VkMemoryRequirements2* pMemoryRequirements
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetBufferMemoryRequirements2 vkGetBufferMemoryRequirements2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetBufferMemoryRequirements2 <- vkGetDeviceProc @VkGetBufferMemoryRequirements2 vkDevice
--
-- or less efficient:
--
-- > myGetBufferMemoryRequirements2 <- vkGetProc @VkGetBufferMemoryRequirements2
--
-- __Note:__ @vkGetBufferMemoryRequirements2Unsafe@ and @vkGetBufferMemoryRequirements2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetBufferMemoryRequirements2@ is an alias
--           of @vkGetBufferMemoryRequirements2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetBufferMemoryRequirements2Safe@.
--
vkGetBufferMemoryRequirements2 ::
                               VkDevice -- ^ device
                                        ->
                                 Ptr VkBufferMemoryRequirementsInfo2 -- ^ pInfo
                                                                     ->
                                   Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                                             -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkGetBufferMemoryRequirements2
  = vkGetBufferMemoryRequirements2Unsafe
#else
vkGetBufferMemoryRequirements2 = vkGetBufferMemoryRequirements2Safe

#endif
{-# INLINE vkGetBufferMemoryRequirements2 #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkGetBufferMemoryRequirements2Unsafe ::
               PFN_vkGetBufferMemoryRequirements2 ->
                 HS_vkGetBufferMemoryRequirements2

foreign import ccall safe "dynamic"
               unwrapVkGetBufferMemoryRequirements2Safe ::
               PFN_vkGetBufferMemoryRequirements2 ->
                 HS_vkGetBufferMemoryRequirements2

instance VulkanProc "vkGetBufferMemoryRequirements2" where
    type VkProcType "vkGetBufferMemoryRequirements2" =
         HS_vkGetBufferMemoryRequirements2
    vkProcSymbol = _VkGetBufferMemoryRequirements2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetBufferMemoryRequirements2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetBufferMemoryRequirements2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetImageSparseMemoryRequirements2 :: CString

pattern VkGetImageSparseMemoryRequirements2 <-
        (is_VkGetImageSparseMemoryRequirements2 -> True)
  where
    VkGetImageSparseMemoryRequirements2
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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetImageSparseMemoryRequirements2 <- vkGetDeviceProc @VkGetImageSparseMemoryRequirements2 vkDevice
--
-- or less efficient:
--
-- > myGetImageSparseMemoryRequirements2 <- vkGetProc @VkGetImageSparseMemoryRequirements2
--
-- __Note:__ @vkGetImageSparseMemoryRequirements2Unsafe@ and @vkGetImageSparseMemoryRequirements2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetImageSparseMemoryRequirements2@ is an alias
--           of @vkGetImageSparseMemoryRequirements2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetImageSparseMemoryRequirements2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkGetImageSparseMemoryRequirements2"
               vkGetImageSparseMemoryRequirements2Unsafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageSparseMemoryRequirementsInfo2 -- ^ pInfo
                                                          ->
                   Ptr Word32 -- ^ pSparseMemoryRequirementCount
                              -> Ptr VkSparseImageMemoryRequirements2 -- ^ pSparseMemoryRequirements
                                                                      -> IO ()

#else
vkGetImageSparseMemoryRequirements2Unsafe ::
                                          VkDevice -- ^ device
                                                   ->
                                            Ptr VkImageSparseMemoryRequirementsInfo2 -- ^ pInfo
                                                                                     ->
                                              Ptr Word32 -- ^ pSparseMemoryRequirementCount
                                                         ->
                                                Ptr VkSparseImageMemoryRequirements2 -- ^ pSparseMemoryRequirements
                                                                                     -> IO ()
vkGetImageSparseMemoryRequirements2Unsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetImageSparseMemoryRequirements2)

{-# NOINLINE vkGetImageSparseMemoryRequirements2Unsafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetImageSparseMemoryRequirements2 <- vkGetDeviceProc @VkGetImageSparseMemoryRequirements2 vkDevice
--
-- or less efficient:
--
-- > myGetImageSparseMemoryRequirements2 <- vkGetProc @VkGetImageSparseMemoryRequirements2
--
-- __Note:__ @vkGetImageSparseMemoryRequirements2Unsafe@ and @vkGetImageSparseMemoryRequirements2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetImageSparseMemoryRequirements2@ is an alias
--           of @vkGetImageSparseMemoryRequirements2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetImageSparseMemoryRequirements2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
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
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetImageSparseMemoryRequirements2)

{-# NOINLINE vkGetImageSparseMemoryRequirements2Safe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetImageSparseMemoryRequirements2 <- vkGetDeviceProc @VkGetImageSparseMemoryRequirements2 vkDevice
--
-- or less efficient:
--
-- > myGetImageSparseMemoryRequirements2 <- vkGetProc @VkGetImageSparseMemoryRequirements2
--
-- __Note:__ @vkGetImageSparseMemoryRequirements2Unsafe@ and @vkGetImageSparseMemoryRequirements2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetImageSparseMemoryRequirements2@ is an alias
--           of @vkGetImageSparseMemoryRequirements2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetImageSparseMemoryRequirements2Safe@.
--
vkGetImageSparseMemoryRequirements2 ::
                                    VkDevice -- ^ device
                                             ->
                                      Ptr VkImageSparseMemoryRequirementsInfo2 -- ^ pInfo
                                                                               ->
                                        Ptr Word32 -- ^ pSparseMemoryRequirementCount
                                                   -> Ptr VkSparseImageMemoryRequirements2 -- ^ pSparseMemoryRequirements
                                                                                           -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkGetImageSparseMemoryRequirements2
  = vkGetImageSparseMemoryRequirements2Unsafe
#else
vkGetImageSparseMemoryRequirements2
  = vkGetImageSparseMemoryRequirements2Safe

#endif
{-# INLINE vkGetImageSparseMemoryRequirements2 #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkGetImageSparseMemoryRequirements2Unsafe ::
               PFN_vkGetImageSparseMemoryRequirements2 ->
                 HS_vkGetImageSparseMemoryRequirements2

foreign import ccall safe "dynamic"
               unwrapVkGetImageSparseMemoryRequirements2Safe ::
               PFN_vkGetImageSparseMemoryRequirements2 ->
                 HS_vkGetImageSparseMemoryRequirements2

instance VulkanProc "vkGetImageSparseMemoryRequirements2" where
    type VkProcType "vkGetImageSparseMemoryRequirements2" =
         HS_vkGetImageSparseMemoryRequirements2
    vkProcSymbol = _VkGetImageSparseMemoryRequirements2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetImageSparseMemoryRequirements2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetImageSparseMemoryRequirements2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

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
  where
    VkGetPhysicalDeviceFeatures2 = _VkGetPhysicalDeviceFeatures2

{-# INLINE _VkGetPhysicalDeviceFeatures2 #-}

_VkGetPhysicalDeviceFeatures2 :: CString
_VkGetPhysicalDeviceFeatures2
  = Ptr "vkGetPhysicalDeviceFeatures2\NUL"#

{-# INLINE is_VkGetPhysicalDeviceFeatures2 #-}

is_VkGetPhysicalDeviceFeatures2 :: CString -> Bool
is_VkGetPhysicalDeviceFeatures2
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceFeatures2

type VkGetPhysicalDeviceFeatures2 = "vkGetPhysicalDeviceFeatures2"

-- |
-- > void vkGetPhysicalDeviceFeatures2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceFeatures2* pFeatures
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFeatures2 vkGetPhysicalDeviceFeatures2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceFeatures2 <- vkGetInstanceProc @VkGetPhysicalDeviceFeatures2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceFeatures2 <- vkGetProc @VkGetPhysicalDeviceFeatures2
--
-- __Note:__ @vkGetPhysicalDeviceFeatures2Unsafe@ and @vkGetPhysicalDeviceFeatures2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceFeatures2@ is an alias
--           of @vkGetPhysicalDeviceFeatures2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceFeatures2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkGetPhysicalDeviceFeatures2"
               vkGetPhysicalDeviceFeatures2Unsafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceFeatures2 -- ^ pFeatures
                                                                 -> IO ()

#else
vkGetPhysicalDeviceFeatures2Unsafe ::
                                   VkPhysicalDevice -- ^ physicalDevice
                                                    -> Ptr VkPhysicalDeviceFeatures2 -- ^ pFeatures
                                                                                     -> IO ()
vkGetPhysicalDeviceFeatures2Unsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetPhysicalDeviceFeatures2)

{-# NOINLINE vkGetPhysicalDeviceFeatures2Unsafe #-}
#endif

-- |
-- > void vkGetPhysicalDeviceFeatures2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceFeatures2* pFeatures
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFeatures2 vkGetPhysicalDeviceFeatures2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceFeatures2 <- vkGetInstanceProc @VkGetPhysicalDeviceFeatures2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceFeatures2 <- vkGetProc @VkGetPhysicalDeviceFeatures2
--
-- __Note:__ @vkGetPhysicalDeviceFeatures2Unsafe@ and @vkGetPhysicalDeviceFeatures2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceFeatures2@ is an alias
--           of @vkGetPhysicalDeviceFeatures2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceFeatures2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkGetPhysicalDeviceFeatures2"
               vkGetPhysicalDeviceFeatures2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceFeatures2 -- ^ pFeatures
                                                                 -> IO ()

#else
vkGetPhysicalDeviceFeatures2Safe ::
                                 VkPhysicalDevice -- ^ physicalDevice
                                                  -> Ptr VkPhysicalDeviceFeatures2 -- ^ pFeatures
                                                                                   -> IO ()
vkGetPhysicalDeviceFeatures2Safe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceFeatures2)

{-# NOINLINE vkGetPhysicalDeviceFeatures2Safe #-}
#endif

-- |
-- > void vkGetPhysicalDeviceFeatures2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceFeatures2* pFeatures
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFeatures2 vkGetPhysicalDeviceFeatures2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceFeatures2 <- vkGetInstanceProc @VkGetPhysicalDeviceFeatures2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceFeatures2 <- vkGetProc @VkGetPhysicalDeviceFeatures2
--
-- __Note:__ @vkGetPhysicalDeviceFeatures2Unsafe@ and @vkGetPhysicalDeviceFeatures2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceFeatures2@ is an alias
--           of @vkGetPhysicalDeviceFeatures2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceFeatures2Safe@.
--
vkGetPhysicalDeviceFeatures2 ::
                             VkPhysicalDevice -- ^ physicalDevice
                                              -> Ptr VkPhysicalDeviceFeatures2 -- ^ pFeatures
                                                                               -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkGetPhysicalDeviceFeatures2 = vkGetPhysicalDeviceFeatures2Unsafe
#else
vkGetPhysicalDeviceFeatures2 = vkGetPhysicalDeviceFeatures2Safe

#endif
{-# INLINE vkGetPhysicalDeviceFeatures2 #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceFeatures2Unsafe ::
               PFN_vkGetPhysicalDeviceFeatures2 -> HS_vkGetPhysicalDeviceFeatures2

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceFeatures2Safe ::
               PFN_vkGetPhysicalDeviceFeatures2 -> HS_vkGetPhysicalDeviceFeatures2

instance VulkanProc "vkGetPhysicalDeviceFeatures2" where
    type VkProcType "vkGetPhysicalDeviceFeatures2" =
         HS_vkGetPhysicalDeviceFeatures2
    vkProcSymbol = _VkGetPhysicalDeviceFeatures2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetPhysicalDeviceFeatures2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetPhysicalDeviceFeatures2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceProperties2 :: CString

pattern VkGetPhysicalDeviceProperties2 <-
        (is_VkGetPhysicalDeviceProperties2 -> True)
  where
    VkGetPhysicalDeviceProperties2 = _VkGetPhysicalDeviceProperties2

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

-- |
-- > void vkGetPhysicalDeviceProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceProperties2* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceProperties2 vkGetPhysicalDeviceProperties2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceProperties2 <- vkGetProc @VkGetPhysicalDeviceProperties2
--
-- __Note:__ @vkGetPhysicalDeviceProperties2Unsafe@ and @vkGetPhysicalDeviceProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceProperties2@ is an alias
--           of @vkGetPhysicalDeviceProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceProperties2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkGetPhysicalDeviceProperties2"
               vkGetPhysicalDeviceProperties2Unsafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceProperties2 -- ^ pProperties
                                                                   -> IO ()

#else
vkGetPhysicalDeviceProperties2Unsafe ::
                                     VkPhysicalDevice -- ^ physicalDevice
                                                      -> Ptr VkPhysicalDeviceProperties2 -- ^ pProperties
                                                                                         -> IO ()
vkGetPhysicalDeviceProperties2Unsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetPhysicalDeviceProperties2)

{-# NOINLINE vkGetPhysicalDeviceProperties2Unsafe #-}
#endif

-- |
-- > void vkGetPhysicalDeviceProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceProperties2* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceProperties2 vkGetPhysicalDeviceProperties2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceProperties2 <- vkGetProc @VkGetPhysicalDeviceProperties2
--
-- __Note:__ @vkGetPhysicalDeviceProperties2Unsafe@ and @vkGetPhysicalDeviceProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceProperties2@ is an alias
--           of @vkGetPhysicalDeviceProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceProperties2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkGetPhysicalDeviceProperties2"
               vkGetPhysicalDeviceProperties2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceProperties2 -- ^ pProperties
                                                                   -> IO ()

#else
vkGetPhysicalDeviceProperties2Safe ::
                                   VkPhysicalDevice -- ^ physicalDevice
                                                    -> Ptr VkPhysicalDeviceProperties2 -- ^ pProperties
                                                                                       -> IO ()
vkGetPhysicalDeviceProperties2Safe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceProperties2)

{-# NOINLINE vkGetPhysicalDeviceProperties2Safe #-}
#endif

-- |
-- > void vkGetPhysicalDeviceProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceProperties2* pProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceProperties2 vkGetPhysicalDeviceProperties2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceProperties2 <- vkGetProc @VkGetPhysicalDeviceProperties2
--
-- __Note:__ @vkGetPhysicalDeviceProperties2Unsafe@ and @vkGetPhysicalDeviceProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceProperties2@ is an alias
--           of @vkGetPhysicalDeviceProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceProperties2Safe@.
--
vkGetPhysicalDeviceProperties2 ::
                               VkPhysicalDevice -- ^ physicalDevice
                                                -> Ptr VkPhysicalDeviceProperties2 -- ^ pProperties
                                                                                   -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkGetPhysicalDeviceProperties2
  = vkGetPhysicalDeviceProperties2Unsafe
#else
vkGetPhysicalDeviceProperties2 = vkGetPhysicalDeviceProperties2Safe

#endif
{-# INLINE vkGetPhysicalDeviceProperties2 #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceProperties2Unsafe ::
               PFN_vkGetPhysicalDeviceProperties2 ->
                 HS_vkGetPhysicalDeviceProperties2

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceProperties2Safe ::
               PFN_vkGetPhysicalDeviceProperties2 ->
                 HS_vkGetPhysicalDeviceProperties2

instance VulkanProc "vkGetPhysicalDeviceProperties2" where
    type VkProcType "vkGetPhysicalDeviceProperties2" =
         HS_vkGetPhysicalDeviceProperties2
    vkProcSymbol = _VkGetPhysicalDeviceProperties2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetPhysicalDeviceProperties2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetPhysicalDeviceProperties2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceFormatProperties2 :: CString

pattern VkGetPhysicalDeviceFormatProperties2 <-
        (is_VkGetPhysicalDeviceFormatProperties2 -> True)
  where
    VkGetPhysicalDeviceFormatProperties2
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

-- |
-- > void vkGetPhysicalDeviceFormatProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkFormat format
-- >     , VkFormatProperties2* pFormatProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFormatProperties2 vkGetPhysicalDeviceFormatProperties2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceFormatProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceFormatProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceFormatProperties2 <- vkGetProc @VkGetPhysicalDeviceFormatProperties2
--
-- __Note:__ @vkGetPhysicalDeviceFormatProperties2Unsafe@ and @vkGetPhysicalDeviceFormatProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceFormatProperties2@ is an alias
--           of @vkGetPhysicalDeviceFormatProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceFormatProperties2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkGetPhysicalDeviceFormatProperties2"
               vkGetPhysicalDeviceFormatProperties2Unsafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkFormat -- ^ format
                                            -> Ptr VkFormatProperties2 -- ^ pFormatProperties
                                                                       -> IO ()

#else
vkGetPhysicalDeviceFormatProperties2Unsafe ::
                                           VkPhysicalDevice -- ^ physicalDevice
                                                            ->
                                             VkFormat -- ^ format
                                                      -> Ptr VkFormatProperties2 -- ^ pFormatProperties
                                                                                 -> IO ()
vkGetPhysicalDeviceFormatProperties2Unsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetPhysicalDeviceFormatProperties2)

{-# NOINLINE vkGetPhysicalDeviceFormatProperties2Unsafe #-}
#endif

-- |
-- > void vkGetPhysicalDeviceFormatProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkFormat format
-- >     , VkFormatProperties2* pFormatProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFormatProperties2 vkGetPhysicalDeviceFormatProperties2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceFormatProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceFormatProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceFormatProperties2 <- vkGetProc @VkGetPhysicalDeviceFormatProperties2
--
-- __Note:__ @vkGetPhysicalDeviceFormatProperties2Unsafe@ and @vkGetPhysicalDeviceFormatProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceFormatProperties2@ is an alias
--           of @vkGetPhysicalDeviceFormatProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceFormatProperties2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkGetPhysicalDeviceFormatProperties2"
               vkGetPhysicalDeviceFormatProperties2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkFormat -- ^ format
                                            -> Ptr VkFormatProperties2 -- ^ pFormatProperties
                                                                       -> IO ()

#else
vkGetPhysicalDeviceFormatProperties2Safe ::
                                         VkPhysicalDevice -- ^ physicalDevice
                                                          ->
                                           VkFormat -- ^ format
                                                    -> Ptr VkFormatProperties2 -- ^ pFormatProperties
                                                                               -> IO ()
vkGetPhysicalDeviceFormatProperties2Safe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceFormatProperties2)

{-# NOINLINE vkGetPhysicalDeviceFormatProperties2Safe #-}
#endif

-- |
-- > void vkGetPhysicalDeviceFormatProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkFormat format
-- >     , VkFormatProperties2* pFormatProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceFormatProperties2 vkGetPhysicalDeviceFormatProperties2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceFormatProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceFormatProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceFormatProperties2 <- vkGetProc @VkGetPhysicalDeviceFormatProperties2
--
-- __Note:__ @vkGetPhysicalDeviceFormatProperties2Unsafe@ and @vkGetPhysicalDeviceFormatProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceFormatProperties2@ is an alias
--           of @vkGetPhysicalDeviceFormatProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceFormatProperties2Safe@.
--
vkGetPhysicalDeviceFormatProperties2 ::
                                     VkPhysicalDevice -- ^ physicalDevice
                                                      ->
                                       VkFormat -- ^ format
                                                -> Ptr VkFormatProperties2 -- ^ pFormatProperties
                                                                           -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkGetPhysicalDeviceFormatProperties2
  = vkGetPhysicalDeviceFormatProperties2Unsafe
#else
vkGetPhysicalDeviceFormatProperties2
  = vkGetPhysicalDeviceFormatProperties2Safe

#endif
{-# INLINE vkGetPhysicalDeviceFormatProperties2 #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceFormatProperties2Unsafe ::
               PFN_vkGetPhysicalDeviceFormatProperties2 ->
                 HS_vkGetPhysicalDeviceFormatProperties2

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceFormatProperties2Safe ::
               PFN_vkGetPhysicalDeviceFormatProperties2 ->
                 HS_vkGetPhysicalDeviceFormatProperties2

instance VulkanProc "vkGetPhysicalDeviceFormatProperties2" where
    type VkProcType "vkGetPhysicalDeviceFormatProperties2" =
         HS_vkGetPhysicalDeviceFormatProperties2
    vkProcSymbol = _VkGetPhysicalDeviceFormatProperties2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceFormatProperties2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceFormatProperties2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceImageFormatProperties2 :: CString

pattern VkGetPhysicalDeviceImageFormatProperties2 <-
        (is_VkGetPhysicalDeviceImageFormatProperties2 -> True)
  where
    VkGetPhysicalDeviceImageFormatProperties2
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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceImageFormatProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceImageFormatProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceImageFormatProperties2 <- vkGetProc @VkGetPhysicalDeviceImageFormatProperties2
--
-- __Note:__ @vkGetPhysicalDeviceImageFormatProperties2Unsafe@ and @vkGetPhysicalDeviceImageFormatProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceImageFormatProperties2@ is an alias
--           of @vkGetPhysicalDeviceImageFormatProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceImageFormatProperties2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe
               "vkGetPhysicalDeviceImageFormatProperties2"
               vkGetPhysicalDeviceImageFormatProperties2Unsafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceImageFormatInfo2 -- ^ pImageFormatInfo
                                                      ->
                   Ptr VkImageFormatProperties2 -- ^ pImageFormatProperties
                                                -> IO VkResult

#else
vkGetPhysicalDeviceImageFormatProperties2Unsafe ::
                                                VkPhysicalDevice -- ^ physicalDevice
                                                                 ->
                                                  Ptr VkPhysicalDeviceImageFormatInfo2 -- ^ pImageFormatInfo
                                                                                       ->
                                                    Ptr VkImageFormatProperties2 -- ^ pImageFormatProperties
                                                                                 -> IO VkResult
vkGetPhysicalDeviceImageFormatProperties2Unsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetPhysicalDeviceImageFormatProperties2)

{-# NOINLINE vkGetPhysicalDeviceImageFormatProperties2Unsafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceImageFormatProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceImageFormatProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceImageFormatProperties2 <- vkGetProc @VkGetPhysicalDeviceImageFormatProperties2
--
-- __Note:__ @vkGetPhysicalDeviceImageFormatProperties2Unsafe@ and @vkGetPhysicalDeviceImageFormatProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceImageFormatProperties2@ is an alias
--           of @vkGetPhysicalDeviceImageFormatProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceImageFormatProperties2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
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
vkGetPhysicalDeviceImageFormatProperties2Safe ::
                                              VkPhysicalDevice -- ^ physicalDevice
                                                               ->
                                                Ptr VkPhysicalDeviceImageFormatInfo2 -- ^ pImageFormatInfo
                                                                                     ->
                                                  Ptr VkImageFormatProperties2 -- ^ pImageFormatProperties
                                                                               -> IO VkResult
vkGetPhysicalDeviceImageFormatProperties2Safe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceImageFormatProperties2)

{-# NOINLINE vkGetPhysicalDeviceImageFormatProperties2Safe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceImageFormatProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceImageFormatProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceImageFormatProperties2 <- vkGetProc @VkGetPhysicalDeviceImageFormatProperties2
--
-- __Note:__ @vkGetPhysicalDeviceImageFormatProperties2Unsafe@ and @vkGetPhysicalDeviceImageFormatProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceImageFormatProperties2@ is an alias
--           of @vkGetPhysicalDeviceImageFormatProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceImageFormatProperties2Safe@.
--
vkGetPhysicalDeviceImageFormatProperties2 ::
                                          VkPhysicalDevice -- ^ physicalDevice
                                                           ->
                                            Ptr VkPhysicalDeviceImageFormatInfo2 -- ^ pImageFormatInfo
                                                                                 ->
                                              Ptr VkImageFormatProperties2 -- ^ pImageFormatProperties
                                                                           -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkGetPhysicalDeviceImageFormatProperties2
  = vkGetPhysicalDeviceImageFormatProperties2Unsafe
#else
vkGetPhysicalDeviceImageFormatProperties2
  = vkGetPhysicalDeviceImageFormatProperties2Safe

#endif
{-# INLINE vkGetPhysicalDeviceImageFormatProperties2 #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceImageFormatProperties2Unsafe ::
               PFN_vkGetPhysicalDeviceImageFormatProperties2 ->
                 HS_vkGetPhysicalDeviceImageFormatProperties2

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceImageFormatProperties2Safe ::
               PFN_vkGetPhysicalDeviceImageFormatProperties2 ->
                 HS_vkGetPhysicalDeviceImageFormatProperties2

instance VulkanProc "vkGetPhysicalDeviceImageFormatProperties2"
         where
    type VkProcType "vkGetPhysicalDeviceImageFormatProperties2" =
         HS_vkGetPhysicalDeviceImageFormatProperties2
    vkProcSymbol = _VkGetPhysicalDeviceImageFormatProperties2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceImageFormatProperties2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceImageFormatProperties2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceQueueFamilyProperties2 :: CString

pattern VkGetPhysicalDeviceQueueFamilyProperties2 <-
        (is_VkGetPhysicalDeviceQueueFamilyProperties2 -> True)
  where
    VkGetPhysicalDeviceQueueFamilyProperties2
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

-- |
-- > void vkGetPhysicalDeviceQueueFamilyProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t* pQueueFamilyPropertyCount
-- >     , VkQueueFamilyProperties2* pQueueFamilyProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceQueueFamilyProperties2 vkGetPhysicalDeviceQueueFamilyProperties2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceQueueFamilyProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceQueueFamilyProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceQueueFamilyProperties2 <- vkGetProc @VkGetPhysicalDeviceQueueFamilyProperties2
--
-- __Note:__ @vkGetPhysicalDeviceQueueFamilyProperties2Unsafe@ and @vkGetPhysicalDeviceQueueFamilyProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceQueueFamilyProperties2@ is an alias
--           of @vkGetPhysicalDeviceQueueFamilyProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceQueueFamilyProperties2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe
               "vkGetPhysicalDeviceQueueFamilyProperties2"
               vkGetPhysicalDeviceQueueFamilyProperties2Unsafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pQueueFamilyPropertyCount
                            -> Ptr VkQueueFamilyProperties2 -- ^ pQueueFamilyProperties
                                                            -> IO ()

#else
vkGetPhysicalDeviceQueueFamilyProperties2Unsafe ::
                                                VkPhysicalDevice -- ^ physicalDevice
                                                                 ->
                                                  Ptr Word32 -- ^ pQueueFamilyPropertyCount
                                                             ->
                                                    Ptr VkQueueFamilyProperties2 -- ^ pQueueFamilyProperties
                                                                                 -> IO ()
vkGetPhysicalDeviceQueueFamilyProperties2Unsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetPhysicalDeviceQueueFamilyProperties2)

{-# NOINLINE vkGetPhysicalDeviceQueueFamilyProperties2Unsafe #-}
#endif

-- |
-- > void vkGetPhysicalDeviceQueueFamilyProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t* pQueueFamilyPropertyCount
-- >     , VkQueueFamilyProperties2* pQueueFamilyProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceQueueFamilyProperties2 vkGetPhysicalDeviceQueueFamilyProperties2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceQueueFamilyProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceQueueFamilyProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceQueueFamilyProperties2 <- vkGetProc @VkGetPhysicalDeviceQueueFamilyProperties2
--
-- __Note:__ @vkGetPhysicalDeviceQueueFamilyProperties2Unsafe@ and @vkGetPhysicalDeviceQueueFamilyProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceQueueFamilyProperties2@ is an alias
--           of @vkGetPhysicalDeviceQueueFamilyProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceQueueFamilyProperties2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe
               "vkGetPhysicalDeviceQueueFamilyProperties2"
               vkGetPhysicalDeviceQueueFamilyProperties2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pQueueFamilyPropertyCount
                            -> Ptr VkQueueFamilyProperties2 -- ^ pQueueFamilyProperties
                                                            -> IO ()

#else
vkGetPhysicalDeviceQueueFamilyProperties2Safe ::
                                              VkPhysicalDevice -- ^ physicalDevice
                                                               ->
                                                Ptr Word32 -- ^ pQueueFamilyPropertyCount
                                                           -> Ptr VkQueueFamilyProperties2 -- ^ pQueueFamilyProperties
                                                                                           -> IO ()
vkGetPhysicalDeviceQueueFamilyProperties2Safe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceQueueFamilyProperties2)

{-# NOINLINE vkGetPhysicalDeviceQueueFamilyProperties2Safe #-}
#endif

-- |
-- > void vkGetPhysicalDeviceQueueFamilyProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t* pQueueFamilyPropertyCount
-- >     , VkQueueFamilyProperties2* pQueueFamilyProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceQueueFamilyProperties2 vkGetPhysicalDeviceQueueFamilyProperties2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceQueueFamilyProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceQueueFamilyProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceQueueFamilyProperties2 <- vkGetProc @VkGetPhysicalDeviceQueueFamilyProperties2
--
-- __Note:__ @vkGetPhysicalDeviceQueueFamilyProperties2Unsafe@ and @vkGetPhysicalDeviceQueueFamilyProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceQueueFamilyProperties2@ is an alias
--           of @vkGetPhysicalDeviceQueueFamilyProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceQueueFamilyProperties2Safe@.
--
vkGetPhysicalDeviceQueueFamilyProperties2 ::
                                          VkPhysicalDevice -- ^ physicalDevice
                                                           ->
                                            Ptr Word32 -- ^ pQueueFamilyPropertyCount
                                                       -> Ptr VkQueueFamilyProperties2 -- ^ pQueueFamilyProperties
                                                                                       -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkGetPhysicalDeviceQueueFamilyProperties2
  = vkGetPhysicalDeviceQueueFamilyProperties2Unsafe
#else
vkGetPhysicalDeviceQueueFamilyProperties2
  = vkGetPhysicalDeviceQueueFamilyProperties2Safe

#endif
{-# INLINE vkGetPhysicalDeviceQueueFamilyProperties2 #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceQueueFamilyProperties2Unsafe ::
               PFN_vkGetPhysicalDeviceQueueFamilyProperties2 ->
                 HS_vkGetPhysicalDeviceQueueFamilyProperties2

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceQueueFamilyProperties2Safe ::
               PFN_vkGetPhysicalDeviceQueueFamilyProperties2 ->
                 HS_vkGetPhysicalDeviceQueueFamilyProperties2

instance VulkanProc "vkGetPhysicalDeviceQueueFamilyProperties2"
         where
    type VkProcType "vkGetPhysicalDeviceQueueFamilyProperties2" =
         HS_vkGetPhysicalDeviceQueueFamilyProperties2
    vkProcSymbol = _VkGetPhysicalDeviceQueueFamilyProperties2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceQueueFamilyProperties2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceQueueFamilyProperties2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceMemoryProperties2 :: CString

pattern VkGetPhysicalDeviceMemoryProperties2 <-
        (is_VkGetPhysicalDeviceMemoryProperties2 -> True)
  where
    VkGetPhysicalDeviceMemoryProperties2
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

-- |
-- > void vkGetPhysicalDeviceMemoryProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceMemoryProperties2* pMemoryProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMemoryProperties2 vkGetPhysicalDeviceMemoryProperties2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceMemoryProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceMemoryProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceMemoryProperties2 <- vkGetProc @VkGetPhysicalDeviceMemoryProperties2
--
-- __Note:__ @vkGetPhysicalDeviceMemoryProperties2Unsafe@ and @vkGetPhysicalDeviceMemoryProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceMemoryProperties2@ is an alias
--           of @vkGetPhysicalDeviceMemoryProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceMemoryProperties2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkGetPhysicalDeviceMemoryProperties2"
               vkGetPhysicalDeviceMemoryProperties2Unsafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceMemoryProperties2 -- ^ pMemoryProperties
                                                                         -> IO ()

#else
vkGetPhysicalDeviceMemoryProperties2Unsafe ::
                                           VkPhysicalDevice -- ^ physicalDevice
                                                            ->
                                             Ptr VkPhysicalDeviceMemoryProperties2 -- ^ pMemoryProperties
                                                                                   -> IO ()
vkGetPhysicalDeviceMemoryProperties2Unsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetPhysicalDeviceMemoryProperties2)

{-# NOINLINE vkGetPhysicalDeviceMemoryProperties2Unsafe #-}
#endif

-- |
-- > void vkGetPhysicalDeviceMemoryProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceMemoryProperties2* pMemoryProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMemoryProperties2 vkGetPhysicalDeviceMemoryProperties2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceMemoryProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceMemoryProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceMemoryProperties2 <- vkGetProc @VkGetPhysicalDeviceMemoryProperties2
--
-- __Note:__ @vkGetPhysicalDeviceMemoryProperties2Unsafe@ and @vkGetPhysicalDeviceMemoryProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceMemoryProperties2@ is an alias
--           of @vkGetPhysicalDeviceMemoryProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceMemoryProperties2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkGetPhysicalDeviceMemoryProperties2"
               vkGetPhysicalDeviceMemoryProperties2Safe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceMemoryProperties2 -- ^ pMemoryProperties
                                                                         -> IO ()

#else
vkGetPhysicalDeviceMemoryProperties2Safe ::
                                         VkPhysicalDevice -- ^ physicalDevice
                                                          ->
                                           Ptr VkPhysicalDeviceMemoryProperties2 -- ^ pMemoryProperties
                                                                                 -> IO ()
vkGetPhysicalDeviceMemoryProperties2Safe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceMemoryProperties2)

{-# NOINLINE vkGetPhysicalDeviceMemoryProperties2Safe #-}
#endif

-- |
-- > void vkGetPhysicalDeviceMemoryProperties2
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkPhysicalDeviceMemoryProperties2* pMemoryProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMemoryProperties2 vkGetPhysicalDeviceMemoryProperties2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceMemoryProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceMemoryProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceMemoryProperties2 <- vkGetProc @VkGetPhysicalDeviceMemoryProperties2
--
-- __Note:__ @vkGetPhysicalDeviceMemoryProperties2Unsafe@ and @vkGetPhysicalDeviceMemoryProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceMemoryProperties2@ is an alias
--           of @vkGetPhysicalDeviceMemoryProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceMemoryProperties2Safe@.
--
vkGetPhysicalDeviceMemoryProperties2 ::
                                     VkPhysicalDevice -- ^ physicalDevice
                                                      ->
                                       Ptr VkPhysicalDeviceMemoryProperties2 -- ^ pMemoryProperties
                                                                             -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkGetPhysicalDeviceMemoryProperties2
  = vkGetPhysicalDeviceMemoryProperties2Unsafe
#else
vkGetPhysicalDeviceMemoryProperties2
  = vkGetPhysicalDeviceMemoryProperties2Safe

#endif
{-# INLINE vkGetPhysicalDeviceMemoryProperties2 #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceMemoryProperties2Unsafe ::
               PFN_vkGetPhysicalDeviceMemoryProperties2 ->
                 HS_vkGetPhysicalDeviceMemoryProperties2

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceMemoryProperties2Safe ::
               PFN_vkGetPhysicalDeviceMemoryProperties2 ->
                 HS_vkGetPhysicalDeviceMemoryProperties2

instance VulkanProc "vkGetPhysicalDeviceMemoryProperties2" where
    type VkProcType "vkGetPhysicalDeviceMemoryProperties2" =
         HS_vkGetPhysicalDeviceMemoryProperties2
    vkProcSymbol = _VkGetPhysicalDeviceMemoryProperties2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceMemoryProperties2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceMemoryProperties2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceSparseImageFormatProperties2 :: CString

pattern VkGetPhysicalDeviceSparseImageFormatProperties2 <-
        (is_VkGetPhysicalDeviceSparseImageFormatProperties2 -> True)
  where
    VkGetPhysicalDeviceSparseImageFormatProperties2
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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceSparseImageFormatProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceSparseImageFormatProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceSparseImageFormatProperties2 <- vkGetProc @VkGetPhysicalDeviceSparseImageFormatProperties2
--
-- __Note:__ @vkGetPhysicalDeviceSparseImageFormatProperties2Unsafe@ and @vkGetPhysicalDeviceSparseImageFormatProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceSparseImageFormatProperties2@ is an alias
--           of @vkGetPhysicalDeviceSparseImageFormatProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceSparseImageFormatProperties2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe
               "vkGetPhysicalDeviceSparseImageFormatProperties2"
               vkGetPhysicalDeviceSparseImageFormatProperties2Unsafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceSparseImageFormatInfo2 -- ^ pFormatInfo
                                                            ->
                   Ptr Word32 -- ^ pPropertyCount
                              -> Ptr VkSparseImageFormatProperties2 -- ^ pProperties
                                                                    -> IO ()

#else
vkGetPhysicalDeviceSparseImageFormatProperties2Unsafe ::
                                                      VkPhysicalDevice -- ^ physicalDevice
                                                                       ->
                                                        Ptr VkPhysicalDeviceSparseImageFormatInfo2 -- ^ pFormatInfo
                                                          ->
                                                          Ptr Word32 -- ^ pPropertyCount
                                                                     ->
                                                            Ptr VkSparseImageFormatProperties2 -- ^ pProperties
                                                                                               ->
                                                              IO ()
vkGetPhysicalDeviceSparseImageFormatProperties2Unsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetPhysicalDeviceSparseImageFormatProperties2)

{-# NOINLINE vkGetPhysicalDeviceSparseImageFormatProperties2Unsafe
             #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceSparseImageFormatProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceSparseImageFormatProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceSparseImageFormatProperties2 <- vkGetProc @VkGetPhysicalDeviceSparseImageFormatProperties2
--
-- __Note:__ @vkGetPhysicalDeviceSparseImageFormatProperties2Unsafe@ and @vkGetPhysicalDeviceSparseImageFormatProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceSparseImageFormatProperties2@ is an alias
--           of @vkGetPhysicalDeviceSparseImageFormatProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceSparseImageFormatProperties2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
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
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceSparseImageFormatProperties2)

{-# NOINLINE vkGetPhysicalDeviceSparseImageFormatProperties2Safe
             #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceSparseImageFormatProperties2 <- vkGetInstanceProc @VkGetPhysicalDeviceSparseImageFormatProperties2 vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceSparseImageFormatProperties2 <- vkGetProc @VkGetPhysicalDeviceSparseImageFormatProperties2
--
-- __Note:__ @vkGetPhysicalDeviceSparseImageFormatProperties2Unsafe@ and @vkGetPhysicalDeviceSparseImageFormatProperties2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceSparseImageFormatProperties2@ is an alias
--           of @vkGetPhysicalDeviceSparseImageFormatProperties2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceSparseImageFormatProperties2Safe@.
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
#ifdef UNSAFE_FFI_DEFAULT
vkGetPhysicalDeviceSparseImageFormatProperties2
  = vkGetPhysicalDeviceSparseImageFormatProperties2Unsafe
#else
vkGetPhysicalDeviceSparseImageFormatProperties2
  = vkGetPhysicalDeviceSparseImageFormatProperties2Safe

#endif
{-# INLINE vkGetPhysicalDeviceSparseImageFormatProperties2 #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceSparseImageFormatProperties2Unsafe ::
               PFN_vkGetPhysicalDeviceSparseImageFormatProperties2 ->
                 HS_vkGetPhysicalDeviceSparseImageFormatProperties2

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceSparseImageFormatProperties2Safe ::
               PFN_vkGetPhysicalDeviceSparseImageFormatProperties2 ->
                 HS_vkGetPhysicalDeviceSparseImageFormatProperties2

instance VulkanProc
           "vkGetPhysicalDeviceSparseImageFormatProperties2"
         where
    type VkProcType "vkGetPhysicalDeviceSparseImageFormatProperties2" =
         HS_vkGetPhysicalDeviceSparseImageFormatProperties2
    vkProcSymbol = _VkGetPhysicalDeviceSparseImageFormatProperties2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceSparseImageFormatProperties2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceSparseImageFormatProperties2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

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
  where
    VkTrimCommandPool = _VkTrimCommandPool

{-# INLINE _VkTrimCommandPool #-}

_VkTrimCommandPool :: CString
_VkTrimCommandPool = Ptr "vkTrimCommandPool\NUL"#

{-# INLINE is_VkTrimCommandPool #-}

is_VkTrimCommandPool :: CString -> Bool
is_VkTrimCommandPool = (EQ ==) . cmpCStrings _VkTrimCommandPool

type VkTrimCommandPool = "vkTrimCommandPool"

-- |
-- > void vkTrimCommandPool
-- >     ( VkDevice device
-- >     , VkCommandPool commandPool
-- >     , VkCommandPoolTrimFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkTrimCommandPool vkTrimCommandPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myTrimCommandPool <- vkGetDeviceProc @VkTrimCommandPool vkDevice
--
-- or less efficient:
--
-- > myTrimCommandPool <- vkGetProc @VkTrimCommandPool
--
-- __Note:__ @vkTrimCommandPoolUnsafe@ and @vkTrimCommandPoolSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkTrimCommandPool@ is an alias
--           of @vkTrimCommandPoolUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkTrimCommandPoolSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkTrimCommandPool"
               vkTrimCommandPoolUnsafe ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> VkCommandPoolTrimFlags -- ^ flags
                                                                   -> IO ()

#else
vkTrimCommandPoolUnsafe ::
                        VkDevice -- ^ device
                                 -> VkCommandPool -- ^ commandPool
                                                  -> VkCommandPoolTrimFlags -- ^ flags
                                                                            -> IO ()
vkTrimCommandPoolUnsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkTrimCommandPool)

{-# NOINLINE vkTrimCommandPoolUnsafe #-}
#endif

-- |
-- > void vkTrimCommandPool
-- >     ( VkDevice device
-- >     , VkCommandPool commandPool
-- >     , VkCommandPoolTrimFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkTrimCommandPool vkTrimCommandPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myTrimCommandPool <- vkGetDeviceProc @VkTrimCommandPool vkDevice
--
-- or less efficient:
--
-- > myTrimCommandPool <- vkGetProc @VkTrimCommandPool
--
-- __Note:__ @vkTrimCommandPoolUnsafe@ and @vkTrimCommandPoolSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkTrimCommandPool@ is an alias
--           of @vkTrimCommandPoolUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkTrimCommandPoolSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkTrimCommandPool" vkTrimCommandPoolSafe
               :: VkDevice -- ^ device
                           -> VkCommandPool -- ^ commandPool
                                            -> VkCommandPoolTrimFlags -- ^ flags
                                                                      -> IO ()

#else
vkTrimCommandPoolSafe ::
                      VkDevice -- ^ device
                               -> VkCommandPool -- ^ commandPool
                                                -> VkCommandPoolTrimFlags -- ^ flags
                                                                          -> IO ()
vkTrimCommandPoolSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkTrimCommandPool)

{-# NOINLINE vkTrimCommandPoolSafe #-}
#endif

-- |
-- > void vkTrimCommandPool
-- >     ( VkDevice device
-- >     , VkCommandPool commandPool
-- >     , VkCommandPoolTrimFlags flags
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkTrimCommandPool vkTrimCommandPool registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myTrimCommandPool <- vkGetDeviceProc @VkTrimCommandPool vkDevice
--
-- or less efficient:
--
-- > myTrimCommandPool <- vkGetProc @VkTrimCommandPool
--
-- __Note:__ @vkTrimCommandPoolUnsafe@ and @vkTrimCommandPoolSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkTrimCommandPool@ is an alias
--           of @vkTrimCommandPoolUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkTrimCommandPoolSafe@.
--
vkTrimCommandPool ::
                  VkDevice -- ^ device
                           -> VkCommandPool -- ^ commandPool
                                            -> VkCommandPoolTrimFlags -- ^ flags
                                                                      -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkTrimCommandPool = vkTrimCommandPoolUnsafe
#else
vkTrimCommandPool = vkTrimCommandPoolSafe

#endif
{-# INLINE vkTrimCommandPool #-}

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

foreign import ccall unsafe "dynamic" unwrapVkTrimCommandPoolUnsafe
               :: PFN_vkTrimCommandPool -> HS_vkTrimCommandPool

foreign import ccall safe "dynamic" unwrapVkTrimCommandPoolSafe ::
               PFN_vkTrimCommandPool -> HS_vkTrimCommandPool

instance VulkanProc "vkTrimCommandPool" where
    type VkProcType "vkTrimCommandPool" = HS_vkTrimCommandPool
    vkProcSymbol = _VkTrimCommandPool

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkTrimCommandPoolUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkTrimCommandPoolSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_ERROR_OUT_OF_POOL_MEMORY :: VkResult

pattern VK_ERROR_OUT_OF_POOL_MEMORY = VkResult (-1000069000)

-- | Format can be used as the source image of image transfer commands
--
--   bitpos = @14@
pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT =
        VkFormatFeatureBitmask 16384

-- | Format can be used as the destination image of image transfer commands
--
--   bitpos = @15@
pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT =
        VkFormatFeatureBitmask 32768

-- | The 3D image can be viewed as a 2D or 2D array image
--
--   bitpos = @5@
pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT ::
        VkImageCreateBitmask a

pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT =
        VkImageCreateBitmask 32

-- | bitpos = @7@
pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT ::
        VkImageCreateBitmask a

pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT =
        VkImageCreateBitmask 128

-- | bitpos = @8@
pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT ::
        VkImageCreateBitmask a

pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT =
        VkImageCreateBitmask 256

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
pattern VK_DEPENDENCY_VIEW_LOCAL_BIT :: VkDependencyBitmask a

pattern VK_DEPENDENCY_VIEW_LOCAL_BIT = VkDependencyBitmask 2

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
        = VkStructureType 1000120000

pattern VkGetDeviceQueue2 :: CString

pattern VkGetDeviceQueue2 <- (is_VkGetDeviceQueue2 -> True)
  where
    VkGetDeviceQueue2 = _VkGetDeviceQueue2

{-# INLINE _VkGetDeviceQueue2 #-}

_VkGetDeviceQueue2 :: CString
_VkGetDeviceQueue2 = Ptr "vkGetDeviceQueue2\NUL"#

{-# INLINE is_VkGetDeviceQueue2 #-}

is_VkGetDeviceQueue2 :: CString -> Bool
is_VkGetDeviceQueue2 = (EQ ==) . cmpCStrings _VkGetDeviceQueue2

type VkGetDeviceQueue2 = "vkGetDeviceQueue2"

-- |
-- > void vkGetDeviceQueue2
-- >     ( VkDevice device
-- >     , const VkDeviceQueueInfo2* pQueueInfo
-- >     , VkQueue* pQueue
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceQueue2 vkGetDeviceQueue2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceQueue2 <- vkGetDeviceProc @VkGetDeviceQueue2 vkDevice
--
-- or less efficient:
--
-- > myGetDeviceQueue2 <- vkGetProc @VkGetDeviceQueue2
--
-- __Note:__ @vkGetDeviceQueue2Unsafe@ and @vkGetDeviceQueue2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDeviceQueue2@ is an alias
--           of @vkGetDeviceQueue2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDeviceQueue2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkGetDeviceQueue2"
               vkGetDeviceQueue2Unsafe ::
               VkDevice -- ^ device
                        -> Ptr VkDeviceQueueInfo2 -- ^ pQueueInfo
                                                  -> Ptr VkQueue -- ^ pQueue
                                                                 -> IO ()

#else
vkGetDeviceQueue2Unsafe ::
                        VkDevice -- ^ device
                                 -> Ptr VkDeviceQueueInfo2 -- ^ pQueueInfo
                                                           -> Ptr VkQueue -- ^ pQueue
                                                                          -> IO ()
vkGetDeviceQueue2Unsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkGetDeviceQueue2)

{-# NOINLINE vkGetDeviceQueue2Unsafe #-}
#endif

-- |
-- > void vkGetDeviceQueue2
-- >     ( VkDevice device
-- >     , const VkDeviceQueueInfo2* pQueueInfo
-- >     , VkQueue* pQueue
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceQueue2 vkGetDeviceQueue2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceQueue2 <- vkGetDeviceProc @VkGetDeviceQueue2 vkDevice
--
-- or less efficient:
--
-- > myGetDeviceQueue2 <- vkGetProc @VkGetDeviceQueue2
--
-- __Note:__ @vkGetDeviceQueue2Unsafe@ and @vkGetDeviceQueue2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDeviceQueue2@ is an alias
--           of @vkGetDeviceQueue2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDeviceQueue2Safe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkGetDeviceQueue2" vkGetDeviceQueue2Safe
               :: VkDevice -- ^ device
                           -> Ptr VkDeviceQueueInfo2 -- ^ pQueueInfo
                                                     -> Ptr VkQueue -- ^ pQueue
                                                                    -> IO ()

#else
vkGetDeviceQueue2Safe ::
                      VkDevice -- ^ device
                               -> Ptr VkDeviceQueueInfo2 -- ^ pQueueInfo
                                                         -> Ptr VkQueue -- ^ pQueue
                                                                        -> IO ()
vkGetDeviceQueue2Safe
  = unsafeDupablePerformIO (vkGetProcSafe @VkGetDeviceQueue2)

{-# NOINLINE vkGetDeviceQueue2Safe #-}
#endif

-- |
-- > void vkGetDeviceQueue2
-- >     ( VkDevice device
-- >     , const VkDeviceQueueInfo2* pQueueInfo
-- >     , VkQueue* pQueue
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceQueue2 vkGetDeviceQueue2 registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceQueue2 <- vkGetDeviceProc @VkGetDeviceQueue2 vkDevice
--
-- or less efficient:
--
-- > myGetDeviceQueue2 <- vkGetProc @VkGetDeviceQueue2
--
-- __Note:__ @vkGetDeviceQueue2Unsafe@ and @vkGetDeviceQueue2Safe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDeviceQueue2@ is an alias
--           of @vkGetDeviceQueue2Unsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDeviceQueue2Safe@.
--
vkGetDeviceQueue2 ::
                  VkDevice -- ^ device
                           -> Ptr VkDeviceQueueInfo2 -- ^ pQueueInfo
                                                     -> Ptr VkQueue -- ^ pQueue
                                                                    -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkGetDeviceQueue2 = vkGetDeviceQueue2Unsafe
#else
vkGetDeviceQueue2 = vkGetDeviceQueue2Safe

#endif
{-# INLINE vkGetDeviceQueue2 #-}

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

foreign import ccall unsafe "dynamic" unwrapVkGetDeviceQueue2Unsafe
               :: PFN_vkGetDeviceQueue2 -> HS_vkGetDeviceQueue2

foreign import ccall safe "dynamic" unwrapVkGetDeviceQueue2Safe ::
               PFN_vkGetDeviceQueue2 -> HS_vkGetDeviceQueue2

instance VulkanProc "vkGetDeviceQueue2" where
    type VkProcType "vkGetDeviceQueue2" = HS_vkGetDeviceQueue2
    vkProcSymbol = _VkGetDeviceQueue2

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetDeviceQueue2Unsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetDeviceQueue2Safe

    {-# INLINE unwrapVkProcPtrSafe #-}

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
pattern VK_QUEUE_PROTECTED_BIT :: VkQueueBitmask a

pattern VK_QUEUE_PROTECTED_BIT = VkQueueBitmask 16

-- | Queue is a protected-capable device queue
--
--   bitpos = @0@
pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT ::
        VkDeviceQueueCreateBitmask a

pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT =
        VkDeviceQueueCreateBitmask 1

-- | Memory is protected
--
--   bitpos = @5@
pattern VK_MEMORY_PROPERTY_PROTECTED_BIT ::
        VkMemoryPropertyBitmask a

pattern VK_MEMORY_PROPERTY_PROTECTED_BIT =
        VkMemoryPropertyBitmask 32

-- | Buffer requires protected memory
--
--   bitpos = @3@
pattern VK_BUFFER_CREATE_PROTECTED_BIT :: VkBufferCreateBitmask a

pattern VK_BUFFER_CREATE_PROTECTED_BIT = VkBufferCreateBitmask 8

-- | Image requires protected memory
--
--   bitpos = @11@
pattern VK_IMAGE_CREATE_PROTECTED_BIT :: VkImageCreateBitmask a

pattern VK_IMAGE_CREATE_PROTECTED_BIT = VkImageCreateBitmask 2048

-- | Command buffers allocated from pool are protected command buffers
--
--   bitpos = @2@
pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT ::
        VkCommandPoolCreateBitmask a

pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT =
        VkCommandPoolCreateBitmask 4

pattern VkCreateSamplerYcbcrConversion :: CString

pattern VkCreateSamplerYcbcrConversion <-
        (is_VkCreateSamplerYcbcrConversion -> True)
  where
    VkCreateSamplerYcbcrConversion = _VkCreateSamplerYcbcrConversion

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSamplerYcbcrConversion <- vkGetDeviceProc @VkCreateSamplerYcbcrConversion vkDevice
--
-- or less efficient:
--
-- > myCreateSamplerYcbcrConversion <- vkGetProc @VkCreateSamplerYcbcrConversion
--
-- __Note:__ @vkCreateSamplerYcbcrConversionUnsafe@ and @vkCreateSamplerYcbcrConversionSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateSamplerYcbcrConversion@ is an alias
--           of @vkCreateSamplerYcbcrConversionUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateSamplerYcbcrConversionSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkCreateSamplerYcbcrConversion"
               vkCreateSamplerYcbcrConversionUnsafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSamplerYcbcrConversionCreateInfo -- ^ pCreateInfo
                                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkSamplerYcbcrConversion -- ^ pYcbcrConversion
                                                  -> IO VkResult

#else
vkCreateSamplerYcbcrConversionUnsafe ::
                                     VkDevice -- ^ device
                                              ->
                                       Ptr VkSamplerYcbcrConversionCreateInfo -- ^ pCreateInfo
                                                                              ->
                                         Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                   ->
                                           Ptr VkSamplerYcbcrConversion -- ^ pYcbcrConversion
                                                                        -> IO VkResult
vkCreateSamplerYcbcrConversionUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkCreateSamplerYcbcrConversion)

{-# NOINLINE vkCreateSamplerYcbcrConversionUnsafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSamplerYcbcrConversion <- vkGetDeviceProc @VkCreateSamplerYcbcrConversion vkDevice
--
-- or less efficient:
--
-- > myCreateSamplerYcbcrConversion <- vkGetProc @VkCreateSamplerYcbcrConversion
--
-- __Note:__ @vkCreateSamplerYcbcrConversionUnsafe@ and @vkCreateSamplerYcbcrConversionSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateSamplerYcbcrConversion@ is an alias
--           of @vkCreateSamplerYcbcrConversionUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateSamplerYcbcrConversionSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
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
vkCreateSamplerYcbcrConversionSafe ::
                                   VkDevice -- ^ device
                                            ->
                                     Ptr VkSamplerYcbcrConversionCreateInfo -- ^ pCreateInfo
                                                                            ->
                                       Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                 ->
                                         Ptr VkSamplerYcbcrConversion -- ^ pYcbcrConversion
                                                                      -> IO VkResult
vkCreateSamplerYcbcrConversionSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkCreateSamplerYcbcrConversion)

{-# NOINLINE vkCreateSamplerYcbcrConversionSafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSamplerYcbcrConversion <- vkGetDeviceProc @VkCreateSamplerYcbcrConversion vkDevice
--
-- or less efficient:
--
-- > myCreateSamplerYcbcrConversion <- vkGetProc @VkCreateSamplerYcbcrConversion
--
-- __Note:__ @vkCreateSamplerYcbcrConversionUnsafe@ and @vkCreateSamplerYcbcrConversionSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateSamplerYcbcrConversion@ is an alias
--           of @vkCreateSamplerYcbcrConversionUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateSamplerYcbcrConversionSafe@.
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
#ifdef UNSAFE_FFI_DEFAULT
vkCreateSamplerYcbcrConversion
  = vkCreateSamplerYcbcrConversionUnsafe
#else
vkCreateSamplerYcbcrConversion = vkCreateSamplerYcbcrConversionSafe

#endif
{-# INLINE vkCreateSamplerYcbcrConversion #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkCreateSamplerYcbcrConversionUnsafe ::
               PFN_vkCreateSamplerYcbcrConversion ->
                 HS_vkCreateSamplerYcbcrConversion

foreign import ccall safe "dynamic"
               unwrapVkCreateSamplerYcbcrConversionSafe ::
               PFN_vkCreateSamplerYcbcrConversion ->
                 HS_vkCreateSamplerYcbcrConversion

instance VulkanProc "vkCreateSamplerYcbcrConversion" where
    type VkProcType "vkCreateSamplerYcbcrConversion" =
         HS_vkCreateSamplerYcbcrConversion
    vkProcSymbol = _VkCreateSamplerYcbcrConversion

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateSamplerYcbcrConversionUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateSamplerYcbcrConversionSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkDestroySamplerYcbcrConversion :: CString

pattern VkDestroySamplerYcbcrConversion <-
        (is_VkDestroySamplerYcbcrConversion -> True)
  where
    VkDestroySamplerYcbcrConversion = _VkDestroySamplerYcbcrConversion

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

-- |
-- > void vkDestroySamplerYcbcrConversion
-- >     ( VkDevice device
-- >     , VkSamplerYcbcrConversion ycbcrConversion
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySamplerYcbcrConversion vkDestroySamplerYcbcrConversion registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroySamplerYcbcrConversion <- vkGetDeviceProc @VkDestroySamplerYcbcrConversion vkDevice
--
-- or less efficient:
--
-- > myDestroySamplerYcbcrConversion <- vkGetProc @VkDestroySamplerYcbcrConversion
--
-- __Note:__ @vkDestroySamplerYcbcrConversionUnsafe@ and @vkDestroySamplerYcbcrConversionSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkDestroySamplerYcbcrConversion@ is an alias
--           of @vkDestroySamplerYcbcrConversionUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkDestroySamplerYcbcrConversionSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkDestroySamplerYcbcrConversion"
               vkDestroySamplerYcbcrConversionUnsafe ::
               VkDevice -- ^ device
                        ->
                 VkSamplerYcbcrConversion -- ^ ycbcrConversion
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

#else
vkDestroySamplerYcbcrConversionUnsafe ::
                                      VkDevice -- ^ device
                                               ->
                                        VkSamplerYcbcrConversion -- ^ ycbcrConversion
                                                                 ->
                                          Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()
vkDestroySamplerYcbcrConversionUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkDestroySamplerYcbcrConversion)

{-# NOINLINE vkDestroySamplerYcbcrConversionUnsafe #-}
#endif

-- |
-- > void vkDestroySamplerYcbcrConversion
-- >     ( VkDevice device
-- >     , VkSamplerYcbcrConversion ycbcrConversion
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySamplerYcbcrConversion vkDestroySamplerYcbcrConversion registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroySamplerYcbcrConversion <- vkGetDeviceProc @VkDestroySamplerYcbcrConversion vkDevice
--
-- or less efficient:
--
-- > myDestroySamplerYcbcrConversion <- vkGetProc @VkDestroySamplerYcbcrConversion
--
-- __Note:__ @vkDestroySamplerYcbcrConversionUnsafe@ and @vkDestroySamplerYcbcrConversionSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkDestroySamplerYcbcrConversion@ is an alias
--           of @vkDestroySamplerYcbcrConversionUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkDestroySamplerYcbcrConversionSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkDestroySamplerYcbcrConversion"
               vkDestroySamplerYcbcrConversionSafe ::
               VkDevice -- ^ device
                        ->
                 VkSamplerYcbcrConversion -- ^ ycbcrConversion
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

#else
vkDestroySamplerYcbcrConversionSafe ::
                                    VkDevice -- ^ device
                                             ->
                                      VkSamplerYcbcrConversion -- ^ ycbcrConversion
                                                               -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                            -> IO ()
vkDestroySamplerYcbcrConversionSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkDestroySamplerYcbcrConversion)

{-# NOINLINE vkDestroySamplerYcbcrConversionSafe #-}
#endif

-- |
-- > void vkDestroySamplerYcbcrConversion
-- >     ( VkDevice device
-- >     , VkSamplerYcbcrConversion ycbcrConversion
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySamplerYcbcrConversion vkDestroySamplerYcbcrConversion registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroySamplerYcbcrConversion <- vkGetDeviceProc @VkDestroySamplerYcbcrConversion vkDevice
--
-- or less efficient:
--
-- > myDestroySamplerYcbcrConversion <- vkGetProc @VkDestroySamplerYcbcrConversion
--
-- __Note:__ @vkDestroySamplerYcbcrConversionUnsafe@ and @vkDestroySamplerYcbcrConversionSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkDestroySamplerYcbcrConversion@ is an alias
--           of @vkDestroySamplerYcbcrConversionUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkDestroySamplerYcbcrConversionSafe@.
--
vkDestroySamplerYcbcrConversion ::
                                VkDevice -- ^ device
                                         ->
                                  VkSamplerYcbcrConversion -- ^ ycbcrConversion
                                                           -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                        -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkDestroySamplerYcbcrConversion
  = vkDestroySamplerYcbcrConversionUnsafe
#else
vkDestroySamplerYcbcrConversion
  = vkDestroySamplerYcbcrConversionSafe

#endif
{-# INLINE vkDestroySamplerYcbcrConversion #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkDestroySamplerYcbcrConversionUnsafe ::
               PFN_vkDestroySamplerYcbcrConversion ->
                 HS_vkDestroySamplerYcbcrConversion

foreign import ccall safe "dynamic"
               unwrapVkDestroySamplerYcbcrConversionSafe ::
               PFN_vkDestroySamplerYcbcrConversion ->
                 HS_vkDestroySamplerYcbcrConversion

instance VulkanProc "vkDestroySamplerYcbcrConversion" where
    type VkProcType "vkDestroySamplerYcbcrConversion" =
         HS_vkDestroySamplerYcbcrConversion
    vkProcSymbol = _VkDestroySamplerYcbcrConversion

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkDestroySamplerYcbcrConversionUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkDestroySamplerYcbcrConversionSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

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
pattern VK_IMAGE_ASPECT_PLANE_0_BIT :: VkImageAspectBitmask a

pattern VK_IMAGE_ASPECT_PLANE_0_BIT = VkImageAspectBitmask 16

-- | bitpos = @5@
pattern VK_IMAGE_ASPECT_PLANE_1_BIT :: VkImageAspectBitmask a

pattern VK_IMAGE_ASPECT_PLANE_1_BIT = VkImageAspectBitmask 32

-- | bitpos = @6@
pattern VK_IMAGE_ASPECT_PLANE_2_BIT :: VkImageAspectBitmask a

pattern VK_IMAGE_ASPECT_PLANE_2_BIT = VkImageAspectBitmask 64

-- | bitpos = @9@
pattern VK_IMAGE_CREATE_DISJOINT_BIT :: VkImageCreateBitmask a

pattern VK_IMAGE_CREATE_DISJOINT_BIT = VkImageCreateBitmask 512

-- | Format can have midpoint rather than cosited chroma samples
--
--   bitpos = @17@
pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT =
        VkFormatFeatureBitmask 131072

-- | Format can be used with linear filtering whilst color conversion is enabled
--
--   bitpos = @18@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
        :: VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
        = VkFormatFeatureBitmask 262144

-- | Format can have different chroma, min and mag filters
--
--   bitpos = @19@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
        :: VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
        = VkFormatFeatureBitmask 524288

-- | bitpos = @20@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
        :: VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
        = VkFormatFeatureBitmask 1048576

-- | bitpos = @21@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
        :: VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
        = VkFormatFeatureBitmask 2097152

-- | Format supports disjoint planes
--
--   bitpos = @22@
pattern VK_FORMAT_FEATURE_DISJOINT_BIT :: VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_DISJOINT_BIT =
        VkFormatFeatureBitmask 4194304

-- | Format can have cosited rather than midpoint chroma samples
--
--   bitpos = @23@
pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT =
        VkFormatFeatureBitmask 8388608

pattern VkCreateDescriptorUpdateTemplate :: CString

pattern VkCreateDescriptorUpdateTemplate <-
        (is_VkCreateDescriptorUpdateTemplate -> True)
  where
    VkCreateDescriptorUpdateTemplate
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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateDescriptorUpdateTemplate <- vkGetDeviceProc @VkCreateDescriptorUpdateTemplate vkDevice
--
-- or less efficient:
--
-- > myCreateDescriptorUpdateTemplate <- vkGetProc @VkCreateDescriptorUpdateTemplate
--
-- __Note:__ @vkCreateDescriptorUpdateTemplateUnsafe@ and @vkCreateDescriptorUpdateTemplateSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateDescriptorUpdateTemplate@ is an alias
--           of @vkCreateDescriptorUpdateTemplateUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateDescriptorUpdateTemplateSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkCreateDescriptorUpdateTemplate"
               vkCreateDescriptorUpdateTemplateUnsafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorUpdateTemplateCreateInfo -- ^ pCreateInfo
                                                          ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkDescriptorUpdateTemplate -- ^ pDescriptorUpdateTemplate
                                                    -> IO VkResult

#else
vkCreateDescriptorUpdateTemplateUnsafe ::
                                       VkDevice -- ^ device
                                                ->
                                         Ptr VkDescriptorUpdateTemplateCreateInfo -- ^ pCreateInfo
                                                                                  ->
                                           Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                     ->
                                             Ptr VkDescriptorUpdateTemplate -- ^ pDescriptorUpdateTemplate
                                                                            -> IO VkResult
vkCreateDescriptorUpdateTemplateUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkCreateDescriptorUpdateTemplate)

{-# NOINLINE vkCreateDescriptorUpdateTemplateUnsafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateDescriptorUpdateTemplate <- vkGetDeviceProc @VkCreateDescriptorUpdateTemplate vkDevice
--
-- or less efficient:
--
-- > myCreateDescriptorUpdateTemplate <- vkGetProc @VkCreateDescriptorUpdateTemplate
--
-- __Note:__ @vkCreateDescriptorUpdateTemplateUnsafe@ and @vkCreateDescriptorUpdateTemplateSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateDescriptorUpdateTemplate@ is an alias
--           of @vkCreateDescriptorUpdateTemplateUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateDescriptorUpdateTemplateSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
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
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkCreateDescriptorUpdateTemplate)

{-# NOINLINE vkCreateDescriptorUpdateTemplateSafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateDescriptorUpdateTemplate <- vkGetDeviceProc @VkCreateDescriptorUpdateTemplate vkDevice
--
-- or less efficient:
--
-- > myCreateDescriptorUpdateTemplate <- vkGetProc @VkCreateDescriptorUpdateTemplate
--
-- __Note:__ @vkCreateDescriptorUpdateTemplateUnsafe@ and @vkCreateDescriptorUpdateTemplateSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateDescriptorUpdateTemplate@ is an alias
--           of @vkCreateDescriptorUpdateTemplateUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateDescriptorUpdateTemplateSafe@.
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
#ifdef UNSAFE_FFI_DEFAULT
vkCreateDescriptorUpdateTemplate
  = vkCreateDescriptorUpdateTemplateUnsafe
#else
vkCreateDescriptorUpdateTemplate
  = vkCreateDescriptorUpdateTemplateSafe

#endif
{-# INLINE vkCreateDescriptorUpdateTemplate #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkCreateDescriptorUpdateTemplateUnsafe ::
               PFN_vkCreateDescriptorUpdateTemplate ->
                 HS_vkCreateDescriptorUpdateTemplate

foreign import ccall safe "dynamic"
               unwrapVkCreateDescriptorUpdateTemplateSafe ::
               PFN_vkCreateDescriptorUpdateTemplate ->
                 HS_vkCreateDescriptorUpdateTemplate

instance VulkanProc "vkCreateDescriptorUpdateTemplate" where
    type VkProcType "vkCreateDescriptorUpdateTemplate" =
         HS_vkCreateDescriptorUpdateTemplate
    vkProcSymbol = _VkCreateDescriptorUpdateTemplate

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCreateDescriptorUpdateTemplateUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateDescriptorUpdateTemplateSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkDestroyDescriptorUpdateTemplate :: CString

pattern VkDestroyDescriptorUpdateTemplate <-
        (is_VkDestroyDescriptorUpdateTemplate -> True)
  where
    VkDestroyDescriptorUpdateTemplate
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

-- |
-- > void vkDestroyDescriptorUpdateTemplate
-- >     ( VkDevice device
-- >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDescriptorUpdateTemplate vkDestroyDescriptorUpdateTemplate registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyDescriptorUpdateTemplate <- vkGetDeviceProc @VkDestroyDescriptorUpdateTemplate vkDevice
--
-- or less efficient:
--
-- > myDestroyDescriptorUpdateTemplate <- vkGetProc @VkDestroyDescriptorUpdateTemplate
--
-- __Note:__ @vkDestroyDescriptorUpdateTemplateUnsafe@ and @vkDestroyDescriptorUpdateTemplateSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkDestroyDescriptorUpdateTemplate@ is an alias
--           of @vkDestroyDescriptorUpdateTemplateUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkDestroyDescriptorUpdateTemplateSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkDestroyDescriptorUpdateTemplate"
               vkDestroyDescriptorUpdateTemplateUnsafe ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

#else
vkDestroyDescriptorUpdateTemplateUnsafe ::
                                        VkDevice -- ^ device
                                                 ->
                                          VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                                     ->
                                            Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()
vkDestroyDescriptorUpdateTemplateUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkDestroyDescriptorUpdateTemplate)

{-# NOINLINE vkDestroyDescriptorUpdateTemplateUnsafe #-}
#endif

-- |
-- > void vkDestroyDescriptorUpdateTemplate
-- >     ( VkDevice device
-- >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDescriptorUpdateTemplate vkDestroyDescriptorUpdateTemplate registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyDescriptorUpdateTemplate <- vkGetDeviceProc @VkDestroyDescriptorUpdateTemplate vkDevice
--
-- or less efficient:
--
-- > myDestroyDescriptorUpdateTemplate <- vkGetProc @VkDestroyDescriptorUpdateTemplate
--
-- __Note:__ @vkDestroyDescriptorUpdateTemplateUnsafe@ and @vkDestroyDescriptorUpdateTemplateSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkDestroyDescriptorUpdateTemplate@ is an alias
--           of @vkDestroyDescriptorUpdateTemplateUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkDestroyDescriptorUpdateTemplateSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkDestroyDescriptorUpdateTemplate"
               vkDestroyDescriptorUpdateTemplateSafe ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

#else
vkDestroyDescriptorUpdateTemplateSafe ::
                                      VkDevice -- ^ device
                                               ->
                                        VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                                   ->
                                          Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()
vkDestroyDescriptorUpdateTemplateSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkDestroyDescriptorUpdateTemplate)

{-# NOINLINE vkDestroyDescriptorUpdateTemplateSafe #-}
#endif

-- |
-- > void vkDestroyDescriptorUpdateTemplate
-- >     ( VkDevice device
-- >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDescriptorUpdateTemplate vkDestroyDescriptorUpdateTemplate registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroyDescriptorUpdateTemplate <- vkGetDeviceProc @VkDestroyDescriptorUpdateTemplate vkDevice
--
-- or less efficient:
--
-- > myDestroyDescriptorUpdateTemplate <- vkGetProc @VkDestroyDescriptorUpdateTemplate
--
-- __Note:__ @vkDestroyDescriptorUpdateTemplateUnsafe@ and @vkDestroyDescriptorUpdateTemplateSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkDestroyDescriptorUpdateTemplate@ is an alias
--           of @vkDestroyDescriptorUpdateTemplateUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkDestroyDescriptorUpdateTemplateSafe@.
--
vkDestroyDescriptorUpdateTemplate ::
                                  VkDevice -- ^ device
                                           ->
                                    VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                               -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                            -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkDestroyDescriptorUpdateTemplate
  = vkDestroyDescriptorUpdateTemplateUnsafe
#else
vkDestroyDescriptorUpdateTemplate
  = vkDestroyDescriptorUpdateTemplateSafe

#endif
{-# INLINE vkDestroyDescriptorUpdateTemplate #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkDestroyDescriptorUpdateTemplateUnsafe ::
               PFN_vkDestroyDescriptorUpdateTemplate ->
                 HS_vkDestroyDescriptorUpdateTemplate

foreign import ccall safe "dynamic"
               unwrapVkDestroyDescriptorUpdateTemplateSafe ::
               PFN_vkDestroyDescriptorUpdateTemplate ->
                 HS_vkDestroyDescriptorUpdateTemplate

instance VulkanProc "vkDestroyDescriptorUpdateTemplate" where
    type VkProcType "vkDestroyDescriptorUpdateTemplate" =
         HS_vkDestroyDescriptorUpdateTemplate
    vkProcSymbol = _VkDestroyDescriptorUpdateTemplate

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkDestroyDescriptorUpdateTemplateUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkDestroyDescriptorUpdateTemplateSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkUpdateDescriptorSetWithTemplate :: CString

pattern VkUpdateDescriptorSetWithTemplate <-
        (is_VkUpdateDescriptorSetWithTemplate -> True)
  where
    VkUpdateDescriptorSetWithTemplate
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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myUpdateDescriptorSetWithTemplate <- vkGetDeviceProc @VkUpdateDescriptorSetWithTemplate vkDevice
--
-- or less efficient:
--
-- > myUpdateDescriptorSetWithTemplate <- vkGetProc @VkUpdateDescriptorSetWithTemplate
--
-- __Note:__ @vkUpdateDescriptorSetWithTemplateUnsafe@ and @vkUpdateDescriptorSetWithTemplateSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkUpdateDescriptorSetWithTemplate@ is an alias
--           of @vkUpdateDescriptorSetWithTemplateUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkUpdateDescriptorSetWithTemplateSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkUpdateDescriptorSetWithTemplate"
               vkUpdateDescriptorSetWithTemplateUnsafe ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorSet -- ^ descriptorSet
                                 -> VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                               -> Ptr Void -- ^ pData
                                                                           -> IO ()

#else
vkUpdateDescriptorSetWithTemplateUnsafe ::
                                        VkDevice -- ^ device
                                                 ->
                                          VkDescriptorSet -- ^ descriptorSet
                                                          ->
                                            VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                                       -> Ptr Void -- ^ pData
                                                                                   -> IO ()
vkUpdateDescriptorSetWithTemplateUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkUpdateDescriptorSetWithTemplate)

{-# NOINLINE vkUpdateDescriptorSetWithTemplateUnsafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myUpdateDescriptorSetWithTemplate <- vkGetDeviceProc @VkUpdateDescriptorSetWithTemplate vkDevice
--
-- or less efficient:
--
-- > myUpdateDescriptorSetWithTemplate <- vkGetProc @VkUpdateDescriptorSetWithTemplate
--
-- __Note:__ @vkUpdateDescriptorSetWithTemplateUnsafe@ and @vkUpdateDescriptorSetWithTemplateSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkUpdateDescriptorSetWithTemplate@ is an alias
--           of @vkUpdateDescriptorSetWithTemplateUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkUpdateDescriptorSetWithTemplateSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkUpdateDescriptorSetWithTemplate"
               vkUpdateDescriptorSetWithTemplateSafe ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorSet -- ^ descriptorSet
                                 -> VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                               -> Ptr Void -- ^ pData
                                                                           -> IO ()

#else
vkUpdateDescriptorSetWithTemplateSafe ::
                                      VkDevice -- ^ device
                                               ->
                                        VkDescriptorSet -- ^ descriptorSet
                                                        ->
                                          VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                                     -> Ptr Void -- ^ pData
                                                                                 -> IO ()
vkUpdateDescriptorSetWithTemplateSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkUpdateDescriptorSetWithTemplate)

{-# NOINLINE vkUpdateDescriptorSetWithTemplateSafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myUpdateDescriptorSetWithTemplate <- vkGetDeviceProc @VkUpdateDescriptorSetWithTemplate vkDevice
--
-- or less efficient:
--
-- > myUpdateDescriptorSetWithTemplate <- vkGetProc @VkUpdateDescriptorSetWithTemplate
--
-- __Note:__ @vkUpdateDescriptorSetWithTemplateUnsafe@ and @vkUpdateDescriptorSetWithTemplateSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkUpdateDescriptorSetWithTemplate@ is an alias
--           of @vkUpdateDescriptorSetWithTemplateUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkUpdateDescriptorSetWithTemplateSafe@.
--
vkUpdateDescriptorSetWithTemplate ::
                                  VkDevice -- ^ device
                                           ->
                                    VkDescriptorSet -- ^ descriptorSet
                                                    ->
                                      VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                                 -> Ptr Void -- ^ pData
                                                                             -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkUpdateDescriptorSetWithTemplate
  = vkUpdateDescriptorSetWithTemplateUnsafe
#else
vkUpdateDescriptorSetWithTemplate
  = vkUpdateDescriptorSetWithTemplateSafe

#endif
{-# INLINE vkUpdateDescriptorSetWithTemplate #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkUpdateDescriptorSetWithTemplateUnsafe ::
               PFN_vkUpdateDescriptorSetWithTemplate ->
                 HS_vkUpdateDescriptorSetWithTemplate

foreign import ccall safe "dynamic"
               unwrapVkUpdateDescriptorSetWithTemplateSafe ::
               PFN_vkUpdateDescriptorSetWithTemplate ->
                 HS_vkUpdateDescriptorSetWithTemplate

instance VulkanProc "vkUpdateDescriptorSetWithTemplate" where
    type VkProcType "vkUpdateDescriptorSetWithTemplate" =
         HS_vkUpdateDescriptorSetWithTemplate
    vkProcSymbol = _VkUpdateDescriptorSetWithTemplate

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkUpdateDescriptorSetWithTemplateUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkUpdateDescriptorSetWithTemplateSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

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
  where
    VkGetPhysicalDeviceExternalBufferProperties
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

-- |
-- > void vkGetPhysicalDeviceExternalBufferProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalBufferInfo* pExternalBufferInfo
-- >     , VkExternalBufferProperties* pExternalBufferProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalBufferProperties vkGetPhysicalDeviceExternalBufferProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceExternalBufferProperties <- vkGetInstanceProc @VkGetPhysicalDeviceExternalBufferProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceExternalBufferProperties <- vkGetProc @VkGetPhysicalDeviceExternalBufferProperties
--
-- __Note:__ @vkGetPhysicalDeviceExternalBufferPropertiesUnsafe@ and @vkGetPhysicalDeviceExternalBufferPropertiesSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceExternalBufferProperties@ is an alias
--           of @vkGetPhysicalDeviceExternalBufferPropertiesUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceExternalBufferPropertiesSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalBufferProperties"
               vkGetPhysicalDeviceExternalBufferPropertiesUnsafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalBufferInfo -- ^ pExternalBufferInfo
                                                        ->
                   Ptr VkExternalBufferProperties -- ^ pExternalBufferProperties
                                                  -> IO ()

#else
vkGetPhysicalDeviceExternalBufferPropertiesUnsafe ::
                                                  VkPhysicalDevice -- ^ physicalDevice
                                                                   ->
                                                    Ptr VkPhysicalDeviceExternalBufferInfo -- ^ pExternalBufferInfo
                                                                                           ->
                                                      Ptr VkExternalBufferProperties -- ^ pExternalBufferProperties
                                                                                     -> IO ()
vkGetPhysicalDeviceExternalBufferPropertiesUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetPhysicalDeviceExternalBufferProperties)

{-# NOINLINE vkGetPhysicalDeviceExternalBufferPropertiesUnsafe #-}
#endif

-- |
-- > void vkGetPhysicalDeviceExternalBufferProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalBufferInfo* pExternalBufferInfo
-- >     , VkExternalBufferProperties* pExternalBufferProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalBufferProperties vkGetPhysicalDeviceExternalBufferProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceExternalBufferProperties <- vkGetInstanceProc @VkGetPhysicalDeviceExternalBufferProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceExternalBufferProperties <- vkGetProc @VkGetPhysicalDeviceExternalBufferProperties
--
-- __Note:__ @vkGetPhysicalDeviceExternalBufferPropertiesUnsafe@ and @vkGetPhysicalDeviceExternalBufferPropertiesSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceExternalBufferProperties@ is an alias
--           of @vkGetPhysicalDeviceExternalBufferPropertiesUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceExternalBufferPropertiesSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
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
vkGetPhysicalDeviceExternalBufferPropertiesSafe ::
                                                VkPhysicalDevice -- ^ physicalDevice
                                                                 ->
                                                  Ptr VkPhysicalDeviceExternalBufferInfo -- ^ pExternalBufferInfo
                                                                                         ->
                                                    Ptr VkExternalBufferProperties -- ^ pExternalBufferProperties
                                                                                   -> IO ()
vkGetPhysicalDeviceExternalBufferPropertiesSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceExternalBufferProperties)

{-# NOINLINE vkGetPhysicalDeviceExternalBufferPropertiesSafe #-}
#endif

-- |
-- > void vkGetPhysicalDeviceExternalBufferProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalBufferInfo* pExternalBufferInfo
-- >     , VkExternalBufferProperties* pExternalBufferProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalBufferProperties vkGetPhysicalDeviceExternalBufferProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceExternalBufferProperties <- vkGetInstanceProc @VkGetPhysicalDeviceExternalBufferProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceExternalBufferProperties <- vkGetProc @VkGetPhysicalDeviceExternalBufferProperties
--
-- __Note:__ @vkGetPhysicalDeviceExternalBufferPropertiesUnsafe@ and @vkGetPhysicalDeviceExternalBufferPropertiesSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceExternalBufferProperties@ is an alias
--           of @vkGetPhysicalDeviceExternalBufferPropertiesUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceExternalBufferPropertiesSafe@.
--
vkGetPhysicalDeviceExternalBufferProperties ::
                                            VkPhysicalDevice -- ^ physicalDevice
                                                             ->
                                              Ptr VkPhysicalDeviceExternalBufferInfo -- ^ pExternalBufferInfo
                                                                                     ->
                                                Ptr VkExternalBufferProperties -- ^ pExternalBufferProperties
                                                                               -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkGetPhysicalDeviceExternalBufferProperties
  = vkGetPhysicalDeviceExternalBufferPropertiesUnsafe
#else
vkGetPhysicalDeviceExternalBufferProperties
  = vkGetPhysicalDeviceExternalBufferPropertiesSafe

#endif
{-# INLINE vkGetPhysicalDeviceExternalBufferProperties #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceExternalBufferPropertiesUnsafe ::
               PFN_vkGetPhysicalDeviceExternalBufferProperties ->
                 HS_vkGetPhysicalDeviceExternalBufferProperties

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceExternalBufferPropertiesSafe ::
               PFN_vkGetPhysicalDeviceExternalBufferProperties ->
                 HS_vkGetPhysicalDeviceExternalBufferProperties

instance VulkanProc "vkGetPhysicalDeviceExternalBufferProperties"
         where
    type VkProcType "vkGetPhysicalDeviceExternalBufferProperties" =
         HS_vkGetPhysicalDeviceExternalBufferProperties
    vkProcSymbol = _VkGetPhysicalDeviceExternalBufferProperties

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceExternalBufferPropertiesUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceExternalBufferPropertiesSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

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
  where
    VkGetPhysicalDeviceExternalFenceProperties
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

-- |
-- > void vkGetPhysicalDeviceExternalFenceProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalFenceInfo* pExternalFenceInfo
-- >     , VkExternalFenceProperties* pExternalFenceProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalFenceProperties vkGetPhysicalDeviceExternalFenceProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceExternalFenceProperties <- vkGetInstanceProc @VkGetPhysicalDeviceExternalFenceProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceExternalFenceProperties <- vkGetProc @VkGetPhysicalDeviceExternalFenceProperties
--
-- __Note:__ @vkGetPhysicalDeviceExternalFencePropertiesUnsafe@ and @vkGetPhysicalDeviceExternalFencePropertiesSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceExternalFenceProperties@ is an alias
--           of @vkGetPhysicalDeviceExternalFencePropertiesUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceExternalFencePropertiesSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalFenceProperties"
               vkGetPhysicalDeviceExternalFencePropertiesUnsafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalFenceInfo -- ^ pExternalFenceInfo
                                                       ->
                   Ptr VkExternalFenceProperties -- ^ pExternalFenceProperties
                                                 -> IO ()

#else
vkGetPhysicalDeviceExternalFencePropertiesUnsafe ::
                                                 VkPhysicalDevice -- ^ physicalDevice
                                                                  ->
                                                   Ptr VkPhysicalDeviceExternalFenceInfo -- ^ pExternalFenceInfo
                                                                                         ->
                                                     Ptr VkExternalFenceProperties -- ^ pExternalFenceProperties
                                                                                   -> IO ()
vkGetPhysicalDeviceExternalFencePropertiesUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetPhysicalDeviceExternalFenceProperties)

{-# NOINLINE vkGetPhysicalDeviceExternalFencePropertiesUnsafe #-}
#endif

-- |
-- > void vkGetPhysicalDeviceExternalFenceProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalFenceInfo* pExternalFenceInfo
-- >     , VkExternalFenceProperties* pExternalFenceProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalFenceProperties vkGetPhysicalDeviceExternalFenceProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceExternalFenceProperties <- vkGetInstanceProc @VkGetPhysicalDeviceExternalFenceProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceExternalFenceProperties <- vkGetProc @VkGetPhysicalDeviceExternalFenceProperties
--
-- __Note:__ @vkGetPhysicalDeviceExternalFencePropertiesUnsafe@ and @vkGetPhysicalDeviceExternalFencePropertiesSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceExternalFenceProperties@ is an alias
--           of @vkGetPhysicalDeviceExternalFencePropertiesUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceExternalFencePropertiesSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
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
vkGetPhysicalDeviceExternalFencePropertiesSafe ::
                                               VkPhysicalDevice -- ^ physicalDevice
                                                                ->
                                                 Ptr VkPhysicalDeviceExternalFenceInfo -- ^ pExternalFenceInfo
                                                                                       ->
                                                   Ptr VkExternalFenceProperties -- ^ pExternalFenceProperties
                                                                                 -> IO ()
vkGetPhysicalDeviceExternalFencePropertiesSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceExternalFenceProperties)

{-# NOINLINE vkGetPhysicalDeviceExternalFencePropertiesSafe #-}
#endif

-- |
-- > void vkGetPhysicalDeviceExternalFenceProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalFenceInfo* pExternalFenceInfo
-- >     , VkExternalFenceProperties* pExternalFenceProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalFenceProperties vkGetPhysicalDeviceExternalFenceProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceExternalFenceProperties <- vkGetInstanceProc @VkGetPhysicalDeviceExternalFenceProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceExternalFenceProperties <- vkGetProc @VkGetPhysicalDeviceExternalFenceProperties
--
-- __Note:__ @vkGetPhysicalDeviceExternalFencePropertiesUnsafe@ and @vkGetPhysicalDeviceExternalFencePropertiesSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceExternalFenceProperties@ is an alias
--           of @vkGetPhysicalDeviceExternalFencePropertiesUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceExternalFencePropertiesSafe@.
--
vkGetPhysicalDeviceExternalFenceProperties ::
                                           VkPhysicalDevice -- ^ physicalDevice
                                                            ->
                                             Ptr VkPhysicalDeviceExternalFenceInfo -- ^ pExternalFenceInfo
                                                                                   ->
                                               Ptr VkExternalFenceProperties -- ^ pExternalFenceProperties
                                                                             -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkGetPhysicalDeviceExternalFenceProperties
  = vkGetPhysicalDeviceExternalFencePropertiesUnsafe
#else
vkGetPhysicalDeviceExternalFenceProperties
  = vkGetPhysicalDeviceExternalFencePropertiesSafe

#endif
{-# INLINE vkGetPhysicalDeviceExternalFenceProperties #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceExternalFencePropertiesUnsafe ::
               PFN_vkGetPhysicalDeviceExternalFenceProperties ->
                 HS_vkGetPhysicalDeviceExternalFenceProperties

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceExternalFencePropertiesSafe ::
               PFN_vkGetPhysicalDeviceExternalFenceProperties ->
                 HS_vkGetPhysicalDeviceExternalFenceProperties

instance VulkanProc "vkGetPhysicalDeviceExternalFenceProperties"
         where
    type VkProcType "vkGetPhysicalDeviceExternalFenceProperties" =
         HS_vkGetPhysicalDeviceExternalFenceProperties
    vkProcSymbol = _VkGetPhysicalDeviceExternalFenceProperties

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceExternalFencePropertiesUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceExternalFencePropertiesSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

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
  where
    VkGetPhysicalDeviceExternalSemaphoreProperties
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

-- |
-- > void vkGetPhysicalDeviceExternalSemaphoreProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalSemaphoreInfo* pExternalSemaphoreInfo
-- >     , VkExternalSemaphoreProperties* pExternalSemaphoreProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalSemaphoreProperties vkGetPhysicalDeviceExternalSemaphoreProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceExternalSemaphoreProperties <- vkGetInstanceProc @VkGetPhysicalDeviceExternalSemaphoreProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceExternalSemaphoreProperties <- vkGetProc @VkGetPhysicalDeviceExternalSemaphoreProperties
--
-- __Note:__ @vkGetPhysicalDeviceExternalSemaphorePropertiesUnsafe@ and @vkGetPhysicalDeviceExternalSemaphorePropertiesSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceExternalSemaphoreProperties@ is an alias
--           of @vkGetPhysicalDeviceExternalSemaphorePropertiesUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceExternalSemaphorePropertiesSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalSemaphoreProperties"
               vkGetPhysicalDeviceExternalSemaphorePropertiesUnsafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalSemaphoreInfo -- ^ pExternalSemaphoreInfo
                                                           ->
                   Ptr VkExternalSemaphoreProperties -- ^ pExternalSemaphoreProperties
                                                     -> IO ()

#else
vkGetPhysicalDeviceExternalSemaphorePropertiesUnsafe ::
                                                     VkPhysicalDevice -- ^ physicalDevice
                                                                      ->
                                                       Ptr VkPhysicalDeviceExternalSemaphoreInfo -- ^ pExternalSemaphoreInfo
                                                                                                 ->
                                                         Ptr VkExternalSemaphoreProperties -- ^ pExternalSemaphoreProperties
                                                                                           -> IO ()
vkGetPhysicalDeviceExternalSemaphorePropertiesUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetPhysicalDeviceExternalSemaphoreProperties)

{-# NOINLINE vkGetPhysicalDeviceExternalSemaphorePropertiesUnsafe
             #-}
#endif

-- |
-- > void vkGetPhysicalDeviceExternalSemaphoreProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalSemaphoreInfo* pExternalSemaphoreInfo
-- >     , VkExternalSemaphoreProperties* pExternalSemaphoreProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalSemaphoreProperties vkGetPhysicalDeviceExternalSemaphoreProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceExternalSemaphoreProperties <- vkGetInstanceProc @VkGetPhysicalDeviceExternalSemaphoreProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceExternalSemaphoreProperties <- vkGetProc @VkGetPhysicalDeviceExternalSemaphoreProperties
--
-- __Note:__ @vkGetPhysicalDeviceExternalSemaphorePropertiesUnsafe@ and @vkGetPhysicalDeviceExternalSemaphorePropertiesSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceExternalSemaphoreProperties@ is an alias
--           of @vkGetPhysicalDeviceExternalSemaphorePropertiesUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceExternalSemaphorePropertiesSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
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
vkGetPhysicalDeviceExternalSemaphorePropertiesSafe ::
                                                   VkPhysicalDevice -- ^ physicalDevice
                                                                    ->
                                                     Ptr VkPhysicalDeviceExternalSemaphoreInfo -- ^ pExternalSemaphoreInfo
                                                                                               ->
                                                       Ptr VkExternalSemaphoreProperties -- ^ pExternalSemaphoreProperties
                                                                                         -> IO ()
vkGetPhysicalDeviceExternalSemaphorePropertiesSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceExternalSemaphoreProperties)

{-# NOINLINE vkGetPhysicalDeviceExternalSemaphorePropertiesSafe #-}
#endif

-- |
-- > void vkGetPhysicalDeviceExternalSemaphoreProperties
-- >     ( VkPhysicalDevice physicalDevice
-- >     , const VkPhysicalDeviceExternalSemaphoreInfo* pExternalSemaphoreInfo
-- >     , VkExternalSemaphoreProperties* pExternalSemaphoreProperties
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalSemaphoreProperties vkGetPhysicalDeviceExternalSemaphoreProperties registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceExternalSemaphoreProperties <- vkGetInstanceProc @VkGetPhysicalDeviceExternalSemaphoreProperties vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceExternalSemaphoreProperties <- vkGetProc @VkGetPhysicalDeviceExternalSemaphoreProperties
--
-- __Note:__ @vkGetPhysicalDeviceExternalSemaphorePropertiesUnsafe@ and @vkGetPhysicalDeviceExternalSemaphorePropertiesSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceExternalSemaphoreProperties@ is an alias
--           of @vkGetPhysicalDeviceExternalSemaphorePropertiesUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceExternalSemaphorePropertiesSafe@.
--
vkGetPhysicalDeviceExternalSemaphoreProperties ::
                                               VkPhysicalDevice -- ^ physicalDevice
                                                                ->
                                                 Ptr VkPhysicalDeviceExternalSemaphoreInfo -- ^ pExternalSemaphoreInfo
                                                                                           ->
                                                   Ptr VkExternalSemaphoreProperties -- ^ pExternalSemaphoreProperties
                                                                                     -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkGetPhysicalDeviceExternalSemaphoreProperties
  = vkGetPhysicalDeviceExternalSemaphorePropertiesUnsafe
#else
vkGetPhysicalDeviceExternalSemaphoreProperties
  = vkGetPhysicalDeviceExternalSemaphorePropertiesSafe

#endif
{-# INLINE vkGetPhysicalDeviceExternalSemaphoreProperties #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceExternalSemaphorePropertiesUnsafe ::
               PFN_vkGetPhysicalDeviceExternalSemaphoreProperties ->
                 HS_vkGetPhysicalDeviceExternalSemaphoreProperties

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceExternalSemaphorePropertiesSafe ::
               PFN_vkGetPhysicalDeviceExternalSemaphoreProperties ->
                 HS_vkGetPhysicalDeviceExternalSemaphoreProperties

instance VulkanProc
           "vkGetPhysicalDeviceExternalSemaphoreProperties"
         where
    type VkProcType "vkGetPhysicalDeviceExternalSemaphoreProperties" =
         HS_vkGetPhysicalDeviceExternalSemaphoreProperties
    vkProcSymbol = _VkGetPhysicalDeviceExternalSemaphoreProperties

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceExternalSemaphorePropertiesUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceExternalSemaphorePropertiesSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

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
  where
    VkGetDescriptorSetLayoutSupport = _VkGetDescriptorSetLayoutSupport

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

-- |
-- > void vkGetDescriptorSetLayoutSupport
-- >     ( VkDevice device
-- >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
-- >     , VkDescriptorSetLayoutSupport* pSupport
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDescriptorSetLayoutSupport vkGetDescriptorSetLayoutSupport registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDescriptorSetLayoutSupport <- vkGetDeviceProc @VkGetDescriptorSetLayoutSupport vkDevice
--
-- or less efficient:
--
-- > myGetDescriptorSetLayoutSupport <- vkGetProc @VkGetDescriptorSetLayoutSupport
--
-- __Note:__ @vkGetDescriptorSetLayoutSupportUnsafe@ and @vkGetDescriptorSetLayoutSupportSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDescriptorSetLayoutSupport@ is an alias
--           of @vkGetDescriptorSetLayoutSupportUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDescriptorSetLayoutSupportSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkGetDescriptorSetLayoutSupport"
               vkGetDescriptorSetLayoutSupportUnsafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                     ->
                   Ptr VkDescriptorSetLayoutSupport -- ^ pSupport
                                                    -> IO ()

#else
vkGetDescriptorSetLayoutSupportUnsafe ::
                                      VkDevice -- ^ device
                                               ->
                                        Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                                            ->
                                          Ptr VkDescriptorSetLayoutSupport -- ^ pSupport
                                                                           -> IO ()
vkGetDescriptorSetLayoutSupportUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetDescriptorSetLayoutSupport)

{-# NOINLINE vkGetDescriptorSetLayoutSupportUnsafe #-}
#endif

-- |
-- > void vkGetDescriptorSetLayoutSupport
-- >     ( VkDevice device
-- >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
-- >     , VkDescriptorSetLayoutSupport* pSupport
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDescriptorSetLayoutSupport vkGetDescriptorSetLayoutSupport registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDescriptorSetLayoutSupport <- vkGetDeviceProc @VkGetDescriptorSetLayoutSupport vkDevice
--
-- or less efficient:
--
-- > myGetDescriptorSetLayoutSupport <- vkGetProc @VkGetDescriptorSetLayoutSupport
--
-- __Note:__ @vkGetDescriptorSetLayoutSupportUnsafe@ and @vkGetDescriptorSetLayoutSupportSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDescriptorSetLayoutSupport@ is an alias
--           of @vkGetDescriptorSetLayoutSupportUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDescriptorSetLayoutSupportSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkGetDescriptorSetLayoutSupport"
               vkGetDescriptorSetLayoutSupportSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                     ->
                   Ptr VkDescriptorSetLayoutSupport -- ^ pSupport
                                                    -> IO ()

#else
vkGetDescriptorSetLayoutSupportSafe ::
                                    VkDevice -- ^ device
                                             ->
                                      Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                                          ->
                                        Ptr VkDescriptorSetLayoutSupport -- ^ pSupport
                                                                         -> IO ()
vkGetDescriptorSetLayoutSupportSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetDescriptorSetLayoutSupport)

{-# NOINLINE vkGetDescriptorSetLayoutSupportSafe #-}
#endif

-- |
-- > void vkGetDescriptorSetLayoutSupport
-- >     ( VkDevice device
-- >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
-- >     , VkDescriptorSetLayoutSupport* pSupport
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDescriptorSetLayoutSupport vkGetDescriptorSetLayoutSupport registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDescriptorSetLayoutSupport <- vkGetDeviceProc @VkGetDescriptorSetLayoutSupport vkDevice
--
-- or less efficient:
--
-- > myGetDescriptorSetLayoutSupport <- vkGetProc @VkGetDescriptorSetLayoutSupport
--
-- __Note:__ @vkGetDescriptorSetLayoutSupportUnsafe@ and @vkGetDescriptorSetLayoutSupportSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDescriptorSetLayoutSupport@ is an alias
--           of @vkGetDescriptorSetLayoutSupportUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDescriptorSetLayoutSupportSafe@.
--
vkGetDescriptorSetLayoutSupport ::
                                VkDevice -- ^ device
                                         ->
                                  Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                                      ->
                                    Ptr VkDescriptorSetLayoutSupport -- ^ pSupport
                                                                     -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkGetDescriptorSetLayoutSupport
  = vkGetDescriptorSetLayoutSupportUnsafe
#else
vkGetDescriptorSetLayoutSupport
  = vkGetDescriptorSetLayoutSupportSafe

#endif
{-# INLINE vkGetDescriptorSetLayoutSupport #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkGetDescriptorSetLayoutSupportUnsafe ::
               PFN_vkGetDescriptorSetLayoutSupport ->
                 HS_vkGetDescriptorSetLayoutSupport

foreign import ccall safe "dynamic"
               unwrapVkGetDescriptorSetLayoutSupportSafe ::
               PFN_vkGetDescriptorSetLayoutSupport ->
                 HS_vkGetDescriptorSetLayoutSupport

instance VulkanProc "vkGetDescriptorSetLayoutSupport" where
    type VkProcType "vkGetDescriptorSetLayoutSupport" =
         HS_vkGetDescriptorSetLayoutSupport
    vkProcSymbol = _VkGetDescriptorSetLayoutSupport

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetDescriptorSetLayoutSupportUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetDescriptorSetLayoutSupportSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

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
