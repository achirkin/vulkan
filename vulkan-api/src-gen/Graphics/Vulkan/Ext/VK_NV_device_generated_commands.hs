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
module Graphics.Vulkan.Ext.VK_NV_device_generated_commands
       (-- * Vulkan extension: @VK_NV_device_generated_commands@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Christoph Kubisch @pixeljetstream@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @278@
        module Graphics.Vulkan.Marshal,
        VkBindIndexBufferIndirectCommandNV,
        VkBindShaderGroupIndirectCommandNV,
        VkBindVertexBufferIndirectCommandNV, VkBlendFactor(..),
        VkBlendOp(..), VkBlendOverlapEXT(..), AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR,
        VkColorComponentBitmask(..), VkColorSpaceKHR(..),
        VkColorComponentFlagBits(), VkColorComponentFlags(),
        VkCompareOp(..), VkCullModeBitmask(..), VkCullModeFlagBits(),
        VkCullModeFlags(), VkAndroidSurfaceCreateFlagsKHR(..),
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
        VkDeviceQueueCreateInfo, VkDynamicState(..), VkExtent2D,
        VkFormat(..), VkFormatFeatureBitmask(..),
        VkFormatFeatureFlagBits(), VkFormatFeatureFlags(), VkFrontFace(..),
        VkGeneratedCommandsInfoNV,
        VkGeneratedCommandsMemoryRequirementsInfoNV,
        VkGraphicsPipelineCreateInfo,
        VkGraphicsPipelineShaderGroupsCreateInfoNV,
        VkGraphicsShaderGroupCreateInfoNV, VkIndexType(..),
        VkIndirectCommandsLayoutCreateInfoNV,
        VkIndirectCommandsLayoutTokenNV,
        VkIndirectCommandsLayoutUsageBitmaskNV(..),
        VkIndirectCommandsTokenTypeNV(..), VkIndirectStateBitmaskNV(..),
        VkIndirectCommandsLayoutUsageFlagBitsNV(),
        VkIndirectCommandsLayoutUsageFlagsNV(),
        VkIndirectStateFlagBitsNV(), VkIndirectStateFlagsNV(),
        VkIndirectCommandsStreamNV, VkLogicOp(..), VkOffset2D,
        VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV,
        VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV,
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        VkPhysicalDeviceLimits, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2, VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceType(..), VkPipelineBindPoint(..),
        VkPipelineCacheHeaderVersion(..), VkPipelineCreateBitmask(..),
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
        VkPipelineStageFlags(), VkPipelineColorBlendAttachmentState,
        VkPipelineColorBlendStateCreateInfo,
        VkPipelineDepthStencilStateCreateInfo,
        VkPipelineDynamicStateCreateInfo,
        VkPipelineInputAssemblyStateCreateInfo,
        VkPipelineMultisampleStateCreateInfo,
        VkPipelineRasterizationStateCreateInfo,
        VkPipelineShaderStageCreateInfo,
        VkPipelineTessellationStateCreateInfo,
        VkPipelineVertexInputStateCreateInfo,
        VkPipelineViewportStateCreateInfo, VkPolygonMode(..),
        VkPrimitiveTopology(..), VkRect2D, VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(),
        VkSetStateFlagsIndirectCommandNV,
        VkShaderFloatControlsIndependence(..), VkShaderInfoTypeAMD(..),
        VkShaderStageBitmask(..), VkShaderCorePropertiesBitmaskAMD(..),
        VkShaderCorePropertiesFlagBitsAMD(),
        VkShaderCorePropertiesFlagsAMD(),
        VkShaderFloatControlsIndependenceKHR(..),
        VkShaderModuleCreateBitmask(..), VkShaderModuleCreateFlagBits(),
        VkShaderModuleCreateFlags(), VkShaderStageFlagBits(),
        VkShaderStageFlags(), VkSpecializationInfo,
        VkSpecializationMapEntry, VkStencilFaceBitmask(..),
        VkStencilOp(..), VkStencilFaceFlagBits(), VkStencilFaceFlags(),
        VkStencilOpState, VkStructureType(..),
        VkVertexInputAttributeDescription, VkVertexInputBindingDescription,
        VkVertexInputRate(..), VkViewport,
        -- > #include "vk_platform.h"
        VkGetGeneratedCommandsMemoryRequirementsNV,
        pattern VkGetGeneratedCommandsMemoryRequirementsNV,
        HS_vkGetGeneratedCommandsMemoryRequirementsNV,
        PFN_vkGetGeneratedCommandsMemoryRequirementsNV,
        VkCmdPreprocessGeneratedCommandsNV,
        pattern VkCmdPreprocessGeneratedCommandsNV,
        HS_vkCmdPreprocessGeneratedCommandsNV,
        PFN_vkCmdPreprocessGeneratedCommandsNV,
        VkCmdExecuteGeneratedCommandsNV,
        pattern VkCmdExecuteGeneratedCommandsNV,
        HS_vkCmdExecuteGeneratedCommandsNV,
        PFN_vkCmdExecuteGeneratedCommandsNV,
        VkCmdBindPipelineShaderGroupNV,
        pattern VkCmdBindPipelineShaderGroupNV,
        HS_vkCmdBindPipelineShaderGroupNV,
        PFN_vkCmdBindPipelineShaderGroupNV,
        VkCreateIndirectCommandsLayoutNV,
        pattern VkCreateIndirectCommandsLayoutNV,
        HS_vkCreateIndirectCommandsLayoutNV,
        PFN_vkCreateIndirectCommandsLayoutNV,
        VkDestroyIndirectCommandsLayoutNV,
        pattern VkDestroyIndirectCommandsLayoutNV,
        HS_vkDestroyIndirectCommandsLayoutNV,
        PFN_vkDestroyIndirectCommandsLayoutNV,
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
        VkAllocationCallbacks, VkMemoryAllocateFlagsInfo,
        VkMemoryAllocateFlagsInfoKHR, VkMemoryAllocateInfo,
        VkMemoryBarrier, VkMemoryDedicatedAllocateInfo,
        VkMemoryDedicatedAllocateInfoKHR, VkMemoryDedicatedRequirements,
        VkMemoryDedicatedRequirementsKHR, VkMemoryFdPropertiesKHR,
        VkMemoryGetFdInfoKHR, VkMemoryHeap,
        VkMemoryHostPointerPropertiesEXT,
        VkMemoryOpaqueCaptureAddressAllocateInfo,
        VkMemoryOpaqueCaptureAddressAllocateInfoKHR,
        VkMemoryPriorityAllocateInfoEXT, VkMemoryRequirements,
        VkMemoryRequirements2, VkMemoryRequirements2KHR, VkMemoryType,
        VK_NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION,
        pattern VK_NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION,
        VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME,
        pattern VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV,
        pattern VK_STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV,
        pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV,
        pattern VK_STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV,
        pattern VK_PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV,
        pattern VK_PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV,
        pattern VK_ACCESS_COMMAND_PREPROCESS_READ_BIT_NV,
        pattern VK_ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV,
        pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV)
       where
import GHC.Ptr                                                     (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                                (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.AccessFlags                      (VkAccessBitmask (..))
import Graphics.Vulkan.Types.Enum.Blend
import Graphics.Vulkan.Types.Enum.Color
import Graphics.Vulkan.Types.Enum.CompareOp
import Graphics.Vulkan.Types.Enum.CullModeFlags
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.DynamicState
import Graphics.Vulkan.Types.Enum.Format
import Graphics.Vulkan.Types.Enum.FrontFace
import Graphics.Vulkan.Types.Enum.IndexType
import Graphics.Vulkan.Types.Enum.Indirect
import Graphics.Vulkan.Types.Enum.InternalAllocationType
import Graphics.Vulkan.Types.Enum.LogicOp
import Graphics.Vulkan.Types.Enum.ObjectType                       (VkObjectType (..))
import Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import Graphics.Vulkan.Types.Enum.Pipeline
import Graphics.Vulkan.Types.Enum.PolygonMode
import Graphics.Vulkan.Types.Enum.PrimitiveTopology
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.Shader
import Graphics.Vulkan.Types.Enum.Stencil
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Enum.SystemAllocationScope
import Graphics.Vulkan.Types.Enum.VertexInputRate
import Graphics.Vulkan.Types.Funcpointers
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.AllocationCallbacks
import Graphics.Vulkan.Types.Struct.Bind                           (VkBindIndexBufferIndirectCommandNV,
                                                                    VkBindShaderGroupIndirectCommandNV,
                                                                    VkBindVertexBufferIndirectCommandNV)
import Graphics.Vulkan.Types.Struct.Device                         (VkDeviceCreateInfo,
                                                                    VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.Extent                         (VkExtent2D)
import Graphics.Vulkan.Types.Struct.GeneratedCommands
import Graphics.Vulkan.Types.Struct.Graphics                       (VkGraphicsPipelineShaderGroupsCreateInfoNV,
                                                                    VkGraphicsShaderGroupCreateInfoNV)
import Graphics.Vulkan.Types.Struct.IndirectCommands
import Graphics.Vulkan.Types.Struct.Memory
import Graphics.Vulkan.Types.Struct.Offset                         (VkOffset2D)
import Graphics.Vulkan.Types.Struct.PhysicalDevice                 (VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV,
                                                                    VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV,
                                                                    VkPhysicalDeviceFeatures2,
                                                                    VkPhysicalDeviceLimits,
                                                                    VkPhysicalDeviceProperties,
                                                                    VkPhysicalDeviceProperties2,
                                                                    VkPhysicalDeviceSparseProperties)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures         (VkPhysicalDeviceFeatures)
import Graphics.Vulkan.Types.Struct.Pipeline                       (VkGraphicsPipelineCreateInfo,
                                                                    VkPipelineColorBlendAttachmentState,
                                                                    VkPipelineColorBlendStateCreateInfo,
                                                                    VkPipelineDepthStencilStateCreateInfo,
                                                                    VkPipelineDynamicStateCreateInfo,
                                                                    VkPipelineInputAssemblyStateCreateInfo,
                                                                    VkPipelineMultisampleStateCreateInfo,
                                                                    VkPipelineRasterizationStateCreateInfo,
                                                                    VkPipelineShaderStageCreateInfo,
                                                                    VkPipelineTessellationStateCreateInfo,
                                                                    VkPipelineVertexInputStateCreateInfo,
                                                                    VkPipelineViewportStateCreateInfo)
import Graphics.Vulkan.Types.Struct.Rect                           (VkRect2D)
import Graphics.Vulkan.Types.Struct.SetStateFlagsIndirectCommandNV (VkSetStateFlagsIndirectCommandNV)
import Graphics.Vulkan.Types.Struct.Specialization                 (VkSpecializationInfo,
                                                                    VkSpecializationMapEntry)
import Graphics.Vulkan.Types.Struct.StencilOpState                 (VkStencilOpState)
import Graphics.Vulkan.Types.Struct.VertexInput                    (VkVertexInputAttributeDescription,
                                                                    VkVertexInputBindingDescription)
import Graphics.Vulkan.Types.Struct.Viewport                       (VkViewport)

pattern VkGetGeneratedCommandsMemoryRequirementsNV :: CString

pattern VkGetGeneratedCommandsMemoryRequirementsNV <-
        (is_VkGetGeneratedCommandsMemoryRequirementsNV -> True)
  where
    VkGetGeneratedCommandsMemoryRequirementsNV
      = _VkGetGeneratedCommandsMemoryRequirementsNV

{-# INLINE _VkGetGeneratedCommandsMemoryRequirementsNV #-}

_VkGetGeneratedCommandsMemoryRequirementsNV :: CString
_VkGetGeneratedCommandsMemoryRequirementsNV
  = Ptr "vkGetGeneratedCommandsMemoryRequirementsNV\NUL"#

{-# INLINE is_VkGetGeneratedCommandsMemoryRequirementsNV #-}

is_VkGetGeneratedCommandsMemoryRequirementsNV :: CString -> Bool
is_VkGetGeneratedCommandsMemoryRequirementsNV
  = (EQ ==) . cmpCStrings _VkGetGeneratedCommandsMemoryRequirementsNV

type VkGetGeneratedCommandsMemoryRequirementsNV =
     "vkGetGeneratedCommandsMemoryRequirementsNV"

-- | > void vkGetGeneratedCommandsMemoryRequirementsNV
--   >     ( VkDevice device
--   >     , const VkGeneratedCommandsMemoryRequirementsInfoNV* pInfo
--   >     , VkMemoryRequirements2* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetGeneratedCommandsMemoryRequirementsNV vkGetGeneratedCommandsMemoryRequirementsNV registry at www.khronos.org>
type HS_vkGetGeneratedCommandsMemoryRequirementsNV =
     VkDevice -- ^ device
              ->
       Ptr VkGeneratedCommandsMemoryRequirementsInfoNV -- ^ pInfo
                                                       ->
         Ptr VkMemoryRequirements2 -- ^ pMemoryRequirements
                                   -> IO ()

type PFN_vkGetGeneratedCommandsMemoryRequirementsNV =
     FunPtr HS_vkGetGeneratedCommandsMemoryRequirementsNV

foreign import ccall unsafe "dynamic"
               unwrapVkGetGeneratedCommandsMemoryRequirementsNVUnsafe ::
               PFN_vkGetGeneratedCommandsMemoryRequirementsNV ->
                 HS_vkGetGeneratedCommandsMemoryRequirementsNV

foreign import ccall safe "dynamic"
               unwrapVkGetGeneratedCommandsMemoryRequirementsNVSafe ::
               PFN_vkGetGeneratedCommandsMemoryRequirementsNV ->
                 HS_vkGetGeneratedCommandsMemoryRequirementsNV

instance VulkanProc "vkGetGeneratedCommandsMemoryRequirementsNV"
         where
    type VkProcType "vkGetGeneratedCommandsMemoryRequirementsNV" =
         HS_vkGetGeneratedCommandsMemoryRequirementsNV
    vkProcSymbol = _VkGetGeneratedCommandsMemoryRequirementsNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetGeneratedCommandsMemoryRequirementsNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetGeneratedCommandsMemoryRequirementsNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdPreprocessGeneratedCommandsNV :: CString

pattern VkCmdPreprocessGeneratedCommandsNV <-
        (is_VkCmdPreprocessGeneratedCommandsNV -> True)
  where
    VkCmdPreprocessGeneratedCommandsNV
      = _VkCmdPreprocessGeneratedCommandsNV

{-# INLINE _VkCmdPreprocessGeneratedCommandsNV #-}

_VkCmdPreprocessGeneratedCommandsNV :: CString
_VkCmdPreprocessGeneratedCommandsNV
  = Ptr "vkCmdPreprocessGeneratedCommandsNV\NUL"#

{-# INLINE is_VkCmdPreprocessGeneratedCommandsNV #-}

is_VkCmdPreprocessGeneratedCommandsNV :: CString -> Bool
is_VkCmdPreprocessGeneratedCommandsNV
  = (EQ ==) . cmpCStrings _VkCmdPreprocessGeneratedCommandsNV

type VkCmdPreprocessGeneratedCommandsNV =
     "vkCmdPreprocessGeneratedCommandsNV"

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdPreprocessGeneratedCommandsNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkGeneratedCommandsInfoNV* pGeneratedCommandsInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdPreprocessGeneratedCommandsNV vkCmdPreprocessGeneratedCommandsNV registry at www.khronos.org>
type HS_vkCmdPreprocessGeneratedCommandsNV =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr VkGeneratedCommandsInfoNV -- ^ pGeneratedCommandsInfo
                                                      -> IO ()

type PFN_vkCmdPreprocessGeneratedCommandsNV =
     FunPtr HS_vkCmdPreprocessGeneratedCommandsNV

foreign import ccall unsafe "dynamic"
               unwrapVkCmdPreprocessGeneratedCommandsNVUnsafe ::
               PFN_vkCmdPreprocessGeneratedCommandsNV ->
                 HS_vkCmdPreprocessGeneratedCommandsNV

foreign import ccall safe "dynamic"
               unwrapVkCmdPreprocessGeneratedCommandsNVSafe ::
               PFN_vkCmdPreprocessGeneratedCommandsNV ->
                 HS_vkCmdPreprocessGeneratedCommandsNV

instance VulkanProc "vkCmdPreprocessGeneratedCommandsNV" where
    type VkProcType "vkCmdPreprocessGeneratedCommandsNV" =
         HS_vkCmdPreprocessGeneratedCommandsNV
    vkProcSymbol = _VkCmdPreprocessGeneratedCommandsNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdPreprocessGeneratedCommandsNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdPreprocessGeneratedCommandsNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdExecuteGeneratedCommandsNV :: CString

pattern VkCmdExecuteGeneratedCommandsNV <-
        (is_VkCmdExecuteGeneratedCommandsNV -> True)
  where
    VkCmdExecuteGeneratedCommandsNV = _VkCmdExecuteGeneratedCommandsNV

{-# INLINE _VkCmdExecuteGeneratedCommandsNV #-}

_VkCmdExecuteGeneratedCommandsNV :: CString
_VkCmdExecuteGeneratedCommandsNV
  = Ptr "vkCmdExecuteGeneratedCommandsNV\NUL"#

{-# INLINE is_VkCmdExecuteGeneratedCommandsNV #-}

is_VkCmdExecuteGeneratedCommandsNV :: CString -> Bool
is_VkCmdExecuteGeneratedCommandsNV
  = (EQ ==) . cmpCStrings _VkCmdExecuteGeneratedCommandsNV

type VkCmdExecuteGeneratedCommandsNV =
     "vkCmdExecuteGeneratedCommandsNV"

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @inside@
--
--   > void vkCmdExecuteGeneratedCommandsNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBool32 isPreprocessed
--   >     , const VkGeneratedCommandsInfoNV* pGeneratedCommandsInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdExecuteGeneratedCommandsNV vkCmdExecuteGeneratedCommandsNV registry at www.khronos.org>
type HS_vkCmdExecuteGeneratedCommandsNV =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkBool32 -- ^ isPreprocessed
                -> Ptr VkGeneratedCommandsInfoNV -- ^ pGeneratedCommandsInfo
                                                 -> IO ()

type PFN_vkCmdExecuteGeneratedCommandsNV =
     FunPtr HS_vkCmdExecuteGeneratedCommandsNV

foreign import ccall unsafe "dynamic"
               unwrapVkCmdExecuteGeneratedCommandsNVUnsafe ::
               PFN_vkCmdExecuteGeneratedCommandsNV ->
                 HS_vkCmdExecuteGeneratedCommandsNV

foreign import ccall safe "dynamic"
               unwrapVkCmdExecuteGeneratedCommandsNVSafe ::
               PFN_vkCmdExecuteGeneratedCommandsNV ->
                 HS_vkCmdExecuteGeneratedCommandsNV

instance VulkanProc "vkCmdExecuteGeneratedCommandsNV" where
    type VkProcType "vkCmdExecuteGeneratedCommandsNV" =
         HS_vkCmdExecuteGeneratedCommandsNV
    vkProcSymbol = _VkCmdExecuteGeneratedCommandsNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdExecuteGeneratedCommandsNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdExecuteGeneratedCommandsNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdBindPipelineShaderGroupNV :: CString

pattern VkCmdBindPipelineShaderGroupNV <-
        (is_VkCmdBindPipelineShaderGroupNV -> True)
  where
    VkCmdBindPipelineShaderGroupNV = _VkCmdBindPipelineShaderGroupNV

{-# INLINE _VkCmdBindPipelineShaderGroupNV #-}

_VkCmdBindPipelineShaderGroupNV :: CString
_VkCmdBindPipelineShaderGroupNV
  = Ptr "vkCmdBindPipelineShaderGroupNV\NUL"#

{-# INLINE is_VkCmdBindPipelineShaderGroupNV #-}

is_VkCmdBindPipelineShaderGroupNV :: CString -> Bool
is_VkCmdBindPipelineShaderGroupNV
  = (EQ ==) . cmpCStrings _VkCmdBindPipelineShaderGroupNV

type VkCmdBindPipelineShaderGroupNV =
     "vkCmdBindPipelineShaderGroupNV"

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdBindPipelineShaderGroupNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineBindPoint pipelineBindPoint
--   >     , VkPipeline pipeline
--   >     , uint32_t groupIndex
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBindPipelineShaderGroupNV vkCmdBindPipelineShaderGroupNV registry at www.khronos.org>
type HS_vkCmdBindPipelineShaderGroupNV =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkPipelineBindPoint -- ^ pipelineBindPoint
                           -> VkPipeline -- ^ pipeline
                                         -> Word32 -- ^ groupIndex
                                                   -> IO ()

type PFN_vkCmdBindPipelineShaderGroupNV =
     FunPtr HS_vkCmdBindPipelineShaderGroupNV

foreign import ccall unsafe "dynamic"
               unwrapVkCmdBindPipelineShaderGroupNVUnsafe ::
               PFN_vkCmdBindPipelineShaderGroupNV ->
                 HS_vkCmdBindPipelineShaderGroupNV

foreign import ccall safe "dynamic"
               unwrapVkCmdBindPipelineShaderGroupNVSafe ::
               PFN_vkCmdBindPipelineShaderGroupNV ->
                 HS_vkCmdBindPipelineShaderGroupNV

instance VulkanProc "vkCmdBindPipelineShaderGroupNV" where
    type VkProcType "vkCmdBindPipelineShaderGroupNV" =
         HS_vkCmdBindPipelineShaderGroupNV
    vkProcSymbol = _VkCmdBindPipelineShaderGroupNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdBindPipelineShaderGroupNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdBindPipelineShaderGroupNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCreateIndirectCommandsLayoutNV :: CString

pattern VkCreateIndirectCommandsLayoutNV <-
        (is_VkCreateIndirectCommandsLayoutNV -> True)
  where
    VkCreateIndirectCommandsLayoutNV
      = _VkCreateIndirectCommandsLayoutNV

{-# INLINE _VkCreateIndirectCommandsLayoutNV #-}

_VkCreateIndirectCommandsLayoutNV :: CString
_VkCreateIndirectCommandsLayoutNV
  = Ptr "vkCreateIndirectCommandsLayoutNV\NUL"#

{-# INLINE is_VkCreateIndirectCommandsLayoutNV #-}

is_VkCreateIndirectCommandsLayoutNV :: CString -> Bool
is_VkCreateIndirectCommandsLayoutNV
  = (EQ ==) . cmpCStrings _VkCreateIndirectCommandsLayoutNV

type VkCreateIndirectCommandsLayoutNV =
     "vkCreateIndirectCommandsLayoutNV"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateIndirectCommandsLayoutNV
--   >     ( VkDevice device
--   >     , const VkIndirectCommandsLayoutCreateInfoNV* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkIndirectCommandsLayoutNV* pIndirectCommandsLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCreateIndirectCommandsLayoutNV vkCreateIndirectCommandsLayoutNV registry at www.khronos.org>
type HS_vkCreateIndirectCommandsLayoutNV =
     VkDevice -- ^ device
              ->
       Ptr VkIndirectCommandsLayoutCreateInfoNV -- ^ pCreateInfo
                                                ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   ->
           Ptr VkIndirectCommandsLayoutNV -- ^ pIndirectCommandsLayout
                                          -> IO VkResult

type PFN_vkCreateIndirectCommandsLayoutNV =
     FunPtr HS_vkCreateIndirectCommandsLayoutNV

foreign import ccall unsafe "dynamic"
               unwrapVkCreateIndirectCommandsLayoutNVUnsafe ::
               PFN_vkCreateIndirectCommandsLayoutNV ->
                 HS_vkCreateIndirectCommandsLayoutNV

foreign import ccall safe "dynamic"
               unwrapVkCreateIndirectCommandsLayoutNVSafe ::
               PFN_vkCreateIndirectCommandsLayoutNV ->
                 HS_vkCreateIndirectCommandsLayoutNV

instance VulkanProc "vkCreateIndirectCommandsLayoutNV" where
    type VkProcType "vkCreateIndirectCommandsLayoutNV" =
         HS_vkCreateIndirectCommandsLayoutNV
    vkProcSymbol = _VkCreateIndirectCommandsLayoutNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCreateIndirectCommandsLayoutNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateIndirectCommandsLayoutNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkDestroyIndirectCommandsLayoutNV :: CString

pattern VkDestroyIndirectCommandsLayoutNV <-
        (is_VkDestroyIndirectCommandsLayoutNV -> True)
  where
    VkDestroyIndirectCommandsLayoutNV
      = _VkDestroyIndirectCommandsLayoutNV

{-# INLINE _VkDestroyIndirectCommandsLayoutNV #-}

_VkDestroyIndirectCommandsLayoutNV :: CString
_VkDestroyIndirectCommandsLayoutNV
  = Ptr "vkDestroyIndirectCommandsLayoutNV\NUL"#

{-# INLINE is_VkDestroyIndirectCommandsLayoutNV #-}

is_VkDestroyIndirectCommandsLayoutNV :: CString -> Bool
is_VkDestroyIndirectCommandsLayoutNV
  = (EQ ==) . cmpCStrings _VkDestroyIndirectCommandsLayoutNV

type VkDestroyIndirectCommandsLayoutNV =
     "vkDestroyIndirectCommandsLayoutNV"

-- | > void vkDestroyIndirectCommandsLayoutNV
--   >     ( VkDevice device
--   >     , VkIndirectCommandsLayoutNV indirectCommandsLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkDestroyIndirectCommandsLayoutNV vkDestroyIndirectCommandsLayoutNV registry at www.khronos.org>
type HS_vkDestroyIndirectCommandsLayoutNV =
     VkDevice -- ^ device
              ->
       VkIndirectCommandsLayoutNV -- ^ indirectCommandsLayout
                                  -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                               -> IO ()

type PFN_vkDestroyIndirectCommandsLayoutNV =
     FunPtr HS_vkDestroyIndirectCommandsLayoutNV

foreign import ccall unsafe "dynamic"
               unwrapVkDestroyIndirectCommandsLayoutNVUnsafe ::
               PFN_vkDestroyIndirectCommandsLayoutNV ->
                 HS_vkDestroyIndirectCommandsLayoutNV

foreign import ccall safe "dynamic"
               unwrapVkDestroyIndirectCommandsLayoutNVSafe ::
               PFN_vkDestroyIndirectCommandsLayoutNV ->
                 HS_vkDestroyIndirectCommandsLayoutNV

instance VulkanProc "vkDestroyIndirectCommandsLayoutNV" where
    type VkProcType "vkDestroyIndirectCommandsLayoutNV" =
         HS_vkDestroyIndirectCommandsLayoutNV
    vkProcSymbol = _VkDestroyIndirectCommandsLayoutNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkDestroyIndirectCommandsLayoutNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkDestroyIndirectCommandsLayoutNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3

type VK_NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3

pattern VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: CString

pattern VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME <-
        (is_VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME -> True)
  where
    VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
      = _VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME

{-# INLINE _VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME #-}

_VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: CString
_VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
  = Ptr "VK_NV_device_generated_commands\NUL"#

{-# INLINE is_VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME #-}

is_VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME ::
                                                  CString -> Bool
is_VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME

type VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME =
     "VK_NV_device_generated_commands"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV
        = VkStructureType 1000277000

pattern VK_STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV =
        VkStructureType 1000277001

pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV
        = VkStructureType 1000277002

pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV =
        VkStructureType 1000277003

pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV =
        VkStructureType 1000277004

pattern VK_STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV =
        VkStructureType 1000277005

pattern VK_STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV
        = VkStructureType 1000277006

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV
        = VkStructureType 1000277007

-- | bitpos = @18@
pattern VK_PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV ::
        VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV =
        VkPipelineCreateBitmask 262144

-- | bitpos = @17@
pattern VK_PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV =
        VkPipelineStageBitmask 131072

-- | bitpos = @17@
pattern VK_ACCESS_COMMAND_PREPROCESS_READ_BIT_NV ::
        VkAccessBitmask a

pattern VK_ACCESS_COMMAND_PREPROCESS_READ_BIT_NV =
        VkAccessBitmask 131072

-- | bitpos = @18@
pattern VK_ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV ::
        VkAccessBitmask a

pattern VK_ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV =
        VkAccessBitmask 262144

-- | VkIndirectCommandsLayoutNV
pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV :: VkObjectType

pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV =
        VkObjectType 1000277000
