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
module Graphics.Vulkan.Ext.VK_KHR_pipeline_executable_properties
       (-- * Vulkan extension: @VK_KHR_pipeline_executable_properties@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jason Ekstrand @jekstrand@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @270@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
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
        VkDeviceDiagnosticsConfigBitmaskNV(..), VkDeviceEventTypeEXT(..),
        VkDeviceGroupPresentModeBitmaskKHR(..), VkDeviceCreateFlagBits(..),
        VkDeviceDiagnosticsConfigFlagBitsNV(),
        VkDeviceDiagnosticsConfigFlagsNV(),
        VkDeviceGroupPresentModeFlagBitsKHR(),
        VkDeviceGroupPresentModeFlagsKHR(), VkDeviceQueueCreateBitmask(..),
        VkDeviceQueueCreateFlagBits(), VkDeviceQueueCreateFlags(),
        VkDeviceQueueCreateInfo, VkPhysicalDeviceFeatures,
        VkPhysicalDeviceFeatures2,
        VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR,
        VkPipelineExecutableInfoKHR,
        VkPipelineExecutableInternalRepresentationKHR,
        VkPipelineExecutablePropertiesKHR, VkPipelineBindPoint(..),
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
        VkPipelineStageFlags(), VkPipelineExecutableStatisticKHR,
        VkPipelineExecutableStatisticValueKHR, VkPipelineInfoKHR,
        VkShaderFloatControlsIndependence(..), VkShaderInfoTypeAMD(..),
        VkShaderStageBitmask(..), VkShaderCorePropertiesBitmaskAMD(..),
        VkShaderCorePropertiesFlagBitsAMD(),
        VkShaderCorePropertiesFlagsAMD(),
        VkShaderFloatControlsIndependenceKHR(..),
        VkShaderModuleCreateBitmask(..), VkShaderModuleCreateFlagBits(),
        VkShaderModuleCreateFlags(), VkShaderStageFlagBits(),
        VkShaderStageFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VkGetPipelineExecutablePropertiesKHR,
        pattern VkGetPipelineExecutablePropertiesKHR,
        HS_vkGetPipelineExecutablePropertiesKHR,
        PFN_vkGetPipelineExecutablePropertiesKHR,
        VkGetPipelineExecutableStatisticsKHR,
        pattern VkGetPipelineExecutableStatisticsKHR,
        HS_vkGetPipelineExecutableStatisticsKHR,
        PFN_vkGetPipelineExecutableStatisticsKHR,
        VkGetPipelineExecutableInternalRepresentationsKHR,
        pattern VkGetPipelineExecutableInternalRepresentationsKHR,
        HS_vkGetPipelineExecutableInternalRepresentationsKHR,
        PFN_vkGetPipelineExecutableInternalRepresentationsKHR,
        VkResult(..), VkAccelerationStructureKHR,
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
        VkPipelineDynamicStateCreateInfo,
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
        VkPipelineShaderStageCreateInfo,
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
        VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION,
        pattern VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION,
        VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME,
        pattern VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR,
        pattern VK_STRUCTURE_TYPE_PIPELINE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR,
        pattern VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR,
        pattern VK_PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR,
        pattern VK_PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                        (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.Pipeline
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.Shader
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceFeatures2,
                                                            VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)
import Graphics.Vulkan.Types.Struct.Pipeline

pattern VkGetPipelineExecutablePropertiesKHR :: CString

pattern VkGetPipelineExecutablePropertiesKHR <-
        (is_VkGetPipelineExecutablePropertiesKHR -> True)
  where
    VkGetPipelineExecutablePropertiesKHR
      = _VkGetPipelineExecutablePropertiesKHR

{-# INLINE _VkGetPipelineExecutablePropertiesKHR #-}

_VkGetPipelineExecutablePropertiesKHR :: CString
_VkGetPipelineExecutablePropertiesKHR
  = Ptr "vkGetPipelineExecutablePropertiesKHR\NUL"#

{-# INLINE is_VkGetPipelineExecutablePropertiesKHR #-}

is_VkGetPipelineExecutablePropertiesKHR :: CString -> Bool
is_VkGetPipelineExecutablePropertiesKHR
  = (EQ ==) . cmpCStrings _VkGetPipelineExecutablePropertiesKHR

type VkGetPipelineExecutablePropertiesKHR =
     "vkGetPipelineExecutablePropertiesKHR"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPipelineExecutablePropertiesKHR
--   >     ( VkDevice                        device
--   >     , const VkPipelineInfoKHR*        pPipelineInfo
--   >     , uint32_t* pExecutableCount
--   >     , VkPipelineExecutablePropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetPipelineExecutablePropertiesKHR vkGetPipelineExecutablePropertiesKHR registry at www.khronos.org>
type HS_vkGetPipelineExecutablePropertiesKHR =
     VkDevice -- ^ device
              ->
       Ptr VkPipelineInfoKHR -- ^ pPipelineInfo
                             ->
         Ptr Word32 -- ^ pExecutableCount
                    -> Ptr VkPipelineExecutablePropertiesKHR -- ^ pProperties
                                                             -> IO VkResult

type PFN_vkGetPipelineExecutablePropertiesKHR =
     FunPtr HS_vkGetPipelineExecutablePropertiesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPipelineExecutablePropertiesKHRUnsafe ::
               PFN_vkGetPipelineExecutablePropertiesKHR ->
                 HS_vkGetPipelineExecutablePropertiesKHR

foreign import ccall safe "dynamic"
               unwrapVkGetPipelineExecutablePropertiesKHRSafe ::
               PFN_vkGetPipelineExecutablePropertiesKHR ->
                 HS_vkGetPipelineExecutablePropertiesKHR

instance VulkanProc "vkGetPipelineExecutablePropertiesKHR" where
    type VkProcType "vkGetPipelineExecutablePropertiesKHR" =
         HS_vkGetPipelineExecutablePropertiesKHR
    vkProcSymbol = _VkGetPipelineExecutablePropertiesKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPipelineExecutablePropertiesKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPipelineExecutablePropertiesKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPipelineExecutableStatisticsKHR :: CString

pattern VkGetPipelineExecutableStatisticsKHR <-
        (is_VkGetPipelineExecutableStatisticsKHR -> True)
  where
    VkGetPipelineExecutableStatisticsKHR
      = _VkGetPipelineExecutableStatisticsKHR

{-# INLINE _VkGetPipelineExecutableStatisticsKHR #-}

_VkGetPipelineExecutableStatisticsKHR :: CString
_VkGetPipelineExecutableStatisticsKHR
  = Ptr "vkGetPipelineExecutableStatisticsKHR\NUL"#

{-# INLINE is_VkGetPipelineExecutableStatisticsKHR #-}

is_VkGetPipelineExecutableStatisticsKHR :: CString -> Bool
is_VkGetPipelineExecutableStatisticsKHR
  = (EQ ==) . cmpCStrings _VkGetPipelineExecutableStatisticsKHR

type VkGetPipelineExecutableStatisticsKHR =
     "vkGetPipelineExecutableStatisticsKHR"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPipelineExecutableStatisticsKHR
--   >     ( VkDevice                        device
--   >     , const VkPipelineExecutableInfoKHR*  pExecutableInfo
--   >     , uint32_t* pStatisticCount
--   >     , VkPipelineExecutableStatisticKHR* pStatistics
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetPipelineExecutableStatisticsKHR vkGetPipelineExecutableStatisticsKHR registry at www.khronos.org>
type HS_vkGetPipelineExecutableStatisticsKHR =
     VkDevice -- ^ device
              ->
       Ptr VkPipelineExecutableInfoKHR -- ^ pExecutableInfo
                                       ->
         Ptr Word32 -- ^ pStatisticCount
                    -> Ptr VkPipelineExecutableStatisticKHR -- ^ pStatistics
                                                            -> IO VkResult

type PFN_vkGetPipelineExecutableStatisticsKHR =
     FunPtr HS_vkGetPipelineExecutableStatisticsKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPipelineExecutableStatisticsKHRUnsafe ::
               PFN_vkGetPipelineExecutableStatisticsKHR ->
                 HS_vkGetPipelineExecutableStatisticsKHR

foreign import ccall safe "dynamic"
               unwrapVkGetPipelineExecutableStatisticsKHRSafe ::
               PFN_vkGetPipelineExecutableStatisticsKHR ->
                 HS_vkGetPipelineExecutableStatisticsKHR

instance VulkanProc "vkGetPipelineExecutableStatisticsKHR" where
    type VkProcType "vkGetPipelineExecutableStatisticsKHR" =
         HS_vkGetPipelineExecutableStatisticsKHR
    vkProcSymbol = _VkGetPipelineExecutableStatisticsKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPipelineExecutableStatisticsKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPipelineExecutableStatisticsKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPipelineExecutableInternalRepresentationsKHR ::
        CString

pattern VkGetPipelineExecutableInternalRepresentationsKHR <-
        (is_VkGetPipelineExecutableInternalRepresentationsKHR -> True)
  where
    VkGetPipelineExecutableInternalRepresentationsKHR
      = _VkGetPipelineExecutableInternalRepresentationsKHR

{-# INLINE _VkGetPipelineExecutableInternalRepresentationsKHR #-}

_VkGetPipelineExecutableInternalRepresentationsKHR :: CString
_VkGetPipelineExecutableInternalRepresentationsKHR
  = Ptr "vkGetPipelineExecutableInternalRepresentationsKHR\NUL"#

{-# INLINE is_VkGetPipelineExecutableInternalRepresentationsKHR #-}

is_VkGetPipelineExecutableInternalRepresentationsKHR ::
                                                     CString -> Bool
is_VkGetPipelineExecutableInternalRepresentationsKHR
  = (EQ ==) .
      cmpCStrings _VkGetPipelineExecutableInternalRepresentationsKHR

type VkGetPipelineExecutableInternalRepresentationsKHR =
     "vkGetPipelineExecutableInternalRepresentationsKHR"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPipelineExecutableInternalRepresentationsKHR
--   >     ( VkDevice                        device
--   >     , const VkPipelineExecutableInfoKHR*  pExecutableInfo
--   >     , uint32_t* pInternalRepresentationCount
--   >     , VkPipelineExecutableInternalRepresentationKHR* pInternalRepresentations
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetPipelineExecutableInternalRepresentationsKHR vkGetPipelineExecutableInternalRepresentationsKHR registry at www.khronos.org>
type HS_vkGetPipelineExecutableInternalRepresentationsKHR =
     VkDevice -- ^ device
              ->
       Ptr VkPipelineExecutableInfoKHR -- ^ pExecutableInfo
                                       ->
         Ptr Word32 -- ^ pInternalRepresentationCount
                    ->
           Ptr VkPipelineExecutableInternalRepresentationKHR -- ^ pInternalRepresentations
                                                             -> IO VkResult

type PFN_vkGetPipelineExecutableInternalRepresentationsKHR =
     FunPtr HS_vkGetPipelineExecutableInternalRepresentationsKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPipelineExecutableInternalRepresentationsKHRUnsafe ::
               PFN_vkGetPipelineExecutableInternalRepresentationsKHR ->
                 HS_vkGetPipelineExecutableInternalRepresentationsKHR

foreign import ccall safe "dynamic"
               unwrapVkGetPipelineExecutableInternalRepresentationsKHRSafe ::
               PFN_vkGetPipelineExecutableInternalRepresentationsKHR ->
                 HS_vkGetPipelineExecutableInternalRepresentationsKHR

instance VulkanProc
           "vkGetPipelineExecutableInternalRepresentationsKHR"
         where
    type VkProcType "vkGetPipelineExecutableInternalRepresentationsKHR"
         = HS_vkGetPipelineExecutableInternalRepresentationsKHR
    vkProcSymbol = _VkGetPipelineExecutableInternalRepresentationsKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPipelineExecutableInternalRepresentationsKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPipelineExecutableInternalRepresentationsKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION = 1

type VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION = 1

pattern VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME ::
        CString

pattern VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME <-
        (is_VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME -> True)
  where
    VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME
      = _VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME

{-# INLINE _VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME
           #-}

_VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME :: CString
_VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME
  = Ptr "VK_KHR_pipeline_executable_properties\NUL"#

{-# INLINE is_VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME
           #-}

is_VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME ::
                                                        CString -> Bool
is_VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME

type VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME =
     "VK_KHR_pipeline_executable_properties"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR
        = VkStructureType 1000269000

pattern VK_STRUCTURE_TYPE_PIPELINE_INFO_KHR :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_INFO_KHR =
        VkStructureType 1000269001

pattern VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR =
        VkStructureType 1000269002

pattern VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR =
        VkStructureType 1000269003

pattern VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR =
        VkStructureType 1000269004

pattern VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR
        = VkStructureType 1000269005

-- | bitpos = @6@
pattern VK_PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR ::
        VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR =
        VkPipelineCreateBitmask 64

-- | bitpos = @7@
pattern VK_PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR
        :: VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR
        = VkPipelineCreateBitmask 128
