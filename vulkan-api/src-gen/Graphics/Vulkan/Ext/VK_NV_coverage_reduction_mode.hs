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
module Graphics.Vulkan.Ext.VK_NV_coverage_reduction_mode
       (-- * Vulkan extension: @VK_NV_coverage_reduction_mode@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Kedarnath Thangudu @kthangudu@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @251@
        --
        -- Required extensions: 'VK_NV_framebuffer_mixed_samples'.
        --

        -- ** Required extensions: 'VK_NV_framebuffer_mixed_samples'.
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkCoverageModulationModeNV(..), VkCoverageReductionModeNV(..),
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
        VkDeviceQueueCreateInfo, VkFramebufferMixedSamplesCombinationNV,
        VkPhysicalDeviceCoverageReductionModeFeaturesNV,
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        VkPipelineCoverageReductionStateCreateInfoNV,
        VkPipelineMultisampleStateCreateInfo, VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV,
        pattern VkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV,
        HS_vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV,
        PFN_vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV,
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
        VkFramebufferAttachmentImageInfo,
        VkFramebufferAttachmentImageInfoKHR,
        VkFramebufferAttachmentsCreateInfo,
        VkFramebufferAttachmentsCreateInfoKHR, VkFramebufferCreateInfo,
        VK_NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION,
        pattern VK_NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION,
        VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME,
        pattern VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV,
        pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                        (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Coverage
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.Framebuffer
import Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceCoverageReductionModeFeaturesNV,
                                                            VkPhysicalDeviceFeatures2)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)
import Graphics.Vulkan.Types.Struct.Pipeline               (VkPipelineCoverageReductionStateCreateInfoNV,
                                                            VkPipelineMultisampleStateCreateInfo)

pattern VkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
        :: CString

pattern VkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
        <-
        (is_VkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
           -> True)
  where
    VkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
      = _VkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV

{-# INLINE _VkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
           #-}

_VkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV ::
                                                                   CString
_VkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
  = Ptr
      "vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV\NUL"#

{-# INLINE is_VkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
           #-}

is_VkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV ::
                                                                     CString -> Bool
is_VkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
  = (EQ ==) .
      cmpCStrings
        _VkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV

type VkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
     =
     "vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pCombinationCount
--   >     , VkFramebufferMixedSamplesCombinationNV* pCombinations
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV registry at www.khronos.org>
type HS_vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
     =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr Word32 -- ^ pCombinationCount
                  ->
         Ptr VkFramebufferMixedSamplesCombinationNV -- ^ pCombinations
                                                    -> IO VkResult

type PFN_vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
     =
     FunPtr
       HS_vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNVUnsafe
               ::
               PFN_vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
                 ->
                 HS_vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNVSafe
               ::
               PFN_vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
                 ->
                 HS_vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV

instance VulkanProc
           "vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV"
         where
    type VkProcType
           "vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV"
         =
         HS_vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV
    vkProcSymbol
      = _VkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION = 1

type VK_NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION = 1

pattern VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME :: CString

pattern VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME <-
        (is_VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME -> True)
  where
    VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME
      = _VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME

{-# INLINE _VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME #-}

_VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME :: CString
_VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME
  = Ptr "VK_NV_coverage_reduction_mode\NUL"#

{-# INLINE is_VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME #-}

is_VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME :: CString -> Bool
is_VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME

type VK_NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME =
     "VK_NV_coverage_reduction_mode"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV
        = VkStructureType 1000250000

pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV
        = VkStructureType 1000250001

pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV
        = VkStructureType 1000250002
