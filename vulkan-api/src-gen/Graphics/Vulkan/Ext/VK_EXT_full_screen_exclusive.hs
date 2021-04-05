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
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_full_screen_exclusive
       (AHardwareBuffer(), ANativeWindow(), CAMetalLayer(), VkBool32(..),
        VkDeviceAddress(..), VkDeviceSize(..), VkFlags(..),
        VkSampleMask(..), pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR,
        VkColorComponentBitmask(..), VkColorSpaceKHR(..),
        VkColorComponentFlagBits(), VkColorComponentFlags(),
        VkCompositeAlphaBitmaskKHR(..), VkCompositeAlphaFlagBitsKHR(),
        VkCompositeAlphaFlagsKHR(), VkExtent2D, VkFormat(..),
        VkFormatFeatureBitmask(..), VkFormatFeatureFlagBits(),
        VkFormatFeatureFlags(), VkFullScreenExclusiveEXT(..),
        VkImageAspectBitmask(..), VkImageCreateBitmask(..),
        VkImageLayout(..), VkImageTiling(..), VkImageType(..),
        VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(),
        VkImageViewCreateBitmask(..), VkImageViewCreateFlagBits(),
        VkImageViewCreateFlags(), VkPhysicalDeviceSurfaceInfo2KHR,
        VkPresentModeKHR(..), VkSharingMode(..), VkStructureType(..),
        VkSurfaceCapabilities2KHR,
        VkSurfaceCapabilitiesFullScreenExclusiveEXT,
        VkSurfaceCapabilitiesKHR, VkSurfaceFullScreenExclusiveInfoEXT,
        VkSurfaceCounterBitmaskEXT(..), VkSurfaceTransformBitmaskKHR(..),
        VkSurfaceCounterFlagBitsEXT(), VkSurfaceCounterFlagsEXT(),
        VkSurfaceTransformFlagBitsKHR(), VkSurfaceTransformFlagsKHR(),
        VkSwapchainImageUsageBitmaskANDROID(..),
        VkSwapchainCreateBitmaskKHR(..), VkSwapchainCreateFlagBitsKHR(),
        VkSwapchainCreateFlagsKHR(),
        VkSwapchainImageUsageFlagBitsANDROID(),
        VkSwapchainImageUsageFlagsANDROID(), VkSwapchainCreateInfoKHR,
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceSurfacePresentModes2EXT,
        pattern VkGetPhysicalDeviceSurfacePresentModes2EXT,
        HS_vkGetPhysicalDeviceSurfacePresentModes2EXT,
        PFN_vkGetPhysicalDeviceSurfacePresentModes2EXT,
        VkAcquireFullScreenExclusiveModeEXT,
        pattern VkAcquireFullScreenExclusiveModeEXT,
        HS_vkAcquireFullScreenExclusiveModeEXT,
        PFN_vkAcquireFullScreenExclusiveModeEXT,
        VkReleaseFullScreenExclusiveModeEXT,
        pattern VkReleaseFullScreenExclusiveModeEXT,
        HS_vkReleaseFullScreenExclusiveModeEXT,
        PFN_vkReleaseFullScreenExclusiveModeEXT, VkResult(..),
        VkAccelerationStructureKHR, VkAccelerationStructureKHR_T(),
        VkAccelerationStructureNV, VkAccelerationStructureNV_T(), VkBuffer,
        VkBufferView, VkBufferView_T(), VkBuffer_T(), VkCommandBuffer,
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
        VkPhysicalDevice16BitStorageFeatures,
        VkPhysicalDevice16BitStorageFeaturesKHR,
        VkPhysicalDevice4444FormatsFeaturesEXT,
        VkPhysicalDevice8BitStorageFeatures,
        VkPhysicalDevice8BitStorageFeaturesKHR,
        VkPhysicalDeviceASTCDecodeFeaturesEXT,
        VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT,
        VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT,
        VkPhysicalDeviceBufferAddressFeaturesEXT,
        VkPhysicalDeviceBufferDeviceAddressFeatures,
        VkPhysicalDeviceBufferDeviceAddressFeaturesEXT,
        VkPhysicalDeviceBufferDeviceAddressFeaturesKHR,
        VkPhysicalDeviceCoherentMemoryFeaturesAMD,
        VkPhysicalDeviceComputeShaderDerivativesFeaturesNV,
        VkPhysicalDeviceConditionalRenderingFeaturesEXT,
        VkPhysicalDeviceConservativeRasterizationPropertiesEXT,
        VkPhysicalDeviceCooperativeMatrixFeaturesNV,
        VkPhysicalDeviceCooperativeMatrixPropertiesNV,
        VkPhysicalDeviceCornerSampledImageFeaturesNV,
        VkPhysicalDeviceCoverageReductionModeFeaturesNV,
        VkPhysicalDeviceCustomBorderColorFeaturesEXT,
        VkPhysicalDeviceCustomBorderColorPropertiesEXT,
        VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV,
        VkPhysicalDeviceDepthClipEnableFeaturesEXT,
        VkPhysicalDeviceDepthStencilResolveProperties,
        VkPhysicalDeviceDepthStencilResolvePropertiesKHR,
        VkPhysicalDeviceDescriptorIndexingFeatures,
        VkPhysicalDeviceDescriptorIndexingFeaturesEXT,
        VkPhysicalDeviceDescriptorIndexingProperties,
        VkPhysicalDeviceDescriptorIndexingPropertiesEXT,
        VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV,
        VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV,
        VkPhysicalDeviceDiagnosticsConfigFeaturesNV,
        VkPhysicalDeviceDiscardRectanglePropertiesEXT,
        VkPhysicalDeviceDriverProperties,
        VkPhysicalDeviceDriverPropertiesKHR,
        VkPhysicalDeviceExclusiveScissorFeaturesNV,
        VkPhysicalDeviceExtendedDynamicStateFeaturesEXT,
        VkPhysicalDeviceExternalBufferInfo,
        VkPhysicalDeviceExternalBufferInfoKHR,
        VkPhysicalDeviceExternalFenceInfo,
        VkPhysicalDeviceExternalFenceInfoKHR,
        VkPhysicalDeviceExternalImageFormatInfo,
        VkPhysicalDeviceExternalImageFormatInfoKHR,
        VkPhysicalDeviceExternalMemoryHostPropertiesEXT,
        VkPhysicalDeviceExternalSemaphoreInfo,
        VkPhysicalDeviceExternalSemaphoreInfoKHR,
        VkPhysicalDeviceFeatures2, VkPhysicalDeviceFeatures2KHR,
        VkPhysicalDeviceFloat16Int8FeaturesKHR,
        VkPhysicalDeviceFloatControlsProperties,
        VkPhysicalDeviceFloatControlsPropertiesKHR,
        VkPhysicalDeviceFragmentDensityMap2FeaturesEXT,
        VkPhysicalDeviceFragmentDensityMap2PropertiesEXT,
        VkPhysicalDeviceFragmentDensityMapFeaturesEXT,
        VkPhysicalDeviceFragmentDensityMapPropertiesEXT,
        VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV,
        VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT,
        VkPhysicalDeviceGroupProperties,
        VkPhysicalDeviceGroupPropertiesKHR,
        VkPhysicalDeviceHostQueryResetFeatures,
        VkPhysicalDeviceHostQueryResetFeaturesEXT,
        VkPhysicalDeviceIDProperties, VkPhysicalDeviceIDPropertiesKHR,
        VkPhysicalDeviceImageDrmFormatModifierInfoEXT,
        VkPhysicalDeviceImageFormatInfo2,
        VkPhysicalDeviceImageFormatInfo2KHR,
        VkPhysicalDeviceImageRobustnessFeaturesEXT,
        VkPhysicalDeviceImageViewImageFormatInfoEXT,
        VkPhysicalDeviceImagelessFramebufferFeatures,
        VkPhysicalDeviceImagelessFramebufferFeaturesKHR,
        VkPhysicalDeviceIndexTypeUint8FeaturesEXT,
        VkPhysicalDeviceInlineUniformBlockFeaturesEXT,
        VkPhysicalDeviceInlineUniformBlockPropertiesEXT,
        VkPhysicalDeviceLimits,
        VkPhysicalDeviceLineRasterizationFeaturesEXT,
        VkPhysicalDeviceLineRasterizationPropertiesEXT,
        VkPhysicalDeviceMaintenance3Properties,
        VkPhysicalDeviceMaintenance3PropertiesKHR,
        VkPhysicalDeviceMemoryBudgetPropertiesEXT,
        VkPhysicalDeviceMemoryPriorityFeaturesEXT,
        VkPhysicalDeviceMemoryProperties,
        VkPhysicalDeviceMemoryProperties2,
        VkPhysicalDeviceMemoryProperties2KHR,
        VkPhysicalDeviceMeshShaderFeaturesNV,
        VkPhysicalDeviceMeshShaderPropertiesNV,
        VkPhysicalDeviceMultiviewFeatures,
        VkPhysicalDeviceMultiviewFeaturesKHR,
        VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX,
        VkPhysicalDeviceMultiviewProperties,
        VkPhysicalDeviceMultiviewPropertiesKHR,
        VkPhysicalDevicePCIBusInfoPropertiesEXT,
        VkPhysicalDevicePerformanceQueryFeaturesKHR,
        VkPhysicalDevicePerformanceQueryPropertiesKHR,
        VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT,
        VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR,
        VkPhysicalDevicePointClippingProperties,
        VkPhysicalDevicePointClippingPropertiesKHR,
        VkPhysicalDevicePrivateDataFeaturesEXT, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2, VkPhysicalDeviceProperties2KHR,
        VkPhysicalDeviceProtectedMemoryFeatures,
        VkPhysicalDeviceProtectedMemoryProperties,
        VkPhysicalDevicePushDescriptorPropertiesKHR,
        VkPhysicalDeviceRayTracingPropertiesNV,
        VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV,
        VkPhysicalDeviceRobustness2FeaturesEXT,
        VkPhysicalDeviceRobustness2PropertiesEXT,
        VkPhysicalDeviceSampleLocationsPropertiesEXT,
        VkPhysicalDeviceSamplerFilterMinmaxProperties,
        VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT,
        VkPhysicalDeviceSamplerYcbcrConversionFeatures,
        VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR,
        VkPhysicalDeviceScalarBlockLayoutFeatures,
        VkPhysicalDeviceScalarBlockLayoutFeaturesEXT,
        VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures,
        VkPhysicalDeviceSeparateDepthStencilLayoutsFeaturesKHR,
        VkPhysicalDeviceShaderAtomicFloatFeaturesEXT,
        VkPhysicalDeviceShaderAtomicInt64Features,
        VkPhysicalDeviceShaderAtomicInt64FeaturesKHR,
        VkPhysicalDeviceShaderClockFeaturesKHR,
        VkPhysicalDeviceShaderCoreProperties2AMD,
        VkPhysicalDeviceShaderCorePropertiesAMD,
        VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT,
        VkPhysicalDeviceShaderDrawParameterFeatures,
        VkPhysicalDeviceShaderDrawParametersFeatures,
        VkPhysicalDeviceShaderFloat16Int8Features,
        VkPhysicalDeviceShaderFloat16Int8FeaturesKHR,
        VkPhysicalDeviceShaderImageFootprintFeaturesNV,
        VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL,
        VkPhysicalDeviceShaderSMBuiltinsFeaturesNV,
        VkPhysicalDeviceShaderSMBuiltinsPropertiesNV,
        VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures,
        VkPhysicalDeviceShaderSubgroupExtendedTypesFeaturesKHR,
        VkPhysicalDeviceShadingRateImageFeaturesNV,
        VkPhysicalDeviceShadingRateImagePropertiesNV,
        VkPhysicalDeviceSparseImageFormatInfo2,
        VkPhysicalDeviceSparseImageFormatInfo2KHR,
        VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceSubgroupProperties,
        VkPhysicalDeviceSubgroupSizeControlFeaturesEXT,
        VkPhysicalDeviceSubgroupSizeControlPropertiesEXT,
        VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT,
        VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT,
        VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT,
        VkPhysicalDeviceTimelineSemaphoreFeatures,
        VkPhysicalDeviceTimelineSemaphoreFeaturesKHR,
        VkPhysicalDeviceTimelineSemaphoreProperties,
        VkPhysicalDeviceTimelineSemaphorePropertiesKHR,
        VkPhysicalDeviceToolPropertiesEXT,
        VkPhysicalDeviceTransformFeedbackFeaturesEXT,
        VkPhysicalDeviceTransformFeedbackPropertiesEXT,
        VkPhysicalDeviceUniformBufferStandardLayoutFeatures,
        VkPhysicalDeviceUniformBufferStandardLayoutFeaturesKHR,
        VkPhysicalDeviceVariablePointerFeatures,
        VkPhysicalDeviceVariablePointerFeaturesKHR,
        VkPhysicalDeviceVariablePointersFeatures,
        VkPhysicalDeviceVariablePointersFeaturesKHR,
        VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT,
        VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT,
        VkPhysicalDeviceVulkan11Features,
        VkPhysicalDeviceVulkan11Properties,
        VkPhysicalDeviceVulkan12Features,
        VkPhysicalDeviceVulkan12Properties,
        VkPhysicalDeviceVulkanMemoryModelFeatures,
        VkPhysicalDeviceVulkanMemoryModelFeaturesKHR,
        VkPhysicalDeviceYcbcrImageArraysFeaturesEXT,
        VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION,
        pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION,
        VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME,
        pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT,
        pattern VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT,
        VkSurfaceFullScreenExclusiveWin32InfoEXT,
        -- > #include "vk_platform.h"
        pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT,
        -- ** Required extensions: 'VK_KHR_device_group', 'VK_KHR_get_physical_device_properties2', 'VK_KHR_surface', 'VK_KHR_get_surface_capabilities2', 'VK_KHR_swapchain'.
        VkGetDeviceGroupSurfacePresentModes2EXT,
        pattern VkGetDeviceGroupSurfacePresentModes2EXT,
        HS_vkGetDeviceGroupSurfacePresentModes2EXT,
        PFN_vkGetDeviceGroupSurfacePresentModes2EXT,
        module Graphics.Vulkan.Marshal,
        VkDeviceDiagnosticsConfigBitmaskNV(..), VkDeviceEventTypeEXT(..),
        VkDeviceGroupPresentModeBitmaskKHR(..), VkDeviceCreateFlagBits(..),
        VkDeviceDiagnosticsConfigFlagBitsNV(),
        VkDeviceDiagnosticsConfigFlagsNV(),
        VkDeviceGroupPresentModeFlagBitsKHR(),
        VkDeviceGroupPresentModeFlagsKHR(), VkDeviceQueueCreateBitmask(..),
        VkDeviceQueueCreateFlagBits(), VkDeviceQueueCreateFlags())
       where
import GHC.Ptr                                           (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                      (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Enum.Color
import Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.Format
import Graphics.Vulkan.Types.Enum.FullScreenExclusiveEXT
import Graphics.Vulkan.Types.Enum.Image
import Graphics.Vulkan.Types.Enum.PresentModeKHR
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.SharingMode
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Enum.Surface
import Graphics.Vulkan.Types.Enum.Swapchain
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.Extent               (VkExtent2D)
import Graphics.Vulkan.Types.Struct.PhysicalDevice
import Graphics.Vulkan.Types.Struct.PlatformWin32Khr     (VkSurfaceCapabilitiesFullScreenExclusiveEXT,
                                                          VkSurfaceFullScreenExclusiveInfoEXT,
                                                          VkSurfaceFullScreenExclusiveWin32InfoEXT)
import Graphics.Vulkan.Types.Struct.Surface              (VkSurfaceCapabilities2KHR,
                                                          VkSurfaceCapabilitiesKHR)
import Graphics.Vulkan.Types.Struct.Swapchain            (VkSwapchainCreateInfoKHR)

pattern VkGetPhysicalDeviceSurfacePresentModes2EXT :: CString

pattern VkGetPhysicalDeviceSurfacePresentModes2EXT <-
        (is_VkGetPhysicalDeviceSurfacePresentModes2EXT -> True)
  where
    VkGetPhysicalDeviceSurfacePresentModes2EXT
      = _VkGetPhysicalDeviceSurfacePresentModes2EXT

{-# INLINE _VkGetPhysicalDeviceSurfacePresentModes2EXT #-}

_VkGetPhysicalDeviceSurfacePresentModes2EXT :: CString
_VkGetPhysicalDeviceSurfacePresentModes2EXT
  = Ptr "vkGetPhysicalDeviceSurfacePresentModes2EXT\NUL"#

{-# INLINE is_VkGetPhysicalDeviceSurfacePresentModes2EXT #-}

is_VkGetPhysicalDeviceSurfacePresentModes2EXT :: CString -> Bool
is_VkGetPhysicalDeviceSurfacePresentModes2EXT
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceSurfacePresentModes2EXT

type VkGetPhysicalDeviceSurfacePresentModes2EXT =
     "vkGetPhysicalDeviceSurfacePresentModes2EXT"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfacePresentModes2EXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSurfaceInfo2KHR* pSurfaceInfo
--   >     , uint32_t* pPresentModeCount
--   >     , VkPresentModeKHR* pPresentModes
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfacePresentModes2EXT vkGetPhysicalDeviceSurfacePresentModes2EXT registry at www.khronos.org>
type HS_vkGetPhysicalDeviceSurfacePresentModes2EXT =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkPhysicalDeviceSurfaceInfo2KHR -- ^ pSurfaceInfo
                                           ->
         Ptr Word32 -- ^ pPresentModeCount
                    -> Ptr VkPresentModeKHR -- ^ pPresentModes
                                            -> IO VkResult

type PFN_vkGetPhysicalDeviceSurfacePresentModes2EXT =
     FunPtr HS_vkGetPhysicalDeviceSurfacePresentModes2EXT

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceSurfacePresentModes2EXTUnsafe ::
               PFN_vkGetPhysicalDeviceSurfacePresentModes2EXT ->
                 HS_vkGetPhysicalDeviceSurfacePresentModes2EXT

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceSurfacePresentModes2EXTSafe ::
               PFN_vkGetPhysicalDeviceSurfacePresentModes2EXT ->
                 HS_vkGetPhysicalDeviceSurfacePresentModes2EXT

instance VulkanProc "vkGetPhysicalDeviceSurfacePresentModes2EXT"
         where
    type VkProcType "vkGetPhysicalDeviceSurfacePresentModes2EXT" =
         HS_vkGetPhysicalDeviceSurfacePresentModes2EXT
    vkProcSymbol = _VkGetPhysicalDeviceSurfacePresentModes2EXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceSurfacePresentModes2EXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceSurfacePresentModes2EXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkAcquireFullScreenExclusiveModeEXT :: CString

pattern VkAcquireFullScreenExclusiveModeEXT <-
        (is_VkAcquireFullScreenExclusiveModeEXT -> True)
  where
    VkAcquireFullScreenExclusiveModeEXT
      = _VkAcquireFullScreenExclusiveModeEXT

{-# INLINE _VkAcquireFullScreenExclusiveModeEXT #-}

_VkAcquireFullScreenExclusiveModeEXT :: CString
_VkAcquireFullScreenExclusiveModeEXT
  = Ptr "vkAcquireFullScreenExclusiveModeEXT\NUL"#

{-# INLINE is_VkAcquireFullScreenExclusiveModeEXT #-}

is_VkAcquireFullScreenExclusiveModeEXT :: CString -> Bool
is_VkAcquireFullScreenExclusiveModeEXT
  = (EQ ==) . cmpCStrings _VkAcquireFullScreenExclusiveModeEXT

type VkAcquireFullScreenExclusiveModeEXT =
     "vkAcquireFullScreenExclusiveModeEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkAcquireFullScreenExclusiveModeEXT
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkAcquireFullScreenExclusiveModeEXT vkAcquireFullScreenExclusiveModeEXT registry at www.khronos.org>
type HS_vkAcquireFullScreenExclusiveModeEXT =
     VkDevice -- ^ device
              -> VkSwapchainKHR -- ^ swapchain
                                -> IO VkResult

type PFN_vkAcquireFullScreenExclusiveModeEXT =
     FunPtr HS_vkAcquireFullScreenExclusiveModeEXT

foreign import ccall unsafe "dynamic"
               unwrapVkAcquireFullScreenExclusiveModeEXTUnsafe ::
               PFN_vkAcquireFullScreenExclusiveModeEXT ->
                 HS_vkAcquireFullScreenExclusiveModeEXT

foreign import ccall safe "dynamic"
               unwrapVkAcquireFullScreenExclusiveModeEXTSafe ::
               PFN_vkAcquireFullScreenExclusiveModeEXT ->
                 HS_vkAcquireFullScreenExclusiveModeEXT

instance VulkanProc "vkAcquireFullScreenExclusiveModeEXT" where
    type VkProcType "vkAcquireFullScreenExclusiveModeEXT" =
         HS_vkAcquireFullScreenExclusiveModeEXT
    vkProcSymbol = _VkAcquireFullScreenExclusiveModeEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkAcquireFullScreenExclusiveModeEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkAcquireFullScreenExclusiveModeEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkReleaseFullScreenExclusiveModeEXT :: CString

pattern VkReleaseFullScreenExclusiveModeEXT <-
        (is_VkReleaseFullScreenExclusiveModeEXT -> True)
  where
    VkReleaseFullScreenExclusiveModeEXT
      = _VkReleaseFullScreenExclusiveModeEXT

{-# INLINE _VkReleaseFullScreenExclusiveModeEXT #-}

_VkReleaseFullScreenExclusiveModeEXT :: CString
_VkReleaseFullScreenExclusiveModeEXT
  = Ptr "vkReleaseFullScreenExclusiveModeEXT\NUL"#

{-# INLINE is_VkReleaseFullScreenExclusiveModeEXT #-}

is_VkReleaseFullScreenExclusiveModeEXT :: CString -> Bool
is_VkReleaseFullScreenExclusiveModeEXT
  = (EQ ==) . cmpCStrings _VkReleaseFullScreenExclusiveModeEXT

type VkReleaseFullScreenExclusiveModeEXT =
     "vkReleaseFullScreenExclusiveModeEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkReleaseFullScreenExclusiveModeEXT
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkReleaseFullScreenExclusiveModeEXT vkReleaseFullScreenExclusiveModeEXT registry at www.khronos.org>
type HS_vkReleaseFullScreenExclusiveModeEXT =
     VkDevice -- ^ device
              -> VkSwapchainKHR -- ^ swapchain
                                -> IO VkResult

type PFN_vkReleaseFullScreenExclusiveModeEXT =
     FunPtr HS_vkReleaseFullScreenExclusiveModeEXT

foreign import ccall unsafe "dynamic"
               unwrapVkReleaseFullScreenExclusiveModeEXTUnsafe ::
               PFN_vkReleaseFullScreenExclusiveModeEXT ->
                 HS_vkReleaseFullScreenExclusiveModeEXT

foreign import ccall safe "dynamic"
               unwrapVkReleaseFullScreenExclusiveModeEXTSafe ::
               PFN_vkReleaseFullScreenExclusiveModeEXT ->
                 HS_vkReleaseFullScreenExclusiveModeEXT

instance VulkanProc "vkReleaseFullScreenExclusiveModeEXT" where
    type VkProcType "vkReleaseFullScreenExclusiveModeEXT" =
         HS_vkReleaseFullScreenExclusiveModeEXT
    vkProcSymbol = _VkReleaseFullScreenExclusiveModeEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkReleaseFullScreenExclusiveModeEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkReleaseFullScreenExclusiveModeEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION = 4

type VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION = 4

pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME :: CString

pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME <-
        (is_VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME -> True)
  where
    VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
      = _VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME

{-# INLINE _VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME #-}

_VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME :: CString
_VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
  = Ptr "VK_EXT_full_screen_exclusive\NUL"#

{-# INLINE is_VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME #-}

is_VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME

type VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME =
     "VK_EXT_full_screen_exclusive"

pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT =
        VkStructureType 1000255000

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
        = VkStructureType 1000255002

pattern VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT :: VkResult

pattern VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT =
        VkResult (-1000255000)

pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
        = VkStructureType 1000255001

pattern VkGetDeviceGroupSurfacePresentModes2EXT :: CString

pattern VkGetDeviceGroupSurfacePresentModes2EXT <-
        (is_VkGetDeviceGroupSurfacePresentModes2EXT -> True)
  where
    VkGetDeviceGroupSurfacePresentModes2EXT
      = _VkGetDeviceGroupSurfacePresentModes2EXT

{-# INLINE _VkGetDeviceGroupSurfacePresentModes2EXT #-}

_VkGetDeviceGroupSurfacePresentModes2EXT :: CString
_VkGetDeviceGroupSurfacePresentModes2EXT
  = Ptr "vkGetDeviceGroupSurfacePresentModes2EXT\NUL"#

{-# INLINE is_VkGetDeviceGroupSurfacePresentModes2EXT #-}

is_VkGetDeviceGroupSurfacePresentModes2EXT :: CString -> Bool
is_VkGetDeviceGroupSurfacePresentModes2EXT
  = (EQ ==) . cmpCStrings _VkGetDeviceGroupSurfacePresentModes2EXT

type VkGetDeviceGroupSurfacePresentModes2EXT =
     "vkGetDeviceGroupSurfacePresentModes2EXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetDeviceGroupSurfacePresentModes2EXT
--   >     ( VkDevice device
--   >     , const VkPhysicalDeviceSurfaceInfo2KHR* pSurfaceInfo
--   >     , VkDeviceGroupPresentModeFlagsKHR* pModes
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetDeviceGroupSurfacePresentModes2EXT vkGetDeviceGroupSurfacePresentModes2EXT registry at www.khronos.org>
type HS_vkGetDeviceGroupSurfacePresentModes2EXT =
     VkDevice -- ^ device
              ->
       Ptr VkPhysicalDeviceSurfaceInfo2KHR -- ^ pSurfaceInfo
                                           ->
         Ptr VkDeviceGroupPresentModeFlagsKHR -- ^ pModes
                                              -> IO VkResult

type PFN_vkGetDeviceGroupSurfacePresentModes2EXT =
     FunPtr HS_vkGetDeviceGroupSurfacePresentModes2EXT

foreign import ccall unsafe "dynamic"
               unwrapVkGetDeviceGroupSurfacePresentModes2EXTUnsafe ::
               PFN_vkGetDeviceGroupSurfacePresentModes2EXT ->
                 HS_vkGetDeviceGroupSurfacePresentModes2EXT

foreign import ccall safe "dynamic"
               unwrapVkGetDeviceGroupSurfacePresentModes2EXTSafe ::
               PFN_vkGetDeviceGroupSurfacePresentModes2EXT ->
                 HS_vkGetDeviceGroupSurfacePresentModes2EXT

instance VulkanProc "vkGetDeviceGroupSurfacePresentModes2EXT" where
    type VkProcType "vkGetDeviceGroupSurfacePresentModes2EXT" =
         HS_vkGetDeviceGroupSurfacePresentModes2EXT
    vkProcSymbol = _VkGetDeviceGroupSurfacePresentModes2EXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetDeviceGroupSurfacePresentModes2EXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetDeviceGroupSurfacePresentModes2EXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetDeviceGroupSurfacePresentModes2EXT :: CString

pattern VkGetDeviceGroupSurfacePresentModes2EXT <-
        (is_VkGetDeviceGroupSurfacePresentModes2EXT -> True)
  where
    VkGetDeviceGroupSurfacePresentModes2EXT
      = _VkGetDeviceGroupSurfacePresentModes2EXT

{-# INLINE _VkGetDeviceGroupSurfacePresentModes2EXT #-}

_VkGetDeviceGroupSurfacePresentModes2EXT :: CString
_VkGetDeviceGroupSurfacePresentModes2EXT
  = Ptr "vkGetDeviceGroupSurfacePresentModes2EXT\NUL"#

{-# INLINE is_VkGetDeviceGroupSurfacePresentModes2EXT #-}

is_VkGetDeviceGroupSurfacePresentModes2EXT :: CString -> Bool
is_VkGetDeviceGroupSurfacePresentModes2EXT
  = (EQ ==) . cmpCStrings _VkGetDeviceGroupSurfacePresentModes2EXT

type VkGetDeviceGroupSurfacePresentModes2EXT =
     "vkGetDeviceGroupSurfacePresentModes2EXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetDeviceGroupSurfacePresentModes2EXT
--   >     ( VkDevice device
--   >     , const VkPhysicalDeviceSurfaceInfo2KHR* pSurfaceInfo
--   >     , VkDeviceGroupPresentModeFlagsKHR* pModes
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetDeviceGroupSurfacePresentModes2EXT vkGetDeviceGroupSurfacePresentModes2EXT registry at www.khronos.org>
type HS_vkGetDeviceGroupSurfacePresentModes2EXT =
     VkDevice -- ^ device
              ->
       Ptr VkPhysicalDeviceSurfaceInfo2KHR -- ^ pSurfaceInfo
                                           ->
         Ptr VkDeviceGroupPresentModeFlagsKHR -- ^ pModes
                                              -> IO VkResult

type PFN_vkGetDeviceGroupSurfacePresentModes2EXT =
     FunPtr HS_vkGetDeviceGroupSurfacePresentModes2EXT

foreign import ccall unsafe "dynamic"
               unwrapVkGetDeviceGroupSurfacePresentModes2EXTUnsafe ::
               PFN_vkGetDeviceGroupSurfacePresentModes2EXT ->
                 HS_vkGetDeviceGroupSurfacePresentModes2EXT

foreign import ccall safe "dynamic"
               unwrapVkGetDeviceGroupSurfacePresentModes2EXTSafe ::
               PFN_vkGetDeviceGroupSurfacePresentModes2EXT ->
                 HS_vkGetDeviceGroupSurfacePresentModes2EXT

instance VulkanProc "vkGetDeviceGroupSurfacePresentModes2EXT" where
    type VkProcType "vkGetDeviceGroupSurfacePresentModes2EXT" =
         HS_vkGetDeviceGroupSurfacePresentModes2EXT
    vkProcSymbol = _VkGetDeviceGroupSurfacePresentModes2EXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetDeviceGroupSurfacePresentModes2EXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetDeviceGroupSurfacePresentModes2EXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}
