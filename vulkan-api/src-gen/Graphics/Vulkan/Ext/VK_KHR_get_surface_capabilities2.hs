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
module Graphics.Vulkan.Ext.VK_KHR_get_surface_capabilities2
       (-- * Vulkan extension: @VK_KHR_get_surface_capabilities2@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @120@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR,
        VkColorComponentBitmask(..), VkColorSpaceKHR(..),
        VkColorComponentFlagBits(), VkColorComponentFlags(),
        VkCompositeAlphaBitmaskKHR(..), VkCompositeAlphaFlagBitsKHR(),
        VkCompositeAlphaFlagsKHR(), VkExtent2D, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..), VkFormat(..),
        VkFormatFeatureBitmask(..), VkFormatFeatureFlagBits(),
        VkFormatFeatureFlags(), VkImageAspectBitmask(..),
        VkImageCreateBitmask(..), VkImageLayout(..), VkImageTiling(..),
        VkImageType(..), VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(),
        VkImageViewCreateBitmask(..), VkImageViewCreateFlagBits(),
        VkImageViewCreateFlags(), VkPhysicalDeviceSurfaceInfo2KHR,
        VkStructureType(..), VkSurfaceCapabilities2KHR,
        VkSurfaceCapabilitiesKHR, VkSurfaceFormat2KHR, VkSurfaceFormatKHR,
        VkSurfaceCounterBitmaskEXT(..), VkSurfaceTransformBitmaskKHR(..),
        VkSurfaceCounterFlagBitsEXT(), VkSurfaceCounterFlagsEXT(),
        VkSurfaceTransformFlagBitsKHR(), VkSurfaceTransformFlagsKHR(),
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceSurfaceCapabilities2KHR,
        pattern VkGetPhysicalDeviceSurfaceCapabilities2KHR,
        HS_vkGetPhysicalDeviceSurfaceCapabilities2KHR,
        PFN_vkGetPhysicalDeviceSurfaceCapabilities2KHR,
        VkGetPhysicalDeviceSurfaceFormats2KHR,
        pattern VkGetPhysicalDeviceSurfaceFormats2KHR,
        HS_vkGetPhysicalDeviceSurfaceFormats2KHR,
        PFN_vkGetPhysicalDeviceSurfaceFormats2KHR,
        module Graphics.Vulkan.Marshal, VkResult(..),
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
        VkValidationCacheEXT, VkValidationCacheEXT_T(), VkExtent3D,
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
        VkSurfaceCapabilities2EXT, VkSurfaceProtectedCapabilitiesKHR,
        VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION,
        pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION,
        VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME,
        pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR,
        pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR)
       where
import GHC.Ptr                                           (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                      (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Enum.Color
import Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR
import Graphics.Vulkan.Types.Enum.Format
import Graphics.Vulkan.Types.Enum.Image
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Enum.Surface
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.Extent
import Graphics.Vulkan.Types.Struct.PhysicalDevice
import Graphics.Vulkan.Types.Struct.Surface

pattern VkGetPhysicalDeviceSurfaceCapabilities2KHR :: CString

pattern VkGetPhysicalDeviceSurfaceCapabilities2KHR <-
        (is_VkGetPhysicalDeviceSurfaceCapabilities2KHR -> True)
  where
    VkGetPhysicalDeviceSurfaceCapabilities2KHR
      = _VkGetPhysicalDeviceSurfaceCapabilities2KHR

{-# INLINE _VkGetPhysicalDeviceSurfaceCapabilities2KHR #-}

_VkGetPhysicalDeviceSurfaceCapabilities2KHR :: CString
_VkGetPhysicalDeviceSurfaceCapabilities2KHR
  = Ptr "vkGetPhysicalDeviceSurfaceCapabilities2KHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceSurfaceCapabilities2KHR #-}

is_VkGetPhysicalDeviceSurfaceCapabilities2KHR :: CString -> Bool
is_VkGetPhysicalDeviceSurfaceCapabilities2KHR
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceSurfaceCapabilities2KHR

type VkGetPhysicalDeviceSurfaceCapabilities2KHR =
     "vkGetPhysicalDeviceSurfaceCapabilities2KHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceCapabilities2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSurfaceInfo2KHR* pSurfaceInfo
--   >     , VkSurfaceCapabilities2KHR* pSurfaceCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfaceCapabilities2KHR vkGetPhysicalDeviceSurfaceCapabilities2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceSurfaceCapabilities2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkPhysicalDeviceSurfaceInfo2KHR -- ^ pSurfaceInfo
                                           ->
         Ptr VkSurfaceCapabilities2KHR -- ^ pSurfaceCapabilities
                                       -> IO VkResult

type PFN_vkGetPhysicalDeviceSurfaceCapabilities2KHR =
     FunPtr HS_vkGetPhysicalDeviceSurfaceCapabilities2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceCapabilities2KHRUnsafe ::
               PFN_vkGetPhysicalDeviceSurfaceCapabilities2KHR ->
                 HS_vkGetPhysicalDeviceSurfaceCapabilities2KHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceCapabilities2KHRSafe ::
               PFN_vkGetPhysicalDeviceSurfaceCapabilities2KHR ->
                 HS_vkGetPhysicalDeviceSurfaceCapabilities2KHR

instance VulkanProc "vkGetPhysicalDeviceSurfaceCapabilities2KHR"
         where
    type VkProcType "vkGetPhysicalDeviceSurfaceCapabilities2KHR" =
         HS_vkGetPhysicalDeviceSurfaceCapabilities2KHR
    vkProcSymbol = _VkGetPhysicalDeviceSurfaceCapabilities2KHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceSurfaceCapabilities2KHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceSurfaceCapabilities2KHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceSurfaceFormats2KHR :: CString

pattern VkGetPhysicalDeviceSurfaceFormats2KHR <-
        (is_VkGetPhysicalDeviceSurfaceFormats2KHR -> True)
  where
    VkGetPhysicalDeviceSurfaceFormats2KHR
      = _VkGetPhysicalDeviceSurfaceFormats2KHR

{-# INLINE _VkGetPhysicalDeviceSurfaceFormats2KHR #-}

_VkGetPhysicalDeviceSurfaceFormats2KHR :: CString
_VkGetPhysicalDeviceSurfaceFormats2KHR
  = Ptr "vkGetPhysicalDeviceSurfaceFormats2KHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceSurfaceFormats2KHR #-}

is_VkGetPhysicalDeviceSurfaceFormats2KHR :: CString -> Bool
is_VkGetPhysicalDeviceSurfaceFormats2KHR
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceSurfaceFormats2KHR

type VkGetPhysicalDeviceSurfaceFormats2KHR =
     "vkGetPhysicalDeviceSurfaceFormats2KHR"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceFormats2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSurfaceInfo2KHR* pSurfaceInfo
--   >     , uint32_t* pSurfaceFormatCount
--   >     , VkSurfaceFormat2KHR* pSurfaceFormats
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfaceFormats2KHR vkGetPhysicalDeviceSurfaceFormats2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceSurfaceFormats2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkPhysicalDeviceSurfaceInfo2KHR -- ^ pSurfaceInfo
                                           ->
         Ptr Word32 -- ^ pSurfaceFormatCount
                    -> Ptr VkSurfaceFormat2KHR -- ^ pSurfaceFormats
                                               -> IO VkResult

type PFN_vkGetPhysicalDeviceSurfaceFormats2KHR =
     FunPtr HS_vkGetPhysicalDeviceSurfaceFormats2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceFormats2KHRUnsafe ::
               PFN_vkGetPhysicalDeviceSurfaceFormats2KHR ->
                 HS_vkGetPhysicalDeviceSurfaceFormats2KHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceFormats2KHRSafe ::
               PFN_vkGetPhysicalDeviceSurfaceFormats2KHR ->
                 HS_vkGetPhysicalDeviceSurfaceFormats2KHR

instance VulkanProc "vkGetPhysicalDeviceSurfaceFormats2KHR" where
    type VkProcType "vkGetPhysicalDeviceSurfaceFormats2KHR" =
         HS_vkGetPhysicalDeviceSurfaceFormats2KHR
    vkProcSymbol = _VkGetPhysicalDeviceSurfaceFormats2KHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceSurfaceFormats2KHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceSurfaceFormats2KHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION = 1

type VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION = 1

pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME :: CString

pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME <-
        (is_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME -> True)
  where
    VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
      = _VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME

{-# INLINE _VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME #-}

_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME :: CString
_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
  = Ptr "VK_KHR_get_surface_capabilities2\NUL"#

{-# INLINE is_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME #-}

is_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME ::
                                                    CString -> Bool
is_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME

type VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME =
     "VK_KHR_get_surface_capabilities2"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR =
        VkStructureType 1000119000

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR =
        VkStructureType 1000119001

pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR :: VkStructureType

pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR =
        VkStructureType 1000119002
