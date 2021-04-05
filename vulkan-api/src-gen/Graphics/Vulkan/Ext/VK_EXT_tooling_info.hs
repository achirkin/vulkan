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
module Graphics.Vulkan.Ext.VK_EXT_tooling_info
       (-- * Vulkan extension: @VK_EXT_tooling_info@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Tobias Hector @tobski@
        --
        -- author: @EXT@
        --
        -- type: @device@
        --
        -- Extension number: @246@
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkPhysicalDeviceToolPropertiesEXT, VkStructureType(..),
        VkToolPurposeBitmaskEXT(..), VkToolPurposeFlagBitsEXT(),
        VkToolPurposeFlagsEXT(), -- > #include "vk_platform.h"
                                 VkGetPhysicalDeviceToolPropertiesEXT,
        pattern VkGetPhysicalDeviceToolPropertiesEXT,
        HS_vkGetPhysicalDeviceToolPropertiesEXT,
        PFN_vkGetPhysicalDeviceToolPropertiesEXT, VkResult(..),
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
        VkPhysicalDeviceSurfaceInfo2KHR,
        VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT,
        VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT,
        VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT,
        VkPhysicalDeviceTimelineSemaphoreFeatures,
        VkPhysicalDeviceTimelineSemaphoreFeaturesKHR,
        VkPhysicalDeviceTimelineSemaphoreProperties,
        VkPhysicalDeviceTimelineSemaphorePropertiesKHR,
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
        VK_EXT_TOOLING_INFO_SPEC_VERSION,
        pattern VK_EXT_TOOLING_INFO_SPEC_VERSION,
        VK_EXT_TOOLING_INFO_EXTENSION_NAME,
        pattern VK_EXT_TOOLING_INFO_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT,
        -- ** Required extensions: 'VK_EXT_debug_report'.
        pattern VK_TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT,
        -- ** Required extensions: 'VK_EXT_debug_marker'.
        pattern VK_TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT)
       where
import GHC.Ptr                                        (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                   (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Enum.ToolPurposeFlagsEXT
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.PhysicalDevice

pattern VkGetPhysicalDeviceToolPropertiesEXT :: CString

pattern VkGetPhysicalDeviceToolPropertiesEXT <-
        (is_VkGetPhysicalDeviceToolPropertiesEXT -> True)
  where
    VkGetPhysicalDeviceToolPropertiesEXT
      = _VkGetPhysicalDeviceToolPropertiesEXT

{-# INLINE _VkGetPhysicalDeviceToolPropertiesEXT #-}

_VkGetPhysicalDeviceToolPropertiesEXT :: CString
_VkGetPhysicalDeviceToolPropertiesEXT
  = Ptr "vkGetPhysicalDeviceToolPropertiesEXT\NUL"#

{-# INLINE is_VkGetPhysicalDeviceToolPropertiesEXT #-}

is_VkGetPhysicalDeviceToolPropertiesEXT :: CString -> Bool
is_VkGetPhysicalDeviceToolPropertiesEXT
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceToolPropertiesEXT

type VkGetPhysicalDeviceToolPropertiesEXT =
     "vkGetPhysicalDeviceToolPropertiesEXT"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceToolPropertiesEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pToolCount
--   >     , VkPhysicalDeviceToolPropertiesEXT* pToolProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetPhysicalDeviceToolPropertiesEXT vkGetPhysicalDeviceToolPropertiesEXT registry at www.khronos.org>
type HS_vkGetPhysicalDeviceToolPropertiesEXT =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr Word32 -- ^ pToolCount
                  -> Ptr VkPhysicalDeviceToolPropertiesEXT -- ^ pToolProperties
                                                           -> IO VkResult

type PFN_vkGetPhysicalDeviceToolPropertiesEXT =
     FunPtr HS_vkGetPhysicalDeviceToolPropertiesEXT

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceToolPropertiesEXTUnsafe ::
               PFN_vkGetPhysicalDeviceToolPropertiesEXT ->
                 HS_vkGetPhysicalDeviceToolPropertiesEXT

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceToolPropertiesEXTSafe ::
               PFN_vkGetPhysicalDeviceToolPropertiesEXT ->
                 HS_vkGetPhysicalDeviceToolPropertiesEXT

instance VulkanProc "vkGetPhysicalDeviceToolPropertiesEXT" where
    type VkProcType "vkGetPhysicalDeviceToolPropertiesEXT" =
         HS_vkGetPhysicalDeviceToolPropertiesEXT
    vkProcSymbol = _VkGetPhysicalDeviceToolPropertiesEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceToolPropertiesEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceToolPropertiesEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_TOOLING_INFO_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_TOOLING_INFO_SPEC_VERSION = 1

type VK_EXT_TOOLING_INFO_SPEC_VERSION = 1

pattern VK_EXT_TOOLING_INFO_EXTENSION_NAME :: CString

pattern VK_EXT_TOOLING_INFO_EXTENSION_NAME <-
        (is_VK_EXT_TOOLING_INFO_EXTENSION_NAME -> True)
  where
    VK_EXT_TOOLING_INFO_EXTENSION_NAME
      = _VK_EXT_TOOLING_INFO_EXTENSION_NAME

{-# INLINE _VK_EXT_TOOLING_INFO_EXTENSION_NAME #-}

_VK_EXT_TOOLING_INFO_EXTENSION_NAME :: CString
_VK_EXT_TOOLING_INFO_EXTENSION_NAME
  = Ptr "VK_EXT_tooling_info\NUL"#

{-# INLINE is_VK_EXT_TOOLING_INFO_EXTENSION_NAME #-}

is_VK_EXT_TOOLING_INFO_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_TOOLING_INFO_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_TOOLING_INFO_EXTENSION_NAME

type VK_EXT_TOOLING_INFO_EXTENSION_NAME = "VK_EXT_tooling_info"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT =
        VkStructureType 1000245000

-- | bitpos = @5@
pattern VK_TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT ::
        VkToolPurposeBitmaskEXT a

pattern VK_TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT =
        VkToolPurposeBitmaskEXT 32

-- | bitpos = @6@
pattern VK_TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT ::
        VkToolPurposeBitmaskEXT a

pattern VK_TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT =
        VkToolPurposeBitmaskEXT 64
