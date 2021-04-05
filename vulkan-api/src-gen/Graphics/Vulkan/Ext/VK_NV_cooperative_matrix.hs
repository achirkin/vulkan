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
module Graphics.Vulkan.Ext.VK_NV_cooperative_matrix
       (-- * Vulkan extension: @VK_NV_cooperative_matrix@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jeffbolznv@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @250@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkComponentSwizzle(..), VkComponentTypeNV(..),
        VkCooperativeMatrixPropertiesNV,
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
        VkDeviceQueueCreateInfo,
        VkPhysicalDeviceCooperativeMatrixFeaturesNV,
        VkPhysicalDeviceCooperativeMatrixPropertiesNV,
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        VkPhysicalDeviceLimits, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2, VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceType(..), VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(), VkScopeNV(..),
        VkShaderFloatControlsIndependence(..), VkShaderInfoTypeAMD(..),
        VkShaderStageBitmask(..), VkShaderCorePropertiesBitmaskAMD(..),
        VkShaderCorePropertiesFlagBitsAMD(),
        VkShaderCorePropertiesFlagsAMD(),
        VkShaderFloatControlsIndependenceKHR(..),
        VkShaderModuleCreateBitmask(..), VkShaderModuleCreateFlagBits(),
        VkShaderModuleCreateFlags(), VkShaderStageFlagBits(),
        VkShaderStageFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceCooperativeMatrixPropertiesNV,
        pattern VkGetPhysicalDeviceCooperativeMatrixPropertiesNV,
        HS_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV,
        PFN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV, VkResult(..),
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
        VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION,
        pattern VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION,
        VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME,
        pattern VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV,
        pattern VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV)
       where
import GHC.Ptr                                                    (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                               (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Component
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.ScopeNV
import Graphics.Vulkan.Types.Enum.Shader
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.CooperativeMatrixPropertiesNV
import Graphics.Vulkan.Types.Struct.Device                        (VkDeviceCreateInfo,
                                                                   VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.PhysicalDevice                (VkPhysicalDeviceCooperativeMatrixFeaturesNV,
                                                                   VkPhysicalDeviceCooperativeMatrixPropertiesNV,
                                                                   VkPhysicalDeviceFeatures2,
                                                                   VkPhysicalDeviceLimits,
                                                                   VkPhysicalDeviceProperties,
                                                                   VkPhysicalDeviceProperties2,
                                                                   VkPhysicalDeviceSparseProperties)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures        (VkPhysicalDeviceFeatures)

pattern VkGetPhysicalDeviceCooperativeMatrixPropertiesNV :: CString

pattern VkGetPhysicalDeviceCooperativeMatrixPropertiesNV <-
        (is_VkGetPhysicalDeviceCooperativeMatrixPropertiesNV -> True)
  where
    VkGetPhysicalDeviceCooperativeMatrixPropertiesNV
      = _VkGetPhysicalDeviceCooperativeMatrixPropertiesNV

{-# INLINE _VkGetPhysicalDeviceCooperativeMatrixPropertiesNV #-}

_VkGetPhysicalDeviceCooperativeMatrixPropertiesNV :: CString
_VkGetPhysicalDeviceCooperativeMatrixPropertiesNV
  = Ptr "vkGetPhysicalDeviceCooperativeMatrixPropertiesNV\NUL"#

{-# INLINE is_VkGetPhysicalDeviceCooperativeMatrixPropertiesNV #-}

is_VkGetPhysicalDeviceCooperativeMatrixPropertiesNV ::
                                                    CString -> Bool
is_VkGetPhysicalDeviceCooperativeMatrixPropertiesNV
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceCooperativeMatrixPropertiesNV

type VkGetPhysicalDeviceCooperativeMatrixPropertiesNV =
     "vkGetPhysicalDeviceCooperativeMatrixPropertiesNV"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceCooperativeMatrixPropertiesNV
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkCooperativeMatrixPropertiesNV* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetPhysicalDeviceCooperativeMatrixPropertiesNV vkGetPhysicalDeviceCooperativeMatrixPropertiesNV registry at www.khronos.org>
type HS_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr Word32 -- ^ pPropertyCount
                  -> Ptr VkCooperativeMatrixPropertiesNV -- ^ pProperties
                                                         -> IO VkResult

type PFN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV =
     FunPtr HS_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceCooperativeMatrixPropertiesNVUnsafe ::
               PFN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV ->
                 HS_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceCooperativeMatrixPropertiesNVSafe ::
               PFN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV ->
                 HS_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV

instance VulkanProc
           "vkGetPhysicalDeviceCooperativeMatrixPropertiesNV"
         where
    type VkProcType "vkGetPhysicalDeviceCooperativeMatrixPropertiesNV"
         = HS_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV
    vkProcSymbol = _VkGetPhysicalDeviceCooperativeMatrixPropertiesNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceCooperativeMatrixPropertiesNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceCooperativeMatrixPropertiesNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION = 1

type VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION = 1

pattern VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME :: CString

pattern VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME <-
        (is_VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME -> True)
  where
    VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME
      = _VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME

{-# INLINE _VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME #-}

_VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME :: CString
_VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME
  = Ptr "VK_NV_cooperative_matrix\NUL"#

{-# INLINE is_VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME #-}

is_VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME :: CString -> Bool
is_VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME

type VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME =
     "VK_NV_cooperative_matrix"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV
        = VkStructureType 1000249000

pattern VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV =
        VkStructureType 1000249001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV
        = VkStructureType 1000249002
