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
module Graphics.Vulkan.Ext.VK_NV_shading_rate_image
       (-- * Vulkan extension: @VK_NV_shading_rate_image@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Pat Brown @nvpbrown@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @165@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkCoarseSampleLocationNV, VkCoarseSampleOrderCustomNV,
        VkCoarseSampleOrderTypeNV(..), VkAndroidSurfaceCreateFlagsKHR(..),
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
        VkDeviceQueueCreateInfo, VkExtent2D, VkOffset2D,
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        VkPhysicalDeviceLimits, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2,
        VkPhysicalDeviceShadingRateImageFeaturesNV,
        VkPhysicalDeviceShadingRateImagePropertiesNV,
        VkPhysicalDeviceSparseProperties, VkPhysicalDeviceType(..),
        VkPipelineViewportCoarseSampleOrderStateCreateInfoNV,
        VkPipelineViewportShadingRateImageStateCreateInfoNV,
        VkPipelineViewportStateCreateInfo, VkRect2D,
        VkSampleCountBitmask(..), VkSampleCountFlagBits(),
        VkSampleCountFlags(), VkShadingRatePaletteEntryNV(..),
        VkShadingRatePaletteNV, VkStructureType(..), VkViewport,
        -- > #include "vk_platform.h"
        VkCmdBindShadingRateImageNV, pattern VkCmdBindShadingRateImageNV,
        HS_vkCmdBindShadingRateImageNV, PFN_vkCmdBindShadingRateImageNV,
        VkCmdSetViewportShadingRatePaletteNV,
        pattern VkCmdSetViewportShadingRatePaletteNV,
        HS_vkCmdSetViewportShadingRatePaletteNV,
        PFN_vkCmdSetViewportShadingRatePaletteNV,
        VkCmdSetCoarseSampleOrderNV, pattern VkCmdSetCoarseSampleOrderNV,
        HS_vkCmdSetCoarseSampleOrderNV, PFN_vkCmdSetCoarseSampleOrderNV,
        VkImageAspectBitmask(..), VkImageCreateBitmask(..),
        VkImageLayout(..), VkImageTiling(..), VkImageType(..),
        VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(),
        VkImageViewCreateBitmask(..), VkImageViewCreateFlagBits(),
        VkImageViewCreateFlags(), VkAccelerationStructureKHR,
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
        VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION,
        pattern VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION,
        VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME,
        pattern VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV,
        pattern VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV,
        pattern VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV,
        pattern VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV,
        pattern VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV,
        pattern VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV,
        pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV,
        pattern VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV)
       where
import GHC.Ptr                                              (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                         (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.AccessFlags               (VkAccessBitmask (..))
import Graphics.Vulkan.Types.Enum.CoarseSampleOrderTypeNV
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.DynamicState              (VkDynamicState (..))
import Graphics.Vulkan.Types.Enum.Image
import Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import Graphics.Vulkan.Types.Enum.Pipeline                  (VkPipelineStageBitmask (..))
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.ShadingRatePaletteEntryNV
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.CoarseSample
import Graphics.Vulkan.Types.Struct.Device                  (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.Extent                  (VkExtent2D)
import Graphics.Vulkan.Types.Struct.Offset                  (VkOffset2D)
import Graphics.Vulkan.Types.Struct.PhysicalDevice          (VkPhysicalDeviceFeatures2,
                                                             VkPhysicalDeviceLimits,
                                                             VkPhysicalDeviceProperties,
                                                             VkPhysicalDeviceProperties2,
                                                             VkPhysicalDeviceShadingRateImageFeaturesNV,
                                                             VkPhysicalDeviceShadingRateImagePropertiesNV,
                                                             VkPhysicalDeviceSparseProperties)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures  (VkPhysicalDeviceFeatures)
import Graphics.Vulkan.Types.Struct.Pipeline                (VkPipelineViewportCoarseSampleOrderStateCreateInfoNV,
                                                             VkPipelineViewportShadingRateImageStateCreateInfoNV,
                                                             VkPipelineViewportStateCreateInfo)
import Graphics.Vulkan.Types.Struct.Rect                    (VkRect2D)
import Graphics.Vulkan.Types.Struct.ShadingRatePaletteNV
import Graphics.Vulkan.Types.Struct.Viewport                (VkViewport)

pattern VkCmdBindShadingRateImageNV :: CString

pattern VkCmdBindShadingRateImageNV <-
        (is_VkCmdBindShadingRateImageNV -> True)
  where
    VkCmdBindShadingRateImageNV = _VkCmdBindShadingRateImageNV

{-# INLINE _VkCmdBindShadingRateImageNV #-}

_VkCmdBindShadingRateImageNV :: CString
_VkCmdBindShadingRateImageNV
  = Ptr "vkCmdBindShadingRateImageNV\NUL"#

{-# INLINE is_VkCmdBindShadingRateImageNV #-}

is_VkCmdBindShadingRateImageNV :: CString -> Bool
is_VkCmdBindShadingRateImageNV
  = (EQ ==) . cmpCStrings _VkCmdBindShadingRateImageNV

type VkCmdBindShadingRateImageNV = "vkCmdBindShadingRateImageNV"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdBindShadingRateImageNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImageView imageView
--   >     , VkImageLayout imageLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBindShadingRateImageNV vkCmdBindShadingRateImageNV registry at www.khronos.org>
type HS_vkCmdBindShadingRateImageNV =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkImageView -- ^ imageView
                                    -> VkImageLayout -- ^ imageLayout
                                                     -> IO ()

type PFN_vkCmdBindShadingRateImageNV =
     FunPtr HS_vkCmdBindShadingRateImageNV

foreign import ccall unsafe "dynamic"
               unwrapVkCmdBindShadingRateImageNVUnsafe ::
               PFN_vkCmdBindShadingRateImageNV -> HS_vkCmdBindShadingRateImageNV

foreign import ccall safe "dynamic"
               unwrapVkCmdBindShadingRateImageNVSafe ::
               PFN_vkCmdBindShadingRateImageNV -> HS_vkCmdBindShadingRateImageNV

instance VulkanProc "vkCmdBindShadingRateImageNV" where
    type VkProcType "vkCmdBindShadingRateImageNV" =
         HS_vkCmdBindShadingRateImageNV
    vkProcSymbol = _VkCmdBindShadingRateImageNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdBindShadingRateImageNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdBindShadingRateImageNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetViewportShadingRatePaletteNV :: CString

pattern VkCmdSetViewportShadingRatePaletteNV <-
        (is_VkCmdSetViewportShadingRatePaletteNV -> True)
  where
    VkCmdSetViewportShadingRatePaletteNV
      = _VkCmdSetViewportShadingRatePaletteNV

{-# INLINE _VkCmdSetViewportShadingRatePaletteNV #-}

_VkCmdSetViewportShadingRatePaletteNV :: CString
_VkCmdSetViewportShadingRatePaletteNV
  = Ptr "vkCmdSetViewportShadingRatePaletteNV\NUL"#

{-# INLINE is_VkCmdSetViewportShadingRatePaletteNV #-}

is_VkCmdSetViewportShadingRatePaletteNV :: CString -> Bool
is_VkCmdSetViewportShadingRatePaletteNV
  = (EQ ==) . cmpCStrings _VkCmdSetViewportShadingRatePaletteNV

type VkCmdSetViewportShadingRatePaletteNV =
     "vkCmdSetViewportShadingRatePaletteNV"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetViewportShadingRatePaletteNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstViewport
--   >     , uint32_t viewportCount
--   >     , const VkShadingRatePaletteNV* pShadingRatePalettes
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetViewportShadingRatePaletteNV vkCmdSetViewportShadingRatePaletteNV registry at www.khronos.org>
type HS_vkCmdSetViewportShadingRatePaletteNV =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ firstViewport
              -> Word32 -- ^ viewportCount
                        -> Ptr VkShadingRatePaletteNV -- ^ pShadingRatePalettes
                                                      -> IO ()

type PFN_vkCmdSetViewportShadingRatePaletteNV =
     FunPtr HS_vkCmdSetViewportShadingRatePaletteNV

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetViewportShadingRatePaletteNVUnsafe ::
               PFN_vkCmdSetViewportShadingRatePaletteNV ->
                 HS_vkCmdSetViewportShadingRatePaletteNV

foreign import ccall safe "dynamic"
               unwrapVkCmdSetViewportShadingRatePaletteNVSafe ::
               PFN_vkCmdSetViewportShadingRatePaletteNV ->
                 HS_vkCmdSetViewportShadingRatePaletteNV

instance VulkanProc "vkCmdSetViewportShadingRatePaletteNV" where
    type VkProcType "vkCmdSetViewportShadingRatePaletteNV" =
         HS_vkCmdSetViewportShadingRatePaletteNV
    vkProcSymbol = _VkCmdSetViewportShadingRatePaletteNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdSetViewportShadingRatePaletteNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkCmdSetViewportShadingRatePaletteNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetCoarseSampleOrderNV :: CString

pattern VkCmdSetCoarseSampleOrderNV <-
        (is_VkCmdSetCoarseSampleOrderNV -> True)
  where
    VkCmdSetCoarseSampleOrderNV = _VkCmdSetCoarseSampleOrderNV

{-# INLINE _VkCmdSetCoarseSampleOrderNV #-}

_VkCmdSetCoarseSampleOrderNV :: CString
_VkCmdSetCoarseSampleOrderNV
  = Ptr "vkCmdSetCoarseSampleOrderNV\NUL"#

{-# INLINE is_VkCmdSetCoarseSampleOrderNV #-}

is_VkCmdSetCoarseSampleOrderNV :: CString -> Bool
is_VkCmdSetCoarseSampleOrderNV
  = (EQ ==) . cmpCStrings _VkCmdSetCoarseSampleOrderNV

type VkCmdSetCoarseSampleOrderNV = "vkCmdSetCoarseSampleOrderNV"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetCoarseSampleOrderNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkCoarseSampleOrderTypeNV sampleOrderType
--   >     , uint32_t customSampleOrderCount
--   >     , const VkCoarseSampleOrderCustomNV* pCustomSampleOrders
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetCoarseSampleOrderNV vkCmdSetCoarseSampleOrderNV registry at www.khronos.org>
type HS_vkCmdSetCoarseSampleOrderNV =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkCoarseSampleOrderTypeNV -- ^ sampleOrderType
                                 ->
         Word32 -- ^ customSampleOrderCount
                -> Ptr VkCoarseSampleOrderCustomNV -- ^ pCustomSampleOrders
                                                   -> IO ()

type PFN_vkCmdSetCoarseSampleOrderNV =
     FunPtr HS_vkCmdSetCoarseSampleOrderNV

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetCoarseSampleOrderNVUnsafe ::
               PFN_vkCmdSetCoarseSampleOrderNV -> HS_vkCmdSetCoarseSampleOrderNV

foreign import ccall safe "dynamic"
               unwrapVkCmdSetCoarseSampleOrderNVSafe ::
               PFN_vkCmdSetCoarseSampleOrderNV -> HS_vkCmdSetCoarseSampleOrderNV

instance VulkanProc "vkCmdSetCoarseSampleOrderNV" where
    type VkProcType "vkCmdSetCoarseSampleOrderNV" =
         HS_vkCmdSetCoarseSampleOrderNV
    vkProcSymbol = _VkCmdSetCoarseSampleOrderNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetCoarseSampleOrderNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetCoarseSampleOrderNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION = 3

type VK_NV_SHADING_RATE_IMAGE_SPEC_VERSION = 3

pattern VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME :: CString

pattern VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME <-
        (is_VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME -> True)
  where
    VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME
      = _VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME

{-# INLINE _VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME #-}

_VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME :: CString
_VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME
  = Ptr "VK_NV_shading_rate_image\NUL"#

{-# INLINE is_VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME #-}

is_VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME :: CString -> Bool
is_VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME

type VK_NV_SHADING_RATE_IMAGE_EXTENSION_NAME =
     "VK_NV_shading_rate_image"

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV
        = VkStructureType 1000164000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV
        = VkStructureType 1000164001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV
        = VkStructureType 1000164002

pattern VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV :: VkImageLayout

pattern VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV =
        VkImageLayout 1000164003

pattern VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV ::
        VkDynamicState

pattern VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV =
        VkDynamicState 1000164004

-- | bitpos = @23@
pattern VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV ::
        VkAccessBitmask a

pattern VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV =
        VkAccessBitmask 8388608

-- | bitpos = @8@
pattern VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV ::
        VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV =
        VkImageUsageBitmask 256

-- | bitpos = @22@
pattern VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV =
        VkPipelineStageBitmask 4194304

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV
        = VkStructureType 1000164005

pattern VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV ::
        VkDynamicState

pattern VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV =
        VkDynamicState 1000164006
