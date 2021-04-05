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
module Graphics.Vulkan.Ext.VK_EXT_line_rasterization
       (-- * Vulkan extension: @VK_EXT_line_rasterization@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jeffbolznv@
        --
        -- author: @EXT@
        --
        -- type: @device@
        --
        -- Extension number: @260@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkCullModeBitmask(..), VkCullModeFlagBits(), VkCullModeFlags(),
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
        VkDeviceQueueCreateInfo, VkFrontFace(..),
        VkLineRasterizationModeEXT(..), VkPhysicalDeviceFeatures,
        VkPhysicalDeviceFeatures2, VkPhysicalDeviceLimits,
        VkPhysicalDeviceLineRasterizationFeaturesEXT,
        VkPhysicalDeviceLineRasterizationPropertiesEXT,
        VkPhysicalDeviceProperties, VkPhysicalDeviceProperties2,
        VkPhysicalDeviceSparseProperties, VkPhysicalDeviceType(..),
        VkPipelineRasterizationLineStateCreateInfoEXT,
        VkPipelineRasterizationStateCreateInfo, VkPolygonMode(..),
        VkSampleCountBitmask(..), VkSampleCountFlagBits(),
        VkSampleCountFlags(), VkStructureType(..), -- > #include "vk_platform.h"
                                                   VkCmdSetLineStippleEXT,
        pattern VkCmdSetLineStippleEXT, HS_vkCmdSetLineStippleEXT,
        PFN_vkCmdSetLineStippleEXT, VkAccelerationStructureKHR,
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
        VK_EXT_LINE_RASTERIZATION_SPEC_VERSION,
        pattern VK_EXT_LINE_RASTERIZATION_SPEC_VERSION,
        VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME,
        pattern VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT,
        pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT,
        pattern VK_DYNAMIC_STATE_LINE_STIPPLE_EXT)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                        (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.CullModeFlags
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.DynamicState             (VkDynamicState (..))
import Graphics.Vulkan.Types.Enum.FrontFace
import Graphics.Vulkan.Types.Enum.LineRasterizationModeEXT
import Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import Graphics.Vulkan.Types.Enum.PolygonMode
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceFeatures2,
                                                            VkPhysicalDeviceLimits,
                                                            VkPhysicalDeviceLineRasterizationFeaturesEXT,
                                                            VkPhysicalDeviceLineRasterizationPropertiesEXT,
                                                            VkPhysicalDeviceProperties,
                                                            VkPhysicalDeviceProperties2,
                                                            VkPhysicalDeviceSparseProperties)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)
import Graphics.Vulkan.Types.Struct.Pipeline               (VkPipelineRasterizationLineStateCreateInfoEXT,
                                                            VkPipelineRasterizationStateCreateInfo)

pattern VkCmdSetLineStippleEXT :: CString

pattern VkCmdSetLineStippleEXT <-
        (is_VkCmdSetLineStippleEXT -> True)
  where
    VkCmdSetLineStippleEXT = _VkCmdSetLineStippleEXT

{-# INLINE _VkCmdSetLineStippleEXT #-}

_VkCmdSetLineStippleEXT :: CString
_VkCmdSetLineStippleEXT = Ptr "vkCmdSetLineStippleEXT\NUL"#

{-# INLINE is_VkCmdSetLineStippleEXT #-}

is_VkCmdSetLineStippleEXT :: CString -> Bool
is_VkCmdSetLineStippleEXT
  = (EQ ==) . cmpCStrings _VkCmdSetLineStippleEXT

type VkCmdSetLineStippleEXT = "vkCmdSetLineStippleEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetLineStippleEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t lineStippleFactor
--   >     , uint16_t lineStipplePattern
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetLineStippleEXT vkCmdSetLineStippleEXT registry at www.khronos.org>
type HS_vkCmdSetLineStippleEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> Word32 -- ^ lineStippleFactor
                               -> Word16 -- ^ lineStipplePattern
                                         -> IO ()

type PFN_vkCmdSetLineStippleEXT = FunPtr HS_vkCmdSetLineStippleEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetLineStippleEXTUnsafe ::
               PFN_vkCmdSetLineStippleEXT -> HS_vkCmdSetLineStippleEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdSetLineStippleEXTSafe ::
               PFN_vkCmdSetLineStippleEXT -> HS_vkCmdSetLineStippleEXT

instance VulkanProc "vkCmdSetLineStippleEXT" where
    type VkProcType "vkCmdSetLineStippleEXT" =
         HS_vkCmdSetLineStippleEXT
    vkProcSymbol = _VkCmdSetLineStippleEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetLineStippleEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetLineStippleEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_LINE_RASTERIZATION_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_EXT_LINE_RASTERIZATION_SPEC_VERSION = 1

type VK_EXT_LINE_RASTERIZATION_SPEC_VERSION = 1

pattern VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME :: CString

pattern VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME <-
        (is_VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME -> True)
  where
    VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME
      = _VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME

{-# INLINE _VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME #-}

_VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME :: CString
_VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME
  = Ptr "VK_EXT_line_rasterization\NUL"#

{-# INLINE is_VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME #-}

is_VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME

type VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME =
     "VK_EXT_line_rasterization"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT
        = VkStructureType 1000259000

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT
        = VkStructureType 1000259001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT
        = VkStructureType 1000259002

pattern VK_DYNAMIC_STATE_LINE_STIPPLE_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_LINE_STIPPLE_EXT =
        VkDynamicState 1000259000
