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
module Graphics.Vulkan.Ext.VK_NV_scissor_exclusive
       (-- * Vulkan extension: @VK_NV_scissor_exclusive@
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
        -- Extension number: @206@
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
        VkDeviceQueueCreateInfo, VkExtent2D, VkOffset2D,
        VkPhysicalDeviceExclusiveScissorFeaturesNV,
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        VkPipelineViewportExclusiveScissorStateCreateInfoNV,
        VkPipelineViewportStateCreateInfo, VkRect2D, VkStructureType(..),
        VkViewport, -- > #include "vk_platform.h"
                    VkCmdSetExclusiveScissorNV,
        pattern VkCmdSetExclusiveScissorNV, HS_vkCmdSetExclusiveScissorNV,
        PFN_vkCmdSetExclusiveScissorNV, VkAccelerationStructureKHR,
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
        VkValidationCacheEXT, VkValidationCacheEXT_T(), VkExtent3D,
        VkOffset3D, VkRectLayerKHR, VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION,
        pattern VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION,
        VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME,
        pattern VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV,
        pattern VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                        (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.DynamicState             (VkDynamicState (..))
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.Extent
import Graphics.Vulkan.Types.Struct.Offset
import Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceExclusiveScissorFeaturesNV,
                                                            VkPhysicalDeviceFeatures2)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)
import Graphics.Vulkan.Types.Struct.Pipeline               (VkPipelineViewportExclusiveScissorStateCreateInfoNV,
                                                            VkPipelineViewportStateCreateInfo)
import Graphics.Vulkan.Types.Struct.Rect
import Graphics.Vulkan.Types.Struct.Viewport               (VkViewport)

pattern VkCmdSetExclusiveScissorNV :: CString

pattern VkCmdSetExclusiveScissorNV <-
        (is_VkCmdSetExclusiveScissorNV -> True)
  where
    VkCmdSetExclusiveScissorNV = _VkCmdSetExclusiveScissorNV

{-# INLINE _VkCmdSetExclusiveScissorNV #-}

_VkCmdSetExclusiveScissorNV :: CString
_VkCmdSetExclusiveScissorNV = Ptr "vkCmdSetExclusiveScissorNV\NUL"#

{-# INLINE is_VkCmdSetExclusiveScissorNV #-}

is_VkCmdSetExclusiveScissorNV :: CString -> Bool
is_VkCmdSetExclusiveScissorNV
  = (EQ ==) . cmpCStrings _VkCmdSetExclusiveScissorNV

type VkCmdSetExclusiveScissorNV = "vkCmdSetExclusiveScissorNV"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetExclusiveScissorNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstExclusiveScissor
--   >     , uint32_t exclusiveScissorCount
--   >     , const VkRect2D* pExclusiveScissors
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetExclusiveScissorNV vkCmdSetExclusiveScissorNV registry at www.khronos.org>
type HS_vkCmdSetExclusiveScissorNV =
     VkCommandBuffer -- ^ commandBuffer
                     -> Word32 -- ^ firstExclusiveScissor
                               -> Word32 -- ^ exclusiveScissorCount
                                         -> Ptr VkRect2D -- ^ pExclusiveScissors
                                                         -> IO ()

type PFN_vkCmdSetExclusiveScissorNV =
     FunPtr HS_vkCmdSetExclusiveScissorNV

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetExclusiveScissorNVUnsafe ::
               PFN_vkCmdSetExclusiveScissorNV -> HS_vkCmdSetExclusiveScissorNV

foreign import ccall safe "dynamic"
               unwrapVkCmdSetExclusiveScissorNVSafe ::
               PFN_vkCmdSetExclusiveScissorNV -> HS_vkCmdSetExclusiveScissorNV

instance VulkanProc "vkCmdSetExclusiveScissorNV" where
    type VkProcType "vkCmdSetExclusiveScissorNV" =
         HS_vkCmdSetExclusiveScissorNV
    vkProcSymbol = _VkCmdSetExclusiveScissorNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetExclusiveScissorNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetExclusiveScissorNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION = 1

type VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION = 1

pattern VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME :: CString

pattern VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME <-
        (is_VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME -> True)
  where
    VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
      = _VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME

{-# INLINE _VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME #-}

_VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME :: CString
_VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
  = Ptr "VK_NV_scissor_exclusive\NUL"#

{-# INLINE is_VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME #-}

is_VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME :: CString -> Bool
is_VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME

type VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME =
     "VK_NV_scissor_exclusive"

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV
        = VkStructureType 1000205000

pattern VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV :: VkDynamicState

pattern VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV =
        VkDynamicState 1000205001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV
        = VkStructureType 1000205002
