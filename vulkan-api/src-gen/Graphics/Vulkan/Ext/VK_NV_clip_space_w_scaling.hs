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
module Graphics.Vulkan.Ext.VK_NV_clip_space_w_scaling
       (VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkExtent2D, VkOffset2D, VkAndroidSurfaceCreateFlagsKHR(..),
        VkBufferViewCreateFlags(..), VkCommandPoolTrimFlags(..),
        VkCommandPoolTrimFlagsKHR(..),
        VkDebugUtilsMessengerCallbackDataFlagsEXT(..),
        VkDebugUtilsMessengerCreateFlagsEXT(..),
        VkDescriptorPoolResetFlags(..),
        VkDescriptorUpdateTemplateCreateFlags(..),
        VkDescriptorUpdateTemplateCreateFlagsKHR(..),
        VkDeviceCreateFlags(..), VkDisplayModeCreateFlagsKHR(..),
        VkDisplaySurfaceCreateFlagsKHR(..), VkEventCreateFlags(..),
        VkExternalFenceFeatureFlagsKHR(..),
        VkExternalFenceHandleTypeFlagsKHR(..),
        VkExternalMemoryFeatureFlagsKHR(..),
        VkExternalMemoryHandleTypeFlagsKHR(..),
        VkExternalSemaphoreFeatureFlagsKHR(..),
        VkExternalSemaphoreHandleTypeFlagsKHR(..),
        VkFenceImportFlagsKHR(..), VkFramebufferCreateFlags(..),
        VkIOSSurfaceCreateFlagsMVK(..), VkImageViewCreateFlags(..),
        VkInstanceCreateFlags(..), VkMacOSSurfaceCreateFlagsMVK(..),
        VkMemoryAllocateFlagsKHR(..), VkMemoryMapFlags(..),
        VkMirSurfaceCreateFlagsKHR(..), VkPeerMemoryFeatureFlagsKHR(..),
        VkPipelineCacheCreateFlags(..),
        VkPipelineColorBlendStateCreateFlags(..),
        VkPipelineCoverageModulationStateCreateFlagsNV(..),
        VkPipelineCoverageToColorStateCreateFlagsNV(..),
        VkPipelineDepthStencilStateCreateFlags(..),
        VkPipelineDiscardRectangleStateCreateFlagsEXT(..),
        VkPipelineDynamicStateCreateFlags(..),
        VkPipelineInputAssemblyStateCreateFlags(..),
        VkPipelineLayoutCreateFlags(..),
        VkPipelineMultisampleStateCreateFlags(..),
        VkPipelineRasterizationConservativeStateCreateFlagsEXT(..),
        VkPipelineRasterizationStateCreateFlags(..),
        VkPipelineShaderStageCreateFlags(..),
        VkPipelineTessellationStateCreateFlags(..),
        VkPipelineVertexInputStateCreateFlags(..),
        VkPipelineViewportStateCreateFlags(..),
        VkPipelineViewportSwizzleStateCreateFlagsNV(..),
        VkQueryPoolCreateFlags(..), VkRenderPassCreateFlags(..),
        VkSamplerCreateFlags(..), VkSemaphoreCreateFlags(..),
        VkSemaphoreImportFlagsKHR(..), VkShaderModuleCreateFlags(..),
        VkValidationCacheCreateFlagsEXT(..), VkViSurfaceCreateFlagsNN(..),
        VkWaylandSurfaceCreateFlagsKHR(..),
        VkWin32SurfaceCreateFlagsKHR(..), VkXcbSurfaceCreateFlagsKHR(..),
        VkXlibSurfaceCreateFlagsKHR(..), VkPipelineViewportStateCreateInfo,
        VkPipelineViewportWScalingStateCreateInfoNV, VkRect2D,
        VkStructureType(..), VkViewport, VkViewportWScalingNV,
        -- > #include "vk_platform.h"
        VkCmdSetViewportWScalingNV, pattern VkCmdSetViewportWScalingNV,
        HS_vkCmdSetViewportWScalingNV, PFN_vkCmdSetViewportWScalingNV,
        VkBuffer, VkBufferView, VkBufferView_T(), VkBuffer_T(),
        VkCommandBuffer, VkCommandBuffer_T(), VkCommandPool,
        VkCommandPool_T(), VkDebugReportCallbackEXT,
        VkDebugReportCallbackEXT_T(), VkDebugUtilsMessengerEXT,
        VkDebugUtilsMessengerEXT_T(), VkDescriptorPool,
        VkDescriptorPool_T(), VkDescriptorSet, VkDescriptorSetLayout,
        VkDescriptorSetLayout_T(), VkDescriptorSet_T(),
        VkDescriptorUpdateTemplate, VkDescriptorUpdateTemplateKHR,
        VkDescriptorUpdateTemplateKHR_T(), VkDescriptorUpdateTemplate_T(),
        VkDevice, VkDeviceMemory, VkDeviceMemory_T(), VkDevice_T(),
        VkDisplayKHR, VkDisplayKHR_T(), VkDisplayModeKHR,
        VkDisplayModeKHR_T(), VkEvent, VkEvent_T(), VkFence, VkFence_T(),
        VkFramebuffer, VkFramebuffer_T(), VkImage, VkImageView,
        VkImageView_T(), VkImage_T(), VkIndirectCommandsLayoutNVX,
        VkIndirectCommandsLayoutNVX_T(), VkInstance, VkInstance_T(),
        VkObjectTableNVX, VkObjectTableNVX_T(), VkPhysicalDevice,
        VkPhysicalDevice_T(), VkPipeline, VkPipelineCache,
        VkPipelineCache_T(), VkPipelineLayout, VkPipelineLayout_T(),
        VkPipeline_T(), VkQueryPool, VkQueryPool_T(), VkQueue, VkQueue_T(),
        VkRenderPass, VkRenderPass_T(), VkSampler,
        VkSamplerYcbcrConversion, VkSamplerYcbcrConversionKHR,
        VkSamplerYcbcrConversionKHR_T(), VkSamplerYcbcrConversion_T(),
        VkSampler_T(), VkSemaphore, VkSemaphore_T(), VkShaderModule,
        VkShaderModule_T(), VkSurfaceKHR, VkSurfaceKHR_T(), VkSwapchainKHR,
        VkSwapchainKHR_T(), VkValidationCacheEXT, VkValidationCacheEXT_T(),
        VkViewportSwizzleNV, VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION,
        pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION,
        VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME,
        pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV,
        pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV)
       where
import           GHC.Ptr                                  (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc             (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.DynamicState  (VkDynamicState (..))
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Extent      (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.Offset      (VkOffset2D)
import           Graphics.Vulkan.Types.Struct.Pipeline    (VkPipelineViewportStateCreateInfo,
                                                           VkPipelineViewportWScalingStateCreateInfoNV)
import           Graphics.Vulkan.Types.Struct.Rect        (VkRect2D)
import           Graphics.Vulkan.Types.Struct.Viewport

pattern VkCmdSetViewportWScalingNV :: CString

pattern VkCmdSetViewportWScalingNV <-
        (is_VkCmdSetViewportWScalingNV -> True)
  where
    VkCmdSetViewportWScalingNV = _VkCmdSetViewportWScalingNV

{-# INLINE _VkCmdSetViewportWScalingNV #-}

_VkCmdSetViewportWScalingNV :: CString
_VkCmdSetViewportWScalingNV = Ptr "vkCmdSetViewportWScalingNV\NUL"#

{-# INLINE is_VkCmdSetViewportWScalingNV #-}

is_VkCmdSetViewportWScalingNV :: CString -> Bool
is_VkCmdSetViewportWScalingNV
  = (EQ ==) . cmpCStrings _VkCmdSetViewportWScalingNV

type VkCmdSetViewportWScalingNV = "vkCmdSetViewportWScalingNV"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetViewportWScalingNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstViewport
--   >     , uint32_t viewportCount
--   >     , const VkViewportWScalingNV* pViewportWScalings
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetViewportWScalingNV vkCmdSetViewportWScalingNV registry at www.khronos.org>
type HS_vkCmdSetViewportWScalingNV =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ firstViewport
              -> Word32 -- ^ viewportCount
                        -> Ptr VkViewportWScalingNV -- ^ pViewportWScalings
                                                    -> IO ()

type PFN_vkCmdSetViewportWScalingNV =
     FunPtr HS_vkCmdSetViewportWScalingNV

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetViewportWScalingNVUnsafe ::
               PFN_vkCmdSetViewportWScalingNV -> HS_vkCmdSetViewportWScalingNV

foreign import ccall safe "dynamic"
               unwrapVkCmdSetViewportWScalingNVSafe ::
               PFN_vkCmdSetViewportWScalingNV -> HS_vkCmdSetViewportWScalingNV

instance VulkanProc "vkCmdSetViewportWScalingNV" where
    type VkProcType "vkCmdSetViewportWScalingNV" =
         HS_vkCmdSetViewportWScalingNV
    vkProcSymbol = _VkCmdSetViewportWScalingNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetViewportWScalingNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetViewportWScalingNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION = 1

type VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION = 1

pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: CString

pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME <-
        (is_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME -> True)
  where
    VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
      = _VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME

{-# INLINE _VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME #-}

_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: CString
_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
  = Ptr "VK_NV_clip_space_w_scaling\NUL"#

{-# INLINE is_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME #-}

is_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: CString -> Bool
is_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME

type VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME =
     "VK_NV_clip_space_w_scaling"

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
        = VkStructureType 1000087000

pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV :: VkDynamicState

pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV =
        VkDynamicState 1000087000
