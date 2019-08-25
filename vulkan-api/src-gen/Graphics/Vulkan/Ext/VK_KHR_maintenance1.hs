{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
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
module Graphics.Vulkan.Ext.VK_KHR_maintenance1
       (VkAndroidSurfaceCreateFlagsKHR(..), VkBufferViewCreateFlags(..),
        VkCommandPoolTrimFlags(..), VkCommandPoolTrimFlagsKHR(..),
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
        VkXlibSurfaceCreateFlagsKHR(..), VkTrimCommandPoolKHR,
        pattern VkTrimCommandPoolKHR, HS_vkTrimCommandPoolKHR,
        PFN_vkTrimCommandPoolKHR, module Graphics.Vulkan.Marshal,
        VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
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
        VK_KHR_MAINTENANCE1_SPEC_VERSION,
        pattern VK_KHR_MAINTENANCE1_SPEC_VERSION,
        VK_KHR_MAINTENANCE1_EXTENSION_NAME,
        pattern VK_KHR_MAINTENANCE1_EXTENSION_NAME,
        pattern VK_ERROR_OUT_OF_POOL_MEMORY_KHR,
        pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR,
        pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR,
        pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR)
       where
import           GHC.Ptr                         (Ptr (..))
import           Graphics.Vulkan.Core_1_1        (pattern VK_ERROR_OUT_OF_POOL_MEMORY,
                                                  pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT,
                                                  pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT,
                                                  pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc    (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Handles

pattern VkTrimCommandPoolKHR :: CString

pattern VkTrimCommandPoolKHR <- (is_VkTrimCommandPoolKHR -> True)
  where
    VkTrimCommandPoolKHR = _VkTrimCommandPoolKHR

{-# INLINE _VkTrimCommandPoolKHR #-}

_VkTrimCommandPoolKHR :: CString
_VkTrimCommandPoolKHR = Ptr "vkTrimCommandPoolKHR\NUL"#

{-# INLINE is_VkTrimCommandPoolKHR #-}

is_VkTrimCommandPoolKHR :: CString -> Bool
is_VkTrimCommandPoolKHR
  = (EQ ==) . cmpCStrings _VkTrimCommandPoolKHR

type VkTrimCommandPoolKHR = "vkTrimCommandPoolKHR"

-- | This is an alias for `vkTrimCommandPool`.
--
--   > void vkTrimCommandPoolKHR
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , VkCommandPoolTrimFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkTrimCommandPoolKHR vkTrimCommandPoolKHR registry at www.khronos.org>
type HS_vkTrimCommandPoolKHR =
     VkDevice -- ^ device
              -> VkCommandPool -- ^ commandPool
                               -> VkCommandPoolTrimFlags -- ^ flags
                                                         -> IO ()

type PFN_vkTrimCommandPoolKHR = FunPtr HS_vkTrimCommandPoolKHR

foreign import ccall unsafe "dynamic"
               unwrapVkTrimCommandPoolKHRUnsafe ::
               PFN_vkTrimCommandPoolKHR -> HS_vkTrimCommandPoolKHR

foreign import ccall safe "dynamic" unwrapVkTrimCommandPoolKHRSafe
               :: PFN_vkTrimCommandPoolKHR -> HS_vkTrimCommandPoolKHR

instance VulkanProc "vkTrimCommandPoolKHR" where
    type VkProcType "vkTrimCommandPoolKHR" = HS_vkTrimCommandPoolKHR
    vkProcSymbol = _VkTrimCommandPoolKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkTrimCommandPoolKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkTrimCommandPoolKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_MAINTENANCE1_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_MAINTENANCE1_SPEC_VERSION = 2

type VK_KHR_MAINTENANCE1_SPEC_VERSION = 2

pattern VK_KHR_MAINTENANCE1_EXTENSION_NAME :: CString

pattern VK_KHR_MAINTENANCE1_EXTENSION_NAME <-
        (is_VK_KHR_MAINTENANCE1_EXTENSION_NAME -> True)
  where
    VK_KHR_MAINTENANCE1_EXTENSION_NAME
      = _VK_KHR_MAINTENANCE1_EXTENSION_NAME

{-# INLINE _VK_KHR_MAINTENANCE1_EXTENSION_NAME #-}

_VK_KHR_MAINTENANCE1_EXTENSION_NAME :: CString
_VK_KHR_MAINTENANCE1_EXTENSION_NAME
  = Ptr "VK_KHR_maintenance1\NUL"#

{-# INLINE is_VK_KHR_MAINTENANCE1_EXTENSION_NAME #-}

is_VK_KHR_MAINTENANCE1_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_MAINTENANCE1_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_MAINTENANCE1_EXTENSION_NAME

type VK_KHR_MAINTENANCE1_EXTENSION_NAME = "VK_KHR_maintenance1"

pattern VK_ERROR_OUT_OF_POOL_MEMORY_KHR =
        VK_ERROR_OUT_OF_POOL_MEMORY

pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR =
        VK_FORMAT_FEATURE_TRANSFER_SRC_BIT

pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR =
        VK_FORMAT_FEATURE_TRANSFER_DST_BIT

pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR =
        VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
