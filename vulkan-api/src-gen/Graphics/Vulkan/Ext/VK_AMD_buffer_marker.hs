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
module Graphics.Vulkan.Ext.VK_AMD_buffer_marker
       (-- * Vulkan extension: @VK_AMD_buffer_marker@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Rakos @drakos-amd@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @180@
        VkCmdWriteBufferMarkerAMD, pattern VkCmdWriteBufferMarkerAMD,
        HS_vkCmdWriteBufferMarkerAMD, PFN_vkCmdWriteBufferMarkerAMD,
        module Graphics.Vulkan.Marshal, VkBool32(..), VkDeviceSize(..),
        VkFlags(..), VkSampleMask(..), VkPipelineBindPoint(..),
        VkPipelineCacheHeaderVersion(..), VkPipelineCreateBitmask(..),
        VkPipelineStageBitmask(..), VkPipelineCacheCreateFlagBits(..),
        VkPipelineColorBlendStateCreateFlagBits(..),
        VkPipelineCreateFlagBits(), VkPipelineCreateFlags(),
        VkPipelineDepthStencilStateCreateFlagBits(..),
        VkPipelineDynamicStateCreateFlagBits(..),
        VkPipelineInputAssemblyStateCreateFlagBits(..),
        VkPipelineLayoutCreateFlagBits(..),
        VkPipelineMultisampleStateCreateFlagBits(..),
        VkPipelineRasterizationStateCreateFlagBits(..),
        VkPipelineShaderStageCreateFlagBits(..), VkPipelineStageFlagBits(),
        VkPipelineStageFlags(),
        VkPipelineTessellationStateCreateFlagBits(..),
        VkPipelineVertexInputStateCreateFlagBits(..),
        VkPipelineViewportStateCreateFlagBits(..), VkBuffer, VkBufferView,
        VkBufferView_T(), VkBuffer_T(), VkCommandBuffer,
        VkCommandBuffer_T(), VkCommandPool, VkCommandPool_T(),
        VkDebugReportCallbackEXT, VkDebugReportCallbackEXT_T(),
        VkDebugUtilsMessengerEXT, VkDebugUtilsMessengerEXT_T(),
        VkDescriptorPool, VkDescriptorPool_T(), VkDescriptorSet,
        VkDescriptorSetLayout, VkDescriptorSetLayout_T(),
        VkDescriptorSet_T(), VkDescriptorUpdateTemplate,
        VkDescriptorUpdateTemplateKHR, VkDescriptorUpdateTemplateKHR_T(),
        VkDescriptorUpdateTemplate_T(), VkDevice, VkDeviceMemory,
        VkDeviceMemory_T(), VkDevice_T(), VkDisplayKHR, VkDisplayKHR_T(),
        VkDisplayModeKHR, VkDisplayModeKHR_T(), VkEvent, VkEvent_T(),
        VkFence, VkFence_T(), VkFramebuffer, VkFramebuffer_T(), VkImage,
        VkImageView, VkImageView_T(), VkImage_T(),
        VkIndirectCommandsLayoutNVX, VkIndirectCommandsLayoutNVX_T(),
        VkInstance, VkInstance_T(), VkObjectTableNVX, VkObjectTableNVX_T(),
        VkPhysicalDevice, VkPhysicalDevice_T(), VkPipeline,
        VkPipelineCache, VkPipelineCache_T(), VkPipelineLayout,
        VkPipelineLayout_T(), VkPipeline_T(), VkQueryPool, VkQueryPool_T(),
        VkQueue, VkQueue_T(), VkRenderPass, VkRenderPass_T(), VkSampler,
        VkSamplerYcbcrConversion, VkSamplerYcbcrConversionKHR,
        VkSamplerYcbcrConversionKHR_T(), VkSamplerYcbcrConversion_T(),
        VkSampler_T(), VkSemaphore, VkSemaphore_T(), VkShaderModule,
        VkShaderModule_T(), VkSurfaceKHR, VkSurfaceKHR_T(), VkSwapchainKHR,
        VkSwapchainKHR_T(), VkValidationCacheEXT, VkValidationCacheEXT_T(),
        VK_AMD_BUFFER_MARKER_SPEC_VERSION,
        pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION,
        VK_AMD_BUFFER_MARKER_EXTENSION_NAME,
        pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME)
       where
import           GHC.Ptr                             (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc        (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.Pipeline
import           Graphics.Vulkan.Types.Handles

pattern VkCmdWriteBufferMarkerAMD :: CString

pattern VkCmdWriteBufferMarkerAMD <-
        (is_VkCmdWriteBufferMarkerAMD -> True)
  where
    VkCmdWriteBufferMarkerAMD = _VkCmdWriteBufferMarkerAMD

{-# INLINE _VkCmdWriteBufferMarkerAMD #-}

_VkCmdWriteBufferMarkerAMD :: CString
_VkCmdWriteBufferMarkerAMD = Ptr "vkCmdWriteBufferMarkerAMD\NUL"#

{-# INLINE is_VkCmdWriteBufferMarkerAMD #-}

is_VkCmdWriteBufferMarkerAMD :: CString -> Bool
is_VkCmdWriteBufferMarkerAMD
  = (EQ ==) . cmpCStrings _VkCmdWriteBufferMarkerAMD

type VkCmdWriteBufferMarkerAMD = "vkCmdWriteBufferMarkerAMD"

-- | Queues: 'transfer', 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   Pipeline: @transfer@
--
--   > void vkCmdWriteBufferMarkerAMD
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineStageFlagBits pipelineStage
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , uint32_t marker
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdWriteBufferMarkerAMD vkCmdWriteBufferMarkerAMD registry at www.khronos.org>
type HS_vkCmdWriteBufferMarkerAMD =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkPipelineStageFlagBits -- ^ pipelineStage
                               ->
         VkBuffer -- ^ dstBuffer
                  -> VkDeviceSize -- ^ dstOffset
                                  -> Word32 -- ^ marker
                                            -> IO ()

type PFN_vkCmdWriteBufferMarkerAMD =
     FunPtr HS_vkCmdWriteBufferMarkerAMD

foreign import ccall unsafe "dynamic"
               unwrapVkCmdWriteBufferMarkerAMDUnsafe ::
               PFN_vkCmdWriteBufferMarkerAMD -> HS_vkCmdWriteBufferMarkerAMD

foreign import ccall safe "dynamic"
               unwrapVkCmdWriteBufferMarkerAMDSafe ::
               PFN_vkCmdWriteBufferMarkerAMD -> HS_vkCmdWriteBufferMarkerAMD

instance VulkanProc "vkCmdWriteBufferMarkerAMD" where
    type VkProcType "vkCmdWriteBufferMarkerAMD" =
         HS_vkCmdWriteBufferMarkerAMD
    vkProcSymbol = _VkCmdWriteBufferMarkerAMD

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdWriteBufferMarkerAMDUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdWriteBufferMarkerAMDSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION = 1

type VK_AMD_BUFFER_MARKER_SPEC_VERSION = 1

pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME :: CString

pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME <-
        (is_VK_AMD_BUFFER_MARKER_EXTENSION_NAME -> True)
  where
    VK_AMD_BUFFER_MARKER_EXTENSION_NAME
      = _VK_AMD_BUFFER_MARKER_EXTENSION_NAME

{-# INLINE _VK_AMD_BUFFER_MARKER_EXTENSION_NAME #-}

_VK_AMD_BUFFER_MARKER_EXTENSION_NAME :: CString
_VK_AMD_BUFFER_MARKER_EXTENSION_NAME
  = Ptr "VK_AMD_buffer_marker\NUL"#

{-# INLINE is_VK_AMD_BUFFER_MARKER_EXTENSION_NAME #-}

is_VK_AMD_BUFFER_MARKER_EXTENSION_NAME :: CString -> Bool
is_VK_AMD_BUFFER_MARKER_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_AMD_BUFFER_MARKER_EXTENSION_NAME

type VK_AMD_BUFFER_MARKER_EXTENSION_NAME = "VK_AMD_buffer_marker"
