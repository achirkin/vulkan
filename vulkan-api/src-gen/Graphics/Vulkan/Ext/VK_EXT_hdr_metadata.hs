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
module Graphics.Vulkan.Ext.VK_EXT_hdr_metadata
       (-- * Vulkan extension: @VK_EXT_hdr_metadata@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Courtney Goeltzenleuchter @courtney-g@
        --
        -- author: @GOOGLE@
        --
        -- type: @device@
        --
        -- Extension number: @106@
        --
        -- Required extensions: 'VK_KHR_swapchain'.
        --

        -- ** Required extensions: 'VK_KHR_swapchain'.
        VkHdrMetadataEXT, VkStructureType(..), VkXYColorEXT,
        -- > #include "vk_platform.h"
        VkSetHdrMetadataEXT, pattern VkSetHdrMetadataEXT,
        HS_vkSetHdrMetadataEXT, PFN_vkSetHdrMetadataEXT, VkBuffer,
        VkBufferView, VkBufferView_T(), VkBuffer_T(), VkCommandBuffer,
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
        VK_EXT_HDR_METADATA_SPEC_VERSION,
        pattern VK_EXT_HDR_METADATA_SPEC_VERSION,
        VK_EXT_HDR_METADATA_EXTENSION_NAME,
        pattern VK_EXT_HDR_METADATA_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT)
       where
import           GHC.Ptr                                     (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                (VulkanProc (..))
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.HdrMetadataEXT
import           Graphics.Vulkan.Types.Struct.XYColorEXT

pattern VkSetHdrMetadataEXT :: CString

pattern VkSetHdrMetadataEXT <- (is_VkSetHdrMetadataEXT -> True)
  where
    VkSetHdrMetadataEXT = _VkSetHdrMetadataEXT

{-# INLINE _VkSetHdrMetadataEXT #-}

_VkSetHdrMetadataEXT :: CString
_VkSetHdrMetadataEXT = Ptr "vkSetHdrMetadataEXT\NUL"#

{-# INLINE is_VkSetHdrMetadataEXT #-}

is_VkSetHdrMetadataEXT :: CString -> Bool
is_VkSetHdrMetadataEXT = (EQ ==) . cmpCStrings _VkSetHdrMetadataEXT

type VkSetHdrMetadataEXT = "vkSetHdrMetadataEXT"

-- | > void vkSetHdrMetadataEXT
--   >     ( VkDevice device
--   >     , uint32_t swapchainCount
--   >     , const VkSwapchainKHR* pSwapchains
--   >     , const VkHdrMetadataEXT* pMetadata
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkSetHdrMetadataEXT vkSetHdrMetadataEXT registry at www.khronos.org>
type HS_vkSetHdrMetadataEXT =
     VkDevice -- ^ device
              ->
       Word32 -- ^ swapchainCount
              -> Ptr VkSwapchainKHR -- ^ pSwapchains
                                    -> Ptr VkHdrMetadataEXT -- ^ pMetadata
                                                            -> IO ()

type PFN_vkSetHdrMetadataEXT = FunPtr HS_vkSetHdrMetadataEXT

foreign import ccall unsafe "dynamic"
               unwrapVkSetHdrMetadataEXTUnsafe ::
               PFN_vkSetHdrMetadataEXT -> HS_vkSetHdrMetadataEXT

foreign import ccall safe "dynamic" unwrapVkSetHdrMetadataEXTSafe
               :: PFN_vkSetHdrMetadataEXT -> HS_vkSetHdrMetadataEXT

instance VulkanProc "vkSetHdrMetadataEXT" where
    type VkProcType "vkSetHdrMetadataEXT" = HS_vkSetHdrMetadataEXT
    vkProcSymbol = _VkSetHdrMetadataEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkSetHdrMetadataEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkSetHdrMetadataEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_HDR_METADATA_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_HDR_METADATA_SPEC_VERSION = 1

type VK_EXT_HDR_METADATA_SPEC_VERSION = 1

pattern VK_EXT_HDR_METADATA_EXTENSION_NAME :: CString

pattern VK_EXT_HDR_METADATA_EXTENSION_NAME <-
        (is_VK_EXT_HDR_METADATA_EXTENSION_NAME -> True)
  where
    VK_EXT_HDR_METADATA_EXTENSION_NAME
      = _VK_EXT_HDR_METADATA_EXTENSION_NAME

{-# INLINE _VK_EXT_HDR_METADATA_EXTENSION_NAME #-}

_VK_EXT_HDR_METADATA_EXTENSION_NAME :: CString
_VK_EXT_HDR_METADATA_EXTENSION_NAME
  = Ptr "VK_EXT_hdr_metadata\NUL"#

{-# INLINE is_VK_EXT_HDR_METADATA_EXTENSION_NAME #-}

is_VK_EXT_HDR_METADATA_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_HDR_METADATA_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_HDR_METADATA_EXTENSION_NAME

type VK_EXT_HDR_METADATA_EXTENSION_NAME = "VK_EXT_hdr_metadata"

pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT =
        VkStructureType 1000105000
