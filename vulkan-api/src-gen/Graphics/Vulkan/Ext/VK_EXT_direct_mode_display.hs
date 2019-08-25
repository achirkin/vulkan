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
module Graphics.Vulkan.Ext.VK_EXT_direct_mode_display
       (-- * Vulkan extension: @VK_EXT_direct_mode_display@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @NV@
        --
        -- type: @instance@
        --
        -- Extension number: @89@
        --
        -- Required extensions: 'VK_KHR_display'.
        --

        -- ** Required extensions: 'VK_KHR_display'.
        VkReleaseDisplayEXT, pattern VkReleaseDisplayEXT,
        HS_vkReleaseDisplayEXT, PFN_vkReleaseDisplayEXT, VkResult(..),
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
        VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION,
        pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION,
        VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME,
        pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME)
       where
import           GHC.Ptr                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc      (VulkanProc (..))
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Handles

pattern VkReleaseDisplayEXT :: CString

pattern VkReleaseDisplayEXT <- (is_VkReleaseDisplayEXT -> True)
  where
    VkReleaseDisplayEXT = _VkReleaseDisplayEXT

{-# INLINE _VkReleaseDisplayEXT #-}

_VkReleaseDisplayEXT :: CString
_VkReleaseDisplayEXT = Ptr "vkReleaseDisplayEXT\NUL"#

{-# INLINE is_VkReleaseDisplayEXT #-}

is_VkReleaseDisplayEXT :: CString -> Bool
is_VkReleaseDisplayEXT = (EQ ==) . cmpCStrings _VkReleaseDisplayEXT

type VkReleaseDisplayEXT = "vkReleaseDisplayEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkReleaseDisplayEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkReleaseDisplayEXT vkReleaseDisplayEXT registry at www.khronos.org>
type HS_vkReleaseDisplayEXT =
     VkPhysicalDevice -- ^ physicalDevice
                      -> VkDisplayKHR -- ^ display
                                      -> IO VkResult

type PFN_vkReleaseDisplayEXT = FunPtr HS_vkReleaseDisplayEXT

foreign import ccall unsafe "dynamic"
               unwrapVkReleaseDisplayEXTUnsafe ::
               PFN_vkReleaseDisplayEXT -> HS_vkReleaseDisplayEXT

foreign import ccall safe "dynamic" unwrapVkReleaseDisplayEXTSafe
               :: PFN_vkReleaseDisplayEXT -> HS_vkReleaseDisplayEXT

instance VulkanProc "vkReleaseDisplayEXT" where
    type VkProcType "vkReleaseDisplayEXT" = HS_vkReleaseDisplayEXT
    vkProcSymbol = _VkReleaseDisplayEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkReleaseDisplayEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkReleaseDisplayEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION = 1

type VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION = 1

pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME :: CString

pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME <-
        (is_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME -> True)
  where
    VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
      = _VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME

{-# INLINE _VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME #-}

_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME :: CString
_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
  = Ptr "VK_EXT_direct_mode_display\NUL"#

{-# INLINE is_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME #-}

is_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME

type VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME =
     "VK_EXT_direct_mode_display"
