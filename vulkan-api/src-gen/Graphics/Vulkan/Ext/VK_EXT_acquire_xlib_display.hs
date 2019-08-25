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
module Graphics.Vulkan.Ext.VK_EXT_acquire_xlib_display
       (-- * Vulkan extension: @VK_EXT_acquire_xlib_display@
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
        -- platform: @xlib_xrandr@
        --
        -- Extension number: @90@
        --
        -- Required extensions: 'VK_EXT_direct_mode_display'.
        --

        -- ** Required extensions: 'VK_EXT_direct_mode_display'.
        VkAcquireXlibDisplayEXT, pattern VkAcquireXlibDisplayEXT,
        HS_vkAcquireXlibDisplayEXT, PFN_vkAcquireXlibDisplayEXT,
        VkGetRandROutputDisplayEXT, pattern VkGetRandROutputDisplayEXT,
        HS_vkGetRandROutputDisplayEXT, PFN_vkGetRandROutputDisplayEXT,
        VkResult(..), VkBuffer, VkBufferView, VkBufferView_T(),
        VkBuffer_T(), VkCommandBuffer, VkCommandBuffer_T(), VkCommandPool,
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
        VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION,
        pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION,
        VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME,
        pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME)
       where
import           GHC.Ptr                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc      (VulkanProc (..))
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Include     (Display, RROutput)

pattern VkAcquireXlibDisplayEXT :: CString

pattern VkAcquireXlibDisplayEXT <-
        (is_VkAcquireXlibDisplayEXT -> True)
  where
    VkAcquireXlibDisplayEXT = _VkAcquireXlibDisplayEXT

{-# INLINE _VkAcquireXlibDisplayEXT #-}

_VkAcquireXlibDisplayEXT :: CString
_VkAcquireXlibDisplayEXT = Ptr "vkAcquireXlibDisplayEXT\NUL"#

{-# INLINE is_VkAcquireXlibDisplayEXT #-}

is_VkAcquireXlibDisplayEXT :: CString -> Bool
is_VkAcquireXlibDisplayEXT
  = (EQ ==) . cmpCStrings _VkAcquireXlibDisplayEXT

type VkAcquireXlibDisplayEXT = "vkAcquireXlibDisplayEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkAcquireXlibDisplayEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , Display* dpy
--   >     , VkDisplayKHR display
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAcquireXlibDisplayEXT vkAcquireXlibDisplayEXT registry at www.khronos.org>
type HS_vkAcquireXlibDisplayEXT =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Ptr Display -- ^ dpy
                                     -> VkDisplayKHR -- ^ display
                                                     -> IO VkResult

type PFN_vkAcquireXlibDisplayEXT =
     FunPtr HS_vkAcquireXlibDisplayEXT

foreign import ccall unsafe "dynamic"
               unwrapVkAcquireXlibDisplayEXTUnsafe ::
               PFN_vkAcquireXlibDisplayEXT -> HS_vkAcquireXlibDisplayEXT

foreign import ccall safe "dynamic"
               unwrapVkAcquireXlibDisplayEXTSafe ::
               PFN_vkAcquireXlibDisplayEXT -> HS_vkAcquireXlibDisplayEXT

instance VulkanProc "vkAcquireXlibDisplayEXT" where
    type VkProcType "vkAcquireXlibDisplayEXT" =
         HS_vkAcquireXlibDisplayEXT
    vkProcSymbol = _VkAcquireXlibDisplayEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkAcquireXlibDisplayEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkAcquireXlibDisplayEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetRandROutputDisplayEXT :: CString

pattern VkGetRandROutputDisplayEXT <-
        (is_VkGetRandROutputDisplayEXT -> True)
  where
    VkGetRandROutputDisplayEXT = _VkGetRandROutputDisplayEXT

{-# INLINE _VkGetRandROutputDisplayEXT #-}

_VkGetRandROutputDisplayEXT :: CString
_VkGetRandROutputDisplayEXT = Ptr "vkGetRandROutputDisplayEXT\NUL"#

{-# INLINE is_VkGetRandROutputDisplayEXT #-}

is_VkGetRandROutputDisplayEXT :: CString -> Bool
is_VkGetRandROutputDisplayEXT
  = (EQ ==) . cmpCStrings _VkGetRandROutputDisplayEXT

type VkGetRandROutputDisplayEXT = "vkGetRandROutputDisplayEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkGetRandROutputDisplayEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , Display* dpy
--   >     , RROutput rrOutput
--   >     , VkDisplayKHR* pDisplay
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetRandROutputDisplayEXT vkGetRandROutputDisplayEXT registry at www.khronos.org>
type HS_vkGetRandROutputDisplayEXT =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr Display -- ^ dpy
                   -> RROutput -- ^ rrOutput
                               -> Ptr VkDisplayKHR -- ^ pDisplay
                                                   -> IO VkResult

type PFN_vkGetRandROutputDisplayEXT =
     FunPtr HS_vkGetRandROutputDisplayEXT

foreign import ccall unsafe "dynamic"
               unwrapVkGetRandROutputDisplayEXTUnsafe ::
               PFN_vkGetRandROutputDisplayEXT -> HS_vkGetRandROutputDisplayEXT

foreign import ccall safe "dynamic"
               unwrapVkGetRandROutputDisplayEXTSafe ::
               PFN_vkGetRandROutputDisplayEXT -> HS_vkGetRandROutputDisplayEXT

instance VulkanProc "vkGetRandROutputDisplayEXT" where
    type VkProcType "vkGetRandROutputDisplayEXT" =
         HS_vkGetRandROutputDisplayEXT
    vkProcSymbol = _VkGetRandROutputDisplayEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetRandROutputDisplayEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetRandROutputDisplayEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION = 1

type VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION = 1

pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME :: CString

pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME <-
        (is_VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME -> True)
  where
    VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
      = _VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME

{-# INLINE _VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME #-}

_VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME :: CString
_VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
  = Ptr "VK_EXT_acquire_xlib_display\NUL"#

{-# INLINE is_VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME #-}

is_VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME

type VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME =
     "VK_EXT_acquire_xlib_display"
