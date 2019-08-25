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
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_xlib_surface
       (VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkStructureType(..), VkAndroidSurfaceCreateFlagsKHR(..),
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
        VkXlibSurfaceCreateFlagsKHR(..), VkXlibSurfaceCreateInfoKHR,
        -- > #include "vk_platform.h"
        VkCreateXlibSurfaceKHR, pattern VkCreateXlibSurfaceKHR,
        HS_vkCreateXlibSurfaceKHR, PFN_vkCreateXlibSurfaceKHR,
        vkCreateXlibSurfaceKHR, vkCreateXlibSurfaceKHRUnsafe,
        vkCreateXlibSurfaceKHRSafe,
        VkGetPhysicalDeviceXlibPresentationSupportKHR,
        pattern VkGetPhysicalDeviceXlibPresentationSupportKHR,
        HS_vkGetPhysicalDeviceXlibPresentationSupportKHR,
        PFN_vkGetPhysicalDeviceXlibPresentationSupportKHR,
        vkGetPhysicalDeviceXlibPresentationSupportKHR,
        vkGetPhysicalDeviceXlibPresentationSupportKHRUnsafe,
        vkGetPhysicalDeviceXlibPresentationSupportKHRSafe,
        module Graphics.Vulkan.Marshal, VkInternalAllocationType(..),
        VkResult(..), VkSystemAllocationScope(..), newVkAllocationFunction,
        newVkDebugReportCallbackEXT, newVkDebugUtilsMessengerCallbackEXT,
        newVkFreeFunction, newVkInternalAllocationNotification,
        newVkInternalFreeNotification, newVkReallocationFunction,
        newVkVoidFunction, unwrapVkAllocationFunction,
        unwrapVkDebugReportCallbackEXT,
        unwrapVkDebugUtilsMessengerCallbackEXT, unwrapVkFreeFunction,
        unwrapVkInternalAllocationNotification,
        unwrapVkInternalFreeNotification, unwrapVkReallocationFunction,
        unwrapVkVoidFunction, HS_vkAllocationFunction,
        HS_vkDebugReportCallbackEXT, HS_vkDebugUtilsMessengerCallbackEXT,
        HS_vkFreeFunction, HS_vkInternalAllocationNotification,
        HS_vkInternalFreeNotification, HS_vkReallocationFunction,
        HS_vkVoidFunction, PFN_vkAllocationFunction,
        PFN_vkDebugReportCallbackEXT, PFN_vkDebugUtilsMessengerCallbackEXT,
        PFN_vkFreeFunction, PFN_vkInternalAllocationNotification,
        PFN_vkInternalFreeNotification, PFN_vkReallocationFunction,
        PFN_vkVoidFunction, VkBuffer, VkBufferView, VkBufferView_T(),
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
        DWORD, Display, HANDLE, HINSTANCE, HWND, LPCWSTR, MirConnection,
        MirSurface, RROutput, SECURITY_ATTRIBUTES, VisualID, Window,
        WlDisplay, WlSurface, XcbConnectionT, XcbVisualidT, XcbWindowT,
        VkAllocationCallbacks, VK_KHR_XLIB_SURFACE_SPEC_VERSION,
        pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION,
        VK_KHR_XLIB_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR)
       where
import           GHC.Ptr                                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.InternalAllocationType
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.SystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Include
import           Graphics.Vulkan.Types.Struct.AllocationCallbacks
import           Graphics.Vulkan.Types.Struct.PlatformXlibKhr
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

pattern VkCreateXlibSurfaceKHR :: CString

pattern VkCreateXlibSurfaceKHR <-
        (is_VkCreateXlibSurfaceKHR -> True)
  where
    VkCreateXlibSurfaceKHR = _VkCreateXlibSurfaceKHR

{-# INLINE _VkCreateXlibSurfaceKHR #-}

_VkCreateXlibSurfaceKHR :: CString
_VkCreateXlibSurfaceKHR = Ptr "vkCreateXlibSurfaceKHR\NUL"#

{-# INLINE is_VkCreateXlibSurfaceKHR #-}

is_VkCreateXlibSurfaceKHR :: CString -> Bool
is_VkCreateXlibSurfaceKHR
  = (EQ ==) . cmpCStrings _VkCreateXlibSurfaceKHR

type VkCreateXlibSurfaceKHR = "vkCreateXlibSurfaceKHR"

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateXlibSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkXlibSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateXlibSurfaceKHR vkCreateXlibSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateXlibSurfaceKHR <- vkGetInstanceProc @VkCreateXlibSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateXlibSurfaceKHR <- vkGetProc @VkCreateXlibSurfaceKHR
--
-- __Note:__ @vkCreateXlibSurfaceKHRUnsafe@ and @vkCreateXlibSurfaceKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateXlibSurfaceKHR@ is an alias
--           of @vkCreateXlibSurfaceKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateXlibSurfaceKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateXlibSurfaceKHR"
               vkCreateXlibSurfaceKHRUnsafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkXlibSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
vkCreateXlibSurfaceKHRUnsafe ::
                             VkInstance -- ^ instance
                                        ->
                               Ptr VkXlibSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                              ->
                                 Ptr VkAllocationCallbacks -- ^ pAllocator
                                                           -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                               -> IO VkResult
vkCreateXlibSurfaceKHRUnsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkCreateXlibSurfaceKHR)

{-# NOINLINE vkCreateXlibSurfaceKHRUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateXlibSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkXlibSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateXlibSurfaceKHR vkCreateXlibSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateXlibSurfaceKHR <- vkGetInstanceProc @VkCreateXlibSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateXlibSurfaceKHR <- vkGetProc @VkCreateXlibSurfaceKHR
--
-- __Note:__ @vkCreateXlibSurfaceKHRUnsafe@ and @vkCreateXlibSurfaceKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateXlibSurfaceKHR@ is an alias
--           of @vkCreateXlibSurfaceKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateXlibSurfaceKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateXlibSurfaceKHR"
               vkCreateXlibSurfaceKHRSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkXlibSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
vkCreateXlibSurfaceKHRSafe ::
                           VkInstance -- ^ instance
                                      ->
                             Ptr VkXlibSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                            ->
                               Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                             -> IO VkResult
vkCreateXlibSurfaceKHRSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateXlibSurfaceKHR)

{-# NOINLINE vkCreateXlibSurfaceKHRSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateXlibSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkXlibSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateXlibSurfaceKHR vkCreateXlibSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateXlibSurfaceKHR <- vkGetInstanceProc @VkCreateXlibSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateXlibSurfaceKHR <- vkGetProc @VkCreateXlibSurfaceKHR
--
-- __Note:__ @vkCreateXlibSurfaceKHRUnsafe@ and @vkCreateXlibSurfaceKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateXlibSurfaceKHR@ is an alias
--           of @vkCreateXlibSurfaceKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateXlibSurfaceKHRSafe@.
--
vkCreateXlibSurfaceKHR ::
                       VkInstance -- ^ instance
                                  ->
                         Ptr VkXlibSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                        ->
                           Ptr VkAllocationCallbacks -- ^ pAllocator
                                                     -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                         -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkCreateXlibSurfaceKHR = vkCreateXlibSurfaceKHRUnsafe
#else
vkCreateXlibSurfaceKHR = vkCreateXlibSurfaceKHRSafe

#endif
{-# INLINE vkCreateXlibSurfaceKHR #-}

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateXlibSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkXlibSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateXlibSurfaceKHR vkCreateXlibSurfaceKHR registry at www.khronos.org>
type HS_vkCreateXlibSurfaceKHR =
     VkInstance -- ^ instance
                ->
       Ptr VkXlibSurfaceCreateInfoKHR -- ^ pCreateInfo
                                      ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateXlibSurfaceKHR = FunPtr HS_vkCreateXlibSurfaceKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCreateXlibSurfaceKHRUnsafe ::
               PFN_vkCreateXlibSurfaceKHR -> HS_vkCreateXlibSurfaceKHR

foreign import ccall safe "dynamic"
               unwrapVkCreateXlibSurfaceKHRSafe ::
               PFN_vkCreateXlibSurfaceKHR -> HS_vkCreateXlibSurfaceKHR

instance VulkanProc "vkCreateXlibSurfaceKHR" where
    type VkProcType "vkCreateXlibSurfaceKHR" =
         HS_vkCreateXlibSurfaceKHR
    vkProcSymbol = _VkCreateXlibSurfaceKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateXlibSurfaceKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateXlibSurfaceKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceXlibPresentationSupportKHR :: CString

pattern VkGetPhysicalDeviceXlibPresentationSupportKHR <-
        (is_VkGetPhysicalDeviceXlibPresentationSupportKHR -> True)
  where
    VkGetPhysicalDeviceXlibPresentationSupportKHR
      = _VkGetPhysicalDeviceXlibPresentationSupportKHR

{-# INLINE _VkGetPhysicalDeviceXlibPresentationSupportKHR #-}

_VkGetPhysicalDeviceXlibPresentationSupportKHR :: CString
_VkGetPhysicalDeviceXlibPresentationSupportKHR
  = Ptr "vkGetPhysicalDeviceXlibPresentationSupportKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceXlibPresentationSupportKHR #-}

is_VkGetPhysicalDeviceXlibPresentationSupportKHR :: CString -> Bool
is_VkGetPhysicalDeviceXlibPresentationSupportKHR
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceXlibPresentationSupportKHR

type VkGetPhysicalDeviceXlibPresentationSupportKHR =
     "vkGetPhysicalDeviceXlibPresentationSupportKHR"

-- |
-- > VkBool32 vkGetPhysicalDeviceXlibPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , Display* dpy
-- >     , VisualID visualID
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceXlibPresentationSupportKHR vkGetPhysicalDeviceXlibPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceXlibPresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceXlibPresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceXlibPresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceXlibPresentationSupportKHR
--
-- __Note:__ @vkGetPhysicalDeviceXlibPresentationSupportKHRUnsafe@ and @vkGetPhysicalDeviceXlibPresentationSupportKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceXlibPresentationSupportKHR@ is an alias
--           of @vkGetPhysicalDeviceXlibPresentationSupportKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceXlibPresentationSupportKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe
               "vkGetPhysicalDeviceXlibPresentationSupportKHR"
               vkGetPhysicalDeviceXlibPresentationSupportKHRUnsafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ queueFamilyIndex
                        -> Ptr Display -- ^ dpy
                                       -> VisualID -- ^ visualID
                                                   -> IO VkBool32

#else
vkGetPhysicalDeviceXlibPresentationSupportKHRUnsafe ::
                                                    VkPhysicalDevice -- ^ physicalDevice
                                                                     ->
                                                      Word32 -- ^ queueFamilyIndex
                                                             ->
                                                        Ptr Display -- ^ dpy
                                                                    -> VisualID -- ^ visualID
                                                                                -> IO VkBool32
vkGetPhysicalDeviceXlibPresentationSupportKHRUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetPhysicalDeviceXlibPresentationSupportKHR)

{-# NOINLINE vkGetPhysicalDeviceXlibPresentationSupportKHRUnsafe
             #-}
#endif

-- |
-- > VkBool32 vkGetPhysicalDeviceXlibPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , Display* dpy
-- >     , VisualID visualID
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceXlibPresentationSupportKHR vkGetPhysicalDeviceXlibPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceXlibPresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceXlibPresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceXlibPresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceXlibPresentationSupportKHR
--
-- __Note:__ @vkGetPhysicalDeviceXlibPresentationSupportKHRUnsafe@ and @vkGetPhysicalDeviceXlibPresentationSupportKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceXlibPresentationSupportKHR@ is an alias
--           of @vkGetPhysicalDeviceXlibPresentationSupportKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceXlibPresentationSupportKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe
               "vkGetPhysicalDeviceXlibPresentationSupportKHR"
               vkGetPhysicalDeviceXlibPresentationSupportKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ queueFamilyIndex
                        -> Ptr Display -- ^ dpy
                                       -> VisualID -- ^ visualID
                                                   -> IO VkBool32

#else
vkGetPhysicalDeviceXlibPresentationSupportKHRSafe ::
                                                  VkPhysicalDevice -- ^ physicalDevice
                                                                   ->
                                                    Word32 -- ^ queueFamilyIndex
                                                           -> Ptr Display -- ^ dpy
                                                                          -> VisualID -- ^ visualID
                                                                                      -> IO VkBool32
vkGetPhysicalDeviceXlibPresentationSupportKHRSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceXlibPresentationSupportKHR)

{-# NOINLINE vkGetPhysicalDeviceXlibPresentationSupportKHRSafe #-}
#endif

-- |
-- > VkBool32 vkGetPhysicalDeviceXlibPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , Display* dpy
-- >     , VisualID visualID
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceXlibPresentationSupportKHR vkGetPhysicalDeviceXlibPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceXlibPresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceXlibPresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceXlibPresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceXlibPresentationSupportKHR
--
-- __Note:__ @vkGetPhysicalDeviceXlibPresentationSupportKHRUnsafe@ and @vkGetPhysicalDeviceXlibPresentationSupportKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceXlibPresentationSupportKHR@ is an alias
--           of @vkGetPhysicalDeviceXlibPresentationSupportKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceXlibPresentationSupportKHRSafe@.
--
vkGetPhysicalDeviceXlibPresentationSupportKHR ::
                                              VkPhysicalDevice -- ^ physicalDevice
                                                               ->
                                                Word32 -- ^ queueFamilyIndex
                                                       -> Ptr Display -- ^ dpy
                                                                      -> VisualID -- ^ visualID
                                                                                  -> IO VkBool32
#ifdef UNSAFE_FFI_DEFAULT
vkGetPhysicalDeviceXlibPresentationSupportKHR
  = vkGetPhysicalDeviceXlibPresentationSupportKHRUnsafe
#else
vkGetPhysicalDeviceXlibPresentationSupportKHR
  = vkGetPhysicalDeviceXlibPresentationSupportKHRSafe

#endif
{-# INLINE vkGetPhysicalDeviceXlibPresentationSupportKHR #-}

-- | > VkBool32 vkGetPhysicalDeviceXlibPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , Display* dpy
--   >     , VisualID visualID
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceXlibPresentationSupportKHR vkGetPhysicalDeviceXlibPresentationSupportKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceXlibPresentationSupportKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Word32 -- ^ queueFamilyIndex
              -> Ptr Display -- ^ dpy
                             -> VisualID -- ^ visualID
                                         -> IO VkBool32

type PFN_vkGetPhysicalDeviceXlibPresentationSupportKHR =
     FunPtr HS_vkGetPhysicalDeviceXlibPresentationSupportKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceXlibPresentationSupportKHRUnsafe ::
               PFN_vkGetPhysicalDeviceXlibPresentationSupportKHR ->
                 HS_vkGetPhysicalDeviceXlibPresentationSupportKHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceXlibPresentationSupportKHRSafe ::
               PFN_vkGetPhysicalDeviceXlibPresentationSupportKHR ->
                 HS_vkGetPhysicalDeviceXlibPresentationSupportKHR

instance VulkanProc "vkGetPhysicalDeviceXlibPresentationSupportKHR"
         where
    type VkProcType "vkGetPhysicalDeviceXlibPresentationSupportKHR" =
         HS_vkGetPhysicalDeviceXlibPresentationSupportKHR
    vkProcSymbol = _VkGetPhysicalDeviceXlibPresentationSupportKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceXlibPresentationSupportKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceXlibPresentationSupportKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION = 6

type VK_KHR_XLIB_SURFACE_SPEC_VERSION = 6

pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_XLIB_SURFACE_EXTENSION_NAME -> True)
  where
    VK_KHR_XLIB_SURFACE_EXTENSION_NAME
      = _VK_KHR_XLIB_SURFACE_EXTENSION_NAME

{-# INLINE _VK_KHR_XLIB_SURFACE_EXTENSION_NAME #-}

_VK_KHR_XLIB_SURFACE_EXTENSION_NAME :: CString
_VK_KHR_XLIB_SURFACE_EXTENSION_NAME
  = Ptr "VK_KHR_xlib_surface\NUL"#

{-# INLINE is_VK_KHR_XLIB_SURFACE_EXTENSION_NAME #-}

is_VK_KHR_XLIB_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_XLIB_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_XLIB_SURFACE_EXTENSION_NAME

type VK_KHR_XLIB_SURFACE_EXTENSION_NAME = "VK_KHR_xlib_surface"

pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000004000
