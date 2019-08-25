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
module Graphics.Vulkan.Ext.VK_KHR_wayland_surface
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
        VkXlibSurfaceCreateFlagsKHR(..), VkWaylandSurfaceCreateInfoKHR,
        -- > #include "vk_platform.h"
        VkCreateWaylandSurfaceKHR, pattern VkCreateWaylandSurfaceKHR,
        HS_vkCreateWaylandSurfaceKHR, PFN_vkCreateWaylandSurfaceKHR,
        vkCreateWaylandSurfaceKHR, vkCreateWaylandSurfaceKHRUnsafe,
        vkCreateWaylandSurfaceKHRSafe,
        VkGetPhysicalDeviceWaylandPresentationSupportKHR,
        pattern VkGetPhysicalDeviceWaylandPresentationSupportKHR,
        HS_vkGetPhysicalDeviceWaylandPresentationSupportKHR,
        PFN_vkGetPhysicalDeviceWaylandPresentationSupportKHR,
        vkGetPhysicalDeviceWaylandPresentationSupportKHR,
        vkGetPhysicalDeviceWaylandPresentationSupportKHRUnsafe,
        vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe,
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
        VkAllocationCallbacks, VK_KHR_WAYLAND_SURFACE_SPEC_VERSION,
        pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION,
        VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR)
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
import           Graphics.Vulkan.Types.Struct.PlatformWaylandKhr
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

pattern VkCreateWaylandSurfaceKHR :: CString

pattern VkCreateWaylandSurfaceKHR <-
        (is_VkCreateWaylandSurfaceKHR -> True)
  where
    VkCreateWaylandSurfaceKHR = _VkCreateWaylandSurfaceKHR

{-# INLINE _VkCreateWaylandSurfaceKHR #-}

_VkCreateWaylandSurfaceKHR :: CString
_VkCreateWaylandSurfaceKHR = Ptr "vkCreateWaylandSurfaceKHR\NUL"#

{-# INLINE is_VkCreateWaylandSurfaceKHR #-}

is_VkCreateWaylandSurfaceKHR :: CString -> Bool
is_VkCreateWaylandSurfaceKHR
  = (EQ ==) . cmpCStrings _VkCreateWaylandSurfaceKHR

type VkCreateWaylandSurfaceKHR = "vkCreateWaylandSurfaceKHR"

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateWaylandSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkWaylandSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateWaylandSurfaceKHR vkCreateWaylandSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateWaylandSurfaceKHR <- vkGetInstanceProc @VkCreateWaylandSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateWaylandSurfaceKHR <- vkGetProc @VkCreateWaylandSurfaceKHR
--
-- __Note:__ @vkCreateWaylandSurfaceKHRUnsafe@ and @vkCreateWaylandSurfaceKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateWaylandSurfaceKHR@ is an alias
--           of @vkCreateWaylandSurfaceKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateWaylandSurfaceKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateWaylandSurfaceKHR"
               vkCreateWaylandSurfaceKHRUnsafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkWaylandSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
vkCreateWaylandSurfaceKHRUnsafe ::
                                VkInstance -- ^ instance
                                           ->
                                  Ptr VkWaylandSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                                    ->
                                    Ptr VkAllocationCallbacks -- ^ pAllocator
                                                              -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                                  -> IO VkResult
vkCreateWaylandSurfaceKHRUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkCreateWaylandSurfaceKHR)

{-# NOINLINE vkCreateWaylandSurfaceKHRUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateWaylandSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkWaylandSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateWaylandSurfaceKHR vkCreateWaylandSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateWaylandSurfaceKHR <- vkGetInstanceProc @VkCreateWaylandSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateWaylandSurfaceKHR <- vkGetProc @VkCreateWaylandSurfaceKHR
--
-- __Note:__ @vkCreateWaylandSurfaceKHRUnsafe@ and @vkCreateWaylandSurfaceKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateWaylandSurfaceKHR@ is an alias
--           of @vkCreateWaylandSurfaceKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateWaylandSurfaceKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateWaylandSurfaceKHR"
               vkCreateWaylandSurfaceKHRSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkWaylandSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
vkCreateWaylandSurfaceKHRSafe ::
                              VkInstance -- ^ instance
                                         ->
                                Ptr VkWaylandSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                                  ->
                                  Ptr VkAllocationCallbacks -- ^ pAllocator
                                                            -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                                -> IO VkResult
vkCreateWaylandSurfaceKHRSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateWaylandSurfaceKHR)

{-# NOINLINE vkCreateWaylandSurfaceKHRSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateWaylandSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkWaylandSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateWaylandSurfaceKHR vkCreateWaylandSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateWaylandSurfaceKHR <- vkGetInstanceProc @VkCreateWaylandSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateWaylandSurfaceKHR <- vkGetProc @VkCreateWaylandSurfaceKHR
--
-- __Note:__ @vkCreateWaylandSurfaceKHRUnsafe@ and @vkCreateWaylandSurfaceKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateWaylandSurfaceKHR@ is an alias
--           of @vkCreateWaylandSurfaceKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateWaylandSurfaceKHRSafe@.
--
vkCreateWaylandSurfaceKHR ::
                          VkInstance -- ^ instance
                                     ->
                            Ptr VkWaylandSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                              ->
                              Ptr VkAllocationCallbacks -- ^ pAllocator
                                                        -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                            -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkCreateWaylandSurfaceKHR = vkCreateWaylandSurfaceKHRUnsafe
#else
vkCreateWaylandSurfaceKHR = vkCreateWaylandSurfaceKHRSafe

#endif
{-# INLINE vkCreateWaylandSurfaceKHR #-}

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateWaylandSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkWaylandSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateWaylandSurfaceKHR vkCreateWaylandSurfaceKHR registry at www.khronos.org>
type HS_vkCreateWaylandSurfaceKHR =
     VkInstance -- ^ instance
                ->
       Ptr VkWaylandSurfaceCreateInfoKHR -- ^ pCreateInfo
                                         ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateWaylandSurfaceKHR =
     FunPtr HS_vkCreateWaylandSurfaceKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCreateWaylandSurfaceKHRUnsafe ::
               PFN_vkCreateWaylandSurfaceKHR -> HS_vkCreateWaylandSurfaceKHR

foreign import ccall safe "dynamic"
               unwrapVkCreateWaylandSurfaceKHRSafe ::
               PFN_vkCreateWaylandSurfaceKHR -> HS_vkCreateWaylandSurfaceKHR

instance VulkanProc "vkCreateWaylandSurfaceKHR" where
    type VkProcType "vkCreateWaylandSurfaceKHR" =
         HS_vkCreateWaylandSurfaceKHR
    vkProcSymbol = _VkCreateWaylandSurfaceKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateWaylandSurfaceKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateWaylandSurfaceKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceWaylandPresentationSupportKHR :: CString

pattern VkGetPhysicalDeviceWaylandPresentationSupportKHR <-
        (is_VkGetPhysicalDeviceWaylandPresentationSupportKHR -> True)
  where
    VkGetPhysicalDeviceWaylandPresentationSupportKHR
      = _VkGetPhysicalDeviceWaylandPresentationSupportKHR

{-# INLINE _VkGetPhysicalDeviceWaylandPresentationSupportKHR #-}

_VkGetPhysicalDeviceWaylandPresentationSupportKHR :: CString
_VkGetPhysicalDeviceWaylandPresentationSupportKHR
  = Ptr "vkGetPhysicalDeviceWaylandPresentationSupportKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceWaylandPresentationSupportKHR #-}

is_VkGetPhysicalDeviceWaylandPresentationSupportKHR ::
                                                    CString -> Bool
is_VkGetPhysicalDeviceWaylandPresentationSupportKHR
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceWaylandPresentationSupportKHR

type VkGetPhysicalDeviceWaylandPresentationSupportKHR =
     "vkGetPhysicalDeviceWaylandPresentationSupportKHR"

-- |
-- > VkBool32 vkGetPhysicalDeviceWaylandPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , struct wl_display* display
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceWaylandPresentationSupportKHR vkGetPhysicalDeviceWaylandPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceWaylandPresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceWaylandPresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceWaylandPresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceWaylandPresentationSupportKHR
--
-- __Note:__ @vkGetPhysicalDeviceWaylandPresentationSupportKHRUnsafe@ and @vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceWaylandPresentationSupportKHR@ is an alias
--           of @vkGetPhysicalDeviceWaylandPresentationSupportKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe
               "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
               vkGetPhysicalDeviceWaylandPresentationSupportKHRUnsafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> Ptr WlDisplay -- ^ display
                                                           -> IO VkBool32

#else
vkGetPhysicalDeviceWaylandPresentationSupportKHRUnsafe ::
                                                       VkPhysicalDevice -- ^ physicalDevice
                                                                        ->
                                                         Word32 -- ^ queueFamilyIndex
                                                                -> Ptr WlDisplay -- ^ display
                                                                                 -> IO VkBool32
vkGetPhysicalDeviceWaylandPresentationSupportKHRUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetPhysicalDeviceWaylandPresentationSupportKHR)

{-# NOINLINE vkGetPhysicalDeviceWaylandPresentationSupportKHRUnsafe
             #-}
#endif

-- |
-- > VkBool32 vkGetPhysicalDeviceWaylandPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , struct wl_display* display
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceWaylandPresentationSupportKHR vkGetPhysicalDeviceWaylandPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceWaylandPresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceWaylandPresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceWaylandPresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceWaylandPresentationSupportKHR
--
-- __Note:__ @vkGetPhysicalDeviceWaylandPresentationSupportKHRUnsafe@ and @vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceWaylandPresentationSupportKHR@ is an alias
--           of @vkGetPhysicalDeviceWaylandPresentationSupportKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe
               "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
               vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> Ptr WlDisplay -- ^ display
                                                           -> IO VkBool32

#else
vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe ::
                                                     VkPhysicalDevice -- ^ physicalDevice
                                                                      ->
                                                       Word32 -- ^ queueFamilyIndex
                                                              -> Ptr WlDisplay -- ^ display
                                                                               -> IO VkBool32
vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceWaylandPresentationSupportKHR)

{-# NOINLINE vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe
             #-}
#endif

-- |
-- > VkBool32 vkGetPhysicalDeviceWaylandPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , struct wl_display* display
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceWaylandPresentationSupportKHR vkGetPhysicalDeviceWaylandPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceWaylandPresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceWaylandPresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceWaylandPresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceWaylandPresentationSupportKHR
--
-- __Note:__ @vkGetPhysicalDeviceWaylandPresentationSupportKHRUnsafe@ and @vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceWaylandPresentationSupportKHR@ is an alias
--           of @vkGetPhysicalDeviceWaylandPresentationSupportKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe@.
--
vkGetPhysicalDeviceWaylandPresentationSupportKHR ::
                                                 VkPhysicalDevice -- ^ physicalDevice
                                                                  ->
                                                   Word32 -- ^ queueFamilyIndex
                                                          -> Ptr WlDisplay -- ^ display
                                                                           -> IO VkBool32
#ifdef UNSAFE_FFI_DEFAULT
vkGetPhysicalDeviceWaylandPresentationSupportKHR
  = vkGetPhysicalDeviceWaylandPresentationSupportKHRUnsafe
#else
vkGetPhysicalDeviceWaylandPresentationSupportKHR
  = vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe

#endif
{-# INLINE vkGetPhysicalDeviceWaylandPresentationSupportKHR #-}

-- | > VkBool32 vkGetPhysicalDeviceWaylandPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , struct wl_display* display
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceWaylandPresentationSupportKHR vkGetPhysicalDeviceWaylandPresentationSupportKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceWaylandPresentationSupportKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Word32 -- ^ queueFamilyIndex
                                -> Ptr WlDisplay -- ^ display
                                                 -> IO VkBool32

type PFN_vkGetPhysicalDeviceWaylandPresentationSupportKHR =
     FunPtr HS_vkGetPhysicalDeviceWaylandPresentationSupportKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceWaylandPresentationSupportKHRUnsafe ::
               PFN_vkGetPhysicalDeviceWaylandPresentationSupportKHR ->
                 HS_vkGetPhysicalDeviceWaylandPresentationSupportKHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceWaylandPresentationSupportKHRSafe ::
               PFN_vkGetPhysicalDeviceWaylandPresentationSupportKHR ->
                 HS_vkGetPhysicalDeviceWaylandPresentationSupportKHR

instance VulkanProc
           "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
         where
    type VkProcType "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
         = HS_vkGetPhysicalDeviceWaylandPresentationSupportKHR
    vkProcSymbol = _VkGetPhysicalDeviceWaylandPresentationSupportKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceWaylandPresentationSupportKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceWaylandPresentationSupportKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION = 6

type VK_KHR_WAYLAND_SURFACE_SPEC_VERSION = 6

pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME -> True)
  where
    VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
      = _VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME

{-# INLINE _VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME #-}

_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME :: CString
_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
  = Ptr "VK_KHR_wayland_surface\NUL"#

{-# INLINE is_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME #-}

is_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME

type VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME =
     "VK_KHR_wayland_surface"

pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000006000
