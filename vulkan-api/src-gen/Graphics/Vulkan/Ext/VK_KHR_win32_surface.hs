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
module Graphics.Vulkan.Ext.VK_KHR_win32_surface
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
        VkXlibSurfaceCreateFlagsKHR(..), VkWin32SurfaceCreateInfoKHR,
        -- > #include "vk_platform.h"
        VkCreateWin32SurfaceKHR, pattern VkCreateWin32SurfaceKHR,
        HS_vkCreateWin32SurfaceKHR, PFN_vkCreateWin32SurfaceKHR,
        vkCreateWin32SurfaceKHR, vkCreateWin32SurfaceKHRUnsafe,
        vkCreateWin32SurfaceKHRSafe,
        VkGetPhysicalDeviceWin32PresentationSupportKHR,
        pattern VkGetPhysicalDeviceWin32PresentationSupportKHR,
        HS_vkGetPhysicalDeviceWin32PresentationSupportKHR,
        PFN_vkGetPhysicalDeviceWin32PresentationSupportKHR,
        vkGetPhysicalDeviceWin32PresentationSupportKHR,
        vkGetPhysicalDeviceWin32PresentationSupportKHRUnsafe,
        vkGetPhysicalDeviceWin32PresentationSupportKHRSafe,
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
        VkAllocationCallbacks, VkD3D12FenceSubmitInfoKHR,
        VkExportFenceWin32HandleInfoKHR, VkExportMemoryWin32HandleInfoKHR,
        VkExportMemoryWin32HandleInfoNV,
        VkExportSemaphoreWin32HandleInfoKHR, VkFenceGetWin32HandleInfoKHR,
        VkImportFenceWin32HandleInfoKHR, VkImportMemoryWin32HandleInfoKHR,
        VkImportMemoryWin32HandleInfoNV,
        VkImportSemaphoreWin32HandleInfoKHR, VkMemoryGetWin32HandleInfoKHR,
        VkMemoryWin32HandlePropertiesKHR, VkSemaphoreGetWin32HandleInfoKHR,
        VkWin32KeyedMutexAcquireReleaseInfoKHR,
        VkWin32KeyedMutexAcquireReleaseInfoNV,
        VK_KHR_WIN32_SURFACE_SPEC_VERSION,
        pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION,
        VK_KHR_WIN32_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR)
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
import           Graphics.Vulkan.Types.Struct.PlatformWin32Khr
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

pattern VkCreateWin32SurfaceKHR :: CString

pattern VkCreateWin32SurfaceKHR <-
        (is_VkCreateWin32SurfaceKHR -> True)
  where
    VkCreateWin32SurfaceKHR = _VkCreateWin32SurfaceKHR

{-# INLINE _VkCreateWin32SurfaceKHR #-}

_VkCreateWin32SurfaceKHR :: CString
_VkCreateWin32SurfaceKHR = Ptr "vkCreateWin32SurfaceKHR\NUL"#

{-# INLINE is_VkCreateWin32SurfaceKHR #-}

is_VkCreateWin32SurfaceKHR :: CString -> Bool
is_VkCreateWin32SurfaceKHR
  = (EQ ==) . cmpCStrings _VkCreateWin32SurfaceKHR

type VkCreateWin32SurfaceKHR = "vkCreateWin32SurfaceKHR"

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateWin32SurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkWin32SurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateWin32SurfaceKHR vkCreateWin32SurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateWin32SurfaceKHR <- vkGetInstanceProc @VkCreateWin32SurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateWin32SurfaceKHR <- vkGetProc @VkCreateWin32SurfaceKHR
--
-- __Note:__ @vkCreateWin32SurfaceKHRUnsafe@ and @vkCreateWin32SurfaceKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateWin32SurfaceKHR@ is an alias
--           of @vkCreateWin32SurfaceKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateWin32SurfaceKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateWin32SurfaceKHR"
               vkCreateWin32SurfaceKHRUnsafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkWin32SurfaceCreateInfoKHR -- ^ pCreateInfo
                                                 ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
vkCreateWin32SurfaceKHRUnsafe ::
                              VkInstance -- ^ instance
                                         ->
                                Ptr VkWin32SurfaceCreateInfoKHR -- ^ pCreateInfo
                                                                ->
                                  Ptr VkAllocationCallbacks -- ^ pAllocator
                                                            -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                                -> IO VkResult
vkCreateWin32SurfaceKHRUnsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkCreateWin32SurfaceKHR)

{-# NOINLINE vkCreateWin32SurfaceKHRUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateWin32SurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkWin32SurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateWin32SurfaceKHR vkCreateWin32SurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateWin32SurfaceKHR <- vkGetInstanceProc @VkCreateWin32SurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateWin32SurfaceKHR <- vkGetProc @VkCreateWin32SurfaceKHR
--
-- __Note:__ @vkCreateWin32SurfaceKHRUnsafe@ and @vkCreateWin32SurfaceKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateWin32SurfaceKHR@ is an alias
--           of @vkCreateWin32SurfaceKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateWin32SurfaceKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateWin32SurfaceKHR"
               vkCreateWin32SurfaceKHRSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkWin32SurfaceCreateInfoKHR -- ^ pCreateInfo
                                                 ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
vkCreateWin32SurfaceKHRSafe ::
                            VkInstance -- ^ instance
                                       ->
                              Ptr VkWin32SurfaceCreateInfoKHR -- ^ pCreateInfo
                                                              ->
                                Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                              -> IO VkResult
vkCreateWin32SurfaceKHRSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateWin32SurfaceKHR)

{-# NOINLINE vkCreateWin32SurfaceKHRSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateWin32SurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkWin32SurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateWin32SurfaceKHR vkCreateWin32SurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateWin32SurfaceKHR <- vkGetInstanceProc @VkCreateWin32SurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateWin32SurfaceKHR <- vkGetProc @VkCreateWin32SurfaceKHR
--
-- __Note:__ @vkCreateWin32SurfaceKHRUnsafe@ and @vkCreateWin32SurfaceKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateWin32SurfaceKHR@ is an alias
--           of @vkCreateWin32SurfaceKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateWin32SurfaceKHRSafe@.
--
vkCreateWin32SurfaceKHR ::
                        VkInstance -- ^ instance
                                   ->
                          Ptr VkWin32SurfaceCreateInfoKHR -- ^ pCreateInfo
                                                          ->
                            Ptr VkAllocationCallbacks -- ^ pAllocator
                                                      -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                          -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkCreateWin32SurfaceKHR = vkCreateWin32SurfaceKHRUnsafe
#else
vkCreateWin32SurfaceKHR = vkCreateWin32SurfaceKHRSafe

#endif
{-# INLINE vkCreateWin32SurfaceKHR #-}

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateWin32SurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkWin32SurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateWin32SurfaceKHR vkCreateWin32SurfaceKHR registry at www.khronos.org>
type HS_vkCreateWin32SurfaceKHR =
     VkInstance -- ^ instance
                ->
       Ptr VkWin32SurfaceCreateInfoKHR -- ^ pCreateInfo
                                       ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateWin32SurfaceKHR =
     FunPtr HS_vkCreateWin32SurfaceKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCreateWin32SurfaceKHRUnsafe ::
               PFN_vkCreateWin32SurfaceKHR -> HS_vkCreateWin32SurfaceKHR

foreign import ccall safe "dynamic"
               unwrapVkCreateWin32SurfaceKHRSafe ::
               PFN_vkCreateWin32SurfaceKHR -> HS_vkCreateWin32SurfaceKHR

instance VulkanProc "vkCreateWin32SurfaceKHR" where
    type VkProcType "vkCreateWin32SurfaceKHR" =
         HS_vkCreateWin32SurfaceKHR
    vkProcSymbol = _VkCreateWin32SurfaceKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateWin32SurfaceKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateWin32SurfaceKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceWin32PresentationSupportKHR :: CString

pattern VkGetPhysicalDeviceWin32PresentationSupportKHR <-
        (is_VkGetPhysicalDeviceWin32PresentationSupportKHR -> True)
  where
    VkGetPhysicalDeviceWin32PresentationSupportKHR
      = _VkGetPhysicalDeviceWin32PresentationSupportKHR

{-# INLINE _VkGetPhysicalDeviceWin32PresentationSupportKHR #-}

_VkGetPhysicalDeviceWin32PresentationSupportKHR :: CString
_VkGetPhysicalDeviceWin32PresentationSupportKHR
  = Ptr "vkGetPhysicalDeviceWin32PresentationSupportKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceWin32PresentationSupportKHR #-}

is_VkGetPhysicalDeviceWin32PresentationSupportKHR ::
                                                  CString -> Bool
is_VkGetPhysicalDeviceWin32PresentationSupportKHR
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceWin32PresentationSupportKHR

type VkGetPhysicalDeviceWin32PresentationSupportKHR =
     "vkGetPhysicalDeviceWin32PresentationSupportKHR"

-- |
-- > VkBool32 vkGetPhysicalDeviceWin32PresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceWin32PresentationSupportKHR vkGetPhysicalDeviceWin32PresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceWin32PresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceWin32PresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceWin32PresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceWin32PresentationSupportKHR
--
-- __Note:__ @vkGetPhysicalDeviceWin32PresentationSupportKHRUnsafe@ and @vkGetPhysicalDeviceWin32PresentationSupportKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceWin32PresentationSupportKHR@ is an alias
--           of @vkGetPhysicalDeviceWin32PresentationSupportKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceWin32PresentationSupportKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe
               "vkGetPhysicalDeviceWin32PresentationSupportKHR"
               vkGetPhysicalDeviceWin32PresentationSupportKHRUnsafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> IO VkBool32

#else
vkGetPhysicalDeviceWin32PresentationSupportKHRUnsafe ::
                                                     VkPhysicalDevice -- ^ physicalDevice
                                                                      -> Word32 -- ^ queueFamilyIndex
                                                                                -> IO VkBool32
vkGetPhysicalDeviceWin32PresentationSupportKHRUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetPhysicalDeviceWin32PresentationSupportKHR)

{-# NOINLINE vkGetPhysicalDeviceWin32PresentationSupportKHRUnsafe
             #-}
#endif

-- |
-- > VkBool32 vkGetPhysicalDeviceWin32PresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceWin32PresentationSupportKHR vkGetPhysicalDeviceWin32PresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceWin32PresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceWin32PresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceWin32PresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceWin32PresentationSupportKHR
--
-- __Note:__ @vkGetPhysicalDeviceWin32PresentationSupportKHRUnsafe@ and @vkGetPhysicalDeviceWin32PresentationSupportKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceWin32PresentationSupportKHR@ is an alias
--           of @vkGetPhysicalDeviceWin32PresentationSupportKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceWin32PresentationSupportKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe
               "vkGetPhysicalDeviceWin32PresentationSupportKHR"
               vkGetPhysicalDeviceWin32PresentationSupportKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> IO VkBool32

#else
vkGetPhysicalDeviceWin32PresentationSupportKHRSafe ::
                                                   VkPhysicalDevice -- ^ physicalDevice
                                                                    -> Word32 -- ^ queueFamilyIndex
                                                                              -> IO VkBool32
vkGetPhysicalDeviceWin32PresentationSupportKHRSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceWin32PresentationSupportKHR)

{-# NOINLINE vkGetPhysicalDeviceWin32PresentationSupportKHRSafe #-}
#endif

-- |
-- > VkBool32 vkGetPhysicalDeviceWin32PresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceWin32PresentationSupportKHR vkGetPhysicalDeviceWin32PresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceWin32PresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceWin32PresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceWin32PresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceWin32PresentationSupportKHR
--
-- __Note:__ @vkGetPhysicalDeviceWin32PresentationSupportKHRUnsafe@ and @vkGetPhysicalDeviceWin32PresentationSupportKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceWin32PresentationSupportKHR@ is an alias
--           of @vkGetPhysicalDeviceWin32PresentationSupportKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceWin32PresentationSupportKHRSafe@.
--
vkGetPhysicalDeviceWin32PresentationSupportKHR ::
                                               VkPhysicalDevice -- ^ physicalDevice
                                                                -> Word32 -- ^ queueFamilyIndex
                                                                          -> IO VkBool32
#ifdef UNSAFE_FFI_DEFAULT
vkGetPhysicalDeviceWin32PresentationSupportKHR
  = vkGetPhysicalDeviceWin32PresentationSupportKHRUnsafe
#else
vkGetPhysicalDeviceWin32PresentationSupportKHR
  = vkGetPhysicalDeviceWin32PresentationSupportKHRSafe

#endif
{-# INLINE vkGetPhysicalDeviceWin32PresentationSupportKHR #-}

-- | > VkBool32 vkGetPhysicalDeviceWin32PresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceWin32PresentationSupportKHR vkGetPhysicalDeviceWin32PresentationSupportKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceWin32PresentationSupportKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Word32 -- ^ queueFamilyIndex
                                -> IO VkBool32

type PFN_vkGetPhysicalDeviceWin32PresentationSupportKHR =
     FunPtr HS_vkGetPhysicalDeviceWin32PresentationSupportKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceWin32PresentationSupportKHRUnsafe ::
               PFN_vkGetPhysicalDeviceWin32PresentationSupportKHR ->
                 HS_vkGetPhysicalDeviceWin32PresentationSupportKHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceWin32PresentationSupportKHRSafe ::
               PFN_vkGetPhysicalDeviceWin32PresentationSupportKHR ->
                 HS_vkGetPhysicalDeviceWin32PresentationSupportKHR

instance VulkanProc
           "vkGetPhysicalDeviceWin32PresentationSupportKHR"
         where
    type VkProcType "vkGetPhysicalDeviceWin32PresentationSupportKHR" =
         HS_vkGetPhysicalDeviceWin32PresentationSupportKHR
    vkProcSymbol = _VkGetPhysicalDeviceWin32PresentationSupportKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceWin32PresentationSupportKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceWin32PresentationSupportKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION = 6

type VK_KHR_WIN32_SURFACE_SPEC_VERSION = 6

pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_WIN32_SURFACE_EXTENSION_NAME -> True)
  where
    VK_KHR_WIN32_SURFACE_EXTENSION_NAME
      = _VK_KHR_WIN32_SURFACE_EXTENSION_NAME

{-# INLINE _VK_KHR_WIN32_SURFACE_EXTENSION_NAME #-}

_VK_KHR_WIN32_SURFACE_EXTENSION_NAME :: CString
_VK_KHR_WIN32_SURFACE_EXTENSION_NAME
  = Ptr "VK_KHR_win32_surface\NUL"#

{-# INLINE is_VK_KHR_WIN32_SURFACE_EXTENSION_NAME #-}

is_VK_KHR_WIN32_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_WIN32_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_WIN32_SURFACE_EXTENSION_NAME

type VK_KHR_WIN32_SURFACE_EXTENSION_NAME = "VK_KHR_win32_surface"

pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000009000
