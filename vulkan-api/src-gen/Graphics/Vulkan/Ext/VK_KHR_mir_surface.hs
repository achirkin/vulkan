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
module Graphics.Vulkan.Ext.VK_KHR_mir_surface
       (VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkAndroidSurfaceCreateFlagsKHR(..), VkBufferViewCreateFlags(..),
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
        VkXlibSurfaceCreateFlagsKHR(..), VkMirSurfaceCreateInfoKHR,
        VkStructureType(..), -- > #include "vk_platform.h"
                             VkCreateMirSurfaceKHR,
        pattern VkCreateMirSurfaceKHR, HS_vkCreateMirSurfaceKHR,
        PFN_vkCreateMirSurfaceKHR, vkCreateMirSurfaceKHR,
        vkCreateMirSurfaceKHRUnsafe, vkCreateMirSurfaceKHRSafe,
        VkGetPhysicalDeviceMirPresentationSupportKHR,
        pattern VkGetPhysicalDeviceMirPresentationSupportKHR,
        HS_vkGetPhysicalDeviceMirPresentationSupportKHR,
        PFN_vkGetPhysicalDeviceMirPresentationSupportKHR,
        vkGetPhysicalDeviceMirPresentationSupportKHR,
        vkGetPhysicalDeviceMirPresentationSupportKHRUnsafe,
        vkGetPhysicalDeviceMirPresentationSupportKHRSafe,
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
        VkAllocationCallbacks, VK_KHR_MIR_SURFACE_SPEC_VERSION,
        pattern VK_KHR_MIR_SURFACE_SPEC_VERSION,
        VK_KHR_MIR_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR)
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
import           Graphics.Vulkan.Types.Struct.PlatformMirKhr
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

pattern VkCreateMirSurfaceKHR :: CString

pattern VkCreateMirSurfaceKHR <- (is_VkCreateMirSurfaceKHR -> True)
  where
    VkCreateMirSurfaceKHR = _VkCreateMirSurfaceKHR

{-# INLINE _VkCreateMirSurfaceKHR #-}

_VkCreateMirSurfaceKHR :: CString
_VkCreateMirSurfaceKHR = Ptr "vkCreateMirSurfaceKHR\NUL"#

{-# INLINE is_VkCreateMirSurfaceKHR #-}

is_VkCreateMirSurfaceKHR :: CString -> Bool
is_VkCreateMirSurfaceKHR
  = (EQ ==) . cmpCStrings _VkCreateMirSurfaceKHR

type VkCreateMirSurfaceKHR = "vkCreateMirSurfaceKHR"

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateMirSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkMirSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateMirSurfaceKHR vkCreateMirSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateMirSurfaceKHR <- vkGetInstanceProc @VkCreateMirSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateMirSurfaceKHR <- vkGetProc @VkCreateMirSurfaceKHR
--
-- __Note:__ @vkCreateMirSurfaceKHRUnsafe@ and @vkCreateMirSurfaceKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateMirSurfaceKHR@ is an alias
--           of @vkCreateMirSurfaceKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateMirSurfaceKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateMirSurfaceKHR"
               vkCreateMirSurfaceKHRUnsafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkMirSurfaceCreateInfoKHR -- ^ pCreateInfo
                                               ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
vkCreateMirSurfaceKHRUnsafe ::
                            VkInstance -- ^ instance
                                       ->
                              Ptr VkMirSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                            ->
                                Ptr VkAllocationCallbacks -- ^ pAllocator
                                                          -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                              -> IO VkResult
vkCreateMirSurfaceKHRUnsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkCreateMirSurfaceKHR)

{-# NOINLINE vkCreateMirSurfaceKHRUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateMirSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkMirSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateMirSurfaceKHR vkCreateMirSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateMirSurfaceKHR <- vkGetInstanceProc @VkCreateMirSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateMirSurfaceKHR <- vkGetProc @VkCreateMirSurfaceKHR
--
-- __Note:__ @vkCreateMirSurfaceKHRUnsafe@ and @vkCreateMirSurfaceKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateMirSurfaceKHR@ is an alias
--           of @vkCreateMirSurfaceKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateMirSurfaceKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateMirSurfaceKHR"
               vkCreateMirSurfaceKHRSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkMirSurfaceCreateInfoKHR -- ^ pCreateInfo
                                               ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
vkCreateMirSurfaceKHRSafe ::
                          VkInstance -- ^ instance
                                     ->
                            Ptr VkMirSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                          ->
                              Ptr VkAllocationCallbacks -- ^ pAllocator
                                                        -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                            -> IO VkResult
vkCreateMirSurfaceKHRSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateMirSurfaceKHR)

{-# NOINLINE vkCreateMirSurfaceKHRSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateMirSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkMirSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateMirSurfaceKHR vkCreateMirSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateMirSurfaceKHR <- vkGetInstanceProc @VkCreateMirSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateMirSurfaceKHR <- vkGetProc @VkCreateMirSurfaceKHR
--
-- __Note:__ @vkCreateMirSurfaceKHRUnsafe@ and @vkCreateMirSurfaceKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateMirSurfaceKHR@ is an alias
--           of @vkCreateMirSurfaceKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateMirSurfaceKHRSafe@.
--
vkCreateMirSurfaceKHR ::
                      VkInstance -- ^ instance
                                 ->
                        Ptr VkMirSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                      ->
                          Ptr VkAllocationCallbacks -- ^ pAllocator
                                                    -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                        -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkCreateMirSurfaceKHR = vkCreateMirSurfaceKHRUnsafe
#else
vkCreateMirSurfaceKHR = vkCreateMirSurfaceKHRSafe

#endif
{-# INLINE vkCreateMirSurfaceKHR #-}

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateMirSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkMirSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateMirSurfaceKHR vkCreateMirSurfaceKHR registry at www.khronos.org>
type HS_vkCreateMirSurfaceKHR =
     VkInstance -- ^ instance
                ->
       Ptr VkMirSurfaceCreateInfoKHR -- ^ pCreateInfo
                                     ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateMirSurfaceKHR = FunPtr HS_vkCreateMirSurfaceKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCreateMirSurfaceKHRUnsafe ::
               PFN_vkCreateMirSurfaceKHR -> HS_vkCreateMirSurfaceKHR

foreign import ccall safe "dynamic" unwrapVkCreateMirSurfaceKHRSafe
               :: PFN_vkCreateMirSurfaceKHR -> HS_vkCreateMirSurfaceKHR

instance VulkanProc "vkCreateMirSurfaceKHR" where
    type VkProcType "vkCreateMirSurfaceKHR" = HS_vkCreateMirSurfaceKHR
    vkProcSymbol = _VkCreateMirSurfaceKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateMirSurfaceKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateMirSurfaceKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceMirPresentationSupportKHR :: CString

pattern VkGetPhysicalDeviceMirPresentationSupportKHR <-
        (is_VkGetPhysicalDeviceMirPresentationSupportKHR -> True)
  where
    VkGetPhysicalDeviceMirPresentationSupportKHR
      = _VkGetPhysicalDeviceMirPresentationSupportKHR

{-# INLINE _VkGetPhysicalDeviceMirPresentationSupportKHR #-}

_VkGetPhysicalDeviceMirPresentationSupportKHR :: CString
_VkGetPhysicalDeviceMirPresentationSupportKHR
  = Ptr "vkGetPhysicalDeviceMirPresentationSupportKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceMirPresentationSupportKHR #-}

is_VkGetPhysicalDeviceMirPresentationSupportKHR :: CString -> Bool
is_VkGetPhysicalDeviceMirPresentationSupportKHR
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceMirPresentationSupportKHR

type VkGetPhysicalDeviceMirPresentationSupportKHR =
     "vkGetPhysicalDeviceMirPresentationSupportKHR"

-- |
-- > VkBool32 vkGetPhysicalDeviceMirPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , MirConnection* connection
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMirPresentationSupportKHR vkGetPhysicalDeviceMirPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceMirPresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceMirPresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceMirPresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceMirPresentationSupportKHR
--
-- __Note:__ @vkGetPhysicalDeviceMirPresentationSupportKHRUnsafe@ and @vkGetPhysicalDeviceMirPresentationSupportKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceMirPresentationSupportKHR@ is an alias
--           of @vkGetPhysicalDeviceMirPresentationSupportKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceMirPresentationSupportKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe
               "vkGetPhysicalDeviceMirPresentationSupportKHR"
               vkGetPhysicalDeviceMirPresentationSupportKHRUnsafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> Ptr MirConnection -- ^ connection
                                                               -> IO VkBool32

#else
vkGetPhysicalDeviceMirPresentationSupportKHRUnsafe ::
                                                   VkPhysicalDevice -- ^ physicalDevice
                                                                    ->
                                                     Word32 -- ^ queueFamilyIndex
                                                            -> Ptr MirConnection -- ^ connection
                                                                                 -> IO VkBool32
vkGetPhysicalDeviceMirPresentationSupportKHRUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetPhysicalDeviceMirPresentationSupportKHR)

{-# NOINLINE vkGetPhysicalDeviceMirPresentationSupportKHRUnsafe #-}
#endif

-- |
-- > VkBool32 vkGetPhysicalDeviceMirPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , MirConnection* connection
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMirPresentationSupportKHR vkGetPhysicalDeviceMirPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceMirPresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceMirPresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceMirPresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceMirPresentationSupportKHR
--
-- __Note:__ @vkGetPhysicalDeviceMirPresentationSupportKHRUnsafe@ and @vkGetPhysicalDeviceMirPresentationSupportKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceMirPresentationSupportKHR@ is an alias
--           of @vkGetPhysicalDeviceMirPresentationSupportKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceMirPresentationSupportKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe
               "vkGetPhysicalDeviceMirPresentationSupportKHR"
               vkGetPhysicalDeviceMirPresentationSupportKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> Ptr MirConnection -- ^ connection
                                                               -> IO VkBool32

#else
vkGetPhysicalDeviceMirPresentationSupportKHRSafe ::
                                                 VkPhysicalDevice -- ^ physicalDevice
                                                                  ->
                                                   Word32 -- ^ queueFamilyIndex
                                                          -> Ptr MirConnection -- ^ connection
                                                                               -> IO VkBool32
vkGetPhysicalDeviceMirPresentationSupportKHRSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceMirPresentationSupportKHR)

{-# NOINLINE vkGetPhysicalDeviceMirPresentationSupportKHRSafe #-}
#endif

-- |
-- > VkBool32 vkGetPhysicalDeviceMirPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , MirConnection* connection
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMirPresentationSupportKHR vkGetPhysicalDeviceMirPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceMirPresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceMirPresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceMirPresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceMirPresentationSupportKHR
--
-- __Note:__ @vkGetPhysicalDeviceMirPresentationSupportKHRUnsafe@ and @vkGetPhysicalDeviceMirPresentationSupportKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDeviceMirPresentationSupportKHR@ is an alias
--           of @vkGetPhysicalDeviceMirPresentationSupportKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDeviceMirPresentationSupportKHRSafe@.
--
vkGetPhysicalDeviceMirPresentationSupportKHR ::
                                             VkPhysicalDevice -- ^ physicalDevice
                                                              ->
                                               Word32 -- ^ queueFamilyIndex
                                                      -> Ptr MirConnection -- ^ connection
                                                                           -> IO VkBool32
#ifdef UNSAFE_FFI_DEFAULT
vkGetPhysicalDeviceMirPresentationSupportKHR
  = vkGetPhysicalDeviceMirPresentationSupportKHRUnsafe
#else
vkGetPhysicalDeviceMirPresentationSupportKHR
  = vkGetPhysicalDeviceMirPresentationSupportKHRSafe

#endif
{-# INLINE vkGetPhysicalDeviceMirPresentationSupportKHR #-}

-- | > VkBool32 vkGetPhysicalDeviceMirPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , MirConnection* connection
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMirPresentationSupportKHR vkGetPhysicalDeviceMirPresentationSupportKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceMirPresentationSupportKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Word32 -- ^ queueFamilyIndex
                                -> Ptr MirConnection -- ^ connection
                                                     -> IO VkBool32

type PFN_vkGetPhysicalDeviceMirPresentationSupportKHR =
     FunPtr HS_vkGetPhysicalDeviceMirPresentationSupportKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceMirPresentationSupportKHRUnsafe ::
               PFN_vkGetPhysicalDeviceMirPresentationSupportKHR ->
                 HS_vkGetPhysicalDeviceMirPresentationSupportKHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceMirPresentationSupportKHRSafe ::
               PFN_vkGetPhysicalDeviceMirPresentationSupportKHR ->
                 HS_vkGetPhysicalDeviceMirPresentationSupportKHR

instance VulkanProc "vkGetPhysicalDeviceMirPresentationSupportKHR"
         where
    type VkProcType "vkGetPhysicalDeviceMirPresentationSupportKHR" =
         HS_vkGetPhysicalDeviceMirPresentationSupportKHR
    vkProcSymbol = _VkGetPhysicalDeviceMirPresentationSupportKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceMirPresentationSupportKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceMirPresentationSupportKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_MIR_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_MIR_SURFACE_SPEC_VERSION = 4

type VK_KHR_MIR_SURFACE_SPEC_VERSION = 4

pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_MIR_SURFACE_EXTENSION_NAME -> True)
  where
    VK_KHR_MIR_SURFACE_EXTENSION_NAME
      = _VK_KHR_MIR_SURFACE_EXTENSION_NAME

{-# INLINE _VK_KHR_MIR_SURFACE_EXTENSION_NAME #-}

_VK_KHR_MIR_SURFACE_EXTENSION_NAME :: CString
_VK_KHR_MIR_SURFACE_EXTENSION_NAME = Ptr "VK_KHR_mir_surface\NUL"#

{-# INLINE is_VK_KHR_MIR_SURFACE_EXTENSION_NAME #-}

is_VK_KHR_MIR_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_MIR_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_MIR_SURFACE_EXTENSION_NAME

type VK_KHR_MIR_SURFACE_EXTENSION_NAME = "VK_KHR_mir_surface"

pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000007000
