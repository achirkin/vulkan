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
module Graphics.Vulkan.Ext.VK_KHR_android_surface
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
        VkXlibSurfaceCreateFlagsKHR(..), VkAndroidSurfaceCreateInfoKHR,
        VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkStructureType(..), -- > #include "vk_platform.h"
                             VkCreateAndroidSurfaceKHR,
        pattern VkCreateAndroidSurfaceKHR, HS_vkCreateAndroidSurfaceKHR,
        PFN_vkCreateAndroidSurfaceKHR, vkCreateAndroidSurfaceKHR,
        vkCreateAndroidSurfaceKHRUnsafe, vkCreateAndroidSurfaceKHRSafe,
        module Graphics.Vulkan.Marshal, _VK_MAKE_VERSION,
        _VK_VERSION_MAJOR, _VK_VERSION_MINOR, _VK_VERSION_PATCH,
        pattern VK_API_VERSION_1_0, pattern VK_API_VERSION_1_1,
        pattern VK_HEADER_VERSION, AHardwareBuffer(), ANativeWindow(),
        VK_API_VERSION_1_0, VK_API_VERSION_1_1, VK_HEADER_VERSION,
        VkInternalAllocationType(..), VkResult(..),
        VkSystemAllocationScope(..), newVkAllocationFunction,
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
        VkAllocationCallbacks,
        VkAndroidHardwareBufferFormatPropertiesANDROID,
        VkAndroidHardwareBufferPropertiesANDROID,
        VkAndroidHardwareBufferUsageANDROID, VkExternalFormatANDROID,
        VkImportAndroidHardwareBufferInfoANDROID,
        VkMemoryGetAndroidHardwareBufferInfoANDROID,
        VK_KHR_ANDROID_SURFACE_SPEC_VERSION,
        pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION,
        VK_KHR_ANDROID_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR)
       where
import           GHC.Ptr                                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Defines
import           Graphics.Vulkan.Types.Enum.InternalAllocationType
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.SystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.AllocationCallbacks
import           Graphics.Vulkan.Types.Struct.PlatformAndroidKhr
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

pattern VkCreateAndroidSurfaceKHR :: CString

pattern VkCreateAndroidSurfaceKHR <-
        (is_VkCreateAndroidSurfaceKHR -> True)
  where
    VkCreateAndroidSurfaceKHR = _VkCreateAndroidSurfaceKHR

{-# INLINE _VkCreateAndroidSurfaceKHR #-}

_VkCreateAndroidSurfaceKHR :: CString
_VkCreateAndroidSurfaceKHR = Ptr "vkCreateAndroidSurfaceKHR\NUL"#

{-# INLINE is_VkCreateAndroidSurfaceKHR #-}

is_VkCreateAndroidSurfaceKHR :: CString -> Bool
is_VkCreateAndroidSurfaceKHR
  = (EQ ==) . cmpCStrings _VkCreateAndroidSurfaceKHR

type VkCreateAndroidSurfaceKHR = "vkCreateAndroidSurfaceKHR"

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
-- > VkResult vkCreateAndroidSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkAndroidSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateAndroidSurfaceKHR vkCreateAndroidSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateAndroidSurfaceKHR <- vkGetInstanceProc @VkCreateAndroidSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateAndroidSurfaceKHR <- vkGetProc @VkCreateAndroidSurfaceKHR
--
-- __Note:__ @vkCreateAndroidSurfaceKHRUnsafe@ and @vkCreateAndroidSurfaceKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateAndroidSurfaceKHR@ is an alias
--           of @vkCreateAndroidSurfaceKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateAndroidSurfaceKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateAndroidSurfaceKHR"
               vkCreateAndroidSurfaceKHRUnsafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkAndroidSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
vkCreateAndroidSurfaceKHRUnsafe ::
                                VkInstance -- ^ instance
                                           ->
                                  Ptr VkAndroidSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                                    ->
                                    Ptr VkAllocationCallbacks -- ^ pAllocator
                                                              -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                                  -> IO VkResult
vkCreateAndroidSurfaceKHRUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkCreateAndroidSurfaceKHR)

{-# NOINLINE vkCreateAndroidSurfaceKHRUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
-- > VkResult vkCreateAndroidSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkAndroidSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateAndroidSurfaceKHR vkCreateAndroidSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateAndroidSurfaceKHR <- vkGetInstanceProc @VkCreateAndroidSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateAndroidSurfaceKHR <- vkGetProc @VkCreateAndroidSurfaceKHR
--
-- __Note:__ @vkCreateAndroidSurfaceKHRUnsafe@ and @vkCreateAndroidSurfaceKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateAndroidSurfaceKHR@ is an alias
--           of @vkCreateAndroidSurfaceKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateAndroidSurfaceKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateAndroidSurfaceKHR"
               vkCreateAndroidSurfaceKHRSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkAndroidSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
vkCreateAndroidSurfaceKHRSafe ::
                              VkInstance -- ^ instance
                                         ->
                                Ptr VkAndroidSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                                  ->
                                  Ptr VkAllocationCallbacks -- ^ pAllocator
                                                            -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                                -> IO VkResult
vkCreateAndroidSurfaceKHRSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateAndroidSurfaceKHR)

{-# NOINLINE vkCreateAndroidSurfaceKHRSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
-- > VkResult vkCreateAndroidSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkAndroidSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateAndroidSurfaceKHR vkCreateAndroidSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateAndroidSurfaceKHR <- vkGetInstanceProc @VkCreateAndroidSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateAndroidSurfaceKHR <- vkGetProc @VkCreateAndroidSurfaceKHR
--
-- __Note:__ @vkCreateAndroidSurfaceKHRUnsafe@ and @vkCreateAndroidSurfaceKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateAndroidSurfaceKHR@ is an alias
--           of @vkCreateAndroidSurfaceKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateAndroidSurfaceKHRSafe@.
--
vkCreateAndroidSurfaceKHR ::
                          VkInstance -- ^ instance
                                     ->
                            Ptr VkAndroidSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                              ->
                              Ptr VkAllocationCallbacks -- ^ pAllocator
                                                        -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                            -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkCreateAndroidSurfaceKHR = vkCreateAndroidSurfaceKHRUnsafe
#else
vkCreateAndroidSurfaceKHR = vkCreateAndroidSurfaceKHRSafe

#endif
{-# INLINE vkCreateAndroidSurfaceKHR #-}

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateAndroidSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkAndroidSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateAndroidSurfaceKHR vkCreateAndroidSurfaceKHR registry at www.khronos.org>
type HS_vkCreateAndroidSurfaceKHR =
     VkInstance -- ^ instance
                ->
       Ptr VkAndroidSurfaceCreateInfoKHR -- ^ pCreateInfo
                                         ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateAndroidSurfaceKHR =
     FunPtr HS_vkCreateAndroidSurfaceKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCreateAndroidSurfaceKHRUnsafe ::
               PFN_vkCreateAndroidSurfaceKHR -> HS_vkCreateAndroidSurfaceKHR

foreign import ccall safe "dynamic"
               unwrapVkCreateAndroidSurfaceKHRSafe ::
               PFN_vkCreateAndroidSurfaceKHR -> HS_vkCreateAndroidSurfaceKHR

instance VulkanProc "vkCreateAndroidSurfaceKHR" where
    type VkProcType "vkCreateAndroidSurfaceKHR" =
         HS_vkCreateAndroidSurfaceKHR
    vkProcSymbol = _VkCreateAndroidSurfaceKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateAndroidSurfaceKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateAndroidSurfaceKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION = 6

type VK_KHR_ANDROID_SURFACE_SPEC_VERSION = 6

pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME -> True)
  where
    VK_KHR_ANDROID_SURFACE_EXTENSION_NAME
      = _VK_KHR_ANDROID_SURFACE_EXTENSION_NAME

{-# INLINE _VK_KHR_ANDROID_SURFACE_EXTENSION_NAME #-}

_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME :: CString
_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME
  = Ptr "VK_KHR_android_surface\NUL"#

{-# INLINE is_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME #-}

is_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_ANDROID_SURFACE_EXTENSION_NAME

type VK_KHR_ANDROID_SURFACE_EXTENSION_NAME =
     "VK_KHR_android_surface"

pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000008000
