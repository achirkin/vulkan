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
module Graphics.Vulkan.Ext.VK_KHR_swapchain
       (-- * Vulkan extension: @VK_KHR_swapchain@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo,Ian Elliott @ianelliottus@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @2@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VkCreateSwapchainKHR, pattern VkCreateSwapchainKHR,
        HS_vkCreateSwapchainKHR, PFN_vkCreateSwapchainKHR,
        vkCreateSwapchainKHR, vkCreateSwapchainKHRUnsafe,
        vkCreateSwapchainKHRSafe, VkDestroySwapchainKHR,
        pattern VkDestroySwapchainKHR, HS_vkDestroySwapchainKHR,
        PFN_vkDestroySwapchainKHR, vkDestroySwapchainKHR,
        vkDestroySwapchainKHRUnsafe, vkDestroySwapchainKHRSafe,
        VkGetSwapchainImagesKHR, pattern VkGetSwapchainImagesKHR,
        HS_vkGetSwapchainImagesKHR, PFN_vkGetSwapchainImagesKHR,
        vkGetSwapchainImagesKHR, vkGetSwapchainImagesKHRUnsafe,
        vkGetSwapchainImagesKHRSafe, VkAcquireNextImageKHR,
        pattern VkAcquireNextImageKHR, HS_vkAcquireNextImageKHR,
        PFN_vkAcquireNextImageKHR, vkAcquireNextImageKHR,
        vkAcquireNextImageKHRUnsafe, vkAcquireNextImageKHRSafe,
        VkQueuePresentKHR, pattern VkQueuePresentKHR, HS_vkQueuePresentKHR,
        PFN_vkQueuePresentKHR, vkQueuePresentKHR, vkQueuePresentKHRUnsafe,
        vkQueuePresentKHRSafe, module Graphics.Vulkan.Marshal,
        VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkColorComponentBitmask(..), VkColorSpaceKHR(..),
        VkColorComponentFlagBits(), VkColorComponentFlags(),
        VkCompositeAlphaBitmaskKHR(..), VkCompositeAlphaFlagBitsKHR(),
        VkCompositeAlphaFlagsKHR(), VkFormat(..),
        VkFormatFeatureBitmask(..), VkFormatFeatureFlagBits(),
        VkFormatFeatureFlags(), VkImageAspectBitmask(..),
        VkImageCreateBitmask(..), VkImageLayout(..), VkImageTiling(..),
        VkImageType(..), VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(),
        VkInternalAllocationType(..), VkPresentModeKHR(..), VkResult(..),
        VkSharingMode(..), VkStructureType(..),
        VkSurfaceCounterBitmaskEXT(..), VkSurfaceTransformBitmaskKHR(..),
        VkSurfaceCounterFlagBitsEXT(), VkSurfaceCounterFlagsEXT(),
        VkSurfaceTransformFlagBitsKHR(), VkSurfaceTransformFlagsKHR(),
        VkSwapchainCreateBitmaskKHR(..), VkSwapchainCreateFlagBitsKHR(),
        VkSwapchainCreateFlagsKHR(), VkSystemAllocationScope(..),
        newVkAllocationFunction, newVkDebugReportCallbackEXT,
        newVkDebugUtilsMessengerCallbackEXT, newVkFreeFunction,
        newVkInternalAllocationNotification, newVkInternalFreeNotification,
        newVkReallocationFunction, newVkVoidFunction,
        unwrapVkAllocationFunction, unwrapVkDebugReportCallbackEXT,
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
        VkAllocationCallbacks, VkExtent2D, VkExtent3D, VkPresentInfoKHR,
        VkPresentRegionKHR, VkPresentRegionsKHR, VkPresentTimeGOOGLE,
        VkPresentTimesInfoGOOGLE, VkSwapchainCounterCreateInfoEXT,
        VkSwapchainCreateInfoKHR, VK_KHR_SWAPCHAIN_SPEC_VERSION,
        pattern VK_KHR_SWAPCHAIN_SPEC_VERSION,
        VK_KHR_SWAPCHAIN_EXTENSION_NAME,
        pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR,
        pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR, pattern VK_SUBOPTIMAL_KHR,
        pattern VK_ERROR_OUT_OF_DATE_KHR,
        pattern VK_OBJECT_TYPE_SWAPCHAIN_KHR, -- ** Required extensions: 'VK_KHR_surface'.
                                              VkAcquireNextImageInfoKHR,
        VkBindImageMemoryInfo, VkBindImageMemorySwapchainInfoKHR,
        VkDeviceGroupPresentCapabilitiesKHR, VkDeviceGroupPresentInfoKHR,
        VkDeviceEventTypeEXT(..), VkDeviceGroupPresentModeBitmaskKHR(..),
        VkDeviceCreateFlagBits(..), VkDeviceGroupPresentModeFlagBitsKHR(),
        VkDeviceGroupPresentModeFlagsKHR(), VkDeviceQueueCreateBitmask(..),
        VkDeviceQueueCreateFlagBits(), VkDeviceQueueCreateFlags(),
        VkDeviceGroupSwapchainCreateInfoKHR, VkImageCreateInfo,
        VkImageSwapchainCreateInfoKHR, VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(),
        -- > #include "vk_platform.h"
        VkGetDeviceGroupPresentCapabilitiesKHR,
        pattern VkGetDeviceGroupPresentCapabilitiesKHR,
        HS_vkGetDeviceGroupPresentCapabilitiesKHR,
        PFN_vkGetDeviceGroupPresentCapabilitiesKHR,
        vkGetDeviceGroupPresentCapabilitiesKHR,
        vkGetDeviceGroupPresentCapabilitiesKHRUnsafe,
        vkGetDeviceGroupPresentCapabilitiesKHRSafe,
        VkGetDeviceGroupSurfacePresentModesKHR,
        pattern VkGetDeviceGroupSurfacePresentModesKHR,
        HS_vkGetDeviceGroupSurfacePresentModesKHR,
        PFN_vkGetDeviceGroupSurfacePresentModesKHR,
        vkGetDeviceGroupSurfacePresentModesKHR,
        vkGetDeviceGroupSurfacePresentModesKHRUnsafe,
        vkGetDeviceGroupSurfacePresentModesKHRSafe,
        VkGetPhysicalDevicePresentRectanglesKHR,
        pattern VkGetPhysicalDevicePresentRectanglesKHR,
        HS_vkGetPhysicalDevicePresentRectanglesKHR,
        PFN_vkGetPhysicalDevicePresentRectanglesKHR,
        vkGetPhysicalDevicePresentRectanglesKHR,
        vkGetPhysicalDevicePresentRectanglesKHRUnsafe,
        vkGetPhysicalDevicePresentRectanglesKHRSafe,
        VkAcquireNextImage2KHR, pattern VkAcquireNextImage2KHR,
        HS_vkAcquireNextImage2KHR, PFN_vkAcquireNextImage2KHR,
        vkAcquireNextImage2KHR, vkAcquireNextImage2KHRUnsafe,
        vkAcquireNextImage2KHRSafe, VkDeviceCreateInfo,
        VkDeviceEventInfoEXT, VkDeviceGeneratedCommandsFeaturesNVX,
        VkDeviceGeneratedCommandsLimitsNVX, VkDeviceGroupBindSparseInfo,
        VkDeviceGroupBindSparseInfoKHR,
        VkDeviceGroupCommandBufferBeginInfo,
        VkDeviceGroupCommandBufferBeginInfoKHR,
        VkDeviceGroupDeviceCreateInfo, VkDeviceGroupDeviceCreateInfoKHR,
        VkDeviceGroupRenderPassBeginInfo,
        VkDeviceGroupRenderPassBeginInfoKHR, VkDeviceGroupSubmitInfo,
        VkDeviceGroupSubmitInfoKHR, VkDeviceQueueCreateInfo,
        VkDeviceQueueGlobalPriorityCreateInfoEXT, VkDeviceQueueInfo2,
        VkOffset2D, VkOffset3D, VkRect2D, VkRectLayerKHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR,
        pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR,
        pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR,
        pattern VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR)
       where
import           GHC.Ptr                                              (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.Color
import           Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR
import           Graphics.Vulkan.Types.Enum.Device
import           Graphics.Vulkan.Types.Enum.Format
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.InternalAllocationType
import           Graphics.Vulkan.Types.Enum.Object                    (VkObjectType (..))
import           Graphics.Vulkan.Types.Enum.PresentModeKHR
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.SharingMode
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.Surface
import           Graphics.Vulkan.Types.Enum.SwapchainCreateFlagsKHR
import           Graphics.Vulkan.Types.Enum.SystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.AcquireNextImageInfoKHR
import           Graphics.Vulkan.Types.Struct.AllocationCallbacks
import           Graphics.Vulkan.Types.Struct.Bind                    (VkBindImageMemoryInfo,
                                                                       VkBindImageMemorySwapchainInfoKHR)
import           Graphics.Vulkan.Types.Struct.Device
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.Image                   (VkImageCreateInfo,
                                                                       VkImageSwapchainCreateInfoKHR)
import           Graphics.Vulkan.Types.Struct.Offset
import           Graphics.Vulkan.Types.Struct.Present
import           Graphics.Vulkan.Types.Struct.Rect
import           Graphics.Vulkan.Types.Struct.SwapchainC
import           System.IO.Unsafe                                     (unsafeDupablePerformIO)

pattern VkCreateSwapchainKHR :: CString

pattern VkCreateSwapchainKHR <- (is_VkCreateSwapchainKHR -> True)
  where
    VkCreateSwapchainKHR = _VkCreateSwapchainKHR

{-# INLINE _VkCreateSwapchainKHR #-}

_VkCreateSwapchainKHR :: CString
_VkCreateSwapchainKHR = Ptr "vkCreateSwapchainKHR\NUL"#

{-# INLINE is_VkCreateSwapchainKHR #-}

is_VkCreateSwapchainKHR :: CString -> Bool
is_VkCreateSwapchainKHR
  = (EQ ==) . cmpCStrings _VkCreateSwapchainKHR

type VkCreateSwapchainKHR = "vkCreateSwapchainKHR"

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_SURFACE_LOST_KHR', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
-- > VkResult vkCreateSwapchainKHR
-- >     ( VkDevice device
-- >     , const VkSwapchainCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSwapchainKHR* pSwapchain
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSwapchainKHR vkCreateSwapchainKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSwapchainKHR <- vkGetDeviceProc @VkCreateSwapchainKHR vkDevice
--
-- or less efficient:
--
-- > myCreateSwapchainKHR <- vkGetProc @VkCreateSwapchainKHR
--
-- __Note:__ @vkCreateSwapchainKHRUnsafe@ and @vkCreateSwapchainKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateSwapchainKHR@ is an alias
--           of @vkCreateSwapchainKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateSwapchainKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateSwapchainKHR"
               vkCreateSwapchainKHRUnsafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfo
                                              ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSwapchainKHR -- ^ pSwapchain
                                                                   -> IO VkResult

#else
vkCreateSwapchainKHRUnsafe ::
                           VkDevice -- ^ device
                                    ->
                             Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfo
                                                          ->
                               Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         -> Ptr VkSwapchainKHR -- ^ pSwapchain
                                                                               -> IO VkResult
vkCreateSwapchainKHRUnsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkCreateSwapchainKHR)

{-# NOINLINE vkCreateSwapchainKHRUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_SURFACE_LOST_KHR', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
-- > VkResult vkCreateSwapchainKHR
-- >     ( VkDevice device
-- >     , const VkSwapchainCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSwapchainKHR* pSwapchain
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSwapchainKHR vkCreateSwapchainKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSwapchainKHR <- vkGetDeviceProc @VkCreateSwapchainKHR vkDevice
--
-- or less efficient:
--
-- > myCreateSwapchainKHR <- vkGetProc @VkCreateSwapchainKHR
--
-- __Note:__ @vkCreateSwapchainKHRUnsafe@ and @vkCreateSwapchainKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateSwapchainKHR@ is an alias
--           of @vkCreateSwapchainKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateSwapchainKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateSwapchainKHR"
               vkCreateSwapchainKHRSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfo
                                              ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSwapchainKHR -- ^ pSwapchain
                                                                   -> IO VkResult

#else
vkCreateSwapchainKHRSafe ::
                         VkDevice -- ^ device
                                  ->
                           Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfo
                                                        ->
                             Ptr VkAllocationCallbacks -- ^ pAllocator
                                                       -> Ptr VkSwapchainKHR -- ^ pSwapchain
                                                                             -> IO VkResult
vkCreateSwapchainKHRSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkCreateSwapchainKHR)

{-# NOINLINE vkCreateSwapchainKHRSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_SURFACE_LOST_KHR', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
-- > VkResult vkCreateSwapchainKHR
-- >     ( VkDevice device
-- >     , const VkSwapchainCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSwapchainKHR* pSwapchain
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSwapchainKHR vkCreateSwapchainKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSwapchainKHR <- vkGetDeviceProc @VkCreateSwapchainKHR vkDevice
--
-- or less efficient:
--
-- > myCreateSwapchainKHR <- vkGetProc @VkCreateSwapchainKHR
--
-- __Note:__ @vkCreateSwapchainKHRUnsafe@ and @vkCreateSwapchainKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateSwapchainKHR@ is an alias
--           of @vkCreateSwapchainKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateSwapchainKHRSafe@.
--
vkCreateSwapchainKHR ::
                     VkDevice -- ^ device
                              ->
                       Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfo
                                                    ->
                         Ptr VkAllocationCallbacks -- ^ pAllocator
                                                   -> Ptr VkSwapchainKHR -- ^ pSwapchain
                                                                         -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkCreateSwapchainKHR = vkCreateSwapchainKHRUnsafe
#else
vkCreateSwapchainKHR = vkCreateSwapchainKHRSafe

#endif
{-# INLINE vkCreateSwapchainKHR #-}

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_SURFACE_LOST_KHR', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateSwapchainKHR
--   >     ( VkDevice device
--   >     , const VkSwapchainCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSwapchainKHR* pSwapchain
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSwapchainKHR vkCreateSwapchainKHR registry at www.khronos.org>
type HS_vkCreateSwapchainKHR =
     VkDevice -- ^ device
              ->
       Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfo
                                    ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSwapchainKHR -- ^ pSwapchain
                                                         -> IO VkResult

type PFN_vkCreateSwapchainKHR = FunPtr HS_vkCreateSwapchainKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCreateSwapchainKHRUnsafe ::
               PFN_vkCreateSwapchainKHR -> HS_vkCreateSwapchainKHR

foreign import ccall safe "dynamic" unwrapVkCreateSwapchainKHRSafe
               :: PFN_vkCreateSwapchainKHR -> HS_vkCreateSwapchainKHR

instance VulkanProc "vkCreateSwapchainKHR" where
    type VkProcType "vkCreateSwapchainKHR" = HS_vkCreateSwapchainKHR
    vkProcSymbol = _VkCreateSwapchainKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateSwapchainKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateSwapchainKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkDestroySwapchainKHR :: CString

pattern VkDestroySwapchainKHR <- (is_VkDestroySwapchainKHR -> True)
  where
    VkDestroySwapchainKHR = _VkDestroySwapchainKHR

{-# INLINE _VkDestroySwapchainKHR #-}

_VkDestroySwapchainKHR :: CString
_VkDestroySwapchainKHR = Ptr "vkDestroySwapchainKHR\NUL"#

{-# INLINE is_VkDestroySwapchainKHR #-}

is_VkDestroySwapchainKHR :: CString -> Bool
is_VkDestroySwapchainKHR
  = (EQ ==) . cmpCStrings _VkDestroySwapchainKHR

type VkDestroySwapchainKHR = "vkDestroySwapchainKHR"

-- |
-- > void vkDestroySwapchainKHR
-- >     ( VkDevice device
-- >     , VkSwapchainKHR swapchain
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySwapchainKHR vkDestroySwapchainKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroySwapchainKHR <- vkGetDeviceProc @VkDestroySwapchainKHR vkDevice
--
-- or less efficient:
--
-- > myDestroySwapchainKHR <- vkGetProc @VkDestroySwapchainKHR
--
-- __Note:__ @vkDestroySwapchainKHRUnsafe@ and @vkDestroySwapchainKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkDestroySwapchainKHR@ is an alias
--           of @vkDestroySwapchainKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkDestroySwapchainKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroySwapchainKHR"
               vkDestroySwapchainKHRUnsafe ::
               VkDevice -- ^ device
                        -> VkSwapchainKHR -- ^ swapchain
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

#else
vkDestroySwapchainKHRUnsafe ::
                            VkDevice -- ^ device
                                     -> VkSwapchainKHR -- ^ swapchain
                                                       -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                    -> IO ()
vkDestroySwapchainKHRUnsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkDestroySwapchainKHR)

{-# NOINLINE vkDestroySwapchainKHRUnsafe #-}
#endif

-- |
-- > void vkDestroySwapchainKHR
-- >     ( VkDevice device
-- >     , VkSwapchainKHR swapchain
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySwapchainKHR vkDestroySwapchainKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroySwapchainKHR <- vkGetDeviceProc @VkDestroySwapchainKHR vkDevice
--
-- or less efficient:
--
-- > myDestroySwapchainKHR <- vkGetProc @VkDestroySwapchainKHR
--
-- __Note:__ @vkDestroySwapchainKHRUnsafe@ and @vkDestroySwapchainKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkDestroySwapchainKHR@ is an alias
--           of @vkDestroySwapchainKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkDestroySwapchainKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroySwapchainKHR"
               vkDestroySwapchainKHRSafe ::
               VkDevice -- ^ device
                        -> VkSwapchainKHR -- ^ swapchain
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

#else
vkDestroySwapchainKHRSafe ::
                          VkDevice -- ^ device
                                   -> VkSwapchainKHR -- ^ swapchain
                                                     -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                  -> IO ()
vkDestroySwapchainKHRSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroySwapchainKHR)

{-# NOINLINE vkDestroySwapchainKHRSafe #-}
#endif

-- |
-- > void vkDestroySwapchainKHR
-- >     ( VkDevice device
-- >     , VkSwapchainKHR swapchain
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySwapchainKHR vkDestroySwapchainKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroySwapchainKHR <- vkGetDeviceProc @VkDestroySwapchainKHR vkDevice
--
-- or less efficient:
--
-- > myDestroySwapchainKHR <- vkGetProc @VkDestroySwapchainKHR
--
-- __Note:__ @vkDestroySwapchainKHRUnsafe@ and @vkDestroySwapchainKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkDestroySwapchainKHR@ is an alias
--           of @vkDestroySwapchainKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkDestroySwapchainKHRSafe@.
--
vkDestroySwapchainKHR ::
                      VkDevice -- ^ device
                               -> VkSwapchainKHR -- ^ swapchain
                                                 -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                              -> IO ()
#ifdef UNSAFE_FFI_DEFAULT
vkDestroySwapchainKHR = vkDestroySwapchainKHRUnsafe
#else
vkDestroySwapchainKHR = vkDestroySwapchainKHRSafe

#endif
{-# INLINE vkDestroySwapchainKHR #-}

-- | > void vkDestroySwapchainKHR
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySwapchainKHR vkDestroySwapchainKHR registry at www.khronos.org>
type HS_vkDestroySwapchainKHR =
     VkDevice -- ^ device
              -> VkSwapchainKHR -- ^ swapchain
                                -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                             -> IO ()

type PFN_vkDestroySwapchainKHR = FunPtr HS_vkDestroySwapchainKHR

foreign import ccall unsafe "dynamic"
               unwrapVkDestroySwapchainKHRUnsafe ::
               PFN_vkDestroySwapchainKHR -> HS_vkDestroySwapchainKHR

foreign import ccall safe "dynamic" unwrapVkDestroySwapchainKHRSafe
               :: PFN_vkDestroySwapchainKHR -> HS_vkDestroySwapchainKHR

instance VulkanProc "vkDestroySwapchainKHR" where
    type VkProcType "vkDestroySwapchainKHR" = HS_vkDestroySwapchainKHR
    vkProcSymbol = _VkDestroySwapchainKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkDestroySwapchainKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkDestroySwapchainKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetSwapchainImagesKHR :: CString

pattern VkGetSwapchainImagesKHR <-
        (is_VkGetSwapchainImagesKHR -> True)
  where
    VkGetSwapchainImagesKHR = _VkGetSwapchainImagesKHR

{-# INLINE _VkGetSwapchainImagesKHR #-}

_VkGetSwapchainImagesKHR :: CString
_VkGetSwapchainImagesKHR = Ptr "vkGetSwapchainImagesKHR\NUL"#

{-# INLINE is_VkGetSwapchainImagesKHR #-}

is_VkGetSwapchainImagesKHR :: CString -> Bool
is_VkGetSwapchainImagesKHR
  = (EQ ==) . cmpCStrings _VkGetSwapchainImagesKHR

type VkGetSwapchainImagesKHR = "vkGetSwapchainImagesKHR"

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkGetSwapchainImagesKHR
-- >     ( VkDevice device
-- >     , VkSwapchainKHR swapchain
-- >     , uint32_t* pSwapchainImageCount
-- >     , VkImage* pSwapchainImages
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetSwapchainImagesKHR vkGetSwapchainImagesKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetSwapchainImagesKHR <- vkGetDeviceProc @VkGetSwapchainImagesKHR vkDevice
--
-- or less efficient:
--
-- > myGetSwapchainImagesKHR <- vkGetProc @VkGetSwapchainImagesKHR
--
-- __Note:__ @vkGetSwapchainImagesKHRUnsafe@ and @vkGetSwapchainImagesKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetSwapchainImagesKHR@ is an alias
--           of @vkGetSwapchainImagesKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetSwapchainImagesKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetSwapchainImagesKHR"
               vkGetSwapchainImagesKHRUnsafe ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                -> Ptr Word32 -- ^ pSwapchainImageCount
                                              -> Ptr VkImage -- ^ pSwapchainImages
                                                             -> IO VkResult

#else
vkGetSwapchainImagesKHRUnsafe ::
                              VkDevice -- ^ device
                                       ->
                                VkSwapchainKHR -- ^ swapchain
                                               -> Ptr Word32 -- ^ pSwapchainImageCount
                                                             -> Ptr VkImage -- ^ pSwapchainImages
                                                                            -> IO VkResult
vkGetSwapchainImagesKHRUnsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkGetSwapchainImagesKHR)

{-# NOINLINE vkGetSwapchainImagesKHRUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkGetSwapchainImagesKHR
-- >     ( VkDevice device
-- >     , VkSwapchainKHR swapchain
-- >     , uint32_t* pSwapchainImageCount
-- >     , VkImage* pSwapchainImages
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetSwapchainImagesKHR vkGetSwapchainImagesKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetSwapchainImagesKHR <- vkGetDeviceProc @VkGetSwapchainImagesKHR vkDevice
--
-- or less efficient:
--
-- > myGetSwapchainImagesKHR <- vkGetProc @VkGetSwapchainImagesKHR
--
-- __Note:__ @vkGetSwapchainImagesKHRUnsafe@ and @vkGetSwapchainImagesKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetSwapchainImagesKHR@ is an alias
--           of @vkGetSwapchainImagesKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetSwapchainImagesKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetSwapchainImagesKHR"
               vkGetSwapchainImagesKHRSafe ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                -> Ptr Word32 -- ^ pSwapchainImageCount
                                              -> Ptr VkImage -- ^ pSwapchainImages
                                                             -> IO VkResult

#else
vkGetSwapchainImagesKHRSafe ::
                            VkDevice -- ^ device
                                     ->
                              VkSwapchainKHR -- ^ swapchain
                                             -> Ptr Word32 -- ^ pSwapchainImageCount
                                                           -> Ptr VkImage -- ^ pSwapchainImages
                                                                          -> IO VkResult
vkGetSwapchainImagesKHRSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkGetSwapchainImagesKHR)

{-# NOINLINE vkGetSwapchainImagesKHRSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkGetSwapchainImagesKHR
-- >     ( VkDevice device
-- >     , VkSwapchainKHR swapchain
-- >     , uint32_t* pSwapchainImageCount
-- >     , VkImage* pSwapchainImages
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetSwapchainImagesKHR vkGetSwapchainImagesKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetSwapchainImagesKHR <- vkGetDeviceProc @VkGetSwapchainImagesKHR vkDevice
--
-- or less efficient:
--
-- > myGetSwapchainImagesKHR <- vkGetProc @VkGetSwapchainImagesKHR
--
-- __Note:__ @vkGetSwapchainImagesKHRUnsafe@ and @vkGetSwapchainImagesKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetSwapchainImagesKHR@ is an alias
--           of @vkGetSwapchainImagesKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetSwapchainImagesKHRSafe@.
--
vkGetSwapchainImagesKHR ::
                        VkDevice -- ^ device
                                 ->
                          VkSwapchainKHR -- ^ swapchain
                                         -> Ptr Word32 -- ^ pSwapchainImageCount
                                                       -> Ptr VkImage -- ^ pSwapchainImages
                                                                      -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkGetSwapchainImagesKHR = vkGetSwapchainImagesKHRUnsafe
#else
vkGetSwapchainImagesKHR = vkGetSwapchainImagesKHRSafe

#endif
{-# INLINE vkGetSwapchainImagesKHR #-}

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetSwapchainImagesKHR
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , uint32_t* pSwapchainImageCount
--   >     , VkImage* pSwapchainImages
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetSwapchainImagesKHR vkGetSwapchainImagesKHR registry at www.khronos.org>
type HS_vkGetSwapchainImagesKHR =
     VkDevice -- ^ device
              ->
       VkSwapchainKHR -- ^ swapchain
                      -> Ptr Word32 -- ^ pSwapchainImageCount
                                    -> Ptr VkImage -- ^ pSwapchainImages
                                                   -> IO VkResult

type PFN_vkGetSwapchainImagesKHR =
     FunPtr HS_vkGetSwapchainImagesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetSwapchainImagesKHRUnsafe ::
               PFN_vkGetSwapchainImagesKHR -> HS_vkGetSwapchainImagesKHR

foreign import ccall safe "dynamic"
               unwrapVkGetSwapchainImagesKHRSafe ::
               PFN_vkGetSwapchainImagesKHR -> HS_vkGetSwapchainImagesKHR

instance VulkanProc "vkGetSwapchainImagesKHR" where
    type VkProcType "vkGetSwapchainImagesKHR" =
         HS_vkGetSwapchainImagesKHR
    vkProcSymbol = _VkGetSwapchainImagesKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetSwapchainImagesKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetSwapchainImagesKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkAcquireNextImageKHR :: CString

pattern VkAcquireNextImageKHR <- (is_VkAcquireNextImageKHR -> True)
  where
    VkAcquireNextImageKHR = _VkAcquireNextImageKHR

{-# INLINE _VkAcquireNextImageKHR #-}

_VkAcquireNextImageKHR :: CString
_VkAcquireNextImageKHR = Ptr "vkAcquireNextImageKHR\NUL"#

{-# INLINE is_VkAcquireNextImageKHR #-}

is_VkAcquireNextImageKHR :: CString -> Bool
is_VkAcquireNextImageKHR
  = (EQ ==) . cmpCStrings _VkAcquireNextImageKHR

type VkAcquireNextImageKHR = "vkAcquireNextImageKHR"

-- |
-- Success codes: 'VK_SUCCESS', 'VK_TIMEOUT', 'VK_NOT_READY', 'VK_SUBOPTIMAL_KHR'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkAcquireNextImageKHR
-- >     ( VkDevice device
-- >     , VkSwapchainKHR swapchain
-- >     , uint64_t timeout
-- >     , VkSemaphore semaphore
-- >     , VkFence fence
-- >     , uint32_t* pImageIndex
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAcquireNextImageKHR vkAcquireNextImageKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myAcquireNextImageKHR <- vkGetDeviceProc @VkAcquireNextImageKHR vkDevice
--
-- or less efficient:
--
-- > myAcquireNextImageKHR <- vkGetProc @VkAcquireNextImageKHR
--
-- __Note:__ @vkAcquireNextImageKHRUnsafe@ and @vkAcquireNextImageKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkAcquireNextImageKHR@ is an alias
--           of @vkAcquireNextImageKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkAcquireNextImageKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkAcquireNextImageKHR"
               vkAcquireNextImageKHRUnsafe ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                ->
                   Word64 -- ^ timeout
                          -> VkSemaphore -- ^ semaphore
                                         -> VkFence -- ^ fence
                                                    -> Ptr Word32 -- ^ pImageIndex
                                                                  -> IO VkResult

#else
vkAcquireNextImageKHRUnsafe ::
                            VkDevice -- ^ device
                                     ->
                              VkSwapchainKHR -- ^ swapchain
                                             ->
                                Word64 -- ^ timeout
                                       -> VkSemaphore -- ^ semaphore
                                                      -> VkFence -- ^ fence
                                                                 -> Ptr Word32 -- ^ pImageIndex
                                                                               -> IO VkResult
vkAcquireNextImageKHRUnsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkAcquireNextImageKHR)

{-# NOINLINE vkAcquireNextImageKHRUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_TIMEOUT', 'VK_NOT_READY', 'VK_SUBOPTIMAL_KHR'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkAcquireNextImageKHR
-- >     ( VkDevice device
-- >     , VkSwapchainKHR swapchain
-- >     , uint64_t timeout
-- >     , VkSemaphore semaphore
-- >     , VkFence fence
-- >     , uint32_t* pImageIndex
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAcquireNextImageKHR vkAcquireNextImageKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myAcquireNextImageKHR <- vkGetDeviceProc @VkAcquireNextImageKHR vkDevice
--
-- or less efficient:
--
-- > myAcquireNextImageKHR <- vkGetProc @VkAcquireNextImageKHR
--
-- __Note:__ @vkAcquireNextImageKHRUnsafe@ and @vkAcquireNextImageKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkAcquireNextImageKHR@ is an alias
--           of @vkAcquireNextImageKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkAcquireNextImageKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkAcquireNextImageKHR"
               vkAcquireNextImageKHRSafe ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                ->
                   Word64 -- ^ timeout
                          -> VkSemaphore -- ^ semaphore
                                         -> VkFence -- ^ fence
                                                    -> Ptr Word32 -- ^ pImageIndex
                                                                  -> IO VkResult

#else
vkAcquireNextImageKHRSafe ::
                          VkDevice -- ^ device
                                   ->
                            VkSwapchainKHR -- ^ swapchain
                                           ->
                              Word64 -- ^ timeout
                                     -> VkSemaphore -- ^ semaphore
                                                    -> VkFence -- ^ fence
                                                               -> Ptr Word32 -- ^ pImageIndex
                                                                             -> IO VkResult
vkAcquireNextImageKHRSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkAcquireNextImageKHR)

{-# NOINLINE vkAcquireNextImageKHRSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_TIMEOUT', 'VK_NOT_READY', 'VK_SUBOPTIMAL_KHR'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkAcquireNextImageKHR
-- >     ( VkDevice device
-- >     , VkSwapchainKHR swapchain
-- >     , uint64_t timeout
-- >     , VkSemaphore semaphore
-- >     , VkFence fence
-- >     , uint32_t* pImageIndex
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAcquireNextImageKHR vkAcquireNextImageKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myAcquireNextImageKHR <- vkGetDeviceProc @VkAcquireNextImageKHR vkDevice
--
-- or less efficient:
--
-- > myAcquireNextImageKHR <- vkGetProc @VkAcquireNextImageKHR
--
-- __Note:__ @vkAcquireNextImageKHRUnsafe@ and @vkAcquireNextImageKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkAcquireNextImageKHR@ is an alias
--           of @vkAcquireNextImageKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkAcquireNextImageKHRSafe@.
--
vkAcquireNextImageKHR ::
                      VkDevice -- ^ device
                               ->
                        VkSwapchainKHR -- ^ swapchain
                                       ->
                          Word64 -- ^ timeout
                                 -> VkSemaphore -- ^ semaphore
                                                -> VkFence -- ^ fence
                                                           -> Ptr Word32 -- ^ pImageIndex
                                                                         -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkAcquireNextImageKHR = vkAcquireNextImageKHRUnsafe
#else
vkAcquireNextImageKHR = vkAcquireNextImageKHRSafe

#endif
{-# INLINE vkAcquireNextImageKHR #-}

-- | Success codes: 'VK_SUCCESS', 'VK_TIMEOUT', 'VK_NOT_READY', 'VK_SUBOPTIMAL_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkAcquireNextImageKHR
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , uint64_t timeout
--   >     , VkSemaphore semaphore
--   >     , VkFence fence
--   >     , uint32_t* pImageIndex
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAcquireNextImageKHR vkAcquireNextImageKHR registry at www.khronos.org>
type HS_vkAcquireNextImageKHR =
     VkDevice -- ^ device
              ->
       VkSwapchainKHR -- ^ swapchain
                      ->
         Word64 -- ^ timeout
                -> VkSemaphore -- ^ semaphore
                               -> VkFence -- ^ fence
                                          -> Ptr Word32 -- ^ pImageIndex
                                                        -> IO VkResult

type PFN_vkAcquireNextImageKHR = FunPtr HS_vkAcquireNextImageKHR

foreign import ccall unsafe "dynamic"
               unwrapVkAcquireNextImageKHRUnsafe ::
               PFN_vkAcquireNextImageKHR -> HS_vkAcquireNextImageKHR

foreign import ccall safe "dynamic" unwrapVkAcquireNextImageKHRSafe
               :: PFN_vkAcquireNextImageKHR -> HS_vkAcquireNextImageKHR

instance VulkanProc "vkAcquireNextImageKHR" where
    type VkProcType "vkAcquireNextImageKHR" = HS_vkAcquireNextImageKHR
    vkProcSymbol = _VkAcquireNextImageKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkAcquireNextImageKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkAcquireNextImageKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkQueuePresentKHR :: CString

pattern VkQueuePresentKHR <- (is_VkQueuePresentKHR -> True)
  where
    VkQueuePresentKHR = _VkQueuePresentKHR

{-# INLINE _VkQueuePresentKHR #-}

_VkQueuePresentKHR :: CString
_VkQueuePresentKHR = Ptr "vkQueuePresentKHR\NUL"#

{-# INLINE is_VkQueuePresentKHR #-}

is_VkQueuePresentKHR :: CString -> Bool
is_VkQueuePresentKHR = (EQ ==) . cmpCStrings _VkQueuePresentKHR

type VkQueuePresentKHR = "vkQueuePresentKHR"

-- |
-- Success codes: 'VK_SUCCESS', 'VK_SUBOPTIMAL_KHR'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkQueuePresentKHR
-- >     ( VkQueue queue
-- >     , const VkPresentInfoKHR* pPresentInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkQueuePresentKHR vkQueuePresentKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myQueuePresentKHR <- vkGetInstanceProc @VkQueuePresentKHR vkInstance
--
-- or less efficient:
--
-- > myQueuePresentKHR <- vkGetProc @VkQueuePresentKHR
--
-- __Note:__ @vkQueuePresentKHRUnsafe@ and @vkQueuePresentKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkQueuePresentKHR@ is an alias
--           of @vkQueuePresentKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkQueuePresentKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkQueuePresentKHR"
               vkQueuePresentKHRUnsafe ::
               VkQueue -- ^ queue
                       -> Ptr VkPresentInfoKHR -- ^ pPresentInfo
                                               -> IO VkResult

#else
vkQueuePresentKHRUnsafe ::
                        VkQueue -- ^ queue
                                -> Ptr VkPresentInfoKHR -- ^ pPresentInfo
                                                        -> IO VkResult
vkQueuePresentKHRUnsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkQueuePresentKHR)

{-# NOINLINE vkQueuePresentKHRUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_SUBOPTIMAL_KHR'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkQueuePresentKHR
-- >     ( VkQueue queue
-- >     , const VkPresentInfoKHR* pPresentInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkQueuePresentKHR vkQueuePresentKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myQueuePresentKHR <- vkGetInstanceProc @VkQueuePresentKHR vkInstance
--
-- or less efficient:
--
-- > myQueuePresentKHR <- vkGetProc @VkQueuePresentKHR
--
-- __Note:__ @vkQueuePresentKHRUnsafe@ and @vkQueuePresentKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkQueuePresentKHR@ is an alias
--           of @vkQueuePresentKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkQueuePresentKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkQueuePresentKHR" vkQueuePresentKHRSafe
               :: VkQueue -- ^ queue
                          -> Ptr VkPresentInfoKHR -- ^ pPresentInfo
                                                  -> IO VkResult

#else
vkQueuePresentKHRSafe ::
                      VkQueue -- ^ queue
                              -> Ptr VkPresentInfoKHR -- ^ pPresentInfo
                                                      -> IO VkResult
vkQueuePresentKHRSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkQueuePresentKHR)

{-# NOINLINE vkQueuePresentKHRSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_SUBOPTIMAL_KHR'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkQueuePresentKHR
-- >     ( VkQueue queue
-- >     , const VkPresentInfoKHR* pPresentInfo
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkQueuePresentKHR vkQueuePresentKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myQueuePresentKHR <- vkGetInstanceProc @VkQueuePresentKHR vkInstance
--
-- or less efficient:
--
-- > myQueuePresentKHR <- vkGetProc @VkQueuePresentKHR
--
-- __Note:__ @vkQueuePresentKHRUnsafe@ and @vkQueuePresentKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkQueuePresentKHR@ is an alias
--           of @vkQueuePresentKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkQueuePresentKHRSafe@.
--
vkQueuePresentKHR :: VkQueue -- ^ queue
                             -> Ptr VkPresentInfoKHR -- ^ pPresentInfo
                                                     -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkQueuePresentKHR = vkQueuePresentKHRUnsafe
#else
vkQueuePresentKHR = vkQueuePresentKHRSafe

#endif
{-# INLINE vkQueuePresentKHR #-}

-- | Success codes: 'VK_SUCCESS', 'VK_SUBOPTIMAL_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkQueuePresentKHR
--   >     ( VkQueue queue
--   >     , const VkPresentInfoKHR* pPresentInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkQueuePresentKHR vkQueuePresentKHR registry at www.khronos.org>
type HS_vkQueuePresentKHR =
     VkQueue -- ^ queue
             -> Ptr VkPresentInfoKHR -- ^ pPresentInfo
                                     -> IO VkResult

type PFN_vkQueuePresentKHR = FunPtr HS_vkQueuePresentKHR

foreign import ccall unsafe "dynamic" unwrapVkQueuePresentKHRUnsafe
               :: PFN_vkQueuePresentKHR -> HS_vkQueuePresentKHR

foreign import ccall safe "dynamic" unwrapVkQueuePresentKHRSafe ::
               PFN_vkQueuePresentKHR -> HS_vkQueuePresentKHR

instance VulkanProc "vkQueuePresentKHR" where
    type VkProcType "vkQueuePresentKHR" = HS_vkQueuePresentKHR
    vkProcSymbol = _VkQueuePresentKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkQueuePresentKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkQueuePresentKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_SWAPCHAIN_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_SWAPCHAIN_SPEC_VERSION = 70

type VK_KHR_SWAPCHAIN_SPEC_VERSION = 70

pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME :: CString

pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME <-
        (is_VK_KHR_SWAPCHAIN_EXTENSION_NAME -> True)
  where
    VK_KHR_SWAPCHAIN_EXTENSION_NAME = _VK_KHR_SWAPCHAIN_EXTENSION_NAME

{-# INLINE _VK_KHR_SWAPCHAIN_EXTENSION_NAME #-}

_VK_KHR_SWAPCHAIN_EXTENSION_NAME :: CString
_VK_KHR_SWAPCHAIN_EXTENSION_NAME = Ptr "VK_KHR_swapchain\NUL"#

{-# INLINE is_VK_KHR_SWAPCHAIN_EXTENSION_NAME #-}

is_VK_KHR_SWAPCHAIN_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_SWAPCHAIN_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_SWAPCHAIN_EXTENSION_NAME

type VK_KHR_SWAPCHAIN_EXTENSION_NAME = "VK_KHR_swapchain"

pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR =
        VkStructureType 1000001000

pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR :: VkStructureType

pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR =
        VkStructureType 1000001001

pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR :: VkImageLayout

pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR = VkImageLayout 1000001002

pattern VK_SUBOPTIMAL_KHR :: VkResult

pattern VK_SUBOPTIMAL_KHR = VkResult 1000001003

pattern VK_ERROR_OUT_OF_DATE_KHR :: VkResult

pattern VK_ERROR_OUT_OF_DATE_KHR = VkResult (-1000001004)

-- | VkSwapchainKHR
pattern VK_OBJECT_TYPE_SWAPCHAIN_KHR :: VkObjectType

pattern VK_OBJECT_TYPE_SWAPCHAIN_KHR = VkObjectType 1000001000

pattern VkGetDeviceGroupPresentCapabilitiesKHR :: CString

pattern VkGetDeviceGroupPresentCapabilitiesKHR <-
        (is_VkGetDeviceGroupPresentCapabilitiesKHR -> True)
  where
    VkGetDeviceGroupPresentCapabilitiesKHR
      = _VkGetDeviceGroupPresentCapabilitiesKHR

{-# INLINE _VkGetDeviceGroupPresentCapabilitiesKHR #-}

_VkGetDeviceGroupPresentCapabilitiesKHR :: CString
_VkGetDeviceGroupPresentCapabilitiesKHR
  = Ptr "vkGetDeviceGroupPresentCapabilitiesKHR\NUL"#

{-# INLINE is_VkGetDeviceGroupPresentCapabilitiesKHR #-}

is_VkGetDeviceGroupPresentCapabilitiesKHR :: CString -> Bool
is_VkGetDeviceGroupPresentCapabilitiesKHR
  = (EQ ==) . cmpCStrings _VkGetDeviceGroupPresentCapabilitiesKHR

type VkGetDeviceGroupPresentCapabilitiesKHR =
     "vkGetDeviceGroupPresentCapabilitiesKHR"

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkGetDeviceGroupPresentCapabilitiesKHR
-- >     ( VkDevice device
-- >     , VkDeviceGroupPresentCapabilitiesKHR* pDeviceGroupPresentCapabilities
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceGroupPresentCapabilitiesKHR vkGetDeviceGroupPresentCapabilitiesKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceGroupPresentCapabilitiesKHR <- vkGetDeviceProc @VkGetDeviceGroupPresentCapabilitiesKHR vkDevice
--
-- or less efficient:
--
-- > myGetDeviceGroupPresentCapabilitiesKHR <- vkGetProc @VkGetDeviceGroupPresentCapabilitiesKHR
--
-- __Note:__ @vkGetDeviceGroupPresentCapabilitiesKHRUnsafe@ and @vkGetDeviceGroupPresentCapabilitiesKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDeviceGroupPresentCapabilitiesKHR@ is an alias
--           of @vkGetDeviceGroupPresentCapabilitiesKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDeviceGroupPresentCapabilitiesKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe
               "vkGetDeviceGroupPresentCapabilitiesKHR"
               vkGetDeviceGroupPresentCapabilitiesKHRUnsafe ::
               VkDevice -- ^ device
                        -> Ptr VkDeviceGroupPresentCapabilitiesKHR -- ^ pDeviceGroupPresentCapabilities
                                                                   -> IO VkResult

#else
vkGetDeviceGroupPresentCapabilitiesKHRUnsafe ::
                                             VkDevice -- ^ device
                                                      ->
                                               Ptr VkDeviceGroupPresentCapabilitiesKHR -- ^ pDeviceGroupPresentCapabilities
                                                                                       ->
                                                 IO VkResult
vkGetDeviceGroupPresentCapabilitiesKHRUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetDeviceGroupPresentCapabilitiesKHR)

{-# NOINLINE vkGetDeviceGroupPresentCapabilitiesKHRUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkGetDeviceGroupPresentCapabilitiesKHR
-- >     ( VkDevice device
-- >     , VkDeviceGroupPresentCapabilitiesKHR* pDeviceGroupPresentCapabilities
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceGroupPresentCapabilitiesKHR vkGetDeviceGroupPresentCapabilitiesKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceGroupPresentCapabilitiesKHR <- vkGetDeviceProc @VkGetDeviceGroupPresentCapabilitiesKHR vkDevice
--
-- or less efficient:
--
-- > myGetDeviceGroupPresentCapabilitiesKHR <- vkGetProc @VkGetDeviceGroupPresentCapabilitiesKHR
--
-- __Note:__ @vkGetDeviceGroupPresentCapabilitiesKHRUnsafe@ and @vkGetDeviceGroupPresentCapabilitiesKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDeviceGroupPresentCapabilitiesKHR@ is an alias
--           of @vkGetDeviceGroupPresentCapabilitiesKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDeviceGroupPresentCapabilitiesKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkGetDeviceGroupPresentCapabilitiesKHR"
               vkGetDeviceGroupPresentCapabilitiesKHRSafe ::
               VkDevice -- ^ device
                        -> Ptr VkDeviceGroupPresentCapabilitiesKHR -- ^ pDeviceGroupPresentCapabilities
                                                                   -> IO VkResult

#else
vkGetDeviceGroupPresentCapabilitiesKHRSafe ::
                                           VkDevice -- ^ device
                                                    ->
                                             Ptr VkDeviceGroupPresentCapabilitiesKHR -- ^ pDeviceGroupPresentCapabilities
                                                                                     -> IO VkResult
vkGetDeviceGroupPresentCapabilitiesKHRSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetDeviceGroupPresentCapabilitiesKHR)

{-# NOINLINE vkGetDeviceGroupPresentCapabilitiesKHRSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkGetDeviceGroupPresentCapabilitiesKHR
-- >     ( VkDevice device
-- >     , VkDeviceGroupPresentCapabilitiesKHR* pDeviceGroupPresentCapabilities
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceGroupPresentCapabilitiesKHR vkGetDeviceGroupPresentCapabilitiesKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceGroupPresentCapabilitiesKHR <- vkGetDeviceProc @VkGetDeviceGroupPresentCapabilitiesKHR vkDevice
--
-- or less efficient:
--
-- > myGetDeviceGroupPresentCapabilitiesKHR <- vkGetProc @VkGetDeviceGroupPresentCapabilitiesKHR
--
-- __Note:__ @vkGetDeviceGroupPresentCapabilitiesKHRUnsafe@ and @vkGetDeviceGroupPresentCapabilitiesKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDeviceGroupPresentCapabilitiesKHR@ is an alias
--           of @vkGetDeviceGroupPresentCapabilitiesKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDeviceGroupPresentCapabilitiesKHRSafe@.
--
vkGetDeviceGroupPresentCapabilitiesKHR ::
                                       VkDevice -- ^ device
                                                ->
                                         Ptr VkDeviceGroupPresentCapabilitiesKHR -- ^ pDeviceGroupPresentCapabilities
                                                                                 -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkGetDeviceGroupPresentCapabilitiesKHR
  = vkGetDeviceGroupPresentCapabilitiesKHRUnsafe
#else
vkGetDeviceGroupPresentCapabilitiesKHR
  = vkGetDeviceGroupPresentCapabilitiesKHRSafe

#endif
{-# INLINE vkGetDeviceGroupPresentCapabilitiesKHR #-}

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDeviceGroupPresentCapabilitiesKHR
--   >     ( VkDevice device
--   >     , VkDeviceGroupPresentCapabilitiesKHR* pDeviceGroupPresentCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceGroupPresentCapabilitiesKHR vkGetDeviceGroupPresentCapabilitiesKHR registry at www.khronos.org>
type HS_vkGetDeviceGroupPresentCapabilitiesKHR =
     VkDevice -- ^ device
              -> Ptr VkDeviceGroupPresentCapabilitiesKHR -- ^ pDeviceGroupPresentCapabilities
                                                         -> IO VkResult

type PFN_vkGetDeviceGroupPresentCapabilitiesKHR =
     FunPtr HS_vkGetDeviceGroupPresentCapabilitiesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetDeviceGroupPresentCapabilitiesKHRUnsafe ::
               PFN_vkGetDeviceGroupPresentCapabilitiesKHR ->
                 HS_vkGetDeviceGroupPresentCapabilitiesKHR

foreign import ccall safe "dynamic"
               unwrapVkGetDeviceGroupPresentCapabilitiesKHRSafe ::
               PFN_vkGetDeviceGroupPresentCapabilitiesKHR ->
                 HS_vkGetDeviceGroupPresentCapabilitiesKHR

instance VulkanProc "vkGetDeviceGroupPresentCapabilitiesKHR" where
    type VkProcType "vkGetDeviceGroupPresentCapabilitiesKHR" =
         HS_vkGetDeviceGroupPresentCapabilitiesKHR
    vkProcSymbol = _VkGetDeviceGroupPresentCapabilitiesKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetDeviceGroupPresentCapabilitiesKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetDeviceGroupPresentCapabilitiesKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetDeviceGroupSurfacePresentModesKHR :: CString

pattern VkGetDeviceGroupSurfacePresentModesKHR <-
        (is_VkGetDeviceGroupSurfacePresentModesKHR -> True)
  where
    VkGetDeviceGroupSurfacePresentModesKHR
      = _VkGetDeviceGroupSurfacePresentModesKHR

{-# INLINE _VkGetDeviceGroupSurfacePresentModesKHR #-}

_VkGetDeviceGroupSurfacePresentModesKHR :: CString
_VkGetDeviceGroupSurfacePresentModesKHR
  = Ptr "vkGetDeviceGroupSurfacePresentModesKHR\NUL"#

{-# INLINE is_VkGetDeviceGroupSurfacePresentModesKHR #-}

is_VkGetDeviceGroupSurfacePresentModesKHR :: CString -> Bool
is_VkGetDeviceGroupSurfacePresentModesKHR
  = (EQ ==) . cmpCStrings _VkGetDeviceGroupSurfacePresentModesKHR

type VkGetDeviceGroupSurfacePresentModesKHR =
     "vkGetDeviceGroupSurfacePresentModesKHR"

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkGetDeviceGroupSurfacePresentModesKHR
-- >     ( VkDevice device
-- >     , VkSurfaceKHR surface
-- >     , VkDeviceGroupPresentModeFlagsKHR* pModes
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceGroupSurfacePresentModesKHR vkGetDeviceGroupSurfacePresentModesKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceGroupSurfacePresentModesKHR <- vkGetDeviceProc @VkGetDeviceGroupSurfacePresentModesKHR vkDevice
--
-- or less efficient:
--
-- > myGetDeviceGroupSurfacePresentModesKHR <- vkGetProc @VkGetDeviceGroupSurfacePresentModesKHR
--
-- __Note:__ @vkGetDeviceGroupSurfacePresentModesKHRUnsafe@ and @vkGetDeviceGroupSurfacePresentModesKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDeviceGroupSurfacePresentModesKHR@ is an alias
--           of @vkGetDeviceGroupSurfacePresentModesKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDeviceGroupSurfacePresentModesKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe
               "vkGetDeviceGroupSurfacePresentModesKHR"
               vkGetDeviceGroupSurfacePresentModesKHRUnsafe ::
               VkDevice -- ^ device
                        ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr VkDeviceGroupPresentModeFlagsKHR -- ^ pModes
                                                                      -> IO VkResult

#else
vkGetDeviceGroupSurfacePresentModesKHRUnsafe ::
                                             VkDevice -- ^ device
                                                      ->
                                               VkSurfaceKHR -- ^ surface
                                                            ->
                                                 Ptr VkDeviceGroupPresentModeFlagsKHR -- ^ pModes
                                                                                      -> IO VkResult
vkGetDeviceGroupSurfacePresentModesKHRUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetDeviceGroupSurfacePresentModesKHR)

{-# NOINLINE vkGetDeviceGroupSurfacePresentModesKHRUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkGetDeviceGroupSurfacePresentModesKHR
-- >     ( VkDevice device
-- >     , VkSurfaceKHR surface
-- >     , VkDeviceGroupPresentModeFlagsKHR* pModes
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceGroupSurfacePresentModesKHR vkGetDeviceGroupSurfacePresentModesKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceGroupSurfacePresentModesKHR <- vkGetDeviceProc @VkGetDeviceGroupSurfacePresentModesKHR vkDevice
--
-- or less efficient:
--
-- > myGetDeviceGroupSurfacePresentModesKHR <- vkGetProc @VkGetDeviceGroupSurfacePresentModesKHR
--
-- __Note:__ @vkGetDeviceGroupSurfacePresentModesKHRUnsafe@ and @vkGetDeviceGroupSurfacePresentModesKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDeviceGroupSurfacePresentModesKHR@ is an alias
--           of @vkGetDeviceGroupSurfacePresentModesKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDeviceGroupSurfacePresentModesKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkGetDeviceGroupSurfacePresentModesKHR"
               vkGetDeviceGroupSurfacePresentModesKHRSafe ::
               VkDevice -- ^ device
                        ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr VkDeviceGroupPresentModeFlagsKHR -- ^ pModes
                                                                      -> IO VkResult

#else
vkGetDeviceGroupSurfacePresentModesKHRSafe ::
                                           VkDevice -- ^ device
                                                    ->
                                             VkSurfaceKHR -- ^ surface
                                                          ->
                                               Ptr VkDeviceGroupPresentModeFlagsKHR -- ^ pModes
                                                                                    -> IO VkResult
vkGetDeviceGroupSurfacePresentModesKHRSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetDeviceGroupSurfacePresentModesKHR)

{-# NOINLINE vkGetDeviceGroupSurfacePresentModesKHRSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkGetDeviceGroupSurfacePresentModesKHR
-- >     ( VkDevice device
-- >     , VkSurfaceKHR surface
-- >     , VkDeviceGroupPresentModeFlagsKHR* pModes
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceGroupSurfacePresentModesKHR vkGetDeviceGroupSurfacePresentModesKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceGroupSurfacePresentModesKHR <- vkGetDeviceProc @VkGetDeviceGroupSurfacePresentModesKHR vkDevice
--
-- or less efficient:
--
-- > myGetDeviceGroupSurfacePresentModesKHR <- vkGetProc @VkGetDeviceGroupSurfacePresentModesKHR
--
-- __Note:__ @vkGetDeviceGroupSurfacePresentModesKHRUnsafe@ and @vkGetDeviceGroupSurfacePresentModesKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetDeviceGroupSurfacePresentModesKHR@ is an alias
--           of @vkGetDeviceGroupSurfacePresentModesKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetDeviceGroupSurfacePresentModesKHRSafe@.
--
vkGetDeviceGroupSurfacePresentModesKHR ::
                                       VkDevice -- ^ device
                                                ->
                                         VkSurfaceKHR -- ^ surface
                                                      ->
                                           Ptr VkDeviceGroupPresentModeFlagsKHR -- ^ pModes
                                                                                -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkGetDeviceGroupSurfacePresentModesKHR
  = vkGetDeviceGroupSurfacePresentModesKHRUnsafe
#else
vkGetDeviceGroupSurfacePresentModesKHR
  = vkGetDeviceGroupSurfacePresentModesKHRSafe

#endif
{-# INLINE vkGetDeviceGroupSurfacePresentModesKHR #-}

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetDeviceGroupSurfacePresentModesKHR
--   >     ( VkDevice device
--   >     , VkSurfaceKHR surface
--   >     , VkDeviceGroupPresentModeFlagsKHR* pModes
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceGroupSurfacePresentModesKHR vkGetDeviceGroupSurfacePresentModesKHR registry at www.khronos.org>
type HS_vkGetDeviceGroupSurfacePresentModesKHR =
     VkDevice -- ^ device
              ->
       VkSurfaceKHR -- ^ surface
                    -> Ptr VkDeviceGroupPresentModeFlagsKHR -- ^ pModes
                                                            -> IO VkResult

type PFN_vkGetDeviceGroupSurfacePresentModesKHR =
     FunPtr HS_vkGetDeviceGroupSurfacePresentModesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetDeviceGroupSurfacePresentModesKHRUnsafe ::
               PFN_vkGetDeviceGroupSurfacePresentModesKHR ->
                 HS_vkGetDeviceGroupSurfacePresentModesKHR

foreign import ccall safe "dynamic"
               unwrapVkGetDeviceGroupSurfacePresentModesKHRSafe ::
               PFN_vkGetDeviceGroupSurfacePresentModesKHR ->
                 HS_vkGetDeviceGroupSurfacePresentModesKHR

instance VulkanProc "vkGetDeviceGroupSurfacePresentModesKHR" where
    type VkProcType "vkGetDeviceGroupSurfacePresentModesKHR" =
         HS_vkGetDeviceGroupSurfacePresentModesKHR
    vkProcSymbol = _VkGetDeviceGroupSurfacePresentModesKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetDeviceGroupSurfacePresentModesKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetDeviceGroupSurfacePresentModesKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDevicePresentRectanglesKHR :: CString

pattern VkGetPhysicalDevicePresentRectanglesKHR <-
        (is_VkGetPhysicalDevicePresentRectanglesKHR -> True)
  where
    VkGetPhysicalDevicePresentRectanglesKHR
      = _VkGetPhysicalDevicePresentRectanglesKHR

{-# INLINE _VkGetPhysicalDevicePresentRectanglesKHR #-}

_VkGetPhysicalDevicePresentRectanglesKHR :: CString
_VkGetPhysicalDevicePresentRectanglesKHR
  = Ptr "vkGetPhysicalDevicePresentRectanglesKHR\NUL"#

{-# INLINE is_VkGetPhysicalDevicePresentRectanglesKHR #-}

is_VkGetPhysicalDevicePresentRectanglesKHR :: CString -> Bool
is_VkGetPhysicalDevicePresentRectanglesKHR
  = (EQ ==) . cmpCStrings _VkGetPhysicalDevicePresentRectanglesKHR

type VkGetPhysicalDevicePresentRectanglesKHR =
     "vkGetPhysicalDevicePresentRectanglesKHR"

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkGetPhysicalDevicePresentRectanglesKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkSurfaceKHR surface
-- >     , uint32_t* pRectCount
-- >     , VkRect2D* pRects
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDevicePresentRectanglesKHR vkGetPhysicalDevicePresentRectanglesKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDevicePresentRectanglesKHR <- vkGetInstanceProc @VkGetPhysicalDevicePresentRectanglesKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDevicePresentRectanglesKHR <- vkGetProc @VkGetPhysicalDevicePresentRectanglesKHR
--
-- __Note:__ @vkGetPhysicalDevicePresentRectanglesKHRUnsafe@ and @vkGetPhysicalDevicePresentRectanglesKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDevicePresentRectanglesKHR@ is an alias
--           of @vkGetPhysicalDevicePresentRectanglesKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDevicePresentRectanglesKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe
               "vkGetPhysicalDevicePresentRectanglesKHR"
               vkGetPhysicalDevicePresentRectanglesKHRUnsafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr Word32 -- ^ pRectCount
                                            -> Ptr VkRect2D -- ^ pRects
                                                            -> IO VkResult

#else
vkGetPhysicalDevicePresentRectanglesKHRUnsafe ::
                                              VkPhysicalDevice -- ^ physicalDevice
                                                               ->
                                                VkSurfaceKHR -- ^ surface
                                                             ->
                                                  Ptr Word32 -- ^ pRectCount
                                                             -> Ptr VkRect2D -- ^ pRects
                                                                             -> IO VkResult
vkGetPhysicalDevicePresentRectanglesKHRUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkGetPhysicalDevicePresentRectanglesKHR)

{-# NOINLINE vkGetPhysicalDevicePresentRectanglesKHRUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkGetPhysicalDevicePresentRectanglesKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkSurfaceKHR surface
-- >     , uint32_t* pRectCount
-- >     , VkRect2D* pRects
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDevicePresentRectanglesKHR vkGetPhysicalDevicePresentRectanglesKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDevicePresentRectanglesKHR <- vkGetInstanceProc @VkGetPhysicalDevicePresentRectanglesKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDevicePresentRectanglesKHR <- vkGetProc @VkGetPhysicalDevicePresentRectanglesKHR
--
-- __Note:__ @vkGetPhysicalDevicePresentRectanglesKHRUnsafe@ and @vkGetPhysicalDevicePresentRectanglesKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDevicePresentRectanglesKHR@ is an alias
--           of @vkGetPhysicalDevicePresentRectanglesKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDevicePresentRectanglesKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkGetPhysicalDevicePresentRectanglesKHR"
               vkGetPhysicalDevicePresentRectanglesKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr Word32 -- ^ pRectCount
                                            -> Ptr VkRect2D -- ^ pRects
                                                            -> IO VkResult

#else
vkGetPhysicalDevicePresentRectanglesKHRSafe ::
                                            VkPhysicalDevice -- ^ physicalDevice
                                                             ->
                                              VkSurfaceKHR -- ^ surface
                                                           ->
                                                Ptr Word32 -- ^ pRectCount
                                                           -> Ptr VkRect2D -- ^ pRects
                                                                           -> IO VkResult
vkGetPhysicalDevicePresentRectanglesKHRSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDevicePresentRectanglesKHR)

{-# NOINLINE vkGetPhysicalDevicePresentRectanglesKHRSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkGetPhysicalDevicePresentRectanglesKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkSurfaceKHR surface
-- >     , uint32_t* pRectCount
-- >     , VkRect2D* pRects
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDevicePresentRectanglesKHR vkGetPhysicalDevicePresentRectanglesKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDevicePresentRectanglesKHR <- vkGetInstanceProc @VkGetPhysicalDevicePresentRectanglesKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDevicePresentRectanglesKHR <- vkGetProc @VkGetPhysicalDevicePresentRectanglesKHR
--
-- __Note:__ @vkGetPhysicalDevicePresentRectanglesKHRUnsafe@ and @vkGetPhysicalDevicePresentRectanglesKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkGetPhysicalDevicePresentRectanglesKHR@ is an alias
--           of @vkGetPhysicalDevicePresentRectanglesKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkGetPhysicalDevicePresentRectanglesKHRSafe@.
--
vkGetPhysicalDevicePresentRectanglesKHR ::
                                        VkPhysicalDevice -- ^ physicalDevice
                                                         ->
                                          VkSurfaceKHR -- ^ surface
                                                       -> Ptr Word32 -- ^ pRectCount
                                                                     -> Ptr VkRect2D -- ^ pRects
                                                                                     -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkGetPhysicalDevicePresentRectanglesKHR
  = vkGetPhysicalDevicePresentRectanglesKHRUnsafe
#else
vkGetPhysicalDevicePresentRectanglesKHR
  = vkGetPhysicalDevicePresentRectanglesKHRSafe

#endif
{-# INLINE vkGetPhysicalDevicePresentRectanglesKHR #-}

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDevicePresentRectanglesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSurfaceKHR surface
--   >     , uint32_t* pRectCount
--   >     , VkRect2D* pRects
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDevicePresentRectanglesKHR vkGetPhysicalDevicePresentRectanglesKHR registry at www.khronos.org>
type HS_vkGetPhysicalDevicePresentRectanglesKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       VkSurfaceKHR -- ^ surface
                    -> Ptr Word32 -- ^ pRectCount
                                  -> Ptr VkRect2D -- ^ pRects
                                                  -> IO VkResult

type PFN_vkGetPhysicalDevicePresentRectanglesKHR =
     FunPtr HS_vkGetPhysicalDevicePresentRectanglesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDevicePresentRectanglesKHRUnsafe ::
               PFN_vkGetPhysicalDevicePresentRectanglesKHR ->
                 HS_vkGetPhysicalDevicePresentRectanglesKHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDevicePresentRectanglesKHRSafe ::
               PFN_vkGetPhysicalDevicePresentRectanglesKHR ->
                 HS_vkGetPhysicalDevicePresentRectanglesKHR

instance VulkanProc "vkGetPhysicalDevicePresentRectanglesKHR" where
    type VkProcType "vkGetPhysicalDevicePresentRectanglesKHR" =
         HS_vkGetPhysicalDevicePresentRectanglesKHR
    vkProcSymbol = _VkGetPhysicalDevicePresentRectanglesKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDevicePresentRectanglesKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDevicePresentRectanglesKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkAcquireNextImage2KHR :: CString

pattern VkAcquireNextImage2KHR <-
        (is_VkAcquireNextImage2KHR -> True)
  where
    VkAcquireNextImage2KHR = _VkAcquireNextImage2KHR

{-# INLINE _VkAcquireNextImage2KHR #-}

_VkAcquireNextImage2KHR :: CString
_VkAcquireNextImage2KHR = Ptr "vkAcquireNextImage2KHR\NUL"#

{-# INLINE is_VkAcquireNextImage2KHR #-}

is_VkAcquireNextImage2KHR :: CString -> Bool
is_VkAcquireNextImage2KHR
  = (EQ ==) . cmpCStrings _VkAcquireNextImage2KHR

type VkAcquireNextImage2KHR = "vkAcquireNextImage2KHR"

-- |
-- Success codes: 'VK_SUCCESS', 'VK_TIMEOUT', 'VK_NOT_READY', 'VK_SUBOPTIMAL_KHR'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkAcquireNextImage2KHR
-- >     ( VkDevice device
-- >     , const VkAcquireNextImageInfoKHR* pAcquireInfo
-- >     , uint32_t* pImageIndex
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAcquireNextImage2KHR vkAcquireNextImage2KHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myAcquireNextImage2KHR <- vkGetDeviceProc @VkAcquireNextImage2KHR vkDevice
--
-- or less efficient:
--
-- > myAcquireNextImage2KHR <- vkGetProc @VkAcquireNextImage2KHR
--
-- __Note:__ @vkAcquireNextImage2KHRUnsafe@ and @vkAcquireNextImage2KHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkAcquireNextImage2KHR@ is an alias
--           of @vkAcquireNextImage2KHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkAcquireNextImage2KHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall unsafe "vkAcquireNextImage2KHR"
               vkAcquireNextImage2KHRUnsafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkAcquireNextImageInfoKHR -- ^ pAcquireInfo
                                               -> Ptr Word32 -- ^ pImageIndex
                                                             -> IO VkResult

#else
vkAcquireNextImage2KHRUnsafe ::
                             VkDevice -- ^ device
                                      ->
                               Ptr VkAcquireNextImageInfoKHR -- ^ pAcquireInfo
                                                             -> Ptr Word32 -- ^ pImageIndex
                                                                           -> IO VkResult
vkAcquireNextImage2KHRUnsafe
  = unsafeDupablePerformIO (vkGetProcUnsafe @VkAcquireNextImage2KHR)

{-# NOINLINE vkAcquireNextImage2KHRUnsafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_TIMEOUT', 'VK_NOT_READY', 'VK_SUBOPTIMAL_KHR'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkAcquireNextImage2KHR
-- >     ( VkDevice device
-- >     , const VkAcquireNextImageInfoKHR* pAcquireInfo
-- >     , uint32_t* pImageIndex
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAcquireNextImage2KHR vkAcquireNextImage2KHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myAcquireNextImage2KHR <- vkGetDeviceProc @VkAcquireNextImage2KHR vkDevice
--
-- or less efficient:
--
-- > myAcquireNextImage2KHR <- vkGetProc @VkAcquireNextImage2KHR
--
-- __Note:__ @vkAcquireNextImage2KHRUnsafe@ and @vkAcquireNextImage2KHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkAcquireNextImage2KHR@ is an alias
--           of @vkAcquireNextImage2KHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkAcquireNextImage2KHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_1
foreign import ccall safe "vkAcquireNextImage2KHR"
               vkAcquireNextImage2KHRSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkAcquireNextImageInfoKHR -- ^ pAcquireInfo
                                               -> Ptr Word32 -- ^ pImageIndex
                                                             -> IO VkResult

#else
vkAcquireNextImage2KHRSafe ::
                           VkDevice -- ^ device
                                    ->
                             Ptr VkAcquireNextImageInfoKHR -- ^ pAcquireInfo
                                                           -> Ptr Word32 -- ^ pImageIndex
                                                                         -> IO VkResult
vkAcquireNextImage2KHRSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkAcquireNextImage2KHR)

{-# NOINLINE vkAcquireNextImage2KHRSafe #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_TIMEOUT', 'VK_NOT_READY', 'VK_SUBOPTIMAL_KHR'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkAcquireNextImage2KHR
-- >     ( VkDevice device
-- >     , const VkAcquireNextImageInfoKHR* pAcquireInfo
-- >     , uint32_t* pImageIndex
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAcquireNextImage2KHR vkAcquireNextImage2KHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-1@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myAcquireNextImage2KHR <- vkGetDeviceProc @VkAcquireNextImage2KHR vkDevice
--
-- or less efficient:
--
-- > myAcquireNextImage2KHR <- vkGetProc @VkAcquireNextImage2KHR
--
-- __Note:__ @vkAcquireNextImage2KHRUnsafe@ and @vkAcquireNextImage2KHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkAcquireNextImage2KHR@ is an alias
--           of @vkAcquireNextImage2KHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkAcquireNextImage2KHRSafe@.
--
vkAcquireNextImage2KHR ::
                       VkDevice -- ^ device
                                ->
                         Ptr VkAcquireNextImageInfoKHR -- ^ pAcquireInfo
                                                       -> Ptr Word32 -- ^ pImageIndex
                                                                     -> IO VkResult
#ifdef UNSAFE_FFI_DEFAULT
vkAcquireNextImage2KHR = vkAcquireNextImage2KHRUnsafe
#else
vkAcquireNextImage2KHR = vkAcquireNextImage2KHRSafe

#endif
{-# INLINE vkAcquireNextImage2KHR #-}

-- | Success codes: 'VK_SUCCESS', 'VK_TIMEOUT', 'VK_NOT_READY', 'VK_SUBOPTIMAL_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkAcquireNextImage2KHR
--   >     ( VkDevice device
--   >     , const VkAcquireNextImageInfoKHR* pAcquireInfo
--   >     , uint32_t* pImageIndex
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAcquireNextImage2KHR vkAcquireNextImage2KHR registry at www.khronos.org>
type HS_vkAcquireNextImage2KHR =
     VkDevice -- ^ device
              ->
       Ptr VkAcquireNextImageInfoKHR -- ^ pAcquireInfo
                                     -> Ptr Word32 -- ^ pImageIndex
                                                   -> IO VkResult

type PFN_vkAcquireNextImage2KHR = FunPtr HS_vkAcquireNextImage2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkAcquireNextImage2KHRUnsafe ::
               PFN_vkAcquireNextImage2KHR -> HS_vkAcquireNextImage2KHR

foreign import ccall safe "dynamic"
               unwrapVkAcquireNextImage2KHRSafe ::
               PFN_vkAcquireNextImage2KHR -> HS_vkAcquireNextImage2KHR

instance VulkanProc "vkAcquireNextImage2KHR" where
    type VkProcType "vkAcquireNextImage2KHR" =
         HS_vkAcquireNextImage2KHR
    vkProcSymbol = _VkAcquireNextImage2KHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkAcquireNextImage2KHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkAcquireNextImage2KHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR =
        VkStructureType 1000060007

pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR =
        VkStructureType 1000060008

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR =
        VkStructureType 1000060009

pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR =
        VkStructureType 1000060010

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR =
        VkStructureType 1000060011

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR =
        VkStructureType 1000060012

-- | Allow images with VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
--
--   bitpos = @0@
pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR ::
        VkSwapchainCreateBitmaskKHR a

pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR =
        VkSwapchainCreateBitmaskKHR 1

-- | Swapchain is protected
--
--   bitpos = @1@
pattern VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR ::
        VkSwapchainCreateBitmaskKHR a

pattern VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR =
        VkSwapchainCreateBitmaskKHR 2
