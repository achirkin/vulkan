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
        -- contact: @James Jones @cubanismo,Ian Elliott ianelliott@google.com@
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
        vkCreateSwapchainKHR, vkCreateSwapchainKHRSafe,
        VkDestroySwapchainKHR, pattern VkDestroySwapchainKHR,
        HS_vkDestroySwapchainKHR, PFN_vkDestroySwapchainKHR,
        vkDestroySwapchainKHR, vkDestroySwapchainKHRSafe,
        VkGetSwapchainImagesKHR, pattern VkGetSwapchainImagesKHR,
        HS_vkGetSwapchainImagesKHR, PFN_vkGetSwapchainImagesKHR,
        vkGetSwapchainImagesKHR, vkGetSwapchainImagesKHRSafe,
        VkAcquireNextImageKHR, pattern VkAcquireNextImageKHR,
        HS_vkAcquireNextImageKHR, PFN_vkAcquireNextImageKHR,
        vkAcquireNextImageKHR, vkAcquireNextImageKHRSafe,
        VkQueuePresentKHR, pattern VkQueuePresentKHR, HS_vkQueuePresentKHR,
        PFN_vkQueuePresentKHR, vkQueuePresentKHR, vkQueuePresentKHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkColorSpaceKHR,
        module Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkPresentModeKHR,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSharingMode,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR,
        module Graphics.Vulkan.Types.Enum.VkSwapchainCreateFlagsKHR,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        module Graphics.Vulkan.Types.Struct.VkExtent2D,
        module Graphics.Vulkan.Types.Struct.VkPresentInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkSwapchainCreateInfoKHR,
        VK_KHR_SWAPCHAIN_SPEC_VERSION,
        pattern VK_KHR_SWAPCHAIN_SPEC_VERSION,
        VK_KHR_SWAPCHAIN_EXTENSION_NAME,
        pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR,
        pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR, pattern VK_SUBOPTIMAL_KHR,
        pattern VK_ERROR_OUT_OF_DATE_KHR,
        pattern VK_OBJECT_TYPE_SWAPCHAIN_KHR,
        -- ** Required extensions: 'VK_KHR_surface'.
        module Graphics.Vulkan.Types.Struct.VkAcquireNextImageInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfo,
        module Graphics.Vulkan.Types.Struct.VkBindImageMemorySwapchainInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentCapabilitiesKHR,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkDeviceGroupPresentModeFlagsKHR,
        module Graphics.Vulkan.Types.Struct.VkDeviceGroupSwapchainCreateInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.Enum.VkImageCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkImageCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkImageLayout,
        module Graphics.Vulkan.Types.Struct.VkImageSwapchainCreateInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkImageTiling,
        module Graphics.Vulkan.Types.Enum.VkImageType,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        -- > #include "vk_platform.h"
        VkGetDeviceGroupPresentCapabilitiesKHR,
        pattern VkGetDeviceGroupPresentCapabilitiesKHR,
        HS_vkGetDeviceGroupPresentCapabilitiesKHR,
        PFN_vkGetDeviceGroupPresentCapabilitiesKHR,
        vkGetDeviceGroupPresentCapabilitiesKHR,
        vkGetDeviceGroupPresentCapabilitiesKHRSafe,
        VkGetDeviceGroupSurfacePresentModesKHR,
        pattern VkGetDeviceGroupSurfacePresentModesKHR,
        HS_vkGetDeviceGroupSurfacePresentModesKHR,
        PFN_vkGetDeviceGroupSurfacePresentModesKHR,
        vkGetDeviceGroupSurfacePresentModesKHR,
        vkGetDeviceGroupSurfacePresentModesKHRSafe,
        VkGetPhysicalDevicePresentRectanglesKHR,
        pattern VkGetPhysicalDevicePresentRectanglesKHR,
        HS_vkGetPhysicalDevicePresentRectanglesKHR,
        PFN_vkGetPhysicalDevicePresentRectanglesKHR,
        vkGetPhysicalDevicePresentRectanglesKHR,
        vkGetPhysicalDevicePresentRectanglesKHRSafe,
        VkAcquireNextImage2KHR, pattern VkAcquireNextImage2KHR,
        HS_vkAcquireNextImage2KHR, PFN_vkAcquireNextImage2KHR,
        vkAcquireNextImage2KHR, vkAcquireNextImage2KHRSafe,
        module Graphics.Vulkan.Types.Struct.VkOffset2D,
        module Graphics.Vulkan.Types.Struct.VkRect2D,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR,
        pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR,
        pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR,
        pattern VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR)
       where
import           GHC.Ptr
                                                                                   (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkColorSpaceKHR
import           Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkDeviceGroupPresentModeFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags
import           Graphics.Vulkan.Types.Enum.VkImageLayout
import           Graphics.Vulkan.Types.Enum.VkImageTiling
import           Graphics.Vulkan.Types.Enum.VkImageType
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkInternalAllocationType
import           Graphics.Vulkan.Types.Enum.VkObjectType
                                                                                   (VkObjectType (..))
import           Graphics.Vulkan.Types.Enum.VkPresentModeKHR
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkSharingMode
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkSwapchainCreateFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkSystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAcquireNextImageInfoKHR
import           Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfo
import           Graphics.Vulkan.Types.Struct.VkBindImageMemorySwapchainInfoKHR
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentCapabilitiesKHR
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentInfoKHR
import           Graphics.Vulkan.Types.Struct.VkDeviceGroupSwapchainCreateInfoKHR
import           Graphics.Vulkan.Types.Struct.VkExtent2D
import           Graphics.Vulkan.Types.Struct.VkExtent3D
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo
import           Graphics.Vulkan.Types.Struct.VkImageSwapchainCreateInfoKHR
import           Graphics.Vulkan.Types.Struct.VkOffset2D
import           Graphics.Vulkan.Types.Struct.VkPresentInfoKHR
import           Graphics.Vulkan.Types.Struct.VkRect2D
import           Graphics.Vulkan.Types.Struct.VkSwapchainCreateInfoKHR
import           System.IO.Unsafe
                                                                                   (unsafeDupablePerformIO)

pattern VkCreateSwapchainKHR :: CString

pattern VkCreateSwapchainKHR <- (is_VkCreateSwapchainKHR -> True)
  where VkCreateSwapchainKHR = _VkCreateSwapchainKHR

{-# INLINE _VkCreateSwapchainKHR #-}

_VkCreateSwapchainKHR :: CString
_VkCreateSwapchainKHR = Ptr "vkCreateSwapchainKHR\NUL"#

{-# INLINE is_VkCreateSwapchainKHR #-}

is_VkCreateSwapchainKHR :: CString -> Bool
is_VkCreateSwapchainKHR
  = (EQ ==) . cmpCStrings _VkCreateSwapchainKHR

type VkCreateSwapchainKHR = "vkCreateSwapchainKHR"

#ifdef NATIVE_FFI_VK_VERSION_1_0
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
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkCreateSwapchainKHR"
               vkCreateSwapchainKHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfo
                                              ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSwapchainKHR -- ^ pSwapchain
                                                                   -> IO VkResult

#else
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
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-0@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSwapchainKHR <- vkGetDeviceProc @VkCreateSwapchainKHR vkDevice
--
vkCreateSwapchainKHR ::
                     VkDevice -- ^ device
                              ->
                       Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfo
                                                    ->
                         Ptr VkAllocationCallbacks -- ^ pAllocator
                                                   -> Ptr VkSwapchainKHR -- ^ pSwapchain
                                                                         -> IO VkResult
vkCreateSwapchainKHR d
  = unsafeDupablePerformIO (vkGetDeviceProc @VkCreateSwapchainKHR d)
      d

{-# INLINE vkCreateSwapchainKHR #-}

{-# WARNING
vkCreateSwapchainKHR"This function requires useNativeFFI-1-0 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-0.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_0
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
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
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
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-0@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSwapchainKHR <- vkGetDeviceProc @VkCreateSwapchainKHR vkDevice
--
vkCreateSwapchainKHRSafe ::
                         VkDevice -- ^ device
                                  ->
                           Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfo
                                                        ->
                             Ptr VkAllocationCallbacks -- ^ pAllocator
                                                       -> Ptr VkSwapchainKHR -- ^ pSwapchain
                                                                             -> IO VkResult
vkCreateSwapchainKHRSafe = vkCreateSwapchainKHR

{-# INLINE vkCreateSwapchainKHRSafe #-}

{-# WARNING
vkCreateSwapchainKHRSafe"This function requires useNativeFFI-1-0 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-0.\n"
 #-}
#endif

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

foreign import ccall "dynamic" unwrapVkCreateSwapchainKHR ::
               PFN_vkCreateSwapchainKHR -> HS_vkCreateSwapchainKHR

instance VulkanProc "vkCreateSwapchainKHR" where
        type VkProcType "vkCreateSwapchainKHR" = HS_vkCreateSwapchainKHR
        vkProcSymbol = _VkCreateSwapchainKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateSwapchainKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkDestroySwapchainKHR :: CString

pattern VkDestroySwapchainKHR <- (is_VkDestroySwapchainKHR -> True)
  where VkDestroySwapchainKHR = _VkDestroySwapchainKHR

{-# INLINE _VkDestroySwapchainKHR #-}

_VkDestroySwapchainKHR :: CString
_VkDestroySwapchainKHR = Ptr "vkDestroySwapchainKHR\NUL"#

{-# INLINE is_VkDestroySwapchainKHR #-}

is_VkDestroySwapchainKHR :: CString -> Bool
is_VkDestroySwapchainKHR
  = (EQ ==) . cmpCStrings _VkDestroySwapchainKHR

type VkDestroySwapchainKHR = "vkDestroySwapchainKHR"

#ifdef NATIVE_FFI_VK_VERSION_1_0
-- |
-- > void vkDestroySwapchainKHR
-- >     ( VkDevice device
-- >     , VkSwapchainKHR swapchain
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySwapchainKHR vkDestroySwapchainKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkDestroySwapchainKHR"
               vkDestroySwapchainKHR ::
               VkDevice -- ^ device
                        -> VkSwapchainKHR -- ^ swapchain
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

#else
-- |
-- > void vkDestroySwapchainKHR
-- >     ( VkDevice device
-- >     , VkSwapchainKHR swapchain
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySwapchainKHR vkDestroySwapchainKHR registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-0@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroySwapchainKHR <- vkGetDeviceProc @VkDestroySwapchainKHR vkDevice
--
vkDestroySwapchainKHR ::
                      VkDevice -- ^ device
                               -> VkSwapchainKHR -- ^ swapchain
                                                 -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                              -> IO ()
vkDestroySwapchainKHR d
  = unsafeDupablePerformIO (vkGetDeviceProc @VkDestroySwapchainKHR d)
      d

{-# INLINE vkDestroySwapchainKHR #-}

{-# WARNING
vkDestroySwapchainKHR"This function requires useNativeFFI-1-0 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-0.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_0
-- |
-- > void vkDestroySwapchainKHR
-- >     ( VkDevice device
-- >     , VkSwapchainKHR swapchain
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySwapchainKHR vkDestroySwapchainKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkDestroySwapchainKHR"
               vkDestroySwapchainKHRSafe ::
               VkDevice -- ^ device
                        -> VkSwapchainKHR -- ^ swapchain
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

#else
-- |
-- > void vkDestroySwapchainKHR
-- >     ( VkDevice device
-- >     , VkSwapchainKHR swapchain
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySwapchainKHR vkDestroySwapchainKHR registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-0@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroySwapchainKHR <- vkGetDeviceProc @VkDestroySwapchainKHR vkDevice
--
vkDestroySwapchainKHRSafe ::
                          VkDevice -- ^ device
                                   -> VkSwapchainKHR -- ^ swapchain
                                                     -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                  -> IO ()
vkDestroySwapchainKHRSafe = vkDestroySwapchainKHR

{-# INLINE vkDestroySwapchainKHRSafe #-}

{-# WARNING
vkDestroySwapchainKHRSafe"This function requires useNativeFFI-1-0 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-0.\n"
 #-}
#endif

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

foreign import ccall "dynamic" unwrapVkDestroySwapchainKHR ::
               PFN_vkDestroySwapchainKHR -> HS_vkDestroySwapchainKHR

instance VulkanProc "vkDestroySwapchainKHR" where
        type VkProcType "vkDestroySwapchainKHR" = HS_vkDestroySwapchainKHR
        vkProcSymbol = _VkDestroySwapchainKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroySwapchainKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetSwapchainImagesKHR :: CString

pattern VkGetSwapchainImagesKHR <-
        (is_VkGetSwapchainImagesKHR -> True)
  where VkGetSwapchainImagesKHR = _VkGetSwapchainImagesKHR

{-# INLINE _VkGetSwapchainImagesKHR #-}

_VkGetSwapchainImagesKHR :: CString
_VkGetSwapchainImagesKHR = Ptr "vkGetSwapchainImagesKHR\NUL"#

{-# INLINE is_VkGetSwapchainImagesKHR #-}

is_VkGetSwapchainImagesKHR :: CString -> Bool
is_VkGetSwapchainImagesKHR
  = (EQ ==) . cmpCStrings _VkGetSwapchainImagesKHR

type VkGetSwapchainImagesKHR = "vkGetSwapchainImagesKHR"

#ifdef NATIVE_FFI_VK_VERSION_1_0
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
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkGetSwapchainImagesKHR"
               vkGetSwapchainImagesKHR ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                -> Ptr Word32 -- ^ pSwapchainImageCount
                                              -> Ptr VkImage -- ^ pSwapchainImages
                                                             -> IO VkResult

#else
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
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-0@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetSwapchainImagesKHR <- vkGetDeviceProc @VkGetSwapchainImagesKHR vkDevice
--
vkGetSwapchainImagesKHR ::
                        VkDevice -- ^ device
                                 ->
                          VkSwapchainKHR -- ^ swapchain
                                         -> Ptr Word32 -- ^ pSwapchainImageCount
                                                       -> Ptr VkImage -- ^ pSwapchainImages
                                                                      -> IO VkResult
vkGetSwapchainImagesKHR d
  = unsafeDupablePerformIO
      (vkGetDeviceProc @VkGetSwapchainImagesKHR d)
      d

{-# INLINE vkGetSwapchainImagesKHR #-}

{-# WARNING
vkGetSwapchainImagesKHR"This function requires useNativeFFI-1-0 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-0.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_0
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
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkGetSwapchainImagesKHR"
               vkGetSwapchainImagesKHRSafe ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                -> Ptr Word32 -- ^ pSwapchainImageCount
                                              -> Ptr VkImage -- ^ pSwapchainImages
                                                             -> IO VkResult

#else
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
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-0@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetSwapchainImagesKHR <- vkGetDeviceProc @VkGetSwapchainImagesKHR vkDevice
--
vkGetSwapchainImagesKHRSafe ::
                            VkDevice -- ^ device
                                     ->
                              VkSwapchainKHR -- ^ swapchain
                                             -> Ptr Word32 -- ^ pSwapchainImageCount
                                                           -> Ptr VkImage -- ^ pSwapchainImages
                                                                          -> IO VkResult
vkGetSwapchainImagesKHRSafe = vkGetSwapchainImagesKHR

{-# INLINE vkGetSwapchainImagesKHRSafe #-}

{-# WARNING
vkGetSwapchainImagesKHRSafe"This function requires useNativeFFI-1-0 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-0.\n"
 #-}
#endif

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

foreign import ccall "dynamic" unwrapVkGetSwapchainImagesKHR ::
               PFN_vkGetSwapchainImagesKHR -> HS_vkGetSwapchainImagesKHR

instance VulkanProc "vkGetSwapchainImagesKHR" where
        type VkProcType "vkGetSwapchainImagesKHR" =
             HS_vkGetSwapchainImagesKHR
        vkProcSymbol = _VkGetSwapchainImagesKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetSwapchainImagesKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkAcquireNextImageKHR :: CString

pattern VkAcquireNextImageKHR <- (is_VkAcquireNextImageKHR -> True)
  where VkAcquireNextImageKHR = _VkAcquireNextImageKHR

{-# INLINE _VkAcquireNextImageKHR #-}

_VkAcquireNextImageKHR :: CString
_VkAcquireNextImageKHR = Ptr "vkAcquireNextImageKHR\NUL"#

{-# INLINE is_VkAcquireNextImageKHR #-}

is_VkAcquireNextImageKHR :: CString -> Bool
is_VkAcquireNextImageKHR
  = (EQ ==) . cmpCStrings _VkAcquireNextImageKHR

type VkAcquireNextImageKHR = "vkAcquireNextImageKHR"

#ifdef NATIVE_FFI_VK_VERSION_1_0
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
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkAcquireNextImageKHR"
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

#else
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
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-0@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myAcquireNextImageKHR <- vkGetDeviceProc @VkAcquireNextImageKHR vkDevice
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
vkAcquireNextImageKHR d
  = unsafeDupablePerformIO (vkGetDeviceProc @VkAcquireNextImageKHR d)
      d

{-# INLINE vkAcquireNextImageKHR #-}

{-# WARNING
vkAcquireNextImageKHR"This function requires useNativeFFI-1-0 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-0.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_0
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
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
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
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-0@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myAcquireNextImageKHR <- vkGetDeviceProc @VkAcquireNextImageKHR vkDevice
--
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
vkAcquireNextImageKHRSafe = vkAcquireNextImageKHR

{-# INLINE vkAcquireNextImageKHRSafe #-}

{-# WARNING
vkAcquireNextImageKHRSafe"This function requires useNativeFFI-1-0 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-0.\n"
 #-}
#endif

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

foreign import ccall "dynamic" unwrapVkAcquireNextImageKHR ::
               PFN_vkAcquireNextImageKHR -> HS_vkAcquireNextImageKHR

instance VulkanProc "vkAcquireNextImageKHR" where
        type VkProcType "vkAcquireNextImageKHR" = HS_vkAcquireNextImageKHR
        vkProcSymbol = _VkAcquireNextImageKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkAcquireNextImageKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkQueuePresentKHR :: CString

pattern VkQueuePresentKHR <- (is_VkQueuePresentKHR -> True)
  where VkQueuePresentKHR = _VkQueuePresentKHR

{-# INLINE _VkQueuePresentKHR #-}

_VkQueuePresentKHR :: CString
_VkQueuePresentKHR = Ptr "vkQueuePresentKHR\NUL"#

{-# INLINE is_VkQueuePresentKHR #-}

is_VkQueuePresentKHR :: CString -> Bool
is_VkQueuePresentKHR = (EQ ==) . cmpCStrings _VkQueuePresentKHR

type VkQueuePresentKHR = "vkQueuePresentKHR"

#ifdef NATIVE_FFI_VK_VERSION_1_0
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
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkQueuePresentKHR" vkQueuePresentKHR
               :: VkQueue -- ^ queue
                          -> Ptr VkPresentInfoKHR -- ^ pPresentInfo
                                                  -> IO VkResult

#else
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
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-0@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myQueuePresentKHR <- vkGetInstanceProc @VkQueuePresentKHR vkInstance
--
vkQueuePresentKHR :: VkQueue -- ^ queue
                             -> Ptr VkPresentInfoKHR -- ^ pPresentInfo
                                                     -> IO VkResult
vkQueuePresentKHR
  = error $
      "Cannot lookup C symbol \"vkQueuePresentKHR\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-0 cabal flag."

{-# WARNING
vkQueuePresentKHR"This function requires useNativeFFI-1-0 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-0.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_0
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
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkQueuePresentKHR" vkQueuePresentKHRSafe
               :: VkQueue -- ^ queue
                          -> Ptr VkPresentInfoKHR -- ^ pPresentInfo
                                                  -> IO VkResult

#else
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
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-0@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myQueuePresentKHR <- vkGetInstanceProc @VkQueuePresentKHR vkInstance
--
vkQueuePresentKHRSafe ::
                      VkQueue -- ^ queue
                              -> Ptr VkPresentInfoKHR -- ^ pPresentInfo
                                                      -> IO VkResult
vkQueuePresentKHRSafe = vkQueuePresentKHR

{-# INLINE vkQueuePresentKHRSafe #-}

{-# WARNING
vkQueuePresentKHRSafe"This function requires useNativeFFI-1-0 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-0.\n"
 #-}
#endif

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

foreign import ccall "dynamic" unwrapVkQueuePresentKHR ::
               PFN_vkQueuePresentKHR -> HS_vkQueuePresentKHR

instance VulkanProc "vkQueuePresentKHR" where
        type VkProcType "vkQueuePresentKHR" = HS_vkQueuePresentKHR
        vkProcSymbol = _VkQueuePresentKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkQueuePresentKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_KHR_SWAPCHAIN_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_SWAPCHAIN_SPEC_VERSION = 70

type VK_KHR_SWAPCHAIN_SPEC_VERSION = 70

pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME :: CString

pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME <-
        (is_VK_KHR_SWAPCHAIN_EXTENSION_NAME -> True)
  where VK_KHR_SWAPCHAIN_EXTENSION_NAME
          = _VK_KHR_SWAPCHAIN_EXTENSION_NAME

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
  where VkGetDeviceGroupPresentCapabilitiesKHR
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
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
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe
               "vkGetDeviceGroupPresentCapabilitiesKHR"
               vkGetDeviceGroupPresentCapabilitiesKHR ::
               VkDevice -- ^ device
                        -> Ptr VkDeviceGroupPresentCapabilitiesKHR -- ^ pDeviceGroupPresentCapabilities
                                                                   -> IO VkResult

#else
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
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceGroupPresentCapabilitiesKHR <- vkGetDeviceProc @VkGetDeviceGroupPresentCapabilitiesKHR vkDevice
--
vkGetDeviceGroupPresentCapabilitiesKHR ::
                                       VkDevice -- ^ device
                                                ->
                                         Ptr VkDeviceGroupPresentCapabilitiesKHR -- ^ pDeviceGroupPresentCapabilities
                                                                                 -> IO VkResult
vkGetDeviceGroupPresentCapabilitiesKHR d
  = unsafeDupablePerformIO
      (vkGetDeviceProc @VkGetDeviceGroupPresentCapabilitiesKHR d)
      d

{-# INLINE vkGetDeviceGroupPresentCapabilitiesKHR #-}

{-# WARNING
vkGetDeviceGroupPresentCapabilitiesKHR"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
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
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkGetDeviceGroupPresentCapabilitiesKHR"
               vkGetDeviceGroupPresentCapabilitiesKHRSafe ::
               VkDevice -- ^ device
                        -> Ptr VkDeviceGroupPresentCapabilitiesKHR -- ^ pDeviceGroupPresentCapabilities
                                                                   -> IO VkResult

#else
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
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceGroupPresentCapabilitiesKHR <- vkGetDeviceProc @VkGetDeviceGroupPresentCapabilitiesKHR vkDevice
--
vkGetDeviceGroupPresentCapabilitiesKHRSafe ::
                                           VkDevice -- ^ device
                                                    ->
                                             Ptr VkDeviceGroupPresentCapabilitiesKHR -- ^ pDeviceGroupPresentCapabilities
                                                                                     -> IO VkResult
vkGetDeviceGroupPresentCapabilitiesKHRSafe
  = vkGetDeviceGroupPresentCapabilitiesKHR

{-# INLINE vkGetDeviceGroupPresentCapabilitiesKHRSafe #-}

{-# WARNING
vkGetDeviceGroupPresentCapabilitiesKHRSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

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

foreign import ccall "dynamic"
               unwrapVkGetDeviceGroupPresentCapabilitiesKHR ::
               PFN_vkGetDeviceGroupPresentCapabilitiesKHR ->
                 HS_vkGetDeviceGroupPresentCapabilitiesKHR

instance VulkanProc "vkGetDeviceGroupPresentCapabilitiesKHR" where
        type VkProcType "vkGetDeviceGroupPresentCapabilitiesKHR" =
             HS_vkGetDeviceGroupPresentCapabilitiesKHR
        vkProcSymbol = _VkGetDeviceGroupPresentCapabilitiesKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetDeviceGroupPresentCapabilitiesKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetDeviceGroupSurfacePresentModesKHR :: CString

pattern VkGetDeviceGroupSurfacePresentModesKHR <-
        (is_VkGetDeviceGroupSurfacePresentModesKHR -> True)
  where VkGetDeviceGroupSurfacePresentModesKHR
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
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
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe
               "vkGetDeviceGroupSurfacePresentModesKHR"
               vkGetDeviceGroupSurfacePresentModesKHR ::
               VkDevice -- ^ device
                        ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr VkDeviceGroupPresentModeFlagsKHR -- ^ pModes
                                                                      -> IO VkResult

#else
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
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceGroupSurfacePresentModesKHR <- vkGetDeviceProc @VkGetDeviceGroupSurfacePresentModesKHR vkDevice
--
vkGetDeviceGroupSurfacePresentModesKHR ::
                                       VkDevice -- ^ device
                                                ->
                                         VkSurfaceKHR -- ^ surface
                                                      ->
                                           Ptr VkDeviceGroupPresentModeFlagsKHR -- ^ pModes
                                                                                -> IO VkResult
vkGetDeviceGroupSurfacePresentModesKHR d
  = unsafeDupablePerformIO
      (vkGetDeviceProc @VkGetDeviceGroupSurfacePresentModesKHR d)
      d

{-# INLINE vkGetDeviceGroupSurfacePresentModesKHR #-}

{-# WARNING
vkGetDeviceGroupSurfacePresentModesKHR"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
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
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkGetDeviceGroupSurfacePresentModesKHR"
               vkGetDeviceGroupSurfacePresentModesKHRSafe ::
               VkDevice -- ^ device
                        ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr VkDeviceGroupPresentModeFlagsKHR -- ^ pModes
                                                                      -> IO VkResult

#else
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
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetDeviceGroupSurfacePresentModesKHR <- vkGetDeviceProc @VkGetDeviceGroupSurfacePresentModesKHR vkDevice
--
vkGetDeviceGroupSurfacePresentModesKHRSafe ::
                                           VkDevice -- ^ device
                                                    ->
                                             VkSurfaceKHR -- ^ surface
                                                          ->
                                               Ptr VkDeviceGroupPresentModeFlagsKHR -- ^ pModes
                                                                                    -> IO VkResult
vkGetDeviceGroupSurfacePresentModesKHRSafe
  = vkGetDeviceGroupSurfacePresentModesKHR

{-# INLINE vkGetDeviceGroupSurfacePresentModesKHRSafe #-}

{-# WARNING
vkGetDeviceGroupSurfacePresentModesKHRSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

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

foreign import ccall "dynamic"
               unwrapVkGetDeviceGroupSurfacePresentModesKHR ::
               PFN_vkGetDeviceGroupSurfacePresentModesKHR ->
                 HS_vkGetDeviceGroupSurfacePresentModesKHR

instance VulkanProc "vkGetDeviceGroupSurfacePresentModesKHR" where
        type VkProcType "vkGetDeviceGroupSurfacePresentModesKHR" =
             HS_vkGetDeviceGroupSurfacePresentModesKHR
        vkProcSymbol = _VkGetDeviceGroupSurfacePresentModesKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetDeviceGroupSurfacePresentModesKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDevicePresentRectanglesKHR :: CString

pattern VkGetPhysicalDevicePresentRectanglesKHR <-
        (is_VkGetPhysicalDevicePresentRectanglesKHR -> True)
  where VkGetPhysicalDevicePresentRectanglesKHR
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

#ifdef NATIVE_FFI_VK_VERSION_1_1
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
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe
               "vkGetPhysicalDevicePresentRectanglesKHR"
               vkGetPhysicalDevicePresentRectanglesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr Word32 -- ^ pRectCount
                                            -> Ptr VkRect2D -- ^ pRects
                                                            -> IO VkResult

#else
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
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDevicePresentRectanglesKHR <- vkGetInstanceProc @VkGetPhysicalDevicePresentRectanglesKHR vkInstance
--
vkGetPhysicalDevicePresentRectanglesKHR ::
                                        VkPhysicalDevice -- ^ physicalDevice
                                                         ->
                                          VkSurfaceKHR -- ^ surface
                                                       -> Ptr Word32 -- ^ pRectCount
                                                                     -> Ptr VkRect2D -- ^ pRects
                                                                                     -> IO VkResult
vkGetPhysicalDevicePresentRectanglesKHR
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDevicePresentRectanglesKHR\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-1 cabal flag."

{-# WARNING
vkGetPhysicalDevicePresentRectanglesKHR"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
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
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkGetPhysicalDevicePresentRectanglesKHR"
               vkGetPhysicalDevicePresentRectanglesKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr Word32 -- ^ pRectCount
                                            -> Ptr VkRect2D -- ^ pRects
                                                            -> IO VkResult

#else
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
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDevicePresentRectanglesKHR <- vkGetInstanceProc @VkGetPhysicalDevicePresentRectanglesKHR vkInstance
--
vkGetPhysicalDevicePresentRectanglesKHRSafe ::
                                            VkPhysicalDevice -- ^ physicalDevice
                                                             ->
                                              VkSurfaceKHR -- ^ surface
                                                           ->
                                                Ptr Word32 -- ^ pRectCount
                                                           -> Ptr VkRect2D -- ^ pRects
                                                                           -> IO VkResult
vkGetPhysicalDevicePresentRectanglesKHRSafe
  = vkGetPhysicalDevicePresentRectanglesKHR

{-# INLINE vkGetPhysicalDevicePresentRectanglesKHRSafe #-}

{-# WARNING
vkGetPhysicalDevicePresentRectanglesKHRSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise, it causes a runtime error!\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

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

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDevicePresentRectanglesKHR ::
               PFN_vkGetPhysicalDevicePresentRectanglesKHR ->
                 HS_vkGetPhysicalDevicePresentRectanglesKHR

instance VulkanProc "vkGetPhysicalDevicePresentRectanglesKHR" where
        type VkProcType "vkGetPhysicalDevicePresentRectanglesKHR" =
             HS_vkGetPhysicalDevicePresentRectanglesKHR
        vkProcSymbol = _VkGetPhysicalDevicePresentRectanglesKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDevicePresentRectanglesKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkAcquireNextImage2KHR :: CString

pattern VkAcquireNextImage2KHR <-
        (is_VkAcquireNextImage2KHR -> True)
  where VkAcquireNextImage2KHR = _VkAcquireNextImage2KHR

{-# INLINE _VkAcquireNextImage2KHR #-}

_VkAcquireNextImage2KHR :: CString
_VkAcquireNextImage2KHR = Ptr "vkAcquireNextImage2KHR\NUL"#

{-# INLINE is_VkAcquireNextImage2KHR #-}

is_VkAcquireNextImage2KHR :: CString -> Bool
is_VkAcquireNextImage2KHR
  = (EQ ==) . cmpCStrings _VkAcquireNextImage2KHR

type VkAcquireNextImage2KHR = "vkAcquireNextImage2KHR"

#ifdef NATIVE_FFI_VK_VERSION_1_1
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
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkAcquireNextImage2KHR"
               vkAcquireNextImage2KHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkAcquireNextImageInfoKHR -- ^ pAcquireInfo
                                               -> Ptr Word32 -- ^ pImageIndex
                                                             -> IO VkResult

#else
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
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myAcquireNextImage2KHR <- vkGetDeviceProc @VkAcquireNextImage2KHR vkDevice
--
vkAcquireNextImage2KHR ::
                       VkDevice -- ^ device
                                ->
                         Ptr VkAcquireNextImageInfoKHR -- ^ pAcquireInfo
                                                       -> Ptr Word32 -- ^ pImageIndex
                                                                     -> IO VkResult
vkAcquireNextImage2KHR d
  = unsafeDupablePerformIO
      (vkGetDeviceProc @VkAcquireNextImage2KHR d)
      d

{-# INLINE vkAcquireNextImage2KHR #-}

{-# WARNING
vkAcquireNextImage2KHR"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_1
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
-- __Note:__ flag @useNativeFFI-1-1@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkAcquireNextImage2KHR"
               vkAcquireNextImage2KHRSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkAcquireNextImageInfoKHR -- ^ pAcquireInfo
                                               -> Ptr Word32 -- ^ pImageIndex
                                                             -> IO VkResult

#else
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
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-1@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myAcquireNextImage2KHR <- vkGetDeviceProc @VkAcquireNextImage2KHR vkDevice
--
vkAcquireNextImage2KHRSafe ::
                           VkDevice -- ^ device
                                    ->
                             Ptr VkAcquireNextImageInfoKHR -- ^ pAcquireInfo
                                                           -> Ptr Word32 -- ^ pImageIndex
                                                                         -> IO VkResult
vkAcquireNextImage2KHRSafe = vkAcquireNextImage2KHR

{-# INLINE vkAcquireNextImage2KHRSafe #-}

{-# WARNING
vkAcquireNextImage2KHRSafe"This function requires useNativeFFI-1-1 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-1.\n"
 #-}
#endif

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

foreign import ccall "dynamic" unwrapVkAcquireNextImage2KHR ::
               PFN_vkAcquireNextImage2KHR -> HS_vkAcquireNextImage2KHR

instance VulkanProc "vkAcquireNextImage2KHR" where
        type VkProcType "vkAcquireNextImage2KHR" =
             HS_vkAcquireNextImage2KHR
        vkProcSymbol = _VkAcquireNextImage2KHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkAcquireNextImage2KHR

        {-# INLINE unwrapVkProcPtr #-}

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
        VkSwapchainCreateFlagBitsKHR

pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR =
        VkSwapchainCreateFlagBitsKHR 1

-- | Swapchain is protected
--
--   bitpos = @1@
pattern VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR ::
        VkSwapchainCreateFlagBitsKHR

pattern VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR =
        VkSwapchainCreateFlagBitsKHR 2
