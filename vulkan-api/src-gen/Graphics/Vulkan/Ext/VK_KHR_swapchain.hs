{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
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
        vkCreateSwapchainKHR, vkCreateSwapchainKHRSafe,
        vkDestroySwapchainKHR, vkDestroySwapchainKHRSafe,
        vkGetSwapchainImagesKHR, vkGetSwapchainImagesKHRSafe,
        vkAcquireNextImageKHR, vkAcquireNextImageKHRSafe,
        vkQueuePresentKHR, vkQueuePresentKHRSafe,
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
        vkGetDeviceGroupPresentCapabilitiesKHR,
        vkGetDeviceGroupPresentCapabilitiesKHRSafe,
        vkGetDeviceGroupSurfacePresentModesKHR,
        vkGetDeviceGroupSurfacePresentModesKHRSafe,
        vkGetPhysicalDevicePresentRectanglesKHR,
        vkGetPhysicalDevicePresentRectanglesKHRSafe,
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateSwapchainKHR.html vkCreateSwapchainKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateSwapchainKHR"
               vkCreateSwapchainKHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfo
                                              ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSwapchainKHR -- ^ pSwapchain
                                                                   -> IO VkResult

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateSwapchainKHR.html vkCreateSwapchainKHR registry at www.khronos.org>
foreign import ccall safe "vkCreateSwapchainKHR"
               vkCreateSwapchainKHRSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfo
                                              ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSwapchainKHR -- ^ pSwapchain
                                                                   -> IO VkResult

-- | > () vkDestroySwapchainKHR
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroySwapchainKHR.html vkDestroySwapchainKHR registry at www.khronos.org>
foreign import ccall unsafe "vkDestroySwapchainKHR"
               vkDestroySwapchainKHR ::
               VkDevice -- ^ device
                        -> VkSwapchainKHR -- ^ swapchain
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

-- | > () vkDestroySwapchainKHR
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDestroySwapchainKHR.html vkDestroySwapchainKHR registry at www.khronos.org>
foreign import ccall safe "vkDestroySwapchainKHR"
               vkDestroySwapchainKHRSafe ::
               VkDevice -- ^ device
                        -> VkSwapchainKHR -- ^ swapchain
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetSwapchainImagesKHR.html vkGetSwapchainImagesKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetSwapchainImagesKHR"
               vkGetSwapchainImagesKHR ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                -> Ptr Word32 -- ^ pSwapchainImageCount
                                              -> Ptr VkImage -- ^ pSwapchainImages
                                                             -> IO VkResult

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetSwapchainImagesKHR.html vkGetSwapchainImagesKHR registry at www.khronos.org>
foreign import ccall safe "vkGetSwapchainImagesKHR"
               vkGetSwapchainImagesKHRSafe ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                -> Ptr Word32 -- ^ pSwapchainImageCount
                                              -> Ptr VkImage -- ^ pSwapchainImages
                                                             -> IO VkResult

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkAcquireNextImageKHR.html vkAcquireNextImageKHR registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkAcquireNextImageKHR.html vkAcquireNextImageKHR registry at www.khronos.org>
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

-- | Success codes: 'VK_SUCCESS', 'VK_SUBOPTIMAL_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkQueuePresentKHR
--   >     ( VkQueue queue
--   >     , const VkPresentInfoKHR* pPresentInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueuePresentKHR.html vkQueuePresentKHR registry at www.khronos.org>
foreign import ccall unsafe "vkQueuePresentKHR" vkQueuePresentKHR
               :: VkQueue -- ^ queue
                          -> Ptr VkPresentInfoKHR -- ^ pPresentInfo
                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_SUBOPTIMAL_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkQueuePresentKHR
--   >     ( VkQueue queue
--   >     , const VkPresentInfoKHR* pPresentInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkQueuePresentKHR.html vkQueuePresentKHR registry at www.khronos.org>
foreign import ccall safe "vkQueuePresentKHR" vkQueuePresentKHRSafe
               :: VkQueue -- ^ queue
                          -> Ptr VkPresentInfoKHR -- ^ pPresentInfo
                                                  -> IO VkResult

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

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDeviceGroupPresentCapabilitiesKHR
--   >     ( VkDevice device
--   >     , VkDeviceGroupPresentCapabilitiesKHR* pDeviceGroupPresentCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceGroupPresentCapabilitiesKHR.html vkGetDeviceGroupPresentCapabilitiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetDeviceGroupPresentCapabilitiesKHR"
               vkGetDeviceGroupPresentCapabilitiesKHR ::
               VkDevice -- ^ device
                        -> Ptr VkDeviceGroupPresentCapabilitiesKHR -- ^ pDeviceGroupPresentCapabilities
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDeviceGroupPresentCapabilitiesKHR
--   >     ( VkDevice device
--   >     , VkDeviceGroupPresentCapabilitiesKHR* pDeviceGroupPresentCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceGroupPresentCapabilitiesKHR.html vkGetDeviceGroupPresentCapabilitiesKHR registry at www.khronos.org>
foreign import ccall safe "vkGetDeviceGroupPresentCapabilitiesKHR"
               vkGetDeviceGroupPresentCapabilitiesKHRSafe ::
               VkDevice -- ^ device
                        -> Ptr VkDeviceGroupPresentCapabilitiesKHR -- ^ pDeviceGroupPresentCapabilities
                                                                   -> IO VkResult

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceGroupSurfacePresentModesKHR.html vkGetDeviceGroupSurfacePresentModesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetDeviceGroupSurfacePresentModesKHR"
               vkGetDeviceGroupSurfacePresentModesKHR ::
               VkDevice -- ^ device
                        ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr VkDeviceGroupPresentModeFlagsKHR -- ^ pModes
                                                                      -> IO VkResult

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetDeviceGroupSurfacePresentModesKHR.html vkGetDeviceGroupSurfacePresentModesKHR registry at www.khronos.org>
foreign import ccall safe "vkGetDeviceGroupSurfacePresentModesKHR"
               vkGetDeviceGroupSurfacePresentModesKHRSafe ::
               VkDevice -- ^ device
                        ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr VkDeviceGroupPresentModeFlagsKHR -- ^ pModes
                                                                      -> IO VkResult

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDevicePresentRectanglesKHR.html vkGetPhysicalDevicePresentRectanglesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDevicePresentRectanglesKHR"
               vkGetPhysicalDevicePresentRectanglesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr Word32 -- ^ pRectCount
                                            -> Ptr VkRect2D -- ^ pRects
                                                            -> IO VkResult

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDevicePresentRectanglesKHR.html vkGetPhysicalDevicePresentRectanglesKHR registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDevicePresentRectanglesKHR"
               vkGetPhysicalDevicePresentRectanglesKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr Word32 -- ^ pRectCount
                                            -> Ptr VkRect2D -- ^ pRects
                                                            -> IO VkResult

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkAcquireNextImage2KHR.html vkAcquireNextImage2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkAcquireNextImage2KHR"
               vkAcquireNextImage2KHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkAcquireNextImageInfoKHR -- ^ pAcquireInfo
                                               -> Ptr Word32 -- ^ pImageIndex
                                                             -> IO VkResult

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkAcquireNextImage2KHR.html vkAcquireNextImage2KHR registry at www.khronos.org>
foreign import ccall safe "vkAcquireNextImage2KHR"
               vkAcquireNextImage2KHRSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkAcquireNextImageInfoKHR -- ^ pAcquireInfo
                                               -> Ptr Word32 -- ^ pImageIndex
                                                             -> IO VkResult

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
