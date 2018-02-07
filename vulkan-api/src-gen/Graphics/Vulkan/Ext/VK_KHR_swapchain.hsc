#include "vulkan/vulkan.h"

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
        vkCreateSwapchainKHR, vkDestroySwapchainKHR,
        vkGetSwapchainImagesKHR, vkAcquireNextImageKHR, vkQueuePresentKHR,
        VK_KHR_SWAPCHAIN_SPEC_VERSION,
        pattern VK_KHR_SWAPCHAIN_SPEC_VERSION,
        VK_KHR_SWAPCHAIN_EXTENSION_NAME,
        pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR,
        pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR, pattern VK_SUBOPTIMAL_KHR,
        pattern VK_ERROR_OUT_OF_DATE_KHR,
        pattern VK_OBJECT_TYPE_SWAPCHAIN_KHR)
       where
import           Foreign.C.String              (CString)
import           GHC.Ptr                       (Ptr (..))
import           Graphics.Vulkan.Base          (VkAllocationCallbacks,
                                                VkPresentInfoKHR,
                                                VkSwapchainCreateInfoKHR)
import           Graphics.Vulkan.Common        (VkDevice, VkFence, VkImage,
                                                VkImageLayout (..),
                                                VkObjectType (..), VkQueue,
                                                VkResult, VkResult (..),
                                                VkSemaphore,
                                                VkStructureType (..),
                                                VkSwapchainKHR, Word32, Word64)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.StructMembers

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateSwapchainKHR.html vkCreateSwapchainKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateSwapchainKHR"
               vkCreateSwapchainKHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfo
                                              ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSwapchainKHR -- ^ pSwapchain
                                                                   -> IO VkResult

-- | > void vkDestroySwapchainKHR
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroySwapchainKHR.html vkDestroySwapchainKHR registry at www.khronos.org>
foreign import ccall unsafe "vkDestroySwapchainKHR"
               vkDestroySwapchainKHR ::
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetSwapchainImagesKHR.html vkGetSwapchainImagesKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetSwapchainImagesKHR"
               vkGetSwapchainImagesKHR ::
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkAcquireNextImageKHR.html vkAcquireNextImageKHR registry at www.khronos.org>
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

-- | Success codes: 'VK_SUCCESS', 'VK_SUBOPTIMAL_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkQueuePresentKHR
--   >     ( VkQueue queue
--   >     , const VkPresentInfoKHR* pPresentInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkQueuePresentKHR.html vkQueuePresentKHR registry at www.khronos.org>
foreign import ccall unsafe "vkQueuePresentKHR" vkQueuePresentKHR
               :: VkQueue -- ^ queue
                          -> Ptr VkPresentInfoKHR -- ^ pPresentInfo
                                                  -> IO VkResult

pattern VK_KHR_SWAPCHAIN_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_SWAPCHAIN_SPEC_VERSION = 68

type VK_KHR_SWAPCHAIN_SPEC_VERSION = 68

pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME :: CString

pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME <-
        (is_VK_KHR_SWAPCHAIN_EXTENSION_NAME -> True)
  where VK_KHR_SWAPCHAIN_EXTENSION_NAME
          = _VK_KHR_SWAPCHAIN_EXTENSION_NAME

_VK_KHR_SWAPCHAIN_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_SWAPCHAIN_EXTENSION_NAME #-}
_VK_KHR_SWAPCHAIN_EXTENSION_NAME = Ptr "VK_KHR_swapchain\NUL"##

is_VK_KHR_SWAPCHAIN_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_SWAPCHAIN_EXTENSION_NAME #-}
is_VK_KHR_SWAPCHAIN_EXTENSION_NAME
  = (_VK_KHR_SWAPCHAIN_EXTENSION_NAME ==)

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
