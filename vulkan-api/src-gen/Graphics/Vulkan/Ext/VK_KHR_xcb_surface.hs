{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_xcb_surface
       (-- * Vulkan extension: @VK_KHR_xcb_surface@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @jessehall,Ian Elliott ianelliott@google.com@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- platform: @xcb@
        --
        -- Extension number: @6@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkXcbSurfaceCreateInfoKHR,
        -- > #include "vk_platform.h"
        vkCreateXcbSurfaceKHR, vkCreateXcbSurfaceKHRSafe,
        vkGetPhysicalDeviceXcbPresentationSupportKHR,
        vkGetPhysicalDeviceXcbPresentationSupportKHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Include,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        VK_KHR_XCB_SURFACE_SPEC_VERSION,
        pattern VK_KHR_XCB_SURFACE_SPEC_VERSION,
        VK_KHR_XCB_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR)
       where
import           GHC.Ptr                                                (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkInternalAllocationType
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Include
import           Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
import           Graphics.Vulkan.Types.Struct.VkXcbSurfaceCreateInfoKHR

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateXcbSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkXcbSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateXcbSurfaceKHR.html vkCreateXcbSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateXcbSurfaceKHR"
               vkCreateXcbSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkXcbSurfaceCreateInfoKHR -- ^ pCreateInfo
                                               ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateXcbSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkXcbSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateXcbSurfaceKHR.html vkCreateXcbSurfaceKHR registry at www.khronos.org>
foreign import ccall safe "vkCreateXcbSurfaceKHR"
               vkCreateXcbSurfaceKHRSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkXcbSurfaceCreateInfoKHR -- ^ pCreateInfo
                                               ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

-- | > VkBool32 vkGetPhysicalDeviceXcbPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , xcb_connection_t* connection
--   >     , xcb_visualid_t visual_id
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceXcbPresentationSupportKHR.html vkGetPhysicalDeviceXcbPresentationSupportKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceXcbPresentationSupportKHR"
               vkGetPhysicalDeviceXcbPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ queueFamilyIndex
                        -> Ptr XcbConnectionT -- ^ connection
                                              -> XcbVisualidT -- ^ visual_id
                                                              -> IO VkBool32

-- | > VkBool32 vkGetPhysicalDeviceXcbPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , xcb_connection_t* connection
--   >     , xcb_visualid_t visual_id
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceXcbPresentationSupportKHR.html vkGetPhysicalDeviceXcbPresentationSupportKHR registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceXcbPresentationSupportKHR"
               vkGetPhysicalDeviceXcbPresentationSupportKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ queueFamilyIndex
                        -> Ptr XcbConnectionT -- ^ connection
                                              -> XcbVisualidT -- ^ visual_id
                                                              -> IO VkBool32

pattern VK_KHR_XCB_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_XCB_SURFACE_SPEC_VERSION = 6

type VK_KHR_XCB_SURFACE_SPEC_VERSION = 6

pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_XCB_SURFACE_EXTENSION_NAME -> True)
  where VK_KHR_XCB_SURFACE_EXTENSION_NAME
          = _VK_KHR_XCB_SURFACE_EXTENSION_NAME

{-# INLINE _VK_KHR_XCB_SURFACE_EXTENSION_NAME #-}

_VK_KHR_XCB_SURFACE_EXTENSION_NAME :: CString
_VK_KHR_XCB_SURFACE_EXTENSION_NAME = Ptr "VK_KHR_xcb_surface\NUL"#

{-# INLINE is_VK_KHR_XCB_SURFACE_EXTENSION_NAME #-}

is_VK_KHR_XCB_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_XCB_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_XCB_SURFACE_EXTENSION_NAME

type VK_KHR_XCB_SURFACE_EXTENSION_NAME = "VK_KHR_xcb_surface"

pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000005000
