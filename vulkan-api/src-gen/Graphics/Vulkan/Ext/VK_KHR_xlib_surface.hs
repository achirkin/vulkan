{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_xlib_surface
       (-- * Vulkan extension: @VK_KHR_xlib_surface@
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
        -- Extension number: @5@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_XLIB_KHR@
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkXlibSurfaceCreateInfoKHR,
        -- > #include "vk_platform.h"
        --
        -- > #include <X11/Xlib.h>
        vkCreateXlibSurfaceKHR,
        vkGetPhysicalDeviceXlibPresentationSupportKHR,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Include,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        VK_KHR_XLIB_SURFACE_SPEC_VERSION,
        pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION,
        VK_KHR_XLIB_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR)
       where
import           GHC.Ptr                                                 (Ptr (..))
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
import           Graphics.Vulkan.Types.Struct.VkXlibSurfaceCreateInfoKHR

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCreateXlibSurfaceKHR.html vkCreateXlibSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateXlibSurfaceKHR"
               vkCreateXlibSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkXlibSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

-- | > VkBool32 vkGetPhysicalDeviceXlibPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , Display* dpy
--   >     , VisualID visualID
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceXlibPresentationSupportKHR.html vkGetPhysicalDeviceXlibPresentationSupportKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceXlibPresentationSupportKHR"
               vkGetPhysicalDeviceXlibPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ queueFamilyIndex
                        -> Ptr Display -- ^ dpy
                                       -> VisualID -- ^ visualID
                                                   -> IO VkBool32

pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION = 6

type VK_KHR_XLIB_SURFACE_SPEC_VERSION = 6

pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_XLIB_SURFACE_EXTENSION_NAME -> True)
  where VK_KHR_XLIB_SURFACE_EXTENSION_NAME
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
