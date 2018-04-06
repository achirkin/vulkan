{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_wayland_surface
       (-- * Vulkan extension: @VK_KHR_wayland_surface@
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
        -- platform: @wayland@
        --
        -- Extension number: @7@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkWaylandSurfaceCreateInfoKHR,
        -- > #include "vk_platform.h"
        VkCreateWaylandSurfaceKHR, pattern VkCreateWaylandSurfaceKHR,
        HS_vkCreateWaylandSurfaceKHR, PFN_vkCreateWaylandSurfaceKHR,
        unwrapVkCreateWaylandSurfaceKHR, vkCreateWaylandSurfaceKHR,
        vkCreateWaylandSurfaceKHRSafe,
        VkGetPhysicalDeviceWaylandPresentationSupportKHR,
        pattern VkGetPhysicalDeviceWaylandPresentationSupportKHR,
        HS_vkGetPhysicalDeviceWaylandPresentationSupportKHR,
        PFN_vkGetPhysicalDeviceWaylandPresentationSupportKHR,
        unwrapVkGetPhysicalDeviceWaylandPresentationSupportKHR,
        vkGetPhysicalDeviceWaylandPresentationSupportKHR,
        vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Include,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        VK_KHR_WAYLAND_SURFACE_SPEC_VERSION,
        pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION,
        VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR)
       where
import           GHC.Ptr                                                    (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.InstanceProc                       (VulkanInstanceProc (..))
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
import           Graphics.Vulkan.Types.Struct.VkWaylandSurfaceCreateInfoKHR

pattern VkCreateWaylandSurfaceKHR :: CString

pattern VkCreateWaylandSurfaceKHR <-
        (is_VkCreateWaylandSurfaceKHR -> True)
  where VkCreateWaylandSurfaceKHR = _VkCreateWaylandSurfaceKHR

{-# INLINE _VkCreateWaylandSurfaceKHR #-}

_VkCreateWaylandSurfaceKHR :: CString
_VkCreateWaylandSurfaceKHR = Ptr "vkCreateWaylandSurfaceKHR\NUL"#

{-# INLINE is_VkCreateWaylandSurfaceKHR #-}

is_VkCreateWaylandSurfaceKHR :: CString -> Bool
is_VkCreateWaylandSurfaceKHR
  = (EQ ==) . cmpCStrings _VkCreateWaylandSurfaceKHR

type VkCreateWaylandSurfaceKHR = "vkCreateWaylandSurfaceKHR"

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateWaylandSurfaceKHR.html vkCreateWaylandSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateWaylandSurfaceKHR"
               vkCreateWaylandSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkWaylandSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateWaylandSurfaceKHR.html vkCreateWaylandSurfaceKHR registry at www.khronos.org>
foreign import ccall safe "vkCreateWaylandSurfaceKHR"
               vkCreateWaylandSurfaceKHRSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkWaylandSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateWaylandSurfaceKHR.html vkCreateWaylandSurfaceKHR registry at www.khronos.org>
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

foreign import ccall "dynamic" unwrapVkCreateWaylandSurfaceKHR ::
               PFN_vkCreateWaylandSurfaceKHR -> HS_vkCreateWaylandSurfaceKHR

instance VulkanInstanceProc "vkCreateWaylandSurfaceKHR" where
        type VkInstanceProcType "vkCreateWaylandSurfaceKHR" =
             HS_vkCreateWaylandSurfaceKHR
        vkInstanceProcSymbol = _VkCreateWaylandSurfaceKHR

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkCreateWaylandSurfaceKHR

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkGetPhysicalDeviceWaylandPresentationSupportKHR :: CString

pattern VkGetPhysicalDeviceWaylandPresentationSupportKHR <-
        (is_VkGetPhysicalDeviceWaylandPresentationSupportKHR -> True)
  where VkGetPhysicalDeviceWaylandPresentationSupportKHR
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

-- | > VkBool32 vkGetPhysicalDeviceWaylandPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , struct wl_display* display
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceWaylandPresentationSupportKHR.html vkGetPhysicalDeviceWaylandPresentationSupportKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
               vkGetPhysicalDeviceWaylandPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> Ptr WlDisplay -- ^ display
                                                           -> IO VkBool32

-- | > VkBool32 vkGetPhysicalDeviceWaylandPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , struct wl_display* display
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceWaylandPresentationSupportKHR.html vkGetPhysicalDeviceWaylandPresentationSupportKHR registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
               vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> Ptr WlDisplay -- ^ display
                                                           -> IO VkBool32

-- | > VkBool32 vkGetPhysicalDeviceWaylandPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , struct wl_display* display
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceWaylandPresentationSupportKHR.html vkGetPhysicalDeviceWaylandPresentationSupportKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceWaylandPresentationSupportKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Word32 -- ^ queueFamilyIndex
                                -> Ptr WlDisplay -- ^ display
                                                 -> IO VkBool32

type PFN_vkGetPhysicalDeviceWaylandPresentationSupportKHR =
     FunPtr HS_vkGetPhysicalDeviceWaylandPresentationSupportKHR

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceWaylandPresentationSupportKHR ::
               PFN_vkGetPhysicalDeviceWaylandPresentationSupportKHR ->
                 HS_vkGetPhysicalDeviceWaylandPresentationSupportKHR

instance VulkanInstanceProc
           "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
         where
        type VkInstanceProcType
               "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
             = HS_vkGetPhysicalDeviceWaylandPresentationSupportKHR
        vkInstanceProcSymbol
          = _VkGetPhysicalDeviceWaylandPresentationSupportKHR

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc
          = unwrapVkGetPhysicalDeviceWaylandPresentationSupportKHR

        {-# INLINE unwrapVkInstanceProc #-}

pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION = 6

type VK_KHR_WAYLAND_SURFACE_SPEC_VERSION = 6

pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME -> True)
  where VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
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
