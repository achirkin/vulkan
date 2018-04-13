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
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.PlatformWaylandKhr,
        -- > #include "vk_platform.h"
        VkCreateWaylandSurfaceKHR, pattern VkCreateWaylandSurfaceKHR,
        HS_vkCreateWaylandSurfaceKHR, PFN_vkCreateWaylandSurfaceKHR,
        vkCreateWaylandSurfaceKHR, vkCreateWaylandSurfaceKHRSafe,
        VkGetPhysicalDeviceWaylandPresentationSupportKHR,
        pattern VkGetPhysicalDeviceWaylandPresentationSupportKHR,
        HS_vkGetPhysicalDeviceWaylandPresentationSupportKHR,
        PFN_vkGetPhysicalDeviceWaylandPresentationSupportKHR,
        vkGetPhysicalDeviceWaylandPresentationSupportKHR,
        vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.InternalAllocationType,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Enum.SystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Include,
        module Graphics.Vulkan.Types.Struct.AllocationCallbacks,
        VK_KHR_WAYLAND_SURFACE_SPEC_VERSION,
        pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION,
        VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR)
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
import           Graphics.Vulkan.Types.Struct.PlatformWaylandKhr
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

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

#ifdef NATIVE_FFI_VK_VERSION_1_0
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateWaylandSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkWaylandSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateWaylandSurfaceKHR vkCreateWaylandSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkCreateWaylandSurfaceKHR"
               vkCreateWaylandSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkWaylandSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateWaylandSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkWaylandSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateWaylandSurfaceKHR vkCreateWaylandSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is disabled, so this function is looked up
--           dynamically at runtime;
--           @vkCreateWaylandSurfaceKHRSafe@ and @vkCreateWaylandSurfaceKHR@ are synonyms.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateWaylandSurfaceKHR <- vkGetInstanceProc @VkCreateWaylandSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateWaylandSurfaceKHR <- vkGetProc @VkCreateWaylandSurfaceKHR
--
vkCreateWaylandSurfaceKHR ::
                          VkInstance -- ^ instance
                                     ->
                            Ptr VkWaylandSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                              ->
                              Ptr VkAllocationCallbacks -- ^ pAllocator
                                                        -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                            -> IO VkResult
vkCreateWaylandSurfaceKHR
  = unsafeDupablePerformIO (vkGetProc @VkCreateWaylandSurfaceKHR)

{-# NOINLINE vkCreateWaylandSurfaceKHR #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_0
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateWaylandSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkWaylandSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateWaylandSurfaceKHR vkCreateWaylandSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkCreateWaylandSurfaceKHR"
               vkCreateWaylandSurfaceKHRSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkWaylandSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateWaylandSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkWaylandSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateWaylandSurfaceKHR vkCreateWaylandSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is disabled, so this function is looked up
--           dynamically at runtime;
--           @vkCreateWaylandSurfaceKHRSafe@ and @vkCreateWaylandSurfaceKHR@ are synonyms.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateWaylandSurfaceKHR <- vkGetInstanceProc @VkCreateWaylandSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateWaylandSurfaceKHR <- vkGetProc @VkCreateWaylandSurfaceKHR
--
vkCreateWaylandSurfaceKHRSafe ::
                              VkInstance -- ^ instance
                                         ->
                                Ptr VkWaylandSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                                  ->
                                  Ptr VkAllocationCallbacks -- ^ pAllocator
                                                            -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                                -> IO VkResult
vkCreateWaylandSurfaceKHRSafe = vkCreateWaylandSurfaceKHR

{-# INLINE vkCreateWaylandSurfaceKHRSafe #-}
#endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateWaylandSurfaceKHR vkCreateWaylandSurfaceKHR registry at www.khronos.org>
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

instance VulkanProc "vkCreateWaylandSurfaceKHR" where
        type VkProcType "vkCreateWaylandSurfaceKHR" =
             HS_vkCreateWaylandSurfaceKHR
        vkProcSymbol = _VkCreateWaylandSurfaceKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateWaylandSurfaceKHR

        {-# INLINE unwrapVkProcPtr #-}

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

#ifdef NATIVE_FFI_VK_VERSION_1_0
-- |
-- > VkBool32 vkGetPhysicalDeviceWaylandPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , struct wl_display* display
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceWaylandPresentationSupportKHR vkGetPhysicalDeviceWaylandPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe
               "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
               vkGetPhysicalDeviceWaylandPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> Ptr WlDisplay -- ^ display
                                                           -> IO VkBool32

#else
-- |
-- > VkBool32 vkGetPhysicalDeviceWaylandPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , struct wl_display* display
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceWaylandPresentationSupportKHR vkGetPhysicalDeviceWaylandPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is disabled, so this function is looked up
--           dynamically at runtime;
--           @vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe@ and @vkGetPhysicalDeviceWaylandPresentationSupportKHR@ are synonyms.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceWaylandPresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceWaylandPresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceWaylandPresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceWaylandPresentationSupportKHR
--
vkGetPhysicalDeviceWaylandPresentationSupportKHR ::
                                                 VkPhysicalDevice -- ^ physicalDevice
                                                                  ->
                                                   Word32 -- ^ queueFamilyIndex
                                                          -> Ptr WlDisplay -- ^ display
                                                                           -> IO VkBool32
vkGetPhysicalDeviceWaylandPresentationSupportKHR
  = unsafeDupablePerformIO
      (vkGetProc @VkGetPhysicalDeviceWaylandPresentationSupportKHR)

{-# NOINLINE vkGetPhysicalDeviceWaylandPresentationSupportKHR #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_0
-- |
-- > VkBool32 vkGetPhysicalDeviceWaylandPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , struct wl_display* display
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceWaylandPresentationSupportKHR vkGetPhysicalDeviceWaylandPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe
               "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
               vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> Ptr WlDisplay -- ^ display
                                                           -> IO VkBool32

#else
-- |
-- > VkBool32 vkGetPhysicalDeviceWaylandPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , struct wl_display* display
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceWaylandPresentationSupportKHR vkGetPhysicalDeviceWaylandPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is disabled, so this function is looked up
--           dynamically at runtime;
--           @vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe@ and @vkGetPhysicalDeviceWaylandPresentationSupportKHR@ are synonyms.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceWaylandPresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceWaylandPresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceWaylandPresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceWaylandPresentationSupportKHR
--
vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe ::
                                                     VkPhysicalDevice -- ^ physicalDevice
                                                                      ->
                                                       Word32 -- ^ queueFamilyIndex
                                                              -> Ptr WlDisplay -- ^ display
                                                                               -> IO VkBool32
vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe
  = vkGetPhysicalDeviceWaylandPresentationSupportKHR

{-# INLINE vkGetPhysicalDeviceWaylandPresentationSupportKHRSafe #-}
#endif

-- | > VkBool32 vkGetPhysicalDeviceWaylandPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , struct wl_display* display
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceWaylandPresentationSupportKHR vkGetPhysicalDeviceWaylandPresentationSupportKHR registry at www.khronos.org>
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

instance VulkanProc
           "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
         where
        type VkProcType "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
             = HS_vkGetPhysicalDeviceWaylandPresentationSupportKHR
        vkProcSymbol = _VkGetPhysicalDeviceWaylandPresentationSupportKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr
          = unwrapVkGetPhysicalDeviceWaylandPresentationSupportKHR

        {-# INLINE unwrapVkProcPtr #-}

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
