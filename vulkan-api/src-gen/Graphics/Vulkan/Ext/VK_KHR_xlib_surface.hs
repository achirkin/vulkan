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
        -- platform: @xlib@
        --
        -- Extension number: @5@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.PlatformXlibKhr,
        -- > #include "vk_platform.h"
        VkCreateXlibSurfaceKHR, pattern VkCreateXlibSurfaceKHR,
        HS_vkCreateXlibSurfaceKHR, PFN_vkCreateXlibSurfaceKHR,
        vkCreateXlibSurfaceKHR, vkCreateXlibSurfaceKHRSafe,
        VkGetPhysicalDeviceXlibPresentationSupportKHR,
        pattern VkGetPhysicalDeviceXlibPresentationSupportKHR,
        HS_vkGetPhysicalDeviceXlibPresentationSupportKHR,
        PFN_vkGetPhysicalDeviceXlibPresentationSupportKHR,
        vkGetPhysicalDeviceXlibPresentationSupportKHR,
        vkGetPhysicalDeviceXlibPresentationSupportKHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.InternalAllocationType,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Enum.SystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Include,
        module Graphics.Vulkan.Types.Struct.AllocationCallbacks,
        VK_KHR_XLIB_SURFACE_SPEC_VERSION,
        pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION,
        VK_KHR_XLIB_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR)
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
import           Graphics.Vulkan.Types.Struct.PlatformXlibKhr
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

pattern VkCreateXlibSurfaceKHR :: CString

pattern VkCreateXlibSurfaceKHR <-
        (is_VkCreateXlibSurfaceKHR -> True)
  where VkCreateXlibSurfaceKHR = _VkCreateXlibSurfaceKHR

{-# INLINE _VkCreateXlibSurfaceKHR #-}

_VkCreateXlibSurfaceKHR :: CString
_VkCreateXlibSurfaceKHR = Ptr "vkCreateXlibSurfaceKHR\NUL"#

{-# INLINE is_VkCreateXlibSurfaceKHR #-}

is_VkCreateXlibSurfaceKHR :: CString -> Bool
is_VkCreateXlibSurfaceKHR
  = (EQ ==) . cmpCStrings _VkCreateXlibSurfaceKHR

type VkCreateXlibSurfaceKHR = "vkCreateXlibSurfaceKHR"

#ifdef NATIVE_FFI_VK_VERSION_1_0
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateXlibSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkXlibSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateXlibSurfaceKHR vkCreateXlibSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkCreateXlibSurfaceKHR"
               vkCreateXlibSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkXlibSurfaceCreateInfoKHR -- ^ pCreateInfo
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
-- > VkResult vkCreateXlibSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkXlibSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateXlibSurfaceKHR vkCreateXlibSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is disabled, so this function is looked up
--           dynamically at runtime;
--           @vkCreateXlibSurfaceKHRSafe@ and @vkCreateXlibSurfaceKHR@ are synonyms.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateXlibSurfaceKHR <- vkGetInstanceProc @VkCreateXlibSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateXlibSurfaceKHR <- vkGetProc @VkCreateXlibSurfaceKHR
--
vkCreateXlibSurfaceKHR ::
                       VkInstance -- ^ instance
                                  ->
                         Ptr VkXlibSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                        ->
                           Ptr VkAllocationCallbacks -- ^ pAllocator
                                                     -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                         -> IO VkResult
vkCreateXlibSurfaceKHR
  = unsafeDupablePerformIO (vkGetProc @VkCreateXlibSurfaceKHR)

{-# NOINLINE vkCreateXlibSurfaceKHR #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_0
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateXlibSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkXlibSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateXlibSurfaceKHR vkCreateXlibSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkCreateXlibSurfaceKHR"
               vkCreateXlibSurfaceKHRSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkXlibSurfaceCreateInfoKHR -- ^ pCreateInfo
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
-- > VkResult vkCreateXlibSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkXlibSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateXlibSurfaceKHR vkCreateXlibSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is disabled, so this function is looked up
--           dynamically at runtime;
--           @vkCreateXlibSurfaceKHRSafe@ and @vkCreateXlibSurfaceKHR@ are synonyms.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateXlibSurfaceKHR <- vkGetInstanceProc @VkCreateXlibSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateXlibSurfaceKHR <- vkGetProc @VkCreateXlibSurfaceKHR
--
vkCreateXlibSurfaceKHRSafe ::
                           VkInstance -- ^ instance
                                      ->
                             Ptr VkXlibSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                            ->
                               Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                             -> IO VkResult
vkCreateXlibSurfaceKHRSafe = vkCreateXlibSurfaceKHR

{-# INLINE vkCreateXlibSurfaceKHRSafe #-}
#endif

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateXlibSurfaceKHR vkCreateXlibSurfaceKHR registry at www.khronos.org>
type HS_vkCreateXlibSurfaceKHR =
     VkInstance -- ^ instance
                ->
       Ptr VkXlibSurfaceCreateInfoKHR -- ^ pCreateInfo
                                      ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateXlibSurfaceKHR = FunPtr HS_vkCreateXlibSurfaceKHR

foreign import ccall "dynamic" unwrapVkCreateXlibSurfaceKHR ::
               PFN_vkCreateXlibSurfaceKHR -> HS_vkCreateXlibSurfaceKHR

instance VulkanProc "vkCreateXlibSurfaceKHR" where
        type VkProcType "vkCreateXlibSurfaceKHR" =
             HS_vkCreateXlibSurfaceKHR
        vkProcSymbol = _VkCreateXlibSurfaceKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateXlibSurfaceKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceXlibPresentationSupportKHR :: CString

pattern VkGetPhysicalDeviceXlibPresentationSupportKHR <-
        (is_VkGetPhysicalDeviceXlibPresentationSupportKHR -> True)
  where VkGetPhysicalDeviceXlibPresentationSupportKHR
          = _VkGetPhysicalDeviceXlibPresentationSupportKHR

{-# INLINE _VkGetPhysicalDeviceXlibPresentationSupportKHR #-}

_VkGetPhysicalDeviceXlibPresentationSupportKHR :: CString
_VkGetPhysicalDeviceXlibPresentationSupportKHR
  = Ptr "vkGetPhysicalDeviceXlibPresentationSupportKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceXlibPresentationSupportKHR #-}

is_VkGetPhysicalDeviceXlibPresentationSupportKHR :: CString -> Bool
is_VkGetPhysicalDeviceXlibPresentationSupportKHR
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceXlibPresentationSupportKHR

type VkGetPhysicalDeviceXlibPresentationSupportKHR =
     "vkGetPhysicalDeviceXlibPresentationSupportKHR"

#ifdef NATIVE_FFI_VK_VERSION_1_0
-- |
-- > VkBool32 vkGetPhysicalDeviceXlibPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , Display* dpy
-- >     , VisualID visualID
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceXlibPresentationSupportKHR vkGetPhysicalDeviceXlibPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe
               "vkGetPhysicalDeviceXlibPresentationSupportKHR"
               vkGetPhysicalDeviceXlibPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ queueFamilyIndex
                        -> Ptr Display -- ^ dpy
                                       -> VisualID -- ^ visualID
                                                   -> IO VkBool32

#else
-- |
-- > VkBool32 vkGetPhysicalDeviceXlibPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , Display* dpy
-- >     , VisualID visualID
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceXlibPresentationSupportKHR vkGetPhysicalDeviceXlibPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is disabled, so this function is looked up
--           dynamically at runtime;
--           @vkGetPhysicalDeviceXlibPresentationSupportKHRSafe@ and @vkGetPhysicalDeviceXlibPresentationSupportKHR@ are synonyms.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceXlibPresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceXlibPresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceXlibPresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceXlibPresentationSupportKHR
--
vkGetPhysicalDeviceXlibPresentationSupportKHR ::
                                              VkPhysicalDevice -- ^ physicalDevice
                                                               ->
                                                Word32 -- ^ queueFamilyIndex
                                                       -> Ptr Display -- ^ dpy
                                                                      -> VisualID -- ^ visualID
                                                                                  -> IO VkBool32
vkGetPhysicalDeviceXlibPresentationSupportKHR
  = unsafeDupablePerformIO
      (vkGetProc @VkGetPhysicalDeviceXlibPresentationSupportKHR)

{-# NOINLINE vkGetPhysicalDeviceXlibPresentationSupportKHR #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_0
-- |
-- > VkBool32 vkGetPhysicalDeviceXlibPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , Display* dpy
-- >     , VisualID visualID
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceXlibPresentationSupportKHR vkGetPhysicalDeviceXlibPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe
               "vkGetPhysicalDeviceXlibPresentationSupportKHR"
               vkGetPhysicalDeviceXlibPresentationSupportKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ queueFamilyIndex
                        -> Ptr Display -- ^ dpy
                                       -> VisualID -- ^ visualID
                                                   -> IO VkBool32

#else
-- |
-- > VkBool32 vkGetPhysicalDeviceXlibPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , Display* dpy
-- >     , VisualID visualID
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceXlibPresentationSupportKHR vkGetPhysicalDeviceXlibPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is disabled, so this function is looked up
--           dynamically at runtime;
--           @vkGetPhysicalDeviceXlibPresentationSupportKHRSafe@ and @vkGetPhysicalDeviceXlibPresentationSupportKHR@ are synonyms.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceXlibPresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceXlibPresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceXlibPresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceXlibPresentationSupportKHR
--
vkGetPhysicalDeviceXlibPresentationSupportKHRSafe ::
                                                  VkPhysicalDevice -- ^ physicalDevice
                                                                   ->
                                                    Word32 -- ^ queueFamilyIndex
                                                           -> Ptr Display -- ^ dpy
                                                                          -> VisualID -- ^ visualID
                                                                                      -> IO VkBool32
vkGetPhysicalDeviceXlibPresentationSupportKHRSafe
  = vkGetPhysicalDeviceXlibPresentationSupportKHR

{-# INLINE vkGetPhysicalDeviceXlibPresentationSupportKHRSafe #-}
#endif

-- | > VkBool32 vkGetPhysicalDeviceXlibPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , Display* dpy
--   >     , VisualID visualID
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceXlibPresentationSupportKHR vkGetPhysicalDeviceXlibPresentationSupportKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceXlibPresentationSupportKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Word32 -- ^ queueFamilyIndex
              -> Ptr Display -- ^ dpy
                             -> VisualID -- ^ visualID
                                         -> IO VkBool32

type PFN_vkGetPhysicalDeviceXlibPresentationSupportKHR =
     FunPtr HS_vkGetPhysicalDeviceXlibPresentationSupportKHR

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceXlibPresentationSupportKHR ::
               PFN_vkGetPhysicalDeviceXlibPresentationSupportKHR ->
                 HS_vkGetPhysicalDeviceXlibPresentationSupportKHR

instance VulkanProc "vkGetPhysicalDeviceXlibPresentationSupportKHR"
         where
        type VkProcType "vkGetPhysicalDeviceXlibPresentationSupportKHR" =
             HS_vkGetPhysicalDeviceXlibPresentationSupportKHR
        vkProcSymbol = _VkGetPhysicalDeviceXlibPresentationSupportKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr
          = unwrapVkGetPhysicalDeviceXlibPresentationSupportKHR

        {-# INLINE unwrapVkProcPtr #-}

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
