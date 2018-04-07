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
module Graphics.Vulkan.Ext.VK_KHR_win32_surface
       (-- * Vulkan extension: @VK_KHR_win32_surface@
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
        -- platform: @win32@
        --
        -- Extension number: @10@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkWin32SurfaceCreateInfoKHR,
        -- > #include "vk_platform.h"
        VkCreateWin32SurfaceKHR, pattern VkCreateWin32SurfaceKHR,
        HS_vkCreateWin32SurfaceKHR, PFN_vkCreateWin32SurfaceKHR,
        unwrapVkCreateWin32SurfaceKHR, vkCreateWin32SurfaceKHR,
        vkCreateWin32SurfaceKHRSafe,
        VkGetPhysicalDeviceWin32PresentationSupportKHR,
        pattern VkGetPhysicalDeviceWin32PresentationSupportKHR,
        HS_vkGetPhysicalDeviceWin32PresentationSupportKHR,
        PFN_vkGetPhysicalDeviceWin32PresentationSupportKHR,
        unwrapVkGetPhysicalDeviceWin32PresentationSupportKHR,
        vkGetPhysicalDeviceWin32PresentationSupportKHR,
        vkGetPhysicalDeviceWin32PresentationSupportKHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Include,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        VK_KHR_WIN32_SURFACE_SPEC_VERSION,
        pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION,
        VK_KHR_WIN32_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR)
       where
import           GHC.Ptr                                                  (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                             (VulkanProc (..))
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
import           Graphics.Vulkan.Types.Struct.VkWin32SurfaceCreateInfoKHR

pattern VkCreateWin32SurfaceKHR :: CString

pattern VkCreateWin32SurfaceKHR <-
        (is_VkCreateWin32SurfaceKHR -> True)
  where VkCreateWin32SurfaceKHR = _VkCreateWin32SurfaceKHR

{-# INLINE _VkCreateWin32SurfaceKHR #-}

_VkCreateWin32SurfaceKHR :: CString
_VkCreateWin32SurfaceKHR = Ptr "vkCreateWin32SurfaceKHR\NUL"#

{-# INLINE is_VkCreateWin32SurfaceKHR #-}

is_VkCreateWin32SurfaceKHR :: CString -> Bool
is_VkCreateWin32SurfaceKHR
  = (EQ ==) . cmpCStrings _VkCreateWin32SurfaceKHR

type VkCreateWin32SurfaceKHR = "vkCreateWin32SurfaceKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateWin32SurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkWin32SurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateWin32SurfaceKHR.html vkCreateWin32SurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateWin32SurfaceKHR"
               vkCreateWin32SurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkWin32SurfaceCreateInfoKHR -- ^ pCreateInfo
                                                 ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateWin32SurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkWin32SurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateWin32SurfaceKHR.html vkCreateWin32SurfaceKHR registry at www.khronos.org>
foreign import ccall safe "vkCreateWin32SurfaceKHR"
               vkCreateWin32SurfaceKHRSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkWin32SurfaceCreateInfoKHR -- ^ pCreateInfo
                                                 ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateWin32SurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkWin32SurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateWin32SurfaceKHR.html vkCreateWin32SurfaceKHR registry at www.khronos.org>
type HS_vkCreateWin32SurfaceKHR =
     VkInstance -- ^ instance
                ->
       Ptr VkWin32SurfaceCreateInfoKHR -- ^ pCreateInfo
                                       ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateWin32SurfaceKHR =
     FunPtr HS_vkCreateWin32SurfaceKHR

foreign import ccall "dynamic" unwrapVkCreateWin32SurfaceKHR ::
               PFN_vkCreateWin32SurfaceKHR -> HS_vkCreateWin32SurfaceKHR

instance VulkanProc "vkCreateWin32SurfaceKHR" where
        type VkProcType "vkCreateWin32SurfaceKHR" =
             HS_vkCreateWin32SurfaceKHR
        vkProcSymbol = _VkCreateWin32SurfaceKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateWin32SurfaceKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceWin32PresentationSupportKHR :: CString

pattern VkGetPhysicalDeviceWin32PresentationSupportKHR <-
        (is_VkGetPhysicalDeviceWin32PresentationSupportKHR -> True)
  where VkGetPhysicalDeviceWin32PresentationSupportKHR
          = _VkGetPhysicalDeviceWin32PresentationSupportKHR

{-# INLINE _VkGetPhysicalDeviceWin32PresentationSupportKHR #-}

_VkGetPhysicalDeviceWin32PresentationSupportKHR :: CString
_VkGetPhysicalDeviceWin32PresentationSupportKHR
  = Ptr "vkGetPhysicalDeviceWin32PresentationSupportKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceWin32PresentationSupportKHR #-}

is_VkGetPhysicalDeviceWin32PresentationSupportKHR ::
                                                  CString -> Bool
is_VkGetPhysicalDeviceWin32PresentationSupportKHR
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceWin32PresentationSupportKHR

type VkGetPhysicalDeviceWin32PresentationSupportKHR =
     "vkGetPhysicalDeviceWin32PresentationSupportKHR"

-- | > VkBool32 vkGetPhysicalDeviceWin32PresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceWin32PresentationSupportKHR.html vkGetPhysicalDeviceWin32PresentationSupportKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceWin32PresentationSupportKHR"
               vkGetPhysicalDeviceWin32PresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> IO VkBool32

-- | > VkBool32 vkGetPhysicalDeviceWin32PresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceWin32PresentationSupportKHR.html vkGetPhysicalDeviceWin32PresentationSupportKHR registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceWin32PresentationSupportKHR"
               vkGetPhysicalDeviceWin32PresentationSupportKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> IO VkBool32

-- | > VkBool32 vkGetPhysicalDeviceWin32PresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceWin32PresentationSupportKHR.html vkGetPhysicalDeviceWin32PresentationSupportKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceWin32PresentationSupportKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Word32 -- ^ queueFamilyIndex
                                -> IO VkBool32

type PFN_vkGetPhysicalDeviceWin32PresentationSupportKHR =
     FunPtr HS_vkGetPhysicalDeviceWin32PresentationSupportKHR

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceWin32PresentationSupportKHR ::
               PFN_vkGetPhysicalDeviceWin32PresentationSupportKHR ->
                 HS_vkGetPhysicalDeviceWin32PresentationSupportKHR

instance VulkanProc
           "vkGetPhysicalDeviceWin32PresentationSupportKHR"
         where
        type VkProcType "vkGetPhysicalDeviceWin32PresentationSupportKHR" =
             HS_vkGetPhysicalDeviceWin32PresentationSupportKHR
        vkProcSymbol = _VkGetPhysicalDeviceWin32PresentationSupportKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr
          = unwrapVkGetPhysicalDeviceWin32PresentationSupportKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION = 6

type VK_KHR_WIN32_SURFACE_SPEC_VERSION = 6

pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_WIN32_SURFACE_EXTENSION_NAME -> True)
  where VK_KHR_WIN32_SURFACE_EXTENSION_NAME
          = _VK_KHR_WIN32_SURFACE_EXTENSION_NAME

{-# INLINE _VK_KHR_WIN32_SURFACE_EXTENSION_NAME #-}

_VK_KHR_WIN32_SURFACE_EXTENSION_NAME :: CString
_VK_KHR_WIN32_SURFACE_EXTENSION_NAME
  = Ptr "VK_KHR_win32_surface\NUL"#

{-# INLINE is_VK_KHR_WIN32_SURFACE_EXTENSION_NAME #-}

is_VK_KHR_WIN32_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_WIN32_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_WIN32_SURFACE_EXTENSION_NAME

type VK_KHR_WIN32_SURFACE_EXTENSION_NAME = "VK_KHR_win32_surface"

pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000009000
