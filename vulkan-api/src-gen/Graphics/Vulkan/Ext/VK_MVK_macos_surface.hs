{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_MVK_macos_surface
       (-- * Vulkan extension: @VK_MVK_macos_surface@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Bill Hollings @billhollings@
        --
        -- author: @MVK@
        --
        -- type: @instance@
        --
        -- Extension number: @124@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_MACOS_MVK@
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkMacOSSurfaceCreateInfoMVK,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        vkCreateMacOSSurfaceMVK, vkCreateMacOSSurfaceMVKSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        VK_MVK_MACOS_SURFACE_SPEC_VERSION,
        pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION,
        VK_MVK_MACOS_SURFACE_EXTENSION_NAME,
        pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK)
       where
import           GHC.Ptr                                                  (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkInternalAllocationType
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
import           Graphics.Vulkan.Types.Struct.VkMacOSSurfaceCreateInfoMVK

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateMacOSSurfaceMVK
--   >     ( VkInstance instance
--   >     , const VkMacOSSurfaceCreateInfoMVK* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCreateMacOSSurfaceMVK.html vkCreateMacOSSurfaceMVK registry at www.khronos.org>
foreign import ccall unsafe "vkCreateMacOSSurfaceMVK"
               vkCreateMacOSSurfaceMVK ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkMacOSSurfaceCreateInfoMVK -- ^ pCreateInfo
                                                 ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateMacOSSurfaceMVK
--   >     ( VkInstance instance
--   >     , const VkMacOSSurfaceCreateInfoMVK* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCreateMacOSSurfaceMVK.html vkCreateMacOSSurfaceMVK registry at www.khronos.org>
foreign import ccall safe "vkCreateMacOSSurfaceMVK"
               vkCreateMacOSSurfaceMVKSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkMacOSSurfaceCreateInfoMVK -- ^ pCreateInfo
                                                 ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION = 2

type VK_MVK_MACOS_SURFACE_SPEC_VERSION = 2

pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME :: CString

pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME <-
        (is_VK_MVK_MACOS_SURFACE_EXTENSION_NAME -> True)
  where VK_MVK_MACOS_SURFACE_EXTENSION_NAME
          = _VK_MVK_MACOS_SURFACE_EXTENSION_NAME

{-# INLINE _VK_MVK_MACOS_SURFACE_EXTENSION_NAME #-}

_VK_MVK_MACOS_SURFACE_EXTENSION_NAME :: CString
_VK_MVK_MACOS_SURFACE_EXTENSION_NAME
  = Ptr "VK_MVK_macos_surface\NUL"#

{-# INLINE is_VK_MVK_MACOS_SURFACE_EXTENSION_NAME #-}

is_VK_MVK_MACOS_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_MVK_MACOS_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_MVK_MACOS_SURFACE_EXTENSION_NAME

type VK_MVK_MACOS_SURFACE_EXTENSION_NAME = "VK_MVK_macos_surface"

pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK =
        VkStructureType 1000123000
