{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_NN_vi_surface
       (-- * Vulkan extension: @VK_NN_vi_surface@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Mathias Heyer @mheyer@
        --
        -- author: @NN@
        --
        -- type: @instance@
        --
        -- platform: @vi@
        --
        -- Extension number: @63@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkViSurfaceCreateInfoNN,
        -- > #include "vk_platform.h"
        vkCreateViSurfaceNN, vkCreateViSurfaceNNSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        VK_NN_VI_SURFACE_SPEC_VERSION,
        pattern VK_NN_VI_SURFACE_SPEC_VERSION,
        VK_NN_VI_SURFACE_EXTENSION_NAME,
        pattern VK_NN_VI_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN)
       where
import           GHC.Ptr                                              (Ptr (..))
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
import           Graphics.Vulkan.Types.Struct.VkViSurfaceCreateInfoNN

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateViSurfaceNN
--   >     ( VkInstance instance
--   >     , const VkViSurfaceCreateInfoNN* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateViSurfaceNN.html vkCreateViSurfaceNN registry at www.khronos.org>
foreign import ccall unsafe "vkCreateViSurfaceNN"
               vkCreateViSurfaceNN ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkViSurfaceCreateInfoNN -- ^ pCreateInfo
                                             ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateViSurfaceNN
--   >     ( VkInstance instance
--   >     , const VkViSurfaceCreateInfoNN* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCreateViSurfaceNN.html vkCreateViSurfaceNN registry at www.khronos.org>
foreign import ccall safe "vkCreateViSurfaceNN"
               vkCreateViSurfaceNNSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkViSurfaceCreateInfoNN -- ^ pCreateInfo
                                             ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

pattern VK_NN_VI_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NN_VI_SURFACE_SPEC_VERSION = 1

type VK_NN_VI_SURFACE_SPEC_VERSION = 1

pattern VK_NN_VI_SURFACE_EXTENSION_NAME :: CString

pattern VK_NN_VI_SURFACE_EXTENSION_NAME <-
        (is_VK_NN_VI_SURFACE_EXTENSION_NAME -> True)
  where VK_NN_VI_SURFACE_EXTENSION_NAME
          = _VK_NN_VI_SURFACE_EXTENSION_NAME

{-# INLINE _VK_NN_VI_SURFACE_EXTENSION_NAME #-}

_VK_NN_VI_SURFACE_EXTENSION_NAME :: CString
_VK_NN_VI_SURFACE_EXTENSION_NAME = Ptr "VK_NN_vi_surface\NUL"#

{-# INLINE is_VK_NN_VI_SURFACE_EXTENSION_NAME #-}

is_VK_NN_VI_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_NN_VI_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_NN_VI_SURFACE_EXTENSION_NAME

type VK_NN_VI_SURFACE_EXTENSION_NAME = "VK_NN_vi_surface"

pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN =
        VkStructureType 1000062000
