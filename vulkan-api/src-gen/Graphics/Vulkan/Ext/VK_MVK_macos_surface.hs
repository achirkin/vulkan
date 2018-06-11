{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
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
        -- platform: @macos@
        --
        -- Extension number: @124@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.PlatformMacosMvk,
        module Graphics.Vulkan.Types.Enum.StructureType,
        -- > #include "vk_platform.h"
        VkCreateMacOSSurfaceMVK, pattern VkCreateMacOSSurfaceMVK,
        HS_vkCreateMacOSSurfaceMVK, PFN_vkCreateMacOSSurfaceMVK,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.InternalAllocationType,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Enum.SystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.AllocationCallbacks,
        VK_MVK_MACOS_SURFACE_SPEC_VERSION,
        pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION,
        VK_MVK_MACOS_SURFACE_EXTENSION_NAME,
        pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK)
       where
import           GHC.Ptr                                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                      (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.InternalAllocationType
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.SystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.AllocationCallbacks
import           Graphics.Vulkan.Types.Struct.PlatformMacosMvk

pattern VkCreateMacOSSurfaceMVK :: CString

pattern VkCreateMacOSSurfaceMVK <-
        (is_VkCreateMacOSSurfaceMVK -> True)
  where VkCreateMacOSSurfaceMVK = _VkCreateMacOSSurfaceMVK

{-# INLINE _VkCreateMacOSSurfaceMVK #-}

_VkCreateMacOSSurfaceMVK :: CString
_VkCreateMacOSSurfaceMVK = Ptr "vkCreateMacOSSurfaceMVK\NUL"#

{-# INLINE is_VkCreateMacOSSurfaceMVK #-}

is_VkCreateMacOSSurfaceMVK :: CString -> Bool
is_VkCreateMacOSSurfaceMVK
  = (EQ ==) . cmpCStrings _VkCreateMacOSSurfaceMVK

type VkCreateMacOSSurfaceMVK = "vkCreateMacOSSurfaceMVK"

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateMacOSSurfaceMVK vkCreateMacOSSurfaceMVK registry at www.khronos.org>
type HS_vkCreateMacOSSurfaceMVK =
     VkInstance -- ^ instance
                ->
       Ptr VkMacOSSurfaceCreateInfoMVK -- ^ pCreateInfo
                                       ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateMacOSSurfaceMVK =
     FunPtr HS_vkCreateMacOSSurfaceMVK

foreign import ccall unsafe "dynamic"
               unwrapVkCreateMacOSSurfaceMVKUnsafe ::
               PFN_vkCreateMacOSSurfaceMVK -> HS_vkCreateMacOSSurfaceMVK

foreign import ccall safe "dynamic"
               unwrapVkCreateMacOSSurfaceMVKSafe ::
               PFN_vkCreateMacOSSurfaceMVK -> HS_vkCreateMacOSSurfaceMVK

instance VulkanProc "vkCreateMacOSSurfaceMVK" where
        type VkProcType "vkCreateMacOSSurfaceMVK" =
             HS_vkCreateMacOSSurfaceMVK
        vkProcSymbol = _VkCreateMacOSSurfaceMVK

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkCreateMacOSSurfaceMVKUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkCreateMacOSSurfaceMVKSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
