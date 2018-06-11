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
module Graphics.Vulkan.Ext.VK_MVK_ios_surface
       (-- * Vulkan extension: @VK_MVK_ios_surface@
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
        -- platform: @ios@
        --
        -- Extension number: @123@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.PlatformIosMvk,
        module Graphics.Vulkan.Types.Enum.StructureType,
        -- > #include "vk_platform.h"
        VkCreateIOSSurfaceMVK, pattern VkCreateIOSSurfaceMVK,
        HS_vkCreateIOSSurfaceMVK, PFN_vkCreateIOSSurfaceMVK,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.InternalAllocationType,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Enum.SystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.AllocationCallbacks,
        VK_MVK_IOS_SURFACE_SPEC_VERSION,
        pattern VK_MVK_IOS_SURFACE_SPEC_VERSION,
        VK_MVK_IOS_SURFACE_EXTENSION_NAME,
        pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK)
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
import           Graphics.Vulkan.Types.Struct.PlatformIosMvk

pattern VkCreateIOSSurfaceMVK :: CString

pattern VkCreateIOSSurfaceMVK <- (is_VkCreateIOSSurfaceMVK -> True)
  where VkCreateIOSSurfaceMVK = _VkCreateIOSSurfaceMVK

{-# INLINE _VkCreateIOSSurfaceMVK #-}

_VkCreateIOSSurfaceMVK :: CString
_VkCreateIOSSurfaceMVK = Ptr "vkCreateIOSSurfaceMVK\NUL"#

{-# INLINE is_VkCreateIOSSurfaceMVK #-}

is_VkCreateIOSSurfaceMVK :: CString -> Bool
is_VkCreateIOSSurfaceMVK
  = (EQ ==) . cmpCStrings _VkCreateIOSSurfaceMVK

type VkCreateIOSSurfaceMVK = "vkCreateIOSSurfaceMVK"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateIOSSurfaceMVK
--   >     ( VkInstance instance
--   >     , const VkIOSSurfaceCreateInfoMVK* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateIOSSurfaceMVK vkCreateIOSSurfaceMVK registry at www.khronos.org>
type HS_vkCreateIOSSurfaceMVK =
     VkInstance -- ^ instance
                ->
       Ptr VkIOSSurfaceCreateInfoMVK -- ^ pCreateInfo
                                     ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateIOSSurfaceMVK = FunPtr HS_vkCreateIOSSurfaceMVK

foreign import ccall unsafe "dynamic"
               unwrapVkCreateIOSSurfaceMVKUnsafe ::
               PFN_vkCreateIOSSurfaceMVK -> HS_vkCreateIOSSurfaceMVK

foreign import ccall safe "dynamic" unwrapVkCreateIOSSurfaceMVKSafe
               :: PFN_vkCreateIOSSurfaceMVK -> HS_vkCreateIOSSurfaceMVK

instance VulkanProc "vkCreateIOSSurfaceMVK" where
        type VkProcType "vkCreateIOSSurfaceMVK" = HS_vkCreateIOSSurfaceMVK
        vkProcSymbol = _VkCreateIOSSurfaceMVK

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkCreateIOSSurfaceMVKUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkCreateIOSSurfaceMVKSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_MVK_IOS_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_MVK_IOS_SURFACE_SPEC_VERSION = 2

type VK_MVK_IOS_SURFACE_SPEC_VERSION = 2

pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME :: CString

pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME <-
        (is_VK_MVK_IOS_SURFACE_EXTENSION_NAME -> True)
  where VK_MVK_IOS_SURFACE_EXTENSION_NAME
          = _VK_MVK_IOS_SURFACE_EXTENSION_NAME

{-# INLINE _VK_MVK_IOS_SURFACE_EXTENSION_NAME #-}

_VK_MVK_IOS_SURFACE_EXTENSION_NAME :: CString
_VK_MVK_IOS_SURFACE_EXTENSION_NAME = Ptr "VK_MVK_ios_surface\NUL"#

{-# INLINE is_VK_MVK_IOS_SURFACE_EXTENSION_NAME #-}

is_VK_MVK_IOS_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_MVK_IOS_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_MVK_IOS_SURFACE_EXTENSION_NAME

type VK_MVK_IOS_SURFACE_EXTENSION_NAME = "VK_MVK_ios_surface"

pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK =
        VkStructureType 1000122000
