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
module Graphics.Vulkan.Ext.VK_NN_vi_surface
       (-- * Vulkan extension: @VK_NN_vi_surface@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Mathias Heyer gitlab:@mheyer@
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
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.PlatformViNn,
        -- > #include "vk_platform.h"
        VkCreateViSurfaceNN, pattern VkCreateViSurfaceNN,
        HS_vkCreateViSurfaceNN, PFN_vkCreateViSurfaceNN,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.InternalAllocationType,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Enum.SystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.AllocationCallbacks,
        VK_NN_VI_SURFACE_SPEC_VERSION,
        pattern VK_NN_VI_SURFACE_SPEC_VERSION,
        VK_NN_VI_SURFACE_EXTENSION_NAME,
        pattern VK_NN_VI_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN)
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
import           Graphics.Vulkan.Types.Struct.PlatformViNn

pattern VkCreateViSurfaceNN :: CString

pattern VkCreateViSurfaceNN <- (is_VkCreateViSurfaceNN -> True)
  where VkCreateViSurfaceNN = _VkCreateViSurfaceNN

{-# INLINE _VkCreateViSurfaceNN #-}

_VkCreateViSurfaceNN :: CString
_VkCreateViSurfaceNN = Ptr "vkCreateViSurfaceNN\NUL"#

{-# INLINE is_VkCreateViSurfaceNN #-}

is_VkCreateViSurfaceNN :: CString -> Bool
is_VkCreateViSurfaceNN = (EQ ==) . cmpCStrings _VkCreateViSurfaceNN

type VkCreateViSurfaceNN = "vkCreateViSurfaceNN"

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateViSurfaceNN vkCreateViSurfaceNN registry at www.khronos.org>
type HS_vkCreateViSurfaceNN =
     VkInstance -- ^ instance
                ->
       Ptr VkViSurfaceCreateInfoNN -- ^ pCreateInfo
                                   ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateViSurfaceNN = FunPtr HS_vkCreateViSurfaceNN

foreign import ccall unsafe "dynamic"
               unwrapVkCreateViSurfaceNNUnsafe ::
               PFN_vkCreateViSurfaceNN -> HS_vkCreateViSurfaceNN

foreign import ccall safe "dynamic" unwrapVkCreateViSurfaceNNSafe
               :: PFN_vkCreateViSurfaceNN -> HS_vkCreateViSurfaceNN

instance VulkanProc "vkCreateViSurfaceNN" where
        type VkProcType "vkCreateViSurfaceNN" = HS_vkCreateViSurfaceNN
        vkProcSymbol = _VkCreateViSurfaceNN

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkCreateViSurfaceNNUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkCreateViSurfaceNNSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
