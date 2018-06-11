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
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_NV_external_memory_win32
       (-- * Vulkan extension: @VK_NV_external_memory_win32@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- platform: @win32@
        --
        -- Extension number: @58@
        --
        -- Required extensions: 'VK_NV_external_memory'.
        --

        -- ** Required extensions: 'VK_NV_external_memory'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.PlatformWin32Khr,
        module Graphics.Vulkan.Types.Enum.External,
        module Graphics.Vulkan.Types.Struct.Memory,
        module Graphics.Vulkan.Types.Enum.StructureType,
        -- > #include "vk_platform.h"
        VkGetMemoryWin32HandleNV, pattern VkGetMemoryWin32HandleNV,
        HS_vkGetMemoryWin32HandleNV, PFN_vkGetMemoryWin32HandleNV,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Handles,
        VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION,
        pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION,
        VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME,
        pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV)
       where
import           GHC.Ptr                                       (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                  (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.External
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Include                 (HANDLE)
import           Graphics.Vulkan.Types.Struct.Memory
import           Graphics.Vulkan.Types.Struct.PlatformWin32Khr

pattern VkGetMemoryWin32HandleNV :: CString

pattern VkGetMemoryWin32HandleNV <-
        (is_VkGetMemoryWin32HandleNV -> True)
  where VkGetMemoryWin32HandleNV = _VkGetMemoryWin32HandleNV

{-# INLINE _VkGetMemoryWin32HandleNV #-}

_VkGetMemoryWin32HandleNV :: CString
_VkGetMemoryWin32HandleNV = Ptr "vkGetMemoryWin32HandleNV\NUL"#

{-# INLINE is_VkGetMemoryWin32HandleNV #-}

is_VkGetMemoryWin32HandleNV :: CString -> Bool
is_VkGetMemoryWin32HandleNV
  = (EQ ==) . cmpCStrings _VkGetMemoryWin32HandleNV

type VkGetMemoryWin32HandleNV = "vkGetMemoryWin32HandleNV"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetMemoryWin32HandleNV
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , VkExternalMemoryHandleTypeFlagsNV handleType
--   >     , HANDLE* pHandle
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetMemoryWin32HandleNV vkGetMemoryWin32HandleNV registry at www.khronos.org>
type HS_vkGetMemoryWin32HandleNV =
     VkDevice -- ^ device
              ->
       VkDeviceMemory -- ^ memory
                      ->
         VkExternalMemoryHandleTypeFlagsNV -- ^ handleType
                                           -> Ptr HANDLE -- ^ pHandle
                                                         -> IO VkResult

type PFN_vkGetMemoryWin32HandleNV =
     FunPtr HS_vkGetMemoryWin32HandleNV

foreign import ccall unsafe "dynamic"
               unwrapVkGetMemoryWin32HandleNVUnsafe ::
               PFN_vkGetMemoryWin32HandleNV -> HS_vkGetMemoryWin32HandleNV

foreign import ccall safe "dynamic"
               unwrapVkGetMemoryWin32HandleNVSafe ::
               PFN_vkGetMemoryWin32HandleNV -> HS_vkGetMemoryWin32HandleNV

instance VulkanProc "vkGetMemoryWin32HandleNV" where
        type VkProcType "vkGetMemoryWin32HandleNV" =
             HS_vkGetMemoryWin32HandleNV
        vkProcSymbol = _VkGetMemoryWin32HandleNV

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkGetMemoryWin32HandleNVUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkGetMemoryWin32HandleNVSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1

type VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1

pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: CString

pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME <-
        (is_VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME -> True)
  where VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
          = _VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME

{-# INLINE _VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME #-}

_VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: CString
_VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  = Ptr "VK_NV_external_memory_win32\NUL"#

{-# INLINE is_VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME #-}

is_VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: CString -> Bool
is_VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME

type VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME =
     "VK_NV_external_memory_win32"

pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV =
        VkStructureType 1000057000

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV =
        VkStructureType 1000057001
