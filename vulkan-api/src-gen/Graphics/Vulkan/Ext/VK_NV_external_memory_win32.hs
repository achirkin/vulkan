{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
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
        -- Extension number: @58@
        --
        -- Required extensions: 'VK_NV_external_memory'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_WIN32_KHR@
        --

        -- ** Required extensions: 'VK_NV_external_memory'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkExportMemoryWin32HandleInfoNV,
        module Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsNV,
        module Graphics.Vulkan.Types.Struct.VkImportMemoryWin32HandleInfoNV,
        module Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        --
        -- > #include <windows.h>
        vkGetMemoryWin32HandleNV, vkGetMemoryWin32HandleNVSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Handles,
        VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION,
        pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION,
        VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME,
        pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV)
       where
import           GHC.Ptr                                                      (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsNV
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Include                                (HANDLE)
import           Graphics.Vulkan.Types.Struct.VkExportMemoryWin32HandleInfoNV
import           Graphics.Vulkan.Types.Struct.VkImportMemoryWin32HandleInfoNV
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetMemoryWin32HandleNV.html vkGetMemoryWin32HandleNV registry at www.khronos.org>
foreign import ccall unsafe "vkGetMemoryWin32HandleNV"
               vkGetMemoryWin32HandleNV ::
               VkDevice -- ^ device
                        ->
                 VkDeviceMemory -- ^ memory
                                ->
                   VkExternalMemoryHandleTypeFlagsNV -- ^ handleType
                                                     -> Ptr HANDLE -- ^ pHandle
                                                                   -> IO VkResult

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetMemoryWin32HandleNV.html vkGetMemoryWin32HandleNV registry at www.khronos.org>
foreign import ccall safe "vkGetMemoryWin32HandleNV"
               vkGetMemoryWin32HandleNVSafe ::
               VkDevice -- ^ device
                        ->
                 VkDeviceMemory -- ^ memory
                                ->
                   VkExternalMemoryHandleTypeFlagsNV -- ^ handleType
                                                     -> Ptr HANDLE -- ^ pHandle
                                                                   -> IO VkResult

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
