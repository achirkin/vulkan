{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_external_semaphore_win32
       (-- * Vulkan extension: @VK_KHR_external_semaphore_win32@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @79@
        --
        -- Required extensions: 'VK_KHR_external_semaphore'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_WIN32_KHR@
        --

        -- ** Required extensions: 'VK_KHR_external_semaphore'.
        module Graphics.Vulkan.Types.Struct.VkD3D12FenceSubmitInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkExportSemaphoreWin32HandleInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkExternalSemaphoreHandleTypeFlagsKHR,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkImportSemaphoreWin32HandleInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkPipelineStageFlags,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkSemaphoreCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkSemaphoreGetWin32HandleInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkSemaphoreImportFlagsKHR,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Struct.VkSubmitInfo,
        -- > #include "vk_platform.h"
        --
        -- > #include <windows.h>
        vkImportSemaphoreWin32HandleKHR,
        vkImportSemaphoreWin32HandleKHRSafe, vkGetSemaphoreWin32HandleKHR,
        vkGetSemaphoreWin32HandleKHRSafe, module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Include,
        VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION,
        VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR)
       where
import           GHC.Ptr
                                                                                   (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkExternalSemaphoreHandleTypeFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkPipelineStageFlags
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkSemaphoreImportFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Include
import           Graphics.Vulkan.Types.Struct.VkD3D12FenceSubmitInfoKHR
import           Graphics.Vulkan.Types.Struct.VkExportSemaphoreWin32HandleInfoKHR
import           Graphics.Vulkan.Types.Struct.VkImportSemaphoreWin32HandleInfoKHR
import           Graphics.Vulkan.Types.Struct.VkSemaphoreCreateInfo
import           Graphics.Vulkan.Types.Struct.VkSemaphoreGetWin32HandleInfoKHR
import           Graphics.Vulkan.Types.Struct.VkSubmitInfo

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkImportSemaphoreWin32HandleKHR
--   >     ( VkDevice device
--   >     , const VkImportSemaphoreWin32HandleInfoKHR* pImportSemaphoreWin32HandleInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkImportSemaphoreWin32HandleKHR.html vkImportSemaphoreWin32HandleKHR registry at www.khronos.org>
foreign import ccall unsafe "vkImportSemaphoreWin32HandleKHR"
               vkImportSemaphoreWin32HandleKHR ::
               VkDevice -- ^ device
                        -> Ptr VkImportSemaphoreWin32HandleInfoKHR -- ^ pImportSemaphoreWin32HandleInfo
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkImportSemaphoreWin32HandleKHR
--   >     ( VkDevice device
--   >     , const VkImportSemaphoreWin32HandleInfoKHR* pImportSemaphoreWin32HandleInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkImportSemaphoreWin32HandleKHR.html vkImportSemaphoreWin32HandleKHR registry at www.khronos.org>
foreign import ccall safe "vkImportSemaphoreWin32HandleKHR"
               vkImportSemaphoreWin32HandleKHRSafe ::
               VkDevice -- ^ device
                        -> Ptr VkImportSemaphoreWin32HandleInfoKHR -- ^ pImportSemaphoreWin32HandleInfo
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetSemaphoreWin32HandleKHR
--   >     ( VkDevice device
--   >     , const VkSemaphoreGetWin32HandleInfoKHR* pGetWin32HandleInfo
--   >     , HANDLE* pHandle
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetSemaphoreWin32HandleKHR.html vkGetSemaphoreWin32HandleKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetSemaphoreWin32HandleKHR"
               vkGetSemaphoreWin32HandleKHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSemaphoreGetWin32HandleInfoKHR -- ^ pGetWin32HandleInfo
                                                      -> Ptr HANDLE -- ^ pHandle
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetSemaphoreWin32HandleKHR
--   >     ( VkDevice device
--   >     , const VkSemaphoreGetWin32HandleInfoKHR* pGetWin32HandleInfo
--   >     , HANDLE* pHandle
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetSemaphoreWin32HandleKHR.html vkGetSemaphoreWin32HandleKHR registry at www.khronos.org>
foreign import ccall safe "vkGetSemaphoreWin32HandleKHR"
               vkGetSemaphoreWin32HandleKHRSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSemaphoreGetWin32HandleInfoKHR -- ^ pGetWin32HandleInfo
                                                      -> Ptr HANDLE -- ^ pHandle
                                                                    -> IO VkResult

pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
  = Ptr "VK_KHR_external_semaphore_win32\NUL"#

{-# INLINE is_VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME ::
                                                  CString -> Bool
is_VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME

type VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME =
     "VK_KHR_external_semaphore_win32"

pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR =
        VkStructureType 1000078000

pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR =
        VkStructureType 1000078001

pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR =
        VkStructureType 1000078002

pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR =
        VkStructureType 1000078003
