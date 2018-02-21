{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_external_fence_fd
       (-- * Vulkan extension: @VK_KHR_external_fence_fd@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @jessehall@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @116@
        --
        -- Required extensions: 'VK_KHR_external_fence'.
        --

        -- ** Required extensions: 'VK_KHR_external_fence'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlagsKHR,
        module Graphics.Vulkan.Types.Struct.VkFenceGetFdInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkFenceImportFlagsKHR,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkImportFenceFdInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        vkImportFenceFdKHR, vkGetFenceFdKHR,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION,
        VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR)
       where
import           GHC.Ptr                                                      (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkFenceImportFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkFenceGetFdInfoKHR
import           Graphics.Vulkan.Types.Struct.VkImportFenceFdInfoKHR

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkImportFenceFdKHR
--   >     ( VkDevice device
--   >     , const VkImportFenceFdInfoKHR* pImportFenceFdInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkImportFenceFdKHR.html vkImportFenceFdKHR registry at www.khronos.org>
foreign import ccall unsafe "vkImportFenceFdKHR" vkImportFenceFdKHR
               :: VkDevice -- ^ device
                           -> Ptr VkImportFenceFdInfoKHR -- ^ pImportFenceFdInfo
                                                         -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetFenceFdKHR
--   >     ( VkDevice device
--   >     , const VkFenceGetFdInfoKHR* pGetFdInfo
--   >     , int* pFd
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetFenceFdKHR.html vkGetFenceFdKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetFenceFdKHR" vkGetFenceFdKHR ::
               VkDevice -- ^ device
                        -> Ptr VkFenceGetFdInfoKHR -- ^ pGetFdInfo
                                                   -> Ptr CInt -- ^ pFd
                                                               -> IO VkResult

pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
  = Ptr "VK_KHR_external_fence_fd\NUL"#

{-# INLINE is_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
  = eqCStrings _VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME

type VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME =
     "VK_KHR_external_fence_fd"

pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR =
        VkStructureType 1000115000

pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR :: VkStructureType

pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR =
        VkStructureType 1000115001
