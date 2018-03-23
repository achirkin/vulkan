{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_external_memory_fd
       (-- * Vulkan extension: @VK_KHR_external_memory_fd@
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
        -- Extension number: @75@
        --
        -- Required extensions: 'VK_KHR_external_memory'.
        --

        -- ** Required extensions: 'VK_KHR_external_memory'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlags,
        module Graphics.Vulkan.Types.Struct.VkImportMemoryFdInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo,
        module Graphics.Vulkan.Types.Struct.VkMemoryFdPropertiesKHR,
        module Graphics.Vulkan.Types.Struct.VkMemoryGetFdInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        vkGetMemoryFdKHR, vkGetMemoryFdKHRSafe, vkGetMemoryFdPropertiesKHR,
        vkGetMemoryFdPropertiesKHRSafe,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION,
        VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR)
       where
import           GHC.Ptr                                                    (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlags
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkImportMemoryFdInfoKHR
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo
import           Graphics.Vulkan.Types.Struct.VkMemoryFdPropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkMemoryGetFdInfoKHR

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetMemoryFdKHR
--   >     ( VkDevice device
--   >     , const VkMemoryGetFdInfoKHR* pGetFdInfo
--   >     , int* pFd
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetMemoryFdKHR.html vkGetMemoryFdKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetMemoryFdKHR" vkGetMemoryFdKHR ::
               VkDevice -- ^ device
                        -> Ptr VkMemoryGetFdInfoKHR -- ^ pGetFdInfo
                                                    -> Ptr CInt -- ^ pFd
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetMemoryFdKHR
--   >     ( VkDevice device
--   >     , const VkMemoryGetFdInfoKHR* pGetFdInfo
--   >     , int* pFd
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetMemoryFdKHR.html vkGetMemoryFdKHR registry at www.khronos.org>
foreign import ccall safe "vkGetMemoryFdKHR" vkGetMemoryFdKHRSafe
               :: VkDevice -- ^ device
                           -> Ptr VkMemoryGetFdInfoKHR -- ^ pGetFdInfo
                                                       -> Ptr CInt -- ^ pFd
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_INVALID_EXTERNAL_HANDLE'.
--
--   > VkResult vkGetMemoryFdPropertiesKHR
--   >     ( VkDevice device
--   >     , VkExternalMemoryHandleTypeFlagBits handleType
--   >     , int fd
--   >     , VkMemoryFdPropertiesKHR* pMemoryFdProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetMemoryFdPropertiesKHR.html vkGetMemoryFdPropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetMemoryFdPropertiesKHR"
               vkGetMemoryFdPropertiesKHR ::
               VkDevice -- ^ device
                        ->
                 VkExternalMemoryHandleTypeFlagBits -- ^ handleType
                                                    ->
                   CInt -- ^ fd
                        -> Ptr VkMemoryFdPropertiesKHR -- ^ pMemoryFdProperties
                                                       -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_INVALID_EXTERNAL_HANDLE'.
--
--   > VkResult vkGetMemoryFdPropertiesKHR
--   >     ( VkDevice device
--   >     , VkExternalMemoryHandleTypeFlagBits handleType
--   >     , int fd
--   >     , VkMemoryFdPropertiesKHR* pMemoryFdProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetMemoryFdPropertiesKHR.html vkGetMemoryFdPropertiesKHR registry at www.khronos.org>
foreign import ccall safe "vkGetMemoryFdPropertiesKHR"
               vkGetMemoryFdPropertiesKHRSafe ::
               VkDevice -- ^ device
                        ->
                 VkExternalMemoryHandleTypeFlagBits -- ^ handleType
                                                    ->
                   CInt -- ^ fd
                        -> Ptr VkMemoryFdPropertiesKHR -- ^ pMemoryFdProperties
                                                       -> IO VkResult

pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
  = Ptr "VK_KHR_external_memory_fd\NUL"#

{-# INLINE is_VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME

type VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME =
     "VK_KHR_external_memory_fd"

pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR =
        VkStructureType 1000074000

pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR =
        VkStructureType 1000074001

pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR :: VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR =
        VkStructureType 1000074002
