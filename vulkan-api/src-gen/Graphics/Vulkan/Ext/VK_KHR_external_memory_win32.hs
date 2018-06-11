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
module Graphics.Vulkan.Ext.VK_KHR_external_memory_win32
       (-- * Vulkan extension: @VK_KHR_external_memory_win32@
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
        -- platform: @win32@
        --
        -- Extension number: @74@
        --
        -- Required extensions: 'VK_KHR_external_memory'.
        --

        -- ** Required extensions: 'VK_KHR_external_memory'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.PlatformWin32Khr,
        module Graphics.Vulkan.Types.Enum.External,
        module Graphics.Vulkan.Types.Struct.Memory,
        module Graphics.Vulkan.Types.Enum.StructureType,
        -- > #include "vk_platform.h"
        VkGetMemoryWin32HandleKHR, pattern VkGetMemoryWin32HandleKHR,
        HS_vkGetMemoryWin32HandleKHR, PFN_vkGetMemoryWin32HandleKHR,
        VkGetMemoryWin32HandlePropertiesKHR,
        pattern VkGetMemoryWin32HandlePropertiesKHR,
        HS_vkGetMemoryWin32HandlePropertiesKHR,
        PFN_vkGetMemoryWin32HandlePropertiesKHR,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION,
        VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR)
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

pattern VkGetMemoryWin32HandleKHR :: CString

pattern VkGetMemoryWin32HandleKHR <-
        (is_VkGetMemoryWin32HandleKHR -> True)
  where VkGetMemoryWin32HandleKHR = _VkGetMemoryWin32HandleKHR

{-# INLINE _VkGetMemoryWin32HandleKHR #-}

_VkGetMemoryWin32HandleKHR :: CString
_VkGetMemoryWin32HandleKHR = Ptr "vkGetMemoryWin32HandleKHR\NUL"#

{-# INLINE is_VkGetMemoryWin32HandleKHR #-}

is_VkGetMemoryWin32HandleKHR :: CString -> Bool
is_VkGetMemoryWin32HandleKHR
  = (EQ ==) . cmpCStrings _VkGetMemoryWin32HandleKHR

type VkGetMemoryWin32HandleKHR = "vkGetMemoryWin32HandleKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetMemoryWin32HandleKHR
--   >     ( VkDevice device
--   >     , const VkMemoryGetWin32HandleInfoKHR* pGetWin32HandleInfo
--   >     , HANDLE* pHandle
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetMemoryWin32HandleKHR vkGetMemoryWin32HandleKHR registry at www.khronos.org>
type HS_vkGetMemoryWin32HandleKHR =
     VkDevice -- ^ device
              ->
       Ptr VkMemoryGetWin32HandleInfoKHR -- ^ pGetWin32HandleInfo
                                         -> Ptr HANDLE -- ^ pHandle
                                                       -> IO VkResult

type PFN_vkGetMemoryWin32HandleKHR =
     FunPtr HS_vkGetMemoryWin32HandleKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetMemoryWin32HandleKHRUnsafe ::
               PFN_vkGetMemoryWin32HandleKHR -> HS_vkGetMemoryWin32HandleKHR

foreign import ccall safe "dynamic"
               unwrapVkGetMemoryWin32HandleKHRSafe ::
               PFN_vkGetMemoryWin32HandleKHR -> HS_vkGetMemoryWin32HandleKHR

instance VulkanProc "vkGetMemoryWin32HandleKHR" where
        type VkProcType "vkGetMemoryWin32HandleKHR" =
             HS_vkGetMemoryWin32HandleKHR
        vkProcSymbol = _VkGetMemoryWin32HandleKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkGetMemoryWin32HandleKHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkGetMemoryWin32HandleKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetMemoryWin32HandlePropertiesKHR :: CString

pattern VkGetMemoryWin32HandlePropertiesKHR <-
        (is_VkGetMemoryWin32HandlePropertiesKHR -> True)
  where VkGetMemoryWin32HandlePropertiesKHR
          = _VkGetMemoryWin32HandlePropertiesKHR

{-# INLINE _VkGetMemoryWin32HandlePropertiesKHR #-}

_VkGetMemoryWin32HandlePropertiesKHR :: CString
_VkGetMemoryWin32HandlePropertiesKHR
  = Ptr "vkGetMemoryWin32HandlePropertiesKHR\NUL"#

{-# INLINE is_VkGetMemoryWin32HandlePropertiesKHR #-}

is_VkGetMemoryWin32HandlePropertiesKHR :: CString -> Bool
is_VkGetMemoryWin32HandlePropertiesKHR
  = (EQ ==) . cmpCStrings _VkGetMemoryWin32HandlePropertiesKHR

type VkGetMemoryWin32HandlePropertiesKHR =
     "vkGetMemoryWin32HandlePropertiesKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_INVALID_EXTERNAL_HANDLE'.
--
--   > VkResult vkGetMemoryWin32HandlePropertiesKHR
--   >     ( VkDevice device
--   >     , VkExternalMemoryHandleTypeFlagBits handleType
--   >     , HANDLE handle
--   >     , VkMemoryWin32HandlePropertiesKHR* pMemoryWin32HandleProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetMemoryWin32HandlePropertiesKHR vkGetMemoryWin32HandlePropertiesKHR registry at www.khronos.org>
type HS_vkGetMemoryWin32HandlePropertiesKHR =
     VkDevice -- ^ device
              ->
       VkExternalMemoryHandleTypeFlagBits -- ^ handleType
                                          ->
         HANDLE -- ^ handle
                -> Ptr VkMemoryWin32HandlePropertiesKHR -- ^ pMemoryWin32HandleProperties
                                                        -> IO VkResult

type PFN_vkGetMemoryWin32HandlePropertiesKHR =
     FunPtr HS_vkGetMemoryWin32HandlePropertiesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetMemoryWin32HandlePropertiesKHRUnsafe ::
               PFN_vkGetMemoryWin32HandlePropertiesKHR ->
                 HS_vkGetMemoryWin32HandlePropertiesKHR

foreign import ccall safe "dynamic"
               unwrapVkGetMemoryWin32HandlePropertiesKHRSafe ::
               PFN_vkGetMemoryWin32HandlePropertiesKHR ->
                 HS_vkGetMemoryWin32HandlePropertiesKHR

instance VulkanProc "vkGetMemoryWin32HandlePropertiesKHR" where
        type VkProcType "vkGetMemoryWin32HandlePropertiesKHR" =
             HS_vkGetMemoryWin32HandlePropertiesKHR
        vkProcSymbol = _VkGetMemoryWin32HandlePropertiesKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkGetMemoryWin32HandlePropertiesKHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkGetMemoryWin32HandlePropertiesKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  = Ptr "VK_KHR_external_memory_win32\NUL"#

{-# INLINE is_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME

type VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME =
     "VK_KHR_external_memory_win32"

pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR =
        VkStructureType 1000073000

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR =
        VkStructureType 1000073001

pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR =
        VkStructureType 1000073002

pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR =
        VkStructureType 1000073003
