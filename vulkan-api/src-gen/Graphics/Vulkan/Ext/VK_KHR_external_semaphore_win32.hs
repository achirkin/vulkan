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
        -- platform: @win32@
        --
        -- Extension number: @79@
        --
        -- Required extensions: 'VK_KHR_external_semaphore'.
        --

        -- ** Required extensions: 'VK_KHR_external_semaphore'.
        module Graphics.Vulkan.Types.Struct.PlatformWin32Khr,
        module Graphics.Vulkan.Types.Enum.External,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.Pipeline,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.Semaphore,
        module Graphics.Vulkan.Types.Enum.SemaphoreImportFlag,
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Struct.SubmitInfo,
        -- > #include "vk_platform.h"
        VkImportSemaphoreWin32HandleKHR,
        pattern VkImportSemaphoreWin32HandleKHR,
        HS_vkImportSemaphoreWin32HandleKHR,
        PFN_vkImportSemaphoreWin32HandleKHR, VkGetSemaphoreWin32HandleKHR,
        pattern VkGetSemaphoreWin32HandleKHR,
        HS_vkGetSemaphoreWin32HandleKHR, PFN_vkGetSemaphoreWin32HandleKHR,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.Result,
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
import           GHC.Ptr                                        (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                   (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.External
import           Graphics.Vulkan.Types.Enum.Pipeline
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.SemaphoreImportFlag
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Include
import           Graphics.Vulkan.Types.Struct.PlatformWin32Khr
import           Graphics.Vulkan.Types.Struct.Semaphore
import           Graphics.Vulkan.Types.Struct.SubmitInfo

pattern VkImportSemaphoreWin32HandleKHR :: CString

pattern VkImportSemaphoreWin32HandleKHR <-
        (is_VkImportSemaphoreWin32HandleKHR -> True)
  where VkImportSemaphoreWin32HandleKHR
          = _VkImportSemaphoreWin32HandleKHR

{-# INLINE _VkImportSemaphoreWin32HandleKHR #-}

_VkImportSemaphoreWin32HandleKHR :: CString
_VkImportSemaphoreWin32HandleKHR
  = Ptr "vkImportSemaphoreWin32HandleKHR\NUL"#

{-# INLINE is_VkImportSemaphoreWin32HandleKHR #-}

is_VkImportSemaphoreWin32HandleKHR :: CString -> Bool
is_VkImportSemaphoreWin32HandleKHR
  = (EQ ==) . cmpCStrings _VkImportSemaphoreWin32HandleKHR

type VkImportSemaphoreWin32HandleKHR =
     "vkImportSemaphoreWin32HandleKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_INVALID_EXTERNAL_HANDLE'.
--
--   > VkResult vkImportSemaphoreWin32HandleKHR
--   >     ( VkDevice device
--   >     , const VkImportSemaphoreWin32HandleInfoKHR* pImportSemaphoreWin32HandleInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkImportSemaphoreWin32HandleKHR vkImportSemaphoreWin32HandleKHR registry at www.khronos.org>
type HS_vkImportSemaphoreWin32HandleKHR =
     VkDevice -- ^ device
              -> Ptr VkImportSemaphoreWin32HandleInfoKHR -- ^ pImportSemaphoreWin32HandleInfo
                                                         -> IO VkResult

type PFN_vkImportSemaphoreWin32HandleKHR =
     FunPtr HS_vkImportSemaphoreWin32HandleKHR

foreign import ccall unsafe "dynamic"
               unwrapVkImportSemaphoreWin32HandleKHRUnsafe ::
               PFN_vkImportSemaphoreWin32HandleKHR ->
                 HS_vkImportSemaphoreWin32HandleKHR

foreign import ccall safe "dynamic"
               unwrapVkImportSemaphoreWin32HandleKHRSafe ::
               PFN_vkImportSemaphoreWin32HandleKHR ->
                 HS_vkImportSemaphoreWin32HandleKHR

instance VulkanProc "vkImportSemaphoreWin32HandleKHR" where
        type VkProcType "vkImportSemaphoreWin32HandleKHR" =
             HS_vkImportSemaphoreWin32HandleKHR
        vkProcSymbol = _VkImportSemaphoreWin32HandleKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkImportSemaphoreWin32HandleKHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkImportSemaphoreWin32HandleKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetSemaphoreWin32HandleKHR :: CString

pattern VkGetSemaphoreWin32HandleKHR <-
        (is_VkGetSemaphoreWin32HandleKHR -> True)
  where VkGetSemaphoreWin32HandleKHR = _VkGetSemaphoreWin32HandleKHR

{-# INLINE _VkGetSemaphoreWin32HandleKHR #-}

_VkGetSemaphoreWin32HandleKHR :: CString
_VkGetSemaphoreWin32HandleKHR
  = Ptr "vkGetSemaphoreWin32HandleKHR\NUL"#

{-# INLINE is_VkGetSemaphoreWin32HandleKHR #-}

is_VkGetSemaphoreWin32HandleKHR :: CString -> Bool
is_VkGetSemaphoreWin32HandleKHR
  = (EQ ==) . cmpCStrings _VkGetSemaphoreWin32HandleKHR

type VkGetSemaphoreWin32HandleKHR = "vkGetSemaphoreWin32HandleKHR"

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetSemaphoreWin32HandleKHR vkGetSemaphoreWin32HandleKHR registry at www.khronos.org>
type HS_vkGetSemaphoreWin32HandleKHR =
     VkDevice -- ^ device
              ->
       Ptr VkSemaphoreGetWin32HandleInfoKHR -- ^ pGetWin32HandleInfo
                                            -> Ptr HANDLE -- ^ pHandle
                                                          -> IO VkResult

type PFN_vkGetSemaphoreWin32HandleKHR =
     FunPtr HS_vkGetSemaphoreWin32HandleKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetSemaphoreWin32HandleKHRUnsafe ::
               PFN_vkGetSemaphoreWin32HandleKHR -> HS_vkGetSemaphoreWin32HandleKHR

foreign import ccall safe "dynamic"
               unwrapVkGetSemaphoreWin32HandleKHRSafe ::
               PFN_vkGetSemaphoreWin32HandleKHR -> HS_vkGetSemaphoreWin32HandleKHR

instance VulkanProc "vkGetSemaphoreWin32HandleKHR" where
        type VkProcType "vkGetSemaphoreWin32HandleKHR" =
             HS_vkGetSemaphoreWin32HandleKHR
        vkProcSymbol = _VkGetSemaphoreWin32HandleKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkGetSemaphoreWin32HandleKHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkGetSemaphoreWin32HandleKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
