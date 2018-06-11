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
module Graphics.Vulkan.Ext.VK_KHR_external_fence_fd
       (-- * Vulkan extension: @VK_KHR_external_fence_fd@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @critsec@
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
        module Graphics.Vulkan.Types.Enum.External,
        module Graphics.Vulkan.Types.Struct.Fence,
        module Graphics.Vulkan.Types.Enum.Fence,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.Import,
        module Graphics.Vulkan.Types.Enum.StructureType,
        -- > #include "vk_platform.h"
        VkImportFenceFdKHR, pattern VkImportFenceFdKHR,
        HS_vkImportFenceFdKHR, PFN_vkImportFenceFdKHR, VkGetFenceFdKHR,
        pattern VkGetFenceFdKHR, HS_vkGetFenceFdKHR, PFN_vkGetFenceFdKHR,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION,
        VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR)
       where
import           GHC.Ptr                                  (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc             (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.External
import           Graphics.Vulkan.Types.Enum.Fence
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Fence
import           Graphics.Vulkan.Types.Struct.Import

pattern VkImportFenceFdKHR :: CString

pattern VkImportFenceFdKHR <- (is_VkImportFenceFdKHR -> True)
  where VkImportFenceFdKHR = _VkImportFenceFdKHR

{-# INLINE _VkImportFenceFdKHR #-}

_VkImportFenceFdKHR :: CString
_VkImportFenceFdKHR = Ptr "vkImportFenceFdKHR\NUL"#

{-# INLINE is_VkImportFenceFdKHR #-}

is_VkImportFenceFdKHR :: CString -> Bool
is_VkImportFenceFdKHR = (EQ ==) . cmpCStrings _VkImportFenceFdKHR

type VkImportFenceFdKHR = "vkImportFenceFdKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_INVALID_EXTERNAL_HANDLE'.
--
--   > VkResult vkImportFenceFdKHR
--   >     ( VkDevice device
--   >     , const VkImportFenceFdInfoKHR* pImportFenceFdInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkImportFenceFdKHR vkImportFenceFdKHR registry at www.khronos.org>
type HS_vkImportFenceFdKHR =
     VkDevice -- ^ device
              -> Ptr VkImportFenceFdInfoKHR -- ^ pImportFenceFdInfo
                                            -> IO VkResult

type PFN_vkImportFenceFdKHR = FunPtr HS_vkImportFenceFdKHR

foreign import ccall unsafe "dynamic"
               unwrapVkImportFenceFdKHRUnsafe ::
               PFN_vkImportFenceFdKHR -> HS_vkImportFenceFdKHR

foreign import ccall safe "dynamic" unwrapVkImportFenceFdKHRSafe ::
               PFN_vkImportFenceFdKHR -> HS_vkImportFenceFdKHR

instance VulkanProc "vkImportFenceFdKHR" where
        type VkProcType "vkImportFenceFdKHR" = HS_vkImportFenceFdKHR
        vkProcSymbol = _VkImportFenceFdKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkImportFenceFdKHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkImportFenceFdKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetFenceFdKHR :: CString

pattern VkGetFenceFdKHR <- (is_VkGetFenceFdKHR -> True)
  where VkGetFenceFdKHR = _VkGetFenceFdKHR

{-# INLINE _VkGetFenceFdKHR #-}

_VkGetFenceFdKHR :: CString
_VkGetFenceFdKHR = Ptr "vkGetFenceFdKHR\NUL"#

{-# INLINE is_VkGetFenceFdKHR #-}

is_VkGetFenceFdKHR :: CString -> Bool
is_VkGetFenceFdKHR = (EQ ==) . cmpCStrings _VkGetFenceFdKHR

type VkGetFenceFdKHR = "vkGetFenceFdKHR"

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetFenceFdKHR vkGetFenceFdKHR registry at www.khronos.org>
type HS_vkGetFenceFdKHR =
     VkDevice -- ^ device
              -> Ptr VkFenceGetFdInfoKHR -- ^ pGetFdInfo
                                         -> Ptr CInt -- ^ pFd
                                                     -> IO VkResult

type PFN_vkGetFenceFdKHR = FunPtr HS_vkGetFenceFdKHR

foreign import ccall unsafe "dynamic" unwrapVkGetFenceFdKHRUnsafe
               :: PFN_vkGetFenceFdKHR -> HS_vkGetFenceFdKHR

foreign import ccall safe "dynamic" unwrapVkGetFenceFdKHRSafe ::
               PFN_vkGetFenceFdKHR -> HS_vkGetFenceFdKHR

instance VulkanProc "vkGetFenceFdKHR" where
        type VkProcType "vkGetFenceFdKHR" = HS_vkGetFenceFdKHR
        vkProcSymbol = _VkGetFenceFdKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkGetFenceFdKHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkGetFenceFdKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
  = (EQ ==) . cmpCStrings _VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME

type VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME =
     "VK_KHR_external_fence_fd"

pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR =
        VkStructureType 1000115000

pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR :: VkStructureType

pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR =
        VkStructureType 1000115001
