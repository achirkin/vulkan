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
        module Graphics.Vulkan.Types.Enum.External,
        module Graphics.Vulkan.Types.Struct.Import,
        module Graphics.Vulkan.Types.Struct.Memory,
        module Graphics.Vulkan.Types.Enum.StructureType, -- > #include "vk_platform.h"
                                                         VkGetMemoryFdKHR,
        pattern VkGetMemoryFdKHR, HS_vkGetMemoryFdKHR,
        PFN_vkGetMemoryFdKHR, VkGetMemoryFdPropertiesKHR,
        pattern VkGetMemoryFdPropertiesKHR, HS_vkGetMemoryFdPropertiesKHR,
        PFN_vkGetMemoryFdPropertiesKHR,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION,
        VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR)
       where
import           GHC.Ptr                                  (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc             (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.External
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Import
import           Graphics.Vulkan.Types.Struct.Memory

pattern VkGetMemoryFdKHR :: CString

pattern VkGetMemoryFdKHR <- (is_VkGetMemoryFdKHR -> True)
  where VkGetMemoryFdKHR = _VkGetMemoryFdKHR

{-# INLINE _VkGetMemoryFdKHR #-}

_VkGetMemoryFdKHR :: CString
_VkGetMemoryFdKHR = Ptr "vkGetMemoryFdKHR\NUL"#

{-# INLINE is_VkGetMemoryFdKHR #-}

is_VkGetMemoryFdKHR :: CString -> Bool
is_VkGetMemoryFdKHR = (EQ ==) . cmpCStrings _VkGetMemoryFdKHR

type VkGetMemoryFdKHR = "vkGetMemoryFdKHR"

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetMemoryFdKHR vkGetMemoryFdKHR registry at www.khronos.org>
type HS_vkGetMemoryFdKHR =
     VkDevice -- ^ device
              -> Ptr VkMemoryGetFdInfoKHR -- ^ pGetFdInfo
                                          -> Ptr CInt -- ^ pFd
                                                      -> IO VkResult

type PFN_vkGetMemoryFdKHR = FunPtr HS_vkGetMemoryFdKHR

foreign import ccall unsafe "dynamic" unwrapVkGetMemoryFdKHRUnsafe
               :: PFN_vkGetMemoryFdKHR -> HS_vkGetMemoryFdKHR

foreign import ccall safe "dynamic" unwrapVkGetMemoryFdKHRSafe ::
               PFN_vkGetMemoryFdKHR -> HS_vkGetMemoryFdKHR

instance VulkanProc "vkGetMemoryFdKHR" where
        type VkProcType "vkGetMemoryFdKHR" = HS_vkGetMemoryFdKHR
        vkProcSymbol = _VkGetMemoryFdKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkGetMemoryFdKHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkGetMemoryFdKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetMemoryFdPropertiesKHR :: CString

pattern VkGetMemoryFdPropertiesKHR <-
        (is_VkGetMemoryFdPropertiesKHR -> True)
  where VkGetMemoryFdPropertiesKHR = _VkGetMemoryFdPropertiesKHR

{-# INLINE _VkGetMemoryFdPropertiesKHR #-}

_VkGetMemoryFdPropertiesKHR :: CString
_VkGetMemoryFdPropertiesKHR = Ptr "vkGetMemoryFdPropertiesKHR\NUL"#

{-# INLINE is_VkGetMemoryFdPropertiesKHR #-}

is_VkGetMemoryFdPropertiesKHR :: CString -> Bool
is_VkGetMemoryFdPropertiesKHR
  = (EQ ==) . cmpCStrings _VkGetMemoryFdPropertiesKHR

type VkGetMemoryFdPropertiesKHR = "vkGetMemoryFdPropertiesKHR"

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetMemoryFdPropertiesKHR vkGetMemoryFdPropertiesKHR registry at www.khronos.org>
type HS_vkGetMemoryFdPropertiesKHR =
     VkDevice -- ^ device
              ->
       VkExternalMemoryHandleTypeFlagBits -- ^ handleType
                                          ->
         CInt -- ^ fd
              -> Ptr VkMemoryFdPropertiesKHR -- ^ pMemoryFdProperties
                                             -> IO VkResult

type PFN_vkGetMemoryFdPropertiesKHR =
     FunPtr HS_vkGetMemoryFdPropertiesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetMemoryFdPropertiesKHRUnsafe ::
               PFN_vkGetMemoryFdPropertiesKHR -> HS_vkGetMemoryFdPropertiesKHR

foreign import ccall safe "dynamic"
               unwrapVkGetMemoryFdPropertiesKHRSafe ::
               PFN_vkGetMemoryFdPropertiesKHR -> HS_vkGetMemoryFdPropertiesKHR

instance VulkanProc "vkGetMemoryFdPropertiesKHR" where
        type VkProcType "vkGetMemoryFdPropertiesKHR" =
             HS_vkGetMemoryFdPropertiesKHR
        vkProcSymbol = _VkGetMemoryFdPropertiesKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkGetMemoryFdPropertiesKHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkGetMemoryFdPropertiesKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
