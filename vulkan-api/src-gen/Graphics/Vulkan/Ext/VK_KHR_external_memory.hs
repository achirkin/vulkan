{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_external_memory
       (-- * Vulkan extension: @VK_KHR_external_memory@
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
        -- Extension number: @73@
        --
        -- Required extensions: 'VK_KHR_external_memory_capabilities'.
        --

        -- ** Required extensions: 'VK_KHR_external_memory_capabilities'.
        VkExportMemoryAllocateInfoKHR,
        VkExternalMemoryBufferCreateInfoKHR,
        VkExternalMemoryImageCreateInfoKHR,
        VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION,
        VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR,
        pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR,
        pattern VK_QUEUE_FAMILY_EXTERNAL_KHR)
       where
import           GHC.Ptr                               (Ptr (..))
import           Graphics.Vulkan.Constants             (pattern VK_QUEUE_FAMILY_EXTERNAL_KHR)
import           Graphics.Vulkan.Core_1_1              (pattern VK_ERROR_INVALID_EXTERNAL_HANDLE,
                                                        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO,
                                                        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO,
                                                        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Struct.Export   (VkExportMemoryAllocateInfoKHR)
import           Graphics.Vulkan.Types.Struct.External (VkExternalMemoryBufferCreateInfoKHR,
                                                        VkExternalMemoryImageCreateInfoKHR)

pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME -> True)
  where
    VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME
      = _VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME
  = Ptr "VK_KHR_external_memory\NUL"#

{-# INLINE is_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME

type VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME =
     "VK_KHR_external_memory"

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR =
        VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR =
        VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR =
        VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO

pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR =
        VK_ERROR_INVALID_EXTERNAL_HANDLE
