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
        module Graphics.Vulkan.Types.Enum.VkBufferCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkBufferCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkBufferUsageFlags,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkExportMemoryAllocateInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.Struct.VkExternalMemoryBufferCreateInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR,
        module Graphics.Vulkan.Types.Struct.VkExternalMemoryImageCreateInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkImageCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkImageCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkImageLayout,
        module Graphics.Vulkan.Types.Enum.VkImageTiling,
        module Graphics.Vulkan.Types.Enum.VkImageType,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkSharingMode,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION,
        VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR,
        pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR,
        pattern VK_QUEUE_FAMILY_EXTERNAL_KHR, VK_QUEUE_FAMILY_EXTERNAL_KHR)
       where
import           GHC.Ptr
                                                                                   (Ptr (..))
import           Graphics.Vulkan.Constants
                                                                                   (VK_QUEUE_FAMILY_EXTERNAL_KHR,
                                                                                   pattern VK_QUEUE_FAMILY_EXTERNAL_KHR)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkBufferCreateFlags
import           Graphics.Vulkan.Types.Enum.VkBufferUsageFlags
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags
import           Graphics.Vulkan.Types.Enum.VkImageLayout
import           Graphics.Vulkan.Types.Enum.VkImageTiling
import           Graphics.Vulkan.Types.Enum.VkImageType
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkResult
                                                                                   (VkResult (..))
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkSharingMode
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Struct.VkBufferCreateInfo
import           Graphics.Vulkan.Types.Struct.VkExportMemoryAllocateInfoKHR
import           Graphics.Vulkan.Types.Struct.VkExtent3D
import           Graphics.Vulkan.Types.Struct.VkExternalMemoryBufferCreateInfoKHR
import           Graphics.Vulkan.Types.Struct.VkExternalMemoryImageCreateInfoKHR
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo

pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME
  = Ptr "VK_KHR_external_memory\NUL"#

{-# INLINE is_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME
  = eqCStrings _VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME

type VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME =
     "VK_KHR_external_memory"

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR =
        VkStructureType 1000072000

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR =
        VkStructureType 1000072001

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR =
        VkStructureType 1000072002

pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR :: VkResult

pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR =
        VkResult (-1000072003)
