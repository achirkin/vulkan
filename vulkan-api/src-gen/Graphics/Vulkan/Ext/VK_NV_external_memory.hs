{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NV_external_memory
       (-- * Vulkan extension: @VK_NV_external_memory@
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
        -- Extension number: @57@
        --
        -- Required extensions: 'VK_NV_external_memory_capabilities'.
        --

        -- ** Required extensions: 'VK_NV_external_memory_capabilities'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkExportMemoryAllocateInfoNV,
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsNV,
        module Graphics.Vulkan.Types.Struct.VkExternalMemoryImageCreateInfoNV,
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
        VK_NV_EXTERNAL_MEMORY_SPEC_VERSION,
        pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION,
        VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME,
        pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV)
       where
import           GHC.Ptr                                                        (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsNV
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags
import           Graphics.Vulkan.Types.Enum.VkImageLayout
import           Graphics.Vulkan.Types.Enum.VkImageTiling
import           Graphics.Vulkan.Types.Enum.VkImageType
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkSharingMode
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Struct.VkExportMemoryAllocateInfoNV
import           Graphics.Vulkan.Types.Struct.VkExtent3D
import           Graphics.Vulkan.Types.Struct.VkExternalMemoryImageCreateInfoNV
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo

pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION = 1

type VK_NV_EXTERNAL_MEMORY_SPEC_VERSION = 1

pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME :: CString

pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME <-
        (is_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME -> True)
  where VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME
          = _VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME

{-# INLINE _VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME #-}

_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME :: CString
_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME
  = Ptr "VK_NV_external_memory\NUL"#

{-# INLINE is_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME #-}

is_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME :: CString -> Bool
is_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME
  = eqCStrings _VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME

type VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_NV_external_memory"

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV =
        VkStructureType 1000056000

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV =
        VkStructureType 1000056001
