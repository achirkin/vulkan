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
        module Graphics.Vulkan.Types.Struct.Export,
        module Graphics.Vulkan.Types.Struct.Extent,
        module Graphics.Vulkan.Types.Enum.External,
        module Graphics.Vulkan.Types.Struct.External,
        module Graphics.Vulkan.Types.Enum.Format,
        module Graphics.Vulkan.Types.Enum.Image,
        module Graphics.Vulkan.Types.Struct.Image,
        module Graphics.Vulkan.Types.Struct.Memory,
        module Graphics.Vulkan.Types.Enum.SampleCountFlags,
        module Graphics.Vulkan.Types.Enum.SharingMode,
        module Graphics.Vulkan.Types.Enum.StructureType,
        -- > #include "vk_platform.h"
        VK_NV_EXTERNAL_MEMORY_SPEC_VERSION,
        pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION,
        VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME,
        pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV)
       where
import           GHC.Ptr                                     (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.External
import           Graphics.Vulkan.Types.Enum.Format
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.SharingMode
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Struct.Export
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.External
import           Graphics.Vulkan.Types.Struct.Image
import           Graphics.Vulkan.Types.Struct.Memory

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
  = (EQ ==) . cmpCStrings _VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME

type VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_NV_external_memory"

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV =
        VkStructureType 1000056000

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV =
        VkStructureType 1000056001
