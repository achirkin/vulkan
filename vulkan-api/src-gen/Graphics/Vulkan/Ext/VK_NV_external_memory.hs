{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NV_external_memory
       (VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkExportMemoryAllocateInfoNV, VkExtent3D,
        VkExternalFenceFeatureBitmask(..),
        VkExternalFenceHandleTypeBitmask(..),
        VkExternalMemoryFeatureBitmask(..),
        VkExternalMemoryFeatureBitmaskNV(..),
        VkExternalMemoryHandleTypeBitmaskNV(..),
        VkExternalMemoryHandleTypeBitmask(..),
        VkExternalSemaphoreFeatureBitmask(..),
        VkExternalSemaphoreHandleTypeBitmask(..),
        VkExternalFenceFeatureFlagBits(),
        VkExternalFenceFeatureFlagBitsKHR(..),
        VkExternalFenceFeatureFlags(), VkExternalFenceHandleTypeFlagBits(),
        VkExternalFenceHandleTypeFlagBitsKHR(..),
        VkExternalFenceHandleTypeFlags(),
        VkExternalMemoryFeatureFlagBits(),
        VkExternalMemoryFeatureFlagBitsKHR(..),
        VkExternalMemoryFeatureFlagBitsNV(),
        VkExternalMemoryFeatureFlags(), VkExternalMemoryFeatureFlagsNV(),
        VkExternalMemoryHandleTypeFlagBits(),
        VkExternalMemoryHandleTypeFlagBitsKHR(..),
        VkExternalMemoryHandleTypeFlagBitsNV(),
        VkExternalMemoryHandleTypeFlags(),
        VkExternalMemoryHandleTypeFlagsNV(),
        VkExternalSemaphoreFeatureFlagBits(),
        VkExternalSemaphoreFeatureFlagBitsKHR(..),
        VkExternalSemaphoreFeatureFlags(),
        VkExternalSemaphoreHandleTypeFlagBits(),
        VkExternalSemaphoreHandleTypeFlagBitsKHR(..),
        VkExternalSemaphoreHandleTypeFlags(),
        VkExternalMemoryImageCreateInfoNV, VkFormat(..),
        VkFormatFeatureBitmask(..), VkFormatFeatureFlagBits(),
        VkFormatFeatureFlags(), VkImageAspectBitmask(..),
        VkImageCreateBitmask(..), VkImageLayout(..), VkImageTiling(..),
        VkImageType(..), VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(), VkImageCreateInfo,
        VkMemoryAllocateInfo, VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(), VkSharingMode(..),
        VkStructureType(..), -- > #include "vk_platform.h"
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
import           Graphics.Vulkan.Types.Struct.Export         (VkExportMemoryAllocateInfoNV)
import           Graphics.Vulkan.Types.Struct.Extent         (VkExtent3D)
import           Graphics.Vulkan.Types.Struct.External       (VkExternalMemoryImageCreateInfoNV)
import           Graphics.Vulkan.Types.Struct.Image          (VkImageCreateInfo)
import           Graphics.Vulkan.Types.Struct.Memory         (VkMemoryAllocateInfo)

pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION = 1

type VK_NV_EXTERNAL_MEMORY_SPEC_VERSION = 1

pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME :: CString

pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME <-
        (is_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME -> True)
  where
    VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME
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
