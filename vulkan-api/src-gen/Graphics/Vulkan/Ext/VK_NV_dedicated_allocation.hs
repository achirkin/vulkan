{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NV_dedicated_allocation
       (VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkBufferCreateBitmask(..), VkBufferUsageBitmask(..),
        VkBufferCreateFlagBits(), VkBufferCreateFlags(),
        VkBufferUsageFlagBits(), VkBufferUsageFlags(),
        VkBufferViewCreateFlagBits(..), VkBufferCreateInfo,
        VkDedicatedAllocationBufferCreateInfoNV,
        VkDedicatedAllocationImageCreateInfoNV,
        VkDedicatedAllocationMemoryAllocateInfoNV, VkExtent3D,
        VkFormat(..), VkFormatFeatureBitmask(..),
        VkFormatFeatureFlagBits(), VkFormatFeatureFlags(),
        VkImageAspectBitmask(..), VkImageCreateBitmask(..),
        VkImageLayout(..), VkImageTiling(..), VkImageType(..),
        VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(), VkImageCreateInfo,
        VkMemoryAllocateInfo, VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(), VkSharingMode(..),
        VkStructureType(..), -- > #include "vk_platform.h"
                             VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION,
        pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION,
        VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME,
        pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV)
       where
import           GHC.Ptr                                          (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.Buffer
import           Graphics.Vulkan.Types.Enum.Format
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.SharingMode
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Struct.Buffer              (VkBufferCreateInfo)
import           Graphics.Vulkan.Types.Struct.DedicatedAllocation (VkDedicatedAllocationBufferCreateInfoNV,
                                                                   VkDedicatedAllocationImageCreateInfoNV,
                                                                   VkDedicatedAllocationMemoryAllocateInfoNV)
import           Graphics.Vulkan.Types.Struct.Extent              (VkExtent3D)
import           Graphics.Vulkan.Types.Struct.Image               (VkImageCreateInfo)
import           Graphics.Vulkan.Types.Struct.Memory              (VkMemoryAllocateInfo)

pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION = 1

type VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION = 1

pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME :: CString

pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME <-
        (is_VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME -> True)
  where
    VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME
      = _VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME

{-# INLINE _VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME #-}

_VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME :: CString
_VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME
  = Ptr "VK_NV_dedicated_allocation\NUL"#

{-# INLINE is_VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME #-}

is_VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME :: CString -> Bool
is_VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME

type VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME =
     "VK_NV_dedicated_allocation"

pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
        = VkStructureType 1000026000

pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
        = VkStructureType 1000026001

pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
        = VkStructureType 1000026002
