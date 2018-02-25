{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_image_format_list
       (-- * Vulkan extension: @VK_KHR_image_format_list@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jason Ekstrand @jekstrand@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @148@
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkImageCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkImageCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkImageFormatListCreateInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkImageLayout,
        module Graphics.Vulkan.Types.Enum.VkImageTiling,
        module Graphics.Vulkan.Types.Enum.VkImageType,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkSharingMode,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION,
        pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION,
        VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME,
        pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR)
       where
import           GHC.Ptr                                                     (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags
import           Graphics.Vulkan.Types.Enum.VkImageLayout
import           Graphics.Vulkan.Types.Enum.VkImageTiling
import           Graphics.Vulkan.Types.Enum.VkImageType
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkSharingMode
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Struct.VkExtent3D
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo
import           Graphics.Vulkan.Types.Struct.VkImageFormatListCreateInfoKHR

pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION = 1

type VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION = 1

pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME :: CString

pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME <-
        (is_VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME -> True)
  where VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
          = _VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME

{-# INLINE _VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME #-}

_VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME :: CString
_VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
  = Ptr "VK_KHR_image_format_list\NUL"#

{-# INLINE is_VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME #-}

is_VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME

type VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME =
     "VK_KHR_image_format_list"

pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR =
        VkStructureType 1000147000
