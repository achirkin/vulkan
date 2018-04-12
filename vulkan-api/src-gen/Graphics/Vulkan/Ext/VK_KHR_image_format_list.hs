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
        module Graphics.Vulkan.Types.Struct.Extent,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.Format,
        module Graphics.Vulkan.Types.Enum.Image,
        module Graphics.Vulkan.Types.Struct.Image,
        module Graphics.Vulkan.Types.Enum.SampleCountFlags,
        module Graphics.Vulkan.Types.Enum.SharingMode,
        module Graphics.Vulkan.Types.Enum.StructureType,
        -- > #include "vk_platform.h"
        VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION,
        pattern VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION,
        VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME,
        pattern VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR)
       where
import           GHC.Ptr                                     (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.Format
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.SharingMode
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.Image

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
