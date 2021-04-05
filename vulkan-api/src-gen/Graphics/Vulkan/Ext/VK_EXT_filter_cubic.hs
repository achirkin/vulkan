{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_filter_cubic
       (AHardwareBuffer(), ANativeWindow(), CAMetalLayer(), VkBool32(..),
        VkDeviceAddress(..), VkDeviceSize(..), VkFlags(..),
        VkSampleMask(..), VkExtent3D,
        VkFilterCubicImageViewImageFormatPropertiesEXT, VkFormat(..),
        VkFormatFeatureBitmask(..), VkFormatFeatureFlagBits(),
        VkFormatFeatureFlags(), VkImageAspectBitmask(..),
        VkImageCreateBitmask(..), VkImageLayout(..), VkImageTiling(..),
        VkImageType(..), VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(),
        VkImageViewCreateBitmask(..), VkImageViewCreateFlagBits(),
        VkImageViewCreateFlags(), VkImageFormatProperties,
        VkImageFormatProperties2, VkPhysicalDeviceImageFormatInfo2,
        VkPhysicalDeviceImageViewImageFormatInfoEXT,
        VkSampleCountBitmask(..), VkSampleCountFlagBits(),
        VkSampleCountFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_EXT_FILTER_CUBIC_SPEC_VERSION,
        pattern VK_EXT_FILTER_CUBIC_SPEC_VERSION,
        VK_EXT_FILTER_CUBIC_EXTENSION_NAME,
        pattern VK_EXT_FILTER_CUBIC_EXTENSION_NAME,
        pattern VK_FILTER_CUBIC_EXT,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT)
       where
import GHC.Ptr                                                                   (Ptr (..))
import Graphics.Vulkan.Ext.VK_IMG_filter_cubic                                   (pattern VK_FILTER_CUBIC_IMG,
                                                                                  pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Enum.Format
import Graphics.Vulkan.Types.Enum.Image
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Struct.Extent                                       (VkExtent3D)
import Graphics.Vulkan.Types.Struct.FilterCubicImageViewImageFormatPropertiesEXT (VkFilterCubicImageViewImageFormatPropertiesEXT)
import Graphics.Vulkan.Types.Struct.Image                                        (VkImageFormatProperties,
                                                                                  VkImageFormatProperties2)
import Graphics.Vulkan.Types.Struct.PhysicalDevice                               (VkPhysicalDeviceImageFormatInfo2,
                                                                                  VkPhysicalDeviceImageViewImageFormatInfoEXT)

pattern VK_EXT_FILTER_CUBIC_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_FILTER_CUBIC_SPEC_VERSION = 3

type VK_EXT_FILTER_CUBIC_SPEC_VERSION = 3

pattern VK_EXT_FILTER_CUBIC_EXTENSION_NAME :: CString

pattern VK_EXT_FILTER_CUBIC_EXTENSION_NAME <-
        (is_VK_EXT_FILTER_CUBIC_EXTENSION_NAME -> True)
  where
    VK_EXT_FILTER_CUBIC_EXTENSION_NAME
      = _VK_EXT_FILTER_CUBIC_EXTENSION_NAME

{-# INLINE _VK_EXT_FILTER_CUBIC_EXTENSION_NAME #-}

_VK_EXT_FILTER_CUBIC_EXTENSION_NAME :: CString
_VK_EXT_FILTER_CUBIC_EXTENSION_NAME
  = Ptr "VK_EXT_filter_cubic\NUL"#

{-# INLINE is_VK_EXT_FILTER_CUBIC_EXTENSION_NAME #-}

is_VK_EXT_FILTER_CUBIC_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_FILTER_CUBIC_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_FILTER_CUBIC_EXTENSION_NAME

type VK_EXT_FILTER_CUBIC_EXTENSION_NAME = "VK_EXT_filter_cubic"

pattern VK_FILTER_CUBIC_EXT = VK_FILTER_CUBIC_IMG

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT =
        VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT
        = VkStructureType 1000170000

pattern VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT
        = VkStructureType 1000170001
