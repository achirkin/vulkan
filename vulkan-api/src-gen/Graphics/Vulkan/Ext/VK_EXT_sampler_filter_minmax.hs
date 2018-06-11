{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_sampler_filter_minmax
       (-- * Vulkan extension: @VK_EXT_sampler_filter_minmax@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jeffbolznv@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @131@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.BorderColor,
        module Graphics.Vulkan.Types.Enum.CompareOp,
        module Graphics.Vulkan.Types.Enum.Filter,
        module Graphics.Vulkan.Types.Struct.PhysicalDevice,
        module Graphics.Vulkan.Types.Enum.PhysicalDeviceType,
        module Graphics.Vulkan.Types.Enum.SampleCountFlags,
        module Graphics.Vulkan.Types.Enum.Sampler,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.Sampler,
        module Graphics.Vulkan.Types.Enum.StructureType,
        -- > #include "vk_platform.h"
        VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION,
        pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION,
        VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME,
        pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT)
       where
import           GHC.Ptr                                       (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.BorderColor
import           Graphics.Vulkan.Types.Enum.CompareOp
import           Graphics.Vulkan.Types.Enum.Filter
import           Graphics.Vulkan.Types.Enum.Format             (VkFormatFeatureBitmask (..),
                                                                VkFormatFeatureFlagBits)
import           Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.Sampler
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Struct.PhysicalDevice
import           Graphics.Vulkan.Types.Struct.Sampler

pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION = 1

type VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION = 1

pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME :: CString

pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME <-
        (is_VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME -> True)
  where VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME
          = _VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME

{-# INLINE _VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME #-}

_VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME :: CString
_VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME
  = Ptr "VK_EXT_sampler_filter_minmax\NUL"#

{-# INLINE is_VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME #-}

is_VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME

type VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME =
     "VK_EXT_sampler_filter_minmax"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT
        = VkStructureType 1000130000

pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT =
        VkStructureType 1000130001

-- | Format can be used with min/max reduction filtering
--
--   bitpos = @16@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT =
        VkFormatFeatureFlagBits 65536
