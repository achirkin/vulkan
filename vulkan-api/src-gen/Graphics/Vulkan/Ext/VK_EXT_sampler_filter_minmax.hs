{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
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
        VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT,
        VkSamplerReductionModeCreateInfoEXT, VkSamplerAddressMode(..),
        VkSamplerMipmapMode(..), VkSamplerReductionMode(..),
        VkSamplerYcbcrModelConversion(..), VkSamplerYcbcrRange(..),
        VkSamplerCreateBitmask(..), VkSamplerCreateFlagBits(),
        VkSamplerCreateFlags(), VkSamplerReductionModeEXT(..),
        VkSamplerYcbcrModelConversionKHR(..), VkSamplerYcbcrRangeKHR(..),
        VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION,
        pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION,
        VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME,
        pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT,
        pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT,
        pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT,
        pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT)
       where
import GHC.Ptr                                     (Ptr (..))
import Graphics.Vulkan.Core_1_2                    (pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT,
                                                    pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES,
                                                    pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Enum.Sampler
import Graphics.Vulkan.Types.Struct.PhysicalDevice (VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT)
import Graphics.Vulkan.Types.Struct.Sampler        (VkSamplerReductionModeCreateInfoEXT)

pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION = 2

type VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION = 2

pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME :: CString

pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME <-
        (is_VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME -> True)
  where
    VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME
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
        =
        VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES

pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT =
        VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT =
        VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT

pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT =
        VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE

pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT =
        VK_SAMPLER_REDUCTION_MODE_MIN

pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT =
        VK_SAMPLER_REDUCTION_MODE_MAX
