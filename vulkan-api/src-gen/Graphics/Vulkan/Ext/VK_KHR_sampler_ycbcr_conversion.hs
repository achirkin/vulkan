{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_sampler_ycbcr_conversion
       (-- * Vulkan extension: @VK_KHR_sampler_ycbcr_conversion@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Andrew Garrard @fluppeteer@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @157@
        --
        -- Required extensions: 'VK_KHR_maintenance1', 'VK_KHR_bind_memory2', 'VK_KHR_get_memory_requirements2', 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_maintenance1', 'VK_KHR_bind_memory2', 'VK_KHR_get_memory_requirements2', 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkBindImagePlaneMemoryInfoKHR,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkBorderColor,
        module Graphics.Vulkan.Types.Enum.VkChromaLocationKHR,
        module Graphics.Vulkan.Types.Enum.VkCompareOp,
        module Graphics.Vulkan.Types.Struct.VkComponentMapping,
        module Graphics.Vulkan.Types.Enum.VkComponentSwizzle,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.Enum.VkFilter,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkImageAspectFlags,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties2KHR,
        module Graphics.Vulkan.Types.Struct.VkImageMemoryRequirementsInfo2KHR,
        module Graphics.Vulkan.Types.Struct.VkImagePlaneMemoryRequirementsInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkImageSubresourceRange,
        module Graphics.Vulkan.Types.Struct.VkImageViewCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkImageViewType,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkSamplerAddressMode,
        module Graphics.Vulkan.Types.Struct.VkSamplerCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkSamplerMipmapMode,
        module Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionCreateInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionImageFormatPropertiesKHR,
        module Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkSamplerYcbcrModelConversionKHR,
        module Graphics.Vulkan.Types.Enum.VkSamplerYcbcrRangeKHR,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        vkCreateSamplerYcbcrConversionKHR,
        vkDestroySamplerYcbcrConversionKHR,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        VK_KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION,
        pattern VK_KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION,
        VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME,
        pattern VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES_KHR,
        pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES_KHR,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR_EXT,
        pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR,
        pattern VK_FORMAT_G8B8G8R8_422_UNORM_KHR,
        pattern VK_FORMAT_B8G8R8G8_422_UNORM_KHR,
        pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM_KHR,
        pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM_KHR,
        pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM_KHR,
        pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM_KHR,
        pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM_KHR,
        pattern VK_FORMAT_R10X6_UNORM_PACK16_KHR,
        pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16_KHR,
        pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16_KHR,
        pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16_KHR,
        pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16_KHR,
        pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_R12X4_UNORM_PACK16_KHR,
        pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16_KHR,
        pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16_KHR,
        pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16_KHR,
        pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16_KHR,
        pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16_KHR,
        pattern VK_FORMAT_G16B16G16R16_422_UNORM_KHR,
        pattern VK_FORMAT_B16G16R16G16_422_UNORM_KHR,
        pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM_KHR,
        pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM_KHR,
        pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM_KHR,
        pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM_KHR,
        pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM_KHR,
        pattern VK_IMAGE_ASPECT_PLANE_0_BIT_KHR,
        pattern VK_IMAGE_ASPECT_PLANE_1_BIT_KHR,
        pattern VK_IMAGE_ASPECT_PLANE_2_BIT_KHR,
        pattern VK_IMAGE_CREATE_DISJOINT_BIT_KHR,
        pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT_KHR,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT_KHR,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT_KHR,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT_KHR,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT_KHR,
        pattern VK_FORMAT_FEATURE_DISJOINT_BIT_KHR,
        pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT_KHR)
       where
import           GHC.Ptr
                                                                                                 (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkBorderColor
import           Graphics.Vulkan.Types.Enum.VkChromaLocationKHR
import           Graphics.Vulkan.Types.Enum.VkCompareOp
import           Graphics.Vulkan.Types.Enum.VkComponentSwizzle
import           Graphics.Vulkan.Types.Enum.VkDebugReportObjectTypeEXT
                                                                                                 (VkDebugReportObjectTypeEXT (..))
import           Graphics.Vulkan.Types.Enum.VkFilter
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkFormatFeatureFlags
                                                                                                 (VkFormatFeatureBitmask (..),
                                                                                                 VkFormatFeatureFlagBits)
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags
                                                                                                 (VkImageCreateBitmask (..),
                                                                                                 VkImageCreateFlagBits)
import           Graphics.Vulkan.Types.Enum.VkImageViewType
import           Graphics.Vulkan.Types.Enum.VkInternalAllocationType
import           Graphics.Vulkan.Types.Enum.VkObjectType
                                                                                                 (VkObjectType (..))
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkSamplerAddressMode
import           Graphics.Vulkan.Types.Enum.VkSamplerMipmapMode
import           Graphics.Vulkan.Types.Enum.VkSamplerYcbcrModelConversionKHR
import           Graphics.Vulkan.Types.Enum.VkSamplerYcbcrRangeKHR
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfoKHR
import           Graphics.Vulkan.Types.Struct.VkBindImagePlaneMemoryInfoKHR
import           Graphics.Vulkan.Types.Struct.VkComponentMapping
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo
import           Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo
import           Graphics.Vulkan.Types.Struct.VkExtent3D
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties2KHR
import           Graphics.Vulkan.Types.Struct.VkImageMemoryRequirementsInfo2KHR
import           Graphics.Vulkan.Types.Struct.VkImagePlaneMemoryRequirementsInfoKHR
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceRange
import           Graphics.Vulkan.Types.Struct.VkImageViewCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
import           Graphics.Vulkan.Types.Struct.VkSamplerCreateInfo
import           Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionCreateInfoKHR
import           Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionImageFormatPropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionInfoKHR

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateSamplerYcbcrConversionKHR
--   >     ( VkDevice device
--   >     , const VkSamplerYcbcrConversionCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSamplerYcbcrConversionKHR* pYcbcrConversion
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCreateSamplerYcbcrConversionKHR.html vkCreateSamplerYcbcrConversionKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateSamplerYcbcrConversionKHR"
               vkCreateSamplerYcbcrConversionKHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSamplerYcbcrConversionCreateInfoKHR -- ^ pCreateInfo
                                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkSamplerYcbcrConversionKHR -- ^ pYcbcrConversion
                                                     -> IO VkResult

-- | > () vkDestroySamplerYcbcrConversionKHR
--   >     ( VkDevice device
--   >     , VkSamplerYcbcrConversionKHR ycbcrConversion
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkDestroySamplerYcbcrConversionKHR.html vkDestroySamplerYcbcrConversionKHR registry at www.khronos.org>
foreign import ccall unsafe "vkDestroySamplerYcbcrConversionKHR"
               vkDestroySamplerYcbcrConversionKHR ::
               VkDevice -- ^ device
                        ->
                 VkSamplerYcbcrConversionKHR -- ^ ycbcrConversion
                                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                          -> IO ()

pattern VK_KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION = 1

type VK_KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION = 1

pattern VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME :: CString

pattern VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME <-
        (is_VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME -> True)
  where VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME
          = _VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME

{-# INLINE _VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME #-}

_VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME :: CString
_VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME
  = Ptr "VK_KHR_sampler_ycbcr_conversion\NUL"#

{-# INLINE is_VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME #-}

is_VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME ::
                                                  CString -> Bool
is_VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME

type VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME =
     "VK_KHR_sampler_ycbcr_conversion"

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO_KHR
        = VkStructureType 1000156000

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO_KHR =
        VkStructureType 1000156001

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO_KHR =
        VkStructureType 1000156002

pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO_KHR
        = VkStructureType 1000156003

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES_KHR
        = VkStructureType 1000156004

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES_KHR
        = VkStructureType 1000156005

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR_EXT
        :: VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR_EXT
        = VkDebugReportObjectTypeEXT 1000156000

pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR :: VkObjectType

pattern VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR =
        VkObjectType 1000156000

pattern VK_FORMAT_G8B8G8R8_422_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G8B8G8R8_422_UNORM_KHR = VkFormat 1000156000

pattern VK_FORMAT_B8G8R8G8_422_UNORM_KHR :: VkFormat

pattern VK_FORMAT_B8G8R8G8_422_UNORM_KHR = VkFormat 1000156001

pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM_KHR =
        VkFormat 1000156002

pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G8_B8R8_2PLANE_420_UNORM_KHR =
        VkFormat 1000156003

pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM_KHR =
        VkFormat 1000156004

pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G8_B8R8_2PLANE_422_UNORM_KHR =
        VkFormat 1000156005

pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM_KHR =
        VkFormat 1000156006

pattern VK_FORMAT_R10X6_UNORM_PACK16_KHR :: VkFormat

pattern VK_FORMAT_R10X6_UNORM_PACK16_KHR = VkFormat 1000156007

pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16_KHR :: VkFormat

pattern VK_FORMAT_R10X6G10X6_UNORM_2PACK16_KHR =
        VkFormat 1000156008

pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16_KHR =
        VkFormat 1000156009

pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16_KHR =
        VkFormat 1000156010

pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16_KHR =
        VkFormat 1000156011

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16_KHR =
        VkFormat 1000156012

pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16_KHR =
        VkFormat 1000156013

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16_KHR =
        VkFormat 1000156014

pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16_KHR =
        VkFormat 1000156015

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16_KHR =
        VkFormat 1000156016

pattern VK_FORMAT_R12X4_UNORM_PACK16_KHR :: VkFormat

pattern VK_FORMAT_R12X4_UNORM_PACK16_KHR = VkFormat 1000156017

pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16_KHR :: VkFormat

pattern VK_FORMAT_R12X4G12X4_UNORM_2PACK16_KHR =
        VkFormat 1000156018

pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16_KHR =
        VkFormat 1000156019

pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16_KHR =
        VkFormat 1000156020

pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16_KHR =
        VkFormat 1000156021

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16_KHR =
        VkFormat 1000156022

pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16_KHR =
        VkFormat 1000156023

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16_KHR =
        VkFormat 1000156024

pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16_KHR =
        VkFormat 1000156025

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16_KHR ::
        VkFormat

pattern VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16_KHR =
        VkFormat 1000156026

pattern VK_FORMAT_G16B16G16R16_422_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G16B16G16R16_422_UNORM_KHR = VkFormat 1000156027

pattern VK_FORMAT_B16G16R16G16_422_UNORM_KHR :: VkFormat

pattern VK_FORMAT_B16G16R16G16_422_UNORM_KHR = VkFormat 1000156028

pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM_KHR =
        VkFormat 1000156029

pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G16_B16R16_2PLANE_420_UNORM_KHR =
        VkFormat 1000156030

pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM_KHR =
        VkFormat 1000156031

pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G16_B16R16_2PLANE_422_UNORM_KHR =
        VkFormat 1000156032

pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM_KHR :: VkFormat

pattern VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM_KHR =
        VkFormat 1000156033

-- | bitpos = @4@
pattern VK_IMAGE_ASPECT_PLANE_0_BIT_KHR :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_PLANE_0_BIT_KHR = VkImageAspectFlagBits 16

-- | bitpos = @5@
pattern VK_IMAGE_ASPECT_PLANE_1_BIT_KHR :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_PLANE_1_BIT_KHR = VkImageAspectFlagBits 32

-- | bitpos = @6@
pattern VK_IMAGE_ASPECT_PLANE_2_BIT_KHR :: VkImageAspectFlagBits

pattern VK_IMAGE_ASPECT_PLANE_2_BIT_KHR = VkImageAspectFlagBits 64

-- | bitpos = @9@
pattern VK_IMAGE_CREATE_DISJOINT_BIT_KHR :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_DISJOINT_BIT_KHR =
        VkImageCreateFlagBits 512

-- | Format can have midpoint rather than cosited chroma samples
--
--   bitpos = @17@
pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT_KHR ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT_KHR =
        VkFormatFeatureFlagBits 131072

-- | Format can be used with linear filtering whilst color conversion is enabled
--
--   bitpos = @18@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT_KHR
        :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT_KHR
        = VkFormatFeatureFlagBits 262144

-- | Format can have different chroma, min and mag filters
--
--   bitpos = @19@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT_KHR
        :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT_KHR
        = VkFormatFeatureFlagBits 524288

-- | bitpos = @20@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT_KHR
        :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT_KHR
        = VkFormatFeatureFlagBits 1048576

-- | bitpos = @21@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT_KHR
        :: VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT_KHR
        = VkFormatFeatureFlagBits 2097152

-- | Format supports disjoint planes
--
--   bitpos = @22@
pattern VK_FORMAT_FEATURE_DISJOINT_BIT_KHR ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_DISJOINT_BIT_KHR =
        VkFormatFeatureFlagBits 4194304

-- | Format can have cosited rather than midpoint chroma samples
--
--   bitpos = @23@
pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT_KHR ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT_KHR =
        VkFormatFeatureFlagBits 8388608
