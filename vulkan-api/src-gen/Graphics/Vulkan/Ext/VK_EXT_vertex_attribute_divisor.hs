{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_vertex_attribute_divisor
       (-- * Vulkan extension: @VK_EXT_vertex_attribute_divisor@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Vikram Kushwaha @vkushwaha@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @191@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties,
        module Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT,
        module Graphics.Vulkan.Types.Struct.VkPipelineVertexInputDivisorStateCreateInfoEXT,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkPipelineVertexInputStateCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Struct.VkVertexInputAttributeDescription,
        module Graphics.Vulkan.Types.Struct.VkVertexInputBindingDescription,
        module Graphics.Vulkan.Types.Struct.VkVertexInputBindingDivisorDescriptionEXT,
        module Graphics.Vulkan.Types.Enum.VkVertexInputRate,
        -- > #include "vk_platform.h"
        VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION,
        pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION,
        VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME,
        pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT)
       where
import           GHC.Ptr
                                                                                                   (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkVertexInputRate
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
import           Graphics.Vulkan.Types.Struct.VkPipelineVertexInputDivisorStateCreateInfoEXT
import           Graphics.Vulkan.Types.Struct.VkPipelineVertexInputStateCreateInfo
import           Graphics.Vulkan.Types.Struct.VkVertexInputAttributeDescription
import           Graphics.Vulkan.Types.Struct.VkVertexInputBindingDescription
import           Graphics.Vulkan.Types.Struct.VkVertexInputBindingDivisorDescriptionEXT

pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION = 1

type VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION = 1

pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME :: CString

pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME <-
        (is_VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME -> True)
  where VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
          = _VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME

{-# INLINE _VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME #-}

_VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME :: CString
_VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
  = Ptr "VK_EXT_vertex_attribute_divisor\NUL"#

{-# INLINE is_VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME #-}

is_VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME ::
                                                  CString -> Bool
is_VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME

type VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME =
     "VK_EXT_vertex_attribute_divisor"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT
        = VkStructureType 1000190000

pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT
        = VkStructureType 1000190001
