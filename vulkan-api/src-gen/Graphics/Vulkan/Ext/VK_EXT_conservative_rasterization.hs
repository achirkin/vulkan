{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_conservative_rasterization
       (-- * Vulkan extension: @VK_EXT_conservative_rasterization@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Piers Daniell @pdaniell@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @102@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkConservativeRasterizationModeEXT,
        module Graphics.Vulkan.Types.Enum.VkCullModeFlags,
        module Graphics.Vulkan.Types.Enum.VkFrontFace,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceConservativeRasterizationPropertiesEXT,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties,
        module Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkPipelineRasterizationConservativeStateCreateInfoEXT,
        module Graphics.Vulkan.Types.Struct.VkPipelineRasterizationStateCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkPolygonMode,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION,
        pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION,
        VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME,
        pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT)
       where
import           GHC.Ptr
                                                                                                      (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkConservativeRasterizationModeEXT
import           Graphics.Vulkan.Types.Enum.VkCullModeFlags
import           Graphics.Vulkan.Types.Enum.VkFrontFace
import           Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.VkPolygonMode
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceConservativeRasterizationPropertiesEXT
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties
import           Graphics.Vulkan.Types.Struct.VkPipelineRasterizationConservativeStateCreateInfoEXT
import           Graphics.Vulkan.Types.Struct.VkPipelineRasterizationStateCreateInfo

pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1

type VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1

pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME :: CString

pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME <-
        (is_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME -> True)
  where VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
          = _VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME

{-# INLINE _VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME #-}

_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME :: CString
_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
  = Ptr "VK_EXT_conservative_rasterization\NUL"#

{-# INLINE is_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME #-}

is_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME ::
                                                    CString -> Bool
is_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME

type VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME =
     "VK_EXT_conservative_rasterization"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
        = VkStructureType 1000101000

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
        = VkStructureType 1000101001
