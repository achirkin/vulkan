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
        -- contact: @Piers Daniell @pdaniell-nv@
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
        module Graphics.Vulkan.Types.Enum.ConservativeRasterizationModeEXT,
        module Graphics.Vulkan.Types.Enum.CullModeFlags,
        module Graphics.Vulkan.Types.Enum.FrontFace,
        module Graphics.Vulkan.Types.Struct.PhysicalDevice,
        module Graphics.Vulkan.Types.Enum.PhysicalDeviceType,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.Pipeline,
        module Graphics.Vulkan.Types.Enum.PolygonMode,
        module Graphics.Vulkan.Types.Enum.SampleCountFlags,
        module Graphics.Vulkan.Types.Enum.StructureType,
        -- > #include "vk_platform.h"
        VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION,
        pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION,
        VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME,
        pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT)
       where
import           GHC.Ptr                                                     (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.ConservativeRasterizationModeEXT
import           Graphics.Vulkan.Types.Enum.CullModeFlags
import           Graphics.Vulkan.Types.Enum.FrontFace
import           Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.PolygonMode
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Struct.PhysicalDevice
import           Graphics.Vulkan.Types.Struct.Pipeline

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
