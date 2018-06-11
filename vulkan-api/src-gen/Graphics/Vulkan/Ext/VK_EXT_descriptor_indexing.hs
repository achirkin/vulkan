{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_descriptor_indexing
       (-- * Vulkan extension: @VK_EXT_descriptor_indexing@
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
        -- Extension number: @162@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_maintenance3'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_maintenance3'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.Descriptor,
        module Graphics.Vulkan.Types.Struct.Descriptor,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.Device,
        module Graphics.Vulkan.Types.Enum.Device,
        module Graphics.Vulkan.Types.Struct.PhysicalDevice,
        module Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures,
        module Graphics.Vulkan.Types.Enum.PhysicalDeviceType,
        module Graphics.Vulkan.Types.Enum.SampleCountFlags,
        module Graphics.Vulkan.Types.Enum.Shader,
        module Graphics.Vulkan.Types.Enum.StructureType,
        -- > #include "vk_platform.h"
        VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION,
        pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION,
        VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME,
        pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT,
        pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT,
        pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT,
        pattern VK_ERROR_FRAGMENTATION_EXT)
       where
import           GHC.Ptr                                             (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.Descriptor
import           Graphics.Vulkan.Types.Enum.Device
import           Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.Result                   (VkResult (..))
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.Shader
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Struct.Descriptor
import           Graphics.Vulkan.Types.Struct.Device
import           Graphics.Vulkan.Types.Struct.PhysicalDevice
import           Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures

pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION = 2

type VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION = 2

pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME :: CString

pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME <-
        (is_VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME -> True)
  where VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
          = _VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME

{-# INLINE _VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME #-}

_VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME :: CString
_VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
  = Ptr "VK_EXT_descriptor_indexing\NUL"#

{-# INLINE is_VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME #-}

is_VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME

type VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME =
     "VK_EXT_descriptor_indexing"

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT
        = VkStructureType 1000161000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
        = VkStructureType 1000161001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
        = VkStructureType 1000161002

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
        = VkStructureType 1000161003

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
        = VkStructureType 1000161004

-- | bitpos = @1@
pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT ::
        VkDescriptorPoolCreateFlagBits

pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT =
        VkDescriptorPoolCreateFlagBits 2

-- | bitpos = @1@
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
        :: VkDescriptorSetLayoutCreateFlagBits

pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
        = VkDescriptorSetLayoutCreateFlagBits 2

pattern VK_ERROR_FRAGMENTATION_EXT :: VkResult

pattern VK_ERROR_FRAGMENTATION_EXT = VkResult (-1000161000)
