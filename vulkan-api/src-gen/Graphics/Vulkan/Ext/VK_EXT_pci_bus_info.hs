{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_pci_bus_info
       (-- * Vulkan extension: @VK_EXT_pci_bus_info@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Matthaeus G. Chajdas @anteru@
        --
        -- author: @EXT@
        --
        -- type: @device@
        --
        -- Extension number: @213@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkPhysicalDeviceLimits, VkPhysicalDevicePCIBusInfoPropertiesEXT,
        VkPhysicalDeviceProperties, VkPhysicalDeviceProperties2,
        VkPhysicalDeviceSparseProperties, VkPhysicalDeviceType(..),
        VkSampleCountBitmask(..), VkSampleCountFlagBits(),
        VkSampleCountFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_EXT_PCI_BUS_INFO_SPEC_VERSION,
        pattern VK_EXT_PCI_BUS_INFO_SPEC_VERSION,
        VK_EXT_PCI_BUS_INFO_EXTENSION_NAME,
        pattern VK_EXT_PCI_BUS_INFO_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT)
       where
import GHC.Ptr                                       (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Struct.PhysicalDevice   (VkPhysicalDeviceLimits, VkPhysicalDevicePCIBusInfoPropertiesEXT,
                                                      VkPhysicalDeviceProperties,
                                                      VkPhysicalDeviceProperties2,
                                                      VkPhysicalDeviceSparseProperties)

pattern VK_EXT_PCI_BUS_INFO_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_PCI_BUS_INFO_SPEC_VERSION = 2

type VK_EXT_PCI_BUS_INFO_SPEC_VERSION = 2

pattern VK_EXT_PCI_BUS_INFO_EXTENSION_NAME :: CString

pattern VK_EXT_PCI_BUS_INFO_EXTENSION_NAME <-
        (is_VK_EXT_PCI_BUS_INFO_EXTENSION_NAME -> True)
  where
    VK_EXT_PCI_BUS_INFO_EXTENSION_NAME
      = _VK_EXT_PCI_BUS_INFO_EXTENSION_NAME

{-# INLINE _VK_EXT_PCI_BUS_INFO_EXTENSION_NAME #-}

_VK_EXT_PCI_BUS_INFO_EXTENSION_NAME :: CString
_VK_EXT_PCI_BUS_INFO_EXTENSION_NAME
  = Ptr "VK_EXT_pci_bus_info\NUL"#

{-# INLINE is_VK_EXT_PCI_BUS_INFO_EXTENSION_NAME #-}

is_VK_EXT_PCI_BUS_INFO_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_PCI_BUS_INFO_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_PCI_BUS_INFO_EXTENSION_NAME

type VK_EXT_PCI_BUS_INFO_EXTENSION_NAME = "VK_EXT_pci_bus_info"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT
        = VkStructureType 1000212000
