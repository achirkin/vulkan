{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_driver_properties
       (-- * Vulkan extension: @VK_KHR_driver_properties@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Rakos @drakos-amd@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @197@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkConformanceVersionKHR, VkDriverId(..), VkDriverIdKHR(..),
        VkPhysicalDeviceDriverPropertiesKHR,
        VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION,
        pattern VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION,
        VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME,
        pattern VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR,
        pattern VK_MAX_DRIVER_NAME_SIZE_KHR,
        pattern VK_MAX_DRIVER_INFO_SIZE_KHR,
        pattern VK_DRIVER_ID_AMD_PROPRIETARY_KHR,
        pattern VK_DRIVER_ID_AMD_OPEN_SOURCE_KHR,
        pattern VK_DRIVER_ID_MESA_RADV_KHR,
        pattern VK_DRIVER_ID_NVIDIA_PROPRIETARY_KHR,
        pattern VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR,
        pattern VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR,
        pattern VK_DRIVER_ID_IMAGINATION_PROPRIETARY_KHR,
        pattern VK_DRIVER_ID_QUALCOMM_PROPRIETARY_KHR,
        pattern VK_DRIVER_ID_ARM_PROPRIETARY_KHR,
        pattern VK_DRIVER_ID_GOOGLE_SWIFTSHADER_KHR,
        pattern VK_DRIVER_ID_GGP_PROPRIETARY_KHR,
        pattern VK_DRIVER_ID_BROADCOM_PROPRIETARY_KHR)
       where
import GHC.Ptr                                         (Ptr (..))
import Graphics.Vulkan.Constants                       (pattern VK_MAX_DRIVER_INFO_SIZE_KHR,
                                                        pattern VK_MAX_DRIVER_NAME_SIZE_KHR)
import Graphics.Vulkan.Core_1_2                        (pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Enum.DriverId
import Graphics.Vulkan.Types.Struct.ConformanceVersion (VkConformanceVersionKHR)
import Graphics.Vulkan.Types.Struct.PhysicalDevice     (VkPhysicalDeviceDriverPropertiesKHR)

pattern VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION = 1

type VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION = 1

pattern VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME :: CString

pattern VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME <-
        (is_VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME -> True)
  where
    VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME
      = _VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME

{-# INLINE _VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME #-}

_VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME :: CString
_VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME
  = Ptr "VK_KHR_driver_properties\NUL"#

{-# INLINE is_VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME #-}

is_VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME

type VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME =
     "VK_KHR_driver_properties"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR =
        VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES

pattern VK_DRIVER_ID_AMD_PROPRIETARY_KHR =
        VK_DRIVER_ID_AMD_PROPRIETARY

pattern VK_DRIVER_ID_AMD_OPEN_SOURCE_KHR =
        VK_DRIVER_ID_AMD_OPEN_SOURCE

pattern VK_DRIVER_ID_MESA_RADV_KHR = VK_DRIVER_ID_MESA_RADV

pattern VK_DRIVER_ID_NVIDIA_PROPRIETARY_KHR =
        VK_DRIVER_ID_NVIDIA_PROPRIETARY

pattern VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR =
        VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS

pattern VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR =
        VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA

pattern VK_DRIVER_ID_IMAGINATION_PROPRIETARY_KHR =
        VK_DRIVER_ID_IMAGINATION_PROPRIETARY

pattern VK_DRIVER_ID_QUALCOMM_PROPRIETARY_KHR =
        VK_DRIVER_ID_QUALCOMM_PROPRIETARY

pattern VK_DRIVER_ID_ARM_PROPRIETARY_KHR =
        VK_DRIVER_ID_ARM_PROPRIETARY

pattern VK_DRIVER_ID_GOOGLE_SWIFTSHADER_KHR =
        VK_DRIVER_ID_GOOGLE_SWIFTSHADER

pattern VK_DRIVER_ID_GGP_PROPRIETARY_KHR =
        VK_DRIVER_ID_GGP_PROPRIETARY

pattern VK_DRIVER_ID_BROADCOM_PROPRIETARY_KHR =
        VK_DRIVER_ID_BROADCOM_PROPRIETARY
