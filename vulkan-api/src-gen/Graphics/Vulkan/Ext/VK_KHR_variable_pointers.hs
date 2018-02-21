{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_variable_pointers
       (-- * Vulkan extension: @VK_KHR_variable_pointers@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @jessehall@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @121@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_storage_buffer_storage_class'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_storage_buffer_storage_class'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceVariablePointerFeaturesKHR,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        VK_KHR_VARIABLE_POINTERS_SPEC_VERSION,
        pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION,
        VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME,
        pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR)
       where
import           GHC.Ptr
                                                                                          (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo
import           Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceVariablePointerFeaturesKHR

pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION = 1

type VK_KHR_VARIABLE_POINTERS_SPEC_VERSION = 1

pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME :: CString

pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME <-
        (is_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME -> True)
  where VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME
          = _VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME

{-# INLINE _VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME #-}

_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME :: CString
_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME
  = Ptr "VK_KHR_variable_pointers\NUL"#

{-# INLINE is_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME #-}

is_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME
  = eqCStrings _VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME

type VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME =
     "VK_KHR_variable_pointers"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR
        = VkStructureType 1000120000
