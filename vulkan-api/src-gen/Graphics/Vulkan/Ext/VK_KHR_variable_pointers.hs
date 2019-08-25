{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
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
        -- contact: @Jesse Hall @critsec@
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
        VkPhysicalDeviceVariablePointerFeaturesKHR,
        VK_KHR_VARIABLE_POINTERS_SPEC_VERSION,
        pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION,
        VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME,
        pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR)
       where
import           GHC.Ptr                                     (Ptr (..))
import           Graphics.Vulkan.Core_1_1                    (pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Struct.PhysicalDevice (VkPhysicalDeviceVariablePointerFeaturesKHR)

pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION = 1

type VK_KHR_VARIABLE_POINTERS_SPEC_VERSION = 1

pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME :: CString

pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME <-
        (is_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME -> True)
  where
    VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME
      = _VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME

{-# INLINE _VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME #-}

_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME :: CString
_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME
  = Ptr "VK_KHR_variable_pointers\NUL"#

{-# INLINE is_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME #-}

is_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME

type VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME =
     "VK_KHR_variable_pointers"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
