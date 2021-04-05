{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_shader_atomic_int64
       (-- * Vulkan extension: @VK_KHR_shader_atomic_int64@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Aaron Hagan @ahagan@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @181@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkPhysicalDeviceShaderAtomicInt64FeaturesKHR,
        VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION,
        pattern VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION,
        VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME,
        pattern VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR)
       where
import GHC.Ptr                                     (Ptr (..))
import Graphics.Vulkan.Core_1_2                    (pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Struct.PhysicalDevice (VkPhysicalDeviceShaderAtomicInt64FeaturesKHR)

pattern VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION = 1

type VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION = 1

pattern VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME :: CString

pattern VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME <-
        (is_VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME -> True)
  where
    VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME
      = _VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME

{-# INLINE _VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME #-}

_VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME :: CString
_VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME
  = Ptr "VK_KHR_shader_atomic_int64\NUL"#

{-# INLINE is_VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME #-}

is_VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME

type VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME =
     "VK_KHR_shader_atomic_int64"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES
