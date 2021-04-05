{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_vulkan_memory_model
       (-- * Vulkan extension: @VK_KHR_vulkan_memory_model@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jeffbolznv@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @212@
        VkPhysicalDeviceVulkanMemoryModelFeaturesKHR,
        VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION,
        pattern VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION,
        VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME,
        pattern VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR)
       where
import GHC.Ptr                                     (Ptr (..))
import Graphics.Vulkan.Core_1_2                    (pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Struct.PhysicalDevice (VkPhysicalDeviceVulkanMemoryModelFeaturesKHR)

pattern VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION = 3

type VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION = 3

pattern VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME :: CString

pattern VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME <-
        (is_VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME -> True)
  where
    VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
      = _VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME

{-# INLINE _VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME #-}

_VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME :: CString
_VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
  = Ptr "VK_KHR_vulkan_memory_model\NUL"#

{-# INLINE is_VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME #-}

is_VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME

type VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME =
     "VK_KHR_vulkan_memory_model"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES
