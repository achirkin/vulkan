{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_scalar_block_layout
       (-- * Vulkan extension: @VK_EXT_scalar_block_layout@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Tobias Hector @tobski@
        --
        -- author: @EXT@
        --
        -- type: @device@
        --
        -- Extension number: @222@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkPhysicalDeviceScalarBlockLayoutFeaturesEXT,
        VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION,
        pattern VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION,
        VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME,
        pattern VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT)
       where
import GHC.Ptr                                     (Ptr (..))
import Graphics.Vulkan.Core_1_2                    (pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Struct.PhysicalDevice (VkPhysicalDeviceScalarBlockLayoutFeaturesEXT)

pattern VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION = 1

type VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION = 1

pattern VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME :: CString

pattern VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME <-
        (is_VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME -> True)
  where
    VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME
      = _VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME

{-# INLINE _VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME #-}

_VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME :: CString
_VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME
  = Ptr "VK_EXT_scalar_block_layout\NUL"#

{-# INLINE is_VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME #-}

is_VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME

type VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME =
     "VK_EXT_scalar_block_layout"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES
