{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_separate_stencil_usage
       (-- * Vulkan extension: @VK_EXT_separate_stencil_usage@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Rakos @drakos-amd@
        --
        -- author: @EXT@
        --
        -- type: @device@
        --
        -- Extension number: @247@
        VkImageStencilUsageCreateInfoEXT,
        VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION,
        pattern VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION,
        VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME,
        pattern VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT)
       where
import GHC.Ptr                            (Ptr (..))
import Graphics.Vulkan.Core_1_2           (pattern VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Struct.Image (VkImageStencilUsageCreateInfoEXT)

pattern VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION = 1

type VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION = 1

pattern VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME :: CString

pattern VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME <-
        (is_VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME -> True)
  where
    VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME
      = _VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME

{-# INLINE _VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME #-}

_VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME :: CString
_VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME
  = Ptr "VK_EXT_separate_stencil_usage\NUL"#

{-# INLINE is_VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME #-}

is_VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME

type VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME =
     "VK_EXT_separate_stencil_usage"

pattern VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT =
        VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO
