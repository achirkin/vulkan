{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_spirv_1_4
       (-- * Vulkan extension: @VK_KHR_spirv_1_4@
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
        -- Extension number: @237@
        --
        -- Required extensions: 'VK_KHR_shader_float_controls'.
        --

        -- ** Required extensions: 'VK_KHR_shader_float_controls'.
        VK_KHR_SPIRV_1_4_SPEC_VERSION,
        pattern VK_KHR_SPIRV_1_4_SPEC_VERSION,
        VK_KHR_SPIRV_1_4_EXTENSION_NAME,
        pattern VK_KHR_SPIRV_1_4_EXTENSION_NAME)
       where
import GHC.Ptr                 (Ptr (..))
import Graphics.Vulkan.Marshal

pattern VK_KHR_SPIRV_1_4_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_SPIRV_1_4_SPEC_VERSION = 1

type VK_KHR_SPIRV_1_4_SPEC_VERSION = 1

pattern VK_KHR_SPIRV_1_4_EXTENSION_NAME :: CString

pattern VK_KHR_SPIRV_1_4_EXTENSION_NAME <-
        (is_VK_KHR_SPIRV_1_4_EXTENSION_NAME -> True)
  where
    VK_KHR_SPIRV_1_4_EXTENSION_NAME = _VK_KHR_SPIRV_1_4_EXTENSION_NAME

{-# INLINE _VK_KHR_SPIRV_1_4_EXTENSION_NAME #-}

_VK_KHR_SPIRV_1_4_EXTENSION_NAME :: CString
_VK_KHR_SPIRV_1_4_EXTENSION_NAME = Ptr "VK_KHR_spirv_1_4\NUL"#

{-# INLINE is_VK_KHR_SPIRV_1_4_EXTENSION_NAME #-}

is_VK_KHR_SPIRV_1_4_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_SPIRV_1_4_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_SPIRV_1_4_EXTENSION_NAME

type VK_KHR_SPIRV_1_4_EXTENSION_NAME = "VK_KHR_spirv_1_4"
