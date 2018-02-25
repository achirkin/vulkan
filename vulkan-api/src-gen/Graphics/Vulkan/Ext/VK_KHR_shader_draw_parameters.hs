{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_shader_draw_parameters
       (-- * Vulkan extension: @VK_KHR_shader_draw_parameters@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Koch @dgkoch@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @64@
        VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION,
        pattern VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION,
        VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME,
        pattern VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME)
       where
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Marshal

pattern VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION = 1

type VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION = 1

pattern VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME :: CString

pattern VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME <-
        (is_VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME -> True)
  where VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME
          = _VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME

{-# INLINE _VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME #-}

_VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME :: CString
_VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME
  = Ptr "VK_KHR_shader_draw_parameters\NUL"#

{-# INLINE is_VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME #-}

is_VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME

type VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME =
     "VK_KHR_shader_draw_parameters"
