{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NV_glsl_shader
       (-- * Vulkan extension: @VK_NV_glsl_shader@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Piers Daniell @pdaniell-nv@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @13@
        VK_NV_GLSL_SHADER_SPEC_VERSION,
        pattern VK_NV_GLSL_SHADER_SPEC_VERSION,
        VK_NV_GLSL_SHADER_EXTENSION_NAME,
        pattern VK_NV_GLSL_SHADER_EXTENSION_NAME,
        pattern VK_ERROR_INVALID_SHADER_NV)
       where
import           GHC.Ptr                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Enum.Result (VkResult (..))

pattern VK_NV_GLSL_SHADER_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_GLSL_SHADER_SPEC_VERSION = 1

type VK_NV_GLSL_SHADER_SPEC_VERSION = 1

pattern VK_NV_GLSL_SHADER_EXTENSION_NAME :: CString

pattern VK_NV_GLSL_SHADER_EXTENSION_NAME <-
        (is_VK_NV_GLSL_SHADER_EXTENSION_NAME -> True)
  where VK_NV_GLSL_SHADER_EXTENSION_NAME
          = _VK_NV_GLSL_SHADER_EXTENSION_NAME

{-# INLINE _VK_NV_GLSL_SHADER_EXTENSION_NAME #-}

_VK_NV_GLSL_SHADER_EXTENSION_NAME :: CString
_VK_NV_GLSL_SHADER_EXTENSION_NAME = Ptr "VK_NV_glsl_shader\NUL"#

{-# INLINE is_VK_NV_GLSL_SHADER_EXTENSION_NAME #-}

is_VK_NV_GLSL_SHADER_EXTENSION_NAME :: CString -> Bool
is_VK_NV_GLSL_SHADER_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_NV_GLSL_SHADER_EXTENSION_NAME

type VK_NV_GLSL_SHADER_EXTENSION_NAME = "VK_NV_glsl_shader"

pattern VK_ERROR_INVALID_SHADER_NV :: VkResult

pattern VK_ERROR_INVALID_SHADER_NV = VkResult (-1000012000)
