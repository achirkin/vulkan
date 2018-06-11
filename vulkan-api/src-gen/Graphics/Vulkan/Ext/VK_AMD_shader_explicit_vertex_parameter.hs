{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_AMD_shader_explicit_vertex_parameter
       (-- * Vulkan extension: @VK_AMD_shader_explicit_vertex_parameter@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Qun Lin, AMD @linqun@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @22@
        VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_SPEC_VERSION,
        pattern VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_SPEC_VERSION,
        VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME,
        pattern VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME)
       where
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Marshal

pattern VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_SPEC_VERSION = 1

type VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_SPEC_VERSION = 1

pattern VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME ::
        CString

pattern VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME <-
        (is_VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME -> True)
  where VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME
          = _VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME

{-# INLINE _VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME
           #-}

_VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME :: CString
_VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME
  = Ptr "VK_AMD_shader_explicit_vertex_parameter\NUL"#

{-# INLINE is_VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME
           #-}

is_VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME ::
                                                          CString -> Bool
is_VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME

type VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME =
     "VK_AMD_shader_explicit_vertex_parameter"
