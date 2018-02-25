{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NV_geometry_shader_passthrough
       (-- * Vulkan extension: @VK_NV_geometry_shader_passthrough@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Koch @dgkoch@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @96@
        VK_NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION,
        pattern VK_NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION,
        VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME,
        pattern VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME)
       where
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Marshal

pattern VK_NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION = 1

type VK_NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION = 1

pattern VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME :: CString

pattern VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME <-
        (is_VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME -> True)
  where VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME
          = _VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME

{-# INLINE _VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME #-}

_VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME :: CString
_VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME
  = Ptr "VK_NV_geometry_shader_passthrough\NUL"#

{-# INLINE is_VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME #-}

is_VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME ::
                                                    CString -> Bool
is_VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME

type VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME =
     "VK_NV_geometry_shader_passthrough"
