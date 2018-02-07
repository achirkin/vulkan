#include "vulkan/vulkan.h"

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
        -- contact: @Piers Daniell @pdaniell@
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
import           Data.Int
import           Data.Word
import           Foreign.C.String                 (CString)
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Base
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Core
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers

pattern VK_NV_GLSL_SHADER_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_GLSL_SHADER_SPEC_VERSION = 1

type VK_NV_GLSL_SHADER_SPEC_VERSION = 1

pattern VK_NV_GLSL_SHADER_EXTENSION_NAME :: CString

pattern VK_NV_GLSL_SHADER_EXTENSION_NAME <-
        (is_VK_NV_GLSL_SHADER_EXTENSION_NAME -> True)
  where VK_NV_GLSL_SHADER_EXTENSION_NAME
          = _VK_NV_GLSL_SHADER_EXTENSION_NAME

_VK_NV_GLSL_SHADER_EXTENSION_NAME :: CString

{-# INLINE _VK_NV_GLSL_SHADER_EXTENSION_NAME #-}
_VK_NV_GLSL_SHADER_EXTENSION_NAME = Ptr "VK_NV_glsl_shader\NUL"##

is_VK_NV_GLSL_SHADER_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_NV_GLSL_SHADER_EXTENSION_NAME #-}
is_VK_NV_GLSL_SHADER_EXTENSION_NAME
  = (_VK_NV_GLSL_SHADER_EXTENSION_NAME ==)

type VK_NV_GLSL_SHADER_EXTENSION_NAME = "VK_NV_glsl_shader"

pattern VK_ERROR_INVALID_SHADER_NV :: VkResult

pattern VK_ERROR_INVALID_SHADER_NV = VkResult (-1000012000)
