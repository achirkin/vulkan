#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NV_extension_166
       (-- * Vulkan extension: @VK_NV_extension_166@
        -- |
        --
        -- supported: @disabled@
        --
        -- contact: @Daniel Koch @dgkoch@
        --
        -- author: @NV@
        --
        -- Extension number: @166@
        VK_EXT_EXTENSION_166_SPEC_VERSION,
        pattern VK_EXT_EXTENSION_166_SPEC_VERSION,
        VK_EXT_EXTENSION_166_EXTENSION_NAME,
        pattern VK_EXT_EXTENSION_166_EXTENSION_NAME)
       where
import           Foreign.C.String        (CString)
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Marshal

pattern VK_EXT_EXTENSION_166_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_EXTENSION_166_SPEC_VERSION = 0

type VK_EXT_EXTENSION_166_SPEC_VERSION = 0

pattern VK_EXT_EXTENSION_166_EXTENSION_NAME :: CString

pattern VK_EXT_EXTENSION_166_EXTENSION_NAME <-
        (is_VK_EXT_EXTENSION_166_EXTENSION_NAME -> True)
  where VK_EXT_EXTENSION_166_EXTENSION_NAME
          = _VK_EXT_EXTENSION_166_EXTENSION_NAME

{-# INLINE _VK_EXT_EXTENSION_166_EXTENSION_NAME #-}

_VK_EXT_EXTENSION_166_EXTENSION_NAME :: CString
_VK_EXT_EXTENSION_166_EXTENSION_NAME
  = Ptr "VK_NV_extension_166\NUL"##

{-# INLINE is_VK_EXT_EXTENSION_166_EXTENSION_NAME #-}

is_VK_EXT_EXTENSION_166_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_EXTENSION_166_EXTENSION_NAME
  = eqCStrings _VK_EXT_EXTENSION_166_EXTENSION_NAME

type VK_EXT_EXTENSION_166_EXTENSION_NAME = "VK_NV_extension_166"
