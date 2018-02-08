#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_AMD_extension_134
       (-- * Vulkan extension: @VK_AMD_extension_134@
        -- |
        --
        -- supported: @disabled@
        --
        -- contact: @Mais Alnasser @malnasse@
        --
        -- author: @AMD@
        --
        -- Extension number: @134@
        VK_AMD_EXTENSION_134_SPEC_VERSION,
        pattern VK_AMD_EXTENSION_134_SPEC_VERSION,
        VK_AMD_EXTENSION_134_EXTENSION_NAME,
        pattern VK_AMD_EXTENSION_134_EXTENSION_NAME)
       where
import           Foreign.C.String (CString)
import           GHC.Ptr          (Ptr (..))

pattern VK_AMD_EXTENSION_134_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_AMD_EXTENSION_134_SPEC_VERSION = 0

type VK_AMD_EXTENSION_134_SPEC_VERSION = 0

pattern VK_AMD_EXTENSION_134_EXTENSION_NAME :: CString

pattern VK_AMD_EXTENSION_134_EXTENSION_NAME <-
        (is_VK_AMD_EXTENSION_134_EXTENSION_NAME -> True)
  where VK_AMD_EXTENSION_134_EXTENSION_NAME
          = _VK_AMD_EXTENSION_134_EXTENSION_NAME

_VK_AMD_EXTENSION_134_EXTENSION_NAME :: CString

{-# INLINE _VK_AMD_EXTENSION_134_EXTENSION_NAME #-}
_VK_AMD_EXTENSION_134_EXTENSION_NAME
  = Ptr "VK_AMD_extension_134\NUL"##

is_VK_AMD_EXTENSION_134_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_AMD_EXTENSION_134_EXTENSION_NAME #-}
is_VK_AMD_EXTENSION_134_EXTENSION_NAME
  = (_VK_AMD_EXTENSION_134_EXTENSION_NAME ==)

type VK_AMD_EXTENSION_134_EXTENSION_NAME = "VK_AMD_extension_134"
