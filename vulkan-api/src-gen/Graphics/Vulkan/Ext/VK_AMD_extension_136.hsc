#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_AMD_extension_136
       (-- * Vulkan extension: @VK_AMD_extension_136@
        -- |
        --
        -- supported: @disabled@
        --
        -- contact: @Mais Alnasser @malnasse@
        --
        -- author: @AMD@
        --
        -- Extension number: @136@
        VK_AMD_EXTENSION_136_SPEC_VERSION,
        pattern VK_AMD_EXTENSION_136_SPEC_VERSION,
        VK_AMD_EXTENSION_136_EXTENSION_NAME,
        pattern VK_AMD_EXTENSION_136_EXTENSION_NAME)
       where
import           Foreign.C.String        (CString)
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Marshal

pattern VK_AMD_EXTENSION_136_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_AMD_EXTENSION_136_SPEC_VERSION = 0

type VK_AMD_EXTENSION_136_SPEC_VERSION = 0

pattern VK_AMD_EXTENSION_136_EXTENSION_NAME :: CString

pattern VK_AMD_EXTENSION_136_EXTENSION_NAME <-
        (is_VK_AMD_EXTENSION_136_EXTENSION_NAME -> True)
  where VK_AMD_EXTENSION_136_EXTENSION_NAME
          = _VK_AMD_EXTENSION_136_EXTENSION_NAME

{-# INLINE _VK_AMD_EXTENSION_136_EXTENSION_NAME #-}

_VK_AMD_EXTENSION_136_EXTENSION_NAME :: CString
_VK_AMD_EXTENSION_136_EXTENSION_NAME
  = Ptr "VK_AMD_extension_136\NUL"##

{-# INLINE is_VK_AMD_EXTENSION_136_EXTENSION_NAME #-}

is_VK_AMD_EXTENSION_136_EXTENSION_NAME :: CString -> Bool
is_VK_AMD_EXTENSION_136_EXTENSION_NAME
  = eqCStrings _VK_AMD_EXTENSION_136_EXTENSION_NAME

type VK_AMD_EXTENSION_136_EXTENSION_NAME = "VK_AMD_extension_136"
