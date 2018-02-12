#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_GOOGLE_extension_192
       (-- * Vulkan extension: @VK_GOOGLE_extension_192@
        -- |
        --
        -- supported: @disabled@
        --
        -- contact: @Jean-Francois Roy @jfroy@
        --
        -- author: @GOOGLE@
        --
        -- Extension number: @192@
        VK_GOOGLE_EXTENSION_192_SPEC_VERSION,
        pattern VK_GOOGLE_EXTENSION_192_SPEC_VERSION,
        VK_GOOGLE_EXTENSION_192_EXTENSION_NAME,
        pattern VK_GOOGLE_EXTENSION_192_EXTENSION_NAME)
       where
import           Foreign.C.String        (CString)
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Marshal

pattern VK_GOOGLE_EXTENSION_192_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_GOOGLE_EXTENSION_192_SPEC_VERSION = 0

type VK_GOOGLE_EXTENSION_192_SPEC_VERSION = 0

pattern VK_GOOGLE_EXTENSION_192_EXTENSION_NAME :: CString

pattern VK_GOOGLE_EXTENSION_192_EXTENSION_NAME <-
        (is_VK_GOOGLE_EXTENSION_192_EXTENSION_NAME -> True)
  where VK_GOOGLE_EXTENSION_192_EXTENSION_NAME
          = _VK_GOOGLE_EXTENSION_192_EXTENSION_NAME

{-# INLINE _VK_GOOGLE_EXTENSION_192_EXTENSION_NAME #-}

_VK_GOOGLE_EXTENSION_192_EXTENSION_NAME :: CString
_VK_GOOGLE_EXTENSION_192_EXTENSION_NAME
  = Ptr "VK_GOOGLE_extension_192\NUL"##

{-# INLINE is_VK_GOOGLE_EXTENSION_192_EXTENSION_NAME #-}

is_VK_GOOGLE_EXTENSION_192_EXTENSION_NAME :: CString -> Bool
is_VK_GOOGLE_EXTENSION_192_EXTENSION_NAME
  = eqCStrings _VK_GOOGLE_EXTENSION_192_EXTENSION_NAME

type VK_GOOGLE_EXTENSION_192_EXTENSION_NAME =
     "VK_GOOGLE_extension_192"
