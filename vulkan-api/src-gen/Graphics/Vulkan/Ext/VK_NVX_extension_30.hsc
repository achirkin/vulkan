#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NVX_extension_30
       (-- * Vulkan extension: @VK_NVX_extension_30@
        -- |
        --
        -- supported: @disabled@
        --
        -- contact: @Jeff Juliano @jjuliano@
        --
        -- author: @NVX@
        --
        -- Extension number: @30@
        VK_NVX_EXTENSION_30_SPEC_VERSION,
        pattern VK_NVX_EXTENSION_30_SPEC_VERSION,
        VK_NVX_EXTENSION_30_EXTENSION_NAME,
        pattern VK_NVX_EXTENSION_30_EXTENSION_NAME)
       where
import           Foreign.C.String        (CString)
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Marshal

pattern VK_NVX_EXTENSION_30_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NVX_EXTENSION_30_SPEC_VERSION = 0

type VK_NVX_EXTENSION_30_SPEC_VERSION = 0

pattern VK_NVX_EXTENSION_30_EXTENSION_NAME :: CString

pattern VK_NVX_EXTENSION_30_EXTENSION_NAME <-
        (is_VK_NVX_EXTENSION_30_EXTENSION_NAME -> True)
  where VK_NVX_EXTENSION_30_EXTENSION_NAME
          = _VK_NVX_EXTENSION_30_EXTENSION_NAME

{-# INLINE _VK_NVX_EXTENSION_30_EXTENSION_NAME #-}

_VK_NVX_EXTENSION_30_EXTENSION_NAME :: CString
_VK_NVX_EXTENSION_30_EXTENSION_NAME
  = Ptr "VK_NVX_extension_30\NUL"##

{-# INLINE is_VK_NVX_EXTENSION_30_EXTENSION_NAME #-}

is_VK_NVX_EXTENSION_30_EXTENSION_NAME :: CString -> Bool
is_VK_NVX_EXTENSION_30_EXTENSION_NAME
  = eqCStrings _VK_NVX_EXTENSION_30_EXTENSION_NAME

type VK_NVX_EXTENSION_30_EXTENSION_NAME = "VK_NVX_extension_30"
