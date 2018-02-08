#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_extension_170
       (-- * Vulkan extension: @VK_EXT_extension_170@
        -- |
        --
        -- supported: @disabled@
        --
        -- contact: @Piers Daniell @pdaniell@
        --
        -- author: @NV@
        --
        -- Extension number: @170@
        VK_EXT_EXTENSION_170_SPEC_VERSION,
        pattern VK_EXT_EXTENSION_170_SPEC_VERSION,
        VK_EXT_EXTENSION_170_EXTENSION_NAME,
        pattern VK_EXT_EXTENSION_170_EXTENSION_NAME)
       where
import           Foreign.C.String (CString)
import           GHC.Ptr          (Ptr (..))

pattern VK_EXT_EXTENSION_170_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_EXTENSION_170_SPEC_VERSION = 0

type VK_EXT_EXTENSION_170_SPEC_VERSION = 0

pattern VK_EXT_EXTENSION_170_EXTENSION_NAME :: CString

pattern VK_EXT_EXTENSION_170_EXTENSION_NAME <-
        (is_VK_EXT_EXTENSION_170_EXTENSION_NAME -> True)
  where VK_EXT_EXTENSION_170_EXTENSION_NAME
          = _VK_EXT_EXTENSION_170_EXTENSION_NAME

_VK_EXT_EXTENSION_170_EXTENSION_NAME :: CString

{-# INLINE _VK_EXT_EXTENSION_170_EXTENSION_NAME #-}
_VK_EXT_EXTENSION_170_EXTENSION_NAME
  = Ptr "VK_EXT_extension_170\NUL"##

is_VK_EXT_EXTENSION_170_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_EXT_EXTENSION_170_EXTENSION_NAME #-}
is_VK_EXT_EXTENSION_170_EXTENSION_NAME
  = (_VK_EXT_EXTENSION_170_EXTENSION_NAME ==)

type VK_EXT_EXTENSION_170_EXTENSION_NAME = "VK_EXT_extension_170"
