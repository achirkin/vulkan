#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_MVK_moltenvk
       (-- * Vulkan extension: @VK_MVK_moltenvk@
        -- |
        --
        -- supported: @disabled@
        --
        -- contact: @Bill Hollings @billhollings@
        --
        -- author: @MVK@
        --
        -- type: @instance@
        --
        -- Extension number: @125@
        VK_MVK_MOLTENVK_SPEC_VERSION,
        pattern VK_MVK_MOLTENVK_SPEC_VERSION,
        VK_MVK_MOLTENVK_EXTENSION_NAME,
        pattern VK_MVK_MOLTENVK_EXTENSION_NAME)
       where
import           Foreign.C.String              (CString)
import           GHC.Ptr                       (Ptr (..))
import           Graphics.Vulkan.StructMembers

pattern VK_MVK_MOLTENVK_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_MVK_MOLTENVK_SPEC_VERSION = 0

type VK_MVK_MOLTENVK_SPEC_VERSION = 0

pattern VK_MVK_MOLTENVK_EXTENSION_NAME :: CString

pattern VK_MVK_MOLTENVK_EXTENSION_NAME <-
        (is_VK_MVK_MOLTENVK_EXTENSION_NAME -> True)
  where VK_MVK_MOLTENVK_EXTENSION_NAME
          = _VK_MVK_MOLTENVK_EXTENSION_NAME

_VK_MVK_MOLTENVK_EXTENSION_NAME :: CString

{-# INLINE _VK_MVK_MOLTENVK_EXTENSION_NAME #-}
_VK_MVK_MOLTENVK_EXTENSION_NAME = Ptr "VK_MVK_moltenvk\NUL"##

is_VK_MVK_MOLTENVK_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_MVK_MOLTENVK_EXTENSION_NAME #-}
is_VK_MVK_MOLTENVK_EXTENSION_NAME
  = (_VK_MVK_MOLTENVK_EXTENSION_NAME ==)

type VK_MVK_MOLTENVK_EXTENSION_NAME = "VK_MVK_moltenvk"
