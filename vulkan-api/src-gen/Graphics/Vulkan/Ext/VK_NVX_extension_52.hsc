#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NVX_extension_52
       (-- * Vulkan extension: @VK_NVX_extension_52@
        -- |
        --
        -- supported: @disabled@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @NVX@
        --
        -- Extension number: @52@
        VK_NVX_EXTENSION_52_SPEC_VERSION,
        pattern VK_NVX_EXTENSION_52_SPEC_VERSION,
        VK_NVX_EXTENSION_52_EXTENSION_NAME,
        pattern VK_NVX_EXTENSION_52_EXTENSION_NAME)
       where
import           Foreign.C.String              (CString)
import           GHC.Ptr                       (Ptr (..))
import           Graphics.Vulkan.StructMembers

pattern VK_NVX_EXTENSION_52_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NVX_EXTENSION_52_SPEC_VERSION = 0

type VK_NVX_EXTENSION_52_SPEC_VERSION = 0

pattern VK_NVX_EXTENSION_52_EXTENSION_NAME :: CString

pattern VK_NVX_EXTENSION_52_EXTENSION_NAME <-
        (is_VK_NVX_EXTENSION_52_EXTENSION_NAME -> True)
  where VK_NVX_EXTENSION_52_EXTENSION_NAME
          = _VK_NVX_EXTENSION_52_EXTENSION_NAME

_VK_NVX_EXTENSION_52_EXTENSION_NAME :: CString

{-# INLINE _VK_NVX_EXTENSION_52_EXTENSION_NAME #-}
_VK_NVX_EXTENSION_52_EXTENSION_NAME
  = Ptr "VK_NVX_extension_52\NUL"##

is_VK_NVX_EXTENSION_52_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_NVX_EXTENSION_52_EXTENSION_NAME #-}
is_VK_NVX_EXTENSION_52_EXTENSION_NAME
  = (_VK_NVX_EXTENSION_52_EXTENSION_NAME ==)

type VK_NVX_EXTENSION_52_EXTENSION_NAME = "VK_NVX_extension_52"
