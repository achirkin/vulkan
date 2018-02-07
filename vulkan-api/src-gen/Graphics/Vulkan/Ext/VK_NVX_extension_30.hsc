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

pattern VK_NVX_EXTENSION_30_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NVX_EXTENSION_30_SPEC_VERSION = 0

type VK_NVX_EXTENSION_30_SPEC_VERSION = 0

pattern VK_NVX_EXTENSION_30_EXTENSION_NAME :: CString

pattern VK_NVX_EXTENSION_30_EXTENSION_NAME <-
        (is_VK_NVX_EXTENSION_30_EXTENSION_NAME -> True)
  where VK_NVX_EXTENSION_30_EXTENSION_NAME
          = _VK_NVX_EXTENSION_30_EXTENSION_NAME

_VK_NVX_EXTENSION_30_EXTENSION_NAME :: CString

{-# INLINE _VK_NVX_EXTENSION_30_EXTENSION_NAME #-}
_VK_NVX_EXTENSION_30_EXTENSION_NAME
  = Ptr "VK_NVX_extension_30\NUL"##

is_VK_NVX_EXTENSION_30_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_NVX_EXTENSION_30_EXTENSION_NAME #-}
is_VK_NVX_EXTENSION_30_EXTENSION_NAME
  = (_VK_NVX_EXTENSION_30_EXTENSION_NAME ==)

type VK_NVX_EXTENSION_30_EXTENSION_NAME = "VK_NVX_extension_30"
