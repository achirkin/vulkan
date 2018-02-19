#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NV_extension_101
       (-- * Vulkan extension: @VK_NV_extension_101@
        -- |
        --
        -- supported: @disabled@
        --
        -- contact: @Daniel Koch @dgkoch@
        --
        -- author: @NV@
        --
        -- Extension number: @101@
        VK_NV_EXTENSION_101_SPEC_VERSION,
        pattern VK_NV_EXTENSION_101_SPEC_VERSION,
        VK_NV_EXTENSION_101_EXTENSION_NAME,
        pattern VK_NV_EXTENSION_101_EXTENSION_NAME)
       where
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Marshal

pattern VK_NV_EXTENSION_101_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_EXTENSION_101_SPEC_VERSION = 0

type VK_NV_EXTENSION_101_SPEC_VERSION = 0

pattern VK_NV_EXTENSION_101_EXTENSION_NAME :: CString

pattern VK_NV_EXTENSION_101_EXTENSION_NAME <-
        (is_VK_NV_EXTENSION_101_EXTENSION_NAME -> True)
  where VK_NV_EXTENSION_101_EXTENSION_NAME
          = _VK_NV_EXTENSION_101_EXTENSION_NAME

{-# INLINE _VK_NV_EXTENSION_101_EXTENSION_NAME #-}

_VK_NV_EXTENSION_101_EXTENSION_NAME :: CString
_VK_NV_EXTENSION_101_EXTENSION_NAME
  = Ptr "VK_NV_extension_101\NUL"##

{-# INLINE is_VK_NV_EXTENSION_101_EXTENSION_NAME #-}

is_VK_NV_EXTENSION_101_EXTENSION_NAME :: CString -> Bool
is_VK_NV_EXTENSION_101_EXTENSION_NAME
  = eqCStrings _VK_NV_EXTENSION_101_EXTENSION_NAME

type VK_NV_EXTENSION_101_EXTENSION_NAME = "VK_NV_extension_101"
