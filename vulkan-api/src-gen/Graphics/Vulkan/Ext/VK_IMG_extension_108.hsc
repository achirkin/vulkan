#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_IMG_extension_108
       (-- * Vulkan extension: @VK_IMG_extension_108@
        -- |
        --
        -- supported: @disabled@
        --
        -- contact: @Michael Worcester @michaelworcester@
        --
        -- author: @IMG@
        --
        -- Extension number: @108@
        VK_IMG_EXTENSION_108_SPEC_VERSION,
        pattern VK_IMG_EXTENSION_108_SPEC_VERSION,
        VK_IMG_EXTENSION_108_EXTENSION_NAME,
        pattern VK_IMG_EXTENSION_108_EXTENSION_NAME)
       where
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Marshal

pattern VK_IMG_EXTENSION_108_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_IMG_EXTENSION_108_SPEC_VERSION = 0

type VK_IMG_EXTENSION_108_SPEC_VERSION = 0

pattern VK_IMG_EXTENSION_108_EXTENSION_NAME :: CString

pattern VK_IMG_EXTENSION_108_EXTENSION_NAME <-
        (is_VK_IMG_EXTENSION_108_EXTENSION_NAME -> True)
  where VK_IMG_EXTENSION_108_EXTENSION_NAME
          = _VK_IMG_EXTENSION_108_EXTENSION_NAME

{-# INLINE _VK_IMG_EXTENSION_108_EXTENSION_NAME #-}

_VK_IMG_EXTENSION_108_EXTENSION_NAME :: CString
_VK_IMG_EXTENSION_108_EXTENSION_NAME
  = Ptr "VK_IMG_extension_108\NUL"##

{-# INLINE is_VK_IMG_EXTENSION_108_EXTENSION_NAME #-}

is_VK_IMG_EXTENSION_108_EXTENSION_NAME :: CString -> Bool
is_VK_IMG_EXTENSION_108_EXTENSION_NAME
  = eqCStrings _VK_IMG_EXTENSION_108_EXTENSION_NAME

type VK_IMG_EXTENSION_108_EXTENSION_NAME = "VK_IMG_extension_108"
