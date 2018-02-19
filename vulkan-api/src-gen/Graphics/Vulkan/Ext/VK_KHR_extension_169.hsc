#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_extension_169
       (-- * Vulkan extension: @VK_KHR_extension_169@
        -- |
        --
        -- supported: @disabled@
        --
        -- contact: @Jeff Bolz @jbolz@
        --
        -- author: @KHR@
        --
        -- Extension number: @169@
        VK_KHR_EXTENSION_169_SPEC_VERSION,
        pattern VK_KHR_EXTENSION_169_SPEC_VERSION,
        VK_KHR_EXTENSION_169_EXTENSION_NAME,
        pattern VK_KHR_EXTENSION_169_EXTENSION_NAME)
       where
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Marshal

pattern VK_KHR_EXTENSION_169_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_EXTENSION_169_SPEC_VERSION = 0

type VK_KHR_EXTENSION_169_SPEC_VERSION = 0

pattern VK_KHR_EXTENSION_169_EXTENSION_NAME :: CString

pattern VK_KHR_EXTENSION_169_EXTENSION_NAME <-
        (is_VK_KHR_EXTENSION_169_EXTENSION_NAME -> True)
  where VK_KHR_EXTENSION_169_EXTENSION_NAME
          = _VK_KHR_EXTENSION_169_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTENSION_169_EXTENSION_NAME #-}

_VK_KHR_EXTENSION_169_EXTENSION_NAME :: CString
_VK_KHR_EXTENSION_169_EXTENSION_NAME
  = Ptr "VK_KHR_extension_169\NUL"##

{-# INLINE is_VK_KHR_EXTENSION_169_EXTENSION_NAME #-}

is_VK_KHR_EXTENSION_169_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_EXTENSION_169_EXTENSION_NAME
  = eqCStrings _VK_KHR_EXTENSION_169_EXTENSION_NAME

type VK_KHR_EXTENSION_169_EXTENSION_NAME = "VK_KHR_extension_169"
