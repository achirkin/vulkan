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
import           Foreign.C.String              (CString)
import           GHC.Ptr                       (Ptr (..))
import           Graphics.Vulkan.StructMembers

pattern VK_KHR_EXTENSION_169_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_EXTENSION_169_SPEC_VERSION = 0

type VK_KHR_EXTENSION_169_SPEC_VERSION = 0

pattern VK_KHR_EXTENSION_169_EXTENSION_NAME :: CString

pattern VK_KHR_EXTENSION_169_EXTENSION_NAME <-
        (is_VK_KHR_EXTENSION_169_EXTENSION_NAME -> True)
  where VK_KHR_EXTENSION_169_EXTENSION_NAME
          = _VK_KHR_EXTENSION_169_EXTENSION_NAME

_VK_KHR_EXTENSION_169_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_EXTENSION_169_EXTENSION_NAME #-}
_VK_KHR_EXTENSION_169_EXTENSION_NAME
  = Ptr "VK_KHR_extension_169\NUL"##

is_VK_KHR_EXTENSION_169_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_EXTENSION_169_EXTENSION_NAME #-}
is_VK_KHR_EXTENSION_169_EXTENSION_NAME
  = (_VK_KHR_EXTENSION_169_EXTENSION_NAME ==)

type VK_KHR_EXTENSION_169_EXTENSION_NAME = "VK_KHR_extension_169"
