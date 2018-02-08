#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_AMD_extension_188
       (-- * Vulkan extension: @VK_AMD_extension_188@
        -- |
        --
        -- supported: @disabled@
        --
        -- contact: @Daniel Rakos @aqnuep@
        --
        -- author: @AMD@
        --
        -- Extension number: @188@
        VK_KHR_EXTENSION_188_SPEC_VERSION,
        pattern VK_KHR_EXTENSION_188_SPEC_VERSION,
        VK_KHR_EXTENSION_188_EXTENSION_NAME,
        pattern VK_KHR_EXTENSION_188_EXTENSION_NAME)
       where
import           Foreign.C.String (CString)
import           GHC.Ptr          (Ptr (..))

pattern VK_KHR_EXTENSION_188_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_EXTENSION_188_SPEC_VERSION = 0

type VK_KHR_EXTENSION_188_SPEC_VERSION = 0

pattern VK_KHR_EXTENSION_188_EXTENSION_NAME :: CString

pattern VK_KHR_EXTENSION_188_EXTENSION_NAME <-
        (is_VK_KHR_EXTENSION_188_EXTENSION_NAME -> True)
  where VK_KHR_EXTENSION_188_EXTENSION_NAME
          = _VK_KHR_EXTENSION_188_EXTENSION_NAME

_VK_KHR_EXTENSION_188_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_EXTENSION_188_EXTENSION_NAME #-}
_VK_KHR_EXTENSION_188_EXTENSION_NAME
  = Ptr "VK_AMD_extension_188\NUL"##

is_VK_KHR_EXTENSION_188_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_EXTENSION_188_EXTENSION_NAME #-}
is_VK_KHR_EXTENSION_188_EXTENSION_NAME
  = (_VK_KHR_EXTENSION_188_EXTENSION_NAME ==)

type VK_KHR_EXTENSION_188_EXTENSION_NAME = "VK_AMD_extension_188"
