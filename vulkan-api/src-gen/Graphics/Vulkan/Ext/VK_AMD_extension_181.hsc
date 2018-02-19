#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_AMD_extension_181
       (-- * Vulkan extension: @VK_AMD_extension_181@
        -- |
        --
        -- supported: @disabled@
        --
        -- contact: @Daniel Rakos @aqnuep@
        --
        -- author: @AMD@
        --
        -- Extension number: @181@
        VK_KHR_EXTENSION_181_SPEC_VERSION,
        pattern VK_KHR_EXTENSION_181_SPEC_VERSION,
        VK_KHR_EXTENSION_181_EXTENSION_NAME,
        pattern VK_KHR_EXTENSION_181_EXTENSION_NAME)
       where
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Marshal

pattern VK_KHR_EXTENSION_181_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_EXTENSION_181_SPEC_VERSION = 0

type VK_KHR_EXTENSION_181_SPEC_VERSION = 0

pattern VK_KHR_EXTENSION_181_EXTENSION_NAME :: CString

pattern VK_KHR_EXTENSION_181_EXTENSION_NAME <-
        (is_VK_KHR_EXTENSION_181_EXTENSION_NAME -> True)
  where VK_KHR_EXTENSION_181_EXTENSION_NAME
          = _VK_KHR_EXTENSION_181_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTENSION_181_EXTENSION_NAME #-}

_VK_KHR_EXTENSION_181_EXTENSION_NAME :: CString
_VK_KHR_EXTENSION_181_EXTENSION_NAME
  = Ptr "VK_AMD_extension_181\NUL"##

{-# INLINE is_VK_KHR_EXTENSION_181_EXTENSION_NAME #-}

is_VK_KHR_EXTENSION_181_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_EXTENSION_181_EXTENSION_NAME
  = eqCStrings _VK_KHR_EXTENSION_181_EXTENSION_NAME

type VK_KHR_EXTENSION_181_EXTENSION_NAME = "VK_AMD_extension_181"
