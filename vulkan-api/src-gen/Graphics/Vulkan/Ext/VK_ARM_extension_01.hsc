#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_ARM_extension_01
       (-- * Vulkan extension: @VK_ARM_extension_01@
        -- |
        --
        -- supported: @disabled@
        --
        -- contact: @Jan-Harald Fredriksen @janharald@
        --
        -- author: @ARM@
        --
        -- type: @device@
        --
        -- Extension number: @67@
        VK_ARM_EXTENSION_01_SPEC_VERSION,
        pattern VK_ARM_EXTENSION_01_SPEC_VERSION,
        VK_ARM_EXTENSION_01_EXTENSION_NAME,
        pattern VK_ARM_EXTENSION_01_EXTENSION_NAME)
       where
import           Foreign.C.String        (CString)
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Marshal

pattern VK_ARM_EXTENSION_01_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_ARM_EXTENSION_01_SPEC_VERSION = 0

type VK_ARM_EXTENSION_01_SPEC_VERSION = 0

pattern VK_ARM_EXTENSION_01_EXTENSION_NAME :: CString

pattern VK_ARM_EXTENSION_01_EXTENSION_NAME <-
        (is_VK_ARM_EXTENSION_01_EXTENSION_NAME -> True)
  where VK_ARM_EXTENSION_01_EXTENSION_NAME
          = _VK_ARM_EXTENSION_01_EXTENSION_NAME

{-# INLINE _VK_ARM_EXTENSION_01_EXTENSION_NAME #-}

_VK_ARM_EXTENSION_01_EXTENSION_NAME :: CString
_VK_ARM_EXTENSION_01_EXTENSION_NAME
  = Ptr "VK_ARM_extension_01\NUL"##

{-# INLINE is_VK_ARM_EXTENSION_01_EXTENSION_NAME #-}

is_VK_ARM_EXTENSION_01_EXTENSION_NAME :: CString -> Bool
is_VK_ARM_EXTENSION_01_EXTENSION_NAME
  = eqCStrings _VK_ARM_EXTENSION_01_EXTENSION_NAME

type VK_ARM_EXTENSION_01_EXTENSION_NAME = "VK_ARM_extension_01"
