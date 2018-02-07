#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_extension_129
       (-- * Vulkan extension: @VK_EXT_extension_129@
        -- |
        --
        -- supported: @disabled@
        --
        -- contact: @Mark Young @MarkY_LunarG@
        --
        -- author: @LUNARG@
        --
        -- Extension number: @129@
        VK_KHR_EXTENSION_129_SPEC_VERSION,
        pattern VK_KHR_EXTENSION_129_SPEC_VERSION,
        VK_KHR_EXTENSION_129_EXTENSION_NAME,
        pattern VK_KHR_EXTENSION_129_EXTENSION_NAME)
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

pattern VK_KHR_EXTENSION_129_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_EXTENSION_129_SPEC_VERSION = 0

type VK_KHR_EXTENSION_129_SPEC_VERSION = 0

pattern VK_KHR_EXTENSION_129_EXTENSION_NAME :: CString

pattern VK_KHR_EXTENSION_129_EXTENSION_NAME <-
        (is_VK_KHR_EXTENSION_129_EXTENSION_NAME -> True)
  where VK_KHR_EXTENSION_129_EXTENSION_NAME
          = _VK_KHR_EXTENSION_129_EXTENSION_NAME

_VK_KHR_EXTENSION_129_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_EXTENSION_129_EXTENSION_NAME #-}
_VK_KHR_EXTENSION_129_EXTENSION_NAME
  = Ptr "VK_EXT_extension_129\NUL"##

is_VK_KHR_EXTENSION_129_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_EXTENSION_129_EXTENSION_NAME #-}
is_VK_KHR_EXTENSION_129_EXTENSION_NAME
  = (_VK_KHR_EXTENSION_129_EXTENSION_NAME ==)

type VK_KHR_EXTENSION_129_EXTENSION_NAME = "VK_EXT_extension_129"
