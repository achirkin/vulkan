#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_AMD_negative_viewport_height
       (-- * Vulkan extension: @VK_AMD_negative_viewport_height@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Matthaeus G. Chajdas @anteru@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @36@
        VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION,
        pattern VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION,
        VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME,
        pattern VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME)
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

pattern VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION = 1

type VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION = 1

pattern VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME :: CString

pattern VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME <-
        (is_VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME -> True)
  where VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME
          = _VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME

_VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME :: CString

{-# INLINE _VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME #-}
_VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME
  = Ptr "VK_AMD_negative_viewport_height\NUL"##

is_VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME ::
                                                  CString -> Bool

{-# INLINE is_VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME #-}
is_VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME
  = (_VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME ==)

type VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME =
     "VK_AMD_negative_viewport_height"
