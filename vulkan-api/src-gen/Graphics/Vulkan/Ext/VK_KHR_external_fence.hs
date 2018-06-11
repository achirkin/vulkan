{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_external_fence
       (-- * Vulkan extension: @VK_KHR_external_fence@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @critsec@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @114@
        --
        -- Required extensions: 'VK_KHR_external_fence_capabilities'.
        --

        -- ** Required extensions: 'VK_KHR_external_fence_capabilities'.
        module Graphics.Vulkan.Types.Struct.Export,
        module Graphics.Vulkan.Types.Enum.Fence,
        module Graphics.Vulkan.Types.Bitmasks,
        VK_KHR_EXTERNAL_FENCE_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION,
        VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR,
        pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR)
       where
import           GHC.Ptr                             (Ptr (..))
import           Graphics.Vulkan.Core_1_1            (pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.Fence
import           Graphics.Vulkan.Types.Struct.Export

pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_FENCE_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_FENCE_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME
  = Ptr "VK_KHR_external_fence\NUL"#

{-# INLINE is_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME

type VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME = "VK_KHR_external_fence"

pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR =
        VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO

pattern VK_FENCE_IMPORT_TEMPORARY_BIT_KHR =
        VK_FENCE_IMPORT_TEMPORARY_BIT
