{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_relaxed_block_layout
       (-- * Vulkan extension: @VK_KHR_relaxed_block_layout@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @John Kessenich @johnkslang@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @145@
        VK_KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION,
        pattern VK_KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION,
        VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME,
        pattern VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME)
       where
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Marshal

pattern VK_KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION = 1

type VK_KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION = 1

pattern VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME :: CString

pattern VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME <-
        (is_VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME -> True)
  where VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME
          = _VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME

{-# INLINE _VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME #-}

_VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME :: CString
_VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME
  = Ptr "VK_KHR_relaxed_block_layout\NUL"#

{-# INLINE is_VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME #-}

is_VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME

type VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME =
     "VK_KHR_relaxed_block_layout"
