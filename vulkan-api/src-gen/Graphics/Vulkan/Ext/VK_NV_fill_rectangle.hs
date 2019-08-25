{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NV_fill_rectangle
       (-- * Vulkan extension: @VK_NV_fill_rectangle@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jeffbolznv@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @154@
        VK_NV_FILL_RECTANGLE_SPEC_VERSION,
        pattern VK_NV_FILL_RECTANGLE_SPEC_VERSION,
        VK_NV_FILL_RECTANGLE_EXTENSION_NAME,
        pattern VK_NV_FILL_RECTANGLE_EXTENSION_NAME,
        pattern VK_POLYGON_MODE_FILL_RECTANGLE_NV)
       where
import           GHC.Ptr                                (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Enum.PolygonMode (VkPolygonMode (..))

pattern VK_NV_FILL_RECTANGLE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_FILL_RECTANGLE_SPEC_VERSION = 1

type VK_NV_FILL_RECTANGLE_SPEC_VERSION = 1

pattern VK_NV_FILL_RECTANGLE_EXTENSION_NAME :: CString

pattern VK_NV_FILL_RECTANGLE_EXTENSION_NAME <-
        (is_VK_NV_FILL_RECTANGLE_EXTENSION_NAME -> True)
  where
    VK_NV_FILL_RECTANGLE_EXTENSION_NAME
      = _VK_NV_FILL_RECTANGLE_EXTENSION_NAME

{-# INLINE _VK_NV_FILL_RECTANGLE_EXTENSION_NAME #-}

_VK_NV_FILL_RECTANGLE_EXTENSION_NAME :: CString
_VK_NV_FILL_RECTANGLE_EXTENSION_NAME
  = Ptr "VK_NV_fill_rectangle\NUL"#

{-# INLINE is_VK_NV_FILL_RECTANGLE_EXTENSION_NAME #-}

is_VK_NV_FILL_RECTANGLE_EXTENSION_NAME :: CString -> Bool
is_VK_NV_FILL_RECTANGLE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_NV_FILL_RECTANGLE_EXTENSION_NAME

type VK_NV_FILL_RECTANGLE_EXTENSION_NAME = "VK_NV_fill_rectangle"

pattern VK_POLYGON_MODE_FILL_RECTANGLE_NV :: VkPolygonMode

pattern VK_POLYGON_MODE_FILL_RECTANGLE_NV =
        VkPolygonMode 1000153000
