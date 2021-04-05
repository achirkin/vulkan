{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_GOOGLE_decorate_string
       (-- * Vulkan extension: @VK_GOOGLE_decorate_string@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Hai Nguyen @chaoticbob@
        --
        -- author: @GOOGLE@
        --
        -- type: @device@
        --
        -- Extension number: @225@
        VK_GOOGLE_DECORATE_STRING_SPEC_VERSION,
        pattern VK_GOOGLE_DECORATE_STRING_SPEC_VERSION,
        VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME,
        pattern VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME)
       where
import GHC.Ptr                 (Ptr (..))
import Graphics.Vulkan.Marshal

pattern VK_GOOGLE_DECORATE_STRING_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_GOOGLE_DECORATE_STRING_SPEC_VERSION = 1

type VK_GOOGLE_DECORATE_STRING_SPEC_VERSION = 1

pattern VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME :: CString

pattern VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME <-
        (is_VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME -> True)
  where
    VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME
      = _VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME

{-# INLINE _VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME #-}

_VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME :: CString
_VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME
  = Ptr "VK_GOOGLE_decorate_string\NUL"#

{-# INLINE is_VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME #-}

is_VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME :: CString -> Bool
is_VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME

type VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME =
     "VK_GOOGLE_decorate_string"
