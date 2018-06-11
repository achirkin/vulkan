{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_storage_buffer_storage_class
       (-- * Vulkan extension: @VK_KHR_storage_buffer_storage_class@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Alexander Galazin @alegal-arm@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @132@
        VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION,
        pattern VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION,
        VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME,
        pattern VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME)
       where
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Marshal

pattern VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION = 1

type VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_SPEC_VERSION = 1

pattern VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME ::
        CString

pattern VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME <-
        (is_VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME -> True)
  where VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
          = _VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME

{-# INLINE _VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME #-}

_VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME :: CString
_VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
  = Ptr "VK_KHR_storage_buffer_storage_class\NUL"#

{-# INLINE is_VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
           #-}

is_VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME ::
                                                      CString -> Bool
is_VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME

type VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME =
     "VK_KHR_storage_buffer_storage_class"
