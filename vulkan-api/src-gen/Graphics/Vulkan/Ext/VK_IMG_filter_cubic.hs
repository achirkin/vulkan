{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_IMG_filter_cubic
       (-- * Vulkan extension: @VK_IMG_filter_cubic@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Tobias Hector @tobski@
        --
        -- author: @IMG@
        --
        -- type: @device@
        --
        -- Extension number: @16@
        VK_IMG_FILTER_CUBIC_SPEC_VERSION,
        pattern VK_IMG_FILTER_CUBIC_SPEC_VERSION,
        VK_IMG_FILTER_CUBIC_EXTENSION_NAME,
        pattern VK_IMG_FILTER_CUBIC_EXTENSION_NAME,
        pattern VK_FILTER_CUBIC_IMG,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG)
       where
import           GHC.Ptr                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Enum.Filter (VkFilter (..))
import           Graphics.Vulkan.Types.Enum.Format (VkFormatFeatureBitmask (..),
                                                    VkFormatFeatureFlagBits)

pattern VK_IMG_FILTER_CUBIC_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_IMG_FILTER_CUBIC_SPEC_VERSION = 1

type VK_IMG_FILTER_CUBIC_SPEC_VERSION = 1

pattern VK_IMG_FILTER_CUBIC_EXTENSION_NAME :: CString

pattern VK_IMG_FILTER_CUBIC_EXTENSION_NAME <-
        (is_VK_IMG_FILTER_CUBIC_EXTENSION_NAME -> True)
  where VK_IMG_FILTER_CUBIC_EXTENSION_NAME
          = _VK_IMG_FILTER_CUBIC_EXTENSION_NAME

{-# INLINE _VK_IMG_FILTER_CUBIC_EXTENSION_NAME #-}

_VK_IMG_FILTER_CUBIC_EXTENSION_NAME :: CString
_VK_IMG_FILTER_CUBIC_EXTENSION_NAME
  = Ptr "VK_IMG_filter_cubic\NUL"#

{-# INLINE is_VK_IMG_FILTER_CUBIC_EXTENSION_NAME #-}

is_VK_IMG_FILTER_CUBIC_EXTENSION_NAME :: CString -> Bool
is_VK_IMG_FILTER_CUBIC_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_IMG_FILTER_CUBIC_EXTENSION_NAME

type VK_IMG_FILTER_CUBIC_EXTENSION_NAME = "VK_IMG_filter_cubic"

pattern VK_FILTER_CUBIC_IMG :: VkFilter

pattern VK_FILTER_CUBIC_IMG = VkFilter 1000015000

-- | Format can be filtered with VK_FILTER_CUBIC_IMG when being sampled
--
--   bitpos = @13@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG =
        VkFormatFeatureFlagBits 8192
