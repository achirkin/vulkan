{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_IMG_format_pvrtc
       (-- * Vulkan extension: @VK_IMG_format_pvrtc@
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
        -- Extension number: @55@
        VK_IMG_FORMAT_PVRTC_SPEC_VERSION,
        pattern VK_IMG_FORMAT_PVRTC_SPEC_VERSION,
        VK_IMG_FORMAT_PVRTC_EXTENSION_NAME,
        pattern VK_IMG_FORMAT_PVRTC_EXTENSION_NAME,
        pattern VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG,
        pattern VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG,
        pattern VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG,
        pattern VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG,
        pattern VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG,
        pattern VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG,
        pattern VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG,
        pattern VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG)
       where
import           GHC.Ptr                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Enum.Format (VkFormat (..))

pattern VK_IMG_FORMAT_PVRTC_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_IMG_FORMAT_PVRTC_SPEC_VERSION = 1

type VK_IMG_FORMAT_PVRTC_SPEC_VERSION = 1

pattern VK_IMG_FORMAT_PVRTC_EXTENSION_NAME :: CString

pattern VK_IMG_FORMAT_PVRTC_EXTENSION_NAME <-
        (is_VK_IMG_FORMAT_PVRTC_EXTENSION_NAME -> True)
  where VK_IMG_FORMAT_PVRTC_EXTENSION_NAME
          = _VK_IMG_FORMAT_PVRTC_EXTENSION_NAME

{-# INLINE _VK_IMG_FORMAT_PVRTC_EXTENSION_NAME #-}

_VK_IMG_FORMAT_PVRTC_EXTENSION_NAME :: CString
_VK_IMG_FORMAT_PVRTC_EXTENSION_NAME
  = Ptr "VK_IMG_format_pvrtc\NUL"#

{-# INLINE is_VK_IMG_FORMAT_PVRTC_EXTENSION_NAME #-}

is_VK_IMG_FORMAT_PVRTC_EXTENSION_NAME :: CString -> Bool
is_VK_IMG_FORMAT_PVRTC_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_IMG_FORMAT_PVRTC_EXTENSION_NAME

type VK_IMG_FORMAT_PVRTC_EXTENSION_NAME = "VK_IMG_format_pvrtc"

pattern VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG :: VkFormat

pattern VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG = VkFormat 1000054000

pattern VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG :: VkFormat

pattern VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG = VkFormat 1000054001

pattern VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG :: VkFormat

pattern VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG = VkFormat 1000054002

pattern VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG :: VkFormat

pattern VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG = VkFormat 1000054003

pattern VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG :: VkFormat

pattern VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG = VkFormat 1000054004

pattern VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG :: VkFormat

pattern VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG = VkFormat 1000054005

pattern VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG :: VkFormat

pattern VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG = VkFormat 1000054006

pattern VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG :: VkFormat

pattern VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG = VkFormat 1000054007
