{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_swapchain_colorspace
       (-- * Vulkan extension: @VK_EXT_swapchain_colorspace@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Courtney Goeltzenleuchter @courtney-g@
        --
        -- author: @GOOGLE@
        --
        -- type: @instance@
        --
        -- Extension number: @105@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VK_EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION,
        pattern VK_EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION,
        VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME,
        pattern VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME,
        pattern VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT,
        pattern VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT,
        pattern VK_COLOR_SPACE_DCI_P3_LINEAR_EXT,
        pattern VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT,
        pattern VK_COLOR_SPACE_BT709_LINEAR_EXT,
        pattern VK_COLOR_SPACE_BT709_NONLINEAR_EXT,
        pattern VK_COLOR_SPACE_BT2020_LINEAR_EXT,
        pattern VK_COLOR_SPACE_HDR10_ST2084_EXT,
        pattern VK_COLOR_SPACE_DOLBYVISION_EXT,
        pattern VK_COLOR_SPACE_HDR10_HLG_EXT,
        pattern VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT,
        pattern VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT,
        pattern VK_COLOR_SPACE_PASS_THROUGH_EXT,
        pattern VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT)
       where
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Enum.Color (VkColorSpaceKHR (..))

pattern VK_EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION = 3

type VK_EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION = 3

pattern VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME :: CString

pattern VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME <-
        (is_VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME -> True)
  where VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME
          = _VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME

{-# INLINE _VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME #-}

_VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME :: CString
_VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME
  = Ptr "VK_EXT_swapchain_colorspace\NUL"#

{-# INLINE is_VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME #-}

is_VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME

type VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME =
     "VK_EXT_swapchain_colorspace"

pattern VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT =
        VkColorSpaceKHR 1000104001

pattern VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT =
        VkColorSpaceKHR 1000104002

pattern VK_COLOR_SPACE_DCI_P3_LINEAR_EXT :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_DCI_P3_LINEAR_EXT =
        VkColorSpaceKHR 1000104003

pattern VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT =
        VkColorSpaceKHR 1000104004

pattern VK_COLOR_SPACE_BT709_LINEAR_EXT :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_BT709_LINEAR_EXT =
        VkColorSpaceKHR 1000104005

pattern VK_COLOR_SPACE_BT709_NONLINEAR_EXT :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_BT709_NONLINEAR_EXT =
        VkColorSpaceKHR 1000104006

pattern VK_COLOR_SPACE_BT2020_LINEAR_EXT :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_BT2020_LINEAR_EXT =
        VkColorSpaceKHR 1000104007

pattern VK_COLOR_SPACE_HDR10_ST2084_EXT :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_HDR10_ST2084_EXT =
        VkColorSpaceKHR 1000104008

pattern VK_COLOR_SPACE_DOLBYVISION_EXT :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_DOLBYVISION_EXT = VkColorSpaceKHR 1000104009

pattern VK_COLOR_SPACE_HDR10_HLG_EXT :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_HDR10_HLG_EXT = VkColorSpaceKHR 1000104010

pattern VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT =
        VkColorSpaceKHR 1000104011

pattern VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT =
        VkColorSpaceKHR 1000104012

pattern VK_COLOR_SPACE_PASS_THROUGH_EXT :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_PASS_THROUGH_EXT =
        VkColorSpaceKHR 1000104013

pattern VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT ::
        VkColorSpaceKHR

pattern VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT =
        VkColorSpaceKHR 1000104014
