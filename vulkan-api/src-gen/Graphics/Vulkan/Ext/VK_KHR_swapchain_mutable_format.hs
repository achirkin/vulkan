{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_swapchain_mutable_format
       (-- * Vulkan extension: @VK_KHR_swapchain_mutable_format@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Rakos @drakos-arm@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @201@
        --
        -- Required extensions: 'VK_KHR_swapchain', 'VK_KHR_maintenance2', 'VK_KHR_image_format_list'.
        --

        -- ** Required extensions: 'VK_KHR_swapchain', 'VK_KHR_maintenance2', 'VK_KHR_image_format_list'.
        VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION,
        pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION,
        VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME,
        pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME,
        pattern VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR)
       where
import GHC.Ptr                              (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Enum.Swapchain (VkSwapchainCreateBitmaskKHR (..))

pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION = 1

type VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION = 1

pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME :: CString

pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME <-
        (is_VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME -> True)
  where
    VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME
      = _VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME

{-# INLINE _VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME #-}

_VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME :: CString
_VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME
  = Ptr "VK_KHR_swapchain_mutable_format\NUL"#

{-# INLINE is_VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME #-}

is_VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME ::
                                                  CString -> Bool
is_VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME

type VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME =
     "VK_KHR_swapchain_mutable_format"

-- | bitpos = @2@
pattern VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR ::
        VkSwapchainCreateBitmaskKHR a

pattern VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR =
        VkSwapchainCreateBitmaskKHR 4
