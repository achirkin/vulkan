{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_GGP_frame_token
       (-- * Vulkan extension: @VK_GGP_frame_token@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jean-Francois Roy @jfroy@
        --
        -- author: @GGP@
        --
        -- type: @device@
        --
        -- platform: @ggp@
        --
        -- Extension number: @192@
        --
        -- Required extensions: 'VK_KHR_swapchain', 'VK_GGP_stream_descriptor_surface'.
        --

        -- ** Required extensions: 'VK_KHR_swapchain', 'VK_GGP_stream_descriptor_surface'.
        VkPresentFrameTokenGGP, VkPresentInfoKHR, VkResult(..),
        VkStructureType(..), -- > #include "vk_platform.h"
                             VK_GGP_FRAME_TOKEN_SPEC_VERSION,
        pattern VK_GGP_FRAME_TOKEN_SPEC_VERSION,
        VK_GGP_FRAME_TOKEN_EXTENSION_NAME,
        pattern VK_GGP_FRAME_TOKEN_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP)
       where
import GHC.Ptr                                  (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Struct.PlatformGgp (VkPresentFrameTokenGGP)
import Graphics.Vulkan.Types.Struct.Present     (VkPresentInfoKHR)

pattern VK_GGP_FRAME_TOKEN_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_GGP_FRAME_TOKEN_SPEC_VERSION = 1

type VK_GGP_FRAME_TOKEN_SPEC_VERSION = 1

pattern VK_GGP_FRAME_TOKEN_EXTENSION_NAME :: CString

pattern VK_GGP_FRAME_TOKEN_EXTENSION_NAME <-
        (is_VK_GGP_FRAME_TOKEN_EXTENSION_NAME -> True)
  where
    VK_GGP_FRAME_TOKEN_EXTENSION_NAME
      = _VK_GGP_FRAME_TOKEN_EXTENSION_NAME

{-# INLINE _VK_GGP_FRAME_TOKEN_EXTENSION_NAME #-}

_VK_GGP_FRAME_TOKEN_EXTENSION_NAME :: CString
_VK_GGP_FRAME_TOKEN_EXTENSION_NAME = Ptr "VK_GGP_frame_token\NUL"#

{-# INLINE is_VK_GGP_FRAME_TOKEN_EXTENSION_NAME #-}

is_VK_GGP_FRAME_TOKEN_EXTENSION_NAME :: CString -> Bool
is_VK_GGP_FRAME_TOKEN_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_GGP_FRAME_TOKEN_EXTENSION_NAME

type VK_GGP_FRAME_TOKEN_EXTENSION_NAME = "VK_GGP_frame_token"

pattern VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP =
        VkStructureType 1000191000
