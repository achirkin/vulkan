#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_direct_mode_display
       (-- * Vulkan extension: @VK_EXT_direct_mode_display@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @NV@
        --
        -- type: @instance@
        --
        -- Extension number: @89@
        --
        -- Required extensions: 'VK_KHR_display'.
        --

        -- ** Required extensions: 'VK_KHR_display'.
        vkReleaseDisplayEXT, VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION,
        pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION,
        VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME,
        pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME)
       where
import           GHC.Ptr                 (Ptr (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkReleaseDisplayEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkReleaseDisplayEXT.html vkReleaseDisplayEXT registry at www.khronos.org>
foreign import ccall unsafe "vkReleaseDisplayEXT"
               vkReleaseDisplayEXT ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkDisplayKHR -- ^ display
                                                -> IO VkResult

pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION = 1

type VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION = 1

pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME :: CString

pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME <-
        (is_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME -> True)
  where VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
          = _VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME

{-# INLINE _VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME #-}

_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME :: CString
_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
  = Ptr "VK_EXT_direct_mode_display\NUL"##

{-# INLINE is_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME #-}

is_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
  = eqCStrings _VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME

type VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME =
     "VK_EXT_direct_mode_display"
