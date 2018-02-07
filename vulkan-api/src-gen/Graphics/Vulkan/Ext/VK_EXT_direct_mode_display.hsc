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
import           Foreign.C.String              (CString)
import           GHC.Ptr                       (Ptr (..))
import           Graphics.Vulkan.Common        (VkDisplayKHR, VkPhysicalDevice,
                                                VkResult)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.StructMembers

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkReleaseDisplayEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkReleaseDisplayEXT.html vkReleaseDisplayEXT registry at www.khronos.org>
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

_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME :: CString

{-# INLINE _VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME #-}
_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
  = Ptr "VK_EXT_direct_mode_display\NUL"##

is_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME #-}
is_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
  = (_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME ==)

type VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME =
     "VK_EXT_direct_mode_display"
