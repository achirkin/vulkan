{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_acquire_xlib_display
       (-- * Vulkan extension: @VK_EXT_acquire_xlib_display@
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
        -- Extension number: @90@
        --
        -- Required extensions: 'VK_EXT_direct_mode_display'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_XLIB_XRANDR_EXT@
        --

        -- ** Required extensions: 'VK_EXT_direct_mode_display'.
        vkAcquireXlibDisplayEXT, vkGetRandROutputDisplayEXT,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Handles,
        VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION,
        pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION,
        VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME,
        pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME)
       where
import           GHC.Ptr                             (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Include       (Display, RROutput)

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkAcquireXlibDisplayEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , Display* dpy
--   >     , VkDisplayKHR display
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkAcquireXlibDisplayEXT.html vkAcquireXlibDisplayEXT registry at www.khronos.org>
foreign import ccall unsafe "vkAcquireXlibDisplayEXT"
               vkAcquireXlibDisplayEXT ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr Display -- ^ dpy
                                               -> VkDisplayKHR -- ^ display
                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkGetRandROutputDisplayEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , Display* dpy
--   >     , RROutput rrOutput
--   >     , VkDisplayKHR* pDisplay
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetRandROutputDisplayEXT.html vkGetRandROutputDisplayEXT registry at www.khronos.org>
foreign import ccall unsafe "vkGetRandROutputDisplayEXT"
               vkGetRandROutputDisplayEXT ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Display -- ^ dpy
                             -> RROutput -- ^ rrOutput
                                         -> Ptr VkDisplayKHR -- ^ pDisplay
                                                             -> IO VkResult

pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION = 1

type VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION = 1

pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME :: CString

pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME <-
        (is_VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME -> True)
  where VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
          = _VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME

{-# INLINE _VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME #-}

_VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME :: CString
_VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
  = Ptr "VK_EXT_acquire_xlib_display\NUL"#

{-# INLINE is_VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME #-}

is_VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME

type VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME =
     "VK_EXT_acquire_xlib_display"
