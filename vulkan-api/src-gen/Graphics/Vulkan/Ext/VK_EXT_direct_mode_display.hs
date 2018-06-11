{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
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
        VkReleaseDisplayEXT, pattern VkReleaseDisplayEXT,
        HS_vkReleaseDisplayEXT, PFN_vkReleaseDisplayEXT,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Handles,
        VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION,
        pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION,
        VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME,
        pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME)
       where
import           GHC.Ptr                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc      (VulkanProc (..))
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Handles

pattern VkReleaseDisplayEXT :: CString

pattern VkReleaseDisplayEXT <- (is_VkReleaseDisplayEXT -> True)
  where VkReleaseDisplayEXT = _VkReleaseDisplayEXT

{-# INLINE _VkReleaseDisplayEXT #-}

_VkReleaseDisplayEXT :: CString
_VkReleaseDisplayEXT = Ptr "vkReleaseDisplayEXT\NUL"#

{-# INLINE is_VkReleaseDisplayEXT #-}

is_VkReleaseDisplayEXT :: CString -> Bool
is_VkReleaseDisplayEXT = (EQ ==) . cmpCStrings _VkReleaseDisplayEXT

type VkReleaseDisplayEXT = "vkReleaseDisplayEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkReleaseDisplayEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkReleaseDisplayEXT vkReleaseDisplayEXT registry at www.khronos.org>
type HS_vkReleaseDisplayEXT =
     VkPhysicalDevice -- ^ physicalDevice
                      -> VkDisplayKHR -- ^ display
                                      -> IO VkResult

type PFN_vkReleaseDisplayEXT = FunPtr HS_vkReleaseDisplayEXT

foreign import ccall unsafe "dynamic"
               unwrapVkReleaseDisplayEXTUnsafe ::
               PFN_vkReleaseDisplayEXT -> HS_vkReleaseDisplayEXT

foreign import ccall safe "dynamic" unwrapVkReleaseDisplayEXTSafe
               :: PFN_vkReleaseDisplayEXT -> HS_vkReleaseDisplayEXT

instance VulkanProc "vkReleaseDisplayEXT" where
        type VkProcType "vkReleaseDisplayEXT" = HS_vkReleaseDisplayEXT
        vkProcSymbol = _VkReleaseDisplayEXT

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkReleaseDisplayEXTUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkReleaseDisplayEXTSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
  = Ptr "VK_EXT_direct_mode_display\NUL"#

{-# INLINE is_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME #-}

is_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME

type VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME =
     "VK_EXT_direct_mode_display"
