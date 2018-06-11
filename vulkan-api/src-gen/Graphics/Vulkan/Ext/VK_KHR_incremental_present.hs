{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_incremental_present
       (-- * Vulkan extension: @VK_KHR_incremental_present@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Ian Elliott @ianelliottus@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @85@
        --
        -- Required extensions: 'VK_KHR_swapchain'.
        --

        -- ** Required extensions: 'VK_KHR_swapchain'.
        module Graphics.Vulkan.Types.Struct.Extent,
        module Graphics.Vulkan.Types.Struct.Offset,
        module Graphics.Vulkan.Types.Struct.Present,
        module Graphics.Vulkan.Types.Struct.Rect,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Enum.StructureType,
        -- > #include "vk_platform.h"
        VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION,
        pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION,
        VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME,
        pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR)
       where
import           GHC.Ptr                                  (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.Offset
import           Graphics.Vulkan.Types.Struct.Present
import           Graphics.Vulkan.Types.Struct.Rect

pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION = 1

type VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION = 1

pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME :: CString

pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME <-
        (is_VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME -> True)
  where VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
          = _VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME

{-# INLINE _VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME #-}

_VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME :: CString
_VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
  = Ptr "VK_KHR_incremental_present\NUL"#

{-# INLINE is_VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME #-}

is_VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME

type VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME =
     "VK_KHR_incremental_present"

pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR :: VkStructureType

pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR =
        VkStructureType 1000084000
