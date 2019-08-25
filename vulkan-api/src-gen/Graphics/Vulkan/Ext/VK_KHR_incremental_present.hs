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
        VkExtent2D, VkOffset2D, VkPresentInfoKHR, VkPresentRegionKHR,
        VkPresentRegionsKHR, VkRectLayerKHR, VkResult(..),
        VkStructureType(..), -- > #include "vk_platform.h"
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
import           Graphics.Vulkan.Types.Struct.Extent      (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.Offset      (VkOffset2D)
import           Graphics.Vulkan.Types.Struct.Present     (VkPresentInfoKHR,
                                                           VkPresentRegionKHR,
                                                           VkPresentRegionsKHR)
import           Graphics.Vulkan.Types.Struct.Rect        (VkRectLayerKHR)

pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION = 1

type VK_KHR_INCREMENTAL_PRESENT_SPEC_VERSION = 1

pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME :: CString

pattern VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME <-
        (is_VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME -> True)
  where
    VK_KHR_INCREMENTAL_PRESENT_EXTENSION_NAME
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
