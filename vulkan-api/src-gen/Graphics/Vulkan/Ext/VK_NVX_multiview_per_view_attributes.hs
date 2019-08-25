{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NVX_multiview_per_view_attributes
       (-- * Vulkan extension: @VK_NVX_multiview_per_view_attributes@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jeffbolznv@
        --
        -- author: @NVX@
        --
        -- type: @device@
        --
        -- Extension number: @98@
        --
        -- Required extensions: 'VK_KHR_multiview'.
        --

        -- ** Required extensions: 'VK_KHR_multiview'.
        module Graphics.Vulkan.Marshal, VkBool32(..), VkDeviceSize(..),
        VkFlags(..), VkSampleMask(..), VkPhysicalDeviceLimits,
        VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX,
        VkPhysicalDeviceProperties, VkPhysicalDeviceProperties2,
        VkPhysicalDeviceSparseProperties, VkPhysicalDeviceType(..),
        VkSampleCountBitmask(..), VkSampleCountFlagBits(),
        VkSampleCountFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION,
        pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION,
        VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME,
        pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX,
        pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX,
        pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX)
       where
import           GHC.Ptr                                       (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.Subpass            (VkSubpassDescriptionBitmask (..))
import           Graphics.Vulkan.Types.Struct.PhysicalDevice   (VkPhysicalDeviceLimits,
                                                                VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX,
                                                                VkPhysicalDeviceProperties,
                                                                VkPhysicalDeviceProperties2,
                                                                VkPhysicalDeviceSparseProperties)

pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION = 1

type VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION = 1

pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME ::
        CString

pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME <-
        (is_VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME -> True)
  where
    VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
      = _VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME

{-# INLINE _VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME #-}

_VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME :: CString
_VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
  = Ptr "VK_NVX_multiview_per_view_attributes\NUL"#

{-# INLINE is_VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
           #-}

is_VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME ::
                                                       CString -> Bool
is_VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME

type VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME =
     "VK_NVX_multiview_per_view_attributes"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
        = VkStructureType 1000097000

-- | bitpos = @0@
pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX ::
        VkSubpassDescriptionBitmask a

pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX =
        VkSubpassDescriptionBitmask 1

-- | bitpos = @1@
pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX ::
        VkSubpassDescriptionBitmask a

pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX =
        VkSubpassDescriptionBitmask 2
