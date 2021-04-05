{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_surface_protected_capabilities
       (AHardwareBuffer(), ANativeWindow(), CAMetalLayer(), VkBool32(..),
        VkDeviceAddress(..), VkDeviceSize(..), VkFlags(..),
        VkSampleMask(..), VkCompositeAlphaBitmaskKHR(..),
        VkCompositeAlphaFlagBitsKHR(), VkCompositeAlphaFlagsKHR(),
        VkExtent2D, VkImageAspectBitmask(..), VkImageCreateBitmask(..),
        VkImageLayout(..), VkImageTiling(..), VkImageType(..),
        VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(),
        VkImageViewCreateBitmask(..), VkImageViewCreateFlagBits(),
        VkImageViewCreateFlags(), VkStructureType(..),
        VkSurfaceCapabilities2KHR, VkSurfaceCapabilitiesKHR,
        VkSurfaceProtectedCapabilitiesKHR, VkSurfaceCounterBitmaskEXT(..),
        VkSurfaceTransformBitmaskKHR(..), VkSurfaceCounterFlagBitsEXT(),
        VkSurfaceCounterFlagsEXT(), VkSurfaceTransformFlagBitsKHR(),
        VkSurfaceTransformFlagsKHR(),
        -- > #include "vk_platform.h"
        VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION,
        pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION,
        VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME,
        pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR)
       where
import GHC.Ptr                                           (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR
import Graphics.Vulkan.Types.Enum.Image
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Enum.Surface
import Graphics.Vulkan.Types.Struct.Extent               (VkExtent2D)
import Graphics.Vulkan.Types.Struct.Surface              (VkSurfaceCapabilities2KHR,
                                                          VkSurfaceCapabilitiesKHR,
                                                          VkSurfaceProtectedCapabilitiesKHR)

pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION = 1

type VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION = 1

pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME ::
        CString

pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME <-
        (is_VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME -> True)
  where
    VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME
      = _VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME

{-# INLINE _VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME
           #-}

_VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME :: CString
_VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME
  = Ptr "VK_KHR_surface_protected_capabilities\NUL"#

{-# INLINE is_VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME
           #-}

is_VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME ::
                                                        CString -> Bool
is_VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME

type VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME =
     "VK_KHR_surface_protected_capabilities"

pattern VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR =
        VkStructureType 1000239000
