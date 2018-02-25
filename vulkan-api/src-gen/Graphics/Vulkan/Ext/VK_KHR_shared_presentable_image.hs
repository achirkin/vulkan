{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_shared_presentable_image
       (-- * Vulkan extension: @VK_KHR_shared_presentable_image@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Alon Or-bach @alonorbach@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @112@
        --
        -- Required extensions: 'VK_KHR_swapchain', 'VK_KHR_get_physical_device_properties2', 'VK_KHR_get_surface_capabilities2'.
        --

        -- ** Required extensions: 'VK_KHR_swapchain', 'VK_KHR_get_physical_device_properties2', 'VK_KHR_get_surface_capabilities2'.
        module Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR,
        module Graphics.Vulkan.Types.Struct.VkExtent2D,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Struct.VkSharedPresentSurfaceCapabilitiesKHR,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Struct.VkSurfaceCapabilities2KHR,
        module Graphics.Vulkan.Types.Struct.VkSurfaceCapabilitiesKHR,
        module Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR,
        -- > #include "vk_platform.h"
        vkGetSwapchainStatusKHR,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION,
        pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION,
        VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME,
        pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR,
        pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR,
        pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR,
        pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR)
       where
import           GHC.Ptr
                                                                                     (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkImageLayout
                                                                                     (VkImageLayout (..))
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkPresentModeKHR
                                                                                     (VkPresentModeKHR (..))
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkExtent2D
import           Graphics.Vulkan.Types.Struct.VkSharedPresentSurfaceCapabilitiesKHR
import           Graphics.Vulkan.Types.Struct.VkSurfaceCapabilities2KHR
import           Graphics.Vulkan.Types.Struct.VkSurfaceCapabilitiesKHR

-- | Success codes: 'VK_SUCCESS', 'VK_SUBOPTIMAL_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetSwapchainStatusKHR
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetSwapchainStatusKHR.html vkGetSwapchainStatusKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetSwapchainStatusKHR"
               vkGetSwapchainStatusKHR ::
               VkDevice -- ^ device
                        -> VkSwapchainKHR -- ^ swapchain
                                          -> IO VkResult

pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION = 1

type VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION = 1

pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME :: CString

pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME <-
        (is_VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME -> True)
  where VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
          = _VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME

{-# INLINE _VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME #-}

_VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME :: CString
_VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
  = Ptr "VK_KHR_shared_presentable_image\NUL"#

{-# INLINE is_VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME #-}

is_VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME ::
                                                  CString -> Bool
is_VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME

type VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME =
     "VK_KHR_shared_presentable_image"

pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR =
        VkStructureType 1000111000

pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR ::
        VkPresentModeKHR

pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR =
        VkPresentModeKHR 1000111000

pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR ::
        VkPresentModeKHR

pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR =
        VkPresentModeKHR 1000111001

pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR :: VkImageLayout

pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR =
        VkImageLayout 1000111000
