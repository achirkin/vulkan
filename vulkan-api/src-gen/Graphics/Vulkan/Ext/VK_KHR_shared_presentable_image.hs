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
        module Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR,
        module Graphics.Vulkan.Types.Struct.Extent,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.Image,
        module Graphics.Vulkan.Types.Struct.SharedPresentSurfaceCapabilitiesKHR,
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Struct.Surface,
        module Graphics.Vulkan.Types.Enum.Surface, -- > #include "vk_platform.h"
                                                   VkGetSwapchainStatusKHR,
        pattern VkGetSwapchainStatusKHR, HS_vkGetSwapchainStatusKHR,
        PFN_vkGetSwapchainStatusKHR,
        module Graphics.Vulkan.Types.Enum.Result,
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
import           Graphics.Vulkan.Marshal.Proc
                                                                                   (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.PresentModeKHR
                                                                                   (VkPresentModeKHR (..))
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.Surface
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.SharedPresentSurfaceCapabilitiesKHR
import           Graphics.Vulkan.Types.Struct.Surface

pattern VkGetSwapchainStatusKHR :: CString

pattern VkGetSwapchainStatusKHR <-
        (is_VkGetSwapchainStatusKHR -> True)
  where VkGetSwapchainStatusKHR = _VkGetSwapchainStatusKHR

{-# INLINE _VkGetSwapchainStatusKHR #-}

_VkGetSwapchainStatusKHR :: CString
_VkGetSwapchainStatusKHR = Ptr "vkGetSwapchainStatusKHR\NUL"#

{-# INLINE is_VkGetSwapchainStatusKHR #-}

is_VkGetSwapchainStatusKHR :: CString -> Bool
is_VkGetSwapchainStatusKHR
  = (EQ ==) . cmpCStrings _VkGetSwapchainStatusKHR

type VkGetSwapchainStatusKHR = "vkGetSwapchainStatusKHR"

-- | Success codes: 'VK_SUCCESS', 'VK_SUBOPTIMAL_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetSwapchainStatusKHR
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetSwapchainStatusKHR vkGetSwapchainStatusKHR registry at www.khronos.org>
type HS_vkGetSwapchainStatusKHR =
     VkDevice -- ^ device
              -> VkSwapchainKHR -- ^ swapchain
                                -> IO VkResult

type PFN_vkGetSwapchainStatusKHR =
     FunPtr HS_vkGetSwapchainStatusKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetSwapchainStatusKHRUnsafe ::
               PFN_vkGetSwapchainStatusKHR -> HS_vkGetSwapchainStatusKHR

foreign import ccall safe "dynamic"
               unwrapVkGetSwapchainStatusKHRSafe ::
               PFN_vkGetSwapchainStatusKHR -> HS_vkGetSwapchainStatusKHR

instance VulkanProc "vkGetSwapchainStatusKHR" where
        type VkProcType "vkGetSwapchainStatusKHR" =
             HS_vkGetSwapchainStatusKHR
        vkProcSymbol = _VkGetSwapchainStatusKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkGetSwapchainStatusKHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkGetSwapchainStatusKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
