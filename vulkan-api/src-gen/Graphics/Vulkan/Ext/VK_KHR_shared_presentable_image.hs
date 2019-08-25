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
        VkCompositeAlphaBitmaskKHR(..), VkCompositeAlphaFlagBitsKHR(),
        VkCompositeAlphaFlagsKHR(), VkExtent2D, VkBool32(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkImageAspectBitmask(..), VkImageCreateBitmask(..),
        VkImageLayout(..), VkImageTiling(..), VkImageType(..),
        VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(),
        VkSharedPresentSurfaceCapabilitiesKHR, VkStructureType(..),
        VkSurfaceCapabilities2KHR, VkSurfaceCapabilitiesKHR,
        VkSurfaceCounterBitmaskEXT(..), VkSurfaceTransformBitmaskKHR(..),
        VkSurfaceCounterFlagBitsEXT(), VkSurfaceCounterFlagsEXT(),
        VkSurfaceTransformFlagBitsKHR(), VkSurfaceTransformFlagsKHR(),
        -- > #include "vk_platform.h"
        VkGetSwapchainStatusKHR, pattern VkGetSwapchainStatusKHR,
        HS_vkGetSwapchainStatusKHR, PFN_vkGetSwapchainStatusKHR,
        VkResult(..), VkBuffer, VkBufferView, VkBufferView_T(),
        VkBuffer_T(), VkCommandBuffer, VkCommandBuffer_T(), VkCommandPool,
        VkCommandPool_T(), VkDebugReportCallbackEXT,
        VkDebugReportCallbackEXT_T(), VkDebugUtilsMessengerEXT,
        VkDebugUtilsMessengerEXT_T(), VkDescriptorPool,
        VkDescriptorPool_T(), VkDescriptorSet, VkDescriptorSetLayout,
        VkDescriptorSetLayout_T(), VkDescriptorSet_T(),
        VkDescriptorUpdateTemplate, VkDescriptorUpdateTemplateKHR,
        VkDescriptorUpdateTemplateKHR_T(), VkDescriptorUpdateTemplate_T(),
        VkDevice, VkDeviceMemory, VkDeviceMemory_T(), VkDevice_T(),
        VkDisplayKHR, VkDisplayKHR_T(), VkDisplayModeKHR,
        VkDisplayModeKHR_T(), VkEvent, VkEvent_T(), VkFence, VkFence_T(),
        VkFramebuffer, VkFramebuffer_T(), VkImage, VkImageView,
        VkImageView_T(), VkImage_T(), VkIndirectCommandsLayoutNVX,
        VkIndirectCommandsLayoutNVX_T(), VkInstance, VkInstance_T(),
        VkObjectTableNVX, VkObjectTableNVX_T(), VkPhysicalDevice,
        VkPhysicalDevice_T(), VkPipeline, VkPipelineCache,
        VkPipelineCache_T(), VkPipelineLayout, VkPipelineLayout_T(),
        VkPipeline_T(), VkQueryPool, VkQueryPool_T(), VkQueue, VkQueue_T(),
        VkRenderPass, VkRenderPass_T(), VkSampler,
        VkSamplerYcbcrConversion, VkSamplerYcbcrConversionKHR,
        VkSamplerYcbcrConversionKHR_T(), VkSamplerYcbcrConversion_T(),
        VkSampler_T(), VkSemaphore, VkSemaphore_T(), VkShaderModule,
        VkShaderModule_T(), VkSurfaceKHR, VkSurfaceKHR_T(), VkSwapchainKHR,
        VkSwapchainKHR_T(), VkValidationCacheEXT, VkValidationCacheEXT_T(),
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
                                                                                   (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.SharedPresentSurfaceCapabilitiesKHR
                                                                                   (VkSharedPresentSurfaceCapabilitiesKHR)
import           Graphics.Vulkan.Types.Struct.Surface
                                                                                   (VkSurfaceCapabilities2KHR,
                                                                                   VkSurfaceCapabilitiesKHR)

pattern VkGetSwapchainStatusKHR :: CString

pattern VkGetSwapchainStatusKHR <-
        (is_VkGetSwapchainStatusKHR -> True)
  where
    VkGetSwapchainStatusKHR = _VkGetSwapchainStatusKHR

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
  where
    VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
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
