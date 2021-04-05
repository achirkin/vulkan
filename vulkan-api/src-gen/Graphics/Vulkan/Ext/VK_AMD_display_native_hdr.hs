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
module Graphics.Vulkan.Ext.VK_AMD_display_native_hdr
       (AHardwareBuffer(), ANativeWindow(), CAMetalLayer(), VkBool32(..),
        VkDeviceAddress(..), VkDeviceSize(..), VkFlags(..),
        VkSampleMask(..), pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR,
        VkColorComponentBitmask(..), VkColorSpaceKHR(..),
        VkColorComponentFlagBits(), VkColorComponentFlags(),
        VkCompositeAlphaBitmaskKHR(..), VkCompositeAlphaFlagBitsKHR(),
        VkCompositeAlphaFlagsKHR(),
        VkDisplayNativeHdrSurfaceCapabilitiesAMD, VkExtent2D, VkFormat(..),
        VkFormatFeatureBitmask(..), VkFormatFeatureFlagBits(),
        VkFormatFeatureFlags(), VkImageAspectBitmask(..),
        VkImageCreateBitmask(..), VkImageLayout(..), VkImageTiling(..),
        VkImageType(..), VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(),
        VkImageViewCreateBitmask(..), VkImageViewCreateFlagBits(),
        VkImageViewCreateFlags(), VkPresentModeKHR(..), VkSharingMode(..),
        VkStructureType(..), VkSurfaceCapabilities2KHR,
        VkSurfaceCapabilitiesKHR, VkSurfaceCounterBitmaskEXT(..),
        VkSurfaceTransformBitmaskKHR(..), VkSurfaceCounterFlagBitsEXT(),
        VkSurfaceCounterFlagsEXT(), VkSurfaceTransformFlagBitsKHR(),
        VkSurfaceTransformFlagsKHR(),
        VkSwapchainImageUsageBitmaskANDROID(..),
        VkSwapchainCreateBitmaskKHR(..), VkSwapchainCreateFlagBitsKHR(),
        VkSwapchainCreateFlagsKHR(),
        VkSwapchainImageUsageFlagBitsANDROID(),
        VkSwapchainImageUsageFlagsANDROID(), VkSwapchainCreateInfoKHR,
        VkSwapchainDisplayNativeHdrCreateInfoAMD, -- > #include "vk_platform.h"
                                                  VkSetLocalDimmingAMD,
        pattern VkSetLocalDimmingAMD, HS_vkSetLocalDimmingAMD,
        PFN_vkSetLocalDimmingAMD, module Graphics.Vulkan.Marshal,
        VkAccelerationStructureKHR, VkAccelerationStructureKHR_T(),
        VkAccelerationStructureNV, VkAccelerationStructureNV_T(), VkBuffer,
        VkBufferView, VkBufferView_T(), VkBuffer_T(), VkCommandBuffer,
        VkCommandBuffer_T(), VkCommandPool, VkCommandPool_T(),
        VkDebugReportCallbackEXT, VkDebugReportCallbackEXT_T(),
        VkDebugUtilsMessengerEXT, VkDebugUtilsMessengerEXT_T(),
        VkDeferredOperationKHR, VkDeferredOperationKHR_T(),
        VkDescriptorPool, VkDescriptorPool_T(), VkDescriptorSet,
        VkDescriptorSetLayout, VkDescriptorSetLayout_T(),
        VkDescriptorSet_T(), VkDescriptorUpdateTemplate,
        VkDescriptorUpdateTemplateKHR, VkDescriptorUpdateTemplateKHR_T(),
        VkDescriptorUpdateTemplate_T(), VkDevice, VkDeviceMemory,
        VkDeviceMemory_T(), VkDevice_T(), VkDisplayKHR, VkDisplayKHR_T(),
        VkDisplayModeKHR, VkDisplayModeKHR_T(), VkEvent, VkEvent_T(),
        VkFence, VkFence_T(), VkFramebuffer, VkFramebuffer_T(), VkImage,
        VkImageView, VkImageView_T(), VkImage_T(),
        VkIndirectCommandsLayoutNV, VkIndirectCommandsLayoutNV_T(),
        VkInstance, VkInstance_T(), VkPerformanceConfigurationINTEL,
        VkPerformanceConfigurationINTEL_T(), VkPhysicalDevice,
        VkPhysicalDevice_T(), VkPipeline, VkPipelineCache,
        VkPipelineCache_T(), VkPipelineLayout, VkPipelineLayout_T(),
        VkPipeline_T(), VkPrivateDataSlotEXT, VkPrivateDataSlotEXT_T(),
        VkQueryPool, VkQueryPool_T(), VkQueue, VkQueue_T(), VkRenderPass,
        VkRenderPass_T(), VkSampler, VkSamplerYcbcrConversion,
        VkSamplerYcbcrConversionKHR, VkSamplerYcbcrConversionKHR_T(),
        VkSamplerYcbcrConversion_T(), VkSampler_T(), VkSemaphore,
        VkSemaphore_T(), VkShaderModule, VkShaderModule_T(), VkSurfaceKHR,
        VkSurfaceKHR_T(), VkSwapchainKHR, VkSwapchainKHR_T(),
        VkValidationCacheEXT, VkValidationCacheEXT_T(),
        VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION,
        pattern VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION,
        VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME,
        pattern VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD,
        pattern VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD,
        pattern VK_COLOR_SPACE_DISPLAY_NATIVE_AMD)
       where
import GHC.Ptr                                           (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                      (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Enum.Color
import Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR
import Graphics.Vulkan.Types.Enum.Format
import Graphics.Vulkan.Types.Enum.Image
import Graphics.Vulkan.Types.Enum.PresentModeKHR
import Graphics.Vulkan.Types.Enum.SharingMode
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Enum.Surface
import Graphics.Vulkan.Types.Enum.Swapchain
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.Display              (VkDisplayNativeHdrSurfaceCapabilitiesAMD)
import Graphics.Vulkan.Types.Struct.Extent               (VkExtent2D)
import Graphics.Vulkan.Types.Struct.Surface              (VkSurfaceCapabilities2KHR,
                                                          VkSurfaceCapabilitiesKHR)
import Graphics.Vulkan.Types.Struct.Swapchain            (VkSwapchainCreateInfoKHR,
                                                          VkSwapchainDisplayNativeHdrCreateInfoAMD)

pattern VkSetLocalDimmingAMD :: CString

pattern VkSetLocalDimmingAMD <- (is_VkSetLocalDimmingAMD -> True)
  where
    VkSetLocalDimmingAMD = _VkSetLocalDimmingAMD

{-# INLINE _VkSetLocalDimmingAMD #-}

_VkSetLocalDimmingAMD :: CString
_VkSetLocalDimmingAMD = Ptr "vkSetLocalDimmingAMD\NUL"#

{-# INLINE is_VkSetLocalDimmingAMD #-}

is_VkSetLocalDimmingAMD :: CString -> Bool
is_VkSetLocalDimmingAMD
  = (EQ ==) . cmpCStrings _VkSetLocalDimmingAMD

type VkSetLocalDimmingAMD = "vkSetLocalDimmingAMD"

-- | > void vkSetLocalDimmingAMD
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapChain
--   >     , VkBool32 localDimmingEnable
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkSetLocalDimmingAMD vkSetLocalDimmingAMD registry at www.khronos.org>
type HS_vkSetLocalDimmingAMD =
     VkDevice -- ^ device
              -> VkSwapchainKHR -- ^ swapChain
                                -> VkBool32 -- ^ localDimmingEnable
                                            -> IO ()

type PFN_vkSetLocalDimmingAMD = FunPtr HS_vkSetLocalDimmingAMD

foreign import ccall unsafe "dynamic"
               unwrapVkSetLocalDimmingAMDUnsafe ::
               PFN_vkSetLocalDimmingAMD -> HS_vkSetLocalDimmingAMD

foreign import ccall safe "dynamic" unwrapVkSetLocalDimmingAMDSafe
               :: PFN_vkSetLocalDimmingAMD -> HS_vkSetLocalDimmingAMD

instance VulkanProc "vkSetLocalDimmingAMD" where
    type VkProcType "vkSetLocalDimmingAMD" = HS_vkSetLocalDimmingAMD
    vkProcSymbol = _VkSetLocalDimmingAMD

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkSetLocalDimmingAMDUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkSetLocalDimmingAMDSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION = 1

type VK_AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION = 1

pattern VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME :: CString

pattern VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME <-
        (is_VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME -> True)
  where
    VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME
      = _VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME

{-# INLINE _VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME #-}

_VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME :: CString
_VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME
  = Ptr "VK_AMD_display_native_hdr\NUL"#

{-# INLINE is_VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME #-}

is_VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME :: CString -> Bool
is_VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME

type VK_AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME =
     "VK_AMD_display_native_hdr"

pattern VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD
        = VkStructureType 1000213000

pattern VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD
        = VkStructureType 1000213001

pattern VK_COLOR_SPACE_DISPLAY_NATIVE_AMD :: VkColorSpaceKHR

pattern VK_COLOR_SPACE_DISPLAY_NATIVE_AMD =
        VkColorSpaceKHR 1000213000
