{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
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
module Graphics.Vulkan.Ext.VK_EXT_display_surface_counter
       (-- * Vulkan extension: @VK_EXT_display_surface_counter@
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
        -- Extension number: @91@
        --
        -- Required extensions: 'VK_KHR_display'.
        --

        -- ** Required extensions: 'VK_KHR_display'.
        VkCompositeAlphaBitmaskKHR(..), VkCompositeAlphaFlagBitsKHR(),
        VkCompositeAlphaFlagsKHR(), VkExtent2D, VkBool32(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkImageAspectBitmask(..), VkImageCreateBitmask(..),
        VkImageLayout(..), VkImageTiling(..), VkImageType(..),
        VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(), VkStructureType(..),
        VkSurfaceCapabilities2EXT, VkSurfaceCounterBitmaskEXT(..),
        VkSurfaceTransformBitmaskKHR(..), VkSurfaceCounterFlagBitsEXT(),
        VkSurfaceCounterFlagsEXT(), VkSurfaceTransformFlagBitsKHR(),
        VkSurfaceTransformFlagsKHR(),
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceSurfaceCapabilities2EXT,
        pattern VkGetPhysicalDeviceSurfaceCapabilities2EXT,
        HS_vkGetPhysicalDeviceSurfaceCapabilities2EXT,
        PFN_vkGetPhysicalDeviceSurfaceCapabilities2EXT,
        module Graphics.Vulkan.Marshal, VkResult(..), VkBuffer,
        VkBufferView, VkBufferView_T(), VkBuffer_T(), VkCommandBuffer,
        VkCommandBuffer_T(), VkCommandPool, VkCommandPool_T(),
        VkDebugReportCallbackEXT, VkDebugReportCallbackEXT_T(),
        VkDebugUtilsMessengerEXT, VkDebugUtilsMessengerEXT_T(),
        VkDescriptorPool, VkDescriptorPool_T(), VkDescriptorSet,
        VkDescriptorSetLayout, VkDescriptorSetLayout_T(),
        VkDescriptorSet_T(), VkDescriptorUpdateTemplate,
        VkDescriptorUpdateTemplateKHR, VkDescriptorUpdateTemplateKHR_T(),
        VkDescriptorUpdateTemplate_T(), VkDevice, VkDeviceMemory,
        VkDeviceMemory_T(), VkDevice_T(), VkDisplayKHR, VkDisplayKHR_T(),
        VkDisplayModeKHR, VkDisplayModeKHR_T(), VkEvent, VkEvent_T(),
        VkFence, VkFence_T(), VkFramebuffer, VkFramebuffer_T(), VkImage,
        VkImageView, VkImageView_T(), VkImage_T(),
        VkIndirectCommandsLayoutNVX, VkIndirectCommandsLayoutNVX_T(),
        VkInstance, VkInstance_T(), VkObjectTableNVX, VkObjectTableNVX_T(),
        VkPhysicalDevice, VkPhysicalDevice_T(), VkPipeline,
        VkPipelineCache, VkPipelineCache_T(), VkPipelineLayout,
        VkPipelineLayout_T(), VkPipeline_T(), VkQueryPool, VkQueryPool_T(),
        VkQueue, VkQueue_T(), VkRenderPass, VkRenderPass_T(), VkSampler,
        VkSamplerYcbcrConversion, VkSamplerYcbcrConversionKHR,
        VkSamplerYcbcrConversionKHR_T(), VkSamplerYcbcrConversion_T(),
        VkSampler_T(), VkSemaphore, VkSemaphore_T(), VkShaderModule,
        VkShaderModule_T(), VkSurfaceKHR, VkSurfaceKHR_T(), VkSwapchainKHR,
        VkSwapchainKHR_T(), VkValidationCacheEXT, VkValidationCacheEXT_T(),
        VkExtent3D, VkSurfaceCapabilities2KHR, VkSurfaceCapabilitiesKHR,
        VkSurfaceFormat2KHR, VkSurfaceFormatKHR,
        VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION,
        pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION,
        VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME,
        pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT,
        pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT)
       where
import           GHC.Ptr                                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                      (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.Surface
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.Surface

pattern VkGetPhysicalDeviceSurfaceCapabilities2EXT :: CString

pattern VkGetPhysicalDeviceSurfaceCapabilities2EXT <-
        (is_VkGetPhysicalDeviceSurfaceCapabilities2EXT -> True)
  where
    VkGetPhysicalDeviceSurfaceCapabilities2EXT
      = _VkGetPhysicalDeviceSurfaceCapabilities2EXT

{-# INLINE _VkGetPhysicalDeviceSurfaceCapabilities2EXT #-}

_VkGetPhysicalDeviceSurfaceCapabilities2EXT :: CString
_VkGetPhysicalDeviceSurfaceCapabilities2EXT
  = Ptr "vkGetPhysicalDeviceSurfaceCapabilities2EXT\NUL"#

{-# INLINE is_VkGetPhysicalDeviceSurfaceCapabilities2EXT #-}

is_VkGetPhysicalDeviceSurfaceCapabilities2EXT :: CString -> Bool
is_VkGetPhysicalDeviceSurfaceCapabilities2EXT
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceSurfaceCapabilities2EXT

type VkGetPhysicalDeviceSurfaceCapabilities2EXT =
     "vkGetPhysicalDeviceSurfaceCapabilities2EXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceCapabilities2EXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSurfaceKHR surface
--   >     , VkSurfaceCapabilities2EXT* pSurfaceCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfaceCapabilities2EXT vkGetPhysicalDeviceSurfaceCapabilities2EXT registry at www.khronos.org>
type HS_vkGetPhysicalDeviceSurfaceCapabilities2EXT =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       VkSurfaceKHR -- ^ surface
                    -> Ptr VkSurfaceCapabilities2EXT -- ^ pSurfaceCapabilities
                                                     -> IO VkResult

type PFN_vkGetPhysicalDeviceSurfaceCapabilities2EXT =
     FunPtr HS_vkGetPhysicalDeviceSurfaceCapabilities2EXT

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceCapabilities2EXTUnsafe ::
               PFN_vkGetPhysicalDeviceSurfaceCapabilities2EXT ->
                 HS_vkGetPhysicalDeviceSurfaceCapabilities2EXT

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceCapabilities2EXTSafe ::
               PFN_vkGetPhysicalDeviceSurfaceCapabilities2EXT ->
                 HS_vkGetPhysicalDeviceSurfaceCapabilities2EXT

instance VulkanProc "vkGetPhysicalDeviceSurfaceCapabilities2EXT"
         where
    type VkProcType "vkGetPhysicalDeviceSurfaceCapabilities2EXT" =
         HS_vkGetPhysicalDeviceSurfaceCapabilities2EXT
    vkProcSymbol = _VkGetPhysicalDeviceSurfaceCapabilities2EXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceSurfaceCapabilities2EXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceSurfaceCapabilities2EXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = 1

type VK_EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION = 1

pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME :: CString

pattern VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME <-
        (is_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME -> True)
  where
    VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
      = _VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME

{-# INLINE _VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME #-}

_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME :: CString
_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
  = Ptr "VK_EXT_display_surface_counter\NUL"#

{-# INLINE is_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME #-}

is_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME

type VK_EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME =
     "VK_EXT_display_surface_counter"

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT =
        VkStructureType 1000090000

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT =
        VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
