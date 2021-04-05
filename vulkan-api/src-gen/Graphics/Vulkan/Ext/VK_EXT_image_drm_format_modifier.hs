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
module Graphics.Vulkan.Ext.VK_EXT_image_drm_format_modifier
       (AHardwareBuffer(), ANativeWindow(), CAMetalLayer(), VkBool32(..),
        VkDeviceAddress(..), VkDeviceSize(..), VkFlags(..),
        VkSampleMask(..), VkDrmFormatModifierPropertiesEXT,
        VkDrmFormatModifierPropertiesListEXT, VkExtent3D, VkFormat(..),
        VkFormatFeatureBitmask(..), VkFormatFeatureFlagBits(),
        VkFormatFeatureFlags(), VkFormatProperties, VkFormatProperties2,
        VkImageAspectBitmask(..), VkImageCreateBitmask(..),
        VkImageLayout(..), VkImageTiling(..), VkImageType(..),
        VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(),
        VkImageViewCreateBitmask(..), VkImageViewCreateFlagBits(),
        VkImageViewCreateFlags(), VkImageCreateInfo,
        VkImageDrmFormatModifierExplicitCreateInfoEXT,
        VkImageDrmFormatModifierListCreateInfoEXT,
        VkImageDrmFormatModifierPropertiesEXT,
        VkPhysicalDeviceImageDrmFormatModifierInfoEXT,
        VkPhysicalDeviceImageFormatInfo2, VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(), VkSharingMode(..),
        VkStructureType(..), VkSubresourceLayout,
        -- > #include "vk_platform.h"
        VkGetImageDrmFormatModifierPropertiesEXT,
        pattern VkGetImageDrmFormatModifierPropertiesEXT,
        HS_vkGetImageDrmFormatModifierPropertiesEXT,
        PFN_vkGetImageDrmFormatModifierPropertiesEXT,
        module Graphics.Vulkan.Marshal, VkResult(..),
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
        VkValidationCacheEXT, VkValidationCacheEXT_T(), VkImageBlit,
        VkImageCopy, VkImageFormatListCreateInfo,
        VkImageFormatListCreateInfoKHR, VkImageFormatProperties,
        VkImageFormatProperties2, VkImageFormatProperties2KHR,
        VkImageMemoryBarrier, VkImageMemoryRequirementsInfo2,
        VkImageMemoryRequirementsInfo2KHR,
        VkImagePlaneMemoryRequirementsInfo,
        VkImagePlaneMemoryRequirementsInfoKHR, VkImageResolve,
        VkImageSparseMemoryRequirementsInfo2,
        VkImageSparseMemoryRequirementsInfo2KHR,
        VkImageStencilUsageCreateInfo, VkImageStencilUsageCreateInfoEXT,
        VkImageSubresource, VkImageSubresourceLayers,
        VkImageSubresourceRange, VkImageSwapchainCreateInfoKHR,
        VkImageViewASTCDecodeModeEXT, VkImageViewAddressPropertiesNVX,
        VkImageViewCreateInfo, VkImageViewHandleInfoNVX,
        VkImageViewUsageCreateInfo, VkImageViewUsageCreateInfoKHR,
        VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION,
        pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION,
        VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME,
        pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME,
        pattern VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT,
        pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT,
        pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT,
        pattern VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT,
        pattern VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT,
        pattern VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT,
        pattern VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT,
        pattern VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT)
       where
import GHC.Ptr                                                  (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                             (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Enum.Format
import Graphics.Vulkan.Types.Enum.Image
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.SharingMode
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.DrmFormatModifierProperties (VkDrmFormatModifierPropertiesEXT,
                                                                 VkDrmFormatModifierPropertiesListEXT)
import Graphics.Vulkan.Types.Struct.Extent                      (VkExtent3D)
import Graphics.Vulkan.Types.Struct.FormatProperties            (VkFormatProperties,
                                                                 VkFormatProperties2)
import Graphics.Vulkan.Types.Struct.Image
import Graphics.Vulkan.Types.Struct.PhysicalDevice              (VkPhysicalDeviceImageDrmFormatModifierInfoEXT,
                                                                 VkPhysicalDeviceImageFormatInfo2)
import Graphics.Vulkan.Types.Struct.SubresourceLayout           (VkSubresourceLayout)

pattern VkGetImageDrmFormatModifierPropertiesEXT :: CString

pattern VkGetImageDrmFormatModifierPropertiesEXT <-
        (is_VkGetImageDrmFormatModifierPropertiesEXT -> True)
  where
    VkGetImageDrmFormatModifierPropertiesEXT
      = _VkGetImageDrmFormatModifierPropertiesEXT

{-# INLINE _VkGetImageDrmFormatModifierPropertiesEXT #-}

_VkGetImageDrmFormatModifierPropertiesEXT :: CString
_VkGetImageDrmFormatModifierPropertiesEXT
  = Ptr "vkGetImageDrmFormatModifierPropertiesEXT\NUL"#

{-# INLINE is_VkGetImageDrmFormatModifierPropertiesEXT #-}

is_VkGetImageDrmFormatModifierPropertiesEXT :: CString -> Bool
is_VkGetImageDrmFormatModifierPropertiesEXT
  = (EQ ==) . cmpCStrings _VkGetImageDrmFormatModifierPropertiesEXT

type VkGetImageDrmFormatModifierPropertiesEXT =
     "vkGetImageDrmFormatModifierPropertiesEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetImageDrmFormatModifierPropertiesEXT
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , VkImageDrmFormatModifierPropertiesEXT* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetImageDrmFormatModifierPropertiesEXT vkGetImageDrmFormatModifierPropertiesEXT registry at www.khronos.org>
type HS_vkGetImageDrmFormatModifierPropertiesEXT =
     VkDevice -- ^ device
              ->
       VkImage -- ^ image
               -> Ptr VkImageDrmFormatModifierPropertiesEXT -- ^ pProperties
                                                            -> IO VkResult

type PFN_vkGetImageDrmFormatModifierPropertiesEXT =
     FunPtr HS_vkGetImageDrmFormatModifierPropertiesEXT

foreign import ccall unsafe "dynamic"
               unwrapVkGetImageDrmFormatModifierPropertiesEXTUnsafe ::
               PFN_vkGetImageDrmFormatModifierPropertiesEXT ->
                 HS_vkGetImageDrmFormatModifierPropertiesEXT

foreign import ccall safe "dynamic"
               unwrapVkGetImageDrmFormatModifierPropertiesEXTSafe ::
               PFN_vkGetImageDrmFormatModifierPropertiesEXT ->
                 HS_vkGetImageDrmFormatModifierPropertiesEXT

instance VulkanProc "vkGetImageDrmFormatModifierPropertiesEXT"
         where
    type VkProcType "vkGetImageDrmFormatModifierPropertiesEXT" =
         HS_vkGetImageDrmFormatModifierPropertiesEXT
    vkProcSymbol = _VkGetImageDrmFormatModifierPropertiesEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetImageDrmFormatModifierPropertiesEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetImageDrmFormatModifierPropertiesEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION = 1

type VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_SPEC_VERSION = 1

pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME :: CString

pattern VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME <-
        (is_VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME -> True)
  where
    VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME
      = _VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME

{-# INLINE _VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME #-}

_VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME :: CString
_VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME
  = Ptr "VK_EXT_image_drm_format_modifier\NUL"#

{-# INLINE is_VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME #-}

is_VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME ::
                                                   CString -> Bool
is_VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME

type VK_EXT_IMAGE_DRM_FORMAT_MODIFIER_EXTENSION_NAME =
     "VK_EXT_image_drm_format_modifier"

pattern VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT ::
        VkResult

pattern VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT =
        VkResult (-1000158000)

pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT =
        VkStructureType 1000158000

pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT =
        VkStructureType 1000158001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT
        = VkStructureType 1000158002

pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT
        = VkStructureType 1000158003

pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT
        = VkStructureType 1000158004

pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
        = VkStructureType 1000158005

pattern VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT :: VkImageTiling

pattern VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT =
        VkImageTiling 1000158000

-- | bitpos = @7@
pattern VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT ::
        VkImageAspectBitmask a

pattern VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT =
        VkImageAspectBitmask 128

-- | bitpos = @8@
pattern VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT ::
        VkImageAspectBitmask a

pattern VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT =
        VkImageAspectBitmask 256

-- | bitpos = @9@
pattern VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT ::
        VkImageAspectBitmask a

pattern VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT =
        VkImageAspectBitmask 512

-- | bitpos = @10@
pattern VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT ::
        VkImageAspectBitmask a

pattern VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT =
        VkImageAspectBitmask 1024
