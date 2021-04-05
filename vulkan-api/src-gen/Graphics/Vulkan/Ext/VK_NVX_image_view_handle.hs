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
module Graphics.Vulkan.Ext.VK_NVX_image_view_handle
       (-- * Vulkan extension: @VK_NVX_image_view_handle@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Eric Werness @ewerness@
        --
        -- author: @NVX@
        --
        -- type: @device@
        --
        -- Extension number: @31@
        VkDescriptorBindingBitmask(..), VkDescriptorPoolCreateBitmask(..),
        VkDescriptorType(..), VkDescriptorUpdateTemplateType(..),
        VkDescriptorBindingFlagBits(), VkDescriptorBindingFlagBitsEXT(..),
        VkDescriptorBindingFlags(), VkDescriptorPoolCreateFlagBits(),
        VkDescriptorPoolCreateFlags(),
        VkDescriptorSetLayoutCreateBitmask(..),
        VkDescriptorSetLayoutCreateFlagBits(),
        VkDescriptorSetLayoutCreateFlags(),
        VkDescriptorUpdateTemplateTypeKHR(..), AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkImageViewAddressPropertiesNVX, VkImageViewHandleInfoNVX,
        VkStructureType(..), -- > #include "vk_platform.h"
                             VkGetImageViewHandleNVX,
        pattern VkGetImageViewHandleNVX, HS_vkGetImageViewHandleNVX,
        PFN_vkGetImageViewHandleNVX, VkGetImageViewAddressNVX,
        pattern VkGetImageViewAddressNVX, HS_vkGetImageViewAddressNVX,
        PFN_vkGetImageViewAddressNVX, module Graphics.Vulkan.Marshal,
        VkResult(..), VkAccelerationStructureKHR,
        VkAccelerationStructureKHR_T(), VkAccelerationStructureNV,
        VkAccelerationStructureNV_T(), VkBuffer, VkBufferView,
        VkBufferView_T(), VkBuffer_T(), VkCommandBuffer,
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
        VkImageCopy, VkImageCreateInfo,
        VkImageDrmFormatModifierExplicitCreateInfoEXT,
        VkImageDrmFormatModifierListCreateInfoEXT,
        VkImageDrmFormatModifierPropertiesEXT, VkImageFormatListCreateInfo,
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
        VkImageViewASTCDecodeModeEXT, VkImageViewCreateInfo,
        VkImageViewUsageCreateInfo, VkImageViewUsageCreateInfoKHR,
        VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION,
        pattern VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION,
        VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME,
        pattern VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX,
        pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX)
       where
import GHC.Ptr                                  (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc             (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Enum.Descriptor
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.Image

pattern VkGetImageViewHandleNVX :: CString

pattern VkGetImageViewHandleNVX <-
        (is_VkGetImageViewHandleNVX -> True)
  where
    VkGetImageViewHandleNVX = _VkGetImageViewHandleNVX

{-# INLINE _VkGetImageViewHandleNVX #-}

_VkGetImageViewHandleNVX :: CString
_VkGetImageViewHandleNVX = Ptr "vkGetImageViewHandleNVX\NUL"#

{-# INLINE is_VkGetImageViewHandleNVX #-}

is_VkGetImageViewHandleNVX :: CString -> Bool
is_VkGetImageViewHandleNVX
  = (EQ ==) . cmpCStrings _VkGetImageViewHandleNVX

type VkGetImageViewHandleNVX = "vkGetImageViewHandleNVX"

-- | > uint32_t vkGetImageViewHandleNVX
--   >     ( VkDevice device
--   >     , const VkImageViewHandleInfoNVX* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetImageViewHandleNVX vkGetImageViewHandleNVX registry at www.khronos.org>
type HS_vkGetImageViewHandleNVX =
     VkDevice -- ^ device
              -> Ptr VkImageViewHandleInfoNVX -- ^ pInfo
                                              -> IO Word32

type PFN_vkGetImageViewHandleNVX =
     FunPtr HS_vkGetImageViewHandleNVX

foreign import ccall unsafe "dynamic"
               unwrapVkGetImageViewHandleNVXUnsafe ::
               PFN_vkGetImageViewHandleNVX -> HS_vkGetImageViewHandleNVX

foreign import ccall safe "dynamic"
               unwrapVkGetImageViewHandleNVXSafe ::
               PFN_vkGetImageViewHandleNVX -> HS_vkGetImageViewHandleNVX

instance VulkanProc "vkGetImageViewHandleNVX" where
    type VkProcType "vkGetImageViewHandleNVX" =
         HS_vkGetImageViewHandleNVX
    vkProcSymbol = _VkGetImageViewHandleNVX

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetImageViewHandleNVXUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetImageViewHandleNVXSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetImageViewAddressNVX :: CString

pattern VkGetImageViewAddressNVX <-
        (is_VkGetImageViewAddressNVX -> True)
  where
    VkGetImageViewAddressNVX = _VkGetImageViewAddressNVX

{-# INLINE _VkGetImageViewAddressNVX #-}

_VkGetImageViewAddressNVX :: CString
_VkGetImageViewAddressNVX = Ptr "vkGetImageViewAddressNVX\NUL"#

{-# INLINE is_VkGetImageViewAddressNVX #-}

is_VkGetImageViewAddressNVX :: CString -> Bool
is_VkGetImageViewAddressNVX
  = (EQ ==) . cmpCStrings _VkGetImageViewAddressNVX

type VkGetImageViewAddressNVX = "vkGetImageViewAddressNVX"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_UNKNOWN'.
--
--   > VkResult vkGetImageViewAddressNVX
--   >     ( VkDevice device
--   >     , VkImageView imageView
--   >     , VkImageViewAddressPropertiesNVX* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetImageViewAddressNVX vkGetImageViewAddressNVX registry at www.khronos.org>
type HS_vkGetImageViewAddressNVX =
     VkDevice -- ^ device
              ->
       VkImageView -- ^ imageView
                   -> Ptr VkImageViewAddressPropertiesNVX -- ^ pProperties
                                                          -> IO VkResult

type PFN_vkGetImageViewAddressNVX =
     FunPtr HS_vkGetImageViewAddressNVX

foreign import ccall unsafe "dynamic"
               unwrapVkGetImageViewAddressNVXUnsafe ::
               PFN_vkGetImageViewAddressNVX -> HS_vkGetImageViewAddressNVX

foreign import ccall safe "dynamic"
               unwrapVkGetImageViewAddressNVXSafe ::
               PFN_vkGetImageViewAddressNVX -> HS_vkGetImageViewAddressNVX

instance VulkanProc "vkGetImageViewAddressNVX" where
    type VkProcType "vkGetImageViewAddressNVX" =
         HS_vkGetImageViewAddressNVX
    vkProcSymbol = _VkGetImageViewAddressNVX

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetImageViewAddressNVXUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetImageViewAddressNVXSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION = 2

type VK_NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION = 2

pattern VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME :: CString

pattern VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME <-
        (is_VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME -> True)
  where
    VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME
      = _VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME

{-# INLINE _VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME #-}

_VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME :: CString
_VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME
  = Ptr "VK_NVX_image_view_handle\NUL"#

{-# INLINE is_VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME #-}

is_VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME :: CString -> Bool
is_VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME

type VK_NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME =
     "VK_NVX_image_view_handle"

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX =
        VkStructureType 1000030000

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX =
        VkStructureType 1000030001
