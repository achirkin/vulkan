{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
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
module Graphics.Vulkan.Ext.VK_EXT_sample_locations
       (-- * Vulkan extension: @VK_EXT_sample_locations@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Rakos @drakos-amd@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @144@
        module Graphics.Vulkan.Marshal, VkAccessBitmask(..),
        VkAccessFlagBits(), VkAccessFlags(),
        VkAttachmentSampleLocationsEXT, VkBool32(..), VkDeviceSize(..),
        VkFlags(..), VkSampleMask(..), VkClearColorValue,
        VkClearDepthStencilValue, VkClearValue, VkExtent2D,
        VkImageAspectBitmask(..), VkImageCreateBitmask(..),
        VkImageLayout(..), VkImageTiling(..), VkImageType(..),
        VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(), VkImageMemoryBarrier,
        VkImageSubresourceRange, VkMultisamplePropertiesEXT, VkOffset2D,
        VkPhysicalDeviceLimits, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2,
        VkPhysicalDeviceSampleLocationsPropertiesEXT,
        VkPhysicalDeviceSparseProperties, VkPhysicalDeviceType(..),
        VkAndroidSurfaceCreateFlagsKHR(..), VkBufferViewCreateFlags(..),
        VkCommandPoolTrimFlags(..), VkCommandPoolTrimFlagsKHR(..),
        VkDebugUtilsMessengerCallbackDataFlagsEXT(..),
        VkDebugUtilsMessengerCreateFlagsEXT(..),
        VkDescriptorPoolResetFlags(..),
        VkDescriptorUpdateTemplateCreateFlags(..),
        VkDescriptorUpdateTemplateCreateFlagsKHR(..),
        VkDeviceCreateFlags(..), VkDisplayModeCreateFlagsKHR(..),
        VkDisplaySurfaceCreateFlagsKHR(..), VkEventCreateFlags(..),
        VkExternalFenceFeatureFlagsKHR(..),
        VkExternalFenceHandleTypeFlagsKHR(..),
        VkExternalMemoryFeatureFlagsKHR(..),
        VkExternalMemoryHandleTypeFlagsKHR(..),
        VkExternalSemaphoreFeatureFlagsKHR(..),
        VkExternalSemaphoreHandleTypeFlagsKHR(..),
        VkFenceImportFlagsKHR(..), VkFramebufferCreateFlags(..),
        VkIOSSurfaceCreateFlagsMVK(..), VkImageViewCreateFlags(..),
        VkInstanceCreateFlags(..), VkMacOSSurfaceCreateFlagsMVK(..),
        VkMemoryAllocateFlagsKHR(..), VkMemoryMapFlags(..),
        VkMirSurfaceCreateFlagsKHR(..), VkPeerMemoryFeatureFlagsKHR(..),
        VkPipelineCacheCreateFlags(..),
        VkPipelineColorBlendStateCreateFlags(..),
        VkPipelineCoverageModulationStateCreateFlagsNV(..),
        VkPipelineCoverageToColorStateCreateFlagsNV(..),
        VkPipelineDepthStencilStateCreateFlags(..),
        VkPipelineDiscardRectangleStateCreateFlagsEXT(..),
        VkPipelineDynamicStateCreateFlags(..),
        VkPipelineInputAssemblyStateCreateFlags(..),
        VkPipelineLayoutCreateFlags(..),
        VkPipelineMultisampleStateCreateFlags(..),
        VkPipelineRasterizationConservativeStateCreateFlagsEXT(..),
        VkPipelineRasterizationStateCreateFlags(..),
        VkPipelineShaderStageCreateFlags(..),
        VkPipelineTessellationStateCreateFlags(..),
        VkPipelineVertexInputStateCreateFlags(..),
        VkPipelineViewportStateCreateFlags(..),
        VkPipelineViewportSwizzleStateCreateFlagsNV(..),
        VkQueryPoolCreateFlags(..), VkRenderPassCreateFlags(..),
        VkSamplerCreateFlags(..), VkSemaphoreCreateFlags(..),
        VkSemaphoreImportFlagsKHR(..), VkShaderModuleCreateFlags(..),
        VkValidationCacheCreateFlagsEXT(..), VkViSurfaceCreateFlagsNN(..),
        VkWaylandSurfaceCreateFlagsKHR(..),
        VkWin32SurfaceCreateFlagsKHR(..), VkXcbSurfaceCreateFlagsKHR(..),
        VkXlibSurfaceCreateFlagsKHR(..),
        VkPipelineMultisampleStateCreateInfo,
        VkPipelineSampleLocationsStateCreateInfoEXT, VkRect2D,
        VkRenderPassBeginInfo, VkRenderPassSampleLocationsBeginInfoEXT,
        VkSampleCountBitmask(..), VkSampleCountFlagBits(),
        VkSampleCountFlags(), VkSampleLocationEXT,
        VkSampleLocationsInfoEXT, VkStructureType(..),
        VkSubpassSampleLocationsEXT, -- > #include "vk_platform.h"
                                     VkCmdSetSampleLocationsEXT,
        pattern VkCmdSetSampleLocationsEXT, HS_vkCmdSetSampleLocationsEXT,
        PFN_vkCmdSetSampleLocationsEXT,
        VkGetPhysicalDeviceMultisamplePropertiesEXT,
        pattern VkGetPhysicalDeviceMultisamplePropertiesEXT,
        HS_vkGetPhysicalDeviceMultisamplePropertiesEXT,
        PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT, VkBuffer,
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
        VkExtent3D, VkImageBlit, VkImageCopy, VkImageCreateInfo,
        VkImageFormatListCreateInfoKHR, VkImageFormatProperties,
        VkImageFormatProperties2, VkImageFormatProperties2KHR,
        VkImageMemoryRequirementsInfo2, VkImageMemoryRequirementsInfo2KHR,
        VkImagePlaneMemoryRequirementsInfo,
        VkImagePlaneMemoryRequirementsInfoKHR, VkImageResolve,
        VkImageSparseMemoryRequirementsInfo2,
        VkImageSparseMemoryRequirementsInfo2KHR, VkImageSubresource,
        VkImageSubresourceLayers, VkImageSwapchainCreateInfoKHR,
        VkImageViewCreateInfo, VkImageViewUsageCreateInfo,
        VkImageViewUsageCreateInfoKHR,
        VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION,
        pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION,
        VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME,
        pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME,
        pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT,
        pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT,
        pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT)
       where
import           GHC.Ptr                                               (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                          (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.AccessFlags
import           Graphics.Vulkan.Types.Enum.DynamicState               (VkDynamicState (..))
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Attachment               (VkAttachmentSampleLocationsEXT)
import           Graphics.Vulkan.Types.Struct.Clear                    (VkClearColorValue,
                                                                        VkClearDepthStencilValue,
                                                                        VkClearValue)
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.Image
import           Graphics.Vulkan.Types.Struct.MultisamplePropertiesEXT
import           Graphics.Vulkan.Types.Struct.Offset                   (VkOffset2D)
import           Graphics.Vulkan.Types.Struct.PhysicalDevice           (VkPhysicalDeviceLimits,
                                                                        VkPhysicalDeviceProperties,
                                                                        VkPhysicalDeviceProperties2,
                                                                        VkPhysicalDeviceSampleLocationsPropertiesEXT,
                                                                        VkPhysicalDeviceSparseProperties)
import           Graphics.Vulkan.Types.Struct.Pipeline                 (VkPipelineMultisampleStateCreateInfo,
                                                                        VkPipelineSampleLocationsStateCreateInfoEXT)
import           Graphics.Vulkan.Types.Struct.Rect                     (VkRect2D)
import           Graphics.Vulkan.Types.Struct.RenderPass               (VkRenderPassBeginInfo,
                                                                        VkRenderPassSampleLocationsBeginInfoEXT)
import           Graphics.Vulkan.Types.Struct.SampleLocation
import           Graphics.Vulkan.Types.Struct.Subpass                  (VkSubpassSampleLocationsEXT)

pattern VkCmdSetSampleLocationsEXT :: CString

pattern VkCmdSetSampleLocationsEXT <-
        (is_VkCmdSetSampleLocationsEXT -> True)
  where
    VkCmdSetSampleLocationsEXT = _VkCmdSetSampleLocationsEXT

{-# INLINE _VkCmdSetSampleLocationsEXT #-}

_VkCmdSetSampleLocationsEXT :: CString
_VkCmdSetSampleLocationsEXT = Ptr "vkCmdSetSampleLocationsEXT\NUL"#

{-# INLINE is_VkCmdSetSampleLocationsEXT #-}

is_VkCmdSetSampleLocationsEXT :: CString -> Bool
is_VkCmdSetSampleLocationsEXT
  = (EQ ==) . cmpCStrings _VkCmdSetSampleLocationsEXT

type VkCmdSetSampleLocationsEXT = "vkCmdSetSampleLocationsEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetSampleLocationsEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkSampleLocationsInfoEXT* pSampleLocationsInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetSampleLocationsEXT vkCmdSetSampleLocationsEXT registry at www.khronos.org>
type HS_vkCmdSetSampleLocationsEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr VkSampleLocationsInfoEXT -- ^ pSampleLocationsInfo
                                                     -> IO ()

type PFN_vkCmdSetSampleLocationsEXT =
     FunPtr HS_vkCmdSetSampleLocationsEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetSampleLocationsEXTUnsafe ::
               PFN_vkCmdSetSampleLocationsEXT -> HS_vkCmdSetSampleLocationsEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdSetSampleLocationsEXTSafe ::
               PFN_vkCmdSetSampleLocationsEXT -> HS_vkCmdSetSampleLocationsEXT

instance VulkanProc "vkCmdSetSampleLocationsEXT" where
    type VkProcType "vkCmdSetSampleLocationsEXT" =
         HS_vkCmdSetSampleLocationsEXT
    vkProcSymbol = _VkCmdSetSampleLocationsEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetSampleLocationsEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetSampleLocationsEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceMultisamplePropertiesEXT :: CString

pattern VkGetPhysicalDeviceMultisamplePropertiesEXT <-
        (is_VkGetPhysicalDeviceMultisamplePropertiesEXT -> True)
  where
    VkGetPhysicalDeviceMultisamplePropertiesEXT
      = _VkGetPhysicalDeviceMultisamplePropertiesEXT

{-# INLINE _VkGetPhysicalDeviceMultisamplePropertiesEXT #-}

_VkGetPhysicalDeviceMultisamplePropertiesEXT :: CString
_VkGetPhysicalDeviceMultisamplePropertiesEXT
  = Ptr "vkGetPhysicalDeviceMultisamplePropertiesEXT\NUL"#

{-# INLINE is_VkGetPhysicalDeviceMultisamplePropertiesEXT #-}

is_VkGetPhysicalDeviceMultisamplePropertiesEXT :: CString -> Bool
is_VkGetPhysicalDeviceMultisamplePropertiesEXT
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceMultisamplePropertiesEXT

type VkGetPhysicalDeviceMultisamplePropertiesEXT =
     "vkGetPhysicalDeviceMultisamplePropertiesEXT"

-- | > void vkGetPhysicalDeviceMultisamplePropertiesEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSampleCountFlagBits samples
--   >     , VkMultisamplePropertiesEXT* pMultisampleProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMultisamplePropertiesEXT vkGetPhysicalDeviceMultisamplePropertiesEXT registry at www.khronos.org>
type HS_vkGetPhysicalDeviceMultisamplePropertiesEXT =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       VkSampleCountFlagBits -- ^ samples
                             -> Ptr VkMultisamplePropertiesEXT -- ^ pMultisampleProperties
                                                               -> IO ()

type PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT =
     FunPtr HS_vkGetPhysicalDeviceMultisamplePropertiesEXT

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceMultisamplePropertiesEXTUnsafe ::
               PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT ->
                 HS_vkGetPhysicalDeviceMultisamplePropertiesEXT

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceMultisamplePropertiesEXTSafe ::
               PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT ->
                 HS_vkGetPhysicalDeviceMultisamplePropertiesEXT

instance VulkanProc "vkGetPhysicalDeviceMultisamplePropertiesEXT"
         where
    type VkProcType "vkGetPhysicalDeviceMultisamplePropertiesEXT" =
         HS_vkGetPhysicalDeviceMultisamplePropertiesEXT
    vkProcSymbol = _VkGetPhysicalDeviceMultisamplePropertiesEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceMultisamplePropertiesEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceMultisamplePropertiesEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION = 1

type VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION = 1

pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: CString

pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME <-
        (is_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME -> True)
  where
    VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
      = _VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME

{-# INLINE _VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME #-}

_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: CString
_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
  = Ptr "VK_EXT_sample_locations\NUL"#

{-# INLINE is_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME #-}

is_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME

type VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME =
     "VK_EXT_sample_locations"

-- | bitpos = @12@
pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
        :: VkImageCreateBitmask a

pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT =
        VkImageCreateBitmask 4096

pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT =
        VkStructureType 1000143000

pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
        = VkStructureType 1000143001

pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
        = VkStructureType 1000143002

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
        = VkStructureType 1000143003

pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT =
        VkStructureType 1000143004

pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT =
        VkDynamicState 1000143000
