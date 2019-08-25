{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
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
module Graphics.Vulkan.Ext.VK_KHR_device_group
       (-- * Vulkan extension: @VK_KHR_device_group@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jeffbolznv@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @61@
        --
        -- Required extensions: 'VK_KHR_device_group_creation'.
        --

        -- ** Required extensions: 'VK_KHR_device_group_creation'.
        VkDeviceGroupBindSparseInfoKHR,
        VkDeviceGroupCommandBufferBeginInfoKHR,
        VkDeviceGroupRenderPassBeginInfoKHR, VkDeviceGroupSubmitInfoKHR,
        VkMemoryAllocateBitmask(..), VkMemoryHeapBitmask(..),
        VkMemoryPropertyBitmask(..), VkMemoryAllocateFlagBits(),
        VkMemoryAllocateFlagBitsKHR(..), VkMemoryAllocateFlags(),
        VkMemoryHeapFlagBits(), VkMemoryHeapFlags(),
        VkMemoryPropertyFlagBits(), VkMemoryPropertyFlags(),
        VkMemoryAllocateFlagsInfoKHR, VkAndroidSurfaceCreateFlagsKHR(..),
        VkBufferViewCreateFlags(..), VkCommandPoolTrimFlags(..),
        VkCommandPoolTrimFlagsKHR(..),
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
        VkXlibSurfaceCreateFlagsKHR(..), VkPeerMemoryFeatureBitmask(..),
        VkPeerMemoryFeatureFlagBits(), VkPeerMemoryFeatureFlagBitsKHR(..),
        VkPeerMemoryFeatureFlags(), VkGetDeviceGroupPeerMemoryFeaturesKHR,
        pattern VkGetDeviceGroupPeerMemoryFeaturesKHR,
        HS_vkGetDeviceGroupPeerMemoryFeaturesKHR,
        PFN_vkGetDeviceGroupPeerMemoryFeaturesKHR, VkCmdSetDeviceMaskKHR,
        pattern VkCmdSetDeviceMaskKHR, HS_vkCmdSetDeviceMaskKHR,
        PFN_vkCmdSetDeviceMaskKHR, VkCmdDispatchBaseKHR,
        pattern VkCmdDispatchBaseKHR, HS_vkCmdDispatchBaseKHR,
        PFN_vkCmdDispatchBaseKHR, module Graphics.Vulkan.Marshal,
        VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkBuffer, VkBufferView, VkBufferView_T(), VkBuffer_T(),
        VkCommandBuffer, VkCommandBuffer_T(), VkCommandPool,
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
        VK_KHR_DEVICE_GROUP_SPEC_VERSION,
        pattern VK_KHR_DEVICE_GROUP_SPEC_VERSION,
        VK_KHR_DEVICE_GROUP_EXTENSION_NAME,
        pattern VK_KHR_DEVICE_GROUP_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR,
        pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR,
        pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR,
        pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR,
        pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR,
        pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR,
        pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR,
        pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHR,
        pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR,
        -- ** Required extensions: 'VK_KHR_bind_memory2', 'VK_KHR_device_group_creation'.
        VkBindBufferMemoryDeviceGroupInfoKHR,
        VkBindImageMemoryDeviceGroupInfoKHR,
        pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR,
        pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR,
        -- ** Required extensions: 'VK_KHR_surface', 'VK_KHR_device_group_creation'.
        VkDeviceGroupPresentCapabilitiesKHR, VkDeviceEventTypeEXT(..),
        VkDeviceGroupPresentModeBitmaskKHR(..), VkDeviceCreateFlagBits(..),
        VkDeviceGroupPresentModeFlagBitsKHR(),
        VkDeviceGroupPresentModeFlagsKHR(), VkDeviceQueueCreateBitmask(..),
        VkDeviceQueueCreateFlagBits(), VkDeviceQueueCreateFlags(),
        VkStructureType(..),
        -- > #include "vk_platform.h"
        pattern VkGetDeviceGroupPresentCapabilitiesKHR,
        HS_vkGetDeviceGroupPresentCapabilitiesKHR,
        PFN_vkGetDeviceGroupPresentCapabilitiesKHR,
        pattern VkGetDeviceGroupSurfacePresentModesKHR,
        HS_vkGetDeviceGroupSurfacePresentModesKHR,
        PFN_vkGetDeviceGroupSurfacePresentModesKHR,
        pattern VkGetPhysicalDevicePresentRectanglesKHR,
        HS_vkGetPhysicalDevicePresentRectanglesKHR,
        PFN_vkGetPhysicalDevicePresentRectanglesKHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR,
        -- ** Required extensions: 'VK_KHR_swapchain', 'VK_KHR_device_group_creation'.
        VkAcquireNextImageInfoKHR, VkBindImageMemoryInfo,
        VkBindImageMemorySwapchainInfoKHR, VkColorComponentBitmask(..),
        VkColorSpaceKHR(..), VkColorComponentFlagBits(),
        VkColorComponentFlags(), VkCompositeAlphaBitmaskKHR(..),
        VkCompositeAlphaFlagBitsKHR(), VkCompositeAlphaFlagsKHR(),
        VkDeviceGroupPresentInfoKHR, VkDeviceGroupSwapchainCreateInfoKHR,
        VkExtent2D, VkExtent3D, VkFormat(..), VkFormatFeatureBitmask(..),
        VkFormatFeatureFlagBits(), VkFormatFeatureFlags(),
        VkImageAspectBitmask(..), VkImageCreateBitmask(..),
        VkImageLayout(..), VkImageTiling(..), VkImageType(..),
        VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(), VkImageCreateInfo,
        VkImageSwapchainCreateInfoKHR, VkPresentInfoKHR,
        VkPresentModeKHR(..), VkResult(..), VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(), VkSharingMode(..),
        VkSurfaceCounterBitmaskEXT(..), VkSurfaceTransformBitmaskKHR(..),
        VkSurfaceCounterFlagBitsEXT(), VkSurfaceCounterFlagsEXT(),
        VkSurfaceTransformFlagBitsKHR(), VkSurfaceTransformFlagsKHR(),
        VkSwapchainCreateBitmaskKHR(..), VkSwapchainCreateFlagBitsKHR(),
        VkSwapchainCreateFlagsKHR(), VkSwapchainCreateInfoKHR,
        -- > #include "vk_platform.h"
        pattern VkAcquireNextImage2KHR, HS_vkAcquireNextImage2KHR,
        PFN_vkAcquireNextImage2KHR,
        pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR,
        pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR)
       where
import           GHC.Ptr                                              (Ptr (..))
import           Graphics.Vulkan.Core_1_1                             (pattern VK_DEPENDENCY_DEVICE_GROUP_BIT,
                                                                       pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT,
                                                                       pattern VK_PIPELINE_CREATE_DISPATCH_BASE,
                                                                       pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT,
                                                                       pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO,
                                                                       pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO,
                                                                       pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO,
                                                                       pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO,
                                                                       pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO,
                                                                       pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO,
                                                                       pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO)
import           Graphics.Vulkan.Ext.VK_KHR_swapchain                 (HS_vkAcquireNextImage2KHR,
                                                                       HS_vkGetDeviceGroupPresentCapabilitiesKHR,
                                                                       HS_vkGetDeviceGroupSurfacePresentModesKHR,
                                                                       HS_vkGetPhysicalDevicePresentRectanglesKHR,
                                                                       PFN_vkAcquireNextImage2KHR,
                                                                       PFN_vkGetDeviceGroupPresentCapabilitiesKHR,
                                                                       PFN_vkGetDeviceGroupSurfacePresentModesKHR,
                                                                       PFN_vkGetPhysicalDevicePresentRectanglesKHR,
                                                                       pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR,
                                                                       pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR,
                                                                       pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR,
                                                                       pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR,
                                                                       pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR,
                                                                       pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR,
                                                                       pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR,
                                                                       pattern VkAcquireNextImage2KHR,
                                                                       pattern VkGetDeviceGroupPresentCapabilitiesKHR,
                                                                       pattern VkGetDeviceGroupSurfacePresentModesKHR,
                                                                       pattern VkGetPhysicalDevicePresentRectanglesKHR)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                         (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.Color
import           Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR
import           Graphics.Vulkan.Types.Enum.Device
import           Graphics.Vulkan.Types.Enum.Format
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.Memory
import           Graphics.Vulkan.Types.Enum.PeerMemoryFeatureFlag
import           Graphics.Vulkan.Types.Enum.PresentModeKHR
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.SharingMode
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.Surface
import           Graphics.Vulkan.Types.Enum.SwapchainCreateFlagsKHR
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.AcquireNextImageInfoKHR (VkAcquireNextImageInfoKHR)
import           Graphics.Vulkan.Types.Struct.Bind                    (VkBindBufferMemoryDeviceGroupInfoKHR,
                                                                       VkBindImageMemoryDeviceGroupInfoKHR,
                                                                       VkBindImageMemoryInfo,
                                                                       VkBindImageMemorySwapchainInfoKHR)
import           Graphics.Vulkan.Types.Struct.Device                  (VkDeviceGroupBindSparseInfoKHR,
                                                                       VkDeviceGroupCommandBufferBeginInfoKHR,
                                                                       VkDeviceGroupPresentCapabilitiesKHR,
                                                                       VkDeviceGroupPresentInfoKHR,
                                                                       VkDeviceGroupRenderPassBeginInfoKHR,
                                                                       VkDeviceGroupSubmitInfoKHR,
                                                                       VkDeviceGroupSwapchainCreateInfoKHR)
import           Graphics.Vulkan.Types.Struct.Extent                  (VkExtent2D,
                                                                       VkExtent3D)
import           Graphics.Vulkan.Types.Struct.Image                   (VkImageCreateInfo,
                                                                       VkImageSwapchainCreateInfoKHR)
import           Graphics.Vulkan.Types.Struct.Memory                  (VkMemoryAllocateFlagsInfoKHR)
import           Graphics.Vulkan.Types.Struct.Present                 (VkPresentInfoKHR)
import           Graphics.Vulkan.Types.Struct.SwapchainC              (VkSwapchainCreateInfoKHR)

pattern VkGetDeviceGroupPeerMemoryFeaturesKHR :: CString

pattern VkGetDeviceGroupPeerMemoryFeaturesKHR <-
        (is_VkGetDeviceGroupPeerMemoryFeaturesKHR -> True)
  where
    VkGetDeviceGroupPeerMemoryFeaturesKHR
      = _VkGetDeviceGroupPeerMemoryFeaturesKHR

{-# INLINE _VkGetDeviceGroupPeerMemoryFeaturesKHR #-}

_VkGetDeviceGroupPeerMemoryFeaturesKHR :: CString
_VkGetDeviceGroupPeerMemoryFeaturesKHR
  = Ptr "vkGetDeviceGroupPeerMemoryFeaturesKHR\NUL"#

{-# INLINE is_VkGetDeviceGroupPeerMemoryFeaturesKHR #-}

is_VkGetDeviceGroupPeerMemoryFeaturesKHR :: CString -> Bool
is_VkGetDeviceGroupPeerMemoryFeaturesKHR
  = (EQ ==) . cmpCStrings _VkGetDeviceGroupPeerMemoryFeaturesKHR

type VkGetDeviceGroupPeerMemoryFeaturesKHR =
     "vkGetDeviceGroupPeerMemoryFeaturesKHR"

-- | This is an alias for `vkGetDeviceGroupPeerMemoryFeatures`.
--
--   > void vkGetDeviceGroupPeerMemoryFeaturesKHR
--   >     ( VkDevice device
--   >     , uint32_t heapIndex
--   >     , uint32_t localDeviceIndex
--   >     , uint32_t remoteDeviceIndex
--   >     , VkPeerMemoryFeatureFlags* pPeerMemoryFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDeviceGroupPeerMemoryFeaturesKHR vkGetDeviceGroupPeerMemoryFeaturesKHR registry at www.khronos.org>
type HS_vkGetDeviceGroupPeerMemoryFeaturesKHR =
     VkDevice -- ^ device
              ->
       Word32 -- ^ heapIndex
              -> Word32 -- ^ localDeviceIndex
                        -> Word32 -- ^ remoteDeviceIndex
                                  -> Ptr VkPeerMemoryFeatureFlags -- ^ pPeerMemoryFeatures
                                                                  -> IO ()

type PFN_vkGetDeviceGroupPeerMemoryFeaturesKHR =
     FunPtr HS_vkGetDeviceGroupPeerMemoryFeaturesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetDeviceGroupPeerMemoryFeaturesKHRUnsafe ::
               PFN_vkGetDeviceGroupPeerMemoryFeaturesKHR ->
                 HS_vkGetDeviceGroupPeerMemoryFeaturesKHR

foreign import ccall safe "dynamic"
               unwrapVkGetDeviceGroupPeerMemoryFeaturesKHRSafe ::
               PFN_vkGetDeviceGroupPeerMemoryFeaturesKHR ->
                 HS_vkGetDeviceGroupPeerMemoryFeaturesKHR

instance VulkanProc "vkGetDeviceGroupPeerMemoryFeaturesKHR" where
    type VkProcType "vkGetDeviceGroupPeerMemoryFeaturesKHR" =
         HS_vkGetDeviceGroupPeerMemoryFeaturesKHR
    vkProcSymbol = _VkGetDeviceGroupPeerMemoryFeaturesKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetDeviceGroupPeerMemoryFeaturesKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetDeviceGroupPeerMemoryFeaturesKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetDeviceMaskKHR :: CString

pattern VkCmdSetDeviceMaskKHR <- (is_VkCmdSetDeviceMaskKHR -> True)
  where
    VkCmdSetDeviceMaskKHR = _VkCmdSetDeviceMaskKHR

{-# INLINE _VkCmdSetDeviceMaskKHR #-}

_VkCmdSetDeviceMaskKHR :: CString
_VkCmdSetDeviceMaskKHR = Ptr "vkCmdSetDeviceMaskKHR\NUL"#

{-# INLINE is_VkCmdSetDeviceMaskKHR #-}

is_VkCmdSetDeviceMaskKHR :: CString -> Bool
is_VkCmdSetDeviceMaskKHR
  = (EQ ==) . cmpCStrings _VkCmdSetDeviceMaskKHR

type VkCmdSetDeviceMaskKHR = "vkCmdSetDeviceMaskKHR"

-- | This is an alias for `vkCmdSetDeviceMask`.
--
--   Queues: 'graphics', 'compute', 'transfer'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetDeviceMaskKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t deviceMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdSetDeviceMaskKHR vkCmdSetDeviceMaskKHR registry at www.khronos.org>
type HS_vkCmdSetDeviceMaskKHR = VkCommandBuffer -- ^ commandBuffer
                                                -> Word32 -- ^ deviceMask
                                                          -> IO ()

type PFN_vkCmdSetDeviceMaskKHR = FunPtr HS_vkCmdSetDeviceMaskKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetDeviceMaskKHRUnsafe ::
               PFN_vkCmdSetDeviceMaskKHR -> HS_vkCmdSetDeviceMaskKHR

foreign import ccall safe "dynamic" unwrapVkCmdSetDeviceMaskKHRSafe
               :: PFN_vkCmdSetDeviceMaskKHR -> HS_vkCmdSetDeviceMaskKHR

instance VulkanProc "vkCmdSetDeviceMaskKHR" where
    type VkProcType "vkCmdSetDeviceMaskKHR" = HS_vkCmdSetDeviceMaskKHR
    vkProcSymbol = _VkCmdSetDeviceMaskKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetDeviceMaskKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetDeviceMaskKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdDispatchBaseKHR :: CString

pattern VkCmdDispatchBaseKHR <- (is_VkCmdDispatchBaseKHR -> True)
  where
    VkCmdDispatchBaseKHR = _VkCmdDispatchBaseKHR

{-# INLINE _VkCmdDispatchBaseKHR #-}

_VkCmdDispatchBaseKHR :: CString
_VkCmdDispatchBaseKHR = Ptr "vkCmdDispatchBaseKHR\NUL"#

{-# INLINE is_VkCmdDispatchBaseKHR #-}

is_VkCmdDispatchBaseKHR :: CString -> Bool
is_VkCmdDispatchBaseKHR
  = (EQ ==) . cmpCStrings _VkCmdDispatchBaseKHR

type VkCmdDispatchBaseKHR = "vkCmdDispatchBaseKHR"

-- | This is an alias for `vkCmdDispatchBase`.
--
--   Queues: 'compute'.
--
--   Renderpass: @outside@
--
--   > void vkCmdDispatchBaseKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t baseGroupX
--   >     , uint32_t baseGroupY
--   >     , uint32_t baseGroupZ
--   >     , uint32_t groupCountX
--   >     , uint32_t groupCountY
--   >     , uint32_t groupCountZ
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDispatchBaseKHR vkCmdDispatchBaseKHR registry at www.khronos.org>
type HS_vkCmdDispatchBaseKHR =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ baseGroupX
              -> Word32 -- ^ baseGroupY
                        -> Word32 -- ^ baseGroupZ
                                  -> Word32 -- ^ groupCountX
                                            -> Word32 -- ^ groupCountY
                                                      -> Word32 -- ^ groupCountZ
                                                                -> IO ()

type PFN_vkCmdDispatchBaseKHR = FunPtr HS_vkCmdDispatchBaseKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCmdDispatchBaseKHRUnsafe ::
               PFN_vkCmdDispatchBaseKHR -> HS_vkCmdDispatchBaseKHR

foreign import ccall safe "dynamic" unwrapVkCmdDispatchBaseKHRSafe
               :: PFN_vkCmdDispatchBaseKHR -> HS_vkCmdDispatchBaseKHR

instance VulkanProc "vkCmdDispatchBaseKHR" where
    type VkProcType "vkCmdDispatchBaseKHR" = HS_vkCmdDispatchBaseKHR
    vkProcSymbol = _VkCmdDispatchBaseKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdDispatchBaseKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdDispatchBaseKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_DEVICE_GROUP_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_DEVICE_GROUP_SPEC_VERSION = 3

type VK_KHR_DEVICE_GROUP_SPEC_VERSION = 3

pattern VK_KHR_DEVICE_GROUP_EXTENSION_NAME :: CString

pattern VK_KHR_DEVICE_GROUP_EXTENSION_NAME <-
        (is_VK_KHR_DEVICE_GROUP_EXTENSION_NAME -> True)
  where
    VK_KHR_DEVICE_GROUP_EXTENSION_NAME
      = _VK_KHR_DEVICE_GROUP_EXTENSION_NAME

{-# INLINE _VK_KHR_DEVICE_GROUP_EXTENSION_NAME #-}

_VK_KHR_DEVICE_GROUP_EXTENSION_NAME :: CString
_VK_KHR_DEVICE_GROUP_EXTENSION_NAME
  = Ptr "VK_KHR_device_group\NUL"#

{-# INLINE is_VK_KHR_DEVICE_GROUP_EXTENSION_NAME #-}

is_VK_KHR_DEVICE_GROUP_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_DEVICE_GROUP_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_DEVICE_GROUP_EXTENSION_NAME

type VK_KHR_DEVICE_GROUP_EXTENSION_NAME = "VK_KHR_device_group"

pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR =
        VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR =
        VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR
        = VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR =
        VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR =
        VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO

pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR =
        VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT

pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR =
        VK_PEER_MEMORY_FEATURE_COPY_DST_BIT

pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR =
        VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT

pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR =
        VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT

pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR =
        VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT

pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR =
        VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT

pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHR =
        VK_PIPELINE_CREATE_DISPATCH_BASE

pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR =
        VK_DEPENDENCY_DEVICE_GROUP_BIT

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR
        = VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR =
        VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO

pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR =
        VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
