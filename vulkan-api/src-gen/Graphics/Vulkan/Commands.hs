{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict                   #-}
module Graphics.Vulkan.Commands
       (-- ** Vulkan command definitions
        vkCreateInstance, vkDestroyInstance, vkEnumeratePhysicalDevices,
        vkGetDeviceProcAddr, vkGetInstanceProcAddr,
        vkGetPhysicalDeviceProperties,
        vkGetPhysicalDeviceQueueFamilyProperties,
        vkGetPhysicalDeviceMemoryProperties, vkGetPhysicalDeviceFeatures,
        vkGetPhysicalDeviceFormatProperties,
        vkGetPhysicalDeviceImageFormatProperties, vkCreateDevice,
        vkDestroyDevice, vkEnumerateInstanceLayerProperties,
        vkEnumerateInstanceExtensionProperties,
        vkEnumerateDeviceLayerProperties,
        vkEnumerateDeviceExtensionProperties, vkGetDeviceQueue,
        vkQueueSubmit, vkQueueWaitIdle, vkDeviceWaitIdle, vkAllocateMemory,
        vkFreeMemory, vkMapMemory, vkUnmapMemory,
        vkFlushMappedMemoryRanges, vkInvalidateMappedMemoryRanges,
        vkGetDeviceMemoryCommitment, vkGetBufferMemoryRequirements,
        vkBindBufferMemory, vkGetImageMemoryRequirements,
        vkBindImageMemory, vkGetImageSparseMemoryRequirements,
        vkGetPhysicalDeviceSparseImageFormatProperties, vkQueueBindSparse,
        vkCreateFence, vkDestroyFence, vkResetFences, vkGetFenceStatus,
        vkWaitForFences, vkCreateSemaphore, vkDestroySemaphore,
        vkCreateEvent, vkDestroyEvent, vkGetEventStatus, vkSetEvent,
        vkResetEvent, vkCreateQueryPool, vkDestroyQueryPool,
        vkGetQueryPoolResults, vkCreateBuffer, vkDestroyBuffer,
        vkCreateBufferView, vkDestroyBufferView, vkCreateImage,
        vkDestroyImage, vkGetImageSubresourceLayout, vkCreateImageView,
        vkDestroyImageView, vkCreateShaderModule, vkDestroyShaderModule,
        vkCreatePipelineCache, vkDestroyPipelineCache,
        vkGetPipelineCacheData, vkMergePipelineCaches,
        vkCreateGraphicsPipelines, vkCreateComputePipelines,
        vkDestroyPipeline, vkCreatePipelineLayout, vkDestroyPipelineLayout,
        vkCreateSampler, vkDestroySampler, vkCreateDescriptorSetLayout,
        vkDestroyDescriptorSetLayout, vkCreateDescriptorPool,
        vkDestroyDescriptorPool, vkResetDescriptorPool,
        vkAllocateDescriptorSets, vkFreeDescriptorSets,
        vkUpdateDescriptorSets, vkCreateFramebuffer, vkDestroyFramebuffer,
        vkCreateRenderPass, vkDestroyRenderPass,
        vkGetRenderAreaGranularity, vkCreateCommandPool,
        vkDestroyCommandPool, vkResetCommandPool, vkAllocateCommandBuffers,
        vkFreeCommandBuffers, vkBeginCommandBuffer, vkEndCommandBuffer,
        vkResetCommandBuffer, vkCmdBindPipeline, vkCmdSetViewport,
        vkCmdSetScissor, vkCmdSetLineWidth, vkCmdSetDepthBias,
        vkCmdSetBlendConstants, vkCmdSetDepthBounds,
        vkCmdSetStencilCompareMask, vkCmdSetStencilWriteMask,
        vkCmdSetStencilReference, vkCmdBindDescriptorSets,
        vkCmdBindIndexBuffer, vkCmdBindVertexBuffers, vkCmdDraw,
        vkCmdDrawIndexed, vkCmdDrawIndirect, vkCmdDrawIndexedIndirect,
        vkCmdDispatch, vkCmdDispatchIndirect, vkCmdCopyBuffer,
        vkCmdCopyImage, vkCmdBlitImage, vkCmdCopyBufferToImage,
        vkCmdCopyImageToBuffer, vkCmdUpdateBuffer, vkCmdFillBuffer,
        vkCmdClearColorImage, vkCmdClearDepthStencilImage,
        vkCmdClearAttachments, vkCmdResolveImage, vkCmdSetEvent,
        vkCmdResetEvent, vkCmdWaitEvents, vkCmdPipelineBarrier,
        vkCmdBeginQuery, vkCmdEndQuery, vkCmdResetQueryPool,
        vkCmdWriteTimestamp, vkCmdCopyQueryPoolResults, vkCmdPushConstants,
        vkCmdBeginRenderPass, vkCmdNextSubpass, vkCmdEndRenderPass,
        vkCmdExecuteCommands, vkCreateAndroidSurfaceKHR,
        vkGetPhysicalDeviceDisplayPropertiesKHR,
        vkGetPhysicalDeviceDisplayPlanePropertiesKHR,
        vkGetDisplayPlaneSupportedDisplaysKHR,
        vkGetDisplayModePropertiesKHR, vkCreateDisplayModeKHR,
        vkGetDisplayPlaneCapabilitiesKHR, vkCreateDisplayPlaneSurfaceKHR,
        vkCreateSharedSwapchainsKHR, vkCreateMirSurfaceKHR,
        vkGetPhysicalDeviceMirPresentationSupportKHR, vkDestroySurfaceKHR,
        vkGetPhysicalDeviceSurfaceSupportKHR,
        vkGetPhysicalDeviceSurfaceCapabilitiesKHR,
        vkGetPhysicalDeviceSurfaceFormatsKHR,
        vkGetPhysicalDeviceSurfacePresentModesKHR, vkCreateSwapchainKHR,
        vkDestroySwapchainKHR, vkGetSwapchainImagesKHR,
        vkAcquireNextImageKHR, vkQueuePresentKHR, vkCreateViSurfaceNN,
        vkCreateWaylandSurfaceKHR,
        vkGetPhysicalDeviceWaylandPresentationSupportKHR,
        vkCreateWin32SurfaceKHR,
        vkGetPhysicalDeviceWin32PresentationSupportKHR,
        vkCreateXlibSurfaceKHR,
        vkGetPhysicalDeviceXlibPresentationSupportKHR,
        vkCreateXcbSurfaceKHR,
        vkGetPhysicalDeviceXcbPresentationSupportKHR,
        vkCreateDebugReportCallbackEXT, vkDestroyDebugReportCallbackEXT,
        vkDebugReportMessageEXT, vkDebugMarkerSetObjectNameEXT,
        vkDebugMarkerSetObjectTagEXT, vkCmdDebugMarkerBeginEXT,
        vkCmdDebugMarkerEndEXT, vkCmdDebugMarkerInsertEXT,
        vkGetPhysicalDeviceExternalImageFormatPropertiesNV,
        vkGetMemoryWin32HandleNV, vkCmdDrawIndirectCountAMD,
        vkCmdDrawIndexedIndirectCountAMD, vkCmdProcessCommandsNVX,
        vkCmdReserveSpaceForCommandsNVX, vkCreateIndirectCommandsLayoutNVX,
        vkDestroyIndirectCommandsLayoutNVX, vkCreateObjectTableNVX,
        vkDestroyObjectTableNVX, vkRegisterObjectsNVX,
        vkUnregisterObjectsNVX,
        vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX,
        vkGetPhysicalDeviceFeatures2KHR, vkGetPhysicalDeviceProperties2KHR,
        vkGetPhysicalDeviceFormatProperties2KHR,
        vkGetPhysicalDeviceImageFormatProperties2KHR,
        vkGetPhysicalDeviceQueueFamilyProperties2KHR,
        vkGetPhysicalDeviceMemoryProperties2KHR,
        vkGetPhysicalDeviceSparseImageFormatProperties2KHR,
        vkCmdPushDescriptorSetKHR, vkTrimCommandPoolKHR,
        vkGetPhysicalDeviceExternalBufferPropertiesKHR,
        vkGetMemoryWin32HandleKHR, vkGetMemoryWin32HandlePropertiesKHR,
        vkGetMemoryFdKHR, vkGetMemoryFdPropertiesKHR,
        vkGetPhysicalDeviceExternalSemaphorePropertiesKHR,
        vkGetSemaphoreWin32HandleKHR, vkImportSemaphoreWin32HandleKHR,
        vkGetSemaphoreFdKHR, vkImportSemaphoreFdKHR,
        vkGetPhysicalDeviceExternalFencePropertiesKHR,
        vkGetFenceWin32HandleKHR, vkImportFenceWin32HandleKHR,
        vkGetFenceFdKHR, vkImportFenceFdKHR, vkReleaseDisplayEXT,
        vkAcquireXlibDisplayEXT, vkGetRandROutputDisplayEXT,
        vkDisplayPowerControlEXT, vkRegisterDeviceEventEXT,
        vkRegisterDisplayEventEXT, vkGetSwapchainCounterEXT,
        vkGetPhysicalDeviceSurfaceCapabilities2EXT,
        vkEnumeratePhysicalDeviceGroupsKHX,
        vkGetDeviceGroupPeerMemoryFeaturesKHX, vkBindBufferMemory2KHR,
        vkBindImageMemory2KHR, vkCmdSetDeviceMaskKHX,
        vkGetDeviceGroupPresentCapabilitiesKHX,
        vkGetDeviceGroupSurfacePresentModesKHX, vkAcquireNextImage2KHX,
        vkCmdDispatchBaseKHX, vkGetPhysicalDevicePresentRectanglesKHX,
        vkCreateDescriptorUpdateTemplateKHR,
        vkDestroyDescriptorUpdateTemplateKHR,
        vkUpdateDescriptorSetWithTemplateKHR,
        vkCmdPushDescriptorSetWithTemplateKHR, vkSetHdrMetadataEXT,
        vkGetSwapchainStatusKHR, vkGetRefreshCycleDurationGOOGLE,
        vkGetPastPresentationTimingGOOGLE, vkCreateIOSSurfaceMVK,
        vkCreateMacOSSurfaceMVK, vkCmdSetViewportWScalingNV,
        vkCmdSetDiscardRectangleEXT, vkCmdSetSampleLocationsEXT,
        vkGetPhysicalDeviceMultisamplePropertiesEXT,
        vkGetPhysicalDeviceSurfaceCapabilities2KHR,
        vkGetPhysicalDeviceSurfaceFormats2KHR,
        vkGetBufferMemoryRequirements2KHR,
        vkGetImageMemoryRequirements2KHR,
        vkGetImageSparseMemoryRequirements2KHR,
        vkCreateSamplerYcbcrConversionKHR,
        vkDestroySamplerYcbcrConversionKHR, vkCreateValidationCacheEXT,
        vkDestroyValidationCacheEXT, vkGetValidationCacheDataEXT,
        vkMergeValidationCachesEXT, vkGetSwapchainGrallocUsageANDROID,
        vkAcquireImageANDROID, vkQueueSignalReleaseImageANDROID,
        vkGetShaderInfoAMD, vkGetMemoryHostPointerPropertiesEXT)
       where
import           Data.Int                    (Int32)
import           Data.Void                   (Void)
import           Data.Word                   (Word32, Word64)
import           Foreign.C.Types             (CChar (..), CFloat (..),
                                              CInt (..), CSize (..),
                                              CULong (..))
import           Foreign.Ptr                 (Ptr)
import           Graphics.Vulkan.SimpleTypes
import           Graphics.Vulkan.Structures

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED', 'VK_ERROR_LAYER_NOT_PRESENT', 'VK_ERROR_EXTENSION_NOT_PRESENT', 'VK_ERROR_INCOMPATIBLE_DRIVER'.
--
--   > VkResult vkCreateInstance
--   >     ( const VkInstanceCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkInstance* pInstance
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateInstance.html vkCreateInstance registry at www.khronos.org>
foreign import ccall unsafe "vkCreateInstance" vkCreateInstance ::
               Foreign.Ptr.Ptr VkInstanceCreateInfo -- ^ pCreateInfo
                                                    ->
                 Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                       ->
                   Foreign.Ptr.Ptr VkInstance -- ^ pInstance
                                              -> IO VkResult

-- | > void vkDestroyInstance
--   >     ( VkInstance instance
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyInstance.html vkDestroyInstance registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyInstance" vkDestroyInstance
               :: VkInstance -- ^ instance
                             -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkEnumeratePhysicalDevices
--   >     ( VkInstance instance
--   >     , uint32_t* pPhysicalDeviceCount
--   >     , VkPhysicalDevice* pPhysicalDevices
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkEnumeratePhysicalDevices.html vkEnumeratePhysicalDevices registry at www.khronos.org>
foreign import ccall unsafe "vkEnumeratePhysicalDevices"
               vkEnumeratePhysicalDevices ::
               VkInstance -- ^ instance
                          ->
                 Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pPhysicalDeviceCount
                                                  ->
                   Foreign.Ptr.Ptr VkPhysicalDevice -- ^ pPhysicalDevices
                                                    -> IO VkResult

-- | > PFN_vkVoidFunction vkGetDeviceProcAddr
--   >     ( VkDevice device
--   >     , const char* pName
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDeviceProcAddr.html vkGetDeviceProcAddr registry at www.khronos.org>
foreign import ccall unsafe "vkGetDeviceProcAddr"
               vkGetDeviceProcAddr ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr Foreign.C.Types.CChar -- ^ pName
                                                       -> IO PFN_vkVoidFunction

-- | > PFN_vkVoidFunction vkGetInstanceProcAddr
--   >     ( VkInstance instance
--   >     , const char* pName
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetInstanceProcAddr.html vkGetInstanceProcAddr registry at www.khronos.org>
foreign import ccall unsafe "vkGetInstanceProcAddr"
               vkGetInstanceProcAddr ::
               VkInstance -- ^ instance
                          ->
                 Foreign.Ptr.Ptr Foreign.C.Types.CChar -- ^ pName
                                                       -> IO PFN_vkVoidFunction

-- | > void vkGetPhysicalDeviceProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceProperties.html vkGetPhysicalDeviceProperties registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceProperties"
               vkGetPhysicalDeviceProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr VkPhysicalDeviceProperties -- ^ pProperties
                                                            -> IO ()

-- | > void vkGetPhysicalDeviceQueueFamilyProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceQueueFamilyProperties.html vkGetPhysicalDeviceQueueFamilyProperties registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceQueueFamilyProperties"
               vkGetPhysicalDeviceQueueFamilyProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pQueueFamilyPropertyCount
                                                  ->
                   Foreign.Ptr.Ptr VkQueueFamilyProperties -- ^ pQueueFamilyProperties
                                                           -> IO ()

-- | > void vkGetPhysicalDeviceMemoryProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceMemoryProperties.html vkGetPhysicalDeviceMemoryProperties registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceMemoryProperties"
               vkGetPhysicalDeviceMemoryProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr VkPhysicalDeviceMemoryProperties -- ^ pMemoryProperties
                                                                  -> IO ()

-- | > void vkGetPhysicalDeviceFeatures
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceFeatures.html vkGetPhysicalDeviceFeatures registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceFeatures"
               vkGetPhysicalDeviceFeatures ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr VkPhysicalDeviceFeatures -- ^ pFeatures
                                                          -> IO ()

-- | > void vkGetPhysicalDeviceFormatProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceFormatProperties.html vkGetPhysicalDeviceFormatProperties registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceFormatProperties"
               vkGetPhysicalDeviceFormatProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkFormat -- ^ format
                          -> Foreign.Ptr.Ptr VkFormatProperties -- ^ pFormatProperties
                                                                -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceImageFormatProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkImageType type
--   >     , VkImageTiling tiling
--   >     , VkImageUsageFlags usage
--   >     , VkImageCreateFlags flags
--   >     , VkImageFormatProperties* pImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceImageFormatProperties.html vkGetPhysicalDeviceImageFormatProperties registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceImageFormatProperties"
               vkGetPhysicalDeviceImageFormatProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkFormat -- ^ format
                          ->
                   VkImageType -- ^ type
                               ->
                     VkImageTiling -- ^ tiling
                                   ->
                       VkImageUsageFlags -- ^ usage
                                         ->
                         VkImageCreateFlags -- ^ flags
                                            ->
                           Foreign.Ptr.Ptr VkImageFormatProperties -- ^ pImageFormatProperties
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED', 'VK_ERROR_EXTENSION_NOT_PRESENT', 'VK_ERROR_FEATURE_NOT_PRESENT', 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkCreateDevice
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkDeviceCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDevice* pDevice
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDevice.html vkCreateDevice registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDevice" vkCreateDevice ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr VkDeviceCreateInfo -- ^ pCreateInfo
                                                    ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkDevice -- ^ pDevice
                                              -> IO VkResult

-- | > void vkDestroyDevice
--   >     ( VkDevice device
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyDevice.html vkDestroyDevice registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyDevice" vkDestroyDevice ::
               VkDevice -- ^ device
                        -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                 -> IO ()

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkEnumerateInstanceLayerProperties
--   >     ( uint32_t* pPropertyCount
--   >     , VkLayerProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkEnumerateInstanceLayerProperties.html vkEnumerateInstanceLayerProperties registry at www.khronos.org>
foreign import ccall unsafe "vkEnumerateInstanceLayerProperties"
               vkEnumerateInstanceLayerProperties ::
               Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pPropertyCount
                                                ->
                 Foreign.Ptr.Ptr VkLayerProperties -- ^ pProperties
                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_LAYER_NOT_PRESENT'.
--
--   > VkResult vkEnumerateInstanceExtensionProperties
--   >     ( const char* pLayerName
--   >     , uint32_t* pPropertyCount
--   >     , VkExtensionProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkEnumerateInstanceExtensionProperties.html vkEnumerateInstanceExtensionProperties registry at www.khronos.org>
foreign import ccall unsafe
               "vkEnumerateInstanceExtensionProperties"
               vkEnumerateInstanceExtensionProperties ::
               Foreign.Ptr.Ptr Foreign.C.Types.CChar -- ^ pLayerName
                                                     ->
                 Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pPropertyCount
                                                  ->
                   Foreign.Ptr.Ptr VkExtensionProperties -- ^ pProperties
                                                         -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkEnumerateDeviceLayerProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkLayerProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkEnumerateDeviceLayerProperties.html vkEnumerateDeviceLayerProperties registry at www.khronos.org>
foreign import ccall unsafe "vkEnumerateDeviceLayerProperties"
               vkEnumerateDeviceLayerProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pPropertyCount
                                                  ->
                   Foreign.Ptr.Ptr VkLayerProperties -- ^ pProperties
                                                     -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_LAYER_NOT_PRESENT'.
--
--   > VkResult vkEnumerateDeviceExtensionProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const char* pLayerName
--   >     , uint32_t* pPropertyCount
--   >     , VkExtensionProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkEnumerateDeviceExtensionProperties.html vkEnumerateDeviceExtensionProperties registry at www.khronos.org>
foreign import ccall unsafe "vkEnumerateDeviceExtensionProperties"
               vkEnumerateDeviceExtensionProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr Foreign.C.Types.CChar -- ^ pLayerName
                                                       ->
                   Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pPropertyCount
                                                    ->
                     Foreign.Ptr.Ptr VkExtensionProperties -- ^ pProperties
                                                           -> IO VkResult

-- | > void vkGetDeviceQueue
--   >     ( VkDevice device
--   >     , uint32_t queueFamilyIndex
--   >     , uint32_t queueIndex
--   >     , VkQueue* pQueue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDeviceQueue.html vkGetDeviceQueue registry at www.khronos.org>
foreign import ccall unsafe "vkGetDeviceQueue" vkGetDeviceQueue ::
               VkDevice -- ^ device
                        ->
                 Data.Word.Word32 -- ^ queueFamilyIndex
                                  ->
                   Data.Word.Word32 -- ^ queueIndex
                                    -> Foreign.Ptr.Ptr VkQueue -- ^ pQueue
                                                               -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkQueueSubmit
--   >     ( VkQueue queue
--   >     , uint32_t submitCount
--   >     , const VkSubmitInfo* pSubmits
--   >     , VkFence fence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkQueueSubmit.html vkQueueSubmit registry at www.khronos.org>
foreign import ccall unsafe "vkQueueSubmit" vkQueueSubmit ::
               VkQueue -- ^ queue
                       ->
                 Data.Word.Word32 -- ^ submitCount
                                  ->
                   Foreign.Ptr.Ptr VkSubmitInfo -- ^ pSubmits
                                                -> VkFence -- ^ fence
                                                           -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkQueueWaitIdle
--   >     ( VkQueue queue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkQueueWaitIdle.html vkQueueWaitIdle registry at www.khronos.org>
foreign import ccall unsafe "vkQueueWaitIdle" vkQueueWaitIdle ::
               VkQueue -- ^ queue
                       -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkDeviceWaitIdle
--   >     ( VkDevice device
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDeviceWaitIdle.html vkDeviceWaitIdle registry at www.khronos.org>
foreign import ccall unsafe "vkDeviceWaitIdle" vkDeviceWaitIdle ::
               VkDevice -- ^ device
                        -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkAllocateMemory
--   >     ( VkDevice device
--   >     , const VkMemoryAllocateInfo* pAllocateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDeviceMemory* pMemory
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkAllocateMemory.html vkAllocateMemory registry at www.khronos.org>
foreign import ccall unsafe "vkAllocateMemory" vkAllocateMemory ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkMemoryAllocateInfo -- ^ pAllocateInfo
                                                      ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkDeviceMemory -- ^ pMemory
                                                    -> IO VkResult

-- | > void vkFreeMemory
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkFreeMemory.html vkFreeMemory registry at www.khronos.org>
foreign import ccall unsafe "vkFreeMemory" vkFreeMemory ::
               VkDevice -- ^ device
                        ->
                 VkDeviceMemory -- ^ memory
                                -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_MEMORY_MAP_FAILED'.
--
--   > VkResult vkMapMemory
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize offset
--   >     , VkDeviceSize size
--   >     , VkMemoryMapFlags flags
--   >     , void** ppData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkMapMemory.html vkMapMemory registry at www.khronos.org>
foreign import ccall unsafe "vkMapMemory" vkMapMemory ::
               VkDevice -- ^ device
                        ->
                 VkDeviceMemory -- ^ memory
                                ->
                   VkDeviceSize -- ^ offset
                                ->
                     VkDeviceSize -- ^ size
                                  ->
                       VkMemoryMapFlags -- ^ flags
                                        ->
                         Foreign.Ptr.Ptr (Foreign.Ptr.Ptr Data.Void.Void) -- ^ ppData
                                                                          -> IO VkResult

-- | > void vkUnmapMemory
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkUnmapMemory.html vkUnmapMemory registry at www.khronos.org>
foreign import ccall unsafe "vkUnmapMemory" vkUnmapMemory ::
               VkDevice -- ^ device
                        -> VkDeviceMemory -- ^ memory
                                          -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkFlushMappedMemoryRanges
--   >     ( VkDevice device
--   >     , uint32_t memoryRangeCount
--   >     , const VkMappedMemoryRange* pMemoryRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkFlushMappedMemoryRanges.html vkFlushMappedMemoryRanges registry at www.khronos.org>
foreign import ccall unsafe "vkFlushMappedMemoryRanges"
               vkFlushMappedMemoryRanges ::
               VkDevice -- ^ device
                        ->
                 Data.Word.Word32 -- ^ memoryRangeCount
                                  ->
                   Foreign.Ptr.Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                       -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkInvalidateMappedMemoryRanges
--   >     ( VkDevice device
--   >     , uint32_t memoryRangeCount
--   >     , const VkMappedMemoryRange* pMemoryRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkInvalidateMappedMemoryRanges.html vkInvalidateMappedMemoryRanges registry at www.khronos.org>
foreign import ccall unsafe "vkInvalidateMappedMemoryRanges"
               vkInvalidateMappedMemoryRanges ::
               VkDevice -- ^ device
                        ->
                 Data.Word.Word32 -- ^ memoryRangeCount
                                  ->
                   Foreign.Ptr.Ptr VkMappedMemoryRange -- ^ pMemoryRanges
                                                       -> IO VkResult

-- | > void vkGetDeviceMemoryCommitment
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize* pCommittedMemoryInBytes
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDeviceMemoryCommitment.html vkGetDeviceMemoryCommitment registry at www.khronos.org>
foreign import ccall unsafe "vkGetDeviceMemoryCommitment"
               vkGetDeviceMemoryCommitment ::
               VkDevice -- ^ device
                        -> VkDeviceMemory -- ^ memory
                                          -> Foreign.Ptr.Ptr VkDeviceSize -- ^ pCommittedMemoryInBytes
                                                                          -> IO ()

-- | > void vkGetBufferMemoryRequirements
--   >     ( VkDevice device
--   >     , VkBuffer buffer
--   >     , VkMemoryRequirements* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetBufferMemoryRequirements.html vkGetBufferMemoryRequirements registry at www.khronos.org>
foreign import ccall unsafe "vkGetBufferMemoryRequirements"
               vkGetBufferMemoryRequirements ::
               VkDevice -- ^ device
                        ->
                 VkBuffer -- ^ buffer
                          -> Foreign.Ptr.Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                                  -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindBufferMemory
--   >     ( VkDevice device
--   >     , VkBuffer buffer
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize memoryOffset
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkBindBufferMemory.html vkBindBufferMemory registry at www.khronos.org>
foreign import ccall unsafe "vkBindBufferMemory" vkBindBufferMemory
               ::
               VkDevice -- ^ device
                        ->
                 VkBuffer -- ^ buffer
                          -> VkDeviceMemory -- ^ memory
                                            -> VkDeviceSize -- ^ memoryOffset
                                                            -> IO VkResult

-- | > void vkGetImageMemoryRequirements
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , VkMemoryRequirements* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetImageMemoryRequirements.html vkGetImageMemoryRequirements registry at www.khronos.org>
foreign import ccall unsafe "vkGetImageMemoryRequirements"
               vkGetImageMemoryRequirements ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         -> Foreign.Ptr.Ptr VkMemoryRequirements -- ^ pMemoryRequirements
                                                                 -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindImageMemory
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , VkDeviceMemory memory
--   >     , VkDeviceSize memoryOffset
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkBindImageMemory.html vkBindImageMemory registry at www.khronos.org>
foreign import ccall unsafe "vkBindImageMemory" vkBindImageMemory
               ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         -> VkDeviceMemory -- ^ memory
                                           -> VkDeviceSize -- ^ memoryOffset
                                                           -> IO VkResult

-- | > void vkGetImageSparseMemoryRequirements
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , uint32_t* pSparseMemoryRequirementCount
--   >     , VkSparseImageMemoryRequirements* pSparseMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetImageSparseMemoryRequirements.html vkGetImageSparseMemoryRequirements registry at www.khronos.org>
foreign import ccall unsafe "vkGetImageSparseMemoryRequirements"
               vkGetImageSparseMemoryRequirements ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         ->
                   Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pSparseMemoryRequirementCount
                                                    ->
                     Foreign.Ptr.Ptr VkSparseImageMemoryRequirements -- ^ pSparseMemoryRequirements
                                                                     -> IO ()

-- | > void vkGetPhysicalDeviceSparseImageFormatProperties
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkImageType type
--   >     , VkSampleCountFlagBits samples
--   >     , VkImageUsageFlags usage
--   >     , VkImageTiling tiling
--   >     , uint32_t* pPropertyCount
--   >     , VkSparseImageFormatProperties* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceSparseImageFormatProperties.html vkGetPhysicalDeviceSparseImageFormatProperties registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceSparseImageFormatProperties"
               vkGetPhysicalDeviceSparseImageFormatProperties ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkFormat -- ^ format
                          ->
                   VkImageType -- ^ type
                               ->
                     VkSampleCountFlagBits -- ^ samples
                                           ->
                       VkImageUsageFlags -- ^ usage
                                         ->
                         VkImageTiling -- ^ tiling
                                       ->
                           Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pPropertyCount
                                                            ->
                             Foreign.Ptr.Ptr VkSparseImageFormatProperties -- ^ pProperties
                                                                           -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   queues: @sparse_binding@
--
--   > VkResult vkQueueBindSparse
--   >     ( VkQueue queue
--   >     , uint32_t bindInfoCount
--   >     , const VkBindSparseInfo* pBindInfo
--   >     , VkFence fence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkQueueBindSparse.html vkQueueBindSparse registry at www.khronos.org>
foreign import ccall unsafe "vkQueueBindSparse" vkQueueBindSparse
               ::
               VkQueue -- ^ queue
                       ->
                 Data.Word.Word32 -- ^ bindInfoCount
                                  ->
                   Foreign.Ptr.Ptr VkBindSparseInfo -- ^ pBindInfo
                                                    -> VkFence -- ^ fence
                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateFence
--   >     ( VkDevice device
--   >     , const VkFenceCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFence* pFence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateFence.html vkCreateFence registry at www.khronos.org>
foreign import ccall unsafe "vkCreateFence" vkCreateFence ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkFenceCreateInfo -- ^ pCreateInfo
                                                   ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkFence -- ^ pFence
                                             -> IO VkResult

-- | > void vkDestroyFence
--   >     ( VkDevice device
--   >     , VkFence fence
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyFence.html vkDestroyFence registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyFence" vkDestroyFence ::
               VkDevice -- ^ device
                        ->
                 VkFence -- ^ fence
                         -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                  -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetFences
--   >     ( VkDevice device
--   >     , uint32_t fenceCount
--   >     , const VkFence* pFences
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkResetFences.html vkResetFences registry at www.khronos.org>
foreign import ccall unsafe "vkResetFences" vkResetFences ::
               VkDevice -- ^ device
                        ->
                 Data.Word.Word32 -- ^ fenceCount
                                  -> Foreign.Ptr.Ptr VkFence -- ^ pFences
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_NOT_READY'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkGetFenceStatus
--   >     ( VkDevice device
--   >     , VkFence fence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetFenceStatus.html vkGetFenceStatus registry at www.khronos.org>
foreign import ccall unsafe "vkGetFenceStatus" vkGetFenceStatus ::
               VkDevice -- ^ device
                        -> VkFence -- ^ fence
                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_TIMEOUT'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkWaitForFences
--   >     ( VkDevice device
--   >     , uint32_t fenceCount
--   >     , const VkFence* pFences
--   >     , VkBool32 waitAll
--   >     , uint64_t timeout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkWaitForFences.html vkWaitForFences registry at www.khronos.org>
foreign import ccall unsafe "vkWaitForFences" vkWaitForFences ::
               VkDevice -- ^ device
                        ->
                 Data.Word.Word32 -- ^ fenceCount
                                  ->
                   Foreign.Ptr.Ptr VkFence -- ^ pFences
                                           ->
                     VkBool32 -- ^ waitAll
                              -> Data.Word.Word64 -- ^ timeout
                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateSemaphore
--   >     ( VkDevice device
--   >     , const VkSemaphoreCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSemaphore* pSemaphore
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateSemaphore.html vkCreateSemaphore registry at www.khronos.org>
foreign import ccall unsafe "vkCreateSemaphore" vkCreateSemaphore
               ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkSemaphoreCreateInfo -- ^ pCreateInfo
                                                       ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkSemaphore -- ^ pSemaphore
                                                 -> IO VkResult

-- | > void vkDestroySemaphore
--   >     ( VkDevice device
--   >     , VkSemaphore semaphore
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroySemaphore.html vkDestroySemaphore registry at www.khronos.org>
foreign import ccall unsafe "vkDestroySemaphore" vkDestroySemaphore
               ::
               VkDevice -- ^ device
                        ->
                 VkSemaphore -- ^ semaphore
                             -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateEvent
--   >     ( VkDevice device
--   >     , const VkEventCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkEvent* pEvent
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateEvent.html vkCreateEvent registry at www.khronos.org>
foreign import ccall unsafe "vkCreateEvent" vkCreateEvent ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkEventCreateInfo -- ^ pCreateInfo
                                                   ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkEvent -- ^ pEvent
                                             -> IO VkResult

-- | > void vkDestroyEvent
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyEvent.html vkDestroyEvent registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyEvent" vkDestroyEvent ::
               VkDevice -- ^ device
                        ->
                 VkEvent -- ^ event
                         -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                  -> IO ()

-- | Success codes: 'VK_EVENT_SET', 'VK_EVENT_RESET'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkGetEventStatus
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetEventStatus.html vkGetEventStatus registry at www.khronos.org>
foreign import ccall unsafe "vkGetEventStatus" vkGetEventStatus ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkSetEvent
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkSetEvent.html vkSetEvent registry at www.khronos.org>
foreign import ccall unsafe "vkSetEvent" vkSetEvent ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetEvent
--   >     ( VkDevice device
--   >     , VkEvent event
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkResetEvent.html vkResetEvent registry at www.khronos.org>
foreign import ccall unsafe "vkResetEvent" vkResetEvent ::
               VkDevice -- ^ device
                        -> VkEvent -- ^ event
                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateQueryPool
--   >     ( VkDevice device
--   >     , const VkQueryPoolCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkQueryPool* pQueryPool
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateQueryPool.html vkCreateQueryPool registry at www.khronos.org>
foreign import ccall unsafe "vkCreateQueryPool" vkCreateQueryPool
               ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkQueryPoolCreateInfo -- ^ pCreateInfo
                                                       ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkQueryPool -- ^ pQueryPool
                                                 -> IO VkResult

-- | > void vkDestroyQueryPool
--   >     ( VkDevice device
--   >     , VkQueryPool queryPool
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyQueryPool.html vkDestroyQueryPool registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyQueryPool" vkDestroyQueryPool
               ::
               VkDevice -- ^ device
                        ->
                 VkQueryPool -- ^ queryPool
                             -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()

-- | Success codes: 'VK_SUCCESS', 'VK_NOT_READY'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkGetQueryPoolResults
--   >     ( VkDevice device
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     , uint32_t queryCount
--   >     , size_t dataSize
--   >     , void* pData
--   >     , VkDeviceSize stride
--   >     , VkQueryResultFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetQueryPoolResults.html vkGetQueryPoolResults registry at www.khronos.org>
foreign import ccall unsafe "vkGetQueryPoolResults"
               vkGetQueryPoolResults ::
               VkDevice -- ^ device
                        ->
                 VkQueryPool -- ^ queryPool
                             ->
                   Data.Word.Word32 -- ^ firstQuery
                                    ->
                     Data.Word.Word32 -- ^ queryCount
                                      ->
                       Foreign.C.Types.CSize -- ^ dataSize
                                             ->
                         Foreign.Ptr.Ptr Data.Void.Void -- ^ pData
                                                        ->
                           VkDeviceSize -- ^ stride
                                        -> VkQueryResultFlags -- ^ flags
                                                              -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateBuffer
--   >     ( VkDevice device
--   >     , const VkBufferCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkBuffer* pBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateBuffer.html vkCreateBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkCreateBuffer" vkCreateBuffer ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkBufferCreateInfo -- ^ pCreateInfo
                                                    ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkBuffer -- ^ pBuffer
                                              -> IO VkResult

-- | > void vkDestroyBuffer
--   >     ( VkDevice device
--   >     , VkBuffer buffer
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyBuffer.html vkDestroyBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyBuffer" vkDestroyBuffer ::
               VkDevice -- ^ device
                        ->
                 VkBuffer -- ^ buffer
                          -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                   -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateBufferView
--   >     ( VkDevice device
--   >     , const VkBufferViewCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkBufferView* pView
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateBufferView.html vkCreateBufferView registry at www.khronos.org>
foreign import ccall unsafe "vkCreateBufferView" vkCreateBufferView
               ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkBufferViewCreateInfo -- ^ pCreateInfo
                                                        ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkBufferView -- ^ pView
                                                  -> IO VkResult

-- | > void vkDestroyBufferView
--   >     ( VkDevice device
--   >     , VkBufferView bufferView
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyBufferView.html vkDestroyBufferView registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyBufferView"
               vkDestroyBufferView ::
               VkDevice -- ^ device
                        ->
                 VkBufferView -- ^ bufferView
                              -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateImage
--   >     ( VkDevice device
--   >     , const VkImageCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkImage* pImage
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateImage.html vkCreateImage registry at www.khronos.org>
foreign import ccall unsafe "vkCreateImage" vkCreateImage ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkImageCreateInfo -- ^ pCreateInfo
                                                   ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkImage -- ^ pImage
                                             -> IO VkResult

-- | > void vkDestroyImage
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyImage.html vkDestroyImage registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyImage" vkDestroyImage ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                  -> IO ()

-- | > void vkGetImageSubresourceLayout
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , const VkImageSubresource* pSubresource
--   >     , VkSubresourceLayout* pLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetImageSubresourceLayout.html vkGetImageSubresourceLayout registry at www.khronos.org>
foreign import ccall unsafe "vkGetImageSubresourceLayout"
               vkGetImageSubresourceLayout ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         ->
                   Foreign.Ptr.Ptr VkImageSubresource -- ^ pSubresource
                                                      ->
                     Foreign.Ptr.Ptr VkSubresourceLayout -- ^ pLayout
                                                         -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateImageView
--   >     ( VkDevice device
--   >     , const VkImageViewCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkImageView* pView
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateImageView.html vkCreateImageView registry at www.khronos.org>
foreign import ccall unsafe "vkCreateImageView" vkCreateImageView
               ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkImageViewCreateInfo -- ^ pCreateInfo
                                                       ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkImageView -- ^ pView
                                                 -> IO VkResult

-- | > void vkDestroyImageView
--   >     ( VkDevice device
--   >     , VkImageView imageView
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyImageView.html vkDestroyImageView registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyImageView" vkDestroyImageView
               ::
               VkDevice -- ^ device
                        ->
                 VkImageView -- ^ imageView
                             -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                      -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
--   > VkResult vkCreateShaderModule
--   >     ( VkDevice device
--   >     , const VkShaderModuleCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkShaderModule* pShaderModule
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateShaderModule.html vkCreateShaderModule registry at www.khronos.org>
foreign import ccall unsafe "vkCreateShaderModule"
               vkCreateShaderModule ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkShaderModuleCreateInfo -- ^ pCreateInfo
                                                          ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkShaderModule -- ^ pShaderModule
                                                    -> IO VkResult

-- | > void vkDestroyShaderModule
--   >     ( VkDevice device
--   >     , VkShaderModule shaderModule
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyShaderModule.html vkDestroyShaderModule registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyShaderModule"
               vkDestroyShaderModule ::
               VkDevice -- ^ device
                        ->
                 VkShaderModule -- ^ shaderModule
                                -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreatePipelineCache
--   >     ( VkDevice device
--   >     , const VkPipelineCacheCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipelineCache* pPipelineCache
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreatePipelineCache.html vkCreatePipelineCache registry at www.khronos.org>
foreign import ccall unsafe "vkCreatePipelineCache"
               vkCreatePipelineCache ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkPipelineCacheCreateInfo -- ^ pCreateInfo
                                                           ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkPipelineCache -- ^ pPipelineCache
                                                     -> IO VkResult

-- | > void vkDestroyPipelineCache
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyPipelineCache.html vkDestroyPipelineCache registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyPipelineCache"
               vkDestroyPipelineCache ::
               VkDevice -- ^ device
                        ->
                 VkPipelineCache -- ^ pipelineCache
                                 -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                          -> IO ()

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPipelineCacheData
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , size_t* pDataSize
--   >     , void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPipelineCacheData.html vkGetPipelineCacheData registry at www.khronos.org>
foreign import ccall unsafe "vkGetPipelineCacheData"
               vkGetPipelineCacheData ::
               VkDevice -- ^ device
                        ->
                 VkPipelineCache -- ^ pipelineCache
                                 ->
                   Foreign.Ptr.Ptr Foreign.C.Types.CSize -- ^ pDataSize
                                                         ->
                     Foreign.Ptr.Ptr Data.Void.Void -- ^ pData
                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkMergePipelineCaches
--   >     ( VkDevice device
--   >     , VkPipelineCache dstCache
--   >     , uint32_t srcCacheCount
--   >     , const VkPipelineCache* pSrcCaches
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkMergePipelineCaches.html vkMergePipelineCaches registry at www.khronos.org>
foreign import ccall unsafe "vkMergePipelineCaches"
               vkMergePipelineCaches ::
               VkDevice -- ^ device
                        ->
                 VkPipelineCache -- ^ dstCache
                                 ->
                   Data.Word.Word32 -- ^ srcCacheCount
                                    -> Foreign.Ptr.Ptr VkPipelineCache -- ^ pSrcCaches
                                                                       -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
--   > VkResult vkCreateGraphicsPipelines
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , uint32_t createInfoCount
--   >     , const VkGraphicsPipelineCreateInfo* pCreateInfos
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipeline* pPipelines
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateGraphicsPipelines.html vkCreateGraphicsPipelines registry at www.khronos.org>
foreign import ccall unsafe "vkCreateGraphicsPipelines"
               vkCreateGraphicsPipelines ::
               VkDevice -- ^ device
                        ->
                 VkPipelineCache -- ^ pipelineCache
                                 ->
                   Data.Word.Word32 -- ^ createInfoCount
                                    ->
                     Foreign.Ptr.Ptr VkGraphicsPipelineCreateInfo -- ^ pCreateInfos
                                                                  ->
                       Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                             ->
                         Foreign.Ptr.Ptr VkPipeline -- ^ pPipelines
                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INVALID_SHADER_NV'.
--
--   > VkResult vkCreateComputePipelines
--   >     ( VkDevice device
--   >     , VkPipelineCache pipelineCache
--   >     , uint32_t createInfoCount
--   >     , const VkComputePipelineCreateInfo* pCreateInfos
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipeline* pPipelines
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateComputePipelines.html vkCreateComputePipelines registry at www.khronos.org>
foreign import ccall unsafe "vkCreateComputePipelines"
               vkCreateComputePipelines ::
               VkDevice -- ^ device
                        ->
                 VkPipelineCache -- ^ pipelineCache
                                 ->
                   Data.Word.Word32 -- ^ createInfoCount
                                    ->
                     Foreign.Ptr.Ptr VkComputePipelineCreateInfo -- ^ pCreateInfos
                                                                 ->
                       Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                             ->
                         Foreign.Ptr.Ptr VkPipeline -- ^ pPipelines
                                                    -> IO VkResult

-- | > void vkDestroyPipeline
--   >     ( VkDevice device
--   >     , VkPipeline pipeline
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyPipeline.html vkDestroyPipeline registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyPipeline" vkDestroyPipeline
               ::
               VkDevice -- ^ device
                        ->
                 VkPipeline -- ^ pipeline
                            -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                     -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreatePipelineLayout
--   >     ( VkDevice device
--   >     , const VkPipelineLayoutCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkPipelineLayout* pPipelineLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreatePipelineLayout.html vkCreatePipelineLayout registry at www.khronos.org>
foreign import ccall unsafe "vkCreatePipelineLayout"
               vkCreatePipelineLayout ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkPipelineLayoutCreateInfo -- ^ pCreateInfo
                                                            ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkPipelineLayout -- ^ pPipelineLayout
                                                      -> IO VkResult

-- | > void vkDestroyPipelineLayout
--   >     ( VkDevice device
--   >     , VkPipelineLayout pipelineLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyPipelineLayout.html vkDestroyPipelineLayout registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyPipelineLayout"
               vkDestroyPipelineLayout ::
               VkDevice -- ^ device
                        ->
                 VkPipelineLayout -- ^ pipelineLayout
                                  -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                           -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_TOO_MANY_OBJECTS'.
--
--   > VkResult vkCreateSampler
--   >     ( VkDevice device
--   >     , const VkSamplerCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSampler* pSampler
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateSampler.html vkCreateSampler registry at www.khronos.org>
foreign import ccall unsafe "vkCreateSampler" vkCreateSampler ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkSamplerCreateInfo -- ^ pCreateInfo
                                                     ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkSampler -- ^ pSampler
                                               -> IO VkResult

-- | > void vkDestroySampler
--   >     ( VkDevice device
--   >     , VkSampler sampler
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroySampler.html vkDestroySampler registry at www.khronos.org>
foreign import ccall unsafe "vkDestroySampler" vkDestroySampler ::
               VkDevice -- ^ device
                        ->
                 VkSampler -- ^ sampler
                           -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                    -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDescriptorSetLayout
--   >     ( VkDevice device
--   >     , const VkDescriptorSetLayoutCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDescriptorSetLayout* pSetLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDescriptorSetLayout.html vkCreateDescriptorSetLayout registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDescriptorSetLayout"
               vkCreateDescriptorSetLayout ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkDescriptorSetLayoutCreateInfo -- ^ pCreateInfo
                                                                 ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkDescriptorSetLayout -- ^ pSetLayout
                                                           -> IO VkResult

-- | > void vkDestroyDescriptorSetLayout
--   >     ( VkDevice device
--   >     , VkDescriptorSetLayout descriptorSetLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyDescriptorSetLayout.html vkDestroyDescriptorSetLayout registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyDescriptorSetLayout"
               vkDestroyDescriptorSetLayout ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorSetLayout -- ^ descriptorSetLayout
                                       ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDescriptorPool
--   >     ( VkDevice device
--   >     , const VkDescriptorPoolCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDescriptorPool* pDescriptorPool
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDescriptorPool.html vkCreateDescriptorPool registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDescriptorPool"
               vkCreateDescriptorPool ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkDescriptorPoolCreateInfo -- ^ pCreateInfo
                                                            ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkDescriptorPool -- ^ pDescriptorPool
                                                      -> IO VkResult

-- | > void vkDestroyDescriptorPool
--   >     ( VkDevice device
--   >     , VkDescriptorPool descriptorPool
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyDescriptorPool.html vkDestroyDescriptorPool registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyDescriptorPool"
               vkDestroyDescriptorPool ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorPool -- ^ descriptorPool
                                  -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                           -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetDescriptorPool
--   >     ( VkDevice device
--   >     , VkDescriptorPool descriptorPool
--   >     , VkDescriptorPoolResetFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkResetDescriptorPool.html vkResetDescriptorPool registry at www.khronos.org>
foreign import ccall unsafe "vkResetDescriptorPool"
               vkResetDescriptorPool ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorPool -- ^ descriptorPool
                                  -> VkDescriptorPoolResetFlags -- ^ flags
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FRAGMENTED_POOL', 'VK_ERROR_OUT_OF_POOL_MEMORY_KHR'.
--
--   > VkResult vkAllocateDescriptorSets
--   >     ( VkDevice device
--   >     , const VkDescriptorSetAllocateInfo* pAllocateInfo
--   >     , VkDescriptorSet* pDescriptorSets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkAllocateDescriptorSets.html vkAllocateDescriptorSets registry at www.khronos.org>
foreign import ccall unsafe "vkAllocateDescriptorSets"
               vkAllocateDescriptorSets ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkDescriptorSetAllocateInfo -- ^ pAllocateInfo
                                                             ->
                   Foreign.Ptr.Ptr VkDescriptorSet -- ^ pDescriptorSets
                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkFreeDescriptorSets
--   >     ( VkDevice device
--   >     , VkDescriptorPool descriptorPool
--   >     , uint32_t descriptorSetCount
--   >     , const VkDescriptorSet* pDescriptorSets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkFreeDescriptorSets.html vkFreeDescriptorSets registry at www.khronos.org>
foreign import ccall unsafe "vkFreeDescriptorSets"
               vkFreeDescriptorSets ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorPool -- ^ descriptorPool
                                  ->
                   Data.Word.Word32 -- ^ descriptorSetCount
                                    -> Foreign.Ptr.Ptr VkDescriptorSet -- ^ pDescriptorSets
                                                                       -> IO VkResult

-- | > void vkUpdateDescriptorSets
--   >     ( VkDevice device
--   >     , uint32_t descriptorWriteCount
--   >     , const VkWriteDescriptorSet* pDescriptorWrites
--   >     , uint32_t descriptorCopyCount
--   >     , const VkCopyDescriptorSet* pDescriptorCopies
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkUpdateDescriptorSets.html vkUpdateDescriptorSets registry at www.khronos.org>
foreign import ccall unsafe "vkUpdateDescriptorSets"
               vkUpdateDescriptorSets ::
               VkDevice -- ^ device
                        ->
                 Data.Word.Word32 -- ^ descriptorWriteCount
                                  ->
                   Foreign.Ptr.Ptr VkWriteDescriptorSet -- ^ pDescriptorWrites
                                                        ->
                     Data.Word.Word32 -- ^ descriptorCopyCount
                                      -> Foreign.Ptr.Ptr VkCopyDescriptorSet -- ^ pDescriptorCopies
                                                                             -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateFramebuffer
--   >     ( VkDevice device
--   >     , const VkFramebufferCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFramebuffer* pFramebuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateFramebuffer.html vkCreateFramebuffer registry at www.khronos.org>
foreign import ccall unsafe "vkCreateFramebuffer"
               vkCreateFramebuffer ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkFramebufferCreateInfo -- ^ pCreateInfo
                                                         ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkFramebuffer -- ^ pFramebuffer
                                                   -> IO VkResult

-- | > void vkDestroyFramebuffer
--   >     ( VkDevice device
--   >     , VkFramebuffer framebuffer
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyFramebuffer.html vkDestroyFramebuffer registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyFramebuffer"
               vkDestroyFramebuffer ::
               VkDevice -- ^ device
                        ->
                 VkFramebuffer -- ^ framebuffer
                               -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                        -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateRenderPass
--   >     ( VkDevice device
--   >     , const VkRenderPassCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkRenderPass* pRenderPass
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateRenderPass.html vkCreateRenderPass registry at www.khronos.org>
foreign import ccall unsafe "vkCreateRenderPass" vkCreateRenderPass
               ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkRenderPassCreateInfo -- ^ pCreateInfo
                                                        ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkRenderPass -- ^ pRenderPass
                                                  -> IO VkResult

-- | > void vkDestroyRenderPass
--   >     ( VkDevice device
--   >     , VkRenderPass renderPass
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyRenderPass.html vkDestroyRenderPass registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyRenderPass"
               vkDestroyRenderPass ::
               VkDevice -- ^ device
                        ->
                 VkRenderPass -- ^ renderPass
                              -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

-- | > void vkGetRenderAreaGranularity
--   >     ( VkDevice device
--   >     , VkRenderPass renderPass
--   >     , VkExtent2D* pGranularity
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetRenderAreaGranularity.html vkGetRenderAreaGranularity registry at www.khronos.org>
foreign import ccall unsafe "vkGetRenderAreaGranularity"
               vkGetRenderAreaGranularity ::
               VkDevice -- ^ device
                        -> VkRenderPass -- ^ renderPass
                                        -> Foreign.Ptr.Ptr VkExtent2D -- ^ pGranularity
                                                                      -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateCommandPool
--   >     ( VkDevice device
--   >     , const VkCommandPoolCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkCommandPool* pCommandPool
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateCommandPool.html vkCreateCommandPool registry at www.khronos.org>
foreign import ccall unsafe "vkCreateCommandPool"
               vkCreateCommandPool ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkCommandPoolCreateInfo -- ^ pCreateInfo
                                                         ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkCommandPool -- ^ pCommandPool
                                                   -> IO VkResult

-- | > void vkDestroyCommandPool
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyCommandPool.html vkDestroyCommandPool registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyCommandPool"
               vkDestroyCommandPool ::
               VkDevice -- ^ device
                        ->
                 VkCommandPool -- ^ commandPool
                               -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                        -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetCommandPool
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , VkCommandPoolResetFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkResetCommandPool.html vkResetCommandPool registry at www.khronos.org>
foreign import ccall unsafe "vkResetCommandPool" vkResetCommandPool
               ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> VkCommandPoolResetFlags -- ^ flags
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkAllocateCommandBuffers
--   >     ( VkDevice device
--   >     , const VkCommandBufferAllocateInfo* pAllocateInfo
--   >     , VkCommandBuffer* pCommandBuffers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkAllocateCommandBuffers.html vkAllocateCommandBuffers registry at www.khronos.org>
foreign import ccall unsafe "vkAllocateCommandBuffers"
               vkAllocateCommandBuffers ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkCommandBufferAllocateInfo -- ^ pAllocateInfo
                                                             ->
                   Foreign.Ptr.Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                   -> IO VkResult

-- | > void vkFreeCommandBuffers
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , uint32_t commandBufferCount
--   >     , const VkCommandBuffer* pCommandBuffers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkFreeCommandBuffers.html vkFreeCommandBuffers registry at www.khronos.org>
foreign import ccall unsafe "vkFreeCommandBuffers"
               vkFreeCommandBuffers ::
               VkDevice -- ^ device
                        ->
                 VkCommandPool -- ^ commandPool
                               ->
                   Data.Word.Word32 -- ^ commandBufferCount
                                    -> Foreign.Ptr.Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                                       -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBeginCommandBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCommandBufferBeginInfo* pBeginInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkBeginCommandBuffer.html vkBeginCommandBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkBeginCommandBuffer"
               vkBeginCommandBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Foreign.Ptr.Ptr VkCommandBufferBeginInfo -- ^ pBeginInfo
                                                          -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkEndCommandBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkEndCommandBuffer.html vkEndCommandBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkEndCommandBuffer" vkEndCommandBuffer
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkResetCommandBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkCommandBufferResetFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkResetCommandBuffer.html vkResetCommandBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkResetCommandBuffer"
               vkResetCommandBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkCommandBufferResetFlags -- ^ flags
                                                            -> IO VkResult

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdBindPipeline
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineBindPoint pipelineBindPoint
--   >     , VkPipeline pipeline
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdBindPipeline.html vkCmdBindPipeline registry at www.khronos.org>
foreign import ccall unsafe "vkCmdBindPipeline" vkCmdBindPipeline
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> VkPipelineBindPoint -- ^ pipelineBindPoint
                                                         -> VkPipeline -- ^ pipeline
                                                                       -> IO ()

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetViewport
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstViewport
--   >     , uint32_t viewportCount
--   >     , const VkViewport* pViewports
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetViewport.html vkCmdSetViewport registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetViewport" vkCmdSetViewport ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Data.Word.Word32 -- ^ firstViewport
                                  ->
                   Data.Word.Word32 -- ^ viewportCount
                                    -> Foreign.Ptr.Ptr VkViewport -- ^ pViewports
                                                                  -> IO ()

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetScissor
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstScissor
--   >     , uint32_t scissorCount
--   >     , const VkRect2D* pScissors
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetScissor.html vkCmdSetScissor registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetScissor" vkCmdSetScissor ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Data.Word.Word32 -- ^ firstScissor
                                  ->
                   Data.Word.Word32 -- ^ scissorCount
                                    -> Foreign.Ptr.Ptr VkRect2D -- ^ pScissors
                                                                -> IO ()

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetLineWidth
--   >     ( VkCommandBuffer commandBuffer
--   >     , float lineWidth
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetLineWidth.html vkCmdSetLineWidth registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetLineWidth" vkCmdSetLineWidth
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> Foreign.C.Types.CFloat -- ^ lineWidth
                                                            -> IO ()

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetDepthBias
--   >     ( VkCommandBuffer commandBuffer
--   >     , float depthBiasConstantFactor
--   >     , float depthBiasClamp
--   >     , float depthBiasSlopeFactor
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetDepthBias.html vkCmdSetDepthBias registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetDepthBias" vkCmdSetDepthBias
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Foreign.C.Types.CFloat -- ^ depthBiasConstantFactor
                                        ->
                   Foreign.C.Types.CFloat -- ^ depthBiasClamp
                                          -> Foreign.C.Types.CFloat -- ^ depthBiasSlopeFactor
                                                                    -> IO ()

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetBlendConstants
--   >     ( VkCommandBuffer commandBuffer
--   >     , const float blendConstants[4]
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetBlendConstants.html vkCmdSetBlendConstants registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetBlendConstants"
               vkCmdSetBlendConstants ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Foreign.Ptr.Ptr Foreign.C.Types.CFloat -- ^ blendConstants
                                                                         -> IO ()

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetDepthBounds
--   >     ( VkCommandBuffer commandBuffer
--   >     , float minDepthBounds
--   >     , float maxDepthBounds
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetDepthBounds.html vkCmdSetDepthBounds registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetDepthBounds"
               vkCmdSetDepthBounds ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Foreign.C.Types.CFloat -- ^ minDepthBounds
                                        -> Foreign.C.Types.CFloat -- ^ maxDepthBounds
                                                                  -> IO ()

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetStencilCompareMask
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkStencilFaceFlags faceMask
--   >     , uint32_t compareMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetStencilCompareMask.html vkCmdSetStencilCompareMask registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetStencilCompareMask"
               vkCmdSetStencilCompareMask ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkStencilFaceFlags -- ^ faceMask
                                                     -> Data.Word.Word32 -- ^ compareMask
                                                                         -> IO ()

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetStencilWriteMask
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkStencilFaceFlags faceMask
--   >     , uint32_t writeMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetStencilWriteMask.html vkCmdSetStencilWriteMask registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetStencilWriteMask"
               vkCmdSetStencilWriteMask ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkStencilFaceFlags -- ^ faceMask
                                                     -> Data.Word.Word32 -- ^ writeMask
                                                                         -> IO ()

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetStencilReference
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkStencilFaceFlags faceMask
--   >     , uint32_t reference
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetStencilReference.html vkCmdSetStencilReference registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetStencilReference"
               vkCmdSetStencilReference ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkStencilFaceFlags -- ^ faceMask
                                                     -> Data.Word.Word32 -- ^ reference
                                                                         -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdBindDescriptorSets
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineBindPoint pipelineBindPoint
--   >     , VkPipelineLayout layout
--   >     , uint32_t firstSet
--   >     , uint32_t descriptorSetCount
--   >     , const VkDescriptorSet* pDescriptorSets
--   >     , uint32_t dynamicOffsetCount
--   >     , const uint32_t* pDynamicOffsets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdBindDescriptorSets.html vkCmdBindDescriptorSets registry at www.khronos.org>
foreign import ccall unsafe "vkCmdBindDescriptorSets"
               vkCmdBindDescriptorSets ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineBindPoint -- ^ pipelineBindPoint
                                     ->
                   VkPipelineLayout -- ^ layout
                                    ->
                     Data.Word.Word32 -- ^ firstSet
                                      ->
                       Data.Word.Word32 -- ^ descriptorSetCount
                                        ->
                         Foreign.Ptr.Ptr VkDescriptorSet -- ^ pDescriptorSets
                                                         ->
                           Data.Word.Word32 -- ^ dynamicOffsetCount
                                            -> Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pDynamicOffsets
                                                                                -> IO ()

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdBindIndexBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkIndexType indexType
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdBindIndexBuffer.html vkCmdBindIndexBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkCmdBindIndexBuffer"
               vkCmdBindIndexBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkBuffer -- ^ buffer
                                           -> VkDeviceSize -- ^ offset
                                                           -> VkIndexType -- ^ indexType
                                                                          -> IO ()

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdBindVertexBuffers
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstBinding
--   >     , uint32_t bindingCount
--   >     , const VkBuffer* pBuffers
--   >     , const VkDeviceSize* pOffsets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdBindVertexBuffers.html vkCmdBindVertexBuffers registry at www.khronos.org>
foreign import ccall unsafe "vkCmdBindVertexBuffers"
               vkCmdBindVertexBuffers ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Data.Word.Word32 -- ^ firstBinding
                                  ->
                   Data.Word.Word32 -- ^ bindingCount
                                    ->
                     Foreign.Ptr.Ptr VkBuffer -- ^ pBuffers
                                              -> Foreign.Ptr.Ptr VkDeviceSize -- ^ pOffsets
                                                                              -> IO ()

-- | queues: @graphics@
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > void vkCmdDraw
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t vertexCount
--   >     , uint32_t instanceCount
--   >     , uint32_t firstVertex
--   >     , uint32_t firstInstance
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDraw.html vkCmdDraw registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDraw" vkCmdDraw ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Data.Word.Word32 -- ^ vertexCount
                                  ->
                   Data.Word.Word32 -- ^ instanceCount
                                    -> Data.Word.Word32 -- ^ firstVertex
                                                        -> Data.Word.Word32 -- ^ firstInstance
                                                                            -> IO ()

-- | queues: @graphics@
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > void vkCmdDrawIndexed
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t indexCount
--   >     , uint32_t instanceCount
--   >     , uint32_t firstIndex
--   >     , int32_t vertexOffset
--   >     , uint32_t firstInstance
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDrawIndexed.html vkCmdDrawIndexed registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDrawIndexed" vkCmdDrawIndexed ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Data.Word.Word32 -- ^ indexCount
                                  ->
                   Data.Word.Word32 -- ^ instanceCount
                                    ->
                     Data.Word.Word32 -- ^ firstIndex
                                      -> Data.Int.Int32 -- ^ vertexOffset
                                                        -> Data.Word.Word32 -- ^ firstInstance
                                                                            -> IO ()

-- | queues: @graphics@
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > void vkCmdDrawIndirect
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , uint32_t drawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDrawIndirect.html vkCmdDrawIndirect registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDrawIndirect" vkCmdDrawIndirect
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          ->
                   VkDeviceSize -- ^ offset
                                -> Data.Word.Word32 -- ^ drawCount
                                                    -> Data.Word.Word32 -- ^ stride
                                                                        -> IO ()

-- | queues: @graphics@
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > void vkCmdDrawIndexedIndirect
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , uint32_t drawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDrawIndexedIndirect.html vkCmdDrawIndexedIndirect registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDrawIndexedIndirect"
               vkCmdDrawIndexedIndirect ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          ->
                   VkDeviceSize -- ^ offset
                                -> Data.Word.Word32 -- ^ drawCount
                                                    -> Data.Word.Word32 -- ^ stride
                                                                        -> IO ()

-- | queues: @compute@
--
--   renderpass: @outside@
--
--   pipeline: @compute@
--
--   > void vkCmdDispatch
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t groupCountX
--   >     , uint32_t groupCountY
--   >     , uint32_t groupCountZ
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDispatch.html vkCmdDispatch registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDispatch" vkCmdDispatch ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Data.Word.Word32 -- ^ groupCountX
                                  -> Data.Word.Word32 -- ^ groupCountY
                                                      -> Data.Word.Word32 -- ^ groupCountZ
                                                                          -> IO ()

-- | queues: @compute@
--
--   renderpass: @outside@
--
--   pipeline: @compute@
--
--   > void vkCmdDispatchIndirect
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDispatchIndirect.html vkCmdDispatchIndirect registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDispatchIndirect"
               vkCmdDispatchIndirect ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkBuffer -- ^ buffer
                                           -> VkDeviceSize -- ^ offset
                                                           -> IO ()

-- | queues: @transfer,graphics,compute@
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > void vkCmdCopyBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer srcBuffer
--   >     , VkBuffer dstBuffer
--   >     , uint32_t regionCount
--   >     , const VkBufferCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdCopyBuffer.html vkCmdCopyBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkCmdCopyBuffer" vkCmdCopyBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ srcBuffer
                          ->
                   VkBuffer -- ^ dstBuffer
                            ->
                     Data.Word.Word32 -- ^ regionCount
                                      -> Foreign.Ptr.Ptr VkBufferCopy -- ^ pRegions
                                                                      -> IO ()

-- | queues: @transfer,graphics,compute@
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > void vkCmdCopyImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkImageCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdCopyImage.html vkCmdCopyImage registry at www.khronos.org>
foreign import ccall unsafe "vkCmdCopyImage" vkCmdCopyImage ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ srcImage
                         ->
                   VkImageLayout -- ^ srcImageLayout
                                 ->
                     VkImage -- ^ dstImage
                             ->
                       VkImageLayout -- ^ dstImageLayout
                                     ->
                         Data.Word.Word32 -- ^ regionCount
                                          -> Foreign.Ptr.Ptr VkImageCopy -- ^ pRegions
                                                                         -> IO ()

-- | queues: @graphics@
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > void vkCmdBlitImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkImageBlit* pRegions
--   >     , VkFilter filter
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdBlitImage.html vkCmdBlitImage registry at www.khronos.org>
foreign import ccall unsafe "vkCmdBlitImage" vkCmdBlitImage ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ srcImage
                         ->
                   VkImageLayout -- ^ srcImageLayout
                                 ->
                     VkImage -- ^ dstImage
                             ->
                       VkImageLayout -- ^ dstImageLayout
                                     ->
                         Data.Word.Word32 -- ^ regionCount
                                          ->
                           Foreign.Ptr.Ptr VkImageBlit -- ^ pRegions
                                                       -> VkFilter -- ^ filter
                                                                   -> IO ()

-- | queues: @transfer,graphics,compute@
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > void vkCmdCopyBufferToImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer srcBuffer
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkBufferImageCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdCopyBufferToImage.html vkCmdCopyBufferToImage registry at www.khronos.org>
foreign import ccall unsafe "vkCmdCopyBufferToImage"
               vkCmdCopyBufferToImage ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ srcBuffer
                          ->
                   VkImage -- ^ dstImage
                           ->
                     VkImageLayout -- ^ dstImageLayout
                                   ->
                       Data.Word.Word32 -- ^ regionCount
                                        -> Foreign.Ptr.Ptr VkBufferImageCopy -- ^ pRegions
                                                                             -> IO ()

-- | queues: @transfer,graphics,compute@
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > void vkCmdCopyImageToBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkBuffer dstBuffer
--   >     , uint32_t regionCount
--   >     , const VkBufferImageCopy* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdCopyImageToBuffer.html vkCmdCopyImageToBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkCmdCopyImageToBuffer"
               vkCmdCopyImageToBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ srcImage
                         ->
                   VkImageLayout -- ^ srcImageLayout
                                 ->
                     VkBuffer -- ^ dstBuffer
                              ->
                       Data.Word.Word32 -- ^ regionCount
                                        -> Foreign.Ptr.Ptr VkBufferImageCopy -- ^ pRegions
                                                                             -> IO ()

-- | queues: @transfer,graphics,compute@
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > void vkCmdUpdateBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , VkDeviceSize dataSize
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdUpdateBuffer.html vkCmdUpdateBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkCmdUpdateBuffer" vkCmdUpdateBuffer
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ dstBuffer
                          ->
                   VkDeviceSize -- ^ dstOffset
                                ->
                     VkDeviceSize -- ^ dataSize
                                  -> Foreign.Ptr.Ptr Data.Void.Void -- ^ pData
                                                                    -> IO ()

-- | transfer support is only available when VK_KHR_maintenance1 is enabled, as documented in valid usage language in the specification
--
--   queues: @transfer,graphics,compute@
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > void vkCmdFillBuffer
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , VkDeviceSize size
--   >     , uint32_t data
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdFillBuffer.html vkCmdFillBuffer registry at www.khronos.org>
foreign import ccall unsafe "vkCmdFillBuffer" vkCmdFillBuffer ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ dstBuffer
                          ->
                   VkDeviceSize -- ^ dstOffset
                                -> VkDeviceSize -- ^ size
                                                -> Data.Word.Word32 -- ^ data
                                                                    -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > void vkCmdClearColorImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage image
--   >     , VkImageLayout imageLayout
--   >     , const VkClearColorValue* pColor
--   >     , uint32_t rangeCount
--   >     , const VkImageSubresourceRange* pRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdClearColorImage.html vkCmdClearColorImage registry at www.khronos.org>
foreign import ccall unsafe "vkCmdClearColorImage"
               vkCmdClearColorImage ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ image
                         ->
                   VkImageLayout -- ^ imageLayout
                                 ->
                     Foreign.Ptr.Ptr VkClearColorValue -- ^ pColor
                                                       ->
                       Data.Word.Word32 -- ^ rangeCount
                                        ->
                         Foreign.Ptr.Ptr VkImageSubresourceRange -- ^ pRanges
                                                                 -> IO ()

-- | queues: @graphics@
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > void vkCmdClearDepthStencilImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage image
--   >     , VkImageLayout imageLayout
--   >     , const VkClearDepthStencilValue* pDepthStencil
--   >     , uint32_t rangeCount
--   >     , const VkImageSubresourceRange* pRanges
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdClearDepthStencilImage.html vkCmdClearDepthStencilImage registry at www.khronos.org>
foreign import ccall unsafe "vkCmdClearDepthStencilImage"
               vkCmdClearDepthStencilImage ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ image
                         ->
                   VkImageLayout -- ^ imageLayout
                                 ->
                     Foreign.Ptr.Ptr VkClearDepthStencilValue -- ^ pDepthStencil
                                                              ->
                       Data.Word.Word32 -- ^ rangeCount
                                        ->
                         Foreign.Ptr.Ptr VkImageSubresourceRange -- ^ pRanges
                                                                 -> IO ()

-- | queues: @graphics@
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > void vkCmdClearAttachments
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t attachmentCount
--   >     , const VkClearAttachment* pAttachments
--   >     , uint32_t rectCount
--   >     , const VkClearRect* pRects
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdClearAttachments.html vkCmdClearAttachments registry at www.khronos.org>
foreign import ccall unsafe "vkCmdClearAttachments"
               vkCmdClearAttachments ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Data.Word.Word32 -- ^ attachmentCount
                                  ->
                   Foreign.Ptr.Ptr VkClearAttachment -- ^ pAttachments
                                                     ->
                     Data.Word.Word32 -- ^ rectCount
                                      -> Foreign.Ptr.Ptr VkClearRect -- ^ pRects
                                                                     -> IO ()

-- | queues: @graphics@
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > void vkCmdResolveImage
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkImage srcImage
--   >     , VkImageLayout srcImageLayout
--   >     , VkImage dstImage
--   >     , VkImageLayout dstImageLayout
--   >     , uint32_t regionCount
--   >     , const VkImageResolve* pRegions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdResolveImage.html vkCmdResolveImage registry at www.khronos.org>
foreign import ccall unsafe "vkCmdResolveImage" vkCmdResolveImage
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkImage -- ^ srcImage
                         ->
                   VkImageLayout -- ^ srcImageLayout
                                 ->
                     VkImage -- ^ dstImage
                             ->
                       VkImageLayout -- ^ dstImageLayout
                                     ->
                         Data.Word.Word32 -- ^ regionCount
                                          -> Foreign.Ptr.Ptr VkImageResolve -- ^ pRegions
                                                                            -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @outside@
--
--   > void vkCmdSetEvent
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkEvent event
--   >     , VkPipelineStageFlags stageMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetEvent.html vkCmdSetEvent registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetEvent" vkCmdSetEvent ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkEvent -- ^ event
                                          -> VkPipelineStageFlags -- ^ stageMask
                                                                  -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @outside@
--
--   > void vkCmdResetEvent
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkEvent event
--   >     , VkPipelineStageFlags stageMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdResetEvent.html vkCmdResetEvent registry at www.khronos.org>
foreign import ccall unsafe "vkCmdResetEvent" vkCmdResetEvent ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkEvent -- ^ event
                                          -> VkPipelineStageFlags -- ^ stageMask
                                                                  -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdWaitEvents
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t eventCount
--   >     , const VkEvent* pEvents
--   >     , VkPipelineStageFlags srcStageMask
--   >     , VkPipelineStageFlags dstStageMask
--   >     , uint32_t memoryBarrierCount
--   >     , const VkMemoryBarrier* pMemoryBarriers
--   >     , uint32_t bufferMemoryBarrierCount
--   >     , const VkBufferMemoryBarrier* pBufferMemoryBarriers
--   >     , uint32_t imageMemoryBarrierCount
--   >     , const VkImageMemoryBarrier* pImageMemoryBarriers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdWaitEvents.html vkCmdWaitEvents registry at www.khronos.org>
foreign import ccall unsafe "vkCmdWaitEvents" vkCmdWaitEvents ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Data.Word.Word32 -- ^ eventCount
                                  ->
                   Foreign.Ptr.Ptr VkEvent -- ^ pEvents
                                           ->
                     VkPipelineStageFlags -- ^ srcStageMask
                                          ->
                       VkPipelineStageFlags -- ^ dstStageMask
                                            ->
                         Data.Word.Word32 -- ^ memoryBarrierCount
                                          ->
                           Foreign.Ptr.Ptr VkMemoryBarrier -- ^ pMemoryBarriers
                                                           ->
                             Data.Word.Word32 -- ^ bufferMemoryBarrierCount
                                              ->
                               Foreign.Ptr.Ptr VkBufferMemoryBarrier -- ^ pBufferMemoryBarriers
                                                                     ->
                                 Data.Word.Word32 -- ^ imageMemoryBarrierCount
                                                  -> Foreign.Ptr.Ptr VkImageMemoryBarrier -- ^ pImageMemoryBarriers
                                                                                          -> IO ()

-- | queues: @transfer,graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdPipelineBarrier
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineStageFlags srcStageMask
--   >     , VkPipelineStageFlags dstStageMask
--   >     , VkDependencyFlags dependencyFlags
--   >     , uint32_t memoryBarrierCount
--   >     , const VkMemoryBarrier* pMemoryBarriers
--   >     , uint32_t bufferMemoryBarrierCount
--   >     , const VkBufferMemoryBarrier* pBufferMemoryBarriers
--   >     , uint32_t imageMemoryBarrierCount
--   >     , const VkImageMemoryBarrier* pImageMemoryBarriers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdPipelineBarrier.html vkCmdPipelineBarrier registry at www.khronos.org>
foreign import ccall unsafe "vkCmdPipelineBarrier"
               vkCmdPipelineBarrier ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineStageFlags -- ^ srcStageMask
                                      ->
                   VkPipelineStageFlags -- ^ dstStageMask
                                        ->
                     VkDependencyFlags -- ^ dependencyFlags
                                       ->
                       Data.Word.Word32 -- ^ memoryBarrierCount
                                        ->
                         Foreign.Ptr.Ptr VkMemoryBarrier -- ^ pMemoryBarriers
                                                         ->
                           Data.Word.Word32 -- ^ bufferMemoryBarrierCount
                                            ->
                             Foreign.Ptr.Ptr VkBufferMemoryBarrier -- ^ pBufferMemoryBarriers
                                                                   ->
                               Data.Word.Word32 -- ^ imageMemoryBarrierCount
                                                -> Foreign.Ptr.Ptr VkImageMemoryBarrier -- ^ pImageMemoryBarriers
                                                                                        -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdBeginQuery
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     , VkQueryControlFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdBeginQuery.html vkCmdBeginQuery registry at www.khronos.org>
foreign import ccall unsafe "vkCmdBeginQuery" vkCmdBeginQuery ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkQueryPool -- ^ queryPool
                             -> Data.Word.Word32 -- ^ query
                                                 -> VkQueryControlFlags -- ^ flags
                                                                        -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdEndQuery
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdEndQuery.html vkCmdEndQuery registry at www.khronos.org>
foreign import ccall unsafe "vkCmdEndQuery" vkCmdEndQuery ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkQueryPool -- ^ queryPool
                                              -> Data.Word.Word32 -- ^ query
                                                                  -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @outside@
--
--   > void vkCmdResetQueryPool
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     , uint32_t queryCount
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdResetQueryPool.html vkCmdResetQueryPool registry at www.khronos.org>
foreign import ccall unsafe "vkCmdResetQueryPool"
               vkCmdResetQueryPool ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkQueryPool -- ^ queryPool
                             -> Data.Word.Word32 -- ^ firstQuery
                                                 -> Data.Word.Word32 -- ^ queryCount
                                                                     -> IO ()

-- | queues: @transfer,graphics,compute@
--
--   renderpass: @both@
--
--   pipeline: @transfer@
--
--   > void vkCmdWriteTimestamp
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineStageFlagBits pipelineStage
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdWriteTimestamp.html vkCmdWriteTimestamp registry at www.khronos.org>
foreign import ccall unsafe "vkCmdWriteTimestamp"
               vkCmdWriteTimestamp ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineStageFlagBits -- ^ pipelineStage
                                         -> VkQueryPool -- ^ queryPool
                                                        -> Data.Word.Word32 -- ^ query
                                                                            -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @outside@
--
--   pipeline: @transfer@
--
--   > void vkCmdCopyQueryPoolResults
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     , uint32_t queryCount
--   >     , VkBuffer dstBuffer
--   >     , VkDeviceSize dstOffset
--   >     , VkDeviceSize stride
--   >     , VkQueryResultFlags flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdCopyQueryPoolResults.html vkCmdCopyQueryPoolResults registry at www.khronos.org>
foreign import ccall unsafe "vkCmdCopyQueryPoolResults"
               vkCmdCopyQueryPoolResults ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkQueryPool -- ^ queryPool
                             ->
                   Data.Word.Word32 -- ^ firstQuery
                                    ->
                     Data.Word.Word32 -- ^ queryCount
                                      ->
                       VkBuffer -- ^ dstBuffer
                                ->
                         VkDeviceSize -- ^ dstOffset
                                      -> VkDeviceSize -- ^ stride
                                                      -> VkQueryResultFlags -- ^ flags
                                                                            -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdPushConstants
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineLayout layout
--   >     , VkShaderStageFlags stageFlags
--   >     , uint32_t offset
--   >     , uint32_t size
--   >     , const void* pValues
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdPushConstants.html vkCmdPushConstants registry at www.khronos.org>
foreign import ccall unsafe "vkCmdPushConstants" vkCmdPushConstants
               ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineLayout -- ^ layout
                                  ->
                   VkShaderStageFlags -- ^ stageFlags
                                      ->
                     Data.Word.Word32 -- ^ offset
                                      ->
                       Data.Word.Word32 -- ^ size
                                        -> Foreign.Ptr.Ptr Data.Void.Void -- ^ pValues
                                                                          -> IO ()

-- | queues: @graphics@
--
--   renderpass: @outside@
--
--   pipeline: @graphics@
--
--   > void vkCmdBeginRenderPass
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkRenderPassBeginInfo* pRenderPassBegin
--   >     , VkSubpassContents contents
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdBeginRenderPass.html vkCmdBeginRenderPass registry at www.khronos.org>
foreign import ccall unsafe "vkCmdBeginRenderPass"
               vkCmdBeginRenderPass ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Foreign.Ptr.Ptr VkRenderPassBeginInfo -- ^ pRenderPassBegin
                                                       -> VkSubpassContents -- ^ contents
                                                                            -> IO ()

-- | queues: @graphics@
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > void vkCmdNextSubpass
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkSubpassContents contents
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdNextSubpass.html vkCmdNextSubpass registry at www.khronos.org>
foreign import ccall unsafe "vkCmdNextSubpass" vkCmdNextSubpass ::
               VkCommandBuffer -- ^ commandBuffer
                               -> VkSubpassContents -- ^ contents
                                                    -> IO ()

-- | queues: @graphics@
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > void vkCmdEndRenderPass
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdEndRenderPass.html vkCmdEndRenderPass registry at www.khronos.org>
foreign import ccall unsafe "vkCmdEndRenderPass" vkCmdEndRenderPass
               :: VkCommandBuffer -- ^ commandBuffer
                                  -> IO ()

-- | queues: @transfer,graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdExecuteCommands
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t commandBufferCount
--   >     , const VkCommandBuffer* pCommandBuffers
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdExecuteCommands.html vkCmdExecuteCommands registry at www.khronos.org>
foreign import ccall unsafe "vkCmdExecuteCommands"
               vkCmdExecuteCommands ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Data.Word.Word32 -- ^ commandBufferCount
                                  -> Foreign.Ptr.Ptr VkCommandBuffer -- ^ pCommandBuffers
                                                                     -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateAndroidSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkAndroidSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateAndroidSurfaceKHR.html vkCreateAndroidSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateAndroidSurfaceKHR"
               vkCreateAndroidSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Foreign.Ptr.Ptr VkAndroidSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                               ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkSurfaceKHR -- ^ pSurface
                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceDisplayPropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayPropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceDisplayPropertiesKHR.html vkGetPhysicalDeviceDisplayPropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceDisplayPropertiesKHR"
               vkGetPhysicalDeviceDisplayPropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pPropertyCount
                                                  ->
                   Foreign.Ptr.Ptr VkDisplayPropertiesKHR -- ^ pProperties
                                                          -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceDisplayPlanePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayPlanePropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceDisplayPlanePropertiesKHR.html vkGetPhysicalDeviceDisplayPlanePropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceDisplayPlanePropertiesKHR"
               vkGetPhysicalDeviceDisplayPlanePropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pPropertyCount
                                                  ->
                   Foreign.Ptr.Ptr VkDisplayPlanePropertiesKHR -- ^ pProperties
                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayPlaneSupportedDisplaysKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t planeIndex
--   >     , uint32_t* pDisplayCount
--   >     , VkDisplayKHR* pDisplays
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDisplayPlaneSupportedDisplaysKHR.html vkGetDisplayPlaneSupportedDisplaysKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetDisplayPlaneSupportedDisplaysKHR"
               vkGetDisplayPlaneSupportedDisplaysKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Data.Word.Word32 -- ^ planeIndex
                                  ->
                   Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pDisplayCount
                                                    ->
                     Foreign.Ptr.Ptr VkDisplayKHR -- ^ pDisplays
                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayModePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayModePropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDisplayModePropertiesKHR.html vkGetDisplayModePropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetDisplayModePropertiesKHR"
               vkGetDisplayModePropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkDisplayKHR -- ^ display
                              ->
                   Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pPropertyCount
                                                    ->
                     Foreign.Ptr.Ptr VkDisplayModePropertiesKHR -- ^ pProperties
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkCreateDisplayModeKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     , const VkDisplayModeCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDisplayModeKHR* pMode
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDisplayModeKHR.html vkCreateDisplayModeKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDisplayModeKHR"
               vkCreateDisplayModeKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkDisplayKHR -- ^ display
                              ->
                   Foreign.Ptr.Ptr VkDisplayModeCreateInfoKHR -- ^ pCreateInfo
                                                              ->
                     Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                           ->
                       Foreign.Ptr.Ptr VkDisplayModeKHR -- ^ pMode
                                                        -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayPlaneCapabilitiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayModeKHR mode
--   >     , uint32_t planeIndex
--   >     , VkDisplayPlaneCapabilitiesKHR* pCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDisplayPlaneCapabilitiesKHR.html vkGetDisplayPlaneCapabilitiesKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetDisplayPlaneCapabilitiesKHR"
               vkGetDisplayPlaneCapabilitiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkDisplayModeKHR -- ^ mode
                                  ->
                   Data.Word.Word32 -- ^ planeIndex
                                    ->
                     Foreign.Ptr.Ptr VkDisplayPlaneCapabilitiesKHR -- ^ pCapabilities
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDisplayPlaneSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkDisplaySurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDisplayPlaneSurfaceKHR.html vkCreateDisplayPlaneSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDisplayPlaneSurfaceKHR"
               vkCreateDisplayPlaneSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Foreign.Ptr.Ptr VkDisplaySurfaceCreateInfoKHR -- ^ pCreateInfo
                                                               ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkSurfaceKHR -- ^ pSurface
                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INCOMPATIBLE_DISPLAY_KHR', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkCreateSharedSwapchainsKHR
--   >     ( VkDevice device
--   >     , uint32_t swapchainCount
--   >     , const VkSwapchainCreateInfoKHR* pCreateInfos
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSwapchainKHR* pSwapchains
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateSharedSwapchainsKHR.html vkCreateSharedSwapchainsKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateSharedSwapchainsKHR"
               vkCreateSharedSwapchainsKHR ::
               VkDevice -- ^ device
                        ->
                 Data.Word.Word32 -- ^ swapchainCount
                                  ->
                   Foreign.Ptr.Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfos
                                                            ->
                     Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                           ->
                       Foreign.Ptr.Ptr VkSwapchainKHR -- ^ pSwapchains
                                                      -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateMirSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkMirSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateMirSurfaceKHR.html vkCreateMirSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateMirSurfaceKHR"
               vkCreateMirSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Foreign.Ptr.Ptr VkMirSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                           ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkSurfaceKHR -- ^ pSurface
                                                  -> IO VkResult

-- | > VkBool32 vkGetPhysicalDeviceMirPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , MirConnection* connection
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceMirPresentationSupportKHR.html vkGetPhysicalDeviceMirPresentationSupportKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceMirPresentationSupportKHR"
               vkGetPhysicalDeviceMirPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Data.Word.Word32 -- ^ queueFamilyIndex
                                  -> Foreign.Ptr.Ptr MirConnection -- ^ connection
                                                                   -> IO VkBool32

-- | > void vkDestroySurfaceKHR
--   >     ( VkInstance instance
--   >     , VkSurfaceKHR surface
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroySurfaceKHR.html vkDestroySurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkDestroySurfaceKHR"
               vkDestroySurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 VkSurfaceKHR -- ^ surface
                              -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , VkSurfaceKHR surface
--   >     , VkBool32* pSupported
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceSurfaceSupportKHR.html vkGetPhysicalDeviceSurfaceSupportKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceSurfaceSupportKHR"
               vkGetPhysicalDeviceSurfaceSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Data.Word.Word32 -- ^ queueFamilyIndex
                                  ->
                   VkSurfaceKHR -- ^ surface
                                -> Foreign.Ptr.Ptr VkBool32 -- ^ pSupported
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceCapabilitiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSurfaceKHR surface
--   >     , VkSurfaceCapabilitiesKHR* pSurfaceCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceSurfaceCapabilitiesKHR.html vkGetPhysicalDeviceSurfaceCapabilitiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"
               vkGetPhysicalDeviceSurfaceCapabilitiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              ->
                   Foreign.Ptr.Ptr VkSurfaceCapabilitiesKHR -- ^ pSurfaceCapabilities
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceFormatsKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSurfaceKHR surface
--   >     , uint32_t* pSurfaceFormatCount
--   >     , VkSurfaceFormatKHR* pSurfaceFormats
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceSurfaceFormatsKHR.html vkGetPhysicalDeviceSurfaceFormatsKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceSurfaceFormatsKHR"
               vkGetPhysicalDeviceSurfaceFormatsKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              ->
                   Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pSurfaceFormatCount
                                                    ->
                     Foreign.Ptr.Ptr VkSurfaceFormatKHR -- ^ pSurfaceFormats
                                                        -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfacePresentModesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSurfaceKHR surface
--   >     , uint32_t* pPresentModeCount
--   >     , VkPresentModeKHR* pPresentModes
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceSurfacePresentModesKHR.html vkGetPhysicalDeviceSurfacePresentModesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceSurfacePresentModesKHR"
               vkGetPhysicalDeviceSurfacePresentModesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              ->
                   Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pPresentModeCount
                                                    ->
                     Foreign.Ptr.Ptr VkPresentModeKHR -- ^ pPresentModes
                                                      -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_SURFACE_LOST_KHR', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateSwapchainKHR
--   >     ( VkDevice device
--   >     , const VkSwapchainCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSwapchainKHR* pSwapchain
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateSwapchainKHR.html vkCreateSwapchainKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateSwapchainKHR"
               vkCreateSwapchainKHR ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfo
                                                          ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkSwapchainKHR -- ^ pSwapchain
                                                    -> IO VkResult

-- | > void vkDestroySwapchainKHR
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroySwapchainKHR.html vkDestroySwapchainKHR registry at www.khronos.org>
foreign import ccall unsafe "vkDestroySwapchainKHR"
               vkDestroySwapchainKHR ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetSwapchainImagesKHR
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , uint32_t* pSwapchainImageCount
--   >     , VkImage* pSwapchainImages
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetSwapchainImagesKHR.html vkGetSwapchainImagesKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetSwapchainImagesKHR"
               vkGetSwapchainImagesKHR ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                ->
                   Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pSwapchainImageCount
                                                    ->
                     Foreign.Ptr.Ptr VkImage -- ^ pSwapchainImages
                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_TIMEOUT', 'VK_NOT_READY', 'VK_SUBOPTIMAL_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkAcquireNextImageKHR
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , uint64_t timeout
--   >     , VkSemaphore semaphore
--   >     , VkFence fence
--   >     , uint32_t* pImageIndex
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkAcquireNextImageKHR.html vkAcquireNextImageKHR registry at www.khronos.org>
foreign import ccall unsafe "vkAcquireNextImageKHR"
               vkAcquireNextImageKHR ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                ->
                   Data.Word.Word64 -- ^ timeout
                                    ->
                     VkSemaphore -- ^ semaphore
                                 ->
                       VkFence -- ^ fence
                               -> Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pImageIndex
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_SUBOPTIMAL_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkQueuePresentKHR
--   >     ( VkQueue queue
--   >     , const VkPresentInfoKHR* pPresentInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkQueuePresentKHR.html vkQueuePresentKHR registry at www.khronos.org>
foreign import ccall unsafe "vkQueuePresentKHR" vkQueuePresentKHR
               :: VkQueue -- ^ queue
                          -> Foreign.Ptr.Ptr VkPresentInfoKHR -- ^ pPresentInfo
                                                              -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateViSurfaceNN
--   >     ( VkInstance instance
--   >     , const VkViSurfaceCreateInfoNN* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateViSurfaceNN.html vkCreateViSurfaceNN registry at www.khronos.org>
foreign import ccall unsafe "vkCreateViSurfaceNN"
               vkCreateViSurfaceNN ::
               VkInstance -- ^ instance
                          ->
                 Foreign.Ptr.Ptr VkViSurfaceCreateInfoNN -- ^ pCreateInfo
                                                         ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkSurfaceKHR -- ^ pSurface
                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateWaylandSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkWaylandSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateWaylandSurfaceKHR.html vkCreateWaylandSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateWaylandSurfaceKHR"
               vkCreateWaylandSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Foreign.Ptr.Ptr VkWaylandSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                               ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkSurfaceKHR -- ^ pSurface
                                                  -> IO VkResult

-- | > VkBool32 vkGetPhysicalDeviceWaylandPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , struct wl_display* display
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceWaylandPresentationSupportKHR.html vkGetPhysicalDeviceWaylandPresentationSupportKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
               vkGetPhysicalDeviceWaylandPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Data.Word.Word32 -- ^ queueFamilyIndex
                                  -> Foreign.Ptr.Ptr Wl_display -- ^ display
                                                                -> IO VkBool32

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateWin32SurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkWin32SurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateWin32SurfaceKHR.html vkCreateWin32SurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateWin32SurfaceKHR"
               vkCreateWin32SurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Foreign.Ptr.Ptr VkWin32SurfaceCreateInfoKHR -- ^ pCreateInfo
                                                             ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkSurfaceKHR -- ^ pSurface
                                                  -> IO VkResult

-- | > VkBool32 vkGetPhysicalDeviceWin32PresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceWin32PresentationSupportKHR.html vkGetPhysicalDeviceWin32PresentationSupportKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceWin32PresentationSupportKHR"
               vkGetPhysicalDeviceWin32PresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Data.Word.Word32 -- ^ queueFamilyIndex
                                                    -> IO VkBool32

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateXlibSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkXlibSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateXlibSurfaceKHR.html vkCreateXlibSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateXlibSurfaceKHR"
               vkCreateXlibSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Foreign.Ptr.Ptr VkXlibSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                            ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkSurfaceKHR -- ^ pSurface
                                                  -> IO VkResult

-- | > VkBool32 vkGetPhysicalDeviceXlibPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , Display* dpy
--   >     , VisualID visualID
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceXlibPresentationSupportKHR.html vkGetPhysicalDeviceXlibPresentationSupportKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceXlibPresentationSupportKHR"
               vkGetPhysicalDeviceXlibPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Data.Word.Word32 -- ^ queueFamilyIndex
                                  ->
                   Foreign.Ptr.Ptr Display -- ^ dpy
                                           -> VisualID -- ^ visualID
                                                       -> IO VkBool32

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateXcbSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkXcbSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateXcbSurfaceKHR.html vkCreateXcbSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateXcbSurfaceKHR"
               vkCreateXcbSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Foreign.Ptr.Ptr VkXcbSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                           ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkSurfaceKHR -- ^ pSurface
                                                  -> IO VkResult

-- | > VkBool32 vkGetPhysicalDeviceXcbPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , xcb_connection_t* connection
--   >     , xcb_visualid_t visual_id
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceXcbPresentationSupportKHR.html vkGetPhysicalDeviceXcbPresentationSupportKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceXcbPresentationSupportKHR"
               vkGetPhysicalDeviceXcbPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Data.Word.Word32 -- ^ queueFamilyIndex
                                  ->
                   Foreign.Ptr.Ptr Xcb_connection_t -- ^ connection
                                                    -> Xcb_visualid_t -- ^ visual_id
                                                                      -> IO VkBool32

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkCreateDebugReportCallbackEXT
--   >     ( VkInstance instance
--   >     , const VkDebugReportCallbackCreateInfoEXT* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDebugReportCallbackEXT* pCallback
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDebugReportCallbackEXT.html vkCreateDebugReportCallbackEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDebugReportCallbackEXT"
               vkCreateDebugReportCallbackEXT ::
               VkInstance -- ^ instance
                          ->
                 Foreign.Ptr.Ptr VkDebugReportCallbackCreateInfoEXT -- ^ pCreateInfo
                                                                    ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkDebugReportCallbackEXT -- ^ pCallback
                                                              -> IO VkResult

-- | > void vkDestroyDebugReportCallbackEXT
--   >     ( VkInstance instance
--   >     , VkDebugReportCallbackEXT callback
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyDebugReportCallbackEXT.html vkDestroyDebugReportCallbackEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyDebugReportCallbackEXT"
               vkDestroyDebugReportCallbackEXT ::
               VkInstance -- ^ instance
                          ->
                 VkDebugReportCallbackEXT -- ^ callback
                                          ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         -> IO ()

-- | > void vkDebugReportMessageEXT
--   >     ( VkInstance instance
--   >     , VkDebugReportFlagsEXT flags
--   >     , VkDebugReportObjectTypeEXT objectType
--   >     , uint64_t object
--   >     , size_t location
--   >     , int32_t messageCode
--   >     , const char* pLayerPrefix
--   >     , const char* pMessage
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDebugReportMessageEXT.html vkDebugReportMessageEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDebugReportMessageEXT"
               vkDebugReportMessageEXT ::
               VkInstance -- ^ instance
                          ->
                 VkDebugReportFlagsEXT -- ^ flags
                                       ->
                   VkDebugReportObjectTypeEXT -- ^ objectType
                                              ->
                     Data.Word.Word64 -- ^ object
                                      ->
                       Foreign.C.Types.CSize -- ^ location
                                             ->
                         Data.Int.Int32 -- ^ messageCode
                                        ->
                           Foreign.Ptr.Ptr Foreign.C.Types.CChar -- ^ pLayerPrefix
                                                                 ->
                             Foreign.Ptr.Ptr Foreign.C.Types.CChar -- ^ pMessage
                                                                   -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkDebugMarkerSetObjectNameEXT
--   >     ( VkDevice device
--   >     , const VkDebugMarkerObjectNameInfoEXT* pNameInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDebugMarkerSetObjectNameEXT.html vkDebugMarkerSetObjectNameEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDebugMarkerSetObjectNameEXT"
               vkDebugMarkerSetObjectNameEXT ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkDebugMarkerObjectNameInfoEXT -- ^ pNameInfo
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkDebugMarkerSetObjectTagEXT
--   >     ( VkDevice device
--   >     , const VkDebugMarkerObjectTagInfoEXT* pTagInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDebugMarkerSetObjectTagEXT.html vkDebugMarkerSetObjectTagEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDebugMarkerSetObjectTagEXT"
               vkDebugMarkerSetObjectTagEXT ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkDebugMarkerObjectTagInfoEXT -- ^ pTagInfo
                                                               -> IO VkResult

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdDebugMarkerBeginEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkDebugMarkerMarkerInfoEXT* pMarkerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDebugMarkerBeginEXT.html vkCmdDebugMarkerBeginEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDebugMarkerBeginEXT"
               vkCmdDebugMarkerBeginEXT ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Foreign.Ptr.Ptr VkDebugMarkerMarkerInfoEXT -- ^ pMarkerInfo
                                                            -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdDebugMarkerEndEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDebugMarkerEndEXT.html vkCmdDebugMarkerEndEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDebugMarkerEndEXT"
               vkCmdDebugMarkerEndEXT :: VkCommandBuffer -- ^ commandBuffer
                                                         -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdDebugMarkerInsertEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkDebugMarkerMarkerInfoEXT* pMarkerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDebugMarkerInsertEXT.html vkCmdDebugMarkerInsertEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDebugMarkerInsertEXT"
               vkCmdDebugMarkerInsertEXT ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Foreign.Ptr.Ptr VkDebugMarkerMarkerInfoEXT -- ^ pMarkerInfo
                                                            -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceExternalImageFormatPropertiesNV
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkImageType type
--   >     , VkImageTiling tiling
--   >     , VkImageUsageFlags usage
--   >     , VkImageCreateFlags flags
--   >     , VkExternalMemoryHandleTypeFlagsNV externalHandleType
--   >     , VkExternalImageFormatPropertiesNV* pExternalImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceExternalImageFormatPropertiesNV.html vkGetPhysicalDeviceExternalImageFormatPropertiesNV registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalImageFormatPropertiesNV"
               vkGetPhysicalDeviceExternalImageFormatPropertiesNV ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkFormat -- ^ format
                          ->
                   VkImageType -- ^ type
                               ->
                     VkImageTiling -- ^ tiling
                                   ->
                       VkImageUsageFlags -- ^ usage
                                         ->
                         VkImageCreateFlags -- ^ flags
                                            ->
                           VkExternalMemoryHandleTypeFlagsNV -- ^ externalHandleType
                                                             ->
                             Foreign.Ptr.Ptr VkExternalImageFormatPropertiesNV -- ^ pExternalImageFormatProperties
                                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetMemoryWin32HandleNV
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , VkExternalMemoryHandleTypeFlagsNV handleType
--   >     , HANDLE* pHandle
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetMemoryWin32HandleNV.html vkGetMemoryWin32HandleNV registry at www.khronos.org>
foreign import ccall unsafe "vkGetMemoryWin32HandleNV"
               vkGetMemoryWin32HandleNV ::
               VkDevice -- ^ device
                        ->
                 VkDeviceMemory -- ^ memory
                                ->
                   VkExternalMemoryHandleTypeFlagsNV -- ^ handleType
                                                     ->
                     Foreign.Ptr.Ptr HANDLE -- ^ pHandle
                                            -> IO VkResult

-- | queues: @graphics@
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > void vkCmdDrawIndirectCountAMD
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkBuffer countBuffer
--   >     , VkDeviceSize countBufferOffset
--   >     , uint32_t maxDrawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDrawIndirectCountAMD.html vkCmdDrawIndirectCountAMD registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDrawIndirectCountAMD"
               vkCmdDrawIndirectCountAMD ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          ->
                   VkDeviceSize -- ^ offset
                                ->
                     VkBuffer -- ^ countBuffer
                              ->
                       VkDeviceSize -- ^ countBufferOffset
                                    -> Data.Word.Word32 -- ^ maxDrawCount
                                                        -> Data.Word.Word32 -- ^ stride
                                                                            -> IO ()

-- | queues: @graphics@
--
--   renderpass: @inside@
--
--   pipeline: @graphics@
--
--   > void vkCmdDrawIndexedIndirectCountAMD
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkBuffer countBuffer
--   >     , VkDeviceSize countBufferOffset
--   >     , uint32_t maxDrawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDrawIndexedIndirectCountAMD.html vkCmdDrawIndexedIndirectCountAMD registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDrawIndexedIndirectCountAMD"
               vkCmdDrawIndexedIndirectCountAMD ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkBuffer -- ^ buffer
                          ->
                   VkDeviceSize -- ^ offset
                                ->
                     VkBuffer -- ^ countBuffer
                              ->
                       VkDeviceSize -- ^ countBufferOffset
                                    -> Data.Word.Word32 -- ^ maxDrawCount
                                                        -> Data.Word.Word32 -- ^ stride
                                                                            -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @inside@
--
--   > void vkCmdProcessCommandsNVX
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCmdProcessCommandsInfoNVX* pProcessCommandsInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdProcessCommandsNVX.html vkCmdProcessCommandsNVX registry at www.khronos.org>
foreign import ccall unsafe "vkCmdProcessCommandsNVX"
               vkCmdProcessCommandsNVX ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Foreign.Ptr.Ptr VkCmdProcessCommandsInfoNVX -- ^ pProcessCommandsInfo
                                                             -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @inside@
--
--   > void vkCmdReserveSpaceForCommandsNVX
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCmdReserveSpaceForCommandsInfoNVX* pReserveSpaceInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdReserveSpaceForCommandsNVX.html vkCmdReserveSpaceForCommandsNVX registry at www.khronos.org>
foreign import ccall unsafe "vkCmdReserveSpaceForCommandsNVX"
               vkCmdReserveSpaceForCommandsNVX ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Foreign.Ptr.Ptr VkCmdReserveSpaceForCommandsInfoNVX -- ^ pReserveSpaceInfo
                                                                     -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateIndirectCommandsLayoutNVX
--   >     ( VkDevice device
--   >     , const VkIndirectCommandsLayoutCreateInfoNVX* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkIndirectCommandsLayoutNVX* pIndirectCommandsLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateIndirectCommandsLayoutNVX.html vkCreateIndirectCommandsLayoutNVX registry at www.khronos.org>
foreign import ccall unsafe "vkCreateIndirectCommandsLayoutNVX"
               vkCreateIndirectCommandsLayoutNVX ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkIndirectCommandsLayoutCreateInfoNVX -- ^ pCreateInfo
                                                                       ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkIndirectCommandsLayoutNVX -- ^ pIndirectCommandsLayout
                                                                 -> IO VkResult

-- | > void vkDestroyIndirectCommandsLayoutNVX
--   >     ( VkDevice device
--   >     , VkIndirectCommandsLayoutNVX indirectCommandsLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyIndirectCommandsLayoutNVX.html vkDestroyIndirectCommandsLayoutNVX registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyIndirectCommandsLayoutNVX"
               vkDestroyIndirectCommandsLayoutNVX ::
               VkDevice -- ^ device
                        ->
                 VkIndirectCommandsLayoutNVX -- ^ indirectCommandsLayout
                                             ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateObjectTableNVX
--   >     ( VkDevice device
--   >     , const VkObjectTableCreateInfoNVX* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkObjectTableNVX* pObjectTable
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateObjectTableNVX.html vkCreateObjectTableNVX registry at www.khronos.org>
foreign import ccall unsafe "vkCreateObjectTableNVX"
               vkCreateObjectTableNVX ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkObjectTableCreateInfoNVX -- ^ pCreateInfo
                                                            ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkObjectTableNVX -- ^ pObjectTable
                                                      -> IO VkResult

-- | > void vkDestroyObjectTableNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyObjectTableNVX.html vkDestroyObjectTableNVX registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyObjectTableNVX"
               vkDestroyObjectTableNVX ::
               VkDevice -- ^ device
                        ->
                 VkObjectTableNVX -- ^ objectTable
                                  -> Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                           -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkRegisterObjectsNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , uint32_t objectCount
--   >     , const VkObjectTableEntryNVX* const*    ppObjectTableEntries
--   >     , const uint32_t* pObjectIndices
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkRegisterObjectsNVX.html vkRegisterObjectsNVX registry at www.khronos.org>
foreign import ccall unsafe "vkRegisterObjectsNVX"
               vkRegisterObjectsNVX ::
               VkDevice -- ^ device
                        ->
                 VkObjectTableNVX -- ^ objectTable
                                  ->
                   Data.Word.Word32 -- ^ objectCount
                                    ->
                     Foreign.Ptr.Ptr (Foreign.Ptr.Ptr VkObjectTableEntryNVX) -- ^ ppObjectTableEntries
                                                                             ->
                       Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pObjectIndices
                                                        -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkUnregisterObjectsNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , uint32_t objectCount
--   >     , const VkObjectEntryTypeNVX* pObjectEntryTypes
--   >     , const uint32_t* pObjectIndices
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkUnregisterObjectsNVX.html vkUnregisterObjectsNVX registry at www.khronos.org>
foreign import ccall unsafe "vkUnregisterObjectsNVX"
               vkUnregisterObjectsNVX ::
               VkDevice -- ^ device
                        ->
                 VkObjectTableNVX -- ^ objectTable
                                  ->
                   Data.Word.Word32 -- ^ objectCount
                                    ->
                     Foreign.Ptr.Ptr VkObjectEntryTypeNVX -- ^ pObjectEntryTypes
                                                          ->
                       Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pObjectIndices
                                                        -> IO VkResult

-- | > void vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDeviceGeneratedCommandsFeaturesNVX* pFeatures
--   >     , VkDeviceGeneratedCommandsLimitsNVX* pLimits
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX.html vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX"
               vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr VkDeviceGeneratedCommandsFeaturesNVX -- ^ pFeatures
                                                                      ->
                   Foreign.Ptr.Ptr VkDeviceGeneratedCommandsLimitsNVX -- ^ pLimits
                                                                      -> IO ()

-- | > void vkGetPhysicalDeviceFeatures2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures2KHR* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceFeatures2KHR.html vkGetPhysicalDeviceFeatures2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceFeatures2KHR"
               vkGetPhysicalDeviceFeatures2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr VkPhysicalDeviceFeatures2KHR -- ^ pFeatures
                                                              -> IO ()

-- | > void vkGetPhysicalDeviceProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties2KHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceProperties2KHR.html vkGetPhysicalDeviceProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceProperties2KHR"
               vkGetPhysicalDeviceProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr VkPhysicalDeviceProperties2KHR -- ^ pProperties
                                                                -> IO ()

-- | > void vkGetPhysicalDeviceFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties2KHR* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceFormatProperties2KHR.html vkGetPhysicalDeviceFormatProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceFormatProperties2KHR"
               vkGetPhysicalDeviceFormatProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkFormat -- ^ format
                          -> Foreign.Ptr.Ptr VkFormatProperties2KHR -- ^ pFormatProperties
                                                                    -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceImageFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceImageFormatInfo2KHR* pImageFormatInfo
--   >     , VkImageFormatProperties2KHR* pImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceImageFormatProperties2KHR.html vkGetPhysicalDeviceImageFormatProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceImageFormatProperties2KHR"
               vkGetPhysicalDeviceImageFormatProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr VkPhysicalDeviceImageFormatInfo2KHR -- ^ pImageFormatInfo
                                                                     ->
                   Foreign.Ptr.Ptr VkImageFormatProperties2KHR -- ^ pImageFormatProperties
                                                               -> IO VkResult

-- | > void vkGetPhysicalDeviceQueueFamilyProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties2KHR* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceQueueFamilyProperties2KHR.html vkGetPhysicalDeviceQueueFamilyProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceQueueFamilyProperties2KHR"
               vkGetPhysicalDeviceQueueFamilyProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pQueueFamilyPropertyCount
                                                  ->
                   Foreign.Ptr.Ptr VkQueueFamilyProperties2KHR -- ^ pQueueFamilyProperties
                                                               -> IO ()

-- | > void vkGetPhysicalDeviceMemoryProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties2KHR* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceMemoryProperties2KHR.html vkGetPhysicalDeviceMemoryProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceMemoryProperties2KHR"
               vkGetPhysicalDeviceMemoryProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr VkPhysicalDeviceMemoryProperties2KHR -- ^ pMemoryProperties
                                                                      -> IO ()

-- | > void vkGetPhysicalDeviceSparseImageFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSparseImageFormatInfo2KHR* pFormatInfo
--   >     , uint32_t* pPropertyCount
--   >     , VkSparseImageFormatProperties2KHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceSparseImageFormatProperties2KHR.html vkGetPhysicalDeviceSparseImageFormatProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceSparseImageFormatProperties2KHR"
               vkGetPhysicalDeviceSparseImageFormatProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr VkPhysicalDeviceSparseImageFormatInfo2KHR -- ^ pFormatInfo
                                                                           ->
                   Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pPropertyCount
                                                    ->
                     Foreign.Ptr.Ptr VkSparseImageFormatProperties2KHR -- ^ pProperties
                                                                       -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdPushDescriptorSetKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineBindPoint pipelineBindPoint
--   >     , VkPipelineLayout layout
--   >     , uint32_t set
--   >     , uint32_t descriptorWriteCount
--   >     , const VkWriteDescriptorSet* pDescriptorWrites
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdPushDescriptorSetKHR.html vkCmdPushDescriptorSetKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCmdPushDescriptorSetKHR"
               vkCmdPushDescriptorSetKHR ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkPipelineBindPoint -- ^ pipelineBindPoint
                                     ->
                   VkPipelineLayout -- ^ layout
                                    ->
                     Data.Word.Word32 -- ^ set
                                      ->
                       Data.Word.Word32 -- ^ descriptorWriteCount
                                        -> Foreign.Ptr.Ptr VkWriteDescriptorSet -- ^ pDescriptorWrites
                                                                                -> IO ()

-- | > void vkTrimCommandPoolKHR
--   >     ( VkDevice device
--   >     , VkCommandPool commandPool
--   >     , VkCommandPoolTrimFlagsKHR flags
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkTrimCommandPoolKHR.html vkTrimCommandPoolKHR registry at www.khronos.org>
foreign import ccall unsafe "vkTrimCommandPoolKHR"
               vkTrimCommandPoolKHR ::
               VkDevice -- ^ device
                        -> VkCommandPool -- ^ commandPool
                                         -> VkCommandPoolTrimFlagsKHR -- ^ flags
                                                                      -> IO ()

-- | > void vkGetPhysicalDeviceExternalBufferPropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalBufferInfoKHR* pExternalBufferInfo
--   >     , VkExternalBufferPropertiesKHR* pExternalBufferProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceExternalBufferPropertiesKHR.html vkGetPhysicalDeviceExternalBufferPropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalBufferPropertiesKHR"
               vkGetPhysicalDeviceExternalBufferPropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr VkPhysicalDeviceExternalBufferInfoKHR -- ^ pExternalBufferInfo
                                                                       ->
                   Foreign.Ptr.Ptr VkExternalBufferPropertiesKHR -- ^ pExternalBufferProperties
                                                                 -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetMemoryWin32HandleKHR
--   >     ( VkDevice device
--   >     , const VkMemoryGetWin32HandleInfoKHR* pGetWin32HandleInfo
--   >     , HANDLE* pHandle
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetMemoryWin32HandleKHR.html vkGetMemoryWin32HandleKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetMemoryWin32HandleKHR"
               vkGetMemoryWin32HandleKHR ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkMemoryGetWin32HandleInfoKHR -- ^ pGetWin32HandleInfo
                                                               ->
                   Foreign.Ptr.Ptr HANDLE -- ^ pHandle
                                          -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkGetMemoryWin32HandlePropertiesKHR
--   >     ( VkDevice device
--   >     , VkExternalMemoryHandleTypeFlagBitsKHR handleType
--   >     , HANDLE handle
--   >     , VkMemoryWin32HandlePropertiesKHR* pMemoryWin32HandleProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetMemoryWin32HandlePropertiesKHR.html vkGetMemoryWin32HandlePropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetMemoryWin32HandlePropertiesKHR"
               vkGetMemoryWin32HandlePropertiesKHR ::
               VkDevice -- ^ device
                        ->
                 VkExternalMemoryHandleTypeFlagBitsKHR -- ^ handleType
                                                       ->
                   HANDLE -- ^ handle
                          ->
                     Foreign.Ptr.Ptr VkMemoryWin32HandlePropertiesKHR -- ^ pMemoryWin32HandleProperties
                                                                      -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetMemoryFdKHR
--   >     ( VkDevice device
--   >     , const VkMemoryGetFdInfoKHR* pGetFdInfo
--   >     , int* pFd
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetMemoryFdKHR.html vkGetMemoryFdKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetMemoryFdKHR" vkGetMemoryFdKHR ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkMemoryGetFdInfoKHR -- ^ pGetFdInfo
                                                      ->
                   Foreign.Ptr.Ptr Foreign.C.Types.CInt -- ^ pFd
                                                        -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkGetMemoryFdPropertiesKHR
--   >     ( VkDevice device
--   >     , VkExternalMemoryHandleTypeFlagBitsKHR handleType
--   >     , int fd
--   >     , VkMemoryFdPropertiesKHR* pMemoryFdProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetMemoryFdPropertiesKHR.html vkGetMemoryFdPropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetMemoryFdPropertiesKHR"
               vkGetMemoryFdPropertiesKHR ::
               VkDevice -- ^ device
                        ->
                 VkExternalMemoryHandleTypeFlagBitsKHR -- ^ handleType
                                                       ->
                   Foreign.C.Types.CInt -- ^ fd
                                        ->
                     Foreign.Ptr.Ptr VkMemoryFdPropertiesKHR -- ^ pMemoryFdProperties
                                                             -> IO VkResult

-- | > void vkGetPhysicalDeviceExternalSemaphorePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalSemaphoreInfoKHR* pExternalSemaphoreInfo
--   >     , VkExternalSemaphorePropertiesKHR* pExternalSemaphoreProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceExternalSemaphorePropertiesKHR.html vkGetPhysicalDeviceExternalSemaphorePropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalSemaphorePropertiesKHR"
               vkGetPhysicalDeviceExternalSemaphorePropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr VkPhysicalDeviceExternalSemaphoreInfoKHR -- ^ pExternalSemaphoreInfo
                                                                          ->
                   Foreign.Ptr.Ptr VkExternalSemaphorePropertiesKHR -- ^ pExternalSemaphoreProperties
                                                                    -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetSemaphoreWin32HandleKHR
--   >     ( VkDevice device
--   >     , const VkSemaphoreGetWin32HandleInfoKHR* pGetWin32HandleInfo
--   >     , HANDLE* pHandle
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetSemaphoreWin32HandleKHR.html vkGetSemaphoreWin32HandleKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetSemaphoreWin32HandleKHR"
               vkGetSemaphoreWin32HandleKHR ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkSemaphoreGetWin32HandleInfoKHR -- ^ pGetWin32HandleInfo
                                                                  ->
                   Foreign.Ptr.Ptr HANDLE -- ^ pHandle
                                          -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkImportSemaphoreWin32HandleKHR
--   >     ( VkDevice device
--   >     , const VkImportSemaphoreWin32HandleInfoKHR* pImportSemaphoreWin32HandleInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkImportSemaphoreWin32HandleKHR.html vkImportSemaphoreWin32HandleKHR registry at www.khronos.org>
foreign import ccall unsafe "vkImportSemaphoreWin32HandleKHR"
               vkImportSemaphoreWin32HandleKHR ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkImportSemaphoreWin32HandleInfoKHR -- ^ pImportSemaphoreWin32HandleInfo
                                                                     -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetSemaphoreFdKHR
--   >     ( VkDevice device
--   >     , const VkSemaphoreGetFdInfoKHR* pGetFdInfo
--   >     , int* pFd
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetSemaphoreFdKHR.html vkGetSemaphoreFdKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetSemaphoreFdKHR"
               vkGetSemaphoreFdKHR ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkSemaphoreGetFdInfoKHR -- ^ pGetFdInfo
                                                         ->
                   Foreign.Ptr.Ptr Foreign.C.Types.CInt -- ^ pFd
                                                        -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkImportSemaphoreFdKHR
--   >     ( VkDevice device
--   >     , const VkImportSemaphoreFdInfoKHR* pImportSemaphoreFdInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkImportSemaphoreFdKHR.html vkImportSemaphoreFdKHR registry at www.khronos.org>
foreign import ccall unsafe "vkImportSemaphoreFdKHR"
               vkImportSemaphoreFdKHR ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkImportSemaphoreFdInfoKHR -- ^ pImportSemaphoreFdInfo
                                                            -> IO VkResult

-- | > void vkGetPhysicalDeviceExternalFencePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalFenceInfoKHR* pExternalFenceInfo
--   >     , VkExternalFencePropertiesKHR* pExternalFenceProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceExternalFencePropertiesKHR.html vkGetPhysicalDeviceExternalFencePropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalFencePropertiesKHR"
               vkGetPhysicalDeviceExternalFencePropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr VkPhysicalDeviceExternalFenceInfoKHR -- ^ pExternalFenceInfo
                                                                      ->
                   Foreign.Ptr.Ptr VkExternalFencePropertiesKHR -- ^ pExternalFenceProperties
                                                                -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetFenceWin32HandleKHR
--   >     ( VkDevice device
--   >     , const VkFenceGetWin32HandleInfoKHR* pGetWin32HandleInfo
--   >     , HANDLE* pHandle
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetFenceWin32HandleKHR.html vkGetFenceWin32HandleKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetFenceWin32HandleKHR"
               vkGetFenceWin32HandleKHR ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkFenceGetWin32HandleInfoKHR -- ^ pGetWin32HandleInfo
                                                              ->
                   Foreign.Ptr.Ptr HANDLE -- ^ pHandle
                                          -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkImportFenceWin32HandleKHR
--   >     ( VkDevice device
--   >     , const VkImportFenceWin32HandleInfoKHR* pImportFenceWin32HandleInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkImportFenceWin32HandleKHR.html vkImportFenceWin32HandleKHR registry at www.khronos.org>
foreign import ccall unsafe "vkImportFenceWin32HandleKHR"
               vkImportFenceWin32HandleKHR ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkImportFenceWin32HandleInfoKHR -- ^ pImportFenceWin32HandleInfo
                                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetFenceFdKHR
--   >     ( VkDevice device
--   >     , const VkFenceGetFdInfoKHR* pGetFdInfo
--   >     , int* pFd
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetFenceFdKHR.html vkGetFenceFdKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetFenceFdKHR" vkGetFenceFdKHR ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkFenceGetFdInfoKHR -- ^ pGetFdInfo
                                                     ->
                   Foreign.Ptr.Ptr Foreign.C.Types.CInt -- ^ pFd
                                                        -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkImportFenceFdKHR
--   >     ( VkDevice device
--   >     , const VkImportFenceFdInfoKHR* pImportFenceFdInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkImportFenceFdKHR.html vkImportFenceFdKHR registry at www.khronos.org>
foreign import ccall unsafe "vkImportFenceFdKHR" vkImportFenceFdKHR
               ::
               VkDevice -- ^ device
                        -> Foreign.Ptr.Ptr VkImportFenceFdInfoKHR -- ^ pImportFenceFdInfo
                                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkReleaseDisplayEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkReleaseDisplayEXT.html vkReleaseDisplayEXT registry at www.khronos.org>
foreign import ccall unsafe "vkReleaseDisplayEXT"
               vkReleaseDisplayEXT ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkDisplayKHR -- ^ display
                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkAcquireXlibDisplayEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , Display* dpy
--   >     , VkDisplayKHR display
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkAcquireXlibDisplayEXT.html vkAcquireXlibDisplayEXT registry at www.khronos.org>
foreign import ccall unsafe "vkAcquireXlibDisplayEXT"
               vkAcquireXlibDisplayEXT ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr Display -- ^ dpy
                                         -> VkDisplayKHR -- ^ display
                                                         -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkGetRandROutputDisplayEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , Display* dpy
--   >     , RROutput rrOutput
--   >     , VkDisplayKHR* pDisplay
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetRandROutputDisplayEXT.html vkGetRandROutputDisplayEXT registry at www.khronos.org>
foreign import ccall unsafe "vkGetRandROutputDisplayEXT"
               vkGetRandROutputDisplayEXT ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr Display -- ^ dpy
                                         ->
                   RROutput -- ^ rrOutput
                            -> Foreign.Ptr.Ptr VkDisplayKHR -- ^ pDisplay
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkDisplayPowerControlEXT
--   >     ( VkDevice device
--   >     , VkDisplayKHR display
--   >     , const VkDisplayPowerInfoEXT* pDisplayPowerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDisplayPowerControlEXT.html vkDisplayPowerControlEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDisplayPowerControlEXT"
               vkDisplayPowerControlEXT ::
               VkDevice -- ^ device
                        ->
                 VkDisplayKHR -- ^ display
                              ->
                   Foreign.Ptr.Ptr VkDisplayPowerInfoEXT -- ^ pDisplayPowerInfo
                                                         -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkRegisterDeviceEventEXT
--   >     ( VkDevice device
--   >     , const VkDeviceEventInfoEXT* pDeviceEventInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFence* pFence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkRegisterDeviceEventEXT.html vkRegisterDeviceEventEXT registry at www.khronos.org>
foreign import ccall unsafe "vkRegisterDeviceEventEXT"
               vkRegisterDeviceEventEXT ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkDeviceEventInfoEXT -- ^ pDeviceEventInfo
                                                      ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkFence -- ^ pFence
                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkRegisterDisplayEventEXT
--   >     ( VkDevice device
--   >     , VkDisplayKHR display
--   >     , const VkDisplayEventInfoEXT* pDisplayEventInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFence* pFence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkRegisterDisplayEventEXT.html vkRegisterDisplayEventEXT registry at www.khronos.org>
foreign import ccall unsafe "vkRegisterDisplayEventEXT"
               vkRegisterDisplayEventEXT ::
               VkDevice -- ^ device
                        ->
                 VkDisplayKHR -- ^ display
                              ->
                   Foreign.Ptr.Ptr VkDisplayEventInfoEXT -- ^ pDisplayEventInfo
                                                         ->
                     Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                           ->
                       Foreign.Ptr.Ptr VkFence -- ^ pFence
                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR'.
--
--   > VkResult vkGetSwapchainCounterEXT
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , VkSurfaceCounterFlagBitsEXT counter
--   >     , uint64_t* pCounterValue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetSwapchainCounterEXT.html vkGetSwapchainCounterEXT registry at www.khronos.org>
foreign import ccall unsafe "vkGetSwapchainCounterEXT"
               vkGetSwapchainCounterEXT ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                ->
                   VkSurfaceCounterFlagBitsEXT -- ^ counter
                                               ->
                     Foreign.Ptr.Ptr Data.Word.Word64 -- ^ pCounterValue
                                                      -> IO VkResult

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceSurfaceCapabilities2EXT.html vkGetPhysicalDeviceSurfaceCapabilities2EXT registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceSurfaceCapabilities2EXT"
               vkGetPhysicalDeviceSurfaceCapabilities2EXT ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              ->
                   Foreign.Ptr.Ptr VkSurfaceCapabilities2EXT -- ^ pSurfaceCapabilities
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkEnumeratePhysicalDeviceGroupsKHX
--   >     ( VkInstance instance
--   >     , uint32_t* pPhysicalDeviceGroupCount
--   >     , VkPhysicalDeviceGroupPropertiesKHX* pPhysicalDeviceGroupProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkEnumeratePhysicalDeviceGroupsKHX.html vkEnumeratePhysicalDeviceGroupsKHX registry at www.khronos.org>
foreign import ccall unsafe "vkEnumeratePhysicalDeviceGroupsKHX"
               vkEnumeratePhysicalDeviceGroupsKHX ::
               VkInstance -- ^ instance
                          ->
                 Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pPhysicalDeviceGroupCount
                                                  ->
                   Foreign.Ptr.Ptr VkPhysicalDeviceGroupPropertiesKHX -- ^ pPhysicalDeviceGroupProperties
                                                                      -> IO VkResult

-- | > void vkGetDeviceGroupPeerMemoryFeaturesKHX
--   >     ( VkDevice device
--   >     , uint32_t heapIndex
--   >     , uint32_t localDeviceIndex
--   >     , uint32_t remoteDeviceIndex
--   >     , VkPeerMemoryFeatureFlagsKHX* pPeerMemoryFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDeviceGroupPeerMemoryFeaturesKHX.html vkGetDeviceGroupPeerMemoryFeaturesKHX registry at www.khronos.org>
foreign import ccall unsafe "vkGetDeviceGroupPeerMemoryFeaturesKHX"
               vkGetDeviceGroupPeerMemoryFeaturesKHX ::
               VkDevice -- ^ device
                        ->
                 Data.Word.Word32 -- ^ heapIndex
                                  ->
                   Data.Word.Word32 -- ^ localDeviceIndex
                                    ->
                     Data.Word.Word32 -- ^ remoteDeviceIndex
                                      ->
                       Foreign.Ptr.Ptr VkPeerMemoryFeatureFlagsKHX -- ^ pPeerMemoryFeatures
                                                                   -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindBufferMemory2KHR
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindBufferMemoryInfoKHR* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkBindBufferMemory2KHR.html vkBindBufferMemory2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkBindBufferMemory2KHR"
               vkBindBufferMemory2KHR ::
               VkDevice -- ^ device
                        ->
                 Data.Word.Word32 -- ^ bindInfoCount
                                  ->
                   Foreign.Ptr.Ptr VkBindBufferMemoryInfoKHR -- ^ pBindInfos
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindImageMemory2KHR
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindImageMemoryInfoKHR* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkBindImageMemory2KHR.html vkBindImageMemory2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkBindImageMemory2KHR"
               vkBindImageMemory2KHR ::
               VkDevice -- ^ device
                        ->
                 Data.Word.Word32 -- ^ bindInfoCount
                                  ->
                   Foreign.Ptr.Ptr VkBindImageMemoryInfoKHR -- ^ pBindInfos
                                                            -> IO VkResult

-- | queues: @graphics,compute,transfer@
--
--   renderpass: @both@
--
--   > void vkCmdSetDeviceMaskKHX
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t deviceMask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetDeviceMaskKHX.html vkCmdSetDeviceMaskKHX registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetDeviceMaskKHX"
               vkCmdSetDeviceMaskKHX ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Data.Word.Word32 -- ^ deviceMask
                                                   -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDeviceGroupPresentCapabilitiesKHX
--   >     ( VkDevice device
--   >     , VkDeviceGroupPresentCapabilitiesKHX* pDeviceGroupPresentCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDeviceGroupPresentCapabilitiesKHX.html vkGetDeviceGroupPresentCapabilitiesKHX registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetDeviceGroupPresentCapabilitiesKHX"
               vkGetDeviceGroupPresentCapabilitiesKHX ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkDeviceGroupPresentCapabilitiesKHX -- ^ pDeviceGroupPresentCapabilities
                                                                     -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetDeviceGroupSurfacePresentModesKHX
--   >     ( VkDevice device
--   >     , VkSurfaceKHR surface
--   >     , VkDeviceGroupPresentModeFlagsKHX* pModes
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDeviceGroupSurfacePresentModesKHX.html vkGetDeviceGroupSurfacePresentModesKHX registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetDeviceGroupSurfacePresentModesKHX"
               vkGetDeviceGroupSurfacePresentModesKHX ::
               VkDevice -- ^ device
                        ->
                 VkSurfaceKHR -- ^ surface
                              ->
                   Foreign.Ptr.Ptr VkDeviceGroupPresentModeFlagsKHX -- ^ pModes
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_TIMEOUT', 'VK_NOT_READY', 'VK_SUBOPTIMAL_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkAcquireNextImage2KHX
--   >     ( VkDevice device
--   >     , const VkAcquireNextImageInfoKHX* pAcquireInfo
--   >     , uint32_t* pImageIndex
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkAcquireNextImage2KHX.html vkAcquireNextImage2KHX registry at www.khronos.org>
foreign import ccall unsafe "vkAcquireNextImage2KHX"
               vkAcquireNextImage2KHX ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkAcquireNextImageInfoKHX -- ^ pAcquireInfo
                                                           ->
                   Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pImageIndex
                                                    -> IO VkResult

-- | queues: @compute@
--
--   renderpass: @outside@
--
--   > void vkCmdDispatchBaseKHX
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t baseGroupX
--   >     , uint32_t baseGroupY
--   >     , uint32_t baseGroupZ
--   >     , uint32_t groupCountX
--   >     , uint32_t groupCountY
--   >     , uint32_t groupCountZ
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdDispatchBaseKHX.html vkCmdDispatchBaseKHX registry at www.khronos.org>
foreign import ccall unsafe "vkCmdDispatchBaseKHX"
               vkCmdDispatchBaseKHX ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Data.Word.Word32 -- ^ baseGroupX
                                  ->
                   Data.Word.Word32 -- ^ baseGroupY
                                    ->
                     Data.Word.Word32 -- ^ baseGroupZ
                                      ->
                       Data.Word.Word32 -- ^ groupCountX
                                        -> Data.Word.Word32 -- ^ groupCountY
                                                            -> Data.Word.Word32 -- ^ groupCountZ
                                                                                -> IO ()

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDevicePresentRectanglesKHX
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSurfaceKHR surface
--   >     , uint32_t* pRectCount
--   >     , VkRect2D* pRects
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDevicePresentRectanglesKHX.html vkGetPhysicalDevicePresentRectanglesKHX registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDevicePresentRectanglesKHX"
               vkGetPhysicalDevicePresentRectanglesKHX ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              ->
                   Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pRectCount
                                                    ->
                     Foreign.Ptr.Ptr VkRect2D -- ^ pRects
                                              -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDescriptorUpdateTemplateKHR
--   >     ( VkDevice device
--   >     , const VkDescriptorUpdateTemplateCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDescriptorUpdateTemplateKHR* pDescriptorUpdateTemplate
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDescriptorUpdateTemplateKHR.html vkCreateDescriptorUpdateTemplateKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDescriptorUpdateTemplateKHR"
               vkCreateDescriptorUpdateTemplateKHR ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkDescriptorUpdateTemplateCreateInfoKHR -- ^ pCreateInfo
                                                                         ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkDescriptorUpdateTemplateKHR -- ^ pDescriptorUpdateTemplate
                                                                   -> IO VkResult

-- | > void vkDestroyDescriptorUpdateTemplateKHR
--   >     ( VkDevice device
--   >     , VkDescriptorUpdateTemplateKHR descriptorUpdateTemplate
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyDescriptorUpdateTemplateKHR.html vkDestroyDescriptorUpdateTemplateKHR registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyDescriptorUpdateTemplateKHR"
               vkDestroyDescriptorUpdateTemplateKHR ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorUpdateTemplateKHR -- ^ descriptorUpdateTemplate
                                               ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         -> IO ()

-- | > void vkUpdateDescriptorSetWithTemplateKHR
--   >     ( VkDevice device
--   >     , VkDescriptorSet descriptorSet
--   >     , VkDescriptorUpdateTemplateKHR descriptorUpdateTemplate
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkUpdateDescriptorSetWithTemplateKHR.html vkUpdateDescriptorSetWithTemplateKHR registry at www.khronos.org>
foreign import ccall unsafe "vkUpdateDescriptorSetWithTemplateKHR"
               vkUpdateDescriptorSetWithTemplateKHR ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorSet -- ^ descriptorSet
                                 ->
                   VkDescriptorUpdateTemplateKHR -- ^ descriptorUpdateTemplate
                                                 ->
                     Foreign.Ptr.Ptr Data.Void.Void -- ^ pData
                                                    -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdPushDescriptorSetWithTemplateKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkDescriptorUpdateTemplateKHR descriptorUpdateTemplate
--   >     , VkPipelineLayout layout
--   >     , uint32_t set
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdPushDescriptorSetWithTemplateKHR.html vkCmdPushDescriptorSetWithTemplateKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCmdPushDescriptorSetWithTemplateKHR"
               vkCmdPushDescriptorSetWithTemplateKHR ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkDescriptorUpdateTemplateKHR -- ^ descriptorUpdateTemplate
                                               ->
                   VkPipelineLayout -- ^ layout
                                    ->
                     Data.Word.Word32 -- ^ set
                                      -> Foreign.Ptr.Ptr Data.Void.Void -- ^ pData
                                                                        -> IO ()

-- | > void vkSetHdrMetadataEXT
--   >     ( VkDevice device
--   >     , uint32_t swapchainCount
--   >     , const VkSwapchainKHR* pSwapchains
--   >     , const VkHdrMetadataEXT* pMetadata
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkSetHdrMetadataEXT.html vkSetHdrMetadataEXT registry at www.khronos.org>
foreign import ccall unsafe "vkSetHdrMetadataEXT"
               vkSetHdrMetadataEXT ::
               VkDevice -- ^ device
                        ->
                 Data.Word.Word32 -- ^ swapchainCount
                                  ->
                   Foreign.Ptr.Ptr VkSwapchainKHR -- ^ pSwapchains
                                                  ->
                     Foreign.Ptr.Ptr VkHdrMetadataEXT -- ^ pMetadata
                                                      -> IO ()

-- | Success codes: 'VK_SUCCESS', 'VK_SUBOPTIMAL_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetSwapchainStatusKHR
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetSwapchainStatusKHR.html vkGetSwapchainStatusKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetSwapchainStatusKHR"
               vkGetSwapchainStatusKHR ::
               VkDevice -- ^ device
                        -> VkSwapchainKHR -- ^ swapchain
                                          -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetRefreshCycleDurationGOOGLE
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , VkRefreshCycleDurationGOOGLE* pDisplayTimingProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetRefreshCycleDurationGOOGLE.html vkGetRefreshCycleDurationGOOGLE registry at www.khronos.org>
foreign import ccall unsafe "vkGetRefreshCycleDurationGOOGLE"
               vkGetRefreshCycleDurationGOOGLE ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                ->
                   Foreign.Ptr.Ptr VkRefreshCycleDurationGOOGLE -- ^ pDisplayTimingProperties
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPastPresentationTimingGOOGLE
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , uint32_t* pPresentationTimingCount
--   >     , VkPastPresentationTimingGOOGLE* pPresentationTimings
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPastPresentationTimingGOOGLE.html vkGetPastPresentationTimingGOOGLE registry at www.khronos.org>
foreign import ccall unsafe "vkGetPastPresentationTimingGOOGLE"
               vkGetPastPresentationTimingGOOGLE ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                ->
                   Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pPresentationTimingCount
                                                    ->
                     Foreign.Ptr.Ptr VkPastPresentationTimingGOOGLE -- ^ pPresentationTimings
                                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateIOSSurfaceMVK
--   >     ( VkInstance instance
--   >     , const VkIOSSurfaceCreateInfoMVK* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateIOSSurfaceMVK.html vkCreateIOSSurfaceMVK registry at www.khronos.org>
foreign import ccall unsafe "vkCreateIOSSurfaceMVK"
               vkCreateIOSSurfaceMVK ::
               VkInstance -- ^ instance
                          ->
                 Foreign.Ptr.Ptr VkIOSSurfaceCreateInfoMVK -- ^ pCreateInfo
                                                           ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkSurfaceKHR -- ^ pSurface
                                                  -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateMacOSSurfaceMVK
--   >     ( VkInstance instance
--   >     , const VkMacOSSurfaceCreateInfoMVK* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateMacOSSurfaceMVK.html vkCreateMacOSSurfaceMVK registry at www.khronos.org>
foreign import ccall unsafe "vkCreateMacOSSurfaceMVK"
               vkCreateMacOSSurfaceMVK ::
               VkInstance -- ^ instance
                          ->
                 Foreign.Ptr.Ptr VkMacOSSurfaceCreateInfoMVK -- ^ pCreateInfo
                                                             ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkSurfaceKHR -- ^ pSurface
                                                  -> IO VkResult

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetViewportWScalingNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstViewport
--   >     , uint32_t viewportCount
--   >     , const VkViewportWScalingNV* pViewportWScalings
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetViewportWScalingNV.html vkCmdSetViewportWScalingNV registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetViewportWScalingNV"
               vkCmdSetViewportWScalingNV ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Data.Word.Word32 -- ^ firstViewport
                                  ->
                   Data.Word.Word32 -- ^ viewportCount
                                    -> Foreign.Ptr.Ptr VkViewportWScalingNV -- ^ pViewportWScalings
                                                                            -> IO ()

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetDiscardRectangleEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstDiscardRectangle
--   >     , uint32_t discardRectangleCount
--   >     , const VkRect2D* pDiscardRectangles
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetDiscardRectangleEXT.html vkCmdSetDiscardRectangleEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetDiscardRectangleEXT"
               vkCmdSetDiscardRectangleEXT ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Data.Word.Word32 -- ^ firstDiscardRectangle
                                  ->
                   Data.Word.Word32 -- ^ discardRectangleCount
                                    -> Foreign.Ptr.Ptr VkRect2D -- ^ pDiscardRectangles
                                                                -> IO ()

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetSampleLocationsEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkSampleLocationsInfoEXT* pSampleLocationsInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetSampleLocationsEXT.html vkCmdSetSampleLocationsEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetSampleLocationsEXT"
               vkCmdSetSampleLocationsEXT ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Foreign.Ptr.Ptr VkSampleLocationsInfoEXT -- ^ pSampleLocationsInfo
                                                          -> IO ()

-- | > void vkGetPhysicalDeviceMultisamplePropertiesEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSampleCountFlagBits samples
--   >     , VkMultisamplePropertiesEXT* pMultisampleProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceMultisamplePropertiesEXT.html vkGetPhysicalDeviceMultisamplePropertiesEXT registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceMultisamplePropertiesEXT"
               vkGetPhysicalDeviceMultisamplePropertiesEXT ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSampleCountFlagBits -- ^ samples
                                       ->
                   Foreign.Ptr.Ptr VkMultisamplePropertiesEXT -- ^ pMultisampleProperties
                                                              -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceCapabilities2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSurfaceInfo2KHR* pSurfaceInfo
--   >     , VkSurfaceCapabilities2KHR* pSurfaceCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceSurfaceCapabilities2KHR.html vkGetPhysicalDeviceSurfaceCapabilities2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceSurfaceCapabilities2KHR"
               vkGetPhysicalDeviceSurfaceCapabilities2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr VkPhysicalDeviceSurfaceInfo2KHR -- ^ pSurfaceInfo
                                                                 ->
                   Foreign.Ptr.Ptr VkSurfaceCapabilities2KHR -- ^ pSurfaceCapabilities
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceFormats2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSurfaceInfo2KHR* pSurfaceInfo
--   >     , uint32_t* pSurfaceFormatCount
--   >     , VkSurfaceFormat2KHR* pSurfaceFormats
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceSurfaceFormats2KHR.html vkGetPhysicalDeviceSurfaceFormats2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceSurfaceFormats2KHR"
               vkGetPhysicalDeviceSurfaceFormats2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Foreign.Ptr.Ptr VkPhysicalDeviceSurfaceInfo2KHR -- ^ pSurfaceInfo
                                                                 ->
                   Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pSurfaceFormatCount
                                                    ->
                     Foreign.Ptr.Ptr VkSurfaceFormat2KHR -- ^ pSurfaceFormats
                                                         -> IO VkResult

-- | > void vkGetBufferMemoryRequirements2KHR
--   >     ( VkDevice device
--   >     , const VkBufferMemoryRequirementsInfo2KHR* pInfo
--   >     , VkMemoryRequirements2KHR* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetBufferMemoryRequirements2KHR.html vkGetBufferMemoryRequirements2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetBufferMemoryRequirements2KHR"
               vkGetBufferMemoryRequirements2KHR ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkBufferMemoryRequirementsInfo2KHR -- ^ pInfo
                                                                    ->
                   Foreign.Ptr.Ptr VkMemoryRequirements2KHR -- ^ pMemoryRequirements
                                                            -> IO ()

-- | > void vkGetImageMemoryRequirements2KHR
--   >     ( VkDevice device
--   >     , const VkImageMemoryRequirementsInfo2KHR* pInfo
--   >     , VkMemoryRequirements2KHR* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetImageMemoryRequirements2KHR.html vkGetImageMemoryRequirements2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetImageMemoryRequirements2KHR"
               vkGetImageMemoryRequirements2KHR ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkImageMemoryRequirementsInfo2KHR -- ^ pInfo
                                                                   ->
                   Foreign.Ptr.Ptr VkMemoryRequirements2KHR -- ^ pMemoryRequirements
                                                            -> IO ()

-- | > void vkGetImageSparseMemoryRequirements2KHR
--   >     ( VkDevice device
--   >     , const VkImageSparseMemoryRequirementsInfo2KHR* pInfo
--   >     , uint32_t* pSparseMemoryRequirementCount
--   >     , VkSparseImageMemoryRequirements2KHR* pSparseMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetImageSparseMemoryRequirements2KHR.html vkGetImageSparseMemoryRequirements2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetImageSparseMemoryRequirements2KHR"
               vkGetImageSparseMemoryRequirements2KHR ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkImageSparseMemoryRequirementsInfo2KHR -- ^ pInfo
                                                                         ->
                   Foreign.Ptr.Ptr Data.Word.Word32 -- ^ pSparseMemoryRequirementCount
                                                    ->
                     Foreign.Ptr.Ptr VkSparseImageMemoryRequirements2KHR -- ^ pSparseMemoryRequirements
                                                                         -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateSamplerYcbcrConversionKHR
--   >     ( VkDevice device
--   >     , const VkSamplerYcbcrConversionCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSamplerYcbcrConversionKHR* pYcbcrConversion
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateSamplerYcbcrConversionKHR.html vkCreateSamplerYcbcrConversionKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateSamplerYcbcrConversionKHR"
               vkCreateSamplerYcbcrConversionKHR ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkSamplerYcbcrConversionCreateInfoKHR -- ^ pCreateInfo
                                                                       ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkSamplerYcbcrConversionKHR -- ^ pYcbcrConversion
                                                                 -> IO VkResult

-- | > void vkDestroySamplerYcbcrConversionKHR
--   >     ( VkDevice device
--   >     , VkSamplerYcbcrConversionKHR ycbcrConversion
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroySamplerYcbcrConversionKHR.html vkDestroySamplerYcbcrConversionKHR registry at www.khronos.org>
foreign import ccall unsafe "vkDestroySamplerYcbcrConversionKHR"
               vkDestroySamplerYcbcrConversionKHR ::
               VkDevice -- ^ device
                        ->
                 VkSamplerYcbcrConversionKHR -- ^ ycbcrConversion
                                             ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkCreateValidationCacheEXT
--   >     ( VkDevice device
--   >     , const VkValidationCacheCreateInfoEXT* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkValidationCacheEXT* pValidationCache
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateValidationCacheEXT.html vkCreateValidationCacheEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCreateValidationCacheEXT"
               vkCreateValidationCacheEXT ::
               VkDevice -- ^ device
                        ->
                 Foreign.Ptr.Ptr VkValidationCacheCreateInfoEXT -- ^ pCreateInfo
                                                                ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         ->
                     Foreign.Ptr.Ptr VkValidationCacheEXT -- ^ pValidationCache
                                                          -> IO VkResult

-- | > void vkDestroyValidationCacheEXT
--   >     ( VkDevice device
--   >     , VkValidationCacheEXT validationCache
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyValidationCacheEXT.html vkDestroyValidationCacheEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyValidationCacheEXT"
               vkDestroyValidationCacheEXT ::
               VkDevice -- ^ device
                        ->
                 VkValidationCacheEXT -- ^ validationCache
                                      ->
                   Foreign.Ptr.Ptr VkAllocationCallbacks -- ^ pAllocator
                                                         -> IO ()

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetValidationCacheDataEXT
--   >     ( VkDevice device
--   >     , VkValidationCacheEXT validationCache
--   >     , size_t* pDataSize
--   >     , void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetValidationCacheDataEXT.html vkGetValidationCacheDataEXT registry at www.khronos.org>
foreign import ccall unsafe "vkGetValidationCacheDataEXT"
               vkGetValidationCacheDataEXT ::
               VkDevice -- ^ device
                        ->
                 VkValidationCacheEXT -- ^ validationCache
                                      ->
                   Foreign.Ptr.Ptr Foreign.C.Types.CSize -- ^ pDataSize
                                                         ->
                     Foreign.Ptr.Ptr Data.Void.Void -- ^ pData
                                                    -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkMergeValidationCachesEXT
--   >     ( VkDevice device
--   >     , VkValidationCacheEXT dstCache
--   >     , uint32_t srcCacheCount
--   >     , const VkValidationCacheEXT* pSrcCaches
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkMergeValidationCachesEXT.html vkMergeValidationCachesEXT registry at www.khronos.org>
foreign import ccall unsafe "vkMergeValidationCachesEXT"
               vkMergeValidationCachesEXT ::
               VkDevice -- ^ device
                        ->
                 VkValidationCacheEXT -- ^ dstCache
                                      ->
                   Data.Word.Word32 -- ^ srcCacheCount
                                    ->
                     Foreign.Ptr.Ptr VkValidationCacheEXT -- ^ pSrcCaches
                                                          -> IO VkResult

-- | > VkResult vkGetSwapchainGrallocUsageANDROID
--   >     ( VkDevice device
--   >     , VkFormat format
--   >     , VkImageUsageFlags imageUsage
--   >     , int* grallocUsage
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetSwapchainGrallocUsageANDROID.html vkGetSwapchainGrallocUsageANDROID registry at www.khronos.org>
foreign import ccall unsafe "vkGetSwapchainGrallocUsageANDROID"
               vkGetSwapchainGrallocUsageANDROID ::
               VkDevice -- ^ device
                        ->
                 VkFormat -- ^ format
                          ->
                   VkImageUsageFlags -- ^ imageUsage
                                     ->
                     Foreign.Ptr.Ptr Foreign.C.Types.CInt -- ^ grallocUsage
                                                          -> IO VkResult

-- | > VkResult vkAcquireImageANDROID
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , int nativeFenceFd
--   >     , VkSemaphore semaphore
--   >     , VkFence fence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkAcquireImageANDROID.html vkAcquireImageANDROID registry at www.khronos.org>
foreign import ccall unsafe "vkAcquireImageANDROID"
               vkAcquireImageANDROID ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         ->
                   Foreign.C.Types.CInt -- ^ nativeFenceFd
                                        -> VkSemaphore -- ^ semaphore
                                                       -> VkFence -- ^ fence
                                                                  -> IO VkResult

-- | > VkResult vkQueueSignalReleaseImageANDROID
--   >     ( VkQueue queue
--   >     , uint32_t waitSemaphoreCount
--   >     , const VkSemaphore* pWaitSemaphores
--   >     , VkImage image
--   >     , int* pNativeFenceFd
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkQueueSignalReleaseImageANDROID.html vkQueueSignalReleaseImageANDROID registry at www.khronos.org>
foreign import ccall unsafe "vkQueueSignalReleaseImageANDROID"
               vkQueueSignalReleaseImageANDROID ::
               VkQueue -- ^ queue
                       ->
                 Data.Word.Word32 -- ^ waitSemaphoreCount
                                  ->
                   Foreign.Ptr.Ptr VkSemaphore -- ^ pWaitSemaphores
                                               ->
                     VkImage -- ^ image
                             -> Foreign.Ptr.Ptr Foreign.C.Types.CInt -- ^ pNativeFenceFd
                                                                     -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_FEATURE_NOT_PRESENT', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetShaderInfoAMD
--   >     ( VkDevice device
--   >     , VkPipeline pipeline
--   >     , VkShaderStageFlagBits shaderStage
--   >     , VkShaderInfoTypeAMD infoType
--   >     , size_t* pInfoSize
--   >     , void* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetShaderInfoAMD.html vkGetShaderInfoAMD registry at www.khronos.org>
foreign import ccall unsafe "vkGetShaderInfoAMD" vkGetShaderInfoAMD
               ::
               VkDevice -- ^ device
                        ->
                 VkPipeline -- ^ pipeline
                            ->
                   VkShaderStageFlagBits -- ^ shaderStage
                                         ->
                     VkShaderInfoTypeAMD -- ^ infoType
                                         ->
                       Foreign.Ptr.Ptr Foreign.C.Types.CSize -- ^ pInfoSize
                                                             ->
                         Foreign.Ptr.Ptr Data.Void.Void -- ^ pInfo
                                                        -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkGetMemoryHostPointerPropertiesEXT
--   >     ( VkDevice device
--   >     , VkExternalMemoryHandleTypeFlagBitsKHR handleType
--   >     , const void* pHostPointer
--   >     , VkMemoryHostPointerPropertiesEXT* pMemoryHostPointerProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetMemoryHostPointerPropertiesEXT.html vkGetMemoryHostPointerPropertiesEXT registry at www.khronos.org>
foreign import ccall unsafe "vkGetMemoryHostPointerPropertiesEXT"
               vkGetMemoryHostPointerPropertiesEXT ::
               VkDevice -- ^ device
                        ->
                 VkExternalMemoryHandleTypeFlagBitsKHR -- ^ handleType
                                                       ->
                   Foreign.Ptr.Ptr Data.Void.Void -- ^ pHostPointer
                                                  ->
                     Foreign.Ptr.Ptr VkMemoryHostPointerPropertiesEXT -- ^ pMemoryHostPointerProperties
                                                                      -> IO VkResult
