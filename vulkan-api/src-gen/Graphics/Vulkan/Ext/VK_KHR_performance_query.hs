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
module Graphics.Vulkan.Ext.VK_KHR_performance_query
       (-- * Vulkan extension: @VK_KHR_performance_query@
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
        -- Extension number: @117@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal,
        VkAcquireProfilingLockBitmaskKHR(..),
        VkAcquireProfilingLockFlagBitsKHR(),
        VkAcquireProfilingLockFlagsKHR(), VkAcquireProfilingLockInfoKHR,
        AHardwareBuffer(), ANativeWindow(), CAMetalLayer(), VkBool32(..),
        VkDeviceAddress(..), VkDeviceSize(..), VkFlags(..),
        VkSampleMask(..), VkAndroidSurfaceCreateFlagsKHR(..),
        VkBufferViewCreateFlags(..),
        VkBuildAccelerationStructureFlagsNV(..),
        VkCommandPoolTrimFlags(..), VkCommandPoolTrimFlagsKHR(..),
        VkDebugUtilsMessengerCallbackDataFlagsEXT(..),
        VkDebugUtilsMessengerCreateFlagsEXT(..),
        VkDescriptorBindingFlagsEXT(..), VkDescriptorPoolResetFlags(..),
        VkDescriptorUpdateTemplateCreateFlags(..),
        VkDescriptorUpdateTemplateCreateFlagsKHR(..),
        VkDeviceCreateFlags(..), VkDirectFBSurfaceCreateFlagsEXT(..),
        VkDisplayModeCreateFlagsKHR(..),
        VkDisplaySurfaceCreateFlagsKHR(..), VkEventCreateFlags(..),
        VkExternalFenceFeatureFlagsKHR(..),
        VkExternalFenceHandleTypeFlagsKHR(..),
        VkExternalMemoryFeatureFlagsKHR(..),
        VkExternalMemoryHandleTypeFlagsKHR(..),
        VkExternalSemaphoreFeatureFlagsKHR(..),
        VkExternalSemaphoreHandleTypeFlagsKHR(..),
        VkFenceImportFlagsKHR(..), VkGeometryFlagsNV(..),
        VkGeometryInstanceFlagsNV(..), VkHeadlessSurfaceCreateFlagsEXT(..),
        VkIOSSurfaceCreateFlagsMVK(..),
        VkImagePipeSurfaceCreateFlagsFUCHSIA(..),
        VkInstanceCreateFlags(..), VkMacOSSurfaceCreateFlagsMVK(..),
        VkMemoryAllocateFlagsKHR(..), VkMemoryMapFlags(..),
        VkMetalSurfaceCreateFlagsEXT(..), VkPeerMemoryFeatureFlagsKHR(..),
        VkPipelineColorBlendStateCreateFlags(..),
        VkPipelineCoverageModulationStateCreateFlagsNV(..),
        VkPipelineCoverageReductionStateCreateFlagsNV(..),
        VkPipelineCoverageToColorStateCreateFlagsNV(..),
        VkPipelineDepthStencilStateCreateFlags(..),
        VkPipelineDiscardRectangleStateCreateFlagsEXT(..),
        VkPipelineDynamicStateCreateFlags(..),
        VkPipelineInputAssemblyStateCreateFlags(..),
        VkPipelineLayoutCreateFlags(..),
        VkPipelineMultisampleStateCreateFlags(..),
        VkPipelineRasterizationConservativeStateCreateFlagsEXT(..),
        VkPipelineRasterizationDepthClipStateCreateFlagsEXT(..),
        VkPipelineRasterizationStateCreateFlags(..),
        VkPipelineRasterizationStateStreamCreateFlagsEXT(..),
        VkPipelineTessellationStateCreateFlags(..),
        VkPipelineVertexInputStateCreateFlags(..),
        VkPipelineViewportStateCreateFlags(..),
        VkPipelineViewportSwizzleStateCreateFlagsNV(..),
        VkQueryPoolCreateFlags(..), VkResolveModeFlagsKHR(..),
        VkSemaphoreCreateFlags(..), VkSemaphoreImportFlagsKHR(..),
        VkSemaphoreWaitFlagsKHR(..),
        VkStreamDescriptorSurfaceCreateFlagsGGP(..),
        VkValidationCacheCreateFlagsEXT(..), VkViSurfaceCreateFlagsNN(..),
        VkWaylandSurfaceCreateFlagsKHR(..),
        VkWin32SurfaceCreateFlagsKHR(..), VkXcbSurfaceCreateFlagsKHR(..),
        VkXlibSurfaceCreateFlagsKHR(..), VkDeviceCreateInfo,
        VkDeviceDiagnosticsConfigBitmaskNV(..), VkDeviceEventTypeEXT(..),
        VkDeviceGroupPresentModeBitmaskKHR(..), VkDeviceCreateFlagBits(..),
        VkDeviceDiagnosticsConfigFlagBitsNV(),
        VkDeviceDiagnosticsConfigFlagsNV(),
        VkDeviceGroupPresentModeFlagBitsKHR(),
        VkDeviceGroupPresentModeFlagsKHR(), VkDeviceQueueCreateBitmask(..),
        VkDeviceQueueCreateFlagBits(), VkDeviceQueueCreateFlags(),
        VkDeviceQueueCreateInfo, VkPerformanceConfigurationTypeINTEL(..),
        VkPerformanceCounterDescriptionBitmaskKHR(..),
        VkPerformanceCounterScopeKHR(..),
        VkPerformanceCounterStorageKHR(..),
        VkPerformanceCounterUnitKHR(..),
        VkPerformanceOverrideTypeINTEL(..),
        VkPerformanceParameterTypeINTEL(..),
        VkPerformanceValueTypeINTEL(..),
        pattern VK_QUERY_SCOPE_COMMAND_BUFFER_KHR,
        pattern VK_QUERY_SCOPE_COMMAND_KHR,
        pattern VK_QUERY_SCOPE_RENDER_PASS_KHR,
        VkPerformanceCounterDescriptionFlagBitsKHR(),
        VkPerformanceCounterDescriptionFlagsKHR(),
        VkPerformanceCounterDescriptionKHR, VkPerformanceCounterKHR,
        VkPerformanceCounterResultKHR, VkPerformanceQuerySubmitInfoKHR,
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        VkPhysicalDeviceLimits,
        VkPhysicalDevicePerformanceQueryFeaturesKHR,
        VkPhysicalDevicePerformanceQueryPropertiesKHR,
        VkPhysicalDeviceProperties, VkPhysicalDeviceProperties2,
        VkPhysicalDeviceSparseProperties, VkPhysicalDeviceType(..),
        VkPipelineBindPoint(..), VkPipelineCacheHeaderVersion(..),
        VkPipelineCreateBitmask(..),
        VkPipelineCreationFeedbackBitmaskEXT(..),
        VkPipelineExecutableStatisticFormatKHR(..),
        VkPipelineStageBitmask(..), VkPipelineCacheCreateBitmask(..),
        VkPipelineCacheCreateFlagBits(), VkPipelineCacheCreateFlags(),
        VkPipelineCompilerControlBitmaskAMD(..),
        VkPipelineCompilerControlFlagBitsAMD(),
        VkPipelineCompilerControlFlagsAMD(), VkPipelineCreateFlagBits(),
        VkPipelineCreateFlags(), VkPipelineCreationFeedbackFlagBitsEXT(),
        VkPipelineCreationFeedbackFlagsEXT(),
        VkPipelineShaderStageCreateBitmask(..),
        VkPipelineShaderStageCreateFlagBits(),
        VkPipelineShaderStageCreateFlags(), VkPipelineStageFlagBits(),
        VkPipelineStageFlags(), VkQueryControlBitmask(..),
        VkQueryPipelineStatisticBitmask(..),
        VkQueryPoolSamplingModeINTEL(..), VkQueryResultBitmask(..),
        VkQueryType(..), VkQueryControlFlagBits(), VkQueryControlFlags(),
        VkQueryPipelineStatisticFlagBits(),
        VkQueryPipelineStatisticFlags(), VkQueryPoolCreateFlagBits(..),
        VkQueryResultFlagBits(), VkQueryResultFlags(),
        VkQueryPoolCreateInfo, VkQueryPoolPerformanceCreateInfoKHR,
        VkSampleCountBitmask(..), VkSampleCountFlagBits(),
        VkSampleCountFlags(), VkStructureType(..), VkSubmitInfo,
        -- > #include "vk_platform.h"
        VkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR,
        pattern VkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR,
        HS_vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR,
        PFN_vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR,
        VkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR,
        pattern VkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR,
        HS_vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR,
        PFN_vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR,
        VkAcquireProfilingLockKHR, pattern VkAcquireProfilingLockKHR,
        HS_vkAcquireProfilingLockKHR, PFN_vkAcquireProfilingLockKHR,
        VkReleaseProfilingLockKHR, pattern VkReleaseProfilingLockKHR,
        HS_vkReleaseProfilingLockKHR, PFN_vkReleaseProfilingLockKHR,
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
        VkValidationCacheEXT, VkValidationCacheEXT_T(),
        VkAcquireNextImageInfoKHR,
        VkPerformanceConfigurationAcquireInfoINTEL,
        VkPerformanceMarkerInfoINTEL, VkPerformanceOverrideInfoINTEL,
        VkPerformanceStreamMarkerInfoINTEL, VkPerformanceValueDataINTEL,
        VkPerformanceValueINTEL, VkQueryPoolCreateInfoINTEL,
        VkQueryPoolPerformanceQueryCreateInfoINTEL,
        VK_KHR_PERFORMANCE_QUERY_SPEC_VERSION,
        pattern VK_KHR_PERFORMANCE_QUERY_SPEC_VERSION,
        VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME,
        pattern VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME,
        pattern VK_QUERY_TYPE_PERFORMANCE_QUERY_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR,
        pattern VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR)
       where
import GHC.Ptr                                                 (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                            (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.AcquireProfilingLockFlagsKHR
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.Performance
import Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import Graphics.Vulkan.Types.Enum.Pipeline
import Graphics.Vulkan.Types.Enum.Query
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.Acquire
import Graphics.Vulkan.Types.Struct.Device                     (VkDeviceCreateInfo,
                                                                VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.Performance
import Graphics.Vulkan.Types.Struct.PhysicalDevice             (VkPhysicalDeviceFeatures2,
                                                                VkPhysicalDeviceLimits,
                                                                VkPhysicalDevicePerformanceQueryFeaturesKHR,
                                                                VkPhysicalDevicePerformanceQueryPropertiesKHR,
                                                                VkPhysicalDeviceProperties,
                                                                VkPhysicalDeviceProperties2,
                                                                VkPhysicalDeviceSparseProperties)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures     (VkPhysicalDeviceFeatures)
import Graphics.Vulkan.Types.Struct.QueryPool
import Graphics.Vulkan.Types.Struct.SubmitInfo                 (VkSubmitInfo)

pattern VkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
        :: CString

pattern VkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
        <-
        (is_VkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
           -> True)
  where
    VkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
      = _VkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR

{-# INLINE _VkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
           #-}

_VkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR ::
                                                                 CString
_VkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
  = Ptr
      "vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR\NUL"#

{-# INLINE is_VkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
           #-}

is_VkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR ::
                                                                   CString -> Bool
is_VkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
  = (EQ ==) .
      cmpCStrings
        _VkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR

type VkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
     = "vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , uint32_t* pCounterCount
--   >     , VkPerformanceCounterKHR* pCounters
--   >     , VkPerformanceCounterDescriptionKHR* pCounterDescriptions
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR registry at www.khronos.org>
type HS_vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
     =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Word32 -- ^ queueFamilyIndex
              ->
         Ptr Word32 -- ^ pCounterCount
                    ->
           Ptr VkPerformanceCounterKHR -- ^ pCounters
                                       ->
             Ptr VkPerformanceCounterDescriptionKHR -- ^ pCounterDescriptions
                                                    -> IO VkResult

type PFN_vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
     =
     FunPtr
       HS_vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR

foreign import ccall unsafe "dynamic"
               unwrapVkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHRUnsafe
               ::
               PFN_vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
                 ->
                 HS_vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR

foreign import ccall safe "dynamic"
               unwrapVkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHRSafe
               ::
               PFN_vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
                 ->
                 HS_vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR

instance VulkanProc
           "vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR"
         where
    type VkProcType
           "vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR"
         =
         HS_vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
    vkProcSymbol
      = _VkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR ::
        CString

pattern VkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR <-
        (is_VkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR ->
           True)
  where
    VkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR
      = _VkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR

{-# INLINE _VkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR
           #-}

_VkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR :: CString
_VkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR
  = Ptr
      "vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR
           #-}

is_VkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR ::
                                                           CString -> Bool
is_VkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR
  = (EQ ==) .
      cmpCStrings
        _VkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR

type VkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR =
     "vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR"

-- | > void vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkQueryPoolPerformanceCreateInfoKHR* pPerformanceQueryCreateInfo
--   >     , uint32_t* pNumPasses
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkQueryPoolPerformanceCreateInfoKHR -- ^ pPerformanceQueryCreateInfo
                                               -> Ptr Word32 -- ^ pNumPasses
                                                             -> IO ()

type PFN_vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR =
     FunPtr HS_vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHRUnsafe
               ::
               PFN_vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR ->
                 HS_vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHRSafe
               ::
               PFN_vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR ->
                 HS_vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR

instance VulkanProc
           "vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR"
         where
    type VkProcType
           "vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR"
         = HS_vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR
    vkProcSymbol
      = _VkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkAcquireProfilingLockKHR :: CString

pattern VkAcquireProfilingLockKHR <-
        (is_VkAcquireProfilingLockKHR -> True)
  where
    VkAcquireProfilingLockKHR = _VkAcquireProfilingLockKHR

{-# INLINE _VkAcquireProfilingLockKHR #-}

_VkAcquireProfilingLockKHR :: CString
_VkAcquireProfilingLockKHR = Ptr "vkAcquireProfilingLockKHR\NUL"#

{-# INLINE is_VkAcquireProfilingLockKHR #-}

is_VkAcquireProfilingLockKHR :: CString -> Bool
is_VkAcquireProfilingLockKHR
  = (EQ ==) . cmpCStrings _VkAcquireProfilingLockKHR

type VkAcquireProfilingLockKHR = "vkAcquireProfilingLockKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_TIMEOUT'.
--
--   > VkResult vkAcquireProfilingLockKHR
--   >     ( VkDevice device
--   >     , const VkAcquireProfilingLockInfoKHR* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkAcquireProfilingLockKHR vkAcquireProfilingLockKHR registry at www.khronos.org>
type HS_vkAcquireProfilingLockKHR =
     VkDevice -- ^ device
              -> Ptr VkAcquireProfilingLockInfoKHR -- ^ pInfo
                                                   -> IO VkResult

type PFN_vkAcquireProfilingLockKHR =
     FunPtr HS_vkAcquireProfilingLockKHR

foreign import ccall unsafe "dynamic"
               unwrapVkAcquireProfilingLockKHRUnsafe ::
               PFN_vkAcquireProfilingLockKHR -> HS_vkAcquireProfilingLockKHR

foreign import ccall safe "dynamic"
               unwrapVkAcquireProfilingLockKHRSafe ::
               PFN_vkAcquireProfilingLockKHR -> HS_vkAcquireProfilingLockKHR

instance VulkanProc "vkAcquireProfilingLockKHR" where
    type VkProcType "vkAcquireProfilingLockKHR" =
         HS_vkAcquireProfilingLockKHR
    vkProcSymbol = _VkAcquireProfilingLockKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkAcquireProfilingLockKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkAcquireProfilingLockKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkReleaseProfilingLockKHR :: CString

pattern VkReleaseProfilingLockKHR <-
        (is_VkReleaseProfilingLockKHR -> True)
  where
    VkReleaseProfilingLockKHR = _VkReleaseProfilingLockKHR

{-# INLINE _VkReleaseProfilingLockKHR #-}

_VkReleaseProfilingLockKHR :: CString
_VkReleaseProfilingLockKHR = Ptr "vkReleaseProfilingLockKHR\NUL"#

{-# INLINE is_VkReleaseProfilingLockKHR #-}

is_VkReleaseProfilingLockKHR :: CString -> Bool
is_VkReleaseProfilingLockKHR
  = (EQ ==) . cmpCStrings _VkReleaseProfilingLockKHR

type VkReleaseProfilingLockKHR = "vkReleaseProfilingLockKHR"

-- | > void vkReleaseProfilingLockKHR
--   >     ( VkDevice device
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkReleaseProfilingLockKHR vkReleaseProfilingLockKHR registry at www.khronos.org>
type HS_vkReleaseProfilingLockKHR = VkDevice -- ^ device
                                             -> IO ()

type PFN_vkReleaseProfilingLockKHR =
     FunPtr HS_vkReleaseProfilingLockKHR

foreign import ccall unsafe "dynamic"
               unwrapVkReleaseProfilingLockKHRUnsafe ::
               PFN_vkReleaseProfilingLockKHR -> HS_vkReleaseProfilingLockKHR

foreign import ccall safe "dynamic"
               unwrapVkReleaseProfilingLockKHRSafe ::
               PFN_vkReleaseProfilingLockKHR -> HS_vkReleaseProfilingLockKHR

instance VulkanProc "vkReleaseProfilingLockKHR" where
    type VkProcType "vkReleaseProfilingLockKHR" =
         HS_vkReleaseProfilingLockKHR
    vkProcSymbol = _VkReleaseProfilingLockKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkReleaseProfilingLockKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkReleaseProfilingLockKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_PERFORMANCE_QUERY_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_PERFORMANCE_QUERY_SPEC_VERSION = 1

type VK_KHR_PERFORMANCE_QUERY_SPEC_VERSION = 1

pattern VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME :: CString

pattern VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME <-
        (is_VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME -> True)
  where
    VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME
      = _VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME

{-# INLINE _VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME #-}

_VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME :: CString
_VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME
  = Ptr "VK_KHR_performance_query\NUL"#

{-# INLINE is_VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME #-}

is_VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME

type VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME =
     "VK_KHR_performance_query"

pattern VK_QUERY_TYPE_PERFORMANCE_QUERY_KHR :: VkQueryType

pattern VK_QUERY_TYPE_PERFORMANCE_QUERY_KHR =
        VkQueryType 1000116000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR
        = VkStructureType 1000116000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR
        = VkStructureType 1000116001

pattern VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR =
        VkStructureType 1000116002

pattern VK_STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR =
        VkStructureType 1000116003

pattern VK_STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR =
        VkStructureType 1000116004

pattern VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR =
        VkStructureType 1000116005

pattern VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR =
        VkStructureType 1000116006
