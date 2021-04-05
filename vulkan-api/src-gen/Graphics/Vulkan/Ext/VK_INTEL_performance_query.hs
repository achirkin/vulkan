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
module Graphics.Vulkan.Ext.VK_INTEL_performance_query
       (-- * Vulkan extension: @VK_INTEL_performance_query@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Lionel Landwerlin @llandwerlin@
        --
        -- author: @INTEL@
        --
        -- type: @device@
        --
        -- Extension number: @211@
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkInitializePerformanceApiInfoINTEL,
        VkPerformanceConfigurationAcquireInfoINTEL,
        VkPerformanceConfigurationTypeINTEL(..),
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
        VkPerformanceMarkerInfoINTEL, VkPerformanceOverrideInfoINTEL,
        VkPerformanceStreamMarkerInfoINTEL, VkPerformanceValueDataINTEL,
        VkPerformanceValueINTEL, VkQueryControlBitmask(..),
        VkQueryPipelineStatisticBitmask(..),
        VkQueryPoolSamplingModeINTEL(..), VkQueryResultBitmask(..),
        VkQueryType(..), VkQueryControlFlagBits(), VkQueryControlFlags(),
        VkQueryPipelineStatisticFlagBits(),
        VkQueryPipelineStatisticFlags(), VkQueryPoolCreateFlagBits(..),
        VkQueryResultFlagBits(), VkQueryResultFlags(),
        VkAndroidSurfaceCreateFlagsKHR(..), VkBufferViewCreateFlags(..),
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
        VkXlibSurfaceCreateFlagsKHR(..), VkQueryPoolCreateInfo,
        VkQueryPoolCreateInfoINTEL,
        VkQueryPoolPerformanceQueryCreateInfoINTEL, VkStructureType(..),
        -- > #include "vk_platform.h"
        VkInitializePerformanceApiINTEL,
        pattern VkInitializePerformanceApiINTEL,
        HS_vkInitializePerformanceApiINTEL,
        PFN_vkInitializePerformanceApiINTEL,
        VkUninitializePerformanceApiINTEL,
        pattern VkUninitializePerformanceApiINTEL,
        HS_vkUninitializePerformanceApiINTEL,
        PFN_vkUninitializePerformanceApiINTEL,
        VkCmdSetPerformanceMarkerINTEL,
        pattern VkCmdSetPerformanceMarkerINTEL,
        HS_vkCmdSetPerformanceMarkerINTEL,
        PFN_vkCmdSetPerformanceMarkerINTEL,
        VkCmdSetPerformanceStreamMarkerINTEL,
        pattern VkCmdSetPerformanceStreamMarkerINTEL,
        HS_vkCmdSetPerformanceStreamMarkerINTEL,
        PFN_vkCmdSetPerformanceStreamMarkerINTEL,
        VkCmdSetPerformanceOverrideINTEL,
        pattern VkCmdSetPerformanceOverrideINTEL,
        HS_vkCmdSetPerformanceOverrideINTEL,
        PFN_vkCmdSetPerformanceOverrideINTEL,
        VkAcquirePerformanceConfigurationINTEL,
        pattern VkAcquirePerformanceConfigurationINTEL,
        HS_vkAcquirePerformanceConfigurationINTEL,
        PFN_vkAcquirePerformanceConfigurationINTEL,
        VkReleasePerformanceConfigurationINTEL,
        pattern VkReleasePerformanceConfigurationINTEL,
        HS_vkReleasePerformanceConfigurationINTEL,
        PFN_vkReleasePerformanceConfigurationINTEL,
        VkQueueSetPerformanceConfigurationINTEL,
        pattern VkQueueSetPerformanceConfigurationINTEL,
        HS_vkQueueSetPerformanceConfigurationINTEL,
        PFN_vkQueueSetPerformanceConfigurationINTEL,
        VkGetPerformanceParameterINTEL,
        pattern VkGetPerformanceParameterINTEL,
        HS_vkGetPerformanceParameterINTEL,
        PFN_vkGetPerformanceParameterINTEL, VkResult(..),
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
        VkPerformanceCounterDescriptionKHR, VkPerformanceCounterKHR,
        VkPerformanceCounterResultKHR, VkPerformanceQuerySubmitInfoKHR,
        VK_INTEL_PERFORMANCE_QUERY_SPEC_VERSION,
        pattern VK_INTEL_PERFORMANCE_QUERY_SPEC_VERSION,
        VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME,
        pattern VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL,
        pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO_INTEL,
        pattern VK_STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL,
        pattern VK_STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL,
        pattern VK_STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL,
        pattern VK_STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL,
        pattern VK_STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL,
        pattern VK_QUERY_TYPE_PERFORMANCE_QUERY_INTEL,
        pattern VK_OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL)
       where
import GHC.Ptr                                                        (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                                   (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.ObjectType                          (VkObjectType (..))
import Graphics.Vulkan.Types.Enum.Performance
import Graphics.Vulkan.Types.Enum.Query
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.InitializePerformanceApiInfoINTEL
import Graphics.Vulkan.Types.Struct.Performance
import Graphics.Vulkan.Types.Struct.QueryPool                         (VkQueryPoolCreateInfo,
                                                                       VkQueryPoolCreateInfoINTEL,
                                                                       VkQueryPoolPerformanceQueryCreateInfoINTEL)

pattern VkInitializePerformanceApiINTEL :: CString

pattern VkInitializePerformanceApiINTEL <-
        (is_VkInitializePerformanceApiINTEL -> True)
  where
    VkInitializePerformanceApiINTEL = _VkInitializePerformanceApiINTEL

{-# INLINE _VkInitializePerformanceApiINTEL #-}

_VkInitializePerformanceApiINTEL :: CString
_VkInitializePerformanceApiINTEL
  = Ptr "vkInitializePerformanceApiINTEL\NUL"#

{-# INLINE is_VkInitializePerformanceApiINTEL #-}

is_VkInitializePerformanceApiINTEL :: CString -> Bool
is_VkInitializePerformanceApiINTEL
  = (EQ ==) . cmpCStrings _VkInitializePerformanceApiINTEL

type VkInitializePerformanceApiINTEL =
     "vkInitializePerformanceApiINTEL"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkInitializePerformanceApiINTEL
--   >     ( VkDevice device
--   >     , const VkInitializePerformanceApiInfoINTEL* pInitializeInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkInitializePerformanceApiINTEL vkInitializePerformanceApiINTEL registry at www.khronos.org>
type HS_vkInitializePerformanceApiINTEL =
     VkDevice -- ^ device
              -> Ptr VkInitializePerformanceApiInfoINTEL -- ^ pInitializeInfo
                                                         -> IO VkResult

type PFN_vkInitializePerformanceApiINTEL =
     FunPtr HS_vkInitializePerformanceApiINTEL

foreign import ccall unsafe "dynamic"
               unwrapVkInitializePerformanceApiINTELUnsafe ::
               PFN_vkInitializePerformanceApiINTEL ->
                 HS_vkInitializePerformanceApiINTEL

foreign import ccall safe "dynamic"
               unwrapVkInitializePerformanceApiINTELSafe ::
               PFN_vkInitializePerformanceApiINTEL ->
                 HS_vkInitializePerformanceApiINTEL

instance VulkanProc "vkInitializePerformanceApiINTEL" where
    type VkProcType "vkInitializePerformanceApiINTEL" =
         HS_vkInitializePerformanceApiINTEL
    vkProcSymbol = _VkInitializePerformanceApiINTEL

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkInitializePerformanceApiINTELUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkInitializePerformanceApiINTELSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkUninitializePerformanceApiINTEL :: CString

pattern VkUninitializePerformanceApiINTEL <-
        (is_VkUninitializePerformanceApiINTEL -> True)
  where
    VkUninitializePerformanceApiINTEL
      = _VkUninitializePerformanceApiINTEL

{-# INLINE _VkUninitializePerformanceApiINTEL #-}

_VkUninitializePerformanceApiINTEL :: CString
_VkUninitializePerformanceApiINTEL
  = Ptr "vkUninitializePerformanceApiINTEL\NUL"#

{-# INLINE is_VkUninitializePerformanceApiINTEL #-}

is_VkUninitializePerformanceApiINTEL :: CString -> Bool
is_VkUninitializePerformanceApiINTEL
  = (EQ ==) . cmpCStrings _VkUninitializePerformanceApiINTEL

type VkUninitializePerformanceApiINTEL =
     "vkUninitializePerformanceApiINTEL"

-- | > void vkUninitializePerformanceApiINTEL
--   >     ( VkDevice device
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkUninitializePerformanceApiINTEL vkUninitializePerformanceApiINTEL registry at www.khronos.org>
type HS_vkUninitializePerformanceApiINTEL = VkDevice -- ^ device
                                                     -> IO ()

type PFN_vkUninitializePerformanceApiINTEL =
     FunPtr HS_vkUninitializePerformanceApiINTEL

foreign import ccall unsafe "dynamic"
               unwrapVkUninitializePerformanceApiINTELUnsafe ::
               PFN_vkUninitializePerformanceApiINTEL ->
                 HS_vkUninitializePerformanceApiINTEL

foreign import ccall safe "dynamic"
               unwrapVkUninitializePerformanceApiINTELSafe ::
               PFN_vkUninitializePerformanceApiINTEL ->
                 HS_vkUninitializePerformanceApiINTEL

instance VulkanProc "vkUninitializePerformanceApiINTEL" where
    type VkProcType "vkUninitializePerformanceApiINTEL" =
         HS_vkUninitializePerformanceApiINTEL
    vkProcSymbol = _VkUninitializePerformanceApiINTEL

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkUninitializePerformanceApiINTELUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkUninitializePerformanceApiINTELSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetPerformanceMarkerINTEL :: CString

pattern VkCmdSetPerformanceMarkerINTEL <-
        (is_VkCmdSetPerformanceMarkerINTEL -> True)
  where
    VkCmdSetPerformanceMarkerINTEL = _VkCmdSetPerformanceMarkerINTEL

{-# INLINE _VkCmdSetPerformanceMarkerINTEL #-}

_VkCmdSetPerformanceMarkerINTEL :: CString
_VkCmdSetPerformanceMarkerINTEL
  = Ptr "vkCmdSetPerformanceMarkerINTEL\NUL"#

{-# INLINE is_VkCmdSetPerformanceMarkerINTEL #-}

is_VkCmdSetPerformanceMarkerINTEL :: CString -> Bool
is_VkCmdSetPerformanceMarkerINTEL
  = (EQ ==) . cmpCStrings _VkCmdSetPerformanceMarkerINTEL

type VkCmdSetPerformanceMarkerINTEL =
     "vkCmdSetPerformanceMarkerINTEL"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   Queues: 'graphics', 'compute', 'transfer'.
--
--   Renderpass: @both@
--
--   > VkResult vkCmdSetPerformanceMarkerINTEL
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkPerformanceMarkerInfoINTEL* pMarkerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetPerformanceMarkerINTEL vkCmdSetPerformanceMarkerINTEL registry at www.khronos.org>
type HS_vkCmdSetPerformanceMarkerINTEL =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr VkPerformanceMarkerInfoINTEL -- ^ pMarkerInfo
                                                         -> IO VkResult

type PFN_vkCmdSetPerformanceMarkerINTEL =
     FunPtr HS_vkCmdSetPerformanceMarkerINTEL

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetPerformanceMarkerINTELUnsafe ::
               PFN_vkCmdSetPerformanceMarkerINTEL ->
                 HS_vkCmdSetPerformanceMarkerINTEL

foreign import ccall safe "dynamic"
               unwrapVkCmdSetPerformanceMarkerINTELSafe ::
               PFN_vkCmdSetPerformanceMarkerINTEL ->
                 HS_vkCmdSetPerformanceMarkerINTEL

instance VulkanProc "vkCmdSetPerformanceMarkerINTEL" where
    type VkProcType "vkCmdSetPerformanceMarkerINTEL" =
         HS_vkCmdSetPerformanceMarkerINTEL
    vkProcSymbol = _VkCmdSetPerformanceMarkerINTEL

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetPerformanceMarkerINTELUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetPerformanceMarkerINTELSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetPerformanceStreamMarkerINTEL :: CString

pattern VkCmdSetPerformanceStreamMarkerINTEL <-
        (is_VkCmdSetPerformanceStreamMarkerINTEL -> True)
  where
    VkCmdSetPerformanceStreamMarkerINTEL
      = _VkCmdSetPerformanceStreamMarkerINTEL

{-# INLINE _VkCmdSetPerformanceStreamMarkerINTEL #-}

_VkCmdSetPerformanceStreamMarkerINTEL :: CString
_VkCmdSetPerformanceStreamMarkerINTEL
  = Ptr "vkCmdSetPerformanceStreamMarkerINTEL\NUL"#

{-# INLINE is_VkCmdSetPerformanceStreamMarkerINTEL #-}

is_VkCmdSetPerformanceStreamMarkerINTEL :: CString -> Bool
is_VkCmdSetPerformanceStreamMarkerINTEL
  = (EQ ==) . cmpCStrings _VkCmdSetPerformanceStreamMarkerINTEL

type VkCmdSetPerformanceStreamMarkerINTEL =
     "vkCmdSetPerformanceStreamMarkerINTEL"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   Queues: 'graphics', 'compute', 'transfer'.
--
--   Renderpass: @both@
--
--   > VkResult vkCmdSetPerformanceStreamMarkerINTEL
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkPerformanceStreamMarkerInfoINTEL* pMarkerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetPerformanceStreamMarkerINTEL vkCmdSetPerformanceStreamMarkerINTEL registry at www.khronos.org>
type HS_vkCmdSetPerformanceStreamMarkerINTEL =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Ptr VkPerformanceStreamMarkerInfoINTEL -- ^ pMarkerInfo
                                              -> IO VkResult

type PFN_vkCmdSetPerformanceStreamMarkerINTEL =
     FunPtr HS_vkCmdSetPerformanceStreamMarkerINTEL

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetPerformanceStreamMarkerINTELUnsafe ::
               PFN_vkCmdSetPerformanceStreamMarkerINTEL ->
                 HS_vkCmdSetPerformanceStreamMarkerINTEL

foreign import ccall safe "dynamic"
               unwrapVkCmdSetPerformanceStreamMarkerINTELSafe ::
               PFN_vkCmdSetPerformanceStreamMarkerINTEL ->
                 HS_vkCmdSetPerformanceStreamMarkerINTEL

instance VulkanProc "vkCmdSetPerformanceStreamMarkerINTEL" where
    type VkProcType "vkCmdSetPerformanceStreamMarkerINTEL" =
         HS_vkCmdSetPerformanceStreamMarkerINTEL
    vkProcSymbol = _VkCmdSetPerformanceStreamMarkerINTEL

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdSetPerformanceStreamMarkerINTELUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkCmdSetPerformanceStreamMarkerINTELSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetPerformanceOverrideINTEL :: CString

pattern VkCmdSetPerformanceOverrideINTEL <-
        (is_VkCmdSetPerformanceOverrideINTEL -> True)
  where
    VkCmdSetPerformanceOverrideINTEL
      = _VkCmdSetPerformanceOverrideINTEL

{-# INLINE _VkCmdSetPerformanceOverrideINTEL #-}

_VkCmdSetPerformanceOverrideINTEL :: CString
_VkCmdSetPerformanceOverrideINTEL
  = Ptr "vkCmdSetPerformanceOverrideINTEL\NUL"#

{-# INLINE is_VkCmdSetPerformanceOverrideINTEL #-}

is_VkCmdSetPerformanceOverrideINTEL :: CString -> Bool
is_VkCmdSetPerformanceOverrideINTEL
  = (EQ ==) . cmpCStrings _VkCmdSetPerformanceOverrideINTEL

type VkCmdSetPerformanceOverrideINTEL =
     "vkCmdSetPerformanceOverrideINTEL"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   Queues: 'graphics', 'compute', 'transfer'.
--
--   Renderpass: @both@
--
--   > VkResult vkCmdSetPerformanceOverrideINTEL
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkPerformanceOverrideInfoINTEL* pOverrideInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetPerformanceOverrideINTEL vkCmdSetPerformanceOverrideINTEL registry at www.khronos.org>
type HS_vkCmdSetPerformanceOverrideINTEL =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Ptr VkPerformanceOverrideInfoINTEL -- ^ pOverrideInfo
                                          -> IO VkResult

type PFN_vkCmdSetPerformanceOverrideINTEL =
     FunPtr HS_vkCmdSetPerformanceOverrideINTEL

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetPerformanceOverrideINTELUnsafe ::
               PFN_vkCmdSetPerformanceOverrideINTEL ->
                 HS_vkCmdSetPerformanceOverrideINTEL

foreign import ccall safe "dynamic"
               unwrapVkCmdSetPerformanceOverrideINTELSafe ::
               PFN_vkCmdSetPerformanceOverrideINTEL ->
                 HS_vkCmdSetPerformanceOverrideINTEL

instance VulkanProc "vkCmdSetPerformanceOverrideINTEL" where
    type VkProcType "vkCmdSetPerformanceOverrideINTEL" =
         HS_vkCmdSetPerformanceOverrideINTEL
    vkProcSymbol = _VkCmdSetPerformanceOverrideINTEL

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdSetPerformanceOverrideINTELUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetPerformanceOverrideINTELSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkAcquirePerformanceConfigurationINTEL :: CString

pattern VkAcquirePerformanceConfigurationINTEL <-
        (is_VkAcquirePerformanceConfigurationINTEL -> True)
  where
    VkAcquirePerformanceConfigurationINTEL
      = _VkAcquirePerformanceConfigurationINTEL

{-# INLINE _VkAcquirePerformanceConfigurationINTEL #-}

_VkAcquirePerformanceConfigurationINTEL :: CString
_VkAcquirePerformanceConfigurationINTEL
  = Ptr "vkAcquirePerformanceConfigurationINTEL\NUL"#

{-# INLINE is_VkAcquirePerformanceConfigurationINTEL #-}

is_VkAcquirePerformanceConfigurationINTEL :: CString -> Bool
is_VkAcquirePerformanceConfigurationINTEL
  = (EQ ==) . cmpCStrings _VkAcquirePerformanceConfigurationINTEL

type VkAcquirePerformanceConfigurationINTEL =
     "vkAcquirePerformanceConfigurationINTEL"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkAcquirePerformanceConfigurationINTEL
--   >     ( VkDevice device
--   >     , const VkPerformanceConfigurationAcquireInfoINTEL* pAcquireInfo
--   >     , VkPerformanceConfigurationINTEL* pConfiguration
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkAcquirePerformanceConfigurationINTEL vkAcquirePerformanceConfigurationINTEL registry at www.khronos.org>
type HS_vkAcquirePerformanceConfigurationINTEL =
     VkDevice -- ^ device
              ->
       Ptr VkPerformanceConfigurationAcquireInfoINTEL -- ^ pAcquireInfo
                                                      ->
         Ptr VkPerformanceConfigurationINTEL -- ^ pConfiguration
                                             -> IO VkResult

type PFN_vkAcquirePerformanceConfigurationINTEL =
     FunPtr HS_vkAcquirePerformanceConfigurationINTEL

foreign import ccall unsafe "dynamic"
               unwrapVkAcquirePerformanceConfigurationINTELUnsafe ::
               PFN_vkAcquirePerformanceConfigurationINTEL ->
                 HS_vkAcquirePerformanceConfigurationINTEL

foreign import ccall safe "dynamic"
               unwrapVkAcquirePerformanceConfigurationINTELSafe ::
               PFN_vkAcquirePerformanceConfigurationINTEL ->
                 HS_vkAcquirePerformanceConfigurationINTEL

instance VulkanProc "vkAcquirePerformanceConfigurationINTEL" where
    type VkProcType "vkAcquirePerformanceConfigurationINTEL" =
         HS_vkAcquirePerformanceConfigurationINTEL
    vkProcSymbol = _VkAcquirePerformanceConfigurationINTEL

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkAcquirePerformanceConfigurationINTELUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkAcquirePerformanceConfigurationINTELSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkReleasePerformanceConfigurationINTEL :: CString

pattern VkReleasePerformanceConfigurationINTEL <-
        (is_VkReleasePerformanceConfigurationINTEL -> True)
  where
    VkReleasePerformanceConfigurationINTEL
      = _VkReleasePerformanceConfigurationINTEL

{-# INLINE _VkReleasePerformanceConfigurationINTEL #-}

_VkReleasePerformanceConfigurationINTEL :: CString
_VkReleasePerformanceConfigurationINTEL
  = Ptr "vkReleasePerformanceConfigurationINTEL\NUL"#

{-# INLINE is_VkReleasePerformanceConfigurationINTEL #-}

is_VkReleasePerformanceConfigurationINTEL :: CString -> Bool
is_VkReleasePerformanceConfigurationINTEL
  = (EQ ==) . cmpCStrings _VkReleasePerformanceConfigurationINTEL

type VkReleasePerformanceConfigurationINTEL =
     "vkReleasePerformanceConfigurationINTEL"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkReleasePerformanceConfigurationINTEL
--   >     ( VkDevice device
--   >     , VkPerformanceConfigurationINTEL configuration
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkReleasePerformanceConfigurationINTEL vkReleasePerformanceConfigurationINTEL registry at www.khronos.org>
type HS_vkReleasePerformanceConfigurationINTEL =
     VkDevice -- ^ device
              -> VkPerformanceConfigurationINTEL -- ^ configuration
                                                 -> IO VkResult

type PFN_vkReleasePerformanceConfigurationINTEL =
     FunPtr HS_vkReleasePerformanceConfigurationINTEL

foreign import ccall unsafe "dynamic"
               unwrapVkReleasePerformanceConfigurationINTELUnsafe ::
               PFN_vkReleasePerformanceConfigurationINTEL ->
                 HS_vkReleasePerformanceConfigurationINTEL

foreign import ccall safe "dynamic"
               unwrapVkReleasePerformanceConfigurationINTELSafe ::
               PFN_vkReleasePerformanceConfigurationINTEL ->
                 HS_vkReleasePerformanceConfigurationINTEL

instance VulkanProc "vkReleasePerformanceConfigurationINTEL" where
    type VkProcType "vkReleasePerformanceConfigurationINTEL" =
         HS_vkReleasePerformanceConfigurationINTEL
    vkProcSymbol = _VkReleasePerformanceConfigurationINTEL

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkReleasePerformanceConfigurationINTELUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkReleasePerformanceConfigurationINTELSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkQueueSetPerformanceConfigurationINTEL :: CString

pattern VkQueueSetPerformanceConfigurationINTEL <-
        (is_VkQueueSetPerformanceConfigurationINTEL -> True)
  where
    VkQueueSetPerformanceConfigurationINTEL
      = _VkQueueSetPerformanceConfigurationINTEL

{-# INLINE _VkQueueSetPerformanceConfigurationINTEL #-}

_VkQueueSetPerformanceConfigurationINTEL :: CString
_VkQueueSetPerformanceConfigurationINTEL
  = Ptr "vkQueueSetPerformanceConfigurationINTEL\NUL"#

{-# INLINE is_VkQueueSetPerformanceConfigurationINTEL #-}

is_VkQueueSetPerformanceConfigurationINTEL :: CString -> Bool
is_VkQueueSetPerformanceConfigurationINTEL
  = (EQ ==) . cmpCStrings _VkQueueSetPerformanceConfigurationINTEL

type VkQueueSetPerformanceConfigurationINTEL =
     "vkQueueSetPerformanceConfigurationINTEL"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkQueueSetPerformanceConfigurationINTEL
--   >     ( VkQueue queue
--   >     , VkPerformanceConfigurationINTEL configuration
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkQueueSetPerformanceConfigurationINTEL vkQueueSetPerformanceConfigurationINTEL registry at www.khronos.org>
type HS_vkQueueSetPerformanceConfigurationINTEL =
     VkQueue -- ^ queue
             -> VkPerformanceConfigurationINTEL -- ^ configuration
                                                -> IO VkResult

type PFN_vkQueueSetPerformanceConfigurationINTEL =
     FunPtr HS_vkQueueSetPerformanceConfigurationINTEL

foreign import ccall unsafe "dynamic"
               unwrapVkQueueSetPerformanceConfigurationINTELUnsafe ::
               PFN_vkQueueSetPerformanceConfigurationINTEL ->
                 HS_vkQueueSetPerformanceConfigurationINTEL

foreign import ccall safe "dynamic"
               unwrapVkQueueSetPerformanceConfigurationINTELSafe ::
               PFN_vkQueueSetPerformanceConfigurationINTEL ->
                 HS_vkQueueSetPerformanceConfigurationINTEL

instance VulkanProc "vkQueueSetPerformanceConfigurationINTEL" where
    type VkProcType "vkQueueSetPerformanceConfigurationINTEL" =
         HS_vkQueueSetPerformanceConfigurationINTEL
    vkProcSymbol = _VkQueueSetPerformanceConfigurationINTEL

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkQueueSetPerformanceConfigurationINTELUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkQueueSetPerformanceConfigurationINTELSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPerformanceParameterINTEL :: CString

pattern VkGetPerformanceParameterINTEL <-
        (is_VkGetPerformanceParameterINTEL -> True)
  where
    VkGetPerformanceParameterINTEL = _VkGetPerformanceParameterINTEL

{-# INLINE _VkGetPerformanceParameterINTEL #-}

_VkGetPerformanceParameterINTEL :: CString
_VkGetPerformanceParameterINTEL
  = Ptr "vkGetPerformanceParameterINTEL\NUL"#

{-# INLINE is_VkGetPerformanceParameterINTEL #-}

is_VkGetPerformanceParameterINTEL :: CString -> Bool
is_VkGetPerformanceParameterINTEL
  = (EQ ==) . cmpCStrings _VkGetPerformanceParameterINTEL

type VkGetPerformanceParameterINTEL =
     "vkGetPerformanceParameterINTEL"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetPerformanceParameterINTEL
--   >     ( VkDevice device
--   >     , VkPerformanceParameterTypeINTEL parameter
--   >     , VkPerformanceValueINTEL* pValue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetPerformanceParameterINTEL vkGetPerformanceParameterINTEL registry at www.khronos.org>
type HS_vkGetPerformanceParameterINTEL =
     VkDevice -- ^ device
              ->
       VkPerformanceParameterTypeINTEL -- ^ parameter
                                       ->
         Ptr VkPerformanceValueINTEL -- ^ pValue
                                     -> IO VkResult

type PFN_vkGetPerformanceParameterINTEL =
     FunPtr HS_vkGetPerformanceParameterINTEL

foreign import ccall unsafe "dynamic"
               unwrapVkGetPerformanceParameterINTELUnsafe ::
               PFN_vkGetPerformanceParameterINTEL ->
                 HS_vkGetPerformanceParameterINTEL

foreign import ccall safe "dynamic"
               unwrapVkGetPerformanceParameterINTELSafe ::
               PFN_vkGetPerformanceParameterINTEL ->
                 HS_vkGetPerformanceParameterINTEL

instance VulkanProc "vkGetPerformanceParameterINTEL" where
    type VkProcType "vkGetPerformanceParameterINTEL" =
         HS_vkGetPerformanceParameterINTEL
    vkProcSymbol = _VkGetPerformanceParameterINTEL

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetPerformanceParameterINTELUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetPerformanceParameterINTELSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_INTEL_PERFORMANCE_QUERY_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_INTEL_PERFORMANCE_QUERY_SPEC_VERSION = 2

type VK_INTEL_PERFORMANCE_QUERY_SPEC_VERSION = 2

pattern VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME :: CString

pattern VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME <-
        (is_VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME -> True)
  where
    VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME
      = _VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME

{-# INLINE _VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME #-}

_VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME :: CString
_VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME
  = Ptr "VK_INTEL_performance_query\NUL"#

{-# INLINE is_VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME #-}

is_VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME :: CString -> Bool
is_VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME

type VK_INTEL_PERFORMANCE_QUERY_EXTENSION_NAME =
     "VK_INTEL_performance_query"

pattern VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL
        = VkStructureType 1000210000

-- | Backwards-compatible alias
pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO_INTEL =
        VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL

pattern VK_STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL =
        VkStructureType 1000210001

pattern VK_STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL =
        VkStructureType 1000210002

pattern VK_STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL =
        VkStructureType 1000210003

pattern VK_STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL =
        VkStructureType 1000210004

pattern VK_STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL
        = VkStructureType 1000210005

pattern VK_QUERY_TYPE_PERFORMANCE_QUERY_INTEL :: VkQueryType

pattern VK_QUERY_TYPE_PERFORMANCE_QUERY_INTEL =
        VkQueryType 1000210000

pattern VK_OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL ::
        VkObjectType

pattern VK_OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL =
        VkObjectType 1000210000
