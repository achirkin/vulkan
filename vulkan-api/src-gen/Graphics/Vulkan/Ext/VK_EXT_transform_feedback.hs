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
module Graphics.Vulkan.Ext.VK_EXT_transform_feedback
       (-- * Vulkan extension: @VK_EXT_transform_feedback@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Piers Daniell @pdaniell-nv@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @29@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkCullModeBitmask(..), VkCullModeFlagBits(), VkCullModeFlags(),
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
        VkXlibSurfaceCreateFlagsKHR(..), VkDeviceCreateInfo,
        VkDeviceDiagnosticsConfigBitmaskNV(..), VkDeviceEventTypeEXT(..),
        VkDeviceGroupPresentModeBitmaskKHR(..), VkDeviceCreateFlagBits(..),
        VkDeviceDiagnosticsConfigFlagBitsNV(),
        VkDeviceDiagnosticsConfigFlagsNV(),
        VkDeviceGroupPresentModeFlagBitsKHR(),
        VkDeviceGroupPresentModeFlagsKHR(), VkDeviceQueueCreateBitmask(..),
        VkDeviceQueueCreateFlagBits(), VkDeviceQueueCreateFlags(),
        VkDeviceQueueCreateInfo, VkFrontFace(..), VkPhysicalDeviceFeatures,
        VkPhysicalDeviceFeatures2, VkPhysicalDeviceLimits,
        VkPhysicalDeviceProperties, VkPhysicalDeviceProperties2,
        VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceTransformFeedbackFeaturesEXT,
        VkPhysicalDeviceTransformFeedbackPropertiesEXT,
        VkPhysicalDeviceType(..), VkPipelineRasterizationStateCreateInfo,
        VkPipelineRasterizationStateStreamCreateInfoEXT, VkPolygonMode(..),
        VkSampleCountBitmask(..), VkSampleCountFlagBits(),
        VkSampleCountFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VkCmdBindTransformFeedbackBuffersEXT,
        pattern VkCmdBindTransformFeedbackBuffersEXT,
        HS_vkCmdBindTransformFeedbackBuffersEXT,
        PFN_vkCmdBindTransformFeedbackBuffersEXT,
        VkCmdBeginTransformFeedbackEXT,
        pattern VkCmdBeginTransformFeedbackEXT,
        HS_vkCmdBeginTransformFeedbackEXT,
        PFN_vkCmdBeginTransformFeedbackEXT, VkCmdEndTransformFeedbackEXT,
        pattern VkCmdEndTransformFeedbackEXT,
        HS_vkCmdEndTransformFeedbackEXT, PFN_vkCmdEndTransformFeedbackEXT,
        VkCmdBeginQueryIndexedEXT, pattern VkCmdBeginQueryIndexedEXT,
        HS_vkCmdBeginQueryIndexedEXT, PFN_vkCmdBeginQueryIndexedEXT,
        VkCmdEndQueryIndexedEXT, pattern VkCmdEndQueryIndexedEXT,
        HS_vkCmdEndQueryIndexedEXT, PFN_vkCmdEndQueryIndexedEXT,
        VkCmdDrawIndirectByteCountEXT,
        pattern VkCmdDrawIndirectByteCountEXT,
        HS_vkCmdDrawIndirectByteCountEXT,
        PFN_vkCmdDrawIndirectByteCountEXT, VkQueryControlBitmask(..),
        VkQueryPipelineStatisticBitmask(..),
        VkQueryPoolSamplingModeINTEL(..), VkQueryResultBitmask(..),
        VkQueryType(..), VkQueryControlFlagBits(), VkQueryControlFlags(),
        VkQueryPipelineStatisticFlagBits(),
        VkQueryPipelineStatisticFlags(), VkQueryPoolCreateFlagBits(..),
        VkQueryResultFlagBits(), VkQueryResultFlags(),
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
        VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION,
        pattern VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION,
        VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME,
        pattern VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT,
        pattern VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT,
        pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT,
        pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT,
        pattern VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT,
        pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT,
        pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT,
        pattern VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                        (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.AccessFlags              (VkAccessBitmask (..))
import Graphics.Vulkan.Types.Enum.Buffer                   (VkBufferUsageBitmask (..))
import Graphics.Vulkan.Types.Enum.CullModeFlags
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.FrontFace
import Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import Graphics.Vulkan.Types.Enum.Pipeline                 (VkPipelineStageBitmask (..))
import Graphics.Vulkan.Types.Enum.PolygonMode
import Graphics.Vulkan.Types.Enum.Query
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceFeatures2,
                                                            VkPhysicalDeviceLimits,
                                                            VkPhysicalDeviceProperties,
                                                            VkPhysicalDeviceProperties2,
                                                            VkPhysicalDeviceSparseProperties,
                                                            VkPhysicalDeviceTransformFeedbackFeaturesEXT,
                                                            VkPhysicalDeviceTransformFeedbackPropertiesEXT)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)
import Graphics.Vulkan.Types.Struct.Pipeline               (VkPipelineRasterizationStateCreateInfo,
                                                            VkPipelineRasterizationStateStreamCreateInfoEXT)

pattern VkCmdBindTransformFeedbackBuffersEXT :: CString

pattern VkCmdBindTransformFeedbackBuffersEXT <-
        (is_VkCmdBindTransformFeedbackBuffersEXT -> True)
  where
    VkCmdBindTransformFeedbackBuffersEXT
      = _VkCmdBindTransformFeedbackBuffersEXT

{-# INLINE _VkCmdBindTransformFeedbackBuffersEXT #-}

_VkCmdBindTransformFeedbackBuffersEXT :: CString
_VkCmdBindTransformFeedbackBuffersEXT
  = Ptr "vkCmdBindTransformFeedbackBuffersEXT\NUL"#

{-# INLINE is_VkCmdBindTransformFeedbackBuffersEXT #-}

is_VkCmdBindTransformFeedbackBuffersEXT :: CString -> Bool
is_VkCmdBindTransformFeedbackBuffersEXT
  = (EQ ==) . cmpCStrings _VkCmdBindTransformFeedbackBuffersEXT

type VkCmdBindTransformFeedbackBuffersEXT =
     "vkCmdBindTransformFeedbackBuffersEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdBindTransformFeedbackBuffersEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstBinding
--   >     , uint32_t bindingCount
--   >     , const VkBuffer* pBuffers
--   >     , const VkDeviceSize* pOffsets
--   >     , const VkDeviceSize* pSizes
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBindTransformFeedbackBuffersEXT vkCmdBindTransformFeedbackBuffersEXT registry at www.khronos.org>
type HS_vkCmdBindTransformFeedbackBuffersEXT =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ firstBinding
              ->
         Word32 -- ^ bindingCount
                ->
           Ptr VkBuffer -- ^ pBuffers
                        -> Ptr VkDeviceSize -- ^ pOffsets
                                            -> Ptr VkDeviceSize -- ^ pSizes
                                                                -> IO ()

type PFN_vkCmdBindTransformFeedbackBuffersEXT =
     FunPtr HS_vkCmdBindTransformFeedbackBuffersEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdBindTransformFeedbackBuffersEXTUnsafe ::
               PFN_vkCmdBindTransformFeedbackBuffersEXT ->
                 HS_vkCmdBindTransformFeedbackBuffersEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdBindTransformFeedbackBuffersEXTSafe ::
               PFN_vkCmdBindTransformFeedbackBuffersEXT ->
                 HS_vkCmdBindTransformFeedbackBuffersEXT

instance VulkanProc "vkCmdBindTransformFeedbackBuffersEXT" where
    type VkProcType "vkCmdBindTransformFeedbackBuffersEXT" =
         HS_vkCmdBindTransformFeedbackBuffersEXT
    vkProcSymbol = _VkCmdBindTransformFeedbackBuffersEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdBindTransformFeedbackBuffersEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkCmdBindTransformFeedbackBuffersEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdBeginTransformFeedbackEXT :: CString

pattern VkCmdBeginTransformFeedbackEXT <-
        (is_VkCmdBeginTransformFeedbackEXT -> True)
  where
    VkCmdBeginTransformFeedbackEXT = _VkCmdBeginTransformFeedbackEXT

{-# INLINE _VkCmdBeginTransformFeedbackEXT #-}

_VkCmdBeginTransformFeedbackEXT :: CString
_VkCmdBeginTransformFeedbackEXT
  = Ptr "vkCmdBeginTransformFeedbackEXT\NUL"#

{-# INLINE is_VkCmdBeginTransformFeedbackEXT #-}

is_VkCmdBeginTransformFeedbackEXT :: CString -> Bool
is_VkCmdBeginTransformFeedbackEXT
  = (EQ ==) . cmpCStrings _VkCmdBeginTransformFeedbackEXT

type VkCmdBeginTransformFeedbackEXT =
     "vkCmdBeginTransformFeedbackEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   > void vkCmdBeginTransformFeedbackEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstCounterBuffer
--   >     , uint32_t counterBufferCount
--   >     , const VkBuffer* pCounterBuffers
--   >     , const VkDeviceSize* pCounterBufferOffsets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginTransformFeedbackEXT vkCmdBeginTransformFeedbackEXT registry at www.khronos.org>
type HS_vkCmdBeginTransformFeedbackEXT =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ firstCounterBuffer
              -> Word32 -- ^ counterBufferCount
                        -> Ptr VkBuffer -- ^ pCounterBuffers
                                        -> Ptr VkDeviceSize -- ^ pCounterBufferOffsets
                                                            -> IO ()

type PFN_vkCmdBeginTransformFeedbackEXT =
     FunPtr HS_vkCmdBeginTransformFeedbackEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdBeginTransformFeedbackEXTUnsafe ::
               PFN_vkCmdBeginTransformFeedbackEXT ->
                 HS_vkCmdBeginTransformFeedbackEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdBeginTransformFeedbackEXTSafe ::
               PFN_vkCmdBeginTransformFeedbackEXT ->
                 HS_vkCmdBeginTransformFeedbackEXT

instance VulkanProc "vkCmdBeginTransformFeedbackEXT" where
    type VkProcType "vkCmdBeginTransformFeedbackEXT" =
         HS_vkCmdBeginTransformFeedbackEXT
    vkProcSymbol = _VkCmdBeginTransformFeedbackEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdBeginTransformFeedbackEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdBeginTransformFeedbackEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdEndTransformFeedbackEXT :: CString

pattern VkCmdEndTransformFeedbackEXT <-
        (is_VkCmdEndTransformFeedbackEXT -> True)
  where
    VkCmdEndTransformFeedbackEXT = _VkCmdEndTransformFeedbackEXT

{-# INLINE _VkCmdEndTransformFeedbackEXT #-}

_VkCmdEndTransformFeedbackEXT :: CString
_VkCmdEndTransformFeedbackEXT
  = Ptr "vkCmdEndTransformFeedbackEXT\NUL"#

{-# INLINE is_VkCmdEndTransformFeedbackEXT #-}

is_VkCmdEndTransformFeedbackEXT :: CString -> Bool
is_VkCmdEndTransformFeedbackEXT
  = (EQ ==) . cmpCStrings _VkCmdEndTransformFeedbackEXT

type VkCmdEndTransformFeedbackEXT = "vkCmdEndTransformFeedbackEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   > void vkCmdEndTransformFeedbackEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstCounterBuffer
--   >     , uint32_t counterBufferCount
--   >     , const VkBuffer* pCounterBuffers
--   >     , const VkDeviceSize* pCounterBufferOffsets
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdEndTransformFeedbackEXT vkCmdEndTransformFeedbackEXT registry at www.khronos.org>
type HS_vkCmdEndTransformFeedbackEXT =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ firstCounterBuffer
              -> Word32 -- ^ counterBufferCount
                        -> Ptr VkBuffer -- ^ pCounterBuffers
                                        -> Ptr VkDeviceSize -- ^ pCounterBufferOffsets
                                                            -> IO ()

type PFN_vkCmdEndTransformFeedbackEXT =
     FunPtr HS_vkCmdEndTransformFeedbackEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdEndTransformFeedbackEXTUnsafe ::
               PFN_vkCmdEndTransformFeedbackEXT -> HS_vkCmdEndTransformFeedbackEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdEndTransformFeedbackEXTSafe ::
               PFN_vkCmdEndTransformFeedbackEXT -> HS_vkCmdEndTransformFeedbackEXT

instance VulkanProc "vkCmdEndTransformFeedbackEXT" where
    type VkProcType "vkCmdEndTransformFeedbackEXT" =
         HS_vkCmdEndTransformFeedbackEXT
    vkProcSymbol = _VkCmdEndTransformFeedbackEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdEndTransformFeedbackEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdEndTransformFeedbackEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdBeginQueryIndexedEXT :: CString

pattern VkCmdBeginQueryIndexedEXT <-
        (is_VkCmdBeginQueryIndexedEXT -> True)
  where
    VkCmdBeginQueryIndexedEXT = _VkCmdBeginQueryIndexedEXT

{-# INLINE _VkCmdBeginQueryIndexedEXT #-}

_VkCmdBeginQueryIndexedEXT :: CString
_VkCmdBeginQueryIndexedEXT = Ptr "vkCmdBeginQueryIndexedEXT\NUL"#

{-# INLINE is_VkCmdBeginQueryIndexedEXT #-}

is_VkCmdBeginQueryIndexedEXT :: CString -> Bool
is_VkCmdBeginQueryIndexedEXT
  = (EQ ==) . cmpCStrings _VkCmdBeginQueryIndexedEXT

type VkCmdBeginQueryIndexedEXT = "vkCmdBeginQueryIndexedEXT"

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdBeginQueryIndexedEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     , VkQueryControlFlags flags
--   >     , uint32_t index
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginQueryIndexedEXT vkCmdBeginQueryIndexedEXT registry at www.khronos.org>
type HS_vkCmdBeginQueryIndexedEXT =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkQueryPool -- ^ queryPool
                   -> Word32 -- ^ query
                             -> VkQueryControlFlags -- ^ flags
                                                    -> Word32 -- ^ index
                                                              -> IO ()

type PFN_vkCmdBeginQueryIndexedEXT =
     FunPtr HS_vkCmdBeginQueryIndexedEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdBeginQueryIndexedEXTUnsafe ::
               PFN_vkCmdBeginQueryIndexedEXT -> HS_vkCmdBeginQueryIndexedEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdBeginQueryIndexedEXTSafe ::
               PFN_vkCmdBeginQueryIndexedEXT -> HS_vkCmdBeginQueryIndexedEXT

instance VulkanProc "vkCmdBeginQueryIndexedEXT" where
    type VkProcType "vkCmdBeginQueryIndexedEXT" =
         HS_vkCmdBeginQueryIndexedEXT
    vkProcSymbol = _VkCmdBeginQueryIndexedEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdBeginQueryIndexedEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdBeginQueryIndexedEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdEndQueryIndexedEXT :: CString

pattern VkCmdEndQueryIndexedEXT <-
        (is_VkCmdEndQueryIndexedEXT -> True)
  where
    VkCmdEndQueryIndexedEXT = _VkCmdEndQueryIndexedEXT

{-# INLINE _VkCmdEndQueryIndexedEXT #-}

_VkCmdEndQueryIndexedEXT :: CString
_VkCmdEndQueryIndexedEXT = Ptr "vkCmdEndQueryIndexedEXT\NUL"#

{-# INLINE is_VkCmdEndQueryIndexedEXT #-}

is_VkCmdEndQueryIndexedEXT :: CString -> Bool
is_VkCmdEndQueryIndexedEXT
  = (EQ ==) . cmpCStrings _VkCmdEndQueryIndexedEXT

type VkCmdEndQueryIndexedEXT = "vkCmdEndQueryIndexedEXT"

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdEndQueryIndexedEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkQueryPool queryPool
--   >     , uint32_t query
--   >     , uint32_t index
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdEndQueryIndexedEXT vkCmdEndQueryIndexedEXT registry at www.khronos.org>
type HS_vkCmdEndQueryIndexedEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkQueryPool -- ^ queryPool
                                    -> Word32 -- ^ query
                                              -> Word32 -- ^ index
                                                        -> IO ()

type PFN_vkCmdEndQueryIndexedEXT =
     FunPtr HS_vkCmdEndQueryIndexedEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdEndQueryIndexedEXTUnsafe ::
               PFN_vkCmdEndQueryIndexedEXT -> HS_vkCmdEndQueryIndexedEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdEndQueryIndexedEXTSafe ::
               PFN_vkCmdEndQueryIndexedEXT -> HS_vkCmdEndQueryIndexedEXT

instance VulkanProc "vkCmdEndQueryIndexedEXT" where
    type VkProcType "vkCmdEndQueryIndexedEXT" =
         HS_vkCmdEndQueryIndexedEXT
    vkProcSymbol = _VkCmdEndQueryIndexedEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdEndQueryIndexedEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdEndQueryIndexedEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdDrawIndirectByteCountEXT :: CString

pattern VkCmdDrawIndirectByteCountEXT <-
        (is_VkCmdDrawIndirectByteCountEXT -> True)
  where
    VkCmdDrawIndirectByteCountEXT = _VkCmdDrawIndirectByteCountEXT

{-# INLINE _VkCmdDrawIndirectByteCountEXT #-}

_VkCmdDrawIndirectByteCountEXT :: CString
_VkCmdDrawIndirectByteCountEXT
  = Ptr "vkCmdDrawIndirectByteCountEXT\NUL"#

{-# INLINE is_VkCmdDrawIndirectByteCountEXT #-}

is_VkCmdDrawIndirectByteCountEXT :: CString -> Bool
is_VkCmdDrawIndirectByteCountEXT
  = (EQ ==) . cmpCStrings _VkCmdDrawIndirectByteCountEXT

type VkCmdDrawIndirectByteCountEXT =
     "vkCmdDrawIndirectByteCountEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdDrawIndirectByteCountEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t instanceCount
--   >     , uint32_t firstInstance
--   >     , VkBuffer counterBuffer
--   >     , VkDeviceSize counterBufferOffset
--   >     , uint32_t counterOffset
--   >     , uint32_t vertexStride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdDrawIndirectByteCountEXT vkCmdDrawIndirectByteCountEXT registry at www.khronos.org>
type HS_vkCmdDrawIndirectByteCountEXT =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ instanceCount
              ->
         Word32 -- ^ firstInstance
                -> VkBuffer -- ^ counterBuffer
                            -> VkDeviceSize -- ^ counterBufferOffset
                                            -> Word32 -- ^ counterOffset
                                                      -> Word32 -- ^ vertexStride
                                                                -> IO ()

type PFN_vkCmdDrawIndirectByteCountEXT =
     FunPtr HS_vkCmdDrawIndirectByteCountEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdDrawIndirectByteCountEXTUnsafe ::
               PFN_vkCmdDrawIndirectByteCountEXT ->
                 HS_vkCmdDrawIndirectByteCountEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdDrawIndirectByteCountEXTSafe ::
               PFN_vkCmdDrawIndirectByteCountEXT ->
                 HS_vkCmdDrawIndirectByteCountEXT

instance VulkanProc "vkCmdDrawIndirectByteCountEXT" where
    type VkProcType "vkCmdDrawIndirectByteCountEXT" =
         HS_vkCmdDrawIndirectByteCountEXT
    vkProcSymbol = _VkCmdDrawIndirectByteCountEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdDrawIndirectByteCountEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdDrawIndirectByteCountEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION = 1

type VK_EXT_TRANSFORM_FEEDBACK_SPEC_VERSION = 1

pattern VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME :: CString

pattern VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME <-
        (is_VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME -> True)
  where
    VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
      = _VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME

{-# INLINE _VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME #-}

_VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME :: CString
_VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
  = Ptr "VK_EXT_transform_feedback\NUL"#

{-# INLINE is_VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME #-}

is_VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME

type VK_EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME =
     "VK_EXT_transform_feedback"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT
        = VkStructureType 1000028000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT
        = VkStructureType 1000028001

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT
        = VkStructureType 1000028002

pattern VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT :: VkQueryType

pattern VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT =
        VkQueryType 1000028004

-- | bitpos = @11@
pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT ::
        VkBufferUsageBitmask a

pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT =
        VkBufferUsageBitmask 2048

-- | bitpos = @12@
pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
        :: VkBufferUsageBitmask a

pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT =
        VkBufferUsageBitmask 4096

-- | bitpos = @25@
pattern VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT ::
        VkAccessBitmask a

pattern VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT =
        VkAccessBitmask 33554432

-- | bitpos = @26@
pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT ::
        VkAccessBitmask a

pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT =
        VkAccessBitmask 67108864

-- | bitpos = @27@
pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT ::
        VkAccessBitmask a

pattern VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT =
        VkAccessBitmask 134217728

-- | bitpos = @24@
pattern VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT =
        VkPipelineStageBitmask 16777216
