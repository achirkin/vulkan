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
module Graphics.Vulkan.Ext.VK_EXT_conditional_rendering
       (-- * Vulkan extension: @VK_EXT_conditional_rendering@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Vikram Kushwaha @vkushwaha@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @82@
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkCommandBufferInheritanceConditionalRenderingInfoEXT,
        VkCommandBufferInheritanceInfo, VkConditionalRenderingBeginInfoEXT,
        VkConditionalRenderingBitmaskEXT(..),
        VkConditionalRenderingFlagBitsEXT(),
        VkConditionalRenderingFlagsEXT(),
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
        VkDeviceQueueCreateInfo,
        VkPhysicalDeviceConditionalRenderingFeaturesEXT,
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        VkQueryControlBitmask(..), VkQueryPipelineStatisticBitmask(..),
        VkQueryPoolSamplingModeINTEL(..), VkQueryResultBitmask(..),
        VkQueryType(..), VkQueryControlFlagBits(), VkQueryControlFlags(),
        VkQueryPipelineStatisticFlagBits(),
        VkQueryPipelineStatisticFlags(), VkQueryPoolCreateFlagBits(..),
        VkQueryResultFlagBits(), VkQueryResultFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VkCmdBeginConditionalRenderingEXT,
        pattern VkCmdBeginConditionalRenderingEXT,
        HS_vkCmdBeginConditionalRenderingEXT,
        PFN_vkCmdBeginConditionalRenderingEXT,
        VkCmdEndConditionalRenderingEXT,
        pattern VkCmdEndConditionalRenderingEXT,
        HS_vkCmdEndConditionalRenderingEXT,
        PFN_vkCmdEndConditionalRenderingEXT, VkAccelerationStructureKHR,
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
        VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION,
        pattern VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION,
        VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME,
        pattern VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT,
        pattern VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT,
        pattern VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT,
        pattern VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT,
        pattern VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT)
       where
import GHC.Ptr                                                       (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                                  (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.AccessFlags                        (VkAccessBitmask (..))
import Graphics.Vulkan.Types.Enum.Buffer                             (VkBufferUsageBitmask (..))
import Graphics.Vulkan.Types.Enum.ConditionalRenderingFlagsEXT
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.Pipeline                           (VkPipelineStageBitmask (..))
import Graphics.Vulkan.Types.Enum.Query
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.Command                          (VkCommandBufferInheritanceConditionalRenderingInfoEXT,
                                                                      VkCommandBufferInheritanceInfo)
import Graphics.Vulkan.Types.Struct.ConditionalRenderingBeginInfoEXT
import Graphics.Vulkan.Types.Struct.Device                           (VkDeviceCreateInfo,
                                                                      VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.PhysicalDevice                   (VkPhysicalDeviceConditionalRenderingFeaturesEXT,
                                                                      VkPhysicalDeviceFeatures2)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures           (VkPhysicalDeviceFeatures)

pattern VkCmdBeginConditionalRenderingEXT :: CString

pattern VkCmdBeginConditionalRenderingEXT <-
        (is_VkCmdBeginConditionalRenderingEXT -> True)
  where
    VkCmdBeginConditionalRenderingEXT
      = _VkCmdBeginConditionalRenderingEXT

{-# INLINE _VkCmdBeginConditionalRenderingEXT #-}

_VkCmdBeginConditionalRenderingEXT :: CString
_VkCmdBeginConditionalRenderingEXT
  = Ptr "vkCmdBeginConditionalRenderingEXT\NUL"#

{-# INLINE is_VkCmdBeginConditionalRenderingEXT #-}

is_VkCmdBeginConditionalRenderingEXT :: CString -> Bool
is_VkCmdBeginConditionalRenderingEXT
  = (EQ ==) . cmpCStrings _VkCmdBeginConditionalRenderingEXT

type VkCmdBeginConditionalRenderingEXT =
     "vkCmdBeginConditionalRenderingEXT"

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdBeginConditionalRenderingEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkConditionalRenderingBeginInfoEXT* pConditionalRenderingBegin
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginConditionalRenderingEXT vkCmdBeginConditionalRenderingEXT registry at www.khronos.org>
type HS_vkCmdBeginConditionalRenderingEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr VkConditionalRenderingBeginInfoEXT -- ^ pConditionalRenderingBegin
                                                               -> IO ()

type PFN_vkCmdBeginConditionalRenderingEXT =
     FunPtr HS_vkCmdBeginConditionalRenderingEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdBeginConditionalRenderingEXTUnsafe ::
               PFN_vkCmdBeginConditionalRenderingEXT ->
                 HS_vkCmdBeginConditionalRenderingEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdBeginConditionalRenderingEXTSafe ::
               PFN_vkCmdBeginConditionalRenderingEXT ->
                 HS_vkCmdBeginConditionalRenderingEXT

instance VulkanProc "vkCmdBeginConditionalRenderingEXT" where
    type VkProcType "vkCmdBeginConditionalRenderingEXT" =
         HS_vkCmdBeginConditionalRenderingEXT
    vkProcSymbol = _VkCmdBeginConditionalRenderingEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdBeginConditionalRenderingEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdBeginConditionalRenderingEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdEndConditionalRenderingEXT :: CString

pattern VkCmdEndConditionalRenderingEXT <-
        (is_VkCmdEndConditionalRenderingEXT -> True)
  where
    VkCmdEndConditionalRenderingEXT = _VkCmdEndConditionalRenderingEXT

{-# INLINE _VkCmdEndConditionalRenderingEXT #-}

_VkCmdEndConditionalRenderingEXT :: CString
_VkCmdEndConditionalRenderingEXT
  = Ptr "vkCmdEndConditionalRenderingEXT\NUL"#

{-# INLINE is_VkCmdEndConditionalRenderingEXT #-}

is_VkCmdEndConditionalRenderingEXT :: CString -> Bool
is_VkCmdEndConditionalRenderingEXT
  = (EQ ==) . cmpCStrings _VkCmdEndConditionalRenderingEXT

type VkCmdEndConditionalRenderingEXT =
     "vkCmdEndConditionalRenderingEXT"

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdEndConditionalRenderingEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdEndConditionalRenderingEXT vkCmdEndConditionalRenderingEXT registry at www.khronos.org>
type HS_vkCmdEndConditionalRenderingEXT = VkCommandBuffer -- ^ commandBuffer
                                                          -> IO ()

type PFN_vkCmdEndConditionalRenderingEXT =
     FunPtr HS_vkCmdEndConditionalRenderingEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdEndConditionalRenderingEXTUnsafe ::
               PFN_vkCmdEndConditionalRenderingEXT ->
                 HS_vkCmdEndConditionalRenderingEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdEndConditionalRenderingEXTSafe ::
               PFN_vkCmdEndConditionalRenderingEXT ->
                 HS_vkCmdEndConditionalRenderingEXT

instance VulkanProc "vkCmdEndConditionalRenderingEXT" where
    type VkProcType "vkCmdEndConditionalRenderingEXT" =
         HS_vkCmdEndConditionalRenderingEXT
    vkProcSymbol = _VkCmdEndConditionalRenderingEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdEndConditionalRenderingEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdEndConditionalRenderingEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION = 2

type VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION = 2

pattern VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME :: CString

pattern VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME <-
        (is_VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME -> True)
  where
    VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
      = _VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME

{-# INLINE _VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME #-}

_VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME :: CString
_VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
  = Ptr "VK_EXT_conditional_rendering\NUL"#

{-# INLINE is_VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME #-}

is_VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME

type VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME =
     "VK_EXT_conditional_rendering"

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT
        = VkStructureType 1000081000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT
        = VkStructureType 1000081001

pattern VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT =
        VkStructureType 1000081002

-- | read access flag for reading conditional rendering predicate
--
--   bitpos = @20@
pattern VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT ::
        VkAccessBitmask a

pattern VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT =
        VkAccessBitmask 1048576

-- | Specifies the buffer can be used as predicate in conditional rendering
--
--   bitpos = @9@
pattern VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT ::
        VkBufferUsageBitmask a

pattern VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT =
        VkBufferUsageBitmask 512

-- | A pipeline stage for conditional rendering predicate fetch
--
--   bitpos = @18@
pattern VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT =
        VkPipelineStageBitmask 262144
