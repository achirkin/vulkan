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
module Graphics.Vulkan.Ext.VK_NV_mesh_shader
       (-- * Vulkan extension: @VK_NV_mesh_shader@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Christoph Kubisch @pixeljetstream@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @203@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
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
        VkDeviceQueueCreateInfo, VkDrawMeshTasksIndirectCommandNV,
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        VkPhysicalDeviceLimits, VkPhysicalDeviceMeshShaderFeaturesNV,
        VkPhysicalDeviceMeshShaderPropertiesNV, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2, VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceType(..), VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VkCmdDrawMeshTasksNV, pattern VkCmdDrawMeshTasksNV,
        HS_vkCmdDrawMeshTasksNV, PFN_vkCmdDrawMeshTasksNV,
        VkCmdDrawMeshTasksIndirectNV, pattern VkCmdDrawMeshTasksIndirectNV,
        HS_vkCmdDrawMeshTasksIndirectNV, PFN_vkCmdDrawMeshTasksIndirectNV,
        VkCmdDrawMeshTasksIndirectCountNV,
        pattern VkCmdDrawMeshTasksIndirectCountNV,
        HS_vkCmdDrawMeshTasksIndirectCountNV,
        PFN_vkCmdDrawMeshTasksIndirectCountNV, VkAccelerationStructureKHR,
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
        VK_NV_MESH_SHADER_SPEC_VERSION,
        pattern VK_NV_MESH_SHADER_SPEC_VERSION,
        VK_NV_MESH_SHADER_EXTENSION_NAME,
        pattern VK_NV_MESH_SHADER_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV,
        pattern VK_SHADER_STAGE_TASK_BIT_NV,
        pattern VK_SHADER_STAGE_MESH_BIT_NV,
        pattern VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV,
        pattern VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                        (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import Graphics.Vulkan.Types.Enum.Pipeline                 (VkPipelineStageBitmask (..))
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.Shader                   (VkShaderStageBitmask (..))
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.Draw                   (VkDrawMeshTasksIndirectCommandNV)
import Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceFeatures2,
                                                            VkPhysicalDeviceLimits,
                                                            VkPhysicalDeviceMeshShaderFeaturesNV,
                                                            VkPhysicalDeviceMeshShaderPropertiesNV,
                                                            VkPhysicalDeviceProperties,
                                                            VkPhysicalDeviceProperties2,
                                                            VkPhysicalDeviceSparseProperties)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)

pattern VkCmdDrawMeshTasksNV :: CString

pattern VkCmdDrawMeshTasksNV <- (is_VkCmdDrawMeshTasksNV -> True)
  where
    VkCmdDrawMeshTasksNV = _VkCmdDrawMeshTasksNV

{-# INLINE _VkCmdDrawMeshTasksNV #-}

_VkCmdDrawMeshTasksNV :: CString
_VkCmdDrawMeshTasksNV = Ptr "vkCmdDrawMeshTasksNV\NUL"#

{-# INLINE is_VkCmdDrawMeshTasksNV #-}

is_VkCmdDrawMeshTasksNV :: CString -> Bool
is_VkCmdDrawMeshTasksNV
  = (EQ ==) . cmpCStrings _VkCmdDrawMeshTasksNV

type VkCmdDrawMeshTasksNV = "vkCmdDrawMeshTasksNV"

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdDrawMeshTasksNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t taskCount
--   >     , uint32_t firstTask
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdDrawMeshTasksNV vkCmdDrawMeshTasksNV registry at www.khronos.org>
type HS_vkCmdDrawMeshTasksNV =
     VkCommandBuffer -- ^ commandBuffer
                     -> Word32 -- ^ taskCount
                               -> Word32 -- ^ firstTask
                                         -> IO ()

type PFN_vkCmdDrawMeshTasksNV = FunPtr HS_vkCmdDrawMeshTasksNV

foreign import ccall unsafe "dynamic"
               unwrapVkCmdDrawMeshTasksNVUnsafe ::
               PFN_vkCmdDrawMeshTasksNV -> HS_vkCmdDrawMeshTasksNV

foreign import ccall safe "dynamic" unwrapVkCmdDrawMeshTasksNVSafe
               :: PFN_vkCmdDrawMeshTasksNV -> HS_vkCmdDrawMeshTasksNV

instance VulkanProc "vkCmdDrawMeshTasksNV" where
    type VkProcType "vkCmdDrawMeshTasksNV" = HS_vkCmdDrawMeshTasksNV
    vkProcSymbol = _VkCmdDrawMeshTasksNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdDrawMeshTasksNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdDrawMeshTasksNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdDrawMeshTasksIndirectNV :: CString

pattern VkCmdDrawMeshTasksIndirectNV <-
        (is_VkCmdDrawMeshTasksIndirectNV -> True)
  where
    VkCmdDrawMeshTasksIndirectNV = _VkCmdDrawMeshTasksIndirectNV

{-# INLINE _VkCmdDrawMeshTasksIndirectNV #-}

_VkCmdDrawMeshTasksIndirectNV :: CString
_VkCmdDrawMeshTasksIndirectNV
  = Ptr "vkCmdDrawMeshTasksIndirectNV\NUL"#

{-# INLINE is_VkCmdDrawMeshTasksIndirectNV #-}

is_VkCmdDrawMeshTasksIndirectNV :: CString -> Bool
is_VkCmdDrawMeshTasksIndirectNV
  = (EQ ==) . cmpCStrings _VkCmdDrawMeshTasksIndirectNV

type VkCmdDrawMeshTasksIndirectNV = "vkCmdDrawMeshTasksIndirectNV"

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdDrawMeshTasksIndirectNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , uint32_t drawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdDrawMeshTasksIndirectNV vkCmdDrawMeshTasksIndirectNV registry at www.khronos.org>
type HS_vkCmdDrawMeshTasksIndirectNV =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkBuffer -- ^ buffer
                -> VkDeviceSize -- ^ offset
                                -> Word32 -- ^ drawCount
                                          -> Word32 -- ^ stride
                                                    -> IO ()

type PFN_vkCmdDrawMeshTasksIndirectNV =
     FunPtr HS_vkCmdDrawMeshTasksIndirectNV

foreign import ccall unsafe "dynamic"
               unwrapVkCmdDrawMeshTasksIndirectNVUnsafe ::
               PFN_vkCmdDrawMeshTasksIndirectNV -> HS_vkCmdDrawMeshTasksIndirectNV

foreign import ccall safe "dynamic"
               unwrapVkCmdDrawMeshTasksIndirectNVSafe ::
               PFN_vkCmdDrawMeshTasksIndirectNV -> HS_vkCmdDrawMeshTasksIndirectNV

instance VulkanProc "vkCmdDrawMeshTasksIndirectNV" where
    type VkProcType "vkCmdDrawMeshTasksIndirectNV" =
         HS_vkCmdDrawMeshTasksIndirectNV
    vkProcSymbol = _VkCmdDrawMeshTasksIndirectNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdDrawMeshTasksIndirectNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdDrawMeshTasksIndirectNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdDrawMeshTasksIndirectCountNV :: CString

pattern VkCmdDrawMeshTasksIndirectCountNV <-
        (is_VkCmdDrawMeshTasksIndirectCountNV -> True)
  where
    VkCmdDrawMeshTasksIndirectCountNV
      = _VkCmdDrawMeshTasksIndirectCountNV

{-# INLINE _VkCmdDrawMeshTasksIndirectCountNV #-}

_VkCmdDrawMeshTasksIndirectCountNV :: CString
_VkCmdDrawMeshTasksIndirectCountNV
  = Ptr "vkCmdDrawMeshTasksIndirectCountNV\NUL"#

{-# INLINE is_VkCmdDrawMeshTasksIndirectCountNV #-}

is_VkCmdDrawMeshTasksIndirectCountNV :: CString -> Bool
is_VkCmdDrawMeshTasksIndirectCountNV
  = (EQ ==) . cmpCStrings _VkCmdDrawMeshTasksIndirectCountNV

type VkCmdDrawMeshTasksIndirectCountNV =
     "vkCmdDrawMeshTasksIndirectCountNV"

-- | Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdDrawMeshTasksIndirectCountNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBuffer buffer
--   >     , VkDeviceSize offset
--   >     , VkBuffer countBuffer
--   >     , VkDeviceSize countBufferOffset
--   >     , uint32_t maxDrawCount
--   >     , uint32_t stride
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdDrawMeshTasksIndirectCountNV vkCmdDrawMeshTasksIndirectCountNV registry at www.khronos.org>
type HS_vkCmdDrawMeshTasksIndirectCountNV =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkBuffer -- ^ buffer
                ->
         VkDeviceSize -- ^ offset
                      ->
           VkBuffer -- ^ countBuffer
                    -> VkDeviceSize -- ^ countBufferOffset
                                    -> Word32 -- ^ maxDrawCount
                                              -> Word32 -- ^ stride
                                                        -> IO ()

type PFN_vkCmdDrawMeshTasksIndirectCountNV =
     FunPtr HS_vkCmdDrawMeshTasksIndirectCountNV

foreign import ccall unsafe "dynamic"
               unwrapVkCmdDrawMeshTasksIndirectCountNVUnsafe ::
               PFN_vkCmdDrawMeshTasksIndirectCountNV ->
                 HS_vkCmdDrawMeshTasksIndirectCountNV

foreign import ccall safe "dynamic"
               unwrapVkCmdDrawMeshTasksIndirectCountNVSafe ::
               PFN_vkCmdDrawMeshTasksIndirectCountNV ->
                 HS_vkCmdDrawMeshTasksIndirectCountNV

instance VulkanProc "vkCmdDrawMeshTasksIndirectCountNV" where
    type VkProcType "vkCmdDrawMeshTasksIndirectCountNV" =
         HS_vkCmdDrawMeshTasksIndirectCountNV
    vkProcSymbol = _VkCmdDrawMeshTasksIndirectCountNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdDrawMeshTasksIndirectCountNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdDrawMeshTasksIndirectCountNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_NV_MESH_SHADER_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_MESH_SHADER_SPEC_VERSION = 1

type VK_NV_MESH_SHADER_SPEC_VERSION = 1

pattern VK_NV_MESH_SHADER_EXTENSION_NAME :: CString

pattern VK_NV_MESH_SHADER_EXTENSION_NAME <-
        (is_VK_NV_MESH_SHADER_EXTENSION_NAME -> True)
  where
    VK_NV_MESH_SHADER_EXTENSION_NAME
      = _VK_NV_MESH_SHADER_EXTENSION_NAME

{-# INLINE _VK_NV_MESH_SHADER_EXTENSION_NAME #-}

_VK_NV_MESH_SHADER_EXTENSION_NAME :: CString
_VK_NV_MESH_SHADER_EXTENSION_NAME = Ptr "VK_NV_mesh_shader\NUL"#

{-# INLINE is_VK_NV_MESH_SHADER_EXTENSION_NAME #-}

is_VK_NV_MESH_SHADER_EXTENSION_NAME :: CString -> Bool
is_VK_NV_MESH_SHADER_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_NV_MESH_SHADER_EXTENSION_NAME

type VK_NV_MESH_SHADER_EXTENSION_NAME = "VK_NV_mesh_shader"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV =
        VkStructureType 1000202000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV
        = VkStructureType 1000202001

-- | bitpos = @6@
pattern VK_SHADER_STAGE_TASK_BIT_NV :: VkShaderStageBitmask a

pattern VK_SHADER_STAGE_TASK_BIT_NV = VkShaderStageBitmask 64

-- | bitpos = @7@
pattern VK_SHADER_STAGE_MESH_BIT_NV :: VkShaderStageBitmask a

pattern VK_SHADER_STAGE_MESH_BIT_NV = VkShaderStageBitmask 128

-- | bitpos = @19@
pattern VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV =
        VkPipelineStageBitmask 524288

-- | bitpos = @20@
pattern VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV =
        VkPipelineStageBitmask 1048576
