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
module Graphics.Vulkan.Ext.VK_EXT_extended_dynamic_state
       (-- * Vulkan extension: @VK_EXT_extended_dynamic_state@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Piers Daniell @pdaniell-nv@
        --
        -- author: @EXT@
        --
        -- type: @device@
        --
        -- Extension number: @268@
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
        VkDeviceQueueCreateInfo,
        VkPhysicalDeviceExtendedDynamicStateFeaturesEXT,
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        VkStructureType(..), -- > #include "vk_platform.h"
                             VkCmdSetCullModeEXT,
        pattern VkCmdSetCullModeEXT, HS_vkCmdSetCullModeEXT,
        PFN_vkCmdSetCullModeEXT, VkCmdSetFrontFaceEXT,
        pattern VkCmdSetFrontFaceEXT, HS_vkCmdSetFrontFaceEXT,
        PFN_vkCmdSetFrontFaceEXT, VkCmdSetPrimitiveTopologyEXT,
        pattern VkCmdSetPrimitiveTopologyEXT,
        HS_vkCmdSetPrimitiveTopologyEXT, PFN_vkCmdSetPrimitiveTopologyEXT,
        VkCmdSetViewportWithCountEXT, pattern VkCmdSetViewportWithCountEXT,
        HS_vkCmdSetViewportWithCountEXT, PFN_vkCmdSetViewportWithCountEXT,
        VkCmdSetScissorWithCountEXT, pattern VkCmdSetScissorWithCountEXT,
        HS_vkCmdSetScissorWithCountEXT, PFN_vkCmdSetScissorWithCountEXT,
        VkCmdBindVertexBuffers2EXT, pattern VkCmdBindVertexBuffers2EXT,
        HS_vkCmdBindVertexBuffers2EXT, PFN_vkCmdBindVertexBuffers2EXT,
        VkCmdSetDepthTestEnableEXT, pattern VkCmdSetDepthTestEnableEXT,
        HS_vkCmdSetDepthTestEnableEXT, PFN_vkCmdSetDepthTestEnableEXT,
        VkCmdSetDepthWriteEnableEXT, pattern VkCmdSetDepthWriteEnableEXT,
        HS_vkCmdSetDepthWriteEnableEXT, PFN_vkCmdSetDepthWriteEnableEXT,
        VkCmdSetDepthCompareOpEXT, pattern VkCmdSetDepthCompareOpEXT,
        HS_vkCmdSetDepthCompareOpEXT, PFN_vkCmdSetDepthCompareOpEXT,
        VkCmdSetDepthBoundsTestEnableEXT,
        pattern VkCmdSetDepthBoundsTestEnableEXT,
        HS_vkCmdSetDepthBoundsTestEnableEXT,
        PFN_vkCmdSetDepthBoundsTestEnableEXT, VkCmdSetStencilTestEnableEXT,
        pattern VkCmdSetStencilTestEnableEXT,
        HS_vkCmdSetStencilTestEnableEXT, PFN_vkCmdSetStencilTestEnableEXT,
        VkCmdSetStencilOpEXT, pattern VkCmdSetStencilOpEXT,
        HS_vkCmdSetStencilOpEXT, PFN_vkCmdSetStencilOpEXT, VkCompareOp(..),
        VkCullModeBitmask(..), VkCullModeFlagBits(), VkCullModeFlags(),
        VkFrontFace(..), VkPrimitiveTopology(..), VkStencilFaceBitmask(..),
        VkStencilOp(..), VkStencilFaceFlagBits(), VkStencilFaceFlags(),
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
        VkValidationCacheEXT, VkValidationCacheEXT_T(), VkExtent2D,
        VkExtent3D, VkOffset2D, VkOffset3D, VkRect2D, VkRectLayerKHR,
        VkViewport, VkViewportSwizzleNV, VkViewportWScalingNV,
        VK_EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION,
        pattern VK_EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION,
        VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME,
        pattern VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT,
        pattern VK_DYNAMIC_STATE_CULL_MODE_EXT,
        pattern VK_DYNAMIC_STATE_FRONT_FACE_EXT,
        pattern VK_DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT,
        pattern VK_DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT,
        pattern VK_DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT,
        pattern VK_DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT,
        pattern VK_DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT,
        pattern VK_DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT,
        pattern VK_DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT,
        pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT,
        pattern VK_DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT,
        pattern VK_DYNAMIC_STATE_STENCIL_OP_EXT)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                        (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.CompareOp
import Graphics.Vulkan.Types.Enum.CullModeFlags
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.DynamicState             (VkDynamicState (..))
import Graphics.Vulkan.Types.Enum.FrontFace
import Graphics.Vulkan.Types.Enum.PrimitiveTopology
import Graphics.Vulkan.Types.Enum.Stencil
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.Extent
import Graphics.Vulkan.Types.Struct.Offset
import Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceExtendedDynamicStateFeaturesEXT,
                                                            VkPhysicalDeviceFeatures2)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)
import Graphics.Vulkan.Types.Struct.Rect
import Graphics.Vulkan.Types.Struct.Viewport

pattern VkCmdSetCullModeEXT :: CString

pattern VkCmdSetCullModeEXT <- (is_VkCmdSetCullModeEXT -> True)
  where
    VkCmdSetCullModeEXT = _VkCmdSetCullModeEXT

{-# INLINE _VkCmdSetCullModeEXT #-}

_VkCmdSetCullModeEXT :: CString
_VkCmdSetCullModeEXT = Ptr "vkCmdSetCullModeEXT\NUL"#

{-# INLINE is_VkCmdSetCullModeEXT #-}

is_VkCmdSetCullModeEXT :: CString -> Bool
is_VkCmdSetCullModeEXT = (EQ ==) . cmpCStrings _VkCmdSetCullModeEXT

type VkCmdSetCullModeEXT = "vkCmdSetCullModeEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetCullModeEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkCullModeFlags cullMode
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetCullModeEXT vkCmdSetCullModeEXT registry at www.khronos.org>
type HS_vkCmdSetCullModeEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkCullModeFlags -- ^ cullMode
                                        -> IO ()

type PFN_vkCmdSetCullModeEXT = FunPtr HS_vkCmdSetCullModeEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetCullModeEXTUnsafe ::
               PFN_vkCmdSetCullModeEXT -> HS_vkCmdSetCullModeEXT

foreign import ccall safe "dynamic" unwrapVkCmdSetCullModeEXTSafe
               :: PFN_vkCmdSetCullModeEXT -> HS_vkCmdSetCullModeEXT

instance VulkanProc "vkCmdSetCullModeEXT" where
    type VkProcType "vkCmdSetCullModeEXT" = HS_vkCmdSetCullModeEXT
    vkProcSymbol = _VkCmdSetCullModeEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetCullModeEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetCullModeEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetFrontFaceEXT :: CString

pattern VkCmdSetFrontFaceEXT <- (is_VkCmdSetFrontFaceEXT -> True)
  where
    VkCmdSetFrontFaceEXT = _VkCmdSetFrontFaceEXT

{-# INLINE _VkCmdSetFrontFaceEXT #-}

_VkCmdSetFrontFaceEXT :: CString
_VkCmdSetFrontFaceEXT = Ptr "vkCmdSetFrontFaceEXT\NUL"#

{-# INLINE is_VkCmdSetFrontFaceEXT #-}

is_VkCmdSetFrontFaceEXT :: CString -> Bool
is_VkCmdSetFrontFaceEXT
  = (EQ ==) . cmpCStrings _VkCmdSetFrontFaceEXT

type VkCmdSetFrontFaceEXT = "vkCmdSetFrontFaceEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetFrontFaceEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkFrontFace frontFace
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetFrontFaceEXT vkCmdSetFrontFaceEXT registry at www.khronos.org>
type HS_vkCmdSetFrontFaceEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkFrontFace -- ^ frontFace
                                    -> IO ()

type PFN_vkCmdSetFrontFaceEXT = FunPtr HS_vkCmdSetFrontFaceEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetFrontFaceEXTUnsafe ::
               PFN_vkCmdSetFrontFaceEXT -> HS_vkCmdSetFrontFaceEXT

foreign import ccall safe "dynamic" unwrapVkCmdSetFrontFaceEXTSafe
               :: PFN_vkCmdSetFrontFaceEXT -> HS_vkCmdSetFrontFaceEXT

instance VulkanProc "vkCmdSetFrontFaceEXT" where
    type VkProcType "vkCmdSetFrontFaceEXT" = HS_vkCmdSetFrontFaceEXT
    vkProcSymbol = _VkCmdSetFrontFaceEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetFrontFaceEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetFrontFaceEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetPrimitiveTopologyEXT :: CString

pattern VkCmdSetPrimitiveTopologyEXT <-
        (is_VkCmdSetPrimitiveTopologyEXT -> True)
  where
    VkCmdSetPrimitiveTopologyEXT = _VkCmdSetPrimitiveTopologyEXT

{-# INLINE _VkCmdSetPrimitiveTopologyEXT #-}

_VkCmdSetPrimitiveTopologyEXT :: CString
_VkCmdSetPrimitiveTopologyEXT
  = Ptr "vkCmdSetPrimitiveTopologyEXT\NUL"#

{-# INLINE is_VkCmdSetPrimitiveTopologyEXT #-}

is_VkCmdSetPrimitiveTopologyEXT :: CString -> Bool
is_VkCmdSetPrimitiveTopologyEXT
  = (EQ ==) . cmpCStrings _VkCmdSetPrimitiveTopologyEXT

type VkCmdSetPrimitiveTopologyEXT = "vkCmdSetPrimitiveTopologyEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetPrimitiveTopologyEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPrimitiveTopology primitiveTopology
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetPrimitiveTopologyEXT vkCmdSetPrimitiveTopologyEXT registry at www.khronos.org>
type HS_vkCmdSetPrimitiveTopologyEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkPrimitiveTopology -- ^ primitiveTopology
                                            -> IO ()

type PFN_vkCmdSetPrimitiveTopologyEXT =
     FunPtr HS_vkCmdSetPrimitiveTopologyEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetPrimitiveTopologyEXTUnsafe ::
               PFN_vkCmdSetPrimitiveTopologyEXT -> HS_vkCmdSetPrimitiveTopologyEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdSetPrimitiveTopologyEXTSafe ::
               PFN_vkCmdSetPrimitiveTopologyEXT -> HS_vkCmdSetPrimitiveTopologyEXT

instance VulkanProc "vkCmdSetPrimitiveTopologyEXT" where
    type VkProcType "vkCmdSetPrimitiveTopologyEXT" =
         HS_vkCmdSetPrimitiveTopologyEXT
    vkProcSymbol = _VkCmdSetPrimitiveTopologyEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetPrimitiveTopologyEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetPrimitiveTopologyEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetViewportWithCountEXT :: CString

pattern VkCmdSetViewportWithCountEXT <-
        (is_VkCmdSetViewportWithCountEXT -> True)
  where
    VkCmdSetViewportWithCountEXT = _VkCmdSetViewportWithCountEXT

{-# INLINE _VkCmdSetViewportWithCountEXT #-}

_VkCmdSetViewportWithCountEXT :: CString
_VkCmdSetViewportWithCountEXT
  = Ptr "vkCmdSetViewportWithCountEXT\NUL"#

{-# INLINE is_VkCmdSetViewportWithCountEXT #-}

is_VkCmdSetViewportWithCountEXT :: CString -> Bool
is_VkCmdSetViewportWithCountEXT
  = (EQ ==) . cmpCStrings _VkCmdSetViewportWithCountEXT

type VkCmdSetViewportWithCountEXT = "vkCmdSetViewportWithCountEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetViewportWithCountEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t viewportCount
--   >     , const VkViewport* pViewports
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetViewportWithCountEXT vkCmdSetViewportWithCountEXT registry at www.khronos.org>
type HS_vkCmdSetViewportWithCountEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> Word32 -- ^ viewportCount
                               -> Ptr VkViewport -- ^ pViewports
                                                 -> IO ()

type PFN_vkCmdSetViewportWithCountEXT =
     FunPtr HS_vkCmdSetViewportWithCountEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetViewportWithCountEXTUnsafe ::
               PFN_vkCmdSetViewportWithCountEXT -> HS_vkCmdSetViewportWithCountEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdSetViewportWithCountEXTSafe ::
               PFN_vkCmdSetViewportWithCountEXT -> HS_vkCmdSetViewportWithCountEXT

instance VulkanProc "vkCmdSetViewportWithCountEXT" where
    type VkProcType "vkCmdSetViewportWithCountEXT" =
         HS_vkCmdSetViewportWithCountEXT
    vkProcSymbol = _VkCmdSetViewportWithCountEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetViewportWithCountEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetViewportWithCountEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetScissorWithCountEXT :: CString

pattern VkCmdSetScissorWithCountEXT <-
        (is_VkCmdSetScissorWithCountEXT -> True)
  where
    VkCmdSetScissorWithCountEXT = _VkCmdSetScissorWithCountEXT

{-# INLINE _VkCmdSetScissorWithCountEXT #-}

_VkCmdSetScissorWithCountEXT :: CString
_VkCmdSetScissorWithCountEXT
  = Ptr "vkCmdSetScissorWithCountEXT\NUL"#

{-# INLINE is_VkCmdSetScissorWithCountEXT #-}

is_VkCmdSetScissorWithCountEXT :: CString -> Bool
is_VkCmdSetScissorWithCountEXT
  = (EQ ==) . cmpCStrings _VkCmdSetScissorWithCountEXT

type VkCmdSetScissorWithCountEXT = "vkCmdSetScissorWithCountEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetScissorWithCountEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t scissorCount
--   >     , const VkRect2D* pScissors
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetScissorWithCountEXT vkCmdSetScissorWithCountEXT registry at www.khronos.org>
type HS_vkCmdSetScissorWithCountEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> Word32 -- ^ scissorCount
                               -> Ptr VkRect2D -- ^ pScissors
                                               -> IO ()

type PFN_vkCmdSetScissorWithCountEXT =
     FunPtr HS_vkCmdSetScissorWithCountEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetScissorWithCountEXTUnsafe ::
               PFN_vkCmdSetScissorWithCountEXT -> HS_vkCmdSetScissorWithCountEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdSetScissorWithCountEXTSafe ::
               PFN_vkCmdSetScissorWithCountEXT -> HS_vkCmdSetScissorWithCountEXT

instance VulkanProc "vkCmdSetScissorWithCountEXT" where
    type VkProcType "vkCmdSetScissorWithCountEXT" =
         HS_vkCmdSetScissorWithCountEXT
    vkProcSymbol = _VkCmdSetScissorWithCountEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetScissorWithCountEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetScissorWithCountEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdBindVertexBuffers2EXT :: CString

pattern VkCmdBindVertexBuffers2EXT <-
        (is_VkCmdBindVertexBuffers2EXT -> True)
  where
    VkCmdBindVertexBuffers2EXT = _VkCmdBindVertexBuffers2EXT

{-# INLINE _VkCmdBindVertexBuffers2EXT #-}

_VkCmdBindVertexBuffers2EXT :: CString
_VkCmdBindVertexBuffers2EXT = Ptr "vkCmdBindVertexBuffers2EXT\NUL"#

{-# INLINE is_VkCmdBindVertexBuffers2EXT #-}

is_VkCmdBindVertexBuffers2EXT :: CString -> Bool
is_VkCmdBindVertexBuffers2EXT
  = (EQ ==) . cmpCStrings _VkCmdBindVertexBuffers2EXT

type VkCmdBindVertexBuffers2EXT = "vkCmdBindVertexBuffers2EXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdBindVertexBuffers2EXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstBinding
--   >     , uint32_t bindingCount
--   >     , const VkBuffer* pBuffers
--   >     , const VkDeviceSize* pOffsets
--   >     , const VkDeviceSize* pSizes
--   >     , const VkDeviceSize* pStrides
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBindVertexBuffers2EXT vkCmdBindVertexBuffers2EXT registry at www.khronos.org>
type HS_vkCmdBindVertexBuffers2EXT =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Word32 -- ^ firstBinding
              ->
         Word32 -- ^ bindingCount
                ->
           Ptr VkBuffer -- ^ pBuffers
                        ->
             Ptr VkDeviceSize -- ^ pOffsets
                              -> Ptr VkDeviceSize -- ^ pSizes
                                                  -> Ptr VkDeviceSize -- ^ pStrides
                                                                      -> IO ()

type PFN_vkCmdBindVertexBuffers2EXT =
     FunPtr HS_vkCmdBindVertexBuffers2EXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdBindVertexBuffers2EXTUnsafe ::
               PFN_vkCmdBindVertexBuffers2EXT -> HS_vkCmdBindVertexBuffers2EXT

foreign import ccall safe "dynamic"
               unwrapVkCmdBindVertexBuffers2EXTSafe ::
               PFN_vkCmdBindVertexBuffers2EXT -> HS_vkCmdBindVertexBuffers2EXT

instance VulkanProc "vkCmdBindVertexBuffers2EXT" where
    type VkProcType "vkCmdBindVertexBuffers2EXT" =
         HS_vkCmdBindVertexBuffers2EXT
    vkProcSymbol = _VkCmdBindVertexBuffers2EXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdBindVertexBuffers2EXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdBindVertexBuffers2EXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetDepthTestEnableEXT :: CString

pattern VkCmdSetDepthTestEnableEXT <-
        (is_VkCmdSetDepthTestEnableEXT -> True)
  where
    VkCmdSetDepthTestEnableEXT = _VkCmdSetDepthTestEnableEXT

{-# INLINE _VkCmdSetDepthTestEnableEXT #-}

_VkCmdSetDepthTestEnableEXT :: CString
_VkCmdSetDepthTestEnableEXT = Ptr "vkCmdSetDepthTestEnableEXT\NUL"#

{-# INLINE is_VkCmdSetDepthTestEnableEXT #-}

is_VkCmdSetDepthTestEnableEXT :: CString -> Bool
is_VkCmdSetDepthTestEnableEXT
  = (EQ ==) . cmpCStrings _VkCmdSetDepthTestEnableEXT

type VkCmdSetDepthTestEnableEXT = "vkCmdSetDepthTestEnableEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetDepthTestEnableEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBool32 depthTestEnable
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetDepthTestEnableEXT vkCmdSetDepthTestEnableEXT registry at www.khronos.org>
type HS_vkCmdSetDepthTestEnableEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkBool32 -- ^ depthTestEnable
                                 -> IO ()

type PFN_vkCmdSetDepthTestEnableEXT =
     FunPtr HS_vkCmdSetDepthTestEnableEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetDepthTestEnableEXTUnsafe ::
               PFN_vkCmdSetDepthTestEnableEXT -> HS_vkCmdSetDepthTestEnableEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdSetDepthTestEnableEXTSafe ::
               PFN_vkCmdSetDepthTestEnableEXT -> HS_vkCmdSetDepthTestEnableEXT

instance VulkanProc "vkCmdSetDepthTestEnableEXT" where
    type VkProcType "vkCmdSetDepthTestEnableEXT" =
         HS_vkCmdSetDepthTestEnableEXT
    vkProcSymbol = _VkCmdSetDepthTestEnableEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetDepthTestEnableEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetDepthTestEnableEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetDepthWriteEnableEXT :: CString

pattern VkCmdSetDepthWriteEnableEXT <-
        (is_VkCmdSetDepthWriteEnableEXT -> True)
  where
    VkCmdSetDepthWriteEnableEXT = _VkCmdSetDepthWriteEnableEXT

{-# INLINE _VkCmdSetDepthWriteEnableEXT #-}

_VkCmdSetDepthWriteEnableEXT :: CString
_VkCmdSetDepthWriteEnableEXT
  = Ptr "vkCmdSetDepthWriteEnableEXT\NUL"#

{-# INLINE is_VkCmdSetDepthWriteEnableEXT #-}

is_VkCmdSetDepthWriteEnableEXT :: CString -> Bool
is_VkCmdSetDepthWriteEnableEXT
  = (EQ ==) . cmpCStrings _VkCmdSetDepthWriteEnableEXT

type VkCmdSetDepthWriteEnableEXT = "vkCmdSetDepthWriteEnableEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetDepthWriteEnableEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBool32 depthWriteEnable
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetDepthWriteEnableEXT vkCmdSetDepthWriteEnableEXT registry at www.khronos.org>
type HS_vkCmdSetDepthWriteEnableEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkBool32 -- ^ depthWriteEnable
                                 -> IO ()

type PFN_vkCmdSetDepthWriteEnableEXT =
     FunPtr HS_vkCmdSetDepthWriteEnableEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetDepthWriteEnableEXTUnsafe ::
               PFN_vkCmdSetDepthWriteEnableEXT -> HS_vkCmdSetDepthWriteEnableEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdSetDepthWriteEnableEXTSafe ::
               PFN_vkCmdSetDepthWriteEnableEXT -> HS_vkCmdSetDepthWriteEnableEXT

instance VulkanProc "vkCmdSetDepthWriteEnableEXT" where
    type VkProcType "vkCmdSetDepthWriteEnableEXT" =
         HS_vkCmdSetDepthWriteEnableEXT
    vkProcSymbol = _VkCmdSetDepthWriteEnableEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetDepthWriteEnableEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetDepthWriteEnableEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetDepthCompareOpEXT :: CString

pattern VkCmdSetDepthCompareOpEXT <-
        (is_VkCmdSetDepthCompareOpEXT -> True)
  where
    VkCmdSetDepthCompareOpEXT = _VkCmdSetDepthCompareOpEXT

{-# INLINE _VkCmdSetDepthCompareOpEXT #-}

_VkCmdSetDepthCompareOpEXT :: CString
_VkCmdSetDepthCompareOpEXT = Ptr "vkCmdSetDepthCompareOpEXT\NUL"#

{-# INLINE is_VkCmdSetDepthCompareOpEXT #-}

is_VkCmdSetDepthCompareOpEXT :: CString -> Bool
is_VkCmdSetDepthCompareOpEXT
  = (EQ ==) . cmpCStrings _VkCmdSetDepthCompareOpEXT

type VkCmdSetDepthCompareOpEXT = "vkCmdSetDepthCompareOpEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetDepthCompareOpEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkCompareOp depthCompareOp
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetDepthCompareOpEXT vkCmdSetDepthCompareOpEXT registry at www.khronos.org>
type HS_vkCmdSetDepthCompareOpEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkCompareOp -- ^ depthCompareOp
                                    -> IO ()

type PFN_vkCmdSetDepthCompareOpEXT =
     FunPtr HS_vkCmdSetDepthCompareOpEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetDepthCompareOpEXTUnsafe ::
               PFN_vkCmdSetDepthCompareOpEXT -> HS_vkCmdSetDepthCompareOpEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdSetDepthCompareOpEXTSafe ::
               PFN_vkCmdSetDepthCompareOpEXT -> HS_vkCmdSetDepthCompareOpEXT

instance VulkanProc "vkCmdSetDepthCompareOpEXT" where
    type VkProcType "vkCmdSetDepthCompareOpEXT" =
         HS_vkCmdSetDepthCompareOpEXT
    vkProcSymbol = _VkCmdSetDepthCompareOpEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetDepthCompareOpEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetDepthCompareOpEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetDepthBoundsTestEnableEXT :: CString

pattern VkCmdSetDepthBoundsTestEnableEXT <-
        (is_VkCmdSetDepthBoundsTestEnableEXT -> True)
  where
    VkCmdSetDepthBoundsTestEnableEXT
      = _VkCmdSetDepthBoundsTestEnableEXT

{-# INLINE _VkCmdSetDepthBoundsTestEnableEXT #-}

_VkCmdSetDepthBoundsTestEnableEXT :: CString
_VkCmdSetDepthBoundsTestEnableEXT
  = Ptr "vkCmdSetDepthBoundsTestEnableEXT\NUL"#

{-# INLINE is_VkCmdSetDepthBoundsTestEnableEXT #-}

is_VkCmdSetDepthBoundsTestEnableEXT :: CString -> Bool
is_VkCmdSetDepthBoundsTestEnableEXT
  = (EQ ==) . cmpCStrings _VkCmdSetDepthBoundsTestEnableEXT

type VkCmdSetDepthBoundsTestEnableEXT =
     "vkCmdSetDepthBoundsTestEnableEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetDepthBoundsTestEnableEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBool32 depthBoundsTestEnable
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetDepthBoundsTestEnableEXT vkCmdSetDepthBoundsTestEnableEXT registry at www.khronos.org>
type HS_vkCmdSetDepthBoundsTestEnableEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkBool32 -- ^ depthBoundsTestEnable
                                 -> IO ()

type PFN_vkCmdSetDepthBoundsTestEnableEXT =
     FunPtr HS_vkCmdSetDepthBoundsTestEnableEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetDepthBoundsTestEnableEXTUnsafe ::
               PFN_vkCmdSetDepthBoundsTestEnableEXT ->
                 HS_vkCmdSetDepthBoundsTestEnableEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdSetDepthBoundsTestEnableEXTSafe ::
               PFN_vkCmdSetDepthBoundsTestEnableEXT ->
                 HS_vkCmdSetDepthBoundsTestEnableEXT

instance VulkanProc "vkCmdSetDepthBoundsTestEnableEXT" where
    type VkProcType "vkCmdSetDepthBoundsTestEnableEXT" =
         HS_vkCmdSetDepthBoundsTestEnableEXT
    vkProcSymbol = _VkCmdSetDepthBoundsTestEnableEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdSetDepthBoundsTestEnableEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetDepthBoundsTestEnableEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetStencilTestEnableEXT :: CString

pattern VkCmdSetStencilTestEnableEXT <-
        (is_VkCmdSetStencilTestEnableEXT -> True)
  where
    VkCmdSetStencilTestEnableEXT = _VkCmdSetStencilTestEnableEXT

{-# INLINE _VkCmdSetStencilTestEnableEXT #-}

_VkCmdSetStencilTestEnableEXT :: CString
_VkCmdSetStencilTestEnableEXT
  = Ptr "vkCmdSetStencilTestEnableEXT\NUL"#

{-# INLINE is_VkCmdSetStencilTestEnableEXT #-}

is_VkCmdSetStencilTestEnableEXT :: CString -> Bool
is_VkCmdSetStencilTestEnableEXT
  = (EQ ==) . cmpCStrings _VkCmdSetStencilTestEnableEXT

type VkCmdSetStencilTestEnableEXT = "vkCmdSetStencilTestEnableEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetStencilTestEnableEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkBool32 stencilTestEnable
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetStencilTestEnableEXT vkCmdSetStencilTestEnableEXT registry at www.khronos.org>
type HS_vkCmdSetStencilTestEnableEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> VkBool32 -- ^ stencilTestEnable
                                 -> IO ()

type PFN_vkCmdSetStencilTestEnableEXT =
     FunPtr HS_vkCmdSetStencilTestEnableEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetStencilTestEnableEXTUnsafe ::
               PFN_vkCmdSetStencilTestEnableEXT -> HS_vkCmdSetStencilTestEnableEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdSetStencilTestEnableEXTSafe ::
               PFN_vkCmdSetStencilTestEnableEXT -> HS_vkCmdSetStencilTestEnableEXT

instance VulkanProc "vkCmdSetStencilTestEnableEXT" where
    type VkProcType "vkCmdSetStencilTestEnableEXT" =
         HS_vkCmdSetStencilTestEnableEXT
    vkProcSymbol = _VkCmdSetStencilTestEnableEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetStencilTestEnableEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetStencilTestEnableEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdSetStencilOpEXT :: CString

pattern VkCmdSetStencilOpEXT <- (is_VkCmdSetStencilOpEXT -> True)
  where
    VkCmdSetStencilOpEXT = _VkCmdSetStencilOpEXT

{-# INLINE _VkCmdSetStencilOpEXT #-}

_VkCmdSetStencilOpEXT :: CString
_VkCmdSetStencilOpEXT = Ptr "vkCmdSetStencilOpEXT\NUL"#

{-# INLINE is_VkCmdSetStencilOpEXT #-}

is_VkCmdSetStencilOpEXT :: CString -> Bool
is_VkCmdSetStencilOpEXT
  = (EQ ==) . cmpCStrings _VkCmdSetStencilOpEXT

type VkCmdSetStencilOpEXT = "vkCmdSetStencilOpEXT"

-- | Queues: 'graphics'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetStencilOpEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkStencilFaceFlags faceMask
--   >     , VkStencilOp failOp
--   >     , VkStencilOp passOp
--   >     , VkStencilOp depthFailOp
--   >     , VkCompareOp compareOp
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetStencilOpEXT vkCmdSetStencilOpEXT registry at www.khronos.org>
type HS_vkCmdSetStencilOpEXT =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkStencilFaceFlags -- ^ faceMask
                          ->
         VkStencilOp -- ^ failOp
                     -> VkStencilOp -- ^ passOp
                                    -> VkStencilOp -- ^ depthFailOp
                                                   -> VkCompareOp -- ^ compareOp
                                                                  -> IO ()

type PFN_vkCmdSetStencilOpEXT = FunPtr HS_vkCmdSetStencilOpEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetStencilOpEXTUnsafe ::
               PFN_vkCmdSetStencilOpEXT -> HS_vkCmdSetStencilOpEXT

foreign import ccall safe "dynamic" unwrapVkCmdSetStencilOpEXTSafe
               :: PFN_vkCmdSetStencilOpEXT -> HS_vkCmdSetStencilOpEXT

instance VulkanProc "vkCmdSetStencilOpEXT" where
    type VkProcType "vkCmdSetStencilOpEXT" = HS_vkCmdSetStencilOpEXT
    vkProcSymbol = _VkCmdSetStencilOpEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetStencilOpEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetStencilOpEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION = 1

type VK_EXT_EXTENDED_DYNAMIC_STATE_SPEC_VERSION = 1

pattern VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME :: CString

pattern VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME <-
        (is_VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME -> True)
  where
    VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME
      = _VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME

{-# INLINE _VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME #-}

_VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME :: CString
_VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME
  = Ptr "VK_EXT_extended_dynamic_state\NUL"#

{-# INLINE is_VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME #-}

is_VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME

type VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME =
     "VK_EXT_extended_dynamic_state"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT
        = VkStructureType 1000267000

pattern VK_DYNAMIC_STATE_CULL_MODE_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_CULL_MODE_EXT = VkDynamicState 1000267000

pattern VK_DYNAMIC_STATE_FRONT_FACE_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_FRONT_FACE_EXT = VkDynamicState 1000267001

pattern VK_DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT =
        VkDynamicState 1000267002

pattern VK_DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT =
        VkDynamicState 1000267003

pattern VK_DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT =
        VkDynamicState 1000267004

pattern VK_DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT ::
        VkDynamicState

pattern VK_DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT =
        VkDynamicState 1000267005

pattern VK_DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT =
        VkDynamicState 1000267006

pattern VK_DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT =
        VkDynamicState 1000267007

pattern VK_DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT =
        VkDynamicState 1000267008

pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT ::
        VkDynamicState

pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT =
        VkDynamicState 1000267009

pattern VK_DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT =
        VkDynamicState 1000267010

pattern VK_DYNAMIC_STATE_STENCIL_OP_EXT :: VkDynamicState

pattern VK_DYNAMIC_STATE_STENCIL_OP_EXT = VkDynamicState 1000267011
