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
module Graphics.Vulkan.Ext.VK_NV_device_diagnostic_checkpoints
       (-- * Vulkan extension: @VK_NV_device_diagnostic_checkpoints@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Nuno Subtil @nsubtil@
        --
        -- author: @NVIDIA@
        --
        -- type: @device@
        --
        -- Extension number: @207@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkCheckpointDataNV, VkExtent3D, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
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
        VkPipelineStageFlags(), VkQueueFamilyCheckpointPropertiesNV,
        VkQueueFamilyProperties, VkQueueFamilyProperties2,
        VkQueueBitmask(..), VkQueueGlobalPriorityEXT(..),
        VkQueueFlagBits(), VkQueueFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VkCmdSetCheckpointNV, pattern VkCmdSetCheckpointNV,
        HS_vkCmdSetCheckpointNV, PFN_vkCmdSetCheckpointNV,
        VkGetQueueCheckpointDataNV, pattern VkGetQueueCheckpointDataNV,
        HS_vkGetQueueCheckpointDataNV, PFN_vkGetQueueCheckpointDataNV,
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
        VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION,
        pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION,
        VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME,
        pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV,
        pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV)
       where
import GHC.Ptr                                       (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                  (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Enum.Pipeline
import Graphics.Vulkan.Types.Enum.Queue
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.CheckpointDataNV
import Graphics.Vulkan.Types.Struct.Extent           (VkExtent3D)
import Graphics.Vulkan.Types.Struct.QueueFamily      (VkQueueFamilyCheckpointPropertiesNV,
                                                      VkQueueFamilyProperties,
                                                      VkQueueFamilyProperties2)

pattern VkCmdSetCheckpointNV :: CString

pattern VkCmdSetCheckpointNV <- (is_VkCmdSetCheckpointNV -> True)
  where
    VkCmdSetCheckpointNV = _VkCmdSetCheckpointNV

{-# INLINE _VkCmdSetCheckpointNV #-}

_VkCmdSetCheckpointNV :: CString
_VkCmdSetCheckpointNV = Ptr "vkCmdSetCheckpointNV\NUL"#

{-# INLINE is_VkCmdSetCheckpointNV #-}

is_VkCmdSetCheckpointNV :: CString -> Bool
is_VkCmdSetCheckpointNV
  = (EQ ==) . cmpCStrings _VkCmdSetCheckpointNV

type VkCmdSetCheckpointNV = "vkCmdSetCheckpointNV"

-- | Queues: 'graphics', 'compute', 'transfer'.
--
--   Renderpass: @both@
--
--   > void vkCmdSetCheckpointNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , const void* pCheckpointMarker
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdSetCheckpointNV vkCmdSetCheckpointNV registry at www.khronos.org>
type HS_vkCmdSetCheckpointNV = VkCommandBuffer -- ^ commandBuffer
                                               -> Ptr Void -- ^ pCheckpointMarker
                                                           -> IO ()

type PFN_vkCmdSetCheckpointNV = FunPtr HS_vkCmdSetCheckpointNV

foreign import ccall unsafe "dynamic"
               unwrapVkCmdSetCheckpointNVUnsafe ::
               PFN_vkCmdSetCheckpointNV -> HS_vkCmdSetCheckpointNV

foreign import ccall safe "dynamic" unwrapVkCmdSetCheckpointNVSafe
               :: PFN_vkCmdSetCheckpointNV -> HS_vkCmdSetCheckpointNV

instance VulkanProc "vkCmdSetCheckpointNV" where
    type VkProcType "vkCmdSetCheckpointNV" = HS_vkCmdSetCheckpointNV
    vkProcSymbol = _VkCmdSetCheckpointNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdSetCheckpointNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdSetCheckpointNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetQueueCheckpointDataNV :: CString

pattern VkGetQueueCheckpointDataNV <-
        (is_VkGetQueueCheckpointDataNV -> True)
  where
    VkGetQueueCheckpointDataNV = _VkGetQueueCheckpointDataNV

{-# INLINE _VkGetQueueCheckpointDataNV #-}

_VkGetQueueCheckpointDataNV :: CString
_VkGetQueueCheckpointDataNV = Ptr "vkGetQueueCheckpointDataNV\NUL"#

{-# INLINE is_VkGetQueueCheckpointDataNV #-}

is_VkGetQueueCheckpointDataNV :: CString -> Bool
is_VkGetQueueCheckpointDataNV
  = (EQ ==) . cmpCStrings _VkGetQueueCheckpointDataNV

type VkGetQueueCheckpointDataNV = "vkGetQueueCheckpointDataNV"

-- | > void vkGetQueueCheckpointDataNV
--   >     ( VkQueue queue
--   >     , uint32_t* pCheckpointDataCount
--   >     , VkCheckpointDataNV* pCheckpointData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetQueueCheckpointDataNV vkGetQueueCheckpointDataNV registry at www.khronos.org>
type HS_vkGetQueueCheckpointDataNV =
     VkQueue -- ^ queue
             -> Ptr Word32 -- ^ pCheckpointDataCount
                           -> Ptr VkCheckpointDataNV -- ^ pCheckpointData
                                                     -> IO ()

type PFN_vkGetQueueCheckpointDataNV =
     FunPtr HS_vkGetQueueCheckpointDataNV

foreign import ccall unsafe "dynamic"
               unwrapVkGetQueueCheckpointDataNVUnsafe ::
               PFN_vkGetQueueCheckpointDataNV -> HS_vkGetQueueCheckpointDataNV

foreign import ccall safe "dynamic"
               unwrapVkGetQueueCheckpointDataNVSafe ::
               PFN_vkGetQueueCheckpointDataNV -> HS_vkGetQueueCheckpointDataNV

instance VulkanProc "vkGetQueueCheckpointDataNV" where
    type VkProcType "vkGetQueueCheckpointDataNV" =
         HS_vkGetQueueCheckpointDataNV
    vkProcSymbol = _VkGetQueueCheckpointDataNV

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetQueueCheckpointDataNVUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetQueueCheckpointDataNVSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION = 2

type VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_SPEC_VERSION = 2

pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME ::
        CString

pattern VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME <-
        (is_VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME -> True)
  where
    VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME
      = _VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME

{-# INLINE _VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME #-}

_VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME :: CString
_VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME
  = Ptr "VK_NV_device_diagnostic_checkpoints\NUL"#

{-# INLINE is_VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME
           #-}

is_VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME ::
                                                      CString -> Bool
is_VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME

type VK_NV_DEVICE_DIAGNOSTIC_CHECKPOINTS_EXTENSION_NAME =
     "VK_NV_device_diagnostic_checkpoints"

pattern VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV :: VkStructureType

pattern VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV =
        VkStructureType 1000206000

pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV =
        VkStructureType 1000206001
