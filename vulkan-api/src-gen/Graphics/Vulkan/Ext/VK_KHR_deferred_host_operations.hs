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
module Graphics.Vulkan.Ext.VK_KHR_deferred_host_operations
       (-- * Vulkan extension: @VK_KHR_deferred_host_operations@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Josh Barczak @jbarczak@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- platform: @provisional@
        --
        -- Extension number: @269@
        module Graphics.Vulkan.Marshal,
        VkAccelerationStructureBuildGeometryInfoKHR,
        VkAccelerationStructureGeometryAabbsDataKHR,
        VkAccelerationStructureGeometryDataKHR,
        VkAccelerationStructureGeometryInstancesDataKHR,
        VkAccelerationStructureGeometryKHR,
        VkAccelerationStructureGeometryTrianglesDataKHR,
        VkAccelerationStructureBuildTypeKHR(..),
        VkAccelerationStructureMemoryRequirementsTypeKHR(..),
        VkAccelerationStructureTypeKHR(..),
        VkAccelerationStructureMemoryRequirementsTypeNV(..),
        VkAccelerationStructureTypeNV(..), AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkBuildAccelerationStructureBitmaskKHR(..),
        VkBuildAccelerationStructureFlagBitsKHR(),
        VkBuildAccelerationStructureFlagBitsNV(..),
        VkBuildAccelerationStructureFlagsKHR(),
        VkCopyAccelerationStructureInfoKHR,
        VkCopyAccelerationStructureModeKHR(..),
        VkCopyAccelerationStructureModeNV(..),
        VkCopyAccelerationStructureToMemoryInfoKHR,
        VkCopyMemoryToAccelerationStructureInfoKHR,
        VkDeferredOperationInfoKHR, VkDeviceOrHostAddressConstKHR,
        VkDeviceOrHostAddressKHR, VkFormat(..), VkFormatFeatureBitmask(..),
        VkFormatFeatureFlagBits(), VkFormatFeatureFlags(),
        VkGeometryInstanceBitmaskKHR(..), VkGeometryBitmaskKHR(..),
        VkGeometryTypeKHR(..), VkGeometryFlagBitsKHR(),
        VkGeometryFlagBitsNV(..), VkGeometryFlagsKHR(),
        VkGeometryInstanceFlagBitsKHR(), VkGeometryInstanceFlagBitsNV(..),
        VkGeometryInstanceFlagsKHR(), VkGeometryTypeNV(..),
        VkIndexType(..), VkPipelineBindPoint(..),
        VkPipelineCacheHeaderVersion(..), VkPipelineCreateBitmask(..),
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
        VkPipelineStageFlags(), VkPipelineLibraryCreateInfoKHR,
        VkPipelineShaderStageCreateInfo, VkRayTracingPipelineCreateInfoKHR,
        VkRayTracingPipelineInterfaceCreateInfoKHR,
        VkRayTracingShaderGroupCreateInfoKHR,
        VkRayTracingShaderGroupTypeKHR(..),
        VkRayTracingShaderGroupTypeNV(..),
        VkShaderFloatControlsIndependence(..), VkShaderInfoTypeAMD(..),
        VkShaderStageBitmask(..), VkShaderCorePropertiesBitmaskAMD(..),
        VkShaderCorePropertiesFlagBitsAMD(),
        VkShaderCorePropertiesFlagsAMD(),
        VkShaderFloatControlsIndependenceKHR(..),
        VkShaderModuleCreateBitmask(..), VkShaderModuleCreateFlagBits(),
        VkShaderModuleCreateFlags(), VkShaderStageFlagBits(),
        VkShaderStageFlags(), VkSpecializationInfo,
        VkSpecializationMapEntry, VkStructureType(..),
        -- > #include "vk_platform.h"
        VkCreateDeferredOperationKHR, pattern VkCreateDeferredOperationKHR,
        HS_vkCreateDeferredOperationKHR, PFN_vkCreateDeferredOperationKHR,
        VkDestroyDeferredOperationKHR,
        pattern VkDestroyDeferredOperationKHR,
        HS_vkDestroyDeferredOperationKHR,
        PFN_vkDestroyDeferredOperationKHR,
        VkGetDeferredOperationMaxConcurrencyKHR,
        pattern VkGetDeferredOperationMaxConcurrencyKHR,
        HS_vkGetDeferredOperationMaxConcurrencyKHR,
        PFN_vkGetDeferredOperationMaxConcurrencyKHR,
        VkGetDeferredOperationResultKHR,
        pattern VkGetDeferredOperationResultKHR,
        HS_vkGetDeferredOperationResultKHR,
        PFN_vkGetDeferredOperationResultKHR, VkDeferredOperationJoinKHR,
        pattern VkDeferredOperationJoinKHR, HS_vkDeferredOperationJoinKHR,
        PFN_vkDeferredOperationJoinKHR, VkInternalAllocationType(..),
        VkResult(..), VkSystemAllocationScope(..), newVkAllocationFunction,
        newVkDebugReportCallbackEXT, newVkDebugUtilsMessengerCallbackEXT,
        newVkFreeFunction, newVkInternalAllocationNotification,
        newVkInternalFreeNotification, newVkReallocationFunction,
        newVkVoidFunction, unwrapVkAllocationFunction,
        unwrapVkDebugReportCallbackEXT,
        unwrapVkDebugUtilsMessengerCallbackEXT, unwrapVkFreeFunction,
        unwrapVkInternalAllocationNotification,
        unwrapVkInternalFreeNotification, unwrapVkReallocationFunction,
        unwrapVkVoidFunction, HS_vkAllocationFunction,
        HS_vkDebugReportCallbackEXT, HS_vkDebugUtilsMessengerCallbackEXT,
        HS_vkFreeFunction, HS_vkInternalAllocationNotification,
        HS_vkInternalFreeNotification, HS_vkReallocationFunction,
        HS_vkVoidFunction, PFN_vkAllocationFunction,
        PFN_vkDebugReportCallbackEXT, PFN_vkDebugUtilsMessengerCallbackEXT,
        PFN_vkFreeFunction, PFN_vkInternalAllocationNotification,
        PFN_vkInternalFreeNotification, PFN_vkReallocationFunction,
        PFN_vkVoidFunction, VkAccelerationStructureKHR,
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
        VkAllocationCallbacks,
        VK_KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION,
        pattern VK_KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION,
        VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME,
        pattern VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR,
        pattern VK_OBJECT_TYPE_DEFERRED_OPERATION_KHR,
        pattern VK_THREAD_IDLE_KHR, pattern VK_THREAD_DONE_KHR,
        pattern VK_OPERATION_DEFERRED_KHR,
        pattern VK_OPERATION_NOT_DEFERRED_KHR)
       where
import GHC.Ptr                                                   (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                              (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Enum.AccelerationStructure
import Graphics.Vulkan.Types.Enum.BuildAccelerationStructureFlag
import Graphics.Vulkan.Types.Enum.CopyAccelerationStructureMode
import Graphics.Vulkan.Types.Enum.Format
import Graphics.Vulkan.Types.Enum.Geometry
import Graphics.Vulkan.Types.Enum.IndexType
import Graphics.Vulkan.Types.Enum.InternalAllocationType
import Graphics.Vulkan.Types.Enum.ObjectType                     (VkObjectType (..))
import Graphics.Vulkan.Types.Enum.Pipeline
import Graphics.Vulkan.Types.Enum.RayTracingShaderGroupType
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.Shader
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Enum.SystemAllocationScope
import Graphics.Vulkan.Types.Funcpointers
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.AllocationCallbacks
import Graphics.Vulkan.Types.Struct.EnableBetaExtensions         (VkAccelerationStructureBuildGeometryInfoKHR,
                                                                  VkAccelerationStructureGeometryAabbsDataKHR,
                                                                  VkAccelerationStructureGeometryDataKHR,
                                                                  VkAccelerationStructureGeometryInstancesDataKHR,
                                                                  VkAccelerationStructureGeometryKHR,
                                                                  VkAccelerationStructureGeometryTrianglesDataKHR,
                                                                  VkCopyAccelerationStructureInfoKHR,
                                                                  VkCopyAccelerationStructureToMemoryInfoKHR,
                                                                  VkCopyMemoryToAccelerationStructureInfoKHR,
                                                                  VkDeferredOperationInfoKHR,
                                                                  VkDeviceOrHostAddressConstKHR,
                                                                  VkDeviceOrHostAddressKHR,
                                                                  VkPipelineLibraryCreateInfoKHR,
                                                                  VkRayTracingPipelineCreateInfoKHR,
                                                                  VkRayTracingPipelineInterfaceCreateInfoKHR,
                                                                  VkRayTracingShaderGroupCreateInfoKHR)
import Graphics.Vulkan.Types.Struct.Pipeline                     (VkPipelineShaderStageCreateInfo)
import Graphics.Vulkan.Types.Struct.Specialization               (VkSpecializationInfo,
                                                                  VkSpecializationMapEntry)

pattern VkCreateDeferredOperationKHR :: CString

pattern VkCreateDeferredOperationKHR <-
        (is_VkCreateDeferredOperationKHR -> True)
  where
    VkCreateDeferredOperationKHR = _VkCreateDeferredOperationKHR

{-# INLINE _VkCreateDeferredOperationKHR #-}

_VkCreateDeferredOperationKHR :: CString
_VkCreateDeferredOperationKHR
  = Ptr "vkCreateDeferredOperationKHR\NUL"#

{-# INLINE is_VkCreateDeferredOperationKHR #-}

is_VkCreateDeferredOperationKHR :: CString -> Bool
is_VkCreateDeferredOperationKHR
  = (EQ ==) . cmpCStrings _VkCreateDeferredOperationKHR

type VkCreateDeferredOperationKHR = "vkCreateDeferredOperationKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkCreateDeferredOperationKHR
--   >     ( VkDevice device
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDeferredOperationKHR* pDeferredOperation
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCreateDeferredOperationKHR vkCreateDeferredOperationKHR registry at www.khronos.org>
type HS_vkCreateDeferredOperationKHR =
     VkDevice -- ^ device
              ->
       Ptr VkAllocationCallbacks -- ^ pAllocator
                                 ->
         Ptr VkDeferredOperationKHR -- ^ pDeferredOperation
                                    -> IO VkResult

type PFN_vkCreateDeferredOperationKHR =
     FunPtr HS_vkCreateDeferredOperationKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCreateDeferredOperationKHRUnsafe ::
               PFN_vkCreateDeferredOperationKHR -> HS_vkCreateDeferredOperationKHR

foreign import ccall safe "dynamic"
               unwrapVkCreateDeferredOperationKHRSafe ::
               PFN_vkCreateDeferredOperationKHR -> HS_vkCreateDeferredOperationKHR

instance VulkanProc "vkCreateDeferredOperationKHR" where
    type VkProcType "vkCreateDeferredOperationKHR" =
         HS_vkCreateDeferredOperationKHR
    vkProcSymbol = _VkCreateDeferredOperationKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateDeferredOperationKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateDeferredOperationKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkDestroyDeferredOperationKHR :: CString

pattern VkDestroyDeferredOperationKHR <-
        (is_VkDestroyDeferredOperationKHR -> True)
  where
    VkDestroyDeferredOperationKHR = _VkDestroyDeferredOperationKHR

{-# INLINE _VkDestroyDeferredOperationKHR #-}

_VkDestroyDeferredOperationKHR :: CString
_VkDestroyDeferredOperationKHR
  = Ptr "vkDestroyDeferredOperationKHR\NUL"#

{-# INLINE is_VkDestroyDeferredOperationKHR #-}

is_VkDestroyDeferredOperationKHR :: CString -> Bool
is_VkDestroyDeferredOperationKHR
  = (EQ ==) . cmpCStrings _VkDestroyDeferredOperationKHR

type VkDestroyDeferredOperationKHR =
     "vkDestroyDeferredOperationKHR"

-- | > void vkDestroyDeferredOperationKHR
--   >     ( VkDevice device
--   >     , VkDeferredOperationKHR operation
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkDestroyDeferredOperationKHR vkDestroyDeferredOperationKHR registry at www.khronos.org>
type HS_vkDestroyDeferredOperationKHR =
     VkDevice -- ^ device
              ->
       VkDeferredOperationKHR -- ^ operation
                              -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                           -> IO ()

type PFN_vkDestroyDeferredOperationKHR =
     FunPtr HS_vkDestroyDeferredOperationKHR

foreign import ccall unsafe "dynamic"
               unwrapVkDestroyDeferredOperationKHRUnsafe ::
               PFN_vkDestroyDeferredOperationKHR ->
                 HS_vkDestroyDeferredOperationKHR

foreign import ccall safe "dynamic"
               unwrapVkDestroyDeferredOperationKHRSafe ::
               PFN_vkDestroyDeferredOperationKHR ->
                 HS_vkDestroyDeferredOperationKHR

instance VulkanProc "vkDestroyDeferredOperationKHR" where
    type VkProcType "vkDestroyDeferredOperationKHR" =
         HS_vkDestroyDeferredOperationKHR
    vkProcSymbol = _VkDestroyDeferredOperationKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkDestroyDeferredOperationKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkDestroyDeferredOperationKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetDeferredOperationMaxConcurrencyKHR :: CString

pattern VkGetDeferredOperationMaxConcurrencyKHR <-
        (is_VkGetDeferredOperationMaxConcurrencyKHR -> True)
  where
    VkGetDeferredOperationMaxConcurrencyKHR
      = _VkGetDeferredOperationMaxConcurrencyKHR

{-# INLINE _VkGetDeferredOperationMaxConcurrencyKHR #-}

_VkGetDeferredOperationMaxConcurrencyKHR :: CString
_VkGetDeferredOperationMaxConcurrencyKHR
  = Ptr "vkGetDeferredOperationMaxConcurrencyKHR\NUL"#

{-# INLINE is_VkGetDeferredOperationMaxConcurrencyKHR #-}

is_VkGetDeferredOperationMaxConcurrencyKHR :: CString -> Bool
is_VkGetDeferredOperationMaxConcurrencyKHR
  = (EQ ==) . cmpCStrings _VkGetDeferredOperationMaxConcurrencyKHR

type VkGetDeferredOperationMaxConcurrencyKHR =
     "vkGetDeferredOperationMaxConcurrencyKHR"

-- | > uint32_t vkGetDeferredOperationMaxConcurrencyKHR
--   >     ( VkDevice device
--   >     , VkDeferredOperationKHR operation
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetDeferredOperationMaxConcurrencyKHR vkGetDeferredOperationMaxConcurrencyKHR registry at www.khronos.org>
type HS_vkGetDeferredOperationMaxConcurrencyKHR =
     VkDevice -- ^ device
              -> VkDeferredOperationKHR -- ^ operation
                                        -> IO Word32

type PFN_vkGetDeferredOperationMaxConcurrencyKHR =
     FunPtr HS_vkGetDeferredOperationMaxConcurrencyKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetDeferredOperationMaxConcurrencyKHRUnsafe ::
               PFN_vkGetDeferredOperationMaxConcurrencyKHR ->
                 HS_vkGetDeferredOperationMaxConcurrencyKHR

foreign import ccall safe "dynamic"
               unwrapVkGetDeferredOperationMaxConcurrencyKHRSafe ::
               PFN_vkGetDeferredOperationMaxConcurrencyKHR ->
                 HS_vkGetDeferredOperationMaxConcurrencyKHR

instance VulkanProc "vkGetDeferredOperationMaxConcurrencyKHR" where
    type VkProcType "vkGetDeferredOperationMaxConcurrencyKHR" =
         HS_vkGetDeferredOperationMaxConcurrencyKHR
    vkProcSymbol = _VkGetDeferredOperationMaxConcurrencyKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetDeferredOperationMaxConcurrencyKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetDeferredOperationMaxConcurrencyKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetDeferredOperationResultKHR :: CString

pattern VkGetDeferredOperationResultKHR <-
        (is_VkGetDeferredOperationResultKHR -> True)
  where
    VkGetDeferredOperationResultKHR = _VkGetDeferredOperationResultKHR

{-# INLINE _VkGetDeferredOperationResultKHR #-}

_VkGetDeferredOperationResultKHR :: CString
_VkGetDeferredOperationResultKHR
  = Ptr "vkGetDeferredOperationResultKHR\NUL"#

{-# INLINE is_VkGetDeferredOperationResultKHR #-}

is_VkGetDeferredOperationResultKHR :: CString -> Bool
is_VkGetDeferredOperationResultKHR
  = (EQ ==) . cmpCStrings _VkGetDeferredOperationResultKHR

type VkGetDeferredOperationResultKHR =
     "vkGetDeferredOperationResultKHR"

-- | Success codes: 'VK_SUCCESS', 'VK_NOT_READY'.
--
--   > VkResult vkGetDeferredOperationResultKHR
--   >     ( VkDevice device
--   >     , VkDeferredOperationKHR operation
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetDeferredOperationResultKHR vkGetDeferredOperationResultKHR registry at www.khronos.org>
type HS_vkGetDeferredOperationResultKHR =
     VkDevice -- ^ device
              -> VkDeferredOperationKHR -- ^ operation
                                        -> IO VkResult

type PFN_vkGetDeferredOperationResultKHR =
     FunPtr HS_vkGetDeferredOperationResultKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetDeferredOperationResultKHRUnsafe ::
               PFN_vkGetDeferredOperationResultKHR ->
                 HS_vkGetDeferredOperationResultKHR

foreign import ccall safe "dynamic"
               unwrapVkGetDeferredOperationResultKHRSafe ::
               PFN_vkGetDeferredOperationResultKHR ->
                 HS_vkGetDeferredOperationResultKHR

instance VulkanProc "vkGetDeferredOperationResultKHR" where
    type VkProcType "vkGetDeferredOperationResultKHR" =
         HS_vkGetDeferredOperationResultKHR
    vkProcSymbol = _VkGetDeferredOperationResultKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetDeferredOperationResultKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetDeferredOperationResultKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkDeferredOperationJoinKHR :: CString

pattern VkDeferredOperationJoinKHR <-
        (is_VkDeferredOperationJoinKHR -> True)
  where
    VkDeferredOperationJoinKHR = _VkDeferredOperationJoinKHR

{-# INLINE _VkDeferredOperationJoinKHR #-}

_VkDeferredOperationJoinKHR :: CString
_VkDeferredOperationJoinKHR = Ptr "vkDeferredOperationJoinKHR\NUL"#

{-# INLINE is_VkDeferredOperationJoinKHR #-}

is_VkDeferredOperationJoinKHR :: CString -> Bool
is_VkDeferredOperationJoinKHR
  = (EQ ==) . cmpCStrings _VkDeferredOperationJoinKHR

type VkDeferredOperationJoinKHR = "vkDeferredOperationJoinKHR"

-- | Success codes: 'VK_SUCCESS', 'VK_THREAD_DONE_KHR', 'VK_THREAD_IDLE_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkDeferredOperationJoinKHR
--   >     ( VkDevice device
--   >     , VkDeferredOperationKHR operation
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkDeferredOperationJoinKHR vkDeferredOperationJoinKHR registry at www.khronos.org>
type HS_vkDeferredOperationJoinKHR =
     VkDevice -- ^ device
              -> VkDeferredOperationKHR -- ^ operation
                                        -> IO VkResult

type PFN_vkDeferredOperationJoinKHR =
     FunPtr HS_vkDeferredOperationJoinKHR

foreign import ccall unsafe "dynamic"
               unwrapVkDeferredOperationJoinKHRUnsafe ::
               PFN_vkDeferredOperationJoinKHR -> HS_vkDeferredOperationJoinKHR

foreign import ccall safe "dynamic"
               unwrapVkDeferredOperationJoinKHRSafe ::
               PFN_vkDeferredOperationJoinKHR -> HS_vkDeferredOperationJoinKHR

instance VulkanProc "vkDeferredOperationJoinKHR" where
    type VkProcType "vkDeferredOperationJoinKHR" =
         HS_vkDeferredOperationJoinKHR
    vkProcSymbol = _VkDeferredOperationJoinKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkDeferredOperationJoinKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkDeferredOperationJoinKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION = 3

type VK_KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION = 3

pattern VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME :: CString

pattern VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME <-
        (is_VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME -> True)
  where
    VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME
      = _VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME

{-# INLINE _VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME #-}

_VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME :: CString
_VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME
  = Ptr "VK_KHR_deferred_host_operations\NUL"#

{-# INLINE is_VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME #-}

is_VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME ::
                                                  CString -> Bool
is_VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME

type VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME =
     "VK_KHR_deferred_host_operations"

pattern VK_STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR =
        VkStructureType 1000268000

pattern VK_OBJECT_TYPE_DEFERRED_OPERATION_KHR :: VkObjectType

pattern VK_OBJECT_TYPE_DEFERRED_OPERATION_KHR =
        VkObjectType 1000268000

pattern VK_THREAD_IDLE_KHR :: VkResult

pattern VK_THREAD_IDLE_KHR = VkResult 1000268000

pattern VK_THREAD_DONE_KHR :: VkResult

pattern VK_THREAD_DONE_KHR = VkResult 1000268001

pattern VK_OPERATION_DEFERRED_KHR :: VkResult

pattern VK_OPERATION_DEFERRED_KHR = VkResult 1000268002

pattern VK_OPERATION_NOT_DEFERRED_KHR :: VkResult

pattern VK_OPERATION_NOT_DEFERRED_KHR = VkResult 1000268003
