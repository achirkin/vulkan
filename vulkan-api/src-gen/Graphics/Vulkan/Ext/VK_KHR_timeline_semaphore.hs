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
module Graphics.Vulkan.Ext.VK_KHR_timeline_semaphore
       (-- * Vulkan extension: @VK_KHR_timeline_semaphore@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jason Ekstrand @jekstrand@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @208@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkPhysicalDeviceTimelineSemaphoreFeaturesKHR,
        VkPhysicalDeviceTimelineSemaphorePropertiesKHR,
        VkSemaphoreSignalInfoKHR, VkSemaphoreTypeCreateInfoKHR,
        VkSemaphoreImportBitmask(..), VkSemaphoreType(..),
        VkSemaphoreWaitBitmask(..), VkSemaphoreImportFlagBits(),
        VkSemaphoreImportFlagBitsKHR(..), VkSemaphoreImportFlags(),
        VkSemaphoreTypeKHR(..), VkSemaphoreWaitFlagBits(),
        VkSemaphoreWaitFlagBitsKHR(..), VkSemaphoreWaitFlags(),
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
        VkXlibSurfaceCreateFlagsKHR(..), VkSemaphoreWaitInfoKHR,
        VkTimelineSemaphoreSubmitInfoKHR, VkGetSemaphoreCounterValueKHR,
        pattern VkGetSemaphoreCounterValueKHR,
        HS_vkGetSemaphoreCounterValueKHR,
        PFN_vkGetSemaphoreCounterValueKHR, VkWaitSemaphoresKHR,
        pattern VkWaitSemaphoresKHR, HS_vkWaitSemaphoresKHR,
        PFN_vkWaitSemaphoresKHR, VkSignalSemaphoreKHR,
        pattern VkSignalSemaphoreKHR, HS_vkSignalSemaphoreKHR,
        PFN_vkSignalSemaphoreKHR, module Graphics.Vulkan.Marshal,
        AHardwareBuffer(), ANativeWindow(), CAMetalLayer(), VkBool32(..),
        VkDeviceAddress(..), VkDeviceSize(..), VkFlags(..),
        VkSampleMask(..), VkResult(..), VkStructureType(..),
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
        VkSemaphoreCreateInfo, VkSemaphoreGetFdInfoKHR,
        VkSemaphoreSignalInfo, VkSemaphoreTypeCreateInfo,
        VkSemaphoreWaitInfo, VK_KHR_TIMELINE_SEMAPHORE_SPEC_VERSION,
        pattern VK_KHR_TIMELINE_SEMAPHORE_SPEC_VERSION,
        VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME,
        pattern VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO_KHR,
        pattern VK_SEMAPHORE_TYPE_BINARY_KHR,
        pattern VK_SEMAPHORE_TYPE_TIMELINE_KHR,
        pattern VK_SEMAPHORE_WAIT_ANY_BIT_KHR)
       where
import GHC.Ptr                                                  (Ptr (..))
import Graphics.Vulkan.Core_1_2                                 (pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES,
                                                                 pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES,
                                                                 pattern VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO,
                                                                 pattern VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO,
                                                                 pattern VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO,
                                                                 pattern VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                             (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.Semaphore
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.PhysicalDevice              (VkPhysicalDeviceTimelineSemaphoreFeaturesKHR,
                                                                 VkPhysicalDeviceTimelineSemaphorePropertiesKHR)
import Graphics.Vulkan.Types.Struct.Semaphore
import Graphics.Vulkan.Types.Struct.TimelineSemaphoreSubmitInfo (VkTimelineSemaphoreSubmitInfoKHR)

pattern VkGetSemaphoreCounterValueKHR :: CString

pattern VkGetSemaphoreCounterValueKHR <-
        (is_VkGetSemaphoreCounterValueKHR -> True)
  where
    VkGetSemaphoreCounterValueKHR = _VkGetSemaphoreCounterValueKHR

{-# INLINE _VkGetSemaphoreCounterValueKHR #-}

_VkGetSemaphoreCounterValueKHR :: CString
_VkGetSemaphoreCounterValueKHR
  = Ptr "vkGetSemaphoreCounterValueKHR\NUL"#

{-# INLINE is_VkGetSemaphoreCounterValueKHR #-}

is_VkGetSemaphoreCounterValueKHR :: CString -> Bool
is_VkGetSemaphoreCounterValueKHR
  = (EQ ==) . cmpCStrings _VkGetSemaphoreCounterValueKHR

type VkGetSemaphoreCounterValueKHR =
     "vkGetSemaphoreCounterValueKHR"

-- | This is an alias for `vkGetSemaphoreCounterValue`.
--
--   Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkGetSemaphoreCounterValueKHR
--   >     ( VkDevice device
--   >     , VkSemaphore semaphore
--   >     , uint64_t* pValue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetSemaphoreCounterValueKHR vkGetSemaphoreCounterValueKHR registry at www.khronos.org>
type HS_vkGetSemaphoreCounterValueKHR =
     VkDevice -- ^ device
              -> VkSemaphore -- ^ semaphore
                             -> Ptr Word64 -- ^ pValue
                                           -> IO VkResult

type PFN_vkGetSemaphoreCounterValueKHR =
     FunPtr HS_vkGetSemaphoreCounterValueKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetSemaphoreCounterValueKHRUnsafe ::
               PFN_vkGetSemaphoreCounterValueKHR ->
                 HS_vkGetSemaphoreCounterValueKHR

foreign import ccall safe "dynamic"
               unwrapVkGetSemaphoreCounterValueKHRSafe ::
               PFN_vkGetSemaphoreCounterValueKHR ->
                 HS_vkGetSemaphoreCounterValueKHR

instance VulkanProc "vkGetSemaphoreCounterValueKHR" where
    type VkProcType "vkGetSemaphoreCounterValueKHR" =
         HS_vkGetSemaphoreCounterValueKHR
    vkProcSymbol = _VkGetSemaphoreCounterValueKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetSemaphoreCounterValueKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetSemaphoreCounterValueKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkWaitSemaphoresKHR :: CString

pattern VkWaitSemaphoresKHR <- (is_VkWaitSemaphoresKHR -> True)
  where
    VkWaitSemaphoresKHR = _VkWaitSemaphoresKHR

{-# INLINE _VkWaitSemaphoresKHR #-}

_VkWaitSemaphoresKHR :: CString
_VkWaitSemaphoresKHR = Ptr "vkWaitSemaphoresKHR\NUL"#

{-# INLINE is_VkWaitSemaphoresKHR #-}

is_VkWaitSemaphoresKHR :: CString -> Bool
is_VkWaitSemaphoresKHR = (EQ ==) . cmpCStrings _VkWaitSemaphoresKHR

type VkWaitSemaphoresKHR = "vkWaitSemaphoresKHR"

-- | This is an alias for `vkWaitSemaphores`.
--
--   Success codes: 'VK_SUCCESS', 'VK_TIMEOUT'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST'.
--
--   > VkResult vkWaitSemaphoresKHR
--   >     ( VkDevice device
--   >     , const VkSemaphoreWaitInfo* pWaitInfo
--   >     , uint64_t timeout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkWaitSemaphoresKHR vkWaitSemaphoresKHR registry at www.khronos.org>
type HS_vkWaitSemaphoresKHR =
     VkDevice -- ^ device
              -> Ptr VkSemaphoreWaitInfo -- ^ pWaitInfo
                                         -> Word64 -- ^ timeout
                                                   -> IO VkResult

type PFN_vkWaitSemaphoresKHR = FunPtr HS_vkWaitSemaphoresKHR

foreign import ccall unsafe "dynamic"
               unwrapVkWaitSemaphoresKHRUnsafe ::
               PFN_vkWaitSemaphoresKHR -> HS_vkWaitSemaphoresKHR

foreign import ccall safe "dynamic" unwrapVkWaitSemaphoresKHRSafe
               :: PFN_vkWaitSemaphoresKHR -> HS_vkWaitSemaphoresKHR

instance VulkanProc "vkWaitSemaphoresKHR" where
    type VkProcType "vkWaitSemaphoresKHR" = HS_vkWaitSemaphoresKHR
    vkProcSymbol = _VkWaitSemaphoresKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkWaitSemaphoresKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkWaitSemaphoresKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkSignalSemaphoreKHR :: CString

pattern VkSignalSemaphoreKHR <- (is_VkSignalSemaphoreKHR -> True)
  where
    VkSignalSemaphoreKHR = _VkSignalSemaphoreKHR

{-# INLINE _VkSignalSemaphoreKHR #-}

_VkSignalSemaphoreKHR :: CString
_VkSignalSemaphoreKHR = Ptr "vkSignalSemaphoreKHR\NUL"#

{-# INLINE is_VkSignalSemaphoreKHR #-}

is_VkSignalSemaphoreKHR :: CString -> Bool
is_VkSignalSemaphoreKHR
  = (EQ ==) . cmpCStrings _VkSignalSemaphoreKHR

type VkSignalSemaphoreKHR = "vkSignalSemaphoreKHR"

-- | This is an alias for `vkSignalSemaphore`.
--
--   Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkSignalSemaphoreKHR
--   >     ( VkDevice device
--   >     , const VkSemaphoreSignalInfo* pSignalInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkSignalSemaphoreKHR vkSignalSemaphoreKHR registry at www.khronos.org>
type HS_vkSignalSemaphoreKHR =
     VkDevice -- ^ device
              -> Ptr VkSemaphoreSignalInfo -- ^ pSignalInfo
                                           -> IO VkResult

type PFN_vkSignalSemaphoreKHR = FunPtr HS_vkSignalSemaphoreKHR

foreign import ccall unsafe "dynamic"
               unwrapVkSignalSemaphoreKHRUnsafe ::
               PFN_vkSignalSemaphoreKHR -> HS_vkSignalSemaphoreKHR

foreign import ccall safe "dynamic" unwrapVkSignalSemaphoreKHRSafe
               :: PFN_vkSignalSemaphoreKHR -> HS_vkSignalSemaphoreKHR

instance VulkanProc "vkSignalSemaphoreKHR" where
    type VkProcType "vkSignalSemaphoreKHR" = HS_vkSignalSemaphoreKHR
    vkProcSymbol = _VkSignalSemaphoreKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkSignalSemaphoreKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkSignalSemaphoreKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_TIMELINE_SEMAPHORE_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_KHR_TIMELINE_SEMAPHORE_SPEC_VERSION = 2

type VK_KHR_TIMELINE_SEMAPHORE_SPEC_VERSION = 2

pattern VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME :: CString

pattern VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME <-
        (is_VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME -> True)
  where
    VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME
      = _VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME

{-# INLINE _VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME #-}

_VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME :: CString
_VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME
  = Ptr "VK_KHR_timeline_semaphore\NUL"#

{-# INLINE is_VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME #-}

is_VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME

type VK_KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME =
     "VK_KHR_timeline_semaphore"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES_KHR
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES_KHR
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES

pattern VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO_KHR =
        VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO

pattern VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO_KHR =
        VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO

pattern VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO_KHR =
        VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO

pattern VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO_KHR =
        VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO

pattern VK_SEMAPHORE_TYPE_BINARY_KHR = VK_SEMAPHORE_TYPE_BINARY

pattern VK_SEMAPHORE_TYPE_TIMELINE_KHR = VK_SEMAPHORE_TYPE_TIMELINE

pattern VK_SEMAPHORE_WAIT_ANY_BIT_KHR = VK_SEMAPHORE_WAIT_ANY_BIT
