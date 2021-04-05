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
module Graphics.Vulkan.Ext.VK_EXT_metal_surface
       (AHardwareBuffer(), ANativeWindow(), CAMetalLayer(), VkBool32(..),
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
        VkXlibSurfaceCreateFlagsKHR(..), VkMetalSurfaceCreateInfoEXT,
        VkStructureType(..), -- > #include "vk_platform.h"
                             VkCreateMetalSurfaceEXT,
        pattern VkCreateMetalSurfaceEXT, HS_vkCreateMetalSurfaceEXT,
        PFN_vkCreateMetalSurfaceEXT, module Graphics.Vulkan.Marshal,
        VkInternalAllocationType(..), VkResult(..),
        VkSystemAllocationScope(..), newVkAllocationFunction,
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
        VkAllocationCallbacks, VK_EXT_METAL_SURFACE_SPEC_VERSION,
        pattern VK_EXT_METAL_SURFACE_SPEC_VERSION,
        VK_EXT_METAL_SURFACE_EXTENSION_NAME,
        pattern VK_EXT_METAL_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT)
       where
import GHC.Ptr                                           (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                      (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.InternalAllocationType
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Enum.SystemAllocationScope
import Graphics.Vulkan.Types.Funcpointers
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.AllocationCallbacks
import Graphics.Vulkan.Types.Struct.PlatformMetalExt

pattern VkCreateMetalSurfaceEXT :: CString

pattern VkCreateMetalSurfaceEXT <-
        (is_VkCreateMetalSurfaceEXT -> True)
  where
    VkCreateMetalSurfaceEXT = _VkCreateMetalSurfaceEXT

{-# INLINE _VkCreateMetalSurfaceEXT #-}

_VkCreateMetalSurfaceEXT :: CString
_VkCreateMetalSurfaceEXT = Ptr "vkCreateMetalSurfaceEXT\NUL"#

{-# INLINE is_VkCreateMetalSurfaceEXT #-}

is_VkCreateMetalSurfaceEXT :: CString -> Bool
is_VkCreateMetalSurfaceEXT
  = (EQ ==) . cmpCStrings _VkCreateMetalSurfaceEXT

type VkCreateMetalSurfaceEXT = "vkCreateMetalSurfaceEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateMetalSurfaceEXT
--   >     ( VkInstance instance
--   >     , const VkMetalSurfaceCreateInfoEXT* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCreateMetalSurfaceEXT vkCreateMetalSurfaceEXT registry at www.khronos.org>
type HS_vkCreateMetalSurfaceEXT =
     VkInstance -- ^ instance
                ->
       Ptr VkMetalSurfaceCreateInfoEXT -- ^ pCreateInfo
                                       ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateMetalSurfaceEXT =
     FunPtr HS_vkCreateMetalSurfaceEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCreateMetalSurfaceEXTUnsafe ::
               PFN_vkCreateMetalSurfaceEXT -> HS_vkCreateMetalSurfaceEXT

foreign import ccall safe "dynamic"
               unwrapVkCreateMetalSurfaceEXTSafe ::
               PFN_vkCreateMetalSurfaceEXT -> HS_vkCreateMetalSurfaceEXT

instance VulkanProc "vkCreateMetalSurfaceEXT" where
    type VkProcType "vkCreateMetalSurfaceEXT" =
         HS_vkCreateMetalSurfaceEXT
    vkProcSymbol = _VkCreateMetalSurfaceEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateMetalSurfaceEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateMetalSurfaceEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_METAL_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_METAL_SURFACE_SPEC_VERSION = 1

type VK_EXT_METAL_SURFACE_SPEC_VERSION = 1

pattern VK_EXT_METAL_SURFACE_EXTENSION_NAME :: CString

pattern VK_EXT_METAL_SURFACE_EXTENSION_NAME <-
        (is_VK_EXT_METAL_SURFACE_EXTENSION_NAME -> True)
  where
    VK_EXT_METAL_SURFACE_EXTENSION_NAME
      = _VK_EXT_METAL_SURFACE_EXTENSION_NAME

{-# INLINE _VK_EXT_METAL_SURFACE_EXTENSION_NAME #-}

_VK_EXT_METAL_SURFACE_EXTENSION_NAME :: CString
_VK_EXT_METAL_SURFACE_EXTENSION_NAME
  = Ptr "VK_EXT_metal_surface\NUL"#

{-# INLINE is_VK_EXT_METAL_SURFACE_EXTENSION_NAME #-}

is_VK_EXT_METAL_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_METAL_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_METAL_SURFACE_EXTENSION_NAME

type VK_EXT_METAL_SURFACE_EXTENSION_NAME = "VK_EXT_metal_surface"

pattern VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT =
        VkStructureType 1000217000
