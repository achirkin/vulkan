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
module Graphics.Vulkan.Ext.VK_FUCHSIA_imagepipe_surface
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
        VkXlibSurfaceCreateFlagsKHR(..),
        VkImagePipeSurfaceCreateInfoFUCHSIA, VkStructureType(..),
        -- > #include "vk_platform.h"
        VkCreateImagePipeSurfaceFUCHSIA,
        pattern VkCreateImagePipeSurfaceFUCHSIA,
        HS_vkCreateImagePipeSurfaceFUCHSIA,
        PFN_vkCreateImagePipeSurfaceFUCHSIA,
        module Graphics.Vulkan.Marshal, VkInternalAllocationType(..),
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
        VkValidationCacheEXT, VkValidationCacheEXT_T(), DWORD, Display,
        GgpFrameToken, GgpStreamDescriptor, HANDLE, HINSTANCE, HMONITOR,
        HWND, IDirectFB, IDirectFBSurface, LPCWSTR, RROutput,
        SECURITY_ATTRIBUTES, VisualID, Window, WlDisplay, WlSurface,
        XcbConnectionT, XcbVisualidT, XcbWindowT, Zx_handle_t,
        VkAllocationCallbacks, VK_FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION,
        pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION,
        VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME,
        pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA)
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
import Graphics.Vulkan.Types.Include
import Graphics.Vulkan.Types.Struct.AllocationCallbacks
import Graphics.Vulkan.Types.Struct.PlatformFuchsia

pattern VkCreateImagePipeSurfaceFUCHSIA :: CString

pattern VkCreateImagePipeSurfaceFUCHSIA <-
        (is_VkCreateImagePipeSurfaceFUCHSIA -> True)
  where
    VkCreateImagePipeSurfaceFUCHSIA = _VkCreateImagePipeSurfaceFUCHSIA

{-# INLINE _VkCreateImagePipeSurfaceFUCHSIA #-}

_VkCreateImagePipeSurfaceFUCHSIA :: CString
_VkCreateImagePipeSurfaceFUCHSIA
  = Ptr "vkCreateImagePipeSurfaceFUCHSIA\NUL"#

{-# INLINE is_VkCreateImagePipeSurfaceFUCHSIA #-}

is_VkCreateImagePipeSurfaceFUCHSIA :: CString -> Bool
is_VkCreateImagePipeSurfaceFUCHSIA
  = (EQ ==) . cmpCStrings _VkCreateImagePipeSurfaceFUCHSIA

type VkCreateImagePipeSurfaceFUCHSIA =
     "vkCreateImagePipeSurfaceFUCHSIA"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateImagePipeSurfaceFUCHSIA
--   >     ( VkInstance instance
--   >     , const VkImagePipeSurfaceCreateInfoFUCHSIA* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCreateImagePipeSurfaceFUCHSIA vkCreateImagePipeSurfaceFUCHSIA registry at www.khronos.org>
type HS_vkCreateImagePipeSurfaceFUCHSIA =
     VkInstance -- ^ instance
                ->
       Ptr VkImagePipeSurfaceCreateInfoFUCHSIA -- ^ pCreateInfo
                                               ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateImagePipeSurfaceFUCHSIA =
     FunPtr HS_vkCreateImagePipeSurfaceFUCHSIA

foreign import ccall unsafe "dynamic"
               unwrapVkCreateImagePipeSurfaceFUCHSIAUnsafe ::
               PFN_vkCreateImagePipeSurfaceFUCHSIA ->
                 HS_vkCreateImagePipeSurfaceFUCHSIA

foreign import ccall safe "dynamic"
               unwrapVkCreateImagePipeSurfaceFUCHSIASafe ::
               PFN_vkCreateImagePipeSurfaceFUCHSIA ->
                 HS_vkCreateImagePipeSurfaceFUCHSIA

instance VulkanProc "vkCreateImagePipeSurfaceFUCHSIA" where
    type VkProcType "vkCreateImagePipeSurfaceFUCHSIA" =
         HS_vkCreateImagePipeSurfaceFUCHSIA
    vkProcSymbol = _VkCreateImagePipeSurfaceFUCHSIA

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateImagePipeSurfaceFUCHSIAUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateImagePipeSurfaceFUCHSIASafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION = 1

type VK_FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION = 1

pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME :: CString

pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME <-
        (is_VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME -> True)
  where
    VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME
      = _VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME

{-# INLINE _VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME #-}

_VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME :: CString
_VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME
  = Ptr "VK_FUCHSIA_imagepipe_surface\NUL"#

{-# INLINE is_VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME #-}

is_VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME

type VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME =
     "VK_FUCHSIA_imagepipe_surface"

pattern VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA =
        VkStructureType 1000214000
