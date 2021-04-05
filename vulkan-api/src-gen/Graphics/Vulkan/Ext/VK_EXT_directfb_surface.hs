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
module Graphics.Vulkan.Ext.VK_EXT_directfb_surface
       (VkAndroidSurfaceCreateFlagsKHR(..), VkBufferViewCreateFlags(..),
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
        VkXlibSurfaceCreateFlagsKHR(..), VkDirectFBSurfaceCreateInfoEXT,
        AHardwareBuffer(), ANativeWindow(), CAMetalLayer(), VkBool32(..),
        VkDeviceAddress(..), VkDeviceSize(..), VkFlags(..),
        VkSampleMask(..), VkStructureType(..), -- > #include "vk_platform.h"
                                               VkCreateDirectFBSurfaceEXT,
        pattern VkCreateDirectFBSurfaceEXT, HS_vkCreateDirectFBSurfaceEXT,
        PFN_vkCreateDirectFBSurfaceEXT,
        VkGetPhysicalDeviceDirectFBPresentationSupportEXT,
        pattern VkGetPhysicalDeviceDirectFBPresentationSupportEXT,
        HS_vkGetPhysicalDeviceDirectFBPresentationSupportEXT,
        PFN_vkGetPhysicalDeviceDirectFBPresentationSupportEXT,
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
        VkAllocationCallbacks, VK_EXT_DIRECTFB_SURFACE_SPEC_VERSION,
        pattern VK_EXT_DIRECTFB_SURFACE_SPEC_VERSION,
        VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME,
        pattern VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT)
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
import Graphics.Vulkan.Types.Struct.PlatformDirectfbExt

pattern VkCreateDirectFBSurfaceEXT :: CString

pattern VkCreateDirectFBSurfaceEXT <-
        (is_VkCreateDirectFBSurfaceEXT -> True)
  where
    VkCreateDirectFBSurfaceEXT = _VkCreateDirectFBSurfaceEXT

{-# INLINE _VkCreateDirectFBSurfaceEXT #-}

_VkCreateDirectFBSurfaceEXT :: CString
_VkCreateDirectFBSurfaceEXT = Ptr "vkCreateDirectFBSurfaceEXT\NUL"#

{-# INLINE is_VkCreateDirectFBSurfaceEXT #-}

is_VkCreateDirectFBSurfaceEXT :: CString -> Bool
is_VkCreateDirectFBSurfaceEXT
  = (EQ ==) . cmpCStrings _VkCreateDirectFBSurfaceEXT

type VkCreateDirectFBSurfaceEXT = "vkCreateDirectFBSurfaceEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDirectFBSurfaceEXT
--   >     ( VkInstance instance
--   >     , const VkDirectFBSurfaceCreateInfoEXT* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCreateDirectFBSurfaceEXT vkCreateDirectFBSurfaceEXT registry at www.khronos.org>
type HS_vkCreateDirectFBSurfaceEXT =
     VkInstance -- ^ instance
                ->
       Ptr VkDirectFBSurfaceCreateInfoEXT -- ^ pCreateInfo
                                          ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateDirectFBSurfaceEXT =
     FunPtr HS_vkCreateDirectFBSurfaceEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCreateDirectFBSurfaceEXTUnsafe ::
               PFN_vkCreateDirectFBSurfaceEXT -> HS_vkCreateDirectFBSurfaceEXT

foreign import ccall safe "dynamic"
               unwrapVkCreateDirectFBSurfaceEXTSafe ::
               PFN_vkCreateDirectFBSurfaceEXT -> HS_vkCreateDirectFBSurfaceEXT

instance VulkanProc "vkCreateDirectFBSurfaceEXT" where
    type VkProcType "vkCreateDirectFBSurfaceEXT" =
         HS_vkCreateDirectFBSurfaceEXT
    vkProcSymbol = _VkCreateDirectFBSurfaceEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateDirectFBSurfaceEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateDirectFBSurfaceEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceDirectFBPresentationSupportEXT ::
        CString

pattern VkGetPhysicalDeviceDirectFBPresentationSupportEXT <-
        (is_VkGetPhysicalDeviceDirectFBPresentationSupportEXT -> True)
  where
    VkGetPhysicalDeviceDirectFBPresentationSupportEXT
      = _VkGetPhysicalDeviceDirectFBPresentationSupportEXT

{-# INLINE _VkGetPhysicalDeviceDirectFBPresentationSupportEXT #-}

_VkGetPhysicalDeviceDirectFBPresentationSupportEXT :: CString
_VkGetPhysicalDeviceDirectFBPresentationSupportEXT
  = Ptr "vkGetPhysicalDeviceDirectFBPresentationSupportEXT\NUL"#

{-# INLINE is_VkGetPhysicalDeviceDirectFBPresentationSupportEXT #-}

is_VkGetPhysicalDeviceDirectFBPresentationSupportEXT ::
                                                     CString -> Bool
is_VkGetPhysicalDeviceDirectFBPresentationSupportEXT
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceDirectFBPresentationSupportEXT

type VkGetPhysicalDeviceDirectFBPresentationSupportEXT =
     "vkGetPhysicalDeviceDirectFBPresentationSupportEXT"

-- | > VkBool32 vkGetPhysicalDeviceDirectFBPresentationSupportEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , IDirectFB* dfb
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetPhysicalDeviceDirectFBPresentationSupportEXT vkGetPhysicalDeviceDirectFBPresentationSupportEXT registry at www.khronos.org>
type HS_vkGetPhysicalDeviceDirectFBPresentationSupportEXT =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Word32 -- ^ queueFamilyIndex
                                -> Ptr IDirectFB -- ^ dfb
                                                 -> IO VkBool32

type PFN_vkGetPhysicalDeviceDirectFBPresentationSupportEXT =
     FunPtr HS_vkGetPhysicalDeviceDirectFBPresentationSupportEXT

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceDirectFBPresentationSupportEXTUnsafe ::
               PFN_vkGetPhysicalDeviceDirectFBPresentationSupportEXT ->
                 HS_vkGetPhysicalDeviceDirectFBPresentationSupportEXT

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceDirectFBPresentationSupportEXTSafe ::
               PFN_vkGetPhysicalDeviceDirectFBPresentationSupportEXT ->
                 HS_vkGetPhysicalDeviceDirectFBPresentationSupportEXT

instance VulkanProc
           "vkGetPhysicalDeviceDirectFBPresentationSupportEXT"
         where
    type VkProcType "vkGetPhysicalDeviceDirectFBPresentationSupportEXT"
         = HS_vkGetPhysicalDeviceDirectFBPresentationSupportEXT
    vkProcSymbol = _VkGetPhysicalDeviceDirectFBPresentationSupportEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceDirectFBPresentationSupportEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceDirectFBPresentationSupportEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_DIRECTFB_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_DIRECTFB_SURFACE_SPEC_VERSION = 1

type VK_EXT_DIRECTFB_SURFACE_SPEC_VERSION = 1

pattern VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME :: CString

pattern VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME <-
        (is_VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME -> True)
  where
    VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME
      = _VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME

{-# INLINE _VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME #-}

_VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME :: CString
_VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME
  = Ptr "VK_EXT_directfb_surface\NUL"#

{-# INLINE is_VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME #-}

is_VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME

type VK_EXT_DIRECTFB_SURFACE_EXTENSION_NAME =
     "VK_EXT_directfb_surface"

pattern VK_STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT =
        VkStructureType 1000346000
