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
module Graphics.Vulkan.Ext.VK_MVK_macos_surface
       (VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkAndroidSurfaceCreateFlagsKHR(..), VkBufferViewCreateFlags(..),
        VkCommandPoolTrimFlags(..), VkCommandPoolTrimFlagsKHR(..),
        VkDebugUtilsMessengerCallbackDataFlagsEXT(..),
        VkDebugUtilsMessengerCreateFlagsEXT(..),
        VkDescriptorPoolResetFlags(..),
        VkDescriptorUpdateTemplateCreateFlags(..),
        VkDescriptorUpdateTemplateCreateFlagsKHR(..),
        VkDeviceCreateFlags(..), VkDisplayModeCreateFlagsKHR(..),
        VkDisplaySurfaceCreateFlagsKHR(..), VkEventCreateFlags(..),
        VkExternalFenceFeatureFlagsKHR(..),
        VkExternalFenceHandleTypeFlagsKHR(..),
        VkExternalMemoryFeatureFlagsKHR(..),
        VkExternalMemoryHandleTypeFlagsKHR(..),
        VkExternalSemaphoreFeatureFlagsKHR(..),
        VkExternalSemaphoreHandleTypeFlagsKHR(..),
        VkFenceImportFlagsKHR(..), VkFramebufferCreateFlags(..),
        VkIOSSurfaceCreateFlagsMVK(..), VkImageViewCreateFlags(..),
        VkInstanceCreateFlags(..), VkMacOSSurfaceCreateFlagsMVK(..),
        VkMemoryAllocateFlagsKHR(..), VkMemoryMapFlags(..),
        VkMirSurfaceCreateFlagsKHR(..), VkPeerMemoryFeatureFlagsKHR(..),
        VkPipelineCacheCreateFlags(..),
        VkPipelineColorBlendStateCreateFlags(..),
        VkPipelineCoverageModulationStateCreateFlagsNV(..),
        VkPipelineCoverageToColorStateCreateFlagsNV(..),
        VkPipelineDepthStencilStateCreateFlags(..),
        VkPipelineDiscardRectangleStateCreateFlagsEXT(..),
        VkPipelineDynamicStateCreateFlags(..),
        VkPipelineInputAssemblyStateCreateFlags(..),
        VkPipelineLayoutCreateFlags(..),
        VkPipelineMultisampleStateCreateFlags(..),
        VkPipelineRasterizationConservativeStateCreateFlagsEXT(..),
        VkPipelineRasterizationStateCreateFlags(..),
        VkPipelineShaderStageCreateFlags(..),
        VkPipelineTessellationStateCreateFlags(..),
        VkPipelineVertexInputStateCreateFlags(..),
        VkPipelineViewportStateCreateFlags(..),
        VkPipelineViewportSwizzleStateCreateFlagsNV(..),
        VkQueryPoolCreateFlags(..), VkRenderPassCreateFlags(..),
        VkSamplerCreateFlags(..), VkSemaphoreCreateFlags(..),
        VkSemaphoreImportFlagsKHR(..), VkShaderModuleCreateFlags(..),
        VkValidationCacheCreateFlagsEXT(..), VkViSurfaceCreateFlagsNN(..),
        VkWaylandSurfaceCreateFlagsKHR(..),
        VkWin32SurfaceCreateFlagsKHR(..), VkXcbSurfaceCreateFlagsKHR(..),
        VkXlibSurfaceCreateFlagsKHR(..), VkMacOSSurfaceCreateInfoMVK,
        VkStructureType(..), -- > #include "vk_platform.h"
                             VkCreateMacOSSurfaceMVK,
        pattern VkCreateMacOSSurfaceMVK, HS_vkCreateMacOSSurfaceMVK,
        PFN_vkCreateMacOSSurfaceMVK, module Graphics.Vulkan.Marshal,
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
        PFN_vkVoidFunction, VkBuffer, VkBufferView, VkBufferView_T(),
        VkBuffer_T(), VkCommandBuffer, VkCommandBuffer_T(), VkCommandPool,
        VkCommandPool_T(), VkDebugReportCallbackEXT,
        VkDebugReportCallbackEXT_T(), VkDebugUtilsMessengerEXT,
        VkDebugUtilsMessengerEXT_T(), VkDescriptorPool,
        VkDescriptorPool_T(), VkDescriptorSet, VkDescriptorSetLayout,
        VkDescriptorSetLayout_T(), VkDescriptorSet_T(),
        VkDescriptorUpdateTemplate, VkDescriptorUpdateTemplateKHR,
        VkDescriptorUpdateTemplateKHR_T(), VkDescriptorUpdateTemplate_T(),
        VkDevice, VkDeviceMemory, VkDeviceMemory_T(), VkDevice_T(),
        VkDisplayKHR, VkDisplayKHR_T(), VkDisplayModeKHR,
        VkDisplayModeKHR_T(), VkEvent, VkEvent_T(), VkFence, VkFence_T(),
        VkFramebuffer, VkFramebuffer_T(), VkImage, VkImageView,
        VkImageView_T(), VkImage_T(), VkIndirectCommandsLayoutNVX,
        VkIndirectCommandsLayoutNVX_T(), VkInstance, VkInstance_T(),
        VkObjectTableNVX, VkObjectTableNVX_T(), VkPhysicalDevice,
        VkPhysicalDevice_T(), VkPipeline, VkPipelineCache,
        VkPipelineCache_T(), VkPipelineLayout, VkPipelineLayout_T(),
        VkPipeline_T(), VkQueryPool, VkQueryPool_T(), VkQueue, VkQueue_T(),
        VkRenderPass, VkRenderPass_T(), VkSampler,
        VkSamplerYcbcrConversion, VkSamplerYcbcrConversionKHR,
        VkSamplerYcbcrConversionKHR_T(), VkSamplerYcbcrConversion_T(),
        VkSampler_T(), VkSemaphore, VkSemaphore_T(), VkShaderModule,
        VkShaderModule_T(), VkSurfaceKHR, VkSurfaceKHR_T(), VkSwapchainKHR,
        VkSwapchainKHR_T(), VkValidationCacheEXT, VkValidationCacheEXT_T(),
        VkAllocationCallbacks, VK_MVK_MACOS_SURFACE_SPEC_VERSION,
        pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION,
        VK_MVK_MACOS_SURFACE_EXTENSION_NAME,
        pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK)
       where
import           GHC.Ptr                                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                      (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.InternalAllocationType
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.SystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.AllocationCallbacks
import           Graphics.Vulkan.Types.Struct.PlatformMacosMvk

pattern VkCreateMacOSSurfaceMVK :: CString

pattern VkCreateMacOSSurfaceMVK <-
        (is_VkCreateMacOSSurfaceMVK -> True)
  where
    VkCreateMacOSSurfaceMVK = _VkCreateMacOSSurfaceMVK

{-# INLINE _VkCreateMacOSSurfaceMVK #-}

_VkCreateMacOSSurfaceMVK :: CString
_VkCreateMacOSSurfaceMVK = Ptr "vkCreateMacOSSurfaceMVK\NUL"#

{-# INLINE is_VkCreateMacOSSurfaceMVK #-}

is_VkCreateMacOSSurfaceMVK :: CString -> Bool
is_VkCreateMacOSSurfaceMVK
  = (EQ ==) . cmpCStrings _VkCreateMacOSSurfaceMVK

type VkCreateMacOSSurfaceMVK = "vkCreateMacOSSurfaceMVK"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateMacOSSurfaceMVK
--   >     ( VkInstance instance
--   >     , const VkMacOSSurfaceCreateInfoMVK* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateMacOSSurfaceMVK vkCreateMacOSSurfaceMVK registry at www.khronos.org>
type HS_vkCreateMacOSSurfaceMVK =
     VkInstance -- ^ instance
                ->
       Ptr VkMacOSSurfaceCreateInfoMVK -- ^ pCreateInfo
                                       ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateMacOSSurfaceMVK =
     FunPtr HS_vkCreateMacOSSurfaceMVK

foreign import ccall unsafe "dynamic"
               unwrapVkCreateMacOSSurfaceMVKUnsafe ::
               PFN_vkCreateMacOSSurfaceMVK -> HS_vkCreateMacOSSurfaceMVK

foreign import ccall safe "dynamic"
               unwrapVkCreateMacOSSurfaceMVKSafe ::
               PFN_vkCreateMacOSSurfaceMVK -> HS_vkCreateMacOSSurfaceMVK

instance VulkanProc "vkCreateMacOSSurfaceMVK" where
    type VkProcType "vkCreateMacOSSurfaceMVK" =
         HS_vkCreateMacOSSurfaceMVK
    vkProcSymbol = _VkCreateMacOSSurfaceMVK

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateMacOSSurfaceMVKUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateMacOSSurfaceMVKSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION = 2

type VK_MVK_MACOS_SURFACE_SPEC_VERSION = 2

pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME :: CString

pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME <-
        (is_VK_MVK_MACOS_SURFACE_EXTENSION_NAME -> True)
  where
    VK_MVK_MACOS_SURFACE_EXTENSION_NAME
      = _VK_MVK_MACOS_SURFACE_EXTENSION_NAME

{-# INLINE _VK_MVK_MACOS_SURFACE_EXTENSION_NAME #-}

_VK_MVK_MACOS_SURFACE_EXTENSION_NAME :: CString
_VK_MVK_MACOS_SURFACE_EXTENSION_NAME
  = Ptr "VK_MVK_macos_surface\NUL"#

{-# INLINE is_VK_MVK_MACOS_SURFACE_EXTENSION_NAME #-}

is_VK_MVK_MACOS_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_MVK_MACOS_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_MVK_MACOS_SURFACE_EXTENSION_NAME

type VK_MVK_MACOS_SURFACE_EXTENSION_NAME = "VK_MVK_macos_surface"

pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK =
        VkStructureType 1000123000
