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
module Graphics.Vulkan.Ext.VK_NN_vi_surface
       (VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkStructureType(..), VkAndroidSurfaceCreateFlagsKHR(..),
        VkBufferViewCreateFlags(..), VkCommandPoolTrimFlags(..),
        VkCommandPoolTrimFlagsKHR(..),
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
        VkXlibSurfaceCreateFlagsKHR(..), VkViSurfaceCreateInfoNN,
        -- > #include "vk_platform.h"
        VkCreateViSurfaceNN, pattern VkCreateViSurfaceNN,
        HS_vkCreateViSurfaceNN, PFN_vkCreateViSurfaceNN,
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
        VkAllocationCallbacks, VK_NN_VI_SURFACE_SPEC_VERSION,
        pattern VK_NN_VI_SURFACE_SPEC_VERSION,
        VK_NN_VI_SURFACE_EXTENSION_NAME,
        pattern VK_NN_VI_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN)
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
import           Graphics.Vulkan.Types.Struct.PlatformViNn

pattern VkCreateViSurfaceNN :: CString

pattern VkCreateViSurfaceNN <- (is_VkCreateViSurfaceNN -> True)
  where
    VkCreateViSurfaceNN = _VkCreateViSurfaceNN

{-# INLINE _VkCreateViSurfaceNN #-}

_VkCreateViSurfaceNN :: CString
_VkCreateViSurfaceNN = Ptr "vkCreateViSurfaceNN\NUL"#

{-# INLINE is_VkCreateViSurfaceNN #-}

is_VkCreateViSurfaceNN :: CString -> Bool
is_VkCreateViSurfaceNN = (EQ ==) . cmpCStrings _VkCreateViSurfaceNN

type VkCreateViSurfaceNN = "vkCreateViSurfaceNN"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateViSurfaceNN
--   >     ( VkInstance instance
--   >     , const VkViSurfaceCreateInfoNN* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateViSurfaceNN vkCreateViSurfaceNN registry at www.khronos.org>
type HS_vkCreateViSurfaceNN =
     VkInstance -- ^ instance
                ->
       Ptr VkViSurfaceCreateInfoNN -- ^ pCreateInfo
                                   ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateViSurfaceNN = FunPtr HS_vkCreateViSurfaceNN

foreign import ccall unsafe "dynamic"
               unwrapVkCreateViSurfaceNNUnsafe ::
               PFN_vkCreateViSurfaceNN -> HS_vkCreateViSurfaceNN

foreign import ccall safe "dynamic" unwrapVkCreateViSurfaceNNSafe
               :: PFN_vkCreateViSurfaceNN -> HS_vkCreateViSurfaceNN

instance VulkanProc "vkCreateViSurfaceNN" where
    type VkProcType "vkCreateViSurfaceNN" = HS_vkCreateViSurfaceNN
    vkProcSymbol = _VkCreateViSurfaceNN

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateViSurfaceNNUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateViSurfaceNNSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_NN_VI_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NN_VI_SURFACE_SPEC_VERSION = 1

type VK_NN_VI_SURFACE_SPEC_VERSION = 1

pattern VK_NN_VI_SURFACE_EXTENSION_NAME :: CString

pattern VK_NN_VI_SURFACE_EXTENSION_NAME <-
        (is_VK_NN_VI_SURFACE_EXTENSION_NAME -> True)
  where
    VK_NN_VI_SURFACE_EXTENSION_NAME = _VK_NN_VI_SURFACE_EXTENSION_NAME

{-# INLINE _VK_NN_VI_SURFACE_EXTENSION_NAME #-}

_VK_NN_VI_SURFACE_EXTENSION_NAME :: CString
_VK_NN_VI_SURFACE_EXTENSION_NAME = Ptr "VK_NN_vi_surface\NUL"#

{-# INLINE is_VK_NN_VI_SURFACE_EXTENSION_NAME #-}

is_VK_NN_VI_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_NN_VI_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_NN_VI_SURFACE_EXTENSION_NAME

type VK_NN_VI_SURFACE_EXTENSION_NAME = "VK_NN_vi_surface"

pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN =
        VkStructureType 1000062000
