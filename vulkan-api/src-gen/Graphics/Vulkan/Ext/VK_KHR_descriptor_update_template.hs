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
module Graphics.Vulkan.Ext.VK_KHR_descriptor_update_template
       (VkAndroidSurfaceCreateFlagsKHR(..), VkBufferViewCreateFlags(..),
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
        VkXlibSurfaceCreateFlagsKHR(..),
        VkDescriptorUpdateTemplateCreateInfoKHR,
        VkDescriptorUpdateTemplateEntryKHR,
        VkDescriptorBindingBitmaskEXT(..),
        VkDescriptorPoolCreateBitmask(..), VkDescriptorType(..),
        VkDescriptorUpdateTemplateType(..),
        VkDescriptorBindingFlagBitsEXT(), VkDescriptorBindingFlagsEXT(),
        VkDescriptorPoolCreateFlagBits(), VkDescriptorPoolCreateFlags(),
        VkDescriptorSetLayoutCreateBitmask(..),
        VkDescriptorSetLayoutCreateFlagBits(),
        VkDescriptorSetLayoutCreateFlags(),
        VkDescriptorUpdateTemplateTypeKHR(..),
        VkCreateDescriptorUpdateTemplateKHR,
        pattern VkCreateDescriptorUpdateTemplateKHR,
        HS_vkCreateDescriptorUpdateTemplateKHR,
        PFN_vkCreateDescriptorUpdateTemplateKHR,
        VkDestroyDescriptorUpdateTemplateKHR,
        pattern VkDestroyDescriptorUpdateTemplateKHR,
        HS_vkDestroyDescriptorUpdateTemplateKHR,
        PFN_vkDestroyDescriptorUpdateTemplateKHR,
        VkUpdateDescriptorSetWithTemplateKHR,
        pattern VkUpdateDescriptorSetWithTemplateKHR,
        HS_vkUpdateDescriptorSetWithTemplateKHR,
        PFN_vkUpdateDescriptorSetWithTemplateKHR,
        module Graphics.Vulkan.Marshal, VkBool32(..), VkDeviceSize(..),
        VkFlags(..), VkSampleMask(..), VkInternalAllocationType(..),
        VkPipelineBindPoint(..), VkPipelineCacheHeaderVersion(..),
        VkPipelineCreateBitmask(..), VkPipelineStageBitmask(..),
        VkPipelineCacheCreateFlagBits(..),
        VkPipelineColorBlendStateCreateFlagBits(..),
        VkPipelineCreateFlagBits(), VkPipelineCreateFlags(),
        VkPipelineDepthStencilStateCreateFlagBits(..),
        VkPipelineDynamicStateCreateFlagBits(..),
        VkPipelineInputAssemblyStateCreateFlagBits(..),
        VkPipelineLayoutCreateFlagBits(..),
        VkPipelineMultisampleStateCreateFlagBits(..),
        VkPipelineRasterizationStateCreateFlagBits(..),
        VkPipelineShaderStageCreateFlagBits(..), VkPipelineStageFlagBits(),
        VkPipelineStageFlags(),
        VkPipelineTessellationStateCreateFlagBits(..),
        VkPipelineVertexInputStateCreateFlagBits(..),
        VkPipelineViewportStateCreateFlagBits(..), VkResult(..),
        VkStructureType(..), VkSystemAllocationScope(..),
        newVkAllocationFunction, newVkDebugReportCallbackEXT,
        newVkDebugUtilsMessengerCallbackEXT, newVkFreeFunction,
        newVkInternalAllocationNotification, newVkInternalFreeNotification,
        newVkReallocationFunction, newVkVoidFunction,
        unwrapVkAllocationFunction, unwrapVkDebugReportCallbackEXT,
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
        VkAllocationCallbacks, VkDescriptorBufferInfo,
        VkDescriptorImageInfo, VkDescriptorPoolCreateInfo,
        VkDescriptorPoolSize, VkDescriptorSetAllocateInfo,
        VkDescriptorSetLayoutBinding,
        VkDescriptorSetLayoutBindingFlagsCreateInfoEXT,
        VkDescriptorSetLayoutCreateInfo, VkDescriptorSetLayoutSupport,
        VkDescriptorSetLayoutSupportKHR,
        VkDescriptorSetVariableDescriptorCountAllocateInfoEXT,
        VkDescriptorSetVariableDescriptorCountLayoutSupportEXT,
        VkDescriptorUpdateTemplateCreateInfo,
        VkDescriptorUpdateTemplateEntry,
        VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION,
        pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION,
        VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME,
        pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR,
        pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR,
        pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR,
        -- ** Required extensions: 'VK_KHR_push_descriptor'.
        pattern VkCmdPushDescriptorSetWithTemplateKHR,
        HS_vkCmdPushDescriptorSetWithTemplateKHR,
        PFN_vkCmdPushDescriptorSetWithTemplateKHR,
        pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR,
        -- ** Required extensions: 'VK_EXT_debug_report'.
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT)
       where
import           GHC.Ptr                                           (Ptr (..))
import           Graphics.Vulkan.Core_1_1                          (pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE,
                                                                    pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO)
import           Graphics.Vulkan.Ext.VK_EXT_debug_report           (pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT)
import           Graphics.Vulkan.Ext.VK_KHR_push_descriptor        (HS_vkCmdPushDescriptorSetWithTemplateKHR,
                                                                    PFN_vkCmdPushDescriptorSetWithTemplateKHR,
                                                                    pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR,
                                                                    pattern VkCmdPushDescriptorSetWithTemplateKHR)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                      (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.Descriptor
import           Graphics.Vulkan.Types.Enum.InternalAllocationType
import           Graphics.Vulkan.Types.Enum.Pipeline
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.SystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.AllocationCallbacks
import           Graphics.Vulkan.Types.Struct.Descriptor

pattern VkCreateDescriptorUpdateTemplateKHR :: CString

pattern VkCreateDescriptorUpdateTemplateKHR <-
        (is_VkCreateDescriptorUpdateTemplateKHR -> True)
  where
    VkCreateDescriptorUpdateTemplateKHR
      = _VkCreateDescriptorUpdateTemplateKHR

{-# INLINE _VkCreateDescriptorUpdateTemplateKHR #-}

_VkCreateDescriptorUpdateTemplateKHR :: CString
_VkCreateDescriptorUpdateTemplateKHR
  = Ptr "vkCreateDescriptorUpdateTemplateKHR\NUL"#

{-# INLINE is_VkCreateDescriptorUpdateTemplateKHR #-}

is_VkCreateDescriptorUpdateTemplateKHR :: CString -> Bool
is_VkCreateDescriptorUpdateTemplateKHR
  = (EQ ==) . cmpCStrings _VkCreateDescriptorUpdateTemplateKHR

type VkCreateDescriptorUpdateTemplateKHR =
     "vkCreateDescriptorUpdateTemplateKHR"

-- | This is an alias for `vkCreateDescriptorUpdateTemplate`.
--
--   Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDescriptorUpdateTemplateKHR
--   >     ( VkDevice device
--   >     , const VkDescriptorUpdateTemplateCreateInfo* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDescriptorUpdateTemplate* pDescriptorUpdateTemplate
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDescriptorUpdateTemplateKHR vkCreateDescriptorUpdateTemplateKHR registry at www.khronos.org>
type HS_vkCreateDescriptorUpdateTemplateKHR =
     VkDevice -- ^ device
              ->
       Ptr VkDescriptorUpdateTemplateCreateInfo -- ^ pCreateInfo
                                                ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   ->
           Ptr VkDescriptorUpdateTemplate -- ^ pDescriptorUpdateTemplate
                                          -> IO VkResult

type PFN_vkCreateDescriptorUpdateTemplateKHR =
     FunPtr HS_vkCreateDescriptorUpdateTemplateKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCreateDescriptorUpdateTemplateKHRUnsafe ::
               PFN_vkCreateDescriptorUpdateTemplateKHR ->
                 HS_vkCreateDescriptorUpdateTemplateKHR

foreign import ccall safe "dynamic"
               unwrapVkCreateDescriptorUpdateTemplateKHRSafe ::
               PFN_vkCreateDescriptorUpdateTemplateKHR ->
                 HS_vkCreateDescriptorUpdateTemplateKHR

instance VulkanProc "vkCreateDescriptorUpdateTemplateKHR" where
    type VkProcType "vkCreateDescriptorUpdateTemplateKHR" =
         HS_vkCreateDescriptorUpdateTemplateKHR
    vkProcSymbol = _VkCreateDescriptorUpdateTemplateKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCreateDescriptorUpdateTemplateKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateDescriptorUpdateTemplateKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkDestroyDescriptorUpdateTemplateKHR :: CString

pattern VkDestroyDescriptorUpdateTemplateKHR <-
        (is_VkDestroyDescriptorUpdateTemplateKHR -> True)
  where
    VkDestroyDescriptorUpdateTemplateKHR
      = _VkDestroyDescriptorUpdateTemplateKHR

{-# INLINE _VkDestroyDescriptorUpdateTemplateKHR #-}

_VkDestroyDescriptorUpdateTemplateKHR :: CString
_VkDestroyDescriptorUpdateTemplateKHR
  = Ptr "vkDestroyDescriptorUpdateTemplateKHR\NUL"#

{-# INLINE is_VkDestroyDescriptorUpdateTemplateKHR #-}

is_VkDestroyDescriptorUpdateTemplateKHR :: CString -> Bool
is_VkDestroyDescriptorUpdateTemplateKHR
  = (EQ ==) . cmpCStrings _VkDestroyDescriptorUpdateTemplateKHR

type VkDestroyDescriptorUpdateTemplateKHR =
     "vkDestroyDescriptorUpdateTemplateKHR"

-- | This is an alias for `vkDestroyDescriptorUpdateTemplate`.
--
--   > void vkDestroyDescriptorUpdateTemplateKHR
--   >     ( VkDevice device
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDescriptorUpdateTemplateKHR vkDestroyDescriptorUpdateTemplateKHR registry at www.khronos.org>
type HS_vkDestroyDescriptorUpdateTemplateKHR =
     VkDevice -- ^ device
              ->
       VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                  -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                               -> IO ()

type PFN_vkDestroyDescriptorUpdateTemplateKHR =
     FunPtr HS_vkDestroyDescriptorUpdateTemplateKHR

foreign import ccall unsafe "dynamic"
               unwrapVkDestroyDescriptorUpdateTemplateKHRUnsafe ::
               PFN_vkDestroyDescriptorUpdateTemplateKHR ->
                 HS_vkDestroyDescriptorUpdateTemplateKHR

foreign import ccall safe "dynamic"
               unwrapVkDestroyDescriptorUpdateTemplateKHRSafe ::
               PFN_vkDestroyDescriptorUpdateTemplateKHR ->
                 HS_vkDestroyDescriptorUpdateTemplateKHR

instance VulkanProc "vkDestroyDescriptorUpdateTemplateKHR" where
    type VkProcType "vkDestroyDescriptorUpdateTemplateKHR" =
         HS_vkDestroyDescriptorUpdateTemplateKHR
    vkProcSymbol = _VkDestroyDescriptorUpdateTemplateKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkDestroyDescriptorUpdateTemplateKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkDestroyDescriptorUpdateTemplateKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkUpdateDescriptorSetWithTemplateKHR :: CString

pattern VkUpdateDescriptorSetWithTemplateKHR <-
        (is_VkUpdateDescriptorSetWithTemplateKHR -> True)
  where
    VkUpdateDescriptorSetWithTemplateKHR
      = _VkUpdateDescriptorSetWithTemplateKHR

{-# INLINE _VkUpdateDescriptorSetWithTemplateKHR #-}

_VkUpdateDescriptorSetWithTemplateKHR :: CString
_VkUpdateDescriptorSetWithTemplateKHR
  = Ptr "vkUpdateDescriptorSetWithTemplateKHR\NUL"#

{-# INLINE is_VkUpdateDescriptorSetWithTemplateKHR #-}

is_VkUpdateDescriptorSetWithTemplateKHR :: CString -> Bool
is_VkUpdateDescriptorSetWithTemplateKHR
  = (EQ ==) . cmpCStrings _VkUpdateDescriptorSetWithTemplateKHR

type VkUpdateDescriptorSetWithTemplateKHR =
     "vkUpdateDescriptorSetWithTemplateKHR"

-- | This is an alias for `vkUpdateDescriptorSetWithTemplate`.
--
--   > void vkUpdateDescriptorSetWithTemplateKHR
--   >     ( VkDevice device
--   >     , VkDescriptorSet descriptorSet
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkUpdateDescriptorSetWithTemplateKHR vkUpdateDescriptorSetWithTemplateKHR registry at www.khronos.org>
type HS_vkUpdateDescriptorSetWithTemplateKHR =
     VkDevice -- ^ device
              ->
       VkDescriptorSet -- ^ descriptorSet
                       -> VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                                     -> Ptr Void -- ^ pData
                                                                 -> IO ()

type PFN_vkUpdateDescriptorSetWithTemplateKHR =
     FunPtr HS_vkUpdateDescriptorSetWithTemplateKHR

foreign import ccall unsafe "dynamic"
               unwrapVkUpdateDescriptorSetWithTemplateKHRUnsafe ::
               PFN_vkUpdateDescriptorSetWithTemplateKHR ->
                 HS_vkUpdateDescriptorSetWithTemplateKHR

foreign import ccall safe "dynamic"
               unwrapVkUpdateDescriptorSetWithTemplateKHRSafe ::
               PFN_vkUpdateDescriptorSetWithTemplateKHR ->
                 HS_vkUpdateDescriptorSetWithTemplateKHR

instance VulkanProc "vkUpdateDescriptorSetWithTemplateKHR" where
    type VkProcType "vkUpdateDescriptorSetWithTemplateKHR" =
         HS_vkUpdateDescriptorSetWithTemplateKHR
    vkProcSymbol = _VkUpdateDescriptorSetWithTemplateKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkUpdateDescriptorSetWithTemplateKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkUpdateDescriptorSetWithTemplateKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION = 1

type VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION = 1

pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME :: CString

pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME <-
        (is_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME -> True)
  where
    VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
      = _VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME

{-# INLINE _VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME #-}

_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME :: CString
_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
  = Ptr "VK_KHR_descriptor_update_template\NUL"#

{-# INLINE is_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME #-}

is_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME ::
                                                    CString -> Bool
is_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME

type VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME =
     "VK_KHR_descriptor_update_template"

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR
        = VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO

pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR =
        VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE

pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR =
        VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT
        = VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT
