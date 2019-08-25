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
module Graphics.Vulkan.Ext.VK_EXT_debug_utils
       (-- * Vulkan extension: @VK_EXT_debug_utils@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Mark Young @marky-lunarg@
        --
        -- author: @EXT@
        --
        -- type: @instance@
        --
        -- Extension number: @129@
        module Graphics.Vulkan.Marshal, VkApplicationInfo, VkBool32(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkDebugUtilsLabelEXT, VkDebugReportBitmaskEXT(..),
        VkDebugReportObjectTypeEXT(..),
        VkDebugUtilsMessageSeverityBitmaskEXT(..),
        VkDebugUtilsMessageTypeBitmaskEXT(..), VkDebugReportFlagBitsEXT(),
        VkDebugReportFlagsEXT(), VkDebugUtilsMessageSeverityFlagBitsEXT(),
        VkDebugUtilsMessageSeverityFlagsEXT(),
        VkDebugUtilsMessageTypeFlagBitsEXT(),
        VkDebugUtilsMessageTypeFlagsEXT(),
        VkDebugUtilsMessengerCallbackDataEXT,
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
        VkXlibSurfaceCreateFlagsKHR(..),
        VkDebugUtilsMessengerCreateInfoEXT, VkDebugUtilsObjectNameInfoEXT,
        VkDebugUtilsObjectTagInfoEXT, VkInstanceCreateInfo,
        VkObjectEntryTypeNVX(..), VkObjectEntryUsageBitmaskNVX(..),
        VkObjectType(..), VkObjectEntryUsageFlagBitsNVX(),
        VkObjectEntryUsageFlagsNVX(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VkSetDebugUtilsObjectNameEXT, pattern VkSetDebugUtilsObjectNameEXT,
        HS_vkSetDebugUtilsObjectNameEXT, PFN_vkSetDebugUtilsObjectNameEXT,
        VkSetDebugUtilsObjectTagEXT, pattern VkSetDebugUtilsObjectTagEXT,
        HS_vkSetDebugUtilsObjectTagEXT, PFN_vkSetDebugUtilsObjectTagEXT,
        VkQueueBeginDebugUtilsLabelEXT,
        pattern VkQueueBeginDebugUtilsLabelEXT,
        HS_vkQueueBeginDebugUtilsLabelEXT,
        PFN_vkQueueBeginDebugUtilsLabelEXT, VkQueueEndDebugUtilsLabelEXT,
        pattern VkQueueEndDebugUtilsLabelEXT,
        HS_vkQueueEndDebugUtilsLabelEXT, PFN_vkQueueEndDebugUtilsLabelEXT,
        VkQueueInsertDebugUtilsLabelEXT,
        pattern VkQueueInsertDebugUtilsLabelEXT,
        HS_vkQueueInsertDebugUtilsLabelEXT,
        PFN_vkQueueInsertDebugUtilsLabelEXT, VkCmdBeginDebugUtilsLabelEXT,
        pattern VkCmdBeginDebugUtilsLabelEXT,
        HS_vkCmdBeginDebugUtilsLabelEXT, PFN_vkCmdBeginDebugUtilsLabelEXT,
        VkCmdEndDebugUtilsLabelEXT, pattern VkCmdEndDebugUtilsLabelEXT,
        HS_vkCmdEndDebugUtilsLabelEXT, PFN_vkCmdEndDebugUtilsLabelEXT,
        VkCmdInsertDebugUtilsLabelEXT,
        pattern VkCmdInsertDebugUtilsLabelEXT,
        HS_vkCmdInsertDebugUtilsLabelEXT,
        PFN_vkCmdInsertDebugUtilsLabelEXT, VkCreateDebugUtilsMessengerEXT,
        pattern VkCreateDebugUtilsMessengerEXT,
        HS_vkCreateDebugUtilsMessengerEXT,
        PFN_vkCreateDebugUtilsMessengerEXT,
        VkDestroyDebugUtilsMessengerEXT,
        pattern VkDestroyDebugUtilsMessengerEXT,
        HS_vkDestroyDebugUtilsMessengerEXT,
        PFN_vkDestroyDebugUtilsMessengerEXT, VkSubmitDebugUtilsMessageEXT,
        pattern VkSubmitDebugUtilsMessageEXT,
        HS_vkSubmitDebugUtilsMessageEXT, PFN_vkSubmitDebugUtilsMessageEXT,
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
        VkAllocationCallbacks, VkDebugMarkerMarkerInfoEXT,
        VkDebugMarkerObjectNameInfoEXT, VkDebugMarkerObjectTagInfoEXT,
        VkDebugReportCallbackCreateInfoEXT,
        VK_EXT_DEBUG_UTILS_SPEC_VERSION,
        pattern VK_EXT_DEBUG_UTILS_SPEC_VERSION,
        VK_EXT_DEBUG_UTILS_EXTENSION_NAME,
        pattern VK_EXT_DEBUG_UTILS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,
        pattern VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT)
       where
import           GHC.Ptr                                                         (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                                    (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.Debug
import           Graphics.Vulkan.Types.Enum.InternalAllocationType
import           Graphics.Vulkan.Types.Enum.Object
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.SystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.AllocationCallbacks
import           Graphics.Vulkan.Types.Struct.ApplicationInfo
import           Graphics.Vulkan.Types.Struct.Debug
import           Graphics.Vulkan.Types.Struct.DebugUtilsLabelEXT
import           Graphics.Vulkan.Types.Struct.DebugUtilsMessengerCallbackDataEXT
import           Graphics.Vulkan.Types.Struct.DebugUtilsMessengerCreateInfoEXT
import           Graphics.Vulkan.Types.Struct.DebugUtilsObjectNameInfoEXT
import           Graphics.Vulkan.Types.Struct.InstanceCreateInfo

pattern VkSetDebugUtilsObjectNameEXT :: CString

pattern VkSetDebugUtilsObjectNameEXT <-
        (is_VkSetDebugUtilsObjectNameEXT -> True)
  where
    VkSetDebugUtilsObjectNameEXT = _VkSetDebugUtilsObjectNameEXT

{-# INLINE _VkSetDebugUtilsObjectNameEXT #-}

_VkSetDebugUtilsObjectNameEXT :: CString
_VkSetDebugUtilsObjectNameEXT
  = Ptr "vkSetDebugUtilsObjectNameEXT\NUL"#

{-# INLINE is_VkSetDebugUtilsObjectNameEXT #-}

is_VkSetDebugUtilsObjectNameEXT :: CString -> Bool
is_VkSetDebugUtilsObjectNameEXT
  = (EQ ==) . cmpCStrings _VkSetDebugUtilsObjectNameEXT

type VkSetDebugUtilsObjectNameEXT = "vkSetDebugUtilsObjectNameEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkSetDebugUtilsObjectNameEXT
--   >     ( VkDevice device
--   >     , const VkDebugUtilsObjectNameInfoEXT* pNameInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkSetDebugUtilsObjectNameEXT vkSetDebugUtilsObjectNameEXT registry at www.khronos.org>
type HS_vkSetDebugUtilsObjectNameEXT =
     VkDevice -- ^ device
              -> Ptr VkDebugUtilsObjectNameInfoEXT -- ^ pNameInfo
                                                   -> IO VkResult

type PFN_vkSetDebugUtilsObjectNameEXT =
     FunPtr HS_vkSetDebugUtilsObjectNameEXT

foreign import ccall unsafe "dynamic"
               unwrapVkSetDebugUtilsObjectNameEXTUnsafe ::
               PFN_vkSetDebugUtilsObjectNameEXT -> HS_vkSetDebugUtilsObjectNameEXT

foreign import ccall safe "dynamic"
               unwrapVkSetDebugUtilsObjectNameEXTSafe ::
               PFN_vkSetDebugUtilsObjectNameEXT -> HS_vkSetDebugUtilsObjectNameEXT

instance VulkanProc "vkSetDebugUtilsObjectNameEXT" where
    type VkProcType "vkSetDebugUtilsObjectNameEXT" =
         HS_vkSetDebugUtilsObjectNameEXT
    vkProcSymbol = _VkSetDebugUtilsObjectNameEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkSetDebugUtilsObjectNameEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkSetDebugUtilsObjectNameEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkSetDebugUtilsObjectTagEXT :: CString

pattern VkSetDebugUtilsObjectTagEXT <-
        (is_VkSetDebugUtilsObjectTagEXT -> True)
  where
    VkSetDebugUtilsObjectTagEXT = _VkSetDebugUtilsObjectTagEXT

{-# INLINE _VkSetDebugUtilsObjectTagEXT #-}

_VkSetDebugUtilsObjectTagEXT :: CString
_VkSetDebugUtilsObjectTagEXT
  = Ptr "vkSetDebugUtilsObjectTagEXT\NUL"#

{-# INLINE is_VkSetDebugUtilsObjectTagEXT #-}

is_VkSetDebugUtilsObjectTagEXT :: CString -> Bool
is_VkSetDebugUtilsObjectTagEXT
  = (EQ ==) . cmpCStrings _VkSetDebugUtilsObjectTagEXT

type VkSetDebugUtilsObjectTagEXT = "vkSetDebugUtilsObjectTagEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkSetDebugUtilsObjectTagEXT
--   >     ( VkDevice device
--   >     , const VkDebugUtilsObjectTagInfoEXT* pTagInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkSetDebugUtilsObjectTagEXT vkSetDebugUtilsObjectTagEXT registry at www.khronos.org>
type HS_vkSetDebugUtilsObjectTagEXT =
     VkDevice -- ^ device
              -> Ptr VkDebugUtilsObjectTagInfoEXT -- ^ pTagInfo
                                                  -> IO VkResult

type PFN_vkSetDebugUtilsObjectTagEXT =
     FunPtr HS_vkSetDebugUtilsObjectTagEXT

foreign import ccall unsafe "dynamic"
               unwrapVkSetDebugUtilsObjectTagEXTUnsafe ::
               PFN_vkSetDebugUtilsObjectTagEXT -> HS_vkSetDebugUtilsObjectTagEXT

foreign import ccall safe "dynamic"
               unwrapVkSetDebugUtilsObjectTagEXTSafe ::
               PFN_vkSetDebugUtilsObjectTagEXT -> HS_vkSetDebugUtilsObjectTagEXT

instance VulkanProc "vkSetDebugUtilsObjectTagEXT" where
    type VkProcType "vkSetDebugUtilsObjectTagEXT" =
         HS_vkSetDebugUtilsObjectTagEXT
    vkProcSymbol = _VkSetDebugUtilsObjectTagEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkSetDebugUtilsObjectTagEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkSetDebugUtilsObjectTagEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkQueueBeginDebugUtilsLabelEXT :: CString

pattern VkQueueBeginDebugUtilsLabelEXT <-
        (is_VkQueueBeginDebugUtilsLabelEXT -> True)
  where
    VkQueueBeginDebugUtilsLabelEXT = _VkQueueBeginDebugUtilsLabelEXT

{-# INLINE _VkQueueBeginDebugUtilsLabelEXT #-}

_VkQueueBeginDebugUtilsLabelEXT :: CString
_VkQueueBeginDebugUtilsLabelEXT
  = Ptr "vkQueueBeginDebugUtilsLabelEXT\NUL"#

{-# INLINE is_VkQueueBeginDebugUtilsLabelEXT #-}

is_VkQueueBeginDebugUtilsLabelEXT :: CString -> Bool
is_VkQueueBeginDebugUtilsLabelEXT
  = (EQ ==) . cmpCStrings _VkQueueBeginDebugUtilsLabelEXT

type VkQueueBeginDebugUtilsLabelEXT =
     "vkQueueBeginDebugUtilsLabelEXT"

-- | > void vkQueueBeginDebugUtilsLabelEXT
--   >     ( VkQueue queue
--   >     , const VkDebugUtilsLabelEXT* pLabelInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkQueueBeginDebugUtilsLabelEXT vkQueueBeginDebugUtilsLabelEXT registry at www.khronos.org>
type HS_vkQueueBeginDebugUtilsLabelEXT =
     VkQueue -- ^ queue
             -> Ptr VkDebugUtilsLabelEXT -- ^ pLabelInfo
                                         -> IO ()

type PFN_vkQueueBeginDebugUtilsLabelEXT =
     FunPtr HS_vkQueueBeginDebugUtilsLabelEXT

foreign import ccall unsafe "dynamic"
               unwrapVkQueueBeginDebugUtilsLabelEXTUnsafe ::
               PFN_vkQueueBeginDebugUtilsLabelEXT ->
                 HS_vkQueueBeginDebugUtilsLabelEXT

foreign import ccall safe "dynamic"
               unwrapVkQueueBeginDebugUtilsLabelEXTSafe ::
               PFN_vkQueueBeginDebugUtilsLabelEXT ->
                 HS_vkQueueBeginDebugUtilsLabelEXT

instance VulkanProc "vkQueueBeginDebugUtilsLabelEXT" where
    type VkProcType "vkQueueBeginDebugUtilsLabelEXT" =
         HS_vkQueueBeginDebugUtilsLabelEXT
    vkProcSymbol = _VkQueueBeginDebugUtilsLabelEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkQueueBeginDebugUtilsLabelEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkQueueBeginDebugUtilsLabelEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkQueueEndDebugUtilsLabelEXT :: CString

pattern VkQueueEndDebugUtilsLabelEXT <-
        (is_VkQueueEndDebugUtilsLabelEXT -> True)
  where
    VkQueueEndDebugUtilsLabelEXT = _VkQueueEndDebugUtilsLabelEXT

{-# INLINE _VkQueueEndDebugUtilsLabelEXT #-}

_VkQueueEndDebugUtilsLabelEXT :: CString
_VkQueueEndDebugUtilsLabelEXT
  = Ptr "vkQueueEndDebugUtilsLabelEXT\NUL"#

{-# INLINE is_VkQueueEndDebugUtilsLabelEXT #-}

is_VkQueueEndDebugUtilsLabelEXT :: CString -> Bool
is_VkQueueEndDebugUtilsLabelEXT
  = (EQ ==) . cmpCStrings _VkQueueEndDebugUtilsLabelEXT

type VkQueueEndDebugUtilsLabelEXT = "vkQueueEndDebugUtilsLabelEXT"

-- | > void vkQueueEndDebugUtilsLabelEXT
--   >     ( VkQueue queue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkQueueEndDebugUtilsLabelEXT vkQueueEndDebugUtilsLabelEXT registry at www.khronos.org>
type HS_vkQueueEndDebugUtilsLabelEXT = VkQueue -- ^ queue
                                               -> IO ()

type PFN_vkQueueEndDebugUtilsLabelEXT =
     FunPtr HS_vkQueueEndDebugUtilsLabelEXT

foreign import ccall unsafe "dynamic"
               unwrapVkQueueEndDebugUtilsLabelEXTUnsafe ::
               PFN_vkQueueEndDebugUtilsLabelEXT -> HS_vkQueueEndDebugUtilsLabelEXT

foreign import ccall safe "dynamic"
               unwrapVkQueueEndDebugUtilsLabelEXTSafe ::
               PFN_vkQueueEndDebugUtilsLabelEXT -> HS_vkQueueEndDebugUtilsLabelEXT

instance VulkanProc "vkQueueEndDebugUtilsLabelEXT" where
    type VkProcType "vkQueueEndDebugUtilsLabelEXT" =
         HS_vkQueueEndDebugUtilsLabelEXT
    vkProcSymbol = _VkQueueEndDebugUtilsLabelEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkQueueEndDebugUtilsLabelEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkQueueEndDebugUtilsLabelEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkQueueInsertDebugUtilsLabelEXT :: CString

pattern VkQueueInsertDebugUtilsLabelEXT <-
        (is_VkQueueInsertDebugUtilsLabelEXT -> True)
  where
    VkQueueInsertDebugUtilsLabelEXT = _VkQueueInsertDebugUtilsLabelEXT

{-# INLINE _VkQueueInsertDebugUtilsLabelEXT #-}

_VkQueueInsertDebugUtilsLabelEXT :: CString
_VkQueueInsertDebugUtilsLabelEXT
  = Ptr "vkQueueInsertDebugUtilsLabelEXT\NUL"#

{-# INLINE is_VkQueueInsertDebugUtilsLabelEXT #-}

is_VkQueueInsertDebugUtilsLabelEXT :: CString -> Bool
is_VkQueueInsertDebugUtilsLabelEXT
  = (EQ ==) . cmpCStrings _VkQueueInsertDebugUtilsLabelEXT

type VkQueueInsertDebugUtilsLabelEXT =
     "vkQueueInsertDebugUtilsLabelEXT"

-- | > void vkQueueInsertDebugUtilsLabelEXT
--   >     ( VkQueue queue
--   >     , const VkDebugUtilsLabelEXT* pLabelInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkQueueInsertDebugUtilsLabelEXT vkQueueInsertDebugUtilsLabelEXT registry at www.khronos.org>
type HS_vkQueueInsertDebugUtilsLabelEXT =
     VkQueue -- ^ queue
             -> Ptr VkDebugUtilsLabelEXT -- ^ pLabelInfo
                                         -> IO ()

type PFN_vkQueueInsertDebugUtilsLabelEXT =
     FunPtr HS_vkQueueInsertDebugUtilsLabelEXT

foreign import ccall unsafe "dynamic"
               unwrapVkQueueInsertDebugUtilsLabelEXTUnsafe ::
               PFN_vkQueueInsertDebugUtilsLabelEXT ->
                 HS_vkQueueInsertDebugUtilsLabelEXT

foreign import ccall safe "dynamic"
               unwrapVkQueueInsertDebugUtilsLabelEXTSafe ::
               PFN_vkQueueInsertDebugUtilsLabelEXT ->
                 HS_vkQueueInsertDebugUtilsLabelEXT

instance VulkanProc "vkQueueInsertDebugUtilsLabelEXT" where
    type VkProcType "vkQueueInsertDebugUtilsLabelEXT" =
         HS_vkQueueInsertDebugUtilsLabelEXT
    vkProcSymbol = _VkQueueInsertDebugUtilsLabelEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkQueueInsertDebugUtilsLabelEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkQueueInsertDebugUtilsLabelEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdBeginDebugUtilsLabelEXT :: CString

pattern VkCmdBeginDebugUtilsLabelEXT <-
        (is_VkCmdBeginDebugUtilsLabelEXT -> True)
  where
    VkCmdBeginDebugUtilsLabelEXT = _VkCmdBeginDebugUtilsLabelEXT

{-# INLINE _VkCmdBeginDebugUtilsLabelEXT #-}

_VkCmdBeginDebugUtilsLabelEXT :: CString
_VkCmdBeginDebugUtilsLabelEXT
  = Ptr "vkCmdBeginDebugUtilsLabelEXT\NUL"#

{-# INLINE is_VkCmdBeginDebugUtilsLabelEXT #-}

is_VkCmdBeginDebugUtilsLabelEXT :: CString -> Bool
is_VkCmdBeginDebugUtilsLabelEXT
  = (EQ ==) . cmpCStrings _VkCmdBeginDebugUtilsLabelEXT

type VkCmdBeginDebugUtilsLabelEXT = "vkCmdBeginDebugUtilsLabelEXT"

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdBeginDebugUtilsLabelEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkDebugUtilsLabelEXT* pLabelInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdBeginDebugUtilsLabelEXT vkCmdBeginDebugUtilsLabelEXT registry at www.khronos.org>
type HS_vkCmdBeginDebugUtilsLabelEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr VkDebugUtilsLabelEXT -- ^ pLabelInfo
                                                 -> IO ()

type PFN_vkCmdBeginDebugUtilsLabelEXT =
     FunPtr HS_vkCmdBeginDebugUtilsLabelEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdBeginDebugUtilsLabelEXTUnsafe ::
               PFN_vkCmdBeginDebugUtilsLabelEXT -> HS_vkCmdBeginDebugUtilsLabelEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdBeginDebugUtilsLabelEXTSafe ::
               PFN_vkCmdBeginDebugUtilsLabelEXT -> HS_vkCmdBeginDebugUtilsLabelEXT

instance VulkanProc "vkCmdBeginDebugUtilsLabelEXT" where
    type VkProcType "vkCmdBeginDebugUtilsLabelEXT" =
         HS_vkCmdBeginDebugUtilsLabelEXT
    vkProcSymbol = _VkCmdBeginDebugUtilsLabelEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdBeginDebugUtilsLabelEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdBeginDebugUtilsLabelEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdEndDebugUtilsLabelEXT :: CString

pattern VkCmdEndDebugUtilsLabelEXT <-
        (is_VkCmdEndDebugUtilsLabelEXT -> True)
  where
    VkCmdEndDebugUtilsLabelEXT = _VkCmdEndDebugUtilsLabelEXT

{-# INLINE _VkCmdEndDebugUtilsLabelEXT #-}

_VkCmdEndDebugUtilsLabelEXT :: CString
_VkCmdEndDebugUtilsLabelEXT = Ptr "vkCmdEndDebugUtilsLabelEXT\NUL"#

{-# INLINE is_VkCmdEndDebugUtilsLabelEXT #-}

is_VkCmdEndDebugUtilsLabelEXT :: CString -> Bool
is_VkCmdEndDebugUtilsLabelEXT
  = (EQ ==) . cmpCStrings _VkCmdEndDebugUtilsLabelEXT

type VkCmdEndDebugUtilsLabelEXT = "vkCmdEndDebugUtilsLabelEXT"

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdEndDebugUtilsLabelEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdEndDebugUtilsLabelEXT vkCmdEndDebugUtilsLabelEXT registry at www.khronos.org>
type HS_vkCmdEndDebugUtilsLabelEXT = VkCommandBuffer -- ^ commandBuffer
                                                     -> IO ()

type PFN_vkCmdEndDebugUtilsLabelEXT =
     FunPtr HS_vkCmdEndDebugUtilsLabelEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdEndDebugUtilsLabelEXTUnsafe ::
               PFN_vkCmdEndDebugUtilsLabelEXT -> HS_vkCmdEndDebugUtilsLabelEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdEndDebugUtilsLabelEXTSafe ::
               PFN_vkCmdEndDebugUtilsLabelEXT -> HS_vkCmdEndDebugUtilsLabelEXT

instance VulkanProc "vkCmdEndDebugUtilsLabelEXT" where
    type VkProcType "vkCmdEndDebugUtilsLabelEXT" =
         HS_vkCmdEndDebugUtilsLabelEXT
    vkProcSymbol = _VkCmdEndDebugUtilsLabelEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdEndDebugUtilsLabelEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdEndDebugUtilsLabelEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdInsertDebugUtilsLabelEXT :: CString

pattern VkCmdInsertDebugUtilsLabelEXT <-
        (is_VkCmdInsertDebugUtilsLabelEXT -> True)
  where
    VkCmdInsertDebugUtilsLabelEXT = _VkCmdInsertDebugUtilsLabelEXT

{-# INLINE _VkCmdInsertDebugUtilsLabelEXT #-}

_VkCmdInsertDebugUtilsLabelEXT :: CString
_VkCmdInsertDebugUtilsLabelEXT
  = Ptr "vkCmdInsertDebugUtilsLabelEXT\NUL"#

{-# INLINE is_VkCmdInsertDebugUtilsLabelEXT #-}

is_VkCmdInsertDebugUtilsLabelEXT :: CString -> Bool
is_VkCmdInsertDebugUtilsLabelEXT
  = (EQ ==) . cmpCStrings _VkCmdInsertDebugUtilsLabelEXT

type VkCmdInsertDebugUtilsLabelEXT =
     "vkCmdInsertDebugUtilsLabelEXT"

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdInsertDebugUtilsLabelEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkDebugUtilsLabelEXT* pLabelInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdInsertDebugUtilsLabelEXT vkCmdInsertDebugUtilsLabelEXT registry at www.khronos.org>
type HS_vkCmdInsertDebugUtilsLabelEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr VkDebugUtilsLabelEXT -- ^ pLabelInfo
                                                 -> IO ()

type PFN_vkCmdInsertDebugUtilsLabelEXT =
     FunPtr HS_vkCmdInsertDebugUtilsLabelEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdInsertDebugUtilsLabelEXTUnsafe ::
               PFN_vkCmdInsertDebugUtilsLabelEXT ->
                 HS_vkCmdInsertDebugUtilsLabelEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdInsertDebugUtilsLabelEXTSafe ::
               PFN_vkCmdInsertDebugUtilsLabelEXT ->
                 HS_vkCmdInsertDebugUtilsLabelEXT

instance VulkanProc "vkCmdInsertDebugUtilsLabelEXT" where
    type VkProcType "vkCmdInsertDebugUtilsLabelEXT" =
         HS_vkCmdInsertDebugUtilsLabelEXT
    vkProcSymbol = _VkCmdInsertDebugUtilsLabelEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdInsertDebugUtilsLabelEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdInsertDebugUtilsLabelEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCreateDebugUtilsMessengerEXT :: CString

pattern VkCreateDebugUtilsMessengerEXT <-
        (is_VkCreateDebugUtilsMessengerEXT -> True)
  where
    VkCreateDebugUtilsMessengerEXT = _VkCreateDebugUtilsMessengerEXT

{-# INLINE _VkCreateDebugUtilsMessengerEXT #-}

_VkCreateDebugUtilsMessengerEXT :: CString
_VkCreateDebugUtilsMessengerEXT
  = Ptr "vkCreateDebugUtilsMessengerEXT\NUL"#

{-# INLINE is_VkCreateDebugUtilsMessengerEXT #-}

is_VkCreateDebugUtilsMessengerEXT :: CString -> Bool
is_VkCreateDebugUtilsMessengerEXT
  = (EQ ==) . cmpCStrings _VkCreateDebugUtilsMessengerEXT

type VkCreateDebugUtilsMessengerEXT =
     "vkCreateDebugUtilsMessengerEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkCreateDebugUtilsMessengerEXT
--   >     ( VkInstance instance
--   >     , const VkDebugUtilsMessengerCreateInfoEXT* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDebugUtilsMessengerEXT* pMessenger
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDebugUtilsMessengerEXT vkCreateDebugUtilsMessengerEXT registry at www.khronos.org>
type HS_vkCreateDebugUtilsMessengerEXT =
     VkInstance -- ^ instance
                ->
       Ptr VkDebugUtilsMessengerCreateInfoEXT -- ^ pCreateInfo
                                              ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   ->
           Ptr VkDebugUtilsMessengerEXT -- ^ pMessenger
                                        -> IO VkResult

type PFN_vkCreateDebugUtilsMessengerEXT =
     FunPtr HS_vkCreateDebugUtilsMessengerEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCreateDebugUtilsMessengerEXTUnsafe ::
               PFN_vkCreateDebugUtilsMessengerEXT ->
                 HS_vkCreateDebugUtilsMessengerEXT

foreign import ccall safe "dynamic"
               unwrapVkCreateDebugUtilsMessengerEXTSafe ::
               PFN_vkCreateDebugUtilsMessengerEXT ->
                 HS_vkCreateDebugUtilsMessengerEXT

instance VulkanProc "vkCreateDebugUtilsMessengerEXT" where
    type VkProcType "vkCreateDebugUtilsMessengerEXT" =
         HS_vkCreateDebugUtilsMessengerEXT
    vkProcSymbol = _VkCreateDebugUtilsMessengerEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateDebugUtilsMessengerEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateDebugUtilsMessengerEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkDestroyDebugUtilsMessengerEXT :: CString

pattern VkDestroyDebugUtilsMessengerEXT <-
        (is_VkDestroyDebugUtilsMessengerEXT -> True)
  where
    VkDestroyDebugUtilsMessengerEXT = _VkDestroyDebugUtilsMessengerEXT

{-# INLINE _VkDestroyDebugUtilsMessengerEXT #-}

_VkDestroyDebugUtilsMessengerEXT :: CString
_VkDestroyDebugUtilsMessengerEXT
  = Ptr "vkDestroyDebugUtilsMessengerEXT\NUL"#

{-# INLINE is_VkDestroyDebugUtilsMessengerEXT #-}

is_VkDestroyDebugUtilsMessengerEXT :: CString -> Bool
is_VkDestroyDebugUtilsMessengerEXT
  = (EQ ==) . cmpCStrings _VkDestroyDebugUtilsMessengerEXT

type VkDestroyDebugUtilsMessengerEXT =
     "vkDestroyDebugUtilsMessengerEXT"

-- | > void vkDestroyDebugUtilsMessengerEXT
--   >     ( VkInstance instance
--   >     , VkDebugUtilsMessengerEXT messenger
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroyDebugUtilsMessengerEXT vkDestroyDebugUtilsMessengerEXT registry at www.khronos.org>
type HS_vkDestroyDebugUtilsMessengerEXT =
     VkInstance -- ^ instance
                ->
       VkDebugUtilsMessengerEXT -- ^ messenger
                                -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                             -> IO ()

type PFN_vkDestroyDebugUtilsMessengerEXT =
     FunPtr HS_vkDestroyDebugUtilsMessengerEXT

foreign import ccall unsafe "dynamic"
               unwrapVkDestroyDebugUtilsMessengerEXTUnsafe ::
               PFN_vkDestroyDebugUtilsMessengerEXT ->
                 HS_vkDestroyDebugUtilsMessengerEXT

foreign import ccall safe "dynamic"
               unwrapVkDestroyDebugUtilsMessengerEXTSafe ::
               PFN_vkDestroyDebugUtilsMessengerEXT ->
                 HS_vkDestroyDebugUtilsMessengerEXT

instance VulkanProc "vkDestroyDebugUtilsMessengerEXT" where
    type VkProcType "vkDestroyDebugUtilsMessengerEXT" =
         HS_vkDestroyDebugUtilsMessengerEXT
    vkProcSymbol = _VkDestroyDebugUtilsMessengerEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkDestroyDebugUtilsMessengerEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkDestroyDebugUtilsMessengerEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkSubmitDebugUtilsMessageEXT :: CString

pattern VkSubmitDebugUtilsMessageEXT <-
        (is_VkSubmitDebugUtilsMessageEXT -> True)
  where
    VkSubmitDebugUtilsMessageEXT = _VkSubmitDebugUtilsMessageEXT

{-# INLINE _VkSubmitDebugUtilsMessageEXT #-}

_VkSubmitDebugUtilsMessageEXT :: CString
_VkSubmitDebugUtilsMessageEXT
  = Ptr "vkSubmitDebugUtilsMessageEXT\NUL"#

{-# INLINE is_VkSubmitDebugUtilsMessageEXT #-}

is_VkSubmitDebugUtilsMessageEXT :: CString -> Bool
is_VkSubmitDebugUtilsMessageEXT
  = (EQ ==) . cmpCStrings _VkSubmitDebugUtilsMessageEXT

type VkSubmitDebugUtilsMessageEXT = "vkSubmitDebugUtilsMessageEXT"

-- | > void vkSubmitDebugUtilsMessageEXT
--   >     ( VkInstance instance
--   >     , VkDebugUtilsMessageSeverityFlagBitsEXT messageSeverity
--   >     , VkDebugUtilsMessageTypeFlagsEXT messageTypes
--   >     , const VkDebugUtilsMessengerCallbackDataEXT* pCallbackData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkSubmitDebugUtilsMessageEXT vkSubmitDebugUtilsMessageEXT registry at www.khronos.org>
type HS_vkSubmitDebugUtilsMessageEXT =
     VkInstance -- ^ instance
                ->
       VkDebugUtilsMessageSeverityFlagBitsEXT -- ^ messageSeverity
                                              ->
         VkDebugUtilsMessageTypeFlagsEXT -- ^ messageTypes
                                         ->
           Ptr VkDebugUtilsMessengerCallbackDataEXT -- ^ pCallbackData
                                                    -> IO ()

type PFN_vkSubmitDebugUtilsMessageEXT =
     FunPtr HS_vkSubmitDebugUtilsMessageEXT

foreign import ccall unsafe "dynamic"
               unwrapVkSubmitDebugUtilsMessageEXTUnsafe ::
               PFN_vkSubmitDebugUtilsMessageEXT -> HS_vkSubmitDebugUtilsMessageEXT

foreign import ccall safe "dynamic"
               unwrapVkSubmitDebugUtilsMessageEXTSafe ::
               PFN_vkSubmitDebugUtilsMessageEXT -> HS_vkSubmitDebugUtilsMessageEXT

instance VulkanProc "vkSubmitDebugUtilsMessageEXT" where
    type VkProcType "vkSubmitDebugUtilsMessageEXT" =
         HS_vkSubmitDebugUtilsMessageEXT
    vkProcSymbol = _VkSubmitDebugUtilsMessageEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkSubmitDebugUtilsMessageEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkSubmitDebugUtilsMessageEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_DEBUG_UTILS_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_DEBUG_UTILS_SPEC_VERSION = 1

type VK_EXT_DEBUG_UTILS_SPEC_VERSION = 1

pattern VK_EXT_DEBUG_UTILS_EXTENSION_NAME :: CString

pattern VK_EXT_DEBUG_UTILS_EXTENSION_NAME <-
        (is_VK_EXT_DEBUG_UTILS_EXTENSION_NAME -> True)
  where
    VK_EXT_DEBUG_UTILS_EXTENSION_NAME
      = _VK_EXT_DEBUG_UTILS_EXTENSION_NAME

{-# INLINE _VK_EXT_DEBUG_UTILS_EXTENSION_NAME #-}

_VK_EXT_DEBUG_UTILS_EXTENSION_NAME :: CString
_VK_EXT_DEBUG_UTILS_EXTENSION_NAME = Ptr "VK_EXT_debug_utils\NUL"#

{-# INLINE is_VK_EXT_DEBUG_UTILS_EXTENSION_NAME #-}

is_VK_EXT_DEBUG_UTILS_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DEBUG_UTILS_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_DEBUG_UTILS_EXTENSION_NAME

type VK_EXT_DEBUG_UTILS_EXTENSION_NAME = "VK_EXT_debug_utils"

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT =
        VkStructureType 1000128000

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT =
        VkStructureType 1000128001

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT =
        VkStructureType 1000128002

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT =
        VkStructureType 1000128003

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT =
        VkStructureType 1000128004

-- | VkDebugUtilsMessengerEXT
pattern VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT :: VkObjectType

pattern VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT =
        VkObjectType 1000128000
