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
module Graphics.Vulkan.Ext.VK_KHR_create_renderpass2
       (-- * Vulkan extension: @VK_KHR_create_renderpass2@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Tobias Hector @tobias@
        --
        -- type: @device@
        --
        -- Extension number: @110@
        --
        -- Required extensions: 'VK_KHR_multiview', 'VK_KHR_maintenance2'.
        --

        -- ** Required extensions: 'VK_KHR_multiview', 'VK_KHR_maintenance2'.
        VkAttachmentDescription2KHR, VkAttachmentReference2KHR,
        VkRenderPassCreateInfo2KHR, VkSubpassBeginInfoKHR,
        VkSubpassDependency2KHR, VkSubpassDescription2KHR,
        VkSubpassEndInfoKHR, VkCreateRenderPass2KHR,
        pattern VkCreateRenderPass2KHR, HS_vkCreateRenderPass2KHR,
        PFN_vkCreateRenderPass2KHR, VkCmdBeginRenderPass2KHR,
        pattern VkCmdBeginRenderPass2KHR, HS_vkCmdBeginRenderPass2KHR,
        PFN_vkCmdBeginRenderPass2KHR, VkCmdNextSubpass2KHR,
        pattern VkCmdNextSubpass2KHR, HS_vkCmdNextSubpass2KHR,
        PFN_vkCmdNextSubpass2KHR, VkCmdEndRenderPass2KHR,
        pattern VkCmdEndRenderPass2KHR, HS_vkCmdEndRenderPass2KHR,
        PFN_vkCmdEndRenderPass2KHR, module Graphics.Vulkan.Marshal,
        AHardwareBuffer(), ANativeWindow(), CAMetalLayer(), VkBool32(..),
        VkDeviceAddress(..), VkDeviceSize(..), VkFlags(..),
        VkSampleMask(..), VkAccessBitmask(..), VkAccessFlagBits(),
        VkAccessFlags(), VkAttachmentDescriptionBitmask(..),
        VkAttachmentLoadOp(..), VkAttachmentStoreOp(..),
        VkAttachmentDescriptionFlagBits(), VkAttachmentDescriptionFlags(),
        VkDependencyBitmask(..), VkDependencyFlagBits(),
        VkDependencyFlags(), VkFormat(..), VkFormatFeatureBitmask(..),
        VkFormatFeatureFlagBits(), VkFormatFeatureFlags(),
        VkImageAspectBitmask(..), VkImageCreateBitmask(..),
        VkImageLayout(..), VkImageTiling(..), VkImageType(..),
        VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(),
        VkImageViewCreateBitmask(..), VkImageViewCreateFlagBits(),
        VkImageViewCreateFlags(), VkInternalAllocationType(..),
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
        VkPipelineStageFlags(), VkRenderPassCreateBitmask(..),
        VkRenderPassCreateFlagBits(), VkRenderPassCreateFlags(),
        VkResult(..), VkSampleCountBitmask(..), VkSampleCountFlagBits(),
        VkSampleCountFlags(), VkStructureType(..), VkSubpassContents(..),
        VkSubpassDescriptionBitmask(..), VkSubpassDescriptionFlagBits(),
        VkSubpassDescriptionFlags(), VkSystemAllocationScope(..),
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
        VkAllocationCallbacks, VkAttachmentDescription,
        VkAttachmentDescription2, VkAttachmentDescriptionStencilLayout,
        VkAttachmentDescriptionStencilLayoutKHR, VkAttachmentReference,
        VkAttachmentReference2, VkAttachmentReferenceStencilLayout,
        VkAttachmentReferenceStencilLayoutKHR,
        VkAttachmentSampleLocationsEXT, VkClearAttachment,
        VkClearColorValue, VkClearDepthStencilValue, VkClearRect,
        VkClearValue, VkExtent2D, VkExtent3D, VkOffset2D, VkOffset3D,
        VkRect2D, VkRectLayerKHR, VkRenderPassAttachmentBeginInfo,
        VkRenderPassAttachmentBeginInfoKHR, VkRenderPassBeginInfo,
        VkRenderPassCreateInfo, VkRenderPassCreateInfo2,
        VkRenderPassFragmentDensityMapCreateInfoEXT,
        VkRenderPassInputAttachmentAspectCreateInfo,
        VkRenderPassInputAttachmentAspectCreateInfoKHR,
        VkRenderPassMultiviewCreateInfo,
        VkRenderPassMultiviewCreateInfoKHR,
        VkRenderPassSampleLocationsBeginInfoEXT,
        VkRenderPassTransformBeginInfoQCOM, VkSubpassBeginInfo,
        VkSubpassDependency, VkSubpassDependency2, VkSubpassDescription,
        VkSubpassDescription2, VkSubpassDescriptionDepthStencilResolve,
        VkSubpassDescriptionDepthStencilResolveKHR, VkSubpassEndInfo,
        VkSubpassSampleLocationsEXT,
        VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION,
        pattern VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION,
        VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME,
        pattern VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR,
        pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR,
        pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR,
        pattern VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR,
        pattern VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR)
       where
import GHC.Ptr                                           (Ptr (..))
import Graphics.Vulkan.Core_1_2                          (pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2,
                                                          pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2,
                                                          pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2,
                                                          pattern VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO,
                                                          pattern VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2,
                                                          pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2,
                                                          pattern VK_STRUCTURE_TYPE_SUBPASS_END_INFO)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                      (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Enum.AccessFlags
import Graphics.Vulkan.Types.Enum.Attachment
import Graphics.Vulkan.Types.Enum.DependencyFlags
import Graphics.Vulkan.Types.Enum.Format
import Graphics.Vulkan.Types.Enum.Image
import Graphics.Vulkan.Types.Enum.InternalAllocationType
import Graphics.Vulkan.Types.Enum.Pipeline
import Graphics.Vulkan.Types.Enum.RenderPassCreateFlags
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Enum.Subpass
import Graphics.Vulkan.Types.Enum.SystemAllocationScope
import Graphics.Vulkan.Types.Funcpointers
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.AllocationCallbacks
import Graphics.Vulkan.Types.Struct.Attachment
import Graphics.Vulkan.Types.Struct.Clear
import Graphics.Vulkan.Types.Struct.Extent
import Graphics.Vulkan.Types.Struct.Offset
import Graphics.Vulkan.Types.Struct.Rect
import Graphics.Vulkan.Types.Struct.RenderPass
import Graphics.Vulkan.Types.Struct.Subpass

pattern VkCreateRenderPass2KHR :: CString

pattern VkCreateRenderPass2KHR <-
        (is_VkCreateRenderPass2KHR -> True)
  where
    VkCreateRenderPass2KHR = _VkCreateRenderPass2KHR

{-# INLINE _VkCreateRenderPass2KHR #-}

_VkCreateRenderPass2KHR :: CString
_VkCreateRenderPass2KHR = Ptr "vkCreateRenderPass2KHR\NUL"#

{-# INLINE is_VkCreateRenderPass2KHR #-}

is_VkCreateRenderPass2KHR :: CString -> Bool
is_VkCreateRenderPass2KHR
  = (EQ ==) . cmpCStrings _VkCreateRenderPass2KHR

type VkCreateRenderPass2KHR = "vkCreateRenderPass2KHR"

-- | This is an alias for `vkCreateRenderPass2`.
--
--   Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateRenderPass2KHR
--   >     ( VkDevice device
--   >     , const VkRenderPassCreateInfo2* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkRenderPass* pRenderPass
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCreateRenderPass2KHR vkCreateRenderPass2KHR registry at www.khronos.org>
type HS_vkCreateRenderPass2KHR =
     VkDevice -- ^ device
              ->
       Ptr VkRenderPassCreateInfo2 -- ^ pCreateInfo
                                   ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkRenderPass -- ^ pRenderPass
                                                       -> IO VkResult

type PFN_vkCreateRenderPass2KHR = FunPtr HS_vkCreateRenderPass2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkCreateRenderPass2KHRUnsafe ::
               PFN_vkCreateRenderPass2KHR -> HS_vkCreateRenderPass2KHR

foreign import ccall safe "dynamic"
               unwrapVkCreateRenderPass2KHRSafe ::
               PFN_vkCreateRenderPass2KHR -> HS_vkCreateRenderPass2KHR

instance VulkanProc "vkCreateRenderPass2KHR" where
    type VkProcType "vkCreateRenderPass2KHR" =
         HS_vkCreateRenderPass2KHR
    vkProcSymbol = _VkCreateRenderPass2KHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCreateRenderPass2KHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCreateRenderPass2KHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdBeginRenderPass2KHR :: CString

pattern VkCmdBeginRenderPass2KHR <-
        (is_VkCmdBeginRenderPass2KHR -> True)
  where
    VkCmdBeginRenderPass2KHR = _VkCmdBeginRenderPass2KHR

{-# INLINE _VkCmdBeginRenderPass2KHR #-}

_VkCmdBeginRenderPass2KHR :: CString
_VkCmdBeginRenderPass2KHR = Ptr "vkCmdBeginRenderPass2KHR\NUL"#

{-# INLINE is_VkCmdBeginRenderPass2KHR #-}

is_VkCmdBeginRenderPass2KHR :: CString -> Bool
is_VkCmdBeginRenderPass2KHR
  = (EQ ==) . cmpCStrings _VkCmdBeginRenderPass2KHR

type VkCmdBeginRenderPass2KHR = "vkCmdBeginRenderPass2KHR"

-- | This is an alias for `vkCmdBeginRenderPass2`.
--
--   Queues: 'graphics'.
--
--   Renderpass: @outside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdBeginRenderPass2KHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkRenderPassBeginInfo*      pRenderPassBegin
--   >     , const VkSubpassBeginInfo*      pSubpassBeginInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass2KHR vkCmdBeginRenderPass2KHR registry at www.khronos.org>
type HS_vkCmdBeginRenderPass2KHR =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Ptr VkRenderPassBeginInfo -- ^ pRenderPassBegin
                                 -> Ptr VkSubpassBeginInfo -- ^ pSubpassBeginInfo
                                                           -> IO ()

type PFN_vkCmdBeginRenderPass2KHR =
     FunPtr HS_vkCmdBeginRenderPass2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkCmdBeginRenderPass2KHRUnsafe ::
               PFN_vkCmdBeginRenderPass2KHR -> HS_vkCmdBeginRenderPass2KHR

foreign import ccall safe "dynamic"
               unwrapVkCmdBeginRenderPass2KHRSafe ::
               PFN_vkCmdBeginRenderPass2KHR -> HS_vkCmdBeginRenderPass2KHR

instance VulkanProc "vkCmdBeginRenderPass2KHR" where
    type VkProcType "vkCmdBeginRenderPass2KHR" =
         HS_vkCmdBeginRenderPass2KHR
    vkProcSymbol = _VkCmdBeginRenderPass2KHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdBeginRenderPass2KHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdBeginRenderPass2KHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdNextSubpass2KHR :: CString

pattern VkCmdNextSubpass2KHR <- (is_VkCmdNextSubpass2KHR -> True)
  where
    VkCmdNextSubpass2KHR = _VkCmdNextSubpass2KHR

{-# INLINE _VkCmdNextSubpass2KHR #-}

_VkCmdNextSubpass2KHR :: CString
_VkCmdNextSubpass2KHR = Ptr "vkCmdNextSubpass2KHR\NUL"#

{-# INLINE is_VkCmdNextSubpass2KHR #-}

is_VkCmdNextSubpass2KHR :: CString -> Bool
is_VkCmdNextSubpass2KHR
  = (EQ ==) . cmpCStrings _VkCmdNextSubpass2KHR

type VkCmdNextSubpass2KHR = "vkCmdNextSubpass2KHR"

-- | This is an alias for `vkCmdNextSubpass2`.
--
--   Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdNextSubpass2KHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkSubpassBeginInfo*      pSubpassBeginInfo
--   >     , const VkSubpassEndInfo*        pSubpassEndInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdNextSubpass2KHR vkCmdNextSubpass2KHR registry at www.khronos.org>
type HS_vkCmdNextSubpass2KHR =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       Ptr VkSubpassBeginInfo -- ^ pSubpassBeginInfo
                              -> Ptr VkSubpassEndInfo -- ^ pSubpassEndInfo
                                                      -> IO ()

type PFN_vkCmdNextSubpass2KHR = FunPtr HS_vkCmdNextSubpass2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkCmdNextSubpass2KHRUnsafe ::
               PFN_vkCmdNextSubpass2KHR -> HS_vkCmdNextSubpass2KHR

foreign import ccall safe "dynamic" unwrapVkCmdNextSubpass2KHRSafe
               :: PFN_vkCmdNextSubpass2KHR -> HS_vkCmdNextSubpass2KHR

instance VulkanProc "vkCmdNextSubpass2KHR" where
    type VkProcType "vkCmdNextSubpass2KHR" = HS_vkCmdNextSubpass2KHR
    vkProcSymbol = _VkCmdNextSubpass2KHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdNextSubpass2KHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdNextSubpass2KHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdEndRenderPass2KHR :: CString

pattern VkCmdEndRenderPass2KHR <-
        (is_VkCmdEndRenderPass2KHR -> True)
  where
    VkCmdEndRenderPass2KHR = _VkCmdEndRenderPass2KHR

{-# INLINE _VkCmdEndRenderPass2KHR #-}

_VkCmdEndRenderPass2KHR :: CString
_VkCmdEndRenderPass2KHR = Ptr "vkCmdEndRenderPass2KHR\NUL"#

{-# INLINE is_VkCmdEndRenderPass2KHR #-}

is_VkCmdEndRenderPass2KHR :: CString -> Bool
is_VkCmdEndRenderPass2KHR
  = (EQ ==) . cmpCStrings _VkCmdEndRenderPass2KHR

type VkCmdEndRenderPass2KHR = "vkCmdEndRenderPass2KHR"

-- | This is an alias for `vkCmdEndRenderPass2`.
--
--   Queues: 'graphics'.
--
--   Renderpass: @inside@
--
--   Pipeline: @graphics@
--
--   > void vkCmdEndRenderPass2KHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkSubpassEndInfo*        pSubpassEndInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdEndRenderPass2KHR vkCmdEndRenderPass2KHR registry at www.khronos.org>
type HS_vkCmdEndRenderPass2KHR =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr VkSubpassEndInfo -- ^ pSubpassEndInfo
                                             -> IO ()

type PFN_vkCmdEndRenderPass2KHR = FunPtr HS_vkCmdEndRenderPass2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkCmdEndRenderPass2KHRUnsafe ::
               PFN_vkCmdEndRenderPass2KHR -> HS_vkCmdEndRenderPass2KHR

foreign import ccall safe "dynamic"
               unwrapVkCmdEndRenderPass2KHRSafe ::
               PFN_vkCmdEndRenderPass2KHR -> HS_vkCmdEndRenderPass2KHR

instance VulkanProc "vkCmdEndRenderPass2KHR" where
    type VkProcType "vkCmdEndRenderPass2KHR" =
         HS_vkCmdEndRenderPass2KHR
    vkProcSymbol = _VkCmdEndRenderPass2KHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdEndRenderPass2KHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdEndRenderPass2KHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION = 1

type VK_KHR_CREATE_RENDERPASS_2_SPEC_VERSION = 1

pattern VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME :: CString

pattern VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME <-
        (is_VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME -> True)
  where
    VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME
      = _VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME

{-# INLINE _VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME #-}

_VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME :: CString
_VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME
  = Ptr "VK_KHR_create_renderpass2\NUL"#

{-# INLINE is_VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME #-}

is_VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME

type VK_KHR_CREATE_RENDERPASS_2_EXTENSION_NAME =
     "VK_KHR_create_renderpass2"

pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR =
        VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2

pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR =
        VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2

pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR =
        VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2

pattern VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR =
        VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2

pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR =
        VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2

pattern VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR =
        VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO

pattern VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR =
        VK_STRUCTURE_TYPE_SUBPASS_END_INFO
