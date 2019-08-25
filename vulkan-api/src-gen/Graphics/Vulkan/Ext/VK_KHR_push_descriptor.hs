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
module Graphics.Vulkan.Ext.VK_KHR_push_descriptor
       (-- * Vulkan extension: @VK_KHR_push_descriptor@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jeffbolznv@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @81@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal, VkBool32(..), VkDeviceSize(..),
        VkFlags(..), VkSampleMask(..), VkPhysicalDeviceLimits,
        VkPhysicalDeviceProperties, VkPhysicalDeviceProperties2,
        VkPhysicalDevicePushDescriptorPropertiesKHR,
        VkPhysicalDeviceSparseProperties, VkPhysicalDeviceType(..),
        VkSampleCountBitmask(..), VkSampleCountFlagBits(),
        VkSampleCountFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VkCmdPushDescriptorSetKHR, pattern VkCmdPushDescriptorSetKHR,
        HS_vkCmdPushDescriptorSetKHR, PFN_vkCmdPushDescriptorSetKHR,
        VkDescriptorBindingBitmaskEXT(..),
        VkDescriptorPoolCreateBitmask(..), VkDescriptorType(..),
        VkDescriptorUpdateTemplateType(..),
        VkDescriptorBindingFlagBitsEXT(), VkDescriptorBindingFlagsEXT(),
        VkDescriptorPoolCreateFlagBits(), VkDescriptorPoolCreateFlags(),
        VkDescriptorSetLayoutCreateBitmask(..),
        VkDescriptorSetLayoutCreateFlagBits(),
        VkDescriptorSetLayoutCreateFlags(),
        VkDescriptorUpdateTemplateTypeKHR(..), VkImageAspectBitmask(..),
        VkImageCreateBitmask(..), VkImageLayout(..), VkImageTiling(..),
        VkImageType(..), VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(),
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
        VkPipelineViewportStateCreateFlagBits(..), VkBuffer, VkBufferView,
        VkBufferView_T(), VkBuffer_T(), VkCommandBuffer,
        VkCommandBuffer_T(), VkCommandPool, VkCommandPool_T(),
        VkDebugReportCallbackEXT, VkDebugReportCallbackEXT_T(),
        VkDebugUtilsMessengerEXT, VkDebugUtilsMessengerEXT_T(),
        VkDescriptorPool, VkDescriptorPool_T(), VkDescriptorSet,
        VkDescriptorSetLayout, VkDescriptorSetLayout_T(),
        VkDescriptorSet_T(), VkDescriptorUpdateTemplate,
        VkDescriptorUpdateTemplateKHR, VkDescriptorUpdateTemplateKHR_T(),
        VkDescriptorUpdateTemplate_T(), VkDevice, VkDeviceMemory,
        VkDeviceMemory_T(), VkDevice_T(), VkDisplayKHR, VkDisplayKHR_T(),
        VkDisplayModeKHR, VkDisplayModeKHR_T(), VkEvent, VkEvent_T(),
        VkFence, VkFence_T(), VkFramebuffer, VkFramebuffer_T(), VkImage,
        VkImageView, VkImageView_T(), VkImage_T(),
        VkIndirectCommandsLayoutNVX, VkIndirectCommandsLayoutNVX_T(),
        VkInstance, VkInstance_T(), VkObjectTableNVX, VkObjectTableNVX_T(),
        VkPhysicalDevice, VkPhysicalDevice_T(), VkPipeline,
        VkPipelineCache, VkPipelineCache_T(), VkPipelineLayout,
        VkPipelineLayout_T(), VkPipeline_T(), VkQueryPool, VkQueryPool_T(),
        VkQueue, VkQueue_T(), VkRenderPass, VkRenderPass_T(), VkSampler,
        VkSamplerYcbcrConversion, VkSamplerYcbcrConversionKHR,
        VkSamplerYcbcrConversionKHR_T(), VkSamplerYcbcrConversion_T(),
        VkSampler_T(), VkSemaphore, VkSemaphore_T(), VkShaderModule,
        VkShaderModule_T(), VkSurfaceKHR, VkSurfaceKHR_T(), VkSwapchainKHR,
        VkSwapchainKHR_T(), VkValidationCacheEXT, VkValidationCacheEXT_T(),
        VkDescriptorBufferInfo, VkDescriptorImageInfo,
        VkDescriptorPoolCreateInfo, VkDescriptorPoolSize,
        VkDescriptorSetAllocateInfo, VkDescriptorSetLayoutBinding,
        VkDescriptorSetLayoutBindingFlagsCreateInfoEXT,
        VkDescriptorSetLayoutCreateInfo, VkDescriptorSetLayoutSupport,
        VkDescriptorSetLayoutSupportKHR,
        VkDescriptorSetVariableDescriptorCountAllocateInfoEXT,
        VkDescriptorSetVariableDescriptorCountLayoutSupportEXT,
        VkDescriptorUpdateTemplateCreateInfo,
        VkDescriptorUpdateTemplateCreateInfoKHR,
        VkDescriptorUpdateTemplateEntry,
        VkDescriptorUpdateTemplateEntryKHR, VkWriteDescriptorSet,
        VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION,
        pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION,
        VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME,
        pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR,
        pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR,
        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkCmdPushDescriptorSetWithTemplateKHR,
        pattern VkCmdPushDescriptorSetWithTemplateKHR,
        HS_vkCmdPushDescriptorSetWithTemplateKHR,
        PFN_vkCmdPushDescriptorSetWithTemplateKHR,
        pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR)
       where
import           GHC.Ptr                                         (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                    (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.Descriptor
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.Pipeline
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Descriptor
import           Graphics.Vulkan.Types.Struct.PhysicalDevice     (VkPhysicalDeviceLimits,
                                                                  VkPhysicalDeviceProperties,
                                                                  VkPhysicalDeviceProperties2,
                                                                  VkPhysicalDevicePushDescriptorPropertiesKHR,
                                                                  VkPhysicalDeviceSparseProperties)
import           Graphics.Vulkan.Types.Struct.WriteDescriptorSet

pattern VkCmdPushDescriptorSetKHR :: CString

pattern VkCmdPushDescriptorSetKHR <-
        (is_VkCmdPushDescriptorSetKHR -> True)
  where
    VkCmdPushDescriptorSetKHR = _VkCmdPushDescriptorSetKHR

{-# INLINE _VkCmdPushDescriptorSetKHR #-}

_VkCmdPushDescriptorSetKHR :: CString
_VkCmdPushDescriptorSetKHR = Ptr "vkCmdPushDescriptorSetKHR\NUL"#

{-# INLINE is_VkCmdPushDescriptorSetKHR #-}

is_VkCmdPushDescriptorSetKHR :: CString -> Bool
is_VkCmdPushDescriptorSetKHR
  = (EQ ==) . cmpCStrings _VkCmdPushDescriptorSetKHR

type VkCmdPushDescriptorSetKHR = "vkCmdPushDescriptorSetKHR"

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdPushDescriptorSetKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineBindPoint pipelineBindPoint
--   >     , VkPipelineLayout layout
--   >     , uint32_t set
--   >     , uint32_t descriptorWriteCount
--   >     , const VkWriteDescriptorSet* pDescriptorWrites
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdPushDescriptorSetKHR vkCmdPushDescriptorSetKHR registry at www.khronos.org>
type HS_vkCmdPushDescriptorSetKHR =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkPipelineBindPoint -- ^ pipelineBindPoint
                           ->
         VkPipelineLayout -- ^ layout
                          ->
           Word32 -- ^ set
                  -> Word32 -- ^ descriptorWriteCount
                            -> Ptr VkWriteDescriptorSet -- ^ pDescriptorWrites
                                                        -> IO ()

type PFN_vkCmdPushDescriptorSetKHR =
     FunPtr HS_vkCmdPushDescriptorSetKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCmdPushDescriptorSetKHRUnsafe ::
               PFN_vkCmdPushDescriptorSetKHR -> HS_vkCmdPushDescriptorSetKHR

foreign import ccall safe "dynamic"
               unwrapVkCmdPushDescriptorSetKHRSafe ::
               PFN_vkCmdPushDescriptorSetKHR -> HS_vkCmdPushDescriptorSetKHR

instance VulkanProc "vkCmdPushDescriptorSetKHR" where
    type VkProcType "vkCmdPushDescriptorSetKHR" =
         HS_vkCmdPushDescriptorSetKHR
    vkProcSymbol = _VkCmdPushDescriptorSetKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkCmdPushDescriptorSetKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkCmdPushDescriptorSetKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION = 2

type VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION = 2

pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME :: CString

pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME <-
        (is_VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME -> True)
  where
    VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
      = _VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME

{-# INLINE _VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME #-}

_VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME :: CString
_VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
  = Ptr "VK_KHR_push_descriptor\NUL"#

{-# INLINE is_VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME #-}

is_VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME

type VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME =
     "VK_KHR_push_descriptor"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
        = VkStructureType 1000080000

-- | Descriptors are pushed via flink:vkCmdPushDescriptorSetKHR
--
--   bitpos = @0@
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR ::
        VkDescriptorSetLayoutCreateBitmask a

pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR =
        VkDescriptorSetLayoutCreateBitmask 1

pattern VkCmdPushDescriptorSetWithTemplateKHR :: CString

pattern VkCmdPushDescriptorSetWithTemplateKHR <-
        (is_VkCmdPushDescriptorSetWithTemplateKHR -> True)
  where
    VkCmdPushDescriptorSetWithTemplateKHR
      = _VkCmdPushDescriptorSetWithTemplateKHR

{-# INLINE _VkCmdPushDescriptorSetWithTemplateKHR #-}

_VkCmdPushDescriptorSetWithTemplateKHR :: CString
_VkCmdPushDescriptorSetWithTemplateKHR
  = Ptr "vkCmdPushDescriptorSetWithTemplateKHR\NUL"#

{-# INLINE is_VkCmdPushDescriptorSetWithTemplateKHR #-}

is_VkCmdPushDescriptorSetWithTemplateKHR :: CString -> Bool
is_VkCmdPushDescriptorSetWithTemplateKHR
  = (EQ ==) . cmpCStrings _VkCmdPushDescriptorSetWithTemplateKHR

type VkCmdPushDescriptorSetWithTemplateKHR =
     "vkCmdPushDescriptorSetWithTemplateKHR"

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdPushDescriptorSetWithTemplateKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , VkPipelineLayout layout
--   >     , uint32_t set
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdPushDescriptorSetWithTemplateKHR vkCmdPushDescriptorSetWithTemplateKHR registry at www.khronos.org>
type HS_vkCmdPushDescriptorSetWithTemplateKHR =
     VkCommandBuffer -- ^ commandBuffer
                     ->
       VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                  ->
         VkPipelineLayout -- ^ layout
                          -> Word32 -- ^ set
                                    -> Ptr Void -- ^ pData
                                                -> IO ()

type PFN_vkCmdPushDescriptorSetWithTemplateKHR =
     FunPtr HS_vkCmdPushDescriptorSetWithTemplateKHR

foreign import ccall unsafe "dynamic"
               unwrapVkCmdPushDescriptorSetWithTemplateKHRUnsafe ::
               PFN_vkCmdPushDescriptorSetWithTemplateKHR ->
                 HS_vkCmdPushDescriptorSetWithTemplateKHR

foreign import ccall safe "dynamic"
               unwrapVkCmdPushDescriptorSetWithTemplateKHRSafe ::
               PFN_vkCmdPushDescriptorSetWithTemplateKHR ->
                 HS_vkCmdPushDescriptorSetWithTemplateKHR

instance VulkanProc "vkCmdPushDescriptorSetWithTemplateKHR" where
    type VkProcType "vkCmdPushDescriptorSetWithTemplateKHR" =
         HS_vkCmdPushDescriptorSetWithTemplateKHR
    vkProcSymbol = _VkCmdPushDescriptorSetWithTemplateKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkCmdPushDescriptorSetWithTemplateKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkCmdPushDescriptorSetWithTemplateKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

-- | Create descriptor update template for pushed descriptor updates
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR ::
        VkDescriptorUpdateTemplateType

pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR =
        VkDescriptorUpdateTemplateType 1
