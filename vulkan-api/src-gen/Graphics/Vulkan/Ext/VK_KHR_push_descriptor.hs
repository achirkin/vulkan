{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_HADDOCK not-home#-}
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
        -- contact: @Jeff Bolz @jbolz@
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
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDevicePushDescriptorPropertiesKHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties,
        module Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        VkCmdPushDescriptorSetKHR, pattern VkCmdPushDescriptorSetKHR,
        HS_vkCmdPushDescriptorSetKHR, PFN_vkCmdPushDescriptorSetKHR,
        unwrapVkCmdPushDescriptorSetKHR, vkCmdPushDescriptorSetKHR,
        vkCmdPushDescriptorSetKHRSafe,
        module Graphics.Vulkan.Types.Enum.VkDescriptorType,
        module Graphics.Vulkan.Types.Enum.VkImageLayout,
        module Graphics.Vulkan.Types.Enum.VkPipelineBindPoint,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkDescriptorBufferInfo,
        module Graphics.Vulkan.Types.Struct.VkDescriptorImageInfo,
        module Graphics.Vulkan.Types.Struct.VkWriteDescriptorSet,
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
        unwrapVkCmdPushDescriptorSetWithTemplateKHR,
        vkCmdPushDescriptorSetWithTemplateKHR,
        vkCmdPushDescriptorSetWithTemplateKHRSafe,
        pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR)
       where
import           GHC.Ptr
                                                                                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
                                                                                           (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkDescriptorSetLayoutCreateFlags
                                                                                           (VkDescriptorSetLayoutCreateBitmask (..),
                                                                                           VkDescriptorSetLayoutCreateFlagBits)
import           Graphics.Vulkan.Types.Enum.VkDescriptorType
import           Graphics.Vulkan.Types.Enum.VkDescriptorUpdateTemplateType
                                                                                           (VkDescriptorUpdateTemplateType (..))
import           Graphics.Vulkan.Types.Enum.VkImageLayout
import           Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.VkPipelineBindPoint
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkDescriptorBufferInfo
import           Graphics.Vulkan.Types.Struct.VkDescriptorImageInfo
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2
import           Graphics.Vulkan.Types.Struct.VkPhysicalDevicePushDescriptorPropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties
import           Graphics.Vulkan.Types.Struct.VkWriteDescriptorSet

pattern VkCmdPushDescriptorSetKHR :: CString

pattern VkCmdPushDescriptorSetKHR <-
        (is_VkCmdPushDescriptorSetKHR -> True)
  where VkCmdPushDescriptorSetKHR = _VkCmdPushDescriptorSetKHR

{-# INLINE _VkCmdPushDescriptorSetKHR #-}

_VkCmdPushDescriptorSetKHR :: CString
_VkCmdPushDescriptorSetKHR = Ptr "vkCmdPushDescriptorSetKHR\NUL"#

{-# INLINE is_VkCmdPushDescriptorSetKHR #-}

is_VkCmdPushDescriptorSetKHR :: CString -> Bool
is_VkCmdPushDescriptorSetKHR
  = (EQ ==) . cmpCStrings _VkCmdPushDescriptorSetKHR

type VkCmdPushDescriptorSetKHR = "vkCmdPushDescriptorSetKHR"

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdPushDescriptorSetKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineBindPoint pipelineBindPoint
--   >     , VkPipelineLayout layout
--   >     , uint32_t set
--   >     , uint32_t descriptorWriteCount
--   >     , const VkWriteDescriptorSet* pDescriptorWrites
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdPushDescriptorSetKHR.html vkCmdPushDescriptorSetKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCmdPushDescriptorSetKHR"
               vkCmdPushDescriptorSetKHR ::
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

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdPushDescriptorSetKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineBindPoint pipelineBindPoint
--   >     , VkPipelineLayout layout
--   >     , uint32_t set
--   >     , uint32_t descriptorWriteCount
--   >     , const VkWriteDescriptorSet* pDescriptorWrites
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdPushDescriptorSetKHR.html vkCmdPushDescriptorSetKHR registry at www.khronos.org>
foreign import ccall safe "vkCmdPushDescriptorSetKHR"
               vkCmdPushDescriptorSetKHRSafe ::
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

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdPushDescriptorSetKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkPipelineBindPoint pipelineBindPoint
--   >     , VkPipelineLayout layout
--   >     , uint32_t set
--   >     , uint32_t descriptorWriteCount
--   >     , const VkWriteDescriptorSet* pDescriptorWrites
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdPushDescriptorSetKHR.html vkCmdPushDescriptorSetKHR registry at www.khronos.org>
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

foreign import ccall "dynamic" unwrapVkCmdPushDescriptorSetKHR ::
               PFN_vkCmdPushDescriptorSetKHR -> HS_vkCmdPushDescriptorSetKHR

instance VulkanProc "vkCmdPushDescriptorSetKHR" where
        type VkProcType "vkCmdPushDescriptorSetKHR" =
             HS_vkCmdPushDescriptorSetKHR
        vkProcSymbol = _VkCmdPushDescriptorSetKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdPushDescriptorSetKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION = 2

type VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION = 2

pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME :: CString

pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME <-
        (is_VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME -> True)
  where VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
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
        VkDescriptorSetLayoutCreateFlagBits

pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR =
        VkDescriptorSetLayoutCreateFlagBits 1

pattern VkCmdPushDescriptorSetWithTemplateKHR :: CString

pattern VkCmdPushDescriptorSetWithTemplateKHR <-
        (is_VkCmdPushDescriptorSetWithTemplateKHR -> True)
  where VkCmdPushDescriptorSetWithTemplateKHR
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

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdPushDescriptorSetWithTemplateKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , VkPipelineLayout layout
--   >     , uint32_t set
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdPushDescriptorSetWithTemplateKHR.html vkCmdPushDescriptorSetWithTemplateKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCmdPushDescriptorSetWithTemplateKHR"
               vkCmdPushDescriptorSetWithTemplateKHR ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                            ->
                   VkPipelineLayout -- ^ layout
                                    -> Word32 -- ^ set
                                              -> Ptr Void -- ^ pData
                                                          -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdPushDescriptorSetWithTemplateKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , VkPipelineLayout layout
--   >     , uint32_t set
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdPushDescriptorSetWithTemplateKHR.html vkCmdPushDescriptorSetWithTemplateKHR registry at www.khronos.org>
foreign import ccall safe "vkCmdPushDescriptorSetWithTemplateKHR"
               vkCmdPushDescriptorSetWithTemplateKHRSafe ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkDescriptorUpdateTemplate -- ^ descriptorUpdateTemplate
                                            ->
                   VkPipelineLayout -- ^ layout
                                    -> Word32 -- ^ set
                                              -> Ptr Void -- ^ pData
                                                          -> IO ()

-- | queues: 'graphics', 'compute'.
--
--   renderpass: @both@
--
--   > () vkCmdPushDescriptorSetWithTemplateKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkDescriptorUpdateTemplate descriptorUpdateTemplate
--   >     , VkPipelineLayout layout
--   >     , uint32_t set
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkCmdPushDescriptorSetWithTemplateKHR.html vkCmdPushDescriptorSetWithTemplateKHR registry at www.khronos.org>
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

foreign import ccall "dynamic"
               unwrapVkCmdPushDescriptorSetWithTemplateKHR ::
               PFN_vkCmdPushDescriptorSetWithTemplateKHR ->
                 HS_vkCmdPushDescriptorSetWithTemplateKHR

instance VulkanProc "vkCmdPushDescriptorSetWithTemplateKHR" where
        type VkProcType "vkCmdPushDescriptorSetWithTemplateKHR" =
             HS_vkCmdPushDescriptorSetWithTemplateKHR
        vkProcSymbol = _VkCmdPushDescriptorSetWithTemplateKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCmdPushDescriptorSetWithTemplateKHR

        {-# INLINE unwrapVkProcPtr #-}

-- | Create descriptor update template for pushed descriptor updates
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR ::
        VkDescriptorUpdateTemplateType

pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR =
        VkDescriptorUpdateTemplateType 1
