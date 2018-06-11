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
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.PhysicalDevice,
        module Graphics.Vulkan.Types.Enum.PhysicalDeviceType,
        module Graphics.Vulkan.Types.Enum.SampleCountFlags,
        module Graphics.Vulkan.Types.Enum.StructureType,
        -- > #include "vk_platform.h"
        VkCmdPushDescriptorSetKHR, pattern VkCmdPushDescriptorSetKHR,
        HS_vkCmdPushDescriptorSetKHR, PFN_vkCmdPushDescriptorSetKHR,
        module Graphics.Vulkan.Types.Enum.Descriptor,
        module Graphics.Vulkan.Types.Enum.Image,
        module Graphics.Vulkan.Types.Enum.Pipeline,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.Descriptor,
        module Graphics.Vulkan.Types.Struct.WriteDescriptorSet,
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
import           Graphics.Vulkan.Types.Struct.PhysicalDevice
import           Graphics.Vulkan.Types.Struct.WriteDescriptorSet

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
