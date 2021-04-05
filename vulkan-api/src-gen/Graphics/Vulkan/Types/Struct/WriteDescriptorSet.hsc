#include "vulkan/vulkan.h"

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.WriteDescriptorSet
       (VkWriteDescriptorSet,
#ifdef VK_ENABLE_BETA_EXTENSIONS
       VkWriteDescriptorSetAccelerationStructureNV,
#endif
        VkWriteDescriptorSetInlineUniformBlockEXT)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.Descriptor             (VkDescriptorType)
import Graphics.Vulkan.Types.Enum.StructureType          (VkStructureType)
import Graphics.Vulkan.Types.Handles                     (VkBufferView,
                                                          VkDescriptorSet)
import Graphics.Vulkan.Types.Struct.Descriptor           (VkDescriptorBufferInfo,
                                                          VkDescriptorImageInfo)
#ifdef VK_ENABLE_BETA_EXTENSIONS
import Graphics.Vulkan.Types.Struct.EnableBetaExtensions (VkWriteDescriptorSetAccelerationStructureKHR)
#endif

-- | > typedef struct VkWriteDescriptorSet {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDescriptorSet        dstSet;
--   >     uint32_t               dstBinding;
--   >     uint32_t               dstArrayElement;
--   >     uint32_t               descriptorCount;
--   >     VkDescriptorType       descriptorType;
--   >     const VkDescriptorImageInfo* pImageInfo;
--   >     const VkDescriptorBufferInfo* pBufferInfo;
--   >     const VkBufferView*    pTexelBufferView;
--   > } VkWriteDescriptorSet;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkWriteDescriptorSet VkWriteDescriptorSet registry at www.khronos.org>
type VkWriteDescriptorSet = VkStruct VkWriteDescriptorSet' -- ' closing tick for hsc2hs

data VkWriteDescriptorSet' -- ' closing tick for hsc2hs

instance VulkanMarshal VkWriteDescriptorSet where
    type StructRep VkWriteDescriptorSet =
         'StructMeta "VkWriteDescriptorSet" VkWriteDescriptorSet  -- ' closing tick for hsc2hs
                                                                 #{size VkWriteDescriptorSet}
           #{alignment VkWriteDescriptorSet}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkWriteDescriptorSet, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkWriteDescriptorSet, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstSet" VkDescriptorSet 'False
                                                         #{offset VkWriteDescriptorSet, dstSet}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstBinding" Word32 'False
                                                    #{offset VkWriteDescriptorSet, dstBinding}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstArrayElement" Word32 'False
                                                         #{offset VkWriteDescriptorSet, dstArrayElement}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorCount" Word32 'False
                                                         #{offset VkWriteDescriptorSet, descriptorCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorType" VkDescriptorType 'False
                                                                  #{offset VkWriteDescriptorSet, descriptorType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pImageInfo" (Ptr VkDescriptorImageInfo) 'False
                #{offset VkWriteDescriptorSet, pImageInfo}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pBufferInfo" (Ptr VkDescriptorBufferInfo) 'False
                #{offset VkWriteDescriptorSet, pBufferInfo}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pTexelBufferView" (Ptr VkBufferView) 'False
                                                                      #{offset VkWriteDescriptorSet, pTexelBufferView}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

#ifdef VK_ENABLE_BETA_EXTENSIONS
-- | Alias for `VkWriteDescriptorSetAccelerationStructureKHR`
type VkWriteDescriptorSetAccelerationStructureNV =
     VkWriteDescriptorSetAccelerationStructureKHR
#endif

-- | > typedef struct VkWriteDescriptorSetInlineUniformBlockEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     uint32_t               dataSize;
--   >     const void* pData;
--   > } VkWriteDescriptorSetInlineUniformBlockEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkWriteDescriptorSetInlineUniformBlockEXT VkWriteDescriptorSetInlineUniformBlockEXT registry at www.khronos.org>
type VkWriteDescriptorSetInlineUniformBlockEXT =
     VkStruct VkWriteDescriptorSetInlineUniformBlockEXT' -- ' closing tick for hsc2hs

data VkWriteDescriptorSetInlineUniformBlockEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkWriteDescriptorSetInlineUniformBlockEXT
         where
    type StructRep VkWriteDescriptorSetInlineUniformBlockEXT =
         'StructMeta "VkWriteDescriptorSetInlineUniformBlockEXT" -- ' closing tick for hsc2hs
           VkWriteDescriptorSetInlineUniformBlockEXT
           #{size VkWriteDescriptorSetInlineUniformBlockEXT}
           #{alignment VkWriteDescriptorSetInlineUniformBlockEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkWriteDescriptorSetInlineUniformBlockEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkWriteDescriptorSetInlineUniformBlockEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dataSize" Word32 'False
                                                  #{offset VkWriteDescriptorSetInlineUniformBlockEXT, dataSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pData" (Ptr Void) 'False
                                                   #{offset VkWriteDescriptorSetInlineUniformBlockEXT, pData}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkWriteDescriptorSet] -- ' closing tick for hsc2hs
