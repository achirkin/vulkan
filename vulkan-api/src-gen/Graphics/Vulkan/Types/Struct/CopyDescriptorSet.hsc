#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.CopyDescriptorSet
       (VkCopyDescriptorSet) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkDescriptorSet)

-- | > typedef struct VkCopyDescriptorSet {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDescriptorSet        srcSet;
--   >     uint32_t               srcBinding;
--   >     uint32_t               srcArrayElement;
--   >     VkDescriptorSet        dstSet;
--   >     uint32_t               dstBinding;
--   >     uint32_t               dstArrayElement;
--   >     uint32_t               descriptorCount;
--   > } VkCopyDescriptorSet;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCopyDescriptorSet VkCopyDescriptorSet registry at www.khronos.org>
type VkCopyDescriptorSet = VkStruct VkCopyDescriptorSet' -- ' closing tick for hsc2hs

data VkCopyDescriptorSet' -- ' closing tick for hsc2hs

instance VulkanMarshal VkCopyDescriptorSet where
    type StructRep VkCopyDescriptorSet =
         'StructMeta "VkCopyDescriptorSet" VkCopyDescriptorSet  -- ' closing tick for hsc2hs
                                                               #{size VkCopyDescriptorSet}
           #{alignment VkCopyDescriptorSet}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkCopyDescriptorSet, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkCopyDescriptorSet, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcSet" VkDescriptorSet 'False 
                                                         #{offset VkCopyDescriptorSet, srcSet}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcBinding" Word32 'False 
                                                    #{offset VkCopyDescriptorSet, srcBinding}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcArrayElement" Word32 'False 
                                                         #{offset VkCopyDescriptorSet, srcArrayElement}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstSet" VkDescriptorSet 'False 
                                                         #{offset VkCopyDescriptorSet, dstSet}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstBinding" Word32 'False 
                                                    #{offset VkCopyDescriptorSet, dstBinding}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstArrayElement" Word32 'False 
                                                         #{offset VkCopyDescriptorSet, dstArrayElement}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorCount" Word32 'False 
                                                         #{offset VkCopyDescriptorSet, descriptorCount}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
