#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.ObjectTable
       (VkObjectTableCreateInfoNVX, VkObjectTableDescriptorSetEntryNVX,
        VkObjectTableEntryNVX, VkObjectTableIndexBufferEntryNVX,
        VkObjectTablePipelineEntryNVX, VkObjectTablePushConstantEntryNVX,
        VkObjectTableVertexBufferEntryNVX)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.IndexType     (VkIndexType)
import           Graphics.Vulkan.Types.Enum.Object        (VkObjectEntryTypeNVX, VkObjectEntryUsageFlagsNVX)
import           Graphics.Vulkan.Types.Enum.Shader        (VkShaderStageFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkBuffer,
                                                           VkDescriptorSet,
                                                           VkPipeline,
                                                           VkPipelineLayout)

-- | > typedef struct VkObjectTableCreateInfoNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                                          objectCount;
--   >     const VkObjectEntryTypeNVX*       pObjectEntryTypes;
--   >     const uint32_t*                   pObjectEntryCounts;
--   >     const VkObjectEntryUsageFlagsNVX* pObjectEntryUsageFlags;
--   >     uint32_t maxUniformBuffersPerDescriptor;
--   >     uint32_t maxStorageBuffersPerDescriptor;
--   >     uint32_t maxStorageImagesPerDescriptor;
--   >     uint32_t maxSampledImagesPerDescriptor;
--   >     uint32_t maxPipelineLayouts;
--   > } VkObjectTableCreateInfoNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkObjectTableCreateInfoNVX VkObjectTableCreateInfoNVX registry at www.khronos.org>
type VkObjectTableCreateInfoNVX =
     VkStruct VkObjectTableCreateInfoNVX' -- ' closing tick for hsc2hs

data VkObjectTableCreateInfoNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal VkObjectTableCreateInfoNVX where
    type StructRep VkObjectTableCreateInfoNVX =
         'StructMeta "VkObjectTableCreateInfoNVX" VkObjectTableCreateInfoNVX -- ' closing tick for hsc2hs
           #{size VkObjectTableCreateInfoNVX}
           #{alignment VkObjectTableCreateInfoNVX}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkObjectTableCreateInfoNVX, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkObjectTableCreateInfoNVX, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "objectCount" Word32 'False 
                                                     #{offset VkObjectTableCreateInfoNVX, objectCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pObjectEntryTypes" (Ptr VkObjectEntryTypeNVX) 'False
                #{offset VkObjectTableCreateInfoNVX, pObjectEntryTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pObjectEntryCounts" (Ptr Word32) 'False 
                                                                  #{offset VkObjectTableCreateInfoNVX, pObjectEntryCounts}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pObjectEntryUsageFlags" -- ' closing tick for hsc2hs
                (Ptr VkObjectEntryUsageFlagsNVX)
                'False -- ' closing tick for hsc2hs
                #{offset VkObjectTableCreateInfoNVX, pObjectEntryUsageFlags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxUniformBuffersPerDescriptor" Word32 'False
                #{offset VkObjectTableCreateInfoNVX, maxUniformBuffersPerDescriptor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxStorageBuffersPerDescriptor" Word32 'False
                #{offset VkObjectTableCreateInfoNVX, maxStorageBuffersPerDescriptor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxStorageImagesPerDescriptor" Word32 'False 
                                                                       #{offset VkObjectTableCreateInfoNVX, maxStorageImagesPerDescriptor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxSampledImagesPerDescriptor" Word32 'False 
                                                                       #{offset VkObjectTableCreateInfoNVX, maxSampledImagesPerDescriptor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPipelineLayouts" Word32 'False 
                                                            #{offset VkObjectTableCreateInfoNVX, maxPipelineLayouts}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkObjectTableDescriptorSetEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkPipelineLayout             pipelineLayout;
--   >     VkDescriptorSet              descriptorSet;
--   > } VkObjectTableDescriptorSetEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkObjectTableDescriptorSetEntryNVX VkObjectTableDescriptorSetEntryNVX registry at www.khronos.org>
type VkObjectTableDescriptorSetEntryNVX =
     VkStruct VkObjectTableDescriptorSetEntryNVX' -- ' closing tick for hsc2hs

data VkObjectTableDescriptorSetEntryNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal VkObjectTableDescriptorSetEntryNVX where
    type StructRep VkObjectTableDescriptorSetEntryNVX =
         'StructMeta "VkObjectTableDescriptorSetEntryNVX" -- ' closing tick for hsc2hs
           VkObjectTableDescriptorSetEntryNVX
           #{size VkObjectTableDescriptorSetEntryNVX}
           #{alignment VkObjectTableDescriptorSetEntryNVX}
           '[('FieldMeta "type" VkObjectEntryTypeNVX 'False  -- ' closing tick for hsc2hs
                                                            #{offset VkObjectTableDescriptorSetEntryNVX, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkObjectEntryUsageFlagsNVX 'False 
                                                                   #{offset VkObjectTableDescriptorSetEntryNVX, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineLayout" VkPipelineLayout 'False 
                                                                  #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorSet" VkDescriptorSet 'False 
                                                                #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkObjectTableEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   > } VkObjectTableEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkObjectTableEntryNVX VkObjectTableEntryNVX registry at www.khronos.org>
type VkObjectTableEntryNVX = VkStruct VkObjectTableEntryNVX' -- ' closing tick for hsc2hs

data VkObjectTableEntryNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal VkObjectTableEntryNVX where
    type StructRep VkObjectTableEntryNVX =
         'StructMeta "VkObjectTableEntryNVX" VkObjectTableEntryNVX  -- ' closing tick for hsc2hs
                                                                   #{size VkObjectTableEntryNVX}
           #{alignment VkObjectTableEntryNVX}
           '[('FieldMeta "type" VkObjectEntryTypeNVX 'False  -- ' closing tick for hsc2hs
                                                            #{offset VkObjectTableEntryNVX, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkObjectEntryUsageFlagsNVX 'False 
                                                                   #{offset VkObjectTableEntryNVX, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkObjectTableIndexBufferEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkBuffer                     buffer;
--   >     VkIndexType                  indexType;
--   > } VkObjectTableIndexBufferEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkObjectTableIndexBufferEntryNVX VkObjectTableIndexBufferEntryNVX registry at www.khronos.org>
type VkObjectTableIndexBufferEntryNVX =
     VkStruct VkObjectTableIndexBufferEntryNVX' -- ' closing tick for hsc2hs

data VkObjectTableIndexBufferEntryNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal VkObjectTableIndexBufferEntryNVX where
    type StructRep VkObjectTableIndexBufferEntryNVX =
         'StructMeta "VkObjectTableIndexBufferEntryNVX" -- ' closing tick for hsc2hs
           VkObjectTableIndexBufferEntryNVX
           #{size VkObjectTableIndexBufferEntryNVX}
           #{alignment VkObjectTableIndexBufferEntryNVX}
           '[('FieldMeta "type" VkObjectEntryTypeNVX 'False  -- ' closing tick for hsc2hs
                                                            #{offset VkObjectTableIndexBufferEntryNVX, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkObjectEntryUsageFlagsNVX 'False 
                                                                   #{offset VkObjectTableIndexBufferEntryNVX, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "buffer" VkBuffer 'False 
                                                  #{offset VkObjectTableIndexBufferEntryNVX, buffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "indexType" VkIndexType 'False 
                                                        #{offset VkObjectTableIndexBufferEntryNVX, indexType}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkObjectTablePipelineEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkPipeline                   pipeline;
--   > } VkObjectTablePipelineEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkObjectTablePipelineEntryNVX VkObjectTablePipelineEntryNVX registry at www.khronos.org>
type VkObjectTablePipelineEntryNVX =
     VkStruct VkObjectTablePipelineEntryNVX' -- ' closing tick for hsc2hs

data VkObjectTablePipelineEntryNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal VkObjectTablePipelineEntryNVX where
    type StructRep VkObjectTablePipelineEntryNVX =
         'StructMeta "VkObjectTablePipelineEntryNVX" -- ' closing tick for hsc2hs
           VkObjectTablePipelineEntryNVX
           #{size VkObjectTablePipelineEntryNVX}
           #{alignment VkObjectTablePipelineEntryNVX}
           '[('FieldMeta "type" VkObjectEntryTypeNVX 'False  -- ' closing tick for hsc2hs
                                                            #{offset VkObjectTablePipelineEntryNVX, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkObjectEntryUsageFlagsNVX 'False 
                                                                   #{offset VkObjectTablePipelineEntryNVX, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipeline" VkPipeline 'False 
                                                      #{offset VkObjectTablePipelineEntryNVX, pipeline}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkObjectTablePushConstantEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkPipelineLayout             pipelineLayout;
--   >     VkShaderStageFlags           stageFlags;
--   > } VkObjectTablePushConstantEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkObjectTablePushConstantEntryNVX VkObjectTablePushConstantEntryNVX registry at www.khronos.org>
type VkObjectTablePushConstantEntryNVX =
     VkStruct VkObjectTablePushConstantEntryNVX' -- ' closing tick for hsc2hs

data VkObjectTablePushConstantEntryNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal VkObjectTablePushConstantEntryNVX where
    type StructRep VkObjectTablePushConstantEntryNVX =
         'StructMeta "VkObjectTablePushConstantEntryNVX" -- ' closing tick for hsc2hs
           VkObjectTablePushConstantEntryNVX
           #{size VkObjectTablePushConstantEntryNVX}
           #{alignment VkObjectTablePushConstantEntryNVX}
           '[('FieldMeta "type" VkObjectEntryTypeNVX 'False  -- ' closing tick for hsc2hs
                                                            #{offset VkObjectTablePushConstantEntryNVX, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkObjectEntryUsageFlagsNVX 'False 
                                                                   #{offset VkObjectTablePushConstantEntryNVX, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineLayout" VkPipelineLayout 'False 
                                                                  #{offset VkObjectTablePushConstantEntryNVX, pipelineLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stageFlags" VkShaderStageFlags 'False 
                                                                #{offset VkObjectTablePushConstantEntryNVX, stageFlags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkObjectTableVertexBufferEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkBuffer                     buffer;
--   > } VkObjectTableVertexBufferEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkObjectTableVertexBufferEntryNVX VkObjectTableVertexBufferEntryNVX registry at www.khronos.org>
type VkObjectTableVertexBufferEntryNVX =
     VkStruct VkObjectTableVertexBufferEntryNVX' -- ' closing tick for hsc2hs

data VkObjectTableVertexBufferEntryNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal VkObjectTableVertexBufferEntryNVX where
    type StructRep VkObjectTableVertexBufferEntryNVX =
         'StructMeta "VkObjectTableVertexBufferEntryNVX" -- ' closing tick for hsc2hs
           VkObjectTableVertexBufferEntryNVX
           #{size VkObjectTableVertexBufferEntryNVX}
           #{alignment VkObjectTableVertexBufferEntryNVX}
           '[('FieldMeta "type" VkObjectEntryTypeNVX 'False  -- ' closing tick for hsc2hs
                                                            #{offset VkObjectTableVertexBufferEntryNVX, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkObjectEntryUsageFlagsNVX 'False 
                                                                   #{offset VkObjectTableVertexBufferEntryNVX, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "buffer" VkBuffer 'False 
                                                  #{offset VkObjectTableVertexBufferEntryNVX, buffer}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
