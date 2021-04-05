#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.GeneratedCommands
       (VkGeneratedCommandsInfoNV,
        VkGeneratedCommandsMemoryRequirementsInfoNV)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.BaseTypes               (VkDeviceSize)
import Graphics.Vulkan.Types.Enum.Pipeline           (VkPipelineBindPoint)
import Graphics.Vulkan.Types.Enum.StructureType      (VkStructureType)
import Graphics.Vulkan.Types.Handles                 (VkBuffer,
                                                      VkIndirectCommandsLayoutNV,
                                                      VkPipeline)
import Graphics.Vulkan.Types.Struct.IndirectCommands (VkIndirectCommandsStreamNV)

-- | > typedef struct VkGeneratedCommandsInfoNV {
--   >     VkStructureType sType;
--   >     const void*                        pNext;
--   >     VkPipelineBindPoint                pipelineBindPoint;
--   >     VkPipeline                         pipeline;
--   >     VkIndirectCommandsLayoutNV         indirectCommandsLayout;
--   >     uint32_t                           streamCount;
--   >     const VkIndirectCommandsStreamNV*  pStreams;
--   >     uint32_t                           sequencesCount;
--   >     VkBuffer                           preprocessBuffer;
--   >     VkDeviceSize                       preprocessOffset;
--   >     VkDeviceSize                       preprocessSize;
--   >     VkBuffer           sequencesCountBuffer;
--   >     VkDeviceSize       sequencesCountOffset;
--   >     VkBuffer           sequencesIndexBuffer;
--   >     VkDeviceSize       sequencesIndexOffset;
--   > } VkGeneratedCommandsInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkGeneratedCommandsInfoNV VkGeneratedCommandsInfoNV registry at www.khronos.org>
type VkGeneratedCommandsInfoNV =
     VkStruct VkGeneratedCommandsInfoNV' -- ' closing tick for hsc2hs

data VkGeneratedCommandsInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkGeneratedCommandsInfoNV where
    type StructRep VkGeneratedCommandsInfoNV =
         'StructMeta "VkGeneratedCommandsInfoNV" VkGeneratedCommandsInfoNV -- ' closing tick for hsc2hs
           #{size VkGeneratedCommandsInfoNV}
           #{alignment VkGeneratedCommandsInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkGeneratedCommandsInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkGeneratedCommandsInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineBindPoint" VkPipelineBindPoint 'False
                #{offset VkGeneratedCommandsInfoNV, pipelineBindPoint}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipeline" VkPipeline 'False 
                                                      #{offset VkGeneratedCommandsInfoNV, pipeline}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "indirectCommandsLayout" VkIndirectCommandsLayoutNV -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkGeneratedCommandsInfoNV, indirectCommandsLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "streamCount" Word32 'False 
                                                     #{offset VkGeneratedCommandsInfoNV, streamCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pStreams" (Ptr VkIndirectCommandsStreamNV) 'False
                #{offset VkGeneratedCommandsInfoNV, pStreams}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sequencesCount" Word32 'False 
                                                        #{offset VkGeneratedCommandsInfoNV, sequencesCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "preprocessBuffer" VkBuffer 'False 
                                                            #{offset VkGeneratedCommandsInfoNV, preprocessBuffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "preprocessOffset" VkDeviceSize 'False 
                                                                #{offset VkGeneratedCommandsInfoNV, preprocessOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "preprocessSize" VkDeviceSize 'False 
                                                              #{offset VkGeneratedCommandsInfoNV, preprocessSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sequencesCountBuffer" VkBuffer 'True 
                                                               #{offset VkGeneratedCommandsInfoNV, sequencesCountBuffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sequencesCountOffset" VkDeviceSize 'True 
                                                                   #{offset VkGeneratedCommandsInfoNV, sequencesCountOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sequencesIndexBuffer" VkBuffer 'True 
                                                               #{offset VkGeneratedCommandsInfoNV, sequencesIndexBuffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sequencesIndexOffset" VkDeviceSize 'True 
                                                                   #{offset VkGeneratedCommandsInfoNV, sequencesIndexOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkGeneratedCommandsMemoryRequirementsInfoNV {
--   >     VkStructureType sType;
--   >     const void*                 pNext;
--   >     VkPipelineBindPoint         pipelineBindPoint;
--   >     VkPipeline                  pipeline;
--   >     VkIndirectCommandsLayoutNV  indirectCommandsLayout;
--   >     uint32_t                    maxSequencesCount;
--   > } VkGeneratedCommandsMemoryRequirementsInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkGeneratedCommandsMemoryRequirementsInfoNV VkGeneratedCommandsMemoryRequirementsInfoNV registry at www.khronos.org>
type VkGeneratedCommandsMemoryRequirementsInfoNV =
     VkStruct VkGeneratedCommandsMemoryRequirementsInfoNV' -- ' closing tick for hsc2hs

data VkGeneratedCommandsMemoryRequirementsInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkGeneratedCommandsMemoryRequirementsInfoNV
         where
    type StructRep VkGeneratedCommandsMemoryRequirementsInfoNV =
         'StructMeta "VkGeneratedCommandsMemoryRequirementsInfoNV" -- ' closing tick for hsc2hs
           VkGeneratedCommandsMemoryRequirementsInfoNV
           #{size VkGeneratedCommandsMemoryRequirementsInfoNV}
           #{alignment VkGeneratedCommandsMemoryRequirementsInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkGeneratedCommandsMemoryRequirementsInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkGeneratedCommandsMemoryRequirementsInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineBindPoint" VkPipelineBindPoint 'False
                #{offset VkGeneratedCommandsMemoryRequirementsInfoNV, pipelineBindPoint}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipeline" VkPipeline 'False 
                                                      #{offset VkGeneratedCommandsMemoryRequirementsInfoNV, pipeline}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "indirectCommandsLayout" VkIndirectCommandsLayoutNV -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkGeneratedCommandsMemoryRequirementsInfoNV, indirectCommandsLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxSequencesCount" Word32 'False 
                                                           #{offset VkGeneratedCommandsMemoryRequirementsInfoNV, maxSequencesCount}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
