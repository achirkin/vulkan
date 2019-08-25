#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Cmd
       (VkCmdProcessCommandsInfoNVX, VkCmdReserveSpaceForCommandsInfoNVX)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes               (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.StructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Handles                 (VkBuffer,
                                                                VkCommandBuffer,
                                                                VkIndirectCommandsLayoutNVX,
                                                                VkObjectTableNVX)
import           Graphics.Vulkan.Types.Struct.IndirectCommands (VkIndirectCommandsTokenNVX)

-- | > typedef struct VkCmdProcessCommandsInfoNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkObjectTableNVX                                         objectTable;
--   >     VkIndirectCommandsLayoutNVX                              indirectCommandsLayout;
--   >     uint32_t                                                 indirectCommandsTokenCount;
--   >     const VkIndirectCommandsTokenNVX*       pIndirectCommandsTokens;
--   >     uint32_t                                                 maxSequencesCount;
--   >     VkCommandBuffer                          targetCommandBuffer;
--   >     VkBuffer                                 sequencesCountBuffer;
--   >     VkDeviceSize                             sequencesCountOffset;
--   >     VkBuffer                                 sequencesIndexBuffer;
--   >     VkDeviceSize                             sequencesIndexOffset;
--   > } VkCmdProcessCommandsInfoNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCmdProcessCommandsInfoNVX VkCmdProcessCommandsInfoNVX registry at www.khronos.org>
type VkCmdProcessCommandsInfoNVX =
     VkStruct VkCmdProcessCommandsInfoNVX' -- ' closing tick for hsc2hs

data VkCmdProcessCommandsInfoNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal VkCmdProcessCommandsInfoNVX where
    type StructRep VkCmdProcessCommandsInfoNVX =
         'StructMeta "VkCmdProcessCommandsInfoNVX" -- ' closing tick for hsc2hs
           VkCmdProcessCommandsInfoNVX
           #{size VkCmdProcessCommandsInfoNVX}
           #{alignment VkCmdProcessCommandsInfoNVX}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkCmdProcessCommandsInfoNVX, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkCmdProcessCommandsInfoNVX, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "objectTable" VkObjectTableNVX 'False 
                                                               #{offset VkCmdProcessCommandsInfoNVX, objectTable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "indirectCommandsLayout" VkIndirectCommandsLayoutNVX -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "indirectCommandsTokenCount" Word32 'False 
                                                                    #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsTokenCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pIndirectCommandsTokens" -- ' closing tick for hsc2hs
                (Ptr VkIndirectCommandsTokenNVX)
                'False -- ' closing tick for hsc2hs
                #{offset VkCmdProcessCommandsInfoNVX, pIndirectCommandsTokens}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxSequencesCount" Word32 'False 
                                                           #{offset VkCmdProcessCommandsInfoNVX, maxSequencesCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "targetCommandBuffer" VkCommandBuffer 'True 
                                                                     #{offset VkCmdProcessCommandsInfoNVX, targetCommandBuffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sequencesCountBuffer" VkBuffer 'True 
                                                               #{offset VkCmdProcessCommandsInfoNVX, sequencesCountBuffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sequencesCountOffset" VkDeviceSize 'True 
                                                                   #{offset VkCmdProcessCommandsInfoNVX, sequencesCountOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sequencesIndexBuffer" VkBuffer 'True 
                                                               #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexBuffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sequencesIndexOffset" VkDeviceSize 'True 
                                                                   #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkCmdReserveSpaceForCommandsInfoNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkObjectTableNVX                                         objectTable;
--   >     VkIndirectCommandsLayoutNVX                              indirectCommandsLayout;
--   >     uint32_t                                                 maxSequencesCount;
--   > } VkCmdReserveSpaceForCommandsInfoNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCmdReserveSpaceForCommandsInfoNVX VkCmdReserveSpaceForCommandsInfoNVX registry at www.khronos.org>
type VkCmdReserveSpaceForCommandsInfoNVX =
     VkStruct VkCmdReserveSpaceForCommandsInfoNVX' -- ' closing tick for hsc2hs

data VkCmdReserveSpaceForCommandsInfoNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal VkCmdReserveSpaceForCommandsInfoNVX where
    type StructRep VkCmdReserveSpaceForCommandsInfoNVX =
         'StructMeta "VkCmdReserveSpaceForCommandsInfoNVX" -- ' closing tick for hsc2hs
           VkCmdReserveSpaceForCommandsInfoNVX
           #{size VkCmdReserveSpaceForCommandsInfoNVX}
           #{alignment VkCmdReserveSpaceForCommandsInfoNVX}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkCmdReserveSpaceForCommandsInfoNVX, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkCmdReserveSpaceForCommandsInfoNVX, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "objectTable" VkObjectTableNVX 'False 
                                                               #{offset VkCmdReserveSpaceForCommandsInfoNVX, objectTable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "indirectCommandsLayout" VkIndirectCommandsLayoutNVX -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkCmdReserveSpaceForCommandsInfoNVX, indirectCommandsLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxSequencesCount" Word32 'False 
                                                           #{offset VkCmdReserveSpaceForCommandsInfoNVX, maxSequencesCount}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
