#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.DrawInd
       (VkDrawIndexedIndirectCommand, VkDrawIndirectCommand) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal

-- | > typedef struct VkDrawIndexedIndirectCommand {
--   >     uint32_t               indexCount;
--   >     uint32_t               instanceCount;
--   >     uint32_t               firstIndex;
--   >     int32_t                vertexOffset;
--   >     uint32_t               firstInstance;
--   > } VkDrawIndexedIndirectCommand;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDrawIndexedIndirectCommand VkDrawIndexedIndirectCommand registry at www.khronos.org>
type VkDrawIndexedIndirectCommand =
     VkStruct VkDrawIndexedIndirectCommand' -- ' closing tick for hsc2hs

data VkDrawIndexedIndirectCommand' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDrawIndexedIndirectCommand where
    type StructRep VkDrawIndexedIndirectCommand =
         'StructMeta "VkDrawIndexedIndirectCommand" -- ' closing tick for hsc2hs
           VkDrawIndexedIndirectCommand
           #{size VkDrawIndexedIndirectCommand}
           #{alignment VkDrawIndexedIndirectCommand}
           '[('FieldMeta "indexCount" Word32 'False  -- ' closing tick for hsc2hs
                                                    #{offset VkDrawIndexedIndirectCommand, indexCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "instanceCount" Word32 'False 
                                                       #{offset VkDrawIndexedIndirectCommand, instanceCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "firstIndex" Word32 'False 
                                                    #{offset VkDrawIndexedIndirectCommand, firstIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexOffset" Int32 'False 
                                                     #{offset VkDrawIndexedIndirectCommand, vertexOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "firstInstance" Word32 'False 
                                                       #{offset VkDrawIndexedIndirectCommand, firstInstance}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDrawIndirectCommand {
--   >     uint32_t               vertexCount;
--   >     uint32_t               instanceCount;
--   >     uint32_t               firstVertex;
--   >     uint32_t               firstInstance;
--   > } VkDrawIndirectCommand;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDrawIndirectCommand VkDrawIndirectCommand registry at www.khronos.org>
type VkDrawIndirectCommand = VkStruct VkDrawIndirectCommand' -- ' closing tick for hsc2hs

data VkDrawIndirectCommand' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDrawIndirectCommand where
    type StructRep VkDrawIndirectCommand =
         'StructMeta "VkDrawIndirectCommand" VkDrawIndirectCommand  -- ' closing tick for hsc2hs
                                                                   #{size VkDrawIndirectCommand}
           #{alignment VkDrawIndirectCommand}
           '[('FieldMeta "vertexCount" Word32 'False  -- ' closing tick for hsc2hs
                                                     #{offset VkDrawIndirectCommand, vertexCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "instanceCount" Word32 'False 
                                                       #{offset VkDrawIndirectCommand, instanceCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "firstVertex" Word32 'False 
                                                     #{offset VkDrawIndirectCommand, firstVertex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "firstInstance" Word32 'False 
                                                       #{offset VkDrawIndirectCommand, firstInstance}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
