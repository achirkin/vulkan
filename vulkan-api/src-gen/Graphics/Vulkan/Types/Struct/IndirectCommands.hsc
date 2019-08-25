#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.IndirectCommands
       (VkIndirectCommandsLayoutCreateInfoNVX,
        VkIndirectCommandsLayoutTokenNVX, VkIndirectCommandsTokenNVX)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes             (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.IndirectCommands (VkIndirectCommandsLayoutUsageFlagsNVX,
                                                              VkIndirectCommandsTokenTypeNVX)
import           Graphics.Vulkan.Types.Enum.Pipeline         (VkPipelineBindPoint)
import           Graphics.Vulkan.Types.Enum.StructureType    (VkStructureType)
import           Graphics.Vulkan.Types.Handles               (VkBuffer)

-- | > typedef struct VkIndirectCommandsLayoutCreateInfoNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkPipelineBindPoint                      pipelineBindPoint;
--   >     VkIndirectCommandsLayoutUsageFlagsNVX    flags;
--   >     uint32_t                                 tokenCount;
--   >     const VkIndirectCommandsLayoutTokenNVX*  pTokens;
--   > } VkIndirectCommandsLayoutCreateInfoNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkIndirectCommandsLayoutCreateInfoNVX VkIndirectCommandsLayoutCreateInfoNVX registry at www.khronos.org>
type VkIndirectCommandsLayoutCreateInfoNVX =
     VkStruct VkIndirectCommandsLayoutCreateInfoNVX' -- ' closing tick for hsc2hs

data VkIndirectCommandsLayoutCreateInfoNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal VkIndirectCommandsLayoutCreateInfoNVX where
    type StructRep VkIndirectCommandsLayoutCreateInfoNVX =
         'StructMeta "VkIndirectCommandsLayoutCreateInfoNVX" -- ' closing tick for hsc2hs
           VkIndirectCommandsLayoutCreateInfoNVX
           #{size VkIndirectCommandsLayoutCreateInfoNVX}
           #{alignment VkIndirectCommandsLayoutCreateInfoNVX}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkIndirectCommandsLayoutCreateInfoNVX, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkIndirectCommandsLayoutCreateInfoNVX, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineBindPoint" VkPipelineBindPoint 'False
                #{offset VkIndirectCommandsLayoutCreateInfoNVX, pipelineBindPoint}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkIndirectCommandsLayoutUsageFlagsNVX 'False
                #{offset VkIndirectCommandsLayoutCreateInfoNVX, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "tokenCount" Word32 'False 
                                                    #{offset VkIndirectCommandsLayoutCreateInfoNVX, tokenCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pTokens" (Ptr VkIndirectCommandsLayoutTokenNVX) 'False
                #{offset VkIndirectCommandsLayoutCreateInfoNVX, pTokens}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkIndirectCommandsLayoutTokenNVX {
--   >     VkIndirectCommandsTokenTypeNVX      tokenType;
--   >     uint32_t                         bindingUnit;
--   >     uint32_t                         dynamicCount;
--   >     uint32_t                         divisor;
--   > } VkIndirectCommandsLayoutTokenNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkIndirectCommandsLayoutTokenNVX VkIndirectCommandsLayoutTokenNVX registry at www.khronos.org>
type VkIndirectCommandsLayoutTokenNVX =
     VkStruct VkIndirectCommandsLayoutTokenNVX' -- ' closing tick for hsc2hs

data VkIndirectCommandsLayoutTokenNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal VkIndirectCommandsLayoutTokenNVX where
    type StructRep VkIndirectCommandsLayoutTokenNVX =
         'StructMeta "VkIndirectCommandsLayoutTokenNVX" -- ' closing tick for hsc2hs
           VkIndirectCommandsLayoutTokenNVX
           #{size VkIndirectCommandsLayoutTokenNVX}
           #{alignment VkIndirectCommandsLayoutTokenNVX}
           '[('FieldMeta "tokenType" VkIndirectCommandsTokenTypeNVX 'False -- ' closing tick for hsc2hs
                #{offset VkIndirectCommandsLayoutTokenNVX, tokenType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bindingUnit" Word32 'False 
                                                     #{offset VkIndirectCommandsLayoutTokenNVX, bindingUnit}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dynamicCount" Word32 'False 
                                                      #{offset VkIndirectCommandsLayoutTokenNVX, dynamicCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "divisor" Word32 'False 
                                                 #{offset VkIndirectCommandsLayoutTokenNVX, divisor}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkIndirectCommandsTokenNVX {
--   >     VkIndirectCommandsTokenTypeNVX      tokenType;
--   >     VkBuffer                         buffer;
--   >     VkDeviceSize                     offset;
--   > } VkIndirectCommandsTokenNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkIndirectCommandsTokenNVX VkIndirectCommandsTokenNVX registry at www.khronos.org>
type VkIndirectCommandsTokenNVX =
     VkStruct VkIndirectCommandsTokenNVX' -- ' closing tick for hsc2hs

data VkIndirectCommandsTokenNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal VkIndirectCommandsTokenNVX where
    type StructRep VkIndirectCommandsTokenNVX =
         'StructMeta "VkIndirectCommandsTokenNVX" VkIndirectCommandsTokenNVX -- ' closing tick for hsc2hs
           #{size VkIndirectCommandsTokenNVX}
           #{alignment VkIndirectCommandsTokenNVX}
           '[('FieldMeta "tokenType" VkIndirectCommandsTokenTypeNVX 'False -- ' closing tick for hsc2hs
                #{offset VkIndirectCommandsTokenNVX, tokenType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "buffer" VkBuffer 'False 
                                                  #{offset VkIndirectCommandsTokenNVX, buffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "offset" VkDeviceSize 'False 
                                                      #{offset VkIndirectCommandsTokenNVX, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
