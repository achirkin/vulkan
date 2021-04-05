#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.IndirectCommands
       (VkIndirectCommandsLayoutCreateInfoNV,
        VkIndirectCommandsLayoutTokenNV, VkIndirectCommandsStreamNV)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.BaseTypes          (VkBool32, VkDeviceSize)
import Graphics.Vulkan.Types.Enum.IndexType     (VkIndexType)
import Graphics.Vulkan.Types.Enum.Indirect      (VkIndirectCommandsLayoutUsageFlagsNV,
                                                 VkIndirectCommandsTokenTypeNV,
                                                 VkIndirectStateFlagsNV)
import Graphics.Vulkan.Types.Enum.Pipeline      (VkPipelineBindPoint)
import Graphics.Vulkan.Types.Enum.Shader        (VkShaderStageFlags)
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import Graphics.Vulkan.Types.Handles            (VkBuffer, VkPipelineLayout)

-- | > typedef struct VkIndirectCommandsLayoutCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                             pNext;
--   >     VkIndirectCommandsLayoutUsageFlagsNV    flags;
--   >     VkPipelineBindPoint                     pipelineBindPoint;
--   >     uint32_t                                tokenCount;
--   >     const VkIndirectCommandsLayoutTokenNV*  pTokens;
--   >     uint32_t                                streamCount;
--   >     const uint32_t*       pStreamStrides;
--   > } VkIndirectCommandsLayoutCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkIndirectCommandsLayoutCreateInfoNV VkIndirectCommandsLayoutCreateInfoNV registry at www.khronos.org>
type VkIndirectCommandsLayoutCreateInfoNV =
     VkStruct VkIndirectCommandsLayoutCreateInfoNV' -- ' closing tick for hsc2hs

data VkIndirectCommandsLayoutCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkIndirectCommandsLayoutCreateInfoNV where
    type StructRep VkIndirectCommandsLayoutCreateInfoNV =
         'StructMeta "VkIndirectCommandsLayoutCreateInfoNV" -- ' closing tick for hsc2hs
           VkIndirectCommandsLayoutCreateInfoNV
           #{size VkIndirectCommandsLayoutCreateInfoNV}
           #{alignment VkIndirectCommandsLayoutCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkIndirectCommandsLayoutCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkIndirectCommandsLayoutCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkIndirectCommandsLayoutUsageFlagsNV 'False
                #{offset VkIndirectCommandsLayoutCreateInfoNV, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineBindPoint" VkPipelineBindPoint 'False
                #{offset VkIndirectCommandsLayoutCreateInfoNV, pipelineBindPoint}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "tokenCount" Word32 'False 
                                                    #{offset VkIndirectCommandsLayoutCreateInfoNV, tokenCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pTokens" (Ptr VkIndirectCommandsLayoutTokenNV) 'False
                #{offset VkIndirectCommandsLayoutCreateInfoNV, pTokens}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "streamCount" Word32 'False 
                                                     #{offset VkIndirectCommandsLayoutCreateInfoNV, streamCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pStreamStrides" (Ptr Word32) 'False 
                                                              #{offset VkIndirectCommandsLayoutCreateInfoNV, pStreamStrides}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkIndirectCommandsLayoutTokenNV {
--   >     VkStructureType sType;
--   >     const void*                    pNext;
--   >     VkIndirectCommandsTokenTypeNV  tokenType;
--   >     uint32_t                       stream;
--   >     uint32_t                       offset;
--   >     uint32_t                                vertexBindingUnit;
--   >     VkBool32                                vertexDynamicStride;
--   >     VkPipelineLayout        pushconstantPipelineLayout;
--   >     VkShaderStageFlags      pushconstantShaderStageFlags;
--   >     uint32_t                                pushconstantOffset;
--   >     uint32_t                                pushconstantSize;
--   >     VkIndirectStateFlagsNV  indirectStateFlags;
--   >     uint32_t                indexTypeCount;
--   >     const VkIndexType* pIndexTypes;
--   >     const uint32_t*    pIndexTypeValues;
--   > } VkIndirectCommandsLayoutTokenNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkIndirectCommandsLayoutTokenNV VkIndirectCommandsLayoutTokenNV registry at www.khronos.org>
type VkIndirectCommandsLayoutTokenNV =
     VkStruct VkIndirectCommandsLayoutTokenNV' -- ' closing tick for hsc2hs

data VkIndirectCommandsLayoutTokenNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkIndirectCommandsLayoutTokenNV where
    type StructRep VkIndirectCommandsLayoutTokenNV =
         'StructMeta "VkIndirectCommandsLayoutTokenNV" -- ' closing tick for hsc2hs
           VkIndirectCommandsLayoutTokenNV
           #{size VkIndirectCommandsLayoutTokenNV}
           #{alignment VkIndirectCommandsLayoutTokenNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkIndirectCommandsLayoutTokenNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkIndirectCommandsLayoutTokenNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "tokenType" VkIndirectCommandsTokenTypeNV 'False
                #{offset VkIndirectCommandsLayoutTokenNV, tokenType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stream" Word32 'False 
                                                #{offset VkIndirectCommandsLayoutTokenNV, stream}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "offset" Word32 'False 
                                                #{offset VkIndirectCommandsLayoutTokenNV, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexBindingUnit" Word32 'False 
                                                           #{offset VkIndirectCommandsLayoutTokenNV, vertexBindingUnit}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexDynamicStride" VkBool32 'False 
                                                               #{offset VkIndirectCommandsLayoutTokenNV, vertexDynamicStride}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pushconstantPipelineLayout" VkPipelineLayout 'True
                #{offset VkIndirectCommandsLayoutTokenNV, pushconstantPipelineLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pushconstantShaderStageFlags" VkShaderStageFlags 'True
                #{offset VkIndirectCommandsLayoutTokenNV, pushconstantShaderStageFlags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pushconstantOffset" Word32 'False 
                                                            #{offset VkIndirectCommandsLayoutTokenNV, pushconstantOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pushconstantSize" Word32 'False 
                                                          #{offset VkIndirectCommandsLayoutTokenNV, pushconstantSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "indirectStateFlags" VkIndirectStateFlagsNV 'True
                #{offset VkIndirectCommandsLayoutTokenNV, indirectStateFlags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "indexTypeCount" Word32 'True 
                                                       #{offset VkIndirectCommandsLayoutTokenNV, indexTypeCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pIndexTypes" (Ptr VkIndexType) 'False 
                                                                #{offset VkIndirectCommandsLayoutTokenNV, pIndexTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pIndexTypeValues" (Ptr Word32) 'False 
                                                                #{offset VkIndirectCommandsLayoutTokenNV, pIndexTypeValues}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkIndirectCommandsStreamNV {
--   >     VkBuffer      buffer;
--   >     VkDeviceSize  offset;
--   > } VkIndirectCommandsStreamNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkIndirectCommandsStreamNV VkIndirectCommandsStreamNV registry at www.khronos.org>
type VkIndirectCommandsStreamNV =
     VkStruct VkIndirectCommandsStreamNV' -- ' closing tick for hsc2hs

data VkIndirectCommandsStreamNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkIndirectCommandsStreamNV where
    type StructRep VkIndirectCommandsStreamNV =
         'StructMeta "VkIndirectCommandsStreamNV" VkIndirectCommandsStreamNV -- ' closing tick for hsc2hs
           #{size VkIndirectCommandsStreamNV}
           #{alignment VkIndirectCommandsStreamNV}
           '[('FieldMeta "buffer" VkBuffer 'False  -- ' closing tick for hsc2hs
                                                  #{offset VkIndirectCommandsStreamNV, buffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "offset" VkDeviceSize 'False 
                                                      #{offset VkIndirectCommandsStreamNV, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
