#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Graphics
       (VkGraphicsPipelineShaderGroupsCreateInfoNV,
        VkGraphicsShaderGroupCreateInfoNV)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import Graphics.Vulkan.Types.Handles            (VkPipeline)
import Graphics.Vulkan.Types.Struct.Pipeline    (VkGraphicsPipelineCreateInfo,
                                                 VkPipelineShaderStageCreateInfo,
                                                 VkPipelineTessellationStateCreateInfo,
                                                 VkPipelineVertexInputStateCreateInfo)

-- | > typedef struct VkGraphicsPipelineShaderGroupsCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                                                pNext;
--   >     uint32_t                                                   groupCount;
--   >     const VkGraphicsShaderGroupCreateInfoNV*  pGroups;
--   >     uint32_t                                   pipelineCount;
--   >     const VkPipeline*                      pPipelines;
--   > } VkGraphicsPipelineShaderGroupsCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkGraphicsPipelineShaderGroupsCreateInfoNV VkGraphicsPipelineShaderGroupsCreateInfoNV registry at www.khronos.org>
type VkGraphicsPipelineShaderGroupsCreateInfoNV =
     VkStruct VkGraphicsPipelineShaderGroupsCreateInfoNV' -- ' closing tick for hsc2hs

data VkGraphicsPipelineShaderGroupsCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkGraphicsPipelineShaderGroupsCreateInfoNV
         where
    type StructRep VkGraphicsPipelineShaderGroupsCreateInfoNV =
         'StructMeta "VkGraphicsPipelineShaderGroupsCreateInfoNV" -- ' closing tick for hsc2hs
           VkGraphicsPipelineShaderGroupsCreateInfoNV
           #{size VkGraphicsPipelineShaderGroupsCreateInfoNV}
           #{alignment VkGraphicsPipelineShaderGroupsCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkGraphicsPipelineShaderGroupsCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkGraphicsPipelineShaderGroupsCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "groupCount" Word32 'False 
                                                    #{offset VkGraphicsPipelineShaderGroupsCreateInfoNV, groupCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pGroups" (Ptr VkGraphicsShaderGroupCreateInfoNV) -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkGraphicsPipelineShaderGroupsCreateInfoNV, pGroups}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineCount" Word32 'True 
                                                      #{offset VkGraphicsPipelineShaderGroupsCreateInfoNV, pipelineCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pPipelines" (Ptr VkPipeline) 'False 
                                                              #{offset VkGraphicsPipelineShaderGroupsCreateInfoNV, pPipelines}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkGraphicsPipelineCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkGraphicsShaderGroupCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                                                pNext;
--   >     uint32_t                                                   stageCount;
--   >     const VkPipelineShaderStageCreateInfo*    pStages;
--   >     const VkPipelineVertexInputStateCreateInfo*                pVertexInputState;
--   >     const VkPipelineTessellationStateCreateInfo*               pTessellationState;
--   > } VkGraphicsShaderGroupCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkGraphicsShaderGroupCreateInfoNV VkGraphicsShaderGroupCreateInfoNV registry at www.khronos.org>
type VkGraphicsShaderGroupCreateInfoNV =
     VkStruct VkGraphicsShaderGroupCreateInfoNV' -- ' closing tick for hsc2hs

data VkGraphicsShaderGroupCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkGraphicsShaderGroupCreateInfoNV where
    type StructRep VkGraphicsShaderGroupCreateInfoNV =
         'StructMeta "VkGraphicsShaderGroupCreateInfoNV" -- ' closing tick for hsc2hs
           VkGraphicsShaderGroupCreateInfoNV
           #{size VkGraphicsShaderGroupCreateInfoNV}
           #{alignment VkGraphicsShaderGroupCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkGraphicsShaderGroupCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkGraphicsShaderGroupCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stageCount" Word32 'False 
                                                    #{offset VkGraphicsShaderGroupCreateInfoNV, stageCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pStages" (Ptr VkPipelineShaderStageCreateInfo) 'False
                #{offset VkGraphicsShaderGroupCreateInfoNV, pStages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pVertexInputState" -- ' closing tick for hsc2hs
                (Ptr VkPipelineVertexInputStateCreateInfo)
                'True -- ' closing tick for hsc2hs
                #{offset VkGraphicsShaderGroupCreateInfoNV, pVertexInputState}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pTessellationState" -- ' closing tick for hsc2hs
                (Ptr VkPipelineTessellationStateCreateInfo)
                'True -- ' closing tick for hsc2hs
                #{offset VkGraphicsShaderGroupCreateInfoNV, pTessellationState}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
