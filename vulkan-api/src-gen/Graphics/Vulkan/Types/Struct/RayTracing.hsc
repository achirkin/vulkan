#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.RayTracing
       (VkRayTracingPipelineCreateInfoNV,
        VkRayTracingShaderGroupCreateInfoNV)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.Pipeline                  (VkPipelineCreateFlags)
import Graphics.Vulkan.Types.Enum.RayTracingShaderGroupType (VkRayTracingShaderGroupTypeKHR)
import Graphics.Vulkan.Types.Enum.StructureType             (VkStructureType)
import Graphics.Vulkan.Types.Handles                        (VkPipeline,
                                                             VkPipelineLayout)
import
#ifdef VK_ENABLE_BETA_EXTENSIONS
  {-# SOURCE #-}
#endif
  Graphics.Vulkan.Types.Struct.Pipeline (VkPipelineShaderStageCreateInfo)

-- | > typedef struct VkRayTracingPipelineCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineCreateFlags  flags;
--   >     uint32_t               stageCount;
--   >     const VkPipelineShaderStageCreateInfo* pStages;
--   >     uint32_t               groupCount;
--   >     const VkRayTracingShaderGroupCreateInfoNV* pGroups;
--   >     uint32_t               maxRecursionDepth;
--   >     VkPipelineLayout       layout;
--   >     VkPipeline      basePipelineHandle;
--   >     int32_t                basePipelineIndex;
--   > } VkRayTracingPipelineCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRayTracingPipelineCreateInfoNV VkRayTracingPipelineCreateInfoNV registry at www.khronos.org>
type VkRayTracingPipelineCreateInfoNV =
     VkStruct VkRayTracingPipelineCreateInfoNV' -- ' closing tick for hsc2hs

data VkRayTracingPipelineCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRayTracingPipelineCreateInfoNV where
    type StructRep VkRayTracingPipelineCreateInfoNV =
         'StructMeta "VkRayTracingPipelineCreateInfoNV" -- ' closing tick for hsc2hs
           VkRayTracingPipelineCreateInfoNV
           #{size VkRayTracingPipelineCreateInfoNV}
           #{alignment VkRayTracingPipelineCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkRayTracingPipelineCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkRayTracingPipelineCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineCreateFlags 'True
                                                             #{offset VkRayTracingPipelineCreateInfoNV, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stageCount" Word32 'False
                                                    #{offset VkRayTracingPipelineCreateInfoNV, stageCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pStages" (Ptr VkPipelineShaderStageCreateInfo) 'False
                #{offset VkRayTracingPipelineCreateInfoNV, pStages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "groupCount" Word32 'False
                                                    #{offset VkRayTracingPipelineCreateInfoNV, groupCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pGroups" (Ptr VkRayTracingShaderGroupCreateInfoNV) -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkRayTracingPipelineCreateInfoNV, pGroups}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxRecursionDepth" Word32 'False
                                                           #{offset VkRayTracingPipelineCreateInfoNV, maxRecursionDepth}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "layout" VkPipelineLayout 'False
                                                          #{offset VkRayTracingPipelineCreateInfoNV, layout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "basePipelineHandle" VkPipeline 'True
                                                               #{offset VkRayTracingPipelineCreateInfoNV, basePipelineHandle}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "basePipelineIndex" Int32 'False
                                                          #{offset VkRayTracingPipelineCreateInfoNV, basePipelineIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkRayTracingShaderGroupCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkRayTracingShaderGroupTypeKHR type;
--   >     uint32_t               generalShader;
--   >     uint32_t               closestHitShader;
--   >     uint32_t               anyHitShader;
--   >     uint32_t               intersectionShader;
--   > } VkRayTracingShaderGroupCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRayTracingShaderGroupCreateInfoNV VkRayTracingShaderGroupCreateInfoNV registry at www.khronos.org>
type VkRayTracingShaderGroupCreateInfoNV =
     VkStruct VkRayTracingShaderGroupCreateInfoNV' -- ' closing tick for hsc2hs

data VkRayTracingShaderGroupCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRayTracingShaderGroupCreateInfoNV where
    type StructRep VkRayTracingShaderGroupCreateInfoNV =
         'StructMeta "VkRayTracingShaderGroupCreateInfoNV" -- ' closing tick for hsc2hs
           VkRayTracingShaderGroupCreateInfoNV
           #{size VkRayTracingShaderGroupCreateInfoNV}
           #{alignment VkRayTracingShaderGroupCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkRayTracingShaderGroupCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkRayTracingShaderGroupCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "type" VkRayTracingShaderGroupTypeKHR 'False
                                                                      #{offset VkRayTracingShaderGroupCreateInfoNV, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "generalShader" Word32 'False
                                                       #{offset VkRayTracingShaderGroupCreateInfoNV, generalShader}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "closestHitShader" Word32 'False
                                                          #{offset VkRayTracingShaderGroupCreateInfoNV, closestHitShader}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "anyHitShader" Word32 'False
                                                      #{offset VkRayTracingShaderGroupCreateInfoNV, anyHitShader}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "intersectionShader" Word32 'False
                                                            #{offset VkRayTracingShaderGroupCreateInfoNV, intersectionShader}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
