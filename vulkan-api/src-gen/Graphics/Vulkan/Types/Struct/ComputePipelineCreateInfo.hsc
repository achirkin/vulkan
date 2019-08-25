#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.ComputePipelineCreateInfo
       (VkComputePipelineCreateInfo) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Pipeline      (VkPipelineCreateFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkPipeline,
                                                           VkPipelineLayout)
import           Graphics.Vulkan.Types.Struct.Pipeline    (VkPipelineShaderStageCreateInfo)

-- | > typedef struct VkComputePipelineCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineCreateFlags  flags;
--   >     VkPipelineShaderStageCreateInfo stage;
--   >     VkPipelineLayout       layout;
--   >     VkPipeline      basePipelineHandle;
--   >     int32_t                basePipelineIndex;
--   > } VkComputePipelineCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkComputePipelineCreateInfo VkComputePipelineCreateInfo registry at www.khronos.org>
type VkComputePipelineCreateInfo =
     VkStruct VkComputePipelineCreateInfo' -- ' closing tick for hsc2hs

data VkComputePipelineCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkComputePipelineCreateInfo where
    type StructRep VkComputePipelineCreateInfo =
         'StructMeta "VkComputePipelineCreateInfo" -- ' closing tick for hsc2hs
           VkComputePipelineCreateInfo
           #{size VkComputePipelineCreateInfo}
           #{alignment VkComputePipelineCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkComputePipelineCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkComputePipelineCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineCreateFlags 'True 
                                                             #{offset VkComputePipelineCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stage" VkPipelineShaderStageCreateInfo 'False
                #{offset VkComputePipelineCreateInfo, stage}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "layout" VkPipelineLayout 'False 
                                                          #{offset VkComputePipelineCreateInfo, layout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "basePipelineHandle" VkPipeline 'True 
                                                               #{offset VkComputePipelineCreateInfo, basePipelineHandle}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "basePipelineIndex" Int32 'False 
                                                          #{offset VkComputePipelineCreateInfo, basePipelineIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
