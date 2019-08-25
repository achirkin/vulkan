#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.QueryPoolCreateInfo
       (VkQueryPoolCreateInfo) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkQueryPoolCreateFlags)
import           Graphics.Vulkan.Types.Enum.Query         (VkQueryPipelineStatisticFlags,
                                                           VkQueryType)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)

-- | > typedef struct VkQueryPoolCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkQueryPoolCreateFlags flags;
--   >     VkQueryType            queryType;
--   >     uint32_t               queryCount;
--   >     VkQueryPipelineStatisticFlags pipelineStatistics;
--   > } VkQueryPoolCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkQueryPoolCreateInfo VkQueryPoolCreateInfo registry at www.khronos.org>
type VkQueryPoolCreateInfo = VkStruct VkQueryPoolCreateInfo' -- ' closing tick for hsc2hs

data VkQueryPoolCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkQueryPoolCreateInfo where
    type StructRep VkQueryPoolCreateInfo =
         'StructMeta "VkQueryPoolCreateInfo" VkQueryPoolCreateInfo  -- ' closing tick for hsc2hs
                                                                   #{size VkQueryPoolCreateInfo}
           #{alignment VkQueryPoolCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkQueryPoolCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkQueryPoolCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkQueryPoolCreateFlags 'True 
                                                              #{offset VkQueryPoolCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queryType" VkQueryType 'False 
                                                        #{offset VkQueryPoolCreateInfo, queryType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queryCount" Word32 'False 
                                                    #{offset VkQueryPoolCreateInfo, queryCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineStatistics" VkQueryPipelineStatisticFlags -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkQueryPoolCreateInfo, pipelineStatistics}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
