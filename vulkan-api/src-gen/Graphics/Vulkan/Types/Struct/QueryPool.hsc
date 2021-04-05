#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.QueryPool
       (VkQueryPoolCreateInfo, VkQueryPoolCreateInfoINTEL,
        VkQueryPoolPerformanceCreateInfoKHR,
        VkQueryPoolPerformanceQueryCreateInfoINTEL)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Bitmasks           (VkQueryPoolCreateFlags)
import Graphics.Vulkan.Types.Enum.Query         (VkQueryPipelineStatisticFlags,
                                                 VkQueryPoolSamplingModeINTEL,
                                                 VkQueryType)
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)

-- | > typedef struct VkQueryPoolCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkQueryPoolCreateFlags flags;
--   >     VkQueryType            queryType;
--   >     uint32_t               queryCount;
--   >     VkQueryPipelineStatisticFlags pipelineStatistics;
--   > } VkQueryPoolCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueryPoolCreateInfo VkQueryPoolCreateInfo registry at www.khronos.org>
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

-- | Alias for `VkQueryPoolPerformanceQueryCreateInfoINTEL`
type VkQueryPoolCreateInfoINTEL =
     VkQueryPoolPerformanceQueryCreateInfoINTEL

-- | > typedef struct VkQueryPoolPerformanceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                             pNext;
--   >     uint32_t                                queueFamilyIndex;
--   >     uint32_t                                counterIndexCount;
--   >     const uint32_t* pCounterIndices;
--   > } VkQueryPoolPerformanceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueryPoolPerformanceCreateInfoKHR VkQueryPoolPerformanceCreateInfoKHR registry at www.khronos.org>
type VkQueryPoolPerformanceCreateInfoKHR =
     VkStruct VkQueryPoolPerformanceCreateInfoKHR' -- ' closing tick for hsc2hs

data VkQueryPoolPerformanceCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkQueryPoolPerformanceCreateInfoKHR where
    type StructRep VkQueryPoolPerformanceCreateInfoKHR =
         'StructMeta "VkQueryPoolPerformanceCreateInfoKHR" -- ' closing tick for hsc2hs
           VkQueryPoolPerformanceCreateInfoKHR
           #{size VkQueryPoolPerformanceCreateInfoKHR}
           #{alignment VkQueryPoolPerformanceCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkQueryPoolPerformanceCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkQueryPoolPerformanceCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queueFamilyIndex" Word32 'False 
                                                          #{offset VkQueryPoolPerformanceCreateInfoKHR, queueFamilyIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "counterIndexCount" Word32 'False 
                                                           #{offset VkQueryPoolPerformanceCreateInfoKHR, counterIndexCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pCounterIndices" (Ptr Word32) 'False 
                                                               #{offset VkQueryPoolPerformanceCreateInfoKHR, pCounterIndices}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkQueryPoolCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkQueryPoolPerformanceQueryCreateInfoINTEL {
--   >     VkStructureType sType;
--   >     const void*                         pNext;
--   >     VkQueryPoolSamplingModeINTEL        performanceCountersSampling;
--   > } VkQueryPoolPerformanceQueryCreateInfoINTEL;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueryPoolPerformanceQueryCreateInfoINTEL VkQueryPoolPerformanceQueryCreateInfoINTEL registry at www.khronos.org>
type VkQueryPoolPerformanceQueryCreateInfoINTEL =
     VkStruct VkQueryPoolPerformanceQueryCreateInfoINTEL' -- ' closing tick for hsc2hs

data VkQueryPoolPerformanceQueryCreateInfoINTEL' -- ' closing tick for hsc2hs

instance VulkanMarshal VkQueryPoolPerformanceQueryCreateInfoINTEL
         where
    type StructRep VkQueryPoolPerformanceQueryCreateInfoINTEL =
         'StructMeta "VkQueryPoolPerformanceQueryCreateInfoINTEL" -- ' closing tick for hsc2hs
           VkQueryPoolPerformanceQueryCreateInfoINTEL
           #{size VkQueryPoolPerformanceQueryCreateInfoINTEL}
           #{alignment VkQueryPoolPerformanceQueryCreateInfoINTEL}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkQueryPoolPerformanceQueryCreateInfoINTEL, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkQueryPoolPerformanceQueryCreateInfoINTEL, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "performanceCountersSampling" -- ' closing tick for hsc2hs
                VkQueryPoolSamplingModeINTEL
                'False -- ' closing tick for hsc2hs
                #{offset VkQueryPoolPerformanceQueryCreateInfoINTEL, performanceCountersSampling}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkQueryPoolCreateInfo] -- ' closing tick for hsc2hs
