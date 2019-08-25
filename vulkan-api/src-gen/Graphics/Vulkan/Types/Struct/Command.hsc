#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Command
       (VkCommandBufferAllocateInfo, VkCommandBufferBeginInfo,
        VkCommandBufferInheritanceInfo, VkCommandPoolCreateInfo)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkBool32)
import           Graphics.Vulkan.Types.Enum.Command       (VkCommandBufferLevel, VkCommandBufferUsageFlags,
                                                           VkCommandPoolCreateFlags)
import           Graphics.Vulkan.Types.Enum.Query         (VkQueryControlFlags, VkQueryPipelineStatisticFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkCommandPool,
                                                           VkFramebuffer,
                                                           VkRenderPass)

-- | > typedef struct VkCommandBufferAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkCommandPool          commandPool;
--   >     VkCommandBufferLevel   level;
--   >     uint32_t               commandBufferCount;
--   > } VkCommandBufferAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCommandBufferAllocateInfo VkCommandBufferAllocateInfo registry at www.khronos.org>
type VkCommandBufferAllocateInfo =
     VkStruct VkCommandBufferAllocateInfo' -- ' closing tick for hsc2hs

data VkCommandBufferAllocateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkCommandBufferAllocateInfo where
    type StructRep VkCommandBufferAllocateInfo =
         'StructMeta "VkCommandBufferAllocateInfo" -- ' closing tick for hsc2hs
           VkCommandBufferAllocateInfo
           #{size VkCommandBufferAllocateInfo}
           #{alignment VkCommandBufferAllocateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkCommandBufferAllocateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkCommandBufferAllocateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "commandPool" VkCommandPool 'False 
                                                            #{offset VkCommandBufferAllocateInfo, commandPool}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "level" VkCommandBufferLevel 'False 
                                                             #{offset VkCommandBufferAllocateInfo, level}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "commandBufferCount" Word32 'False 
                                                            #{offset VkCommandBufferAllocateInfo, commandBufferCount}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkCommandBufferBeginInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkCommandBufferUsageFlags  flags;
--   >     const VkCommandBufferInheritanceInfo*       pInheritanceInfo;
--   > } VkCommandBufferBeginInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCommandBufferBeginInfo VkCommandBufferBeginInfo registry at www.khronos.org>
type VkCommandBufferBeginInfo = VkStruct VkCommandBufferBeginInfo' -- ' closing tick for hsc2hs

data VkCommandBufferBeginInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkCommandBufferBeginInfo where
    type StructRep VkCommandBufferBeginInfo =
         'StructMeta "VkCommandBufferBeginInfo" VkCommandBufferBeginInfo -- ' closing tick for hsc2hs
           #{size VkCommandBufferBeginInfo}
           #{alignment VkCommandBufferBeginInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkCommandBufferBeginInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkCommandBufferBeginInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkCommandBufferUsageFlags 'True 
                                                                 #{offset VkCommandBufferBeginInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pInheritanceInfo" (Ptr VkCommandBufferInheritanceInfo) -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkCommandBufferBeginInfo, pInheritanceInfo}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkCommandBufferInheritanceInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkRenderPass    renderPass;
--   >     uint32_t               subpass;
--   >     VkFramebuffer   framebuffer;
--   >     VkBool32               occlusionQueryEnable;
--   >     VkQueryControlFlags    queryFlags;
--   >     VkQueryPipelineStatisticFlags pipelineStatistics;
--   > } VkCommandBufferInheritanceInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCommandBufferInheritanceInfo VkCommandBufferInheritanceInfo registry at www.khronos.org>
type VkCommandBufferInheritanceInfo =
     VkStruct VkCommandBufferInheritanceInfo' -- ' closing tick for hsc2hs

data VkCommandBufferInheritanceInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkCommandBufferInheritanceInfo where
    type StructRep VkCommandBufferInheritanceInfo =
         'StructMeta "VkCommandBufferInheritanceInfo" -- ' closing tick for hsc2hs
           VkCommandBufferInheritanceInfo
           #{size VkCommandBufferInheritanceInfo}
           #{alignment VkCommandBufferInheritanceInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkCommandBufferInheritanceInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkCommandBufferInheritanceInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "renderPass" VkRenderPass 'True 
                                                         #{offset VkCommandBufferInheritanceInfo, renderPass}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subpass" Word32 'False 
                                                 #{offset VkCommandBufferInheritanceInfo, subpass}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "framebuffer" VkFramebuffer 'True 
                                                           #{offset VkCommandBufferInheritanceInfo, framebuffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "occlusionQueryEnable" VkBool32 'False 
                                                                #{offset VkCommandBufferInheritanceInfo, occlusionQueryEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queryFlags" VkQueryControlFlags 'True 
                                                                #{offset VkCommandBufferInheritanceInfo, queryFlags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineStatistics" VkQueryPipelineStatisticFlags -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkCommandBufferInheritanceInfo, pipelineStatistics}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkCommandPoolCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkCommandPoolCreateFlags   flags;
--   >     uint32_t               queueFamilyIndex;
--   > } VkCommandPoolCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCommandPoolCreateInfo VkCommandPoolCreateInfo registry at www.khronos.org>
type VkCommandPoolCreateInfo = VkStruct VkCommandPoolCreateInfo' -- ' closing tick for hsc2hs

data VkCommandPoolCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkCommandPoolCreateInfo where
    type StructRep VkCommandPoolCreateInfo =
         'StructMeta "VkCommandPoolCreateInfo" VkCommandPoolCreateInfo -- ' closing tick for hsc2hs
           #{size VkCommandPoolCreateInfo}
           #{alignment VkCommandPoolCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkCommandPoolCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkCommandPoolCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkCommandPoolCreateFlags 'True 
                                                                #{offset VkCommandPoolCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queueFamilyIndex" Word32 'False 
                                                          #{offset VkCommandPoolCreateInfo, queueFamilyIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
