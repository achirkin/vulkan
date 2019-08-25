#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.SubmitInfo (VkSubmitInfo) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Pipeline      (VkPipelineStageFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkCommandBuffer,
                                                           VkSemaphore)

-- | > typedef struct VkSubmitInfo {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     uint32_t       waitSemaphoreCount;
--   >     const VkSemaphore*     pWaitSemaphores;
--   >     const VkPipelineStageFlags*           pWaitDstStageMask;
--   >     uint32_t       commandBufferCount;
--   >     const VkCommandBuffer*     pCommandBuffers;
--   >     uint32_t       signalSemaphoreCount;
--   >     const VkSemaphore*     pSignalSemaphores;
--   > } VkSubmitInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSubmitInfo VkSubmitInfo registry at www.khronos.org>
type VkSubmitInfo = VkStruct VkSubmitInfo' -- ' closing tick for hsc2hs

data VkSubmitInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSubmitInfo where
    type StructRep VkSubmitInfo =
         'StructMeta "VkSubmitInfo" VkSubmitInfo  -- ' closing tick for hsc2hs
                                                 #{size VkSubmitInfo}
           #{alignment VkSubmitInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSubmitInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSubmitInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "waitSemaphoreCount" Word32 'True 
                                                           #{offset VkSubmitInfo, waitSemaphoreCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pWaitSemaphores" (Ptr VkSemaphore) 'False 
                                                                    #{offset VkSubmitInfo, pWaitSemaphores}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pWaitDstStageMask" (Ptr VkPipelineStageFlags) 'False
                #{offset VkSubmitInfo, pWaitDstStageMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "commandBufferCount" Word32 'True 
                                                           #{offset VkSubmitInfo, commandBufferCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pCommandBuffers" (Ptr VkCommandBuffer) 'False
                #{offset VkSubmitInfo, pCommandBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "signalSemaphoreCount" Word32 'True 
                                                             #{offset VkSubmitInfo, signalSemaphoreCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pSignalSemaphores" (Ptr VkSemaphore) 'False 
                                                                      #{offset VkSubmitInfo, pSignalSemaphores}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
