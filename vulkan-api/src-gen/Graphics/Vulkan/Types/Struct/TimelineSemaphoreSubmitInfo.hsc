#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.TimelineSemaphoreSubmitInfo
       (VkTimelineSemaphoreSubmitInfo, VkTimelineSemaphoreSubmitInfoKHR)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import Graphics.Vulkan.Types.Struct.Bind        (VkBindSparseInfo)
import Graphics.Vulkan.Types.Struct.SubmitInfo  (VkSubmitInfo)

-- | > typedef struct VkTimelineSemaphoreSubmitInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         waitSemaphoreValueCount;
--   >     const uint64_t* pWaitSemaphoreValues;
--   >     uint32_t         signalSemaphoreValueCount;
--   >     const uint64_t* pSignalSemaphoreValues;
--   > } VkTimelineSemaphoreSubmitInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkTimelineSemaphoreSubmitInfo VkTimelineSemaphoreSubmitInfo registry at www.khronos.org>
type VkTimelineSemaphoreSubmitInfo =
     VkStruct VkTimelineSemaphoreSubmitInfo' -- ' closing tick for hsc2hs

data VkTimelineSemaphoreSubmitInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkTimelineSemaphoreSubmitInfo where
    type StructRep VkTimelineSemaphoreSubmitInfo =
         'StructMeta "VkTimelineSemaphoreSubmitInfo" -- ' closing tick for hsc2hs
           VkTimelineSemaphoreSubmitInfo
           #{size VkTimelineSemaphoreSubmitInfo}
           #{alignment VkTimelineSemaphoreSubmitInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkTimelineSemaphoreSubmitInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkTimelineSemaphoreSubmitInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "waitSemaphoreValueCount" Word32 'True 
                                                                #{offset VkTimelineSemaphoreSubmitInfo, waitSemaphoreValueCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pWaitSemaphoreValues" (Ptr Word64) 'True 
                                                                   #{offset VkTimelineSemaphoreSubmitInfo, pWaitSemaphoreValues}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "signalSemaphoreValueCount" Word32 'True 
                                                                  #{offset VkTimelineSemaphoreSubmitInfo, signalSemaphoreValueCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pSignalSemaphoreValues" (Ptr Word64) 'True 
                                                                     #{offset VkTimelineSemaphoreSubmitInfo, pSignalSemaphoreValues}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkSubmitInfo, VkBindSparseInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkTimelineSemaphoreSubmitInfo`
type VkTimelineSemaphoreSubmitInfoKHR =
     VkTimelineSemaphoreSubmitInfo
