#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.CheckpointDataNV
       (VkCheckpointDataNV) where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.Pipeline      (VkPipelineStageFlagBits)
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)

-- | > typedef struct VkCheckpointDataNV {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     VkPipelineStageFlagBits   stage;
--   >     void* pCheckpointMarker;
--   > } VkCheckpointDataNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCheckpointDataNV VkCheckpointDataNV registry at www.khronos.org>
type VkCheckpointDataNV = VkStruct VkCheckpointDataNV' -- ' closing tick for hsc2hs

data VkCheckpointDataNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkCheckpointDataNV where
    type StructRep VkCheckpointDataNV =
         'StructMeta "VkCheckpointDataNV" VkCheckpointDataNV  -- ' closing tick for hsc2hs
                                                             #{size VkCheckpointDataNV}
           #{alignment VkCheckpointDataNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkCheckpointDataNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkCheckpointDataNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stage" VkPipelineStageFlagBits 'False 
                                                                #{offset VkCheckpointDataNV, stage}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pCheckpointMarker" (Ptr Void) 'False 
                                                               #{offset VkCheckpointDataNV, pCheckpointMarker}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
