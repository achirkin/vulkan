#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Specialization
       (VkSpecializationInfo, VkSpecializationMapEntry) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal

-- | > typedef struct VkSpecializationInfo {
--   >     uint32_t               mapEntryCount;
--   >     const VkSpecializationMapEntry* pMapEntries;
--   >     size_t                 dataSize;
--   >     const void*            pData;
--   > } VkSpecializationInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSpecializationInfo VkSpecializationInfo registry at www.khronos.org>
type VkSpecializationInfo = VkStruct VkSpecializationInfo' -- ' closing tick for hsc2hs

data VkSpecializationInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSpecializationInfo where
    type StructRep VkSpecializationInfo =
         'StructMeta "VkSpecializationInfo" VkSpecializationInfo  -- ' closing tick for hsc2hs
                                                                 #{size VkSpecializationInfo}
           #{alignment VkSpecializationInfo}
           '[('FieldMeta "mapEntryCount" Word32 'True  -- ' closing tick for hsc2hs
                                                      #{offset VkSpecializationInfo, mapEntryCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pMapEntries" (Ptr VkSpecializationMapEntry) 'False
                #{offset VkSpecializationInfo, pMapEntries}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dataSize" CSize 'True 
                                                #{offset VkSpecializationInfo, dataSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pData" (Ptr Void) 'False 
                                                   #{offset VkSpecializationInfo, pData}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSpecializationMapEntry {
--   >     uint32_t               constantID;
--   >     uint32_t               offset;
--   >     size_t                 size;
--   > } VkSpecializationMapEntry;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSpecializationMapEntry VkSpecializationMapEntry registry at www.khronos.org>
type VkSpecializationMapEntry = VkStruct VkSpecializationMapEntry' -- ' closing tick for hsc2hs

data VkSpecializationMapEntry' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSpecializationMapEntry where
    type StructRep VkSpecializationMapEntry =
         'StructMeta "VkSpecializationMapEntry" VkSpecializationMapEntry -- ' closing tick for hsc2hs
           #{size VkSpecializationMapEntry}
           #{alignment VkSpecializationMapEntry}
           '[('FieldMeta "constantID" Word32 'False  -- ' closing tick for hsc2hs
                                                    #{offset VkSpecializationMapEntry, constantID}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "offset" Word32 'False 
                                                #{offset VkSpecializationMapEntry, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "size" CSize 'False 
                                             #{offset VkSpecializationMapEntry, size}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
