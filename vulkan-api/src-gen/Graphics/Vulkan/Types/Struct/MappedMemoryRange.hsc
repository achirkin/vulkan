#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.MappedMemoryRange
       (VkMappedMemoryRange) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkDeviceMemory)

-- | > typedef struct VkMappedMemoryRange {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDeviceMemory         memory;
--   >     VkDeviceSize           offset;
--   >     VkDeviceSize           size;
--   > } VkMappedMemoryRange;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMappedMemoryRange VkMappedMemoryRange registry at www.khronos.org>
type VkMappedMemoryRange = VkStruct VkMappedMemoryRange' -- ' closing tick for hsc2hs

data VkMappedMemoryRange' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMappedMemoryRange where
    type StructRep VkMappedMemoryRange =
         'StructMeta "VkMappedMemoryRange" VkMappedMemoryRange  -- ' closing tick for hsc2hs
                                                               #{size VkMappedMemoryRange}
           #{alignment VkMappedMemoryRange}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMappedMemoryRange, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMappedMemoryRange, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memory" VkDeviceMemory 'False 
                                                        #{offset VkMappedMemoryRange, memory}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "offset" VkDeviceSize 'False 
                                                      #{offset VkMappedMemoryRange, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "size" VkDeviceSize 'False 
                                                    #{offset VkMappedMemoryRange, size}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
