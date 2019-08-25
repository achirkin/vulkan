#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.DedicatedAllocation
       (VkDedicatedAllocationBufferCreateInfoNV,
        VkDedicatedAllocationImageCreateInfoNV,
        VkDedicatedAllocationMemoryAllocateInfoNV)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkBool32)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkBuffer, VkImage)
import           Graphics.Vulkan.Types.Struct.Buffer      (VkBufferCreateInfo)
import           Graphics.Vulkan.Types.Struct.Image       (VkImageCreateInfo)
import           Graphics.Vulkan.Types.Struct.Memory      (VkMemoryAllocateInfo)

-- | > typedef struct VkDedicatedAllocationBufferCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         dedicatedAllocation;
--   > } VkDedicatedAllocationBufferCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDedicatedAllocationBufferCreateInfoNV VkDedicatedAllocationBufferCreateInfoNV registry at www.khronos.org>
type VkDedicatedAllocationBufferCreateInfoNV =
     VkStruct VkDedicatedAllocationBufferCreateInfoNV' -- ' closing tick for hsc2hs

data VkDedicatedAllocationBufferCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDedicatedAllocationBufferCreateInfoNV
         where
    type StructRep VkDedicatedAllocationBufferCreateInfoNV =
         'StructMeta "VkDedicatedAllocationBufferCreateInfoNV" -- ' closing tick for hsc2hs
           VkDedicatedAllocationBufferCreateInfoNV
           #{size VkDedicatedAllocationBufferCreateInfoNV}
           #{alignment VkDedicatedAllocationBufferCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDedicatedAllocationBufferCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dedicatedAllocation" VkBool32 'False 
                                                               #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkBufferCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkDedicatedAllocationImageCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         dedicatedAllocation;
--   > } VkDedicatedAllocationImageCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDedicatedAllocationImageCreateInfoNV VkDedicatedAllocationImageCreateInfoNV registry at www.khronos.org>
type VkDedicatedAllocationImageCreateInfoNV =
     VkStruct VkDedicatedAllocationImageCreateInfoNV' -- ' closing tick for hsc2hs

data VkDedicatedAllocationImageCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDedicatedAllocationImageCreateInfoNV where
    type StructRep VkDedicatedAllocationImageCreateInfoNV =
         'StructMeta "VkDedicatedAllocationImageCreateInfoNV" -- ' closing tick for hsc2hs
           VkDedicatedAllocationImageCreateInfoNV
           #{size VkDedicatedAllocationImageCreateInfoNV}
           #{alignment VkDedicatedAllocationImageCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDedicatedAllocationImageCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDedicatedAllocationImageCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dedicatedAllocation" VkBool32 'False 
                                                               #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkImageCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkDedicatedAllocationMemoryAllocateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImage          image;
--   >     VkBuffer         buffer;
--   > } VkDedicatedAllocationMemoryAllocateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDedicatedAllocationMemoryAllocateInfoNV VkDedicatedAllocationMemoryAllocateInfoNV registry at www.khronos.org>
type VkDedicatedAllocationMemoryAllocateInfoNV =
     VkStruct VkDedicatedAllocationMemoryAllocateInfoNV' -- ' closing tick for hsc2hs

data VkDedicatedAllocationMemoryAllocateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDedicatedAllocationMemoryAllocateInfoNV
         where
    type StructRep VkDedicatedAllocationMemoryAllocateInfoNV =
         'StructMeta "VkDedicatedAllocationMemoryAllocateInfoNV" -- ' closing tick for hsc2hs
           VkDedicatedAllocationMemoryAllocateInfoNV
           #{size VkDedicatedAllocationMemoryAllocateInfoNV}
           #{alignment VkDedicatedAllocationMemoryAllocateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "image" VkImage 'True 
                                               #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "buffer" VkBuffer 'True 
                                                 #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs
