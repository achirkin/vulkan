#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Memory
       (VkMemoryAllocateFlagsInfo, VkMemoryAllocateFlagsInfoKHR,
        VkMemoryAllocateInfo, VkMemoryBarrier,
        VkMemoryDedicatedAllocateInfo, VkMemoryDedicatedAllocateInfoKHR,
        VkMemoryDedicatedRequirements, VkMemoryDedicatedRequirementsKHR,
        VkMemoryFdPropertiesKHR, VkMemoryGetFdInfoKHR, VkMemoryHeap,
        VkMemoryHostPointerPropertiesEXT, VkMemoryRequirements,
        VkMemoryRequirements2, VkMemoryRequirements2KHR, VkMemoryType)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkBool32,
                                                           VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.AccessFlags   (VkAccessFlags)
import           Graphics.Vulkan.Types.Enum.External      (VkExternalMemoryHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.Memory        (VkMemoryAllocateFlags,
                                                           VkMemoryHeapFlags,
                                                           VkMemoryPropertyFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkBuffer,
                                                           VkDeviceMemory,
                                                           VkImage)

-- | > typedef struct VkMemoryAllocateFlagsInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkMemoryAllocateFlags flags;
--   >     uint32_t                         deviceMask;
--   > } VkMemoryAllocateFlagsInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryAllocateFlagsInfo VkMemoryAllocateFlagsInfo registry at www.khronos.org>
type VkMemoryAllocateFlagsInfo =
     VkStruct VkMemoryAllocateFlagsInfo' -- ' closing tick for hsc2hs

data VkMemoryAllocateFlagsInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMemoryAllocateFlagsInfo where
    type StructRep VkMemoryAllocateFlagsInfo =
         'StructMeta "VkMemoryAllocateFlagsInfo" VkMemoryAllocateFlagsInfo -- ' closing tick for hsc2hs
           #{size VkMemoryAllocateFlagsInfo}
           #{alignment VkMemoryAllocateFlagsInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMemoryAllocateFlagsInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMemoryAllocateFlagsInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkMemoryAllocateFlags 'True 
                                                             #{offset VkMemoryAllocateFlagsInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceMask" Word32 'False 
                                                    #{offset VkMemoryAllocateFlagsInfo, deviceMask}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkMemoryAllocateFlagsInfo`
type VkMemoryAllocateFlagsInfoKHR = VkMemoryAllocateFlagsInfo

-- | > typedef struct VkMemoryAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDeviceSize           allocationSize;
--   >     uint32_t               memoryTypeIndex;
--   > } VkMemoryAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryAllocateInfo VkMemoryAllocateInfo registry at www.khronos.org>
type VkMemoryAllocateInfo = VkStruct VkMemoryAllocateInfo' -- ' closing tick for hsc2hs

data VkMemoryAllocateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMemoryAllocateInfo where
    type StructRep VkMemoryAllocateInfo =
         'StructMeta "VkMemoryAllocateInfo" VkMemoryAllocateInfo  -- ' closing tick for hsc2hs
                                                                 #{size VkMemoryAllocateInfo}
           #{alignment VkMemoryAllocateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMemoryAllocateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMemoryAllocateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "allocationSize" VkDeviceSize 'False 
                                                              #{offset VkMemoryAllocateInfo, allocationSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryTypeIndex" Word32 'False 
                                                         #{offset VkMemoryAllocateInfo, memoryTypeIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkMemoryBarrier {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkAccessFlags          srcAccessMask;
--   >     VkAccessFlags          dstAccessMask;
--   > } VkMemoryBarrier;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryBarrier VkMemoryBarrier registry at www.khronos.org>
type VkMemoryBarrier = VkStruct VkMemoryBarrier' -- ' closing tick for hsc2hs

data VkMemoryBarrier' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMemoryBarrier where
    type StructRep VkMemoryBarrier =
         'StructMeta "VkMemoryBarrier" VkMemoryBarrier  -- ' closing tick for hsc2hs
                                                       #{size VkMemoryBarrier}
           #{alignment VkMemoryBarrier}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMemoryBarrier, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMemoryBarrier, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcAccessMask" VkAccessFlags 'True 
                                                             #{offset VkMemoryBarrier, srcAccessMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstAccessMask" VkAccessFlags 'True 
                                                             #{offset VkMemoryBarrier, dstAccessMask}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkMemoryDedicatedAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImage          image;
--   >     VkBuffer         buffer;
--   > } VkMemoryDedicatedAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryDedicatedAllocateInfo VkMemoryDedicatedAllocateInfo registry at www.khronos.org>
type VkMemoryDedicatedAllocateInfo =
     VkStruct VkMemoryDedicatedAllocateInfo' -- ' closing tick for hsc2hs

data VkMemoryDedicatedAllocateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMemoryDedicatedAllocateInfo where
    type StructRep VkMemoryDedicatedAllocateInfo =
         'StructMeta "VkMemoryDedicatedAllocateInfo" -- ' closing tick for hsc2hs
           VkMemoryDedicatedAllocateInfo
           #{size VkMemoryDedicatedAllocateInfo}
           #{alignment VkMemoryDedicatedAllocateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMemoryDedicatedAllocateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMemoryDedicatedAllocateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "image" VkImage 'True 
                                               #{offset VkMemoryDedicatedAllocateInfo, image}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "buffer" VkBuffer 'True 
                                                 #{offset VkMemoryDedicatedAllocateInfo, buffer}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkMemoryDedicatedAllocateInfo`
type VkMemoryDedicatedAllocateInfoKHR =
     VkMemoryDedicatedAllocateInfo

-- | > typedef struct VkMemoryDedicatedRequirements {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         prefersDedicatedAllocation;
--   >     VkBool32                         requiresDedicatedAllocation;
--   > } VkMemoryDedicatedRequirements;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryDedicatedRequirements VkMemoryDedicatedRequirements registry at www.khronos.org>
type VkMemoryDedicatedRequirements =
     VkStruct VkMemoryDedicatedRequirements' -- ' closing tick for hsc2hs

data VkMemoryDedicatedRequirements' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMemoryDedicatedRequirements where
    type StructRep VkMemoryDedicatedRequirements =
         'StructMeta "VkMemoryDedicatedRequirements" -- ' closing tick for hsc2hs
           VkMemoryDedicatedRequirements
           #{size VkMemoryDedicatedRequirements}
           #{alignment VkMemoryDedicatedRequirements}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMemoryDedicatedRequirements, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMemoryDedicatedRequirements, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "prefersDedicatedAllocation" VkBool32 'False 
                                                                      #{offset VkMemoryDedicatedRequirements, prefersDedicatedAllocation}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "requiresDedicatedAllocation" VkBool32 'False 
                                                                       #{offset VkMemoryDedicatedRequirements, requiresDedicatedAllocation}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkMemoryRequirements2] -- ' closing tick for hsc2hs

-- | Alias for `VkMemoryDedicatedRequirements`
type VkMemoryDedicatedRequirementsKHR =
     VkMemoryDedicatedRequirements

-- | > typedef struct VkMemoryFdPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         memoryTypeBits;
--   > } VkMemoryFdPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryFdPropertiesKHR VkMemoryFdPropertiesKHR registry at www.khronos.org>
type VkMemoryFdPropertiesKHR = VkStruct VkMemoryFdPropertiesKHR' -- ' closing tick for hsc2hs

data VkMemoryFdPropertiesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMemoryFdPropertiesKHR where
    type StructRep VkMemoryFdPropertiesKHR =
         'StructMeta "VkMemoryFdPropertiesKHR" VkMemoryFdPropertiesKHR -- ' closing tick for hsc2hs
           #{size VkMemoryFdPropertiesKHR}
           #{alignment VkMemoryFdPropertiesKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMemoryFdPropertiesKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMemoryFdPropertiesKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryTypeBits" Word32 'False 
                                                        #{offset VkMemoryFdPropertiesKHR, memoryTypeBits}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkMemoryGetFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceMemory                   memory;
--   >     VkExternalMemoryHandleTypeFlagBits handleType;
--   > } VkMemoryGetFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryGetFdInfoKHR VkMemoryGetFdInfoKHR registry at www.khronos.org>
type VkMemoryGetFdInfoKHR = VkStruct VkMemoryGetFdInfoKHR' -- ' closing tick for hsc2hs

data VkMemoryGetFdInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMemoryGetFdInfoKHR where
    type StructRep VkMemoryGetFdInfoKHR =
         'StructMeta "VkMemoryGetFdInfoKHR" VkMemoryGetFdInfoKHR  -- ' closing tick for hsc2hs
                                                                 #{size VkMemoryGetFdInfoKHR}
           #{alignment VkMemoryGetFdInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMemoryGetFdInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMemoryGetFdInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memory" VkDeviceMemory 'False 
                                                        #{offset VkMemoryGetFdInfoKHR, memory}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalMemoryHandleTypeFlagBits 'False
                #{offset VkMemoryGetFdInfoKHR, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkMemoryHeap {
--   >     VkDeviceSize           size;
--   >     VkMemoryHeapFlags      flags;
--   > } VkMemoryHeap;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryHeap VkMemoryHeap registry at www.khronos.org>
type VkMemoryHeap = VkStruct VkMemoryHeap' -- ' closing tick for hsc2hs

data VkMemoryHeap' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMemoryHeap where
    type StructRep VkMemoryHeap =
         'StructMeta "VkMemoryHeap" VkMemoryHeap  -- ' closing tick for hsc2hs
                                                 #{size VkMemoryHeap}
           #{alignment VkMemoryHeap}
           '[('FieldMeta "size" VkDeviceSize 'False  -- ' closing tick for hsc2hs
                                                    #{offset VkMemoryHeap, size}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkMemoryHeapFlags 'True 
                                                         #{offset VkMemoryHeap, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkMemoryHostPointerPropertiesEXT {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     uint32_t memoryTypeBits;
--   > } VkMemoryHostPointerPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryHostPointerPropertiesEXT VkMemoryHostPointerPropertiesEXT registry at www.khronos.org>
type VkMemoryHostPointerPropertiesEXT =
     VkStruct VkMemoryHostPointerPropertiesEXT' -- ' closing tick for hsc2hs

data VkMemoryHostPointerPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMemoryHostPointerPropertiesEXT where
    type StructRep VkMemoryHostPointerPropertiesEXT =
         'StructMeta "VkMemoryHostPointerPropertiesEXT" -- ' closing tick for hsc2hs
           VkMemoryHostPointerPropertiesEXT
           #{size VkMemoryHostPointerPropertiesEXT}
           #{alignment VkMemoryHostPointerPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMemoryHostPointerPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMemoryHostPointerPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryTypeBits" Word32 'False 
                                                        #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkMemoryRequirements {
--   >     VkDeviceSize           size;
--   >     VkDeviceSize           alignment;
--   >     uint32_t               memoryTypeBits;
--   > } VkMemoryRequirements;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryRequirements VkMemoryRequirements registry at www.khronos.org>
type VkMemoryRequirements = VkStruct VkMemoryRequirements' -- ' closing tick for hsc2hs

data VkMemoryRequirements' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMemoryRequirements where
    type StructRep VkMemoryRequirements =
         'StructMeta "VkMemoryRequirements" VkMemoryRequirements  -- ' closing tick for hsc2hs
                                                                 #{size VkMemoryRequirements}
           #{alignment VkMemoryRequirements}
           '[('FieldMeta "size" VkDeviceSize 'False  -- ' closing tick for hsc2hs
                                                    #{offset VkMemoryRequirements, size}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "alignment" VkDeviceSize 'False 
                                                         #{offset VkMemoryRequirements, alignment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryTypeBits" Word32 'False 
                                                        #{offset VkMemoryRequirements, memoryTypeBits}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkMemoryRequirements2 {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkMemoryRequirements                                                 memoryRequirements;
--   > } VkMemoryRequirements2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryRequirements2 VkMemoryRequirements2 registry at www.khronos.org>
type VkMemoryRequirements2 = VkStruct VkMemoryRequirements2' -- ' closing tick for hsc2hs

data VkMemoryRequirements2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMemoryRequirements2 where
    type StructRep VkMemoryRequirements2 =
         'StructMeta "VkMemoryRequirements2" VkMemoryRequirements2  -- ' closing tick for hsc2hs
                                                                   #{size VkMemoryRequirements2}
           #{alignment VkMemoryRequirements2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMemoryRequirements2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMemoryRequirements2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryRequirements" VkMemoryRequirements 'False
                #{offset VkMemoryRequirements2, memoryRequirements}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkMemoryRequirements2`
type VkMemoryRequirements2KHR = VkMemoryRequirements2

-- | > typedef struct VkMemoryType {
--   >     VkMemoryPropertyFlags  propertyFlags;
--   >     uint32_t               heapIndex;
--   > } VkMemoryType;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryType VkMemoryType registry at www.khronos.org>
type VkMemoryType = VkStruct VkMemoryType' -- ' closing tick for hsc2hs

data VkMemoryType' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMemoryType where
    type StructRep VkMemoryType =
         'StructMeta "VkMemoryType" VkMemoryType  -- ' closing tick for hsc2hs
                                                 #{size VkMemoryType}
           #{alignment VkMemoryType}
           '[('FieldMeta "propertyFlags" VkMemoryPropertyFlags 'True  -- ' closing tick for hsc2hs
                                                                     #{offset VkMemoryType, propertyFlags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "heapIndex" Word32 'False 
                                                   #{offset VkMemoryType, heapIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
