#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Sparse
       (VkSparseBufferMemoryBindInfo, VkSparseImageFormatProperties,
        VkSparseImageFormatProperties2, VkSparseImageFormatProperties2KHR,
        VkSparseImageMemoryBind, VkSparseImageMemoryBindInfo,
        VkSparseImageMemoryRequirements, VkSparseImageMemoryRequirements2,
        VkSparseImageMemoryRequirements2KHR,
        VkSparseImageOpaqueMemoryBindInfo, VkSparseMemoryBind)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.Image         (VkImageAspectFlags)
import           Graphics.Vulkan.Types.Enum.Sparse        (VkSparseImageFormatFlags,
                                                           VkSparseMemoryBindFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkBuffer,
                                                           VkDeviceMemory,
                                                           VkImage)
import           Graphics.Vulkan.Types.Struct.Extent      (VkExtent3D)
import           Graphics.Vulkan.Types.Struct.Image       (VkImageSubresource)
import           Graphics.Vulkan.Types.Struct.Offset      (VkOffset3D)

-- | > typedef struct VkSparseBufferMemoryBindInfo {
--   >     VkBuffer buffer;
--   >     uint32_t               bindCount;
--   >     const VkSparseMemoryBind* pBinds;
--   > } VkSparseBufferMemoryBindInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseBufferMemoryBindInfo VkSparseBufferMemoryBindInfo registry at www.khronos.org>
type VkSparseBufferMemoryBindInfo =
     VkStruct VkSparseBufferMemoryBindInfo' -- ' closing tick for hsc2hs

data VkSparseBufferMemoryBindInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSparseBufferMemoryBindInfo where
    type StructRep VkSparseBufferMemoryBindInfo =
         'StructMeta "VkSparseBufferMemoryBindInfo" -- ' closing tick for hsc2hs
           VkSparseBufferMemoryBindInfo
           #{size VkSparseBufferMemoryBindInfo}
           #{alignment VkSparseBufferMemoryBindInfo}
           '[('FieldMeta "buffer" VkBuffer 'False  -- ' closing tick for hsc2hs
                                                  #{offset VkSparseBufferMemoryBindInfo, buffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bindCount" Word32 'False 
                                                   #{offset VkSparseBufferMemoryBindInfo, bindCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pBinds" (Ptr VkSparseMemoryBind) 'False 
                                                                  #{offset VkSparseBufferMemoryBindInfo, pBinds}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSparseImageFormatProperties {
--   >     VkImageAspectFlags     aspectMask;
--   >     VkExtent3D             imageGranularity;
--   >     VkSparseImageFormatFlags flags;
--   > } VkSparseImageFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageFormatProperties VkSparseImageFormatProperties registry at www.khronos.org>
type VkSparseImageFormatProperties =
     VkStruct VkSparseImageFormatProperties' -- ' closing tick for hsc2hs

data VkSparseImageFormatProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSparseImageFormatProperties where
    type StructRep VkSparseImageFormatProperties =
         'StructMeta "VkSparseImageFormatProperties" -- ' closing tick for hsc2hs
           VkSparseImageFormatProperties
           #{size VkSparseImageFormatProperties}
           #{alignment VkSparseImageFormatProperties}
           '[('FieldMeta "aspectMask" VkImageAspectFlags 'True  -- ' closing tick for hsc2hs
                                                               #{offset VkSparseImageFormatProperties, aspectMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageGranularity" VkExtent3D 'False 
                                                              #{offset VkSparseImageFormatProperties, imageGranularity}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkSparseImageFormatFlags 'True 
                                                                #{offset VkSparseImageFormatProperties, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSparseImageFormatProperties2 {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkSparseImageFormatProperties    properties;
--   > } VkSparseImageFormatProperties2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageFormatProperties2 VkSparseImageFormatProperties2 registry at www.khronos.org>
type VkSparseImageFormatProperties2 =
     VkStruct VkSparseImageFormatProperties2' -- ' closing tick for hsc2hs

data VkSparseImageFormatProperties2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSparseImageFormatProperties2 where
    type StructRep VkSparseImageFormatProperties2 =
         'StructMeta "VkSparseImageFormatProperties2" -- ' closing tick for hsc2hs
           VkSparseImageFormatProperties2
           #{size VkSparseImageFormatProperties2}
           #{alignment VkSparseImageFormatProperties2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSparseImageFormatProperties2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSparseImageFormatProperties2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "properties" VkSparseImageFormatProperties 'False
                #{offset VkSparseImageFormatProperties2, properties}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkSparseImageFormatProperties2`
type VkSparseImageFormatProperties2KHR =
     VkSparseImageFormatProperties2

-- | > typedef struct VkSparseImageMemoryBind {
--   >     VkImageSubresource     subresource;
--   >     VkOffset3D             offset;
--   >     VkExtent3D             extent;
--   >     VkDeviceMemory         memory;
--   >     VkDeviceSize           memoryOffset;
--   >     VkSparseMemoryBindFlagsflags;
--   > } VkSparseImageMemoryBind;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageMemoryBind VkSparseImageMemoryBind registry at www.khronos.org>
type VkSparseImageMemoryBind = VkStruct VkSparseImageMemoryBind' -- ' closing tick for hsc2hs

data VkSparseImageMemoryBind' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSparseImageMemoryBind where
    type StructRep VkSparseImageMemoryBind =
         'StructMeta "VkSparseImageMemoryBind" VkSparseImageMemoryBind -- ' closing tick for hsc2hs
           #{size VkSparseImageMemoryBind}
           #{alignment VkSparseImageMemoryBind}
           '[('FieldMeta "subresource" VkImageSubresource 'False  -- ' closing tick for hsc2hs
                                                                 #{offset VkSparseImageMemoryBind, subresource}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "offset" VkOffset3D 'False 
                                                    #{offset VkSparseImageMemoryBind, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "extent" VkExtent3D 'False 
                                                    #{offset VkSparseImageMemoryBind, extent}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memory" VkDeviceMemory 'True 
                                                       #{offset VkSparseImageMemoryBind, memory}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryOffset" VkDeviceSize 'False 
                                                            #{offset VkSparseImageMemoryBind, memoryOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkSparseMemoryBindFlags 'True 
                                                               #{offset VkSparseImageMemoryBind, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSparseImageMemoryBindInfo {
--   >     VkImage image;
--   >     uint32_t               bindCount;
--   >     const VkSparseImageMemoryBind* pBinds;
--   > } VkSparseImageMemoryBindInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageMemoryBindInfo VkSparseImageMemoryBindInfo registry at www.khronos.org>
type VkSparseImageMemoryBindInfo =
     VkStruct VkSparseImageMemoryBindInfo' -- ' closing tick for hsc2hs

data VkSparseImageMemoryBindInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSparseImageMemoryBindInfo where
    type StructRep VkSparseImageMemoryBindInfo =
         'StructMeta "VkSparseImageMemoryBindInfo" -- ' closing tick for hsc2hs
           VkSparseImageMemoryBindInfo
           #{size VkSparseImageMemoryBindInfo}
           #{alignment VkSparseImageMemoryBindInfo}
           '[('FieldMeta "image" VkImage 'False  -- ' closing tick for hsc2hs
                                                #{offset VkSparseImageMemoryBindInfo, image}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bindCount" Word32 'False 
                                                   #{offset VkSparseImageMemoryBindInfo, bindCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pBinds" (Ptr VkSparseImageMemoryBind) 'False 
                                                                       #{offset VkSparseImageMemoryBindInfo, pBinds}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSparseImageMemoryRequirements {
--   >     VkSparseImageFormatProperties formatProperties;
--   >     uint32_t               imageMipTailFirstLod;
--   >     VkDeviceSize           imageMipTailSize;
--   >     VkDeviceSize           imageMipTailOffset;
--   >     VkDeviceSize           imageMipTailStride;
--   > } VkSparseImageMemoryRequirements;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageMemoryRequirements VkSparseImageMemoryRequirements registry at www.khronos.org>
type VkSparseImageMemoryRequirements =
     VkStruct VkSparseImageMemoryRequirements' -- ' closing tick for hsc2hs

data VkSparseImageMemoryRequirements' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSparseImageMemoryRequirements where
    type StructRep VkSparseImageMemoryRequirements =
         'StructMeta "VkSparseImageMemoryRequirements" -- ' closing tick for hsc2hs
           VkSparseImageMemoryRequirements
           #{size VkSparseImageMemoryRequirements}
           #{alignment VkSparseImageMemoryRequirements}
           '[('FieldMeta "formatProperties" VkSparseImageFormatProperties
                'False -- ' closing tick for hsc2hs
                #{offset VkSparseImageMemoryRequirements, formatProperties}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageMipTailFirstLod" Word32 'False 
                                                              #{offset VkSparseImageMemoryRequirements, imageMipTailFirstLod}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageMipTailSize" VkDeviceSize 'False 
                                                                #{offset VkSparseImageMemoryRequirements, imageMipTailSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageMipTailOffset" VkDeviceSize 'False 
                                                                  #{offset VkSparseImageMemoryRequirements, imageMipTailOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageMipTailStride" VkDeviceSize 'False 
                                                                  #{offset VkSparseImageMemoryRequirements, imageMipTailStride}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSparseImageMemoryRequirements2 {
--   >     VkStructureType sType;
--   >     void*                                       pNext;
--   >     VkSparseImageMemoryRequirements                                      memoryRequirements;
--   > } VkSparseImageMemoryRequirements2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageMemoryRequirements2 VkSparseImageMemoryRequirements2 registry at www.khronos.org>
type VkSparseImageMemoryRequirements2 =
     VkStruct VkSparseImageMemoryRequirements2' -- ' closing tick for hsc2hs

data VkSparseImageMemoryRequirements2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSparseImageMemoryRequirements2 where
    type StructRep VkSparseImageMemoryRequirements2 =
         'StructMeta "VkSparseImageMemoryRequirements2" -- ' closing tick for hsc2hs
           VkSparseImageMemoryRequirements2
           #{size VkSparseImageMemoryRequirements2}
           #{alignment VkSparseImageMemoryRequirements2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSparseImageMemoryRequirements2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkSparseImageMemoryRequirements2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryRequirements" VkSparseImageMemoryRequirements -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkSparseImageMemoryRequirements2, memoryRequirements}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkSparseImageMemoryRequirements2`
type VkSparseImageMemoryRequirements2KHR =
     VkSparseImageMemoryRequirements2

-- | > typedef struct VkSparseImageOpaqueMemoryBindInfo {
--   >     VkImage image;
--   >     uint32_t               bindCount;
--   >     const VkSparseMemoryBind* pBinds;
--   > } VkSparseImageOpaqueMemoryBindInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageOpaqueMemoryBindInfo VkSparseImageOpaqueMemoryBindInfo registry at www.khronos.org>
type VkSparseImageOpaqueMemoryBindInfo =
     VkStruct VkSparseImageOpaqueMemoryBindInfo' -- ' closing tick for hsc2hs

data VkSparseImageOpaqueMemoryBindInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSparseImageOpaqueMemoryBindInfo where
    type StructRep VkSparseImageOpaqueMemoryBindInfo =
         'StructMeta "VkSparseImageOpaqueMemoryBindInfo" -- ' closing tick for hsc2hs
           VkSparseImageOpaqueMemoryBindInfo
           #{size VkSparseImageOpaqueMemoryBindInfo}
           #{alignment VkSparseImageOpaqueMemoryBindInfo}
           '[('FieldMeta "image" VkImage 'False  -- ' closing tick for hsc2hs
                                                #{offset VkSparseImageOpaqueMemoryBindInfo, image}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bindCount" Word32 'False 
                                                   #{offset VkSparseImageOpaqueMemoryBindInfo, bindCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pBinds" (Ptr VkSparseMemoryBind) 'False 
                                                                  #{offset VkSparseImageOpaqueMemoryBindInfo, pBinds}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSparseMemoryBind {
--   >     VkDeviceSize           resourceOffset;
--   >     VkDeviceSize           size;
--   >     VkDeviceMemory         memory;
--   >     VkDeviceSize           memoryOffset;
--   >     VkSparseMemoryBindFlagsflags;
--   > } VkSparseMemoryBind;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseMemoryBind VkSparseMemoryBind registry at www.khronos.org>
type VkSparseMemoryBind = VkStruct VkSparseMemoryBind' -- ' closing tick for hsc2hs

data VkSparseMemoryBind' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSparseMemoryBind where
    type StructRep VkSparseMemoryBind =
         'StructMeta "VkSparseMemoryBind" VkSparseMemoryBind  -- ' closing tick for hsc2hs
                                                             #{size VkSparseMemoryBind}
           #{alignment VkSparseMemoryBind}
           '[('FieldMeta "resourceOffset" VkDeviceSize 'False  -- ' closing tick for hsc2hs
                                                              #{offset VkSparseMemoryBind, resourceOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "size" VkDeviceSize 'False 
                                                    #{offset VkSparseMemoryBind, size}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memory" VkDeviceMemory 'True 
                                                       #{offset VkSparseMemoryBind, memory}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryOffset" VkDeviceSize 'False 
                                                            #{offset VkSparseMemoryBind, memoryOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkSparseMemoryBindFlags 'True 
                                                               #{offset VkSparseMemoryBind, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
