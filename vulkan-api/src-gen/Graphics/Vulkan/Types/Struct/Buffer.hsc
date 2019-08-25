#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Buffer
       (VkBufferCopy, VkBufferCreateInfo, VkBufferImageCopy,
        VkBufferMemoryBarrier, VkBufferMemoryRequirementsInfo2,
        VkBufferMemoryRequirementsInfo2KHR, VkBufferViewCreateInfo)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkDeviceSize)
import           Graphics.Vulkan.Types.Bitmasks           (VkBufferViewCreateFlags)
import           Graphics.Vulkan.Types.Enum.AccessFlags   (VkAccessFlags)
import           Graphics.Vulkan.Types.Enum.Buffer        (VkBufferCreateFlags,
                                                           VkBufferUsageFlags)
import           Graphics.Vulkan.Types.Enum.Format        (VkFormat)
import           Graphics.Vulkan.Types.Enum.SharingMode   (VkSharingMode)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkBuffer)
import           Graphics.Vulkan.Types.Struct.Extent      (VkExtent3D)
import           Graphics.Vulkan.Types.Struct.Image       (VkImageSubresourceLayers)
import           Graphics.Vulkan.Types.Struct.Offset      (VkOffset3D)

-- | > typedef struct VkBufferCopy {
--   >     VkDeviceSize           srcOffset;
--   >     VkDeviceSize           dstOffset;
--   >     VkDeviceSize           size;
--   > } VkBufferCopy;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBufferCopy VkBufferCopy registry at www.khronos.org>
type VkBufferCopy = VkStruct VkBufferCopy' -- ' closing tick for hsc2hs

data VkBufferCopy' -- ' closing tick for hsc2hs

instance VulkanMarshal VkBufferCopy where
    type StructRep VkBufferCopy =
         'StructMeta "VkBufferCopy" VkBufferCopy  -- ' closing tick for hsc2hs
                                                 #{size VkBufferCopy}
           #{alignment VkBufferCopy}
           '[('FieldMeta "srcOffset" VkDeviceSize 'False  -- ' closing tick for hsc2hs
                                                         #{offset VkBufferCopy, srcOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstOffset" VkDeviceSize 'False 
                                                         #{offset VkBufferCopy, dstOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "size" VkDeviceSize 'False 
                                                    #{offset VkBufferCopy, size}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkBufferCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkBufferCreateFlags    flags;
--   >     VkDeviceSize           size;
--   >     VkBufferUsageFlags     usage;
--   >     VkSharingMode          sharingMode;
--   >     uint32_t               queueFamilyIndexCount;
--   >     const uint32_t*        pQueueFamilyIndices;
--   > } VkBufferCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBufferCreateInfo VkBufferCreateInfo registry at www.khronos.org>
type VkBufferCreateInfo = VkStruct VkBufferCreateInfo' -- ' closing tick for hsc2hs

data VkBufferCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkBufferCreateInfo where
    type StructRep VkBufferCreateInfo =
         'StructMeta "VkBufferCreateInfo" VkBufferCreateInfo  -- ' closing tick for hsc2hs
                                                             #{size VkBufferCreateInfo}
           #{alignment VkBufferCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkBufferCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkBufferCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkBufferCreateFlags 'True 
                                                           #{offset VkBufferCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "size" VkDeviceSize 'False 
                                                    #{offset VkBufferCreateInfo, size}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "usage" VkBufferUsageFlags 'False 
                                                           #{offset VkBufferCreateInfo, usage}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sharingMode" VkSharingMode 'False 
                                                            #{offset VkBufferCreateInfo, sharingMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queueFamilyIndexCount" Word32 'True 
                                                              #{offset VkBufferCreateInfo, queueFamilyIndexCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pQueueFamilyIndices" (Ptr Word32) 'False 
                                                                   #{offset VkBufferCreateInfo, pQueueFamilyIndices}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkBufferImageCopy {
--   >     VkDeviceSize           bufferOffset;
--   >     uint32_t               bufferRowLength;
--   >     uint32_t               bufferImageHeight;
--   >     VkImageSubresourceLayers imageSubresource;
--   >     VkOffset3D             imageOffset;
--   >     VkExtent3D             imageExtent;
--   > } VkBufferImageCopy;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBufferImageCopy VkBufferImageCopy registry at www.khronos.org>
type VkBufferImageCopy = VkStruct VkBufferImageCopy' -- ' closing tick for hsc2hs

data VkBufferImageCopy' -- ' closing tick for hsc2hs

instance VulkanMarshal VkBufferImageCopy where
    type StructRep VkBufferImageCopy =
         'StructMeta "VkBufferImageCopy" VkBufferImageCopy  -- ' closing tick for hsc2hs
                                                           #{size VkBufferImageCopy}
           #{alignment VkBufferImageCopy}
           '[('FieldMeta "bufferOffset" VkDeviceSize 'False  -- ' closing tick for hsc2hs
                                                            #{offset VkBufferImageCopy, bufferOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bufferRowLength" Word32 'False 
                                                         #{offset VkBufferImageCopy, bufferRowLength}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bufferImageHeight" Word32 'False 
                                                           #{offset VkBufferImageCopy, bufferImageHeight}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageSubresource" VkImageSubresourceLayers 'False
                #{offset VkBufferImageCopy, imageSubresource}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageOffset" VkOffset3D 'False 
                                                         #{offset VkBufferImageCopy, imageOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageExtent" VkExtent3D 'False 
                                                         #{offset VkBufferImageCopy, imageExtent}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkBufferMemoryBarrier {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkAccessFlags          srcAccessMask;
--   >     VkAccessFlags          dstAccessMask;
--   >     uint32_t               srcQueueFamilyIndex;
--   >     uint32_t               dstQueueFamilyIndex;
--   >     VkBuffer               buffer;
--   >     VkDeviceSize           offset;
--   >     VkDeviceSize           size;
--   > } VkBufferMemoryBarrier;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBufferMemoryBarrier VkBufferMemoryBarrier registry at www.khronos.org>
type VkBufferMemoryBarrier = VkStruct VkBufferMemoryBarrier' -- ' closing tick for hsc2hs

data VkBufferMemoryBarrier' -- ' closing tick for hsc2hs

instance VulkanMarshal VkBufferMemoryBarrier where
    type StructRep VkBufferMemoryBarrier =
         'StructMeta "VkBufferMemoryBarrier" VkBufferMemoryBarrier  -- ' closing tick for hsc2hs
                                                                   #{size VkBufferMemoryBarrier}
           #{alignment VkBufferMemoryBarrier}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkBufferMemoryBarrier, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkBufferMemoryBarrier, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcAccessMask" VkAccessFlags 'True 
                                                             #{offset VkBufferMemoryBarrier, srcAccessMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstAccessMask" VkAccessFlags 'True 
                                                             #{offset VkBufferMemoryBarrier, dstAccessMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcQueueFamilyIndex" Word32 'False 
                                                             #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstQueueFamilyIndex" Word32 'False 
                                                             #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "buffer" VkBuffer 'False 
                                                  #{offset VkBufferMemoryBarrier, buffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "offset" VkDeviceSize 'False 
                                                      #{offset VkBufferMemoryBarrier, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "size" VkDeviceSize 'False 
                                                    #{offset VkBufferMemoryBarrier, size}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkBufferMemoryRequirementsInfo2 {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkBuffer                                                             buffer;
--   > } VkBufferMemoryRequirementsInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBufferMemoryRequirementsInfo2 VkBufferMemoryRequirementsInfo2 registry at www.khronos.org>
type VkBufferMemoryRequirementsInfo2 =
     VkStruct VkBufferMemoryRequirementsInfo2' -- ' closing tick for hsc2hs

data VkBufferMemoryRequirementsInfo2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkBufferMemoryRequirementsInfo2 where
    type StructRep VkBufferMemoryRequirementsInfo2 =
         'StructMeta "VkBufferMemoryRequirementsInfo2" -- ' closing tick for hsc2hs
           VkBufferMemoryRequirementsInfo2
           #{size VkBufferMemoryRequirementsInfo2}
           #{alignment VkBufferMemoryRequirementsInfo2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkBufferMemoryRequirementsInfo2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkBufferMemoryRequirementsInfo2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "buffer" VkBuffer 'False 
                                                  #{offset VkBufferMemoryRequirementsInfo2, buffer}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkBufferMemoryRequirementsInfo2`
type VkBufferMemoryRequirementsInfo2KHR =
     VkBufferMemoryRequirementsInfo2

-- | > typedef struct VkBufferViewCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkBufferViewCreateFlagsflags;
--   >     VkBuffer               buffer;
--   >     VkFormat               format;
--   >     VkDeviceSize           offset;
--   >     VkDeviceSize           range;
--   > } VkBufferViewCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBufferViewCreateInfo VkBufferViewCreateInfo registry at www.khronos.org>
type VkBufferViewCreateInfo = VkStruct VkBufferViewCreateInfo' -- ' closing tick for hsc2hs

data VkBufferViewCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkBufferViewCreateInfo where
    type StructRep VkBufferViewCreateInfo =
         'StructMeta "VkBufferViewCreateInfo" VkBufferViewCreateInfo -- ' closing tick for hsc2hs
           #{size VkBufferViewCreateInfo}
           #{alignment VkBufferViewCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkBufferViewCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkBufferViewCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkBufferViewCreateFlags 'True 
                                                               #{offset VkBufferViewCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "buffer" VkBuffer 'False 
                                                  #{offset VkBufferViewCreateInfo, buffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "format" VkFormat 'False 
                                                  #{offset VkBufferViewCreateInfo, format}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "offset" VkDeviceSize 'False 
                                                      #{offset VkBufferViewCreateInfo, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "range" VkDeviceSize 'False 
                                                     #{offset VkBufferViewCreateInfo, range}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
