#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Image
       (VkImageBlit, VkImageCopy, VkImageCreateInfo,
        VkImageDrmFormatModifierExplicitCreateInfoEXT,
        VkImageDrmFormatModifierListCreateInfoEXT,
        VkImageDrmFormatModifierPropertiesEXT, VkImageFormatListCreateInfo,
        VkImageFormatListCreateInfoKHR, VkImageFormatProperties,
        VkImageFormatProperties2, VkImageFormatProperties2KHR,
        VkImageMemoryBarrier, VkImageMemoryRequirementsInfo2,
        VkImageMemoryRequirementsInfo2KHR,
        VkImagePlaneMemoryRequirementsInfo,
        VkImagePlaneMemoryRequirementsInfoKHR, VkImageResolve,
        VkImageSparseMemoryRequirementsInfo2,
        VkImageSparseMemoryRequirementsInfo2KHR,
        VkImageStencilUsageCreateInfo, VkImageStencilUsageCreateInfoEXT,
        VkImageSubresource, VkImageSubresourceLayers,
        VkImageSubresourceRange, VkImageSwapchainCreateInfoKHR,
        VkImageViewASTCDecodeModeEXT, VkImageViewAddressPropertiesNVX,
        VkImageViewCreateInfo, VkImageViewHandleInfoNVX,
        VkImageViewUsageCreateInfo, VkImageViewUsageCreateInfoKHR)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.BaseTypes                (VkDeviceAddress,
                                                       VkDeviceSize)
import Graphics.Vulkan.Types.Enum.AccessFlags         (VkAccessFlags)
import Graphics.Vulkan.Types.Enum.Descriptor          (VkDescriptorType)
import Graphics.Vulkan.Types.Enum.Format              (VkFormat)
import Graphics.Vulkan.Types.Enum.Image               (VkImageAspectFlagBits,
                                                       VkImageAspectFlags,
                                                       VkImageCreateFlags,
                                                       VkImageLayout,
                                                       VkImageTiling,
                                                       VkImageType,
                                                       VkImageUsageFlags,
                                                       VkImageViewCreateFlags,
                                                       VkImageViewType)
import Graphics.Vulkan.Types.Enum.SampleCountFlags    (VkSampleCountFlagBits,
                                                       VkSampleCountFlags)
import Graphics.Vulkan.Types.Enum.SharingMode         (VkSharingMode)
import Graphics.Vulkan.Types.Enum.StructureType       (VkStructureType)
import Graphics.Vulkan.Types.Handles                  (VkImage, VkImageView,
                                                       VkSampler,
                                                       VkSwapchainKHR)
import Graphics.Vulkan.Types.Struct.ComponentMapping  (VkComponentMapping)
import Graphics.Vulkan.Types.Struct.Extent            (VkExtent3D)
import Graphics.Vulkan.Types.Struct.Offset            (VkOffset3D)
import Graphics.Vulkan.Types.Struct.PhysicalDevice    (VkPhysicalDeviceImageFormatInfo2)
import Graphics.Vulkan.Types.Struct.SubresourceLayout (VkSubresourceLayout)
import Graphics.Vulkan.Types.Struct.Swapchain         (VkSwapchainCreateInfoKHR)

-- | > typedef struct VkImageBlit {
--   >     VkImageSubresourceLayers srcSubresource;
--   >     VkOffset3D             srcOffsets[2];
--   >     VkImageSubresourceLayers dstSubresource;
--   >     VkOffset3D             dstOffsets[2];
--   > } VkImageBlit;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageBlit VkImageBlit registry at www.khronos.org>
type VkImageBlit = VkStruct VkImageBlit' -- ' closing tick for hsc2hs

data VkImageBlit' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageBlit where
    type StructRep VkImageBlit =
         'StructMeta "VkImageBlit" VkImageBlit  -- ' closing tick for hsc2hs
                                               #{size VkImageBlit}
           #{alignment VkImageBlit}
           '[('FieldMeta "srcSubresource" VkImageSubresourceLayers 'False -- ' closing tick for hsc2hs
                #{offset VkImageBlit, srcSubresource}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcOffsets" VkOffset3D 'False 
                                                        #{offset VkImageBlit, srcOffsets}
                2
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstSubresource" VkImageSubresourceLayers 'False
                #{offset VkImageBlit, dstSubresource}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstOffsets" VkOffset3D 'False 
                                                        #{offset VkImageBlit, dstOffsets}
                2
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageCopy {
--   >     VkImageSubresourceLayers srcSubresource;
--   >     VkOffset3D             srcOffset;
--   >     VkImageSubresourceLayers dstSubresource;
--   >     VkOffset3D             dstOffset;
--   >     VkExtent3D             extent;
--   > } VkImageCopy;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageCopy VkImageCopy registry at www.khronos.org>
type VkImageCopy = VkStruct VkImageCopy' -- ' closing tick for hsc2hs

data VkImageCopy' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageCopy where
    type StructRep VkImageCopy =
         'StructMeta "VkImageCopy" VkImageCopy  -- ' closing tick for hsc2hs
                                               #{size VkImageCopy}
           #{alignment VkImageCopy}
           '[('FieldMeta "srcSubresource" VkImageSubresourceLayers 'False -- ' closing tick for hsc2hs
                #{offset VkImageCopy, srcSubresource}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcOffset" VkOffset3D 'False 
                                                       #{offset VkImageCopy, srcOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstSubresource" VkImageSubresourceLayers 'False
                #{offset VkImageCopy, dstSubresource}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstOffset" VkOffset3D 'False 
                                                       #{offset VkImageCopy, dstOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "extent" VkExtent3D 'False 
                                                    #{offset VkImageCopy, extent}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkImageCreateFlags     flags;
--   >     VkImageType            imageType;
--   >     VkFormat               format;
--   >     VkExtent3D             extent;
--   >     uint32_t               mipLevels;
--   >     uint32_t               arrayLayers;
--   >     VkSampleCountFlagBits  samples;
--   >     VkImageTiling          tiling;
--   >     VkImageUsageFlags      usage;
--   >     VkSharingMode          sharingMode;
--   >     uint32_t               queueFamilyIndexCount;
--   >     const uint32_t*        pQueueFamilyIndices;
--   >     VkImageLayout          initialLayout;
--   > } VkImageCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageCreateInfo VkImageCreateInfo registry at www.khronos.org>
type VkImageCreateInfo = VkStruct VkImageCreateInfo' -- ' closing tick for hsc2hs

data VkImageCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageCreateInfo where
    type StructRep VkImageCreateInfo =
         'StructMeta "VkImageCreateInfo" VkImageCreateInfo  -- ' closing tick for hsc2hs
                                                           #{size VkImageCreateInfo}
           #{alignment VkImageCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImageCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImageCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkImageCreateFlags 'True 
                                                          #{offset VkImageCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageType" VkImageType 'False 
                                                        #{offset VkImageCreateInfo, imageType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "format" VkFormat 'False 
                                                  #{offset VkImageCreateInfo, format}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "extent" VkExtent3D 'False 
                                                    #{offset VkImageCreateInfo, extent}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "mipLevels" Word32 'False 
                                                   #{offset VkImageCreateInfo, mipLevels}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "arrayLayers" Word32 'False 
                                                     #{offset VkImageCreateInfo, arrayLayers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "samples" VkSampleCountFlagBits 'False 
                                                                #{offset VkImageCreateInfo, samples}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "tiling" VkImageTiling 'False 
                                                       #{offset VkImageCreateInfo, tiling}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "usage" VkImageUsageFlags 'False 
                                                          #{offset VkImageCreateInfo, usage}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sharingMode" VkSharingMode 'False 
                                                            #{offset VkImageCreateInfo, sharingMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queueFamilyIndexCount" Word32 'True 
                                                              #{offset VkImageCreateInfo, queueFamilyIndexCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pQueueFamilyIndices" (Ptr Word32) 'False 
                                                                   #{offset VkImageCreateInfo, pQueueFamilyIndices}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "initialLayout" VkImageLayout 'False 
                                                              #{offset VkImageCreateInfo, initialLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageDrmFormatModifierExplicitCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     uint64_t drmFormatModifier;
--   >     uint32_t drmFormatModifierPlaneCount;
--   >     const VkSubresourceLayout* pPlaneLayouts;
--   > } VkImageDrmFormatModifierExplicitCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageDrmFormatModifierExplicitCreateInfoEXT VkImageDrmFormatModifierExplicitCreateInfoEXT registry at www.khronos.org>
type VkImageDrmFormatModifierExplicitCreateInfoEXT =
     VkStruct VkImageDrmFormatModifierExplicitCreateInfoEXT' -- ' closing tick for hsc2hs

data VkImageDrmFormatModifierExplicitCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkImageDrmFormatModifierExplicitCreateInfoEXT
         where
    type StructRep VkImageDrmFormatModifierExplicitCreateInfoEXT =
         'StructMeta "VkImageDrmFormatModifierExplicitCreateInfoEXT" -- ' closing tick for hsc2hs
           VkImageDrmFormatModifierExplicitCreateInfoEXT
           #{size VkImageDrmFormatModifierExplicitCreateInfoEXT}
           #{alignment VkImageDrmFormatModifierExplicitCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImageDrmFormatModifierExplicitCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImageDrmFormatModifierExplicitCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "drmFormatModifier" Word64 'False 
                                                           #{offset VkImageDrmFormatModifierExplicitCreateInfoEXT, drmFormatModifier}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "drmFormatModifierPlaneCount" Word32 'False 
                                                                     #{offset VkImageDrmFormatModifierExplicitCreateInfoEXT, drmFormatModifierPlaneCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pPlaneLayouts" (Ptr VkSubresourceLayout) 'False
                #{offset VkImageDrmFormatModifierExplicitCreateInfoEXT, pPlaneLayouts}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkImageCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageDrmFormatModifierListCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     uint32_t drmFormatModifierCount;
--   >     const uint64_t* pDrmFormatModifiers;
--   > } VkImageDrmFormatModifierListCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageDrmFormatModifierListCreateInfoEXT VkImageDrmFormatModifierListCreateInfoEXT registry at www.khronos.org>
type VkImageDrmFormatModifierListCreateInfoEXT =
     VkStruct VkImageDrmFormatModifierListCreateInfoEXT' -- ' closing tick for hsc2hs

data VkImageDrmFormatModifierListCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageDrmFormatModifierListCreateInfoEXT
         where
    type StructRep VkImageDrmFormatModifierListCreateInfoEXT =
         'StructMeta "VkImageDrmFormatModifierListCreateInfoEXT" -- ' closing tick for hsc2hs
           VkImageDrmFormatModifierListCreateInfoEXT
           #{size VkImageDrmFormatModifierListCreateInfoEXT}
           #{alignment VkImageDrmFormatModifierListCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImageDrmFormatModifierListCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImageDrmFormatModifierListCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "drmFormatModifierCount" Word32 'False 
                                                                #{offset VkImageDrmFormatModifierListCreateInfoEXT, drmFormatModifierCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDrmFormatModifiers" (Ptr Word64) 'False 
                                                                   #{offset VkImageDrmFormatModifierListCreateInfoEXT, pDrmFormatModifiers}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkImageCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageDrmFormatModifierPropertiesEXT {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     uint64_t drmFormatModifier;
--   > } VkImageDrmFormatModifierPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageDrmFormatModifierPropertiesEXT VkImageDrmFormatModifierPropertiesEXT registry at www.khronos.org>
type VkImageDrmFormatModifierPropertiesEXT =
     VkStruct VkImageDrmFormatModifierPropertiesEXT' -- ' closing tick for hsc2hs

data VkImageDrmFormatModifierPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageDrmFormatModifierPropertiesEXT where
    type StructRep VkImageDrmFormatModifierPropertiesEXT =
         'StructMeta "VkImageDrmFormatModifierPropertiesEXT" -- ' closing tick for hsc2hs
           VkImageDrmFormatModifierPropertiesEXT
           #{size VkImageDrmFormatModifierPropertiesEXT}
           #{alignment VkImageDrmFormatModifierPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImageDrmFormatModifierPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImageDrmFormatModifierPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "drmFormatModifier" Word64 'False 
                                                           #{offset VkImageDrmFormatModifierPropertiesEXT, drmFormatModifier}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageFormatListCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     uint32_t               viewFormatCount;
--   >     const VkFormat*  pViewFormats;
--   > } VkImageFormatListCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageFormatListCreateInfo VkImageFormatListCreateInfo registry at www.khronos.org>
type VkImageFormatListCreateInfo =
     VkStruct VkImageFormatListCreateInfo' -- ' closing tick for hsc2hs

data VkImageFormatListCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageFormatListCreateInfo where
    type StructRep VkImageFormatListCreateInfo =
         'StructMeta "VkImageFormatListCreateInfo" -- ' closing tick for hsc2hs
           VkImageFormatListCreateInfo
           #{size VkImageFormatListCreateInfo}
           #{alignment VkImageFormatListCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImageFormatListCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImageFormatListCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "viewFormatCount" Word32 'True 
                                                        #{offset VkImageFormatListCreateInfo, viewFormatCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pViewFormats" (Ptr VkFormat) 'False 
                                                              #{offset VkImageFormatListCreateInfo, pViewFormats}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkImageCreateInfo, VkSwapchainCreateInfoKHR, -- ' closing tick for hsc2hs
             VkPhysicalDeviceImageFormatInfo2]

-- | Alias for `VkImageFormatListCreateInfo`
type VkImageFormatListCreateInfoKHR = VkImageFormatListCreateInfo

-- | > typedef struct VkImageFormatProperties {
--   >     VkExtent3D             maxExtent;
--   >     uint32_t               maxMipLevels;
--   >     uint32_t               maxArrayLayers;
--   >     VkSampleCountFlags     sampleCounts;
--   >     VkDeviceSize           maxResourceSize;
--   > } VkImageFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageFormatProperties VkImageFormatProperties registry at www.khronos.org>
type VkImageFormatProperties = VkStruct VkImageFormatProperties' -- ' closing tick for hsc2hs

data VkImageFormatProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageFormatProperties where
    type StructRep VkImageFormatProperties =
         'StructMeta "VkImageFormatProperties" VkImageFormatProperties -- ' closing tick for hsc2hs
           #{size VkImageFormatProperties}
           #{alignment VkImageFormatProperties}
           '[('FieldMeta "maxExtent" VkExtent3D 'False  -- ' closing tick for hsc2hs
                                                       #{offset VkImageFormatProperties, maxExtent}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxMipLevels" Word32 'False 
                                                      #{offset VkImageFormatProperties, maxMipLevels}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxArrayLayers" Word32 'False 
                                                        #{offset VkImageFormatProperties, maxArrayLayers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampleCounts" VkSampleCountFlags 'True 
                                                                 #{offset VkImageFormatProperties, sampleCounts}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxResourceSize" VkDeviceSize 'False 
                                                               #{offset VkImageFormatProperties, maxResourceSize}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageFormatProperties2 {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkImageFormatProperties          imageFormatProperties;
--   > } VkImageFormatProperties2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageFormatProperties2 VkImageFormatProperties2 registry at www.khronos.org>
type VkImageFormatProperties2 = VkStruct VkImageFormatProperties2' -- ' closing tick for hsc2hs

data VkImageFormatProperties2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageFormatProperties2 where
    type StructRep VkImageFormatProperties2 =
         'StructMeta "VkImageFormatProperties2" VkImageFormatProperties2 -- ' closing tick for hsc2hs
           #{size VkImageFormatProperties2}
           #{alignment VkImageFormatProperties2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImageFormatProperties2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImageFormatProperties2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageFormatProperties" VkImageFormatProperties 'False
                #{offset VkImageFormatProperties2, imageFormatProperties}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkImageFormatProperties2`
type VkImageFormatProperties2KHR = VkImageFormatProperties2

-- | > typedef struct VkImageMemoryBarrier {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkAccessFlags          srcAccessMask;
--   >     VkAccessFlags          dstAccessMask;
--   >     VkImageLayout          oldLayout;
--   >     VkImageLayout          newLayout;
--   >     uint32_t               srcQueueFamilyIndex;
--   >     uint32_t               dstQueueFamilyIndex;
--   >     VkImage                image;
--   >     VkImageSubresourceRange subresourceRange;
--   > } VkImageMemoryBarrier;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageMemoryBarrier VkImageMemoryBarrier registry at www.khronos.org>
type VkImageMemoryBarrier = VkStruct VkImageMemoryBarrier' -- ' closing tick for hsc2hs

data VkImageMemoryBarrier' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageMemoryBarrier where
    type StructRep VkImageMemoryBarrier =
         'StructMeta "VkImageMemoryBarrier" VkImageMemoryBarrier  -- ' closing tick for hsc2hs
                                                                 #{size VkImageMemoryBarrier}
           #{alignment VkImageMemoryBarrier}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImageMemoryBarrier, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImageMemoryBarrier, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcAccessMask" VkAccessFlags 'False 
                                                              #{offset VkImageMemoryBarrier, srcAccessMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstAccessMask" VkAccessFlags 'False 
                                                              #{offset VkImageMemoryBarrier, dstAccessMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "oldLayout" VkImageLayout 'False 
                                                          #{offset VkImageMemoryBarrier, oldLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "newLayout" VkImageLayout 'False 
                                                          #{offset VkImageMemoryBarrier, newLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcQueueFamilyIndex" Word32 'False 
                                                             #{offset VkImageMemoryBarrier, srcQueueFamilyIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstQueueFamilyIndex" Word32 'False 
                                                             #{offset VkImageMemoryBarrier, dstQueueFamilyIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "image" VkImage 'False 
                                                #{offset VkImageMemoryBarrier, image}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subresourceRange" VkImageSubresourceRange 'False
                #{offset VkImageMemoryBarrier, subresourceRange}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageMemoryRequirementsInfo2 {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkImage                                                              image;
--   > } VkImageMemoryRequirementsInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageMemoryRequirementsInfo2 VkImageMemoryRequirementsInfo2 registry at www.khronos.org>
type VkImageMemoryRequirementsInfo2 =
     VkStruct VkImageMemoryRequirementsInfo2' -- ' closing tick for hsc2hs

data VkImageMemoryRequirementsInfo2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageMemoryRequirementsInfo2 where
    type StructRep VkImageMemoryRequirementsInfo2 =
         'StructMeta "VkImageMemoryRequirementsInfo2" -- ' closing tick for hsc2hs
           VkImageMemoryRequirementsInfo2
           #{size VkImageMemoryRequirementsInfo2}
           #{alignment VkImageMemoryRequirementsInfo2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImageMemoryRequirementsInfo2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImageMemoryRequirementsInfo2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "image" VkImage 'False 
                                                #{offset VkImageMemoryRequirementsInfo2, image}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkImageMemoryRequirementsInfo2`
type VkImageMemoryRequirementsInfo2KHR =
     VkImageMemoryRequirementsInfo2

-- | > typedef struct VkImagePlaneMemoryRequirementsInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImageAspectFlagBits            planeAspect;
--   > } VkImagePlaneMemoryRequirementsInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImagePlaneMemoryRequirementsInfo VkImagePlaneMemoryRequirementsInfo registry at www.khronos.org>
type VkImagePlaneMemoryRequirementsInfo =
     VkStruct VkImagePlaneMemoryRequirementsInfo' -- ' closing tick for hsc2hs

data VkImagePlaneMemoryRequirementsInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImagePlaneMemoryRequirementsInfo where
    type StructRep VkImagePlaneMemoryRequirementsInfo =
         'StructMeta "VkImagePlaneMemoryRequirementsInfo" -- ' closing tick for hsc2hs
           VkImagePlaneMemoryRequirementsInfo
           #{size VkImagePlaneMemoryRequirementsInfo}
           #{alignment VkImagePlaneMemoryRequirementsInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImagePlaneMemoryRequirementsInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImagePlaneMemoryRequirementsInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "planeAspect" VkImageAspectFlagBits 'False 
                                                                    #{offset VkImagePlaneMemoryRequirementsInfo, planeAspect}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkImageMemoryRequirementsInfo2] -- ' closing tick for hsc2hs

-- | Alias for `VkImagePlaneMemoryRequirementsInfo`
type VkImagePlaneMemoryRequirementsInfoKHR =
     VkImagePlaneMemoryRequirementsInfo

-- | > typedef struct VkImageResolve {
--   >     VkImageSubresourceLayers srcSubresource;
--   >     VkOffset3D             srcOffset;
--   >     VkImageSubresourceLayers dstSubresource;
--   >     VkOffset3D             dstOffset;
--   >     VkExtent3D             extent;
--   > } VkImageResolve;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageResolve VkImageResolve registry at www.khronos.org>
type VkImageResolve = VkStruct VkImageResolve' -- ' closing tick for hsc2hs

data VkImageResolve' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageResolve where
    type StructRep VkImageResolve =
         'StructMeta "VkImageResolve" VkImageResolve  -- ' closing tick for hsc2hs
                                                     #{size VkImageResolve}
           #{alignment VkImageResolve}
           '[('FieldMeta "srcSubresource" VkImageSubresourceLayers 'False -- ' closing tick for hsc2hs
                #{offset VkImageResolve, srcSubresource}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcOffset" VkOffset3D 'False 
                                                       #{offset VkImageResolve, srcOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstSubresource" VkImageSubresourceLayers 'False
                #{offset VkImageResolve, dstSubresource}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstOffset" VkOffset3D 'False 
                                                       #{offset VkImageResolve, dstOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "extent" VkExtent3D 'False 
                                                    #{offset VkImageResolve, extent}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageSparseMemoryRequirementsInfo2 {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkImage                                                              image;
--   > } VkImageSparseMemoryRequirementsInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageSparseMemoryRequirementsInfo2 VkImageSparseMemoryRequirementsInfo2 registry at www.khronos.org>
type VkImageSparseMemoryRequirementsInfo2 =
     VkStruct VkImageSparseMemoryRequirementsInfo2' -- ' closing tick for hsc2hs

data VkImageSparseMemoryRequirementsInfo2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageSparseMemoryRequirementsInfo2 where
    type StructRep VkImageSparseMemoryRequirementsInfo2 =
         'StructMeta "VkImageSparseMemoryRequirementsInfo2" -- ' closing tick for hsc2hs
           VkImageSparseMemoryRequirementsInfo2
           #{size VkImageSparseMemoryRequirementsInfo2}
           #{alignment VkImageSparseMemoryRequirementsInfo2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImageSparseMemoryRequirementsInfo2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImageSparseMemoryRequirementsInfo2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "image" VkImage 'False 
                                                #{offset VkImageSparseMemoryRequirementsInfo2, image}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkImageSparseMemoryRequirementsInfo2`
type VkImageSparseMemoryRequirementsInfo2KHR =
     VkImageSparseMemoryRequirementsInfo2

-- | > typedef struct VkImageStencilUsageCreateInfo {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkImageUsageFlags stencilUsage;
--   > } VkImageStencilUsageCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo VkImageStencilUsageCreateInfo registry at www.khronos.org>
type VkImageStencilUsageCreateInfo =
     VkStruct VkImageStencilUsageCreateInfo' -- ' closing tick for hsc2hs

data VkImageStencilUsageCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageStencilUsageCreateInfo where
    type StructRep VkImageStencilUsageCreateInfo =
         'StructMeta "VkImageStencilUsageCreateInfo" -- ' closing tick for hsc2hs
           VkImageStencilUsageCreateInfo
           #{size VkImageStencilUsageCreateInfo}
           #{alignment VkImageStencilUsageCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImageStencilUsageCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImageStencilUsageCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stencilUsage" VkImageUsageFlags 'False 
                                                                 #{offset VkImageStencilUsageCreateInfo, stencilUsage}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkImageCreateInfo, VkPhysicalDeviceImageFormatInfo2] -- ' closing tick for hsc2hs

-- | Alias for `VkImageStencilUsageCreateInfo`
type VkImageStencilUsageCreateInfoEXT =
     VkImageStencilUsageCreateInfo

-- | > typedef struct VkImageSubresource {
--   >     VkImageAspectFlags     aspectMask;
--   >     uint32_t               mipLevel;
--   >     uint32_t               arrayLayer;
--   > } VkImageSubresource;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageSubresource VkImageSubresource registry at www.khronos.org>
type VkImageSubresource = VkStruct VkImageSubresource' -- ' closing tick for hsc2hs

data VkImageSubresource' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageSubresource where
    type StructRep VkImageSubresource =
         'StructMeta "VkImageSubresource" VkImageSubresource  -- ' closing tick for hsc2hs
                                                             #{size VkImageSubresource}
           #{alignment VkImageSubresource}
           '[('FieldMeta "aspectMask" VkImageAspectFlags 'False  -- ' closing tick for hsc2hs
                                                                #{offset VkImageSubresource, aspectMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "mipLevel" Word32 'False 
                                                  #{offset VkImageSubresource, mipLevel}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "arrayLayer" Word32 'False 
                                                    #{offset VkImageSubresource, arrayLayer}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageSubresourceLayers {
--   >     VkImageAspectFlags     aspectMask;
--   >     uint32_t               mipLevel;
--   >     uint32_t               baseArrayLayer;
--   >     uint32_t               layerCount;
--   > } VkImageSubresourceLayers;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageSubresourceLayers VkImageSubresourceLayers registry at www.khronos.org>
type VkImageSubresourceLayers = VkStruct VkImageSubresourceLayers' -- ' closing tick for hsc2hs

data VkImageSubresourceLayers' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageSubresourceLayers where
    type StructRep VkImageSubresourceLayers =
         'StructMeta "VkImageSubresourceLayers" VkImageSubresourceLayers -- ' closing tick for hsc2hs
           #{size VkImageSubresourceLayers}
           #{alignment VkImageSubresourceLayers}
           '[('FieldMeta "aspectMask" VkImageAspectFlags 'False  -- ' closing tick for hsc2hs
                                                                #{offset VkImageSubresourceLayers, aspectMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "mipLevel" Word32 'False 
                                                  #{offset VkImageSubresourceLayers, mipLevel}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "baseArrayLayer" Word32 'False 
                                                        #{offset VkImageSubresourceLayers, baseArrayLayer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "layerCount" Word32 'False 
                                                    #{offset VkImageSubresourceLayers, layerCount}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageSubresourceRange {
--   >     VkImageAspectFlags     aspectMask;
--   >     uint32_t               baseMipLevel;
--   >     uint32_t               levelCount;
--   >     uint32_t               baseArrayLayer;
--   >     uint32_t               layerCount;
--   > } VkImageSubresourceRange;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageSubresourceRange VkImageSubresourceRange registry at www.khronos.org>
type VkImageSubresourceRange = VkStruct VkImageSubresourceRange' -- ' closing tick for hsc2hs

data VkImageSubresourceRange' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageSubresourceRange where
    type StructRep VkImageSubresourceRange =
         'StructMeta "VkImageSubresourceRange" VkImageSubresourceRange -- ' closing tick for hsc2hs
           #{size VkImageSubresourceRange}
           #{alignment VkImageSubresourceRange}
           '[('FieldMeta "aspectMask" VkImageAspectFlags 'False  -- ' closing tick for hsc2hs
                                                                #{offset VkImageSubresourceRange, aspectMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "baseMipLevel" Word32 'False 
                                                      #{offset VkImageSubresourceRange, baseMipLevel}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "levelCount" Word32 'False 
                                                    #{offset VkImageSubresourceRange, levelCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "baseArrayLayer" Word32 'False 
                                                        #{offset VkImageSubresourceRange, baseArrayLayer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "layerCount" Word32 'False 
                                                    #{offset VkImageSubresourceRange, layerCount}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageSwapchainCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSwapchainKHR   swapchain;
--   > } VkImageSwapchainCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageSwapchainCreateInfoKHR VkImageSwapchainCreateInfoKHR registry at www.khronos.org>
type VkImageSwapchainCreateInfoKHR =
     VkStruct VkImageSwapchainCreateInfoKHR' -- ' closing tick for hsc2hs

data VkImageSwapchainCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageSwapchainCreateInfoKHR where
    type StructRep VkImageSwapchainCreateInfoKHR =
         'StructMeta "VkImageSwapchainCreateInfoKHR" -- ' closing tick for hsc2hs
           VkImageSwapchainCreateInfoKHR
           #{size VkImageSwapchainCreateInfoKHR}
           #{alignment VkImageSwapchainCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImageSwapchainCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImageSwapchainCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "swapchain" VkSwapchainKHR 'True 
                                                          #{offset VkImageSwapchainCreateInfoKHR, swapchain}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkImageCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageViewASTCDecodeModeEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkFormat                         decodeMode;
--   > } VkImageViewASTCDecodeModeEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageViewASTCDecodeModeEXT VkImageViewASTCDecodeModeEXT registry at www.khronos.org>
type VkImageViewASTCDecodeModeEXT =
     VkStruct VkImageViewASTCDecodeModeEXT' -- ' closing tick for hsc2hs

data VkImageViewASTCDecodeModeEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageViewASTCDecodeModeEXT where
    type StructRep VkImageViewASTCDecodeModeEXT =
         'StructMeta "VkImageViewASTCDecodeModeEXT" -- ' closing tick for hsc2hs
           VkImageViewASTCDecodeModeEXT
           #{size VkImageViewASTCDecodeModeEXT}
           #{alignment VkImageViewASTCDecodeModeEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImageViewASTCDecodeModeEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImageViewASTCDecodeModeEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "decodeMode" VkFormat 'False 
                                                      #{offset VkImageViewASTCDecodeModeEXT, decodeMode}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkImageViewCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageViewAddressPropertiesNVX {
--   >     VkStructureType sType;
--   >     void*              pNext;
--   >     VkDeviceAddress    deviceAddress;
--   >     VkDeviceSize       size;
--   > } VkImageViewAddressPropertiesNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageViewAddressPropertiesNVX VkImageViewAddressPropertiesNVX registry at www.khronos.org>
type VkImageViewAddressPropertiesNVX =
     VkStruct VkImageViewAddressPropertiesNVX' -- ' closing tick for hsc2hs

data VkImageViewAddressPropertiesNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageViewAddressPropertiesNVX where
    type StructRep VkImageViewAddressPropertiesNVX =
         'StructMeta "VkImageViewAddressPropertiesNVX" -- ' closing tick for hsc2hs
           VkImageViewAddressPropertiesNVX
           #{size VkImageViewAddressPropertiesNVX}
           #{alignment VkImageViewAddressPropertiesNVX}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImageViewAddressPropertiesNVX, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImageViewAddressPropertiesNVX, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceAddress" VkDeviceAddress 'False 
                                                                #{offset VkImageViewAddressPropertiesNVX, deviceAddress}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "size" VkDeviceSize 'False 
                                                    #{offset VkImageViewAddressPropertiesNVX, size}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageViewCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkImageViewCreateFlags flags;
--   >     VkImage                image;
--   >     VkImageViewType        viewType;
--   >     VkFormat               format;
--   >     VkComponentMapping     components;
--   >     VkImageSubresourceRange subresourceRange;
--   > } VkImageViewCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageViewCreateInfo VkImageViewCreateInfo registry at www.khronos.org>
type VkImageViewCreateInfo = VkStruct VkImageViewCreateInfo' -- ' closing tick for hsc2hs

data VkImageViewCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageViewCreateInfo where
    type StructRep VkImageViewCreateInfo =
         'StructMeta "VkImageViewCreateInfo" VkImageViewCreateInfo  -- ' closing tick for hsc2hs
                                                                   #{size VkImageViewCreateInfo}
           #{alignment VkImageViewCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImageViewCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImageViewCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkImageViewCreateFlags 'True 
                                                              #{offset VkImageViewCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "image" VkImage 'False 
                                                #{offset VkImageViewCreateInfo, image}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "viewType" VkImageViewType 'False 
                                                           #{offset VkImageViewCreateInfo, viewType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "format" VkFormat 'False 
                                                  #{offset VkImageViewCreateInfo, format}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "components" VkComponentMapping 'False 
                                                                #{offset VkImageViewCreateInfo, components}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subresourceRange" VkImageSubresourceRange 'False
                #{offset VkImageViewCreateInfo, subresourceRange}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageViewHandleInfoNVX {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkImageView                         imageView;
--   >     VkDescriptorType                    descriptorType;
--   >     VkSampler           sampler;
--   > } VkImageViewHandleInfoNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageViewHandleInfoNVX VkImageViewHandleInfoNVX registry at www.khronos.org>
type VkImageViewHandleInfoNVX = VkStruct VkImageViewHandleInfoNVX' -- ' closing tick for hsc2hs

data VkImageViewHandleInfoNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageViewHandleInfoNVX where
    type StructRep VkImageViewHandleInfoNVX =
         'StructMeta "VkImageViewHandleInfoNVX" VkImageViewHandleInfoNVX -- ' closing tick for hsc2hs
           #{size VkImageViewHandleInfoNVX}
           #{alignment VkImageViewHandleInfoNVX}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImageViewHandleInfoNVX, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImageViewHandleInfoNVX, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageView" VkImageView 'False 
                                                        #{offset VkImageViewHandleInfoNVX, imageView}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorType" VkDescriptorType 'False 
                                                                  #{offset VkImageViewHandleInfoNVX, descriptorType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampler" VkSampler 'True 
                                                   #{offset VkImageViewHandleInfoNVX, sampler}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkImageViewUsageCreateInfo {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkImageUsageFlags usage;
--   > } VkImageViewUsageCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageViewUsageCreateInfo VkImageViewUsageCreateInfo registry at www.khronos.org>
type VkImageViewUsageCreateInfo =
     VkStruct VkImageViewUsageCreateInfo' -- ' closing tick for hsc2hs

data VkImageViewUsageCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImageViewUsageCreateInfo where
    type StructRep VkImageViewUsageCreateInfo =
         'StructMeta "VkImageViewUsageCreateInfo" VkImageViewUsageCreateInfo -- ' closing tick for hsc2hs
           #{size VkImageViewUsageCreateInfo}
           #{alignment VkImageViewUsageCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImageViewUsageCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImageViewUsageCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "usage" VkImageUsageFlags 'False 
                                                          #{offset VkImageViewUsageCreateInfo, usage}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkImageViewCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkImageViewUsageCreateInfo`
type VkImageViewUsageCreateInfoKHR = VkImageViewUsageCreateInfo
