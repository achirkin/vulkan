#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Framebuffer
       (VkFramebufferAttachmentImageInfo,
        VkFramebufferAttachmentImageInfoKHR,
        VkFramebufferAttachmentsCreateInfo,
        VkFramebufferAttachmentsCreateInfoKHR, VkFramebufferCreateInfo,
        VkFramebufferMixedSamplesCombinationNV)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.Coverage               (VkCoverageReductionModeNV)
import Graphics.Vulkan.Types.Enum.Format                 (VkFormat)
import Graphics.Vulkan.Types.Enum.FramebufferCreateFlags (VkFramebufferCreateFlags)
import Graphics.Vulkan.Types.Enum.Image                  (VkImageCreateFlags,
                                                          VkImageUsageFlags)
import Graphics.Vulkan.Types.Enum.SampleCountFlags       (VkSampleCountFlagBits,
                                                          VkSampleCountFlags)
import Graphics.Vulkan.Types.Enum.StructureType          (VkStructureType)
import Graphics.Vulkan.Types.Handles                     (VkImageView,
                                                          VkRenderPass)

-- | > typedef struct VkFramebufferAttachmentImageInfo {
--   >     VkStructureType sType;
--   >     const void*                              pNext;
--   >     VkImageCreateFlags       flags;
--   >     VkImageUsageFlags                        usage;
--   >     uint32_t                                 width;
--   >     uint32_t                                 height;
--   >     uint32_t                                 layerCount;
--   >     uint32_t                 viewFormatCount;
--   >     const VkFormat*    pViewFormats;
--   > } VkFramebufferAttachmentImageInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFramebufferAttachmentImageInfo VkFramebufferAttachmentImageInfo registry at www.khronos.org>
type VkFramebufferAttachmentImageInfo =
     VkStruct VkFramebufferAttachmentImageInfo' -- ' closing tick for hsc2hs

data VkFramebufferAttachmentImageInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkFramebufferAttachmentImageInfo where
    type StructRep VkFramebufferAttachmentImageInfo =
         'StructMeta "VkFramebufferAttachmentImageInfo" -- ' closing tick for hsc2hs
           VkFramebufferAttachmentImageInfo
           #{size VkFramebufferAttachmentImageInfo}
           #{alignment VkFramebufferAttachmentImageInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkFramebufferAttachmentImageInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkFramebufferAttachmentImageInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkImageCreateFlags 'True 
                                                          #{offset VkFramebufferAttachmentImageInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "usage" VkImageUsageFlags 'False 
                                                          #{offset VkFramebufferAttachmentImageInfo, usage}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "width" Word32 'False 
                                               #{offset VkFramebufferAttachmentImageInfo, width}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "height" Word32 'False 
                                                #{offset VkFramebufferAttachmentImageInfo, height}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "layerCount" Word32 'False 
                                                    #{offset VkFramebufferAttachmentImageInfo, layerCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "viewFormatCount" Word32 'True 
                                                        #{offset VkFramebufferAttachmentImageInfo, viewFormatCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pViewFormats" (Ptr VkFormat) 'False 
                                                              #{offset VkFramebufferAttachmentImageInfo, pViewFormats}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkFramebufferAttachmentImageInfo`
type VkFramebufferAttachmentImageInfoKHR =
     VkFramebufferAttachmentImageInfo

-- | > typedef struct VkFramebufferAttachmentsCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                              pNext;
--   >     uint32_t                 attachmentImageInfoCount;
--   >     const VkFramebufferAttachmentImageInfo* pAttachmentImageInfos;
--   > } VkFramebufferAttachmentsCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFramebufferAttachmentsCreateInfo VkFramebufferAttachmentsCreateInfo registry at www.khronos.org>
type VkFramebufferAttachmentsCreateInfo =
     VkStruct VkFramebufferAttachmentsCreateInfo' -- ' closing tick for hsc2hs

data VkFramebufferAttachmentsCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkFramebufferAttachmentsCreateInfo where
    type StructRep VkFramebufferAttachmentsCreateInfo =
         'StructMeta "VkFramebufferAttachmentsCreateInfo" -- ' closing tick for hsc2hs
           VkFramebufferAttachmentsCreateInfo
           #{size VkFramebufferAttachmentsCreateInfo}
           #{alignment VkFramebufferAttachmentsCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkFramebufferAttachmentsCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkFramebufferAttachmentsCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "attachmentImageInfoCount" Word32 'True 
                                                                 #{offset VkFramebufferAttachmentsCreateInfo, attachmentImageInfoCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAttachmentImageInfos" -- ' closing tick for hsc2hs
                (Ptr VkFramebufferAttachmentImageInfo)
                'False -- ' closing tick for hsc2hs
                #{offset VkFramebufferAttachmentsCreateInfo, pAttachmentImageInfos}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkFramebufferCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkFramebufferAttachmentsCreateInfo`
type VkFramebufferAttachmentsCreateInfoKHR =
     VkFramebufferAttachmentsCreateInfo

-- | > typedef struct VkFramebufferCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkFramebufferCreateFlags    flags;
--   >     VkRenderPass           renderPass;
--   >     uint32_t               attachmentCount;
--   >     const VkImageView*     pAttachments;
--   >     uint32_t               width;
--   >     uint32_t               height;
--   >     uint32_t               layers;
--   > } VkFramebufferCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFramebufferCreateInfo VkFramebufferCreateInfo registry at www.khronos.org>
type VkFramebufferCreateInfo = VkStruct VkFramebufferCreateInfo' -- ' closing tick for hsc2hs

data VkFramebufferCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkFramebufferCreateInfo where
    type StructRep VkFramebufferCreateInfo =
         'StructMeta "VkFramebufferCreateInfo" VkFramebufferCreateInfo -- ' closing tick for hsc2hs
           #{size VkFramebufferCreateInfo}
           #{alignment VkFramebufferCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkFramebufferCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkFramebufferCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkFramebufferCreateFlags 'True 
                                                                #{offset VkFramebufferCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "renderPass" VkRenderPass 'False 
                                                          #{offset VkFramebufferCreateInfo, renderPass}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "attachmentCount" Word32 'True 
                                                        #{offset VkFramebufferCreateInfo, attachmentCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAttachments" (Ptr VkImageView) 'False 
                                                                 #{offset VkFramebufferCreateInfo, pAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "width" Word32 'False 
                                               #{offset VkFramebufferCreateInfo, width}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "height" Word32 'False 
                                                #{offset VkFramebufferCreateInfo, height}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "layers" Word32 'False 
                                                #{offset VkFramebufferCreateInfo, layers}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkFramebufferMixedSamplesCombinationNV {
--   >     VkStructureType sType;
--   >     void*                      pNext;
--   >     VkCoverageReductionModeNV  coverageReductionMode;
--   >     VkSampleCountFlagBits      rasterizationSamples;
--   >     VkSampleCountFlags         depthStencilSamples;
--   >     VkSampleCountFlags         colorSamples;
--   > } VkFramebufferMixedSamplesCombinationNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFramebufferMixedSamplesCombinationNV VkFramebufferMixedSamplesCombinationNV registry at www.khronos.org>
type VkFramebufferMixedSamplesCombinationNV =
     VkStruct VkFramebufferMixedSamplesCombinationNV' -- ' closing tick for hsc2hs

data VkFramebufferMixedSamplesCombinationNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkFramebufferMixedSamplesCombinationNV where
    type StructRep VkFramebufferMixedSamplesCombinationNV =
         'StructMeta "VkFramebufferMixedSamplesCombinationNV" -- ' closing tick for hsc2hs
           VkFramebufferMixedSamplesCombinationNV
           #{size VkFramebufferMixedSamplesCombinationNV}
           #{alignment VkFramebufferMixedSamplesCombinationNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkFramebufferMixedSamplesCombinationNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkFramebufferMixedSamplesCombinationNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "coverageReductionMode" VkCoverageReductionModeNV -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkFramebufferMixedSamplesCombinationNV, coverageReductionMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "rasterizationSamples" VkSampleCountFlagBits 'False
                #{offset VkFramebufferMixedSamplesCombinationNV, rasterizationSamples}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthStencilSamples" VkSampleCountFlags 'False
                #{offset VkFramebufferMixedSamplesCombinationNV, depthStencilSamples}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "colorSamples" VkSampleCountFlags 'False 
                                                                  #{offset VkFramebufferMixedSamplesCombinationNV, colorSamples}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
