#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.FramebufferCreateInfo
       (VkFramebufferCreateInfo) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkFramebufferCreateFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkImageView,
                                                           VkRenderPass)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkFramebufferCreateInfo VkFramebufferCreateInfo registry at www.khronos.org>
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
