#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.InputAttachmentAspectReference
       (VkInputAttachmentAspectReference,
        VkInputAttachmentAspectReferenceKHR)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Image (VkImageAspectFlags)

-- | > typedef struct VkInputAttachmentAspectReference {
--   >     uint32_t                        subpass;
--   >     uint32_t                        inputAttachmentIndex;
--   >     VkImageAspectFlags              aspectMask;
--   > } VkInputAttachmentAspectReference;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkInputAttachmentAspectReference VkInputAttachmentAspectReference registry at www.khronos.org>
type VkInputAttachmentAspectReference =
     VkStruct VkInputAttachmentAspectReference' -- ' closing tick for hsc2hs

data VkInputAttachmentAspectReference' -- ' closing tick for hsc2hs

instance VulkanMarshal VkInputAttachmentAspectReference where
    type StructRep VkInputAttachmentAspectReference =
         'StructMeta "VkInputAttachmentAspectReference" -- ' closing tick for hsc2hs
           VkInputAttachmentAspectReference
           #{size VkInputAttachmentAspectReference}
           #{alignment VkInputAttachmentAspectReference}
           '[('FieldMeta "subpass" Word32 'False  -- ' closing tick for hsc2hs
                                                 #{offset VkInputAttachmentAspectReference, subpass}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "inputAttachmentIndex" Word32 'False 
                                                              #{offset VkInputAttachmentAspectReference, inputAttachmentIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "aspectMask" VkImageAspectFlags 'False 
                                                                #{offset VkInputAttachmentAspectReference, aspectMask}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkInputAttachmentAspectReference`
type VkInputAttachmentAspectReferenceKHR =
     VkInputAttachmentAspectReference
