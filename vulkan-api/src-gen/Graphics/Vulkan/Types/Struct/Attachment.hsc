#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Attachment
       (VkAttachmentDescription, VkAttachmentReference,
        VkAttachmentSampleLocationsEXT)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Attachment       (VkAttachmentDescriptionFlags,
                                                              VkAttachmentLoadOp,
                                                              VkAttachmentStoreOp)
import           Graphics.Vulkan.Types.Enum.Format           (VkFormat)
import           Graphics.Vulkan.Types.Enum.Image            (VkImageLayout)
import           Graphics.Vulkan.Types.Enum.SampleCountFlags (VkSampleCountFlagBits)
import           Graphics.Vulkan.Types.Struct.SampleLocation (VkSampleLocationsInfoEXT)

-- | > typedef struct VkAttachmentDescription {
--   >     VkAttachmentDescriptionFlags flags;
--   >     VkFormat               format;
--   >     VkSampleCountFlagBits  samples;
--   >     VkAttachmentLoadOp     loadOp;
--   >     VkAttachmentStoreOp    storeOp;
--   >     VkAttachmentLoadOp     stencilLoadOp;
--   >     VkAttachmentStoreOp    stencilStoreOp;
--   >     VkImageLayout          initialLayout;
--   >     VkImageLayout          finalLayout;
--   > } VkAttachmentDescription;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAttachmentDescription VkAttachmentDescription registry at www.khronos.org>
type VkAttachmentDescription = VkStruct VkAttachmentDescription' -- ' closing tick for hsc2hs

data VkAttachmentDescription' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAttachmentDescription where
    type StructRep VkAttachmentDescription =
         'StructMeta "VkAttachmentDescription" VkAttachmentDescription -- ' closing tick for hsc2hs
           #{size VkAttachmentDescription}
           #{alignment VkAttachmentDescription}
           '[('FieldMeta "flags" VkAttachmentDescriptionFlags 'True  -- ' closing tick for hsc2hs
                                                                    #{offset VkAttachmentDescription, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "format" VkFormat 'False 
                                                  #{offset VkAttachmentDescription, format}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "samples" VkSampleCountFlagBits 'False 
                                                                #{offset VkAttachmentDescription, samples}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "loadOp" VkAttachmentLoadOp 'False 
                                                            #{offset VkAttachmentDescription, loadOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "storeOp" VkAttachmentStoreOp 'False 
                                                              #{offset VkAttachmentDescription, storeOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stencilLoadOp" VkAttachmentLoadOp 'False 
                                                                   #{offset VkAttachmentDescription, stencilLoadOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stencilStoreOp" VkAttachmentStoreOp 'False 
                                                                     #{offset VkAttachmentDescription, stencilStoreOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "initialLayout" VkImageLayout 'False 
                                                              #{offset VkAttachmentDescription, initialLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "finalLayout" VkImageLayout 'False 
                                                            #{offset VkAttachmentDescription, finalLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkAttachmentReference {
--   >     uint32_t               attachment;
--   >     VkImageLayout          layout;
--   > } VkAttachmentReference;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAttachmentReference VkAttachmentReference registry at www.khronos.org>
type VkAttachmentReference = VkStruct VkAttachmentReference' -- ' closing tick for hsc2hs

data VkAttachmentReference' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAttachmentReference where
    type StructRep VkAttachmentReference =
         'StructMeta "VkAttachmentReference" VkAttachmentReference  -- ' closing tick for hsc2hs
                                                                   #{size VkAttachmentReference}
           #{alignment VkAttachmentReference}
           '[('FieldMeta "attachment" Word32 'False  -- ' closing tick for hsc2hs
                                                    #{offset VkAttachmentReference, attachment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "layout" VkImageLayout 'False 
                                                       #{offset VkAttachmentReference, layout}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkAttachmentSampleLocationsEXT {
--   >     uint32_t                         attachmentIndex;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkAttachmentSampleLocationsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAttachmentSampleLocationsEXT VkAttachmentSampleLocationsEXT registry at www.khronos.org>
type VkAttachmentSampleLocationsEXT =
     VkStruct VkAttachmentSampleLocationsEXT' -- ' closing tick for hsc2hs

data VkAttachmentSampleLocationsEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAttachmentSampleLocationsEXT where
    type StructRep VkAttachmentSampleLocationsEXT =
         'StructMeta "VkAttachmentSampleLocationsEXT" -- ' closing tick for hsc2hs
           VkAttachmentSampleLocationsEXT
           #{size VkAttachmentSampleLocationsEXT}
           #{alignment VkAttachmentSampleLocationsEXT}
           '[('FieldMeta "attachmentIndex" Word32 'False  -- ' closing tick for hsc2hs
                                                         #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampleLocationsInfo" VkSampleLocationsInfoEXT 'False
                #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
