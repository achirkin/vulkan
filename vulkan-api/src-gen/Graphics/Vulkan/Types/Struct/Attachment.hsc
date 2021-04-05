#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Attachment
       (VkAttachmentDescription, VkAttachmentDescription2,
        VkAttachmentDescription2KHR, VkAttachmentDescriptionStencilLayout,
        VkAttachmentDescriptionStencilLayoutKHR, VkAttachmentReference,
        VkAttachmentReference2, VkAttachmentReference2KHR,
        VkAttachmentReferenceStencilLayout,
        VkAttachmentReferenceStencilLayoutKHR,
        VkAttachmentSampleLocationsEXT)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.Attachment       (VkAttachmentDescriptionFlags,
                                                    VkAttachmentLoadOp,
                                                    VkAttachmentStoreOp)
import Graphics.Vulkan.Types.Enum.Format           (VkFormat)
import Graphics.Vulkan.Types.Enum.Image            (VkImageAspectFlags,
                                                    VkImageLayout)
import Graphics.Vulkan.Types.Enum.SampleCountFlags (VkSampleCountFlagBits)
import Graphics.Vulkan.Types.Enum.StructureType    (VkStructureType)
import {-# SOURCE #-} Graphics.Vulkan.Types.Struct.SampleLocation (VkSampleLocationsInfoEXT)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAttachmentDescription VkAttachmentDescription registry at www.khronos.org>
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

-- | > typedef struct VkAttachmentDescription2 {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkAttachmentDescriptionFlags flags;
--   >     VkFormat                                     format;
--   >     VkSampleCountFlagBits                        samples;
--   >     VkAttachmentLoadOp                           loadOp;
--   >     VkAttachmentStoreOp                          storeOp;
--   >     VkAttachmentLoadOp                           stencilLoadOp;
--   >     VkAttachmentStoreOp                          stencilStoreOp;
--   >     VkImageLayout                                initialLayout;
--   >     VkImageLayout                                finalLayout;
--   > } VkAttachmentDescription2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAttachmentDescription2 VkAttachmentDescription2 registry at www.khronos.org>
type VkAttachmentDescription2 = VkStruct VkAttachmentDescription2' -- ' closing tick for hsc2hs

data VkAttachmentDescription2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAttachmentDescription2 where
    type StructRep VkAttachmentDescription2 =
         'StructMeta "VkAttachmentDescription2" VkAttachmentDescription2 -- ' closing tick for hsc2hs
           #{size VkAttachmentDescription2}
           #{alignment VkAttachmentDescription2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAttachmentDescription2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkAttachmentDescription2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkAttachmentDescriptionFlags 'True
                                                                    #{offset VkAttachmentDescription2, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "format" VkFormat 'False
                                                  #{offset VkAttachmentDescription2, format}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "samples" VkSampleCountFlagBits 'False
                                                                #{offset VkAttachmentDescription2, samples}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "loadOp" VkAttachmentLoadOp 'False
                                                            #{offset VkAttachmentDescription2, loadOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "storeOp" VkAttachmentStoreOp 'False
                                                              #{offset VkAttachmentDescription2, storeOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stencilLoadOp" VkAttachmentLoadOp 'False
                                                                   #{offset VkAttachmentDescription2, stencilLoadOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stencilStoreOp" VkAttachmentStoreOp 'False
                                                                     #{offset VkAttachmentDescription2, stencilStoreOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "initialLayout" VkImageLayout 'False
                                                              #{offset VkAttachmentDescription2, initialLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "finalLayout" VkImageLayout 'False
                                                            #{offset VkAttachmentDescription2, finalLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkAttachmentDescription2`
type VkAttachmentDescription2KHR = VkAttachmentDescription2

-- | > typedef struct VkAttachmentDescriptionStencilLayout {
--   >     VkStructureTypesType;
--   >     void*    pNext;
--   >     VkImageLayout                  stencilInitialLayout;
--   >     VkImageLayout                  stencilFinalLayout;
--   > } VkAttachmentDescriptionStencilLayout;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAttachmentDescriptionStencilLayout VkAttachmentDescriptionStencilLayout registry at www.khronos.org>
type VkAttachmentDescriptionStencilLayout =
     VkStruct VkAttachmentDescriptionStencilLayout' -- ' closing tick for hsc2hs

data VkAttachmentDescriptionStencilLayout' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAttachmentDescriptionStencilLayout where
    type StructRep VkAttachmentDescriptionStencilLayout =
         'StructMeta "VkAttachmentDescriptionStencilLayout" -- ' closing tick for hsc2hs
           VkAttachmentDescriptionStencilLayout
           #{size VkAttachmentDescriptionStencilLayout}
           #{alignment VkAttachmentDescriptionStencilLayout}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAttachmentDescriptionStencilLayout, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkAttachmentDescriptionStencilLayout, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stencilInitialLayout" VkImageLayout 'False
                                                                     #{offset VkAttachmentDescriptionStencilLayout, stencilInitialLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stencilFinalLayout" VkImageLayout 'False
                                                                   #{offset VkAttachmentDescriptionStencilLayout, stencilFinalLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkAttachmentDescription2] -- ' closing tick for hsc2hs

-- | Alias for `VkAttachmentDescriptionStencilLayout`
type VkAttachmentDescriptionStencilLayoutKHR =
     VkAttachmentDescriptionStencilLayout

-- | > typedef struct VkAttachmentReference {
--   >     uint32_t               attachment;
--   >     VkImageLayout          layout;
--   > } VkAttachmentReference;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAttachmentReference VkAttachmentReference registry at www.khronos.org>
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

-- | > typedef struct VkAttachmentReference2 {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     uint32_t                          attachment;
--   >     VkImageLayout                     layout;
--   >     VkImageAspectFlags aspectMask;
--   > } VkAttachmentReference2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAttachmentReference2 VkAttachmentReference2 registry at www.khronos.org>
type VkAttachmentReference2 = VkStruct VkAttachmentReference2' -- ' closing tick for hsc2hs

data VkAttachmentReference2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAttachmentReference2 where
    type StructRep VkAttachmentReference2 =
         'StructMeta "VkAttachmentReference2" VkAttachmentReference2 -- ' closing tick for hsc2hs
           #{size VkAttachmentReference2}
           #{alignment VkAttachmentReference2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAttachmentReference2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkAttachmentReference2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "attachment" Word32 'False
                                                    #{offset VkAttachmentReference2, attachment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "layout" VkImageLayout 'False
                                                       #{offset VkAttachmentReference2, layout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "aspectMask" VkImageAspectFlags 'False
                                                                #{offset VkAttachmentReference2, aspectMask}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkAttachmentReference2`
type VkAttachmentReference2KHR = VkAttachmentReference2

-- | > typedef struct VkAttachmentReferenceStencilLayout {
--   >     VkStructureTypesType;
--   >     void*    pNext;
--   >     VkImageLayout                  stencilLayout;
--   > } VkAttachmentReferenceStencilLayout;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAttachmentReferenceStencilLayout VkAttachmentReferenceStencilLayout registry at www.khronos.org>
type VkAttachmentReferenceStencilLayout =
     VkStruct VkAttachmentReferenceStencilLayout' -- ' closing tick for hsc2hs

data VkAttachmentReferenceStencilLayout' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAttachmentReferenceStencilLayout where
    type StructRep VkAttachmentReferenceStencilLayout =
         'StructMeta "VkAttachmentReferenceStencilLayout" -- ' closing tick for hsc2hs
           VkAttachmentReferenceStencilLayout
           #{size VkAttachmentReferenceStencilLayout}
           #{alignment VkAttachmentReferenceStencilLayout}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAttachmentReferenceStencilLayout, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkAttachmentReferenceStencilLayout, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stencilLayout" VkImageLayout 'False
                                                              #{offset VkAttachmentReferenceStencilLayout, stencilLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkAttachmentReference2] -- ' closing tick for hsc2hs

-- | Alias for `VkAttachmentReferenceStencilLayout`
type VkAttachmentReferenceStencilLayoutKHR =
     VkAttachmentReferenceStencilLayout

-- | > typedef struct VkAttachmentSampleLocationsEXT {
--   >     uint32_t                         attachmentIndex;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkAttachmentSampleLocationsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAttachmentSampleLocationsEXT VkAttachmentSampleLocationsEXT registry at www.khronos.org>
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
