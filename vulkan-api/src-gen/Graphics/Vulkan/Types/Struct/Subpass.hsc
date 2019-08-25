#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Subpass
       (VkSubpassDependency, VkSubpassDescription,
        VkSubpassSampleLocationsEXT)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.AccessFlags      (VkAccessFlags)
import           Graphics.Vulkan.Types.Enum.DependencyFlags  (VkDependencyFlags)
import           Graphics.Vulkan.Types.Enum.Pipeline         (VkPipelineBindPoint,
                                                              VkPipelineStageFlags)
import           Graphics.Vulkan.Types.Enum.Subpass          (VkSubpassDescriptionFlags)
import           Graphics.Vulkan.Types.Struct.Attachment     (VkAttachmentReference)
import           Graphics.Vulkan.Types.Struct.SampleLocation (VkSampleLocationsInfoEXT)

-- | > typedef struct VkSubpassDependency {
--   >     uint32_t               srcSubpass;
--   >     uint32_t               dstSubpass;
--   >     VkPipelineStageFlags   srcStageMask;
--   >     VkPipelineStageFlags   dstStageMask;
--   >     VkAccessFlags          srcAccessMask;
--   >     VkAccessFlags          dstAccessMask;
--   >     VkDependencyFlags      dependencyFlags;
--   > } VkSubpassDependency;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSubpassDependency VkSubpassDependency registry at www.khronos.org>
type VkSubpassDependency = VkStruct VkSubpassDependency' -- ' closing tick for hsc2hs

data VkSubpassDependency' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSubpassDependency where
    type StructRep VkSubpassDependency =
         'StructMeta "VkSubpassDependency" VkSubpassDependency  -- ' closing tick for hsc2hs
                                                               #{size VkSubpassDependency}
           #{alignment VkSubpassDependency}
           '[('FieldMeta "srcSubpass" Word32 'False  -- ' closing tick for hsc2hs
                                                    #{offset VkSubpassDependency, srcSubpass}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstSubpass" Word32 'False 
                                                    #{offset VkSubpassDependency, dstSubpass}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcStageMask" VkPipelineStageFlags 'False 
                                                                    #{offset VkSubpassDependency, srcStageMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstStageMask" VkPipelineStageFlags 'False 
                                                                    #{offset VkSubpassDependency, dstStageMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcAccessMask" VkAccessFlags 'True 
                                                             #{offset VkSubpassDependency, srcAccessMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstAccessMask" VkAccessFlags 'True 
                                                             #{offset VkSubpassDependency, dstAccessMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dependencyFlags" VkDependencyFlags 'True 
                                                                   #{offset VkSubpassDependency, dependencyFlags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSubpassDescription {
--   >     VkSubpassDescriptionFlags flags;
--   >     VkPipelineBindPoint    pipelineBindPoint;
--   >     uint32_t               inputAttachmentCount;
--   >     const VkAttachmentReference* pInputAttachments;
--   >     uint32_t               colorAttachmentCount;
--   >     const VkAttachmentReference* pColorAttachments;
--   >     const VkAttachmentReference* pResolveAttachments;
--   >     const VkAttachmentReference* pDepthStencilAttachment;
--   >     uint32_t               preserveAttachmentCount;
--   >     const uint32_t* pPreserveAttachments;
--   > } VkSubpassDescription;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSubpassDescription VkSubpassDescription registry at www.khronos.org>
type VkSubpassDescription = VkStruct VkSubpassDescription' -- ' closing tick for hsc2hs

data VkSubpassDescription' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSubpassDescription where
    type StructRep VkSubpassDescription =
         'StructMeta "VkSubpassDescription" VkSubpassDescription  -- ' closing tick for hsc2hs
                                                                 #{size VkSubpassDescription}
           #{alignment VkSubpassDescription}
           '[('FieldMeta "flags" VkSubpassDescriptionFlags 'True  -- ' closing tick for hsc2hs
                                                                 #{offset VkSubpassDescription, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineBindPoint" VkPipelineBindPoint 'False
                #{offset VkSubpassDescription, pipelineBindPoint}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "inputAttachmentCount" Word32 'True 
                                                             #{offset VkSubpassDescription, inputAttachmentCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pInputAttachments" (Ptr VkAttachmentReference) 'False
                #{offset VkSubpassDescription, pInputAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "colorAttachmentCount" Word32 'True 
                                                             #{offset VkSubpassDescription, colorAttachmentCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pColorAttachments" (Ptr VkAttachmentReference) 'False
                #{offset VkSubpassDescription, pColorAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pResolveAttachments" (Ptr VkAttachmentReference) 'True
                #{offset VkSubpassDescription, pResolveAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDepthStencilAttachment" (Ptr VkAttachmentReference) -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkSubpassDescription, pDepthStencilAttachment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "preserveAttachmentCount" Word32 'True 
                                                                #{offset VkSubpassDescription, preserveAttachmentCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pPreserveAttachments" (Ptr Word32) 'False 
                                                                    #{offset VkSubpassDescription, pPreserveAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkSubpassSampleLocationsEXT {
--   >     uint32_t                         subpassIndex;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkSubpassSampleLocationsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSubpassSampleLocationsEXT VkSubpassSampleLocationsEXT registry at www.khronos.org>
type VkSubpassSampleLocationsEXT =
     VkStruct VkSubpassSampleLocationsEXT' -- ' closing tick for hsc2hs

data VkSubpassSampleLocationsEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSubpassSampleLocationsEXT where
    type StructRep VkSubpassSampleLocationsEXT =
         'StructMeta "VkSubpassSampleLocationsEXT" -- ' closing tick for hsc2hs
           VkSubpassSampleLocationsEXT
           #{size VkSubpassSampleLocationsEXT}
           #{alignment VkSubpassSampleLocationsEXT}
           '[('FieldMeta "subpassIndex" Word32 'False  -- ' closing tick for hsc2hs
                                                      #{offset VkSubpassSampleLocationsEXT, subpassIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampleLocationsInfo" VkSampleLocationsInfoEXT 'False
                #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
