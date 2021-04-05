#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Subpass
       (VkSubpassBeginInfo, VkSubpassBeginInfoKHR, VkSubpassDependency,
        VkSubpassDependency2, VkSubpassDependency2KHR,
        VkSubpassDescription, VkSubpassDescription2,
        VkSubpassDescription2KHR, VkSubpassDescriptionDepthStencilResolve,
        VkSubpassDescriptionDepthStencilResolveKHR, VkSubpassEndInfo,
        VkSubpassEndInfoKHR, VkSubpassSampleLocationsEXT)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.AccessFlags      (VkAccessFlags)
import Graphics.Vulkan.Types.Enum.DependencyFlags  (VkDependencyFlags)
import Graphics.Vulkan.Types.Enum.Pipeline         (VkPipelineBindPoint,
                                                    VkPipelineStageFlags)
import Graphics.Vulkan.Types.Enum.ResolveModeFlag  (VkResolveModeFlagBits)
import Graphics.Vulkan.Types.Enum.StructureType    (VkStructureType)
import Graphics.Vulkan.Types.Enum.Subpass          (VkSubpassContents,
                                                    VkSubpassDescriptionFlags)
import Graphics.Vulkan.Types.Struct.Attachment     (VkAttachmentReference,
                                                    VkAttachmentReference2)
import {-# SOURCE #-} Graphics.Vulkan.Types.Struct.SampleLocation (VkSampleLocationsInfoEXT)

-- | > typedef struct VkSubpassBeginInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkSubpassContents      contents;
--   > } VkSubpassBeginInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSubpassBeginInfo VkSubpassBeginInfo registry at www.khronos.org>
type VkSubpassBeginInfo = VkStruct VkSubpassBeginInfo' -- ' closing tick for hsc2hs

data VkSubpassBeginInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSubpassBeginInfo where
    type StructRep VkSubpassBeginInfo =
         'StructMeta "VkSubpassBeginInfo" VkSubpassBeginInfo  -- ' closing tick for hsc2hs
                                                             #{size VkSubpassBeginInfo}
           #{alignment VkSubpassBeginInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSubpassBeginInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkSubpassBeginInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "contents" VkSubpassContents 'False
                                                             #{offset VkSubpassBeginInfo, contents}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkSubpassBeginInfo`
type VkSubpassBeginInfoKHR = VkSubpassBeginInfo

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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSubpassDependency VkSubpassDependency registry at www.khronos.org>
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

-- | > typedef struct VkSubpassDependency2 {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     uint32_t                          srcSubpass;
--   >     uint32_t                          dstSubpass;
--   >     VkPipelineStageFlags              srcStageMask;
--   >     VkPipelineStageFlags              dstStageMask;
--   >     VkAccessFlags     srcAccessMask;
--   >     VkAccessFlags     dstAccessMask;
--   >     VkDependencyFlags dependencyFlags;
--   >     int32_t           viewOffset;
--   > } VkSubpassDependency2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSubpassDependency2 VkSubpassDependency2 registry at www.khronos.org>
type VkSubpassDependency2 = VkStruct VkSubpassDependency2' -- ' closing tick for hsc2hs

data VkSubpassDependency2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSubpassDependency2 where
    type StructRep VkSubpassDependency2 =
         'StructMeta "VkSubpassDependency2" VkSubpassDependency2  -- ' closing tick for hsc2hs
                                                                 #{size VkSubpassDependency2}
           #{alignment VkSubpassDependency2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSubpassDependency2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkSubpassDependency2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcSubpass" Word32 'False
                                                    #{offset VkSubpassDependency2, srcSubpass}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstSubpass" Word32 'False
                                                    #{offset VkSubpassDependency2, dstSubpass}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcStageMask" VkPipelineStageFlags 'False
                                                                    #{offset VkSubpassDependency2, srcStageMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstStageMask" VkPipelineStageFlags 'False
                                                                    #{offset VkSubpassDependency2, dstStageMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcAccessMask" VkAccessFlags 'True
                                                             #{offset VkSubpassDependency2, srcAccessMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstAccessMask" VkAccessFlags 'True
                                                             #{offset VkSubpassDependency2, dstAccessMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dependencyFlags" VkDependencyFlags 'True
                                                                   #{offset VkSubpassDependency2, dependencyFlags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "viewOffset" Int32 'True
                                                  #{offset VkSubpassDependency2, viewOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkSubpassDependency2`
type VkSubpassDependency2KHR = VkSubpassDependency2

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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSubpassDescription VkSubpassDescription registry at www.khronos.org>
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

-- | > typedef struct VkSubpassDescription2 {
--   >     VkStructureType sType;
--   >     const void*                           pNext;
--   >     VkSubpassDescriptionFlags                   flags;
--   >     VkPipelineBindPoint                                         pipelineBindPoint;
--   >     uint32_t                                                    viewMask;
--   >     uint32_t                                    inputAttachmentCount;
--   >     const VkAttachmentReference2*    pInputAttachments;
--   >     uint32_t                                    colorAttachmentCount;
--   >     const VkAttachmentReference2*    pColorAttachments;
--   >     const VkAttachmentReference2* pResolveAttachments;
--   >     const VkAttachmentReference2*               pDepthStencilAttachment;
--   >     uint32_t                                    preserveAttachmentCount;
--   >     const uint32_t*               pPreserveAttachments;
--   > } VkSubpassDescription2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSubpassDescription2 VkSubpassDescription2 registry at www.khronos.org>
type VkSubpassDescription2 = VkStruct VkSubpassDescription2' -- ' closing tick for hsc2hs

data VkSubpassDescription2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSubpassDescription2 where
    type StructRep VkSubpassDescription2 =
         'StructMeta "VkSubpassDescription2" VkSubpassDescription2  -- ' closing tick for hsc2hs
                                                                   #{size VkSubpassDescription2}
           #{alignment VkSubpassDescription2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSubpassDescription2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkSubpassDescription2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkSubpassDescriptionFlags 'True
                                                                 #{offset VkSubpassDescription2, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineBindPoint" VkPipelineBindPoint 'False
                #{offset VkSubpassDescription2, pipelineBindPoint}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "viewMask" Word32 'False
                                                  #{offset VkSubpassDescription2, viewMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "inputAttachmentCount" Word32 'True
                                                             #{offset VkSubpassDescription2, inputAttachmentCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pInputAttachments" (Ptr VkAttachmentReference2) 'False
                #{offset VkSubpassDescription2, pInputAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "colorAttachmentCount" Word32 'True
                                                             #{offset VkSubpassDescription2, colorAttachmentCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pColorAttachments" (Ptr VkAttachmentReference2) 'False
                #{offset VkSubpassDescription2, pColorAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pResolveAttachments" (Ptr VkAttachmentReference2) -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkSubpassDescription2, pResolveAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDepthStencilAttachment" (Ptr VkAttachmentReference2) -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkSubpassDescription2, pDepthStencilAttachment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "preserveAttachmentCount" Word32 'True
                                                                #{offset VkSubpassDescription2, preserveAttachmentCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pPreserveAttachments" (Ptr Word32) 'False
                                                                    #{offset VkSubpassDescription2, pPreserveAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkSubpassDescription2`
type VkSubpassDescription2KHR = VkSubpassDescription2

-- | > typedef struct VkSubpassDescriptionDepthStencilResolve {
--   >     VkStructureType sType;
--   >     const void*                                              pNext;
--   >     VkResolveModeFlagBits                                    depthResolveMode;
--   >     VkResolveModeFlagBits                                    stencilResolveMode;
--   >     const VkAttachmentReference2*            pDepthStencilResolveAttachment;
--   > } VkSubpassDescriptionDepthStencilResolve;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSubpassDescriptionDepthStencilResolve VkSubpassDescriptionDepthStencilResolve registry at www.khronos.org>
type VkSubpassDescriptionDepthStencilResolve =
     VkStruct VkSubpassDescriptionDepthStencilResolve' -- ' closing tick for hsc2hs

data VkSubpassDescriptionDepthStencilResolve' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSubpassDescriptionDepthStencilResolve
         where
    type StructRep VkSubpassDescriptionDepthStencilResolve =
         'StructMeta "VkSubpassDescriptionDepthStencilResolve" -- ' closing tick for hsc2hs
           VkSubpassDescriptionDepthStencilResolve
           #{size VkSubpassDescriptionDepthStencilResolve}
           #{alignment VkSubpassDescriptionDepthStencilResolve}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSubpassDescriptionDepthStencilResolve, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkSubpassDescriptionDepthStencilResolve, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthResolveMode" VkResolveModeFlagBits 'False
                #{offset VkSubpassDescriptionDepthStencilResolve, depthResolveMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stencilResolveMode" VkResolveModeFlagBits 'False
                #{offset VkSubpassDescriptionDepthStencilResolve, stencilResolveMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDepthStencilResolveAttachment" -- ' closing tick for hsc2hs
                (Ptr VkAttachmentReference2)
                'True -- ' closing tick for hsc2hs
                #{offset VkSubpassDescriptionDepthStencilResolve, pDepthStencilResolveAttachment}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkSubpassDescription2] -- ' closing tick for hsc2hs

-- | Alias for `VkSubpassDescriptionDepthStencilResolve`
type VkSubpassDescriptionDepthStencilResolveKHR =
     VkSubpassDescriptionDepthStencilResolve

-- | > typedef struct VkSubpassEndInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   > } VkSubpassEndInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSubpassEndInfo VkSubpassEndInfo registry at www.khronos.org>
type VkSubpassEndInfo = VkStruct VkSubpassEndInfo' -- ' closing tick for hsc2hs

data VkSubpassEndInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSubpassEndInfo where
    type StructRep VkSubpassEndInfo =
         'StructMeta "VkSubpassEndInfo" VkSubpassEndInfo  -- ' closing tick for hsc2hs
                                                         #{size VkSubpassEndInfo}
           #{alignment VkSubpassEndInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkSubpassEndInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkSubpassEndInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkSubpassEndInfo`
type VkSubpassEndInfoKHR = VkSubpassEndInfo

-- | > typedef struct VkSubpassSampleLocationsEXT {
--   >     uint32_t                         subpassIndex;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkSubpassSampleLocationsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSubpassSampleLocationsEXT VkSubpassSampleLocationsEXT registry at www.khronos.org>
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
