#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.RenderPass
       (VkRenderPassAttachmentBeginInfo,
        VkRenderPassAttachmentBeginInfoKHR, VkRenderPassBeginInfo,
        VkRenderPassCreateInfo, VkRenderPassCreateInfo2,
        VkRenderPassCreateInfo2KHR,
        VkRenderPassFragmentDensityMapCreateInfoEXT,
        VkRenderPassInputAttachmentAspectCreateInfo,
        VkRenderPassInputAttachmentAspectCreateInfoKHR,
        VkRenderPassMultiviewCreateInfo,
        VkRenderPassMultiviewCreateInfoKHR,
        VkRenderPassSampleLocationsBeginInfoEXT,
        VkRenderPassTransformBeginInfoQCOM)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.RenderPassCreateFlags            (VkRenderPassCreateFlags)
import Graphics.Vulkan.Types.Enum.StructureType                    (VkStructureType)
import Graphics.Vulkan.Types.Enum.Surface                          (VkSurfaceTransformFlagBitsKHR)
import Graphics.Vulkan.Types.Handles                               (VkFramebuffer,
                                                                    VkImageView,
                                                                    VkRenderPass)
import Graphics.Vulkan.Types.Struct.Attachment                     (VkAttachmentDescription,
                                                                    VkAttachmentDescription2,
                                                                    VkAttachmentReference,
                                                                    VkAttachmentSampleLocationsEXT)
import Graphics.Vulkan.Types.Struct.Clear                          (VkClearValue)
import Graphics.Vulkan.Types.Struct.InputAttachmentAspectReference (VkInputAttachmentAspectReference)
import Graphics.Vulkan.Types.Struct.Rect                           (VkRect2D)
import Graphics.Vulkan.Types.Struct.Subpass                        (VkSubpassDependency,
                                                                    VkSubpassDependency2,
                                                                    VkSubpassDescription,
                                                                    VkSubpassDescription2,
                                                                    VkSubpassSampleLocationsEXT)

-- | > typedef struct VkRenderPassAttachmentBeginInfo {
--   >     VkStructureType sType;
--   >     const void*                              pNext;
--   >     uint32_t                 attachmentCount;
--   >     const VkImageView* pAttachments;
--   > } VkRenderPassAttachmentBeginInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRenderPassAttachmentBeginInfo VkRenderPassAttachmentBeginInfo registry at www.khronos.org>
type VkRenderPassAttachmentBeginInfo =
     VkStruct VkRenderPassAttachmentBeginInfo' -- ' closing tick for hsc2hs

data VkRenderPassAttachmentBeginInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRenderPassAttachmentBeginInfo where
    type StructRep VkRenderPassAttachmentBeginInfo =
         'StructMeta "VkRenderPassAttachmentBeginInfo" -- ' closing tick for hsc2hs
           VkRenderPassAttachmentBeginInfo
           #{size VkRenderPassAttachmentBeginInfo}
           #{alignment VkRenderPassAttachmentBeginInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkRenderPassAttachmentBeginInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkRenderPassAttachmentBeginInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "attachmentCount" Word32 'True 
                                                        #{offset VkRenderPassAttachmentBeginInfo, attachmentCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAttachments" (Ptr VkImageView) 'False 
                                                                 #{offset VkRenderPassAttachmentBeginInfo, pAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkRenderPassBeginInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkRenderPassAttachmentBeginInfo`
type VkRenderPassAttachmentBeginInfoKHR =
     VkRenderPassAttachmentBeginInfo

-- | > typedef struct VkRenderPassBeginInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkRenderPass           renderPass;
--   >     VkFramebuffer          framebuffer;
--   >     VkRect2D               renderArea;
--   >     uint32_t               clearValueCount;
--   >     const VkClearValue*    pClearValues;
--   > } VkRenderPassBeginInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRenderPassBeginInfo VkRenderPassBeginInfo registry at www.khronos.org>
type VkRenderPassBeginInfo = VkStruct VkRenderPassBeginInfo' -- ' closing tick for hsc2hs

data VkRenderPassBeginInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRenderPassBeginInfo where
    type StructRep VkRenderPassBeginInfo =
         'StructMeta "VkRenderPassBeginInfo" VkRenderPassBeginInfo  -- ' closing tick for hsc2hs
                                                                   #{size VkRenderPassBeginInfo}
           #{alignment VkRenderPassBeginInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkRenderPassBeginInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkRenderPassBeginInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "renderPass" VkRenderPass 'False 
                                                          #{offset VkRenderPassBeginInfo, renderPass}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "framebuffer" VkFramebuffer 'False 
                                                            #{offset VkRenderPassBeginInfo, framebuffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "renderArea" VkRect2D 'False 
                                                      #{offset VkRenderPassBeginInfo, renderArea}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "clearValueCount" Word32 'True 
                                                        #{offset VkRenderPassBeginInfo, clearValueCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pClearValues" (Ptr VkClearValue) 'False 
                                                                  #{offset VkRenderPassBeginInfo, pClearValues}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkRenderPassCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkRenderPassCreateFlags flags;
--   >     uint32_t   attachmentCount;
--   >     const VkAttachmentDescription* pAttachments;
--   >     uint32_t               subpassCount;
--   >     const VkSubpassDescription* pSubpasses;
--   >     uint32_t       dependencyCount;
--   >     const VkSubpassDependency* pDependencies;
--   > } VkRenderPassCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRenderPassCreateInfo VkRenderPassCreateInfo registry at www.khronos.org>
type VkRenderPassCreateInfo = VkStruct VkRenderPassCreateInfo' -- ' closing tick for hsc2hs

data VkRenderPassCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRenderPassCreateInfo where
    type StructRep VkRenderPassCreateInfo =
         'StructMeta "VkRenderPassCreateInfo" VkRenderPassCreateInfo -- ' closing tick for hsc2hs
           #{size VkRenderPassCreateInfo}
           #{alignment VkRenderPassCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkRenderPassCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkRenderPassCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkRenderPassCreateFlags 'True 
                                                               #{offset VkRenderPassCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "attachmentCount" Word32 'True 
                                                        #{offset VkRenderPassCreateInfo, attachmentCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAttachments" (Ptr VkAttachmentDescription) 'False
                #{offset VkRenderPassCreateInfo, pAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subpassCount" Word32 'False 
                                                      #{offset VkRenderPassCreateInfo, subpassCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pSubpasses" (Ptr VkSubpassDescription) 'False
                #{offset VkRenderPassCreateInfo, pSubpasses}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dependencyCount" Word32 'True 
                                                        #{offset VkRenderPassCreateInfo, dependencyCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDependencies" (Ptr VkSubpassDependency) 'False
                #{offset VkRenderPassCreateInfo, pDependencies}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkRenderPassCreateInfo2 {
--   >     VkStructureType sType;
--   >     const void*                                              pNext;
--   >     VkRenderPassCreateFlags                  flags;
--   >     uint32_t                                 attachmentCount;
--   >     const VkAttachmentDescription2*    pAttachments;
--   >     uint32_t                                                 subpassCount;
--   >     const VkSubpassDescription2*          pSubpasses;
--   >     uint32_t                                 dependencyCount;
--   >     const VkSubpassDependency2*        pDependencies;
--   >     uint32_t                                 correlatedViewMaskCount;
--   >     const uint32_t*            pCorrelatedViewMasks;
--   > } VkRenderPassCreateInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRenderPassCreateInfo2 VkRenderPassCreateInfo2 registry at www.khronos.org>
type VkRenderPassCreateInfo2 = VkStruct VkRenderPassCreateInfo2' -- ' closing tick for hsc2hs

data VkRenderPassCreateInfo2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRenderPassCreateInfo2 where
    type StructRep VkRenderPassCreateInfo2 =
         'StructMeta "VkRenderPassCreateInfo2" VkRenderPassCreateInfo2 -- ' closing tick for hsc2hs
           #{size VkRenderPassCreateInfo2}
           #{alignment VkRenderPassCreateInfo2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkRenderPassCreateInfo2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkRenderPassCreateInfo2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkRenderPassCreateFlags 'True 
                                                               #{offset VkRenderPassCreateInfo2, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "attachmentCount" Word32 'True 
                                                        #{offset VkRenderPassCreateInfo2, attachmentCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAttachments" (Ptr VkAttachmentDescription2) 'False
                #{offset VkRenderPassCreateInfo2, pAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subpassCount" Word32 'False 
                                                      #{offset VkRenderPassCreateInfo2, subpassCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pSubpasses" (Ptr VkSubpassDescription2) 'False
                #{offset VkRenderPassCreateInfo2, pSubpasses}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dependencyCount" Word32 'True 
                                                        #{offset VkRenderPassCreateInfo2, dependencyCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDependencies" (Ptr VkSubpassDependency2) 'False
                #{offset VkRenderPassCreateInfo2, pDependencies}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "correlatedViewMaskCount" Word32 'True 
                                                                #{offset VkRenderPassCreateInfo2, correlatedViewMaskCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pCorrelatedViewMasks" (Ptr Word32) 'False 
                                                                    #{offset VkRenderPassCreateInfo2, pCorrelatedViewMasks}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkRenderPassCreateInfo2`
type VkRenderPassCreateInfo2KHR = VkRenderPassCreateInfo2

-- | > typedef struct VkRenderPassFragmentDensityMapCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkAttachmentReference            fragmentDensityMapAttachment;
--   > } VkRenderPassFragmentDensityMapCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRenderPassFragmentDensityMapCreateInfoEXT VkRenderPassFragmentDensityMapCreateInfoEXT registry at www.khronos.org>
type VkRenderPassFragmentDensityMapCreateInfoEXT =
     VkStruct VkRenderPassFragmentDensityMapCreateInfoEXT' -- ' closing tick for hsc2hs

data VkRenderPassFragmentDensityMapCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRenderPassFragmentDensityMapCreateInfoEXT
         where
    type StructRep VkRenderPassFragmentDensityMapCreateInfoEXT =
         'StructMeta "VkRenderPassFragmentDensityMapCreateInfoEXT" -- ' closing tick for hsc2hs
           VkRenderPassFragmentDensityMapCreateInfoEXT
           #{size VkRenderPassFragmentDensityMapCreateInfoEXT}
           #{alignment VkRenderPassFragmentDensityMapCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkRenderPassFragmentDensityMapCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkRenderPassFragmentDensityMapCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fragmentDensityMapAttachment" VkAttachmentReference -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkRenderPassFragmentDensityMapCreateInfoEXT, fragmentDensityMapAttachment}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkRenderPassCreateInfo, VkRenderPassCreateInfo2] -- ' closing tick for hsc2hs

-- | > typedef struct VkRenderPassInputAttachmentAspectCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                     pNext;
--   >     uint32_t                        aspectReferenceCount;
--   >     const VkInputAttachmentAspectReference* pAspectReferences;
--   > } VkRenderPassInputAttachmentAspectCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRenderPassInputAttachmentAspectCreateInfo VkRenderPassInputAttachmentAspectCreateInfo registry at www.khronos.org>
type VkRenderPassInputAttachmentAspectCreateInfo =
     VkStruct VkRenderPassInputAttachmentAspectCreateInfo' -- ' closing tick for hsc2hs

data VkRenderPassInputAttachmentAspectCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRenderPassInputAttachmentAspectCreateInfo
         where
    type StructRep VkRenderPassInputAttachmentAspectCreateInfo =
         'StructMeta "VkRenderPassInputAttachmentAspectCreateInfo" -- ' closing tick for hsc2hs
           VkRenderPassInputAttachmentAspectCreateInfo
           #{size VkRenderPassInputAttachmentAspectCreateInfo}
           #{alignment VkRenderPassInputAttachmentAspectCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkRenderPassInputAttachmentAspectCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkRenderPassInputAttachmentAspectCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "aspectReferenceCount" Word32 'False 
                                                              #{offset VkRenderPassInputAttachmentAspectCreateInfo, aspectReferenceCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAspectReferences" -- ' closing tick for hsc2hs
                (Ptr VkInputAttachmentAspectReference)
                'False -- ' closing tick for hsc2hs
                #{offset VkRenderPassInputAttachmentAspectCreateInfo, pAspectReferences}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkRenderPassCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkRenderPassInputAttachmentAspectCreateInfo`
type VkRenderPassInputAttachmentAspectCreateInfoKHR =
     VkRenderPassInputAttachmentAspectCreateInfo

-- | > typedef struct VkRenderPassMultiviewCreateInfo {
--   >     VkStructureType        sType;
--   >     const void*            pNext;
--   >     uint32_t               subpassCount;
--   >     const uint32_t*     pViewMasks;
--   >     uint32_t               dependencyCount;
--   >     const int32_t*   pViewOffsets;
--   >     uint32_t               correlationMaskCount;
--   >     const uint32_t* pCorrelationMasks;
--   > } VkRenderPassMultiviewCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRenderPassMultiviewCreateInfo VkRenderPassMultiviewCreateInfo registry at www.khronos.org>
type VkRenderPassMultiviewCreateInfo =
     VkStruct VkRenderPassMultiviewCreateInfo' -- ' closing tick for hsc2hs

data VkRenderPassMultiviewCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRenderPassMultiviewCreateInfo where
    type StructRep VkRenderPassMultiviewCreateInfo =
         'StructMeta "VkRenderPassMultiviewCreateInfo" -- ' closing tick for hsc2hs
           VkRenderPassMultiviewCreateInfo
           #{size VkRenderPassMultiviewCreateInfo}
           #{alignment VkRenderPassMultiviewCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkRenderPassMultiviewCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkRenderPassMultiviewCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subpassCount" Word32 'True 
                                                     #{offset VkRenderPassMultiviewCreateInfo, subpassCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pViewMasks" (Ptr Word32) 'False 
                                                          #{offset VkRenderPassMultiviewCreateInfo, pViewMasks}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dependencyCount" Word32 'True 
                                                        #{offset VkRenderPassMultiviewCreateInfo, dependencyCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pViewOffsets" (Ptr Int32) 'False 
                                                           #{offset VkRenderPassMultiviewCreateInfo, pViewOffsets}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "correlationMaskCount" Word32 'True 
                                                             #{offset VkRenderPassMultiviewCreateInfo, correlationMaskCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pCorrelationMasks" (Ptr Word32) 'False 
                                                                 #{offset VkRenderPassMultiviewCreateInfo, pCorrelationMasks}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkRenderPassCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkRenderPassMultiviewCreateInfo`
type VkRenderPassMultiviewCreateInfoKHR =
     VkRenderPassMultiviewCreateInfo

-- | > typedef struct VkRenderPassSampleLocationsBeginInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         attachmentInitialSampleLocationsCount;
--   >     const VkAttachmentSampleLocationsEXT* pAttachmentInitialSampleLocations;
--   >     uint32_t         postSubpassSampleLocationsCount;
--   >     const VkSubpassSampleLocationsEXT* pPostSubpassSampleLocations;
--   > } VkRenderPassSampleLocationsBeginInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRenderPassSampleLocationsBeginInfoEXT VkRenderPassSampleLocationsBeginInfoEXT registry at www.khronos.org>
type VkRenderPassSampleLocationsBeginInfoEXT =
     VkStruct VkRenderPassSampleLocationsBeginInfoEXT' -- ' closing tick for hsc2hs

data VkRenderPassSampleLocationsBeginInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRenderPassSampleLocationsBeginInfoEXT
         where
    type StructRep VkRenderPassSampleLocationsBeginInfoEXT =
         'StructMeta "VkRenderPassSampleLocationsBeginInfoEXT" -- ' closing tick for hsc2hs
           VkRenderPassSampleLocationsBeginInfoEXT
           #{size VkRenderPassSampleLocationsBeginInfoEXT}
           #{alignment VkRenderPassSampleLocationsBeginInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "attachmentInitialSampleLocationsCount" Word32 'True
                #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAttachmentInitialSampleLocations" -- ' closing tick for hsc2hs
                (Ptr VkAttachmentSampleLocationsEXT)
                'False -- ' closing tick for hsc2hs
                #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "postSubpassSampleLocationsCount" Word32 'True
                #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pPostSubpassSampleLocations" -- ' closing tick for hsc2hs
                (Ptr VkSubpassSampleLocationsEXT)
                'False -- ' closing tick for hsc2hs
                #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkRenderPassBeginInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkRenderPassTransformBeginInfoQCOM {
--   >     VkStructureType sType;
--   >     void*                           pNext;
--   >     VkSurfaceTransformFlagBitsKHR   transform;
--   > } VkRenderPassTransformBeginInfoQCOM;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRenderPassTransformBeginInfoQCOM VkRenderPassTransformBeginInfoQCOM registry at www.khronos.org>
type VkRenderPassTransformBeginInfoQCOM =
     VkStruct VkRenderPassTransformBeginInfoQCOM' -- ' closing tick for hsc2hs

data VkRenderPassTransformBeginInfoQCOM' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRenderPassTransformBeginInfoQCOM where
    type StructRep VkRenderPassTransformBeginInfoQCOM =
         'StructMeta "VkRenderPassTransformBeginInfoQCOM" -- ' closing tick for hsc2hs
           VkRenderPassTransformBeginInfoQCOM
           #{size VkRenderPassTransformBeginInfoQCOM}
           #{alignment VkRenderPassTransformBeginInfoQCOM}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkRenderPassTransformBeginInfoQCOM, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkRenderPassTransformBeginInfoQCOM, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "transform" VkSurfaceTransformFlagBitsKHR 'False
                #{offset VkRenderPassTransformBeginInfoQCOM, transform}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkRenderPassBeginInfo] -- ' closing tick for hsc2hs
