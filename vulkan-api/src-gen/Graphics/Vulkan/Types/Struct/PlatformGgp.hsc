#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformGgp
       (VkPresentFrameTokenGGP, VkStreamDescriptorSurfaceCreateInfoGGP)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Bitmasks           (VkStreamDescriptorSurfaceCreateFlagsGGP)
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import Graphics.Vulkan.Types.Include            (GgpFrameToken,
                                                 GgpStreamDescriptor)
import Graphics.Vulkan.Types.Struct.Present     (VkPresentInfoKHR)

-- | > typedef struct VkPresentFrameTokenGGP {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     GgpFrameToken                    frameToken;
--   > } VkPresentFrameTokenGGP;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPresentFrameTokenGGP VkPresentFrameTokenGGP registry at www.khronos.org>
type VkPresentFrameTokenGGP = VkStruct VkPresentFrameTokenGGP' -- ' closing tick for hsc2hs

data VkPresentFrameTokenGGP' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPresentFrameTokenGGP where
    type StructRep VkPresentFrameTokenGGP =
         'StructMeta "VkPresentFrameTokenGGP" VkPresentFrameTokenGGP -- ' closing tick for hsc2hs
           #{size VkPresentFrameTokenGGP}
           #{alignment VkPresentFrameTokenGGP}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPresentFrameTokenGGP, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPresentFrameTokenGGP, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "frameToken" GgpFrameToken 'False 
                                                           #{offset VkPresentFrameTokenGGP, frameToken}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPresentInfoKHR] -- ' closing tick for hsc2hs

-- | > typedef struct VkStreamDescriptorSurfaceCreateInfoGGP {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkStreamDescriptorSurfaceCreateFlagsGGP flags;
--   >     GgpStreamDescriptor              streamDescriptor;
--   > } VkStreamDescriptorSurfaceCreateInfoGGP;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkStreamDescriptorSurfaceCreateInfoGGP VkStreamDescriptorSurfaceCreateInfoGGP registry at www.khronos.org>
type VkStreamDescriptorSurfaceCreateInfoGGP =
     VkStruct VkStreamDescriptorSurfaceCreateInfoGGP' -- ' closing tick for hsc2hs

data VkStreamDescriptorSurfaceCreateInfoGGP' -- ' closing tick for hsc2hs

instance VulkanMarshal VkStreamDescriptorSurfaceCreateInfoGGP where
    type StructRep VkStreamDescriptorSurfaceCreateInfoGGP =
         'StructMeta "VkStreamDescriptorSurfaceCreateInfoGGP" -- ' closing tick for hsc2hs
           VkStreamDescriptorSurfaceCreateInfoGGP
           #{size VkStreamDescriptorSurfaceCreateInfoGGP}
           #{alignment VkStreamDescriptorSurfaceCreateInfoGGP}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkStreamDescriptorSurfaceCreateInfoGGP, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkStreamDescriptorSurfaceCreateInfoGGP, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkStreamDescriptorSurfaceCreateFlagsGGP 'True
                #{offset VkStreamDescriptorSurfaceCreateInfoGGP, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "streamDescriptor" GgpStreamDescriptor 'False 
                                                                       #{offset VkStreamDescriptorSurfaceCreateInfoGGP, streamDescriptor}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
