#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.ConditionalRenderingBeginInfoEXT
       (VkConditionalRenderingBeginInfoEXT) where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.BaseTypes                         (VkDeviceSize)
import Graphics.Vulkan.Types.Enum.ConditionalRenderingFlagsEXT (VkConditionalRenderingFlagsEXT)
import Graphics.Vulkan.Types.Enum.StructureType                (VkStructureType)
import Graphics.Vulkan.Types.Handles                           (VkBuffer)

-- | > typedef struct VkConditionalRenderingBeginInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBuffer                         buffer;
--   >     VkDeviceSize                     offset;
--   >     VkConditionalRenderingFlagsEXT    flags;
--   > } VkConditionalRenderingBeginInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkConditionalRenderingBeginInfoEXT VkConditionalRenderingBeginInfoEXT registry at www.khronos.org>
type VkConditionalRenderingBeginInfoEXT =
     VkStruct VkConditionalRenderingBeginInfoEXT' -- ' closing tick for hsc2hs

data VkConditionalRenderingBeginInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkConditionalRenderingBeginInfoEXT where
    type StructRep VkConditionalRenderingBeginInfoEXT =
         'StructMeta "VkConditionalRenderingBeginInfoEXT" -- ' closing tick for hsc2hs
           VkConditionalRenderingBeginInfoEXT
           #{size VkConditionalRenderingBeginInfoEXT}
           #{alignment VkConditionalRenderingBeginInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkConditionalRenderingBeginInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkConditionalRenderingBeginInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "buffer" VkBuffer 'False 
                                                  #{offset VkConditionalRenderingBeginInfoEXT, buffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "offset" VkDeviceSize 'False 
                                                      #{offset VkConditionalRenderingBeginInfoEXT, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkConditionalRenderingFlagsEXT 'True 
                                                                      #{offset VkConditionalRenderingBeginInfoEXT, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
