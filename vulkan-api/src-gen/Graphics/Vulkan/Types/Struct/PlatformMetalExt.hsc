#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformMetalExt
       (VkMetalSurfaceCreateInfoEXT) where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.BaseTypes          (CAMetalLayer)
import Graphics.Vulkan.Types.Bitmasks           (VkMetalSurfaceCreateFlagsEXT)
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)

-- | > typedef struct VkMetalSurfaceCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                                    pNext;
--   >     VkMetalSurfaceCreateFlagsEXT   flags;
--   >     const CAMetalLayer*      pLayer;
--   > } VkMetalSurfaceCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkMetalSurfaceCreateInfoEXT VkMetalSurfaceCreateInfoEXT registry at www.khronos.org>
type VkMetalSurfaceCreateInfoEXT =
     VkStruct VkMetalSurfaceCreateInfoEXT' -- ' closing tick for hsc2hs

data VkMetalSurfaceCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMetalSurfaceCreateInfoEXT where
    type StructRep VkMetalSurfaceCreateInfoEXT =
         'StructMeta "VkMetalSurfaceCreateInfoEXT" -- ' closing tick for hsc2hs
           VkMetalSurfaceCreateInfoEXT
           #{size VkMetalSurfaceCreateInfoEXT}
           #{alignment VkMetalSurfaceCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMetalSurfaceCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMetalSurfaceCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkMetalSurfaceCreateFlagsEXT 'True 
                                                                    #{offset VkMetalSurfaceCreateInfoEXT, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pLayer" (Ptr CAMetalLayer) 'False 
                                                            #{offset VkMetalSurfaceCreateInfoEXT, pLayer}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
