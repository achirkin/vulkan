#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformDirectfbExt
       (VkDirectFBSurfaceCreateInfoEXT) where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Bitmasks           (VkDirectFBSurfaceCreateFlagsEXT)
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import Graphics.Vulkan.Types.Include            (IDirectFB, IDirectFBSurface)

-- | > typedef struct VkDirectFBSurfaceCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDirectFBSurfaceCreateFlagsEXT   flags;
--   >     IDirectFB*                       dfb;
--   >     IDirectFBSurface*                surface;
--   > } VkDirectFBSurfaceCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkDirectFBSurfaceCreateInfoEXT VkDirectFBSurfaceCreateInfoEXT registry at www.khronos.org>
type VkDirectFBSurfaceCreateInfoEXT =
     VkStruct VkDirectFBSurfaceCreateInfoEXT' -- ' closing tick for hsc2hs

data VkDirectFBSurfaceCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDirectFBSurfaceCreateInfoEXT where
    type StructRep VkDirectFBSurfaceCreateInfoEXT =
         'StructMeta "VkDirectFBSurfaceCreateInfoEXT" -- ' closing tick for hsc2hs
           VkDirectFBSurfaceCreateInfoEXT
           #{size VkDirectFBSurfaceCreateInfoEXT}
           #{alignment VkDirectFBSurfaceCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDirectFBSurfaceCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDirectFBSurfaceCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkDirectFBSurfaceCreateFlagsEXT 'True 
                                                                       #{offset VkDirectFBSurfaceCreateInfoEXT, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dfb" (Ptr IDirectFB) 'False 
                                                      #{offset VkDirectFBSurfaceCreateInfoEXT, dfb}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "surface" (Ptr IDirectFBSurface) 'False 
                                                                 #{offset VkDirectFBSurfaceCreateInfoEXT, surface}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
