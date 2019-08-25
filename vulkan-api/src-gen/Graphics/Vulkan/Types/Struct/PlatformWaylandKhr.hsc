#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformWaylandKhr
       (VkWaylandSurfaceCreateInfoKHR) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkWaylandSurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Include            (WlDisplay, WlSurface)

-- | > typedef struct VkWaylandSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkWaylandSurfaceCreateFlagsKHR   flags;
--   >     struct wl_display*               display;
--   >     struct wl_surface*               surface;
--   > } VkWaylandSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkWaylandSurfaceCreateInfoKHR VkWaylandSurfaceCreateInfoKHR registry at www.khronos.org>
type VkWaylandSurfaceCreateInfoKHR =
     VkStruct VkWaylandSurfaceCreateInfoKHR' -- ' closing tick for hsc2hs

data VkWaylandSurfaceCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkWaylandSurfaceCreateInfoKHR where
    type StructRep VkWaylandSurfaceCreateInfoKHR =
         'StructMeta "VkWaylandSurfaceCreateInfoKHR" -- ' closing tick for hsc2hs
           VkWaylandSurfaceCreateInfoKHR
           #{size VkWaylandSurfaceCreateInfoKHR}
           #{alignment VkWaylandSurfaceCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkWaylandSurfaceCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkWaylandSurfaceCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkWaylandSurfaceCreateFlagsKHR 'True 
                                                                      #{offset VkWaylandSurfaceCreateInfoKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "display" (Ptr WlDisplay) 'False 
                                                          #{offset VkWaylandSurfaceCreateInfoKHR, display}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "surface" (Ptr WlSurface) 'False 
                                                          #{offset VkWaylandSurfaceCreateInfoKHR, surface}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
