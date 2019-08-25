#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformMirKhr
       (VkMirSurfaceCreateInfoKHR) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkMirSurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Include            (MirConnection,
                                                           MirSurface)

-- | > typedef struct VkMirSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkMirSurfaceCreateFlagsKHR   flags;
--   >     MirConnection*                   connection;
--   >     MirSurface*                      mirSurface;
--   > } VkMirSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMirSurfaceCreateInfoKHR VkMirSurfaceCreateInfoKHR registry at www.khronos.org>
type VkMirSurfaceCreateInfoKHR =
     VkStruct VkMirSurfaceCreateInfoKHR' -- ' closing tick for hsc2hs

data VkMirSurfaceCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMirSurfaceCreateInfoKHR where
    type StructRep VkMirSurfaceCreateInfoKHR =
         'StructMeta "VkMirSurfaceCreateInfoKHR" VkMirSurfaceCreateInfoKHR -- ' closing tick for hsc2hs
           #{size VkMirSurfaceCreateInfoKHR}
           #{alignment VkMirSurfaceCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMirSurfaceCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMirSurfaceCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkMirSurfaceCreateFlagsKHR 'True 
                                                                  #{offset VkMirSurfaceCreateInfoKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "connection" (Ptr MirConnection) 'False 
                                                                 #{offset VkMirSurfaceCreateInfoKHR, connection}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "mirSurface" (Ptr MirSurface) 'False 
                                                              #{offset VkMirSurfaceCreateInfoKHR, mirSurface}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
