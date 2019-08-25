#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformXcbKhr
       (VkXcbSurfaceCreateInfoKHR) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkXcbSurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Include            (XcbConnectionT,
                                                           XcbWindowT)

-- | > typedef struct VkXcbSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkXcbSurfaceCreateFlagsKHR   flags;
--   >     xcb_connection_t*                connection;
--   >     xcb_window_t                     window;
--   > } VkXcbSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkXcbSurfaceCreateInfoKHR VkXcbSurfaceCreateInfoKHR registry at www.khronos.org>
type VkXcbSurfaceCreateInfoKHR =
     VkStruct VkXcbSurfaceCreateInfoKHR' -- ' closing tick for hsc2hs

data VkXcbSurfaceCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkXcbSurfaceCreateInfoKHR where
    type StructRep VkXcbSurfaceCreateInfoKHR =
         'StructMeta "VkXcbSurfaceCreateInfoKHR" VkXcbSurfaceCreateInfoKHR -- ' closing tick for hsc2hs
           #{size VkXcbSurfaceCreateInfoKHR}
           #{alignment VkXcbSurfaceCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkXcbSurfaceCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkXcbSurfaceCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkXcbSurfaceCreateFlagsKHR 'True 
                                                                  #{offset VkXcbSurfaceCreateInfoKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "connection" (Ptr XcbConnectionT) 'False 
                                                                  #{offset VkXcbSurfaceCreateInfoKHR, connection}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "window" XcbWindowT 'False 
                                                    #{offset VkXcbSurfaceCreateInfoKHR, window}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
