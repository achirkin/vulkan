#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformXlibKhr
       (VkXlibSurfaceCreateInfoKHR) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkXlibSurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Include            (Display, Window)

-- | > typedef struct VkXlibSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkXlibSurfaceCreateFlagsKHR   flags;
--   >     Display*                         dpy;
--   >     Window                           window;
--   > } VkXlibSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkXlibSurfaceCreateInfoKHR VkXlibSurfaceCreateInfoKHR registry at www.khronos.org>
type VkXlibSurfaceCreateInfoKHR =
     VkStruct VkXlibSurfaceCreateInfoKHR' -- ' closing tick for hsc2hs

data VkXlibSurfaceCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkXlibSurfaceCreateInfoKHR where
    type StructRep VkXlibSurfaceCreateInfoKHR =
         'StructMeta "VkXlibSurfaceCreateInfoKHR" VkXlibSurfaceCreateInfoKHR -- ' closing tick for hsc2hs
           #{size VkXlibSurfaceCreateInfoKHR}
           #{alignment VkXlibSurfaceCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkXlibSurfaceCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkXlibSurfaceCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkXlibSurfaceCreateFlagsKHR 'True 
                                                                   #{offset VkXlibSurfaceCreateInfoKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dpy" (Ptr Display) 'False 
                                                    #{offset VkXlibSurfaceCreateInfoKHR, dpy}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "window" Window 'False 
                                                #{offset VkXlibSurfaceCreateInfoKHR, window}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
