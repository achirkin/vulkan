#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformViNn
       (VkViSurfaceCreateInfoNN) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkViSurfaceCreateFlagsNN)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)

-- | > typedef struct VkViSurfaceCreateInfoNN {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkViSurfaceCreateFlagsNN   flags;
--   >     void*                            window;
--   > } VkViSurfaceCreateInfoNN;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkViSurfaceCreateInfoNN VkViSurfaceCreateInfoNN registry at www.khronos.org>
type VkViSurfaceCreateInfoNN = VkStruct VkViSurfaceCreateInfoNN' -- ' closing tick for hsc2hs

data VkViSurfaceCreateInfoNN' -- ' closing tick for hsc2hs

instance VulkanMarshal VkViSurfaceCreateInfoNN where
    type StructRep VkViSurfaceCreateInfoNN =
         'StructMeta "VkViSurfaceCreateInfoNN" VkViSurfaceCreateInfoNN -- ' closing tick for hsc2hs
           #{size VkViSurfaceCreateInfoNN}
           #{alignment VkViSurfaceCreateInfoNN}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkViSurfaceCreateInfoNN, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkViSurfaceCreateInfoNN, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkViSurfaceCreateFlagsNN 'True 
                                                                #{offset VkViSurfaceCreateInfoNN, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "window" (Ptr Void) 'False 
                                                    #{offset VkViSurfaceCreateInfoNN, window}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
