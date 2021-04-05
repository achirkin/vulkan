#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.HeadlessSurfaceCreateInfoEXT
       (VkHeadlessSurfaceCreateInfoEXT) where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Bitmasks           (VkHeadlessSurfaceCreateFlagsEXT)
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)

-- | > typedef struct VkHeadlessSurfaceCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkHeadlessSurfaceCreateFlagsEXT   flags;
--   > } VkHeadlessSurfaceCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkHeadlessSurfaceCreateInfoEXT VkHeadlessSurfaceCreateInfoEXT registry at www.khronos.org>
type VkHeadlessSurfaceCreateInfoEXT =
     VkStruct VkHeadlessSurfaceCreateInfoEXT' -- ' closing tick for hsc2hs

data VkHeadlessSurfaceCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkHeadlessSurfaceCreateInfoEXT where
    type StructRep VkHeadlessSurfaceCreateInfoEXT =
         'StructMeta "VkHeadlessSurfaceCreateInfoEXT" -- ' closing tick for hsc2hs
           VkHeadlessSurfaceCreateInfoEXT
           #{size VkHeadlessSurfaceCreateInfoEXT}
           #{alignment VkHeadlessSurfaceCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkHeadlessSurfaceCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkHeadlessSurfaceCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkHeadlessSurfaceCreateFlagsEXT 'True 
                                                                       #{offset VkHeadlessSurfaceCreateInfoEXT, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
