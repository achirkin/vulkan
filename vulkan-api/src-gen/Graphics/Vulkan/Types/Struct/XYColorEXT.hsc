#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.XYColorEXT (VkXYColorEXT) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal

-- | Chromaticity coordinate
--
--   > typedef struct VkXYColorEXT {
--   >     float   x;
--   >     float   y;
--   > } VkXYColorEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkXYColorEXT VkXYColorEXT registry at www.khronos.org>
type VkXYColorEXT = VkStruct VkXYColorEXT' -- ' closing tick for hsc2hs

data VkXYColorEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkXYColorEXT where
    type StructRep VkXYColorEXT =
         'StructMeta "VkXYColorEXT" VkXYColorEXT  -- ' closing tick for hsc2hs
                                                 #{size VkXYColorEXT}
           #{alignment VkXYColorEXT}
           '[('FieldMeta "x" (
                              #{type float}
                              ) 'False  -- ' closing tick for hsc2hs
                                       #{offset VkXYColorEXT, x}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "y" ( -- ' closing tick for hsc2hs
                              #{type float}
                              ) 'False  -- ' closing tick for hsc2hs
                                       #{offset VkXYColorEXT, y}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
