#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.ComponentMapping
       (VkComponentMapping) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.ComponentSwizzle (VkComponentSwizzle)

-- | > typedef struct VkComponentMapping {
--   >     VkComponentSwizzle r;
--   >     VkComponentSwizzle g;
--   >     VkComponentSwizzle b;
--   >     VkComponentSwizzle a;
--   > } VkComponentMapping;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkComponentMapping VkComponentMapping registry at www.khronos.org>
type VkComponentMapping = VkStruct VkComponentMapping' -- ' closing tick for hsc2hs

data VkComponentMapping' -- ' closing tick for hsc2hs

instance VulkanMarshal VkComponentMapping where
    type StructRep VkComponentMapping =
         'StructMeta "VkComponentMapping" VkComponentMapping  -- ' closing tick for hsc2hs
                                                             #{size VkComponentMapping}
           #{alignment VkComponentMapping}
           '[('FieldMeta "r" VkComponentSwizzle 'False  -- ' closing tick for hsc2hs
                                                       #{offset VkComponentMapping, r}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "g" VkComponentSwizzle 'False 
                                                       #{offset VkComponentMapping, g}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "b" VkComponentSwizzle 'False 
                                                       #{offset VkComponentMapping, b}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "a" VkComponentSwizzle 'False 
                                                       #{offset VkComponentMapping, a}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
