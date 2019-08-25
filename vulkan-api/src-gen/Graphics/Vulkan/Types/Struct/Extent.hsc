#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Extent (VkExtent2D, VkExtent3D)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal

-- | > typedef struct VkExtent2D {
--   >     uint32_t        width;
--   >     uint32_t        height;
--   > } VkExtent2D;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExtent2D VkExtent2D registry at www.khronos.org>
type VkExtent2D = VkStruct VkExtent2D' -- ' closing tick for hsc2hs

data VkExtent2D' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExtent2D where
    type StructRep VkExtent2D =
         'StructMeta "VkExtent2D" VkExtent2D  -- ' closing tick for hsc2hs
                                             #{size VkExtent2D}
           #{alignment VkExtent2D}
           '[('FieldMeta "width" Word32 'False  -- ' closing tick for hsc2hs
                                               #{offset VkExtent2D, width}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "height" Word32 'False 
                                                #{offset VkExtent2D, height}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkExtent3D {
--   >     uint32_t        width;
--   >     uint32_t        height;
--   >     uint32_t        depth;
--   > } VkExtent3D;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExtent3D VkExtent3D registry at www.khronos.org>
type VkExtent3D = VkStruct VkExtent3D' -- ' closing tick for hsc2hs

data VkExtent3D' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExtent3D where
    type StructRep VkExtent3D =
         'StructMeta "VkExtent3D" VkExtent3D  -- ' closing tick for hsc2hs
                                             #{size VkExtent3D}
           #{alignment VkExtent3D}
           '[('FieldMeta "width" Word32 'False  -- ' closing tick for hsc2hs
                                               #{offset VkExtent3D, width}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "height" Word32 'False 
                                                #{offset VkExtent3D, height}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depth" Word32 'False 
                                               #{offset VkExtent3D, depth}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
