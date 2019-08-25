#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Offset (VkOffset2D, VkOffset3D)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal

-- | > typedef struct VkOffset2D {
--   >     int32_t        x;
--   >     int32_t        y;
--   > } VkOffset2D;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkOffset2D VkOffset2D registry at www.khronos.org>
type VkOffset2D = VkStruct VkOffset2D' -- ' closing tick for hsc2hs

data VkOffset2D' -- ' closing tick for hsc2hs

instance VulkanMarshal VkOffset2D where
    type StructRep VkOffset2D =
         'StructMeta "VkOffset2D" VkOffset2D  -- ' closing tick for hsc2hs
                                             #{size VkOffset2D}
           #{alignment VkOffset2D}
           '[('FieldMeta "x" Int32 'False  -- ' closing tick for hsc2hs
                                          #{offset VkOffset2D, x}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "y" Int32 'False 
                                          #{offset VkOffset2D, y} 1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkOffset3D {
--   >     int32_t        x;
--   >     int32_t        y;
--   >     int32_t        z;
--   > } VkOffset3D;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkOffset3D VkOffset3D registry at www.khronos.org>
type VkOffset3D = VkStruct VkOffset3D' -- ' closing tick for hsc2hs

data VkOffset3D' -- ' closing tick for hsc2hs

instance VulkanMarshal VkOffset3D where
    type StructRep VkOffset3D =
         'StructMeta "VkOffset3D" VkOffset3D  -- ' closing tick for hsc2hs
                                             #{size VkOffset3D}
           #{alignment VkOffset3D}
           '[('FieldMeta "x" Int32 'False  -- ' closing tick for hsc2hs
                                          #{offset VkOffset3D, x}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "y" Int32 'False 
                                          #{offset VkOffset3D, y} 1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "z" Int32 'False 
                                          #{offset VkOffset3D, z} 1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
