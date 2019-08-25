#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Rect (VkRect2D, VkRectLayerKHR)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.Extent (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.Offset (VkOffset2D)

-- | > typedef struct VkRect2D {
--   >     VkOffset2D     offset;
--   >     VkExtent2D     extent;
--   > } VkRect2D;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkRect2D VkRect2D registry at www.khronos.org>
type VkRect2D = VkStruct VkRect2D' -- ' closing tick for hsc2hs

data VkRect2D' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRect2D where
    type StructRep VkRect2D =
         'StructMeta "VkRect2D" VkRect2D  -- ' closing tick for hsc2hs
                                         #{size VkRect2D}
           #{alignment VkRect2D}
           '[('FieldMeta "offset" VkOffset2D 'False  -- ' closing tick for hsc2hs
                                                    #{offset VkRect2D, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "extent" VkExtent2D 'False 
                                                    #{offset VkRect2D, extent}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkRectLayerKHR {
--   >     VkOffset2D                       offset;
--   >     VkExtent2D                       extent;
--   >     uint32_t                         layer;
--   > } VkRectLayerKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkRectLayerKHR VkRectLayerKHR registry at www.khronos.org>
type VkRectLayerKHR = VkStruct VkRectLayerKHR' -- ' closing tick for hsc2hs

data VkRectLayerKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRectLayerKHR where
    type StructRep VkRectLayerKHR =
         'StructMeta "VkRectLayerKHR" VkRectLayerKHR  -- ' closing tick for hsc2hs
                                                     #{size VkRectLayerKHR}
           #{alignment VkRectLayerKHR}
           '[('FieldMeta "offset" VkOffset2D 'False  -- ' closing tick for hsc2hs
                                                    #{offset VkRectLayerKHR, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "extent" VkExtent2D 'False 
                                                    #{offset VkRectLayerKHR, extent}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "layer" Word32 'False 
                                               #{offset VkRectLayerKHR, layer}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
