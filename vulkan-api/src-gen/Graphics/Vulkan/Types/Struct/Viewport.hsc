#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Viewport
       (VkViewport, VkViewportSwizzleNV, VkViewportWScalingNV) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.ViewportCoordinateSwizzleNV (VkViewportCoordinateSwizzleNV)

-- | > typedef struct VkViewport {
--   >     float          x;
--   >     float          y;
--   >     float          width;
--   >     float          height;
--   >     float          minDepth;
--   >     float          maxDepth;
--   > } VkViewport;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkViewport VkViewport registry at www.khronos.org>
type VkViewport = VkStruct VkViewport' -- ' closing tick for hsc2hs

data VkViewport' -- ' closing tick for hsc2hs

instance VulkanMarshal VkViewport where
    type StructRep VkViewport =
         'StructMeta "VkViewport" VkViewport  -- ' closing tick for hsc2hs
                                             #{size VkViewport}
           #{alignment VkViewport}
           '[('FieldMeta "x" (
                              #{type float}
                              ) 'False  -- ' closing tick for hsc2hs
                                       #{offset VkViewport, x}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "y" ( -- ' closing tick for hsc2hs
                              #{type float}
                              ) 'False  -- ' closing tick for hsc2hs
                                       #{offset VkViewport, y}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "width" ( -- ' closing tick for hsc2hs
                                  #{type float}
                                  ) 'False  -- ' closing tick for hsc2hs
                                           #{offset VkViewport, width}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "height" ( -- ' closing tick for hsc2hs
                                   #{type float}
                                   ) 'False  -- ' closing tick for hsc2hs
                                            #{offset VkViewport, height}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minDepth" ( -- ' closing tick for hsc2hs
                                     #{type float}
                                     ) 'False  -- ' closing tick for hsc2hs
                                              #{offset VkViewport, minDepth}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDepth" ( -- ' closing tick for hsc2hs
                                     #{type float}
                                     ) 'False  -- ' closing tick for hsc2hs
                                              #{offset VkViewport, maxDepth}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkViewportSwizzleNV {
--   >     VkViewportCoordinateSwizzleNV          x;
--   >     VkViewportCoordinateSwizzleNV          y;
--   >     VkViewportCoordinateSwizzleNV          z;
--   >     VkViewportCoordinateSwizzleNV          w;
--   > } VkViewportSwizzleNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkViewportSwizzleNV VkViewportSwizzleNV registry at www.khronos.org>
type VkViewportSwizzleNV = VkStruct VkViewportSwizzleNV' -- ' closing tick for hsc2hs

data VkViewportSwizzleNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkViewportSwizzleNV where
    type StructRep VkViewportSwizzleNV =
         'StructMeta "VkViewportSwizzleNV" VkViewportSwizzleNV  -- ' closing tick for hsc2hs
                                                               #{size VkViewportSwizzleNV}
           #{alignment VkViewportSwizzleNV}
           '[('FieldMeta "x" VkViewportCoordinateSwizzleNV 'False  -- ' closing tick for hsc2hs
                                                                  #{offset VkViewportSwizzleNV, x}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "y" VkViewportCoordinateSwizzleNV 'False 
                                                                  #{offset VkViewportSwizzleNV, y}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "z" VkViewportCoordinateSwizzleNV 'False 
                                                                  #{offset VkViewportSwizzleNV, z}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "w" VkViewportCoordinateSwizzleNV 'False 
                                                                  #{offset VkViewportSwizzleNV, w}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkViewportWScalingNV {
--   >     float          xcoeff;
--   >     float          ycoeff;
--   > } VkViewportWScalingNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkViewportWScalingNV VkViewportWScalingNV registry at www.khronos.org>
type VkViewportWScalingNV = VkStruct VkViewportWScalingNV' -- ' closing tick for hsc2hs

data VkViewportWScalingNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkViewportWScalingNV where
    type StructRep VkViewportWScalingNV =
         'StructMeta "VkViewportWScalingNV" VkViewportWScalingNV  -- ' closing tick for hsc2hs
                                                                 #{size VkViewportWScalingNV}
           #{alignment VkViewportWScalingNV}
           '[('FieldMeta "xcoeff" (
                                   #{type float}
                                   ) 'False  -- ' closing tick for hsc2hs
                                            #{offset VkViewportWScalingNV, xcoeff}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "ycoeff" ( -- ' closing tick for hsc2hs
                                   #{type float}
                                   ) 'False  -- ' closing tick for hsc2hs
                                            #{offset VkViewportWScalingNV, ycoeff}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
