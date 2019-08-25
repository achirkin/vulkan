#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Clear
       (VkClearAttachment, VkClearColorValue, VkClearDepthStencilValue,
        VkClearRect, VkClearValue)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Image  (VkImageAspectFlags)
import           Graphics.Vulkan.Types.Struct.Rect (VkRect2D)

-- | > typedef struct VkClearAttachment {
--   >     VkImageAspectFlags     aspectMask;
--   >     uint32_t               colorAttachment;
--   >     VkClearValue           clearValue;
--   > } VkClearAttachment;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkClearAttachment VkClearAttachment registry at www.khronos.org>
type VkClearAttachment = VkStruct VkClearAttachment' -- ' closing tick for hsc2hs

data VkClearAttachment' -- ' closing tick for hsc2hs

instance VulkanMarshal VkClearAttachment where
    type StructRep VkClearAttachment =
         'StructMeta "VkClearAttachment" VkClearAttachment  -- ' closing tick for hsc2hs
                                                           #{size VkClearAttachment}
           #{alignment VkClearAttachment}
           '[('FieldMeta "aspectMask" VkImageAspectFlags 'False  -- ' closing tick for hsc2hs
                                                                #{offset VkClearAttachment, aspectMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "colorAttachment" Word32 'False 
                                                         #{offset VkClearAttachment, colorAttachment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "clearValue" VkClearValue 'False 
                                                          #{offset VkClearAttachment, clearValue}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | // Union allowing specification of floating point, integer, or unsigned integer color data. Actual value selected is based on image/attachment being cleared.
--
--   > typedef union VkClearColorValue {
--   >     float                  float32[4];
--   >     int32_t                int32[4];
--   >     uint32_t               uint32[4];
--   > } VkClearColorValue;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkClearColorValue VkClearColorValue registry at www.khronos.org>
type VkClearColorValue = VkStruct VkClearColorValue' -- ' closing tick for hsc2hs

data VkClearColorValue' -- ' closing tick for hsc2hs

instance VulkanMarshal VkClearColorValue where
    type StructRep VkClearColorValue =
         'StructMeta "VkClearColorValue" VkClearColorValue  -- ' closing tick for hsc2hs
                                                           #{size VkClearColorValue}
           #{alignment VkClearColorValue}
           '[('FieldMeta "float32" (
                                    #{type float}
                                    ) 'False -- ' closing tick for hsc2hs
                #{offset VkClearColorValue, float32}
                4
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "int32" Int32 'False 
                                              #{offset VkClearColorValue, int32}
                4
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "uint32" Word32 'False 
                                                #{offset VkClearColorValue, uint32}
                4
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkClearDepthStencilValue {
--   >     float                  depth;
--   >     uint32_t               stencil;
--   > } VkClearDepthStencilValue;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkClearDepthStencilValue VkClearDepthStencilValue registry at www.khronos.org>
type VkClearDepthStencilValue = VkStruct VkClearDepthStencilValue' -- ' closing tick for hsc2hs

data VkClearDepthStencilValue' -- ' closing tick for hsc2hs

instance VulkanMarshal VkClearDepthStencilValue where
    type StructRep VkClearDepthStencilValue =
         'StructMeta "VkClearDepthStencilValue" VkClearDepthStencilValue -- ' closing tick for hsc2hs
           #{size VkClearDepthStencilValue}
           #{alignment VkClearDepthStencilValue}
           '[('FieldMeta "depth" (
                                  #{type float}
                                  ) 'False  -- ' closing tick for hsc2hs
                                           #{offset VkClearDepthStencilValue, depth}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stencil" Word32 'False 
                                                 #{offset VkClearDepthStencilValue, stencil}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkClearRect {
--   >     VkRect2D       rect;
--   >     uint32_t       baseArrayLayer;
--   >     uint32_t       layerCount;
--   > } VkClearRect;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkClearRect VkClearRect registry at www.khronos.org>
type VkClearRect = VkStruct VkClearRect' -- ' closing tick for hsc2hs

data VkClearRect' -- ' closing tick for hsc2hs

instance VulkanMarshal VkClearRect where
    type StructRep VkClearRect =
         'StructMeta "VkClearRect" VkClearRect  -- ' closing tick for hsc2hs
                                               #{size VkClearRect}
           #{alignment VkClearRect}
           '[('FieldMeta "rect" VkRect2D 'False  -- ' closing tick for hsc2hs
                                                #{offset VkClearRect, rect}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "baseArrayLayer" Word32 'False 
                                                        #{offset VkClearRect, baseArrayLayer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "layerCount" Word32 'False 
                                                    #{offset VkClearRect, layerCount}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | // Union allowing specification of color or depth and stencil values. Actual value selected is based on attachment being cleared.
--
--   > typedef union VkClearValue {
--   >     VkClearColorValue      color;
--   >     VkClearDepthStencilValue depthStencil;
--   > } VkClearValue;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkClearValue VkClearValue registry at www.khronos.org>
type VkClearValue = VkStruct VkClearValue' -- ' closing tick for hsc2hs

data VkClearValue' -- ' closing tick for hsc2hs

instance VulkanMarshal VkClearValue where
    type StructRep VkClearValue =
         'StructMeta "VkClearValue" VkClearValue  -- ' closing tick for hsc2hs
                                                 #{size VkClearValue}
           #{alignment VkClearValue}
           '[('FieldMeta "color" VkClearColorValue 'False  -- ' closing tick for hsc2hs
                                                          #{offset VkClearValue, color}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthStencil" VkClearDepthStencilValue 'False
                #{offset VkClearValue, depthStencil}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
