#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PushConstantRange
       (VkPushConstantRange) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Shader (VkShaderStageFlags)

-- | > typedef struct VkPushConstantRange {
--   >     VkShaderStageFlags     stageFlags;
--   >     uint32_t               offset;
--   >     uint32_t               size;
--   > } VkPushConstantRange;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPushConstantRange VkPushConstantRange registry at www.khronos.org>
type VkPushConstantRange = VkStruct VkPushConstantRange' -- ' closing tick for hsc2hs

data VkPushConstantRange' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPushConstantRange where
    type StructRep VkPushConstantRange =
         'StructMeta "VkPushConstantRange" VkPushConstantRange  -- ' closing tick for hsc2hs
                                                               #{size VkPushConstantRange}
           #{alignment VkPushConstantRange}
           '[('FieldMeta "stageFlags" VkShaderStageFlags 'False  -- ' closing tick for hsc2hs
                                                                #{offset VkPushConstantRange, stageFlags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "offset" Word32 'False 
                                                #{offset VkPushConstantRange, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "size" Word32 'False 
                                              #{offset VkPushConstantRange, size}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
