#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.SetStateFlagsIndirectCommandNV
       (VkSetStateFlagsIndirectCommandNV) where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal

-- | > typedef struct VkSetStateFlagsIndirectCommandNV {
--   >     uint32_t          data;
--   > } VkSetStateFlagsIndirectCommandNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkSetStateFlagsIndirectCommandNV VkSetStateFlagsIndirectCommandNV registry at www.khronos.org>
type VkSetStateFlagsIndirectCommandNV =
     VkStruct VkSetStateFlagsIndirectCommandNV' -- ' closing tick for hsc2hs

data VkSetStateFlagsIndirectCommandNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkSetStateFlagsIndirectCommandNV where
    type StructRep VkSetStateFlagsIndirectCommandNV =
         'StructMeta "VkSetStateFlagsIndirectCommandNV" -- ' closing tick for hsc2hs
           VkSetStateFlagsIndirectCommandNV
           #{size VkSetStateFlagsIndirectCommandNV}
           #{alignment VkSetStateFlagsIndirectCommandNV}
           '[('FieldMeta "data" Word32 'False  -- ' closing tick for hsc2hs
                                              #{offset VkSetStateFlagsIndirectCommandNV, data}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
