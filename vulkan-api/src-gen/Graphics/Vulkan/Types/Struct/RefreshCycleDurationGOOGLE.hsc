#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.RefreshCycleDurationGOOGLE
       (VkRefreshCycleDurationGOOGLE) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal

-- | > typedef struct VkRefreshCycleDurationGOOGLE {
--   >     uint64_t                         refreshDuration;
--   > } VkRefreshCycleDurationGOOGLE;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkRefreshCycleDurationGOOGLE VkRefreshCycleDurationGOOGLE registry at www.khronos.org>
type VkRefreshCycleDurationGOOGLE =
     VkStruct VkRefreshCycleDurationGOOGLE' -- ' closing tick for hsc2hs

data VkRefreshCycleDurationGOOGLE' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRefreshCycleDurationGOOGLE where
    type StructRep VkRefreshCycleDurationGOOGLE =
         'StructMeta "VkRefreshCycleDurationGOOGLE" -- ' closing tick for hsc2hs
           VkRefreshCycleDurationGOOGLE
           #{size VkRefreshCycleDurationGOOGLE}
           #{alignment VkRefreshCycleDurationGOOGLE}
           '[('FieldMeta "refreshDuration" Word64 'False  -- ' closing tick for hsc2hs
                                                         #{offset VkRefreshCycleDurationGOOGLE, refreshDuration}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
