#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PastPresentationTimingGOOGLE
       (VkPastPresentationTimingGOOGLE) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal

-- | > typedef struct VkPastPresentationTimingGOOGLE {
--   >     uint32_t                         presentID;
--   >     uint64_t                         desiredPresentTime;
--   >     uint64_t                         actualPresentTime;
--   >     uint64_t                         earliestPresentTime;
--   >     uint64_t                         presentMargin;
--   > } VkPastPresentationTimingGOOGLE;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPastPresentationTimingGOOGLE VkPastPresentationTimingGOOGLE registry at www.khronos.org>
type VkPastPresentationTimingGOOGLE =
     VkStruct VkPastPresentationTimingGOOGLE' -- ' closing tick for hsc2hs

data VkPastPresentationTimingGOOGLE' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPastPresentationTimingGOOGLE where
    type StructRep VkPastPresentationTimingGOOGLE =
         'StructMeta "VkPastPresentationTimingGOOGLE" -- ' closing tick for hsc2hs
           VkPastPresentationTimingGOOGLE
           #{size VkPastPresentationTimingGOOGLE}
           #{alignment VkPastPresentationTimingGOOGLE}
           '[('FieldMeta "presentID" Word32 'False  -- ' closing tick for hsc2hs
                                                   #{offset VkPastPresentationTimingGOOGLE, presentID}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "desiredPresentTime" Word64 'False 
                                                            #{offset VkPastPresentationTimingGOOGLE, desiredPresentTime}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "actualPresentTime" Word64 'False 
                                                           #{offset VkPastPresentationTimingGOOGLE, actualPresentTime}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "earliestPresentTime" Word64 'False 
                                                             #{offset VkPastPresentationTimingGOOGLE, earliestPresentTime}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "presentMargin" Word64 'False 
                                                       #{offset VkPastPresentationTimingGOOGLE, presentMargin}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
