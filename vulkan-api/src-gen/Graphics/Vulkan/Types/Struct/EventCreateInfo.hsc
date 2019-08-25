#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.EventCreateInfo
       (VkEventCreateInfo) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkEventCreateFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)

-- | > typedef struct VkEventCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkEventCreateFlags     flags;
--   > } VkEventCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkEventCreateInfo VkEventCreateInfo registry at www.khronos.org>
type VkEventCreateInfo = VkStruct VkEventCreateInfo' -- ' closing tick for hsc2hs

data VkEventCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkEventCreateInfo where
    type StructRep VkEventCreateInfo =
         'StructMeta "VkEventCreateInfo" VkEventCreateInfo  -- ' closing tick for hsc2hs
                                                           #{size VkEventCreateInfo}
           #{alignment VkEventCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkEventCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkEventCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkEventCreateFlags 'True 
                                                          #{offset VkEventCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
