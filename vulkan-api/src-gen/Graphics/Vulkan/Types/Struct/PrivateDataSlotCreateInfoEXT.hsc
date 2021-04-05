#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PrivateDataSlotCreateInfoEXT
       (VkPrivateDataSlotCreateInfoEXT) where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.PrivateDataSlotCreateFlagsEXT (VkPrivateDataSlotCreateFlagsEXT)
import Graphics.Vulkan.Types.Enum.StructureType                 (VkStructureType)

-- | > typedef struct VkPrivateDataSlotCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkPrivateDataSlotCreateFlagsEXT        flags;
--   > } VkPrivateDataSlotCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPrivateDataSlotCreateInfoEXT VkPrivateDataSlotCreateInfoEXT registry at www.khronos.org>
type VkPrivateDataSlotCreateInfoEXT =
     VkStruct VkPrivateDataSlotCreateInfoEXT' -- ' closing tick for hsc2hs

data VkPrivateDataSlotCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPrivateDataSlotCreateInfoEXT where
    type StructRep VkPrivateDataSlotCreateInfoEXT =
         'StructMeta "VkPrivateDataSlotCreateInfoEXT" -- ' closing tick for hsc2hs
           VkPrivateDataSlotCreateInfoEXT
           #{size VkPrivateDataSlotCreateInfoEXT}
           #{alignment VkPrivateDataSlotCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPrivateDataSlotCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPrivateDataSlotCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPrivateDataSlotCreateFlagsEXT 'False
                #{offset VkPrivateDataSlotCreateInfoEXT, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
