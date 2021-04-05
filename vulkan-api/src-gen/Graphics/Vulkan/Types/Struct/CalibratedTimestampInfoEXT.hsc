#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.CalibratedTimestampInfoEXT
       (VkCalibratedTimestampInfoEXT) where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import Graphics.Vulkan.Types.Enum.TimeDomainEXT (VkTimeDomainEXT)

-- | > typedef struct VkCalibratedTimestampInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkTimeDomainEXT        timeDomain;
--   > } VkCalibratedTimestampInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCalibratedTimestampInfoEXT VkCalibratedTimestampInfoEXT registry at www.khronos.org>
type VkCalibratedTimestampInfoEXT =
     VkStruct VkCalibratedTimestampInfoEXT' -- ' closing tick for hsc2hs

data VkCalibratedTimestampInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkCalibratedTimestampInfoEXT where
    type StructRep VkCalibratedTimestampInfoEXT =
         'StructMeta "VkCalibratedTimestampInfoEXT" -- ' closing tick for hsc2hs
           VkCalibratedTimestampInfoEXT
           #{size VkCalibratedTimestampInfoEXT}
           #{alignment VkCalibratedTimestampInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkCalibratedTimestampInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkCalibratedTimestampInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "timeDomain" VkTimeDomainEXT 'False 
                                                             #{offset VkCalibratedTimestampInfoEXT, timeDomain}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
