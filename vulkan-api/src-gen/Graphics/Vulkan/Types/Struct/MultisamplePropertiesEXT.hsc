#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.MultisamplePropertiesEXT
       (VkMultisamplePropertiesEXT) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.Extent      (VkExtent2D)

-- | > typedef struct VkMultisamplePropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExtent2D                       maxSampleLocationGridSize;
--   > } VkMultisamplePropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMultisamplePropertiesEXT VkMultisamplePropertiesEXT registry at www.khronos.org>
type VkMultisamplePropertiesEXT =
     VkStruct VkMultisamplePropertiesEXT' -- ' closing tick for hsc2hs

data VkMultisamplePropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMultisamplePropertiesEXT where
    type StructRep VkMultisamplePropertiesEXT =
         'StructMeta "VkMultisamplePropertiesEXT" VkMultisamplePropertiesEXT -- ' closing tick for hsc2hs
           #{size VkMultisamplePropertiesEXT}
           #{alignment VkMultisamplePropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMultisamplePropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMultisamplePropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxSampleLocationGridSize" VkExtent2D 'False 
                                                                       #{offset VkMultisamplePropertiesEXT, maxSampleLocationGridSize}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
