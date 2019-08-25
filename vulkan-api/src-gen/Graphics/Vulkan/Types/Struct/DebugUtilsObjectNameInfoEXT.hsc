#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.DebugUtilsObjectNameInfoEXT
       (VkDebugUtilsObjectNameInfoEXT) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Object        (VkObjectType)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)

-- | > typedef struct VkDebugUtilsObjectNameInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                                            pNext;
--   >     VkObjectType                                           objectType;
--   >     uint64_t                                               objectHandle;
--   >     const char*      pObjectName;
--   > } VkDebugUtilsObjectNameInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugUtilsObjectNameInfoEXT VkDebugUtilsObjectNameInfoEXT registry at www.khronos.org>
type VkDebugUtilsObjectNameInfoEXT =
     VkStruct VkDebugUtilsObjectNameInfoEXT' -- ' closing tick for hsc2hs

data VkDebugUtilsObjectNameInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDebugUtilsObjectNameInfoEXT where
    type StructRep VkDebugUtilsObjectNameInfoEXT =
         'StructMeta "VkDebugUtilsObjectNameInfoEXT" -- ' closing tick for hsc2hs
           VkDebugUtilsObjectNameInfoEXT
           #{size VkDebugUtilsObjectNameInfoEXT}
           #{alignment VkDebugUtilsObjectNameInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDebugUtilsObjectNameInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDebugUtilsObjectNameInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "objectType" VkObjectType 'False 
                                                          #{offset VkDebugUtilsObjectNameInfoEXT, objectType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "objectHandle" Word64 'False 
                                                      #{offset VkDebugUtilsObjectNameInfoEXT, objectHandle}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pObjectName" CString 'True 
                                                     #{offset VkDebugUtilsObjectNameInfoEXT, pObjectName}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
