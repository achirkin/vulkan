#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.DebugUtilsLabelEXT
       (VkDebugUtilsLabelEXT) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)

-- | > typedef struct VkDebugUtilsLabelEXT {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     const char*      pLabelName;
--   >     float                  color[4];
--   > } VkDebugUtilsLabelEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugUtilsLabelEXT VkDebugUtilsLabelEXT registry at www.khronos.org>
type VkDebugUtilsLabelEXT = VkStruct VkDebugUtilsLabelEXT' -- ' closing tick for hsc2hs

data VkDebugUtilsLabelEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDebugUtilsLabelEXT where
    type StructRep VkDebugUtilsLabelEXT =
         'StructMeta "VkDebugUtilsLabelEXT" VkDebugUtilsLabelEXT  -- ' closing tick for hsc2hs
                                                                 #{size VkDebugUtilsLabelEXT}
           #{alignment VkDebugUtilsLabelEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDebugUtilsLabelEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDebugUtilsLabelEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pLabelName" CString 'False 
                                                     #{offset VkDebugUtilsLabelEXT, pLabelName}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "color" ( -- ' closing tick for hsc2hs
                                  #{type float}
                                  ) 'True  -- ' closing tick for hsc2hs
                                          #{offset VkDebugUtilsLabelEXT, color}
                4
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
