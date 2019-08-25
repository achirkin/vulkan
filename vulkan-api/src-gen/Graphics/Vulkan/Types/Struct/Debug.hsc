#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Debug
       (VkDebugMarkerMarkerInfoEXT, VkDebugMarkerObjectNameInfoEXT,
        VkDebugMarkerObjectTagInfoEXT, VkDebugReportCallbackCreateInfoEXT,
        VkDebugUtilsObjectTagInfoEXT)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Debug                (VkDebugReportFlagsEXT,
                                                                  VkDebugReportObjectTypeEXT)
import           Graphics.Vulkan.Types.Enum.Object               (VkObjectType)
import           Graphics.Vulkan.Types.Enum.StructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Funcpointers              (PFN_vkDebugReportCallbackEXT)
import           Graphics.Vulkan.Types.Struct.InstanceCreateInfo (VkInstanceCreateInfo)

-- | > typedef struct VkDebugMarkerMarkerInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const char* pMarkerName;
--   >     float            color[4];
--   > } VkDebugMarkerMarkerInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugMarkerMarkerInfoEXT VkDebugMarkerMarkerInfoEXT registry at www.khronos.org>
type VkDebugMarkerMarkerInfoEXT =
     VkStruct VkDebugMarkerMarkerInfoEXT' -- ' closing tick for hsc2hs

data VkDebugMarkerMarkerInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDebugMarkerMarkerInfoEXT where
    type StructRep VkDebugMarkerMarkerInfoEXT =
         'StructMeta "VkDebugMarkerMarkerInfoEXT" VkDebugMarkerMarkerInfoEXT -- ' closing tick for hsc2hs
           #{size VkDebugMarkerMarkerInfoEXT}
           #{alignment VkDebugMarkerMarkerInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDebugMarkerMarkerInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDebugMarkerMarkerInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pMarkerName" CString 'False 
                                                      #{offset VkDebugMarkerMarkerInfoEXT, pMarkerName}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "color" ( -- ' closing tick for hsc2hs
                                  #{type float}
                                  ) 'True  -- ' closing tick for hsc2hs
                                          #{offset VkDebugMarkerMarkerInfoEXT, color}
                4
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDebugMarkerObjectNameInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDebugReportObjectTypeEXT       objectType;
--   >     uint64_t                         object;
--   >     const char* pObjectName;
--   > } VkDebugMarkerObjectNameInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugMarkerObjectNameInfoEXT VkDebugMarkerObjectNameInfoEXT registry at www.khronos.org>
type VkDebugMarkerObjectNameInfoEXT =
     VkStruct VkDebugMarkerObjectNameInfoEXT' -- ' closing tick for hsc2hs

data VkDebugMarkerObjectNameInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDebugMarkerObjectNameInfoEXT where
    type StructRep VkDebugMarkerObjectNameInfoEXT =
         'StructMeta "VkDebugMarkerObjectNameInfoEXT" -- ' closing tick for hsc2hs
           VkDebugMarkerObjectNameInfoEXT
           #{size VkDebugMarkerObjectNameInfoEXT}
           #{alignment VkDebugMarkerObjectNameInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDebugMarkerObjectNameInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDebugMarkerObjectNameInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "objectType" VkDebugReportObjectTypeEXT 'False
                #{offset VkDebugMarkerObjectNameInfoEXT, objectType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "object" Word64 'False 
                                                #{offset VkDebugMarkerObjectNameInfoEXT, object}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pObjectName" CString 'False 
                                                      #{offset VkDebugMarkerObjectNameInfoEXT, pObjectName}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDebugMarkerObjectTagInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDebugReportObjectTypeEXT       objectType;
--   >     uint64_t                         object;
--   >     uint64_t                         tagName;
--   >     size_t                           tagSize;
--   >     const void*        pTag;
--   > } VkDebugMarkerObjectTagInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugMarkerObjectTagInfoEXT VkDebugMarkerObjectTagInfoEXT registry at www.khronos.org>
type VkDebugMarkerObjectTagInfoEXT =
     VkStruct VkDebugMarkerObjectTagInfoEXT' -- ' closing tick for hsc2hs

data VkDebugMarkerObjectTagInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDebugMarkerObjectTagInfoEXT where
    type StructRep VkDebugMarkerObjectTagInfoEXT =
         'StructMeta "VkDebugMarkerObjectTagInfoEXT" -- ' closing tick for hsc2hs
           VkDebugMarkerObjectTagInfoEXT
           #{size VkDebugMarkerObjectTagInfoEXT}
           #{alignment VkDebugMarkerObjectTagInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDebugMarkerObjectTagInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDebugMarkerObjectTagInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "objectType" VkDebugReportObjectTypeEXT 'False
                #{offset VkDebugMarkerObjectTagInfoEXT, objectType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "object" Word64 'False 
                                                #{offset VkDebugMarkerObjectTagInfoEXT, object}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "tagName" Word64 'False 
                                                 #{offset VkDebugMarkerObjectTagInfoEXT, tagName}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "tagSize" CSize 'False 
                                                #{offset VkDebugMarkerObjectTagInfoEXT, tagSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pTag" (Ptr Void) 'False 
                                                  #{offset VkDebugMarkerObjectTagInfoEXT, pTag}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDebugReportCallbackCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDebugReportFlagsEXT            flags;
--   >     PFN_vkDebugReportCallbackEXT     pfnCallback;
--   >     void*            pUserData;
--   > } VkDebugReportCallbackCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugReportCallbackCreateInfoEXT VkDebugReportCallbackCreateInfoEXT registry at www.khronos.org>
type VkDebugReportCallbackCreateInfoEXT =
     VkStruct VkDebugReportCallbackCreateInfoEXT' -- ' closing tick for hsc2hs

data VkDebugReportCallbackCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDebugReportCallbackCreateInfoEXT where
    type StructRep VkDebugReportCallbackCreateInfoEXT =
         'StructMeta "VkDebugReportCallbackCreateInfoEXT" -- ' closing tick for hsc2hs
           VkDebugReportCallbackCreateInfoEXT
           #{size VkDebugReportCallbackCreateInfoEXT}
           #{alignment VkDebugReportCallbackCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDebugReportCallbackCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDebugReportCallbackCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkDebugReportFlagsEXT 'True 
                                                             #{offset VkDebugReportCallbackCreateInfoEXT, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pfnCallback" PFN_vkDebugReportCallbackEXT 'False
                #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pUserData" (Ptr Void) 'True 
                                                      #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkInstanceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkDebugUtilsObjectTagInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkObjectType                           objectType;
--   >     uint64_t                               objectHandle;
--   >     uint64_t                               tagName;
--   >     size_t                                 tagSize;
--   >     const void*              pTag;
--   > } VkDebugUtilsObjectTagInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugUtilsObjectTagInfoEXT VkDebugUtilsObjectTagInfoEXT registry at www.khronos.org>
type VkDebugUtilsObjectTagInfoEXT =
     VkStruct VkDebugUtilsObjectTagInfoEXT' -- ' closing tick for hsc2hs

data VkDebugUtilsObjectTagInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDebugUtilsObjectTagInfoEXT where
    type StructRep VkDebugUtilsObjectTagInfoEXT =
         'StructMeta "VkDebugUtilsObjectTagInfoEXT" -- ' closing tick for hsc2hs
           VkDebugUtilsObjectTagInfoEXT
           #{size VkDebugUtilsObjectTagInfoEXT}
           #{alignment VkDebugUtilsObjectTagInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDebugUtilsObjectTagInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDebugUtilsObjectTagInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "objectType" VkObjectType 'False 
                                                          #{offset VkDebugUtilsObjectTagInfoEXT, objectType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "objectHandle" Word64 'False 
                                                      #{offset VkDebugUtilsObjectTagInfoEXT, objectHandle}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "tagName" Word64 'False 
                                                 #{offset VkDebugUtilsObjectTagInfoEXT, tagName}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "tagSize" CSize 'False 
                                                #{offset VkDebugUtilsObjectTagInfoEXT, tagSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pTag" (Ptr Void) 'False 
                                                  #{offset VkDebugUtilsObjectTagInfoEXT, pTag}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
