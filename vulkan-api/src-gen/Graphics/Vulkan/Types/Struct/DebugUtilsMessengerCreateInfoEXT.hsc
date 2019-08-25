#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.DebugUtilsMessengerCreateInfoEXT
       (VkDebugUtilsMessengerCreateInfoEXT) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                  (VkDebugUtilsMessengerCreateFlagsEXT)
import           Graphics.Vulkan.Types.Enum.Debug                (VkDebugUtilsMessageSeverityFlagsEXT,
                                                                  VkDebugUtilsMessageTypeFlagsEXT)
import           Graphics.Vulkan.Types.Enum.StructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Funcpointers              (PFN_vkDebugUtilsMessengerCallbackEXT)
import           Graphics.Vulkan.Types.Struct.InstanceCreateInfo (VkInstanceCreateInfo)

-- | > typedef struct VkDebugUtilsMessengerCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                                          pNext;
--   >     VkDebugUtilsMessengerCreateFlagsEXT  flags;
--   >     VkDebugUtilsMessageSeverityFlagsEXT                  messageSeverity;
--   >     VkDebugUtilsMessageTypeFlagsEXT                      messageType;
--   >     PFN_vkDebugUtilsMessengerCallbackEXT                 pfnUserCallback;
--   >     void*                                pUserData;
--   > } VkDebugUtilsMessengerCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugUtilsMessengerCreateInfoEXT VkDebugUtilsMessengerCreateInfoEXT registry at www.khronos.org>
type VkDebugUtilsMessengerCreateInfoEXT =
     VkStruct VkDebugUtilsMessengerCreateInfoEXT' -- ' closing tick for hsc2hs

data VkDebugUtilsMessengerCreateInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDebugUtilsMessengerCreateInfoEXT where
    type StructRep VkDebugUtilsMessengerCreateInfoEXT =
         'StructMeta "VkDebugUtilsMessengerCreateInfoEXT" -- ' closing tick for hsc2hs
           VkDebugUtilsMessengerCreateInfoEXT
           #{size VkDebugUtilsMessengerCreateInfoEXT}
           #{alignment VkDebugUtilsMessengerCreateInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDebugUtilsMessengerCreateInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkDebugUtilsMessengerCreateInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkDebugUtilsMessengerCreateFlagsEXT 'True
                #{offset VkDebugUtilsMessengerCreateInfoEXT, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "messageSeverity" VkDebugUtilsMessageSeverityFlagsEXT -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkDebugUtilsMessengerCreateInfoEXT, messageSeverity}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "messageType" VkDebugUtilsMessageTypeFlagsEXT 'False
                #{offset VkDebugUtilsMessengerCreateInfoEXT, messageType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pfnUserCallback" PFN_vkDebugUtilsMessengerCallbackEXT -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkDebugUtilsMessengerCreateInfoEXT, pfnUserCallback}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pUserData" (Ptr Void) 'True 
                                                      #{offset VkDebugUtilsMessengerCreateInfoEXT, pUserData}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkInstanceCreateInfo] -- ' closing tick for hsc2hs
