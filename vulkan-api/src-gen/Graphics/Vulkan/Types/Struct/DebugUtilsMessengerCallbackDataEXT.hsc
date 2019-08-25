#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.DebugUtilsMessengerCallbackDataEXT
       (VkDebugUtilsMessengerCallbackDataEXT) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                           (VkDebugUtilsMessengerCallbackDataFlagsEXT)
import           Graphics.Vulkan.Types.Enum.StructureType                 (VkStructureType)
import           Graphics.Vulkan.Types.Struct.DebugUtilsLabelEXT          (VkDebugUtilsLabelEXT)
import           Graphics.Vulkan.Types.Struct.DebugUtilsObjectNameInfoEXT (VkDebugUtilsObjectNameInfoEXT)

-- | > typedef struct VkDebugUtilsMessengerCallbackDataEXT {
--   >     VkStructureType sType;
--   >     const void*                                                        pNext;
--   >     VkDebugUtilsMessengerCallbackDataFlagsEXT                          flags;
--   >     const char*                                  pMessageIdName;
--   >     int32_t                                                            messageIdNumber;
--   >     const char*                                                  pMessage;
--   >     uint32_t                                                           queueLabelCount;
--   >     VkDebugUtilsLabelEXT*  pQueueLabels;
--   >     uint32_t                                                           cmdBufLabelCount;
--   >     VkDebugUtilsLabelEXT* pCmdBufLabels;
--   >     uint32_t                                                                           objectCount;
--   >     VkDebugUtilsObjectNameInfoEXT*             pObjects;
--   > } VkDebugUtilsMessengerCallbackDataEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDebugUtilsMessengerCallbackDataEXT VkDebugUtilsMessengerCallbackDataEXT registry at www.khronos.org>
type VkDebugUtilsMessengerCallbackDataEXT =
     VkStruct VkDebugUtilsMessengerCallbackDataEXT' -- ' closing tick for hsc2hs

data VkDebugUtilsMessengerCallbackDataEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDebugUtilsMessengerCallbackDataEXT where
    type StructRep VkDebugUtilsMessengerCallbackDataEXT =
         'StructMeta "VkDebugUtilsMessengerCallbackDataEXT" -- ' closing tick for hsc2hs
           VkDebugUtilsMessengerCallbackDataEXT
           #{size VkDebugUtilsMessengerCallbackDataEXT}
           #{alignment VkDebugUtilsMessengerCallbackDataEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDebugUtilsMessengerCallbackDataEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'True 
                                                  #{offset VkDebugUtilsMessengerCallbackDataEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkDebugUtilsMessengerCallbackDataFlagsEXT 'True
                #{offset VkDebugUtilsMessengerCallbackDataEXT, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pMessageIdName" CString 'True 
                                                        #{offset VkDebugUtilsMessengerCallbackDataEXT, pMessageIdName}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "messageIdNumber" Int32 'True 
                                                       #{offset VkDebugUtilsMessengerCallbackDataEXT, messageIdNumber}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pMessage" CString 'False 
                                                   #{offset VkDebugUtilsMessengerCallbackDataEXT, pMessage}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queueLabelCount" Word32 'True 
                                                        #{offset VkDebugUtilsMessengerCallbackDataEXT, queueLabelCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pQueueLabels" (Ptr VkDebugUtilsLabelEXT) 'True
                #{offset VkDebugUtilsMessengerCallbackDataEXT, pQueueLabels}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "cmdBufLabelCount" Word32 'True 
                                                         #{offset VkDebugUtilsMessengerCallbackDataEXT, cmdBufLabelCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pCmdBufLabels" (Ptr VkDebugUtilsLabelEXT) 'True
                #{offset VkDebugUtilsMessengerCallbackDataEXT, pCmdBufLabels}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "objectCount" Word32 'False 
                                                     #{offset VkDebugUtilsMessengerCallbackDataEXT, objectCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pObjects" (Ptr VkDebugUtilsObjectNameInfoEXT) 'False
                #{offset VkDebugUtilsMessengerCallbackDataEXT, pObjects}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
