#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.InstanceCreateInfo
       (VkInstanceCreateInfo) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks               (VkInstanceCreateFlags)
import           Graphics.Vulkan.Types.Enum.StructureType     (VkStructureType)
import           Graphics.Vulkan.Types.Struct.ApplicationInfo (VkApplicationInfo)

-- | > typedef struct VkInstanceCreateInfo {
--   >     VkStructureType sType;
--   >     const void*     pNext;
--   >     VkInstanceCreateFlags  flags;
--   >     const VkApplicationInfo* pApplicationInfo;
--   >     uint32_t               enabledLayerCount;
--   >     const char* const*      ppEnabledLayerNames;
--   >     uint32_t               enabledExtensionCount;
--   >     const char* const*      ppEnabledExtensionNames;
--   > } VkInstanceCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkInstanceCreateInfo VkInstanceCreateInfo registry at www.khronos.org>
type VkInstanceCreateInfo = VkStruct VkInstanceCreateInfo' -- ' closing tick for hsc2hs

data VkInstanceCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkInstanceCreateInfo where
    type StructRep VkInstanceCreateInfo =
         'StructMeta "VkInstanceCreateInfo" VkInstanceCreateInfo  -- ' closing tick for hsc2hs
                                                                 #{size VkInstanceCreateInfo}
           #{alignment VkInstanceCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkInstanceCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkInstanceCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkInstanceCreateFlags 'True 
                                                             #{offset VkInstanceCreateInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pApplicationInfo" (Ptr VkApplicationInfo) 'True
                #{offset VkInstanceCreateInfo, pApplicationInfo}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "enabledLayerCount" Word32 'True 
                                                          #{offset VkInstanceCreateInfo, enabledLayerCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "ppEnabledLayerNames" (Ptr CString) 'False 
                                                                    #{offset VkInstanceCreateInfo, ppEnabledLayerNames}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "enabledExtensionCount" Word32 'True 
                                                              #{offset VkInstanceCreateInfo, enabledExtensionCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "ppEnabledExtensionNames" (Ptr CString) 'False
                #{offset VkInstanceCreateInfo, ppEnabledExtensionNames}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
