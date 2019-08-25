#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.ApplicationInfo
       (VkApplicationInfo) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)

-- | > typedef struct VkApplicationInfo {
--   >     VkStructureType sType;
--   >     const void*     pNext;
--   >     const char*     pApplicationName;
--   >     uint32_t        applicationVersion;
--   >     const char*     pEngineName;
--   >     uint32_t        engineVersion;
--   >     uint32_t        apiVersion;
--   > } VkApplicationInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkApplicationInfo VkApplicationInfo registry at www.khronos.org>
type VkApplicationInfo = VkStruct VkApplicationInfo' -- ' closing tick for hsc2hs

data VkApplicationInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkApplicationInfo where
    type StructRep VkApplicationInfo =
         'StructMeta "VkApplicationInfo" VkApplicationInfo  -- ' closing tick for hsc2hs
                                                           #{size VkApplicationInfo}
           #{alignment VkApplicationInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkApplicationInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkApplicationInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pApplicationName" CString 'True 
                                                          #{offset VkApplicationInfo, pApplicationName}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "applicationVersion" Word32 'False 
                                                            #{offset VkApplicationInfo, applicationVersion}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pEngineName" CString 'True 
                                                     #{offset VkApplicationInfo, pEngineName}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "engineVersion" Word32 'False 
                                                       #{offset VkApplicationInfo, engineVersion}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "apiVersion" Word32 'False 
                                                    #{offset VkApplicationInfo, apiVersion}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
