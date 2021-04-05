#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.ConformanceVersion
       (VkConformanceVersion, VkConformanceVersionKHR) where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal

-- | > typedef struct VkConformanceVersion {
--   >     uint8_t                          major;
--   >     uint8_t                          minor;
--   >     uint8_t                          subminor;
--   >     uint8_t                          patch;
--   > } VkConformanceVersion;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkConformanceVersion VkConformanceVersion registry at www.khronos.org>
type VkConformanceVersion = VkStruct VkConformanceVersion' -- ' closing tick for hsc2hs

data VkConformanceVersion' -- ' closing tick for hsc2hs

instance VulkanMarshal VkConformanceVersion where
    type StructRep VkConformanceVersion =
         'StructMeta "VkConformanceVersion" VkConformanceVersion  -- ' closing tick for hsc2hs
                                                                 #{size VkConformanceVersion}
           #{alignment VkConformanceVersion}
           '[('FieldMeta "major" Word8 'False  -- ' closing tick for hsc2hs
                                              #{offset VkConformanceVersion, major}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minor" Word8 'False 
                                              #{offset VkConformanceVersion, minor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subminor" Word8 'False 
                                                 #{offset VkConformanceVersion, subminor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "patch" Word8 'False 
                                              #{offset VkConformanceVersion, patch}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkConformanceVersion`
type VkConformanceVersionKHR = VkConformanceVersion
