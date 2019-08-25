#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.LayerProperties
       (VkLayerProperties) where
import           Graphics.Vulkan.Constants        (VK_MAX_DESCRIPTION_SIZE,
                                                   VK_MAX_EXTENSION_NAME_SIZE)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal

-- | > typedef struct VkLayerProperties {
--   >     char            layerName[VK_MAX_EXTENSION_NAME_SIZE];
--   >     uint32_t        specVersion;
--   >     uint32_t        implementationVersion;
--   >     char            description[VK_MAX_DESCRIPTION_SIZE];
--   > } VkLayerProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkLayerProperties VkLayerProperties registry at www.khronos.org>
type VkLayerProperties = VkStruct VkLayerProperties' -- ' closing tick for hsc2hs

data VkLayerProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkLayerProperties where
    type StructRep VkLayerProperties =
         'StructMeta "VkLayerProperties" VkLayerProperties  -- ' closing tick for hsc2hs
                                                           #{size VkLayerProperties}
           #{alignment VkLayerProperties}
           '[('FieldMeta "layerName" CChar 'False  -- ' closing tick for hsc2hs
                                                  #{offset VkLayerProperties, layerName}
                VK_MAX_EXTENSION_NAME_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "specVersion" Word32 'False 
                                                     #{offset VkLayerProperties, specVersion}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "implementationVersion" Word32 'False 
                                                               #{offset VkLayerProperties, implementationVersion}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "description" CChar 'False 
                                                    #{offset VkLayerProperties, description}
                VK_MAX_DESCRIPTION_SIZE
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
