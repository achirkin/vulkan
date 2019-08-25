#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.ExtensionProperties
       (VkExtensionProperties) where
import           Graphics.Vulkan.Constants        (VK_MAX_EXTENSION_NAME_SIZE)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal

-- | > typedef struct VkExtensionProperties {
--   >     char            extensionName[VK_MAX_EXTENSION_NAME_SIZE];
--   >     uint32_t        specVersion;
--   > } VkExtensionProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExtensionProperties VkExtensionProperties registry at www.khronos.org>
type VkExtensionProperties = VkStruct VkExtensionProperties' -- ' closing tick for hsc2hs

data VkExtensionProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExtensionProperties where
    type StructRep VkExtensionProperties =
         'StructMeta "VkExtensionProperties" VkExtensionProperties  -- ' closing tick for hsc2hs
                                                                   #{size VkExtensionProperties}
           #{alignment VkExtensionProperties}
           '[('FieldMeta "extensionName" CChar 'False  -- ' closing tick for hsc2hs
                                                      #{offset VkExtensionProperties, extensionName}
                VK_MAX_EXTENSION_NAME_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "specVersion" Word32 'False 
                                                     #{offset VkExtensionProperties, specVersion}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
