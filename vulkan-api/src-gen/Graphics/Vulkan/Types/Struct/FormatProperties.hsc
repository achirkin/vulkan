#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.FormatProperties
       (VkFormatProperties, VkFormatProperties2, VkFormatProperties2KHR)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Format        (VkFormatFeatureFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)

-- | > typedef struct VkFormatProperties {
--   >     VkFormatFeatureFlags   linearTilingFeatures;
--   >     VkFormatFeatureFlags   optimalTilingFeatures;
--   >     VkFormatFeatureFlags   bufferFeatures;
--   > } VkFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkFormatProperties VkFormatProperties registry at www.khronos.org>
type VkFormatProperties = VkStruct VkFormatProperties' -- ' closing tick for hsc2hs

data VkFormatProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkFormatProperties where
    type StructRep VkFormatProperties =
         'StructMeta "VkFormatProperties" VkFormatProperties  -- ' closing tick for hsc2hs
                                                             #{size VkFormatProperties}
           #{alignment VkFormatProperties}
           '[('FieldMeta "linearTilingFeatures" VkFormatFeatureFlags 'True -- ' closing tick for hsc2hs
                #{offset VkFormatProperties, linearTilingFeatures}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "optimalTilingFeatures" VkFormatFeatureFlags 'True
                #{offset VkFormatProperties, optimalTilingFeatures}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bufferFeatures" VkFormatFeatureFlags 'True 
                                                                     #{offset VkFormatProperties, bufferFeatures}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkFormatProperties2 {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkFormatProperties               formatProperties;
--   > } VkFormatProperties2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkFormatProperties2 VkFormatProperties2 registry at www.khronos.org>
type VkFormatProperties2 = VkStruct VkFormatProperties2' -- ' closing tick for hsc2hs

data VkFormatProperties2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkFormatProperties2 where
    type StructRep VkFormatProperties2 =
         'StructMeta "VkFormatProperties2" VkFormatProperties2  -- ' closing tick for hsc2hs
                                                               #{size VkFormatProperties2}
           #{alignment VkFormatProperties2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkFormatProperties2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkFormatProperties2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "formatProperties" VkFormatProperties 'False 
                                                                      #{offset VkFormatProperties2, formatProperties}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkFormatProperties2`
type VkFormatProperties2KHR = VkFormatProperties2
