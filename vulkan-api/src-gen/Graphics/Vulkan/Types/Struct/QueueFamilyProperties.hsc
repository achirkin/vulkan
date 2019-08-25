#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.QueueFamilyProperties
       (VkQueueFamilyProperties, VkQueueFamilyProperties2,
        VkQueueFamilyProperties2KHR)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Queue         (VkQueueFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.Extent      (VkExtent3D)

-- | > typedef struct VkQueueFamilyProperties {
--   >     VkQueueFlags           queueFlags;
--   >     uint32_t               queueCount;
--   >     uint32_t               timestampValidBits;
--   >     VkExtent3D             minImageTransferGranularity;
--   > } VkQueueFamilyProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkQueueFamilyProperties VkQueueFamilyProperties registry at www.khronos.org>
type VkQueueFamilyProperties = VkStruct VkQueueFamilyProperties' -- ' closing tick for hsc2hs

data VkQueueFamilyProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkQueueFamilyProperties where
    type StructRep VkQueueFamilyProperties =
         'StructMeta "VkQueueFamilyProperties" VkQueueFamilyProperties -- ' closing tick for hsc2hs
           #{size VkQueueFamilyProperties}
           #{alignment VkQueueFamilyProperties}
           '[('FieldMeta "queueFlags" VkQueueFlags 'True  -- ' closing tick for hsc2hs
                                                         #{offset VkQueueFamilyProperties, queueFlags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queueCount" Word32 'False 
                                                    #{offset VkQueueFamilyProperties, queueCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "timestampValidBits" Word32 'False 
                                                            #{offset VkQueueFamilyProperties, timestampValidBits}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minImageTransferGranularity" VkExtent3D 'False
                #{offset VkQueueFamilyProperties, minImageTransferGranularity}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkQueueFamilyProperties2 {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkQueueFamilyProperties          queueFamilyProperties;
--   > } VkQueueFamilyProperties2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkQueueFamilyProperties2 VkQueueFamilyProperties2 registry at www.khronos.org>
type VkQueueFamilyProperties2 = VkStruct VkQueueFamilyProperties2' -- ' closing tick for hsc2hs

data VkQueueFamilyProperties2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkQueueFamilyProperties2 where
    type StructRep VkQueueFamilyProperties2 =
         'StructMeta "VkQueueFamilyProperties2" VkQueueFamilyProperties2 -- ' closing tick for hsc2hs
           #{size VkQueueFamilyProperties2}
           #{alignment VkQueueFamilyProperties2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkQueueFamilyProperties2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkQueueFamilyProperties2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queueFamilyProperties" VkQueueFamilyProperties 'False
                #{offset VkQueueFamilyProperties2, queueFamilyProperties}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkQueueFamilyProperties2`
type VkQueueFamilyProperties2KHR = VkQueueFamilyProperties2
