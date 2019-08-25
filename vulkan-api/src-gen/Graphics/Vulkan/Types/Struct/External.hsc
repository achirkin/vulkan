#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.External
       (VkExternalBufferProperties, VkExternalBufferPropertiesKHR,
        VkExternalFenceProperties, VkExternalFencePropertiesKHR,
        VkExternalImageFormatProperties,
        VkExternalImageFormatPropertiesKHR,
        VkExternalImageFormatPropertiesNV,
        VkExternalMemoryBufferCreateInfo,
        VkExternalMemoryBufferCreateInfoKHR,
        VkExternalMemoryImageCreateInfo,
        VkExternalMemoryImageCreateInfoKHR,
        VkExternalMemoryImageCreateInfoNV, VkExternalMemoryProperties,
        VkExternalMemoryPropertiesKHR, VkExternalSemaphoreProperties,
        VkExternalSemaphorePropertiesKHR)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.External      (VkExternalFenceFeatureFlags,
                                                           VkExternalFenceHandleTypeFlags,
                                                           VkExternalMemoryFeatureFlags,
                                                           VkExternalMemoryFeatureFlagsNV,
                                                           VkExternalMemoryHandleTypeFlags,
                                                           VkExternalMemoryHandleTypeFlagsNV,
                                                           VkExternalSemaphoreFeatureFlags,
                                                           VkExternalSemaphoreHandleTypeFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.Buffer      (VkBufferCreateInfo)
import           Graphics.Vulkan.Types.Struct.Image       (VkImageCreateInfo, VkImageFormatProperties,
                                                           VkImageFormatProperties2)

-- | > typedef struct VkExternalBufferProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalMemoryProperties    externalMemoryProperties;
--   > } VkExternalBufferProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalBufferProperties VkExternalBufferProperties registry at www.khronos.org>
type VkExternalBufferProperties =
     VkStruct VkExternalBufferProperties' -- ' closing tick for hsc2hs

data VkExternalBufferProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExternalBufferProperties where
    type StructRep VkExternalBufferProperties =
         'StructMeta "VkExternalBufferProperties" VkExternalBufferProperties -- ' closing tick for hsc2hs
           #{size VkExternalBufferProperties}
           #{alignment VkExternalBufferProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkExternalBufferProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkExternalBufferProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "externalMemoryProperties" VkExternalMemoryProperties -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkExternalBufferProperties, externalMemoryProperties}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkExternalBufferProperties`
type VkExternalBufferPropertiesKHR = VkExternalBufferProperties

-- | > typedef struct VkExternalFenceProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalFenceHandleTypeFlags exportFromImportedHandleTypes;
--   >     VkExternalFenceHandleTypeFlags compatibleHandleTypes;
--   >     VkExternalFenceFeatureFlags externalFenceFeatures;
--   > } VkExternalFenceProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalFenceProperties VkExternalFenceProperties registry at www.khronos.org>
type VkExternalFenceProperties =
     VkStruct VkExternalFenceProperties' -- ' closing tick for hsc2hs

data VkExternalFenceProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExternalFenceProperties where
    type StructRep VkExternalFenceProperties =
         'StructMeta "VkExternalFenceProperties" VkExternalFenceProperties -- ' closing tick for hsc2hs
           #{size VkExternalFenceProperties}
           #{alignment VkExternalFenceProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkExternalFenceProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkExternalFenceProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "exportFromImportedHandleTypes" -- ' closing tick for hsc2hs
                VkExternalFenceHandleTypeFlags
                'False -- ' closing tick for hsc2hs
                #{offset VkExternalFenceProperties, exportFromImportedHandleTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "compatibleHandleTypes" VkExternalFenceHandleTypeFlags -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkExternalFenceProperties, compatibleHandleTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "externalFenceFeatures" VkExternalFenceFeatureFlags -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkExternalFenceProperties, externalFenceFeatures}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkExternalFenceProperties`
type VkExternalFencePropertiesKHR = VkExternalFenceProperties

-- | > typedef struct VkExternalImageFormatProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalMemoryProperties externalMemoryProperties;
--   > } VkExternalImageFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalImageFormatProperties VkExternalImageFormatProperties registry at www.khronos.org>
type VkExternalImageFormatProperties =
     VkStruct VkExternalImageFormatProperties' -- ' closing tick for hsc2hs

data VkExternalImageFormatProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExternalImageFormatProperties where
    type StructRep VkExternalImageFormatProperties =
         'StructMeta "VkExternalImageFormatProperties" -- ' closing tick for hsc2hs
           VkExternalImageFormatProperties
           #{size VkExternalImageFormatProperties}
           #{alignment VkExternalImageFormatProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkExternalImageFormatProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkExternalImageFormatProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "externalMemoryProperties" VkExternalMemoryProperties -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkExternalImageFormatProperties, externalMemoryProperties}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkImageFormatProperties2] -- ' closing tick for hsc2hs

-- | Alias for `VkExternalImageFormatProperties`
type VkExternalImageFormatPropertiesKHR =
     VkExternalImageFormatProperties

-- | > typedef struct VkExternalImageFormatPropertiesNV {
--   >     VkImageFormatProperties          imageFormatProperties;
--   >     VkExternalMemoryFeatureFlagsNV   externalMemoryFeatures;
--   >     VkExternalMemoryHandleTypeFlagsNV exportFromImportedHandleTypes;
--   >     VkExternalMemoryHandleTypeFlagsNV compatibleHandleTypes;
--   > } VkExternalImageFormatPropertiesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalImageFormatPropertiesNV VkExternalImageFormatPropertiesNV registry at www.khronos.org>
type VkExternalImageFormatPropertiesNV =
     VkStruct VkExternalImageFormatPropertiesNV' -- ' closing tick for hsc2hs

data VkExternalImageFormatPropertiesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExternalImageFormatPropertiesNV where
    type StructRep VkExternalImageFormatPropertiesNV =
         'StructMeta "VkExternalImageFormatPropertiesNV" -- ' closing tick for hsc2hs
           VkExternalImageFormatPropertiesNV
           #{size VkExternalImageFormatPropertiesNV}
           #{alignment VkExternalImageFormatPropertiesNV}
           '[('FieldMeta "imageFormatProperties" VkImageFormatProperties
                'False -- ' closing tick for hsc2hs
                #{offset VkExternalImageFormatPropertiesNV, imageFormatProperties}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "externalMemoryFeatures" VkExternalMemoryFeatureFlagsNV -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkExternalImageFormatPropertiesNV, externalMemoryFeatures}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "exportFromImportedHandleTypes" -- ' closing tick for hsc2hs
                VkExternalMemoryHandleTypeFlagsNV
                'True -- ' closing tick for hsc2hs
                #{offset VkExternalImageFormatPropertiesNV, exportFromImportedHandleTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "compatibleHandleTypes" -- ' closing tick for hsc2hs
                VkExternalMemoryHandleTypeFlagsNV
                'True -- ' closing tick for hsc2hs
                #{offset VkExternalImageFormatPropertiesNV, compatibleHandleTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkExternalMemoryBufferCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlags handleTypes;
--   > } VkExternalMemoryBufferCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalMemoryBufferCreateInfo VkExternalMemoryBufferCreateInfo registry at www.khronos.org>
type VkExternalMemoryBufferCreateInfo =
     VkStruct VkExternalMemoryBufferCreateInfo' -- ' closing tick for hsc2hs

data VkExternalMemoryBufferCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExternalMemoryBufferCreateInfo where
    type StructRep VkExternalMemoryBufferCreateInfo =
         'StructMeta "VkExternalMemoryBufferCreateInfo" -- ' closing tick for hsc2hs
           VkExternalMemoryBufferCreateInfo
           #{size VkExternalMemoryBufferCreateInfo}
           #{alignment VkExternalMemoryBufferCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkExternalMemoryBufferCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkExternalMemoryBufferCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleTypes" VkExternalMemoryHandleTypeFlags 'True
                #{offset VkExternalMemoryBufferCreateInfo, handleTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkBufferCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkExternalMemoryBufferCreateInfo`
type VkExternalMemoryBufferCreateInfoKHR =
     VkExternalMemoryBufferCreateInfo

-- | > typedef struct VkExternalMemoryImageCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlags handleTypes;
--   > } VkExternalMemoryImageCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalMemoryImageCreateInfo VkExternalMemoryImageCreateInfo registry at www.khronos.org>
type VkExternalMemoryImageCreateInfo =
     VkStruct VkExternalMemoryImageCreateInfo' -- ' closing tick for hsc2hs

data VkExternalMemoryImageCreateInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExternalMemoryImageCreateInfo where
    type StructRep VkExternalMemoryImageCreateInfo =
         'StructMeta "VkExternalMemoryImageCreateInfo" -- ' closing tick for hsc2hs
           VkExternalMemoryImageCreateInfo
           #{size VkExternalMemoryImageCreateInfo}
           #{alignment VkExternalMemoryImageCreateInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkExternalMemoryImageCreateInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkExternalMemoryImageCreateInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleTypes" VkExternalMemoryHandleTypeFlags 'False
                #{offset VkExternalMemoryImageCreateInfo, handleTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkImageCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkExternalMemoryImageCreateInfo`
type VkExternalMemoryImageCreateInfoKHR =
     VkExternalMemoryImageCreateInfo

-- | > typedef struct VkExternalMemoryImageCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsNV handleTypes;
--   > } VkExternalMemoryImageCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalMemoryImageCreateInfoNV VkExternalMemoryImageCreateInfoNV registry at www.khronos.org>
type VkExternalMemoryImageCreateInfoNV =
     VkStruct VkExternalMemoryImageCreateInfoNV' -- ' closing tick for hsc2hs

data VkExternalMemoryImageCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExternalMemoryImageCreateInfoNV where
    type StructRep VkExternalMemoryImageCreateInfoNV =
         'StructMeta "VkExternalMemoryImageCreateInfoNV" -- ' closing tick for hsc2hs
           VkExternalMemoryImageCreateInfoNV
           #{size VkExternalMemoryImageCreateInfoNV}
           #{alignment VkExternalMemoryImageCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkExternalMemoryImageCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkExternalMemoryImageCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleTypes" VkExternalMemoryHandleTypeFlagsNV 'True
                #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkImageCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkExternalMemoryProperties {
--   >     VkExternalMemoryFeatureFlags  externalMemoryFeatures;
--   >     VkExternalMemoryHandleTypeFlags exportFromImportedHandleTypes;
--   >     VkExternalMemoryHandleTypeFlags compatibleHandleTypes;
--   > } VkExternalMemoryProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalMemoryProperties VkExternalMemoryProperties registry at www.khronos.org>
type VkExternalMemoryProperties =
     VkStruct VkExternalMemoryProperties' -- ' closing tick for hsc2hs

data VkExternalMemoryProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExternalMemoryProperties where
    type StructRep VkExternalMemoryProperties =
         'StructMeta "VkExternalMemoryProperties" VkExternalMemoryProperties -- ' closing tick for hsc2hs
           #{size VkExternalMemoryProperties}
           #{alignment VkExternalMemoryProperties}
           '[('FieldMeta "externalMemoryFeatures" VkExternalMemoryFeatureFlags
                'False -- ' closing tick for hsc2hs
                #{offset VkExternalMemoryProperties, externalMemoryFeatures}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "exportFromImportedHandleTypes" -- ' closing tick for hsc2hs
                VkExternalMemoryHandleTypeFlags
                'True -- ' closing tick for hsc2hs
                #{offset VkExternalMemoryProperties, exportFromImportedHandleTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "compatibleHandleTypes" VkExternalMemoryHandleTypeFlags -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkExternalMemoryProperties, compatibleHandleTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkExternalMemoryProperties`
type VkExternalMemoryPropertiesKHR = VkExternalMemoryProperties

-- | > typedef struct VkExternalSemaphoreProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalSemaphoreHandleTypeFlags exportFromImportedHandleTypes;
--   >     VkExternalSemaphoreHandleTypeFlags compatibleHandleTypes;
--   >     VkExternalSemaphoreFeatureFlags externalSemaphoreFeatures;
--   > } VkExternalSemaphoreProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalSemaphoreProperties VkExternalSemaphoreProperties registry at www.khronos.org>
type VkExternalSemaphoreProperties =
     VkStruct VkExternalSemaphoreProperties' -- ' closing tick for hsc2hs

data VkExternalSemaphoreProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExternalSemaphoreProperties where
    type StructRep VkExternalSemaphoreProperties =
         'StructMeta "VkExternalSemaphoreProperties" -- ' closing tick for hsc2hs
           VkExternalSemaphoreProperties
           #{size VkExternalSemaphoreProperties}
           #{alignment VkExternalSemaphoreProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkExternalSemaphoreProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkExternalSemaphoreProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "exportFromImportedHandleTypes" -- ' closing tick for hsc2hs
                VkExternalSemaphoreHandleTypeFlags
                'False -- ' closing tick for hsc2hs
                #{offset VkExternalSemaphoreProperties, exportFromImportedHandleTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "compatibleHandleTypes" -- ' closing tick for hsc2hs
                VkExternalSemaphoreHandleTypeFlags
                'False -- ' closing tick for hsc2hs
                #{offset VkExternalSemaphoreProperties, compatibleHandleTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "externalSemaphoreFeatures" -- ' closing tick for hsc2hs
                VkExternalSemaphoreFeatureFlags
                'True -- ' closing tick for hsc2hs
                #{offset VkExternalSemaphoreProperties, externalSemaphoreFeatures}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkExternalSemaphoreProperties`
type VkExternalSemaphorePropertiesKHR =
     VkExternalSemaphoreProperties
