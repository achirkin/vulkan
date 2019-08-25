#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformAndroidKhr
       (VkAndroidHardwareBufferFormatPropertiesANDROID,
        VkAndroidHardwareBufferPropertiesANDROID,
        VkAndroidHardwareBufferUsageANDROID, VkAndroidSurfaceCreateInfoKHR,
        VkExternalFormatANDROID, VkImportAndroidHardwareBufferInfoANDROID,
        VkMemoryGetAndroidHardwareBufferInfoANDROID)
       where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes               (VkDeviceSize)
import           Graphics.Vulkan.Types.Bitmasks                (VkAndroidSurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Defines                 (AHardwareBuffer,
                                                                ANativeWindow)
import           Graphics.Vulkan.Types.Enum.ChromaLocation     (VkChromaLocation)
import           Graphics.Vulkan.Types.Enum.Format             (VkFormat, VkFormatFeatureFlags)
import           Graphics.Vulkan.Types.Enum.Sampler            (VkSamplerYcbcrModelConversion,
                                                                VkSamplerYcbcrRange)
import           Graphics.Vulkan.Types.Enum.StructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Handles                 (VkDeviceMemory)
import           Graphics.Vulkan.Types.Struct.ComponentMapping (VkComponentMapping)
import           Graphics.Vulkan.Types.Struct.Image            (VkImageCreateInfo,
                                                                VkImageFormatProperties2)
import           Graphics.Vulkan.Types.Struct.Memory           (VkMemoryAllocateInfo)
import           Graphics.Vulkan.Types.Struct.Sampler          (VkSamplerYcbcrConversionCreateInfo)

-- | > typedef struct VkAndroidHardwareBufferFormatPropertiesANDROID {
--   >     VkStructureType sType;
--   >     void*                              pNext;
--   >     VkFormat                           format;
--   >     uint64_t                           externalFormat;
--   >     VkFormatFeatureFlags               formatFeatures;
--   >     VkComponentMapping                 samplerYcbcrConversionComponents;
--   >     VkSamplerYcbcrModelConversion      suggestedYcbcrModel;
--   >     VkSamplerYcbcrRange                suggestedYcbcrRange;
--   >     VkChromaLocation                   suggestedXChromaOffset;
--   >     VkChromaLocation                   suggestedYChromaOffset;
--   > } VkAndroidHardwareBufferFormatPropertiesANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAndroidHardwareBufferFormatPropertiesANDROID VkAndroidHardwareBufferFormatPropertiesANDROID registry at www.khronos.org>
type VkAndroidHardwareBufferFormatPropertiesANDROID =
     VkStruct VkAndroidHardwareBufferFormatPropertiesANDROID' -- ' closing tick for hsc2hs

data VkAndroidHardwareBufferFormatPropertiesANDROID' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkAndroidHardwareBufferFormatPropertiesANDROID
         where
    type StructRep VkAndroidHardwareBufferFormatPropertiesANDROID =
         'StructMeta "VkAndroidHardwareBufferFormatPropertiesANDROID" -- ' closing tick for hsc2hs
           VkAndroidHardwareBufferFormatPropertiesANDROID
           #{size VkAndroidHardwareBufferFormatPropertiesANDROID}
           #{alignment VkAndroidHardwareBufferFormatPropertiesANDROID}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "format" VkFormat 'False 
                                                  #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, format}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "externalFormat" Word64 'False 
                                                        #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, externalFormat}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "formatFeatures" VkFormatFeatureFlags 'False 
                                                                      #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, formatFeatures}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "samplerYcbcrConversionComponents" VkComponentMapping -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, samplerYcbcrConversionComponents}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "suggestedYcbcrModel" VkSamplerYcbcrModelConversion -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYcbcrModel}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "suggestedYcbcrRange" VkSamplerYcbcrRange 'False
                #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYcbcrRange}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "suggestedXChromaOffset" VkChromaLocation 'False
                #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedXChromaOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "suggestedYChromaOffset" VkChromaLocation 'False
                #{offset VkAndroidHardwareBufferFormatPropertiesANDROID, suggestedYChromaOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkAndroidHardwareBufferPropertiesANDROID] -- ' closing tick for hsc2hs

-- | > typedef struct VkAndroidHardwareBufferPropertiesANDROID {
--   >     VkStructureType sType;
--   >     void*                              pNext;
--   >     VkDeviceSize                       allocationSize;
--   >     uint32_t                           memoryTypeBits;
--   > } VkAndroidHardwareBufferPropertiesANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAndroidHardwareBufferPropertiesANDROID VkAndroidHardwareBufferPropertiesANDROID registry at www.khronos.org>
type VkAndroidHardwareBufferPropertiesANDROID =
     VkStruct VkAndroidHardwareBufferPropertiesANDROID' -- ' closing tick for hsc2hs

data VkAndroidHardwareBufferPropertiesANDROID' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAndroidHardwareBufferPropertiesANDROID
         where
    type StructRep VkAndroidHardwareBufferPropertiesANDROID =
         'StructMeta "VkAndroidHardwareBufferPropertiesANDROID" -- ' closing tick for hsc2hs
           VkAndroidHardwareBufferPropertiesANDROID
           #{size VkAndroidHardwareBufferPropertiesANDROID}
           #{alignment VkAndroidHardwareBufferPropertiesANDROID}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAndroidHardwareBufferPropertiesANDROID, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkAndroidHardwareBufferPropertiesANDROID, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "allocationSize" VkDeviceSize 'False 
                                                              #{offset VkAndroidHardwareBufferPropertiesANDROID, allocationSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryTypeBits" Word32 'False 
                                                        #{offset VkAndroidHardwareBufferPropertiesANDROID, memoryTypeBits}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkAndroidHardwareBufferUsageANDROID {
--   >     VkStructureType sType;
--   >     void*                              pNext;
--   >     uint64_t                           androidHardwareBufferUsage;
--   > } VkAndroidHardwareBufferUsageANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAndroidHardwareBufferUsageANDROID VkAndroidHardwareBufferUsageANDROID registry at www.khronos.org>
type VkAndroidHardwareBufferUsageANDROID =
     VkStruct VkAndroidHardwareBufferUsageANDROID' -- ' closing tick for hsc2hs

data VkAndroidHardwareBufferUsageANDROID' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAndroidHardwareBufferUsageANDROID where
    type StructRep VkAndroidHardwareBufferUsageANDROID =
         'StructMeta "VkAndroidHardwareBufferUsageANDROID" -- ' closing tick for hsc2hs
           VkAndroidHardwareBufferUsageANDROID
           #{size VkAndroidHardwareBufferUsageANDROID}
           #{alignment VkAndroidHardwareBufferUsageANDROID}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAndroidHardwareBufferUsageANDROID, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkAndroidHardwareBufferUsageANDROID, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "androidHardwareBufferUsage" Word64 'False 
                                                                    #{offset VkAndroidHardwareBufferUsageANDROID, androidHardwareBufferUsage}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkImageFormatProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkAndroidSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                    pNext;
--   >     VkAndroidSurfaceCreateFlagsKHR flags;
--   >     struct ANativeWindow*    window;
--   > } VkAndroidSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAndroidSurfaceCreateInfoKHR VkAndroidSurfaceCreateInfoKHR registry at www.khronos.org>
type VkAndroidSurfaceCreateInfoKHR =
     VkStruct VkAndroidSurfaceCreateInfoKHR' -- ' closing tick for hsc2hs

data VkAndroidSurfaceCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAndroidSurfaceCreateInfoKHR where
    type StructRep VkAndroidSurfaceCreateInfoKHR =
         'StructMeta "VkAndroidSurfaceCreateInfoKHR" -- ' closing tick for hsc2hs
           VkAndroidSurfaceCreateInfoKHR
           #{size VkAndroidSurfaceCreateInfoKHR}
           #{alignment VkAndroidSurfaceCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAndroidSurfaceCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkAndroidSurfaceCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkAndroidSurfaceCreateFlagsKHR 'True 
                                                                      #{offset VkAndroidSurfaceCreateInfoKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "window" (Ptr ANativeWindow) 'False 
                                                             #{offset VkAndroidSurfaceCreateInfoKHR, window}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkExternalFormatANDROID {
--   >     VkStructureType sType;
--   >     void*                              pNext;
--   >     uint64_t                           externalFormat;
--   > } VkExternalFormatANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalFormatANDROID VkExternalFormatANDROID registry at www.khronos.org>
type VkExternalFormatANDROID = VkStruct VkExternalFormatANDROID' -- ' closing tick for hsc2hs

data VkExternalFormatANDROID' -- ' closing tick for hsc2hs

instance VulkanMarshal VkExternalFormatANDROID where
    type StructRep VkExternalFormatANDROID =
         'StructMeta "VkExternalFormatANDROID" VkExternalFormatANDROID -- ' closing tick for hsc2hs
           #{size VkExternalFormatANDROID}
           #{alignment VkExternalFormatANDROID}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkExternalFormatANDROID, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkExternalFormatANDROID, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "externalFormat" Word64 'False 
                                                        #{offset VkExternalFormatANDROID, externalFormat}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkImageCreateInfo, VkSamplerYcbcrConversionCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkImportAndroidHardwareBufferInfoANDROID {
--   >     VkStructureType sType;
--   >     const void*                        pNext;
--   >     struct AHardwareBuffer*            buffer;
--   > } VkImportAndroidHardwareBufferInfoANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportAndroidHardwareBufferInfoANDROID VkImportAndroidHardwareBufferInfoANDROID registry at www.khronos.org>
type VkImportAndroidHardwareBufferInfoANDROID =
     VkStruct VkImportAndroidHardwareBufferInfoANDROID' -- ' closing tick for hsc2hs

data VkImportAndroidHardwareBufferInfoANDROID' -- ' closing tick for hsc2hs

instance VulkanMarshal VkImportAndroidHardwareBufferInfoANDROID
         where
    type StructRep VkImportAndroidHardwareBufferInfoANDROID =
         'StructMeta "VkImportAndroidHardwareBufferInfoANDROID" -- ' closing tick for hsc2hs
           VkImportAndroidHardwareBufferInfoANDROID
           #{size VkImportAndroidHardwareBufferInfoANDROID}
           #{alignment VkImportAndroidHardwareBufferInfoANDROID}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkImportAndroidHardwareBufferInfoANDROID, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkImportAndroidHardwareBufferInfoANDROID, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "buffer" (Ptr AHardwareBuffer) 'False 
                                                               #{offset VkImportAndroidHardwareBufferInfoANDROID, buffer}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkMemoryGetAndroidHardwareBufferInfoANDROID {
--   >     VkStructureType sType;
--   >     const void*                        pNext;
--   >     VkDeviceMemory                     memory;
--   > } VkMemoryGetAndroidHardwareBufferInfoANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryGetAndroidHardwareBufferInfoANDROID VkMemoryGetAndroidHardwareBufferInfoANDROID registry at www.khronos.org>
type VkMemoryGetAndroidHardwareBufferInfoANDROID =
     VkStruct VkMemoryGetAndroidHardwareBufferInfoANDROID' -- ' closing tick for hsc2hs

data VkMemoryGetAndroidHardwareBufferInfoANDROID' -- ' closing tick for hsc2hs

instance VulkanMarshal VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
    type StructRep VkMemoryGetAndroidHardwareBufferInfoANDROID =
         'StructMeta "VkMemoryGetAndroidHardwareBufferInfoANDROID" -- ' closing tick for hsc2hs
           VkMemoryGetAndroidHardwareBufferInfoANDROID
           #{size VkMemoryGetAndroidHardwareBufferInfoANDROID}
           #{alignment VkMemoryGetAndroidHardwareBufferInfoANDROID}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memory" VkDeviceMemory 'False 
                                                        #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, memory}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
