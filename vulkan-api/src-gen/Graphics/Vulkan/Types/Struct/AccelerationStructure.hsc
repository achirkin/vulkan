#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.AccelerationStructure
       (VkAccelerationStructureCreateInfoNV,
        VkAccelerationStructureInfoNV, VkAccelerationStructureInstanceNV,
        VkAccelerationStructureMemoryRequirementsInfoNV)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.BaseTypes                   (VkDeviceSize)
import Graphics.Vulkan.Types.Bitmasks                    (VkBuildAccelerationStructureFlagsNV)
import Graphics.Vulkan.Types.Enum.AccelerationStructure  (VkAccelerationStructureMemoryRequirementsTypeNV,
                                                          VkAccelerationStructureTypeNV)
import Graphics.Vulkan.Types.Enum.StructureType          (VkStructureType)
import Graphics.Vulkan.Types.Handles                     (VkAccelerationStructureNV)
import Graphics.Vulkan.Types.Struct.EnableBetaExtensions (VkAccelerationStructureInstanceKHR)
import Graphics.Vulkan.Types.Struct.Geometry             (VkGeometryNV)

-- | > typedef struct VkAccelerationStructureCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkDeviceSize                           compactedSize;
--   >     VkAccelerationStructureInfoNV          info;
--   > } VkAccelerationStructureCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureCreateInfoNV VkAccelerationStructureCreateInfoNV registry at www.khronos.org>
type VkAccelerationStructureCreateInfoNV =
     VkStruct VkAccelerationStructureCreateInfoNV' -- ' closing tick for hsc2hs

data VkAccelerationStructureCreateInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAccelerationStructureCreateInfoNV where
    type StructRep VkAccelerationStructureCreateInfoNV =
         'StructMeta "VkAccelerationStructureCreateInfoNV" -- ' closing tick for hsc2hs
           VkAccelerationStructureCreateInfoNV
           #{size VkAccelerationStructureCreateInfoNV}
           #{alignment VkAccelerationStructureCreateInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAccelerationStructureCreateInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkAccelerationStructureCreateInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "compactedSize" VkDeviceSize 'False 
                                                             #{offset VkAccelerationStructureCreateInfoNV, compactedSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "info" VkAccelerationStructureInfoNV 'False 
                                                                     #{offset VkAccelerationStructureCreateInfoNV, info}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkAccelerationStructureInfoNV {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkAccelerationStructureTypeNV         type;
--   >     VkBuildAccelerationStructureFlagsNVflags;
--   >     uint32_t               instanceCount;
--   >     uint32_t               geometryCount;
--   >     const VkGeometryNV* pGeometries;
--   > } VkAccelerationStructureInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureInfoNV VkAccelerationStructureInfoNV registry at www.khronos.org>
type VkAccelerationStructureInfoNV =
     VkStruct VkAccelerationStructureInfoNV' -- ' closing tick for hsc2hs

data VkAccelerationStructureInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAccelerationStructureInfoNV where
    type StructRep VkAccelerationStructureInfoNV =
         'StructMeta "VkAccelerationStructureInfoNV" -- ' closing tick for hsc2hs
           VkAccelerationStructureInfoNV
           #{size VkAccelerationStructureInfoNV}
           #{alignment VkAccelerationStructureInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAccelerationStructureInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkAccelerationStructureInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "type" VkAccelerationStructureTypeNV 'False 
                                                                     #{offset VkAccelerationStructureInfoNV, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkBuildAccelerationStructureFlagsNV 'True
                #{offset VkAccelerationStructureInfoNV, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "instanceCount" Word32 'True 
                                                      #{offset VkAccelerationStructureInfoNV, instanceCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "geometryCount" Word32 'True 
                                                      #{offset VkAccelerationStructureInfoNV, geometryCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pGeometries" (Ptr VkGeometryNV) 'False 
                                                                 #{offset VkAccelerationStructureInfoNV, pGeometries}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkAccelerationStructureInstanceKHR`
type VkAccelerationStructureInstanceNV =
     VkAccelerationStructureInstanceKHR

-- | > typedef struct VkAccelerationStructureMemoryRequirementsInfoNV {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkAccelerationStructureMemoryRequirementsTypeNV                     type;
--   >     VkAccelerationStructureNV                                           accelerationStructure;
--   > } VkAccelerationStructureMemoryRequirementsInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureMemoryRequirementsInfoNV VkAccelerationStructureMemoryRequirementsInfoNV registry at www.khronos.org>
type VkAccelerationStructureMemoryRequirementsInfoNV =
     VkStruct VkAccelerationStructureMemoryRequirementsInfoNV' -- ' closing tick for hsc2hs

data VkAccelerationStructureMemoryRequirementsInfoNV' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkAccelerationStructureMemoryRequirementsInfoNV
         where
    type StructRep VkAccelerationStructureMemoryRequirementsInfoNV =
         'StructMeta "VkAccelerationStructureMemoryRequirementsInfoNV" -- ' closing tick for hsc2hs
           VkAccelerationStructureMemoryRequirementsInfoNV
           #{size VkAccelerationStructureMemoryRequirementsInfoNV}
           #{alignment VkAccelerationStructureMemoryRequirementsInfoNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAccelerationStructureMemoryRequirementsInfoNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkAccelerationStructureMemoryRequirementsInfoNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "type" VkAccelerationStructureMemoryRequirementsTypeNV -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkAccelerationStructureMemoryRequirementsInfoNV, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "accelerationStructure" VkAccelerationStructureNV -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkAccelerationStructureMemoryRequirementsInfoNV, accelerationStructure}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
