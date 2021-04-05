#include "vulkan/vulkan.h"

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.EnableBetaExtensions
       (VkAabbPositionsKHR, VkAccelerationStructureBuildGeometryInfoKHR,
        VkAccelerationStructureBuildOffsetInfoKHR,
        VkAccelerationStructureCreateGeometryTypeInfoKHR,
        VkAccelerationStructureCreateInfoKHR,
        VkAccelerationStructureDeviceAddressInfoKHR,
        VkAccelerationStructureGeometryAabbsDataKHR,
        VkAccelerationStructureGeometryDataKHR,
        VkAccelerationStructureGeometryInstancesDataKHR,
        VkAccelerationStructureGeometryKHR,
        VkAccelerationStructureGeometryTrianglesDataKHR,
        VkAccelerationStructureInstanceKHR,
        VkAccelerationStructureMemoryRequirementsInfoKHR,
        VkAccelerationStructureVersionKHR,
        VkBindAccelerationStructureMemoryInfoKHR,
        VkCopyAccelerationStructureInfoKHR,
        VkCopyAccelerationStructureToMemoryInfoKHR,
        VkCopyMemoryToAccelerationStructureInfoKHR,
        VkDeferredOperationInfoKHR, VkDeviceOrHostAddressConstKHR,
        VkDeviceOrHostAddressKHR, VkPhysicalDeviceRayTracingFeaturesKHR,
        VkPhysicalDeviceRayTracingPropertiesKHR,
        VkPipelineLibraryCreateInfoKHR, VkRayTracingPipelineCreateInfoKHR,
        VkRayTracingPipelineInterfaceCreateInfoKHR,
        VkRayTracingShaderGroupCreateInfoKHR, VkStridedBufferRegionKHR,
        VkTraceRaysIndirectCommandKHR, VkTransformMatrixKHR,
        VkWriteDescriptorSetAccelerationStructureKHR)
       where

import GHC.TypeLits (type (+))

import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.BaseTypes                           (VkBool32, VkDeviceAddress,
                                                                  VkDeviceSize)
import Graphics.Vulkan.Types.Enum.AccelerationStructure          (VkAccelerationStructureBuildTypeKHR,
                                                                  VkAccelerationStructureMemoryRequirementsTypeKHR,
                                                                  VkAccelerationStructureTypeKHR)
import Graphics.Vulkan.Types.Enum.BuildAccelerationStructureFlag (VkBuildAccelerationStructureFlagsKHR)
import Graphics.Vulkan.Types.Enum.CopyAccelerationStructureMode  (VkCopyAccelerationStructureModeKHR)
import Graphics.Vulkan.Types.Enum.Format                         (VkFormat)
import Graphics.Vulkan.Types.Enum.Geometry                       (VkGeometryFlagsKHR,
                                                                  VkGeometryTypeKHR)
import Graphics.Vulkan.Types.Enum.IndexType                      (VkIndexType)
import Graphics.Vulkan.Types.Enum.Pipeline                       (VkPipelineCreateFlags)
import Graphics.Vulkan.Types.Enum.RayTracingShaderGroupType      (VkRayTracingShaderGroupTypeKHR)
import Graphics.Vulkan.Types.Enum.StructureType                  (VkStructureType)
import Graphics.Vulkan.Types.Handles                             (VkAccelerationStructureKHR,
                                                                  VkBuffer,
                                                                  VkDeferredOperationKHR,
                                                                  VkDeviceMemory,
                                                                  VkPipeline,
                                                                  VkPipelineLayout)
import Graphics.Vulkan.Types.Struct.Device                       (VkDeviceCreateInfo)
import Graphics.Vulkan.Types.Struct.PhysicalDevice               (VkPhysicalDeviceFeatures2,
                                                                  VkPhysicalDeviceProperties2)
import
#ifdef VK_ENABLE_BETA_EXTENSIONS
  {-# SOURCE #-}
#endif
    Graphics.Vulkan.Types.Struct.Pipeline                        (VkPipelineShaderStageCreateInfo)
import {-# SOURCE #-} Graphics.Vulkan.Types.Struct.WriteDescriptorSet (VkWriteDescriptorSet)

-- | > typedef struct VkAabbPositionsKHR {
--   >     float                                                   minX;
--   >     float                                                   minY;
--   >     float                                                   minZ;
--   >     float                                                   maxX;
--   >     float                                                   maxY;
--   >     float                                                   maxZ;
--   > } VkAabbPositionsKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAabbPositionsKHR VkAabbPositionsKHR registry at www.khronos.org>
type VkAabbPositionsKHR = VkStruct VkAabbPositionsKHR' -- ' closing tick for hsc2hs

data VkAabbPositionsKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAabbPositionsKHR where
    type StructRep VkAabbPositionsKHR =
         'StructMeta "VkAabbPositionsKHR" VkAabbPositionsKHR  -- ' closing tick for hsc2hs
                                                             #{size VkAabbPositionsKHR}
           #{alignment VkAabbPositionsKHR}
           '[('FieldMeta "minX" (
                                 #{type float}
                                 ) 'False  -- ' closing tick for hsc2hs
                                          #{offset VkAabbPositionsKHR, minX}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minY" ( -- ' closing tick for hsc2hs
                                 #{type float}
                                 ) 'False  -- ' closing tick for hsc2hs
                                          #{offset VkAabbPositionsKHR, minY}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minZ" ( -- ' closing tick for hsc2hs
                                 #{type float}
                                 ) 'False  -- ' closing tick for hsc2hs
                                          #{offset VkAabbPositionsKHR, minZ}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxX" ( -- ' closing tick for hsc2hs
                                 #{type float}
                                 ) 'False  -- ' closing tick for hsc2hs
                                          #{offset VkAabbPositionsKHR, maxX}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxY" ( -- ' closing tick for hsc2hs
                                 #{type float}
                                 ) 'False  -- ' closing tick for hsc2hs
                                          #{offset VkAabbPositionsKHR, maxY}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxZ" ( -- ' closing tick for hsc2hs
                                 #{type float}
                                 ) 'False  -- ' closing tick for hsc2hs
                                          #{offset VkAabbPositionsKHR, maxZ}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkAccelerationStructureBuildGeometryInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                                        pNext;
--   >     VkAccelerationStructureTypeKHR                                     type;
--   >     VkBuildAccelerationStructureFlagsKHR               flags;
--   >     VkBool32                                                           update;
--   >     VkAccelerationStructureKHR                         srcAccelerationStructure;
--   >     VkAccelerationStructureKHR                                         dstAccelerationStructure;
--   >     VkBool32                                                           geometryArrayOfPointers;
--   >     uint32_t                                           geometryCount;
--   >     const VkAccelerationStructureGeometryKHR* const*   ppGeometries;
--   >     VkDeviceOrHostAddressKHR                                           scratchData;
--   > } VkAccelerationStructureBuildGeometryInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureBuildGeometryInfoKHR VkAccelerationStructureBuildGeometryInfoKHR registry at www.khronos.org>
type VkAccelerationStructureBuildGeometryInfoKHR =
     VkStruct VkAccelerationStructureBuildGeometryInfoKHR' -- ' closing tick for hsc2hs

data VkAccelerationStructureBuildGeometryInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAccelerationStructureBuildGeometryInfoKHR
         where
    type StructRep VkAccelerationStructureBuildGeometryInfoKHR =
         'StructMeta "VkAccelerationStructureBuildGeometryInfoKHR" -- ' closing tick for hsc2hs
           VkAccelerationStructureBuildGeometryInfoKHR
           #{size VkAccelerationStructureBuildGeometryInfoKHR}
           #{alignment VkAccelerationStructureBuildGeometryInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAccelerationStructureBuildGeometryInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkAccelerationStructureBuildGeometryInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "type" VkAccelerationStructureTypeKHR 'False
                                                                      #{offset VkAccelerationStructureBuildGeometryInfoKHR, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkBuildAccelerationStructureFlagsKHR 'True
                #{offset VkAccelerationStructureBuildGeometryInfoKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "update" VkBool32 'False
                                                  #{offset VkAccelerationStructureBuildGeometryInfoKHR, update}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "srcAccelerationStructure" VkAccelerationStructureKHR -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkAccelerationStructureBuildGeometryInfoKHR, srcAccelerationStructure}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dstAccelerationStructure" VkAccelerationStructureKHR -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkAccelerationStructureBuildGeometryInfoKHR, dstAccelerationStructure}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "geometryArrayOfPointers" VkBool32 'False
                                                                   #{offset VkAccelerationStructureBuildGeometryInfoKHR, geometryArrayOfPointers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "geometryCount" Word32 'True
                                                      #{offset VkAccelerationStructureBuildGeometryInfoKHR, geometryCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "ppGeometries" -- ' closing tick for hsc2hs
                (Ptr (Ptr VkAccelerationStructureGeometryKHR))
                'False -- ' closing tick for hsc2hs
                #{offset VkAccelerationStructureBuildGeometryInfoKHR, ppGeometries}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "scratchData" VkDeviceOrHostAddressKHR 'False
                                                                       #{offset VkAccelerationStructureBuildGeometryInfoKHR, scratchData}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkAccelerationStructureBuildOffsetInfoKHR {
--   >     uint32_t                                                primitiveCount;
--   >     uint32_t                                                primitiveOffset;
--   >     uint32_t                                firstVertex;
--   >     uint32_t                                transformOffset;
--   > } VkAccelerationStructureBuildOffsetInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureBuildOffsetInfoKHR VkAccelerationStructureBuildOffsetInfoKHR registry at www.khronos.org>
type VkAccelerationStructureBuildOffsetInfoKHR =
     VkStruct VkAccelerationStructureBuildOffsetInfoKHR' -- ' closing tick for hsc2hs

data VkAccelerationStructureBuildOffsetInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAccelerationStructureBuildOffsetInfoKHR
         where
    type StructRep VkAccelerationStructureBuildOffsetInfoKHR =
         'StructMeta "VkAccelerationStructureBuildOffsetInfoKHR" -- ' closing tick for hsc2hs
           VkAccelerationStructureBuildOffsetInfoKHR
           #{size VkAccelerationStructureBuildOffsetInfoKHR}
           #{alignment VkAccelerationStructureBuildOffsetInfoKHR}
           '[('FieldMeta "primitiveCount" Word32 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAccelerationStructureBuildOffsetInfoKHR, primitiveCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "primitiveOffset" Word32 'False
                                                         #{offset VkAccelerationStructureBuildOffsetInfoKHR, primitiveOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "firstVertex" Word32 'True
                                                    #{offset VkAccelerationStructureBuildOffsetInfoKHR, firstVertex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "transformOffset" Word32 'True
                                                        #{offset VkAccelerationStructureBuildOffsetInfoKHR, transformOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkAccelerationStructureCreateGeometryTypeInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                             pNext;
--   >     VkGeometryTypeKHR                                       geometryType;
--   >     uint32_t                                                maxPrimitiveCount;
--   >     VkIndexType                                              indexType;
--   >     uint32_t                                maxVertexCount;
--   >     VkFormat                                vertexFormat;
--   >     VkBool32                                allowsTransforms;
--   > } VkAccelerationStructureCreateGeometryTypeInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureCreateGeometryTypeInfoKHR VkAccelerationStructureCreateGeometryTypeInfoKHR registry at www.khronos.org>
type VkAccelerationStructureCreateGeometryTypeInfoKHR =
     VkStruct VkAccelerationStructureCreateGeometryTypeInfoKHR' -- ' closing tick for hsc2hs

data VkAccelerationStructureCreateGeometryTypeInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkAccelerationStructureCreateGeometryTypeInfoKHR
         where
    type StructRep VkAccelerationStructureCreateGeometryTypeInfoKHR =
         'StructMeta "VkAccelerationStructureCreateGeometryTypeInfoKHR" -- ' closing tick for hsc2hs
           VkAccelerationStructureCreateGeometryTypeInfoKHR
           #{size VkAccelerationStructureCreateGeometryTypeInfoKHR}
           #{alignment VkAccelerationStructureCreateGeometryTypeInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAccelerationStructureCreateGeometryTypeInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkAccelerationStructureCreateGeometryTypeInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "geometryType" VkGeometryTypeKHR 'False
                                                                 #{offset VkAccelerationStructureCreateGeometryTypeInfoKHR, geometryType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPrimitiveCount" Word32 'False
                                                           #{offset VkAccelerationStructureCreateGeometryTypeInfoKHR, maxPrimitiveCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "indexType" VkIndexType 'False
                                                        #{offset VkAccelerationStructureCreateGeometryTypeInfoKHR, indexType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxVertexCount" Word32 'True
                                                       #{offset VkAccelerationStructureCreateGeometryTypeInfoKHR, maxVertexCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexFormat" VkFormat 'True
                                                       #{offset VkAccelerationStructureCreateGeometryTypeInfoKHR, vertexFormat}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "allowsTransforms" VkBool32 'True
                                                           #{offset VkAccelerationStructureCreateGeometryTypeInfoKHR, allowsTransforms}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkAccelerationStructureCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                             pNext;
--   >     VkDeviceSize                                            compactedSize;
--   >     VkAccelerationStructureTypeKHR                          type;
--   >     VkBuildAccelerationStructureFlagsKHR    flags;
--   >     uint32_t                                maxGeometryCount;
--   >     const VkAccelerationStructureCreateGeometryTypeInfoKHR* pGeometryInfos;
--   >     VkDeviceAddress                         deviceAddress;
--   > } VkAccelerationStructureCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureCreateInfoKHR VkAccelerationStructureCreateInfoKHR registry at www.khronos.org>
type VkAccelerationStructureCreateInfoKHR =
     VkStruct VkAccelerationStructureCreateInfoKHR' -- ' closing tick for hsc2hs

data VkAccelerationStructureCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAccelerationStructureCreateInfoKHR where
    type StructRep VkAccelerationStructureCreateInfoKHR =
         'StructMeta "VkAccelerationStructureCreateInfoKHR" -- ' closing tick for hsc2hs
           VkAccelerationStructureCreateInfoKHR
           #{size VkAccelerationStructureCreateInfoKHR}
           #{alignment VkAccelerationStructureCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAccelerationStructureCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkAccelerationStructureCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "compactedSize" VkDeviceSize 'False
                                                             #{offset VkAccelerationStructureCreateInfoKHR, compactedSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "type" VkAccelerationStructureTypeKHR 'False
                                                                      #{offset VkAccelerationStructureCreateInfoKHR, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkBuildAccelerationStructureFlagsKHR 'True
                #{offset VkAccelerationStructureCreateInfoKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxGeometryCount" Word32 'True
                                                         #{offset VkAccelerationStructureCreateInfoKHR, maxGeometryCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pGeometryInfos" -- ' closing tick for hsc2hs
                (Ptr VkAccelerationStructureCreateGeometryTypeInfoKHR)
                'False -- ' closing tick for hsc2hs
                #{offset VkAccelerationStructureCreateInfoKHR, pGeometryInfos}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceAddress" VkDeviceAddress 'True
                                                               #{offset VkAccelerationStructureCreateInfoKHR, deviceAddress}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkAccelerationStructureDeviceAddressInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                            pNext;
--   >     VkAccelerationStructureKHR                             accelerationStructure;
--   > } VkAccelerationStructureDeviceAddressInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureDeviceAddressInfoKHR VkAccelerationStructureDeviceAddressInfoKHR registry at www.khronos.org>
type VkAccelerationStructureDeviceAddressInfoKHR =
     VkStruct VkAccelerationStructureDeviceAddressInfoKHR' -- ' closing tick for hsc2hs

data VkAccelerationStructureDeviceAddressInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAccelerationStructureDeviceAddressInfoKHR
         where
    type StructRep VkAccelerationStructureDeviceAddressInfoKHR =
         'StructMeta "VkAccelerationStructureDeviceAddressInfoKHR" -- ' closing tick for hsc2hs
           VkAccelerationStructureDeviceAddressInfoKHR
           #{size VkAccelerationStructureDeviceAddressInfoKHR}
           #{alignment VkAccelerationStructureDeviceAddressInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAccelerationStructureDeviceAddressInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkAccelerationStructureDeviceAddressInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "accelerationStructure" VkAccelerationStructureKHR -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkAccelerationStructureDeviceAddressInfoKHR, accelerationStructure}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkAccelerationStructureGeometryAabbsDataKHR {
--   >     VkStructureType sType;
--   >     const void*                           pNext;
--   >     VkDeviceOrHostAddressConstKHR         data;
--   >     VkDeviceSize                          stride;
--   > } VkAccelerationStructureGeometryAabbsDataKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureGeometryAabbsDataKHR VkAccelerationStructureGeometryAabbsDataKHR registry at www.khronos.org>
type VkAccelerationStructureGeometryAabbsDataKHR =
     VkStruct VkAccelerationStructureGeometryAabbsDataKHR' -- ' closing tick for hsc2hs

data VkAccelerationStructureGeometryAabbsDataKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAccelerationStructureGeometryAabbsDataKHR
         where
    type StructRep VkAccelerationStructureGeometryAabbsDataKHR =
         'StructMeta "VkAccelerationStructureGeometryAabbsDataKHR" -- ' closing tick for hsc2hs
           VkAccelerationStructureGeometryAabbsDataKHR
           #{size VkAccelerationStructureGeometryAabbsDataKHR}
           #{alignment VkAccelerationStructureGeometryAabbsDataKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAccelerationStructureGeometryAabbsDataKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkAccelerationStructureGeometryAabbsDataKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "data" VkDeviceOrHostAddressConstKHR 'False
                                                                     #{offset VkAccelerationStructureGeometryAabbsDataKHR, data}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stride" VkDeviceSize 'False
                                                      #{offset VkAccelerationStructureGeometryAabbsDataKHR, stride}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef union VkAccelerationStructureGeometryDataKHR {
--   >     VkAccelerationStructureGeometryTrianglesDataKHR triangles;
--   >     VkAccelerationStructureGeometryAabbsDataKHR         aabbs;
--   >     VkAccelerationStructureGeometryInstancesDataKHR instances;
--   > } VkAccelerationStructureGeometryDataKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureGeometryDataKHR VkAccelerationStructureGeometryDataKHR registry at www.khronos.org>
type VkAccelerationStructureGeometryDataKHR =
     VkStruct VkAccelerationStructureGeometryDataKHR' -- ' closing tick for hsc2hs

data VkAccelerationStructureGeometryDataKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAccelerationStructureGeometryDataKHR where
    type StructRep VkAccelerationStructureGeometryDataKHR =
         'StructMeta "VkAccelerationStructureGeometryDataKHR" -- ' closing tick for hsc2hs
           VkAccelerationStructureGeometryDataKHR
           #{size VkAccelerationStructureGeometryDataKHR}
           #{alignment VkAccelerationStructureGeometryDataKHR}
           '[('FieldMeta "triangles"
                VkAccelerationStructureGeometryTrianglesDataKHR
                'False -- ' closing tick for hsc2hs
                #{offset VkAccelerationStructureGeometryDataKHR, triangles}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "aabbs" VkAccelerationStructureGeometryAabbsDataKHR -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkAccelerationStructureGeometryDataKHR, aabbs}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "instances" -- ' closing tick for hsc2hs
                VkAccelerationStructureGeometryInstancesDataKHR
                'False -- ' closing tick for hsc2hs
                #{offset VkAccelerationStructureGeometryDataKHR, instances}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkAccelerationStructureGeometryInstancesDataKHR {
--   >     VkStructureType sType;
--   >     const void*                           pNext;
--   >     VkBool32                              arrayOfPointers;
--   >     VkDeviceOrHostAddressConstKHR         data;
--   > } VkAccelerationStructureGeometryInstancesDataKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureGeometryInstancesDataKHR VkAccelerationStructureGeometryInstancesDataKHR registry at www.khronos.org>
type VkAccelerationStructureGeometryInstancesDataKHR =
     VkStruct VkAccelerationStructureGeometryInstancesDataKHR' -- ' closing tick for hsc2hs

data VkAccelerationStructureGeometryInstancesDataKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkAccelerationStructureGeometryInstancesDataKHR
         where
    type StructRep VkAccelerationStructureGeometryInstancesDataKHR =
         'StructMeta "VkAccelerationStructureGeometryInstancesDataKHR" -- ' closing tick for hsc2hs
           VkAccelerationStructureGeometryInstancesDataKHR
           #{size VkAccelerationStructureGeometryInstancesDataKHR}
           #{alignment VkAccelerationStructureGeometryInstancesDataKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAccelerationStructureGeometryInstancesDataKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkAccelerationStructureGeometryInstancesDataKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "arrayOfPointers" VkBool32 'False
                                                           #{offset VkAccelerationStructureGeometryInstancesDataKHR, arrayOfPointers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "data" VkDeviceOrHostAddressConstKHR 'False
                                                                     #{offset VkAccelerationStructureGeometryInstancesDataKHR, data}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkAccelerationStructureGeometryKHR {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkGeometryTypeKHR                      geometryType;
--   >     VkAccelerationStructureGeometryDataKHR geometry;
--   >     VkGeometryFlagsKHR     flags;
--   > } VkAccelerationStructureGeometryKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureGeometryKHR VkAccelerationStructureGeometryKHR registry at www.khronos.org>
type VkAccelerationStructureGeometryKHR =
     VkStruct VkAccelerationStructureGeometryKHR' -- ' closing tick for hsc2hs

data VkAccelerationStructureGeometryKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAccelerationStructureGeometryKHR where
    type StructRep VkAccelerationStructureGeometryKHR =
         'StructMeta "VkAccelerationStructureGeometryKHR" -- ' closing tick for hsc2hs
           VkAccelerationStructureGeometryKHR
           #{size VkAccelerationStructureGeometryKHR}
           #{alignment VkAccelerationStructureGeometryKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAccelerationStructureGeometryKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkAccelerationStructureGeometryKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "geometryType" VkGeometryTypeKHR 'False
                                                                 #{offset VkAccelerationStructureGeometryKHR, geometryType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "geometry" VkAccelerationStructureGeometryDataKHR -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkAccelerationStructureGeometryKHR, geometry}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkGeometryFlagsKHR 'True
                                                          #{offset VkAccelerationStructureGeometryKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkAccelerationStructureGeometryTrianglesDataKHR {
--   >     VkStructureType sType;
--   >     const void*                                   pNext;
--   >     VkFormat                                      vertexFormat;
--   >     VkDeviceOrHostAddressConstKHR                 vertexData;
--   >     VkDeviceSize                                  vertexStride;
--   >     VkIndexType                                   indexType;
--   >     VkDeviceOrHostAddressConstKHR indexData;
--   >     VkDeviceOrHostAddressConstKHR transformData;
--   > } VkAccelerationStructureGeometryTrianglesDataKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureGeometryTrianglesDataKHR VkAccelerationStructureGeometryTrianglesDataKHR registry at www.khronos.org>
type VkAccelerationStructureGeometryTrianglesDataKHR =
     VkStruct VkAccelerationStructureGeometryTrianglesDataKHR' -- ' closing tick for hsc2hs

data VkAccelerationStructureGeometryTrianglesDataKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkAccelerationStructureGeometryTrianglesDataKHR
         where
    type StructRep VkAccelerationStructureGeometryTrianglesDataKHR =
         'StructMeta "VkAccelerationStructureGeometryTrianglesDataKHR" -- ' closing tick for hsc2hs
           VkAccelerationStructureGeometryTrianglesDataKHR
           #{size VkAccelerationStructureGeometryTrianglesDataKHR}
           #{alignment VkAccelerationStructureGeometryTrianglesDataKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAccelerationStructureGeometryTrianglesDataKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkAccelerationStructureGeometryTrianglesDataKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexFormat" VkFormat 'False
                                                        #{offset VkAccelerationStructureGeometryTrianglesDataKHR, vertexFormat}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexData" VkDeviceOrHostAddressConstKHR 'False
                #{offset VkAccelerationStructureGeometryTrianglesDataKHR, vertexData}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexStride" VkDeviceSize 'False
                                                            #{offset VkAccelerationStructureGeometryTrianglesDataKHR, vertexStride}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "indexType" VkIndexType 'False
                                                        #{offset VkAccelerationStructureGeometryTrianglesDataKHR, indexType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "indexData" VkDeviceOrHostAddressConstKHR 'True
                #{offset VkAccelerationStructureGeometryTrianglesDataKHR, indexData}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "transformData" VkDeviceOrHostAddressConstKHR 'True
                #{offset VkAccelerationStructureGeometryTrianglesDataKHR, transformData}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkAccelerationStructureInstanceKHR {
--   >     VkTransformMatrixKHR                                    transform;
--   >     uint32_t                                                instanceCustomIndex:24;
--   >     uint32_t                                                mask:8;
--   >     uint32_t                                                instanceShaderBindingTableRecordOffset:24;
--   >     VkGeometryInstanceFlagsKHR                              flags:8;
--   >     uint64_t                                                accelerationStructureReference;
--   > } VkAccelerationStructureInstanceKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureInstanceKHR VkAccelerationStructureInstanceKHR registry at www.khronos.org>
type VkAccelerationStructureInstanceKHR =
     VkStruct VkAccelerationStructureInstanceKHR' -- ' closing tick for hsc2hs

data VkAccelerationStructureInstanceKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAccelerationStructureInstanceKHR where
    type StructRep VkAccelerationStructureInstanceKHR =
         'StructMeta "VkAccelerationStructureInstanceKHR" -- ' closing tick for hsc2hs
           VkAccelerationStructureInstanceKHR
           #{size VkAccelerationStructureInstanceKHR}
           #{alignment VkAccelerationStructureInstanceKHR}
           '[('FieldMeta "transform" VkTransformMatrixKHR 'False 0 -- ' closing tick for hsc2hs
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "instanceCustomIndexAndMask" Word32 'False #{size VkTransformMatrixKHR}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "instanceShaderBindingTableRecordOffsetAndFlags" Word32 'False
                (#{size VkTransformMatrixKHR} + 4)
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "accelerationStructureReference" Word64 'False
                (#{size VkTransformMatrixKHR} + 8)
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkAccelerationStructureMemoryRequirementsInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkAccelerationStructureMemoryRequirementsTypeKHR                     type;
--   >     VkAccelerationStructureBuildTypeKHR                                  buildType;
--   >     VkAccelerationStructureKHR                                           accelerationStructure;
--   > } VkAccelerationStructureMemoryRequirementsInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureMemoryRequirementsInfoKHR VkAccelerationStructureMemoryRequirementsInfoKHR registry at www.khronos.org>
type VkAccelerationStructureMemoryRequirementsInfoKHR =
     VkStruct VkAccelerationStructureMemoryRequirementsInfoKHR' -- ' closing tick for hsc2hs

data VkAccelerationStructureMemoryRequirementsInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkAccelerationStructureMemoryRequirementsInfoKHR
         where
    type StructRep VkAccelerationStructureMemoryRequirementsInfoKHR =
         'StructMeta "VkAccelerationStructureMemoryRequirementsInfoKHR" -- ' closing tick for hsc2hs
           VkAccelerationStructureMemoryRequirementsInfoKHR
           #{size VkAccelerationStructureMemoryRequirementsInfoKHR}
           #{alignment VkAccelerationStructureMemoryRequirementsInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAccelerationStructureMemoryRequirementsInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkAccelerationStructureMemoryRequirementsInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "type" VkAccelerationStructureMemoryRequirementsTypeKHR -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkAccelerationStructureMemoryRequirementsInfoKHR, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "buildType" VkAccelerationStructureBuildTypeKHR 'False
                #{offset VkAccelerationStructureMemoryRequirementsInfoKHR, buildType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "accelerationStructure" VkAccelerationStructureKHR -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkAccelerationStructureMemoryRequirementsInfoKHR, accelerationStructure}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkAccelerationStructureVersionKHR {
--   >     VkStructureType sType;
--   >     const void*                                            pNext;
--   >     const uint8_t*                    versionData;
--   > } VkAccelerationStructureVersionKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkAccelerationStructureVersionKHR VkAccelerationStructureVersionKHR registry at www.khronos.org>
type VkAccelerationStructureVersionKHR =
     VkStruct VkAccelerationStructureVersionKHR' -- ' closing tick for hsc2hs

data VkAccelerationStructureVersionKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkAccelerationStructureVersionKHR where
    type StructRep VkAccelerationStructureVersionKHR =
         'StructMeta "VkAccelerationStructureVersionKHR" -- ' closing tick for hsc2hs
           VkAccelerationStructureVersionKHR
           #{size VkAccelerationStructureVersionKHR}
           #{alignment VkAccelerationStructureVersionKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkAccelerationStructureVersionKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkAccelerationStructureVersionKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "versionData" (Ptr Word8) 'False
                                                          #{offset VkAccelerationStructureVersionKHR, versionData}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkBindAccelerationStructureMemoryInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkAccelerationStructureKHR       accelerationStructure;
--   >     VkDeviceMemory                   memory;
--   >     VkDeviceSize                     memoryOffset;
--   >     uint32_t         deviceIndexCount;
--   >     const uint32_t*  pDeviceIndices;
--   > } VkBindAccelerationStructureMemoryInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkBindAccelerationStructureMemoryInfoKHR VkBindAccelerationStructureMemoryInfoKHR registry at www.khronos.org>
type VkBindAccelerationStructureMemoryInfoKHR =
     VkStruct VkBindAccelerationStructureMemoryInfoKHR' -- ' closing tick for hsc2hs

data VkBindAccelerationStructureMemoryInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkBindAccelerationStructureMemoryInfoKHR
         where
    type StructRep VkBindAccelerationStructureMemoryInfoKHR =
         'StructMeta "VkBindAccelerationStructureMemoryInfoKHR" -- ' closing tick for hsc2hs
           VkBindAccelerationStructureMemoryInfoKHR
           #{size VkBindAccelerationStructureMemoryInfoKHR}
           #{alignment VkBindAccelerationStructureMemoryInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkBindAccelerationStructureMemoryInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkBindAccelerationStructureMemoryInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "accelerationStructure" VkAccelerationStructureKHR -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkBindAccelerationStructureMemoryInfoKHR, accelerationStructure}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memory" VkDeviceMemory 'False
                                                        #{offset VkBindAccelerationStructureMemoryInfoKHR, memory}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryOffset" VkDeviceSize 'False
                                                            #{offset VkBindAccelerationStructureMemoryInfoKHR, memoryOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceIndexCount" Word32 'True
                                                         #{offset VkBindAccelerationStructureMemoryInfoKHR, deviceIndexCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pDeviceIndices" (Ptr Word32) 'False
                                                              #{offset VkBindAccelerationStructureMemoryInfoKHR, pDeviceIndices}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkCopyAccelerationStructureInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                            pNext;
--   >     VkAccelerationStructureKHR                             src;
--   >     VkAccelerationStructureKHR                             dst;
--   >     VkCopyAccelerationStructureModeKHR                     mode;
--   > } VkCopyAccelerationStructureInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCopyAccelerationStructureInfoKHR VkCopyAccelerationStructureInfoKHR registry at www.khronos.org>
type VkCopyAccelerationStructureInfoKHR =
     VkStruct VkCopyAccelerationStructureInfoKHR' -- ' closing tick for hsc2hs

data VkCopyAccelerationStructureInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkCopyAccelerationStructureInfoKHR where
    type StructRep VkCopyAccelerationStructureInfoKHR =
         'StructMeta "VkCopyAccelerationStructureInfoKHR" -- ' closing tick for hsc2hs
           VkCopyAccelerationStructureInfoKHR
           #{size VkCopyAccelerationStructureInfoKHR}
           #{alignment VkCopyAccelerationStructureInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkCopyAccelerationStructureInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkCopyAccelerationStructureInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "src" VkAccelerationStructureKHR 'False
                                                                 #{offset VkCopyAccelerationStructureInfoKHR, src}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dst" VkAccelerationStructureKHR 'False
                                                                 #{offset VkCopyAccelerationStructureInfoKHR, dst}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "mode" VkCopyAccelerationStructureModeKHR 'False
                #{offset VkCopyAccelerationStructureInfoKHR, mode}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkCopyAccelerationStructureToMemoryInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                            pNext;
--   >     VkAccelerationStructureKHR                             src;
--   >     VkDeviceOrHostAddressKHR                               dst;
--   >     VkCopyAccelerationStructureModeKHR                     mode;
--   > } VkCopyAccelerationStructureToMemoryInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCopyAccelerationStructureToMemoryInfoKHR VkCopyAccelerationStructureToMemoryInfoKHR registry at www.khronos.org>
type VkCopyAccelerationStructureToMemoryInfoKHR =
     VkStruct VkCopyAccelerationStructureToMemoryInfoKHR' -- ' closing tick for hsc2hs

data VkCopyAccelerationStructureToMemoryInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkCopyAccelerationStructureToMemoryInfoKHR
         where
    type StructRep VkCopyAccelerationStructureToMemoryInfoKHR =
         'StructMeta "VkCopyAccelerationStructureToMemoryInfoKHR" -- ' closing tick for hsc2hs
           VkCopyAccelerationStructureToMemoryInfoKHR
           #{size VkCopyAccelerationStructureToMemoryInfoKHR}
           #{alignment VkCopyAccelerationStructureToMemoryInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkCopyAccelerationStructureToMemoryInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkCopyAccelerationStructureToMemoryInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "src" VkAccelerationStructureKHR 'False
                                                                 #{offset VkCopyAccelerationStructureToMemoryInfoKHR, src}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dst" VkDeviceOrHostAddressKHR 'False
                                                               #{offset VkCopyAccelerationStructureToMemoryInfoKHR, dst}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "mode" VkCopyAccelerationStructureModeKHR 'False
                #{offset VkCopyAccelerationStructureToMemoryInfoKHR, mode}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkCopyMemoryToAccelerationStructureInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                            pNext;
--   >     VkDeviceOrHostAddressConstKHR                          src;
--   >     VkAccelerationStructureKHR                             dst;
--   >     VkCopyAccelerationStructureModeKHR                     mode;
--   > } VkCopyMemoryToAccelerationStructureInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCopyMemoryToAccelerationStructureInfoKHR VkCopyMemoryToAccelerationStructureInfoKHR registry at www.khronos.org>
type VkCopyMemoryToAccelerationStructureInfoKHR =
     VkStruct VkCopyMemoryToAccelerationStructureInfoKHR' -- ' closing tick for hsc2hs

data VkCopyMemoryToAccelerationStructureInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkCopyMemoryToAccelerationStructureInfoKHR
         where
    type StructRep VkCopyMemoryToAccelerationStructureInfoKHR =
         'StructMeta "VkCopyMemoryToAccelerationStructureInfoKHR" -- ' closing tick for hsc2hs
           VkCopyMemoryToAccelerationStructureInfoKHR
           #{size VkCopyMemoryToAccelerationStructureInfoKHR}
           #{alignment VkCopyMemoryToAccelerationStructureInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkCopyMemoryToAccelerationStructureInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkCopyMemoryToAccelerationStructureInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "src" VkDeviceOrHostAddressConstKHR 'False
                                                                    #{offset VkCopyMemoryToAccelerationStructureInfoKHR, src}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dst" VkAccelerationStructureKHR 'False
                                                                 #{offset VkCopyMemoryToAccelerationStructureInfoKHR, dst}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "mode" VkCopyAccelerationStructureModeKHR 'False
                #{offset VkCopyMemoryToAccelerationStructureInfoKHR, mode}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkDeferredOperationInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                   pNext;
--   >     VkDeferredOperationKHR        operationHandle;
--   > } VkDeferredOperationInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkDeferredOperationInfoKHR VkDeferredOperationInfoKHR registry at www.khronos.org>
type VkDeferredOperationInfoKHR =
     VkStruct VkDeferredOperationInfoKHR' -- ' closing tick for hsc2hs

data VkDeferredOperationInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeferredOperationInfoKHR where
    type StructRep VkDeferredOperationInfoKHR =
         'StructMeta "VkDeferredOperationInfoKHR" VkDeferredOperationInfoKHR -- ' closing tick for hsc2hs
           #{size VkDeferredOperationInfoKHR}
           #{alignment VkDeferredOperationInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkDeferredOperationInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkDeferredOperationInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "operationHandle" VkDeferredOperationKHR 'False
                #{offset VkDeferredOperationInfoKHR, operationHandle}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkRayTracingPipelineCreateInfoKHR, -- ' closing tick for hsc2hs
             VkAccelerationStructureBuildGeometryInfoKHR,
             VkCopyAccelerationStructureInfoKHR,
             VkCopyMemoryToAccelerationStructureInfoKHR,
             VkCopyAccelerationStructureToMemoryInfoKHR]

-- | > typedef union VkDeviceOrHostAddressConstKHR {
--   >     VkDeviceAddress            deviceAddress;
--   >     const void*                hostAddress;
--   > } VkDeviceOrHostAddressConstKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkDeviceOrHostAddressConstKHR VkDeviceOrHostAddressConstKHR registry at www.khronos.org>
type VkDeviceOrHostAddressConstKHR =
     VkStruct VkDeviceOrHostAddressConstKHR' -- ' closing tick for hsc2hs

data VkDeviceOrHostAddressConstKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceOrHostAddressConstKHR where
    type StructRep VkDeviceOrHostAddressConstKHR =
         'StructMeta "VkDeviceOrHostAddressConstKHR" -- ' closing tick for hsc2hs
           VkDeviceOrHostAddressConstKHR
           #{size VkDeviceOrHostAddressConstKHR}
           #{alignment VkDeviceOrHostAddressConstKHR}
           '[('FieldMeta "deviceAddress" VkDeviceAddress 'False  -- ' closing tick for hsc2hs
                                                                #{offset VkDeviceOrHostAddressConstKHR, deviceAddress}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "hostAddress" (Ptr Void) 'False
                                                         #{offset VkDeviceOrHostAddressConstKHR, hostAddress}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef union VkDeviceOrHostAddressKHR {
--   >     VkDeviceAddress            deviceAddress;
--   >     void*                      hostAddress;
--   > } VkDeviceOrHostAddressKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkDeviceOrHostAddressKHR VkDeviceOrHostAddressKHR registry at www.khronos.org>
type VkDeviceOrHostAddressKHR = VkStruct VkDeviceOrHostAddressKHR' -- ' closing tick for hsc2hs

data VkDeviceOrHostAddressKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkDeviceOrHostAddressKHR where
    type StructRep VkDeviceOrHostAddressKHR =
         'StructMeta "VkDeviceOrHostAddressKHR" VkDeviceOrHostAddressKHR -- ' closing tick for hsc2hs
           #{size VkDeviceOrHostAddressKHR}
           #{alignment VkDeviceOrHostAddressKHR}
           '[('FieldMeta "deviceAddress" VkDeviceAddress 'False  -- ' closing tick for hsc2hs
                                                                #{offset VkDeviceOrHostAddressKHR, deviceAddress}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "hostAddress" (Ptr Void) 'False
                                                         #{offset VkDeviceOrHostAddressKHR, hostAddress}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceRayTracingFeaturesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         rayTracing;
--   >     VkBool32                         rayTracingShaderGroupHandleCaptureReplay;
--   >     VkBool32                         rayTracingShaderGroupHandleCaptureReplayMixed;
--   >     VkBool32                         rayTracingAccelerationStructureCaptureReplay;
--   >     VkBool32                         rayTracingIndirectTraceRays;
--   >     VkBool32                         rayTracingIndirectAccelerationStructureBuild;
--   >     VkBool32                         rayTracingHostAccelerationStructureCommands;
--   >     VkBool32                         rayQuery;
--   >     VkBool32                         rayTracingPrimitiveCulling;
--   > } VkPhysicalDeviceRayTracingFeaturesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceRayTracingFeaturesKHR VkPhysicalDeviceRayTracingFeaturesKHR registry at www.khronos.org>
type VkPhysicalDeviceRayTracingFeaturesKHR =
     VkStruct VkPhysicalDeviceRayTracingFeaturesKHR' -- ' closing tick for hsc2hs

data VkPhysicalDeviceRayTracingFeaturesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceRayTracingFeaturesKHR where
    type StructRep VkPhysicalDeviceRayTracingFeaturesKHR =
         'StructMeta "VkPhysicalDeviceRayTracingFeaturesKHR" -- ' closing tick for hsc2hs
           VkPhysicalDeviceRayTracingFeaturesKHR
           #{size VkPhysicalDeviceRayTracingFeaturesKHR}
           #{alignment VkPhysicalDeviceRayTracingFeaturesKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceRayTracingFeaturesKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPhysicalDeviceRayTracingFeaturesKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "rayTracing" VkBool32 'False
                                                      #{offset VkPhysicalDeviceRayTracingFeaturesKHR, rayTracing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "rayTracingShaderGroupHandleCaptureReplay" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceRayTracingFeaturesKHR, rayTracingShaderGroupHandleCaptureReplay}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "rayTracingShaderGroupHandleCaptureReplayMixed" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceRayTracingFeaturesKHR, rayTracingShaderGroupHandleCaptureReplayMixed}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "rayTracingAccelerationStructureCaptureReplay" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceRayTracingFeaturesKHR, rayTracingAccelerationStructureCaptureReplay}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "rayTracingIndirectTraceRays" VkBool32 'False
                                                                       #{offset VkPhysicalDeviceRayTracingFeaturesKHR, rayTracingIndirectTraceRays}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "rayTracingIndirectAccelerationStructureBuild" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceRayTracingFeaturesKHR, rayTracingIndirectAccelerationStructureBuild}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "rayTracingHostAccelerationStructureCommands" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceRayTracingFeaturesKHR, rayTracingHostAccelerationStructureCommands}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "rayQuery" VkBool32 'False
                                                    #{offset VkPhysicalDeviceRayTracingFeaturesKHR, rayQuery}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "rayTracingPrimitiveCulling" VkBool32 'False
                                                                      #{offset VkPhysicalDeviceRayTracingFeaturesKHR, rayTracingPrimitiveCulling}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceRayTracingPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         shaderGroupHandleSize;
--   >     uint32_t                         maxRecursionDepth;
--   >     uint32_t                         maxShaderGroupStride;
--   >     uint32_t                         shaderGroupBaseAlignment;
--   >     uint64_t                         maxGeometryCount;
--   >     uint64_t                         maxInstanceCount;
--   >     uint64_t                         maxPrimitiveCount;
--   >     uint32_t                         maxDescriptorSetAccelerationStructures;
--   >     uint32_t                         shaderGroupHandleCaptureReplaySize;
--   > } VkPhysicalDeviceRayTracingPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceRayTracingPropertiesKHR VkPhysicalDeviceRayTracingPropertiesKHR registry at www.khronos.org>
type VkPhysicalDeviceRayTracingPropertiesKHR =
     VkStruct VkPhysicalDeviceRayTracingPropertiesKHR' -- ' closing tick for hsc2hs

data VkPhysicalDeviceRayTracingPropertiesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceRayTracingPropertiesKHR
         where
    type StructRep VkPhysicalDeviceRayTracingPropertiesKHR =
         'StructMeta "VkPhysicalDeviceRayTracingPropertiesKHR" -- ' closing tick for hsc2hs
           VkPhysicalDeviceRayTracingPropertiesKHR
           #{size VkPhysicalDeviceRayTracingPropertiesKHR}
           #{alignment VkPhysicalDeviceRayTracingPropertiesKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceRayTracingPropertiesKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPhysicalDeviceRayTracingPropertiesKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderGroupHandleSize" Word32 'False
                                                               #{offset VkPhysicalDeviceRayTracingPropertiesKHR, shaderGroupHandleSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxRecursionDepth" Word32 'False
                                                           #{offset VkPhysicalDeviceRayTracingPropertiesKHR, maxRecursionDepth}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxShaderGroupStride" Word32 'False
                                                              #{offset VkPhysicalDeviceRayTracingPropertiesKHR, maxShaderGroupStride}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderGroupBaseAlignment" Word32 'False
                                                                  #{offset VkPhysicalDeviceRayTracingPropertiesKHR, shaderGroupBaseAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxGeometryCount" Word64 'False
                                                          #{offset VkPhysicalDeviceRayTracingPropertiesKHR, maxGeometryCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxInstanceCount" Word64 'False
                                                          #{offset VkPhysicalDeviceRayTracingPropertiesKHR, maxInstanceCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPrimitiveCount" Word64 'False
                                                           #{offset VkPhysicalDeviceRayTracingPropertiesKHR, maxPrimitiveCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetAccelerationStructures" Word32 'False
                #{offset VkPhysicalDeviceRayTracingPropertiesKHR, maxDescriptorSetAccelerationStructures}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderGroupHandleCaptureReplaySize" Word32 'False
                #{offset VkPhysicalDeviceRayTracingPropertiesKHR, shaderGroupHandleCaptureReplaySize}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPipelineLibraryCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                            pNext;
--   >     uint32_t                               libraryCount;
--   >     const VkPipeline*                   pLibraries;
--   > } VkPipelineLibraryCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineLibraryCreateInfoKHR VkPipelineLibraryCreateInfoKHR registry at www.khronos.org>
type VkPipelineLibraryCreateInfoKHR =
     VkStruct VkPipelineLibraryCreateInfoKHR' -- ' closing tick for hsc2hs

data VkPipelineLibraryCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPipelineLibraryCreateInfoKHR where
    type StructRep VkPipelineLibraryCreateInfoKHR =
         'StructMeta "VkPipelineLibraryCreateInfoKHR" -- ' closing tick for hsc2hs
           VkPipelineLibraryCreateInfoKHR
           #{size VkPipelineLibraryCreateInfoKHR}
           #{alignment VkPipelineLibraryCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPipelineLibraryCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkPipelineLibraryCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "libraryCount" Word32 'True
                                                     #{offset VkPipelineLibraryCreateInfoKHR, libraryCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pLibraries" (Ptr VkPipeline) 'False
                                                              #{offset VkPipelineLibraryCreateInfoKHR, pLibraries}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkRayTracingPipelineCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineCreateFlags  flags;
--   >     uint32_t stageCount;
--   >     const VkPipelineShaderStageCreateInfo* pStages;
--   >     uint32_t groupCount;
--   >     const VkRayTracingShaderGroupCreateInfoKHR* pGroups;
--   >     uint32_t               maxRecursionDepth;
--   >     VkPipelineLibraryCreateInfoKHR libraries;
--   >     const VkRayTracingPipelineInterfaceCreateInfoKHR* pLibraryInterface;
--   >     VkPipelineLayout       layout;
--   >     VkPipeline      basePipelineHandle;
--   >     int32_t                basePipelineIndex;
--   > } VkRayTracingPipelineCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRayTracingPipelineCreateInfoKHR VkRayTracingPipelineCreateInfoKHR registry at www.khronos.org>
type VkRayTracingPipelineCreateInfoKHR =
     VkStruct VkRayTracingPipelineCreateInfoKHR' -- ' closing tick for hsc2hs

data VkRayTracingPipelineCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRayTracingPipelineCreateInfoKHR where
    type StructRep VkRayTracingPipelineCreateInfoKHR =
         'StructMeta "VkRayTracingPipelineCreateInfoKHR" -- ' closing tick for hsc2hs
           VkRayTracingPipelineCreateInfoKHR
           #{size VkRayTracingPipelineCreateInfoKHR}
           #{alignment VkRayTracingPipelineCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkRayTracingPipelineCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkRayTracingPipelineCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkPipelineCreateFlags 'True
                                                             #{offset VkRayTracingPipelineCreateInfoKHR, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stageCount" Word32 'True
                                                   #{offset VkRayTracingPipelineCreateInfoKHR, stageCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pStages" (Ptr VkPipelineShaderStageCreateInfo) 'False
                #{offset VkRayTracingPipelineCreateInfoKHR, pStages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "groupCount" Word32 'True
                                                   #{offset VkRayTracingPipelineCreateInfoKHR, groupCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pGroups" (Ptr VkRayTracingShaderGroupCreateInfoKHR) -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkRayTracingPipelineCreateInfoKHR, pGroups}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxRecursionDepth" Word32 'False
                                                           #{offset VkRayTracingPipelineCreateInfoKHR, maxRecursionDepth}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "libraries" VkPipelineLibraryCreateInfoKHR 'False
                #{offset VkRayTracingPipelineCreateInfoKHR, libraries}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pLibraryInterface" -- ' closing tick for hsc2hs
                (Ptr VkRayTracingPipelineInterfaceCreateInfoKHR)
                'True -- ' closing tick for hsc2hs
                #{offset VkRayTracingPipelineCreateInfoKHR, pLibraryInterface}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "layout" VkPipelineLayout 'False
                                                          #{offset VkRayTracingPipelineCreateInfoKHR, layout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "basePipelineHandle" VkPipeline 'True
                                                               #{offset VkRayTracingPipelineCreateInfoKHR, basePipelineHandle}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "basePipelineIndex" Int32 'False
                                                          #{offset VkRayTracingPipelineCreateInfoKHR, basePipelineIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkRayTracingPipelineInterfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                            pNext;
--   >     uint32_t                                               maxPayloadSize;
--   >     uint32_t                                               maxAttributeSize;
--   >     uint32_t                                               maxCallableSize;
--   > } VkRayTracingPipelineInterfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRayTracingPipelineInterfaceCreateInfoKHR VkRayTracingPipelineInterfaceCreateInfoKHR registry at www.khronos.org>
type VkRayTracingPipelineInterfaceCreateInfoKHR =
     VkStruct VkRayTracingPipelineInterfaceCreateInfoKHR' -- ' closing tick for hsc2hs

data VkRayTracingPipelineInterfaceCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRayTracingPipelineInterfaceCreateInfoKHR
         where
    type StructRep VkRayTracingPipelineInterfaceCreateInfoKHR =
         'StructMeta "VkRayTracingPipelineInterfaceCreateInfoKHR" -- ' closing tick for hsc2hs
           VkRayTracingPipelineInterfaceCreateInfoKHR
           #{size VkRayTracingPipelineInterfaceCreateInfoKHR}
           #{alignment VkRayTracingPipelineInterfaceCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkRayTracingPipelineInterfaceCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkRayTracingPipelineInterfaceCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPayloadSize" Word32 'False
                                                        #{offset VkRayTracingPipelineInterfaceCreateInfoKHR, maxPayloadSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxAttributeSize" Word32 'False
                                                          #{offset VkRayTracingPipelineInterfaceCreateInfoKHR, maxAttributeSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxCallableSize" Word32 'False
                                                         #{offset VkRayTracingPipelineInterfaceCreateInfoKHR, maxCallableSize}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkRayTracingShaderGroupCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkRayTracingShaderGroupTypeKHR type;
--   >     uint32_t               generalShader;
--   >     uint32_t               closestHitShader;
--   >     uint32_t               anyHitShader;
--   >     uint32_t               intersectionShader;
--   >     const void* pShaderGroupCaptureReplayHandle;
--   > } VkRayTracingShaderGroupCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkRayTracingShaderGroupCreateInfoKHR VkRayTracingShaderGroupCreateInfoKHR registry at www.khronos.org>
type VkRayTracingShaderGroupCreateInfoKHR =
     VkStruct VkRayTracingShaderGroupCreateInfoKHR' -- ' closing tick for hsc2hs

data VkRayTracingShaderGroupCreateInfoKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkRayTracingShaderGroupCreateInfoKHR where
    type StructRep VkRayTracingShaderGroupCreateInfoKHR =
         'StructMeta "VkRayTracingShaderGroupCreateInfoKHR" -- ' closing tick for hsc2hs
           VkRayTracingShaderGroupCreateInfoKHR
           #{size VkRayTracingShaderGroupCreateInfoKHR}
           #{alignment VkRayTracingShaderGroupCreateInfoKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkRayTracingShaderGroupCreateInfoKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkRayTracingShaderGroupCreateInfoKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "type" VkRayTracingShaderGroupTypeKHR 'False
                                                                      #{offset VkRayTracingShaderGroupCreateInfoKHR, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "generalShader" Word32 'False
                                                       #{offset VkRayTracingShaderGroupCreateInfoKHR, generalShader}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "closestHitShader" Word32 'False
                                                          #{offset VkRayTracingShaderGroupCreateInfoKHR, closestHitShader}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "anyHitShader" Word32 'False
                                                      #{offset VkRayTracingShaderGroupCreateInfoKHR, anyHitShader}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "intersectionShader" Word32 'False
                                                            #{offset VkRayTracingShaderGroupCreateInfoKHR, intersectionShader}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pShaderGroupCaptureReplayHandle" (Ptr Void) 'True
                #{offset VkRayTracingShaderGroupCreateInfoKHR, pShaderGroupCaptureReplayHandle}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkStridedBufferRegionKHR {
--   >     VkBuffer         buffer;
--   >     VkDeviceSize                     offset;
--   >     VkDeviceSize                     stride;
--   >     VkDeviceSize                     size;
--   > } VkStridedBufferRegionKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkStridedBufferRegionKHR VkStridedBufferRegionKHR registry at www.khronos.org>
type VkStridedBufferRegionKHR = VkStruct VkStridedBufferRegionKHR' -- ' closing tick for hsc2hs

data VkStridedBufferRegionKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkStridedBufferRegionKHR where
    type StructRep VkStridedBufferRegionKHR =
         'StructMeta "VkStridedBufferRegionKHR" VkStridedBufferRegionKHR -- ' closing tick for hsc2hs
           #{size VkStridedBufferRegionKHR}
           #{alignment VkStridedBufferRegionKHR}
           '[('FieldMeta "buffer" VkBuffer 'True  -- ' closing tick for hsc2hs
                                                 #{offset VkStridedBufferRegionKHR, buffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "offset" VkDeviceSize 'False
                                                      #{offset VkStridedBufferRegionKHR, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stride" VkDeviceSize 'False
                                                      #{offset VkStridedBufferRegionKHR, stride}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "size" VkDeviceSize 'False
                                                    #{offset VkStridedBufferRegionKHR, size}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkTraceRaysIndirectCommandKHR {
--   >     uint32_t               width;
--   >     uint32_t               height;
--   >     uint32_t               depth;
--   > } VkTraceRaysIndirectCommandKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkTraceRaysIndirectCommandKHR VkTraceRaysIndirectCommandKHR registry at www.khronos.org>
type VkTraceRaysIndirectCommandKHR =
     VkStruct VkTraceRaysIndirectCommandKHR' -- ' closing tick for hsc2hs

data VkTraceRaysIndirectCommandKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkTraceRaysIndirectCommandKHR where
    type StructRep VkTraceRaysIndirectCommandKHR =
         'StructMeta "VkTraceRaysIndirectCommandKHR" -- ' closing tick for hsc2hs
           VkTraceRaysIndirectCommandKHR
           #{size VkTraceRaysIndirectCommandKHR}
           #{alignment VkTraceRaysIndirectCommandKHR}
           '[('FieldMeta "width" Word32 'False  -- ' closing tick for hsc2hs
                                               #{offset VkTraceRaysIndirectCommandKHR, width}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "height" Word32 'False
                                                #{offset VkTraceRaysIndirectCommandKHR, height}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depth" Word32 'False
                                               #{offset VkTraceRaysIndirectCommandKHR, depth}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkTransformMatrixKHR {
--   >     float                                                   matrix[3][4];
--   > } VkTransformMatrixKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkTransformMatrixKHR VkTransformMatrixKHR registry at www.khronos.org>
type VkTransformMatrixKHR = VkStruct VkTransformMatrixKHR' -- ' closing tick for hsc2hs

data VkTransformMatrixKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkTransformMatrixKHR where
    type StructRep VkTransformMatrixKHR =
         'StructMeta "VkTransformMatrixKHR" VkTransformMatrixKHR  -- ' closing tick for hsc2hs
                                                                 #{size VkTransformMatrixKHR}
           #{alignment VkTransformMatrixKHR}
           '[('FieldMeta "matrix" (
                                   #{type float}
                                   ) 'False  -- ' closing tick for hsc2hs
                                            #{offset VkTransformMatrixKHR, matrix}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkWriteDescriptorSetAccelerationStructureKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         accelerationStructureCount;
--   >     const VkAccelerationStructureKHR* pAccelerationStructures;
--   > } VkWriteDescriptorSetAccelerationStructureKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkWriteDescriptorSetAccelerationStructureKHR VkWriteDescriptorSetAccelerationStructureKHR registry at www.khronos.org>
type VkWriteDescriptorSetAccelerationStructureKHR =
     VkStruct VkWriteDescriptorSetAccelerationStructureKHR' -- ' closing tick for hsc2hs

data VkWriteDescriptorSetAccelerationStructureKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkWriteDescriptorSetAccelerationStructureKHR
         where
    type StructRep VkWriteDescriptorSetAccelerationStructureKHR =
         'StructMeta "VkWriteDescriptorSetAccelerationStructureKHR" -- ' closing tick for hsc2hs
           VkWriteDescriptorSetAccelerationStructureKHR
           #{size VkWriteDescriptorSetAccelerationStructureKHR}
           #{alignment VkWriteDescriptorSetAccelerationStructureKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkWriteDescriptorSetAccelerationStructureKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False
                                                   #{offset VkWriteDescriptorSetAccelerationStructureKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "accelerationStructureCount" Word32 'False
                                                                    #{offset VkWriteDescriptorSetAccelerationStructureKHR, accelerationStructureCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pAccelerationStructures" -- ' closing tick for hsc2hs
                (Ptr VkAccelerationStructureKHR)
                'False -- ' closing tick for hsc2hs
                #{offset VkWriteDescriptorSetAccelerationStructureKHR, pAccelerationStructures}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkWriteDescriptorSet] -- ' closing tick for hsc2hs
