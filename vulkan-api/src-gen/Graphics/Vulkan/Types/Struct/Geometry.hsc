#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Geometry
       (VkGeometryAABBNV, VkGeometryDataNV, VkGeometryNV,
        VkGeometryTrianglesNV)
       where
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.BaseTypes          (VkDeviceSize)
import Graphics.Vulkan.Types.Enum.Format        (VkFormat)
import Graphics.Vulkan.Types.Enum.Geometry      (VkGeometryFlagsKHR,
                                                 VkGeometryTypeKHR)
import Graphics.Vulkan.Types.Enum.IndexType     (VkIndexType)
import Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import Graphics.Vulkan.Types.Handles            (VkBuffer)

-- | > typedef struct VkGeometryAABBNV {
--   >     VkStructureType sType;
--   >     const void*                pNext;
--   >     VkBuffer   aabbData;
--   >     uint32_t                   numAABBs;
--   >     uint32_t                   stride;
--   >     VkDeviceSize               offset;
--   > } VkGeometryAABBNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkGeometryAABBNV VkGeometryAABBNV registry at www.khronos.org>
type VkGeometryAABBNV = VkStruct VkGeometryAABBNV' -- ' closing tick for hsc2hs

data VkGeometryAABBNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkGeometryAABBNV where
    type StructRep VkGeometryAABBNV =
         'StructMeta "VkGeometryAABBNV" VkGeometryAABBNV  -- ' closing tick for hsc2hs
                                                         #{size VkGeometryAABBNV}
           #{alignment VkGeometryAABBNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkGeometryAABBNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkGeometryAABBNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "aabbData" VkBuffer 'True 
                                                   #{offset VkGeometryAABBNV, aabbData}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "numAABBs" Word32 'False 
                                                  #{offset VkGeometryAABBNV, numAABBs}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stride" Word32 'False 
                                                #{offset VkGeometryAABBNV, stride}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "offset" VkDeviceSize 'False 
                                                      #{offset VkGeometryAABBNV, offset}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkGeometryDataNV {
--   >     VkGeometryTrianglesNV                  triangles;
--   >     VkGeometryAABBNV                       aabbs;
--   > } VkGeometryDataNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkGeometryDataNV VkGeometryDataNV registry at www.khronos.org>
type VkGeometryDataNV = VkStruct VkGeometryDataNV' -- ' closing tick for hsc2hs

data VkGeometryDataNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkGeometryDataNV where
    type StructRep VkGeometryDataNV =
         'StructMeta "VkGeometryDataNV" VkGeometryDataNV  -- ' closing tick for hsc2hs
                                                         #{size VkGeometryDataNV}
           #{alignment VkGeometryDataNV}
           '[('FieldMeta "triangles" VkGeometryTrianglesNV 'False  -- ' closing tick for hsc2hs
                                                                  #{offset VkGeometryDataNV, triangles}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "aabbs" VkGeometryAABBNV 'False 
                                                         #{offset VkGeometryDataNV, aabbs}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkGeometryNV {
--   >     VkStructureType sType;
--   >     const void*                                   pNext;
--   >     VkGeometryTypeKHR                  geometryType;
--   >     VkGeometryDataNV                              geometry;
--   >     VkGeometryFlagsKHR flags;
--   > } VkGeometryNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkGeometryNV VkGeometryNV registry at www.khronos.org>
type VkGeometryNV = VkStruct VkGeometryNV' -- ' closing tick for hsc2hs

data VkGeometryNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkGeometryNV where
    type StructRep VkGeometryNV =
         'StructMeta "VkGeometryNV" VkGeometryNV  -- ' closing tick for hsc2hs
                                                 #{size VkGeometryNV}
           #{alignment VkGeometryNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkGeometryNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkGeometryNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "geometryType" VkGeometryTypeKHR 'False 
                                                                 #{offset VkGeometryNV, geometryType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "geometry" VkGeometryDataNV 'False 
                                                            #{offset VkGeometryNV, geometry}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkGeometryFlagsKHR 'True 
                                                          #{offset VkGeometryNV, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkGeometryTrianglesNV {
--   >     VkStructureType sType;
--   >     const void*                pNext;
--   >     VkBuffer   vertexData;
--   >     VkDeviceSize               vertexOffset;
--   >     uint32_t                   vertexCount;
--   >     VkDeviceSize               vertexStride;
--   >     VkFormat                   vertexFormat;
--   >     VkBuffer   indexData;
--   >     VkDeviceSize               indexOffset;
--   >     uint32_t                   indexCount;
--   >     VkIndexType                indexType;
--   >     VkBuffer   transformData;
--   >     VkDeviceSize               transformOffset;
--   > } VkGeometryTrianglesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkGeometryTrianglesNV VkGeometryTrianglesNV registry at www.khronos.org>
type VkGeometryTrianglesNV = VkStruct VkGeometryTrianglesNV' -- ' closing tick for hsc2hs

data VkGeometryTrianglesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkGeometryTrianglesNV where
    type StructRep VkGeometryTrianglesNV =
         'StructMeta "VkGeometryTrianglesNV" VkGeometryTrianglesNV  -- ' closing tick for hsc2hs
                                                                   #{size VkGeometryTrianglesNV}
           #{alignment VkGeometryTrianglesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkGeometryTrianglesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkGeometryTrianglesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexData" VkBuffer 'True 
                                                     #{offset VkGeometryTrianglesNV, vertexData}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexOffset" VkDeviceSize 'False 
                                                            #{offset VkGeometryTrianglesNV, vertexOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexCount" Word32 'False 
                                                     #{offset VkGeometryTrianglesNV, vertexCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexStride" VkDeviceSize 'False 
                                                            #{offset VkGeometryTrianglesNV, vertexStride}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexFormat" VkFormat 'False 
                                                        #{offset VkGeometryTrianglesNV, vertexFormat}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "indexData" VkBuffer 'True 
                                                    #{offset VkGeometryTrianglesNV, indexData}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "indexOffset" VkDeviceSize 'False 
                                                           #{offset VkGeometryTrianglesNV, indexOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "indexCount" Word32 'False 
                                                    #{offset VkGeometryTrianglesNV, indexCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "indexType" VkIndexType 'False 
                                                        #{offset VkGeometryTrianglesNV, indexType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "transformData" VkBuffer 'True 
                                                        #{offset VkGeometryTrianglesNV, transformData}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "transformOffset" VkDeviceSize 'False 
                                                               #{offset VkGeometryTrianglesNV, transformOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
