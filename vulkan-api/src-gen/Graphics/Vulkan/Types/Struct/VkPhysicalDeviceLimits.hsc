#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits
       (VkPhysicalDeviceLimits(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                                  (KnownNat,
                                                                natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes               (VkBool32,
                                                                VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags (VkSampleCountFlags)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceLimits {
--   >     uint32_t               maxImageDimension1D;
--   >     uint32_t               maxImageDimension2D;
--   >     uint32_t               maxImageDimension3D;
--   >     uint32_t               maxImageDimensionCube;
--   >     uint32_t               maxImageArrayLayers;
--   >     uint32_t               maxTexelBufferElements;
--   >     uint32_t               maxUniformBufferRange;
--   >     uint32_t               maxStorageBufferRange;
--   >     uint32_t               maxPushConstantsSize;
--   >     uint32_t               maxMemoryAllocationCount;
--   >     uint32_t               maxSamplerAllocationCount;
--   >     VkDeviceSize           bufferImageGranularity;
--   >     VkDeviceSize           sparseAddressSpaceSize;
--   >     uint32_t               maxBoundDescriptorSets;
--   >     uint32_t               maxPerStageDescriptorSamplers;
--   >     uint32_t               maxPerStageDescriptorUniformBuffers;
--   >     uint32_t               maxPerStageDescriptorStorageBuffers;
--   >     uint32_t               maxPerStageDescriptorSampledImages;
--   >     uint32_t               maxPerStageDescriptorStorageImages;
--   >     uint32_t               maxPerStageDescriptorInputAttachments;
--   >     uint32_t               maxPerStageResources;
--   >     uint32_t               maxDescriptorSetSamplers;
--   >     uint32_t               maxDescriptorSetUniformBuffers;
--   >     uint32_t               maxDescriptorSetUniformBuffersDynamic;
--   >     uint32_t               maxDescriptorSetStorageBuffers;
--   >     uint32_t               maxDescriptorSetStorageBuffersDynamic;
--   >     uint32_t               maxDescriptorSetSampledImages;
--   >     uint32_t               maxDescriptorSetStorageImages;
--   >     uint32_t               maxDescriptorSetInputAttachments;
--   >     uint32_t               maxVertexInputAttributes;
--   >     uint32_t               maxVertexInputBindings;
--   >     uint32_t               maxVertexInputAttributeOffset;
--   >     uint32_t               maxVertexInputBindingStride;
--   >     uint32_t               maxVertexOutputComponents;
--   >     uint32_t               maxTessellationGenerationLevel;
--   >     uint32_t               maxTessellationPatchSize;
--   >     uint32_t               maxTessellationControlPerVertexInputComponents;
--   >     uint32_t               maxTessellationControlPerVertexOutputComponents;
--   >     uint32_t               maxTessellationControlPerPatchOutputComponents;
--   >     uint32_t               maxTessellationControlTotalOutputComponents;
--   >     uint32_t               maxTessellationEvaluationInputComponents;
--   >     uint32_t               maxTessellationEvaluationOutputComponents;
--   >     uint32_t               maxGeometryShaderInvocations;
--   >     uint32_t               maxGeometryInputComponents;
--   >     uint32_t               maxGeometryOutputComponents;
--   >     uint32_t               maxGeometryOutputVertices;
--   >     uint32_t               maxGeometryTotalOutputComponents;
--   >     uint32_t               maxFragmentInputComponents;
--   >     uint32_t               maxFragmentOutputAttachments;
--   >     uint32_t               maxFragmentDualSrcAttachments;
--   >     uint32_t               maxFragmentCombinedOutputResources;
--   >     uint32_t               maxComputeSharedMemorySize;
--   >     uint32_t               maxComputeWorkGroupCount[3];
--   >     uint32_t               maxComputeWorkGroupInvocations;
--   >     uint32_t               maxComputeWorkGroupSize[3];
--   >     uint32_t               subPixelPrecisionBits;
--   >     uint32_t               subTexelPrecisionBits;
--   >     uint32_t               mipmapPrecisionBits;
--   >     uint32_t               maxDrawIndexedIndexValue;
--   >     uint32_t               maxDrawIndirectCount;
--   >     float                  maxSamplerLodBias;
--   >     float                  maxSamplerAnisotropy;
--   >     uint32_t               maxViewports;
--   >     uint32_t               maxViewportDimensions[2];
--   >     float                  viewportBoundsRange[2];
--   >     uint32_t               viewportSubPixelBits;
--   >     size_t                 minMemoryMapAlignment;
--   >     VkDeviceSize           minTexelBufferOffsetAlignment;
--   >     VkDeviceSize           minUniformBufferOffsetAlignment;
--   >     VkDeviceSize           minStorageBufferOffsetAlignment;
--   >     int32_t                minTexelOffset;
--   >     uint32_t               maxTexelOffset;
--   >     int32_t                minTexelGatherOffset;
--   >     uint32_t               maxTexelGatherOffset;
--   >     float                  minInterpolationOffset;
--   >     float                  maxInterpolationOffset;
--   >     uint32_t               subPixelInterpolationOffsetBits;
--   >     uint32_t               maxFramebufferWidth;
--   >     uint32_t               maxFramebufferHeight;
--   >     uint32_t               maxFramebufferLayers;
--   >     VkSampleCountFlags     framebufferColorSampleCounts;
--   >     VkSampleCountFlags     framebufferDepthSampleCounts;
--   >     VkSampleCountFlags     framebufferStencilSampleCounts;
--   >     VkSampleCountFlags     framebufferNoAttachmentsSampleCounts;
--   >     uint32_t               maxColorAttachments;
--   >     VkSampleCountFlags     sampledImageColorSampleCounts;
--   >     VkSampleCountFlags     sampledImageIntegerSampleCounts;
--   >     VkSampleCountFlags     sampledImageDepthSampleCounts;
--   >     VkSampleCountFlags     sampledImageStencilSampleCounts;
--   >     VkSampleCountFlags     storageImageSampleCounts;
--   >     uint32_t               maxSampleMaskWords;
--   >     VkBool32               timestampComputeAndGraphics;
--   >     float                  timestampPeriod;
--   >     uint32_t               maxClipDistances;
--   >     uint32_t               maxCullDistances;
--   >     uint32_t               maxCombinedClipAndCullDistances;
--   >     uint32_t               discreteQueuePriorities;
--   >     float                  pointSizeRange[2];
--   >     float                  lineWidthRange[2];
--   >     float                  pointSizeGranularity;
--   >     float                  lineWidthGranularity;
--   >     VkBool32               strictLines;
--   >     VkBool32               standardSampleLocations;
--   >     VkDeviceSize           optimalBufferCopyOffsetAlignment;
--   >     VkDeviceSize           optimalBufferCopyRowPitchAlignment;
--   >     VkDeviceSize           nonCoherentAtomSize;
--   > } VkPhysicalDeviceLimits;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceLimits.html VkPhysicalDeviceLimits registry at www.khronos.org>
data VkPhysicalDeviceLimits = VkPhysicalDeviceLimits## Addr##
                                                      ByteArray##

instance Eq VkPhysicalDeviceLimits where
        (VkPhysicalDeviceLimits## a _) == x@(VkPhysicalDeviceLimits## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceLimits where
        (VkPhysicalDeviceLimits## a _) `compare`
          x@(VkPhysicalDeviceLimits## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceLimits where
        sizeOf ~_ = #{size VkPhysicalDeviceLimits}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPhysicalDeviceLimits}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceLimits where
        unsafeAddr (VkPhysicalDeviceLimits## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceLimits## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceLimits## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceLimits where
        type StructFields VkPhysicalDeviceLimits =
             '["maxImageDimension1D", "maxImageDimension2D", -- ' closing tick for hsc2hs
               "maxImageDimension3D", "maxImageDimensionCube",
               "maxImageArrayLayers", "maxTexelBufferElements",
               "maxUniformBufferRange", "maxStorageBufferRange",
               "maxPushConstantsSize", "maxMemoryAllocationCount",
               "maxSamplerAllocationCount", "bufferImageGranularity",
               "sparseAddressSpaceSize", "maxBoundDescriptorSets",
               "maxPerStageDescriptorSamplers",
               "maxPerStageDescriptorUniformBuffers",
               "maxPerStageDescriptorStorageBuffers",
               "maxPerStageDescriptorSampledImages",
               "maxPerStageDescriptorStorageImages",
               "maxPerStageDescriptorInputAttachments", "maxPerStageResources",
               "maxDescriptorSetSamplers", "maxDescriptorSetUniformBuffers",
               "maxDescriptorSetUniformBuffersDynamic",
               "maxDescriptorSetStorageBuffers",
               "maxDescriptorSetStorageBuffersDynamic",
               "maxDescriptorSetSampledImages", "maxDescriptorSetStorageImages",
               "maxDescriptorSetInputAttachments", "maxVertexInputAttributes",
               "maxVertexInputBindings", "maxVertexInputAttributeOffset",
               "maxVertexInputBindingStride", "maxVertexOutputComponents",
               "maxTessellationGenerationLevel", "maxTessellationPatchSize",
               "maxTessellationControlPerVertexInputComponents",
               "maxTessellationControlPerVertexOutputComponents",
               "maxTessellationControlPerPatchOutputComponents",
               "maxTessellationControlTotalOutputComponents",
               "maxTessellationEvaluationInputComponents",
               "maxTessellationEvaluationOutputComponents",
               "maxGeometryShaderInvocations", "maxGeometryInputComponents",
               "maxGeometryOutputComponents", "maxGeometryOutputVertices",
               "maxGeometryTotalOutputComponents", "maxFragmentInputComponents",
               "maxFragmentOutputAttachments", "maxFragmentDualSrcAttachments",
               "maxFragmentCombinedOutputResources", "maxComputeSharedMemorySize",
               "maxComputeWorkGroupCount", "maxComputeWorkGroupInvocations",
               "maxComputeWorkGroupSize", "subPixelPrecisionBits",
               "subTexelPrecisionBits", "mipmapPrecisionBits",
               "maxDrawIndexedIndexValue", "maxDrawIndirectCount",
               "maxSamplerLodBias", "maxSamplerAnisotropy", "maxViewports",
               "maxViewportDimensions", "viewportBoundsRange",
               "viewportSubPixelBits", "minMemoryMapAlignment",
               "minTexelBufferOffsetAlignment", "minUniformBufferOffsetAlignment",
               "minStorageBufferOffsetAlignment", "minTexelOffset",
               "maxTexelOffset", "minTexelGatherOffset", "maxTexelGatherOffset",
               "minInterpolationOffset", "maxInterpolationOffset",
               "subPixelInterpolationOffsetBits", "maxFramebufferWidth",
               "maxFramebufferHeight", "maxFramebufferLayers",
               "framebufferColorSampleCounts", "framebufferDepthSampleCounts",
               "framebufferStencilSampleCounts",
               "framebufferNoAttachmentsSampleCounts", "maxColorAttachments",
               "sampledImageColorSampleCounts", "sampledImageIntegerSampleCounts",
               "sampledImageDepthSampleCounts", "sampledImageStencilSampleCounts",
               "storageImageSampleCounts", "maxSampleMaskWords",
               "timestampComputeAndGraphics", "timestampPeriod",
               "maxClipDistances", "maxCullDistances",
               "maxCombinedClipAndCullDistances", "discreteQueuePriorities",
               "pointSizeRange", "lineWidthRange", "pointSizeGranularity",
               "lineWidthGranularity", "strictLines", "standardSampleLocations",
               "optimalBufferCopyOffsetAlignment",
               "optimalBufferCopyRowPitchAlignment", "nonCoherentAtomSize"]
        type CUnionType VkPhysicalDeviceLimits = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceLimits = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceLimits = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkMaxImageDimension1D VkPhysicalDeviceLimits where
        type VkMaxImageDimension1DMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxImageDimension1D #-}
        vkMaxImageDimension1D x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxImageDimension1D})

        {-# INLINE vkMaxImageDimension1DByteOffset #-}
        vkMaxImageDimension1DByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxImageDimension1D}

        {-# INLINE readVkMaxImageDimension1D #-}
        readVkMaxImageDimension1D p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxImageDimension1D}

        {-# INLINE writeVkMaxImageDimension1D #-}
        writeVkMaxImageDimension1D p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxImageDimension1D}

instance {-# OVERLAPPING #-}
         HasField "maxImageDimension1D" VkPhysicalDeviceLimits where
        type FieldType "maxImageDimension1D" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxImageDimension1D" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxImageDimension1D" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxImageDimension1D}
        type FieldIsArray "maxImageDimension1D" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxImageDimension1D}

instance CanReadField "maxImageDimension1D" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxImageDimension1D

        {-# INLINE readField #-}
        readField = readVkMaxImageDimension1D

instance CanWriteField "maxImageDimension1D" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxImageDimension1D

instance {-# OVERLAPPING #-}
         HasVkMaxImageDimension2D VkPhysicalDeviceLimits where
        type VkMaxImageDimension2DMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxImageDimension2D #-}
        vkMaxImageDimension2D x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxImageDimension2D})

        {-# INLINE vkMaxImageDimension2DByteOffset #-}
        vkMaxImageDimension2DByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxImageDimension2D}

        {-# INLINE readVkMaxImageDimension2D #-}
        readVkMaxImageDimension2D p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxImageDimension2D}

        {-# INLINE writeVkMaxImageDimension2D #-}
        writeVkMaxImageDimension2D p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxImageDimension2D}

instance {-# OVERLAPPING #-}
         HasField "maxImageDimension2D" VkPhysicalDeviceLimits where
        type FieldType "maxImageDimension2D" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxImageDimension2D" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxImageDimension2D" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxImageDimension2D}
        type FieldIsArray "maxImageDimension2D" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxImageDimension2D}

instance CanReadField "maxImageDimension2D" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxImageDimension2D

        {-# INLINE readField #-}
        readField = readVkMaxImageDimension2D

instance CanWriteField "maxImageDimension2D" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxImageDimension2D

instance {-# OVERLAPPING #-}
         HasVkMaxImageDimension3D VkPhysicalDeviceLimits where
        type VkMaxImageDimension3DMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxImageDimension3D #-}
        vkMaxImageDimension3D x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxImageDimension3D})

        {-# INLINE vkMaxImageDimension3DByteOffset #-}
        vkMaxImageDimension3DByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxImageDimension3D}

        {-# INLINE readVkMaxImageDimension3D #-}
        readVkMaxImageDimension3D p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxImageDimension3D}

        {-# INLINE writeVkMaxImageDimension3D #-}
        writeVkMaxImageDimension3D p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxImageDimension3D}

instance {-# OVERLAPPING #-}
         HasField "maxImageDimension3D" VkPhysicalDeviceLimits where
        type FieldType "maxImageDimension3D" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxImageDimension3D" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxImageDimension3D" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxImageDimension3D}
        type FieldIsArray "maxImageDimension3D" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxImageDimension3D}

instance CanReadField "maxImageDimension3D" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxImageDimension3D

        {-# INLINE readField #-}
        readField = readVkMaxImageDimension3D

instance CanWriteField "maxImageDimension3D" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxImageDimension3D

instance {-# OVERLAPPING #-}
         HasVkMaxImageDimensionCube VkPhysicalDeviceLimits where
        type VkMaxImageDimensionCubeMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxImageDimensionCube #-}
        vkMaxImageDimensionCube x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxImageDimensionCube})

        {-# INLINE vkMaxImageDimensionCubeByteOffset #-}
        vkMaxImageDimensionCubeByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxImageDimensionCube}

        {-# INLINE readVkMaxImageDimensionCube #-}
        readVkMaxImageDimensionCube p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxImageDimensionCube}

        {-# INLINE writeVkMaxImageDimensionCube #-}
        writeVkMaxImageDimensionCube p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxImageDimensionCube}

instance {-# OVERLAPPING #-}
         HasField "maxImageDimensionCube" VkPhysicalDeviceLimits where
        type FieldType "maxImageDimensionCube" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxImageDimensionCube" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxImageDimensionCube" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxImageDimensionCube}
        type FieldIsArray "maxImageDimensionCube" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxImageDimensionCube}

instance CanReadField "maxImageDimensionCube"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxImageDimensionCube

        {-# INLINE readField #-}
        readField = readVkMaxImageDimensionCube

instance CanWriteField "maxImageDimensionCube"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxImageDimensionCube

instance {-# OVERLAPPING #-}
         HasVkMaxImageArrayLayers VkPhysicalDeviceLimits where
        type VkMaxImageArrayLayersMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxImageArrayLayers #-}
        vkMaxImageArrayLayers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxImageArrayLayers})

        {-# INLINE vkMaxImageArrayLayersByteOffset #-}
        vkMaxImageArrayLayersByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxImageArrayLayers}

        {-# INLINE readVkMaxImageArrayLayers #-}
        readVkMaxImageArrayLayers p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxImageArrayLayers}

        {-# INLINE writeVkMaxImageArrayLayers #-}
        writeVkMaxImageArrayLayers p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxImageArrayLayers}

instance {-# OVERLAPPING #-}
         HasField "maxImageArrayLayers" VkPhysicalDeviceLimits where
        type FieldType "maxImageArrayLayers" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxImageArrayLayers" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxImageArrayLayers" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxImageArrayLayers}
        type FieldIsArray "maxImageArrayLayers" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxImageArrayLayers}

instance CanReadField "maxImageArrayLayers" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxImageArrayLayers

        {-# INLINE readField #-}
        readField = readVkMaxImageArrayLayers

instance CanWriteField "maxImageArrayLayers" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxImageArrayLayers

instance {-# OVERLAPPING #-}
         HasVkMaxTexelBufferElements VkPhysicalDeviceLimits where
        type VkMaxTexelBufferElementsMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxTexelBufferElements #-}
        vkMaxTexelBufferElements x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTexelBufferElements})

        {-# INLINE vkMaxTexelBufferElementsByteOffset #-}
        vkMaxTexelBufferElementsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxTexelBufferElements}

        {-# INLINE readVkMaxTexelBufferElements #-}
        readVkMaxTexelBufferElements p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTexelBufferElements}

        {-# INLINE writeVkMaxTexelBufferElements #-}
        writeVkMaxTexelBufferElements p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTexelBufferElements}

instance {-# OVERLAPPING #-}
         HasField "maxTexelBufferElements" VkPhysicalDeviceLimits where
        type FieldType "maxTexelBufferElements" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxTexelBufferElements" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxTexelBufferElements" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxTexelBufferElements}
        type FieldIsArray "maxTexelBufferElements" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxTexelBufferElements}

instance CanReadField "maxTexelBufferElements"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxTexelBufferElements

        {-# INLINE readField #-}
        readField = readVkMaxTexelBufferElements

instance CanWriteField "maxTexelBufferElements"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxTexelBufferElements

instance {-# OVERLAPPING #-}
         HasVkMaxUniformBufferRange VkPhysicalDeviceLimits where
        type VkMaxUniformBufferRangeMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxUniformBufferRange #-}
        vkMaxUniformBufferRange x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxUniformBufferRange})

        {-# INLINE vkMaxUniformBufferRangeByteOffset #-}
        vkMaxUniformBufferRangeByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxUniformBufferRange}

        {-# INLINE readVkMaxUniformBufferRange #-}
        readVkMaxUniformBufferRange p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxUniformBufferRange}

        {-# INLINE writeVkMaxUniformBufferRange #-}
        writeVkMaxUniformBufferRange p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxUniformBufferRange}

instance {-# OVERLAPPING #-}
         HasField "maxUniformBufferRange" VkPhysicalDeviceLimits where
        type FieldType "maxUniformBufferRange" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxUniformBufferRange" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxUniformBufferRange" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxUniformBufferRange}
        type FieldIsArray "maxUniformBufferRange" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxUniformBufferRange}

instance CanReadField "maxUniformBufferRange"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxUniformBufferRange

        {-# INLINE readField #-}
        readField = readVkMaxUniformBufferRange

instance CanWriteField "maxUniformBufferRange"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxUniformBufferRange

instance {-# OVERLAPPING #-}
         HasVkMaxStorageBufferRange VkPhysicalDeviceLimits where
        type VkMaxStorageBufferRangeMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxStorageBufferRange #-}
        vkMaxStorageBufferRange x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxStorageBufferRange})

        {-# INLINE vkMaxStorageBufferRangeByteOffset #-}
        vkMaxStorageBufferRangeByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxStorageBufferRange}

        {-# INLINE readVkMaxStorageBufferRange #-}
        readVkMaxStorageBufferRange p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxStorageBufferRange}

        {-# INLINE writeVkMaxStorageBufferRange #-}
        writeVkMaxStorageBufferRange p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxStorageBufferRange}

instance {-# OVERLAPPING #-}
         HasField "maxStorageBufferRange" VkPhysicalDeviceLimits where
        type FieldType "maxStorageBufferRange" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxStorageBufferRange" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxStorageBufferRange" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxStorageBufferRange}
        type FieldIsArray "maxStorageBufferRange" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxStorageBufferRange}

instance CanReadField "maxStorageBufferRange"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxStorageBufferRange

        {-# INLINE readField #-}
        readField = readVkMaxStorageBufferRange

instance CanWriteField "maxStorageBufferRange"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxStorageBufferRange

instance {-# OVERLAPPING #-}
         HasVkMaxPushConstantsSize VkPhysicalDeviceLimits where
        type VkMaxPushConstantsSizeMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxPushConstantsSize #-}
        vkMaxPushConstantsSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxPushConstantsSize})

        {-# INLINE vkMaxPushConstantsSizeByteOffset #-}
        vkMaxPushConstantsSizeByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxPushConstantsSize}

        {-# INLINE readVkMaxPushConstantsSize #-}
        readVkMaxPushConstantsSize p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxPushConstantsSize}

        {-# INLINE writeVkMaxPushConstantsSize #-}
        writeVkMaxPushConstantsSize p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxPushConstantsSize}

instance {-# OVERLAPPING #-}
         HasField "maxPushConstantsSize" VkPhysicalDeviceLimits where
        type FieldType "maxPushConstantsSize" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxPushConstantsSize" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxPushConstantsSize" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxPushConstantsSize}
        type FieldIsArray "maxPushConstantsSize" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxPushConstantsSize}

instance CanReadField "maxPushConstantsSize" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxPushConstantsSize

        {-# INLINE readField #-}
        readField = readVkMaxPushConstantsSize

instance CanWriteField "maxPushConstantsSize"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxPushConstantsSize

instance {-# OVERLAPPING #-}
         HasVkMaxMemoryAllocationCount VkPhysicalDeviceLimits where
        type VkMaxMemoryAllocationCountMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxMemoryAllocationCount #-}
        vkMaxMemoryAllocationCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxMemoryAllocationCount})

        {-# INLINE vkMaxMemoryAllocationCountByteOffset #-}
        vkMaxMemoryAllocationCountByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxMemoryAllocationCount}

        {-# INLINE readVkMaxMemoryAllocationCount #-}
        readVkMaxMemoryAllocationCount p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxMemoryAllocationCount}

        {-# INLINE writeVkMaxMemoryAllocationCount #-}
        writeVkMaxMemoryAllocationCount p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxMemoryAllocationCount}

instance {-# OVERLAPPING #-}
         HasField "maxMemoryAllocationCount" VkPhysicalDeviceLimits where
        type FieldType "maxMemoryAllocationCount" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxMemoryAllocationCount"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxMemoryAllocationCount" VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxMemoryAllocationCount}
        type FieldIsArray "maxMemoryAllocationCount" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxMemoryAllocationCount}

instance CanReadField "maxMemoryAllocationCount"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxMemoryAllocationCount

        {-# INLINE readField #-}
        readField = readVkMaxMemoryAllocationCount

instance CanWriteField "maxMemoryAllocationCount"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxMemoryAllocationCount

instance {-# OVERLAPPING #-}
         HasVkMaxSamplerAllocationCount VkPhysicalDeviceLimits where
        type VkMaxSamplerAllocationCountMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxSamplerAllocationCount #-}
        vkMaxSamplerAllocationCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxSamplerAllocationCount})

        {-# INLINE vkMaxSamplerAllocationCountByteOffset #-}
        vkMaxSamplerAllocationCountByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxSamplerAllocationCount}

        {-# INLINE readVkMaxSamplerAllocationCount #-}
        readVkMaxSamplerAllocationCount p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxSamplerAllocationCount}

        {-# INLINE writeVkMaxSamplerAllocationCount #-}
        writeVkMaxSamplerAllocationCount p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxSamplerAllocationCount}

instance {-# OVERLAPPING #-}
         HasField "maxSamplerAllocationCount" VkPhysicalDeviceLimits where
        type FieldType "maxSamplerAllocationCount" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxSamplerAllocationCount"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSamplerAllocationCount" VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxSamplerAllocationCount}
        type FieldIsArray "maxSamplerAllocationCount"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxSamplerAllocationCount}

instance CanReadField "maxSamplerAllocationCount"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxSamplerAllocationCount

        {-# INLINE readField #-}
        readField = readVkMaxSamplerAllocationCount

instance CanWriteField "maxSamplerAllocationCount"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxSamplerAllocationCount

instance {-# OVERLAPPING #-}
         HasVkBufferImageGranularity VkPhysicalDeviceLimits where
        type VkBufferImageGranularityMType VkPhysicalDeviceLimits =
             VkDeviceSize

        {-# NOINLINE vkBufferImageGranularity #-}
        vkBufferImageGranularity x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, bufferImageGranularity})

        {-# INLINE vkBufferImageGranularityByteOffset #-}
        vkBufferImageGranularityByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, bufferImageGranularity}

        {-# INLINE readVkBufferImageGranularity #-}
        readVkBufferImageGranularity p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, bufferImageGranularity}

        {-# INLINE writeVkBufferImageGranularity #-}
        writeVkBufferImageGranularity p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, bufferImageGranularity}

instance {-# OVERLAPPING #-}
         HasField "bufferImageGranularity" VkPhysicalDeviceLimits where
        type FieldType "bufferImageGranularity" VkPhysicalDeviceLimits =
             VkDeviceSize
        type FieldOptional "bufferImageGranularity" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "bufferImageGranularity" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, bufferImageGranularity}
        type FieldIsArray "bufferImageGranularity" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, bufferImageGranularity}

instance CanReadField "bufferImageGranularity"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkBufferImageGranularity

        {-# INLINE readField #-}
        readField = readVkBufferImageGranularity

instance CanWriteField "bufferImageGranularity"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkBufferImageGranularity

instance {-# OVERLAPPING #-}
         HasVkSparseAddressSpaceSize VkPhysicalDeviceLimits where
        type VkSparseAddressSpaceSizeMType VkPhysicalDeviceLimits =
             VkDeviceSize

        {-# NOINLINE vkSparseAddressSpaceSize #-}
        vkSparseAddressSpaceSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, sparseAddressSpaceSize})

        {-# INLINE vkSparseAddressSpaceSizeByteOffset #-}
        vkSparseAddressSpaceSizeByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, sparseAddressSpaceSize}

        {-# INLINE readVkSparseAddressSpaceSize #-}
        readVkSparseAddressSpaceSize p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, sparseAddressSpaceSize}

        {-# INLINE writeVkSparseAddressSpaceSize #-}
        writeVkSparseAddressSpaceSize p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, sparseAddressSpaceSize}

instance {-# OVERLAPPING #-}
         HasField "sparseAddressSpaceSize" VkPhysicalDeviceLimits where
        type FieldType "sparseAddressSpaceSize" VkPhysicalDeviceLimits =
             VkDeviceSize
        type FieldOptional "sparseAddressSpaceSize" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sparseAddressSpaceSize" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, sparseAddressSpaceSize}
        type FieldIsArray "sparseAddressSpaceSize" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, sparseAddressSpaceSize}

instance CanReadField "sparseAddressSpaceSize"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkSparseAddressSpaceSize

        {-# INLINE readField #-}
        readField = readVkSparseAddressSpaceSize

instance CanWriteField "sparseAddressSpaceSize"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkSparseAddressSpaceSize

instance {-# OVERLAPPING #-}
         HasVkMaxBoundDescriptorSets VkPhysicalDeviceLimits where
        type VkMaxBoundDescriptorSetsMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxBoundDescriptorSets #-}
        vkMaxBoundDescriptorSets x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxBoundDescriptorSets})

        {-# INLINE vkMaxBoundDescriptorSetsByteOffset #-}
        vkMaxBoundDescriptorSetsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxBoundDescriptorSets}

        {-# INLINE readVkMaxBoundDescriptorSets #-}
        readVkMaxBoundDescriptorSets p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxBoundDescriptorSets}

        {-# INLINE writeVkMaxBoundDescriptorSets #-}
        writeVkMaxBoundDescriptorSets p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxBoundDescriptorSets}

instance {-# OVERLAPPING #-}
         HasField "maxBoundDescriptorSets" VkPhysicalDeviceLimits where
        type FieldType "maxBoundDescriptorSets" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxBoundDescriptorSets" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxBoundDescriptorSets" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxBoundDescriptorSets}
        type FieldIsArray "maxBoundDescriptorSets" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxBoundDescriptorSets}

instance CanReadField "maxBoundDescriptorSets"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxBoundDescriptorSets

        {-# INLINE readField #-}
        readField = readVkMaxBoundDescriptorSets

instance CanWriteField "maxBoundDescriptorSets"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxBoundDescriptorSets

instance {-# OVERLAPPING #-}
         HasVkMaxPerStageDescriptorSamplers VkPhysicalDeviceLimits where
        type VkMaxPerStageDescriptorSamplersMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxPerStageDescriptorSamplers #-}
        vkMaxPerStageDescriptorSamplers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSamplers})

        {-# INLINE vkMaxPerStageDescriptorSamplersByteOffset #-}
        vkMaxPerStageDescriptorSamplersByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSamplers}

        {-# INLINE readVkMaxPerStageDescriptorSamplers #-}
        readVkMaxPerStageDescriptorSamplers p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSamplers}

        {-# INLINE writeVkMaxPerStageDescriptorSamplers #-}
        writeVkMaxPerStageDescriptorSamplers p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSamplers}

instance {-# OVERLAPPING #-}
         HasField "maxPerStageDescriptorSamplers" VkPhysicalDeviceLimits
         where
        type FieldType "maxPerStageDescriptorSamplers"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxPerStageDescriptorSamplers"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxPerStageDescriptorSamplers"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSamplers}
        type FieldIsArray "maxPerStageDescriptorSamplers"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSamplers}

instance CanReadField "maxPerStageDescriptorSamplers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxPerStageDescriptorSamplers

        {-# INLINE readField #-}
        readField = readVkMaxPerStageDescriptorSamplers

instance CanWriteField "maxPerStageDescriptorSamplers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxPerStageDescriptorSamplers

instance {-# OVERLAPPING #-}
         HasVkMaxPerStageDescriptorUniformBuffers VkPhysicalDeviceLimits
         where
        type VkMaxPerStageDescriptorUniformBuffersMType
               VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxPerStageDescriptorUniformBuffers #-}
        vkMaxPerStageDescriptorUniformBuffers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorUniformBuffers})

        {-# INLINE vkMaxPerStageDescriptorUniformBuffersByteOffset #-}
        vkMaxPerStageDescriptorUniformBuffersByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorUniformBuffers}

        {-# INLINE readVkMaxPerStageDescriptorUniformBuffers #-}
        readVkMaxPerStageDescriptorUniformBuffers p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorUniformBuffers}

        {-# INLINE writeVkMaxPerStageDescriptorUniformBuffers #-}
        writeVkMaxPerStageDescriptorUniformBuffers p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorUniformBuffers}

instance {-# OVERLAPPING #-}
         HasField "maxPerStageDescriptorUniformBuffers"
           VkPhysicalDeviceLimits
         where
        type FieldType "maxPerStageDescriptorUniformBuffers"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxPerStageDescriptorUniformBuffers"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxPerStageDescriptorUniformBuffers"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorUniformBuffers}
        type FieldIsArray "maxPerStageDescriptorUniformBuffers"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorUniformBuffers}

instance CanReadField "maxPerStageDescriptorUniformBuffers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxPerStageDescriptorUniformBuffers

        {-# INLINE readField #-}
        readField = readVkMaxPerStageDescriptorUniformBuffers

instance CanWriteField "maxPerStageDescriptorUniformBuffers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxPerStageDescriptorUniformBuffers

instance {-# OVERLAPPING #-}
         HasVkMaxPerStageDescriptorStorageBuffers VkPhysicalDeviceLimits
         where
        type VkMaxPerStageDescriptorStorageBuffersMType
               VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxPerStageDescriptorStorageBuffers #-}
        vkMaxPerStageDescriptorStorageBuffers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageBuffers})

        {-# INLINE vkMaxPerStageDescriptorStorageBuffersByteOffset #-}
        vkMaxPerStageDescriptorStorageBuffersByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageBuffers}

        {-# INLINE readVkMaxPerStageDescriptorStorageBuffers #-}
        readVkMaxPerStageDescriptorStorageBuffers p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageBuffers}

        {-# INLINE writeVkMaxPerStageDescriptorStorageBuffers #-}
        writeVkMaxPerStageDescriptorStorageBuffers p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageBuffers}

instance {-# OVERLAPPING #-}
         HasField "maxPerStageDescriptorStorageBuffers"
           VkPhysicalDeviceLimits
         where
        type FieldType "maxPerStageDescriptorStorageBuffers"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxPerStageDescriptorStorageBuffers"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxPerStageDescriptorStorageBuffers"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageBuffers}
        type FieldIsArray "maxPerStageDescriptorStorageBuffers"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageBuffers}

instance CanReadField "maxPerStageDescriptorStorageBuffers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxPerStageDescriptorStorageBuffers

        {-# INLINE readField #-}
        readField = readVkMaxPerStageDescriptorStorageBuffers

instance CanWriteField "maxPerStageDescriptorStorageBuffers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxPerStageDescriptorStorageBuffers

instance {-# OVERLAPPING #-}
         HasVkMaxPerStageDescriptorSampledImages VkPhysicalDeviceLimits
         where
        type VkMaxPerStageDescriptorSampledImagesMType
               VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxPerStageDescriptorSampledImages #-}
        vkMaxPerStageDescriptorSampledImages x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSampledImages})

        {-# INLINE vkMaxPerStageDescriptorSampledImagesByteOffset #-}
        vkMaxPerStageDescriptorSampledImagesByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSampledImages}

        {-# INLINE readVkMaxPerStageDescriptorSampledImages #-}
        readVkMaxPerStageDescriptorSampledImages p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSampledImages}

        {-# INLINE writeVkMaxPerStageDescriptorSampledImages #-}
        writeVkMaxPerStageDescriptorSampledImages p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSampledImages}

instance {-# OVERLAPPING #-}
         HasField "maxPerStageDescriptorSampledImages"
           VkPhysicalDeviceLimits
         where
        type FieldType "maxPerStageDescriptorSampledImages"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxPerStageDescriptorSampledImages"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxPerStageDescriptorSampledImages"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSampledImages}
        type FieldIsArray "maxPerStageDescriptorSampledImages"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSampledImages}

instance CanReadField "maxPerStageDescriptorSampledImages"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxPerStageDescriptorSampledImages

        {-# INLINE readField #-}
        readField = readVkMaxPerStageDescriptorSampledImages

instance CanWriteField "maxPerStageDescriptorSampledImages"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxPerStageDescriptorSampledImages

instance {-# OVERLAPPING #-}
         HasVkMaxPerStageDescriptorStorageImages VkPhysicalDeviceLimits
         where
        type VkMaxPerStageDescriptorStorageImagesMType
               VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxPerStageDescriptorStorageImages #-}
        vkMaxPerStageDescriptorStorageImages x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageImages})

        {-# INLINE vkMaxPerStageDescriptorStorageImagesByteOffset #-}
        vkMaxPerStageDescriptorStorageImagesByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageImages}

        {-# INLINE readVkMaxPerStageDescriptorStorageImages #-}
        readVkMaxPerStageDescriptorStorageImages p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageImages}

        {-# INLINE writeVkMaxPerStageDescriptorStorageImages #-}
        writeVkMaxPerStageDescriptorStorageImages p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageImages}

instance {-# OVERLAPPING #-}
         HasField "maxPerStageDescriptorStorageImages"
           VkPhysicalDeviceLimits
         where
        type FieldType "maxPerStageDescriptorStorageImages"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxPerStageDescriptorStorageImages"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxPerStageDescriptorStorageImages"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageImages}
        type FieldIsArray "maxPerStageDescriptorStorageImages"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageImages}

instance CanReadField "maxPerStageDescriptorStorageImages"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxPerStageDescriptorStorageImages

        {-# INLINE readField #-}
        readField = readVkMaxPerStageDescriptorStorageImages

instance CanWriteField "maxPerStageDescriptorStorageImages"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxPerStageDescriptorStorageImages

instance {-# OVERLAPPING #-}
         HasVkMaxPerStageDescriptorInputAttachments VkPhysicalDeviceLimits
         where
        type VkMaxPerStageDescriptorInputAttachmentsMType
               VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxPerStageDescriptorInputAttachments #-}
        vkMaxPerStageDescriptorInputAttachments x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorInputAttachments})

        {-# INLINE vkMaxPerStageDescriptorInputAttachmentsByteOffset #-}
        vkMaxPerStageDescriptorInputAttachmentsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorInputAttachments}

        {-# INLINE readVkMaxPerStageDescriptorInputAttachments #-}
        readVkMaxPerStageDescriptorInputAttachments p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorInputAttachments}

        {-# INLINE writeVkMaxPerStageDescriptorInputAttachments #-}
        writeVkMaxPerStageDescriptorInputAttachments p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorInputAttachments}

instance {-# OVERLAPPING #-}
         HasField "maxPerStageDescriptorInputAttachments"
           VkPhysicalDeviceLimits
         where
        type FieldType "maxPerStageDescriptorInputAttachments"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxPerStageDescriptorInputAttachments"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxPerStageDescriptorInputAttachments"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorInputAttachments}
        type FieldIsArray "maxPerStageDescriptorInputAttachments"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorInputAttachments}

instance CanReadField "maxPerStageDescriptorInputAttachments"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxPerStageDescriptorInputAttachments

        {-# INLINE readField #-}
        readField = readVkMaxPerStageDescriptorInputAttachments

instance CanWriteField "maxPerStageDescriptorInputAttachments"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxPerStageDescriptorInputAttachments

instance {-# OVERLAPPING #-}
         HasVkMaxPerStageResources VkPhysicalDeviceLimits where
        type VkMaxPerStageResourcesMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxPerStageResources #-}
        vkMaxPerStageResources x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxPerStageResources})

        {-# INLINE vkMaxPerStageResourcesByteOffset #-}
        vkMaxPerStageResourcesByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxPerStageResources}

        {-# INLINE readVkMaxPerStageResources #-}
        readVkMaxPerStageResources p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageResources}

        {-# INLINE writeVkMaxPerStageResources #-}
        writeVkMaxPerStageResources p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageResources}

instance {-# OVERLAPPING #-}
         HasField "maxPerStageResources" VkPhysicalDeviceLimits where
        type FieldType "maxPerStageResources" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxPerStageResources" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxPerStageResources" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxPerStageResources}
        type FieldIsArray "maxPerStageResources" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxPerStageResources}

instance CanReadField "maxPerStageResources" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxPerStageResources

        {-# INLINE readField #-}
        readField = readVkMaxPerStageResources

instance CanWriteField "maxPerStageResources"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxPerStageResources

instance {-# OVERLAPPING #-}
         HasVkMaxDescriptorSetSamplers VkPhysicalDeviceLimits where
        type VkMaxDescriptorSetSamplersMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxDescriptorSetSamplers #-}
        vkMaxDescriptorSetSamplers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDescriptorSetSamplers})

        {-# INLINE vkMaxDescriptorSetSamplersByteOffset #-}
        vkMaxDescriptorSetSamplersByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxDescriptorSetSamplers}

        {-# INLINE readVkMaxDescriptorSetSamplers #-}
        readVkMaxDescriptorSetSamplers p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetSamplers}

        {-# INLINE writeVkMaxDescriptorSetSamplers #-}
        writeVkMaxDescriptorSetSamplers p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetSamplers}

instance {-# OVERLAPPING #-}
         HasField "maxDescriptorSetSamplers" VkPhysicalDeviceLimits where
        type FieldType "maxDescriptorSetSamplers" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxDescriptorSetSamplers"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDescriptorSetSamplers" VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxDescriptorSetSamplers}
        type FieldIsArray "maxDescriptorSetSamplers" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxDescriptorSetSamplers}

instance CanReadField "maxDescriptorSetSamplers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxDescriptorSetSamplers

        {-# INLINE readField #-}
        readField = readVkMaxDescriptorSetSamplers

instance CanWriteField "maxDescriptorSetSamplers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxDescriptorSetSamplers

instance {-# OVERLAPPING #-}
         HasVkMaxDescriptorSetUniformBuffers VkPhysicalDeviceLimits where
        type VkMaxDescriptorSetUniformBuffersMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxDescriptorSetUniformBuffers #-}
        vkMaxDescriptorSetUniformBuffers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffers})

        {-# INLINE vkMaxDescriptorSetUniformBuffersByteOffset #-}
        vkMaxDescriptorSetUniformBuffersByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffers}

        {-# INLINE readVkMaxDescriptorSetUniformBuffers #-}
        readVkMaxDescriptorSetUniformBuffers p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffers}

        {-# INLINE writeVkMaxDescriptorSetUniformBuffers #-}
        writeVkMaxDescriptorSetUniformBuffers p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffers}

instance {-# OVERLAPPING #-}
         HasField "maxDescriptorSetUniformBuffers" VkPhysicalDeviceLimits
         where
        type FieldType "maxDescriptorSetUniformBuffers"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxDescriptorSetUniformBuffers"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDescriptorSetUniformBuffers"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffers}
        type FieldIsArray "maxDescriptorSetUniformBuffers"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffers}

instance CanReadField "maxDescriptorSetUniformBuffers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxDescriptorSetUniformBuffers

        {-# INLINE readField #-}
        readField = readVkMaxDescriptorSetUniformBuffers

instance CanWriteField "maxDescriptorSetUniformBuffers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxDescriptorSetUniformBuffers

instance {-# OVERLAPPING #-}
         HasVkMaxDescriptorSetUniformBuffersDynamic VkPhysicalDeviceLimits
         where
        type VkMaxDescriptorSetUniformBuffersDynamicMType
               VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxDescriptorSetUniformBuffersDynamic #-}
        vkMaxDescriptorSetUniformBuffersDynamic x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffersDynamic})

        {-# INLINE vkMaxDescriptorSetUniformBuffersDynamicByteOffset #-}
        vkMaxDescriptorSetUniformBuffersDynamicByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffersDynamic}

        {-# INLINE readVkMaxDescriptorSetUniformBuffersDynamic #-}
        readVkMaxDescriptorSetUniformBuffersDynamic p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffersDynamic}

        {-# INLINE writeVkMaxDescriptorSetUniformBuffersDynamic #-}
        writeVkMaxDescriptorSetUniformBuffersDynamic p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffersDynamic}

instance {-# OVERLAPPING #-}
         HasField "maxDescriptorSetUniformBuffersDynamic"
           VkPhysicalDeviceLimits
         where
        type FieldType "maxDescriptorSetUniformBuffersDynamic"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxDescriptorSetUniformBuffersDynamic"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDescriptorSetUniformBuffersDynamic"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffersDynamic}
        type FieldIsArray "maxDescriptorSetUniformBuffersDynamic"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffersDynamic}

instance CanReadField "maxDescriptorSetUniformBuffersDynamic"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxDescriptorSetUniformBuffersDynamic

        {-# INLINE readField #-}
        readField = readVkMaxDescriptorSetUniformBuffersDynamic

instance CanWriteField "maxDescriptorSetUniformBuffersDynamic"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxDescriptorSetUniformBuffersDynamic

instance {-# OVERLAPPING #-}
         HasVkMaxDescriptorSetStorageBuffers VkPhysicalDeviceLimits where
        type VkMaxDescriptorSetStorageBuffersMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxDescriptorSetStorageBuffers #-}
        vkMaxDescriptorSetStorageBuffers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffers})

        {-# INLINE vkMaxDescriptorSetStorageBuffersByteOffset #-}
        vkMaxDescriptorSetStorageBuffersByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffers}

        {-# INLINE readVkMaxDescriptorSetStorageBuffers #-}
        readVkMaxDescriptorSetStorageBuffers p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffers}

        {-# INLINE writeVkMaxDescriptorSetStorageBuffers #-}
        writeVkMaxDescriptorSetStorageBuffers p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffers}

instance {-# OVERLAPPING #-}
         HasField "maxDescriptorSetStorageBuffers" VkPhysicalDeviceLimits
         where
        type FieldType "maxDescriptorSetStorageBuffers"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxDescriptorSetStorageBuffers"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDescriptorSetStorageBuffers"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffers}
        type FieldIsArray "maxDescriptorSetStorageBuffers"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffers}

instance CanReadField "maxDescriptorSetStorageBuffers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxDescriptorSetStorageBuffers

        {-# INLINE readField #-}
        readField = readVkMaxDescriptorSetStorageBuffers

instance CanWriteField "maxDescriptorSetStorageBuffers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxDescriptorSetStorageBuffers

instance {-# OVERLAPPING #-}
         HasVkMaxDescriptorSetStorageBuffersDynamic VkPhysicalDeviceLimits
         where
        type VkMaxDescriptorSetStorageBuffersDynamicMType
               VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxDescriptorSetStorageBuffersDynamic #-}
        vkMaxDescriptorSetStorageBuffersDynamic x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffersDynamic})

        {-# INLINE vkMaxDescriptorSetStorageBuffersDynamicByteOffset #-}
        vkMaxDescriptorSetStorageBuffersDynamicByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffersDynamic}

        {-# INLINE readVkMaxDescriptorSetStorageBuffersDynamic #-}
        readVkMaxDescriptorSetStorageBuffersDynamic p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffersDynamic}

        {-# INLINE writeVkMaxDescriptorSetStorageBuffersDynamic #-}
        writeVkMaxDescriptorSetStorageBuffersDynamic p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffersDynamic}

instance {-# OVERLAPPING #-}
         HasField "maxDescriptorSetStorageBuffersDynamic"
           VkPhysicalDeviceLimits
         where
        type FieldType "maxDescriptorSetStorageBuffersDynamic"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxDescriptorSetStorageBuffersDynamic"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDescriptorSetStorageBuffersDynamic"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffersDynamic}
        type FieldIsArray "maxDescriptorSetStorageBuffersDynamic"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffersDynamic}

instance CanReadField "maxDescriptorSetStorageBuffersDynamic"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxDescriptorSetStorageBuffersDynamic

        {-# INLINE readField #-}
        readField = readVkMaxDescriptorSetStorageBuffersDynamic

instance CanWriteField "maxDescriptorSetStorageBuffersDynamic"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxDescriptorSetStorageBuffersDynamic

instance {-# OVERLAPPING #-}
         HasVkMaxDescriptorSetSampledImages VkPhysicalDeviceLimits where
        type VkMaxDescriptorSetSampledImagesMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxDescriptorSetSampledImages #-}
        vkMaxDescriptorSetSampledImages x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDescriptorSetSampledImages})

        {-# INLINE vkMaxDescriptorSetSampledImagesByteOffset #-}
        vkMaxDescriptorSetSampledImagesByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxDescriptorSetSampledImages}

        {-# INLINE readVkMaxDescriptorSetSampledImages #-}
        readVkMaxDescriptorSetSampledImages p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetSampledImages}

        {-# INLINE writeVkMaxDescriptorSetSampledImages #-}
        writeVkMaxDescriptorSetSampledImages p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetSampledImages}

instance {-# OVERLAPPING #-}
         HasField "maxDescriptorSetSampledImages" VkPhysicalDeviceLimits
         where
        type FieldType "maxDescriptorSetSampledImages"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxDescriptorSetSampledImages"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDescriptorSetSampledImages"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxDescriptorSetSampledImages}
        type FieldIsArray "maxDescriptorSetSampledImages"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxDescriptorSetSampledImages}

instance CanReadField "maxDescriptorSetSampledImages"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxDescriptorSetSampledImages

        {-# INLINE readField #-}
        readField = readVkMaxDescriptorSetSampledImages

instance CanWriteField "maxDescriptorSetSampledImages"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxDescriptorSetSampledImages

instance {-# OVERLAPPING #-}
         HasVkMaxDescriptorSetStorageImages VkPhysicalDeviceLimits where
        type VkMaxDescriptorSetStorageImagesMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxDescriptorSetStorageImages #-}
        vkMaxDescriptorSetStorageImages x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageImages})

        {-# INLINE vkMaxDescriptorSetStorageImagesByteOffset #-}
        vkMaxDescriptorSetStorageImagesByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageImages}

        {-# INLINE readVkMaxDescriptorSetStorageImages #-}
        readVkMaxDescriptorSetStorageImages p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageImages}

        {-# INLINE writeVkMaxDescriptorSetStorageImages #-}
        writeVkMaxDescriptorSetStorageImages p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageImages}

instance {-# OVERLAPPING #-}
         HasField "maxDescriptorSetStorageImages" VkPhysicalDeviceLimits
         where
        type FieldType "maxDescriptorSetStorageImages"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxDescriptorSetStorageImages"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDescriptorSetStorageImages"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageImages}
        type FieldIsArray "maxDescriptorSetStorageImages"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageImages}

instance CanReadField "maxDescriptorSetStorageImages"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxDescriptorSetStorageImages

        {-# INLINE readField #-}
        readField = readVkMaxDescriptorSetStorageImages

instance CanWriteField "maxDescriptorSetStorageImages"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxDescriptorSetStorageImages

instance {-# OVERLAPPING #-}
         HasVkMaxDescriptorSetInputAttachments VkPhysicalDeviceLimits where
        type VkMaxDescriptorSetInputAttachmentsMType VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxDescriptorSetInputAttachments #-}
        vkMaxDescriptorSetInputAttachments x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDescriptorSetInputAttachments})

        {-# INLINE vkMaxDescriptorSetInputAttachmentsByteOffset #-}
        vkMaxDescriptorSetInputAttachmentsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxDescriptorSetInputAttachments}

        {-# INLINE readVkMaxDescriptorSetInputAttachments #-}
        readVkMaxDescriptorSetInputAttachments p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetInputAttachments}

        {-# INLINE writeVkMaxDescriptorSetInputAttachments #-}
        writeVkMaxDescriptorSetInputAttachments p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetInputAttachments}

instance {-# OVERLAPPING #-}
         HasField "maxDescriptorSetInputAttachments" VkPhysicalDeviceLimits
         where
        type FieldType "maxDescriptorSetInputAttachments"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxDescriptorSetInputAttachments"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDescriptorSetInputAttachments"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxDescriptorSetInputAttachments}
        type FieldIsArray "maxDescriptorSetInputAttachments"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxDescriptorSetInputAttachments}

instance CanReadField "maxDescriptorSetInputAttachments"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxDescriptorSetInputAttachments

        {-# INLINE readField #-}
        readField = readVkMaxDescriptorSetInputAttachments

instance CanWriteField "maxDescriptorSetInputAttachments"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxDescriptorSetInputAttachments

instance {-# OVERLAPPING #-}
         HasVkMaxVertexInputAttributes VkPhysicalDeviceLimits where
        type VkMaxVertexInputAttributesMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxVertexInputAttributes #-}
        vkMaxVertexInputAttributes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxVertexInputAttributes})

        {-# INLINE vkMaxVertexInputAttributesByteOffset #-}
        vkMaxVertexInputAttributesByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxVertexInputAttributes}

        {-# INLINE readVkMaxVertexInputAttributes #-}
        readVkMaxVertexInputAttributes p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxVertexInputAttributes}

        {-# INLINE writeVkMaxVertexInputAttributes #-}
        writeVkMaxVertexInputAttributes p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxVertexInputAttributes}

instance {-# OVERLAPPING #-}
         HasField "maxVertexInputAttributes" VkPhysicalDeviceLimits where
        type FieldType "maxVertexInputAttributes" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxVertexInputAttributes"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxVertexInputAttributes" VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxVertexInputAttributes}
        type FieldIsArray "maxVertexInputAttributes" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxVertexInputAttributes}

instance CanReadField "maxVertexInputAttributes"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxVertexInputAttributes

        {-# INLINE readField #-}
        readField = readVkMaxVertexInputAttributes

instance CanWriteField "maxVertexInputAttributes"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxVertexInputAttributes

instance {-# OVERLAPPING #-}
         HasVkMaxVertexInputBindings VkPhysicalDeviceLimits where
        type VkMaxVertexInputBindingsMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxVertexInputBindings #-}
        vkMaxVertexInputBindings x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxVertexInputBindings})

        {-# INLINE vkMaxVertexInputBindingsByteOffset #-}
        vkMaxVertexInputBindingsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxVertexInputBindings}

        {-# INLINE readVkMaxVertexInputBindings #-}
        readVkMaxVertexInputBindings p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxVertexInputBindings}

        {-# INLINE writeVkMaxVertexInputBindings #-}
        writeVkMaxVertexInputBindings p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxVertexInputBindings}

instance {-# OVERLAPPING #-}
         HasField "maxVertexInputBindings" VkPhysicalDeviceLimits where
        type FieldType "maxVertexInputBindings" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxVertexInputBindings" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxVertexInputBindings" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxVertexInputBindings}
        type FieldIsArray "maxVertexInputBindings" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxVertexInputBindings}

instance CanReadField "maxVertexInputBindings"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxVertexInputBindings

        {-# INLINE readField #-}
        readField = readVkMaxVertexInputBindings

instance CanWriteField "maxVertexInputBindings"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxVertexInputBindings

instance {-# OVERLAPPING #-}
         HasVkMaxVertexInputAttributeOffset VkPhysicalDeviceLimits where
        type VkMaxVertexInputAttributeOffsetMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxVertexInputAttributeOffset #-}
        vkMaxVertexInputAttributeOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxVertexInputAttributeOffset})

        {-# INLINE vkMaxVertexInputAttributeOffsetByteOffset #-}
        vkMaxVertexInputAttributeOffsetByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxVertexInputAttributeOffset}

        {-# INLINE readVkMaxVertexInputAttributeOffset #-}
        readVkMaxVertexInputAttributeOffset p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxVertexInputAttributeOffset}

        {-# INLINE writeVkMaxVertexInputAttributeOffset #-}
        writeVkMaxVertexInputAttributeOffset p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxVertexInputAttributeOffset}

instance {-# OVERLAPPING #-}
         HasField "maxVertexInputAttributeOffset" VkPhysicalDeviceLimits
         where
        type FieldType "maxVertexInputAttributeOffset"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxVertexInputAttributeOffset"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxVertexInputAttributeOffset"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxVertexInputAttributeOffset}
        type FieldIsArray "maxVertexInputAttributeOffset"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxVertexInputAttributeOffset}

instance CanReadField "maxVertexInputAttributeOffset"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxVertexInputAttributeOffset

        {-# INLINE readField #-}
        readField = readVkMaxVertexInputAttributeOffset

instance CanWriteField "maxVertexInputAttributeOffset"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxVertexInputAttributeOffset

instance {-# OVERLAPPING #-}
         HasVkMaxVertexInputBindingStride VkPhysicalDeviceLimits where
        type VkMaxVertexInputBindingStrideMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxVertexInputBindingStride #-}
        vkMaxVertexInputBindingStride x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxVertexInputBindingStride})

        {-# INLINE vkMaxVertexInputBindingStrideByteOffset #-}
        vkMaxVertexInputBindingStrideByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxVertexInputBindingStride}

        {-# INLINE readVkMaxVertexInputBindingStride #-}
        readVkMaxVertexInputBindingStride p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxVertexInputBindingStride}

        {-# INLINE writeVkMaxVertexInputBindingStride #-}
        writeVkMaxVertexInputBindingStride p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxVertexInputBindingStride}

instance {-# OVERLAPPING #-}
         HasField "maxVertexInputBindingStride" VkPhysicalDeviceLimits where
        type FieldType "maxVertexInputBindingStride" VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxVertexInputBindingStride"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxVertexInputBindingStride"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxVertexInputBindingStride}
        type FieldIsArray "maxVertexInputBindingStride"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxVertexInputBindingStride}

instance CanReadField "maxVertexInputBindingStride"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxVertexInputBindingStride

        {-# INLINE readField #-}
        readField = readVkMaxVertexInputBindingStride

instance CanWriteField "maxVertexInputBindingStride"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxVertexInputBindingStride

instance {-# OVERLAPPING #-}
         HasVkMaxVertexOutputComponents VkPhysicalDeviceLimits where
        type VkMaxVertexOutputComponentsMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxVertexOutputComponents #-}
        vkMaxVertexOutputComponents x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxVertexOutputComponents})

        {-# INLINE vkMaxVertexOutputComponentsByteOffset #-}
        vkMaxVertexOutputComponentsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxVertexOutputComponents}

        {-# INLINE readVkMaxVertexOutputComponents #-}
        readVkMaxVertexOutputComponents p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxVertexOutputComponents}

        {-# INLINE writeVkMaxVertexOutputComponents #-}
        writeVkMaxVertexOutputComponents p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxVertexOutputComponents}

instance {-# OVERLAPPING #-}
         HasField "maxVertexOutputComponents" VkPhysicalDeviceLimits where
        type FieldType "maxVertexOutputComponents" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxVertexOutputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxVertexOutputComponents" VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxVertexOutputComponents}
        type FieldIsArray "maxVertexOutputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxVertexOutputComponents}

instance CanReadField "maxVertexOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxVertexOutputComponents

        {-# INLINE readField #-}
        readField = readVkMaxVertexOutputComponents

instance CanWriteField "maxVertexOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxVertexOutputComponents

instance {-# OVERLAPPING #-}
         HasVkMaxTessellationGenerationLevel VkPhysicalDeviceLimits where
        type VkMaxTessellationGenerationLevelMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxTessellationGenerationLevel #-}
        vkMaxTessellationGenerationLevel x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTessellationGenerationLevel})

        {-# INLINE vkMaxTessellationGenerationLevelByteOffset #-}
        vkMaxTessellationGenerationLevelByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxTessellationGenerationLevel}

        {-# INLINE readVkMaxTessellationGenerationLevel #-}
        readVkMaxTessellationGenerationLevel p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationGenerationLevel}

        {-# INLINE writeVkMaxTessellationGenerationLevel #-}
        writeVkMaxTessellationGenerationLevel p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationGenerationLevel}

instance {-# OVERLAPPING #-}
         HasField "maxTessellationGenerationLevel" VkPhysicalDeviceLimits
         where
        type FieldType "maxTessellationGenerationLevel"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxTessellationGenerationLevel"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxTessellationGenerationLevel"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxTessellationGenerationLevel}
        type FieldIsArray "maxTessellationGenerationLevel"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxTessellationGenerationLevel}

instance CanReadField "maxTessellationGenerationLevel"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxTessellationGenerationLevel

        {-# INLINE readField #-}
        readField = readVkMaxTessellationGenerationLevel

instance CanWriteField "maxTessellationGenerationLevel"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxTessellationGenerationLevel

instance {-# OVERLAPPING #-}
         HasVkMaxTessellationPatchSize VkPhysicalDeviceLimits where
        type VkMaxTessellationPatchSizeMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxTessellationPatchSize #-}
        vkMaxTessellationPatchSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTessellationPatchSize})

        {-# INLINE vkMaxTessellationPatchSizeByteOffset #-}
        vkMaxTessellationPatchSizeByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxTessellationPatchSize}

        {-# INLINE readVkMaxTessellationPatchSize #-}
        readVkMaxTessellationPatchSize p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationPatchSize}

        {-# INLINE writeVkMaxTessellationPatchSize #-}
        writeVkMaxTessellationPatchSize p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationPatchSize}

instance {-# OVERLAPPING #-}
         HasField "maxTessellationPatchSize" VkPhysicalDeviceLimits where
        type FieldType "maxTessellationPatchSize" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxTessellationPatchSize"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxTessellationPatchSize" VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxTessellationPatchSize}
        type FieldIsArray "maxTessellationPatchSize" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxTessellationPatchSize}

instance CanReadField "maxTessellationPatchSize"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxTessellationPatchSize

        {-# INLINE readField #-}
        readField = readVkMaxTessellationPatchSize

instance CanWriteField "maxTessellationPatchSize"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxTessellationPatchSize

instance {-# OVERLAPPING #-}
         HasVkMaxTessellationControlPerVertexInputComponents
           VkPhysicalDeviceLimits
         where
        type VkMaxTessellationControlPerVertexInputComponentsMType
               VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxTessellationControlPerVertexInputComponents #-}
        vkMaxTessellationControlPerVertexInputComponents x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexInputComponents})

        {-# INLINE vkMaxTessellationControlPerVertexInputComponentsByteOffset
                   #-}
        vkMaxTessellationControlPerVertexInputComponentsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexInputComponents}

        {-# INLINE readVkMaxTessellationControlPerVertexInputComponents #-}
        readVkMaxTessellationControlPerVertexInputComponents p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexInputComponents}

        {-# INLINE writeVkMaxTessellationControlPerVertexInputComponents
                   #-}
        writeVkMaxTessellationControlPerVertexInputComponents p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexInputComponents}

instance {-# OVERLAPPING #-}
         HasField "maxTessellationControlPerVertexInputComponents"
           VkPhysicalDeviceLimits
         where
        type FieldType "maxTessellationControlPerVertexInputComponents"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxTessellationControlPerVertexInputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxTessellationControlPerVertexInputComponents"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexInputComponents}
        type FieldIsArray "maxTessellationControlPerVertexInputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexInputComponents}

instance CanReadField
           "maxTessellationControlPerVertexInputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxTessellationControlPerVertexInputComponents

        {-# INLINE readField #-}
        readField = readVkMaxTessellationControlPerVertexInputComponents

instance CanWriteField
           "maxTessellationControlPerVertexInputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxTessellationControlPerVertexInputComponents

instance {-# OVERLAPPING #-}
         HasVkMaxTessellationControlPerVertexOutputComponents
           VkPhysicalDeviceLimits
         where
        type VkMaxTessellationControlPerVertexOutputComponentsMType
               VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxTessellationControlPerVertexOutputComponents #-}
        vkMaxTessellationControlPerVertexOutputComponents x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexOutputComponents})

        {-# INLINE vkMaxTessellationControlPerVertexOutputComponentsByteOffset
                   #-}
        vkMaxTessellationControlPerVertexOutputComponentsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexOutputComponents}

        {-# INLINE readVkMaxTessellationControlPerVertexOutputComponents
                   #-}
        readVkMaxTessellationControlPerVertexOutputComponents p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexOutputComponents}

        {-# INLINE writeVkMaxTessellationControlPerVertexOutputComponents
                   #-}
        writeVkMaxTessellationControlPerVertexOutputComponents p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexOutputComponents}

instance {-# OVERLAPPING #-}
         HasField "maxTessellationControlPerVertexOutputComponents"
           VkPhysicalDeviceLimits
         where
        type FieldType "maxTessellationControlPerVertexOutputComponents"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional
               "maxTessellationControlPerVertexOutputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxTessellationControlPerVertexOutputComponents"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexOutputComponents}
        type FieldIsArray "maxTessellationControlPerVertexOutputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexOutputComponents}

instance CanReadField
           "maxTessellationControlPerVertexOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxTessellationControlPerVertexOutputComponents

        {-# INLINE readField #-}
        readField = readVkMaxTessellationControlPerVertexOutputComponents

instance CanWriteField
           "maxTessellationControlPerVertexOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxTessellationControlPerVertexOutputComponents

instance {-# OVERLAPPING #-}
         HasVkMaxTessellationControlPerPatchOutputComponents
           VkPhysicalDeviceLimits
         where
        type VkMaxTessellationControlPerPatchOutputComponentsMType
               VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxTessellationControlPerPatchOutputComponents #-}
        vkMaxTessellationControlPerPatchOutputComponents x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTessellationControlPerPatchOutputComponents})

        {-# INLINE vkMaxTessellationControlPerPatchOutputComponentsByteOffset
                   #-}
        vkMaxTessellationControlPerPatchOutputComponentsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxTessellationControlPerPatchOutputComponents}

        {-# INLINE readVkMaxTessellationControlPerPatchOutputComponents #-}
        readVkMaxTessellationControlPerPatchOutputComponents p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationControlPerPatchOutputComponents}

        {-# INLINE writeVkMaxTessellationControlPerPatchOutputComponents
                   #-}
        writeVkMaxTessellationControlPerPatchOutputComponents p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationControlPerPatchOutputComponents}

instance {-# OVERLAPPING #-}
         HasField "maxTessellationControlPerPatchOutputComponents"
           VkPhysicalDeviceLimits
         where
        type FieldType "maxTessellationControlPerPatchOutputComponents"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxTessellationControlPerPatchOutputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxTessellationControlPerPatchOutputComponents"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxTessellationControlPerPatchOutputComponents}
        type FieldIsArray "maxTessellationControlPerPatchOutputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxTessellationControlPerPatchOutputComponents}

instance CanReadField
           "maxTessellationControlPerPatchOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxTessellationControlPerPatchOutputComponents

        {-# INLINE readField #-}
        readField = readVkMaxTessellationControlPerPatchOutputComponents

instance CanWriteField
           "maxTessellationControlPerPatchOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxTessellationControlPerPatchOutputComponents

instance {-# OVERLAPPING #-}
         HasVkMaxTessellationControlTotalOutputComponents
           VkPhysicalDeviceLimits
         where
        type VkMaxTessellationControlTotalOutputComponentsMType
               VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxTessellationControlTotalOutputComponents #-}
        vkMaxTessellationControlTotalOutputComponents x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTessellationControlTotalOutputComponents})

        {-# INLINE vkMaxTessellationControlTotalOutputComponentsByteOffset
                   #-}
        vkMaxTessellationControlTotalOutputComponentsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxTessellationControlTotalOutputComponents}

        {-# INLINE readVkMaxTessellationControlTotalOutputComponents #-}
        readVkMaxTessellationControlTotalOutputComponents p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationControlTotalOutputComponents}

        {-# INLINE writeVkMaxTessellationControlTotalOutputComponents #-}
        writeVkMaxTessellationControlTotalOutputComponents p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationControlTotalOutputComponents}

instance {-# OVERLAPPING #-}
         HasField "maxTessellationControlTotalOutputComponents"
           VkPhysicalDeviceLimits
         where
        type FieldType "maxTessellationControlTotalOutputComponents"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxTessellationControlTotalOutputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxTessellationControlTotalOutputComponents"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxTessellationControlTotalOutputComponents}
        type FieldIsArray "maxTessellationControlTotalOutputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxTessellationControlTotalOutputComponents}

instance CanReadField "maxTessellationControlTotalOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxTessellationControlTotalOutputComponents

        {-# INLINE readField #-}
        readField = readVkMaxTessellationControlTotalOutputComponents

instance CanWriteField
           "maxTessellationControlTotalOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxTessellationControlTotalOutputComponents

instance {-# OVERLAPPING #-}
         HasVkMaxTessellationEvaluationInputComponents
           VkPhysicalDeviceLimits
         where
        type VkMaxTessellationEvaluationInputComponentsMType
               VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxTessellationEvaluationInputComponents #-}
        vkMaxTessellationEvaluationInputComponents x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationInputComponents})

        {-# INLINE vkMaxTessellationEvaluationInputComponentsByteOffset #-}
        vkMaxTessellationEvaluationInputComponentsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationInputComponents}

        {-# INLINE readVkMaxTessellationEvaluationInputComponents #-}
        readVkMaxTessellationEvaluationInputComponents p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationInputComponents}

        {-# INLINE writeVkMaxTessellationEvaluationInputComponents #-}
        writeVkMaxTessellationEvaluationInputComponents p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationInputComponents}

instance {-# OVERLAPPING #-}
         HasField "maxTessellationEvaluationInputComponents"
           VkPhysicalDeviceLimits
         where
        type FieldType "maxTessellationEvaluationInputComponents"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxTessellationEvaluationInputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxTessellationEvaluationInputComponents"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationInputComponents}
        type FieldIsArray "maxTessellationEvaluationInputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationInputComponents}

instance CanReadField "maxTessellationEvaluationInputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxTessellationEvaluationInputComponents

        {-# INLINE readField #-}
        readField = readVkMaxTessellationEvaluationInputComponents

instance CanWriteField "maxTessellationEvaluationInputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxTessellationEvaluationInputComponents

instance {-# OVERLAPPING #-}
         HasVkMaxTessellationEvaluationOutputComponents
           VkPhysicalDeviceLimits
         where
        type VkMaxTessellationEvaluationOutputComponentsMType
               VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxTessellationEvaluationOutputComponents #-}
        vkMaxTessellationEvaluationOutputComponents x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationOutputComponents})

        {-# INLINE vkMaxTessellationEvaluationOutputComponentsByteOffset
                   #-}
        vkMaxTessellationEvaluationOutputComponentsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationOutputComponents}

        {-# INLINE readVkMaxTessellationEvaluationOutputComponents #-}
        readVkMaxTessellationEvaluationOutputComponents p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationOutputComponents}

        {-# INLINE writeVkMaxTessellationEvaluationOutputComponents #-}
        writeVkMaxTessellationEvaluationOutputComponents p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationOutputComponents}

instance {-# OVERLAPPING #-}
         HasField "maxTessellationEvaluationOutputComponents"
           VkPhysicalDeviceLimits
         where
        type FieldType "maxTessellationEvaluationOutputComponents"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxTessellationEvaluationOutputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxTessellationEvaluationOutputComponents"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationOutputComponents}
        type FieldIsArray "maxTessellationEvaluationOutputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationOutputComponents}

instance CanReadField "maxTessellationEvaluationOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxTessellationEvaluationOutputComponents

        {-# INLINE readField #-}
        readField = readVkMaxTessellationEvaluationOutputComponents

instance CanWriteField "maxTessellationEvaluationOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxTessellationEvaluationOutputComponents

instance {-# OVERLAPPING #-}
         HasVkMaxGeometryShaderInvocations VkPhysicalDeviceLimits where
        type VkMaxGeometryShaderInvocationsMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxGeometryShaderInvocations #-}
        vkMaxGeometryShaderInvocations x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxGeometryShaderInvocations})

        {-# INLINE vkMaxGeometryShaderInvocationsByteOffset #-}
        vkMaxGeometryShaderInvocationsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxGeometryShaderInvocations}

        {-# INLINE readVkMaxGeometryShaderInvocations #-}
        readVkMaxGeometryShaderInvocations p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryShaderInvocations}

        {-# INLINE writeVkMaxGeometryShaderInvocations #-}
        writeVkMaxGeometryShaderInvocations p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryShaderInvocations}

instance {-# OVERLAPPING #-}
         HasField "maxGeometryShaderInvocations" VkPhysicalDeviceLimits
         where
        type FieldType "maxGeometryShaderInvocations"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxGeometryShaderInvocations"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxGeometryShaderInvocations"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxGeometryShaderInvocations}
        type FieldIsArray "maxGeometryShaderInvocations"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxGeometryShaderInvocations}

instance CanReadField "maxGeometryShaderInvocations"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxGeometryShaderInvocations

        {-# INLINE readField #-}
        readField = readVkMaxGeometryShaderInvocations

instance CanWriteField "maxGeometryShaderInvocations"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxGeometryShaderInvocations

instance {-# OVERLAPPING #-}
         HasVkMaxGeometryInputComponents VkPhysicalDeviceLimits where
        type VkMaxGeometryInputComponentsMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxGeometryInputComponents #-}
        vkMaxGeometryInputComponents x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxGeometryInputComponents})

        {-# INLINE vkMaxGeometryInputComponentsByteOffset #-}
        vkMaxGeometryInputComponentsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxGeometryInputComponents}

        {-# INLINE readVkMaxGeometryInputComponents #-}
        readVkMaxGeometryInputComponents p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryInputComponents}

        {-# INLINE writeVkMaxGeometryInputComponents #-}
        writeVkMaxGeometryInputComponents p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryInputComponents}

instance {-# OVERLAPPING #-}
         HasField "maxGeometryInputComponents" VkPhysicalDeviceLimits where
        type FieldType "maxGeometryInputComponents" VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxGeometryInputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxGeometryInputComponents"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxGeometryInputComponents}
        type FieldIsArray "maxGeometryInputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxGeometryInputComponents}

instance CanReadField "maxGeometryInputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxGeometryInputComponents

        {-# INLINE readField #-}
        readField = readVkMaxGeometryInputComponents

instance CanWriteField "maxGeometryInputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxGeometryInputComponents

instance {-# OVERLAPPING #-}
         HasVkMaxGeometryOutputComponents VkPhysicalDeviceLimits where
        type VkMaxGeometryOutputComponentsMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxGeometryOutputComponents #-}
        vkMaxGeometryOutputComponents x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxGeometryOutputComponents})

        {-# INLINE vkMaxGeometryOutputComponentsByteOffset #-}
        vkMaxGeometryOutputComponentsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxGeometryOutputComponents}

        {-# INLINE readVkMaxGeometryOutputComponents #-}
        readVkMaxGeometryOutputComponents p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryOutputComponents}

        {-# INLINE writeVkMaxGeometryOutputComponents #-}
        writeVkMaxGeometryOutputComponents p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryOutputComponents}

instance {-# OVERLAPPING #-}
         HasField "maxGeometryOutputComponents" VkPhysicalDeviceLimits where
        type FieldType "maxGeometryOutputComponents" VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxGeometryOutputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxGeometryOutputComponents"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxGeometryOutputComponents}
        type FieldIsArray "maxGeometryOutputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxGeometryOutputComponents}

instance CanReadField "maxGeometryOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxGeometryOutputComponents

        {-# INLINE readField #-}
        readField = readVkMaxGeometryOutputComponents

instance CanWriteField "maxGeometryOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxGeometryOutputComponents

instance {-# OVERLAPPING #-}
         HasVkMaxGeometryOutputVertices VkPhysicalDeviceLimits where
        type VkMaxGeometryOutputVerticesMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxGeometryOutputVertices #-}
        vkMaxGeometryOutputVertices x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxGeometryOutputVertices})

        {-# INLINE vkMaxGeometryOutputVerticesByteOffset #-}
        vkMaxGeometryOutputVerticesByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxGeometryOutputVertices}

        {-# INLINE readVkMaxGeometryOutputVertices #-}
        readVkMaxGeometryOutputVertices p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryOutputVertices}

        {-# INLINE writeVkMaxGeometryOutputVertices #-}
        writeVkMaxGeometryOutputVertices p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryOutputVertices}

instance {-# OVERLAPPING #-}
         HasField "maxGeometryOutputVertices" VkPhysicalDeviceLimits where
        type FieldType "maxGeometryOutputVertices" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxGeometryOutputVertices"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxGeometryOutputVertices" VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxGeometryOutputVertices}
        type FieldIsArray "maxGeometryOutputVertices"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxGeometryOutputVertices}

instance CanReadField "maxGeometryOutputVertices"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxGeometryOutputVertices

        {-# INLINE readField #-}
        readField = readVkMaxGeometryOutputVertices

instance CanWriteField "maxGeometryOutputVertices"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxGeometryOutputVertices

instance {-# OVERLAPPING #-}
         HasVkMaxGeometryTotalOutputComponents VkPhysicalDeviceLimits where
        type VkMaxGeometryTotalOutputComponentsMType VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxGeometryTotalOutputComponents #-}
        vkMaxGeometryTotalOutputComponents x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxGeometryTotalOutputComponents})

        {-# INLINE vkMaxGeometryTotalOutputComponentsByteOffset #-}
        vkMaxGeometryTotalOutputComponentsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxGeometryTotalOutputComponents}

        {-# INLINE readVkMaxGeometryTotalOutputComponents #-}
        readVkMaxGeometryTotalOutputComponents p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryTotalOutputComponents}

        {-# INLINE writeVkMaxGeometryTotalOutputComponents #-}
        writeVkMaxGeometryTotalOutputComponents p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryTotalOutputComponents}

instance {-# OVERLAPPING #-}
         HasField "maxGeometryTotalOutputComponents" VkPhysicalDeviceLimits
         where
        type FieldType "maxGeometryTotalOutputComponents"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxGeometryTotalOutputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxGeometryTotalOutputComponents"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxGeometryTotalOutputComponents}
        type FieldIsArray "maxGeometryTotalOutputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxGeometryTotalOutputComponents}

instance CanReadField "maxGeometryTotalOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxGeometryTotalOutputComponents

        {-# INLINE readField #-}
        readField = readVkMaxGeometryTotalOutputComponents

instance CanWriteField "maxGeometryTotalOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxGeometryTotalOutputComponents

instance {-# OVERLAPPING #-}
         HasVkMaxFragmentInputComponents VkPhysicalDeviceLimits where
        type VkMaxFragmentInputComponentsMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxFragmentInputComponents #-}
        vkMaxFragmentInputComponents x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxFragmentInputComponents})

        {-# INLINE vkMaxFragmentInputComponentsByteOffset #-}
        vkMaxFragmentInputComponentsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxFragmentInputComponents}

        {-# INLINE readVkMaxFragmentInputComponents #-}
        readVkMaxFragmentInputComponents p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxFragmentInputComponents}

        {-# INLINE writeVkMaxFragmentInputComponents #-}
        writeVkMaxFragmentInputComponents p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxFragmentInputComponents}

instance {-# OVERLAPPING #-}
         HasField "maxFragmentInputComponents" VkPhysicalDeviceLimits where
        type FieldType "maxFragmentInputComponents" VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxFragmentInputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxFragmentInputComponents"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxFragmentInputComponents}
        type FieldIsArray "maxFragmentInputComponents"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxFragmentInputComponents}

instance CanReadField "maxFragmentInputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxFragmentInputComponents

        {-# INLINE readField #-}
        readField = readVkMaxFragmentInputComponents

instance CanWriteField "maxFragmentInputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxFragmentInputComponents

instance {-# OVERLAPPING #-}
         HasVkMaxFragmentOutputAttachments VkPhysicalDeviceLimits where
        type VkMaxFragmentOutputAttachmentsMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxFragmentOutputAttachments #-}
        vkMaxFragmentOutputAttachments x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxFragmentOutputAttachments})

        {-# INLINE vkMaxFragmentOutputAttachmentsByteOffset #-}
        vkMaxFragmentOutputAttachmentsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxFragmentOutputAttachments}

        {-# INLINE readVkMaxFragmentOutputAttachments #-}
        readVkMaxFragmentOutputAttachments p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxFragmentOutputAttachments}

        {-# INLINE writeVkMaxFragmentOutputAttachments #-}
        writeVkMaxFragmentOutputAttachments p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxFragmentOutputAttachments}

instance {-# OVERLAPPING #-}
         HasField "maxFragmentOutputAttachments" VkPhysicalDeviceLimits
         where
        type FieldType "maxFragmentOutputAttachments"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxFragmentOutputAttachments"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxFragmentOutputAttachments"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxFragmentOutputAttachments}
        type FieldIsArray "maxFragmentOutputAttachments"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxFragmentOutputAttachments}

instance CanReadField "maxFragmentOutputAttachments"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxFragmentOutputAttachments

        {-# INLINE readField #-}
        readField = readVkMaxFragmentOutputAttachments

instance CanWriteField "maxFragmentOutputAttachments"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxFragmentOutputAttachments

instance {-# OVERLAPPING #-}
         HasVkMaxFragmentDualSrcAttachments VkPhysicalDeviceLimits where
        type VkMaxFragmentDualSrcAttachmentsMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxFragmentDualSrcAttachments #-}
        vkMaxFragmentDualSrcAttachments x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxFragmentDualSrcAttachments})

        {-# INLINE vkMaxFragmentDualSrcAttachmentsByteOffset #-}
        vkMaxFragmentDualSrcAttachmentsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxFragmentDualSrcAttachments}

        {-# INLINE readVkMaxFragmentDualSrcAttachments #-}
        readVkMaxFragmentDualSrcAttachments p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxFragmentDualSrcAttachments}

        {-# INLINE writeVkMaxFragmentDualSrcAttachments #-}
        writeVkMaxFragmentDualSrcAttachments p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxFragmentDualSrcAttachments}

instance {-# OVERLAPPING #-}
         HasField "maxFragmentDualSrcAttachments" VkPhysicalDeviceLimits
         where
        type FieldType "maxFragmentDualSrcAttachments"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxFragmentDualSrcAttachments"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxFragmentDualSrcAttachments"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxFragmentDualSrcAttachments}
        type FieldIsArray "maxFragmentDualSrcAttachments"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxFragmentDualSrcAttachments}

instance CanReadField "maxFragmentDualSrcAttachments"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxFragmentDualSrcAttachments

        {-# INLINE readField #-}
        readField = readVkMaxFragmentDualSrcAttachments

instance CanWriteField "maxFragmentDualSrcAttachments"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxFragmentDualSrcAttachments

instance {-# OVERLAPPING #-}
         HasVkMaxFragmentCombinedOutputResources VkPhysicalDeviceLimits
         where
        type VkMaxFragmentCombinedOutputResourcesMType
               VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxFragmentCombinedOutputResources #-}
        vkMaxFragmentCombinedOutputResources x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxFragmentCombinedOutputResources})

        {-# INLINE vkMaxFragmentCombinedOutputResourcesByteOffset #-}
        vkMaxFragmentCombinedOutputResourcesByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxFragmentCombinedOutputResources}

        {-# INLINE readVkMaxFragmentCombinedOutputResources #-}
        readVkMaxFragmentCombinedOutputResources p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxFragmentCombinedOutputResources}

        {-# INLINE writeVkMaxFragmentCombinedOutputResources #-}
        writeVkMaxFragmentCombinedOutputResources p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxFragmentCombinedOutputResources}

instance {-# OVERLAPPING #-}
         HasField "maxFragmentCombinedOutputResources"
           VkPhysicalDeviceLimits
         where
        type FieldType "maxFragmentCombinedOutputResources"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxFragmentCombinedOutputResources"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxFragmentCombinedOutputResources"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxFragmentCombinedOutputResources}
        type FieldIsArray "maxFragmentCombinedOutputResources"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxFragmentCombinedOutputResources}

instance CanReadField "maxFragmentCombinedOutputResources"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxFragmentCombinedOutputResources

        {-# INLINE readField #-}
        readField = readVkMaxFragmentCombinedOutputResources

instance CanWriteField "maxFragmentCombinedOutputResources"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxFragmentCombinedOutputResources

instance {-# OVERLAPPING #-}
         HasVkMaxComputeSharedMemorySize VkPhysicalDeviceLimits where
        type VkMaxComputeSharedMemorySizeMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxComputeSharedMemorySize #-}
        vkMaxComputeSharedMemorySize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxComputeSharedMemorySize})

        {-# INLINE vkMaxComputeSharedMemorySizeByteOffset #-}
        vkMaxComputeSharedMemorySizeByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxComputeSharedMemorySize}

        {-# INLINE readVkMaxComputeSharedMemorySize #-}
        readVkMaxComputeSharedMemorySize p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxComputeSharedMemorySize}

        {-# INLINE writeVkMaxComputeSharedMemorySize #-}
        writeVkMaxComputeSharedMemorySize p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxComputeSharedMemorySize}

instance {-# OVERLAPPING #-}
         HasField "maxComputeSharedMemorySize" VkPhysicalDeviceLimits where
        type FieldType "maxComputeSharedMemorySize" VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxComputeSharedMemorySize"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxComputeSharedMemorySize"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxComputeSharedMemorySize}
        type FieldIsArray "maxComputeSharedMemorySize"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxComputeSharedMemorySize}

instance CanReadField "maxComputeSharedMemorySize"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxComputeSharedMemorySize

        {-# INLINE readField #-}
        readField = readVkMaxComputeSharedMemorySize

instance CanWriteField "maxComputeSharedMemorySize"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxComputeSharedMemorySize

instance {-# OVERLAPPING #-}
         HasVkMaxComputeWorkGroupCountArray VkPhysicalDeviceLimits where
        type VkMaxComputeWorkGroupCountArrayMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxComputeWorkGroupCountArray #-}
        vkMaxComputeWorkGroupCountArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: Word32) +
                    #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupCount}))

        {-# INLINE vkMaxComputeWorkGroupCountArrayByteOffset #-}
        vkMaxComputeWorkGroupCountArrayByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupCount}

        {-# INLINE readVkMaxComputeWorkGroupCountArray #-}
        readVkMaxComputeWorkGroupCountArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: Word32) +
                 #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupCount})

        {-# INLINE writeVkMaxComputeWorkGroupCountArray #-}
        writeVkMaxComputeWorkGroupCountArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: Word32) +
                 #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupCount})

instance {-# OVERLAPPING #-}
         HasField "maxComputeWorkGroupCount" VkPhysicalDeviceLimits where
        type FieldType "maxComputeWorkGroupCount" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxComputeWorkGroupCount"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxComputeWorkGroupCount" VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupCount}
        type FieldIsArray "maxComputeWorkGroupCount" VkPhysicalDeviceLimits
             = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupCount}

instance (KnownNat idx,
          IndexInBounds "maxComputeWorkGroupCount" idx
            VkPhysicalDeviceLimits) =>
         CanReadFieldArray "maxComputeWorkGroupCount" idx
           VkPhysicalDeviceLimits
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "maxComputeWorkGroupCount" 0
                         VkPhysicalDeviceLimits
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "maxComputeWorkGroupCount" 1
                         VkPhysicalDeviceLimits
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "maxComputeWorkGroupCount" 2
                         VkPhysicalDeviceLimits
                       #-}
        type FieldArrayLength "maxComputeWorkGroupCount"
               VkPhysicalDeviceLimits
             = 3

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 3

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkMaxComputeWorkGroupCountArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkMaxComputeWorkGroupCountArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "maxComputeWorkGroupCount" idx
            VkPhysicalDeviceLimits) =>
         CanWriteFieldArray "maxComputeWorkGroupCount" idx
           VkPhysicalDeviceLimits
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "maxComputeWorkGroupCount" 0
                         VkPhysicalDeviceLimits
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "maxComputeWorkGroupCount" 1
                         VkPhysicalDeviceLimits
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "maxComputeWorkGroupCount" 2
                         VkPhysicalDeviceLimits
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray x
          = writeVkMaxComputeWorkGroupCountArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkMaxComputeWorkGroupInvocations VkPhysicalDeviceLimits where
        type VkMaxComputeWorkGroupInvocationsMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxComputeWorkGroupInvocations #-}
        vkMaxComputeWorkGroupInvocations x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupInvocations})

        {-# INLINE vkMaxComputeWorkGroupInvocationsByteOffset #-}
        vkMaxComputeWorkGroupInvocationsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupInvocations}

        {-# INLINE readVkMaxComputeWorkGroupInvocations #-}
        readVkMaxComputeWorkGroupInvocations p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupInvocations}

        {-# INLINE writeVkMaxComputeWorkGroupInvocations #-}
        writeVkMaxComputeWorkGroupInvocations p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupInvocations}

instance {-# OVERLAPPING #-}
         HasField "maxComputeWorkGroupInvocations" VkPhysicalDeviceLimits
         where
        type FieldType "maxComputeWorkGroupInvocations"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxComputeWorkGroupInvocations"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxComputeWorkGroupInvocations"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupInvocations}
        type FieldIsArray "maxComputeWorkGroupInvocations"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupInvocations}

instance CanReadField "maxComputeWorkGroupInvocations"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxComputeWorkGroupInvocations

        {-# INLINE readField #-}
        readField = readVkMaxComputeWorkGroupInvocations

instance CanWriteField "maxComputeWorkGroupInvocations"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxComputeWorkGroupInvocations

instance {-# OVERLAPPING #-}
         HasVkMaxComputeWorkGroupSizeArray VkPhysicalDeviceLimits where
        type VkMaxComputeWorkGroupSizeArrayMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxComputeWorkGroupSizeArray #-}
        vkMaxComputeWorkGroupSizeArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: Word32) +
                    #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupSize}))

        {-# INLINE vkMaxComputeWorkGroupSizeArrayByteOffset #-}
        vkMaxComputeWorkGroupSizeArrayByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupSize}

        {-# INLINE readVkMaxComputeWorkGroupSizeArray #-}
        readVkMaxComputeWorkGroupSizeArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: Word32) +
                 #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupSize})

        {-# INLINE writeVkMaxComputeWorkGroupSizeArray #-}
        writeVkMaxComputeWorkGroupSizeArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: Word32) +
                 #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupSize})

instance {-# OVERLAPPING #-}
         HasField "maxComputeWorkGroupSize" VkPhysicalDeviceLimits where
        type FieldType "maxComputeWorkGroupSize" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxComputeWorkGroupSize" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxComputeWorkGroupSize" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupSize}
        type FieldIsArray "maxComputeWorkGroupSize" VkPhysicalDeviceLimits
             = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupSize}

instance (KnownNat idx,
          IndexInBounds "maxComputeWorkGroupSize" idx
            VkPhysicalDeviceLimits) =>
         CanReadFieldArray "maxComputeWorkGroupSize" idx
           VkPhysicalDeviceLimits
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "maxComputeWorkGroupSize" 0
                         VkPhysicalDeviceLimits
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "maxComputeWorkGroupSize" 1
                         VkPhysicalDeviceLimits
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "maxComputeWorkGroupSize" 2
                         VkPhysicalDeviceLimits
                       #-}
        type FieldArrayLength "maxComputeWorkGroupSize"
               VkPhysicalDeviceLimits
             = 3

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 3

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkMaxComputeWorkGroupSizeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkMaxComputeWorkGroupSizeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "maxComputeWorkGroupSize" idx
            VkPhysicalDeviceLimits) =>
         CanWriteFieldArray "maxComputeWorkGroupSize" idx
           VkPhysicalDeviceLimits
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "maxComputeWorkGroupSize" 0
                         VkPhysicalDeviceLimits
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "maxComputeWorkGroupSize" 1
                         VkPhysicalDeviceLimits
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "maxComputeWorkGroupSize" 2
                         VkPhysicalDeviceLimits
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray x
          = writeVkMaxComputeWorkGroupSizeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSubPixelPrecisionBits VkPhysicalDeviceLimits where
        type VkSubPixelPrecisionBitsMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkSubPixelPrecisionBits #-}
        vkSubPixelPrecisionBits x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, subPixelPrecisionBits})

        {-# INLINE vkSubPixelPrecisionBitsByteOffset #-}
        vkSubPixelPrecisionBitsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, subPixelPrecisionBits}

        {-# INLINE readVkSubPixelPrecisionBits #-}
        readVkSubPixelPrecisionBits p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, subPixelPrecisionBits}

        {-# INLINE writeVkSubPixelPrecisionBits #-}
        writeVkSubPixelPrecisionBits p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, subPixelPrecisionBits}

instance {-# OVERLAPPING #-}
         HasField "subPixelPrecisionBits" VkPhysicalDeviceLimits where
        type FieldType "subPixelPrecisionBits" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "subPixelPrecisionBits" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "subPixelPrecisionBits" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, subPixelPrecisionBits}
        type FieldIsArray "subPixelPrecisionBits" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, subPixelPrecisionBits}

instance CanReadField "subPixelPrecisionBits"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkSubPixelPrecisionBits

        {-# INLINE readField #-}
        readField = readVkSubPixelPrecisionBits

instance CanWriteField "subPixelPrecisionBits"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkSubPixelPrecisionBits

instance {-# OVERLAPPING #-}
         HasVkSubTexelPrecisionBits VkPhysicalDeviceLimits where
        type VkSubTexelPrecisionBitsMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkSubTexelPrecisionBits #-}
        vkSubTexelPrecisionBits x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, subTexelPrecisionBits})

        {-# INLINE vkSubTexelPrecisionBitsByteOffset #-}
        vkSubTexelPrecisionBitsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, subTexelPrecisionBits}

        {-# INLINE readVkSubTexelPrecisionBits #-}
        readVkSubTexelPrecisionBits p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, subTexelPrecisionBits}

        {-# INLINE writeVkSubTexelPrecisionBits #-}
        writeVkSubTexelPrecisionBits p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, subTexelPrecisionBits}

instance {-# OVERLAPPING #-}
         HasField "subTexelPrecisionBits" VkPhysicalDeviceLimits where
        type FieldType "subTexelPrecisionBits" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "subTexelPrecisionBits" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "subTexelPrecisionBits" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, subTexelPrecisionBits}
        type FieldIsArray "subTexelPrecisionBits" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, subTexelPrecisionBits}

instance CanReadField "subTexelPrecisionBits"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkSubTexelPrecisionBits

        {-# INLINE readField #-}
        readField = readVkSubTexelPrecisionBits

instance CanWriteField "subTexelPrecisionBits"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkSubTexelPrecisionBits

instance {-# OVERLAPPING #-}
         HasVkMipmapPrecisionBits VkPhysicalDeviceLimits where
        type VkMipmapPrecisionBitsMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMipmapPrecisionBits #-}
        vkMipmapPrecisionBits x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, mipmapPrecisionBits})

        {-# INLINE vkMipmapPrecisionBitsByteOffset #-}
        vkMipmapPrecisionBitsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, mipmapPrecisionBits}

        {-# INLINE readVkMipmapPrecisionBits #-}
        readVkMipmapPrecisionBits p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, mipmapPrecisionBits}

        {-# INLINE writeVkMipmapPrecisionBits #-}
        writeVkMipmapPrecisionBits p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, mipmapPrecisionBits}

instance {-# OVERLAPPING #-}
         HasField "mipmapPrecisionBits" VkPhysicalDeviceLimits where
        type FieldType "mipmapPrecisionBits" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "mipmapPrecisionBits" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "mipmapPrecisionBits" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, mipmapPrecisionBits}
        type FieldIsArray "mipmapPrecisionBits" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, mipmapPrecisionBits}

instance CanReadField "mipmapPrecisionBits" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMipmapPrecisionBits

        {-# INLINE readField #-}
        readField = readVkMipmapPrecisionBits

instance CanWriteField "mipmapPrecisionBits" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMipmapPrecisionBits

instance {-# OVERLAPPING #-}
         HasVkMaxDrawIndexedIndexValue VkPhysicalDeviceLimits where
        type VkMaxDrawIndexedIndexValueMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxDrawIndexedIndexValue #-}
        vkMaxDrawIndexedIndexValue x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDrawIndexedIndexValue})

        {-# INLINE vkMaxDrawIndexedIndexValueByteOffset #-}
        vkMaxDrawIndexedIndexValueByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxDrawIndexedIndexValue}

        {-# INLINE readVkMaxDrawIndexedIndexValue #-}
        readVkMaxDrawIndexedIndexValue p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDrawIndexedIndexValue}

        {-# INLINE writeVkMaxDrawIndexedIndexValue #-}
        writeVkMaxDrawIndexedIndexValue p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDrawIndexedIndexValue}

instance {-# OVERLAPPING #-}
         HasField "maxDrawIndexedIndexValue" VkPhysicalDeviceLimits where
        type FieldType "maxDrawIndexedIndexValue" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxDrawIndexedIndexValue"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDrawIndexedIndexValue" VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxDrawIndexedIndexValue}
        type FieldIsArray "maxDrawIndexedIndexValue" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxDrawIndexedIndexValue}

instance CanReadField "maxDrawIndexedIndexValue"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxDrawIndexedIndexValue

        {-# INLINE readField #-}
        readField = readVkMaxDrawIndexedIndexValue

instance CanWriteField "maxDrawIndexedIndexValue"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxDrawIndexedIndexValue

instance {-# OVERLAPPING #-}
         HasVkMaxDrawIndirectCount VkPhysicalDeviceLimits where
        type VkMaxDrawIndirectCountMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxDrawIndirectCount #-}
        vkMaxDrawIndirectCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDrawIndirectCount})

        {-# INLINE vkMaxDrawIndirectCountByteOffset #-}
        vkMaxDrawIndirectCountByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxDrawIndirectCount}

        {-# INLINE readVkMaxDrawIndirectCount #-}
        readVkMaxDrawIndirectCount p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDrawIndirectCount}

        {-# INLINE writeVkMaxDrawIndirectCount #-}
        writeVkMaxDrawIndirectCount p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDrawIndirectCount}

instance {-# OVERLAPPING #-}
         HasField "maxDrawIndirectCount" VkPhysicalDeviceLimits where
        type FieldType "maxDrawIndirectCount" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxDrawIndirectCount" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDrawIndirectCount" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxDrawIndirectCount}
        type FieldIsArray "maxDrawIndirectCount" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxDrawIndirectCount}

instance CanReadField "maxDrawIndirectCount" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxDrawIndirectCount

        {-# INLINE readField #-}
        readField = readVkMaxDrawIndirectCount

instance CanWriteField "maxDrawIndirectCount"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxDrawIndirectCount

instance {-# OVERLAPPING #-}
         HasVkMaxSamplerLodBias VkPhysicalDeviceLimits where
        type VkMaxSamplerLodBiasMType VkPhysicalDeviceLimits =
             #{type float}

        {-# NOINLINE vkMaxSamplerLodBias #-}
        vkMaxSamplerLodBias x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxSamplerLodBias})

        {-# INLINE vkMaxSamplerLodBiasByteOffset #-}
        vkMaxSamplerLodBiasByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxSamplerLodBias}

        {-# INLINE readVkMaxSamplerLodBias #-}
        readVkMaxSamplerLodBias p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxSamplerLodBias}

        {-# INLINE writeVkMaxSamplerLodBias #-}
        writeVkMaxSamplerLodBias p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxSamplerLodBias}

instance {-# OVERLAPPING #-}
         HasField "maxSamplerLodBias" VkPhysicalDeviceLimits where
        type FieldType "maxSamplerLodBias" VkPhysicalDeviceLimits =
             #{type float}
        type FieldOptional "maxSamplerLodBias" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSamplerLodBias" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxSamplerLodBias}
        type FieldIsArray "maxSamplerLodBias" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxSamplerLodBias}

instance CanReadField "maxSamplerLodBias" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxSamplerLodBias

        {-# INLINE readField #-}
        readField = readVkMaxSamplerLodBias

instance CanWriteField "maxSamplerLodBias" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxSamplerLodBias

instance {-# OVERLAPPING #-}
         HasVkMaxSamplerAnisotropy VkPhysicalDeviceLimits where
        type VkMaxSamplerAnisotropyMType VkPhysicalDeviceLimits =
             #{type float}

        {-# NOINLINE vkMaxSamplerAnisotropy #-}
        vkMaxSamplerAnisotropy x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxSamplerAnisotropy})

        {-# INLINE vkMaxSamplerAnisotropyByteOffset #-}
        vkMaxSamplerAnisotropyByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxSamplerAnisotropy}

        {-# INLINE readVkMaxSamplerAnisotropy #-}
        readVkMaxSamplerAnisotropy p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxSamplerAnisotropy}

        {-# INLINE writeVkMaxSamplerAnisotropy #-}
        writeVkMaxSamplerAnisotropy p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxSamplerAnisotropy}

instance {-# OVERLAPPING #-}
         HasField "maxSamplerAnisotropy" VkPhysicalDeviceLimits where
        type FieldType "maxSamplerAnisotropy" VkPhysicalDeviceLimits =
             #{type float}
        type FieldOptional "maxSamplerAnisotropy" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSamplerAnisotropy" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxSamplerAnisotropy}
        type FieldIsArray "maxSamplerAnisotropy" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxSamplerAnisotropy}

instance CanReadField "maxSamplerAnisotropy" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxSamplerAnisotropy

        {-# INLINE readField #-}
        readField = readVkMaxSamplerAnisotropy

instance CanWriteField "maxSamplerAnisotropy"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxSamplerAnisotropy

instance {-# OVERLAPPING #-}
         HasVkMaxViewports VkPhysicalDeviceLimits where
        type VkMaxViewportsMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxViewports #-}
        vkMaxViewports x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxViewports})

        {-# INLINE vkMaxViewportsByteOffset #-}
        vkMaxViewportsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxViewports}

        {-# INLINE readVkMaxViewports #-}
        readVkMaxViewports p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxViewports}

        {-# INLINE writeVkMaxViewports #-}
        writeVkMaxViewports p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxViewports}

instance {-# OVERLAPPING #-}
         HasField "maxViewports" VkPhysicalDeviceLimits where
        type FieldType "maxViewports" VkPhysicalDeviceLimits = Word32
        type FieldOptional "maxViewports" VkPhysicalDeviceLimits = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxViewports" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxViewports}
        type FieldIsArray "maxViewports" VkPhysicalDeviceLimits = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxViewports}

instance CanReadField "maxViewports" VkPhysicalDeviceLimits where
        {-# INLINE getField #-}
        getField = vkMaxViewports

        {-# INLINE readField #-}
        readField = readVkMaxViewports

instance CanWriteField "maxViewports" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField = writeVkMaxViewports

instance {-# OVERLAPPING #-}
         HasVkMaxViewportDimensionsArray VkPhysicalDeviceLimits where
        type VkMaxViewportDimensionsArrayMType VkPhysicalDeviceLimits =
             Word32

        {-# NOINLINE vkMaxViewportDimensionsArray #-}
        vkMaxViewportDimensionsArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: Word32) +
                    #{offset VkPhysicalDeviceLimits, maxViewportDimensions}))

        {-# INLINE vkMaxViewportDimensionsArrayByteOffset #-}
        vkMaxViewportDimensionsArrayByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxViewportDimensions}

        {-# INLINE readVkMaxViewportDimensionsArray #-}
        readVkMaxViewportDimensionsArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: Word32) +
                 #{offset VkPhysicalDeviceLimits, maxViewportDimensions})

        {-# INLINE writeVkMaxViewportDimensionsArray #-}
        writeVkMaxViewportDimensionsArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: Word32) +
                 #{offset VkPhysicalDeviceLimits, maxViewportDimensions})

instance {-# OVERLAPPING #-}
         HasField "maxViewportDimensions" VkPhysicalDeviceLimits where
        type FieldType "maxViewportDimensions" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxViewportDimensions" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxViewportDimensions" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxViewportDimensions}
        type FieldIsArray "maxViewportDimensions" VkPhysicalDeviceLimits =
             'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxViewportDimensions}

instance (KnownNat idx,
          IndexInBounds "maxViewportDimensions" idx
            VkPhysicalDeviceLimits) =>
         CanReadFieldArray "maxViewportDimensions" idx
           VkPhysicalDeviceLimits
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "maxViewportDimensions" 0 VkPhysicalDeviceLimits
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "maxViewportDimensions" 1 VkPhysicalDeviceLimits
                       #-}
        type FieldArrayLength "maxViewportDimensions"
               VkPhysicalDeviceLimits
             = 2

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 2

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkMaxViewportDimensionsArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkMaxViewportDimensionsArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "maxViewportDimensions" idx
            VkPhysicalDeviceLimits) =>
         CanWriteFieldArray "maxViewportDimensions" idx
           VkPhysicalDeviceLimits
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "maxViewportDimensions" 0 VkPhysicalDeviceLimits
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "maxViewportDimensions" 1 VkPhysicalDeviceLimits
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray x
          = writeVkMaxViewportDimensionsArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkViewportBoundsRangeArray VkPhysicalDeviceLimits where
        type VkViewportBoundsRangeArrayMType VkPhysicalDeviceLimits =
             #{type float}

        {-# NOINLINE vkViewportBoundsRangeArray #-}
        vkViewportBoundsRangeArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: #{type float}) +
                    #{offset VkPhysicalDeviceLimits, viewportBoundsRange}))

        {-# INLINE vkViewportBoundsRangeArrayByteOffset #-}
        vkViewportBoundsRangeArrayByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, viewportBoundsRange}

        {-# INLINE readVkViewportBoundsRangeArray #-}
        readVkViewportBoundsRangeArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkPhysicalDeviceLimits, viewportBoundsRange})

        {-# INLINE writeVkViewportBoundsRangeArray #-}
        writeVkViewportBoundsRangeArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkPhysicalDeviceLimits, viewportBoundsRange})

instance {-# OVERLAPPING #-}
         HasField "viewportBoundsRange" VkPhysicalDeviceLimits where
        type FieldType "viewportBoundsRange" VkPhysicalDeviceLimits =
             #{type float}
        type FieldOptional "viewportBoundsRange" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "viewportBoundsRange" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, viewportBoundsRange}
        type FieldIsArray "viewportBoundsRange" VkPhysicalDeviceLimits =
             'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, viewportBoundsRange}

instance (KnownNat idx,
          IndexInBounds "viewportBoundsRange" idx VkPhysicalDeviceLimits) =>
         CanReadFieldArray "viewportBoundsRange" idx VkPhysicalDeviceLimits
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "viewportBoundsRange" 0 VkPhysicalDeviceLimits
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "viewportBoundsRange" 1 VkPhysicalDeviceLimits
                       #-}
        type FieldArrayLength "viewportBoundsRange" VkPhysicalDeviceLimits
             = 2

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 2

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkViewportBoundsRangeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkViewportBoundsRangeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "viewportBoundsRange" idx VkPhysicalDeviceLimits) =>
         CanWriteFieldArray "viewportBoundsRange" idx VkPhysicalDeviceLimits
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "viewportBoundsRange" 0 VkPhysicalDeviceLimits
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "viewportBoundsRange" 1 VkPhysicalDeviceLimits
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray x
          = writeVkViewportBoundsRangeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkViewportSubPixelBits VkPhysicalDeviceLimits where
        type VkViewportSubPixelBitsMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkViewportSubPixelBits #-}
        vkViewportSubPixelBits x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, viewportSubPixelBits})

        {-# INLINE vkViewportSubPixelBitsByteOffset #-}
        vkViewportSubPixelBitsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, viewportSubPixelBits}

        {-# INLINE readVkViewportSubPixelBits #-}
        readVkViewportSubPixelBits p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, viewportSubPixelBits}

        {-# INLINE writeVkViewportSubPixelBits #-}
        writeVkViewportSubPixelBits p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, viewportSubPixelBits}

instance {-# OVERLAPPING #-}
         HasField "viewportSubPixelBits" VkPhysicalDeviceLimits where
        type FieldType "viewportSubPixelBits" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "viewportSubPixelBits" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "viewportSubPixelBits" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, viewportSubPixelBits}
        type FieldIsArray "viewportSubPixelBits" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, viewportSubPixelBits}

instance CanReadField "viewportSubPixelBits" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkViewportSubPixelBits

        {-# INLINE readField #-}
        readField = readVkViewportSubPixelBits

instance CanWriteField "viewportSubPixelBits"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkViewportSubPixelBits

instance {-# OVERLAPPING #-}
         HasVkMinMemoryMapAlignment VkPhysicalDeviceLimits where
        type VkMinMemoryMapAlignmentMType VkPhysicalDeviceLimits = CSize

        {-# NOINLINE vkMinMemoryMapAlignment #-}
        vkMinMemoryMapAlignment x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, minMemoryMapAlignment})

        {-# INLINE vkMinMemoryMapAlignmentByteOffset #-}
        vkMinMemoryMapAlignmentByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, minMemoryMapAlignment}

        {-# INLINE readVkMinMemoryMapAlignment #-}
        readVkMinMemoryMapAlignment p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, minMemoryMapAlignment}

        {-# INLINE writeVkMinMemoryMapAlignment #-}
        writeVkMinMemoryMapAlignment p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, minMemoryMapAlignment}

instance {-# OVERLAPPING #-}
         HasField "minMemoryMapAlignment" VkPhysicalDeviceLimits where
        type FieldType "minMemoryMapAlignment" VkPhysicalDeviceLimits =
             CSize
        type FieldOptional "minMemoryMapAlignment" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minMemoryMapAlignment" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, minMemoryMapAlignment}
        type FieldIsArray "minMemoryMapAlignment" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, minMemoryMapAlignment}

instance CanReadField "minMemoryMapAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMinMemoryMapAlignment

        {-# INLINE readField #-}
        readField = readVkMinMemoryMapAlignment

instance CanWriteField "minMemoryMapAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinMemoryMapAlignment

instance {-# OVERLAPPING #-}
         HasVkMinTexelBufferOffsetAlignment VkPhysicalDeviceLimits where
        type VkMinTexelBufferOffsetAlignmentMType VkPhysicalDeviceLimits =
             VkDeviceSize

        {-# NOINLINE vkMinTexelBufferOffsetAlignment #-}
        vkMinTexelBufferOffsetAlignment x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, minTexelBufferOffsetAlignment})

        {-# INLINE vkMinTexelBufferOffsetAlignmentByteOffset #-}
        vkMinTexelBufferOffsetAlignmentByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, minTexelBufferOffsetAlignment}

        {-# INLINE readVkMinTexelBufferOffsetAlignment #-}
        readVkMinTexelBufferOffsetAlignment p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, minTexelBufferOffsetAlignment}

        {-# INLINE writeVkMinTexelBufferOffsetAlignment #-}
        writeVkMinTexelBufferOffsetAlignment p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, minTexelBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         HasField "minTexelBufferOffsetAlignment" VkPhysicalDeviceLimits
         where
        type FieldType "minTexelBufferOffsetAlignment"
               VkPhysicalDeviceLimits
             = VkDeviceSize
        type FieldOptional "minTexelBufferOffsetAlignment"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minTexelBufferOffsetAlignment"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, minTexelBufferOffsetAlignment}
        type FieldIsArray "minTexelBufferOffsetAlignment"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, minTexelBufferOffsetAlignment}

instance CanReadField "minTexelBufferOffsetAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMinTexelBufferOffsetAlignment

        {-# INLINE readField #-}
        readField = readVkMinTexelBufferOffsetAlignment

instance CanWriteField "minTexelBufferOffsetAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinTexelBufferOffsetAlignment

instance {-# OVERLAPPING #-}
         HasVkMinUniformBufferOffsetAlignment VkPhysicalDeviceLimits where
        type VkMinUniformBufferOffsetAlignmentMType VkPhysicalDeviceLimits
             = VkDeviceSize

        {-# NOINLINE vkMinUniformBufferOffsetAlignment #-}
        vkMinUniformBufferOffsetAlignment x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, minUniformBufferOffsetAlignment})

        {-# INLINE vkMinUniformBufferOffsetAlignmentByteOffset #-}
        vkMinUniformBufferOffsetAlignmentByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, minUniformBufferOffsetAlignment}

        {-# INLINE readVkMinUniformBufferOffsetAlignment #-}
        readVkMinUniformBufferOffsetAlignment p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, minUniformBufferOffsetAlignment}

        {-# INLINE writeVkMinUniformBufferOffsetAlignment #-}
        writeVkMinUniformBufferOffsetAlignment p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, minUniformBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         HasField "minUniformBufferOffsetAlignment" VkPhysicalDeviceLimits
         where
        type FieldType "minUniformBufferOffsetAlignment"
               VkPhysicalDeviceLimits
             = VkDeviceSize
        type FieldOptional "minUniformBufferOffsetAlignment"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minUniformBufferOffsetAlignment"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, minUniformBufferOffsetAlignment}
        type FieldIsArray "minUniformBufferOffsetAlignment"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, minUniformBufferOffsetAlignment}

instance CanReadField "minUniformBufferOffsetAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMinUniformBufferOffsetAlignment

        {-# INLINE readField #-}
        readField = readVkMinUniformBufferOffsetAlignment

instance CanWriteField "minUniformBufferOffsetAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinUniformBufferOffsetAlignment

instance {-# OVERLAPPING #-}
         HasVkMinStorageBufferOffsetAlignment VkPhysicalDeviceLimits where
        type VkMinStorageBufferOffsetAlignmentMType VkPhysicalDeviceLimits
             = VkDeviceSize

        {-# NOINLINE vkMinStorageBufferOffsetAlignment #-}
        vkMinStorageBufferOffsetAlignment x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, minStorageBufferOffsetAlignment})

        {-# INLINE vkMinStorageBufferOffsetAlignmentByteOffset #-}
        vkMinStorageBufferOffsetAlignmentByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, minStorageBufferOffsetAlignment}

        {-# INLINE readVkMinStorageBufferOffsetAlignment #-}
        readVkMinStorageBufferOffsetAlignment p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, minStorageBufferOffsetAlignment}

        {-# INLINE writeVkMinStorageBufferOffsetAlignment #-}
        writeVkMinStorageBufferOffsetAlignment p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, minStorageBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         HasField "minStorageBufferOffsetAlignment" VkPhysicalDeviceLimits
         where
        type FieldType "minStorageBufferOffsetAlignment"
               VkPhysicalDeviceLimits
             = VkDeviceSize
        type FieldOptional "minStorageBufferOffsetAlignment"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minStorageBufferOffsetAlignment"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, minStorageBufferOffsetAlignment}
        type FieldIsArray "minStorageBufferOffsetAlignment"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, minStorageBufferOffsetAlignment}

instance CanReadField "minStorageBufferOffsetAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMinStorageBufferOffsetAlignment

        {-# INLINE readField #-}
        readField = readVkMinStorageBufferOffsetAlignment

instance CanWriteField "minStorageBufferOffsetAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinStorageBufferOffsetAlignment

instance {-# OVERLAPPING #-}
         HasVkMinTexelOffset VkPhysicalDeviceLimits where
        type VkMinTexelOffsetMType VkPhysicalDeviceLimits = Int32

        {-# NOINLINE vkMinTexelOffset #-}
        vkMinTexelOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, minTexelOffset})

        {-# INLINE vkMinTexelOffsetByteOffset #-}
        vkMinTexelOffsetByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, minTexelOffset}

        {-# INLINE readVkMinTexelOffset #-}
        readVkMinTexelOffset p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, minTexelOffset}

        {-# INLINE writeVkMinTexelOffset #-}
        writeVkMinTexelOffset p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, minTexelOffset}

instance {-# OVERLAPPING #-}
         HasField "minTexelOffset" VkPhysicalDeviceLimits where
        type FieldType "minTexelOffset" VkPhysicalDeviceLimits = Int32
        type FieldOptional "minTexelOffset" VkPhysicalDeviceLimits = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minTexelOffset" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, minTexelOffset}
        type FieldIsArray "minTexelOffset" VkPhysicalDeviceLimits = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, minTexelOffset}

instance CanReadField "minTexelOffset" VkPhysicalDeviceLimits where
        {-# INLINE getField #-}
        getField = vkMinTexelOffset

        {-# INLINE readField #-}
        readField = readVkMinTexelOffset

instance CanWriteField "minTexelOffset" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinTexelOffset

instance {-# OVERLAPPING #-}
         HasVkMaxTexelOffset VkPhysicalDeviceLimits where
        type VkMaxTexelOffsetMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxTexelOffset #-}
        vkMaxTexelOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTexelOffset})

        {-# INLINE vkMaxTexelOffsetByteOffset #-}
        vkMaxTexelOffsetByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxTexelOffset}

        {-# INLINE readVkMaxTexelOffset #-}
        readVkMaxTexelOffset p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTexelOffset}

        {-# INLINE writeVkMaxTexelOffset #-}
        writeVkMaxTexelOffset p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTexelOffset}

instance {-# OVERLAPPING #-}
         HasField "maxTexelOffset" VkPhysicalDeviceLimits where
        type FieldType "maxTexelOffset" VkPhysicalDeviceLimits = Word32
        type FieldOptional "maxTexelOffset" VkPhysicalDeviceLimits = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxTexelOffset" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxTexelOffset}
        type FieldIsArray "maxTexelOffset" VkPhysicalDeviceLimits = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxTexelOffset}

instance CanReadField "maxTexelOffset" VkPhysicalDeviceLimits where
        {-# INLINE getField #-}
        getField = vkMaxTexelOffset

        {-# INLINE readField #-}
        readField = readVkMaxTexelOffset

instance CanWriteField "maxTexelOffset" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxTexelOffset

instance {-# OVERLAPPING #-}
         HasVkMinTexelGatherOffset VkPhysicalDeviceLimits where
        type VkMinTexelGatherOffsetMType VkPhysicalDeviceLimits = Int32

        {-# NOINLINE vkMinTexelGatherOffset #-}
        vkMinTexelGatherOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, minTexelGatherOffset})

        {-# INLINE vkMinTexelGatherOffsetByteOffset #-}
        vkMinTexelGatherOffsetByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, minTexelGatherOffset}

        {-# INLINE readVkMinTexelGatherOffset #-}
        readVkMinTexelGatherOffset p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, minTexelGatherOffset}

        {-# INLINE writeVkMinTexelGatherOffset #-}
        writeVkMinTexelGatherOffset p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, minTexelGatherOffset}

instance {-# OVERLAPPING #-}
         HasField "minTexelGatherOffset" VkPhysicalDeviceLimits where
        type FieldType "minTexelGatherOffset" VkPhysicalDeviceLimits =
             Int32
        type FieldOptional "minTexelGatherOffset" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minTexelGatherOffset" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, minTexelGatherOffset}
        type FieldIsArray "minTexelGatherOffset" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, minTexelGatherOffset}

instance CanReadField "minTexelGatherOffset" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMinTexelGatherOffset

        {-# INLINE readField #-}
        readField = readVkMinTexelGatherOffset

instance CanWriteField "minTexelGatherOffset"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinTexelGatherOffset

instance {-# OVERLAPPING #-}
         HasVkMaxTexelGatherOffset VkPhysicalDeviceLimits where
        type VkMaxTexelGatherOffsetMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxTexelGatherOffset #-}
        vkMaxTexelGatherOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTexelGatherOffset})

        {-# INLINE vkMaxTexelGatherOffsetByteOffset #-}
        vkMaxTexelGatherOffsetByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxTexelGatherOffset}

        {-# INLINE readVkMaxTexelGatherOffset #-}
        readVkMaxTexelGatherOffset p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTexelGatherOffset}

        {-# INLINE writeVkMaxTexelGatherOffset #-}
        writeVkMaxTexelGatherOffset p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTexelGatherOffset}

instance {-# OVERLAPPING #-}
         HasField "maxTexelGatherOffset" VkPhysicalDeviceLimits where
        type FieldType "maxTexelGatherOffset" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxTexelGatherOffset" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxTexelGatherOffset" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxTexelGatherOffset}
        type FieldIsArray "maxTexelGatherOffset" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxTexelGatherOffset}

instance CanReadField "maxTexelGatherOffset" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxTexelGatherOffset

        {-# INLINE readField #-}
        readField = readVkMaxTexelGatherOffset

instance CanWriteField "maxTexelGatherOffset"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxTexelGatherOffset

instance {-# OVERLAPPING #-}
         HasVkMinInterpolationOffset VkPhysicalDeviceLimits where
        type VkMinInterpolationOffsetMType VkPhysicalDeviceLimits =
             #{type float}

        {-# NOINLINE vkMinInterpolationOffset #-}
        vkMinInterpolationOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, minInterpolationOffset})

        {-# INLINE vkMinInterpolationOffsetByteOffset #-}
        vkMinInterpolationOffsetByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, minInterpolationOffset}

        {-# INLINE readVkMinInterpolationOffset #-}
        readVkMinInterpolationOffset p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, minInterpolationOffset}

        {-# INLINE writeVkMinInterpolationOffset #-}
        writeVkMinInterpolationOffset p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, minInterpolationOffset}

instance {-# OVERLAPPING #-}
         HasField "minInterpolationOffset" VkPhysicalDeviceLimits where
        type FieldType "minInterpolationOffset" VkPhysicalDeviceLimits =
             #{type float}
        type FieldOptional "minInterpolationOffset" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minInterpolationOffset" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, minInterpolationOffset}
        type FieldIsArray "minInterpolationOffset" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, minInterpolationOffset}

instance CanReadField "minInterpolationOffset"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMinInterpolationOffset

        {-# INLINE readField #-}
        readField = readVkMinInterpolationOffset

instance CanWriteField "minInterpolationOffset"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinInterpolationOffset

instance {-# OVERLAPPING #-}
         HasVkMaxInterpolationOffset VkPhysicalDeviceLimits where
        type VkMaxInterpolationOffsetMType VkPhysicalDeviceLimits =
             #{type float}

        {-# NOINLINE vkMaxInterpolationOffset #-}
        vkMaxInterpolationOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxInterpolationOffset})

        {-# INLINE vkMaxInterpolationOffsetByteOffset #-}
        vkMaxInterpolationOffsetByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxInterpolationOffset}

        {-# INLINE readVkMaxInterpolationOffset #-}
        readVkMaxInterpolationOffset p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxInterpolationOffset}

        {-# INLINE writeVkMaxInterpolationOffset #-}
        writeVkMaxInterpolationOffset p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxInterpolationOffset}

instance {-# OVERLAPPING #-}
         HasField "maxInterpolationOffset" VkPhysicalDeviceLimits where
        type FieldType "maxInterpolationOffset" VkPhysicalDeviceLimits =
             #{type float}
        type FieldOptional "maxInterpolationOffset" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxInterpolationOffset" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxInterpolationOffset}
        type FieldIsArray "maxInterpolationOffset" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxInterpolationOffset}

instance CanReadField "maxInterpolationOffset"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxInterpolationOffset

        {-# INLINE readField #-}
        readField = readVkMaxInterpolationOffset

instance CanWriteField "maxInterpolationOffset"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxInterpolationOffset

instance {-# OVERLAPPING #-}
         HasVkSubPixelInterpolationOffsetBits VkPhysicalDeviceLimits where
        type VkSubPixelInterpolationOffsetBitsMType VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkSubPixelInterpolationOffsetBits #-}
        vkSubPixelInterpolationOffsetBits x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, subPixelInterpolationOffsetBits})

        {-# INLINE vkSubPixelInterpolationOffsetBitsByteOffset #-}
        vkSubPixelInterpolationOffsetBitsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, subPixelInterpolationOffsetBits}

        {-# INLINE readVkSubPixelInterpolationOffsetBits #-}
        readVkSubPixelInterpolationOffsetBits p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, subPixelInterpolationOffsetBits}

        {-# INLINE writeVkSubPixelInterpolationOffsetBits #-}
        writeVkSubPixelInterpolationOffsetBits p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, subPixelInterpolationOffsetBits}

instance {-# OVERLAPPING #-}
         HasField "subPixelInterpolationOffsetBits" VkPhysicalDeviceLimits
         where
        type FieldType "subPixelInterpolationOffsetBits"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "subPixelInterpolationOffsetBits"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "subPixelInterpolationOffsetBits"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, subPixelInterpolationOffsetBits}
        type FieldIsArray "subPixelInterpolationOffsetBits"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, subPixelInterpolationOffsetBits}

instance CanReadField "subPixelInterpolationOffsetBits"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkSubPixelInterpolationOffsetBits

        {-# INLINE readField #-}
        readField = readVkSubPixelInterpolationOffsetBits

instance CanWriteField "subPixelInterpolationOffsetBits"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkSubPixelInterpolationOffsetBits

instance {-# OVERLAPPING #-}
         HasVkMaxFramebufferWidth VkPhysicalDeviceLimits where
        type VkMaxFramebufferWidthMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxFramebufferWidth #-}
        vkMaxFramebufferWidth x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxFramebufferWidth})

        {-# INLINE vkMaxFramebufferWidthByteOffset #-}
        vkMaxFramebufferWidthByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxFramebufferWidth}

        {-# INLINE readVkMaxFramebufferWidth #-}
        readVkMaxFramebufferWidth p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxFramebufferWidth}

        {-# INLINE writeVkMaxFramebufferWidth #-}
        writeVkMaxFramebufferWidth p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxFramebufferWidth}

instance {-# OVERLAPPING #-}
         HasField "maxFramebufferWidth" VkPhysicalDeviceLimits where
        type FieldType "maxFramebufferWidth" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxFramebufferWidth" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxFramebufferWidth" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxFramebufferWidth}
        type FieldIsArray "maxFramebufferWidth" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxFramebufferWidth}

instance CanReadField "maxFramebufferWidth" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxFramebufferWidth

        {-# INLINE readField #-}
        readField = readVkMaxFramebufferWidth

instance CanWriteField "maxFramebufferWidth" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxFramebufferWidth

instance {-# OVERLAPPING #-}
         HasVkMaxFramebufferHeight VkPhysicalDeviceLimits where
        type VkMaxFramebufferHeightMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxFramebufferHeight #-}
        vkMaxFramebufferHeight x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxFramebufferHeight})

        {-# INLINE vkMaxFramebufferHeightByteOffset #-}
        vkMaxFramebufferHeightByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxFramebufferHeight}

        {-# INLINE readVkMaxFramebufferHeight #-}
        readVkMaxFramebufferHeight p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxFramebufferHeight}

        {-# INLINE writeVkMaxFramebufferHeight #-}
        writeVkMaxFramebufferHeight p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxFramebufferHeight}

instance {-# OVERLAPPING #-}
         HasField "maxFramebufferHeight" VkPhysicalDeviceLimits where
        type FieldType "maxFramebufferHeight" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxFramebufferHeight" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxFramebufferHeight" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxFramebufferHeight}
        type FieldIsArray "maxFramebufferHeight" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxFramebufferHeight}

instance CanReadField "maxFramebufferHeight" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxFramebufferHeight

        {-# INLINE readField #-}
        readField = readVkMaxFramebufferHeight

instance CanWriteField "maxFramebufferHeight"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxFramebufferHeight

instance {-# OVERLAPPING #-}
         HasVkMaxFramebufferLayers VkPhysicalDeviceLimits where
        type VkMaxFramebufferLayersMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxFramebufferLayers #-}
        vkMaxFramebufferLayers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxFramebufferLayers})

        {-# INLINE vkMaxFramebufferLayersByteOffset #-}
        vkMaxFramebufferLayersByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxFramebufferLayers}

        {-# INLINE readVkMaxFramebufferLayers #-}
        readVkMaxFramebufferLayers p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxFramebufferLayers}

        {-# INLINE writeVkMaxFramebufferLayers #-}
        writeVkMaxFramebufferLayers p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxFramebufferLayers}

instance {-# OVERLAPPING #-}
         HasField "maxFramebufferLayers" VkPhysicalDeviceLimits where
        type FieldType "maxFramebufferLayers" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxFramebufferLayers" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxFramebufferLayers" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxFramebufferLayers}
        type FieldIsArray "maxFramebufferLayers" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxFramebufferLayers}

instance CanReadField "maxFramebufferLayers" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxFramebufferLayers

        {-# INLINE readField #-}
        readField = readVkMaxFramebufferLayers

instance CanWriteField "maxFramebufferLayers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxFramebufferLayers

instance {-# OVERLAPPING #-}
         HasVkFramebufferColorSampleCounts VkPhysicalDeviceLimits where
        type VkFramebufferColorSampleCountsMType VkPhysicalDeviceLimits =
             VkSampleCountFlags

        {-# NOINLINE vkFramebufferColorSampleCounts #-}
        vkFramebufferColorSampleCounts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, framebufferColorSampleCounts})

        {-# INLINE vkFramebufferColorSampleCountsByteOffset #-}
        vkFramebufferColorSampleCountsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, framebufferColorSampleCounts}

        {-# INLINE readVkFramebufferColorSampleCounts #-}
        readVkFramebufferColorSampleCounts p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, framebufferColorSampleCounts}

        {-# INLINE writeVkFramebufferColorSampleCounts #-}
        writeVkFramebufferColorSampleCounts p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, framebufferColorSampleCounts}

instance {-# OVERLAPPING #-}
         HasField "framebufferColorSampleCounts" VkPhysicalDeviceLimits
         where
        type FieldType "framebufferColorSampleCounts"
               VkPhysicalDeviceLimits
             = VkSampleCountFlags
        type FieldOptional "framebufferColorSampleCounts"
               VkPhysicalDeviceLimits
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "framebufferColorSampleCounts"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, framebufferColorSampleCounts}
        type FieldIsArray "framebufferColorSampleCounts"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, framebufferColorSampleCounts}

instance CanReadField "framebufferColorSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkFramebufferColorSampleCounts

        {-# INLINE readField #-}
        readField = readVkFramebufferColorSampleCounts

instance CanWriteField "framebufferColorSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkFramebufferColorSampleCounts

instance {-# OVERLAPPING #-}
         HasVkFramebufferDepthSampleCounts VkPhysicalDeviceLimits where
        type VkFramebufferDepthSampleCountsMType VkPhysicalDeviceLimits =
             VkSampleCountFlags

        {-# NOINLINE vkFramebufferDepthSampleCounts #-}
        vkFramebufferDepthSampleCounts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, framebufferDepthSampleCounts})

        {-# INLINE vkFramebufferDepthSampleCountsByteOffset #-}
        vkFramebufferDepthSampleCountsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, framebufferDepthSampleCounts}

        {-# INLINE readVkFramebufferDepthSampleCounts #-}
        readVkFramebufferDepthSampleCounts p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, framebufferDepthSampleCounts}

        {-# INLINE writeVkFramebufferDepthSampleCounts #-}
        writeVkFramebufferDepthSampleCounts p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, framebufferDepthSampleCounts}

instance {-# OVERLAPPING #-}
         HasField "framebufferDepthSampleCounts" VkPhysicalDeviceLimits
         where
        type FieldType "framebufferDepthSampleCounts"
               VkPhysicalDeviceLimits
             = VkSampleCountFlags
        type FieldOptional "framebufferDepthSampleCounts"
               VkPhysicalDeviceLimits
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "framebufferDepthSampleCounts"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, framebufferDepthSampleCounts}
        type FieldIsArray "framebufferDepthSampleCounts"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, framebufferDepthSampleCounts}

instance CanReadField "framebufferDepthSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkFramebufferDepthSampleCounts

        {-# INLINE readField #-}
        readField = readVkFramebufferDepthSampleCounts

instance CanWriteField "framebufferDepthSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkFramebufferDepthSampleCounts

instance {-# OVERLAPPING #-}
         HasVkFramebufferStencilSampleCounts VkPhysicalDeviceLimits where
        type VkFramebufferStencilSampleCountsMType VkPhysicalDeviceLimits =
             VkSampleCountFlags

        {-# NOINLINE vkFramebufferStencilSampleCounts #-}
        vkFramebufferStencilSampleCounts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, framebufferStencilSampleCounts})

        {-# INLINE vkFramebufferStencilSampleCountsByteOffset #-}
        vkFramebufferStencilSampleCountsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, framebufferStencilSampleCounts}

        {-# INLINE readVkFramebufferStencilSampleCounts #-}
        readVkFramebufferStencilSampleCounts p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, framebufferStencilSampleCounts}

        {-# INLINE writeVkFramebufferStencilSampleCounts #-}
        writeVkFramebufferStencilSampleCounts p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, framebufferStencilSampleCounts}

instance {-# OVERLAPPING #-}
         HasField "framebufferStencilSampleCounts" VkPhysicalDeviceLimits
         where
        type FieldType "framebufferStencilSampleCounts"
               VkPhysicalDeviceLimits
             = VkSampleCountFlags
        type FieldOptional "framebufferStencilSampleCounts"
               VkPhysicalDeviceLimits
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "framebufferStencilSampleCounts"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, framebufferStencilSampleCounts}
        type FieldIsArray "framebufferStencilSampleCounts"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, framebufferStencilSampleCounts}

instance CanReadField "framebufferStencilSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkFramebufferStencilSampleCounts

        {-# INLINE readField #-}
        readField = readVkFramebufferStencilSampleCounts

instance CanWriteField "framebufferStencilSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkFramebufferStencilSampleCounts

instance {-# OVERLAPPING #-}
         HasVkFramebufferNoAttachmentsSampleCounts VkPhysicalDeviceLimits
         where
        type VkFramebufferNoAttachmentsSampleCountsMType
               VkPhysicalDeviceLimits
             = VkSampleCountFlags

        {-# NOINLINE vkFramebufferNoAttachmentsSampleCounts #-}
        vkFramebufferNoAttachmentsSampleCounts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, framebufferNoAttachmentsSampleCounts})

        {-# INLINE vkFramebufferNoAttachmentsSampleCountsByteOffset #-}
        vkFramebufferNoAttachmentsSampleCountsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, framebufferNoAttachmentsSampleCounts}

        {-# INLINE readVkFramebufferNoAttachmentsSampleCounts #-}
        readVkFramebufferNoAttachmentsSampleCounts p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, framebufferNoAttachmentsSampleCounts}

        {-# INLINE writeVkFramebufferNoAttachmentsSampleCounts #-}
        writeVkFramebufferNoAttachmentsSampleCounts p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, framebufferNoAttachmentsSampleCounts}

instance {-# OVERLAPPING #-}
         HasField "framebufferNoAttachmentsSampleCounts"
           VkPhysicalDeviceLimits
         where
        type FieldType "framebufferNoAttachmentsSampleCounts"
               VkPhysicalDeviceLimits
             = VkSampleCountFlags
        type FieldOptional "framebufferNoAttachmentsSampleCounts"
               VkPhysicalDeviceLimits
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "framebufferNoAttachmentsSampleCounts"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, framebufferNoAttachmentsSampleCounts}
        type FieldIsArray "framebufferNoAttachmentsSampleCounts"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, framebufferNoAttachmentsSampleCounts}

instance CanReadField "framebufferNoAttachmentsSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkFramebufferNoAttachmentsSampleCounts

        {-# INLINE readField #-}
        readField = readVkFramebufferNoAttachmentsSampleCounts

instance CanWriteField "framebufferNoAttachmentsSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkFramebufferNoAttachmentsSampleCounts

instance {-# OVERLAPPING #-}
         HasVkMaxColorAttachments VkPhysicalDeviceLimits where
        type VkMaxColorAttachmentsMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxColorAttachments #-}
        vkMaxColorAttachments x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxColorAttachments})

        {-# INLINE vkMaxColorAttachmentsByteOffset #-}
        vkMaxColorAttachmentsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxColorAttachments}

        {-# INLINE readVkMaxColorAttachments #-}
        readVkMaxColorAttachments p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxColorAttachments}

        {-# INLINE writeVkMaxColorAttachments #-}
        writeVkMaxColorAttachments p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxColorAttachments}

instance {-# OVERLAPPING #-}
         HasField "maxColorAttachments" VkPhysicalDeviceLimits where
        type FieldType "maxColorAttachments" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "maxColorAttachments" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxColorAttachments" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxColorAttachments}
        type FieldIsArray "maxColorAttachments" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxColorAttachments}

instance CanReadField "maxColorAttachments" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxColorAttachments

        {-# INLINE readField #-}
        readField = readVkMaxColorAttachments

instance CanWriteField "maxColorAttachments" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxColorAttachments

instance {-# OVERLAPPING #-}
         HasVkSampledImageColorSampleCounts VkPhysicalDeviceLimits where
        type VkSampledImageColorSampleCountsMType VkPhysicalDeviceLimits =
             VkSampleCountFlags

        {-# NOINLINE vkSampledImageColorSampleCounts #-}
        vkSampledImageColorSampleCounts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, sampledImageColorSampleCounts})

        {-# INLINE vkSampledImageColorSampleCountsByteOffset #-}
        vkSampledImageColorSampleCountsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, sampledImageColorSampleCounts}

        {-# INLINE readVkSampledImageColorSampleCounts #-}
        readVkSampledImageColorSampleCounts p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, sampledImageColorSampleCounts}

        {-# INLINE writeVkSampledImageColorSampleCounts #-}
        writeVkSampledImageColorSampleCounts p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, sampledImageColorSampleCounts}

instance {-# OVERLAPPING #-}
         HasField "sampledImageColorSampleCounts" VkPhysicalDeviceLimits
         where
        type FieldType "sampledImageColorSampleCounts"
               VkPhysicalDeviceLimits
             = VkSampleCountFlags
        type FieldOptional "sampledImageColorSampleCounts"
               VkPhysicalDeviceLimits
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "sampledImageColorSampleCounts"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, sampledImageColorSampleCounts}
        type FieldIsArray "sampledImageColorSampleCounts"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, sampledImageColorSampleCounts}

instance CanReadField "sampledImageColorSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkSampledImageColorSampleCounts

        {-# INLINE readField #-}
        readField = readVkSampledImageColorSampleCounts

instance CanWriteField "sampledImageColorSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampledImageColorSampleCounts

instance {-# OVERLAPPING #-}
         HasVkSampledImageIntegerSampleCounts VkPhysicalDeviceLimits where
        type VkSampledImageIntegerSampleCountsMType VkPhysicalDeviceLimits
             = VkSampleCountFlags

        {-# NOINLINE vkSampledImageIntegerSampleCounts #-}
        vkSampledImageIntegerSampleCounts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, sampledImageIntegerSampleCounts})

        {-# INLINE vkSampledImageIntegerSampleCountsByteOffset #-}
        vkSampledImageIntegerSampleCountsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, sampledImageIntegerSampleCounts}

        {-# INLINE readVkSampledImageIntegerSampleCounts #-}
        readVkSampledImageIntegerSampleCounts p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, sampledImageIntegerSampleCounts}

        {-# INLINE writeVkSampledImageIntegerSampleCounts #-}
        writeVkSampledImageIntegerSampleCounts p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, sampledImageIntegerSampleCounts}

instance {-# OVERLAPPING #-}
         HasField "sampledImageIntegerSampleCounts" VkPhysicalDeviceLimits
         where
        type FieldType "sampledImageIntegerSampleCounts"
               VkPhysicalDeviceLimits
             = VkSampleCountFlags
        type FieldOptional "sampledImageIntegerSampleCounts"
               VkPhysicalDeviceLimits
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "sampledImageIntegerSampleCounts"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, sampledImageIntegerSampleCounts}
        type FieldIsArray "sampledImageIntegerSampleCounts"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, sampledImageIntegerSampleCounts}

instance CanReadField "sampledImageIntegerSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkSampledImageIntegerSampleCounts

        {-# INLINE readField #-}
        readField = readVkSampledImageIntegerSampleCounts

instance CanWriteField "sampledImageIntegerSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampledImageIntegerSampleCounts

instance {-# OVERLAPPING #-}
         HasVkSampledImageDepthSampleCounts VkPhysicalDeviceLimits where
        type VkSampledImageDepthSampleCountsMType VkPhysicalDeviceLimits =
             VkSampleCountFlags

        {-# NOINLINE vkSampledImageDepthSampleCounts #-}
        vkSampledImageDepthSampleCounts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, sampledImageDepthSampleCounts})

        {-# INLINE vkSampledImageDepthSampleCountsByteOffset #-}
        vkSampledImageDepthSampleCountsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, sampledImageDepthSampleCounts}

        {-# INLINE readVkSampledImageDepthSampleCounts #-}
        readVkSampledImageDepthSampleCounts p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, sampledImageDepthSampleCounts}

        {-# INLINE writeVkSampledImageDepthSampleCounts #-}
        writeVkSampledImageDepthSampleCounts p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, sampledImageDepthSampleCounts}

instance {-# OVERLAPPING #-}
         HasField "sampledImageDepthSampleCounts" VkPhysicalDeviceLimits
         where
        type FieldType "sampledImageDepthSampleCounts"
               VkPhysicalDeviceLimits
             = VkSampleCountFlags
        type FieldOptional "sampledImageDepthSampleCounts"
               VkPhysicalDeviceLimits
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "sampledImageDepthSampleCounts"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, sampledImageDepthSampleCounts}
        type FieldIsArray "sampledImageDepthSampleCounts"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, sampledImageDepthSampleCounts}

instance CanReadField "sampledImageDepthSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkSampledImageDepthSampleCounts

        {-# INLINE readField #-}
        readField = readVkSampledImageDepthSampleCounts

instance CanWriteField "sampledImageDepthSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampledImageDepthSampleCounts

instance {-# OVERLAPPING #-}
         HasVkSampledImageStencilSampleCounts VkPhysicalDeviceLimits where
        type VkSampledImageStencilSampleCountsMType VkPhysicalDeviceLimits
             = VkSampleCountFlags

        {-# NOINLINE vkSampledImageStencilSampleCounts #-}
        vkSampledImageStencilSampleCounts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, sampledImageStencilSampleCounts})

        {-# INLINE vkSampledImageStencilSampleCountsByteOffset #-}
        vkSampledImageStencilSampleCountsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, sampledImageStencilSampleCounts}

        {-# INLINE readVkSampledImageStencilSampleCounts #-}
        readVkSampledImageStencilSampleCounts p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, sampledImageStencilSampleCounts}

        {-# INLINE writeVkSampledImageStencilSampleCounts #-}
        writeVkSampledImageStencilSampleCounts p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, sampledImageStencilSampleCounts}

instance {-# OVERLAPPING #-}
         HasField "sampledImageStencilSampleCounts" VkPhysicalDeviceLimits
         where
        type FieldType "sampledImageStencilSampleCounts"
               VkPhysicalDeviceLimits
             = VkSampleCountFlags
        type FieldOptional "sampledImageStencilSampleCounts"
               VkPhysicalDeviceLimits
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "sampledImageStencilSampleCounts"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, sampledImageStencilSampleCounts}
        type FieldIsArray "sampledImageStencilSampleCounts"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, sampledImageStencilSampleCounts}

instance CanReadField "sampledImageStencilSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkSampledImageStencilSampleCounts

        {-# INLINE readField #-}
        readField = readVkSampledImageStencilSampleCounts

instance CanWriteField "sampledImageStencilSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampledImageStencilSampleCounts

instance {-# OVERLAPPING #-}
         HasVkStorageImageSampleCounts VkPhysicalDeviceLimits where
        type VkStorageImageSampleCountsMType VkPhysicalDeviceLimits =
             VkSampleCountFlags

        {-# NOINLINE vkStorageImageSampleCounts #-}
        vkStorageImageSampleCounts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, storageImageSampleCounts})

        {-# INLINE vkStorageImageSampleCountsByteOffset #-}
        vkStorageImageSampleCountsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, storageImageSampleCounts}

        {-# INLINE readVkStorageImageSampleCounts #-}
        readVkStorageImageSampleCounts p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, storageImageSampleCounts}

        {-# INLINE writeVkStorageImageSampleCounts #-}
        writeVkStorageImageSampleCounts p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, storageImageSampleCounts}

instance {-# OVERLAPPING #-}
         HasField "storageImageSampleCounts" VkPhysicalDeviceLimits where
        type FieldType "storageImageSampleCounts" VkPhysicalDeviceLimits =
             VkSampleCountFlags
        type FieldOptional "storageImageSampleCounts"
               VkPhysicalDeviceLimits
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "storageImageSampleCounts" VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, storageImageSampleCounts}
        type FieldIsArray "storageImageSampleCounts" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, storageImageSampleCounts}

instance CanReadField "storageImageSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkStorageImageSampleCounts

        {-# INLINE readField #-}
        readField = readVkStorageImageSampleCounts

instance CanWriteField "storageImageSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkStorageImageSampleCounts

instance {-# OVERLAPPING #-}
         HasVkMaxSampleMaskWords VkPhysicalDeviceLimits where
        type VkMaxSampleMaskWordsMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxSampleMaskWords #-}
        vkMaxSampleMaskWords x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxSampleMaskWords})

        {-# INLINE vkMaxSampleMaskWordsByteOffset #-}
        vkMaxSampleMaskWordsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxSampleMaskWords}

        {-# INLINE readVkMaxSampleMaskWords #-}
        readVkMaxSampleMaskWords p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxSampleMaskWords}

        {-# INLINE writeVkMaxSampleMaskWords #-}
        writeVkMaxSampleMaskWords p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxSampleMaskWords}

instance {-# OVERLAPPING #-}
         HasField "maxSampleMaskWords" VkPhysicalDeviceLimits where
        type FieldType "maxSampleMaskWords" VkPhysicalDeviceLimits = Word32
        type FieldOptional "maxSampleMaskWords" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSampleMaskWords" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxSampleMaskWords}
        type FieldIsArray "maxSampleMaskWords" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxSampleMaskWords}

instance CanReadField "maxSampleMaskWords" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxSampleMaskWords

        {-# INLINE readField #-}
        readField = readVkMaxSampleMaskWords

instance CanWriteField "maxSampleMaskWords" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxSampleMaskWords

instance {-# OVERLAPPING #-}
         HasVkTimestampComputeAndGraphics VkPhysicalDeviceLimits where
        type VkTimestampComputeAndGraphicsMType VkPhysicalDeviceLimits =
             VkBool32

        {-# NOINLINE vkTimestampComputeAndGraphics #-}
        vkTimestampComputeAndGraphics x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, timestampComputeAndGraphics})

        {-# INLINE vkTimestampComputeAndGraphicsByteOffset #-}
        vkTimestampComputeAndGraphicsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, timestampComputeAndGraphics}

        {-# INLINE readVkTimestampComputeAndGraphics #-}
        readVkTimestampComputeAndGraphics p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, timestampComputeAndGraphics}

        {-# INLINE writeVkTimestampComputeAndGraphics #-}
        writeVkTimestampComputeAndGraphics p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, timestampComputeAndGraphics}

instance {-# OVERLAPPING #-}
         HasField "timestampComputeAndGraphics" VkPhysicalDeviceLimits where
        type FieldType "timestampComputeAndGraphics" VkPhysicalDeviceLimits
             = VkBool32
        type FieldOptional "timestampComputeAndGraphics"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "timestampComputeAndGraphics"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, timestampComputeAndGraphics}
        type FieldIsArray "timestampComputeAndGraphics"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, timestampComputeAndGraphics}

instance CanReadField "timestampComputeAndGraphics"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkTimestampComputeAndGraphics

        {-# INLINE readField #-}
        readField = readVkTimestampComputeAndGraphics

instance CanWriteField "timestampComputeAndGraphics"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkTimestampComputeAndGraphics

instance {-# OVERLAPPING #-}
         HasVkTimestampPeriod VkPhysicalDeviceLimits where
        type VkTimestampPeriodMType VkPhysicalDeviceLimits =
             #{type float}

        {-# NOINLINE vkTimestampPeriod #-}
        vkTimestampPeriod x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, timestampPeriod})

        {-# INLINE vkTimestampPeriodByteOffset #-}
        vkTimestampPeriodByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, timestampPeriod}

        {-# INLINE readVkTimestampPeriod #-}
        readVkTimestampPeriod p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, timestampPeriod}

        {-# INLINE writeVkTimestampPeriod #-}
        writeVkTimestampPeriod p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, timestampPeriod}

instance {-# OVERLAPPING #-}
         HasField "timestampPeriod" VkPhysicalDeviceLimits where
        type FieldType "timestampPeriod" VkPhysicalDeviceLimits =
             #{type float}
        type FieldOptional "timestampPeriod" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "timestampPeriod" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, timestampPeriod}
        type FieldIsArray "timestampPeriod" VkPhysicalDeviceLimits = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, timestampPeriod}

instance CanReadField "timestampPeriod" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkTimestampPeriod

        {-# INLINE readField #-}
        readField = readVkTimestampPeriod

instance CanWriteField "timestampPeriod" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkTimestampPeriod

instance {-# OVERLAPPING #-}
         HasVkMaxClipDistances VkPhysicalDeviceLimits where
        type VkMaxClipDistancesMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxClipDistances #-}
        vkMaxClipDistances x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxClipDistances})

        {-# INLINE vkMaxClipDistancesByteOffset #-}
        vkMaxClipDistancesByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxClipDistances}

        {-# INLINE readVkMaxClipDistances #-}
        readVkMaxClipDistances p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxClipDistances}

        {-# INLINE writeVkMaxClipDistances #-}
        writeVkMaxClipDistances p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxClipDistances}

instance {-# OVERLAPPING #-}
         HasField "maxClipDistances" VkPhysicalDeviceLimits where
        type FieldType "maxClipDistances" VkPhysicalDeviceLimits = Word32
        type FieldOptional "maxClipDistances" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxClipDistances" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxClipDistances}
        type FieldIsArray "maxClipDistances" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxClipDistances}

instance CanReadField "maxClipDistances" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxClipDistances

        {-# INLINE readField #-}
        readField = readVkMaxClipDistances

instance CanWriteField "maxClipDistances" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxClipDistances

instance {-# OVERLAPPING #-}
         HasVkMaxCullDistances VkPhysicalDeviceLimits where
        type VkMaxCullDistancesMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkMaxCullDistances #-}
        vkMaxCullDistances x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxCullDistances})

        {-# INLINE vkMaxCullDistancesByteOffset #-}
        vkMaxCullDistancesByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxCullDistances}

        {-# INLINE readVkMaxCullDistances #-}
        readVkMaxCullDistances p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxCullDistances}

        {-# INLINE writeVkMaxCullDistances #-}
        writeVkMaxCullDistances p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxCullDistances}

instance {-# OVERLAPPING #-}
         HasField "maxCullDistances" VkPhysicalDeviceLimits where
        type FieldType "maxCullDistances" VkPhysicalDeviceLimits = Word32
        type FieldOptional "maxCullDistances" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxCullDistances" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, maxCullDistances}
        type FieldIsArray "maxCullDistances" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxCullDistances}

instance CanReadField "maxCullDistances" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxCullDistances

        {-# INLINE readField #-}
        readField = readVkMaxCullDistances

instance CanWriteField "maxCullDistances" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxCullDistances

instance {-# OVERLAPPING #-}
         HasVkMaxCombinedClipAndCullDistances VkPhysicalDeviceLimits where
        type VkMaxCombinedClipAndCullDistancesMType VkPhysicalDeviceLimits
             = Word32

        {-# NOINLINE vkMaxCombinedClipAndCullDistances #-}
        vkMaxCombinedClipAndCullDistances x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxCombinedClipAndCullDistances})

        {-# INLINE vkMaxCombinedClipAndCullDistancesByteOffset #-}
        vkMaxCombinedClipAndCullDistancesByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, maxCombinedClipAndCullDistances}

        {-# INLINE readVkMaxCombinedClipAndCullDistances #-}
        readVkMaxCombinedClipAndCullDistances p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxCombinedClipAndCullDistances}

        {-# INLINE writeVkMaxCombinedClipAndCullDistances #-}
        writeVkMaxCombinedClipAndCullDistances p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxCombinedClipAndCullDistances}

instance {-# OVERLAPPING #-}
         HasField "maxCombinedClipAndCullDistances" VkPhysicalDeviceLimits
         where
        type FieldType "maxCombinedClipAndCullDistances"
               VkPhysicalDeviceLimits
             = Word32
        type FieldOptional "maxCombinedClipAndCullDistances"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxCombinedClipAndCullDistances"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, maxCombinedClipAndCullDistances}
        type FieldIsArray "maxCombinedClipAndCullDistances"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, maxCombinedClipAndCullDistances}

instance CanReadField "maxCombinedClipAndCullDistances"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkMaxCombinedClipAndCullDistances

        {-# INLINE readField #-}
        readField = readVkMaxCombinedClipAndCullDistances

instance CanWriteField "maxCombinedClipAndCullDistances"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxCombinedClipAndCullDistances

instance {-# OVERLAPPING #-}
         HasVkDiscreteQueuePriorities VkPhysicalDeviceLimits where
        type VkDiscreteQueuePrioritiesMType VkPhysicalDeviceLimits = Word32

        {-# NOINLINE vkDiscreteQueuePriorities #-}
        vkDiscreteQueuePriorities x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, discreteQueuePriorities})

        {-# INLINE vkDiscreteQueuePrioritiesByteOffset #-}
        vkDiscreteQueuePrioritiesByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, discreteQueuePriorities}

        {-# INLINE readVkDiscreteQueuePriorities #-}
        readVkDiscreteQueuePriorities p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, discreteQueuePriorities}

        {-# INLINE writeVkDiscreteQueuePriorities #-}
        writeVkDiscreteQueuePriorities p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, discreteQueuePriorities}

instance {-# OVERLAPPING #-}
         HasField "discreteQueuePriorities" VkPhysicalDeviceLimits where
        type FieldType "discreteQueuePriorities" VkPhysicalDeviceLimits =
             Word32
        type FieldOptional "discreteQueuePriorities" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "discreteQueuePriorities" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, discreteQueuePriorities}
        type FieldIsArray "discreteQueuePriorities" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, discreteQueuePriorities}

instance CanReadField "discreteQueuePriorities"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkDiscreteQueuePriorities

        {-# INLINE readField #-}
        readField = readVkDiscreteQueuePriorities

instance CanWriteField "discreteQueuePriorities"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkDiscreteQueuePriorities

instance {-# OVERLAPPING #-}
         HasVkPointSizeRangeArray VkPhysicalDeviceLimits where
        type VkPointSizeRangeArrayMType VkPhysicalDeviceLimits =
             #{type float}

        {-# NOINLINE vkPointSizeRangeArray #-}
        vkPointSizeRangeArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: #{type float}) +
                    #{offset VkPhysicalDeviceLimits, pointSizeRange}))

        {-# INLINE vkPointSizeRangeArrayByteOffset #-}
        vkPointSizeRangeArrayByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, pointSizeRange}

        {-# INLINE readVkPointSizeRangeArray #-}
        readVkPointSizeRangeArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkPhysicalDeviceLimits, pointSizeRange})

        {-# INLINE writeVkPointSizeRangeArray #-}
        writeVkPointSizeRangeArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkPhysicalDeviceLimits, pointSizeRange})

instance {-# OVERLAPPING #-}
         HasField "pointSizeRange" VkPhysicalDeviceLimits where
        type FieldType "pointSizeRange" VkPhysicalDeviceLimits =
             #{type float}
        type FieldOptional "pointSizeRange" VkPhysicalDeviceLimits = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pointSizeRange" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, pointSizeRange}
        type FieldIsArray "pointSizeRange" VkPhysicalDeviceLimits = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, pointSizeRange}

instance (KnownNat idx,
          IndexInBounds "pointSizeRange" idx VkPhysicalDeviceLimits) =>
         CanReadFieldArray "pointSizeRange" idx VkPhysicalDeviceLimits
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "pointSizeRange" 0 VkPhysicalDeviceLimits #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "pointSizeRange" 1 VkPhysicalDeviceLimits #-}
        type FieldArrayLength "pointSizeRange" VkPhysicalDeviceLimits = 2

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 2

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkPointSizeRangeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkPointSizeRangeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "pointSizeRange" idx VkPhysicalDeviceLimits) =>
         CanWriteFieldArray "pointSizeRange" idx VkPhysicalDeviceLimits
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "pointSizeRange" 0 VkPhysicalDeviceLimits #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "pointSizeRange" 1 VkPhysicalDeviceLimits #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray x
          = writeVkPointSizeRangeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkLineWidthRangeArray VkPhysicalDeviceLimits where
        type VkLineWidthRangeArrayMType VkPhysicalDeviceLimits =
             #{type float}

        {-# NOINLINE vkLineWidthRangeArray #-}
        vkLineWidthRangeArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: #{type float}) +
                    #{offset VkPhysicalDeviceLimits, lineWidthRange}))

        {-# INLINE vkLineWidthRangeArrayByteOffset #-}
        vkLineWidthRangeArrayByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, lineWidthRange}

        {-# INLINE readVkLineWidthRangeArray #-}
        readVkLineWidthRangeArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkPhysicalDeviceLimits, lineWidthRange})

        {-# INLINE writeVkLineWidthRangeArray #-}
        writeVkLineWidthRangeArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkPhysicalDeviceLimits, lineWidthRange})

instance {-# OVERLAPPING #-}
         HasField "lineWidthRange" VkPhysicalDeviceLimits where
        type FieldType "lineWidthRange" VkPhysicalDeviceLimits =
             #{type float}
        type FieldOptional "lineWidthRange" VkPhysicalDeviceLimits = 'False -- ' closing tick for hsc2hs
        type FieldOffset "lineWidthRange" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, lineWidthRange}
        type FieldIsArray "lineWidthRange" VkPhysicalDeviceLimits = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, lineWidthRange}

instance (KnownNat idx,
          IndexInBounds "lineWidthRange" idx VkPhysicalDeviceLimits) =>
         CanReadFieldArray "lineWidthRange" idx VkPhysicalDeviceLimits
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "lineWidthRange" 0 VkPhysicalDeviceLimits #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "lineWidthRange" 1 VkPhysicalDeviceLimits #-}
        type FieldArrayLength "lineWidthRange" VkPhysicalDeviceLimits = 2

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 2

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkLineWidthRangeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkLineWidthRangeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "lineWidthRange" idx VkPhysicalDeviceLimits) =>
         CanWriteFieldArray "lineWidthRange" idx VkPhysicalDeviceLimits
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "lineWidthRange" 0 VkPhysicalDeviceLimits #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "lineWidthRange" 1 VkPhysicalDeviceLimits #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray x
          = writeVkLineWidthRangeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkPointSizeGranularity VkPhysicalDeviceLimits where
        type VkPointSizeGranularityMType VkPhysicalDeviceLimits =
             #{type float}

        {-# NOINLINE vkPointSizeGranularity #-}
        vkPointSizeGranularity x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, pointSizeGranularity})

        {-# INLINE vkPointSizeGranularityByteOffset #-}
        vkPointSizeGranularityByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, pointSizeGranularity}

        {-# INLINE readVkPointSizeGranularity #-}
        readVkPointSizeGranularity p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, pointSizeGranularity}

        {-# INLINE writeVkPointSizeGranularity #-}
        writeVkPointSizeGranularity p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, pointSizeGranularity}

instance {-# OVERLAPPING #-}
         HasField "pointSizeGranularity" VkPhysicalDeviceLimits where
        type FieldType "pointSizeGranularity" VkPhysicalDeviceLimits =
             #{type float}
        type FieldOptional "pointSizeGranularity" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pointSizeGranularity" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, pointSizeGranularity}
        type FieldIsArray "pointSizeGranularity" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, pointSizeGranularity}

instance CanReadField "pointSizeGranularity" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkPointSizeGranularity

        {-# INLINE readField #-}
        readField = readVkPointSizeGranularity

instance CanWriteField "pointSizeGranularity"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkPointSizeGranularity

instance {-# OVERLAPPING #-}
         HasVkLineWidthGranularity VkPhysicalDeviceLimits where
        type VkLineWidthGranularityMType VkPhysicalDeviceLimits =
             #{type float}

        {-# NOINLINE vkLineWidthGranularity #-}
        vkLineWidthGranularity x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, lineWidthGranularity})

        {-# INLINE vkLineWidthGranularityByteOffset #-}
        vkLineWidthGranularityByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, lineWidthGranularity}

        {-# INLINE readVkLineWidthGranularity #-}
        readVkLineWidthGranularity p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, lineWidthGranularity}

        {-# INLINE writeVkLineWidthGranularity #-}
        writeVkLineWidthGranularity p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, lineWidthGranularity}

instance {-# OVERLAPPING #-}
         HasField "lineWidthGranularity" VkPhysicalDeviceLimits where
        type FieldType "lineWidthGranularity" VkPhysicalDeviceLimits =
             #{type float}
        type FieldOptional "lineWidthGranularity" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "lineWidthGranularity" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, lineWidthGranularity}
        type FieldIsArray "lineWidthGranularity" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, lineWidthGranularity}

instance CanReadField "lineWidthGranularity" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkLineWidthGranularity

        {-# INLINE readField #-}
        readField = readVkLineWidthGranularity

instance CanWriteField "lineWidthGranularity"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkLineWidthGranularity

instance {-# OVERLAPPING #-}
         HasVkStrictLines VkPhysicalDeviceLimits where
        type VkStrictLinesMType VkPhysicalDeviceLimits = VkBool32

        {-# NOINLINE vkStrictLines #-}
        vkStrictLines x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, strictLines})

        {-# INLINE vkStrictLinesByteOffset #-}
        vkStrictLinesByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, strictLines}

        {-# INLINE readVkStrictLines #-}
        readVkStrictLines p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, strictLines}

        {-# INLINE writeVkStrictLines #-}
        writeVkStrictLines p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, strictLines}

instance {-# OVERLAPPING #-}
         HasField "strictLines" VkPhysicalDeviceLimits where
        type FieldType "strictLines" VkPhysicalDeviceLimits = VkBool32
        type FieldOptional "strictLines" VkPhysicalDeviceLimits = 'False -- ' closing tick for hsc2hs
        type FieldOffset "strictLines" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, strictLines}
        type FieldIsArray "strictLines" VkPhysicalDeviceLimits = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, strictLines}

instance CanReadField "strictLines" VkPhysicalDeviceLimits where
        {-# INLINE getField #-}
        getField = vkStrictLines

        {-# INLINE readField #-}
        readField = readVkStrictLines

instance CanWriteField "strictLines" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField = writeVkStrictLines

instance {-# OVERLAPPING #-}
         HasVkStandardSampleLocations VkPhysicalDeviceLimits where
        type VkStandardSampleLocationsMType VkPhysicalDeviceLimits =
             VkBool32

        {-# NOINLINE vkStandardSampleLocations #-}
        vkStandardSampleLocations x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, standardSampleLocations})

        {-# INLINE vkStandardSampleLocationsByteOffset #-}
        vkStandardSampleLocationsByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, standardSampleLocations}

        {-# INLINE readVkStandardSampleLocations #-}
        readVkStandardSampleLocations p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, standardSampleLocations}

        {-# INLINE writeVkStandardSampleLocations #-}
        writeVkStandardSampleLocations p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, standardSampleLocations}

instance {-# OVERLAPPING #-}
         HasField "standardSampleLocations" VkPhysicalDeviceLimits where
        type FieldType "standardSampleLocations" VkPhysicalDeviceLimits =
             VkBool32
        type FieldOptional "standardSampleLocations" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "standardSampleLocations" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, standardSampleLocations}
        type FieldIsArray "standardSampleLocations" VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, standardSampleLocations}

instance CanReadField "standardSampleLocations"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkStandardSampleLocations

        {-# INLINE readField #-}
        readField = readVkStandardSampleLocations

instance CanWriteField "standardSampleLocations"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkStandardSampleLocations

instance {-# OVERLAPPING #-}
         HasVkOptimalBufferCopyOffsetAlignment VkPhysicalDeviceLimits where
        type VkOptimalBufferCopyOffsetAlignmentMType VkPhysicalDeviceLimits
             = VkDeviceSize

        {-# NOINLINE vkOptimalBufferCopyOffsetAlignment #-}
        vkOptimalBufferCopyOffsetAlignment x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, optimalBufferCopyOffsetAlignment})

        {-# INLINE vkOptimalBufferCopyOffsetAlignmentByteOffset #-}
        vkOptimalBufferCopyOffsetAlignmentByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, optimalBufferCopyOffsetAlignment}

        {-# INLINE readVkOptimalBufferCopyOffsetAlignment #-}
        readVkOptimalBufferCopyOffsetAlignment p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, optimalBufferCopyOffsetAlignment}

        {-# INLINE writeVkOptimalBufferCopyOffsetAlignment #-}
        writeVkOptimalBufferCopyOffsetAlignment p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, optimalBufferCopyOffsetAlignment}

instance {-# OVERLAPPING #-}
         HasField "optimalBufferCopyOffsetAlignment" VkPhysicalDeviceLimits
         where
        type FieldType "optimalBufferCopyOffsetAlignment"
               VkPhysicalDeviceLimits
             = VkDeviceSize
        type FieldOptional "optimalBufferCopyOffsetAlignment"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "optimalBufferCopyOffsetAlignment"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, optimalBufferCopyOffsetAlignment}
        type FieldIsArray "optimalBufferCopyOffsetAlignment"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, optimalBufferCopyOffsetAlignment}

instance CanReadField "optimalBufferCopyOffsetAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkOptimalBufferCopyOffsetAlignment

        {-# INLINE readField #-}
        readField = readVkOptimalBufferCopyOffsetAlignment

instance CanWriteField "optimalBufferCopyOffsetAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkOptimalBufferCopyOffsetAlignment

instance {-# OVERLAPPING #-}
         HasVkOptimalBufferCopyRowPitchAlignment VkPhysicalDeviceLimits
         where
        type VkOptimalBufferCopyRowPitchAlignmentMType
               VkPhysicalDeviceLimits
             = VkDeviceSize

        {-# NOINLINE vkOptimalBufferCopyRowPitchAlignment #-}
        vkOptimalBufferCopyRowPitchAlignment x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, optimalBufferCopyRowPitchAlignment})

        {-# INLINE vkOptimalBufferCopyRowPitchAlignmentByteOffset #-}
        vkOptimalBufferCopyRowPitchAlignmentByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, optimalBufferCopyRowPitchAlignment}

        {-# INLINE readVkOptimalBufferCopyRowPitchAlignment #-}
        readVkOptimalBufferCopyRowPitchAlignment p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, optimalBufferCopyRowPitchAlignment}

        {-# INLINE writeVkOptimalBufferCopyRowPitchAlignment #-}
        writeVkOptimalBufferCopyRowPitchAlignment p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, optimalBufferCopyRowPitchAlignment}

instance {-# OVERLAPPING #-}
         HasField "optimalBufferCopyRowPitchAlignment"
           VkPhysicalDeviceLimits
         where
        type FieldType "optimalBufferCopyRowPitchAlignment"
               VkPhysicalDeviceLimits
             = VkDeviceSize
        type FieldOptional "optimalBufferCopyRowPitchAlignment"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "optimalBufferCopyRowPitchAlignment"
               VkPhysicalDeviceLimits
             =
             #{offset VkPhysicalDeviceLimits, optimalBufferCopyRowPitchAlignment}
        type FieldIsArray "optimalBufferCopyRowPitchAlignment"
               VkPhysicalDeviceLimits
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, optimalBufferCopyRowPitchAlignment}

instance CanReadField "optimalBufferCopyRowPitchAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkOptimalBufferCopyRowPitchAlignment

        {-# INLINE readField #-}
        readField = readVkOptimalBufferCopyRowPitchAlignment

instance CanWriteField "optimalBufferCopyRowPitchAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkOptimalBufferCopyRowPitchAlignment

instance {-# OVERLAPPING #-}
         HasVkNonCoherentAtomSize VkPhysicalDeviceLimits where
        type VkNonCoherentAtomSizeMType VkPhysicalDeviceLimits =
             VkDeviceSize

        {-# NOINLINE vkNonCoherentAtomSize #-}
        vkNonCoherentAtomSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, nonCoherentAtomSize})

        {-# INLINE vkNonCoherentAtomSizeByteOffset #-}
        vkNonCoherentAtomSizeByteOffset ~_
          = #{offset VkPhysicalDeviceLimits, nonCoherentAtomSize}

        {-# INLINE readVkNonCoherentAtomSize #-}
        readVkNonCoherentAtomSize p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, nonCoherentAtomSize}

        {-# INLINE writeVkNonCoherentAtomSize #-}
        writeVkNonCoherentAtomSize p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, nonCoherentAtomSize}

instance {-# OVERLAPPING #-}
         HasField "nonCoherentAtomSize" VkPhysicalDeviceLimits where
        type FieldType "nonCoherentAtomSize" VkPhysicalDeviceLimits =
             VkDeviceSize
        type FieldOptional "nonCoherentAtomSize" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "nonCoherentAtomSize" VkPhysicalDeviceLimits =
             #{offset VkPhysicalDeviceLimits, nonCoherentAtomSize}
        type FieldIsArray "nonCoherentAtomSize" VkPhysicalDeviceLimits =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceLimits, nonCoherentAtomSize}

instance CanReadField "nonCoherentAtomSize" VkPhysicalDeviceLimits
         where
        {-# INLINE getField #-}
        getField = vkNonCoherentAtomSize

        {-# INLINE readField #-}
        readField = readVkNonCoherentAtomSize

instance CanWriteField "nonCoherentAtomSize" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField = writeVkNonCoherentAtomSize

instance Show VkPhysicalDeviceLimits where
        showsPrec d x
          = showString "VkPhysicalDeviceLimits {" .
              showString "vkMaxImageDimension1D = " .
                showsPrec d (vkMaxImageDimension1D x) .
                  showString ", " .
                    showString "vkMaxImageDimension2D = " .
                      showsPrec d (vkMaxImageDimension2D x) .
                        showString ", " .
                          showString "vkMaxImageDimension3D = " .
                            showsPrec d (vkMaxImageDimension3D x) .
                              showString ", " .
                                showString "vkMaxImageDimensionCube = " .
                                  showsPrec d (vkMaxImageDimensionCube x) .
                                    showString ", " .
                                      showString "vkMaxImageArrayLayers = " .
                                        showsPrec d (vkMaxImageArrayLayers x) .
                                          showString ", " .
                                            showString "vkMaxTexelBufferElements = " .
                                              showsPrec d (vkMaxTexelBufferElements x) .
                                                showString ", " .
                                                  showString "vkMaxUniformBufferRange = " .
                                                    showsPrec d (vkMaxUniformBufferRange x) .
                                                      showString ", " .
                                                        showString "vkMaxStorageBufferRange = " .
                                                          showsPrec d (vkMaxStorageBufferRange x) .
                                                            showString ", " .
                                                              showString "vkMaxPushConstantsSize = "
                                                                .
                                                                showsPrec d
                                                                  (vkMaxPushConstantsSize x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkMaxMemoryAllocationCount = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkMaxMemoryAllocationCount
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "vkMaxSamplerAllocationCount = "
                                                                            .
                                                                            showsPrec d
                                                                              (vkMaxSamplerAllocationCount
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "vkBufferImageGranularity = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (vkBufferImageGranularity
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "vkSparseAddressSpaceSize = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (vkSparseAddressSpaceSize
                                                                                             x)
                                                                                          .
                                                                                          showString
                                                                                            ", "
                                                                                            .
                                                                                            showString
                                                                                              "vkMaxBoundDescriptorSets = "
                                                                                              .
                                                                                              showsPrec
                                                                                                d
                                                                                                (vkMaxBoundDescriptorSets
                                                                                                   x)
                                                                                                .
                                                                                                showString
                                                                                                  ", "
                                                                                                  .
                                                                                                  showString
                                                                                                    "vkMaxPerStageDescriptorSamplers = "
                                                                                                    .
                                                                                                    showsPrec
                                                                                                      d
                                                                                                      (vkMaxPerStageDescriptorSamplers
                                                                                                         x)
                                                                                                      .
                                                                                                      showString
                                                                                                        ", "
                                                                                                        .
                                                                                                        showString
                                                                                                          "vkMaxPerStageDescriptorUniformBuffers = "
                                                                                                          .
                                                                                                          showsPrec
                                                                                                            d
                                                                                                            (vkMaxPerStageDescriptorUniformBuffers
                                                                                                               x)
                                                                                                            .
                                                                                                            showString
                                                                                                              ", "
                                                                                                              .
                                                                                                              showString
                                                                                                                "vkMaxPerStageDescriptorStorageBuffers = "
                                                                                                                .
                                                                                                                showsPrec
                                                                                                                  d
                                                                                                                  (vkMaxPerStageDescriptorStorageBuffers
                                                                                                                     x)
                                                                                                                  .
                                                                                                                  showString
                                                                                                                    ", "
                                                                                                                    .
                                                                                                                    showString
                                                                                                                      "vkMaxPerStageDescriptorSampledImages = "
                                                                                                                      .
                                                                                                                      showsPrec
                                                                                                                        d
                                                                                                                        (vkMaxPerStageDescriptorSampledImages
                                                                                                                           x)
                                                                                                                        .
                                                                                                                        showString
                                                                                                                          ", "
                                                                                                                          .
                                                                                                                          showString
                                                                                                                            "vkMaxPerStageDescriptorStorageImages = "
                                                                                                                            .
                                                                                                                            showsPrec
                                                                                                                              d
                                                                                                                              (vkMaxPerStageDescriptorStorageImages
                                                                                                                                 x)
                                                                                                                              .
                                                                                                                              showString
                                                                                                                                ", "
                                                                                                                                .
                                                                                                                                showString
                                                                                                                                  "vkMaxPerStageDescriptorInputAttachments = "
                                                                                                                                  .
                                                                                                                                  showsPrec
                                                                                                                                    d
                                                                                                                                    (vkMaxPerStageDescriptorInputAttachments
                                                                                                                                       x)
                                                                                                                                    .
                                                                                                                                    showString
                                                                                                                                      ", "
                                                                                                                                      .
                                                                                                                                      showString
                                                                                                                                        "vkMaxPerStageResources = "
                                                                                                                                        .
                                                                                                                                        showsPrec
                                                                                                                                          d
                                                                                                                                          (vkMaxPerStageResources
                                                                                                                                             x)
                                                                                                                                          .
                                                                                                                                          showString
                                                                                                                                            ", "
                                                                                                                                            .
                                                                                                                                            showString
                                                                                                                                              "vkMaxDescriptorSetSamplers = "
                                                                                                                                              .
                                                                                                                                              showsPrec
                                                                                                                                                d
                                                                                                                                                (vkMaxDescriptorSetSamplers
                                                                                                                                                   x)
                                                                                                                                                .
                                                                                                                                                showString
                                                                                                                                                  ", "
                                                                                                                                                  .
                                                                                                                                                  showString
                                                                                                                                                    "vkMaxDescriptorSetUniformBuffers = "
                                                                                                                                                    .
                                                                                                                                                    showsPrec
                                                                                                                                                      d
                                                                                                                                                      (vkMaxDescriptorSetUniformBuffers
                                                                                                                                                         x)
                                                                                                                                                      .
                                                                                                                                                      showString
                                                                                                                                                        ", "
                                                                                                                                                        .
                                                                                                                                                        showString
                                                                                                                                                          "vkMaxDescriptorSetUniformBuffersDynamic = "
                                                                                                                                                          .
                                                                                                                                                          showsPrec
                                                                                                                                                            d
                                                                                                                                                            (vkMaxDescriptorSetUniformBuffersDynamic
                                                                                                                                                               x)
                                                                                                                                                            .
                                                                                                                                                            showString
                                                                                                                                                              ", "
                                                                                                                                                              .
                                                                                                                                                              showString
                                                                                                                                                                "vkMaxDescriptorSetStorageBuffers = "
                                                                                                                                                                .
                                                                                                                                                                showsPrec
                                                                                                                                                                  d
                                                                                                                                                                  (vkMaxDescriptorSetStorageBuffers
                                                                                                                                                                     x)
                                                                                                                                                                  .
                                                                                                                                                                  showString
                                                                                                                                                                    ", "
                                                                                                                                                                    .
                                                                                                                                                                    showString
                                                                                                                                                                      "vkMaxDescriptorSetStorageBuffersDynamic = "
                                                                                                                                                                      .
                                                                                                                                                                      showsPrec
                                                                                                                                                                        d
                                                                                                                                                                        (vkMaxDescriptorSetStorageBuffersDynamic
                                                                                                                                                                           x)
                                                                                                                                                                        .
                                                                                                                                                                        showString
                                                                                                                                                                          ", "
                                                                                                                                                                          .
                                                                                                                                                                          showString
                                                                                                                                                                            "vkMaxDescriptorSetSampledImages = "
                                                                                                                                                                            .
                                                                                                                                                                            showsPrec
                                                                                                                                                                              d
                                                                                                                                                                              (vkMaxDescriptorSetSampledImages
                                                                                                                                                                                 x)
                                                                                                                                                                              .
                                                                                                                                                                              showString
                                                                                                                                                                                ", "
                                                                                                                                                                                .
                                                                                                                                                                                showString
                                                                                                                                                                                  "vkMaxDescriptorSetStorageImages = "
                                                                                                                                                                                  .
                                                                                                                                                                                  showsPrec
                                                                                                                                                                                    d
                                                                                                                                                                                    (vkMaxDescriptorSetStorageImages
                                                                                                                                                                                       x)
                                                                                                                                                                                    .
                                                                                                                                                                                    showString
                                                                                                                                                                                      ", "
                                                                                                                                                                                      .
                                                                                                                                                                                      showString
                                                                                                                                                                                        "vkMaxDescriptorSetInputAttachments = "
                                                                                                                                                                                        .
                                                                                                                                                                                        showsPrec
                                                                                                                                                                                          d
                                                                                                                                                                                          (vkMaxDescriptorSetInputAttachments
                                                                                                                                                                                             x)
                                                                                                                                                                                          .
                                                                                                                                                                                          showString
                                                                                                                                                                                            ", "
                                                                                                                                                                                            .
                                                                                                                                                                                            showString
                                                                                                                                                                                              "vkMaxVertexInputAttributes = "
                                                                                                                                                                                              .
                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                d
                                                                                                                                                                                                (vkMaxVertexInputAttributes
                                                                                                                                                                                                   x)
                                                                                                                                                                                                .
                                                                                                                                                                                                showString
                                                                                                                                                                                                  ", "
                                                                                                                                                                                                  .
                                                                                                                                                                                                  showString
                                                                                                                                                                                                    "vkMaxVertexInputBindings = "
                                                                                                                                                                                                    .
                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                      d
                                                                                                                                                                                                      (vkMaxVertexInputBindings
                                                                                                                                                                                                         x)
                                                                                                                                                                                                      .
                                                                                                                                                                                                      showString
                                                                                                                                                                                                        ", "
                                                                                                                                                                                                        .
                                                                                                                                                                                                        showString
                                                                                                                                                                                                          "vkMaxVertexInputAttributeOffset = "
                                                                                                                                                                                                          .
                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                            d
                                                                                                                                                                                                            (vkMaxVertexInputAttributeOffset
                                                                                                                                                                                                               x)
                                                                                                                                                                                                            .
                                                                                                                                                                                                            showString
                                                                                                                                                                                                              ", "
                                                                                                                                                                                                              .
                                                                                                                                                                                                              showString
                                                                                                                                                                                                                "vkMaxVertexInputBindingStride = "
                                                                                                                                                                                                                .
                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                  d
                                                                                                                                                                                                                  (vkMaxVertexInputBindingStride
                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                  .
                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                    .
                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                      "vkMaxVertexOutputComponents = "
                                                                                                                                                                                                                      .
                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                        d
                                                                                                                                                                                                                        (vkMaxVertexOutputComponents
                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                        .
                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                          .
                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                            "vkMaxTessellationGenerationLevel = "
                                                                                                                                                                                                                            .
                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                              d
                                                                                                                                                                                                                              (vkMaxTessellationGenerationLevel
                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                              .
                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                  "vkMaxTessellationPatchSize = "
                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                    (vkMaxTessellationPatchSize
                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                        "vkMaxTessellationControlPerVertexInputComponents = "
                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                          (vkMaxTessellationControlPerVertexInputComponents
                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                              "vkMaxTessellationControlPerVertexOutputComponents = "
                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                (vkMaxTessellationControlPerVertexOutputComponents
                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                    "vkMaxTessellationControlPerPatchOutputComponents = "
                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                      (vkMaxTessellationControlPerPatchOutputComponents
                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                          "vkMaxTessellationControlTotalOutputComponents = "
                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                            (vkMaxTessellationControlTotalOutputComponents
                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                "vkMaxTessellationEvaluationInputComponents = "
                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                  (vkMaxTessellationEvaluationInputComponents
                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                      "vkMaxTessellationEvaluationOutputComponents = "
                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                        (vkMaxTessellationEvaluationOutputComponents
                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                            "vkMaxGeometryShaderInvocations = "
                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                              (vkMaxGeometryShaderInvocations
                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                  "vkMaxGeometryInputComponents = "
                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                    (vkMaxGeometryInputComponents
                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                        "vkMaxGeometryOutputComponents = "
                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                          (vkMaxGeometryOutputComponents
                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                              "vkMaxGeometryOutputVertices = "
                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                (vkMaxGeometryOutputVertices
                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                    "vkMaxGeometryTotalOutputComponents = "
                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                      (vkMaxGeometryTotalOutputComponents
                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                          "vkMaxFragmentInputComponents = "
                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                                                                            (vkMaxFragmentInputComponents
                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                "vkMaxFragmentOutputAttachments = "
                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                                                                  (vkMaxFragmentOutputAttachments
                                                                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                      "vkMaxFragmentDualSrcAttachments = "
                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                        (vkMaxFragmentDualSrcAttachments
                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                            "vkMaxFragmentCombinedOutputResources = "
                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                              (vkMaxFragmentCombinedOutputResources
                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                  "vkMaxComputeSharedMemorySize = "
                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                                                                    (vkMaxComputeSharedMemorySize
                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                        "vkMaxComputeWorkGroupCountArray = ["
                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                                                                          (map
                                                                                                                                                                                                                                                                                                                                             (vkMaxComputeWorkGroupCountArray
                                                                                                                                                                                                                                                                                                                                                x)
                                                                                                                                                                                                                                                                                                                                             [1
                                                                                                                                                                                                                                                                                                                                              ..
                                                                                                                                                                                                                                                                                                                                              3])
                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                          showChar
                                                                                                                                                                                                                                                                                                                                            ']'
                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                "vkMaxComputeWorkGroupInvocations = "
                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                                                                                                  (vkMaxComputeWorkGroupInvocations
                                                                                                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                      "vkMaxComputeWorkGroupSizeArray = ["
                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                                                        (map
                                                                                                                                                                                                                                                                                                                                                           (vkMaxComputeWorkGroupSizeArray
                                                                                                                                                                                                                                                                                                                                                              x)
                                                                                                                                                                                                                                                                                                                                                           [1
                                                                                                                                                                                                                                                                                                                                                            ..
                                                                                                                                                                                                                                                                                                                                                            3])
                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                        showChar
                                                                                                                                                                                                                                                                                                                                                          ']'
                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                              "vkSubPixelPrecisionBits = "
                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                                                                                (vkSubPixelPrecisionBits
                                                                                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                    "vkSubTexelPrecisionBits = "
                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                                                                                      (vkSubTexelPrecisionBits
                                                                                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                          "vkMipmapPrecisionBits = "
                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                                                                                                                                            (vkMipmapPrecisionBits
                                                                                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                "vkMaxDrawIndexedIndexValue = "
                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                                                                                                                                  (vkMaxDrawIndexedIndexValue
                                                                                                                                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                      "vkMaxDrawIndirectCount = "
                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                                                                                        (vkMaxDrawIndirectCount
                                                                                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                            "vkMaxSamplerLodBias = "
                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                                                                                              (vkMaxSamplerLodBias
                                                                                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                  "vkMaxSamplerAnisotropy = "
                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                                                                                                                                    (vkMaxSamplerAnisotropy
                                                                                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                        "vkMaxViewports = "
                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                                                                                                                                          (vkMaxViewports
                                                                                                                                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                              "vkMaxViewportDimensionsArray = ["
                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                                                                                                                                (map
                                                                                                                                                                                                                                                                                                                                                                                                                   (vkMaxViewportDimensionsArray
                                                                                                                                                                                                                                                                                                                                                                                                                      x)
                                                                                                                                                                                                                                                                                                                                                                                                                   [1
                                                                                                                                                                                                                                                                                                                                                                                                                    ..
                                                                                                                                                                                                                                                                                                                                                                                                                    2])
                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                showChar
                                                                                                                                                                                                                                                                                                                                                                                                                  ']'
                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                      "vkViewportBoundsRangeArray = ["
                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                                                                                                                        (map
                                                                                                                                                                                                                                                                                                                                                                                                                           (vkViewportBoundsRangeArray
                                                                                                                                                                                                                                                                                                                                                                                                                              x)
                                                                                                                                                                                                                                                                                                                                                                                                                           [1
                                                                                                                                                                                                                                                                                                                                                                                                                            ..
                                                                                                                                                                                                                                                                                                                                                                                                                            2])
                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                        showChar
                                                                                                                                                                                                                                                                                                                                                                                                                          ']'
                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                              "vkViewportSubPixelBits = "
                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                                                                                                                                                (vkViewportSubPixelBits
                                                                                                                                                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                    "vkMinMemoryMapAlignment = "
                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                                                                                                                                                      (vkMinMemoryMapAlignment
                                                                                                                                                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                          "vkMinTexelBufferOffsetAlignment = "
                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                                                                                                                                                                                                            (vkMinTexelBufferOffsetAlignment
                                                                                                                                                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                "vkMinUniformBufferOffsetAlignment = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                                                                                                                                                                                                  (vkMinUniformBufferOffsetAlignment
                                                                                                                                                                                                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                      "vkMinStorageBufferOffsetAlignment = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                                                                                                                                                        (vkMinStorageBufferOffsetAlignment
                                                                                                                                                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                            "vkMinTexelOffset = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                                                                                                                                                              (vkMinTexelOffset
                                                                                                                                                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "vkMaxTexelOffset = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (vkMaxTexelOffset
                                                                                                                                                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "vkMinTexelGatherOffset = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (vkMinTexelGatherOffset
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "vkMaxTexelGatherOffset = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (vkMaxTexelGatherOffset
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "vkMinInterpolationOffset = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (vkMinInterpolationOffset
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "vkMaxInterpolationOffset = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (vkMaxInterpolationOffset
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "vkSubPixelInterpolationOffsetBits = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (vkSubPixelInterpolationOffsetBits
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "vkMaxFramebufferWidth = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (vkMaxFramebufferWidth
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "vkMaxFramebufferHeight = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (vkMaxFramebufferHeight
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "vkMaxFramebufferLayers = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (vkMaxFramebufferLayers
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "vkFramebufferColorSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (vkFramebufferColorSampleCounts
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "vkFramebufferDepthSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (vkFramebufferDepthSampleCounts
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "vkFramebufferStencilSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (vkFramebufferStencilSampleCounts
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "vkFramebufferNoAttachmentsSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (vkFramebufferNoAttachmentsSampleCounts
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "vkMaxColorAttachments = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (vkMaxColorAttachments
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "vkSampledImageColorSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (vkSampledImageColorSampleCounts
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "vkSampledImageIntegerSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (vkSampledImageIntegerSampleCounts
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "vkSampledImageDepthSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (vkSampledImageDepthSampleCounts
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "vkSampledImageStencilSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (vkSampledImageStencilSampleCounts
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "vkStorageImageSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (vkStorageImageSampleCounts
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "vkMaxSampleMaskWords = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (vkMaxSampleMaskWords
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "vkTimestampComputeAndGraphics = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (vkTimestampComputeAndGraphics
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "vkTimestampPeriod = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (vkTimestampPeriod
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "vkMaxClipDistances = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (vkMaxClipDistances
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "vkMaxCullDistances = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (vkMaxCullDistances
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "vkMaxCombinedClipAndCullDistances = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (vkMaxCombinedClipAndCullDistances
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "vkDiscreteQueuePriorities = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (vkDiscreteQueuePriorities
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "vkPointSizeRangeArray = ["
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (map
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   (vkPointSizeRangeArray
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   [1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ..
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    2])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showChar
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ']'
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "vkLineWidthRangeArray = ["
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (map
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           (vkLineWidthRangeArray
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           [1
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ..
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            2])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showChar
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ']'
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "vkPointSizeGranularity = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (vkPointSizeGranularity
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "vkLineWidthGranularity = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (vkLineWidthGranularity
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "vkStrictLines = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (vkStrictLines
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "vkStandardSampleLocations = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (vkStandardSampleLocations
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "vkOptimalBufferCopyOffsetAlignment = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (vkOptimalBufferCopyOffsetAlignment
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "vkOptimalBufferCopyRowPitchAlignment = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (vkOptimalBufferCopyRowPitchAlignment
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "vkNonCoherentAtomSize = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (vkNonCoherentAtomSize
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showChar
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      '}'
