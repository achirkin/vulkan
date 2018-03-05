#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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

instance {-# OVERLAPPING #-}
         CanReadField "maxImageDimension1D" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxImageDimension1D})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxImageDimension1D}

instance {-# OVERLAPPING #-}
         CanWriteField "maxImageDimension1D" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxImageDimension1D}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxImageDimension2D" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxImageDimension2D})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxImageDimension2D}

instance {-# OVERLAPPING #-}
         CanWriteField "maxImageDimension2D" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxImageDimension2D}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxImageDimension3D" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxImageDimension3D})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxImageDimension3D}

instance {-# OVERLAPPING #-}
         CanWriteField "maxImageDimension3D" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxImageDimension3D}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxImageDimensionCube" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxImageDimensionCube})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxImageDimensionCube}

instance {-# OVERLAPPING #-}
         CanWriteField "maxImageDimensionCube" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxImageDimensionCube}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxImageArrayLayers" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxImageArrayLayers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxImageArrayLayers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxImageArrayLayers" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxImageArrayLayers}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxTexelBufferElements" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTexelBufferElements})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTexelBufferElements}

instance {-# OVERLAPPING #-}
         CanWriteField "maxTexelBufferElements" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTexelBufferElements}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxUniformBufferRange" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxUniformBufferRange})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxUniformBufferRange}

instance {-# OVERLAPPING #-}
         CanWriteField "maxUniformBufferRange" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxUniformBufferRange}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxStorageBufferRange" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxStorageBufferRange})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxStorageBufferRange}

instance {-# OVERLAPPING #-}
         CanWriteField "maxStorageBufferRange" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxStorageBufferRange}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxPushConstantsSize" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxPushConstantsSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxPushConstantsSize}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPushConstantsSize" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxPushConstantsSize}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxMemoryAllocationCount" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxMemoryAllocationCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxMemoryAllocationCount}

instance {-# OVERLAPPING #-}
         CanWriteField "maxMemoryAllocationCount" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxMemoryAllocationCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxSamplerAllocationCount" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxSamplerAllocationCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxSamplerAllocationCount}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSamplerAllocationCount" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxSamplerAllocationCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "bufferImageGranularity" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, bufferImageGranularity})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, bufferImageGranularity}

instance {-# OVERLAPPING #-}
         CanWriteField "bufferImageGranularity" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, bufferImageGranularity}

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

instance {-# OVERLAPPING #-}
         CanReadField "sparseAddressSpaceSize" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, sparseAddressSpaceSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, sparseAddressSpaceSize}

instance {-# OVERLAPPING #-}
         CanWriteField "sparseAddressSpaceSize" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, sparseAddressSpaceSize}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxBoundDescriptorSets" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxBoundDescriptorSets})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxBoundDescriptorSets}

instance {-# OVERLAPPING #-}
         CanWriteField "maxBoundDescriptorSets" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxBoundDescriptorSets}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxPerStageDescriptorSamplers" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSamplers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSamplers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPerStageDescriptorSamplers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSamplers}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxPerStageDescriptorUniformBuffers"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorUniformBuffers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorUniformBuffers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPerStageDescriptorUniformBuffers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorUniformBuffers}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxPerStageDescriptorStorageBuffers"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageBuffers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageBuffers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPerStageDescriptorStorageBuffers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageBuffers}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxPerStageDescriptorSampledImages"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSampledImages})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSampledImages}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPerStageDescriptorSampledImages"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSampledImages}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxPerStageDescriptorStorageImages"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageImages})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageImages}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPerStageDescriptorStorageImages"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageImages}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxPerStageDescriptorInputAttachments"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorInputAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorInputAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPerStageDescriptorInputAttachments"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorInputAttachments}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxPerStageResources" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxPerStageResources})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageResources}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPerStageResources" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxPerStageResources}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxDescriptorSetSamplers" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDescriptorSetSamplers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetSamplers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDescriptorSetSamplers" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetSamplers}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxDescriptorSetUniformBuffers"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDescriptorSetUniformBuffers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffers}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxDescriptorSetUniformBuffersDynamic"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffersDynamic})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffersDynamic}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDescriptorSetUniformBuffersDynamic"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffersDynamic}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxDescriptorSetStorageBuffers"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDescriptorSetStorageBuffers"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffers}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxDescriptorSetStorageBuffersDynamic"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffersDynamic})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffersDynamic}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDescriptorSetStorageBuffersDynamic"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffersDynamic}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxDescriptorSetSampledImages" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDescriptorSetSampledImages})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetSampledImages}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDescriptorSetSampledImages"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetSampledImages}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxDescriptorSetStorageImages" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageImages})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageImages}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDescriptorSetStorageImages"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageImages}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxDescriptorSetInputAttachments"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDescriptorSetInputAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetInputAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDescriptorSetInputAttachments"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDescriptorSetInputAttachments}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxVertexInputAttributes" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxVertexInputAttributes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxVertexInputAttributes}

instance {-# OVERLAPPING #-}
         CanWriteField "maxVertexInputAttributes" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxVertexInputAttributes}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxVertexInputBindings" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxVertexInputBindings})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxVertexInputBindings}

instance {-# OVERLAPPING #-}
         CanWriteField "maxVertexInputBindings" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxVertexInputBindings}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxVertexInputAttributeOffset" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxVertexInputAttributeOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxVertexInputAttributeOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "maxVertexInputAttributeOffset"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxVertexInputAttributeOffset}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxVertexInputBindingStride" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxVertexInputBindingStride})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxVertexInputBindingStride}

instance {-# OVERLAPPING #-}
         CanWriteField "maxVertexInputBindingStride" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxVertexInputBindingStride}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxVertexOutputComponents" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxVertexOutputComponents})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxVertexOutputComponents}

instance {-# OVERLAPPING #-}
         CanWriteField "maxVertexOutputComponents" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxVertexOutputComponents}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxTessellationGenerationLevel"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTessellationGenerationLevel})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationGenerationLevel}

instance {-# OVERLAPPING #-}
         CanWriteField "maxTessellationGenerationLevel"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationGenerationLevel}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxTessellationPatchSize" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTessellationPatchSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationPatchSize}

instance {-# OVERLAPPING #-}
         CanWriteField "maxTessellationPatchSize" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationPatchSize}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxTessellationControlPerVertexInputComponents"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexInputComponents})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexInputComponents}

instance {-# OVERLAPPING #-}
         CanWriteField "maxTessellationControlPerVertexInputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexInputComponents}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxTessellationControlPerVertexOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexOutputComponents})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexOutputComponents}

instance {-# OVERLAPPING #-}
         CanWriteField "maxTessellationControlPerVertexOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexOutputComponents}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxTessellationControlPerPatchOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTessellationControlPerPatchOutputComponents})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationControlPerPatchOutputComponents}

instance {-# OVERLAPPING #-}
         CanWriteField "maxTessellationControlPerPatchOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationControlPerPatchOutputComponents}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxTessellationControlTotalOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTessellationControlTotalOutputComponents})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationControlTotalOutputComponents}

instance {-# OVERLAPPING #-}
         CanWriteField "maxTessellationControlTotalOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationControlTotalOutputComponents}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxTessellationEvaluationInputComponents"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationInputComponents})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationInputComponents}

instance {-# OVERLAPPING #-}
         CanWriteField "maxTessellationEvaluationInputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationInputComponents}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxTessellationEvaluationOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationOutputComponents})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationOutputComponents}

instance {-# OVERLAPPING #-}
         CanWriteField "maxTessellationEvaluationOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationOutputComponents}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxGeometryShaderInvocations" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxGeometryShaderInvocations})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryShaderInvocations}

instance {-# OVERLAPPING #-}
         CanWriteField "maxGeometryShaderInvocations" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryShaderInvocations}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxGeometryInputComponents" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxGeometryInputComponents})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryInputComponents}

instance {-# OVERLAPPING #-}
         CanWriteField "maxGeometryInputComponents" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryInputComponents}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxGeometryOutputComponents" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxGeometryOutputComponents})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryOutputComponents}

instance {-# OVERLAPPING #-}
         CanWriteField "maxGeometryOutputComponents" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryOutputComponents}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxGeometryOutputVertices" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxGeometryOutputVertices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryOutputVertices}

instance {-# OVERLAPPING #-}
         CanWriteField "maxGeometryOutputVertices" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryOutputVertices}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxGeometryTotalOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxGeometryTotalOutputComponents})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryTotalOutputComponents}

instance {-# OVERLAPPING #-}
         CanWriteField "maxGeometryTotalOutputComponents"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxGeometryTotalOutputComponents}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxFragmentInputComponents" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxFragmentInputComponents})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxFragmentInputComponents}

instance {-# OVERLAPPING #-}
         CanWriteField "maxFragmentInputComponents" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxFragmentInputComponents}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxFragmentOutputAttachments" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxFragmentOutputAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxFragmentOutputAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField "maxFragmentOutputAttachments" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxFragmentOutputAttachments}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxFragmentDualSrcAttachments" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxFragmentDualSrcAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxFragmentDualSrcAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField "maxFragmentDualSrcAttachments"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxFragmentDualSrcAttachments}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxFragmentCombinedOutputResources"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxFragmentCombinedOutputResources})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxFragmentCombinedOutputResources}

instance {-# OVERLAPPING #-}
         CanWriteField "maxFragmentCombinedOutputResources"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxFragmentCombinedOutputResources}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxComputeSharedMemorySize" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxComputeSharedMemorySize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxComputeSharedMemorySize}

instance {-# OVERLAPPING #-}
         CanWriteField "maxComputeSharedMemorySize" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxComputeSharedMemorySize}

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

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupCount}
                      +
                      sizeOf (undefined :: Word32) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceLimits, maxComputeWorkGroupCount}
                 +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceLimits, maxComputeWorkGroupCount}
                 +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         CanReadField "maxComputeWorkGroupInvocations"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupInvocations})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupInvocations}

instance {-# OVERLAPPING #-}
         CanWriteField "maxComputeWorkGroupInvocations"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupInvocations}

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

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupSize}
                      +
                      sizeOf (undefined :: Word32) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceLimits, maxComputeWorkGroupSize}
                 +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceLimits, maxComputeWorkGroupSize}
                 +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         CanReadField "subPixelPrecisionBits" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, subPixelPrecisionBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, subPixelPrecisionBits}

instance {-# OVERLAPPING #-}
         CanWriteField "subPixelPrecisionBits" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, subPixelPrecisionBits}

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

instance {-# OVERLAPPING #-}
         CanReadField "subTexelPrecisionBits" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, subTexelPrecisionBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, subTexelPrecisionBits}

instance {-# OVERLAPPING #-}
         CanWriteField "subTexelPrecisionBits" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, subTexelPrecisionBits}

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

instance {-# OVERLAPPING #-}
         CanReadField "mipmapPrecisionBits" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, mipmapPrecisionBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, mipmapPrecisionBits}

instance {-# OVERLAPPING #-}
         CanWriteField "mipmapPrecisionBits" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, mipmapPrecisionBits}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxDrawIndexedIndexValue" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDrawIndexedIndexValue})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDrawIndexedIndexValue}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDrawIndexedIndexValue" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDrawIndexedIndexValue}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxDrawIndirectCount" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxDrawIndirectCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxDrawIndirectCount}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDrawIndirectCount" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxDrawIndirectCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxSamplerLodBias" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxSamplerLodBias})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxSamplerLodBias}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSamplerLodBias" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxSamplerLodBias}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxSamplerAnisotropy" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxSamplerAnisotropy})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxSamplerAnisotropy}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSamplerAnisotropy" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxSamplerAnisotropy}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxViewports" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxViewports})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxViewports}

instance {-# OVERLAPPING #-}
         CanWriteField "maxViewports" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxViewports}

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

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceLimits, maxViewportDimensions}
                      +
                      sizeOf (undefined :: Word32) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceLimits, maxViewportDimensions}
                 +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceLimits, maxViewportDimensions}
                 +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceLimits, viewportBoundsRange}
                      +
                      sizeOf (undefined :: #{type float}) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceLimits, viewportBoundsRange}
                 +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceLimits, viewportBoundsRange}
                 +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         CanReadField "viewportSubPixelBits" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, viewportSubPixelBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, viewportSubPixelBits}

instance {-# OVERLAPPING #-}
         CanWriteField "viewportSubPixelBits" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, viewportSubPixelBits}

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

instance {-# OVERLAPPING #-}
         CanReadField "minMemoryMapAlignment" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, minMemoryMapAlignment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, minMemoryMapAlignment}

instance {-# OVERLAPPING #-}
         CanWriteField "minMemoryMapAlignment" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, minMemoryMapAlignment}

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

instance {-# OVERLAPPING #-}
         CanReadField "minTexelBufferOffsetAlignment" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, minTexelBufferOffsetAlignment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, minTexelBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         CanWriteField "minTexelBufferOffsetAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, minTexelBufferOffsetAlignment}

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

instance {-# OVERLAPPING #-}
         CanReadField "minUniformBufferOffsetAlignment"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, minUniformBufferOffsetAlignment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, minUniformBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         CanWriteField "minUniformBufferOffsetAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, minUniformBufferOffsetAlignment}

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

instance {-# OVERLAPPING #-}
         CanReadField "minStorageBufferOffsetAlignment"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, minStorageBufferOffsetAlignment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, minStorageBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         CanWriteField "minStorageBufferOffsetAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, minStorageBufferOffsetAlignment}

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

instance {-# OVERLAPPING #-}
         CanReadField "minTexelOffset" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, minTexelOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, minTexelOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "minTexelOffset" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, minTexelOffset}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxTexelOffset" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTexelOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTexelOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "maxTexelOffset" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTexelOffset}

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

instance {-# OVERLAPPING #-}
         CanReadField "minTexelGatherOffset" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, minTexelGatherOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, minTexelGatherOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "minTexelGatherOffset" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, minTexelGatherOffset}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxTexelGatherOffset" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxTexelGatherOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxTexelGatherOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "maxTexelGatherOffset" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxTexelGatherOffset}

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

instance {-# OVERLAPPING #-}
         CanReadField "minInterpolationOffset" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, minInterpolationOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, minInterpolationOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "minInterpolationOffset" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, minInterpolationOffset}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxInterpolationOffset" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxInterpolationOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxInterpolationOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "maxInterpolationOffset" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxInterpolationOffset}

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

instance {-# OVERLAPPING #-}
         CanReadField "subPixelInterpolationOffsetBits"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, subPixelInterpolationOffsetBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, subPixelInterpolationOffsetBits}

instance {-# OVERLAPPING #-}
         CanWriteField "subPixelInterpolationOffsetBits"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, subPixelInterpolationOffsetBits}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxFramebufferWidth" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxFramebufferWidth})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxFramebufferWidth}

instance {-# OVERLAPPING #-}
         CanWriteField "maxFramebufferWidth" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxFramebufferWidth}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxFramebufferHeight" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxFramebufferHeight})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxFramebufferHeight}

instance {-# OVERLAPPING #-}
         CanWriteField "maxFramebufferHeight" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxFramebufferHeight}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxFramebufferLayers" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxFramebufferLayers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxFramebufferLayers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxFramebufferLayers" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxFramebufferLayers}

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

instance {-# OVERLAPPING #-}
         CanReadField "framebufferColorSampleCounts" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, framebufferColorSampleCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, framebufferColorSampleCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "framebufferColorSampleCounts" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, framebufferColorSampleCounts}

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

instance {-# OVERLAPPING #-}
         CanReadField "framebufferDepthSampleCounts" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, framebufferDepthSampleCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, framebufferDepthSampleCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "framebufferDepthSampleCounts" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, framebufferDepthSampleCounts}

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

instance {-# OVERLAPPING #-}
         CanReadField "framebufferStencilSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, framebufferStencilSampleCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, framebufferStencilSampleCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "framebufferStencilSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, framebufferStencilSampleCounts}

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

instance {-# OVERLAPPING #-}
         CanReadField "framebufferNoAttachmentsSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, framebufferNoAttachmentsSampleCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, framebufferNoAttachmentsSampleCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "framebufferNoAttachmentsSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, framebufferNoAttachmentsSampleCounts}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxColorAttachments" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxColorAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxColorAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField "maxColorAttachments" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxColorAttachments}

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

instance {-# OVERLAPPING #-}
         CanReadField "sampledImageColorSampleCounts" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, sampledImageColorSampleCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, sampledImageColorSampleCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "sampledImageColorSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, sampledImageColorSampleCounts}

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

instance {-# OVERLAPPING #-}
         CanReadField "sampledImageIntegerSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, sampledImageIntegerSampleCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, sampledImageIntegerSampleCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "sampledImageIntegerSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, sampledImageIntegerSampleCounts}

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

instance {-# OVERLAPPING #-}
         CanReadField "sampledImageDepthSampleCounts" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, sampledImageDepthSampleCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, sampledImageDepthSampleCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "sampledImageDepthSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, sampledImageDepthSampleCounts}

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

instance {-# OVERLAPPING #-}
         CanReadField "sampledImageStencilSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, sampledImageStencilSampleCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, sampledImageStencilSampleCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "sampledImageStencilSampleCounts"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, sampledImageStencilSampleCounts}

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

instance {-# OVERLAPPING #-}
         CanReadField "storageImageSampleCounts" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, storageImageSampleCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, storageImageSampleCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "storageImageSampleCounts" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, storageImageSampleCounts}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxSampleMaskWords" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxSampleMaskWords})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxSampleMaskWords}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSampleMaskWords" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxSampleMaskWords}

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

instance {-# OVERLAPPING #-}
         CanReadField "timestampComputeAndGraphics" VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, timestampComputeAndGraphics})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, timestampComputeAndGraphics}

instance {-# OVERLAPPING #-}
         CanWriteField "timestampComputeAndGraphics" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, timestampComputeAndGraphics}

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

instance {-# OVERLAPPING #-}
         CanReadField "timestampPeriod" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, timestampPeriod})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, timestampPeriod}

instance {-# OVERLAPPING #-}
         CanWriteField "timestampPeriod" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, timestampPeriod}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxClipDistances" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxClipDistances})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxClipDistances}

instance {-# OVERLAPPING #-}
         CanWriteField "maxClipDistances" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxClipDistances}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxCullDistances" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxCullDistances})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxCullDistances}

instance {-# OVERLAPPING #-}
         CanWriteField "maxCullDistances" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxCullDistances}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxCombinedClipAndCullDistances"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, maxCombinedClipAndCullDistances})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, maxCombinedClipAndCullDistances}

instance {-# OVERLAPPING #-}
         CanWriteField "maxCombinedClipAndCullDistances"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, maxCombinedClipAndCullDistances}

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

instance {-# OVERLAPPING #-}
         CanReadField "discreteQueuePriorities" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, discreteQueuePriorities})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, discreteQueuePriorities}

instance {-# OVERLAPPING #-}
         CanWriteField "discreteQueuePriorities" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, discreteQueuePriorities}

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

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceLimits, pointSizeRange} +
                      sizeOf (undefined :: #{type float}) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceLimits, pointSizeRange} +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "pointSizeRange" idx VkPhysicalDeviceLimits) =>
         CanWriteFieldArray "pointSizeRange" idx VkPhysicalDeviceLimits
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "pointSizeRange" 0 VkPhysicalDeviceLimits #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "pointSizeRange" 1 VkPhysicalDeviceLimits #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceLimits, pointSizeRange} +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceLimits, lineWidthRange} +
                      sizeOf (undefined :: #{type float}) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceLimits, lineWidthRange} +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "lineWidthRange" idx VkPhysicalDeviceLimits) =>
         CanWriteFieldArray "lineWidthRange" idx VkPhysicalDeviceLimits
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "lineWidthRange" 0 VkPhysicalDeviceLimits #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "lineWidthRange" 1 VkPhysicalDeviceLimits #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceLimits, lineWidthRange} +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         CanReadField "pointSizeGranularity" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, pointSizeGranularity})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, pointSizeGranularity}

instance {-# OVERLAPPING #-}
         CanWriteField "pointSizeGranularity" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, pointSizeGranularity}

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

instance {-# OVERLAPPING #-}
         CanReadField "lineWidthGranularity" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, lineWidthGranularity})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, lineWidthGranularity}

instance {-# OVERLAPPING #-}
         CanWriteField "lineWidthGranularity" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, lineWidthGranularity}

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

instance {-# OVERLAPPING #-}
         CanReadField "strictLines" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, strictLines})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, strictLines}

instance {-# OVERLAPPING #-}
         CanWriteField "strictLines" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, strictLines}

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

instance {-# OVERLAPPING #-}
         CanReadField "standardSampleLocations" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, standardSampleLocations})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, standardSampleLocations}

instance {-# OVERLAPPING #-}
         CanWriteField "standardSampleLocations" VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, standardSampleLocations}

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

instance {-# OVERLAPPING #-}
         CanReadField "optimalBufferCopyOffsetAlignment"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, optimalBufferCopyOffsetAlignment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, optimalBufferCopyOffsetAlignment}

instance {-# OVERLAPPING #-}
         CanWriteField "optimalBufferCopyOffsetAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, optimalBufferCopyOffsetAlignment}

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

instance {-# OVERLAPPING #-}
         CanReadField "optimalBufferCopyRowPitchAlignment"
           VkPhysicalDeviceLimits
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, optimalBufferCopyRowPitchAlignment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, optimalBufferCopyRowPitchAlignment}

instance {-# OVERLAPPING #-}
         CanWriteField "optimalBufferCopyRowPitchAlignment"
           VkPhysicalDeviceLimits
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, optimalBufferCopyRowPitchAlignment}

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

instance {-# OVERLAPPING #-}
         CanReadField "nonCoherentAtomSize" VkPhysicalDeviceLimits where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceLimits, nonCoherentAtomSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceLimits, nonCoherentAtomSize}

instance {-# OVERLAPPING #-}
         CanWriteField "nonCoherentAtomSize" VkPhysicalDeviceLimits where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceLimits, nonCoherentAtomSize}

instance Show VkPhysicalDeviceLimits where
        showsPrec d x
          = showString "VkPhysicalDeviceLimits {" .
              showString "maxImageDimension1D = " .
                showsPrec d (getField @"maxImageDimension1D" x) .
                  showString ", " .
                    showString "maxImageDimension2D = " .
                      showsPrec d (getField @"maxImageDimension2D" x) .
                        showString ", " .
                          showString "maxImageDimension3D = " .
                            showsPrec d (getField @"maxImageDimension3D" x) .
                              showString ", " .
                                showString "maxImageDimensionCube = " .
                                  showsPrec d (getField @"maxImageDimensionCube" x) .
                                    showString ", " .
                                      showString "maxImageArrayLayers = " .
                                        showsPrec d (getField @"maxImageArrayLayers" x) .
                                          showString ", " .
                                            showString "maxTexelBufferElements = " .
                                              showsPrec d (getField @"maxTexelBufferElements" x) .
                                                showString ", " .
                                                  showString "maxUniformBufferRange = " .
                                                    showsPrec d
                                                      (getField @"maxUniformBufferRange" x)
                                                      .
                                                      showString ", " .
                                                        showString "maxStorageBufferRange = " .
                                                          showsPrec d
                                                            (getField @"maxStorageBufferRange" x)
                                                            .
                                                            showString ", " .
                                                              showString "maxPushConstantsSize = " .
                                                                showsPrec d
                                                                  (getField @"maxPushConstantsSize"
                                                                     x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "maxMemoryAllocationCount = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"maxMemoryAllocationCount"
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "maxSamplerAllocationCount = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"maxSamplerAllocationCount"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "bufferImageGranularity = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"bufferImageGranularity"
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "sparseAddressSpaceSize = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (getField
                                                                                             @"sparseAddressSpaceSize"
                                                                                             x)
                                                                                          .
                                                                                          showString
                                                                                            ", "
                                                                                            .
                                                                                            showString
                                                                                              "maxBoundDescriptorSets = "
                                                                                              .
                                                                                              showsPrec
                                                                                                d
                                                                                                (getField
                                                                                                   @"maxBoundDescriptorSets"
                                                                                                   x)
                                                                                                .
                                                                                                showString
                                                                                                  ", "
                                                                                                  .
                                                                                                  showString
                                                                                                    "maxPerStageDescriptorSamplers = "
                                                                                                    .
                                                                                                    showsPrec
                                                                                                      d
                                                                                                      (getField
                                                                                                         @"maxPerStageDescriptorSamplers"
                                                                                                         x)
                                                                                                      .
                                                                                                      showString
                                                                                                        ", "
                                                                                                        .
                                                                                                        showString
                                                                                                          "maxPerStageDescriptorUniformBuffers = "
                                                                                                          .
                                                                                                          showsPrec
                                                                                                            d
                                                                                                            (getField
                                                                                                               @"maxPerStageDescriptorUniformBuffers"
                                                                                                               x)
                                                                                                            .
                                                                                                            showString
                                                                                                              ", "
                                                                                                              .
                                                                                                              showString
                                                                                                                "maxPerStageDescriptorStorageBuffers = "
                                                                                                                .
                                                                                                                showsPrec
                                                                                                                  d
                                                                                                                  (getField
                                                                                                                     @"maxPerStageDescriptorStorageBuffers"
                                                                                                                     x)
                                                                                                                  .
                                                                                                                  showString
                                                                                                                    ", "
                                                                                                                    .
                                                                                                                    showString
                                                                                                                      "maxPerStageDescriptorSampledImages = "
                                                                                                                      .
                                                                                                                      showsPrec
                                                                                                                        d
                                                                                                                        (getField
                                                                                                                           @"maxPerStageDescriptorSampledImages"
                                                                                                                           x)
                                                                                                                        .
                                                                                                                        showString
                                                                                                                          ", "
                                                                                                                          .
                                                                                                                          showString
                                                                                                                            "maxPerStageDescriptorStorageImages = "
                                                                                                                            .
                                                                                                                            showsPrec
                                                                                                                              d
                                                                                                                              (getField
                                                                                                                                 @"maxPerStageDescriptorStorageImages"
                                                                                                                                 x)
                                                                                                                              .
                                                                                                                              showString
                                                                                                                                ", "
                                                                                                                                .
                                                                                                                                showString
                                                                                                                                  "maxPerStageDescriptorInputAttachments = "
                                                                                                                                  .
                                                                                                                                  showsPrec
                                                                                                                                    d
                                                                                                                                    (getField
                                                                                                                                       @"maxPerStageDescriptorInputAttachments"
                                                                                                                                       x)
                                                                                                                                    .
                                                                                                                                    showString
                                                                                                                                      ", "
                                                                                                                                      .
                                                                                                                                      showString
                                                                                                                                        "maxPerStageResources = "
                                                                                                                                        .
                                                                                                                                        showsPrec
                                                                                                                                          d
                                                                                                                                          (getField
                                                                                                                                             @"maxPerStageResources"
                                                                                                                                             x)
                                                                                                                                          .
                                                                                                                                          showString
                                                                                                                                            ", "
                                                                                                                                            .
                                                                                                                                            showString
                                                                                                                                              "maxDescriptorSetSamplers = "
                                                                                                                                              .
                                                                                                                                              showsPrec
                                                                                                                                                d
                                                                                                                                                (getField
                                                                                                                                                   @"maxDescriptorSetSamplers"
                                                                                                                                                   x)
                                                                                                                                                .
                                                                                                                                                showString
                                                                                                                                                  ", "
                                                                                                                                                  .
                                                                                                                                                  showString
                                                                                                                                                    "maxDescriptorSetUniformBuffers = "
                                                                                                                                                    .
                                                                                                                                                    showsPrec
                                                                                                                                                      d
                                                                                                                                                      (getField
                                                                                                                                                         @"maxDescriptorSetUniformBuffers"
                                                                                                                                                         x)
                                                                                                                                                      .
                                                                                                                                                      showString
                                                                                                                                                        ", "
                                                                                                                                                        .
                                                                                                                                                        showString
                                                                                                                                                          "maxDescriptorSetUniformBuffersDynamic = "
                                                                                                                                                          .
                                                                                                                                                          showsPrec
                                                                                                                                                            d
                                                                                                                                                            (getField
                                                                                                                                                               @"maxDescriptorSetUniformBuffersDynamic"
                                                                                                                                                               x)
                                                                                                                                                            .
                                                                                                                                                            showString
                                                                                                                                                              ", "
                                                                                                                                                              .
                                                                                                                                                              showString
                                                                                                                                                                "maxDescriptorSetStorageBuffers = "
                                                                                                                                                                .
                                                                                                                                                                showsPrec
                                                                                                                                                                  d
                                                                                                                                                                  (getField
                                                                                                                                                                     @"maxDescriptorSetStorageBuffers"
                                                                                                                                                                     x)
                                                                                                                                                                  .
                                                                                                                                                                  showString
                                                                                                                                                                    ", "
                                                                                                                                                                    .
                                                                                                                                                                    showString
                                                                                                                                                                      "maxDescriptorSetStorageBuffersDynamic = "
                                                                                                                                                                      .
                                                                                                                                                                      showsPrec
                                                                                                                                                                        d
                                                                                                                                                                        (getField
                                                                                                                                                                           @"maxDescriptorSetStorageBuffersDynamic"
                                                                                                                                                                           x)
                                                                                                                                                                        .
                                                                                                                                                                        showString
                                                                                                                                                                          ", "
                                                                                                                                                                          .
                                                                                                                                                                          showString
                                                                                                                                                                            "maxDescriptorSetSampledImages = "
                                                                                                                                                                            .
                                                                                                                                                                            showsPrec
                                                                                                                                                                              d
                                                                                                                                                                              (getField
                                                                                                                                                                                 @"maxDescriptorSetSampledImages"
                                                                                                                                                                                 x)
                                                                                                                                                                              .
                                                                                                                                                                              showString
                                                                                                                                                                                ", "
                                                                                                                                                                                .
                                                                                                                                                                                showString
                                                                                                                                                                                  "maxDescriptorSetStorageImages = "
                                                                                                                                                                                  .
                                                                                                                                                                                  showsPrec
                                                                                                                                                                                    d
                                                                                                                                                                                    (getField
                                                                                                                                                                                       @"maxDescriptorSetStorageImages"
                                                                                                                                                                                       x)
                                                                                                                                                                                    .
                                                                                                                                                                                    showString
                                                                                                                                                                                      ", "
                                                                                                                                                                                      .
                                                                                                                                                                                      showString
                                                                                                                                                                                        "maxDescriptorSetInputAttachments = "
                                                                                                                                                                                        .
                                                                                                                                                                                        showsPrec
                                                                                                                                                                                          d
                                                                                                                                                                                          (getField
                                                                                                                                                                                             @"maxDescriptorSetInputAttachments"
                                                                                                                                                                                             x)
                                                                                                                                                                                          .
                                                                                                                                                                                          showString
                                                                                                                                                                                            ", "
                                                                                                                                                                                            .
                                                                                                                                                                                            showString
                                                                                                                                                                                              "maxVertexInputAttributes = "
                                                                                                                                                                                              .
                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                d
                                                                                                                                                                                                (getField
                                                                                                                                                                                                   @"maxVertexInputAttributes"
                                                                                                                                                                                                   x)
                                                                                                                                                                                                .
                                                                                                                                                                                                showString
                                                                                                                                                                                                  ", "
                                                                                                                                                                                                  .
                                                                                                                                                                                                  showString
                                                                                                                                                                                                    "maxVertexInputBindings = "
                                                                                                                                                                                                    .
                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                      d
                                                                                                                                                                                                      (getField
                                                                                                                                                                                                         @"maxVertexInputBindings"
                                                                                                                                                                                                         x)
                                                                                                                                                                                                      .
                                                                                                                                                                                                      showString
                                                                                                                                                                                                        ", "
                                                                                                                                                                                                        .
                                                                                                                                                                                                        showString
                                                                                                                                                                                                          "maxVertexInputAttributeOffset = "
                                                                                                                                                                                                          .
                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                            d
                                                                                                                                                                                                            (getField
                                                                                                                                                                                                               @"maxVertexInputAttributeOffset"
                                                                                                                                                                                                               x)
                                                                                                                                                                                                            .
                                                                                                                                                                                                            showString
                                                                                                                                                                                                              ", "
                                                                                                                                                                                                              .
                                                                                                                                                                                                              showString
                                                                                                                                                                                                                "maxVertexInputBindingStride = "
                                                                                                                                                                                                                .
                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                  d
                                                                                                                                                                                                                  (getField
                                                                                                                                                                                                                     @"maxVertexInputBindingStride"
                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                  .
                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                    .
                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                      "maxVertexOutputComponents = "
                                                                                                                                                                                                                      .
                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                        d
                                                                                                                                                                                                                        (getField
                                                                                                                                                                                                                           @"maxVertexOutputComponents"
                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                        .
                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                          .
                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                            "maxTessellationGenerationLevel = "
                                                                                                                                                                                                                            .
                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                              d
                                                                                                                                                                                                                              (getField
                                                                                                                                                                                                                                 @"maxTessellationGenerationLevel"
                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                              .
                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                  "maxTessellationPatchSize = "
                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                    (getField
                                                                                                                                                                                                                                       @"maxTessellationPatchSize"
                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                        "maxTessellationControlPerVertexInputComponents = "
                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                          (getField
                                                                                                                                                                                                                                             @"maxTessellationControlPerVertexInputComponents"
                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                              "maxTessellationControlPerVertexOutputComponents = "
                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                (getField
                                                                                                                                                                                                                                                   @"maxTessellationControlPerVertexOutputComponents"
                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                    "maxTessellationControlPerPatchOutputComponents = "
                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                      (getField
                                                                                                                                                                                                                                                         @"maxTessellationControlPerPatchOutputComponents"
                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                          "maxTessellationControlTotalOutputComponents = "
                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                            (getField
                                                                                                                                                                                                                                                               @"maxTessellationControlTotalOutputComponents"
                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                "maxTessellationEvaluationInputComponents = "
                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                  (getField
                                                                                                                                                                                                                                                                     @"maxTessellationEvaluationInputComponents"
                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                      "maxTessellationEvaluationOutputComponents = "
                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                        (getField
                                                                                                                                                                                                                                                                           @"maxTessellationEvaluationOutputComponents"
                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                            "maxGeometryShaderInvocations = "
                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                              (getField
                                                                                                                                                                                                                                                                                 @"maxGeometryShaderInvocations"
                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                  "maxGeometryInputComponents = "
                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                    (getField
                                                                                                                                                                                                                                                                                       @"maxGeometryInputComponents"
                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                        "maxGeometryOutputComponents = "
                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                          (getField
                                                                                                                                                                                                                                                                                             @"maxGeometryOutputComponents"
                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                              "maxGeometryOutputVertices = "
                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                (getField
                                                                                                                                                                                                                                                                                                   @"maxGeometryOutputVertices"
                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                    "maxGeometryTotalOutputComponents = "
                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                      (getField
                                                                                                                                                                                                                                                                                                         @"maxGeometryTotalOutputComponents"
                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                          "maxFragmentInputComponents = "
                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                                                                            (getField
                                                                                                                                                                                                                                                                                                               @"maxFragmentInputComponents"
                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                "maxFragmentOutputAttachments = "
                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                                                                  (getField
                                                                                                                                                                                                                                                                                                                     @"maxFragmentOutputAttachments"
                                                                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                      "maxFragmentDualSrcAttachments = "
                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                        (getField
                                                                                                                                                                                                                                                                                                                           @"maxFragmentDualSrcAttachments"
                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                            "maxFragmentCombinedOutputResources = "
                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                              (getField
                                                                                                                                                                                                                                                                                                                                 @"maxFragmentCombinedOutputResources"
                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                  "maxComputeSharedMemorySize = "
                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                                                                    (getField
                                                                                                                                                                                                                                                                                                                                       @"maxComputeSharedMemorySize"
                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                      (showString
                                                                                                                                                                                                                                                                                                                                         "maxComputeWorkGroupCount = ["
                                                                                                                                                                                                                                                                                                                                         .
                                                                                                                                                                                                                                                                                                                                         showsPrec
                                                                                                                                                                                                                                                                                                                                           d
                                                                                                                                                                                                                                                                                                                                           (let s = sizeOf
                                                                                                                                                                                                                                                                                                                                                      (undefined
                                                                                                                                                                                                                                                                                                                                                         ::
                                                                                                                                                                                                                                                                                                                                                         FieldType
                                                                                                                                                                                                                                                                                                                                                           "maxComputeWorkGroupCount"
                                                                                                                                                                                                                                                                                                                                                           VkPhysicalDeviceLimits)
                                                                                                                                                                                                                                                                                                                                                o = fieldOffset
                                                                                                                                                                                                                                                                                                                                                      @"maxComputeWorkGroupCount"
                                                                                                                                                                                                                                                                                                                                                      @VkPhysicalDeviceLimits
                                                                                                                                                                                                                                                                                                                                                f i
                                                                                                                                                                                                                                                                                                                                                  = peekByteOff
                                                                                                                                                                                                                                                                                                                                                      (unsafePtr
                                                                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                                                                      i
                                                                                                                                                                                                                                                                                                                                                      ::
                                                                                                                                                                                                                                                                                                                                                      IO
                                                                                                                                                                                                                                                                                                                                                        (FieldType
                                                                                                                                                                                                                                                                                                                                                           "maxComputeWorkGroupCount"
                                                                                                                                                                                                                                                                                                                                                           VkPhysicalDeviceLimits)
                                                                                                                                                                                                                                                                                                                                              in
                                                                                                                                                                                                                                                                                                                                              unsafeDupablePerformIO
                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                mapM
                                                                                                                                                                                                                                                                                                                                                  f
                                                                                                                                                                                                                                                                                                                                                $
                                                                                                                                                                                                                                                                                                                                                map
                                                                                                                                                                                                                                                                                                                                                  (\ i
                                                                                                                                                                                                                                                                                                                                                     ->
                                                                                                                                                                                                                                                                                                                                                     o +
                                                                                                                                                                                                                                                                                                                                                       i *
                                                                                                                                                                                                                                                                                                                                                         s)
                                                                                                                                                                                                                                                                                                                                                  [0
                                                                                                                                                                                                                                                                                                                                                   ..
                                                                                                                                                                                                                                                                                                                                                   3 -
                                                                                                                                                                                                                                                                                                                                                     1])
                                                                                                                                                                                                                                                                                                                                           .
                                                                                                                                                                                                                                                                                                                                           showChar
                                                                                                                                                                                                                                                                                                                                             ']')
                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                            "maxComputeWorkGroupInvocations = "
                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                                              (getField
                                                                                                                                                                                                                                                                                                                                                 @"maxComputeWorkGroupInvocations"
                                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                (showString
                                                                                                                                                                                                                                                                                                                                                   "maxComputeWorkGroupSize = ["
                                                                                                                                                                                                                                                                                                                                                   .
                                                                                                                                                                                                                                                                                                                                                   showsPrec
                                                                                                                                                                                                                                                                                                                                                     d
                                                                                                                                                                                                                                                                                                                                                     (let s = sizeOf
                                                                                                                                                                                                                                                                                                                                                                (undefined
                                                                                                                                                                                                                                                                                                                                                                   ::
                                                                                                                                                                                                                                                                                                                                                                   FieldType
                                                                                                                                                                                                                                                                                                                                                                     "maxComputeWorkGroupSize"
                                                                                                                                                                                                                                                                                                                                                                     VkPhysicalDeviceLimits)
                                                                                                                                                                                                                                                                                                                                                          o = fieldOffset
                                                                                                                                                                                                                                                                                                                                                                @"maxComputeWorkGroupSize"
                                                                                                                                                                                                                                                                                                                                                                @VkPhysicalDeviceLimits
                                                                                                                                                                                                                                                                                                                                                          f i
                                                                                                                                                                                                                                                                                                                                                            = peekByteOff
                                                                                                                                                                                                                                                                                                                                                                (unsafePtr
                                                                                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                                                                                i
                                                                                                                                                                                                                                                                                                                                                                ::
                                                                                                                                                                                                                                                                                                                                                                IO
                                                                                                                                                                                                                                                                                                                                                                  (FieldType
                                                                                                                                                                                                                                                                                                                                                                     "maxComputeWorkGroupSize"
                                                                                                                                                                                                                                                                                                                                                                     VkPhysicalDeviceLimits)
                                                                                                                                                                                                                                                                                                                                                        in
                                                                                                                                                                                                                                                                                                                                                        unsafeDupablePerformIO
                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                          mapM
                                                                                                                                                                                                                                                                                                                                                            f
                                                                                                                                                                                                                                                                                                                                                          $
                                                                                                                                                                                                                                                                                                                                                          map
                                                                                                                                                                                                                                                                                                                                                            (\ i
                                                                                                                                                                                                                                                                                                                                                               ->
                                                                                                                                                                                                                                                                                                                                                               o +
                                                                                                                                                                                                                                                                                                                                                                 i *
                                                                                                                                                                                                                                                                                                                                                                   s)
                                                                                                                                                                                                                                                                                                                                                            [0
                                                                                                                                                                                                                                                                                                                                                             ..
                                                                                                                                                                                                                                                                                                                                                             3 -
                                                                                                                                                                                                                                                                                                                                                               1])
                                                                                                                                                                                                                                                                                                                                                     .
                                                                                                                                                                                                                                                                                                                                                     showChar
                                                                                                                                                                                                                                                                                                                                                       ']')
                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                      "subPixelPrecisionBits = "
                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                                                        (getField
                                                                                                                                                                                                                                                                                                                                                           @"subPixelPrecisionBits"
                                                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                            "subTexelPrecisionBits = "
                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                                                              (getField
                                                                                                                                                                                                                                                                                                                                                                 @"subTexelPrecisionBits"
                                                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                  "mipmapPrecisionBits = "
                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                                                                                                    (getField
                                                                                                                                                                                                                                                                                                                                                                       @"mipmapPrecisionBits"
                                                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                        "maxDrawIndexedIndexValue = "
                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                                                                                                          (getField
                                                                                                                                                                                                                                                                                                                                                                             @"maxDrawIndexedIndexValue"
                                                                                                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                              "maxDrawIndirectCount = "
                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                                                                                                (getField
                                                                                                                                                                                                                                                                                                                                                                                   @"maxDrawIndirectCount"
                                                                                                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                    "maxSamplerLodBias = "
                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                                                                                                      (getField
                                                                                                                                                                                                                                                                                                                                                                                         @"maxSamplerLodBias"
                                                                                                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                          "maxSamplerAnisotropy = "
                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                                                                                                                                                            (getField
                                                                                                                                                                                                                                                                                                                                                                                               @"maxSamplerAnisotropy"
                                                                                                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                "maxViewports = "
                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                                                                                                                                                  (getField
                                                                                                                                                                                                                                                                                                                                                                                                     @"maxViewports"
                                                                                                                                                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                    (showString
                                                                                                                                                                                                                                                                                                                                                                                                       "maxViewportDimensions = ["
                                                                                                                                                                                                                                                                                                                                                                                                       .
                                                                                                                                                                                                                                                                                                                                                                                                       showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                         d
                                                                                                                                                                                                                                                                                                                                                                                                         (let s = sizeOf
                                                                                                                                                                                                                                                                                                                                                                                                                    (undefined
                                                                                                                                                                                                                                                                                                                                                                                                                       ::
                                                                                                                                                                                                                                                                                                                                                                                                                       FieldType
                                                                                                                                                                                                                                                                                                                                                                                                                         "maxViewportDimensions"
                                                                                                                                                                                                                                                                                                                                                                                                                         VkPhysicalDeviceLimits)
                                                                                                                                                                                                                                                                                                                                                                                                              o = fieldOffset
                                                                                                                                                                                                                                                                                                                                                                                                                    @"maxViewportDimensions"
                                                                                                                                                                                                                                                                                                                                                                                                                    @VkPhysicalDeviceLimits
                                                                                                                                                                                                                                                                                                                                                                                                              f i
                                                                                                                                                                                                                                                                                                                                                                                                                = peekByteOff
                                                                                                                                                                                                                                                                                                                                                                                                                    (unsafePtr
                                                                                                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                                                                                                    i
                                                                                                                                                                                                                                                                                                                                                                                                                    ::
                                                                                                                                                                                                                                                                                                                                                                                                                    IO
                                                                                                                                                                                                                                                                                                                                                                                                                      (FieldType
                                                                                                                                                                                                                                                                                                                                                                                                                         "maxViewportDimensions"
                                                                                                                                                                                                                                                                                                                                                                                                                         VkPhysicalDeviceLimits)
                                                                                                                                                                                                                                                                                                                                                                                                            in
                                                                                                                                                                                                                                                                                                                                                                                                            unsafeDupablePerformIO
                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                              mapM
                                                                                                                                                                                                                                                                                                                                                                                                                f
                                                                                                                                                                                                                                                                                                                                                                                                              $
                                                                                                                                                                                                                                                                                                                                                                                                              map
                                                                                                                                                                                                                                                                                                                                                                                                                (\ i
                                                                                                                                                                                                                                                                                                                                                                                                                   ->
                                                                                                                                                                                                                                                                                                                                                                                                                   o +
                                                                                                                                                                                                                                                                                                                                                                                                                     i *
                                                                                                                                                                                                                                                                                                                                                                                                                       s)
                                                                                                                                                                                                                                                                                                                                                                                                                [0
                                                                                                                                                                                                                                                                                                                                                                                                                 ..
                                                                                                                                                                                                                                                                                                                                                                                                                 2 -
                                                                                                                                                                                                                                                                                                                                                                                                                   1])
                                                                                                                                                                                                                                                                                                                                                                                                         .
                                                                                                                                                                                                                                                                                                                                                                                                         showChar
                                                                                                                                                                                                                                                                                                                                                                                                           ']')
                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                        (showString
                                                                                                                                                                                                                                                                                                                                                                                                           "viewportBoundsRange = ["
                                                                                                                                                                                                                                                                                                                                                                                                           .
                                                                                                                                                                                                                                                                                                                                                                                                           showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                             d
                                                                                                                                                                                                                                                                                                                                                                                                             (let s = sizeOf
                                                                                                                                                                                                                                                                                                                                                                                                                        (undefined
                                                                                                                                                                                                                                                                                                                                                                                                                           ::
                                                                                                                                                                                                                                                                                                                                                                                                                           FieldType
                                                                                                                                                                                                                                                                                                                                                                                                                             "viewportBoundsRange"
                                                                                                                                                                                                                                                                                                                                                                                                                             VkPhysicalDeviceLimits)
                                                                                                                                                                                                                                                                                                                                                                                                                  o = fieldOffset
                                                                                                                                                                                                                                                                                                                                                                                                                        @"viewportBoundsRange"
                                                                                                                                                                                                                                                                                                                                                                                                                        @VkPhysicalDeviceLimits
                                                                                                                                                                                                                                                                                                                                                                                                                  f i
                                                                                                                                                                                                                                                                                                                                                                                                                    = peekByteOff
                                                                                                                                                                                                                                                                                                                                                                                                                        (unsafePtr
                                                                                                                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                                                                                                                        i
                                                                                                                                                                                                                                                                                                                                                                                                                        ::
                                                                                                                                                                                                                                                                                                                                                                                                                        IO
                                                                                                                                                                                                                                                                                                                                                                                                                          (FieldType
                                                                                                                                                                                                                                                                                                                                                                                                                             "viewportBoundsRange"
                                                                                                                                                                                                                                                                                                                                                                                                                             VkPhysicalDeviceLimits)
                                                                                                                                                                                                                                                                                                                                                                                                                in
                                                                                                                                                                                                                                                                                                                                                                                                                unsafeDupablePerformIO
                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                  mapM
                                                                                                                                                                                                                                                                                                                                                                                                                    f
                                                                                                                                                                                                                                                                                                                                                                                                                  $
                                                                                                                                                                                                                                                                                                                                                                                                                  map
                                                                                                                                                                                                                                                                                                                                                                                                                    (\ i
                                                                                                                                                                                                                                                                                                                                                                                                                       ->
                                                                                                                                                                                                                                                                                                                                                                                                                       o +
                                                                                                                                                                                                                                                                                                                                                                                                                         i *
                                                                                                                                                                                                                                                                                                                                                                                                                           s)
                                                                                                                                                                                                                                                                                                                                                                                                                    [0
                                                                                                                                                                                                                                                                                                                                                                                                                     ..
                                                                                                                                                                                                                                                                                                                                                                                                                     2 -
                                                                                                                                                                                                                                                                                                                                                                                                                       1])
                                                                                                                                                                                                                                                                                                                                                                                                             .
                                                                                                                                                                                                                                                                                                                                                                                                             showChar
                                                                                                                                                                                                                                                                                                                                                                                                               ']')
                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                              "viewportSubPixelBits = "
                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                                                                                                                                (getField
                                                                                                                                                                                                                                                                                                                                                                                                                   @"viewportSubPixelBits"
                                                                                                                                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                    "minMemoryMapAlignment = "
                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                                                                                                                                      (getField
                                                                                                                                                                                                                                                                                                                                                                                                                         @"minMemoryMapAlignment"
                                                                                                                                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                          "minTexelBufferOffsetAlignment = "
                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                                                                                                                                                                                            (getField
                                                                                                                                                                                                                                                                                                                                                                                                                               @"minTexelBufferOffsetAlignment"
                                                                                                                                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                "minUniformBufferOffsetAlignment = "
                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                                                                                                                                                                                  (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                     @"minUniformBufferOffsetAlignment"
                                                                                                                                                                                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                      "minStorageBufferOffsetAlignment = "
                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                                                                                                                                        (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                           @"minStorageBufferOffsetAlignment"
                                                                                                                                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                            "minTexelOffset = "
                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                                                                                                                                              (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                 @"minTexelOffset"
                                                                                                                                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                  "maxTexelOffset = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                                                                                                                                                                                    (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                       @"maxTexelOffset"
                                                                                                                                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                        "minTexelGatherOffset = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                                                                                                                                                                                          (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                             @"minTexelGatherOffset"
                                                                                                                                                                                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                              "maxTexelGatherOffset = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @"maxTexelGatherOffset"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "minInterpolationOffset = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         @"minInterpolationOffset"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "maxInterpolationOffset = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                               @"maxInterpolationOffset"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "subPixelInterpolationOffsetBits = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     @"subPixelInterpolationOffsetBits"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "maxFramebufferWidth = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           @"maxFramebufferWidth"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "maxFramebufferHeight = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @"maxFramebufferHeight"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "maxFramebufferLayers = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       @"maxFramebufferLayers"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "framebufferColorSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             @"framebufferColorSampleCounts"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "framebufferDepthSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @"framebufferDepthSampleCounts"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "framebufferStencilSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         @"framebufferStencilSampleCounts"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "framebufferNoAttachmentsSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               @"framebufferNoAttachmentsSampleCounts"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "maxColorAttachments = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     @"maxColorAttachments"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "sampledImageColorSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           @"sampledImageColorSampleCounts"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "sampledImageIntegerSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @"sampledImageIntegerSampleCounts"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "sampledImageDepthSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       @"sampledImageDepthSampleCounts"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "sampledImageStencilSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             @"sampledImageStencilSampleCounts"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "storageImageSampleCounts = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @"storageImageSampleCounts"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "maxSampleMaskWords = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         @"maxSampleMaskWords"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "timestampComputeAndGraphics = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               @"timestampComputeAndGraphics"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "timestampPeriod = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     @"timestampPeriod"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "maxClipDistances = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           @"maxClipDistances"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "maxCullDistances = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @"maxCullDistances"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "maxCombinedClipAndCullDistances = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       @"maxCombinedClipAndCullDistances"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "discreteQueuePriorities = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             @"discreteQueuePriorities"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "pointSizeRange = ["
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 (let s = sizeOf
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (undefined
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ::
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               FieldType
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "pointSizeRange"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 VkPhysicalDeviceLimits)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      o = fieldOffset
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            @"pointSizeRange"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            @VkPhysicalDeviceLimits
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      f i
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        = peekByteOff
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (unsafePtr
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            i
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ::
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            IO
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (FieldType
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "pointSizeRange"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 VkPhysicalDeviceLimits)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    in
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    unsafeDupablePerformIO
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      mapM
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        f
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      $
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      map
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (\ i
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           o +
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             i *
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               s)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        [0
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ..
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         2 -
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           1])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 showChar
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ']')
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "lineWidthRange = ["
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     (let s = sizeOf
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (undefined
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ::
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   FieldType
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "lineWidthRange"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     VkPhysicalDeviceLimits)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          o = fieldOffset
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                @"lineWidthRange"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                @VkPhysicalDeviceLimits
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          f i
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            = peekByteOff
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (unsafePtr
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                i
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ::
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                IO
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (FieldType
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "lineWidthRange"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     VkPhysicalDeviceLimits)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        in
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        unsafeDupablePerformIO
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          mapM
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            f
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          $
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          map
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (\ i
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ->
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               o +
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 i *
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   s)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            [0
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ..
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             2 -
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               1])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     showChar
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ']')
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "pointSizeGranularity = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           @"pointSizeGranularity"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "lineWidthGranularity = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @"lineWidthGranularity"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "strictLines = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       @"strictLines"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "standardSampleLocations = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             @"standardSampleLocations"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "optimalBufferCopyOffsetAlignment = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @"optimalBufferCopyOffsetAlignment"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "optimalBufferCopyRowPitchAlignment = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         @"optimalBufferCopyRowPitchAlignment"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "nonCoherentAtomSize = "
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (getField
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               @"nonCoherentAtomSize"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            showChar
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              '}'
