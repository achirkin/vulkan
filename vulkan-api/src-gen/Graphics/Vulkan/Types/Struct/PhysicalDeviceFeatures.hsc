#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures
       (VkPhysicalDeviceFeatures(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Base                         (Addr##, ByteArray##,
                                                   byteArrayContents##,
                                                   plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes  (VkBool32)
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceFeatures {
--   >     VkBool32               robustBufferAccess;
--   >     VkBool32               fullDrawIndexUint32;
--   >     VkBool32               imageCubeArray;
--   >     VkBool32               independentBlend;
--   >     VkBool32               geometryShader;
--   >     VkBool32               tessellationShader;
--   >     VkBool32               sampleRateShading;
--   >     VkBool32               dualSrcBlend;
--   >     VkBool32               logicOp;
--   >     VkBool32               multiDrawIndirect;
--   >     VkBool32               drawIndirectFirstInstance;
--   >     VkBool32               depthClamp;
--   >     VkBool32               depthBiasClamp;
--   >     VkBool32               fillModeNonSolid;
--   >     VkBool32               depthBounds;
--   >     VkBool32               wideLines;
--   >     VkBool32               largePoints;
--   >     VkBool32               alphaToOne;
--   >     VkBool32               multiViewport;
--   >     VkBool32               samplerAnisotropy;
--   >     VkBool32               textureCompressionETC2;
--   >     VkBool32               textureCompressionASTC_LDR;
--   >     VkBool32               textureCompressionBC;
--   >     VkBool32               occlusionQueryPrecise;
--   >     VkBool32               pipelineStatisticsQuery;
--   >     VkBool32               vertexPipelineStoresAndAtomics;
--   >     VkBool32               fragmentStoresAndAtomics;
--   >     VkBool32               shaderTessellationAndGeometryPointSize;
--   >     VkBool32               shaderImageGatherExtended;
--   >     VkBool32               shaderStorageImageExtendedFormats;
--   >     VkBool32               shaderStorageImageMultisample;
--   >     VkBool32               shaderStorageImageReadWithoutFormat;
--   >     VkBool32               shaderStorageImageWriteWithoutFormat;
--   >     VkBool32               shaderUniformBufferArrayDynamicIndexing;
--   >     VkBool32               shaderSampledImageArrayDynamicIndexing;
--   >     VkBool32               shaderStorageBufferArrayDynamicIndexing;
--   >     VkBool32               shaderStorageImageArrayDynamicIndexing;
--   >     VkBool32               shaderClipDistance;
--   >     VkBool32               shaderCullDistance;
--   >     VkBool32               shaderFloat64;
--   >     VkBool32               shaderInt64;
--   >     VkBool32               shaderInt16;
--   >     VkBool32               shaderResourceResidency;
--   >     VkBool32               shaderResourceMinLod;
--   >     VkBool32               sparseBinding;
--   >     VkBool32               sparseResidencyBuffer;
--   >     VkBool32               sparseResidencyImage2D;
--   >     VkBool32               sparseResidencyImage3D;
--   >     VkBool32               sparseResidency2Samples;
--   >     VkBool32               sparseResidency4Samples;
--   >     VkBool32               sparseResidency8Samples;
--   >     VkBool32               sparseResidency16Samples;
--   >     VkBool32               sparseResidencyAliased;
--   >     VkBool32               variableMultisampleRate;
--   >     VkBool32               inheritedQueries;
--   > } VkPhysicalDeviceFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceFeatures VkPhysicalDeviceFeatures registry at www.khronos.org>
data VkPhysicalDeviceFeatures = VkPhysicalDeviceFeatures## Addr##
                                                          ByteArray##

instance Eq VkPhysicalDeviceFeatures where
        (VkPhysicalDeviceFeatures## a _) ==
          x@(VkPhysicalDeviceFeatures## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceFeatures where
        (VkPhysicalDeviceFeatures## a _) `compare`
          x@(VkPhysicalDeviceFeatures## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceFeatures where
        sizeOf ~_ = #{size VkPhysicalDeviceFeatures}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPhysicalDeviceFeatures}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceFeatures where
        unsafeAddr (VkPhysicalDeviceFeatures## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceFeatures## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceFeatures## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceFeatures where
        type StructFields VkPhysicalDeviceFeatures =
             '["robustBufferAccess", "fullDrawIndexUint32", "imageCubeArray", -- ' closing tick for hsc2hs
               "independentBlend", "geometryShader", "tessellationShader",
               "sampleRateShading", "dualSrcBlend", "logicOp",
               "multiDrawIndirect", "drawIndirectFirstInstance", "depthClamp",
               "depthBiasClamp", "fillModeNonSolid", "depthBounds", "wideLines",
               "largePoints", "alphaToOne", "multiViewport", "samplerAnisotropy",
               "textureCompressionETC2", "textureCompressionASTC_LDR",
               "textureCompressionBC", "occlusionQueryPrecise",
               "pipelineStatisticsQuery", "vertexPipelineStoresAndAtomics",
               "fragmentStoresAndAtomics",
               "shaderTessellationAndGeometryPointSize",
               "shaderImageGatherExtended", "shaderStorageImageExtendedFormats",
               "shaderStorageImageMultisample",
               "shaderStorageImageReadWithoutFormat",
               "shaderStorageImageWriteWithoutFormat",
               "shaderUniformBufferArrayDynamicIndexing",
               "shaderSampledImageArrayDynamicIndexing",
               "shaderStorageBufferArrayDynamicIndexing",
               "shaderStorageImageArrayDynamicIndexing", "shaderClipDistance",
               "shaderCullDistance", "shaderFloat64", "shaderInt64",
               "shaderInt16", "shaderResourceResidency", "shaderResourceMinLod",
               "sparseBinding", "sparseResidencyBuffer", "sparseResidencyImage2D",
               "sparseResidencyImage3D", "sparseResidency2Samples",
               "sparseResidency4Samples", "sparseResidency8Samples",
               "sparseResidency16Samples", "sparseResidencyAliased",
               "variableMultisampleRate", "inheritedQueries"]
        type CUnionType VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceFeatures = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "robustBufferAccess" VkPhysicalDeviceFeatures where
        type FieldType "robustBufferAccess" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "robustBufferAccess" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "robustBufferAccess" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, robustBufferAccess}
        type FieldIsArray "robustBufferAccess" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, robustBufferAccess}

instance {-# OVERLAPPING #-}
         CanReadField "robustBufferAccess" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, robustBufferAccess})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, robustBufferAccess}

instance {-# OVERLAPPING #-}
         CanWriteField "robustBufferAccess" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, robustBufferAccess}

instance {-# OVERLAPPING #-}
         HasField "fullDrawIndexUint32" VkPhysicalDeviceFeatures where
        type FieldType "fullDrawIndexUint32" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "fullDrawIndexUint32" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "fullDrawIndexUint32" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, fullDrawIndexUint32}
        type FieldIsArray "fullDrawIndexUint32" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, fullDrawIndexUint32}

instance {-# OVERLAPPING #-}
         CanReadField "fullDrawIndexUint32" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, fullDrawIndexUint32})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, fullDrawIndexUint32}

instance {-# OVERLAPPING #-}
         CanWriteField "fullDrawIndexUint32" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, fullDrawIndexUint32}

instance {-# OVERLAPPING #-}
         HasField "imageCubeArray" VkPhysicalDeviceFeatures where
        type FieldType "imageCubeArray" VkPhysicalDeviceFeatures = VkBool32
        type FieldOptional "imageCubeArray" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "imageCubeArray" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, imageCubeArray}
        type FieldIsArray "imageCubeArray" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, imageCubeArray}

instance {-# OVERLAPPING #-}
         CanReadField "imageCubeArray" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, imageCubeArray})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, imageCubeArray}

instance {-# OVERLAPPING #-}
         CanWriteField "imageCubeArray" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, imageCubeArray}

instance {-# OVERLAPPING #-}
         HasField "independentBlend" VkPhysicalDeviceFeatures where
        type FieldType "independentBlend" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "independentBlend" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "independentBlend" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, independentBlend}
        type FieldIsArray "independentBlend" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, independentBlend}

instance {-# OVERLAPPING #-}
         CanReadField "independentBlend" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, independentBlend})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, independentBlend}

instance {-# OVERLAPPING #-}
         CanWriteField "independentBlend" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, independentBlend}

instance {-# OVERLAPPING #-}
         HasField "geometryShader" VkPhysicalDeviceFeatures where
        type FieldType "geometryShader" VkPhysicalDeviceFeatures = VkBool32
        type FieldOptional "geometryShader" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "geometryShader" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, geometryShader}
        type FieldIsArray "geometryShader" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, geometryShader}

instance {-# OVERLAPPING #-}
         CanReadField "geometryShader" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, geometryShader})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, geometryShader}

instance {-# OVERLAPPING #-}
         CanWriteField "geometryShader" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, geometryShader}

instance {-# OVERLAPPING #-}
         HasField "tessellationShader" VkPhysicalDeviceFeatures where
        type FieldType "tessellationShader" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "tessellationShader" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "tessellationShader" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, tessellationShader}
        type FieldIsArray "tessellationShader" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, tessellationShader}

instance {-# OVERLAPPING #-}
         CanReadField "tessellationShader" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, tessellationShader})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, tessellationShader}

instance {-# OVERLAPPING #-}
         CanWriteField "tessellationShader" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, tessellationShader}

instance {-# OVERLAPPING #-}
         HasField "sampleRateShading" VkPhysicalDeviceFeatures where
        type FieldType "sampleRateShading" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "sampleRateShading" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleRateShading" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, sampleRateShading}
        type FieldIsArray "sampleRateShading" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, sampleRateShading}

instance {-# OVERLAPPING #-}
         CanReadField "sampleRateShading" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sampleRateShading})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sampleRateShading}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleRateShading" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sampleRateShading}

instance {-# OVERLAPPING #-}
         HasField "dualSrcBlend" VkPhysicalDeviceFeatures where
        type FieldType "dualSrcBlend" VkPhysicalDeviceFeatures = VkBool32
        type FieldOptional "dualSrcBlend" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dualSrcBlend" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, dualSrcBlend}
        type FieldIsArray "dualSrcBlend" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, dualSrcBlend}

instance {-# OVERLAPPING #-}
         CanReadField "dualSrcBlend" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, dualSrcBlend})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, dualSrcBlend}

instance {-# OVERLAPPING #-}
         CanWriteField "dualSrcBlend" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, dualSrcBlend}

instance {-# OVERLAPPING #-}
         HasField "logicOp" VkPhysicalDeviceFeatures where
        type FieldType "logicOp" VkPhysicalDeviceFeatures = VkBool32
        type FieldOptional "logicOp" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs
        type FieldOffset "logicOp" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, logicOp}
        type FieldIsArray "logicOp" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, logicOp}

instance {-# OVERLAPPING #-}
         CanReadField "logicOp" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, logicOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, logicOp}

instance {-# OVERLAPPING #-}
         CanWriteField "logicOp" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, logicOp}

instance {-# OVERLAPPING #-}
         HasField "multiDrawIndirect" VkPhysicalDeviceFeatures where
        type FieldType "multiDrawIndirect" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "multiDrawIndirect" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "multiDrawIndirect" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, multiDrawIndirect}
        type FieldIsArray "multiDrawIndirect" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, multiDrawIndirect}

instance {-# OVERLAPPING #-}
         CanReadField "multiDrawIndirect" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, multiDrawIndirect})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, multiDrawIndirect}

instance {-# OVERLAPPING #-}
         CanWriteField "multiDrawIndirect" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, multiDrawIndirect}

instance {-# OVERLAPPING #-}
         HasField "drawIndirectFirstInstance" VkPhysicalDeviceFeatures where
        type FieldType "drawIndirectFirstInstance" VkPhysicalDeviceFeatures
             = VkBool32
        type FieldOptional "drawIndirectFirstInstance"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "drawIndirectFirstInstance"
               VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, drawIndirectFirstInstance}
        type FieldIsArray "drawIndirectFirstInstance"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, drawIndirectFirstInstance}

instance {-# OVERLAPPING #-}
         CanReadField "drawIndirectFirstInstance" VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, drawIndirectFirstInstance})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, drawIndirectFirstInstance}

instance {-# OVERLAPPING #-}
         CanWriteField "drawIndirectFirstInstance" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, drawIndirectFirstInstance}

instance {-# OVERLAPPING #-}
         HasField "depthClamp" VkPhysicalDeviceFeatures where
        type FieldType "depthClamp" VkPhysicalDeviceFeatures = VkBool32
        type FieldOptional "depthClamp" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthClamp" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, depthClamp}
        type FieldIsArray "depthClamp" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, depthClamp}

instance {-# OVERLAPPING #-}
         CanReadField "depthClamp" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, depthClamp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, depthClamp}

instance {-# OVERLAPPING #-}
         CanWriteField "depthClamp" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, depthClamp}

instance {-# OVERLAPPING #-}
         HasField "depthBiasClamp" VkPhysicalDeviceFeatures where
        type FieldType "depthBiasClamp" VkPhysicalDeviceFeatures = VkBool32
        type FieldOptional "depthBiasClamp" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "depthBiasClamp" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, depthBiasClamp}
        type FieldIsArray "depthBiasClamp" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, depthBiasClamp}

instance {-# OVERLAPPING #-}
         CanReadField "depthBiasClamp" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, depthBiasClamp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, depthBiasClamp}

instance {-# OVERLAPPING #-}
         CanWriteField "depthBiasClamp" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, depthBiasClamp}

instance {-# OVERLAPPING #-}
         HasField "fillModeNonSolid" VkPhysicalDeviceFeatures where
        type FieldType "fillModeNonSolid" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "fillModeNonSolid" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "fillModeNonSolid" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, fillModeNonSolid}
        type FieldIsArray "fillModeNonSolid" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, fillModeNonSolid}

instance {-# OVERLAPPING #-}
         CanReadField "fillModeNonSolid" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, fillModeNonSolid})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, fillModeNonSolid}

instance {-# OVERLAPPING #-}
         CanWriteField "fillModeNonSolid" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, fillModeNonSolid}

instance {-# OVERLAPPING #-}
         HasField "depthBounds" VkPhysicalDeviceFeatures where
        type FieldType "depthBounds" VkPhysicalDeviceFeatures = VkBool32
        type FieldOptional "depthBounds" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthBounds" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, depthBounds}
        type FieldIsArray "depthBounds" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, depthBounds}

instance {-# OVERLAPPING #-}
         CanReadField "depthBounds" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, depthBounds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, depthBounds}

instance {-# OVERLAPPING #-}
         CanWriteField "depthBounds" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, depthBounds}

instance {-# OVERLAPPING #-}
         HasField "wideLines" VkPhysicalDeviceFeatures where
        type FieldType "wideLines" VkPhysicalDeviceFeatures = VkBool32
        type FieldOptional "wideLines" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs
        type FieldOffset "wideLines" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, wideLines}
        type FieldIsArray "wideLines" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, wideLines}

instance {-# OVERLAPPING #-}
         CanReadField "wideLines" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, wideLines})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, wideLines}

instance {-# OVERLAPPING #-}
         CanWriteField "wideLines" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, wideLines}

instance {-# OVERLAPPING #-}
         HasField "largePoints" VkPhysicalDeviceFeatures where
        type FieldType "largePoints" VkPhysicalDeviceFeatures = VkBool32
        type FieldOptional "largePoints" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs
        type FieldOffset "largePoints" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, largePoints}
        type FieldIsArray "largePoints" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, largePoints}

instance {-# OVERLAPPING #-}
         CanReadField "largePoints" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, largePoints})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, largePoints}

instance {-# OVERLAPPING #-}
         CanWriteField "largePoints" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, largePoints}

instance {-# OVERLAPPING #-}
         HasField "alphaToOne" VkPhysicalDeviceFeatures where
        type FieldType "alphaToOne" VkPhysicalDeviceFeatures = VkBool32
        type FieldOptional "alphaToOne" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs
        type FieldOffset "alphaToOne" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, alphaToOne}
        type FieldIsArray "alphaToOne" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, alphaToOne}

instance {-# OVERLAPPING #-}
         CanReadField "alphaToOne" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, alphaToOne})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, alphaToOne}

instance {-# OVERLAPPING #-}
         CanWriteField "alphaToOne" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, alphaToOne}

instance {-# OVERLAPPING #-}
         HasField "multiViewport" VkPhysicalDeviceFeatures where
        type FieldType "multiViewport" VkPhysicalDeviceFeatures = VkBool32
        type FieldOptional "multiViewport" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "multiViewport" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, multiViewport}
        type FieldIsArray "multiViewport" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, multiViewport}

instance {-# OVERLAPPING #-}
         CanReadField "multiViewport" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, multiViewport})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, multiViewport}

instance {-# OVERLAPPING #-}
         CanWriteField "multiViewport" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, multiViewport}

instance {-# OVERLAPPING #-}
         HasField "samplerAnisotropy" VkPhysicalDeviceFeatures where
        type FieldType "samplerAnisotropy" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "samplerAnisotropy" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "samplerAnisotropy" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, samplerAnisotropy}
        type FieldIsArray "samplerAnisotropy" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, samplerAnisotropy}

instance {-# OVERLAPPING #-}
         CanReadField "samplerAnisotropy" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, samplerAnisotropy})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, samplerAnisotropy}

instance {-# OVERLAPPING #-}
         CanWriteField "samplerAnisotropy" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, samplerAnisotropy}

instance {-# OVERLAPPING #-}
         HasField "textureCompressionETC2" VkPhysicalDeviceFeatures where
        type FieldType "textureCompressionETC2" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "textureCompressionETC2"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "textureCompressionETC2" VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, textureCompressionETC2}
        type FieldIsArray "textureCompressionETC2" VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, textureCompressionETC2}

instance {-# OVERLAPPING #-}
         CanReadField "textureCompressionETC2" VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, textureCompressionETC2})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, textureCompressionETC2}

instance {-# OVERLAPPING #-}
         CanWriteField "textureCompressionETC2" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, textureCompressionETC2}

instance {-# OVERLAPPING #-}
         HasField "textureCompressionASTC_LDR" VkPhysicalDeviceFeatures
         where
        type FieldType "textureCompressionASTC_LDR"
               VkPhysicalDeviceFeatures
             = VkBool32
        type FieldOptional "textureCompressionASTC_LDR"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "textureCompressionASTC_LDR"
               VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, textureCompressionASTC_LDR}
        type FieldIsArray "textureCompressionASTC_LDR"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, textureCompressionASTC_LDR}

instance {-# OVERLAPPING #-}
         CanReadField "textureCompressionASTC_LDR" VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, textureCompressionASTC_LDR})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, textureCompressionASTC_LDR}

instance {-# OVERLAPPING #-}
         CanWriteField "textureCompressionASTC_LDR" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, textureCompressionASTC_LDR}

instance {-# OVERLAPPING #-}
         HasField "textureCompressionBC" VkPhysicalDeviceFeatures where
        type FieldType "textureCompressionBC" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "textureCompressionBC" VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "textureCompressionBC" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, textureCompressionBC}
        type FieldIsArray "textureCompressionBC" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, textureCompressionBC}

instance {-# OVERLAPPING #-}
         CanReadField "textureCompressionBC" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, textureCompressionBC})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, textureCompressionBC}

instance {-# OVERLAPPING #-}
         CanWriteField "textureCompressionBC" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, textureCompressionBC}

instance {-# OVERLAPPING #-}
         HasField "occlusionQueryPrecise" VkPhysicalDeviceFeatures where
        type FieldType "occlusionQueryPrecise" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "occlusionQueryPrecise" VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "occlusionQueryPrecise" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, occlusionQueryPrecise}
        type FieldIsArray "occlusionQueryPrecise" VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, occlusionQueryPrecise}

instance {-# OVERLAPPING #-}
         CanReadField "occlusionQueryPrecise" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, occlusionQueryPrecise})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, occlusionQueryPrecise}

instance {-# OVERLAPPING #-}
         CanWriteField "occlusionQueryPrecise" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, occlusionQueryPrecise}

instance {-# OVERLAPPING #-}
         HasField "pipelineStatisticsQuery" VkPhysicalDeviceFeatures where
        type FieldType "pipelineStatisticsQuery" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "pipelineStatisticsQuery"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pipelineStatisticsQuery" VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, pipelineStatisticsQuery}
        type FieldIsArray "pipelineStatisticsQuery"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, pipelineStatisticsQuery}

instance {-# OVERLAPPING #-}
         CanReadField "pipelineStatisticsQuery" VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, pipelineStatisticsQuery})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, pipelineStatisticsQuery}

instance {-# OVERLAPPING #-}
         CanWriteField "pipelineStatisticsQuery" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, pipelineStatisticsQuery}

instance {-# OVERLAPPING #-}
         HasField "vertexPipelineStoresAndAtomics" VkPhysicalDeviceFeatures
         where
        type FieldType "vertexPipelineStoresAndAtomics"
               VkPhysicalDeviceFeatures
             = VkBool32
        type FieldOptional "vertexPipelineStoresAndAtomics"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "vertexPipelineStoresAndAtomics"
               VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, vertexPipelineStoresAndAtomics}
        type FieldIsArray "vertexPipelineStoresAndAtomics"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, vertexPipelineStoresAndAtomics}

instance {-# OVERLAPPING #-}
         CanReadField "vertexPipelineStoresAndAtomics"
           VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, vertexPipelineStoresAndAtomics})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, vertexPipelineStoresAndAtomics}

instance {-# OVERLAPPING #-}
         CanWriteField "vertexPipelineStoresAndAtomics"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, vertexPipelineStoresAndAtomics}

instance {-# OVERLAPPING #-}
         HasField "fragmentStoresAndAtomics" VkPhysicalDeviceFeatures where
        type FieldType "fragmentStoresAndAtomics" VkPhysicalDeviceFeatures
             = VkBool32
        type FieldOptional "fragmentStoresAndAtomics"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fragmentStoresAndAtomics"
               VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, fragmentStoresAndAtomics}
        type FieldIsArray "fragmentStoresAndAtomics"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, fragmentStoresAndAtomics}

instance {-# OVERLAPPING #-}
         CanReadField "fragmentStoresAndAtomics" VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, fragmentStoresAndAtomics})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, fragmentStoresAndAtomics}

instance {-# OVERLAPPING #-}
         CanWriteField "fragmentStoresAndAtomics" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, fragmentStoresAndAtomics}

instance {-# OVERLAPPING #-}
         HasField "shaderTessellationAndGeometryPointSize"
           VkPhysicalDeviceFeatures
         where
        type FieldType "shaderTessellationAndGeometryPointSize"
               VkPhysicalDeviceFeatures
             = VkBool32
        type FieldOptional "shaderTessellationAndGeometryPointSize"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderTessellationAndGeometryPointSize"
               VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, shaderTessellationAndGeometryPointSize}
        type FieldIsArray "shaderTessellationAndGeometryPointSize"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderTessellationAndGeometryPointSize}

instance {-# OVERLAPPING #-}
         CanReadField "shaderTessellationAndGeometryPointSize"
           VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderTessellationAndGeometryPointSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderTessellationAndGeometryPointSize}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderTessellationAndGeometryPointSize"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderTessellationAndGeometryPointSize}

instance {-# OVERLAPPING #-}
         HasField "shaderImageGatherExtended" VkPhysicalDeviceFeatures where
        type FieldType "shaderImageGatherExtended" VkPhysicalDeviceFeatures
             = VkBool32
        type FieldOptional "shaderImageGatherExtended"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderImageGatherExtended"
               VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, shaderImageGatherExtended}
        type FieldIsArray "shaderImageGatherExtended"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderImageGatherExtended}

instance {-# OVERLAPPING #-}
         CanReadField "shaderImageGatherExtended" VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderImageGatherExtended})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderImageGatherExtended}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderImageGatherExtended" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderImageGatherExtended}

instance {-# OVERLAPPING #-}
         HasField "shaderStorageImageExtendedFormats"
           VkPhysicalDeviceFeatures
         where
        type FieldType "shaderStorageImageExtendedFormats"
               VkPhysicalDeviceFeatures
             = VkBool32
        type FieldOptional "shaderStorageImageExtendedFormats"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderStorageImageExtendedFormats"
               VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, shaderStorageImageExtendedFormats}
        type FieldIsArray "shaderStorageImageExtendedFormats"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderStorageImageExtendedFormats}

instance {-# OVERLAPPING #-}
         CanReadField "shaderStorageImageExtendedFormats"
           VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderStorageImageExtendedFormats})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageExtendedFormats}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderStorageImageExtendedFormats"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageExtendedFormats}

instance {-# OVERLAPPING #-}
         HasField "shaderStorageImageMultisample" VkPhysicalDeviceFeatures
         where
        type FieldType "shaderStorageImageMultisample"
               VkPhysicalDeviceFeatures
             = VkBool32
        type FieldOptional "shaderStorageImageMultisample"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderStorageImageMultisample"
               VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, shaderStorageImageMultisample}
        type FieldIsArray "shaderStorageImageMultisample"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderStorageImageMultisample}

instance {-# OVERLAPPING #-}
         CanReadField "shaderStorageImageMultisample"
           VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderStorageImageMultisample})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageMultisample}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderStorageImageMultisample"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageMultisample}

instance {-# OVERLAPPING #-}
         HasField "shaderStorageImageReadWithoutFormat"
           VkPhysicalDeviceFeatures
         where
        type FieldType "shaderStorageImageReadWithoutFormat"
               VkPhysicalDeviceFeatures
             = VkBool32
        type FieldOptional "shaderStorageImageReadWithoutFormat"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderStorageImageReadWithoutFormat"
               VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, shaderStorageImageReadWithoutFormat}
        type FieldIsArray "shaderStorageImageReadWithoutFormat"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderStorageImageReadWithoutFormat}

instance {-# OVERLAPPING #-}
         CanReadField "shaderStorageImageReadWithoutFormat"
           VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderStorageImageReadWithoutFormat})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageReadWithoutFormat}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderStorageImageReadWithoutFormat"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageReadWithoutFormat}

instance {-# OVERLAPPING #-}
         HasField "shaderStorageImageWriteWithoutFormat"
           VkPhysicalDeviceFeatures
         where
        type FieldType "shaderStorageImageWriteWithoutFormat"
               VkPhysicalDeviceFeatures
             = VkBool32
        type FieldOptional "shaderStorageImageWriteWithoutFormat"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderStorageImageWriteWithoutFormat"
               VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, shaderStorageImageWriteWithoutFormat}
        type FieldIsArray "shaderStorageImageWriteWithoutFormat"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderStorageImageWriteWithoutFormat}

instance {-# OVERLAPPING #-}
         CanReadField "shaderStorageImageWriteWithoutFormat"
           VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderStorageImageWriteWithoutFormat})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageWriteWithoutFormat}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderStorageImageWriteWithoutFormat"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageWriteWithoutFormat}

instance {-# OVERLAPPING #-}
         HasField "shaderUniformBufferArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        type FieldType "shaderUniformBufferArrayDynamicIndexing"
               VkPhysicalDeviceFeatures
             = VkBool32
        type FieldOptional "shaderUniformBufferArrayDynamicIndexing"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderUniformBufferArrayDynamicIndexing"
               VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, shaderUniformBufferArrayDynamicIndexing}
        type FieldIsArray "shaderUniformBufferArrayDynamicIndexing"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderUniformBufferArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         CanReadField "shaderUniformBufferArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderUniformBufferArrayDynamicIndexing})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderUniformBufferArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderUniformBufferArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderUniformBufferArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         HasField "shaderSampledImageArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        type FieldType "shaderSampledImageArrayDynamicIndexing"
               VkPhysicalDeviceFeatures
             = VkBool32
        type FieldOptional "shaderSampledImageArrayDynamicIndexing"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderSampledImageArrayDynamicIndexing"
               VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, shaderSampledImageArrayDynamicIndexing}
        type FieldIsArray "shaderSampledImageArrayDynamicIndexing"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderSampledImageArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         CanReadField "shaderSampledImageArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderSampledImageArrayDynamicIndexing})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderSampledImageArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderSampledImageArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderSampledImageArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         HasField "shaderStorageBufferArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        type FieldType "shaderStorageBufferArrayDynamicIndexing"
               VkPhysicalDeviceFeatures
             = VkBool32
        type FieldOptional "shaderStorageBufferArrayDynamicIndexing"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderStorageBufferArrayDynamicIndexing"
               VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, shaderStorageBufferArrayDynamicIndexing}
        type FieldIsArray "shaderStorageBufferArrayDynamicIndexing"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderStorageBufferArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         CanReadField "shaderStorageBufferArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderStorageBufferArrayDynamicIndexing})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageBufferArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderStorageBufferArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageBufferArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         HasField "shaderStorageImageArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        type FieldType "shaderStorageImageArrayDynamicIndexing"
               VkPhysicalDeviceFeatures
             = VkBool32
        type FieldOptional "shaderStorageImageArrayDynamicIndexing"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderStorageImageArrayDynamicIndexing"
               VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, shaderStorageImageArrayDynamicIndexing}
        type FieldIsArray "shaderStorageImageArrayDynamicIndexing"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderStorageImageArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         CanReadField "shaderStorageImageArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderStorageImageArrayDynamicIndexing})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderStorageImageArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageArrayDynamicIndexing}

instance {-# OVERLAPPING #-}
         HasField "shaderClipDistance" VkPhysicalDeviceFeatures where
        type FieldType "shaderClipDistance" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "shaderClipDistance" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderClipDistance" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, shaderClipDistance}
        type FieldIsArray "shaderClipDistance" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderClipDistance}

instance {-# OVERLAPPING #-}
         CanReadField "shaderClipDistance" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderClipDistance})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderClipDistance}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderClipDistance" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderClipDistance}

instance {-# OVERLAPPING #-}
         HasField "shaderCullDistance" VkPhysicalDeviceFeatures where
        type FieldType "shaderCullDistance" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "shaderCullDistance" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderCullDistance" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, shaderCullDistance}
        type FieldIsArray "shaderCullDistance" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderCullDistance}

instance {-# OVERLAPPING #-}
         CanReadField "shaderCullDistance" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderCullDistance})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderCullDistance}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderCullDistance" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderCullDistance}

instance {-# OVERLAPPING #-}
         HasField "shaderFloat64" VkPhysicalDeviceFeatures where
        type FieldType "shaderFloat64" VkPhysicalDeviceFeatures = VkBool32
        type FieldOptional "shaderFloat64" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderFloat64" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, shaderFloat64}
        type FieldIsArray "shaderFloat64" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderFloat64}

instance {-# OVERLAPPING #-}
         CanReadField "shaderFloat64" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderFloat64})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderFloat64}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderFloat64" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderFloat64}

instance {-# OVERLAPPING #-}
         HasField "shaderInt64" VkPhysicalDeviceFeatures where
        type FieldType "shaderInt64" VkPhysicalDeviceFeatures = VkBool32
        type FieldOptional "shaderInt64" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderInt64" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, shaderInt64}
        type FieldIsArray "shaderInt64" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderInt64}

instance {-# OVERLAPPING #-}
         CanReadField "shaderInt64" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderInt64})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderInt64}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderInt64" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderInt64}

instance {-# OVERLAPPING #-}
         HasField "shaderInt16" VkPhysicalDeviceFeatures where
        type FieldType "shaderInt16" VkPhysicalDeviceFeatures = VkBool32
        type FieldOptional "shaderInt16" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderInt16" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, shaderInt16}
        type FieldIsArray "shaderInt16" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderInt16}

instance {-# OVERLAPPING #-}
         CanReadField "shaderInt16" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderInt16})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderInt16}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderInt16" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderInt16}

instance {-# OVERLAPPING #-}
         HasField "shaderResourceResidency" VkPhysicalDeviceFeatures where
        type FieldType "shaderResourceResidency" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "shaderResourceResidency"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderResourceResidency" VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, shaderResourceResidency}
        type FieldIsArray "shaderResourceResidency"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderResourceResidency}

instance {-# OVERLAPPING #-}
         CanReadField "shaderResourceResidency" VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderResourceResidency})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderResourceResidency}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderResourceResidency" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderResourceResidency}

instance {-# OVERLAPPING #-}
         HasField "shaderResourceMinLod" VkPhysicalDeviceFeatures where
        type FieldType "shaderResourceMinLod" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "shaderResourceMinLod" VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderResourceMinLod" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, shaderResourceMinLod}
        type FieldIsArray "shaderResourceMinLod" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, shaderResourceMinLod}

instance {-# OVERLAPPING #-}
         CanReadField "shaderResourceMinLod" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderResourceMinLod})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderResourceMinLod}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderResourceMinLod" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderResourceMinLod}

instance {-# OVERLAPPING #-}
         HasField "sparseBinding" VkPhysicalDeviceFeatures where
        type FieldType "sparseBinding" VkPhysicalDeviceFeatures = VkBool32
        type FieldOptional "sparseBinding" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sparseBinding" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, sparseBinding}
        type FieldIsArray "sparseBinding" VkPhysicalDeviceFeatures = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, sparseBinding}

instance {-# OVERLAPPING #-}
         CanReadField "sparseBinding" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseBinding})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseBinding}

instance {-# OVERLAPPING #-}
         CanWriteField "sparseBinding" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseBinding}

instance {-# OVERLAPPING #-}
         HasField "sparseResidencyBuffer" VkPhysicalDeviceFeatures where
        type FieldType "sparseResidencyBuffer" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "sparseResidencyBuffer" VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sparseResidencyBuffer" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, sparseResidencyBuffer}
        type FieldIsArray "sparseResidencyBuffer" VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, sparseResidencyBuffer}

instance {-# OVERLAPPING #-}
         CanReadField "sparseResidencyBuffer" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseResidencyBuffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidencyBuffer}

instance {-# OVERLAPPING #-}
         CanWriteField "sparseResidencyBuffer" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidencyBuffer}

instance {-# OVERLAPPING #-}
         HasField "sparseResidencyImage2D" VkPhysicalDeviceFeatures where
        type FieldType "sparseResidencyImage2D" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "sparseResidencyImage2D"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sparseResidencyImage2D" VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, sparseResidencyImage2D}
        type FieldIsArray "sparseResidencyImage2D" VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, sparseResidencyImage2D}

instance {-# OVERLAPPING #-}
         CanReadField "sparseResidencyImage2D" VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseResidencyImage2D})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidencyImage2D}

instance {-# OVERLAPPING #-}
         CanWriteField "sparseResidencyImage2D" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidencyImage2D}

instance {-# OVERLAPPING #-}
         HasField "sparseResidencyImage3D" VkPhysicalDeviceFeatures where
        type FieldType "sparseResidencyImage3D" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "sparseResidencyImage3D"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sparseResidencyImage3D" VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, sparseResidencyImage3D}
        type FieldIsArray "sparseResidencyImage3D" VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, sparseResidencyImage3D}

instance {-# OVERLAPPING #-}
         CanReadField "sparseResidencyImage3D" VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseResidencyImage3D})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidencyImage3D}

instance {-# OVERLAPPING #-}
         CanWriteField "sparseResidencyImage3D" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidencyImage3D}

instance {-# OVERLAPPING #-}
         HasField "sparseResidency2Samples" VkPhysicalDeviceFeatures where
        type FieldType "sparseResidency2Samples" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "sparseResidency2Samples"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sparseResidency2Samples" VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, sparseResidency2Samples}
        type FieldIsArray "sparseResidency2Samples"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, sparseResidency2Samples}

instance {-# OVERLAPPING #-}
         CanReadField "sparseResidency2Samples" VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseResidency2Samples})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidency2Samples}

instance {-# OVERLAPPING #-}
         CanWriteField "sparseResidency2Samples" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidency2Samples}

instance {-# OVERLAPPING #-}
         HasField "sparseResidency4Samples" VkPhysicalDeviceFeatures where
        type FieldType "sparseResidency4Samples" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "sparseResidency4Samples"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sparseResidency4Samples" VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, sparseResidency4Samples}
        type FieldIsArray "sparseResidency4Samples"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, sparseResidency4Samples}

instance {-# OVERLAPPING #-}
         CanReadField "sparseResidency4Samples" VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseResidency4Samples})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidency4Samples}

instance {-# OVERLAPPING #-}
         CanWriteField "sparseResidency4Samples" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidency4Samples}

instance {-# OVERLAPPING #-}
         HasField "sparseResidency8Samples" VkPhysicalDeviceFeatures where
        type FieldType "sparseResidency8Samples" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "sparseResidency8Samples"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sparseResidency8Samples" VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, sparseResidency8Samples}
        type FieldIsArray "sparseResidency8Samples"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, sparseResidency8Samples}

instance {-# OVERLAPPING #-}
         CanReadField "sparseResidency8Samples" VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseResidency8Samples})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidency8Samples}

instance {-# OVERLAPPING #-}
         CanWriteField "sparseResidency8Samples" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidency8Samples}

instance {-# OVERLAPPING #-}
         HasField "sparseResidency16Samples" VkPhysicalDeviceFeatures where
        type FieldType "sparseResidency16Samples" VkPhysicalDeviceFeatures
             = VkBool32
        type FieldOptional "sparseResidency16Samples"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sparseResidency16Samples"
               VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, sparseResidency16Samples}
        type FieldIsArray "sparseResidency16Samples"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, sparseResidency16Samples}

instance {-# OVERLAPPING #-}
         CanReadField "sparseResidency16Samples" VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseResidency16Samples})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidency16Samples}

instance {-# OVERLAPPING #-}
         CanWriteField "sparseResidency16Samples" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidency16Samples}

instance {-# OVERLAPPING #-}
         HasField "sparseResidencyAliased" VkPhysicalDeviceFeatures where
        type FieldType "sparseResidencyAliased" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "sparseResidencyAliased"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sparseResidencyAliased" VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, sparseResidencyAliased}
        type FieldIsArray "sparseResidencyAliased" VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, sparseResidencyAliased}

instance {-# OVERLAPPING #-}
         CanReadField "sparseResidencyAliased" VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseResidencyAliased})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidencyAliased}

instance {-# OVERLAPPING #-}
         CanWriteField "sparseResidencyAliased" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidencyAliased}

instance {-# OVERLAPPING #-}
         HasField "variableMultisampleRate" VkPhysicalDeviceFeatures where
        type FieldType "variableMultisampleRate" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "variableMultisampleRate"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "variableMultisampleRate" VkPhysicalDeviceFeatures
             =
             #{offset VkPhysicalDeviceFeatures, variableMultisampleRate}
        type FieldIsArray "variableMultisampleRate"
               VkPhysicalDeviceFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, variableMultisampleRate}

instance {-# OVERLAPPING #-}
         CanReadField "variableMultisampleRate" VkPhysicalDeviceFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, variableMultisampleRate})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, variableMultisampleRate}

instance {-# OVERLAPPING #-}
         CanWriteField "variableMultisampleRate" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, variableMultisampleRate}

instance {-# OVERLAPPING #-}
         HasField "inheritedQueries" VkPhysicalDeviceFeatures where
        type FieldType "inheritedQueries" VkPhysicalDeviceFeatures =
             VkBool32
        type FieldOptional "inheritedQueries" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "inheritedQueries" VkPhysicalDeviceFeatures =
             #{offset VkPhysicalDeviceFeatures, inheritedQueries}
        type FieldIsArray "inheritedQueries" VkPhysicalDeviceFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceFeatures, inheritedQueries}

instance {-# OVERLAPPING #-}
         CanReadField "inheritedQueries" VkPhysicalDeviceFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, inheritedQueries})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, inheritedQueries}

instance {-# OVERLAPPING #-}
         CanWriteField "inheritedQueries" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, inheritedQueries}

instance Show VkPhysicalDeviceFeatures where
        showsPrec d x
          = showString "VkPhysicalDeviceFeatures {" .
              showString "robustBufferAccess = " .
                showsPrec d (getField @"robustBufferAccess" x) .
                  showString ", " .
                    showString "fullDrawIndexUint32 = " .
                      showsPrec d (getField @"fullDrawIndexUint32" x) .
                        showString ", " .
                          showString "imageCubeArray = " .
                            showsPrec d (getField @"imageCubeArray" x) .
                              showString ", " .
                                showString "independentBlend = " .
                                  showsPrec d (getField @"independentBlend" x) .
                                    showString ", " .
                                      showString "geometryShader = " .
                                        showsPrec d (getField @"geometryShader" x) .
                                          showString ", " .
                                            showString "tessellationShader = " .
                                              showsPrec d (getField @"tessellationShader" x) .
                                                showString ", " .
                                                  showString "sampleRateShading = " .
                                                    showsPrec d (getField @"sampleRateShading" x) .
                                                      showString ", " .
                                                        showString "dualSrcBlend = " .
                                                          showsPrec d (getField @"dualSrcBlend" x) .
                                                            showString ", " .
                                                              showString "logicOp = " .
                                                                showsPrec d (getField @"logicOp" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "multiDrawIndirect = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"multiDrawIndirect"
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "drawIndirectFirstInstance = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"drawIndirectFirstInstance"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "depthClamp = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"depthClamp"
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "depthBiasClamp = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (getField
                                                                                             @"depthBiasClamp"
                                                                                             x)
                                                                                          .
                                                                                          showString
                                                                                            ", "
                                                                                            .
                                                                                            showString
                                                                                              "fillModeNonSolid = "
                                                                                              .
                                                                                              showsPrec
                                                                                                d
                                                                                                (getField
                                                                                                   @"fillModeNonSolid"
                                                                                                   x)
                                                                                                .
                                                                                                showString
                                                                                                  ", "
                                                                                                  .
                                                                                                  showString
                                                                                                    "depthBounds = "
                                                                                                    .
                                                                                                    showsPrec
                                                                                                      d
                                                                                                      (getField
                                                                                                         @"depthBounds"
                                                                                                         x)
                                                                                                      .
                                                                                                      showString
                                                                                                        ", "
                                                                                                        .
                                                                                                        showString
                                                                                                          "wideLines = "
                                                                                                          .
                                                                                                          showsPrec
                                                                                                            d
                                                                                                            (getField
                                                                                                               @"wideLines"
                                                                                                               x)
                                                                                                            .
                                                                                                            showString
                                                                                                              ", "
                                                                                                              .
                                                                                                              showString
                                                                                                                "largePoints = "
                                                                                                                .
                                                                                                                showsPrec
                                                                                                                  d
                                                                                                                  (getField
                                                                                                                     @"largePoints"
                                                                                                                     x)
                                                                                                                  .
                                                                                                                  showString
                                                                                                                    ", "
                                                                                                                    .
                                                                                                                    showString
                                                                                                                      "alphaToOne = "
                                                                                                                      .
                                                                                                                      showsPrec
                                                                                                                        d
                                                                                                                        (getField
                                                                                                                           @"alphaToOne"
                                                                                                                           x)
                                                                                                                        .
                                                                                                                        showString
                                                                                                                          ", "
                                                                                                                          .
                                                                                                                          showString
                                                                                                                            "multiViewport = "
                                                                                                                            .
                                                                                                                            showsPrec
                                                                                                                              d
                                                                                                                              (getField
                                                                                                                                 @"multiViewport"
                                                                                                                                 x)
                                                                                                                              .
                                                                                                                              showString
                                                                                                                                ", "
                                                                                                                                .
                                                                                                                                showString
                                                                                                                                  "samplerAnisotropy = "
                                                                                                                                  .
                                                                                                                                  showsPrec
                                                                                                                                    d
                                                                                                                                    (getField
                                                                                                                                       @"samplerAnisotropy"
                                                                                                                                       x)
                                                                                                                                    .
                                                                                                                                    showString
                                                                                                                                      ", "
                                                                                                                                      .
                                                                                                                                      showString
                                                                                                                                        "textureCompressionETC2 = "
                                                                                                                                        .
                                                                                                                                        showsPrec
                                                                                                                                          d
                                                                                                                                          (getField
                                                                                                                                             @"textureCompressionETC2"
                                                                                                                                             x)
                                                                                                                                          .
                                                                                                                                          showString
                                                                                                                                            ", "
                                                                                                                                            .
                                                                                                                                            showString
                                                                                                                                              "textureCompressionASTC_LDR = "
                                                                                                                                              .
                                                                                                                                              showsPrec
                                                                                                                                                d
                                                                                                                                                (getField
                                                                                                                                                   @"textureCompressionASTC_LDR"
                                                                                                                                                   x)
                                                                                                                                                .
                                                                                                                                                showString
                                                                                                                                                  ", "
                                                                                                                                                  .
                                                                                                                                                  showString
                                                                                                                                                    "textureCompressionBC = "
                                                                                                                                                    .
                                                                                                                                                    showsPrec
                                                                                                                                                      d
                                                                                                                                                      (getField
                                                                                                                                                         @"textureCompressionBC"
                                                                                                                                                         x)
                                                                                                                                                      .
                                                                                                                                                      showString
                                                                                                                                                        ", "
                                                                                                                                                        .
                                                                                                                                                        showString
                                                                                                                                                          "occlusionQueryPrecise = "
                                                                                                                                                          .
                                                                                                                                                          showsPrec
                                                                                                                                                            d
                                                                                                                                                            (getField
                                                                                                                                                               @"occlusionQueryPrecise"
                                                                                                                                                               x)
                                                                                                                                                            .
                                                                                                                                                            showString
                                                                                                                                                              ", "
                                                                                                                                                              .
                                                                                                                                                              showString
                                                                                                                                                                "pipelineStatisticsQuery = "
                                                                                                                                                                .
                                                                                                                                                                showsPrec
                                                                                                                                                                  d
                                                                                                                                                                  (getField
                                                                                                                                                                     @"pipelineStatisticsQuery"
                                                                                                                                                                     x)
                                                                                                                                                                  .
                                                                                                                                                                  showString
                                                                                                                                                                    ", "
                                                                                                                                                                    .
                                                                                                                                                                    showString
                                                                                                                                                                      "vertexPipelineStoresAndAtomics = "
                                                                                                                                                                      .
                                                                                                                                                                      showsPrec
                                                                                                                                                                        d
                                                                                                                                                                        (getField
                                                                                                                                                                           @"vertexPipelineStoresAndAtomics"
                                                                                                                                                                           x)
                                                                                                                                                                        .
                                                                                                                                                                        showString
                                                                                                                                                                          ", "
                                                                                                                                                                          .
                                                                                                                                                                          showString
                                                                                                                                                                            "fragmentStoresAndAtomics = "
                                                                                                                                                                            .
                                                                                                                                                                            showsPrec
                                                                                                                                                                              d
                                                                                                                                                                              (getField
                                                                                                                                                                                 @"fragmentStoresAndAtomics"
                                                                                                                                                                                 x)
                                                                                                                                                                              .
                                                                                                                                                                              showString
                                                                                                                                                                                ", "
                                                                                                                                                                                .
                                                                                                                                                                                showString
                                                                                                                                                                                  "shaderTessellationAndGeometryPointSize = "
                                                                                                                                                                                  .
                                                                                                                                                                                  showsPrec
                                                                                                                                                                                    d
                                                                                                                                                                                    (getField
                                                                                                                                                                                       @"shaderTessellationAndGeometryPointSize"
                                                                                                                                                                                       x)
                                                                                                                                                                                    .
                                                                                                                                                                                    showString
                                                                                                                                                                                      ", "
                                                                                                                                                                                      .
                                                                                                                                                                                      showString
                                                                                                                                                                                        "shaderImageGatherExtended = "
                                                                                                                                                                                        .
                                                                                                                                                                                        showsPrec
                                                                                                                                                                                          d
                                                                                                                                                                                          (getField
                                                                                                                                                                                             @"shaderImageGatherExtended"
                                                                                                                                                                                             x)
                                                                                                                                                                                          .
                                                                                                                                                                                          showString
                                                                                                                                                                                            ", "
                                                                                                                                                                                            .
                                                                                                                                                                                            showString
                                                                                                                                                                                              "shaderStorageImageExtendedFormats = "
                                                                                                                                                                                              .
                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                d
                                                                                                                                                                                                (getField
                                                                                                                                                                                                   @"shaderStorageImageExtendedFormats"
                                                                                                                                                                                                   x)
                                                                                                                                                                                                .
                                                                                                                                                                                                showString
                                                                                                                                                                                                  ", "
                                                                                                                                                                                                  .
                                                                                                                                                                                                  showString
                                                                                                                                                                                                    "shaderStorageImageMultisample = "
                                                                                                                                                                                                    .
                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                      d
                                                                                                                                                                                                      (getField
                                                                                                                                                                                                         @"shaderStorageImageMultisample"
                                                                                                                                                                                                         x)
                                                                                                                                                                                                      .
                                                                                                                                                                                                      showString
                                                                                                                                                                                                        ", "
                                                                                                                                                                                                        .
                                                                                                                                                                                                        showString
                                                                                                                                                                                                          "shaderStorageImageReadWithoutFormat = "
                                                                                                                                                                                                          .
                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                            d
                                                                                                                                                                                                            (getField
                                                                                                                                                                                                               @"shaderStorageImageReadWithoutFormat"
                                                                                                                                                                                                               x)
                                                                                                                                                                                                            .
                                                                                                                                                                                                            showString
                                                                                                                                                                                                              ", "
                                                                                                                                                                                                              .
                                                                                                                                                                                                              showString
                                                                                                                                                                                                                "shaderStorageImageWriteWithoutFormat = "
                                                                                                                                                                                                                .
                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                  d
                                                                                                                                                                                                                  (getField
                                                                                                                                                                                                                     @"shaderStorageImageWriteWithoutFormat"
                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                  .
                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                    .
                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                      "shaderUniformBufferArrayDynamicIndexing = "
                                                                                                                                                                                                                      .
                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                        d
                                                                                                                                                                                                                        (getField
                                                                                                                                                                                                                           @"shaderUniformBufferArrayDynamicIndexing"
                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                        .
                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                          .
                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                            "shaderSampledImageArrayDynamicIndexing = "
                                                                                                                                                                                                                            .
                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                              d
                                                                                                                                                                                                                              (getField
                                                                                                                                                                                                                                 @"shaderSampledImageArrayDynamicIndexing"
                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                              .
                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                  "shaderStorageBufferArrayDynamicIndexing = "
                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                    (getField
                                                                                                                                                                                                                                       @"shaderStorageBufferArrayDynamicIndexing"
                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                        "shaderStorageImageArrayDynamicIndexing = "
                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                          (getField
                                                                                                                                                                                                                                             @"shaderStorageImageArrayDynamicIndexing"
                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                              "shaderClipDistance = "
                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                (getField
                                                                                                                                                                                                                                                   @"shaderClipDistance"
                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                    "shaderCullDistance = "
                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                      (getField
                                                                                                                                                                                                                                                         @"shaderCullDistance"
                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                          "shaderFloat64 = "
                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                            (getField
                                                                                                                                                                                                                                                               @"shaderFloat64"
                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                "shaderInt64 = "
                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                  (getField
                                                                                                                                                                                                                                                                     @"shaderInt64"
                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                      "shaderInt16 = "
                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                        (getField
                                                                                                                                                                                                                                                                           @"shaderInt16"
                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                            "shaderResourceResidency = "
                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                              (getField
                                                                                                                                                                                                                                                                                 @"shaderResourceResidency"
                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                  "shaderResourceMinLod = "
                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                    (getField
                                                                                                                                                                                                                                                                                       @"shaderResourceMinLod"
                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                        "sparseBinding = "
                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                          (getField
                                                                                                                                                                                                                                                                                             @"sparseBinding"
                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                              "sparseResidencyBuffer = "
                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                (getField
                                                                                                                                                                                                                                                                                                   @"sparseResidencyBuffer"
                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                    "sparseResidencyImage2D = "
                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                      (getField
                                                                                                                                                                                                                                                                                                         @"sparseResidencyImage2D"
                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                          "sparseResidencyImage3D = "
                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                                                                            (getField
                                                                                                                                                                                                                                                                                                               @"sparseResidencyImage3D"
                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                "sparseResidency2Samples = "
                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                                                                  (getField
                                                                                                                                                                                                                                                                                                                     @"sparseResidency2Samples"
                                                                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                      "sparseResidency4Samples = "
                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                        (getField
                                                                                                                                                                                                                                                                                                                           @"sparseResidency4Samples"
                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                            "sparseResidency8Samples = "
                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                              (getField
                                                                                                                                                                                                                                                                                                                                 @"sparseResidency8Samples"
                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                  "sparseResidency16Samples = "
                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                                                                    (getField
                                                                                                                                                                                                                                                                                                                                       @"sparseResidency16Samples"
                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                        "sparseResidencyAliased = "
                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                                                                          (getField
                                                                                                                                                                                                                                                                                                                                             @"sparseResidencyAliased"
                                                                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                              "variableMultisampleRate = "
                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                                                                (getField
                                                                                                                                                                                                                                                                                                                                                   @"variableMultisampleRate"
                                                                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                    "inheritedQueries = "
                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                                                                      (getField
                                                                                                                                                                                                                                                                                                                                                         @"inheritedQueries"
                                                                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                      showChar
                                                                                                                                                                                                                                                                                                                                                        '}'
