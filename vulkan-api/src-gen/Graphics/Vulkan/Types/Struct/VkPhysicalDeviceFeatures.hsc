#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures
       (VkPhysicalDeviceFeatures(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes     (VkBool32)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceFeatures.html VkPhysicalDeviceFeatures registry at www.khronos.org>
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
         HasVkRobustBufferAccess VkPhysicalDeviceFeatures where
        type VkRobustBufferAccessMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkRobustBufferAccess #-}
        vkRobustBufferAccess x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, robustBufferAccess})

        {-# INLINE vkRobustBufferAccessByteOffset #-}
        vkRobustBufferAccessByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, robustBufferAccess}

        {-# INLINE readVkRobustBufferAccess #-}
        readVkRobustBufferAccess p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, robustBufferAccess}

        {-# INLINE writeVkRobustBufferAccess #-}
        writeVkRobustBufferAccess p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, robustBufferAccess}

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

instance CanReadField "robustBufferAccess" VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkRobustBufferAccess

        {-# INLINE readField #-}
        readField = readVkRobustBufferAccess

instance CanWriteField "robustBufferAccess"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkRobustBufferAccess

instance {-# OVERLAPPING #-}
         HasVkFullDrawIndexUint32 VkPhysicalDeviceFeatures where
        type VkFullDrawIndexUint32MType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkFullDrawIndexUint32 #-}
        vkFullDrawIndexUint32 x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, fullDrawIndexUint32})

        {-# INLINE vkFullDrawIndexUint32ByteOffset #-}
        vkFullDrawIndexUint32ByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, fullDrawIndexUint32}

        {-# INLINE readVkFullDrawIndexUint32 #-}
        readVkFullDrawIndexUint32 p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, fullDrawIndexUint32}

        {-# INLINE writeVkFullDrawIndexUint32 #-}
        writeVkFullDrawIndexUint32 p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, fullDrawIndexUint32}

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

instance CanReadField "fullDrawIndexUint32"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkFullDrawIndexUint32

        {-# INLINE readField #-}
        readField = readVkFullDrawIndexUint32

instance CanWriteField "fullDrawIndexUint32"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkFullDrawIndexUint32

instance {-# OVERLAPPING #-}
         HasVkImageCubeArray VkPhysicalDeviceFeatures where
        type VkImageCubeArrayMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkImageCubeArray #-}
        vkImageCubeArray x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, imageCubeArray})

        {-# INLINE vkImageCubeArrayByteOffset #-}
        vkImageCubeArrayByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, imageCubeArray}

        {-# INLINE readVkImageCubeArray #-}
        readVkImageCubeArray p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, imageCubeArray}

        {-# INLINE writeVkImageCubeArray #-}
        writeVkImageCubeArray p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, imageCubeArray}

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

instance CanReadField "imageCubeArray" VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkImageCubeArray

        {-# INLINE readField #-}
        readField = readVkImageCubeArray

instance CanWriteField "imageCubeArray" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkImageCubeArray

instance {-# OVERLAPPING #-}
         HasVkIndependentBlend VkPhysicalDeviceFeatures where
        type VkIndependentBlendMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkIndependentBlend #-}
        vkIndependentBlend x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, independentBlend})

        {-# INLINE vkIndependentBlendByteOffset #-}
        vkIndependentBlendByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, independentBlend}

        {-# INLINE readVkIndependentBlend #-}
        readVkIndependentBlend p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, independentBlend}

        {-# INLINE writeVkIndependentBlend #-}
        writeVkIndependentBlend p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, independentBlend}

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

instance CanReadField "independentBlend" VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkIndependentBlend

        {-# INLINE readField #-}
        readField = readVkIndependentBlend

instance CanWriteField "independentBlend" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkIndependentBlend

instance {-# OVERLAPPING #-}
         HasVkGeometryShader VkPhysicalDeviceFeatures where
        type VkGeometryShaderMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkGeometryShader #-}
        vkGeometryShader x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, geometryShader})

        {-# INLINE vkGeometryShaderByteOffset #-}
        vkGeometryShaderByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, geometryShader}

        {-# INLINE readVkGeometryShader #-}
        readVkGeometryShader p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, geometryShader}

        {-# INLINE writeVkGeometryShader #-}
        writeVkGeometryShader p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, geometryShader}

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

instance CanReadField "geometryShader" VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkGeometryShader

        {-# INLINE readField #-}
        readField = readVkGeometryShader

instance CanWriteField "geometryShader" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkGeometryShader

instance {-# OVERLAPPING #-}
         HasVkTessellationShader VkPhysicalDeviceFeatures where
        type VkTessellationShaderMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkTessellationShader #-}
        vkTessellationShader x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, tessellationShader})

        {-# INLINE vkTessellationShaderByteOffset #-}
        vkTessellationShaderByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, tessellationShader}

        {-# INLINE readVkTessellationShader #-}
        readVkTessellationShader p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, tessellationShader}

        {-# INLINE writeVkTessellationShader #-}
        writeVkTessellationShader p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, tessellationShader}

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

instance CanReadField "tessellationShader" VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkTessellationShader

        {-# INLINE readField #-}
        readField = readVkTessellationShader

instance CanWriteField "tessellationShader"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkTessellationShader

instance {-# OVERLAPPING #-}
         HasVkSampleRateShading VkPhysicalDeviceFeatures where
        type VkSampleRateShadingMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkSampleRateShading #-}
        vkSampleRateShading x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sampleRateShading})

        {-# INLINE vkSampleRateShadingByteOffset #-}
        vkSampleRateShadingByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, sampleRateShading}

        {-# INLINE readVkSampleRateShading #-}
        readVkSampleRateShading p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sampleRateShading}

        {-# INLINE writeVkSampleRateShading #-}
        writeVkSampleRateShading p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sampleRateShading}

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

instance CanReadField "sampleRateShading" VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkSampleRateShading

        {-# INLINE readField #-}
        readField = readVkSampleRateShading

instance CanWriteField "sampleRateShading" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampleRateShading

instance {-# OVERLAPPING #-}
         HasVkDualSrcBlend VkPhysicalDeviceFeatures where
        type VkDualSrcBlendMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkDualSrcBlend #-}
        vkDualSrcBlend x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, dualSrcBlend})

        {-# INLINE vkDualSrcBlendByteOffset #-}
        vkDualSrcBlendByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, dualSrcBlend}

        {-# INLINE readVkDualSrcBlend #-}
        readVkDualSrcBlend p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, dualSrcBlend}

        {-# INLINE writeVkDualSrcBlend #-}
        writeVkDualSrcBlend p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, dualSrcBlend}

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

instance CanReadField "dualSrcBlend" VkPhysicalDeviceFeatures where
        {-# INLINE getField #-}
        getField = vkDualSrcBlend

        {-# INLINE readField #-}
        readField = readVkDualSrcBlend

instance CanWriteField "dualSrcBlend" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkDualSrcBlend

instance {-# OVERLAPPING #-} HasVkLogicOp VkPhysicalDeviceFeatures
         where
        type VkLogicOpMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkLogicOp #-}
        vkLogicOp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, logicOp})

        {-# INLINE vkLogicOpByteOffset #-}
        vkLogicOpByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, logicOp}

        {-# INLINE readVkLogicOp #-}
        readVkLogicOp p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, logicOp}

        {-# INLINE writeVkLogicOp #-}
        writeVkLogicOp p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, logicOp}

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

instance CanReadField "logicOp" VkPhysicalDeviceFeatures where
        {-# INLINE getField #-}
        getField = vkLogicOp

        {-# INLINE readField #-}
        readField = readVkLogicOp

instance CanWriteField "logicOp" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField = writeVkLogicOp

instance {-# OVERLAPPING #-}
         HasVkMultiDrawIndirect VkPhysicalDeviceFeatures where
        type VkMultiDrawIndirectMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkMultiDrawIndirect #-}
        vkMultiDrawIndirect x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, multiDrawIndirect})

        {-# INLINE vkMultiDrawIndirectByteOffset #-}
        vkMultiDrawIndirectByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, multiDrawIndirect}

        {-# INLINE readVkMultiDrawIndirect #-}
        readVkMultiDrawIndirect p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, multiDrawIndirect}

        {-# INLINE writeVkMultiDrawIndirect #-}
        writeVkMultiDrawIndirect p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, multiDrawIndirect}

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

instance CanReadField "multiDrawIndirect" VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkMultiDrawIndirect

        {-# INLINE readField #-}
        readField = readVkMultiDrawIndirect

instance CanWriteField "multiDrawIndirect" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkMultiDrawIndirect

instance {-# OVERLAPPING #-}
         HasVkDrawIndirectFirstInstance VkPhysicalDeviceFeatures where
        type VkDrawIndirectFirstInstanceMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkDrawIndirectFirstInstance #-}
        vkDrawIndirectFirstInstance x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, drawIndirectFirstInstance})

        {-# INLINE vkDrawIndirectFirstInstanceByteOffset #-}
        vkDrawIndirectFirstInstanceByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, drawIndirectFirstInstance}

        {-# INLINE readVkDrawIndirectFirstInstance #-}
        readVkDrawIndirectFirstInstance p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, drawIndirectFirstInstance}

        {-# INLINE writeVkDrawIndirectFirstInstance #-}
        writeVkDrawIndirectFirstInstance p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, drawIndirectFirstInstance}

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

instance CanReadField "drawIndirectFirstInstance"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkDrawIndirectFirstInstance

        {-# INLINE readField #-}
        readField = readVkDrawIndirectFirstInstance

instance CanWriteField "drawIndirectFirstInstance"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkDrawIndirectFirstInstance

instance {-# OVERLAPPING #-}
         HasVkDepthClamp VkPhysicalDeviceFeatures where
        type VkDepthClampMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkDepthClamp #-}
        vkDepthClamp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, depthClamp})

        {-# INLINE vkDepthClampByteOffset #-}
        vkDepthClampByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, depthClamp}

        {-# INLINE readVkDepthClamp #-}
        readVkDepthClamp p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, depthClamp}

        {-# INLINE writeVkDepthClamp #-}
        writeVkDepthClamp p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, depthClamp}

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

instance CanReadField "depthClamp" VkPhysicalDeviceFeatures where
        {-# INLINE getField #-}
        getField = vkDepthClamp

        {-# INLINE readField #-}
        readField = readVkDepthClamp

instance CanWriteField "depthClamp" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField = writeVkDepthClamp

instance {-# OVERLAPPING #-}
         HasVkDepthBiasClamp VkPhysicalDeviceFeatures where
        type VkDepthBiasClampMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkDepthBiasClamp #-}
        vkDepthBiasClamp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, depthBiasClamp})

        {-# INLINE vkDepthBiasClampByteOffset #-}
        vkDepthBiasClampByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, depthBiasClamp}

        {-# INLINE readVkDepthBiasClamp #-}
        readVkDepthBiasClamp p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, depthBiasClamp}

        {-# INLINE writeVkDepthBiasClamp #-}
        writeVkDepthBiasClamp p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, depthBiasClamp}

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

instance CanReadField "depthBiasClamp" VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkDepthBiasClamp

        {-# INLINE readField #-}
        readField = readVkDepthBiasClamp

instance CanWriteField "depthBiasClamp" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkDepthBiasClamp

instance {-# OVERLAPPING #-}
         HasVkFillModeNonSolid VkPhysicalDeviceFeatures where
        type VkFillModeNonSolidMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkFillModeNonSolid #-}
        vkFillModeNonSolid x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, fillModeNonSolid})

        {-# INLINE vkFillModeNonSolidByteOffset #-}
        vkFillModeNonSolidByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, fillModeNonSolid}

        {-# INLINE readVkFillModeNonSolid #-}
        readVkFillModeNonSolid p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, fillModeNonSolid}

        {-# INLINE writeVkFillModeNonSolid #-}
        writeVkFillModeNonSolid p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, fillModeNonSolid}

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

instance CanReadField "fillModeNonSolid" VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkFillModeNonSolid

        {-# INLINE readField #-}
        readField = readVkFillModeNonSolid

instance CanWriteField "fillModeNonSolid" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkFillModeNonSolid

instance {-# OVERLAPPING #-}
         HasVkDepthBounds VkPhysicalDeviceFeatures where
        type VkDepthBoundsMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkDepthBounds #-}
        vkDepthBounds x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, depthBounds})

        {-# INLINE vkDepthBoundsByteOffset #-}
        vkDepthBoundsByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, depthBounds}

        {-# INLINE readVkDepthBounds #-}
        readVkDepthBounds p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, depthBounds}

        {-# INLINE writeVkDepthBounds #-}
        writeVkDepthBounds p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, depthBounds}

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

instance CanReadField "depthBounds" VkPhysicalDeviceFeatures where
        {-# INLINE getField #-}
        getField = vkDepthBounds

        {-# INLINE readField #-}
        readField = readVkDepthBounds

instance CanWriteField "depthBounds" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField = writeVkDepthBounds

instance {-# OVERLAPPING #-}
         HasVkWideLines VkPhysicalDeviceFeatures where
        type VkWideLinesMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkWideLines #-}
        vkWideLines x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, wideLines})

        {-# INLINE vkWideLinesByteOffset #-}
        vkWideLinesByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, wideLines}

        {-# INLINE readVkWideLines #-}
        readVkWideLines p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, wideLines}

        {-# INLINE writeVkWideLines #-}
        writeVkWideLines p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, wideLines}

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

instance CanReadField "wideLines" VkPhysicalDeviceFeatures where
        {-# INLINE getField #-}
        getField = vkWideLines

        {-# INLINE readField #-}
        readField = readVkWideLines

instance CanWriteField "wideLines" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField = writeVkWideLines

instance {-# OVERLAPPING #-}
         HasVkLargePoints VkPhysicalDeviceFeatures where
        type VkLargePointsMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkLargePoints #-}
        vkLargePoints x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, largePoints})

        {-# INLINE vkLargePointsByteOffset #-}
        vkLargePointsByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, largePoints}

        {-# INLINE readVkLargePoints #-}
        readVkLargePoints p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, largePoints}

        {-# INLINE writeVkLargePoints #-}
        writeVkLargePoints p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, largePoints}

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

instance CanReadField "largePoints" VkPhysicalDeviceFeatures where
        {-# INLINE getField #-}
        getField = vkLargePoints

        {-# INLINE readField #-}
        readField = readVkLargePoints

instance CanWriteField "largePoints" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField = writeVkLargePoints

instance {-# OVERLAPPING #-}
         HasVkAlphaToOne VkPhysicalDeviceFeatures where
        type VkAlphaToOneMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkAlphaToOne #-}
        vkAlphaToOne x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, alphaToOne})

        {-# INLINE vkAlphaToOneByteOffset #-}
        vkAlphaToOneByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, alphaToOne}

        {-# INLINE readVkAlphaToOne #-}
        readVkAlphaToOne p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, alphaToOne}

        {-# INLINE writeVkAlphaToOne #-}
        writeVkAlphaToOne p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, alphaToOne}

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

instance CanReadField "alphaToOne" VkPhysicalDeviceFeatures where
        {-# INLINE getField #-}
        getField = vkAlphaToOne

        {-# INLINE readField #-}
        readField = readVkAlphaToOne

instance CanWriteField "alphaToOne" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField = writeVkAlphaToOne

instance {-# OVERLAPPING #-}
         HasVkMultiViewport VkPhysicalDeviceFeatures where
        type VkMultiViewportMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkMultiViewport #-}
        vkMultiViewport x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, multiViewport})

        {-# INLINE vkMultiViewportByteOffset #-}
        vkMultiViewportByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, multiViewport}

        {-# INLINE readVkMultiViewport #-}
        readVkMultiViewport p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, multiViewport}

        {-# INLINE writeVkMultiViewport #-}
        writeVkMultiViewport p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, multiViewport}

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

instance CanReadField "multiViewport" VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkMultiViewport

        {-# INLINE readField #-}
        readField = readVkMultiViewport

instance CanWriteField "multiViewport" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkMultiViewport

instance {-# OVERLAPPING #-}
         HasVkSamplerAnisotropy VkPhysicalDeviceFeatures where
        type VkSamplerAnisotropyMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkSamplerAnisotropy #-}
        vkSamplerAnisotropy x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, samplerAnisotropy})

        {-# INLINE vkSamplerAnisotropyByteOffset #-}
        vkSamplerAnisotropyByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, samplerAnisotropy}

        {-# INLINE readVkSamplerAnisotropy #-}
        readVkSamplerAnisotropy p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, samplerAnisotropy}

        {-# INLINE writeVkSamplerAnisotropy #-}
        writeVkSamplerAnisotropy p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, samplerAnisotropy}

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

instance CanReadField "samplerAnisotropy" VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkSamplerAnisotropy

        {-# INLINE readField #-}
        readField = readVkSamplerAnisotropy

instance CanWriteField "samplerAnisotropy" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkSamplerAnisotropy

instance {-# OVERLAPPING #-}
         HasVkTextureCompressionETC2 VkPhysicalDeviceFeatures where
        type VkTextureCompressionETC2MType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkTextureCompressionETC2 #-}
        vkTextureCompressionETC2 x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, textureCompressionETC2})

        {-# INLINE vkTextureCompressionETC2ByteOffset #-}
        vkTextureCompressionETC2ByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, textureCompressionETC2}

        {-# INLINE readVkTextureCompressionETC2 #-}
        readVkTextureCompressionETC2 p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, textureCompressionETC2}

        {-# INLINE writeVkTextureCompressionETC2 #-}
        writeVkTextureCompressionETC2 p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, textureCompressionETC2}

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

instance CanReadField "textureCompressionETC2"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkTextureCompressionETC2

        {-# INLINE readField #-}
        readField = readVkTextureCompressionETC2

instance CanWriteField "textureCompressionETC2"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkTextureCompressionETC2

instance {-# OVERLAPPING #-}
         HasVkTextureCompressionASTC_LDR VkPhysicalDeviceFeatures where
        type VkTextureCompressionASTC_LDRMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkTextureCompressionASTC_LDR #-}
        vkTextureCompressionASTC_LDR x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, textureCompressionASTC_LDR})

        {-# INLINE vkTextureCompressionASTC_LDRByteOffset #-}
        vkTextureCompressionASTC_LDRByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, textureCompressionASTC_LDR}

        {-# INLINE readVkTextureCompressionASTC_LDR #-}
        readVkTextureCompressionASTC_LDR p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, textureCompressionASTC_LDR}

        {-# INLINE writeVkTextureCompressionASTC_LDR #-}
        writeVkTextureCompressionASTC_LDR p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, textureCompressionASTC_LDR}

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

instance CanReadField "textureCompressionASTC_LDR"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkTextureCompressionASTC_LDR

        {-# INLINE readField #-}
        readField = readVkTextureCompressionASTC_LDR

instance CanWriteField "textureCompressionASTC_LDR"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkTextureCompressionASTC_LDR

instance {-# OVERLAPPING #-}
         HasVkTextureCompressionBC VkPhysicalDeviceFeatures where
        type VkTextureCompressionBCMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkTextureCompressionBC #-}
        vkTextureCompressionBC x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, textureCompressionBC})

        {-# INLINE vkTextureCompressionBCByteOffset #-}
        vkTextureCompressionBCByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, textureCompressionBC}

        {-# INLINE readVkTextureCompressionBC #-}
        readVkTextureCompressionBC p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, textureCompressionBC}

        {-# INLINE writeVkTextureCompressionBC #-}
        writeVkTextureCompressionBC p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, textureCompressionBC}

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

instance CanReadField "textureCompressionBC"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkTextureCompressionBC

        {-# INLINE readField #-}
        readField = readVkTextureCompressionBC

instance CanWriteField "textureCompressionBC"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkTextureCompressionBC

instance {-# OVERLAPPING #-}
         HasVkOcclusionQueryPrecise VkPhysicalDeviceFeatures where
        type VkOcclusionQueryPreciseMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkOcclusionQueryPrecise #-}
        vkOcclusionQueryPrecise x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, occlusionQueryPrecise})

        {-# INLINE vkOcclusionQueryPreciseByteOffset #-}
        vkOcclusionQueryPreciseByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, occlusionQueryPrecise}

        {-# INLINE readVkOcclusionQueryPrecise #-}
        readVkOcclusionQueryPrecise p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, occlusionQueryPrecise}

        {-# INLINE writeVkOcclusionQueryPrecise #-}
        writeVkOcclusionQueryPrecise p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, occlusionQueryPrecise}

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

instance CanReadField "occlusionQueryPrecise"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkOcclusionQueryPrecise

        {-# INLINE readField #-}
        readField = readVkOcclusionQueryPrecise

instance CanWriteField "occlusionQueryPrecise"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkOcclusionQueryPrecise

instance {-# OVERLAPPING #-}
         HasVkPipelineStatisticsQuery VkPhysicalDeviceFeatures where
        type VkPipelineStatisticsQueryMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkPipelineStatisticsQuery #-}
        vkPipelineStatisticsQuery x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, pipelineStatisticsQuery})

        {-# INLINE vkPipelineStatisticsQueryByteOffset #-}
        vkPipelineStatisticsQueryByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, pipelineStatisticsQuery}

        {-# INLINE readVkPipelineStatisticsQuery #-}
        readVkPipelineStatisticsQuery p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, pipelineStatisticsQuery}

        {-# INLINE writeVkPipelineStatisticsQuery #-}
        writeVkPipelineStatisticsQuery p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, pipelineStatisticsQuery}

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

instance CanReadField "pipelineStatisticsQuery"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkPipelineStatisticsQuery

        {-# INLINE readField #-}
        readField = readVkPipelineStatisticsQuery

instance CanWriteField "pipelineStatisticsQuery"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkPipelineStatisticsQuery

instance {-# OVERLAPPING #-}
         HasVkVertexPipelineStoresAndAtomics VkPhysicalDeviceFeatures where
        type VkVertexPipelineStoresAndAtomicsMType VkPhysicalDeviceFeatures
             = VkBool32

        {-# NOINLINE vkVertexPipelineStoresAndAtomics #-}
        vkVertexPipelineStoresAndAtomics x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, vertexPipelineStoresAndAtomics})

        {-# INLINE vkVertexPipelineStoresAndAtomicsByteOffset #-}
        vkVertexPipelineStoresAndAtomicsByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, vertexPipelineStoresAndAtomics}

        {-# INLINE readVkVertexPipelineStoresAndAtomics #-}
        readVkVertexPipelineStoresAndAtomics p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, vertexPipelineStoresAndAtomics}

        {-# INLINE writeVkVertexPipelineStoresAndAtomics #-}
        writeVkVertexPipelineStoresAndAtomics p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, vertexPipelineStoresAndAtomics}

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

instance CanReadField "vertexPipelineStoresAndAtomics"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkVertexPipelineStoresAndAtomics

        {-# INLINE readField #-}
        readField = readVkVertexPipelineStoresAndAtomics

instance CanWriteField "vertexPipelineStoresAndAtomics"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkVertexPipelineStoresAndAtomics

instance {-# OVERLAPPING #-}
         HasVkFragmentStoresAndAtomics VkPhysicalDeviceFeatures where
        type VkFragmentStoresAndAtomicsMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkFragmentStoresAndAtomics #-}
        vkFragmentStoresAndAtomics x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, fragmentStoresAndAtomics})

        {-# INLINE vkFragmentStoresAndAtomicsByteOffset #-}
        vkFragmentStoresAndAtomicsByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, fragmentStoresAndAtomics}

        {-# INLINE readVkFragmentStoresAndAtomics #-}
        readVkFragmentStoresAndAtomics p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, fragmentStoresAndAtomics}

        {-# INLINE writeVkFragmentStoresAndAtomics #-}
        writeVkFragmentStoresAndAtomics p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, fragmentStoresAndAtomics}

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

instance CanReadField "fragmentStoresAndAtomics"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkFragmentStoresAndAtomics

        {-# INLINE readField #-}
        readField = readVkFragmentStoresAndAtomics

instance CanWriteField "fragmentStoresAndAtomics"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkFragmentStoresAndAtomics

instance {-# OVERLAPPING #-}
         HasVkShaderTessellationAndGeometryPointSize
           VkPhysicalDeviceFeatures
         where
        type VkShaderTessellationAndGeometryPointSizeMType
               VkPhysicalDeviceFeatures
             = VkBool32

        {-# NOINLINE vkShaderTessellationAndGeometryPointSize #-}
        vkShaderTessellationAndGeometryPointSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderTessellationAndGeometryPointSize})

        {-# INLINE vkShaderTessellationAndGeometryPointSizeByteOffset #-}
        vkShaderTessellationAndGeometryPointSizeByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderTessellationAndGeometryPointSize}

        {-# INLINE readVkShaderTessellationAndGeometryPointSize #-}
        readVkShaderTessellationAndGeometryPointSize p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderTessellationAndGeometryPointSize}

        {-# INLINE writeVkShaderTessellationAndGeometryPointSize #-}
        writeVkShaderTessellationAndGeometryPointSize p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderTessellationAndGeometryPointSize}

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

instance CanReadField "shaderTessellationAndGeometryPointSize"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkShaderTessellationAndGeometryPointSize

        {-# INLINE readField #-}
        readField = readVkShaderTessellationAndGeometryPointSize

instance CanWriteField "shaderTessellationAndGeometryPointSize"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkShaderTessellationAndGeometryPointSize

instance {-# OVERLAPPING #-}
         HasVkShaderImageGatherExtended VkPhysicalDeviceFeatures where
        type VkShaderImageGatherExtendedMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkShaderImageGatherExtended #-}
        vkShaderImageGatherExtended x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderImageGatherExtended})

        {-# INLINE vkShaderImageGatherExtendedByteOffset #-}
        vkShaderImageGatherExtendedByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderImageGatherExtended}

        {-# INLINE readVkShaderImageGatherExtended #-}
        readVkShaderImageGatherExtended p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderImageGatherExtended}

        {-# INLINE writeVkShaderImageGatherExtended #-}
        writeVkShaderImageGatherExtended p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderImageGatherExtended}

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

instance CanReadField "shaderImageGatherExtended"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkShaderImageGatherExtended

        {-# INLINE readField #-}
        readField = readVkShaderImageGatherExtended

instance CanWriteField "shaderImageGatherExtended"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkShaderImageGatherExtended

instance {-# OVERLAPPING #-}
         HasVkShaderStorageImageExtendedFormats VkPhysicalDeviceFeatures
         where
        type VkShaderStorageImageExtendedFormatsMType
               VkPhysicalDeviceFeatures
             = VkBool32

        {-# NOINLINE vkShaderStorageImageExtendedFormats #-}
        vkShaderStorageImageExtendedFormats x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderStorageImageExtendedFormats})

        {-# INLINE vkShaderStorageImageExtendedFormatsByteOffset #-}
        vkShaderStorageImageExtendedFormatsByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderStorageImageExtendedFormats}

        {-# INLINE readVkShaderStorageImageExtendedFormats #-}
        readVkShaderStorageImageExtendedFormats p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageExtendedFormats}

        {-# INLINE writeVkShaderStorageImageExtendedFormats #-}
        writeVkShaderStorageImageExtendedFormats p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageExtendedFormats}

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

instance CanReadField "shaderStorageImageExtendedFormats"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkShaderStorageImageExtendedFormats

        {-# INLINE readField #-}
        readField = readVkShaderStorageImageExtendedFormats

instance CanWriteField "shaderStorageImageExtendedFormats"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkShaderStorageImageExtendedFormats

instance {-# OVERLAPPING #-}
         HasVkShaderStorageImageMultisample VkPhysicalDeviceFeatures where
        type VkShaderStorageImageMultisampleMType VkPhysicalDeviceFeatures
             = VkBool32

        {-# NOINLINE vkShaderStorageImageMultisample #-}
        vkShaderStorageImageMultisample x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderStorageImageMultisample})

        {-# INLINE vkShaderStorageImageMultisampleByteOffset #-}
        vkShaderStorageImageMultisampleByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderStorageImageMultisample}

        {-# INLINE readVkShaderStorageImageMultisample #-}
        readVkShaderStorageImageMultisample p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageMultisample}

        {-# INLINE writeVkShaderStorageImageMultisample #-}
        writeVkShaderStorageImageMultisample p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageMultisample}

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

instance CanReadField "shaderStorageImageMultisample"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkShaderStorageImageMultisample

        {-# INLINE readField #-}
        readField = readVkShaderStorageImageMultisample

instance CanWriteField "shaderStorageImageMultisample"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkShaderStorageImageMultisample

instance {-# OVERLAPPING #-}
         HasVkShaderStorageImageReadWithoutFormat VkPhysicalDeviceFeatures
         where
        type VkShaderStorageImageReadWithoutFormatMType
               VkPhysicalDeviceFeatures
             = VkBool32

        {-# NOINLINE vkShaderStorageImageReadWithoutFormat #-}
        vkShaderStorageImageReadWithoutFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderStorageImageReadWithoutFormat})

        {-# INLINE vkShaderStorageImageReadWithoutFormatByteOffset #-}
        vkShaderStorageImageReadWithoutFormatByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderStorageImageReadWithoutFormat}

        {-# INLINE readVkShaderStorageImageReadWithoutFormat #-}
        readVkShaderStorageImageReadWithoutFormat p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageReadWithoutFormat}

        {-# INLINE writeVkShaderStorageImageReadWithoutFormat #-}
        writeVkShaderStorageImageReadWithoutFormat p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageReadWithoutFormat}

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

instance CanReadField "shaderStorageImageReadWithoutFormat"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkShaderStorageImageReadWithoutFormat

        {-# INLINE readField #-}
        readField = readVkShaderStorageImageReadWithoutFormat

instance CanWriteField "shaderStorageImageReadWithoutFormat"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkShaderStorageImageReadWithoutFormat

instance {-# OVERLAPPING #-}
         HasVkShaderStorageImageWriteWithoutFormat VkPhysicalDeviceFeatures
         where
        type VkShaderStorageImageWriteWithoutFormatMType
               VkPhysicalDeviceFeatures
             = VkBool32

        {-# NOINLINE vkShaderStorageImageWriteWithoutFormat #-}
        vkShaderStorageImageWriteWithoutFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderStorageImageWriteWithoutFormat})

        {-# INLINE vkShaderStorageImageWriteWithoutFormatByteOffset #-}
        vkShaderStorageImageWriteWithoutFormatByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderStorageImageWriteWithoutFormat}

        {-# INLINE readVkShaderStorageImageWriteWithoutFormat #-}
        readVkShaderStorageImageWriteWithoutFormat p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageWriteWithoutFormat}

        {-# INLINE writeVkShaderStorageImageWriteWithoutFormat #-}
        writeVkShaderStorageImageWriteWithoutFormat p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageWriteWithoutFormat}

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

instance CanReadField "shaderStorageImageWriteWithoutFormat"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkShaderStorageImageWriteWithoutFormat

        {-# INLINE readField #-}
        readField = readVkShaderStorageImageWriteWithoutFormat

instance CanWriteField "shaderStorageImageWriteWithoutFormat"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkShaderStorageImageWriteWithoutFormat

instance {-# OVERLAPPING #-}
         HasVkShaderUniformBufferArrayDynamicIndexing
           VkPhysicalDeviceFeatures
         where
        type VkShaderUniformBufferArrayDynamicIndexingMType
               VkPhysicalDeviceFeatures
             = VkBool32

        {-# NOINLINE vkShaderUniformBufferArrayDynamicIndexing #-}
        vkShaderUniformBufferArrayDynamicIndexing x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderUniformBufferArrayDynamicIndexing})

        {-# INLINE vkShaderUniformBufferArrayDynamicIndexingByteOffset #-}
        vkShaderUniformBufferArrayDynamicIndexingByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderUniformBufferArrayDynamicIndexing}

        {-# INLINE readVkShaderUniformBufferArrayDynamicIndexing #-}
        readVkShaderUniformBufferArrayDynamicIndexing p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderUniformBufferArrayDynamicIndexing}

        {-# INLINE writeVkShaderUniformBufferArrayDynamicIndexing #-}
        writeVkShaderUniformBufferArrayDynamicIndexing p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderUniformBufferArrayDynamicIndexing}

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

instance CanReadField "shaderUniformBufferArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkShaderUniformBufferArrayDynamicIndexing

        {-# INLINE readField #-}
        readField = readVkShaderUniformBufferArrayDynamicIndexing

instance CanWriteField "shaderUniformBufferArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkShaderUniformBufferArrayDynamicIndexing

instance {-# OVERLAPPING #-}
         HasVkShaderSampledImageArrayDynamicIndexing
           VkPhysicalDeviceFeatures
         where
        type VkShaderSampledImageArrayDynamicIndexingMType
               VkPhysicalDeviceFeatures
             = VkBool32

        {-# NOINLINE vkShaderSampledImageArrayDynamicIndexing #-}
        vkShaderSampledImageArrayDynamicIndexing x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderSampledImageArrayDynamicIndexing})

        {-# INLINE vkShaderSampledImageArrayDynamicIndexingByteOffset #-}
        vkShaderSampledImageArrayDynamicIndexingByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderSampledImageArrayDynamicIndexing}

        {-# INLINE readVkShaderSampledImageArrayDynamicIndexing #-}
        readVkShaderSampledImageArrayDynamicIndexing p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderSampledImageArrayDynamicIndexing}

        {-# INLINE writeVkShaderSampledImageArrayDynamicIndexing #-}
        writeVkShaderSampledImageArrayDynamicIndexing p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderSampledImageArrayDynamicIndexing}

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

instance CanReadField "shaderSampledImageArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkShaderSampledImageArrayDynamicIndexing

        {-# INLINE readField #-}
        readField = readVkShaderSampledImageArrayDynamicIndexing

instance CanWriteField "shaderSampledImageArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkShaderSampledImageArrayDynamicIndexing

instance {-# OVERLAPPING #-}
         HasVkShaderStorageBufferArrayDynamicIndexing
           VkPhysicalDeviceFeatures
         where
        type VkShaderStorageBufferArrayDynamicIndexingMType
               VkPhysicalDeviceFeatures
             = VkBool32

        {-# NOINLINE vkShaderStorageBufferArrayDynamicIndexing #-}
        vkShaderStorageBufferArrayDynamicIndexing x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderStorageBufferArrayDynamicIndexing})

        {-# INLINE vkShaderStorageBufferArrayDynamicIndexingByteOffset #-}
        vkShaderStorageBufferArrayDynamicIndexingByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderStorageBufferArrayDynamicIndexing}

        {-# INLINE readVkShaderStorageBufferArrayDynamicIndexing #-}
        readVkShaderStorageBufferArrayDynamicIndexing p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageBufferArrayDynamicIndexing}

        {-# INLINE writeVkShaderStorageBufferArrayDynamicIndexing #-}
        writeVkShaderStorageBufferArrayDynamicIndexing p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageBufferArrayDynamicIndexing}

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

instance CanReadField "shaderStorageBufferArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkShaderStorageBufferArrayDynamicIndexing

        {-# INLINE readField #-}
        readField = readVkShaderStorageBufferArrayDynamicIndexing

instance CanWriteField "shaderStorageBufferArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkShaderStorageBufferArrayDynamicIndexing

instance {-# OVERLAPPING #-}
         HasVkShaderStorageImageArrayDynamicIndexing
           VkPhysicalDeviceFeatures
         where
        type VkShaderStorageImageArrayDynamicIndexingMType
               VkPhysicalDeviceFeatures
             = VkBool32

        {-# NOINLINE vkShaderStorageImageArrayDynamicIndexing #-}
        vkShaderStorageImageArrayDynamicIndexing x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderStorageImageArrayDynamicIndexing})

        {-# INLINE vkShaderStorageImageArrayDynamicIndexingByteOffset #-}
        vkShaderStorageImageArrayDynamicIndexingByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderStorageImageArrayDynamicIndexing}

        {-# INLINE readVkShaderStorageImageArrayDynamicIndexing #-}
        readVkShaderStorageImageArrayDynamicIndexing p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageArrayDynamicIndexing}

        {-# INLINE writeVkShaderStorageImageArrayDynamicIndexing #-}
        writeVkShaderStorageImageArrayDynamicIndexing p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderStorageImageArrayDynamicIndexing}

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

instance CanReadField "shaderStorageImageArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkShaderStorageImageArrayDynamicIndexing

        {-# INLINE readField #-}
        readField = readVkShaderStorageImageArrayDynamicIndexing

instance CanWriteField "shaderStorageImageArrayDynamicIndexing"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkShaderStorageImageArrayDynamicIndexing

instance {-# OVERLAPPING #-}
         HasVkShaderClipDistance VkPhysicalDeviceFeatures where
        type VkShaderClipDistanceMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkShaderClipDistance #-}
        vkShaderClipDistance x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderClipDistance})

        {-# INLINE vkShaderClipDistanceByteOffset #-}
        vkShaderClipDistanceByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderClipDistance}

        {-# INLINE readVkShaderClipDistance #-}
        readVkShaderClipDistance p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderClipDistance}

        {-# INLINE writeVkShaderClipDistance #-}
        writeVkShaderClipDistance p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderClipDistance}

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

instance CanReadField "shaderClipDistance" VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkShaderClipDistance

        {-# INLINE readField #-}
        readField = readVkShaderClipDistance

instance CanWriteField "shaderClipDistance"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkShaderClipDistance

instance {-# OVERLAPPING #-}
         HasVkShaderCullDistance VkPhysicalDeviceFeatures where
        type VkShaderCullDistanceMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkShaderCullDistance #-}
        vkShaderCullDistance x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderCullDistance})

        {-# INLINE vkShaderCullDistanceByteOffset #-}
        vkShaderCullDistanceByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderCullDistance}

        {-# INLINE readVkShaderCullDistance #-}
        readVkShaderCullDistance p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderCullDistance}

        {-# INLINE writeVkShaderCullDistance #-}
        writeVkShaderCullDistance p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderCullDistance}

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

instance CanReadField "shaderCullDistance" VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkShaderCullDistance

        {-# INLINE readField #-}
        readField = readVkShaderCullDistance

instance CanWriteField "shaderCullDistance"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkShaderCullDistance

instance {-# OVERLAPPING #-}
         HasVkShaderFloat64 VkPhysicalDeviceFeatures where
        type VkShaderFloat64MType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkShaderFloat64 #-}
        vkShaderFloat64 x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderFloat64})

        {-# INLINE vkShaderFloat64ByteOffset #-}
        vkShaderFloat64ByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderFloat64}

        {-# INLINE readVkShaderFloat64 #-}
        readVkShaderFloat64 p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderFloat64}

        {-# INLINE writeVkShaderFloat64 #-}
        writeVkShaderFloat64 p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderFloat64}

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

instance CanReadField "shaderFloat64" VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkShaderFloat64

        {-# INLINE readField #-}
        readField = readVkShaderFloat64

instance CanWriteField "shaderFloat64" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkShaderFloat64

instance {-# OVERLAPPING #-}
         HasVkShaderInt64 VkPhysicalDeviceFeatures where
        type VkShaderInt64MType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkShaderInt64 #-}
        vkShaderInt64 x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderInt64})

        {-# INLINE vkShaderInt64ByteOffset #-}
        vkShaderInt64ByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderInt64}

        {-# INLINE readVkShaderInt64 #-}
        readVkShaderInt64 p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderInt64}

        {-# INLINE writeVkShaderInt64 #-}
        writeVkShaderInt64 p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderInt64}

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

instance CanReadField "shaderInt64" VkPhysicalDeviceFeatures where
        {-# INLINE getField #-}
        getField = vkShaderInt64

        {-# INLINE readField #-}
        readField = readVkShaderInt64

instance CanWriteField "shaderInt64" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField = writeVkShaderInt64

instance {-# OVERLAPPING #-}
         HasVkShaderInt16 VkPhysicalDeviceFeatures where
        type VkShaderInt16MType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkShaderInt16 #-}
        vkShaderInt16 x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderInt16})

        {-# INLINE vkShaderInt16ByteOffset #-}
        vkShaderInt16ByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderInt16}

        {-# INLINE readVkShaderInt16 #-}
        readVkShaderInt16 p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderInt16}

        {-# INLINE writeVkShaderInt16 #-}
        writeVkShaderInt16 p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderInt16}

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

instance CanReadField "shaderInt16" VkPhysicalDeviceFeatures where
        {-# INLINE getField #-}
        getField = vkShaderInt16

        {-# INLINE readField #-}
        readField = readVkShaderInt16

instance CanWriteField "shaderInt16" VkPhysicalDeviceFeatures where
        {-# INLINE writeField #-}
        writeField = writeVkShaderInt16

instance {-# OVERLAPPING #-}
         HasVkShaderResourceResidency VkPhysicalDeviceFeatures where
        type VkShaderResourceResidencyMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkShaderResourceResidency #-}
        vkShaderResourceResidency x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderResourceResidency})

        {-# INLINE vkShaderResourceResidencyByteOffset #-}
        vkShaderResourceResidencyByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderResourceResidency}

        {-# INLINE readVkShaderResourceResidency #-}
        readVkShaderResourceResidency p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderResourceResidency}

        {-# INLINE writeVkShaderResourceResidency #-}
        writeVkShaderResourceResidency p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderResourceResidency}

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

instance CanReadField "shaderResourceResidency"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkShaderResourceResidency

        {-# INLINE readField #-}
        readField = readVkShaderResourceResidency

instance CanWriteField "shaderResourceResidency"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkShaderResourceResidency

instance {-# OVERLAPPING #-}
         HasVkShaderResourceMinLod VkPhysicalDeviceFeatures where
        type VkShaderResourceMinLodMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkShaderResourceMinLod #-}
        vkShaderResourceMinLod x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, shaderResourceMinLod})

        {-# INLINE vkShaderResourceMinLodByteOffset #-}
        vkShaderResourceMinLodByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, shaderResourceMinLod}

        {-# INLINE readVkShaderResourceMinLod #-}
        readVkShaderResourceMinLod p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, shaderResourceMinLod}

        {-# INLINE writeVkShaderResourceMinLod #-}
        writeVkShaderResourceMinLod p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, shaderResourceMinLod}

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

instance CanReadField "shaderResourceMinLod"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkShaderResourceMinLod

        {-# INLINE readField #-}
        readField = readVkShaderResourceMinLod

instance CanWriteField "shaderResourceMinLod"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkShaderResourceMinLod

instance {-# OVERLAPPING #-}
         HasVkSparseBinding VkPhysicalDeviceFeatures where
        type VkSparseBindingMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkSparseBinding #-}
        vkSparseBinding x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseBinding})

        {-# INLINE vkSparseBindingByteOffset #-}
        vkSparseBindingByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, sparseBinding}

        {-# INLINE readVkSparseBinding #-}
        readVkSparseBinding p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseBinding}

        {-# INLINE writeVkSparseBinding #-}
        writeVkSparseBinding p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseBinding}

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

instance CanReadField "sparseBinding" VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkSparseBinding

        {-# INLINE readField #-}
        readField = readVkSparseBinding

instance CanWriteField "sparseBinding" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkSparseBinding

instance {-# OVERLAPPING #-}
         HasVkSparseResidencyBuffer VkPhysicalDeviceFeatures where
        type VkSparseResidencyBufferMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkSparseResidencyBuffer #-}
        vkSparseResidencyBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseResidencyBuffer})

        {-# INLINE vkSparseResidencyBufferByteOffset #-}
        vkSparseResidencyBufferByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, sparseResidencyBuffer}

        {-# INLINE readVkSparseResidencyBuffer #-}
        readVkSparseResidencyBuffer p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidencyBuffer}

        {-# INLINE writeVkSparseResidencyBuffer #-}
        writeVkSparseResidencyBuffer p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidencyBuffer}

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

instance CanReadField "sparseResidencyBuffer"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkSparseResidencyBuffer

        {-# INLINE readField #-}
        readField = readVkSparseResidencyBuffer

instance CanWriteField "sparseResidencyBuffer"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkSparseResidencyBuffer

instance {-# OVERLAPPING #-}
         HasVkSparseResidencyImage2D VkPhysicalDeviceFeatures where
        type VkSparseResidencyImage2DMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkSparseResidencyImage2D #-}
        vkSparseResidencyImage2D x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseResidencyImage2D})

        {-# INLINE vkSparseResidencyImage2DByteOffset #-}
        vkSparseResidencyImage2DByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, sparseResidencyImage2D}

        {-# INLINE readVkSparseResidencyImage2D #-}
        readVkSparseResidencyImage2D p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidencyImage2D}

        {-# INLINE writeVkSparseResidencyImage2D #-}
        writeVkSparseResidencyImage2D p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidencyImage2D}

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

instance CanReadField "sparseResidencyImage2D"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkSparseResidencyImage2D

        {-# INLINE readField #-}
        readField = readVkSparseResidencyImage2D

instance CanWriteField "sparseResidencyImage2D"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkSparseResidencyImage2D

instance {-# OVERLAPPING #-}
         HasVkSparseResidencyImage3D VkPhysicalDeviceFeatures where
        type VkSparseResidencyImage3DMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkSparseResidencyImage3D #-}
        vkSparseResidencyImage3D x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseResidencyImage3D})

        {-# INLINE vkSparseResidencyImage3DByteOffset #-}
        vkSparseResidencyImage3DByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, sparseResidencyImage3D}

        {-# INLINE readVkSparseResidencyImage3D #-}
        readVkSparseResidencyImage3D p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidencyImage3D}

        {-# INLINE writeVkSparseResidencyImage3D #-}
        writeVkSparseResidencyImage3D p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidencyImage3D}

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

instance CanReadField "sparseResidencyImage3D"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkSparseResidencyImage3D

        {-# INLINE readField #-}
        readField = readVkSparseResidencyImage3D

instance CanWriteField "sparseResidencyImage3D"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkSparseResidencyImage3D

instance {-# OVERLAPPING #-}
         HasVkSparseResidency2Samples VkPhysicalDeviceFeatures where
        type VkSparseResidency2SamplesMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkSparseResidency2Samples #-}
        vkSparseResidency2Samples x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseResidency2Samples})

        {-# INLINE vkSparseResidency2SamplesByteOffset #-}
        vkSparseResidency2SamplesByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, sparseResidency2Samples}

        {-# INLINE readVkSparseResidency2Samples #-}
        readVkSparseResidency2Samples p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidency2Samples}

        {-# INLINE writeVkSparseResidency2Samples #-}
        writeVkSparseResidency2Samples p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidency2Samples}

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

instance CanReadField "sparseResidency2Samples"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkSparseResidency2Samples

        {-# INLINE readField #-}
        readField = readVkSparseResidency2Samples

instance CanWriteField "sparseResidency2Samples"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkSparseResidency2Samples

instance {-# OVERLAPPING #-}
         HasVkSparseResidency4Samples VkPhysicalDeviceFeatures where
        type VkSparseResidency4SamplesMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkSparseResidency4Samples #-}
        vkSparseResidency4Samples x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseResidency4Samples})

        {-# INLINE vkSparseResidency4SamplesByteOffset #-}
        vkSparseResidency4SamplesByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, sparseResidency4Samples}

        {-# INLINE readVkSparseResidency4Samples #-}
        readVkSparseResidency4Samples p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidency4Samples}

        {-# INLINE writeVkSparseResidency4Samples #-}
        writeVkSparseResidency4Samples p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidency4Samples}

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

instance CanReadField "sparseResidency4Samples"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkSparseResidency4Samples

        {-# INLINE readField #-}
        readField = readVkSparseResidency4Samples

instance CanWriteField "sparseResidency4Samples"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkSparseResidency4Samples

instance {-# OVERLAPPING #-}
         HasVkSparseResidency8Samples VkPhysicalDeviceFeatures where
        type VkSparseResidency8SamplesMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkSparseResidency8Samples #-}
        vkSparseResidency8Samples x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseResidency8Samples})

        {-# INLINE vkSparseResidency8SamplesByteOffset #-}
        vkSparseResidency8SamplesByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, sparseResidency8Samples}

        {-# INLINE readVkSparseResidency8Samples #-}
        readVkSparseResidency8Samples p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidency8Samples}

        {-# INLINE writeVkSparseResidency8Samples #-}
        writeVkSparseResidency8Samples p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidency8Samples}

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

instance CanReadField "sparseResidency8Samples"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkSparseResidency8Samples

        {-# INLINE readField #-}
        readField = readVkSparseResidency8Samples

instance CanWriteField "sparseResidency8Samples"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkSparseResidency8Samples

instance {-# OVERLAPPING #-}
         HasVkSparseResidency16Samples VkPhysicalDeviceFeatures where
        type VkSparseResidency16SamplesMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkSparseResidency16Samples #-}
        vkSparseResidency16Samples x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseResidency16Samples})

        {-# INLINE vkSparseResidency16SamplesByteOffset #-}
        vkSparseResidency16SamplesByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, sparseResidency16Samples}

        {-# INLINE readVkSparseResidency16Samples #-}
        readVkSparseResidency16Samples p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidency16Samples}

        {-# INLINE writeVkSparseResidency16Samples #-}
        writeVkSparseResidency16Samples p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidency16Samples}

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

instance CanReadField "sparseResidency16Samples"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkSparseResidency16Samples

        {-# INLINE readField #-}
        readField = readVkSparseResidency16Samples

instance CanWriteField "sparseResidency16Samples"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkSparseResidency16Samples

instance {-# OVERLAPPING #-}
         HasVkSparseResidencyAliased VkPhysicalDeviceFeatures where
        type VkSparseResidencyAliasedMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkSparseResidencyAliased #-}
        vkSparseResidencyAliased x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, sparseResidencyAliased})

        {-# INLINE vkSparseResidencyAliasedByteOffset #-}
        vkSparseResidencyAliasedByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, sparseResidencyAliased}

        {-# INLINE readVkSparseResidencyAliased #-}
        readVkSparseResidencyAliased p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidencyAliased}

        {-# INLINE writeVkSparseResidencyAliased #-}
        writeVkSparseResidencyAliased p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, sparseResidencyAliased}

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

instance CanReadField "sparseResidencyAliased"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkSparseResidencyAliased

        {-# INLINE readField #-}
        readField = readVkSparseResidencyAliased

instance CanWriteField "sparseResidencyAliased"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkSparseResidencyAliased

instance {-# OVERLAPPING #-}
         HasVkVariableMultisampleRate VkPhysicalDeviceFeatures where
        type VkVariableMultisampleRateMType VkPhysicalDeviceFeatures =
             VkBool32

        {-# NOINLINE vkVariableMultisampleRate #-}
        vkVariableMultisampleRate x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, variableMultisampleRate})

        {-# INLINE vkVariableMultisampleRateByteOffset #-}
        vkVariableMultisampleRateByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, variableMultisampleRate}

        {-# INLINE readVkVariableMultisampleRate #-}
        readVkVariableMultisampleRate p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, variableMultisampleRate}

        {-# INLINE writeVkVariableMultisampleRate #-}
        writeVkVariableMultisampleRate p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, variableMultisampleRate}

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

instance CanReadField "variableMultisampleRate"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkVariableMultisampleRate

        {-# INLINE readField #-}
        readField = readVkVariableMultisampleRate

instance CanWriteField "variableMultisampleRate"
           VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkVariableMultisampleRate

instance {-# OVERLAPPING #-}
         HasVkInheritedQueries VkPhysicalDeviceFeatures where
        type VkInheritedQueriesMType VkPhysicalDeviceFeatures = VkBool32

        {-# NOINLINE vkInheritedQueries #-}
        vkInheritedQueries x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures, inheritedQueries})

        {-# INLINE vkInheritedQueriesByteOffset #-}
        vkInheritedQueriesByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures, inheritedQueries}

        {-# INLINE readVkInheritedQueries #-}
        readVkInheritedQueries p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures, inheritedQueries}

        {-# INLINE writeVkInheritedQueries #-}
        writeVkInheritedQueries p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures, inheritedQueries}

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

instance CanReadField "inheritedQueries" VkPhysicalDeviceFeatures
         where
        {-# INLINE getField #-}
        getField = vkInheritedQueries

        {-# INLINE readField #-}
        readField = readVkInheritedQueries

instance CanWriteField "inheritedQueries" VkPhysicalDeviceFeatures
         where
        {-# INLINE writeField #-}
        writeField = writeVkInheritedQueries

instance Show VkPhysicalDeviceFeatures where
        showsPrec d x
          = showString "VkPhysicalDeviceFeatures {" .
              showString "vkRobustBufferAccess = " .
                showsPrec d (vkRobustBufferAccess x) .
                  showString ", " .
                    showString "vkFullDrawIndexUint32 = " .
                      showsPrec d (vkFullDrawIndexUint32 x) .
                        showString ", " .
                          showString "vkImageCubeArray = " .
                            showsPrec d (vkImageCubeArray x) .
                              showString ", " .
                                showString "vkIndependentBlend = " .
                                  showsPrec d (vkIndependentBlend x) .
                                    showString ", " .
                                      showString "vkGeometryShader = " .
                                        showsPrec d (vkGeometryShader x) .
                                          showString ", " .
                                            showString "vkTessellationShader = " .
                                              showsPrec d (vkTessellationShader x) .
                                                showString ", " .
                                                  showString "vkSampleRateShading = " .
                                                    showsPrec d (vkSampleRateShading x) .
                                                      showString ", " .
                                                        showString "vkDualSrcBlend = " .
                                                          showsPrec d (vkDualSrcBlend x) .
                                                            showString ", " .
                                                              showString "vkLogicOp = " .
                                                                showsPrec d (vkLogicOp x) .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkMultiDrawIndirect = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkMultiDrawIndirect x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "vkDrawIndirectFirstInstance = "
                                                                            .
                                                                            showsPrec d
                                                                              (vkDrawIndirectFirstInstance
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "vkDepthClamp = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (vkDepthClamp x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "vkDepthBiasClamp = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (vkDepthBiasClamp
                                                                                             x)
                                                                                          .
                                                                                          showString
                                                                                            ", "
                                                                                            .
                                                                                            showString
                                                                                              "vkFillModeNonSolid = "
                                                                                              .
                                                                                              showsPrec
                                                                                                d
                                                                                                (vkFillModeNonSolid
                                                                                                   x)
                                                                                                .
                                                                                                showString
                                                                                                  ", "
                                                                                                  .
                                                                                                  showString
                                                                                                    "vkDepthBounds = "
                                                                                                    .
                                                                                                    showsPrec
                                                                                                      d
                                                                                                      (vkDepthBounds
                                                                                                         x)
                                                                                                      .
                                                                                                      showString
                                                                                                        ", "
                                                                                                        .
                                                                                                        showString
                                                                                                          "vkWideLines = "
                                                                                                          .
                                                                                                          showsPrec
                                                                                                            d
                                                                                                            (vkWideLines
                                                                                                               x)
                                                                                                            .
                                                                                                            showString
                                                                                                              ", "
                                                                                                              .
                                                                                                              showString
                                                                                                                "vkLargePoints = "
                                                                                                                .
                                                                                                                showsPrec
                                                                                                                  d
                                                                                                                  (vkLargePoints
                                                                                                                     x)
                                                                                                                  .
                                                                                                                  showString
                                                                                                                    ", "
                                                                                                                    .
                                                                                                                    showString
                                                                                                                      "vkAlphaToOne = "
                                                                                                                      .
                                                                                                                      showsPrec
                                                                                                                        d
                                                                                                                        (vkAlphaToOne
                                                                                                                           x)
                                                                                                                        .
                                                                                                                        showString
                                                                                                                          ", "
                                                                                                                          .
                                                                                                                          showString
                                                                                                                            "vkMultiViewport = "
                                                                                                                            .
                                                                                                                            showsPrec
                                                                                                                              d
                                                                                                                              (vkMultiViewport
                                                                                                                                 x)
                                                                                                                              .
                                                                                                                              showString
                                                                                                                                ", "
                                                                                                                                .
                                                                                                                                showString
                                                                                                                                  "vkSamplerAnisotropy = "
                                                                                                                                  .
                                                                                                                                  showsPrec
                                                                                                                                    d
                                                                                                                                    (vkSamplerAnisotropy
                                                                                                                                       x)
                                                                                                                                    .
                                                                                                                                    showString
                                                                                                                                      ", "
                                                                                                                                      .
                                                                                                                                      showString
                                                                                                                                        "vkTextureCompressionETC2 = "
                                                                                                                                        .
                                                                                                                                        showsPrec
                                                                                                                                          d
                                                                                                                                          (vkTextureCompressionETC2
                                                                                                                                             x)
                                                                                                                                          .
                                                                                                                                          showString
                                                                                                                                            ", "
                                                                                                                                            .
                                                                                                                                            showString
                                                                                                                                              "vkTextureCompressionASTC_LDR = "
                                                                                                                                              .
                                                                                                                                              showsPrec
                                                                                                                                                d
                                                                                                                                                (vkTextureCompressionASTC_LDR
                                                                                                                                                   x)
                                                                                                                                                .
                                                                                                                                                showString
                                                                                                                                                  ", "
                                                                                                                                                  .
                                                                                                                                                  showString
                                                                                                                                                    "vkTextureCompressionBC = "
                                                                                                                                                    .
                                                                                                                                                    showsPrec
                                                                                                                                                      d
                                                                                                                                                      (vkTextureCompressionBC
                                                                                                                                                         x)
                                                                                                                                                      .
                                                                                                                                                      showString
                                                                                                                                                        ", "
                                                                                                                                                        .
                                                                                                                                                        showString
                                                                                                                                                          "vkOcclusionQueryPrecise = "
                                                                                                                                                          .
                                                                                                                                                          showsPrec
                                                                                                                                                            d
                                                                                                                                                            (vkOcclusionQueryPrecise
                                                                                                                                                               x)
                                                                                                                                                            .
                                                                                                                                                            showString
                                                                                                                                                              ", "
                                                                                                                                                              .
                                                                                                                                                              showString
                                                                                                                                                                "vkPipelineStatisticsQuery = "
                                                                                                                                                                .
                                                                                                                                                                showsPrec
                                                                                                                                                                  d
                                                                                                                                                                  (vkPipelineStatisticsQuery
                                                                                                                                                                     x)
                                                                                                                                                                  .
                                                                                                                                                                  showString
                                                                                                                                                                    ", "
                                                                                                                                                                    .
                                                                                                                                                                    showString
                                                                                                                                                                      "vkVertexPipelineStoresAndAtomics = "
                                                                                                                                                                      .
                                                                                                                                                                      showsPrec
                                                                                                                                                                        d
                                                                                                                                                                        (vkVertexPipelineStoresAndAtomics
                                                                                                                                                                           x)
                                                                                                                                                                        .
                                                                                                                                                                        showString
                                                                                                                                                                          ", "
                                                                                                                                                                          .
                                                                                                                                                                          showString
                                                                                                                                                                            "vkFragmentStoresAndAtomics = "
                                                                                                                                                                            .
                                                                                                                                                                            showsPrec
                                                                                                                                                                              d
                                                                                                                                                                              (vkFragmentStoresAndAtomics
                                                                                                                                                                                 x)
                                                                                                                                                                              .
                                                                                                                                                                              showString
                                                                                                                                                                                ", "
                                                                                                                                                                                .
                                                                                                                                                                                showString
                                                                                                                                                                                  "vkShaderTessellationAndGeometryPointSize = "
                                                                                                                                                                                  .
                                                                                                                                                                                  showsPrec
                                                                                                                                                                                    d
                                                                                                                                                                                    (vkShaderTessellationAndGeometryPointSize
                                                                                                                                                                                       x)
                                                                                                                                                                                    .
                                                                                                                                                                                    showString
                                                                                                                                                                                      ", "
                                                                                                                                                                                      .
                                                                                                                                                                                      showString
                                                                                                                                                                                        "vkShaderImageGatherExtended = "
                                                                                                                                                                                        .
                                                                                                                                                                                        showsPrec
                                                                                                                                                                                          d
                                                                                                                                                                                          (vkShaderImageGatherExtended
                                                                                                                                                                                             x)
                                                                                                                                                                                          .
                                                                                                                                                                                          showString
                                                                                                                                                                                            ", "
                                                                                                                                                                                            .
                                                                                                                                                                                            showString
                                                                                                                                                                                              "vkShaderStorageImageExtendedFormats = "
                                                                                                                                                                                              .
                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                d
                                                                                                                                                                                                (vkShaderStorageImageExtendedFormats
                                                                                                                                                                                                   x)
                                                                                                                                                                                                .
                                                                                                                                                                                                showString
                                                                                                                                                                                                  ", "
                                                                                                                                                                                                  .
                                                                                                                                                                                                  showString
                                                                                                                                                                                                    "vkShaderStorageImageMultisample = "
                                                                                                                                                                                                    .
                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                      d
                                                                                                                                                                                                      (vkShaderStorageImageMultisample
                                                                                                                                                                                                         x)
                                                                                                                                                                                                      .
                                                                                                                                                                                                      showString
                                                                                                                                                                                                        ", "
                                                                                                                                                                                                        .
                                                                                                                                                                                                        showString
                                                                                                                                                                                                          "vkShaderStorageImageReadWithoutFormat = "
                                                                                                                                                                                                          .
                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                            d
                                                                                                                                                                                                            (vkShaderStorageImageReadWithoutFormat
                                                                                                                                                                                                               x)
                                                                                                                                                                                                            .
                                                                                                                                                                                                            showString
                                                                                                                                                                                                              ", "
                                                                                                                                                                                                              .
                                                                                                                                                                                                              showString
                                                                                                                                                                                                                "vkShaderStorageImageWriteWithoutFormat = "
                                                                                                                                                                                                                .
                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                  d
                                                                                                                                                                                                                  (vkShaderStorageImageWriteWithoutFormat
                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                  .
                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                    .
                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                      "vkShaderUniformBufferArrayDynamicIndexing = "
                                                                                                                                                                                                                      .
                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                        d
                                                                                                                                                                                                                        (vkShaderUniformBufferArrayDynamicIndexing
                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                        .
                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                          .
                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                            "vkShaderSampledImageArrayDynamicIndexing = "
                                                                                                                                                                                                                            .
                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                              d
                                                                                                                                                                                                                              (vkShaderSampledImageArrayDynamicIndexing
                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                              .
                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                  "vkShaderStorageBufferArrayDynamicIndexing = "
                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                    (vkShaderStorageBufferArrayDynamicIndexing
                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                        "vkShaderStorageImageArrayDynamicIndexing = "
                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                          (vkShaderStorageImageArrayDynamicIndexing
                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                              "vkShaderClipDistance = "
                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                (vkShaderClipDistance
                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                    "vkShaderCullDistance = "
                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                      (vkShaderCullDistance
                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                          "vkShaderFloat64 = "
                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                            (vkShaderFloat64
                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                "vkShaderInt64 = "
                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                  (vkShaderInt64
                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                      "vkShaderInt16 = "
                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                        (vkShaderInt16
                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                            "vkShaderResourceResidency = "
                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                              (vkShaderResourceResidency
                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                  "vkShaderResourceMinLod = "
                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                    (vkShaderResourceMinLod
                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                        "vkSparseBinding = "
                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                          (vkSparseBinding
                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                              "vkSparseResidencyBuffer = "
                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                (vkSparseResidencyBuffer
                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                    "vkSparseResidencyImage2D = "
                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                      (vkSparseResidencyImage2D
                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                        ", "
                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                          "vkSparseResidencyImage3D = "
                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                          showsPrec
                                                                                                                                                                                                                                                                                                            d
                                                                                                                                                                                                                                                                                                            (vkSparseResidencyImage3D
                                                                                                                                                                                                                                                                                                               x)
                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                              ", "
                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                "vkSparseResidency2Samples = "
                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                showsPrec
                                                                                                                                                                                                                                                                                                                  d
                                                                                                                                                                                                                                                                                                                  (vkSparseResidency2Samples
                                                                                                                                                                                                                                                                                                                     x)
                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                    ", "
                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                      "vkSparseResidency4Samples = "
                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                      showsPrec
                                                                                                                                                                                                                                                                                                                        d
                                                                                                                                                                                                                                                                                                                        (vkSparseResidency4Samples
                                                                                                                                                                                                                                                                                                                           x)
                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                        showString
                                                                                                                                                                                                                                                                                                                          ", "
                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                            "vkSparseResidency8Samples = "
                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                            showsPrec
                                                                                                                                                                                                                                                                                                                              d
                                                                                                                                                                                                                                                                                                                              (vkSparseResidency8Samples
                                                                                                                                                                                                                                                                                                                                 x)
                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                              showString
                                                                                                                                                                                                                                                                                                                                ", "
                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                  "vkSparseResidency16Samples = "
                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                  showsPrec
                                                                                                                                                                                                                                                                                                                                    d
                                                                                                                                                                                                                                                                                                                                    (vkSparseResidency16Samples
                                                                                                                                                                                                                                                                                                                                       x)
                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                    showString
                                                                                                                                                                                                                                                                                                                                      ", "
                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                      showString
                                                                                                                                                                                                                                                                                                                                        "vkSparseResidencyAliased = "
                                                                                                                                                                                                                                                                                                                                        .
                                                                                                                                                                                                                                                                                                                                        showsPrec
                                                                                                                                                                                                                                                                                                                                          d
                                                                                                                                                                                                                                                                                                                                          (vkSparseResidencyAliased
                                                                                                                                                                                                                                                                                                                                             x)
                                                                                                                                                                                                                                                                                                                                          .
                                                                                                                                                                                                                                                                                                                                          showString
                                                                                                                                                                                                                                                                                                                                            ", "
                                                                                                                                                                                                                                                                                                                                            .
                                                                                                                                                                                                                                                                                                                                            showString
                                                                                                                                                                                                                                                                                                                                              "vkVariableMultisampleRate = "
                                                                                                                                                                                                                                                                                                                                              .
                                                                                                                                                                                                                                                                                                                                              showsPrec
                                                                                                                                                                                                                                                                                                                                                d
                                                                                                                                                                                                                                                                                                                                                (vkVariableMultisampleRate
                                                                                                                                                                                                                                                                                                                                                   x)
                                                                                                                                                                                                                                                                                                                                                .
                                                                                                                                                                                                                                                                                                                                                showString
                                                                                                                                                                                                                                                                                                                                                  ", "
                                                                                                                                                                                                                                                                                                                                                  .
                                                                                                                                                                                                                                                                                                                                                  showString
                                                                                                                                                                                                                                                                                                                                                    "vkInheritedQueries = "
                                                                                                                                                                                                                                                                                                                                                    .
                                                                                                                                                                                                                                                                                                                                                    showsPrec
                                                                                                                                                                                                                                                                                                                                                      d
                                                                                                                                                                                                                                                                                                                                                      (vkInheritedQueries
                                                                                                                                                                                                                                                                                                                                                         x)
                                                                                                                                                                                                                                                                                                                                                      .
                                                                                                                                                                                                                                                                                                                                                      showChar
                                                                                                                                                                                                                                                                                                                                                        '}'
