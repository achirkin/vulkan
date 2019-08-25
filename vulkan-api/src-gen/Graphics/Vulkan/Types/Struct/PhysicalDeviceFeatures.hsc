#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures
       (VkPhysicalDeviceFeatures) where
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes  (VkBool32)

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
type VkPhysicalDeviceFeatures = VkStruct VkPhysicalDeviceFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceFeatures where
    type StructRep VkPhysicalDeviceFeatures =
         'StructMeta "VkPhysicalDeviceFeatures" VkPhysicalDeviceFeatures -- ' closing tick for hsc2hs
           #{size VkPhysicalDeviceFeatures}
           #{alignment VkPhysicalDeviceFeatures}
           '[('FieldMeta "robustBufferAccess" VkBool32 'False  -- ' closing tick for hsc2hs
                                                              #{offset VkPhysicalDeviceFeatures, robustBufferAccess}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fullDrawIndexUint32" VkBool32 'False 
                                                               #{offset VkPhysicalDeviceFeatures, fullDrawIndexUint32}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageCubeArray" VkBool32 'False 
                                                          #{offset VkPhysicalDeviceFeatures, imageCubeArray}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "independentBlend" VkBool32 'False 
                                                            #{offset VkPhysicalDeviceFeatures, independentBlend}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "geometryShader" VkBool32 'False 
                                                          #{offset VkPhysicalDeviceFeatures, geometryShader}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "tessellationShader" VkBool32 'False 
                                                              #{offset VkPhysicalDeviceFeatures, tessellationShader}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampleRateShading" VkBool32 'False 
                                                             #{offset VkPhysicalDeviceFeatures, sampleRateShading}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dualSrcBlend" VkBool32 'False 
                                                        #{offset VkPhysicalDeviceFeatures, dualSrcBlend}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "logicOp" VkBool32 'False 
                                                   #{offset VkPhysicalDeviceFeatures, logicOp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "multiDrawIndirect" VkBool32 'False 
                                                             #{offset VkPhysicalDeviceFeatures, multiDrawIndirect}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "drawIndirectFirstInstance" VkBool32 'False 
                                                                     #{offset VkPhysicalDeviceFeatures, drawIndirectFirstInstance}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthClamp" VkBool32 'False 
                                                      #{offset VkPhysicalDeviceFeatures, depthClamp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthBiasClamp" VkBool32 'False 
                                                          #{offset VkPhysicalDeviceFeatures, depthBiasClamp}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fillModeNonSolid" VkBool32 'False 
                                                            #{offset VkPhysicalDeviceFeatures, fillModeNonSolid}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthBounds" VkBool32 'False 
                                                       #{offset VkPhysicalDeviceFeatures, depthBounds}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "wideLines" VkBool32 'False 
                                                     #{offset VkPhysicalDeviceFeatures, wideLines}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "largePoints" VkBool32 'False 
                                                       #{offset VkPhysicalDeviceFeatures, largePoints}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "alphaToOne" VkBool32 'False 
                                                      #{offset VkPhysicalDeviceFeatures, alphaToOne}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "multiViewport" VkBool32 'False 
                                                         #{offset VkPhysicalDeviceFeatures, multiViewport}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "samplerAnisotropy" VkBool32 'False 
                                                             #{offset VkPhysicalDeviceFeatures, samplerAnisotropy}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "textureCompressionETC2" VkBool32 'False 
                                                                  #{offset VkPhysicalDeviceFeatures, textureCompressionETC2}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "textureCompressionASTC_LDR" VkBool32 'False 
                                                                      #{offset VkPhysicalDeviceFeatures, textureCompressionASTC_LDR}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "textureCompressionBC" VkBool32 'False 
                                                                #{offset VkPhysicalDeviceFeatures, textureCompressionBC}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "occlusionQueryPrecise" VkBool32 'False 
                                                                 #{offset VkPhysicalDeviceFeatures, occlusionQueryPrecise}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineStatisticsQuery" VkBool32 'False 
                                                                   #{offset VkPhysicalDeviceFeatures, pipelineStatisticsQuery}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexPipelineStoresAndAtomics" VkBool32 'False
                #{offset VkPhysicalDeviceFeatures, vertexPipelineStoresAndAtomics}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fragmentStoresAndAtomics" VkBool32 'False 
                                                                    #{offset VkPhysicalDeviceFeatures, fragmentStoresAndAtomics}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderTessellationAndGeometryPointSize" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceFeatures, shaderTessellationAndGeometryPointSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderImageGatherExtended" VkBool32 'False 
                                                                     #{offset VkPhysicalDeviceFeatures, shaderImageGatherExtended}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageImageExtendedFormats" VkBool32 'False
                #{offset VkPhysicalDeviceFeatures, shaderStorageImageExtendedFormats}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageImageMultisample" VkBool32 'False
                #{offset VkPhysicalDeviceFeatures, shaderStorageImageMultisample}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageImageReadWithoutFormat" VkBool32 'False
                #{offset VkPhysicalDeviceFeatures, shaderStorageImageReadWithoutFormat}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageImageWriteWithoutFormat" VkBool32 'False
                #{offset VkPhysicalDeviceFeatures, shaderStorageImageWriteWithoutFormat}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderUniformBufferArrayDynamicIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceFeatures, shaderUniformBufferArrayDynamicIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSampledImageArrayDynamicIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceFeatures, shaderSampledImageArrayDynamicIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageBufferArrayDynamicIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceFeatures, shaderStorageBufferArrayDynamicIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageImageArrayDynamicIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceFeatures, shaderStorageImageArrayDynamicIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderClipDistance" VkBool32 'False 
                                                              #{offset VkPhysicalDeviceFeatures, shaderClipDistance}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderCullDistance" VkBool32 'False 
                                                              #{offset VkPhysicalDeviceFeatures, shaderCullDistance}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderFloat64" VkBool32 'False 
                                                         #{offset VkPhysicalDeviceFeatures, shaderFloat64}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderInt64" VkBool32 'False 
                                                       #{offset VkPhysicalDeviceFeatures, shaderInt64}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderInt16" VkBool32 'False 
                                                       #{offset VkPhysicalDeviceFeatures, shaderInt16}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderResourceResidency" VkBool32 'False 
                                                                   #{offset VkPhysicalDeviceFeatures, shaderResourceResidency}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderResourceMinLod" VkBool32 'False 
                                                                #{offset VkPhysicalDeviceFeatures, shaderResourceMinLod}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sparseBinding" VkBool32 'False 
                                                         #{offset VkPhysicalDeviceFeatures, sparseBinding}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sparseResidencyBuffer" VkBool32 'False 
                                                                 #{offset VkPhysicalDeviceFeatures, sparseResidencyBuffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sparseResidencyImage2D" VkBool32 'False 
                                                                  #{offset VkPhysicalDeviceFeatures, sparseResidencyImage2D}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sparseResidencyImage3D" VkBool32 'False 
                                                                  #{offset VkPhysicalDeviceFeatures, sparseResidencyImage3D}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sparseResidency2Samples" VkBool32 'False 
                                                                   #{offset VkPhysicalDeviceFeatures, sparseResidency2Samples}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sparseResidency4Samples" VkBool32 'False 
                                                                   #{offset VkPhysicalDeviceFeatures, sparseResidency4Samples}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sparseResidency8Samples" VkBool32 'False 
                                                                   #{offset VkPhysicalDeviceFeatures, sparseResidency8Samples}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sparseResidency16Samples" VkBool32 'False 
                                                                    #{offset VkPhysicalDeviceFeatures, sparseResidency16Samples}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sparseResidencyAliased" VkBool32 'False 
                                                                  #{offset VkPhysicalDeviceFeatures, sparseResidencyAliased}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "variableMultisampleRate" VkBool32 'False 
                                                                   #{offset VkPhysicalDeviceFeatures, variableMultisampleRate}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "inheritedQueries" VkBool32 'False 
                                                            #{offset VkPhysicalDeviceFeatures, inheritedQueries}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs
