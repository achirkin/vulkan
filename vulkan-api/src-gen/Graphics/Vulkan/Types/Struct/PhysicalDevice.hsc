#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PhysicalDevice
       (VkPhysicalDevice16BitStorageFeatures,
        VkPhysicalDevice16BitStorageFeaturesKHR,
        VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT,
        VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT,
        VkPhysicalDeviceConservativeRasterizationPropertiesEXT,
        VkPhysicalDeviceDescriptorIndexingFeaturesEXT,
        VkPhysicalDeviceDescriptorIndexingPropertiesEXT,
        VkPhysicalDeviceDiscardRectanglePropertiesEXT,
        VkPhysicalDeviceExternalBufferInfo,
        VkPhysicalDeviceExternalBufferInfoKHR,
        VkPhysicalDeviceExternalFenceInfo,
        VkPhysicalDeviceExternalFenceInfoKHR,
        VkPhysicalDeviceExternalImageFormatInfo,
        VkPhysicalDeviceExternalImageFormatInfoKHR,
        VkPhysicalDeviceExternalMemoryHostPropertiesEXT,
        VkPhysicalDeviceExternalSemaphoreInfo,
        VkPhysicalDeviceExternalSemaphoreInfoKHR,
        VkPhysicalDeviceFeatures2, VkPhysicalDeviceFeatures2KHR,
        VkPhysicalDeviceGroupProperties,
        VkPhysicalDeviceGroupPropertiesKHR, VkPhysicalDeviceIDProperties,
        VkPhysicalDeviceIDPropertiesKHR, VkPhysicalDeviceImageFormatInfo2,
        VkPhysicalDeviceImageFormatInfo2KHR, VkPhysicalDeviceLimits,
        VkPhysicalDeviceMaintenance3Properties,
        VkPhysicalDeviceMaintenance3PropertiesKHR,
        VkPhysicalDeviceMemoryProperties,
        VkPhysicalDeviceMemoryProperties2,
        VkPhysicalDeviceMemoryProperties2KHR,
        VkPhysicalDeviceMultiviewFeatures,
        VkPhysicalDeviceMultiviewFeaturesKHR,
        VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX,
        VkPhysicalDeviceMultiviewProperties,
        VkPhysicalDeviceMultiviewPropertiesKHR,
        VkPhysicalDevicePointClippingProperties,
        VkPhysicalDevicePointClippingPropertiesKHR,
        VkPhysicalDeviceProperties, VkPhysicalDeviceProperties2,
        VkPhysicalDeviceProperties2KHR,
        VkPhysicalDeviceProtectedMemoryFeatures,
        VkPhysicalDeviceProtectedMemoryProperties,
        VkPhysicalDevicePushDescriptorPropertiesKHR,
        VkPhysicalDeviceSampleLocationsPropertiesEXT,
        VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT,
        VkPhysicalDeviceSamplerYcbcrConversionFeatures,
        VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR,
        VkPhysicalDeviceShaderCorePropertiesAMD,
        VkPhysicalDeviceShaderDrawParameterFeatures,
        VkPhysicalDeviceSparseImageFormatInfo2,
        VkPhysicalDeviceSparseImageFormatInfo2KHR,
        VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceSubgroupProperties,
        VkPhysicalDeviceSurfaceInfo2KHR,
        VkPhysicalDeviceVariablePointerFeatures,
        VkPhysicalDeviceVariablePointerFeaturesKHR,
        VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT)
       where
import           Graphics.Vulkan.Constants                           (VK_LUID_SIZE,
                                                                      VK_MAX_DEVICE_GROUP_SIZE,
                                                                      VK_MAX_MEMORY_HEAPS,
                                                                      VK_MAX_MEMORY_TYPES,
                                                                      VK_MAX_PHYSICAL_DEVICE_NAME_SIZE,
                                                                      VK_UUID_SIZE)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                     (VkBool32, VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.Buffer                   (VkBufferCreateFlags,
                                                                      VkBufferUsageFlags)
import           Graphics.Vulkan.Types.Enum.External                 (VkExternalFenceHandleTypeFlagBits,
                                                                      VkExternalMemoryHandleTypeFlagBits,
                                                                      VkExternalSemaphoreHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.Format                   (VkFormat)
import           Graphics.Vulkan.Types.Enum.Image                    (VkImageCreateFlags,
                                                                      VkImageTiling,
                                                                      VkImageType,
                                                                      VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.PhysicalDeviceType       (VkPhysicalDeviceType)
import           Graphics.Vulkan.Types.Enum.PointClippingBehavior    (VkPointClippingBehavior)
import           Graphics.Vulkan.Types.Enum.SampleCountFlags         (VkSampleCountFlagBits,
                                                                      VkSampleCountFlags)
import           Graphics.Vulkan.Types.Enum.Shader                   (VkShaderStageFlags)
import           Graphics.Vulkan.Types.Enum.StructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Enum.SubgroupFeatureFlags     (VkSubgroupFeatureFlags)
import           Graphics.Vulkan.Types.Handles                       (VkPhysicalDevice,
                                                                      VkSurfaceKHR)
import           Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo)
import           Graphics.Vulkan.Types.Struct.Extent                 (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.Memory                 (VkMemoryHeap,
                                                                      VkMemoryType)
import           Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)

-- | > typedef struct VkPhysicalDevice16BitStorageFeatures {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     VkBool32                         storageBuffer16BitAccess;
--   >     VkBool32                         uniformAndStorageBuffer16BitAccess;
--   >     VkBool32                         storagePushConstant16;
--   >     VkBool32                         storageInputOutput16;
--   > } VkPhysicalDevice16BitStorageFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDevice16BitStorageFeatures VkPhysicalDevice16BitStorageFeatures registry at www.khronos.org>
type VkPhysicalDevice16BitStorageFeatures =
     VkStruct VkPhysicalDevice16BitStorageFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDevice16BitStorageFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDevice16BitStorageFeatures where
    type StructRep VkPhysicalDevice16BitStorageFeatures =
         'StructMeta "VkPhysicalDevice16BitStorageFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDevice16BitStorageFeatures
           #{size VkPhysicalDevice16BitStorageFeatures}
           #{alignment VkPhysicalDevice16BitStorageFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDevice16BitStorageFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDevice16BitStorageFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "storageBuffer16BitAccess" VkBool32 'False 
                                                                    #{offset VkPhysicalDevice16BitStorageFeatures, storageBuffer16BitAccess}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "uniformAndStorageBuffer16BitAccess" VkBool32 'False
                #{offset VkPhysicalDevice16BitStorageFeatures, uniformAndStorageBuffer16BitAccess}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "storagePushConstant16" VkBool32 'False 
                                                                 #{offset VkPhysicalDevice16BitStorageFeatures, storagePushConstant16}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "storageInputOutput16" VkBool32 'False 
                                                                #{offset VkPhysicalDevice16BitStorageFeatures, storageInputOutput16}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDevice16BitStorageFeatures`
type VkPhysicalDevice16BitStorageFeaturesKHR =
     VkPhysicalDevice16BitStorageFeatures

-- | > typedef struct VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         advancedBlendCoherentOperations;
--   > } VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT =
     VkStruct VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
         where
    type StructRep VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT =
         'StructMeta "VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
           #{size VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT}
           #{alignment VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "advancedBlendCoherentOperations" VkBool32 'False
                #{offset VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT, advancedBlendCoherentOperations}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         advancedBlendMaxColorAttachments;
--   >     VkBool32                         advancedBlendIndependentBlend;
--   >     VkBool32                         advancedBlendNonPremultipliedSrcColor;
--   >     VkBool32                         advancedBlendNonPremultipliedDstColor;
--   >     VkBool32                         advancedBlendCorrelatedOverlap;
--   >     VkBool32                         advancedBlendAllOperations;
--   > } VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT =
     VkStruct VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
    type StructRep VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         =
         'StructMeta "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
           #{size VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT}
           #{alignment VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "advancedBlendMaxColorAttachments" Word32 'False
                #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendMaxColorAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "advancedBlendIndependentBlend" VkBool32 'False
                #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendIndependentBlend}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "advancedBlendNonPremultipliedSrcColor" VkBool32 'False
                #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedSrcColor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "advancedBlendNonPremultipliedDstColor" VkBool32 'False
                #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedDstColor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "advancedBlendCorrelatedOverlap" VkBool32 'False
                #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendCorrelatedOverlap}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "advancedBlendAllOperations" VkBool32 'False 
                                                                      #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendAllOperations}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceConservativeRasterizationPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     float                  primitiveOverestimationSize;
--   >     float                  maxExtraPrimitiveOverestimationSize;
--   >     float                  extraPrimitiveOverestimationSizeGranularity;
--   >     VkBool32               primitiveUnderestimation;
--   >     VkBool32               conservativePointAndLineRasterization;
--   >     VkBool32               degenerateTrianglesRasterized;
--   >     VkBool32               degenerateLinesRasterized;
--   >     VkBool32               fullyCoveredFragmentShaderInputVariable;
--   >     VkBool32               conservativeRasterizationPostDepthCoverage;
--   > } VkPhysicalDeviceConservativeRasterizationPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceConservativeRasterizationPropertiesEXT VkPhysicalDeviceConservativeRasterizationPropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceConservativeRasterizationPropertiesEXT =
     VkStruct VkPhysicalDeviceConservativeRasterizationPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceConservativeRasterizationPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         where
    type StructRep
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
         =
         'StructMeta -- ' closing tick for hsc2hs
           "VkPhysicalDeviceConservativeRasterizationPropertiesEXT"
           VkPhysicalDeviceConservativeRasterizationPropertiesEXT
           #{size VkPhysicalDeviceConservativeRasterizationPropertiesEXT}
           #{alignment VkPhysicalDeviceConservativeRasterizationPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "primitiveOverestimationSize" -- ' closing tick for hsc2hs
                (#{type float})
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveOverestimationSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxExtraPrimitiveOverestimationSize" -- ' closing tick for hsc2hs
                (#{type float})
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, maxExtraPrimitiveOverestimationSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "extraPrimitiveOverestimationSizeGranularity" -- ' closing tick for hsc2hs
                (#{type float})
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, extraPrimitiveOverestimationSizeGranularity}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "primitiveUnderestimation" VkBool32 'False 
                                                                    #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, primitiveUnderestimation}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "conservativePointAndLineRasterization" VkBool32 'False
                #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativePointAndLineRasterization}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "degenerateTrianglesRasterized" VkBool32 'False
                #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateTrianglesRasterized}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "degenerateLinesRasterized" VkBool32 'False 
                                                                     #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, degenerateLinesRasterized}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fullyCoveredFragmentShaderInputVariable" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, fullyCoveredFragmentShaderInputVariable}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "conservativeRasterizationPostDepthCoverage" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceConservativeRasterizationPropertiesEXT, conservativeRasterizationPostDepthCoverage}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceDescriptorIndexingFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32               shaderInputAttachmentArrayDynamicIndexing;
--   >     VkBool32               shaderUniformTexelBufferArrayDynamicIndexing;
--   >     VkBool32               shaderStorageTexelBufferArrayDynamicIndexing;
--   >     VkBool32               shaderUniformBufferArrayNonUniformIndexing;
--   >     VkBool32               shaderSampledImageArrayNonUniformIndexing;
--   >     VkBool32               shaderStorageBufferArrayNonUniformIndexing;
--   >     VkBool32               shaderStorageImageArrayNonUniformIndexing;
--   >     VkBool32               shaderInputAttachmentArrayNonUniformIndexing;
--   >     VkBool32               shaderUniformTexelBufferArrayNonUniformIndexing;
--   >     VkBool32               shaderStorageTexelBufferArrayNonUniformIndexing;
--   >     VkBool32               descriptorBindingUniformBufferUpdateAfterBind;
--   >     VkBool32               descriptorBindingSampledImageUpdateAfterBind;
--   >     VkBool32               descriptorBindingStorageImageUpdateAfterBind;
--   >     VkBool32               descriptorBindingStorageBufferUpdateAfterBind;
--   >     VkBool32               descriptorBindingUniformTexelBufferUpdateAfterBind;
--   >     VkBool32               descriptorBindingStorageTexelBufferUpdateAfterBind;
--   >     VkBool32               descriptorBindingUpdateUnusedWhilePending;
--   >     VkBool32               descriptorBindingPartiallyBound;
--   >     VkBool32               descriptorBindingVariableDescriptorCount;
--   >     VkBool32               runtimeDescriptorArray;
--   > } VkPhysicalDeviceDescriptorIndexingFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceDescriptorIndexingFeaturesEXT VkPhysicalDeviceDescriptorIndexingFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceDescriptorIndexingFeaturesEXT =
     VkStruct VkPhysicalDeviceDescriptorIndexingFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceDescriptorIndexingFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
         where
    type StructRep VkPhysicalDeviceDescriptorIndexingFeaturesEXT =
         'StructMeta "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceDescriptorIndexingFeaturesEXT
           #{size VkPhysicalDeviceDescriptorIndexingFeaturesEXT}
           #{alignment VkPhysicalDeviceDescriptorIndexingFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderInputAttachmentArrayDynamicIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderInputAttachmentArrayDynamicIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderUniformTexelBufferArrayDynamicIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformTexelBufferArrayDynamicIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageTexelBufferArrayDynamicIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageTexelBufferArrayDynamicIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderUniformBufferArrayNonUniformIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformBufferArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSampledImageArrayNonUniformIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderSampledImageArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageBufferArrayNonUniformIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageBufferArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageImageArrayNonUniformIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageImageArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderInputAttachmentArrayNonUniformIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderInputAttachmentArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderUniformTexelBufferArrayNonUniformIndexing" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderUniformTexelBufferArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageTexelBufferArrayNonUniformIndexing" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, shaderStorageTexelBufferArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingUniformBufferUpdateAfterBind" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUniformBufferUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingSampledImageUpdateAfterBind" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingSampledImageUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingStorageImageUpdateAfterBind" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageImageUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingStorageBufferUpdateAfterBind" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageBufferUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingUniformTexelBufferUpdateAfterBind" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUniformTexelBufferUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingStorageTexelBufferUpdateAfterBind" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingStorageTexelBufferUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingUpdateUnusedWhilePending" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingUpdateUnusedWhilePending}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingPartiallyBound" VkBool32 'False
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingPartiallyBound}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingVariableDescriptorCount" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, descriptorBindingVariableDescriptorCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "runtimeDescriptorArray" VkBool32 'False 
                                                                  #{offset VkPhysicalDeviceDescriptorIndexingFeaturesEXT, runtimeDescriptorArray}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceDescriptorIndexingPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t               maxUpdateAfterBindDescriptorsInAllPools;
--   >     VkBool32               shaderUniformBufferArrayNonUniformIndexingNative;
--   >     VkBool32               shaderSampledImageArrayNonUniformIndexingNative;
--   >     VkBool32               shaderStorageBufferArrayNonUniformIndexingNative;
--   >     VkBool32               shaderStorageImageArrayNonUniformIndexingNative;
--   >     VkBool32               shaderInputAttachmentArrayNonUniformIndexingNative;
--   >     VkBool32               robustBufferAccessUpdateAfterBind;
--   >     VkBool32               quadDivergentImplicitLod;
--   >     uint32_t               maxPerStageDescriptorUpdateAfterBindSamplers;
--   >     uint32_t               maxPerStageDescriptorUpdateAfterBindUniformBuffers;
--   >     uint32_t               maxPerStageDescriptorUpdateAfterBindStorageBuffers;
--   >     uint32_t               maxPerStageDescriptorUpdateAfterBindSampledImages;
--   >     uint32_t               maxPerStageDescriptorUpdateAfterBindStorageImages;
--   >     uint32_t               maxPerStageDescriptorUpdateAfterBindInputAttachments;
--   >     uint32_t               maxPerStageUpdateAfterBindResources;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindSamplers;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindUniformBuffers;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindUniformBuffersDynamic;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindStorageBuffers;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindStorageBuffersDynamic;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindSampledImages;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindStorageImages;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindInputAttachments;
--   > } VkPhysicalDeviceDescriptorIndexingPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceDescriptorIndexingPropertiesEXT VkPhysicalDeviceDescriptorIndexingPropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceDescriptorIndexingPropertiesEXT =
     VkStruct VkPhysicalDeviceDescriptorIndexingPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceDescriptorIndexingPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
         where
    type StructRep VkPhysicalDeviceDescriptorIndexingPropertiesEXT =
         'StructMeta "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceDescriptorIndexingPropertiesEXT
           #{size VkPhysicalDeviceDescriptorIndexingPropertiesEXT}
           #{alignment VkPhysicalDeviceDescriptorIndexingPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxUpdateAfterBindDescriptorsInAllPools" Word32 'False
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxUpdateAfterBindDescriptorsInAllPools}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderUniformBufferArrayNonUniformIndexingNative" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderUniformBufferArrayNonUniformIndexingNative}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSampledImageArrayNonUniformIndexingNative" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderSampledImageArrayNonUniformIndexingNative}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageBufferArrayNonUniformIndexingNative" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderStorageBufferArrayNonUniformIndexingNative}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageImageArrayNonUniformIndexingNative" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderStorageImageArrayNonUniformIndexingNative}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderInputAttachmentArrayNonUniformIndexingNative" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, shaderInputAttachmentArrayNonUniformIndexingNative}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "robustBufferAccessUpdateAfterBind" VkBool32 'False
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, robustBufferAccessUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "quadDivergentImplicitLod" VkBool32 'False 
                                                                    #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, quadDivergentImplicitLod}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindSamplers" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindSamplers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindUniformBuffers" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindUniformBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindStorageBuffers" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindStorageBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindSampledImages" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindSampledImages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindStorageImages" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindStorageImages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindInputAttachments" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageDescriptorUpdateAfterBindInputAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageUpdateAfterBindResources" Word32 'False
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxPerStageUpdateAfterBindResources}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindSamplers" Word32 'False
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindSamplers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindUniformBuffers" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindUniformBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindUniformBuffersDynamic}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindStorageBuffers" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageBuffersDynamic}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindSampledImages" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindSampledImages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindStorageImages" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindStorageImages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindInputAttachments" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingPropertiesEXT, maxDescriptorSetUpdateAfterBindInputAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceDiscardRectanglePropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     uint32_t               maxDiscardRectangles;
--   > } VkPhysicalDeviceDiscardRectanglePropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceDiscardRectanglePropertiesEXT VkPhysicalDeviceDiscardRectanglePropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceDiscardRectanglePropertiesEXT =
     VkStruct VkPhysicalDeviceDiscardRectanglePropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceDiscardRectanglePropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
         where
    type StructRep VkPhysicalDeviceDiscardRectanglePropertiesEXT =
         'StructMeta "VkPhysicalDeviceDiscardRectanglePropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceDiscardRectanglePropertiesEXT
           #{size VkPhysicalDeviceDiscardRectanglePropertiesEXT}
           #{alignment VkPhysicalDeviceDiscardRectanglePropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDiscardRectangles" Word32 'False 
                                                              #{offset VkPhysicalDeviceDiscardRectanglePropertiesEXT, maxDiscardRectangles}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceExternalBufferInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBufferCreateFlags flags;
--   >     VkBufferUsageFlags               usage;
--   >     VkExternalMemoryHandleTypeFlagBits handleType;
--   > } VkPhysicalDeviceExternalBufferInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceExternalBufferInfo VkPhysicalDeviceExternalBufferInfo registry at www.khronos.org>
type VkPhysicalDeviceExternalBufferInfo =
     VkStruct VkPhysicalDeviceExternalBufferInfo' -- ' closing tick for hsc2hs

data VkPhysicalDeviceExternalBufferInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceExternalBufferInfo where
    type StructRep VkPhysicalDeviceExternalBufferInfo =
         'StructMeta "VkPhysicalDeviceExternalBufferInfo" -- ' closing tick for hsc2hs
           VkPhysicalDeviceExternalBufferInfo
           #{size VkPhysicalDeviceExternalBufferInfo}
           #{alignment VkPhysicalDeviceExternalBufferInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceExternalBufferInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceExternalBufferInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkBufferCreateFlags 'True 
                                                           #{offset VkPhysicalDeviceExternalBufferInfo, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "usage" VkBufferUsageFlags 'False 
                                                           #{offset VkPhysicalDeviceExternalBufferInfo, usage}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalMemoryHandleTypeFlagBits 'False
                #{offset VkPhysicalDeviceExternalBufferInfo, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceExternalBufferInfo`
type VkPhysicalDeviceExternalBufferInfoKHR =
     VkPhysicalDeviceExternalBufferInfo

-- | > typedef struct VkPhysicalDeviceExternalFenceInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalFenceHandleTypeFlagBits handleType;
--   > } VkPhysicalDeviceExternalFenceInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceExternalFenceInfo VkPhysicalDeviceExternalFenceInfo registry at www.khronos.org>
type VkPhysicalDeviceExternalFenceInfo =
     VkStruct VkPhysicalDeviceExternalFenceInfo' -- ' closing tick for hsc2hs

data VkPhysicalDeviceExternalFenceInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceExternalFenceInfo where
    type StructRep VkPhysicalDeviceExternalFenceInfo =
         'StructMeta "VkPhysicalDeviceExternalFenceInfo" -- ' closing tick for hsc2hs
           VkPhysicalDeviceExternalFenceInfo
           #{size VkPhysicalDeviceExternalFenceInfo}
           #{alignment VkPhysicalDeviceExternalFenceInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceExternalFenceInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceExternalFenceInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalFenceHandleTypeFlagBits 'False
                #{offset VkPhysicalDeviceExternalFenceInfo, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceExternalFenceInfo`
type VkPhysicalDeviceExternalFenceInfoKHR =
     VkPhysicalDeviceExternalFenceInfo

-- | > typedef struct VkPhysicalDeviceExternalImageFormatInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagBits handleType;
--   > } VkPhysicalDeviceExternalImageFormatInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceExternalImageFormatInfo VkPhysicalDeviceExternalImageFormatInfo registry at www.khronos.org>
type VkPhysicalDeviceExternalImageFormatInfo =
     VkStruct VkPhysicalDeviceExternalImageFormatInfo' -- ' closing tick for hsc2hs

data VkPhysicalDeviceExternalImageFormatInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceExternalImageFormatInfo
         where
    type StructRep VkPhysicalDeviceExternalImageFormatInfo =
         'StructMeta "VkPhysicalDeviceExternalImageFormatInfo" -- ' closing tick for hsc2hs
           VkPhysicalDeviceExternalImageFormatInfo
           #{size VkPhysicalDeviceExternalImageFormatInfo}
           #{alignment VkPhysicalDeviceExternalImageFormatInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceExternalImageFormatInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceExternalImageFormatInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalMemoryHandleTypeFlagBits 'True
                #{offset VkPhysicalDeviceExternalImageFormatInfo, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceImageFormatInfo2] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceExternalImageFormatInfo`
type VkPhysicalDeviceExternalImageFormatInfoKHR =
     VkPhysicalDeviceExternalImageFormatInfo

-- | > typedef struct VkPhysicalDeviceExternalMemoryHostPropertiesEXT {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkDeviceSize minImportedHostPointerAlignment;
--   > } VkPhysicalDeviceExternalMemoryHostPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceExternalMemoryHostPropertiesEXT VkPhysicalDeviceExternalMemoryHostPropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceExternalMemoryHostPropertiesEXT =
     VkStruct VkPhysicalDeviceExternalMemoryHostPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceExternalMemoryHostPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
    type StructRep VkPhysicalDeviceExternalMemoryHostPropertiesEXT =
         'StructMeta "VkPhysicalDeviceExternalMemoryHostPropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
           #{size VkPhysicalDeviceExternalMemoryHostPropertiesEXT}
           #{alignment VkPhysicalDeviceExternalMemoryHostPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minImportedHostPointerAlignment" VkDeviceSize 'False
                #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, minImportedHostPointerAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceExternalSemaphoreInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalSemaphoreHandleTypeFlagBits handleType;
--   > } VkPhysicalDeviceExternalSemaphoreInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceExternalSemaphoreInfo VkPhysicalDeviceExternalSemaphoreInfo registry at www.khronos.org>
type VkPhysicalDeviceExternalSemaphoreInfo =
     VkStruct VkPhysicalDeviceExternalSemaphoreInfo' -- ' closing tick for hsc2hs

data VkPhysicalDeviceExternalSemaphoreInfo' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceExternalSemaphoreInfo where
    type StructRep VkPhysicalDeviceExternalSemaphoreInfo =
         'StructMeta "VkPhysicalDeviceExternalSemaphoreInfo" -- ' closing tick for hsc2hs
           VkPhysicalDeviceExternalSemaphoreInfo
           #{size VkPhysicalDeviceExternalSemaphoreInfo}
           #{alignment VkPhysicalDeviceExternalSemaphoreInfo}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceExternalSemaphoreInfo, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceExternalSemaphoreInfo, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "handleType" VkExternalSemaphoreHandleTypeFlagBits -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceExternalSemaphoreInfo, handleType}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceExternalSemaphoreInfo`
type VkPhysicalDeviceExternalSemaphoreInfoKHR =
     VkPhysicalDeviceExternalSemaphoreInfo

-- | > typedef struct VkPhysicalDeviceFeatures2 {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPhysicalDeviceFeatures         features;
--   > } VkPhysicalDeviceFeatures2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceFeatures2 VkPhysicalDeviceFeatures2 registry at www.khronos.org>
type VkPhysicalDeviceFeatures2 =
     VkStruct VkPhysicalDeviceFeatures2' -- ' closing tick for hsc2hs

data VkPhysicalDeviceFeatures2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceFeatures2 where
    type StructRep VkPhysicalDeviceFeatures2 =
         'StructMeta "VkPhysicalDeviceFeatures2" VkPhysicalDeviceFeatures2 -- ' closing tick for hsc2hs
           #{size VkPhysicalDeviceFeatures2}
           #{alignment VkPhysicalDeviceFeatures2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceFeatures2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceFeatures2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "features" VkPhysicalDeviceFeatures 'False 
                                                                    #{offset VkPhysicalDeviceFeatures2, features}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceFeatures2`
type VkPhysicalDeviceFeatures2KHR = VkPhysicalDeviceFeatures2

-- | > typedef struct VkPhysicalDeviceGroupProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         physicalDeviceCount;
--   >     VkPhysicalDevice                 physicalDevices[VK_MAX_DEVICE_GROUP_SIZE];
--   >     VkBool32                         subsetAllocation;
--   > } VkPhysicalDeviceGroupProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceGroupProperties VkPhysicalDeviceGroupProperties registry at www.khronos.org>
type VkPhysicalDeviceGroupProperties =
     VkStruct VkPhysicalDeviceGroupProperties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceGroupProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceGroupProperties where
    type StructRep VkPhysicalDeviceGroupProperties =
         'StructMeta "VkPhysicalDeviceGroupProperties" -- ' closing tick for hsc2hs
           VkPhysicalDeviceGroupProperties
           #{size VkPhysicalDeviceGroupProperties}
           #{alignment VkPhysicalDeviceGroupProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceGroupProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceGroupProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "physicalDeviceCount" Word32 'False 
                                                             #{offset VkPhysicalDeviceGroupProperties, physicalDeviceCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "physicalDevices" VkPhysicalDevice 'False 
                                                                   #{offset VkPhysicalDeviceGroupProperties, physicalDevices}
                VK_MAX_DEVICE_GROUP_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subsetAllocation" VkBool32 'False 
                                                            #{offset VkPhysicalDeviceGroupProperties, subsetAllocation}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceGroupProperties`
type VkPhysicalDeviceGroupPropertiesKHR =
     VkPhysicalDeviceGroupProperties

-- | > typedef struct VkPhysicalDeviceIDProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint8_t                          deviceUUID[VK_UUID_SIZE];
--   >     uint8_t                          driverUUID[VK_UUID_SIZE];
--   >     uint8_t                          deviceLUID[VK_LUID_SIZE];
--   >     uint32_t                         deviceNodeMask;
--   >     VkBool32                         deviceLUIDValid;
--   > } VkPhysicalDeviceIDProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceIDProperties VkPhysicalDeviceIDProperties registry at www.khronos.org>
type VkPhysicalDeviceIDProperties =
     VkStruct VkPhysicalDeviceIDProperties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceIDProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceIDProperties where
    type StructRep VkPhysicalDeviceIDProperties =
         'StructMeta "VkPhysicalDeviceIDProperties" -- ' closing tick for hsc2hs
           VkPhysicalDeviceIDProperties
           #{size VkPhysicalDeviceIDProperties}
           #{alignment VkPhysicalDeviceIDProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceIDProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceIDProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceUUID" Word8 'False 
                                                   #{offset VkPhysicalDeviceIDProperties, deviceUUID}
                VK_UUID_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "driverUUID" Word8 'False 
                                                   #{offset VkPhysicalDeviceIDProperties, driverUUID}
                VK_UUID_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceLUID" Word8 'False 
                                                   #{offset VkPhysicalDeviceIDProperties, deviceLUID}
                VK_LUID_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceNodeMask" Word32 'False 
                                                        #{offset VkPhysicalDeviceIDProperties, deviceNodeMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceLUIDValid" VkBool32 'False 
                                                           #{offset VkPhysicalDeviceIDProperties, deviceLUIDValid}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceIDProperties`
type VkPhysicalDeviceIDPropertiesKHR = VkPhysicalDeviceIDProperties

-- | > typedef struct VkPhysicalDeviceImageFormatInfo2 {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkFormat                         format;
--   >     VkImageType                      type;
--   >     VkImageTiling                    tiling;
--   >     VkImageUsageFlags                usage;
--   >     VkImageCreateFlags flags;
--   > } VkPhysicalDeviceImageFormatInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceImageFormatInfo2 VkPhysicalDeviceImageFormatInfo2 registry at www.khronos.org>
type VkPhysicalDeviceImageFormatInfo2 =
     VkStruct VkPhysicalDeviceImageFormatInfo2' -- ' closing tick for hsc2hs

data VkPhysicalDeviceImageFormatInfo2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceImageFormatInfo2 where
    type StructRep VkPhysicalDeviceImageFormatInfo2 =
         'StructMeta "VkPhysicalDeviceImageFormatInfo2" -- ' closing tick for hsc2hs
           VkPhysicalDeviceImageFormatInfo2
           #{size VkPhysicalDeviceImageFormatInfo2}
           #{alignment VkPhysicalDeviceImageFormatInfo2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceImageFormatInfo2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceImageFormatInfo2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "format" VkFormat 'False 
                                                  #{offset VkPhysicalDeviceImageFormatInfo2, format}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "type" VkImageType 'False 
                                                   #{offset VkPhysicalDeviceImageFormatInfo2, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "tiling" VkImageTiling 'False 
                                                       #{offset VkPhysicalDeviceImageFormatInfo2, tiling}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "usage" VkImageUsageFlags 'False 
                                                          #{offset VkPhysicalDeviceImageFormatInfo2, usage}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "flags" VkImageCreateFlags 'True 
                                                          #{offset VkPhysicalDeviceImageFormatInfo2, flags}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceImageFormatInfo2`
type VkPhysicalDeviceImageFormatInfo2KHR =
     VkPhysicalDeviceImageFormatInfo2

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceLimits VkPhysicalDeviceLimits registry at www.khronos.org>
type VkPhysicalDeviceLimits = VkStruct VkPhysicalDeviceLimits' -- ' closing tick for hsc2hs

data VkPhysicalDeviceLimits' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceLimits where
    type StructRep VkPhysicalDeviceLimits =
         'StructMeta "VkPhysicalDeviceLimits" VkPhysicalDeviceLimits -- ' closing tick for hsc2hs
           #{size VkPhysicalDeviceLimits}
           #{alignment VkPhysicalDeviceLimits}
           '[('FieldMeta "maxImageDimension1D" Word32 'False  -- ' closing tick for hsc2hs
                                                             #{offset VkPhysicalDeviceLimits, maxImageDimension1D}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxImageDimension2D" Word32 'False 
                                                             #{offset VkPhysicalDeviceLimits, maxImageDimension2D}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxImageDimension3D" Word32 'False 
                                                             #{offset VkPhysicalDeviceLimits, maxImageDimension3D}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxImageDimensionCube" Word32 'False 
                                                               #{offset VkPhysicalDeviceLimits, maxImageDimensionCube}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxImageArrayLayers" Word32 'False 
                                                             #{offset VkPhysicalDeviceLimits, maxImageArrayLayers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTexelBufferElements" Word32 'False 
                                                                #{offset VkPhysicalDeviceLimits, maxTexelBufferElements}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxUniformBufferRange" Word32 'False 
                                                               #{offset VkPhysicalDeviceLimits, maxUniformBufferRange}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxStorageBufferRange" Word32 'False 
                                                               #{offset VkPhysicalDeviceLimits, maxStorageBufferRange}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPushConstantsSize" Word32 'False 
                                                              #{offset VkPhysicalDeviceLimits, maxPushConstantsSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxMemoryAllocationCount" Word32 'False 
                                                                  #{offset VkPhysicalDeviceLimits, maxMemoryAllocationCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxSamplerAllocationCount" Word32 'False 
                                                                   #{offset VkPhysicalDeviceLimits, maxSamplerAllocationCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bufferImageGranularity" VkDeviceSize 'False 
                                                                      #{offset VkPhysicalDeviceLimits, bufferImageGranularity}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sparseAddressSpaceSize" VkDeviceSize 'False 
                                                                      #{offset VkPhysicalDeviceLimits, sparseAddressSpaceSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxBoundDescriptorSets" Word32 'False 
                                                                #{offset VkPhysicalDeviceLimits, maxBoundDescriptorSets}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorSamplers" Word32 'False 
                                                                       #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSamplers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUniformBuffers" Word32 'False
                #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorUniformBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorStorageBuffers" Word32 'False
                #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorSampledImages" Word32 'False
                #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorSampledImages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorStorageImages" Word32 'False
                #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorStorageImages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorInputAttachments" Word32 'False
                #{offset VkPhysicalDeviceLimits, maxPerStageDescriptorInputAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageResources" Word32 'False 
                                                              #{offset VkPhysicalDeviceLimits, maxPerStageResources}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetSamplers" Word32 'False 
                                                                  #{offset VkPhysicalDeviceLimits, maxDescriptorSetSamplers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUniformBuffers" Word32 'False
                #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUniformBuffersDynamic" Word32 'False
                #{offset VkPhysicalDeviceLimits, maxDescriptorSetUniformBuffersDynamic}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetStorageBuffers" Word32 'False
                #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetStorageBuffersDynamic" Word32 'False
                #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageBuffersDynamic}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetSampledImages" Word32 'False 
                                                                       #{offset VkPhysicalDeviceLimits, maxDescriptorSetSampledImages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetStorageImages" Word32 'False 
                                                                       #{offset VkPhysicalDeviceLimits, maxDescriptorSetStorageImages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetInputAttachments" Word32 'False
                #{offset VkPhysicalDeviceLimits, maxDescriptorSetInputAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxVertexInputAttributes" Word32 'False 
                                                                  #{offset VkPhysicalDeviceLimits, maxVertexInputAttributes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxVertexInputBindings" Word32 'False 
                                                                #{offset VkPhysicalDeviceLimits, maxVertexInputBindings}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxVertexInputAttributeOffset" Word32 'False 
                                                                       #{offset VkPhysicalDeviceLimits, maxVertexInputAttributeOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxVertexInputBindingStride" Word32 'False 
                                                                     #{offset VkPhysicalDeviceLimits, maxVertexInputBindingStride}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxVertexOutputComponents" Word32 'False 
                                                                   #{offset VkPhysicalDeviceLimits, maxVertexOutputComponents}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTessellationGenerationLevel" Word32 'False
                #{offset VkPhysicalDeviceLimits, maxTessellationGenerationLevel}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTessellationPatchSize" Word32 'False 
                                                                  #{offset VkPhysicalDeviceLimits, maxTessellationPatchSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTessellationControlPerVertexInputComponents" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexInputComponents}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTessellationControlPerVertexOutputComponents" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, maxTessellationControlPerVertexOutputComponents}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTessellationControlPerPatchOutputComponents" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, maxTessellationControlPerPatchOutputComponents}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTessellationControlTotalOutputComponents" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, maxTessellationControlTotalOutputComponents}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTessellationEvaluationInputComponents" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationInputComponents}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTessellationEvaluationOutputComponents" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, maxTessellationEvaluationOutputComponents}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxGeometryShaderInvocations" Word32 'False 
                                                                      #{offset VkPhysicalDeviceLimits, maxGeometryShaderInvocations}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxGeometryInputComponents" Word32 'False 
                                                                    #{offset VkPhysicalDeviceLimits, maxGeometryInputComponents}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxGeometryOutputComponents" Word32 'False 
                                                                     #{offset VkPhysicalDeviceLimits, maxGeometryOutputComponents}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxGeometryOutputVertices" Word32 'False 
                                                                   #{offset VkPhysicalDeviceLimits, maxGeometryOutputVertices}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxGeometryTotalOutputComponents" Word32 'False
                #{offset VkPhysicalDeviceLimits, maxGeometryTotalOutputComponents}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxFragmentInputComponents" Word32 'False 
                                                                    #{offset VkPhysicalDeviceLimits, maxFragmentInputComponents}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxFragmentOutputAttachments" Word32 'False 
                                                                      #{offset VkPhysicalDeviceLimits, maxFragmentOutputAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxFragmentDualSrcAttachments" Word32 'False 
                                                                       #{offset VkPhysicalDeviceLimits, maxFragmentDualSrcAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxFragmentCombinedOutputResources" Word32 'False
                #{offset VkPhysicalDeviceLimits, maxFragmentCombinedOutputResources}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxComputeSharedMemorySize" Word32 'False 
                                                                    #{offset VkPhysicalDeviceLimits, maxComputeSharedMemorySize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxComputeWorkGroupCount" Word32 'False 
                                                                  #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupCount}
                3
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxComputeWorkGroupInvocations" Word32 'False
                #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupInvocations}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxComputeWorkGroupSize" Word32 'False 
                                                                 #{offset VkPhysicalDeviceLimits, maxComputeWorkGroupSize}
                3
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subPixelPrecisionBits" Word32 'False 
                                                               #{offset VkPhysicalDeviceLimits, subPixelPrecisionBits}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subTexelPrecisionBits" Word32 'False 
                                                               #{offset VkPhysicalDeviceLimits, subTexelPrecisionBits}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "mipmapPrecisionBits" Word32 'False 
                                                             #{offset VkPhysicalDeviceLimits, mipmapPrecisionBits}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDrawIndexedIndexValue" Word32 'False 
                                                                  #{offset VkPhysicalDeviceLimits, maxDrawIndexedIndexValue}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDrawIndirectCount" Word32 'False 
                                                              #{offset VkPhysicalDeviceLimits, maxDrawIndirectCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxSamplerLodBias" ( -- ' closing tick for hsc2hs
                                              #{type float}
                                              ) 'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, maxSamplerLodBias}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxSamplerAnisotropy" ( -- ' closing tick for hsc2hs
                                                 #{type float})
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, maxSamplerAnisotropy}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxViewports" Word32 'False 
                                                      #{offset VkPhysicalDeviceLimits, maxViewports}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxViewportDimensions" Word32 'False 
                                                               #{offset VkPhysicalDeviceLimits, maxViewportDimensions}
                2
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "viewportBoundsRange" ( -- ' closing tick for hsc2hs
                                                #{type float})
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, viewportBoundsRange}
                2
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "viewportSubPixelBits" Word32 'False 
                                                              #{offset VkPhysicalDeviceLimits, viewportSubPixelBits}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minMemoryMapAlignment" CSize 'False 
                                                              #{offset VkPhysicalDeviceLimits, minMemoryMapAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minTexelBufferOffsetAlignment" VkDeviceSize 'False
                #{offset VkPhysicalDeviceLimits, minTexelBufferOffsetAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minUniformBufferOffsetAlignment" VkDeviceSize 'False
                #{offset VkPhysicalDeviceLimits, minUniformBufferOffsetAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minStorageBufferOffsetAlignment" VkDeviceSize 'False
                #{offset VkPhysicalDeviceLimits, minStorageBufferOffsetAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minTexelOffset" Int32 'False 
                                                       #{offset VkPhysicalDeviceLimits, minTexelOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTexelOffset" Word32 'False 
                                                        #{offset VkPhysicalDeviceLimits, maxTexelOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minTexelGatherOffset" Int32 'False 
                                                             #{offset VkPhysicalDeviceLimits, minTexelGatherOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTexelGatherOffset" Word32 'False 
                                                              #{offset VkPhysicalDeviceLimits, maxTexelGatherOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minInterpolationOffset" ( -- ' closing tick for hsc2hs
                                                   #{type float})
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, minInterpolationOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxInterpolationOffset" ( -- ' closing tick for hsc2hs
                                                   #{type float})
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, maxInterpolationOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subPixelInterpolationOffsetBits" Word32 'False
                #{offset VkPhysicalDeviceLimits, subPixelInterpolationOffsetBits}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxFramebufferWidth" Word32 'False 
                                                             #{offset VkPhysicalDeviceLimits, maxFramebufferWidth}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxFramebufferHeight" Word32 'False 
                                                              #{offset VkPhysicalDeviceLimits, maxFramebufferHeight}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxFramebufferLayers" Word32 'False 
                                                              #{offset VkPhysicalDeviceLimits, maxFramebufferLayers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "framebufferColorSampleCounts" VkSampleCountFlags 'True
                #{offset VkPhysicalDeviceLimits, framebufferColorSampleCounts}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "framebufferDepthSampleCounts" VkSampleCountFlags 'True
                #{offset VkPhysicalDeviceLimits, framebufferDepthSampleCounts}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "framebufferStencilSampleCounts" VkSampleCountFlags -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, framebufferStencilSampleCounts}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "framebufferNoAttachmentsSampleCounts" -- ' closing tick for hsc2hs
                VkSampleCountFlags
                'True -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, framebufferNoAttachmentsSampleCounts}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxColorAttachments" Word32 'False 
                                                             #{offset VkPhysicalDeviceLimits, maxColorAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampledImageColorSampleCounts" VkSampleCountFlags -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, sampledImageColorSampleCounts}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampledImageIntegerSampleCounts" VkSampleCountFlags -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, sampledImageIntegerSampleCounts}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampledImageDepthSampleCounts" VkSampleCountFlags -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, sampledImageDepthSampleCounts}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampledImageStencilSampleCounts" VkSampleCountFlags -- ' closing tick for hsc2hs
                'True -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, sampledImageStencilSampleCounts}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "storageImageSampleCounts" VkSampleCountFlags 'True
                #{offset VkPhysicalDeviceLimits, storageImageSampleCounts}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxSampleMaskWords" Word32 'False 
                                                            #{offset VkPhysicalDeviceLimits, maxSampleMaskWords}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "timestampComputeAndGraphics" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceLimits, timestampComputeAndGraphics}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "timestampPeriod" ( -- ' closing tick for hsc2hs
                                            #{type float}
                                            ) 'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, timestampPeriod}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxClipDistances" Word32 'False 
                                                          #{offset VkPhysicalDeviceLimits, maxClipDistances}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxCullDistances" Word32 'False 
                                                          #{offset VkPhysicalDeviceLimits, maxCullDistances}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxCombinedClipAndCullDistances" Word32 'False
                #{offset VkPhysicalDeviceLimits, maxCombinedClipAndCullDistances}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "discreteQueuePriorities" Word32 'False 
                                                                 #{offset VkPhysicalDeviceLimits, discreteQueuePriorities}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pointSizeRange" ( -- ' closing tick for hsc2hs
                                           #{type float}
                                           ) 'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, pointSizeRange}
                2
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "lineWidthRange" ( -- ' closing tick for hsc2hs
                                           #{type float}
                                           ) 'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, lineWidthRange}
                2
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pointSizeGranularity" ( -- ' closing tick for hsc2hs
                                                 #{type float})
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, pointSizeGranularity}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "lineWidthGranularity" ( -- ' closing tick for hsc2hs
                                                 #{type float})
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, lineWidthGranularity}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "strictLines" VkBool32 'False 
                                                       #{offset VkPhysicalDeviceLimits, strictLines}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "standardSampleLocations" VkBool32 'False 
                                                                   #{offset VkPhysicalDeviceLimits, standardSampleLocations}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "optimalBufferCopyOffsetAlignment" VkDeviceSize 'False
                #{offset VkPhysicalDeviceLimits, optimalBufferCopyOffsetAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "optimalBufferCopyRowPitchAlignment" VkDeviceSize -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceLimits, optimalBufferCopyRowPitchAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "nonCoherentAtomSize" VkDeviceSize 'False 
                                                                   #{offset VkPhysicalDeviceLimits, nonCoherentAtomSize}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceMaintenance3Properties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         maxPerSetDescriptors;
--   >     VkDeviceSize                     maxMemoryAllocationSize;
--   > } VkPhysicalDeviceMaintenance3Properties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceMaintenance3Properties VkPhysicalDeviceMaintenance3Properties registry at www.khronos.org>
type VkPhysicalDeviceMaintenance3Properties =
     VkStruct VkPhysicalDeviceMaintenance3Properties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceMaintenance3Properties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceMaintenance3Properties where
    type StructRep VkPhysicalDeviceMaintenance3Properties =
         'StructMeta "VkPhysicalDeviceMaintenance3Properties" -- ' closing tick for hsc2hs
           VkPhysicalDeviceMaintenance3Properties
           #{size VkPhysicalDeviceMaintenance3Properties}
           #{alignment VkPhysicalDeviceMaintenance3Properties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceMaintenance3Properties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceMaintenance3Properties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerSetDescriptors" Word32 'False 
                                                              #{offset VkPhysicalDeviceMaintenance3Properties, maxPerSetDescriptors}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxMemoryAllocationSize" VkDeviceSize 'False 
                                                                       #{offset VkPhysicalDeviceMaintenance3Properties, maxMemoryAllocationSize}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceMaintenance3Properties`
type VkPhysicalDeviceMaintenance3PropertiesKHR =
     VkPhysicalDeviceMaintenance3Properties

-- | > typedef struct VkPhysicalDeviceMemoryProperties {
--   >     uint32_t               memoryTypeCount;
--   >     VkMemoryType           memoryTypes[VK_MAX_MEMORY_TYPES];
--   >     uint32_t               memoryHeapCount;
--   >     VkMemoryHeap           memoryHeaps[VK_MAX_MEMORY_HEAPS];
--   > } VkPhysicalDeviceMemoryProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceMemoryProperties VkPhysicalDeviceMemoryProperties registry at www.khronos.org>
type VkPhysicalDeviceMemoryProperties =
     VkStruct VkPhysicalDeviceMemoryProperties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceMemoryProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceMemoryProperties where
    type StructRep VkPhysicalDeviceMemoryProperties =
         'StructMeta "VkPhysicalDeviceMemoryProperties" -- ' closing tick for hsc2hs
           VkPhysicalDeviceMemoryProperties
           #{size VkPhysicalDeviceMemoryProperties}
           #{alignment VkPhysicalDeviceMemoryProperties}
           '[('FieldMeta "memoryTypeCount" Word32 'False  -- ' closing tick for hsc2hs
                                                         #{offset VkPhysicalDeviceMemoryProperties, memoryTypeCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryTypes" VkMemoryType 'False 
                                                           #{offset VkPhysicalDeviceMemoryProperties, memoryTypes}
                VK_MAX_MEMORY_TYPES
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryHeapCount" Word32 'False 
                                                         #{offset VkPhysicalDeviceMemoryProperties, memoryHeapCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryHeaps" VkMemoryHeap 'False 
                                                           #{offset VkPhysicalDeviceMemoryProperties, memoryHeaps}
                VK_MAX_MEMORY_HEAPS
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceMemoryProperties2 {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPhysicalDeviceMemoryProperties memoryProperties;
--   > } VkPhysicalDeviceMemoryProperties2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceMemoryProperties2 VkPhysicalDeviceMemoryProperties2 registry at www.khronos.org>
type VkPhysicalDeviceMemoryProperties2 =
     VkStruct VkPhysicalDeviceMemoryProperties2' -- ' closing tick for hsc2hs

data VkPhysicalDeviceMemoryProperties2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceMemoryProperties2 where
    type StructRep VkPhysicalDeviceMemoryProperties2 =
         'StructMeta "VkPhysicalDeviceMemoryProperties2" -- ' closing tick for hsc2hs
           VkPhysicalDeviceMemoryProperties2
           #{size VkPhysicalDeviceMemoryProperties2}
           #{alignment VkPhysicalDeviceMemoryProperties2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceMemoryProperties2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceMemoryProperties2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryProperties" VkPhysicalDeviceMemoryProperties -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceMemoryProperties2, memoryProperties}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceMemoryProperties2`
type VkPhysicalDeviceMemoryProperties2KHR =
     VkPhysicalDeviceMemoryProperties2

-- | > typedef struct VkPhysicalDeviceMultiviewFeatures {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         multiview;
--   >     VkBool32                         multiviewGeometryShader;
--   >     VkBool32                         multiviewTessellationShader;
--   > } VkPhysicalDeviceMultiviewFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceMultiviewFeatures VkPhysicalDeviceMultiviewFeatures registry at www.khronos.org>
type VkPhysicalDeviceMultiviewFeatures =
     VkStruct VkPhysicalDeviceMultiviewFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceMultiviewFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceMultiviewFeatures where
    type StructRep VkPhysicalDeviceMultiviewFeatures =
         'StructMeta "VkPhysicalDeviceMultiviewFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceMultiviewFeatures
           #{size VkPhysicalDeviceMultiviewFeatures}
           #{alignment VkPhysicalDeviceMultiviewFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceMultiviewFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceMultiviewFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "multiview" VkBool32 'False 
                                                     #{offset VkPhysicalDeviceMultiviewFeatures, multiview}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "multiviewGeometryShader" VkBool32 'False 
                                                                   #{offset VkPhysicalDeviceMultiviewFeatures, multiviewGeometryShader}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "multiviewTessellationShader" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceMultiviewFeatures, multiviewTessellationShader}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceMultiviewFeatures`
type VkPhysicalDeviceMultiviewFeaturesKHR =
     VkPhysicalDeviceMultiviewFeatures

-- | > typedef struct VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         perViewPositionAllComponents;
--   > } VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX registry at www.khronos.org>
type VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX =
     VkStruct VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX' -- ' closing tick for hsc2hs

data VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
    type StructRep
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         =
         'StructMeta -- ' closing tick for hsc2hs
           "VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
           #{size VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX}
           #{alignment VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "perViewPositionAllComponents" VkBool32 'False
                #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceMultiviewProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         maxMultiviewViewCount;
--   >     uint32_t                         maxMultiviewInstanceIndex;
--   > } VkPhysicalDeviceMultiviewProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceMultiviewProperties VkPhysicalDeviceMultiviewProperties registry at www.khronos.org>
type VkPhysicalDeviceMultiviewProperties =
     VkStruct VkPhysicalDeviceMultiviewProperties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceMultiviewProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceMultiviewProperties where
    type StructRep VkPhysicalDeviceMultiviewProperties =
         'StructMeta "VkPhysicalDeviceMultiviewProperties" -- ' closing tick for hsc2hs
           VkPhysicalDeviceMultiviewProperties
           #{size VkPhysicalDeviceMultiviewProperties}
           #{alignment VkPhysicalDeviceMultiviewProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceMultiviewProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceMultiviewProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxMultiviewViewCount" Word32 'False 
                                                               #{offset VkPhysicalDeviceMultiviewProperties, maxMultiviewViewCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxMultiviewInstanceIndex" Word32 'False 
                                                                   #{offset VkPhysicalDeviceMultiviewProperties, maxMultiviewInstanceIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceMultiviewProperties`
type VkPhysicalDeviceMultiviewPropertiesKHR =
     VkPhysicalDeviceMultiviewProperties

-- | > typedef struct VkPhysicalDevicePointClippingProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPointClippingBehavior      pointClippingBehavior;
--   > } VkPhysicalDevicePointClippingProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDevicePointClippingProperties VkPhysicalDevicePointClippingProperties registry at www.khronos.org>
type VkPhysicalDevicePointClippingProperties =
     VkStruct VkPhysicalDevicePointClippingProperties' -- ' closing tick for hsc2hs

data VkPhysicalDevicePointClippingProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDevicePointClippingProperties
         where
    type StructRep VkPhysicalDevicePointClippingProperties =
         'StructMeta "VkPhysicalDevicePointClippingProperties" -- ' closing tick for hsc2hs
           VkPhysicalDevicePointClippingProperties
           #{size VkPhysicalDevicePointClippingProperties}
           #{alignment VkPhysicalDevicePointClippingProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDevicePointClippingProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDevicePointClippingProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pointClippingBehavior" VkPointClippingBehavior 'False
                #{offset VkPhysicalDevicePointClippingProperties, pointClippingBehavior}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDevicePointClippingProperties`
type VkPhysicalDevicePointClippingPropertiesKHR =
     VkPhysicalDevicePointClippingProperties

-- | > typedef struct VkPhysicalDeviceProperties {
--   >     uint32_t       apiVersion;
--   >     uint32_t       driverVersion;
--   >     uint32_t       vendorID;
--   >     uint32_t       deviceID;
--   >     VkPhysicalDeviceType deviceType;
--   >     char           deviceName[VK_MAX_PHYSICAL_DEVICE_NAME_SIZE];
--   >     uint8_t        pipelineCacheUUID[VK_UUID_SIZE];
--   >     VkPhysicalDeviceLimits limits;
--   >     VkPhysicalDeviceSparseProperties sparseProperties;
--   > } VkPhysicalDeviceProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceProperties VkPhysicalDeviceProperties registry at www.khronos.org>
type VkPhysicalDeviceProperties =
     VkStruct VkPhysicalDeviceProperties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceProperties where
    type StructRep VkPhysicalDeviceProperties =
         'StructMeta "VkPhysicalDeviceProperties" VkPhysicalDeviceProperties -- ' closing tick for hsc2hs
           #{size VkPhysicalDeviceProperties}
           #{alignment VkPhysicalDeviceProperties}
           '[('FieldMeta "apiVersion" Word32 'False  -- ' closing tick for hsc2hs
                                                    #{offset VkPhysicalDeviceProperties, apiVersion}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "driverVersion" Word32 'False 
                                                       #{offset VkPhysicalDeviceProperties, driverVersion}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vendorID" Word32 'False 
                                                  #{offset VkPhysicalDeviceProperties, vendorID}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceID" Word32 'False 
                                                  #{offset VkPhysicalDeviceProperties, deviceID}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceType" VkPhysicalDeviceType 'False 
                                                                  #{offset VkPhysicalDeviceProperties, deviceType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceName" CChar 'False 
                                                   #{offset VkPhysicalDeviceProperties, deviceName}
                VK_MAX_PHYSICAL_DEVICE_NAME_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineCacheUUID" Word8 'False 
                                                          #{offset VkPhysicalDeviceProperties, pipelineCacheUUID}
                VK_UUID_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "limits" VkPhysicalDeviceLimits 'False 
                                                                #{offset VkPhysicalDeviceProperties, limits}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sparseProperties" VkPhysicalDeviceSparseProperties -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceProperties, sparseProperties}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceProperties2 {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPhysicalDeviceProperties       properties;
--   > } VkPhysicalDeviceProperties2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceProperties2 VkPhysicalDeviceProperties2 registry at www.khronos.org>
type VkPhysicalDeviceProperties2 =
     VkStruct VkPhysicalDeviceProperties2' -- ' closing tick for hsc2hs

data VkPhysicalDeviceProperties2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceProperties2 where
    type StructRep VkPhysicalDeviceProperties2 =
         'StructMeta "VkPhysicalDeviceProperties2" -- ' closing tick for hsc2hs
           VkPhysicalDeviceProperties2
           #{size VkPhysicalDeviceProperties2}
           #{alignment VkPhysicalDeviceProperties2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceProperties2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceProperties2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "properties" VkPhysicalDeviceProperties 'False
                #{offset VkPhysicalDeviceProperties2, properties}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceProperties2`
type VkPhysicalDeviceProperties2KHR = VkPhysicalDeviceProperties2

-- | > typedef struct VkPhysicalDeviceProtectedMemoryFeatures {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     VkBool32                            protectedMemory;
--   > } VkPhysicalDeviceProtectedMemoryFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceProtectedMemoryFeatures VkPhysicalDeviceProtectedMemoryFeatures registry at www.khronos.org>
type VkPhysicalDeviceProtectedMemoryFeatures =
     VkStruct VkPhysicalDeviceProtectedMemoryFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceProtectedMemoryFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceProtectedMemoryFeatures
         where
    type StructRep VkPhysicalDeviceProtectedMemoryFeatures =
         'StructMeta "VkPhysicalDeviceProtectedMemoryFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceProtectedMemoryFeatures
           #{size VkPhysicalDeviceProtectedMemoryFeatures}
           #{alignment VkPhysicalDeviceProtectedMemoryFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceProtectedMemoryFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceProtectedMemoryFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "protectedMemory" VkBool32 'False 
                                                           #{offset VkPhysicalDeviceProtectedMemoryFeatures, protectedMemory}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceProtectedMemoryProperties {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     VkBool32                            protectedNoFault;
--   > } VkPhysicalDeviceProtectedMemoryProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceProtectedMemoryProperties VkPhysicalDeviceProtectedMemoryProperties registry at www.khronos.org>
type VkPhysicalDeviceProtectedMemoryProperties =
     VkStruct VkPhysicalDeviceProtectedMemoryProperties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceProtectedMemoryProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceProtectedMemoryProperties
         where
    type StructRep VkPhysicalDeviceProtectedMemoryProperties =
         'StructMeta "VkPhysicalDeviceProtectedMemoryProperties" -- ' closing tick for hsc2hs
           VkPhysicalDeviceProtectedMemoryProperties
           #{size VkPhysicalDeviceProtectedMemoryProperties}
           #{alignment VkPhysicalDeviceProtectedMemoryProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceProtectedMemoryProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceProtectedMemoryProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "protectedNoFault" VkBool32 'False 
                                                            #{offset VkPhysicalDeviceProtectedMemoryProperties, protectedNoFault}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDevicePushDescriptorPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         maxPushDescriptors;
--   > } VkPhysicalDevicePushDescriptorPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDevicePushDescriptorPropertiesKHR VkPhysicalDevicePushDescriptorPropertiesKHR registry at www.khronos.org>
type VkPhysicalDevicePushDescriptorPropertiesKHR =
     VkStruct VkPhysicalDevicePushDescriptorPropertiesKHR' -- ' closing tick for hsc2hs

data VkPhysicalDevicePushDescriptorPropertiesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDevicePushDescriptorPropertiesKHR
         where
    type StructRep VkPhysicalDevicePushDescriptorPropertiesKHR =
         'StructMeta "VkPhysicalDevicePushDescriptorPropertiesKHR" -- ' closing tick for hsc2hs
           VkPhysicalDevicePushDescriptorPropertiesKHR
           #{size VkPhysicalDevicePushDescriptorPropertiesKHR}
           #{alignment VkPhysicalDevicePushDescriptorPropertiesKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPushDescriptors" Word32 'False 
                                                            #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, maxPushDescriptors}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceSampleLocationsPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkSampleCountFlags               sampleLocationSampleCounts;
--   >     VkExtent2D                       maxSampleLocationGridSize;
--   >     float                            sampleLocationCoordinateRange[2];
--   >     uint32_t                         sampleLocationSubPixelBits;
--   >     VkBool32                         variableSampleLocations;
--   > } VkPhysicalDeviceSampleLocationsPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceSampleLocationsPropertiesEXT VkPhysicalDeviceSampleLocationsPropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceSampleLocationsPropertiesEXT =
     VkStruct VkPhysicalDeviceSampleLocationsPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceSampleLocationsPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceSampleLocationsPropertiesEXT
         where
    type StructRep VkPhysicalDeviceSampleLocationsPropertiesEXT =
         'StructMeta "VkPhysicalDeviceSampleLocationsPropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceSampleLocationsPropertiesEXT
           #{size VkPhysicalDeviceSampleLocationsPropertiesEXT}
           #{alignment VkPhysicalDeviceSampleLocationsPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampleLocationSampleCounts" VkSampleCountFlags 'False
                #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSampleCounts}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxSampleLocationGridSize" VkExtent2D 'False 
                                                                       #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, maxSampleLocationGridSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampleLocationCoordinateRange" -- ' closing tick for hsc2hs
                (#{type float})
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationCoordinateRange}
                2
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sampleLocationSubPixelBits" Word32 'False 
                                                                    #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, sampleLocationSubPixelBits}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "variableSampleLocations" VkBool32 'False 
                                                                   #{offset VkPhysicalDeviceSampleLocationsPropertiesEXT, variableSampleLocations}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     VkBool32               filterMinmaxSingleComponentFormats;
--   >     VkBool32               filterMinmaxImageComponentMapping;
--   > } VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT =
     VkStruct VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
    type StructRep VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT =
         'StructMeta "VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
           #{size VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT}
           #{alignment VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "filterMinmaxSingleComponentFormats" VkBool32 'False
                #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, filterMinmaxSingleComponentFormats}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "filterMinmaxImageComponentMapping" VkBool32 'False
                #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, filterMinmaxImageComponentMapping}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceSamplerYcbcrConversionFeatures {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     VkBool32                         samplerYcbcrConversion;
--   > } VkPhysicalDeviceSamplerYcbcrConversionFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceSamplerYcbcrConversionFeatures VkPhysicalDeviceSamplerYcbcrConversionFeatures registry at www.khronos.org>
type VkPhysicalDeviceSamplerYcbcrConversionFeatures =
     VkStruct VkPhysicalDeviceSamplerYcbcrConversionFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceSamplerYcbcrConversionFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceSamplerYcbcrConversionFeatures
         where
    type StructRep VkPhysicalDeviceSamplerYcbcrConversionFeatures =
         'StructMeta "VkPhysicalDeviceSamplerYcbcrConversionFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceSamplerYcbcrConversionFeatures
           #{size VkPhysicalDeviceSamplerYcbcrConversionFeatures}
           #{alignment VkPhysicalDeviceSamplerYcbcrConversionFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "samplerYcbcrConversion" VkBool32 'False 
                                                                  #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, samplerYcbcrConversion}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceSamplerYcbcrConversionFeatures`
type VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR =
     VkPhysicalDeviceSamplerYcbcrConversionFeatures

-- | > typedef struct VkPhysicalDeviceShaderCorePropertiesAMD {
--   >     VkStructureType sType;
--   >     void*    pNext;
--   >     uint32_t shaderEngineCount;
--   >     uint32_t shaderArraysPerEngineCount;
--   >     uint32_t computeUnitsPerShaderArray;
--   >     uint32_t simdPerComputeUnit;
--   >     uint32_t wavefrontsPerSimd;
--   >     uint32_t wavefrontSize;
--   >     uint32_t sgprsPerSimd;
--   >     uint32_t minSgprAllocation;
--   >     uint32_t maxSgprAllocation;
--   >     uint32_t sgprAllocationGranularity;
--   >     uint32_t vgprsPerSimd;
--   >     uint32_t minVgprAllocation;
--   >     uint32_t maxVgprAllocation;
--   >     uint32_t vgprAllocationGranularity;
--   > } VkPhysicalDeviceShaderCorePropertiesAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceShaderCorePropertiesAMD VkPhysicalDeviceShaderCorePropertiesAMD registry at www.khronos.org>
type VkPhysicalDeviceShaderCorePropertiesAMD =
     VkStruct VkPhysicalDeviceShaderCorePropertiesAMD' -- ' closing tick for hsc2hs

data VkPhysicalDeviceShaderCorePropertiesAMD' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceShaderCorePropertiesAMD
         where
    type StructRep VkPhysicalDeviceShaderCorePropertiesAMD =
         'StructMeta "VkPhysicalDeviceShaderCorePropertiesAMD" -- ' closing tick for hsc2hs
           VkPhysicalDeviceShaderCorePropertiesAMD
           #{size VkPhysicalDeviceShaderCorePropertiesAMD}
           #{alignment VkPhysicalDeviceShaderCorePropertiesAMD}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceShaderCorePropertiesAMD, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderEngineCount" Word32 'False 
                                                           #{offset VkPhysicalDeviceShaderCorePropertiesAMD, shaderEngineCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderArraysPerEngineCount" Word32 'False 
                                                                    #{offset VkPhysicalDeviceShaderCorePropertiesAMD, shaderArraysPerEngineCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "computeUnitsPerShaderArray" Word32 'False 
                                                                    #{offset VkPhysicalDeviceShaderCorePropertiesAMD, computeUnitsPerShaderArray}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "simdPerComputeUnit" Word32 'False 
                                                            #{offset VkPhysicalDeviceShaderCorePropertiesAMD, simdPerComputeUnit}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "wavefrontsPerSimd" Word32 'False 
                                                           #{offset VkPhysicalDeviceShaderCorePropertiesAMD, wavefrontsPerSimd}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "wavefrontSize" Word32 'False 
                                                       #{offset VkPhysicalDeviceShaderCorePropertiesAMD, wavefrontSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sgprsPerSimd" Word32 'False 
                                                      #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sgprsPerSimd}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minSgprAllocation" Word32 'False 
                                                           #{offset VkPhysicalDeviceShaderCorePropertiesAMD, minSgprAllocation}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxSgprAllocation" Word32 'False 
                                                           #{offset VkPhysicalDeviceShaderCorePropertiesAMD, maxSgprAllocation}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sgprAllocationGranularity" Word32 'False 
                                                                   #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sgprAllocationGranularity}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vgprsPerSimd" Word32 'False 
                                                      #{offset VkPhysicalDeviceShaderCorePropertiesAMD, vgprsPerSimd}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minVgprAllocation" Word32 'False 
                                                           #{offset VkPhysicalDeviceShaderCorePropertiesAMD, minVgprAllocation}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxVgprAllocation" Word32 'False 
                                                           #{offset VkPhysicalDeviceShaderCorePropertiesAMD, maxVgprAllocation}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vgprAllocationGranularity" Word32 'False 
                                                                   #{offset VkPhysicalDeviceShaderCorePropertiesAMD, vgprAllocationGranularity}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceShaderDrawParameterFeatures {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         shaderDrawParameters;
--   > } VkPhysicalDeviceShaderDrawParameterFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceShaderDrawParameterFeatures VkPhysicalDeviceShaderDrawParameterFeatures registry at www.khronos.org>
type VkPhysicalDeviceShaderDrawParameterFeatures =
     VkStruct VkPhysicalDeviceShaderDrawParameterFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceShaderDrawParameterFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceShaderDrawParameterFeatures
         where
    type StructRep VkPhysicalDeviceShaderDrawParameterFeatures =
         'StructMeta "VkPhysicalDeviceShaderDrawParameterFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceShaderDrawParameterFeatures
           #{size VkPhysicalDeviceShaderDrawParameterFeatures}
           #{alignment VkPhysicalDeviceShaderDrawParameterFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceShaderDrawParameterFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceShaderDrawParameterFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDrawParameters" VkBool32 'False 
                                                                #{offset VkPhysicalDeviceShaderDrawParameterFeatures, shaderDrawParameters}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceSparseImageFormatInfo2 {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkFormat                         format;
--   >     VkImageType                      type;
--   >     VkSampleCountFlagBits            samples;
--   >     VkImageUsageFlags                usage;
--   >     VkImageTiling                    tiling;
--   > } VkPhysicalDeviceSparseImageFormatInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceSparseImageFormatInfo2 VkPhysicalDeviceSparseImageFormatInfo2 registry at www.khronos.org>
type VkPhysicalDeviceSparseImageFormatInfo2 =
     VkStruct VkPhysicalDeviceSparseImageFormatInfo2' -- ' closing tick for hsc2hs

data VkPhysicalDeviceSparseImageFormatInfo2' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceSparseImageFormatInfo2 where
    type StructRep VkPhysicalDeviceSparseImageFormatInfo2 =
         'StructMeta "VkPhysicalDeviceSparseImageFormatInfo2" -- ' closing tick for hsc2hs
           VkPhysicalDeviceSparseImageFormatInfo2
           #{size VkPhysicalDeviceSparseImageFormatInfo2}
           #{alignment VkPhysicalDeviceSparseImageFormatInfo2}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceSparseImageFormatInfo2, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceSparseImageFormatInfo2, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "format" VkFormat 'False 
                                                  #{offset VkPhysicalDeviceSparseImageFormatInfo2, format}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "type" VkImageType 'False 
                                                   #{offset VkPhysicalDeviceSparseImageFormatInfo2, type}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "samples" VkSampleCountFlagBits 'False 
                                                                #{offset VkPhysicalDeviceSparseImageFormatInfo2, samples}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "usage" VkImageUsageFlags 'False 
                                                          #{offset VkPhysicalDeviceSparseImageFormatInfo2, usage}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "tiling" VkImageTiling 'False 
                                                       #{offset VkPhysicalDeviceSparseImageFormatInfo2, tiling}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceSparseImageFormatInfo2`
type VkPhysicalDeviceSparseImageFormatInfo2KHR =
     VkPhysicalDeviceSparseImageFormatInfo2

-- | > typedef struct VkPhysicalDeviceSparseProperties {
--   >     VkBool32               residencyStandard2DBlockShape;
--   >     VkBool32               residencyStandard2DMultisampleBlockShape;
--   >     VkBool32               residencyStandard3DBlockShape;
--   >     VkBool32               residencyAlignedMipSize;
--   >     VkBool32               residencyNonResidentStrict;
--   > } VkPhysicalDeviceSparseProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceSparseProperties VkPhysicalDeviceSparseProperties registry at www.khronos.org>
type VkPhysicalDeviceSparseProperties =
     VkStruct VkPhysicalDeviceSparseProperties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceSparseProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceSparseProperties where
    type StructRep VkPhysicalDeviceSparseProperties =
         'StructMeta "VkPhysicalDeviceSparseProperties" -- ' closing tick for hsc2hs
           VkPhysicalDeviceSparseProperties
           #{size VkPhysicalDeviceSparseProperties}
           #{alignment VkPhysicalDeviceSparseProperties}
           '[('FieldMeta "residencyStandard2DBlockShape" VkBool32 'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DBlockShape}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "residencyStandard2DMultisampleBlockShape" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DMultisampleBlockShape}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "residencyStandard3DBlockShape" VkBool32 'False
                #{offset VkPhysicalDeviceSparseProperties, residencyStandard3DBlockShape}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "residencyAlignedMipSize" VkBool32 'False 
                                                                   #{offset VkPhysicalDeviceSparseProperties, residencyAlignedMipSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "residencyNonResidentStrict" VkBool32 'False 
                                                                      #{offset VkPhysicalDeviceSparseProperties, residencyNonResidentStrict}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceSubgroupProperties {
--   >     VkStructureType sType;
--   >     void*                   pNext;
--   >     uint32_t                      subgroupSize;
--   >     VkShaderStageFlags            supportedStages;
--   >     VkSubgroupFeatureFlags        supportedOperations;
--   >     VkBool32 quadOperationsInAllStages;
--   > } VkPhysicalDeviceSubgroupProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceSubgroupProperties VkPhysicalDeviceSubgroupProperties registry at www.khronos.org>
type VkPhysicalDeviceSubgroupProperties =
     VkStruct VkPhysicalDeviceSubgroupProperties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceSubgroupProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceSubgroupProperties where
    type StructRep VkPhysicalDeviceSubgroupProperties =
         'StructMeta "VkPhysicalDeviceSubgroupProperties" -- ' closing tick for hsc2hs
           VkPhysicalDeviceSubgroupProperties
           #{size VkPhysicalDeviceSubgroupProperties}
           #{alignment VkPhysicalDeviceSubgroupProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceSubgroupProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceSubgroupProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subgroupSize" Word32 'False 
                                                      #{offset VkPhysicalDeviceSubgroupProperties, subgroupSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "supportedStages" VkShaderStageFlags 'False 
                                                                     #{offset VkPhysicalDeviceSubgroupProperties, supportedStages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "supportedOperations" VkSubgroupFeatureFlags 'False
                #{offset VkPhysicalDeviceSubgroupProperties, supportedOperations}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "quadOperationsInAllStages" VkBool32 'False 
                                                                     #{offset VkPhysicalDeviceSubgroupProperties, quadOperationsInAllStages}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceSurfaceInfo2KHR {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkSurfaceKHR surface;
--   > } VkPhysicalDeviceSurfaceInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceSurfaceInfo2KHR VkPhysicalDeviceSurfaceInfo2KHR registry at www.khronos.org>
type VkPhysicalDeviceSurfaceInfo2KHR =
     VkStruct VkPhysicalDeviceSurfaceInfo2KHR' -- ' closing tick for hsc2hs

data VkPhysicalDeviceSurfaceInfo2KHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceSurfaceInfo2KHR where
    type StructRep VkPhysicalDeviceSurfaceInfo2KHR =
         'StructMeta "VkPhysicalDeviceSurfaceInfo2KHR" -- ' closing tick for hsc2hs
           VkPhysicalDeviceSurfaceInfo2KHR
           #{size VkPhysicalDeviceSurfaceInfo2KHR}
           #{alignment VkPhysicalDeviceSurfaceInfo2KHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceSurfaceInfo2KHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceSurfaceInfo2KHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "surface" VkSurfaceKHR 'False 
                                                       #{offset VkPhysicalDeviceSurfaceInfo2KHR, surface}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceVariablePointerFeatures {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         variablePointersStorageBuffer;
--   >     VkBool32                         variablePointers;
--   > } VkPhysicalDeviceVariablePointerFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceVariablePointerFeatures VkPhysicalDeviceVariablePointerFeatures registry at www.khronos.org>
type VkPhysicalDeviceVariablePointerFeatures =
     VkStruct VkPhysicalDeviceVariablePointerFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceVariablePointerFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceVariablePointerFeatures
         where
    type StructRep VkPhysicalDeviceVariablePointerFeatures =
         'StructMeta "VkPhysicalDeviceVariablePointerFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceVariablePointerFeatures
           #{size VkPhysicalDeviceVariablePointerFeatures}
           #{alignment VkPhysicalDeviceVariablePointerFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceVariablePointerFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceVariablePointerFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "variablePointersStorageBuffer" VkBool32 'False
                #{offset VkPhysicalDeviceVariablePointerFeatures, variablePointersStorageBuffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "variablePointers" VkBool32 'False 
                                                            #{offset VkPhysicalDeviceVariablePointerFeatures, variablePointers}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceVariablePointerFeatures`
type VkPhysicalDeviceVariablePointerFeaturesKHR =
     VkPhysicalDeviceVariablePointerFeatures

-- | > typedef struct VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     uint32_t               maxVertexAttribDivisor;
--   > } VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT =
     VkStruct VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         where
    type StructRep VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
         =
         'StructMeta "VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
           #{size VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT}
           #{alignment VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxVertexAttribDivisor" Word32 'False 
                                                                #{offset VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT, maxVertexAttribDivisor}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs
