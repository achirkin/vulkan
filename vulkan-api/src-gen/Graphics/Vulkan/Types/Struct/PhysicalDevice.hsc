#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PhysicalDevice
       (VkPhysicalDevice16BitStorageFeatures,
        VkPhysicalDevice16BitStorageFeaturesKHR,
        VkPhysicalDevice4444FormatsFeaturesEXT,
        VkPhysicalDevice8BitStorageFeatures,
        VkPhysicalDevice8BitStorageFeaturesKHR,
        VkPhysicalDeviceASTCDecodeFeaturesEXT,
        VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT,
        VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT,
        VkPhysicalDeviceBufferAddressFeaturesEXT,
        VkPhysicalDeviceBufferDeviceAddressFeatures,
        VkPhysicalDeviceBufferDeviceAddressFeaturesEXT,
        VkPhysicalDeviceBufferDeviceAddressFeaturesKHR,
        VkPhysicalDeviceCoherentMemoryFeaturesAMD,
        VkPhysicalDeviceComputeShaderDerivativesFeaturesNV,
        VkPhysicalDeviceConditionalRenderingFeaturesEXT,
        VkPhysicalDeviceConservativeRasterizationPropertiesEXT,
        VkPhysicalDeviceCooperativeMatrixFeaturesNV,
        VkPhysicalDeviceCooperativeMatrixPropertiesNV,
        VkPhysicalDeviceCornerSampledImageFeaturesNV,
        VkPhysicalDeviceCoverageReductionModeFeaturesNV,
        VkPhysicalDeviceCustomBorderColorFeaturesEXT,
        VkPhysicalDeviceCustomBorderColorPropertiesEXT,
        VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV,
        VkPhysicalDeviceDepthClipEnableFeaturesEXT,
        VkPhysicalDeviceDepthStencilResolveProperties,
        VkPhysicalDeviceDepthStencilResolvePropertiesKHR,
        VkPhysicalDeviceDescriptorIndexingFeatures,
        VkPhysicalDeviceDescriptorIndexingFeaturesEXT,
        VkPhysicalDeviceDescriptorIndexingProperties,
        VkPhysicalDeviceDescriptorIndexingPropertiesEXT,
        VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV,
        VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV,
        VkPhysicalDeviceDiagnosticsConfigFeaturesNV,
        VkPhysicalDeviceDiscardRectanglePropertiesEXT,
        VkPhysicalDeviceDriverProperties,
        VkPhysicalDeviceDriverPropertiesKHR,
        VkPhysicalDeviceExclusiveScissorFeaturesNV,
        VkPhysicalDeviceExtendedDynamicStateFeaturesEXT,
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
        VkPhysicalDeviceFloat16Int8FeaturesKHR,
        VkPhysicalDeviceFloatControlsProperties,
        VkPhysicalDeviceFloatControlsPropertiesKHR,
        VkPhysicalDeviceFragmentDensityMap2FeaturesEXT,
        VkPhysicalDeviceFragmentDensityMap2PropertiesEXT,
        VkPhysicalDeviceFragmentDensityMapFeaturesEXT,
        VkPhysicalDeviceFragmentDensityMapPropertiesEXT,
        VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV,
        VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT,
        VkPhysicalDeviceGroupProperties,
        VkPhysicalDeviceGroupPropertiesKHR,
        VkPhysicalDeviceHostQueryResetFeatures,
        VkPhysicalDeviceHostQueryResetFeaturesEXT,
        VkPhysicalDeviceIDProperties, VkPhysicalDeviceIDPropertiesKHR,
        VkPhysicalDeviceImageDrmFormatModifierInfoEXT,
        VkPhysicalDeviceImageFormatInfo2,
        VkPhysicalDeviceImageFormatInfo2KHR,
        VkPhysicalDeviceImageRobustnessFeaturesEXT,
        VkPhysicalDeviceImageViewImageFormatInfoEXT,
        VkPhysicalDeviceImagelessFramebufferFeatures,
        VkPhysicalDeviceImagelessFramebufferFeaturesKHR,
        VkPhysicalDeviceIndexTypeUint8FeaturesEXT,
        VkPhysicalDeviceInlineUniformBlockFeaturesEXT,
        VkPhysicalDeviceInlineUniformBlockPropertiesEXT,
        VkPhysicalDeviceLimits,
        VkPhysicalDeviceLineRasterizationFeaturesEXT,
        VkPhysicalDeviceLineRasterizationPropertiesEXT,
        VkPhysicalDeviceMaintenance3Properties,
        VkPhysicalDeviceMaintenance3PropertiesKHR,
        VkPhysicalDeviceMemoryBudgetPropertiesEXT,
        VkPhysicalDeviceMemoryPriorityFeaturesEXT,
        VkPhysicalDeviceMemoryProperties,
        VkPhysicalDeviceMemoryProperties2,
        VkPhysicalDeviceMemoryProperties2KHR,
        VkPhysicalDeviceMeshShaderFeaturesNV,
        VkPhysicalDeviceMeshShaderPropertiesNV,
        VkPhysicalDeviceMultiviewFeatures,
        VkPhysicalDeviceMultiviewFeaturesKHR,
        VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX,
        VkPhysicalDeviceMultiviewProperties,
        VkPhysicalDeviceMultiviewPropertiesKHR,
        VkPhysicalDevicePCIBusInfoPropertiesEXT,
        VkPhysicalDevicePerformanceQueryFeaturesKHR,
        VkPhysicalDevicePerformanceQueryPropertiesKHR,
        VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT,
        VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR,
        VkPhysicalDevicePointClippingProperties,
        VkPhysicalDevicePointClippingPropertiesKHR,
        VkPhysicalDevicePrivateDataFeaturesEXT, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2, VkPhysicalDeviceProperties2KHR,
        VkPhysicalDeviceProtectedMemoryFeatures,
        VkPhysicalDeviceProtectedMemoryProperties,
        VkPhysicalDevicePushDescriptorPropertiesKHR,
        VkPhysicalDeviceRayTracingPropertiesNV,
        VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV,
        VkPhysicalDeviceRobustness2FeaturesEXT,
        VkPhysicalDeviceRobustness2PropertiesEXT,
        VkPhysicalDeviceSampleLocationsPropertiesEXT,
        VkPhysicalDeviceSamplerFilterMinmaxProperties,
        VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT,
        VkPhysicalDeviceSamplerYcbcrConversionFeatures,
        VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR,
        VkPhysicalDeviceScalarBlockLayoutFeatures,
        VkPhysicalDeviceScalarBlockLayoutFeaturesEXT,
        VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures,
        VkPhysicalDeviceSeparateDepthStencilLayoutsFeaturesKHR,
        VkPhysicalDeviceShaderAtomicFloatFeaturesEXT,
        VkPhysicalDeviceShaderAtomicInt64Features,
        VkPhysicalDeviceShaderAtomicInt64FeaturesKHR,
        VkPhysicalDeviceShaderClockFeaturesKHR,
        VkPhysicalDeviceShaderCoreProperties2AMD,
        VkPhysicalDeviceShaderCorePropertiesAMD,
        VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT,
        VkPhysicalDeviceShaderDrawParameterFeatures,
        VkPhysicalDeviceShaderDrawParametersFeatures,
        VkPhysicalDeviceShaderFloat16Int8Features,
        VkPhysicalDeviceShaderFloat16Int8FeaturesKHR,
        VkPhysicalDeviceShaderImageFootprintFeaturesNV,
        VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL,
        VkPhysicalDeviceShaderSMBuiltinsFeaturesNV,
        VkPhysicalDeviceShaderSMBuiltinsPropertiesNV,
        VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures,
        VkPhysicalDeviceShaderSubgroupExtendedTypesFeaturesKHR,
        VkPhysicalDeviceShadingRateImageFeaturesNV,
        VkPhysicalDeviceShadingRateImagePropertiesNV,
        VkPhysicalDeviceSparseImageFormatInfo2,
        VkPhysicalDeviceSparseImageFormatInfo2KHR,
        VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceSubgroupProperties,
        VkPhysicalDeviceSubgroupSizeControlFeaturesEXT,
        VkPhysicalDeviceSubgroupSizeControlPropertiesEXT,
        VkPhysicalDeviceSurfaceInfo2KHR,
        VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT,
        VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT,
        VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT,
        VkPhysicalDeviceTimelineSemaphoreFeatures,
        VkPhysicalDeviceTimelineSemaphoreFeaturesKHR,
        VkPhysicalDeviceTimelineSemaphoreProperties,
        VkPhysicalDeviceTimelineSemaphorePropertiesKHR,
        VkPhysicalDeviceToolPropertiesEXT,
        VkPhysicalDeviceTransformFeedbackFeaturesEXT,
        VkPhysicalDeviceTransformFeedbackPropertiesEXT,
        VkPhysicalDeviceUniformBufferStandardLayoutFeatures,
        VkPhysicalDeviceUniformBufferStandardLayoutFeaturesKHR,
        VkPhysicalDeviceVariablePointerFeatures,
        VkPhysicalDeviceVariablePointerFeaturesKHR,
        VkPhysicalDeviceVariablePointersFeatures,
        VkPhysicalDeviceVariablePointersFeaturesKHR,
        VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT,
        VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT,
        VkPhysicalDeviceVulkan11Features,
        VkPhysicalDeviceVulkan11Properties,
        VkPhysicalDeviceVulkan12Features,
        VkPhysicalDeviceVulkan12Properties,
        VkPhysicalDeviceVulkanMemoryModelFeatures,
        VkPhysicalDeviceVulkanMemoryModelFeaturesKHR,
        VkPhysicalDeviceYcbcrImageArraysFeaturesEXT)
       where
import Graphics.Vulkan.Constants                           (VK_LUID_SIZE, VK_MAX_DESCRIPTION_SIZE,
                                                            VK_MAX_DEVICE_GROUP_SIZE,
                                                            VK_MAX_DRIVER_INFO_SIZE,
                                                            VK_MAX_DRIVER_NAME_SIZE,
                                                            VK_MAX_EXTENSION_NAME_SIZE,
                                                            VK_MAX_MEMORY_HEAPS,
                                                            VK_MAX_MEMORY_TYPES,
                                                            VK_MAX_PHYSICAL_DEVICE_NAME_SIZE,
                                                            VK_UUID_SIZE)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.BaseTypes                     (VkBool32,
                                                            VkDeviceSize)
import Graphics.Vulkan.Types.Enum.Buffer                   (VkBufferCreateFlags,
                                                            VkBufferUsageFlags)
import Graphics.Vulkan.Types.Enum.DriverId                 (VkDriverId)
import Graphics.Vulkan.Types.Enum.External                 (VkExternalFenceHandleTypeFlagBits,
                                                            VkExternalMemoryHandleTypeFlagBits,
                                                            VkExternalSemaphoreHandleTypeFlagBits)
import Graphics.Vulkan.Types.Enum.Format                   (VkFormat)
import Graphics.Vulkan.Types.Enum.Image                    (VkImageCreateFlags,
                                                            VkImageTiling,
                                                            VkImageType,
                                                            VkImageUsageFlags,
                                                            VkImageViewType)
import Graphics.Vulkan.Types.Enum.PhysicalDeviceType       (VkPhysicalDeviceType)
import Graphics.Vulkan.Types.Enum.PointClippingBehavior    (VkPointClippingBehavior)
import Graphics.Vulkan.Types.Enum.ResolveModeFlag          (VkResolveModeFlags)
import Graphics.Vulkan.Types.Enum.SampleCountFlags         (VkSampleCountFlagBits,
                                                            VkSampleCountFlags)
import Graphics.Vulkan.Types.Enum.Shader                   (VkShaderCorePropertiesFlagsAMD,
                                                            VkShaderFloatControlsIndependence,
                                                            VkShaderStageFlags)
import Graphics.Vulkan.Types.Enum.SharingMode              (VkSharingMode)
import Graphics.Vulkan.Types.Enum.StructureType            (VkStructureType)
import Graphics.Vulkan.Types.Enum.SubgroupFeatureFlags     (VkSubgroupFeatureFlags)
import Graphics.Vulkan.Types.Enum.ToolPurposeFlagsEXT      (VkToolPurposeFlagsEXT)
import Graphics.Vulkan.Types.Handles                       (VkPhysicalDevice,
                                                            VkSurfaceKHR)
import Graphics.Vulkan.Types.Struct.ConformanceVersion     (VkConformanceVersion)
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo)
import Graphics.Vulkan.Types.Struct.Extent                 (VkExtent2D)
import Graphics.Vulkan.Types.Struct.Memory                 (VkMemoryHeap,
                                                            VkMemoryType)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)

-- | > typedef struct VkPhysicalDevice16BitStorageFeatures {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     VkBool32                         storageBuffer16BitAccess;
--   >     VkBool32                         uniformAndStorageBuffer16BitAccess;
--   >     VkBool32                         storagePushConstant16;
--   >     VkBool32                         storageInputOutput16;
--   > } VkPhysicalDevice16BitStorageFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDevice16BitStorageFeatures VkPhysicalDevice16BitStorageFeatures registry at www.khronos.org>
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

-- | > typedef struct VkPhysicalDevice4444FormatsFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           formatA4R4G4B4;
--   >     VkBool32                           formatA4B4G4R4;
--   > } VkPhysicalDevice4444FormatsFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDevice4444FormatsFeaturesEXT VkPhysicalDevice4444FormatsFeaturesEXT registry at www.khronos.org>
type VkPhysicalDevice4444FormatsFeaturesEXT =
     VkStruct VkPhysicalDevice4444FormatsFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDevice4444FormatsFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDevice4444FormatsFeaturesEXT where
    type StructRep VkPhysicalDevice4444FormatsFeaturesEXT =
         'StructMeta "VkPhysicalDevice4444FormatsFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDevice4444FormatsFeaturesEXT
           #{size VkPhysicalDevice4444FormatsFeaturesEXT}
           #{alignment VkPhysicalDevice4444FormatsFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDevice4444FormatsFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDevice4444FormatsFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "formatA4R4G4B4" VkBool32 'False 
                                                          #{offset VkPhysicalDevice4444FormatsFeaturesEXT, formatA4R4G4B4}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "formatA4B4G4R4" VkBool32 'False 
                                                          #{offset VkPhysicalDevice4444FormatsFeaturesEXT, formatA4B4G4R4}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDevice8BitStorageFeatures {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     VkBool32                         storageBuffer8BitAccess;
--   >     VkBool32                         uniformAndStorageBuffer8BitAccess;
--   >     VkBool32                         storagePushConstant8;
--   > } VkPhysicalDevice8BitStorageFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDevice8BitStorageFeatures VkPhysicalDevice8BitStorageFeatures registry at www.khronos.org>
type VkPhysicalDevice8BitStorageFeatures =
     VkStruct VkPhysicalDevice8BitStorageFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDevice8BitStorageFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDevice8BitStorageFeatures where
    type StructRep VkPhysicalDevice8BitStorageFeatures =
         'StructMeta "VkPhysicalDevice8BitStorageFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDevice8BitStorageFeatures
           #{size VkPhysicalDevice8BitStorageFeatures}
           #{alignment VkPhysicalDevice8BitStorageFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDevice8BitStorageFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDevice8BitStorageFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "storageBuffer8BitAccess" VkBool32 'False 
                                                                   #{offset VkPhysicalDevice8BitStorageFeatures, storageBuffer8BitAccess}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "uniformAndStorageBuffer8BitAccess" VkBool32 'False
                #{offset VkPhysicalDevice8BitStorageFeatures, uniformAndStorageBuffer8BitAccess}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "storagePushConstant8" VkBool32 'False 
                                                                #{offset VkPhysicalDevice8BitStorageFeatures, storagePushConstant8}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDevice8BitStorageFeatures`
type VkPhysicalDevice8BitStorageFeaturesKHR =
     VkPhysicalDevice8BitStorageFeatures

-- | > typedef struct VkPhysicalDeviceASTCDecodeFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     VkBool32                         decodeModeSharedExponent;
--   > } VkPhysicalDeviceASTCDecodeFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceASTCDecodeFeaturesEXT VkPhysicalDeviceASTCDecodeFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceASTCDecodeFeaturesEXT =
     VkStruct VkPhysicalDeviceASTCDecodeFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceASTCDecodeFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceASTCDecodeFeaturesEXT where
    type StructRep VkPhysicalDeviceASTCDecodeFeaturesEXT =
         'StructMeta "VkPhysicalDeviceASTCDecodeFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceASTCDecodeFeaturesEXT
           #{size VkPhysicalDeviceASTCDecodeFeaturesEXT}
           #{alignment VkPhysicalDeviceASTCDecodeFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceASTCDecodeFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceASTCDecodeFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "decodeModeSharedExponent" VkBool32 'False 
                                                                    #{offset VkPhysicalDeviceASTCDecodeFeaturesEXT, decodeModeSharedExponent}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         advancedBlendCoherentOperations;
--   > } VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT registry at www.khronos.org>
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
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT registry at www.khronos.org>
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

-- | Alias for `VkPhysicalDeviceBufferDeviceAddressFeaturesEXT`
type VkPhysicalDeviceBufferAddressFeaturesEXT =
     VkPhysicalDeviceBufferDeviceAddressFeaturesEXT

-- | > typedef struct VkPhysicalDeviceBufferDeviceAddressFeatures {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           bufferDeviceAddress;
--   >     VkBool32                           bufferDeviceAddressCaptureReplay;
--   >     VkBool32                           bufferDeviceAddressMultiDevice;
--   > } VkPhysicalDeviceBufferDeviceAddressFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceBufferDeviceAddressFeatures VkPhysicalDeviceBufferDeviceAddressFeatures registry at www.khronos.org>
type VkPhysicalDeviceBufferDeviceAddressFeatures =
     VkStruct VkPhysicalDeviceBufferDeviceAddressFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceBufferDeviceAddressFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceBufferDeviceAddressFeatures
         where
    type StructRep VkPhysicalDeviceBufferDeviceAddressFeatures =
         'StructMeta "VkPhysicalDeviceBufferDeviceAddressFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceBufferDeviceAddressFeatures
           #{size VkPhysicalDeviceBufferDeviceAddressFeatures}
           #{alignment VkPhysicalDeviceBufferDeviceAddressFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceBufferDeviceAddressFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceBufferDeviceAddressFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bufferDeviceAddress" VkBool32 'False 
                                                               #{offset VkPhysicalDeviceBufferDeviceAddressFeatures, bufferDeviceAddress}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bufferDeviceAddressCaptureReplay" VkBool32 'False
                #{offset VkPhysicalDeviceBufferDeviceAddressFeatures, bufferDeviceAddressCaptureReplay}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bufferDeviceAddressMultiDevice" VkBool32 'False
                #{offset VkPhysicalDeviceBufferDeviceAddressFeatures, bufferDeviceAddressMultiDevice}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceBufferDeviceAddressFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           bufferDeviceAddress;
--   >     VkBool32                           bufferDeviceAddressCaptureReplay;
--   >     VkBool32                           bufferDeviceAddressMultiDevice;
--   > } VkPhysicalDeviceBufferDeviceAddressFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceBufferDeviceAddressFeaturesEXT VkPhysicalDeviceBufferDeviceAddressFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceBufferDeviceAddressFeaturesEXT =
     VkStruct VkPhysicalDeviceBufferDeviceAddressFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceBufferDeviceAddressFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceBufferDeviceAddressFeaturesEXT
         where
    type StructRep VkPhysicalDeviceBufferDeviceAddressFeaturesEXT =
         'StructMeta "VkPhysicalDeviceBufferDeviceAddressFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceBufferDeviceAddressFeaturesEXT
           #{size VkPhysicalDeviceBufferDeviceAddressFeaturesEXT}
           #{alignment VkPhysicalDeviceBufferDeviceAddressFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceBufferDeviceAddressFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceBufferDeviceAddressFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bufferDeviceAddress" VkBool32 'False 
                                                               #{offset VkPhysicalDeviceBufferDeviceAddressFeaturesEXT, bufferDeviceAddress}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bufferDeviceAddressCaptureReplay" VkBool32 'False
                #{offset VkPhysicalDeviceBufferDeviceAddressFeaturesEXT, bufferDeviceAddressCaptureReplay}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bufferDeviceAddressMultiDevice" VkBool32 'False
                #{offset VkPhysicalDeviceBufferDeviceAddressFeaturesEXT, bufferDeviceAddressMultiDevice}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceBufferDeviceAddressFeatures`
type VkPhysicalDeviceBufferDeviceAddressFeaturesKHR =
     VkPhysicalDeviceBufferDeviceAddressFeatures

-- | > typedef struct VkPhysicalDeviceCoherentMemoryFeaturesAMD {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           deviceCoherentMemory;
--   > } VkPhysicalDeviceCoherentMemoryFeaturesAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceCoherentMemoryFeaturesAMD VkPhysicalDeviceCoherentMemoryFeaturesAMD registry at www.khronos.org>
type VkPhysicalDeviceCoherentMemoryFeaturesAMD =
     VkStruct VkPhysicalDeviceCoherentMemoryFeaturesAMD' -- ' closing tick for hsc2hs

data VkPhysicalDeviceCoherentMemoryFeaturesAMD' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceCoherentMemoryFeaturesAMD
         where
    type StructRep VkPhysicalDeviceCoherentMemoryFeaturesAMD =
         'StructMeta "VkPhysicalDeviceCoherentMemoryFeaturesAMD" -- ' closing tick for hsc2hs
           VkPhysicalDeviceCoherentMemoryFeaturesAMD
           #{size VkPhysicalDeviceCoherentMemoryFeaturesAMD}
           #{alignment VkPhysicalDeviceCoherentMemoryFeaturesAMD}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceCoherentMemoryFeaturesAMD, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceCoherentMemoryFeaturesAMD, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceCoherentMemory" VkBool32 'False 
                                                                #{offset VkPhysicalDeviceCoherentMemoryFeaturesAMD, deviceCoherentMemory}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceComputeShaderDerivativesFeaturesNV {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         computeDerivativeGroupQuads;
--   >     VkBool32                         computeDerivativeGroupLinear;
--   > } VkPhysicalDeviceComputeShaderDerivativesFeaturesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceComputeShaderDerivativesFeaturesNV VkPhysicalDeviceComputeShaderDerivativesFeaturesNV registry at www.khronos.org>
type VkPhysicalDeviceComputeShaderDerivativesFeaturesNV =
     VkStruct VkPhysicalDeviceComputeShaderDerivativesFeaturesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceComputeShaderDerivativesFeaturesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceComputeShaderDerivativesFeaturesNV
         where
    type StructRep VkPhysicalDeviceComputeShaderDerivativesFeaturesNV =
         'StructMeta "VkPhysicalDeviceComputeShaderDerivativesFeaturesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceComputeShaderDerivativesFeaturesNV
           #{size VkPhysicalDeviceComputeShaderDerivativesFeaturesNV}
           #{alignment VkPhysicalDeviceComputeShaderDerivativesFeaturesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceComputeShaderDerivativesFeaturesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceComputeShaderDerivativesFeaturesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "computeDerivativeGroupQuads" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceComputeShaderDerivativesFeaturesNV, computeDerivativeGroupQuads}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "computeDerivativeGroupLinear" VkBool32 'False
                #{offset VkPhysicalDeviceComputeShaderDerivativesFeaturesNV, computeDerivativeGroupLinear}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceConditionalRenderingFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           conditionalRendering;
--   >     VkBool32                           inheritedConditionalRendering;
--   > } VkPhysicalDeviceConditionalRenderingFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceConditionalRenderingFeaturesEXT VkPhysicalDeviceConditionalRenderingFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceConditionalRenderingFeaturesEXT =
     VkStruct VkPhysicalDeviceConditionalRenderingFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceConditionalRenderingFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceConditionalRenderingFeaturesEXT
         where
    type StructRep VkPhysicalDeviceConditionalRenderingFeaturesEXT =
         'StructMeta "VkPhysicalDeviceConditionalRenderingFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceConditionalRenderingFeaturesEXT
           #{size VkPhysicalDeviceConditionalRenderingFeaturesEXT}
           #{alignment VkPhysicalDeviceConditionalRenderingFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceConditionalRenderingFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceConditionalRenderingFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "conditionalRendering" VkBool32 'False 
                                                                #{offset VkPhysicalDeviceConditionalRenderingFeaturesEXT, conditionalRendering}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "inheritedConditionalRendering" VkBool32 'False
                #{offset VkPhysicalDeviceConditionalRenderingFeaturesEXT, inheritedConditionalRendering}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceConservativeRasterizationPropertiesEXT VkPhysicalDeviceConservativeRasterizationPropertiesEXT registry at www.khronos.org>
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
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceCooperativeMatrixFeaturesNV {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     VkBool32                            cooperativeMatrix;
--   >     VkBool32                            cooperativeMatrixRobustBufferAccess;
--   > } VkPhysicalDeviceCooperativeMatrixFeaturesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceCooperativeMatrixFeaturesNV VkPhysicalDeviceCooperativeMatrixFeaturesNV registry at www.khronos.org>
type VkPhysicalDeviceCooperativeMatrixFeaturesNV =
     VkStruct VkPhysicalDeviceCooperativeMatrixFeaturesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceCooperativeMatrixFeaturesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceCooperativeMatrixFeaturesNV
         where
    type StructRep VkPhysicalDeviceCooperativeMatrixFeaturesNV =
         'StructMeta "VkPhysicalDeviceCooperativeMatrixFeaturesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceCooperativeMatrixFeaturesNV
           #{size VkPhysicalDeviceCooperativeMatrixFeaturesNV}
           #{alignment VkPhysicalDeviceCooperativeMatrixFeaturesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceCooperativeMatrixFeaturesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceCooperativeMatrixFeaturesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "cooperativeMatrix" VkBool32 'False 
                                                             #{offset VkPhysicalDeviceCooperativeMatrixFeaturesNV, cooperativeMatrix}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "cooperativeMatrixRobustBufferAccess" VkBool32 'False
                #{offset VkPhysicalDeviceCooperativeMatrixFeaturesNV, cooperativeMatrixRobustBufferAccess}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceCooperativeMatrixPropertiesNV {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     VkShaderStageFlags                  cooperativeMatrixSupportedStages;
--   > } VkPhysicalDeviceCooperativeMatrixPropertiesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceCooperativeMatrixPropertiesNV VkPhysicalDeviceCooperativeMatrixPropertiesNV registry at www.khronos.org>
type VkPhysicalDeviceCooperativeMatrixPropertiesNV =
     VkStruct VkPhysicalDeviceCooperativeMatrixPropertiesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceCooperativeMatrixPropertiesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceCooperativeMatrixPropertiesNV
         where
    type StructRep VkPhysicalDeviceCooperativeMatrixPropertiesNV =
         'StructMeta "VkPhysicalDeviceCooperativeMatrixPropertiesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceCooperativeMatrixPropertiesNV
           #{size VkPhysicalDeviceCooperativeMatrixPropertiesNV}
           #{alignment VkPhysicalDeviceCooperativeMatrixPropertiesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceCooperativeMatrixPropertiesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceCooperativeMatrixPropertiesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "cooperativeMatrixSupportedStages" VkShaderStageFlags -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceCooperativeMatrixPropertiesNV, cooperativeMatrixSupportedStages}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceCornerSampledImageFeaturesNV {
--   >     VkStructureType sType;
--   >     void*                              pNext;
--   >     VkBool32                           cornerSampledImage;
--   > } VkPhysicalDeviceCornerSampledImageFeaturesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceCornerSampledImageFeaturesNV VkPhysicalDeviceCornerSampledImageFeaturesNV registry at www.khronos.org>
type VkPhysicalDeviceCornerSampledImageFeaturesNV =
     VkStruct VkPhysicalDeviceCornerSampledImageFeaturesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceCornerSampledImageFeaturesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceCornerSampledImageFeaturesNV
         where
    type StructRep VkPhysicalDeviceCornerSampledImageFeaturesNV =
         'StructMeta "VkPhysicalDeviceCornerSampledImageFeaturesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceCornerSampledImageFeaturesNV
           #{size VkPhysicalDeviceCornerSampledImageFeaturesNV}
           #{alignment VkPhysicalDeviceCornerSampledImageFeaturesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceCornerSampledImageFeaturesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceCornerSampledImageFeaturesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "cornerSampledImage" VkBool32 'False 
                                                              #{offset VkPhysicalDeviceCornerSampledImageFeaturesNV, cornerSampledImage}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceCoverageReductionModeFeaturesNV {
--   >     VkStructureTypesType;
--   >     void*    pNext;
--   >     VkBool32                       coverageReductionMode;
--   > } VkPhysicalDeviceCoverageReductionModeFeaturesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceCoverageReductionModeFeaturesNV VkPhysicalDeviceCoverageReductionModeFeaturesNV registry at www.khronos.org>
type VkPhysicalDeviceCoverageReductionModeFeaturesNV =
     VkStruct VkPhysicalDeviceCoverageReductionModeFeaturesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceCoverageReductionModeFeaturesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceCoverageReductionModeFeaturesNV
         where
    type StructRep VkPhysicalDeviceCoverageReductionModeFeaturesNV =
         'StructMeta "VkPhysicalDeviceCoverageReductionModeFeaturesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceCoverageReductionModeFeaturesNV
           #{size VkPhysicalDeviceCoverageReductionModeFeaturesNV}
           #{alignment VkPhysicalDeviceCoverageReductionModeFeaturesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceCoverageReductionModeFeaturesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceCoverageReductionModeFeaturesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "coverageReductionMode" VkBool32 'False 
                                                                 #{offset VkPhysicalDeviceCoverageReductionModeFeaturesNV, coverageReductionMode}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceCustomBorderColorFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           customBorderColors;
--   >     VkBool32                           customBorderColorWithoutFormat;
--   > } VkPhysicalDeviceCustomBorderColorFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceCustomBorderColorFeaturesEXT VkPhysicalDeviceCustomBorderColorFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceCustomBorderColorFeaturesEXT =
     VkStruct VkPhysicalDeviceCustomBorderColorFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceCustomBorderColorFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceCustomBorderColorFeaturesEXT
         where
    type StructRep VkPhysicalDeviceCustomBorderColorFeaturesEXT =
         'StructMeta "VkPhysicalDeviceCustomBorderColorFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceCustomBorderColorFeaturesEXT
           #{size VkPhysicalDeviceCustomBorderColorFeaturesEXT}
           #{alignment VkPhysicalDeviceCustomBorderColorFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceCustomBorderColorFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceCustomBorderColorFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "customBorderColors" VkBool32 'False 
                                                              #{offset VkPhysicalDeviceCustomBorderColorFeaturesEXT, customBorderColors}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "customBorderColorWithoutFormat" VkBool32 'False
                #{offset VkPhysicalDeviceCustomBorderColorFeaturesEXT, customBorderColorWithoutFormat}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceCustomBorderColorPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                                                                   pNext;
--   >     uint32_t                                                                                      maxCustomBorderColorSamplers;
--   > } VkPhysicalDeviceCustomBorderColorPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceCustomBorderColorPropertiesEXT VkPhysicalDeviceCustomBorderColorPropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceCustomBorderColorPropertiesEXT =
     VkStruct VkPhysicalDeviceCustomBorderColorPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceCustomBorderColorPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceCustomBorderColorPropertiesEXT
         where
    type StructRep VkPhysicalDeviceCustomBorderColorPropertiesEXT =
         'StructMeta "VkPhysicalDeviceCustomBorderColorPropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceCustomBorderColorPropertiesEXT
           #{size VkPhysicalDeviceCustomBorderColorPropertiesEXT}
           #{alignment VkPhysicalDeviceCustomBorderColorPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceCustomBorderColorPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceCustomBorderColorPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxCustomBorderColorSamplers" Word32 'False 
                                                                      #{offset VkPhysicalDeviceCustomBorderColorPropertiesEXT, maxCustomBorderColorSamplers}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         dedicatedAllocationImageAliasing;
--   > } VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV registry at www.khronos.org>
type VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV =
     VkStruct
       VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
         where
    type StructRep
           VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
         =
         'StructMeta -- ' closing tick for hsc2hs
           "VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV"
           VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
           #{size VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV}
           #{alignment VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "dedicatedAllocationImageAliasing" VkBool32 'False
                #{offset VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV, dedicatedAllocationImageAliasing}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceDepthClipEnableFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     VkBool32               depthClipEnable;
--   > } VkPhysicalDeviceDepthClipEnableFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceDepthClipEnableFeaturesEXT VkPhysicalDeviceDepthClipEnableFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceDepthClipEnableFeaturesEXT =
     VkStruct VkPhysicalDeviceDepthClipEnableFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceDepthClipEnableFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceDepthClipEnableFeaturesEXT
         where
    type StructRep VkPhysicalDeviceDepthClipEnableFeaturesEXT =
         'StructMeta "VkPhysicalDeviceDepthClipEnableFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceDepthClipEnableFeaturesEXT
           #{size VkPhysicalDeviceDepthClipEnableFeaturesEXT}
           #{alignment VkPhysicalDeviceDepthClipEnableFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceDepthClipEnableFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceDepthClipEnableFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "depthClipEnable" VkBool32 'False 
                                                           #{offset VkPhysicalDeviceDepthClipEnableFeaturesEXT, depthClipEnable}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceDepthStencilResolveProperties {
--   >     VkStructureType sType;
--   >     void*                                pNext;
--   >     VkResolveModeFlags                   supportedDepthResolveModes;
--   >     VkResolveModeFlags                   supportedStencilResolveModes;
--   >     VkBool32                             independentResolveNone;
--   >     VkBool32                             independentResolve;
--   > } VkPhysicalDeviceDepthStencilResolveProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceDepthStencilResolveProperties VkPhysicalDeviceDepthStencilResolveProperties registry at www.khronos.org>
type VkPhysicalDeviceDepthStencilResolveProperties =
     VkStruct VkPhysicalDeviceDepthStencilResolveProperties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceDepthStencilResolveProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceDepthStencilResolveProperties
         where
    type StructRep VkPhysicalDeviceDepthStencilResolveProperties =
         'StructMeta "VkPhysicalDeviceDepthStencilResolveProperties" -- ' closing tick for hsc2hs
           VkPhysicalDeviceDepthStencilResolveProperties
           #{size VkPhysicalDeviceDepthStencilResolveProperties}
           #{alignment VkPhysicalDeviceDepthStencilResolveProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceDepthStencilResolveProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceDepthStencilResolveProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "supportedDepthResolveModes" VkResolveModeFlags 'False
                #{offset VkPhysicalDeviceDepthStencilResolveProperties, supportedDepthResolveModes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "supportedStencilResolveModes" VkResolveModeFlags -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDepthStencilResolveProperties, supportedStencilResolveModes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "independentResolveNone" VkBool32 'False 
                                                                  #{offset VkPhysicalDeviceDepthStencilResolveProperties, independentResolveNone}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "independentResolve" VkBool32 'False 
                                                              #{offset VkPhysicalDeviceDepthStencilResolveProperties, independentResolve}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceDepthStencilResolveProperties`
type VkPhysicalDeviceDepthStencilResolvePropertiesKHR =
     VkPhysicalDeviceDepthStencilResolveProperties

-- | > typedef struct VkPhysicalDeviceDescriptorIndexingFeatures {
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
--   > } VkPhysicalDeviceDescriptorIndexingFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceDescriptorIndexingFeatures VkPhysicalDeviceDescriptorIndexingFeatures registry at www.khronos.org>
type VkPhysicalDeviceDescriptorIndexingFeatures =
     VkStruct VkPhysicalDeviceDescriptorIndexingFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceDescriptorIndexingFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceDescriptorIndexingFeatures
         where
    type StructRep VkPhysicalDeviceDescriptorIndexingFeatures =
         'StructMeta "VkPhysicalDeviceDescriptorIndexingFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceDescriptorIndexingFeatures
           #{size VkPhysicalDeviceDescriptorIndexingFeatures}
           #{alignment VkPhysicalDeviceDescriptorIndexingFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceDescriptorIndexingFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceDescriptorIndexingFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderInputAttachmentArrayDynamicIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, shaderInputAttachmentArrayDynamicIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderUniformTexelBufferArrayDynamicIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, shaderUniformTexelBufferArrayDynamicIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageTexelBufferArrayDynamicIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, shaderStorageTexelBufferArrayDynamicIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderUniformBufferArrayNonUniformIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, shaderUniformBufferArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSampledImageArrayNonUniformIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, shaderSampledImageArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageBufferArrayNonUniformIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, shaderStorageBufferArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageImageArrayNonUniformIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, shaderStorageImageArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderInputAttachmentArrayNonUniformIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, shaderInputAttachmentArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderUniformTexelBufferArrayNonUniformIndexing" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, shaderUniformTexelBufferArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageTexelBufferArrayNonUniformIndexing" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, shaderStorageTexelBufferArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingUniformBufferUpdateAfterBind" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, descriptorBindingUniformBufferUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingSampledImageUpdateAfterBind" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, descriptorBindingSampledImageUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingStorageImageUpdateAfterBind" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, descriptorBindingStorageImageUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingStorageBufferUpdateAfterBind" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, descriptorBindingStorageBufferUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingUniformTexelBufferUpdateAfterBind" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, descriptorBindingUniformTexelBufferUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingStorageTexelBufferUpdateAfterBind" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, descriptorBindingStorageTexelBufferUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingUpdateUnusedWhilePending" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, descriptorBindingUpdateUnusedWhilePending}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingPartiallyBound" VkBool32 'False
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, descriptorBindingPartiallyBound}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingVariableDescriptorCount" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingFeatures, descriptorBindingVariableDescriptorCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "runtimeDescriptorArray" VkBool32 'False 
                                                                  #{offset VkPhysicalDeviceDescriptorIndexingFeatures, runtimeDescriptorArray}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceDescriptorIndexingFeatures`
type VkPhysicalDeviceDescriptorIndexingFeaturesEXT =
     VkPhysicalDeviceDescriptorIndexingFeatures

-- | > typedef struct VkPhysicalDeviceDescriptorIndexingProperties {
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
--   > } VkPhysicalDeviceDescriptorIndexingProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceDescriptorIndexingProperties VkPhysicalDeviceDescriptorIndexingProperties registry at www.khronos.org>
type VkPhysicalDeviceDescriptorIndexingProperties =
     VkStruct VkPhysicalDeviceDescriptorIndexingProperties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceDescriptorIndexingProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceDescriptorIndexingProperties
         where
    type StructRep VkPhysicalDeviceDescriptorIndexingProperties =
         'StructMeta "VkPhysicalDeviceDescriptorIndexingProperties" -- ' closing tick for hsc2hs
           VkPhysicalDeviceDescriptorIndexingProperties
           #{size VkPhysicalDeviceDescriptorIndexingProperties}
           #{alignment VkPhysicalDeviceDescriptorIndexingProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceDescriptorIndexingProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceDescriptorIndexingProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxUpdateAfterBindDescriptorsInAllPools" Word32 'False
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, maxUpdateAfterBindDescriptorsInAllPools}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderUniformBufferArrayNonUniformIndexingNative" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, shaderUniformBufferArrayNonUniformIndexingNative}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSampledImageArrayNonUniformIndexingNative" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, shaderSampledImageArrayNonUniformIndexingNative}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageBufferArrayNonUniformIndexingNative" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, shaderStorageBufferArrayNonUniformIndexingNative}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageImageArrayNonUniformIndexingNative" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, shaderStorageImageArrayNonUniformIndexingNative}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderInputAttachmentArrayNonUniformIndexingNative" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, shaderInputAttachmentArrayNonUniformIndexingNative}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "robustBufferAccessUpdateAfterBind" VkBool32 'False
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, robustBufferAccessUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "quadDivergentImplicitLod" VkBool32 'False 
                                                                    #{offset VkPhysicalDeviceDescriptorIndexingProperties, quadDivergentImplicitLod}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindSamplers" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, maxPerStageDescriptorUpdateAfterBindSamplers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindUniformBuffers" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, maxPerStageDescriptorUpdateAfterBindUniformBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindStorageBuffers" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, maxPerStageDescriptorUpdateAfterBindStorageBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindSampledImages" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, maxPerStageDescriptorUpdateAfterBindSampledImages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindStorageImages" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, maxPerStageDescriptorUpdateAfterBindStorageImages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindInputAttachments" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, maxPerStageDescriptorUpdateAfterBindInputAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageUpdateAfterBindResources" Word32 'False
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, maxPerStageUpdateAfterBindResources}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindSamplers" Word32 'False
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, maxDescriptorSetUpdateAfterBindSamplers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindUniformBuffers" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, maxDescriptorSetUpdateAfterBindUniformBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, maxDescriptorSetUpdateAfterBindUniformBuffersDynamic}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindStorageBuffers" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, maxDescriptorSetUpdateAfterBindStorageBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, maxDescriptorSetUpdateAfterBindStorageBuffersDynamic}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindSampledImages" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, maxDescriptorSetUpdateAfterBindSampledImages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindStorageImages" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, maxDescriptorSetUpdateAfterBindStorageImages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindInputAttachments" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDescriptorIndexingProperties, maxDescriptorSetUpdateAfterBindInputAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceDescriptorIndexingProperties`
type VkPhysicalDeviceDescriptorIndexingPropertiesEXT =
     VkPhysicalDeviceDescriptorIndexingProperties

-- | > typedef struct VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV {
--   >     VkStructureTypesType;
--   >     void*    pNext;
--   >     VkBool32                       deviceGeneratedCommands;
--   > } VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV registry at www.khronos.org>
type VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV =
     VkStruct VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV
         where
    type StructRep VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV =
         'StructMeta "VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV
           #{size VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV}
           #{alignment VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceGeneratedCommands" VkBool32 'False 
                                                                   #{offset VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV, deviceGeneratedCommands}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV {
--   >     VkStructureType sType;
--   >     void*    pNext;
--   >     uint32_t         maxGraphicsShaderGroupCount;
--   >     uint32_t         maxIndirectSequenceCount;
--   >     uint32_t         maxIndirectCommandsTokenCount;
--   >     uint32_t         maxIndirectCommandsStreamCount;
--   >     uint32_t         maxIndirectCommandsTokenOffset;
--   >     uint32_t         maxIndirectCommandsStreamStride;
--   >     uint32_t         minSequencesCountBufferOffsetAlignment;
--   >     uint32_t         minSequencesIndexBufferOffsetAlignment;
--   >     uint32_t         minIndirectCommandsBufferOffsetAlignment;
--   > } VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV registry at www.khronos.org>
type VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV =
     VkStruct VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV
         where
    type StructRep VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV
         =
         'StructMeta "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV
           #{size VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV}
           #{alignment VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxGraphicsShaderGroupCount" Word32 'False 
                                                                     #{offset VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV, maxGraphicsShaderGroupCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxIndirectSequenceCount" Word32 'False 
                                                                  #{offset VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV, maxIndirectSequenceCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxIndirectCommandsTokenCount" Word32 'False 
                                                                       #{offset VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV, maxIndirectCommandsTokenCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxIndirectCommandsStreamCount" Word32 'False
                #{offset VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV, maxIndirectCommandsStreamCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxIndirectCommandsTokenOffset" Word32 'False
                #{offset VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV, maxIndirectCommandsTokenOffset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxIndirectCommandsStreamStride" Word32 'False
                #{offset VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV, maxIndirectCommandsStreamStride}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minSequencesCountBufferOffsetAlignment" Word32 'False
                #{offset VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV, minSequencesCountBufferOffsetAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minSequencesIndexBufferOffsetAlignment" Word32 'False
                #{offset VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV, minSequencesIndexBufferOffsetAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minIndirectCommandsBufferOffsetAlignment" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV, minIndirectCommandsBufferOffsetAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceDiagnosticsConfigFeaturesNV {
--   >     VkStructureTypesType;
--   >     void*    pNext;
--   >     VkBool32                       diagnosticsConfig;
--   > } VkPhysicalDeviceDiagnosticsConfigFeaturesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceDiagnosticsConfigFeaturesNV VkPhysicalDeviceDiagnosticsConfigFeaturesNV registry at www.khronos.org>
type VkPhysicalDeviceDiagnosticsConfigFeaturesNV =
     VkStruct VkPhysicalDeviceDiagnosticsConfigFeaturesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceDiagnosticsConfigFeaturesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceDiagnosticsConfigFeaturesNV
         where
    type StructRep VkPhysicalDeviceDiagnosticsConfigFeaturesNV =
         'StructMeta "VkPhysicalDeviceDiagnosticsConfigFeaturesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceDiagnosticsConfigFeaturesNV
           #{size VkPhysicalDeviceDiagnosticsConfigFeaturesNV}
           #{alignment VkPhysicalDeviceDiagnosticsConfigFeaturesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceDiagnosticsConfigFeaturesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceDiagnosticsConfigFeaturesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "diagnosticsConfig" VkBool32 'False 
                                                             #{offset VkPhysicalDeviceDiagnosticsConfigFeaturesNV, diagnosticsConfig}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceDiscardRectanglePropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     uint32_t               maxDiscardRectangles;
--   > } VkPhysicalDeviceDiscardRectanglePropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceDiscardRectanglePropertiesEXT VkPhysicalDeviceDiscardRectanglePropertiesEXT registry at www.khronos.org>
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
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceDriverProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkDriverId                       driverID;
--   >     char                             driverName[VK_MAX_DRIVER_NAME_SIZE];
--   >     char                             driverInfo[VK_MAX_DRIVER_INFO_SIZE];
--   >     VkConformanceVersion             conformanceVersion;
--   > } VkPhysicalDeviceDriverProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceDriverProperties VkPhysicalDeviceDriverProperties registry at www.khronos.org>
type VkPhysicalDeviceDriverProperties =
     VkStruct VkPhysicalDeviceDriverProperties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceDriverProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceDriverProperties where
    type StructRep VkPhysicalDeviceDriverProperties =
         'StructMeta "VkPhysicalDeviceDriverProperties" -- ' closing tick for hsc2hs
           VkPhysicalDeviceDriverProperties
           #{size VkPhysicalDeviceDriverProperties}
           #{alignment VkPhysicalDeviceDriverProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceDriverProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceDriverProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "driverID" VkDriverId 'False 
                                                      #{offset VkPhysicalDeviceDriverProperties, driverID}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "driverName" CChar 'False 
                                                   #{offset VkPhysicalDeviceDriverProperties, driverName}
                VK_MAX_DRIVER_NAME_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "driverInfo" CChar 'False 
                                                   #{offset VkPhysicalDeviceDriverProperties, driverInfo}
                VK_MAX_DRIVER_INFO_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "conformanceVersion" VkConformanceVersion 'False
                #{offset VkPhysicalDeviceDriverProperties, conformanceVersion}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceDriverProperties`
type VkPhysicalDeviceDriverPropertiesKHR =
     VkPhysicalDeviceDriverProperties

-- | > typedef struct VkPhysicalDeviceExclusiveScissorFeaturesNV {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         exclusiveScissor;
--   > } VkPhysicalDeviceExclusiveScissorFeaturesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceExclusiveScissorFeaturesNV VkPhysicalDeviceExclusiveScissorFeaturesNV registry at www.khronos.org>
type VkPhysicalDeviceExclusiveScissorFeaturesNV =
     VkStruct VkPhysicalDeviceExclusiveScissorFeaturesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceExclusiveScissorFeaturesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceExclusiveScissorFeaturesNV
         where
    type StructRep VkPhysicalDeviceExclusiveScissorFeaturesNV =
         'StructMeta "VkPhysicalDeviceExclusiveScissorFeaturesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceExclusiveScissorFeaturesNV
           #{size VkPhysicalDeviceExclusiveScissorFeaturesNV}
           #{alignment VkPhysicalDeviceExclusiveScissorFeaturesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceExclusiveScissorFeaturesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceExclusiveScissorFeaturesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "exclusiveScissor" VkBool32 'False 
                                                            #{offset VkPhysicalDeviceExclusiveScissorFeaturesNV, exclusiveScissor}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceExtendedDynamicStateFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           extendedDynamicState;
--   > } VkPhysicalDeviceExtendedDynamicStateFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceExtendedDynamicStateFeaturesEXT VkPhysicalDeviceExtendedDynamicStateFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceExtendedDynamicStateFeaturesEXT =
     VkStruct VkPhysicalDeviceExtendedDynamicStateFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceExtendedDynamicStateFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceExtendedDynamicStateFeaturesEXT
         where
    type StructRep VkPhysicalDeviceExtendedDynamicStateFeaturesEXT =
         'StructMeta "VkPhysicalDeviceExtendedDynamicStateFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceExtendedDynamicStateFeaturesEXT
           #{size VkPhysicalDeviceExtendedDynamicStateFeaturesEXT}
           #{alignment VkPhysicalDeviceExtendedDynamicStateFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceExtendedDynamicStateFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceExtendedDynamicStateFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "extendedDynamicState" VkBool32 'False 
                                                                #{offset VkPhysicalDeviceExtendedDynamicStateFeaturesEXT, extendedDynamicState}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceExternalBufferInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBufferCreateFlags flags;
--   >     VkBufferUsageFlags               usage;
--   >     VkExternalMemoryHandleTypeFlagBits handleType;
--   > } VkPhysicalDeviceExternalBufferInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceExternalBufferInfo VkPhysicalDeviceExternalBufferInfo registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceExternalFenceInfo VkPhysicalDeviceExternalFenceInfo registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceExternalImageFormatInfo VkPhysicalDeviceExternalImageFormatInfo registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceExternalMemoryHostPropertiesEXT VkPhysicalDeviceExternalMemoryHostPropertiesEXT registry at www.khronos.org>
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
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceExternalSemaphoreInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalSemaphoreHandleTypeFlagBits handleType;
--   > } VkPhysicalDeviceExternalSemaphoreInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceExternalSemaphoreInfo VkPhysicalDeviceExternalSemaphoreInfo registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceFeatures2 VkPhysicalDeviceFeatures2 registry at www.khronos.org>
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

-- | Alias for `VkPhysicalDeviceShaderFloat16Int8Features`
type VkPhysicalDeviceFloat16Int8FeaturesKHR =
     VkPhysicalDeviceShaderFloat16Int8Features

-- | > typedef struct VkPhysicalDeviceFloatControlsProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkShaderFloatControlsIndependence denormBehaviorIndependence;
--   >     VkShaderFloatControlsIndependence roundingModeIndependence;
--   >     VkBool32                         shaderSignedZeroInfNanPreserveFloat16;
--   >     VkBool32                         shaderSignedZeroInfNanPreserveFloat32;
--   >     VkBool32                         shaderSignedZeroInfNanPreserveFloat64;
--   >     VkBool32                         shaderDenormPreserveFloat16;
--   >     VkBool32                         shaderDenormPreserveFloat32;
--   >     VkBool32                         shaderDenormPreserveFloat64;
--   >     VkBool32                         shaderDenormFlushToZeroFloat16;
--   >     VkBool32                         shaderDenormFlushToZeroFloat32;
--   >     VkBool32                         shaderDenormFlushToZeroFloat64;
--   >     VkBool32                         shaderRoundingModeRTEFloat16;
--   >     VkBool32                         shaderRoundingModeRTEFloat32;
--   >     VkBool32                         shaderRoundingModeRTEFloat64;
--   >     VkBool32                         shaderRoundingModeRTZFloat16;
--   >     VkBool32                         shaderRoundingModeRTZFloat32;
--   >     VkBool32                         shaderRoundingModeRTZFloat64;
--   > } VkPhysicalDeviceFloatControlsProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceFloatControlsProperties VkPhysicalDeviceFloatControlsProperties registry at www.khronos.org>
type VkPhysicalDeviceFloatControlsProperties =
     VkStruct VkPhysicalDeviceFloatControlsProperties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceFloatControlsProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceFloatControlsProperties
         where
    type StructRep VkPhysicalDeviceFloatControlsProperties =
         'StructMeta "VkPhysicalDeviceFloatControlsProperties" -- ' closing tick for hsc2hs
           VkPhysicalDeviceFloatControlsProperties
           #{size VkPhysicalDeviceFloatControlsProperties}
           #{alignment VkPhysicalDeviceFloatControlsProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceFloatControlsProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceFloatControlsProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "denormBehaviorIndependence" -- ' closing tick for hsc2hs
                VkShaderFloatControlsIndependence
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceFloatControlsProperties, denormBehaviorIndependence}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "roundingModeIndependence" -- ' closing tick for hsc2hs
                VkShaderFloatControlsIndependence
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceFloatControlsProperties, roundingModeIndependence}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSignedZeroInfNanPreserveFloat16" VkBool32 'False
                #{offset VkPhysicalDeviceFloatControlsProperties, shaderSignedZeroInfNanPreserveFloat16}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSignedZeroInfNanPreserveFloat32" VkBool32 'False
                #{offset VkPhysicalDeviceFloatControlsProperties, shaderSignedZeroInfNanPreserveFloat32}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSignedZeroInfNanPreserveFloat64" VkBool32 'False
                #{offset VkPhysicalDeviceFloatControlsProperties, shaderSignedZeroInfNanPreserveFloat64}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDenormPreserveFloat16" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceFloatControlsProperties, shaderDenormPreserveFloat16}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDenormPreserveFloat32" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceFloatControlsProperties, shaderDenormPreserveFloat32}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDenormPreserveFloat64" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceFloatControlsProperties, shaderDenormPreserveFloat64}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDenormFlushToZeroFloat16" VkBool32 'False
                #{offset VkPhysicalDeviceFloatControlsProperties, shaderDenormFlushToZeroFloat16}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDenormFlushToZeroFloat32" VkBool32 'False
                #{offset VkPhysicalDeviceFloatControlsProperties, shaderDenormFlushToZeroFloat32}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDenormFlushToZeroFloat64" VkBool32 'False
                #{offset VkPhysicalDeviceFloatControlsProperties, shaderDenormFlushToZeroFloat64}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderRoundingModeRTEFloat16" VkBool32 'False
                #{offset VkPhysicalDeviceFloatControlsProperties, shaderRoundingModeRTEFloat16}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderRoundingModeRTEFloat32" VkBool32 'False
                #{offset VkPhysicalDeviceFloatControlsProperties, shaderRoundingModeRTEFloat32}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderRoundingModeRTEFloat64" VkBool32 'False
                #{offset VkPhysicalDeviceFloatControlsProperties, shaderRoundingModeRTEFloat64}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderRoundingModeRTZFloat16" VkBool32 'False
                #{offset VkPhysicalDeviceFloatControlsProperties, shaderRoundingModeRTZFloat16}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderRoundingModeRTZFloat32" VkBool32 'False
                #{offset VkPhysicalDeviceFloatControlsProperties, shaderRoundingModeRTZFloat32}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderRoundingModeRTZFloat64" VkBool32 'False
                #{offset VkPhysicalDeviceFloatControlsProperties, shaderRoundingModeRTZFloat64}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceFloatControlsProperties`
type VkPhysicalDeviceFloatControlsPropertiesKHR =
     VkPhysicalDeviceFloatControlsProperties

-- | > typedef struct VkPhysicalDeviceFragmentDensityMap2FeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         fragmentDensityMapDeferred;
--   > } VkPhysicalDeviceFragmentDensityMap2FeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceFragmentDensityMap2FeaturesEXT VkPhysicalDeviceFragmentDensityMap2FeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceFragmentDensityMap2FeaturesEXT =
     VkStruct VkPhysicalDeviceFragmentDensityMap2FeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceFragmentDensityMap2FeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceFragmentDensityMap2FeaturesEXT
         where
    type StructRep VkPhysicalDeviceFragmentDensityMap2FeaturesEXT =
         'StructMeta "VkPhysicalDeviceFragmentDensityMap2FeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceFragmentDensityMap2FeaturesEXT
           #{size VkPhysicalDeviceFragmentDensityMap2FeaturesEXT}
           #{alignment VkPhysicalDeviceFragmentDensityMap2FeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceFragmentDensityMap2FeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceFragmentDensityMap2FeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fragmentDensityMapDeferred" VkBool32 'False 
                                                                      #{offset VkPhysicalDeviceFragmentDensityMap2FeaturesEXT, fragmentDensityMapDeferred}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceFragmentDensityMap2PropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                          pNext;
--   >     VkBool32                       subsampledLoads;
--   >     VkBool32                       subsampledCoarseReconstructionEarlyAccess;
--   >     uint32_t                       maxSubsampledArrayLayers;
--   >     uint32_t                       maxDescriptorSetSubsampledSamplers;
--   > } VkPhysicalDeviceFragmentDensityMap2PropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceFragmentDensityMap2PropertiesEXT VkPhysicalDeviceFragmentDensityMap2PropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceFragmentDensityMap2PropertiesEXT =
     VkStruct VkPhysicalDeviceFragmentDensityMap2PropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceFragmentDensityMap2PropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceFragmentDensityMap2PropertiesEXT
         where
    type StructRep VkPhysicalDeviceFragmentDensityMap2PropertiesEXT =
         'StructMeta "VkPhysicalDeviceFragmentDensityMap2PropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceFragmentDensityMap2PropertiesEXT
           #{size VkPhysicalDeviceFragmentDensityMap2PropertiesEXT}
           #{alignment VkPhysicalDeviceFragmentDensityMap2PropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceFragmentDensityMap2PropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceFragmentDensityMap2PropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subsampledLoads" VkBool32 'False 
                                                           #{offset VkPhysicalDeviceFragmentDensityMap2PropertiesEXT, subsampledLoads}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subsampledCoarseReconstructionEarlyAccess" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceFragmentDensityMap2PropertiesEXT, subsampledCoarseReconstructionEarlyAccess}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxSubsampledArrayLayers" Word32 'False 
                                                                  #{offset VkPhysicalDeviceFragmentDensityMap2PropertiesEXT, maxSubsampledArrayLayers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetSubsampledSamplers" Word32 'False
                #{offset VkPhysicalDeviceFragmentDensityMap2PropertiesEXT, maxDescriptorSetSubsampledSamplers}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceFragmentDensityMapFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         fragmentDensityMap;
--   >     VkBool32                         fragmentDensityMapDynamic;
--   >     VkBool32                         fragmentDensityMapNonSubsampledImages;
--   > } VkPhysicalDeviceFragmentDensityMapFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceFragmentDensityMapFeaturesEXT VkPhysicalDeviceFragmentDensityMapFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceFragmentDensityMapFeaturesEXT =
     VkStruct VkPhysicalDeviceFragmentDensityMapFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceFragmentDensityMapFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceFragmentDensityMapFeaturesEXT
         where
    type StructRep VkPhysicalDeviceFragmentDensityMapFeaturesEXT =
         'StructMeta "VkPhysicalDeviceFragmentDensityMapFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceFragmentDensityMapFeaturesEXT
           #{size VkPhysicalDeviceFragmentDensityMapFeaturesEXT}
           #{alignment VkPhysicalDeviceFragmentDensityMapFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceFragmentDensityMapFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceFragmentDensityMapFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fragmentDensityMap" VkBool32 'False 
                                                              #{offset VkPhysicalDeviceFragmentDensityMapFeaturesEXT, fragmentDensityMap}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fragmentDensityMapDynamic" VkBool32 'False 
                                                                     #{offset VkPhysicalDeviceFragmentDensityMapFeaturesEXT, fragmentDensityMapDynamic}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fragmentDensityMapNonSubsampledImages" VkBool32 'False
                #{offset VkPhysicalDeviceFragmentDensityMapFeaturesEXT, fragmentDensityMapNonSubsampledImages}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceFragmentDensityMapPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExtent2D                       minFragmentDensityTexelSize;
--   >     VkExtent2D                       maxFragmentDensityTexelSize;
--   >     VkBool32                         fragmentDensityInvocations;
--   > } VkPhysicalDeviceFragmentDensityMapPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceFragmentDensityMapPropertiesEXT VkPhysicalDeviceFragmentDensityMapPropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceFragmentDensityMapPropertiesEXT =
     VkStruct VkPhysicalDeviceFragmentDensityMapPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceFragmentDensityMapPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceFragmentDensityMapPropertiesEXT
         where
    type StructRep VkPhysicalDeviceFragmentDensityMapPropertiesEXT =
         'StructMeta "VkPhysicalDeviceFragmentDensityMapPropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceFragmentDensityMapPropertiesEXT
           #{size VkPhysicalDeviceFragmentDensityMapPropertiesEXT}
           #{alignment VkPhysicalDeviceFragmentDensityMapPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceFragmentDensityMapPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceFragmentDensityMapPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minFragmentDensityTexelSize" VkExtent2D 'False
                #{offset VkPhysicalDeviceFragmentDensityMapPropertiesEXT, minFragmentDensityTexelSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxFragmentDensityTexelSize" VkExtent2D 'False
                #{offset VkPhysicalDeviceFragmentDensityMapPropertiesEXT, maxFragmentDensityTexelSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fragmentDensityInvocations" VkBool32 'False 
                                                                      #{offset VkPhysicalDeviceFragmentDensityMapPropertiesEXT, fragmentDensityInvocations}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         fragmentShaderBarycentric;
--   > } VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV registry at www.khronos.org>
type VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV =
     VkStruct VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV
         where
    type StructRep VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV
         =
         'StructMeta "VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV
           #{size VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV}
           #{alignment VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fragmentShaderBarycentric" VkBool32 'False 
                                                                     #{offset VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV, fragmentShaderBarycentric}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     VkBool32               fragmentShaderSampleInterlock;
--   >     VkBool32               fragmentShaderPixelInterlock;
--   >     VkBool32               fragmentShaderShadingRateInterlock;
--   > } VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT =
     VkStruct VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT
         where
    type StructRep VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT =
         'StructMeta "VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT
           #{size VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT}
           #{alignment VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fragmentShaderSampleInterlock" VkBool32 'False
                #{offset VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT, fragmentShaderSampleInterlock}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fragmentShaderPixelInterlock" VkBool32 'False
                #{offset VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT, fragmentShaderPixelInterlock}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "fragmentShaderShadingRateInterlock" VkBool32 'False
                #{offset VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT, fragmentShaderShadingRateInterlock}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceGroupProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         physicalDeviceCount;
--   >     VkPhysicalDevice                 physicalDevices[VK_MAX_DEVICE_GROUP_SIZE];
--   >     VkBool32                         subsetAllocation;
--   > } VkPhysicalDeviceGroupProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceGroupProperties VkPhysicalDeviceGroupProperties registry at www.khronos.org>
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

-- | > typedef struct VkPhysicalDeviceHostQueryResetFeatures {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           hostQueryReset;
--   > } VkPhysicalDeviceHostQueryResetFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceHostQueryResetFeatures VkPhysicalDeviceHostQueryResetFeatures registry at www.khronos.org>
type VkPhysicalDeviceHostQueryResetFeatures =
     VkStruct VkPhysicalDeviceHostQueryResetFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceHostQueryResetFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceHostQueryResetFeatures where
    type StructRep VkPhysicalDeviceHostQueryResetFeatures =
         'StructMeta "VkPhysicalDeviceHostQueryResetFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceHostQueryResetFeatures
           #{size VkPhysicalDeviceHostQueryResetFeatures}
           #{alignment VkPhysicalDeviceHostQueryResetFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceHostQueryResetFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceHostQueryResetFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "hostQueryReset" VkBool32 'False 
                                                          #{offset VkPhysicalDeviceHostQueryResetFeatures, hostQueryReset}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceHostQueryResetFeatures`
type VkPhysicalDeviceHostQueryResetFeaturesEXT =
     VkPhysicalDeviceHostQueryResetFeatures

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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceIDProperties VkPhysicalDeviceIDProperties registry at www.khronos.org>
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

-- | > typedef struct VkPhysicalDeviceImageDrmFormatModifierInfoEXT {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     uint64_t drmFormatModifier;
--   >     VkSharingMode sharingMode;
--   >     uint32_t queueFamilyIndexCount;
--   >     const uint32_t* pQueueFamilyIndices;
--   > } VkPhysicalDeviceImageDrmFormatModifierInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceImageDrmFormatModifierInfoEXT VkPhysicalDeviceImageDrmFormatModifierInfoEXT registry at www.khronos.org>
type VkPhysicalDeviceImageDrmFormatModifierInfoEXT =
     VkStruct VkPhysicalDeviceImageDrmFormatModifierInfoEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceImageDrmFormatModifierInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceImageDrmFormatModifierInfoEXT
         where
    type StructRep VkPhysicalDeviceImageDrmFormatModifierInfoEXT =
         'StructMeta "VkPhysicalDeviceImageDrmFormatModifierInfoEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceImageDrmFormatModifierInfoEXT
           #{size VkPhysicalDeviceImageDrmFormatModifierInfoEXT}
           #{alignment VkPhysicalDeviceImageDrmFormatModifierInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceImageDrmFormatModifierInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceImageDrmFormatModifierInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "drmFormatModifier" Word64 'False 
                                                           #{offset VkPhysicalDeviceImageDrmFormatModifierInfoEXT, drmFormatModifier}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sharingMode" VkSharingMode 'False 
                                                            #{offset VkPhysicalDeviceImageDrmFormatModifierInfoEXT, sharingMode}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "queueFamilyIndexCount" Word32 'True 
                                                              #{offset VkPhysicalDeviceImageDrmFormatModifierInfoEXT, queueFamilyIndexCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pQueueFamilyIndices" (Ptr Word32) 'False 
                                                                   #{offset VkPhysicalDeviceImageDrmFormatModifierInfoEXT, pQueueFamilyIndices}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceImageFormatInfo2] -- ' closing tick for hsc2hs

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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceImageFormatInfo2 VkPhysicalDeviceImageFormatInfo2 registry at www.khronos.org>
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

-- | > typedef struct VkPhysicalDeviceImageRobustnessFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           robustImageAccess;
--   > } VkPhysicalDeviceImageRobustnessFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceImageRobustnessFeaturesEXT VkPhysicalDeviceImageRobustnessFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceImageRobustnessFeaturesEXT =
     VkStruct VkPhysicalDeviceImageRobustnessFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceImageRobustnessFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceImageRobustnessFeaturesEXT
         where
    type StructRep VkPhysicalDeviceImageRobustnessFeaturesEXT =
         'StructMeta "VkPhysicalDeviceImageRobustnessFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceImageRobustnessFeaturesEXT
           #{size VkPhysicalDeviceImageRobustnessFeaturesEXT}
           #{alignment VkPhysicalDeviceImageRobustnessFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceImageRobustnessFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceImageRobustnessFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "robustImageAccess" VkBool32 'False 
                                                             #{offset VkPhysicalDeviceImageRobustnessFeaturesEXT, robustImageAccess}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceImageViewImageFormatInfoEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkImageViewType                  imageViewType;
--   > } VkPhysicalDeviceImageViewImageFormatInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceImageViewImageFormatInfoEXT VkPhysicalDeviceImageViewImageFormatInfoEXT registry at www.khronos.org>
type VkPhysicalDeviceImageViewImageFormatInfoEXT =
     VkStruct VkPhysicalDeviceImageViewImageFormatInfoEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceImageViewImageFormatInfoEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceImageViewImageFormatInfoEXT
         where
    type StructRep VkPhysicalDeviceImageViewImageFormatInfoEXT =
         'StructMeta "VkPhysicalDeviceImageViewImageFormatInfoEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceImageViewImageFormatInfoEXT
           #{size VkPhysicalDeviceImageViewImageFormatInfoEXT}
           #{alignment VkPhysicalDeviceImageViewImageFormatInfoEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceImageViewImageFormatInfoEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceImageViewImageFormatInfoEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageViewType" VkImageViewType 'False 
                                                                #{offset VkPhysicalDeviceImageViewImageFormatInfoEXT, imageViewType}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceImageFormatInfo2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceImagelessFramebufferFeatures {
--   >     VkStructureType sType;
--   >     void*                                    pNext;
--   >     VkBool32                                 imagelessFramebuffer;
--   > } VkPhysicalDeviceImagelessFramebufferFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceImagelessFramebufferFeatures VkPhysicalDeviceImagelessFramebufferFeatures registry at www.khronos.org>
type VkPhysicalDeviceImagelessFramebufferFeatures =
     VkStruct VkPhysicalDeviceImagelessFramebufferFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceImagelessFramebufferFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceImagelessFramebufferFeatures
         where
    type StructRep VkPhysicalDeviceImagelessFramebufferFeatures =
         'StructMeta "VkPhysicalDeviceImagelessFramebufferFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceImagelessFramebufferFeatures
           #{size VkPhysicalDeviceImagelessFramebufferFeatures}
           #{alignment VkPhysicalDeviceImagelessFramebufferFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceImagelessFramebufferFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceImagelessFramebufferFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imagelessFramebuffer" VkBool32 'False 
                                                                #{offset VkPhysicalDeviceImagelessFramebufferFeatures, imagelessFramebuffer}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceImagelessFramebufferFeatures`
type VkPhysicalDeviceImagelessFramebufferFeaturesKHR =
     VkPhysicalDeviceImagelessFramebufferFeatures

-- | > typedef struct VkPhysicalDeviceIndexTypeUint8FeaturesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           indexTypeUint8;
--   > } VkPhysicalDeviceIndexTypeUint8FeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceIndexTypeUint8FeaturesEXT VkPhysicalDeviceIndexTypeUint8FeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceIndexTypeUint8FeaturesEXT =
     VkStruct VkPhysicalDeviceIndexTypeUint8FeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceIndexTypeUint8FeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceIndexTypeUint8FeaturesEXT
         where
    type StructRep VkPhysicalDeviceIndexTypeUint8FeaturesEXT =
         'StructMeta "VkPhysicalDeviceIndexTypeUint8FeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceIndexTypeUint8FeaturesEXT
           #{size VkPhysicalDeviceIndexTypeUint8FeaturesEXT}
           #{alignment VkPhysicalDeviceIndexTypeUint8FeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceIndexTypeUint8FeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceIndexTypeUint8FeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "indexTypeUint8" VkBool32 'False 
                                                          #{offset VkPhysicalDeviceIndexTypeUint8FeaturesEXT, indexTypeUint8}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceInlineUniformBlockFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     VkBool32               inlineUniformBlock;
--   >     VkBool32               descriptorBindingInlineUniformBlockUpdateAfterBind;
--   > } VkPhysicalDeviceInlineUniformBlockFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceInlineUniformBlockFeaturesEXT VkPhysicalDeviceInlineUniformBlockFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceInlineUniformBlockFeaturesEXT =
     VkStruct VkPhysicalDeviceInlineUniformBlockFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceInlineUniformBlockFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceInlineUniformBlockFeaturesEXT
         where
    type StructRep VkPhysicalDeviceInlineUniformBlockFeaturesEXT =
         'StructMeta "VkPhysicalDeviceInlineUniformBlockFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceInlineUniformBlockFeaturesEXT
           #{size VkPhysicalDeviceInlineUniformBlockFeaturesEXT}
           #{alignment VkPhysicalDeviceInlineUniformBlockFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceInlineUniformBlockFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceInlineUniformBlockFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "inlineUniformBlock" VkBool32 'False 
                                                              #{offset VkPhysicalDeviceInlineUniformBlockFeaturesEXT, inlineUniformBlock}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingInlineUniformBlockUpdateAfterBind" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceInlineUniformBlockFeaturesEXT, descriptorBindingInlineUniformBlockUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceInlineUniformBlockPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     uint32_t               maxInlineUniformBlockSize;
--   >     uint32_t               maxPerStageDescriptorInlineUniformBlocks;
--   >     uint32_t               maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks;
--   >     uint32_t               maxDescriptorSetInlineUniformBlocks;
--   >     uint32_t               maxDescriptorSetUpdateAfterBindInlineUniformBlocks;
--   > } VkPhysicalDeviceInlineUniformBlockPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceInlineUniformBlockPropertiesEXT VkPhysicalDeviceInlineUniformBlockPropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceInlineUniformBlockPropertiesEXT =
     VkStruct VkPhysicalDeviceInlineUniformBlockPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceInlineUniformBlockPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceInlineUniformBlockPropertiesEXT
         where
    type StructRep VkPhysicalDeviceInlineUniformBlockPropertiesEXT =
         'StructMeta "VkPhysicalDeviceInlineUniformBlockPropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceInlineUniformBlockPropertiesEXT
           #{size VkPhysicalDeviceInlineUniformBlockPropertiesEXT}
           #{alignment VkPhysicalDeviceInlineUniformBlockPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceInlineUniformBlockPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceInlineUniformBlockPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxInlineUniformBlockSize" Word32 'False 
                                                                   #{offset VkPhysicalDeviceInlineUniformBlockPropertiesEXT, maxInlineUniformBlockSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorInlineUniformBlocks" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceInlineUniformBlockPropertiesEXT, maxPerStageDescriptorInlineUniformBlocks}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta -- ' closing tick for hsc2hs
                "maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks"
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceInlineUniformBlockPropertiesEXT, maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetInlineUniformBlocks" Word32 'False
                #{offset VkPhysicalDeviceInlineUniformBlockPropertiesEXT, maxDescriptorSetInlineUniformBlocks}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindInlineUniformBlocks" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceInlineUniformBlockPropertiesEXT, maxDescriptorSetUpdateAfterBindInlineUniformBlocks}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceLimits VkPhysicalDeviceLimits registry at www.khronos.org>
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

-- | > typedef struct VkPhysicalDeviceLineRasterizationFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           rectangularLines;
--   >     VkBool32                           bresenhamLines;
--   >     VkBool32                           smoothLines;
--   >     VkBool32                           stippledRectangularLines;
--   >     VkBool32                           stippledBresenhamLines;
--   >     VkBool32                           stippledSmoothLines;
--   > } VkPhysicalDeviceLineRasterizationFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceLineRasterizationFeaturesEXT VkPhysicalDeviceLineRasterizationFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceLineRasterizationFeaturesEXT =
     VkStruct VkPhysicalDeviceLineRasterizationFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceLineRasterizationFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceLineRasterizationFeaturesEXT
         where
    type StructRep VkPhysicalDeviceLineRasterizationFeaturesEXT =
         'StructMeta "VkPhysicalDeviceLineRasterizationFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceLineRasterizationFeaturesEXT
           #{size VkPhysicalDeviceLineRasterizationFeaturesEXT}
           #{alignment VkPhysicalDeviceLineRasterizationFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceLineRasterizationFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceLineRasterizationFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "rectangularLines" VkBool32 'False 
                                                            #{offset VkPhysicalDeviceLineRasterizationFeaturesEXT, rectangularLines}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bresenhamLines" VkBool32 'False 
                                                          #{offset VkPhysicalDeviceLineRasterizationFeaturesEXT, bresenhamLines}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "smoothLines" VkBool32 'False 
                                                       #{offset VkPhysicalDeviceLineRasterizationFeaturesEXT, smoothLines}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stippledRectangularLines" VkBool32 'False 
                                                                    #{offset VkPhysicalDeviceLineRasterizationFeaturesEXT, stippledRectangularLines}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stippledBresenhamLines" VkBool32 'False 
                                                                  #{offset VkPhysicalDeviceLineRasterizationFeaturesEXT, stippledBresenhamLines}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "stippledSmoothLines" VkBool32 'False 
                                                               #{offset VkPhysicalDeviceLineRasterizationFeaturesEXT, stippledSmoothLines}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceLineRasterizationPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     uint32_t                            lineSubPixelPrecisionBits;
--   > } VkPhysicalDeviceLineRasterizationPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceLineRasterizationPropertiesEXT VkPhysicalDeviceLineRasterizationPropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceLineRasterizationPropertiesEXT =
     VkStruct VkPhysicalDeviceLineRasterizationPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceLineRasterizationPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceLineRasterizationPropertiesEXT
         where
    type StructRep VkPhysicalDeviceLineRasterizationPropertiesEXT =
         'StructMeta "VkPhysicalDeviceLineRasterizationPropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceLineRasterizationPropertiesEXT
           #{size VkPhysicalDeviceLineRasterizationPropertiesEXT}
           #{alignment VkPhysicalDeviceLineRasterizationPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceLineRasterizationPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceLineRasterizationPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "lineSubPixelPrecisionBits" Word32 'False 
                                                                   #{offset VkPhysicalDeviceLineRasterizationPropertiesEXT, lineSubPixelPrecisionBits}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceMaintenance3Properties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         maxPerSetDescriptors;
--   >     VkDeviceSize                     maxMemoryAllocationSize;
--   > } VkPhysicalDeviceMaintenance3Properties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceMaintenance3Properties VkPhysicalDeviceMaintenance3Properties registry at www.khronos.org>
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

-- | > typedef struct VkPhysicalDeviceMemoryBudgetPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkDeviceSize                       heapBudget[VK_MAX_MEMORY_HEAPS];
--   >     VkDeviceSize                       heapUsage[VK_MAX_MEMORY_HEAPS];
--   > } VkPhysicalDeviceMemoryBudgetPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceMemoryBudgetPropertiesEXT VkPhysicalDeviceMemoryBudgetPropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceMemoryBudgetPropertiesEXT =
     VkStruct VkPhysicalDeviceMemoryBudgetPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceMemoryBudgetPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceMemoryBudgetPropertiesEXT
         where
    type StructRep VkPhysicalDeviceMemoryBudgetPropertiesEXT =
         'StructMeta "VkPhysicalDeviceMemoryBudgetPropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceMemoryBudgetPropertiesEXT
           #{size VkPhysicalDeviceMemoryBudgetPropertiesEXT}
           #{alignment VkPhysicalDeviceMemoryBudgetPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceMemoryBudgetPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceMemoryBudgetPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "heapBudget" VkDeviceSize 'False 
                                                          #{offset VkPhysicalDeviceMemoryBudgetPropertiesEXT, heapBudget}
                VK_MAX_MEMORY_HEAPS
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "heapUsage" VkDeviceSize 'False 
                                                         #{offset VkPhysicalDeviceMemoryBudgetPropertiesEXT, heapUsage}
                VK_MAX_MEMORY_HEAPS
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceMemoryProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceMemoryPriorityFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           memoryPriority;
--   > } VkPhysicalDeviceMemoryPriorityFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceMemoryPriorityFeaturesEXT VkPhysicalDeviceMemoryPriorityFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceMemoryPriorityFeaturesEXT =
     VkStruct VkPhysicalDeviceMemoryPriorityFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceMemoryPriorityFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceMemoryPriorityFeaturesEXT
         where
    type StructRep VkPhysicalDeviceMemoryPriorityFeaturesEXT =
         'StructMeta "VkPhysicalDeviceMemoryPriorityFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceMemoryPriorityFeaturesEXT
           #{size VkPhysicalDeviceMemoryPriorityFeaturesEXT}
           #{alignment VkPhysicalDeviceMemoryPriorityFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceMemoryPriorityFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceMemoryPriorityFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "memoryPriority" VkBool32 'False 
                                                          #{offset VkPhysicalDeviceMemoryPriorityFeaturesEXT, memoryPriority}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceMemoryProperties {
--   >     uint32_t               memoryTypeCount;
--   >     VkMemoryType           memoryTypes[VK_MAX_MEMORY_TYPES];
--   >     uint32_t               memoryHeapCount;
--   >     VkMemoryHeap           memoryHeaps[VK_MAX_MEMORY_HEAPS];
--   > } VkPhysicalDeviceMemoryProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceMemoryProperties VkPhysicalDeviceMemoryProperties registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceMemoryProperties2 VkPhysicalDeviceMemoryProperties2 registry at www.khronos.org>
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

-- | > typedef struct VkPhysicalDeviceMeshShaderFeaturesNV {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     VkBool32                            taskShader;
--   >     VkBool32                            meshShader;
--   > } VkPhysicalDeviceMeshShaderFeaturesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceMeshShaderFeaturesNV VkPhysicalDeviceMeshShaderFeaturesNV registry at www.khronos.org>
type VkPhysicalDeviceMeshShaderFeaturesNV =
     VkStruct VkPhysicalDeviceMeshShaderFeaturesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceMeshShaderFeaturesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceMeshShaderFeaturesNV where
    type StructRep VkPhysicalDeviceMeshShaderFeaturesNV =
         'StructMeta "VkPhysicalDeviceMeshShaderFeaturesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceMeshShaderFeaturesNV
           #{size VkPhysicalDeviceMeshShaderFeaturesNV}
           #{alignment VkPhysicalDeviceMeshShaderFeaturesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceMeshShaderFeaturesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceMeshShaderFeaturesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "taskShader" VkBool32 'False 
                                                      #{offset VkPhysicalDeviceMeshShaderFeaturesNV, taskShader}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "meshShader" VkBool32 'False 
                                                      #{offset VkPhysicalDeviceMeshShaderFeaturesNV, meshShader}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceMeshShaderPropertiesNV {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     uint32_t                            maxDrawMeshTasksCount;
--   >     uint32_t                            maxTaskWorkGroupInvocations;
--   >     uint32_t                            maxTaskWorkGroupSize[3];
--   >     uint32_t                            maxTaskTotalMemorySize;
--   >     uint32_t                            maxTaskOutputCount;
--   >     uint32_t                            maxMeshWorkGroupInvocations;
--   >     uint32_t                            maxMeshWorkGroupSize[3];
--   >     uint32_t                            maxMeshTotalMemorySize;
--   >     uint32_t                            maxMeshOutputVertices;
--   >     uint32_t                            maxMeshOutputPrimitives;
--   >     uint32_t                            maxMeshMultiviewViewCount;
--   >     uint32_t                            meshOutputPerVertexGranularity;
--   >     uint32_t                            meshOutputPerPrimitiveGranularity;
--   > } VkPhysicalDeviceMeshShaderPropertiesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceMeshShaderPropertiesNV VkPhysicalDeviceMeshShaderPropertiesNV registry at www.khronos.org>
type VkPhysicalDeviceMeshShaderPropertiesNV =
     VkStruct VkPhysicalDeviceMeshShaderPropertiesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceMeshShaderPropertiesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceMeshShaderPropertiesNV where
    type StructRep VkPhysicalDeviceMeshShaderPropertiesNV =
         'StructMeta "VkPhysicalDeviceMeshShaderPropertiesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceMeshShaderPropertiesNV
           #{size VkPhysicalDeviceMeshShaderPropertiesNV}
           #{alignment VkPhysicalDeviceMeshShaderPropertiesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceMeshShaderPropertiesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceMeshShaderPropertiesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDrawMeshTasksCount" Word32 'False 
                                                               #{offset VkPhysicalDeviceMeshShaderPropertiesNV, maxDrawMeshTasksCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTaskWorkGroupInvocations" Word32 'False 
                                                                     #{offset VkPhysicalDeviceMeshShaderPropertiesNV, maxTaskWorkGroupInvocations}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTaskWorkGroupSize" Word32 'False 
                                                              #{offset VkPhysicalDeviceMeshShaderPropertiesNV, maxTaskWorkGroupSize}
                3
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTaskTotalMemorySize" Word32 'False 
                                                                #{offset VkPhysicalDeviceMeshShaderPropertiesNV, maxTaskTotalMemorySize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTaskOutputCount" Word32 'False 
                                                            #{offset VkPhysicalDeviceMeshShaderPropertiesNV, maxTaskOutputCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxMeshWorkGroupInvocations" Word32 'False 
                                                                     #{offset VkPhysicalDeviceMeshShaderPropertiesNV, maxMeshWorkGroupInvocations}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxMeshWorkGroupSize" Word32 'False 
                                                              #{offset VkPhysicalDeviceMeshShaderPropertiesNV, maxMeshWorkGroupSize}
                3
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxMeshTotalMemorySize" Word32 'False 
                                                                #{offset VkPhysicalDeviceMeshShaderPropertiesNV, maxMeshTotalMemorySize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxMeshOutputVertices" Word32 'False 
                                                               #{offset VkPhysicalDeviceMeshShaderPropertiesNV, maxMeshOutputVertices}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxMeshOutputPrimitives" Word32 'False 
                                                                 #{offset VkPhysicalDeviceMeshShaderPropertiesNV, maxMeshOutputPrimitives}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxMeshMultiviewViewCount" Word32 'False 
                                                                   #{offset VkPhysicalDeviceMeshShaderPropertiesNV, maxMeshMultiviewViewCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "meshOutputPerVertexGranularity" Word32 'False
                #{offset VkPhysicalDeviceMeshShaderPropertiesNV, meshOutputPerVertexGranularity}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "meshOutputPerPrimitiveGranularity" Word32 'False
                #{offset VkPhysicalDeviceMeshShaderPropertiesNV, meshOutputPerPrimitiveGranularity}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceMultiviewFeatures {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         multiview;
--   >     VkBool32                         multiviewGeometryShader;
--   >     VkBool32                         multiviewTessellationShader;
--   > } VkPhysicalDeviceMultiviewFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceMultiviewFeatures VkPhysicalDeviceMultiviewFeatures registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceMultiviewProperties VkPhysicalDeviceMultiviewProperties registry at www.khronos.org>
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

-- | > typedef struct VkPhysicalDevicePCIBusInfoPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     uint32_t               pciDomain;
--   >     uint32_t               pciBus;
--   >     uint32_t               pciDevice;
--   >     uint32_t               pciFunction;
--   > } VkPhysicalDevicePCIBusInfoPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDevicePCIBusInfoPropertiesEXT VkPhysicalDevicePCIBusInfoPropertiesEXT registry at www.khronos.org>
type VkPhysicalDevicePCIBusInfoPropertiesEXT =
     VkStruct VkPhysicalDevicePCIBusInfoPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDevicePCIBusInfoPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDevicePCIBusInfoPropertiesEXT
         where
    type StructRep VkPhysicalDevicePCIBusInfoPropertiesEXT =
         'StructMeta "VkPhysicalDevicePCIBusInfoPropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDevicePCIBusInfoPropertiesEXT
           #{size VkPhysicalDevicePCIBusInfoPropertiesEXT}
           #{alignment VkPhysicalDevicePCIBusInfoPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDevicePCIBusInfoPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDevicePCIBusInfoPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pciDomain" Word32 'False 
                                                   #{offset VkPhysicalDevicePCIBusInfoPropertiesEXT, pciDomain}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pciBus" Word32 'False 
                                                #{offset VkPhysicalDevicePCIBusInfoPropertiesEXT, pciBus}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pciDevice" Word32 'False 
                                                   #{offset VkPhysicalDevicePCIBusInfoPropertiesEXT, pciDevice}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pciFunction" Word32 'False 
                                                     #{offset VkPhysicalDevicePCIBusInfoPropertiesEXT, pciFunction}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDevicePerformanceQueryFeaturesKHR {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     VkBool32                         performanceCounterQueryPools;
--   >     VkBool32                         performanceCounterMultipleQueryPools;
--   > } VkPhysicalDevicePerformanceQueryFeaturesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDevicePerformanceQueryFeaturesKHR VkPhysicalDevicePerformanceQueryFeaturesKHR registry at www.khronos.org>
type VkPhysicalDevicePerformanceQueryFeaturesKHR =
     VkStruct VkPhysicalDevicePerformanceQueryFeaturesKHR' -- ' closing tick for hsc2hs

data VkPhysicalDevicePerformanceQueryFeaturesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDevicePerformanceQueryFeaturesKHR
         where
    type StructRep VkPhysicalDevicePerformanceQueryFeaturesKHR =
         'StructMeta "VkPhysicalDevicePerformanceQueryFeaturesKHR" -- ' closing tick for hsc2hs
           VkPhysicalDevicePerformanceQueryFeaturesKHR
           #{size VkPhysicalDevicePerformanceQueryFeaturesKHR}
           #{alignment VkPhysicalDevicePerformanceQueryFeaturesKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDevicePerformanceQueryFeaturesKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDevicePerformanceQueryFeaturesKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "performanceCounterQueryPools" VkBool32 'False
                #{offset VkPhysicalDevicePerformanceQueryFeaturesKHR, performanceCounterQueryPools}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "performanceCounterMultipleQueryPools" VkBool32 'False
                #{offset VkPhysicalDevicePerformanceQueryFeaturesKHR, performanceCounterMultipleQueryPools}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDevicePerformanceQueryPropertiesKHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkBool32 allowCommandBufferQueryCopies;
--   > } VkPhysicalDevicePerformanceQueryPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDevicePerformanceQueryPropertiesKHR VkPhysicalDevicePerformanceQueryPropertiesKHR registry at www.khronos.org>
type VkPhysicalDevicePerformanceQueryPropertiesKHR =
     VkStruct VkPhysicalDevicePerformanceQueryPropertiesKHR' -- ' closing tick for hsc2hs

data VkPhysicalDevicePerformanceQueryPropertiesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDevicePerformanceQueryPropertiesKHR
         where
    type StructRep VkPhysicalDevicePerformanceQueryPropertiesKHR =
         'StructMeta "VkPhysicalDevicePerformanceQueryPropertiesKHR" -- ' closing tick for hsc2hs
           VkPhysicalDevicePerformanceQueryPropertiesKHR
           #{size VkPhysicalDevicePerformanceQueryPropertiesKHR}
           #{alignment VkPhysicalDevicePerformanceQueryPropertiesKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDevicePerformanceQueryPropertiesKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDevicePerformanceQueryPropertiesKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "allowCommandBufferQueryCopies" VkBool32 'False
                #{offset VkPhysicalDevicePerformanceQueryPropertiesKHR, allowCommandBufferQueryCopies}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkBool32                                                         pipelineCreationCacheControl;
--   > } VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT registry at www.khronos.org>
type VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT =
     VkStruct VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT
         where
    type StructRep
           VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT
         =
         'StructMeta -- ' closing tick for hsc2hs
           "VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT"
           VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT
           #{size VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT}
           #{alignment VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineCreationCacheControl" VkBool32 'False
                #{offset VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT, pipelineCreationCacheControl}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR {
--   >     VkStructureType sType;
--   >     void*              pNext;
--   >     VkBool32           pipelineExecutableInfo;
--   > } VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR registry at www.khronos.org>
type VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR =
     VkStruct VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR' -- ' closing tick for hsc2hs

data VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR
         where
    type StructRep
           VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR
         =
         'StructMeta -- ' closing tick for hsc2hs
           "VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR"
           VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR
           #{size VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR}
           #{alignment VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pipelineExecutableInfo" VkBool32 'False 
                                                                  #{offset VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR, pipelineExecutableInfo}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDevicePointClippingProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPointClippingBehavior      pointClippingBehavior;
--   > } VkPhysicalDevicePointClippingProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDevicePointClippingProperties VkPhysicalDevicePointClippingProperties registry at www.khronos.org>
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

-- | > typedef struct VkPhysicalDevicePrivateDataFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                                  pNext;
--   >     VkBool32                               privateData;
--   > } VkPhysicalDevicePrivateDataFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDevicePrivateDataFeaturesEXT VkPhysicalDevicePrivateDataFeaturesEXT registry at www.khronos.org>
type VkPhysicalDevicePrivateDataFeaturesEXT =
     VkStruct VkPhysicalDevicePrivateDataFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDevicePrivateDataFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDevicePrivateDataFeaturesEXT where
    type StructRep VkPhysicalDevicePrivateDataFeaturesEXT =
         'StructMeta "VkPhysicalDevicePrivateDataFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDevicePrivateDataFeaturesEXT
           #{size VkPhysicalDevicePrivateDataFeaturesEXT}
           #{alignment VkPhysicalDevicePrivateDataFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDevicePrivateDataFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDevicePrivateDataFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "privateData" VkBool32 'False 
                                                       #{offset VkPhysicalDevicePrivateDataFeaturesEXT, privateData}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceProperties VkPhysicalDeviceProperties registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceProperties2 VkPhysicalDeviceProperties2 registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceProtectedMemoryFeatures VkPhysicalDeviceProtectedMemoryFeatures registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceProtectedMemoryProperties VkPhysicalDeviceProtectedMemoryProperties registry at www.khronos.org>
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
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDevicePushDescriptorPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         maxPushDescriptors;
--   > } VkPhysicalDevicePushDescriptorPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDevicePushDescriptorPropertiesKHR VkPhysicalDevicePushDescriptorPropertiesKHR registry at www.khronos.org>
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
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceRayTracingPropertiesNV {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         shaderGroupHandleSize;
--   >     uint32_t                         maxRecursionDepth;
--   >     uint32_t                         maxShaderGroupStride;
--   >     uint32_t                         shaderGroupBaseAlignment;
--   >     uint64_t                         maxGeometryCount;
--   >     uint64_t                         maxInstanceCount;
--   >     uint64_t                         maxTriangleCount;
--   >     uint32_t                         maxDescriptorSetAccelerationStructures;
--   > } VkPhysicalDeviceRayTracingPropertiesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceRayTracingPropertiesNV VkPhysicalDeviceRayTracingPropertiesNV registry at www.khronos.org>
type VkPhysicalDeviceRayTracingPropertiesNV =
     VkStruct VkPhysicalDeviceRayTracingPropertiesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceRayTracingPropertiesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceRayTracingPropertiesNV where
    type StructRep VkPhysicalDeviceRayTracingPropertiesNV =
         'StructMeta "VkPhysicalDeviceRayTracingPropertiesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceRayTracingPropertiesNV
           #{size VkPhysicalDeviceRayTracingPropertiesNV}
           #{alignment VkPhysicalDeviceRayTracingPropertiesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceRayTracingPropertiesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceRayTracingPropertiesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderGroupHandleSize" Word32 'False 
                                                               #{offset VkPhysicalDeviceRayTracingPropertiesNV, shaderGroupHandleSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxRecursionDepth" Word32 'False 
                                                           #{offset VkPhysicalDeviceRayTracingPropertiesNV, maxRecursionDepth}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxShaderGroupStride" Word32 'False 
                                                              #{offset VkPhysicalDeviceRayTracingPropertiesNV, maxShaderGroupStride}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderGroupBaseAlignment" Word32 'False 
                                                                  #{offset VkPhysicalDeviceRayTracingPropertiesNV, shaderGroupBaseAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxGeometryCount" Word64 'False 
                                                          #{offset VkPhysicalDeviceRayTracingPropertiesNV, maxGeometryCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxInstanceCount" Word64 'False 
                                                          #{offset VkPhysicalDeviceRayTracingPropertiesNV, maxInstanceCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTriangleCount" Word64 'False 
                                                          #{offset VkPhysicalDeviceRayTracingPropertiesNV, maxTriangleCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetAccelerationStructures" Word32 'False
                #{offset VkPhysicalDeviceRayTracingPropertiesNV, maxDescriptorSetAccelerationStructures}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV {
--   >     VkStructureTypesType;
--   >     void*    pNext;
--   >     VkBool32                       representativeFragmentTest;
--   > } VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV registry at www.khronos.org>
type VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV =
     VkStruct VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV
         where
    type StructRep VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV
         =
         'StructMeta "VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV
           #{size VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV}
           #{alignment VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "representativeFragmentTest" VkBool32 'False 
                                                                      #{offset VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV, representativeFragmentTest}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceRobustness2FeaturesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           robustBufferAccess2;
--   >     VkBool32                           robustImageAccess2;
--   >     VkBool32                           nullDescriptor;
--   > } VkPhysicalDeviceRobustness2FeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceRobustness2FeaturesEXT VkPhysicalDeviceRobustness2FeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceRobustness2FeaturesEXT =
     VkStruct VkPhysicalDeviceRobustness2FeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceRobustness2FeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceRobustness2FeaturesEXT where
    type StructRep VkPhysicalDeviceRobustness2FeaturesEXT =
         'StructMeta "VkPhysicalDeviceRobustness2FeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceRobustness2FeaturesEXT
           #{size VkPhysicalDeviceRobustness2FeaturesEXT}
           #{alignment VkPhysicalDeviceRobustness2FeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceRobustness2FeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceRobustness2FeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "robustBufferAccess2" VkBool32 'False 
                                                               #{offset VkPhysicalDeviceRobustness2FeaturesEXT, robustBufferAccess2}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "robustImageAccess2" VkBool32 'False 
                                                              #{offset VkPhysicalDeviceRobustness2FeaturesEXT, robustImageAccess2}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "nullDescriptor" VkBool32 'False 
                                                          #{offset VkPhysicalDeviceRobustness2FeaturesEXT, nullDescriptor}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceRobustness2PropertiesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkDeviceSize                       robustStorageBufferAccessSizeAlignment;
--   >     VkDeviceSize                       robustUniformBufferAccessSizeAlignment;
--   > } VkPhysicalDeviceRobustness2PropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceRobustness2PropertiesEXT VkPhysicalDeviceRobustness2PropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceRobustness2PropertiesEXT =
     VkStruct VkPhysicalDeviceRobustness2PropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceRobustness2PropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceRobustness2PropertiesEXT
         where
    type StructRep VkPhysicalDeviceRobustness2PropertiesEXT =
         'StructMeta "VkPhysicalDeviceRobustness2PropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceRobustness2PropertiesEXT
           #{size VkPhysicalDeviceRobustness2PropertiesEXT}
           #{alignment VkPhysicalDeviceRobustness2PropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceRobustness2PropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceRobustness2PropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "robustStorageBufferAccessSizeAlignment" VkDeviceSize -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceRobustness2PropertiesEXT, robustStorageBufferAccessSizeAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "robustUniformBufferAccessSizeAlignment" VkDeviceSize -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceRobustness2PropertiesEXT, robustUniformBufferAccessSizeAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceSampleLocationsPropertiesEXT VkPhysicalDeviceSampleLocationsPropertiesEXT registry at www.khronos.org>
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

-- | > typedef struct VkPhysicalDeviceSamplerFilterMinmaxProperties {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     VkBool32               filterMinmaxSingleComponentFormats;
--   >     VkBool32               filterMinmaxImageComponentMapping;
--   > } VkPhysicalDeviceSamplerFilterMinmaxProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceSamplerFilterMinmaxProperties VkPhysicalDeviceSamplerFilterMinmaxProperties registry at www.khronos.org>
type VkPhysicalDeviceSamplerFilterMinmaxProperties =
     VkStruct VkPhysicalDeviceSamplerFilterMinmaxProperties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceSamplerFilterMinmaxProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceSamplerFilterMinmaxProperties
         where
    type StructRep VkPhysicalDeviceSamplerFilterMinmaxProperties =
         'StructMeta "VkPhysicalDeviceSamplerFilterMinmaxProperties" -- ' closing tick for hsc2hs
           VkPhysicalDeviceSamplerFilterMinmaxProperties
           #{size VkPhysicalDeviceSamplerFilterMinmaxProperties}
           #{alignment VkPhysicalDeviceSamplerFilterMinmaxProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceSamplerFilterMinmaxProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceSamplerFilterMinmaxProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "filterMinmaxSingleComponentFormats" VkBool32 'False
                #{offset VkPhysicalDeviceSamplerFilterMinmaxProperties, filterMinmaxSingleComponentFormats}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "filterMinmaxImageComponentMapping" VkBool32 'False
                #{offset VkPhysicalDeviceSamplerFilterMinmaxProperties, filterMinmaxImageComponentMapping}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceSamplerFilterMinmaxProperties`
type VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT =
     VkPhysicalDeviceSamplerFilterMinmaxProperties

-- | > typedef struct VkPhysicalDeviceSamplerYcbcrConversionFeatures {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     VkBool32                         samplerYcbcrConversion;
--   > } VkPhysicalDeviceSamplerYcbcrConversionFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceSamplerYcbcrConversionFeatures VkPhysicalDeviceSamplerYcbcrConversionFeatures registry at www.khronos.org>
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

-- | > typedef struct VkPhysicalDeviceScalarBlockLayoutFeatures {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     VkBool32                            scalarBlockLayout;
--   > } VkPhysicalDeviceScalarBlockLayoutFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceScalarBlockLayoutFeatures VkPhysicalDeviceScalarBlockLayoutFeatures registry at www.khronos.org>
type VkPhysicalDeviceScalarBlockLayoutFeatures =
     VkStruct VkPhysicalDeviceScalarBlockLayoutFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceScalarBlockLayoutFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceScalarBlockLayoutFeatures
         where
    type StructRep VkPhysicalDeviceScalarBlockLayoutFeatures =
         'StructMeta "VkPhysicalDeviceScalarBlockLayoutFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceScalarBlockLayoutFeatures
           #{size VkPhysicalDeviceScalarBlockLayoutFeatures}
           #{alignment VkPhysicalDeviceScalarBlockLayoutFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceScalarBlockLayoutFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceScalarBlockLayoutFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "scalarBlockLayout" VkBool32 'False 
                                                             #{offset VkPhysicalDeviceScalarBlockLayoutFeatures, scalarBlockLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceScalarBlockLayoutFeatures`
type VkPhysicalDeviceScalarBlockLayoutFeaturesEXT =
     VkPhysicalDeviceScalarBlockLayoutFeatures

-- | > typedef struct VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures {
--   >     VkStructureTypesType;
--   >     void*    pNext;
--   >     VkBool32                       separateDepthStencilLayouts;
--   > } VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures registry at www.khronos.org>
type VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures =
     VkStruct VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures
         where
    type StructRep VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures
         =
         'StructMeta "VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures
           #{size VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures}
           #{alignment VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "separateDepthStencilLayouts" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures, separateDepthStencilLayouts}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures`
type VkPhysicalDeviceSeparateDepthStencilLayoutsFeaturesKHR =
     VkPhysicalDeviceSeparateDepthStencilLayoutsFeatures

-- | > typedef struct VkPhysicalDeviceShaderAtomicFloatFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     VkBool32                            shaderBufferFloat32Atomics;
--   >     VkBool32                            shaderBufferFloat32AtomicAdd;
--   >     VkBool32                            shaderBufferFloat64Atomics;
--   >     VkBool32                            shaderBufferFloat64AtomicAdd;
--   >     VkBool32                            shaderSharedFloat32Atomics;
--   >     VkBool32                            shaderSharedFloat32AtomicAdd;
--   >     VkBool32                            shaderSharedFloat64Atomics;
--   >     VkBool32                            shaderSharedFloat64AtomicAdd;
--   >     VkBool32                            shaderImageFloat32Atomics;
--   >     VkBool32                            shaderImageFloat32AtomicAdd;
--   >     VkBool32                            sparseImageFloat32Atomics;
--   >     VkBool32                            sparseImageFloat32AtomicAdd;
--   > } VkPhysicalDeviceShaderAtomicFloatFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceShaderAtomicFloatFeaturesEXT VkPhysicalDeviceShaderAtomicFloatFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceShaderAtomicFloatFeaturesEXT =
     VkStruct VkPhysicalDeviceShaderAtomicFloatFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceShaderAtomicFloatFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceShaderAtomicFloatFeaturesEXT
         where
    type StructRep VkPhysicalDeviceShaderAtomicFloatFeaturesEXT =
         'StructMeta "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceShaderAtomicFloatFeaturesEXT
           #{size VkPhysicalDeviceShaderAtomicFloatFeaturesEXT}
           #{alignment VkPhysicalDeviceShaderAtomicFloatFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceShaderAtomicFloatFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceShaderAtomicFloatFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderBufferFloat32Atomics" VkBool32 'False 
                                                                      #{offset VkPhysicalDeviceShaderAtomicFloatFeaturesEXT, shaderBufferFloat32Atomics}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderBufferFloat32AtomicAdd" VkBool32 'False
                #{offset VkPhysicalDeviceShaderAtomicFloatFeaturesEXT, shaderBufferFloat32AtomicAdd}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderBufferFloat64Atomics" VkBool32 'False 
                                                                      #{offset VkPhysicalDeviceShaderAtomicFloatFeaturesEXT, shaderBufferFloat64Atomics}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderBufferFloat64AtomicAdd" VkBool32 'False
                #{offset VkPhysicalDeviceShaderAtomicFloatFeaturesEXT, shaderBufferFloat64AtomicAdd}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSharedFloat32Atomics" VkBool32 'False 
                                                                      #{offset VkPhysicalDeviceShaderAtomicFloatFeaturesEXT, shaderSharedFloat32Atomics}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSharedFloat32AtomicAdd" VkBool32 'False
                #{offset VkPhysicalDeviceShaderAtomicFloatFeaturesEXT, shaderSharedFloat32AtomicAdd}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSharedFloat64Atomics" VkBool32 'False 
                                                                      #{offset VkPhysicalDeviceShaderAtomicFloatFeaturesEXT, shaderSharedFloat64Atomics}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSharedFloat64AtomicAdd" VkBool32 'False
                #{offset VkPhysicalDeviceShaderAtomicFloatFeaturesEXT, shaderSharedFloat64AtomicAdd}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderImageFloat32Atomics" VkBool32 'False 
                                                                     #{offset VkPhysicalDeviceShaderAtomicFloatFeaturesEXT, shaderImageFloat32Atomics}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderImageFloat32AtomicAdd" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceShaderAtomicFloatFeaturesEXT, shaderImageFloat32AtomicAdd}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sparseImageFloat32Atomics" VkBool32 'False 
                                                                     #{offset VkPhysicalDeviceShaderAtomicFloatFeaturesEXT, sparseImageFloat32Atomics}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "sparseImageFloat32AtomicAdd" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceShaderAtomicFloatFeaturesEXT, sparseImageFloat32AtomicAdd}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceShaderAtomicInt64Features {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     VkBool32                            shaderBufferInt64Atomics;
--   >     VkBool32                            shaderSharedInt64Atomics;
--   > } VkPhysicalDeviceShaderAtomicInt64Features;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceShaderAtomicInt64Features VkPhysicalDeviceShaderAtomicInt64Features registry at www.khronos.org>
type VkPhysicalDeviceShaderAtomicInt64Features =
     VkStruct VkPhysicalDeviceShaderAtomicInt64Features' -- ' closing tick for hsc2hs

data VkPhysicalDeviceShaderAtomicInt64Features' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceShaderAtomicInt64Features
         where
    type StructRep VkPhysicalDeviceShaderAtomicInt64Features =
         'StructMeta "VkPhysicalDeviceShaderAtomicInt64Features" -- ' closing tick for hsc2hs
           VkPhysicalDeviceShaderAtomicInt64Features
           #{size VkPhysicalDeviceShaderAtomicInt64Features}
           #{alignment VkPhysicalDeviceShaderAtomicInt64Features}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceShaderAtomicInt64Features, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceShaderAtomicInt64Features, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderBufferInt64Atomics" VkBool32 'False 
                                                                    #{offset VkPhysicalDeviceShaderAtomicInt64Features, shaderBufferInt64Atomics}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSharedInt64Atomics" VkBool32 'False 
                                                                    #{offset VkPhysicalDeviceShaderAtomicInt64Features, shaderSharedInt64Atomics}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceShaderAtomicInt64Features`
type VkPhysicalDeviceShaderAtomicInt64FeaturesKHR =
     VkPhysicalDeviceShaderAtomicInt64Features

-- | > typedef struct VkPhysicalDeviceShaderClockFeaturesKHR {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     VkBool32                            shaderSubgroupClock;
--   >     VkBool32                            shaderDeviceClock;
--   > } VkPhysicalDeviceShaderClockFeaturesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceShaderClockFeaturesKHR VkPhysicalDeviceShaderClockFeaturesKHR registry at www.khronos.org>
type VkPhysicalDeviceShaderClockFeaturesKHR =
     VkStruct VkPhysicalDeviceShaderClockFeaturesKHR' -- ' closing tick for hsc2hs

data VkPhysicalDeviceShaderClockFeaturesKHR' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceShaderClockFeaturesKHR where
    type StructRep VkPhysicalDeviceShaderClockFeaturesKHR =
         'StructMeta "VkPhysicalDeviceShaderClockFeaturesKHR" -- ' closing tick for hsc2hs
           VkPhysicalDeviceShaderClockFeaturesKHR
           #{size VkPhysicalDeviceShaderClockFeaturesKHR}
           #{alignment VkPhysicalDeviceShaderClockFeaturesKHR}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceShaderClockFeaturesKHR, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceShaderClockFeaturesKHR, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSubgroupClock" VkBool32 'False 
                                                               #{offset VkPhysicalDeviceShaderClockFeaturesKHR, shaderSubgroupClock}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDeviceClock" VkBool32 'False 
                                                             #{offset VkPhysicalDeviceShaderClockFeaturesKHR, shaderDeviceClock}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceShaderCoreProperties2AMD {
--   >     VkStructureType sType;
--   >     void*    pNext;
--   >     VkShaderCorePropertiesFlagsAMD shaderCoreFeatures;
--   >     uint32_t activeComputeUnitCount;
--   > } VkPhysicalDeviceShaderCoreProperties2AMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceShaderCoreProperties2AMD VkPhysicalDeviceShaderCoreProperties2AMD registry at www.khronos.org>
type VkPhysicalDeviceShaderCoreProperties2AMD =
     VkStruct VkPhysicalDeviceShaderCoreProperties2AMD' -- ' closing tick for hsc2hs

data VkPhysicalDeviceShaderCoreProperties2AMD' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceShaderCoreProperties2AMD
         where
    type StructRep VkPhysicalDeviceShaderCoreProperties2AMD =
         'StructMeta "VkPhysicalDeviceShaderCoreProperties2AMD" -- ' closing tick for hsc2hs
           VkPhysicalDeviceShaderCoreProperties2AMD
           #{size VkPhysicalDeviceShaderCoreProperties2AMD}
           #{alignment VkPhysicalDeviceShaderCoreProperties2AMD}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceShaderCoreProperties2AMD, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceShaderCoreProperties2AMD, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderCoreFeatures" VkShaderCorePropertiesFlagsAMD -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceShaderCoreProperties2AMD, shaderCoreFeatures}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "activeComputeUnitCount" Word32 'False 
                                                                #{offset VkPhysicalDeviceShaderCoreProperties2AMD, activeComputeUnitCount}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceShaderCorePropertiesAMD VkPhysicalDeviceShaderCorePropertiesAMD registry at www.khronos.org>
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

-- | > typedef struct VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           shaderDemoteToHelperInvocation;
--   > } VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT =
     VkStruct VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT
         where
    type StructRep
           VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT
         =
         'StructMeta -- ' closing tick for hsc2hs
           "VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT"
           VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT
           #{size VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT}
           #{alignment VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDemoteToHelperInvocation" VkBool32 'False
                #{offset VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT, shaderDemoteToHelperInvocation}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceShaderDrawParametersFeatures`
type VkPhysicalDeviceShaderDrawParameterFeatures =
     VkPhysicalDeviceShaderDrawParametersFeatures

-- | > typedef struct VkPhysicalDeviceShaderDrawParametersFeatures {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         shaderDrawParameters;
--   > } VkPhysicalDeviceShaderDrawParametersFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceShaderDrawParametersFeatures VkPhysicalDeviceShaderDrawParametersFeatures registry at www.khronos.org>
type VkPhysicalDeviceShaderDrawParametersFeatures =
     VkStruct VkPhysicalDeviceShaderDrawParametersFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceShaderDrawParametersFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceShaderDrawParametersFeatures
         where
    type StructRep VkPhysicalDeviceShaderDrawParametersFeatures =
         'StructMeta "VkPhysicalDeviceShaderDrawParametersFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceShaderDrawParametersFeatures
           #{size VkPhysicalDeviceShaderDrawParametersFeatures}
           #{alignment VkPhysicalDeviceShaderDrawParametersFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceShaderDrawParametersFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceShaderDrawParametersFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDrawParameters" VkBool32 'False 
                                                                #{offset VkPhysicalDeviceShaderDrawParametersFeatures, shaderDrawParameters}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceShaderFloat16Int8Features {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     VkBool32                         shaderFloat16;
--   >     VkBool32                         shaderInt8;
--   > } VkPhysicalDeviceShaderFloat16Int8Features;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceShaderFloat16Int8Features VkPhysicalDeviceShaderFloat16Int8Features registry at www.khronos.org>
type VkPhysicalDeviceShaderFloat16Int8Features =
     VkStruct VkPhysicalDeviceShaderFloat16Int8Features' -- ' closing tick for hsc2hs

data VkPhysicalDeviceShaderFloat16Int8Features' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceShaderFloat16Int8Features
         where
    type StructRep VkPhysicalDeviceShaderFloat16Int8Features =
         'StructMeta "VkPhysicalDeviceShaderFloat16Int8Features" -- ' closing tick for hsc2hs
           VkPhysicalDeviceShaderFloat16Int8Features
           #{size VkPhysicalDeviceShaderFloat16Int8Features}
           #{alignment VkPhysicalDeviceShaderFloat16Int8Features}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceShaderFloat16Int8Features, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceShaderFloat16Int8Features, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderFloat16" VkBool32 'False 
                                                         #{offset VkPhysicalDeviceShaderFloat16Int8Features, shaderFloat16}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderInt8" VkBool32 'False 
                                                      #{offset VkPhysicalDeviceShaderFloat16Int8Features, shaderInt8}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceShaderFloat16Int8Features`
type VkPhysicalDeviceShaderFloat16Int8FeaturesKHR =
     VkPhysicalDeviceShaderFloat16Int8Features

-- | > typedef struct VkPhysicalDeviceShaderImageFootprintFeaturesNV {
--   >     VkStructureType sType;
--   >     void*                              pNext;
--   >     VkBool32                           imageFootprint;
--   > } VkPhysicalDeviceShaderImageFootprintFeaturesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceShaderImageFootprintFeaturesNV VkPhysicalDeviceShaderImageFootprintFeaturesNV registry at www.khronos.org>
type VkPhysicalDeviceShaderImageFootprintFeaturesNV =
     VkStruct VkPhysicalDeviceShaderImageFootprintFeaturesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceShaderImageFootprintFeaturesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceShaderImageFootprintFeaturesNV
         where
    type StructRep VkPhysicalDeviceShaderImageFootprintFeaturesNV =
         'StructMeta "VkPhysicalDeviceShaderImageFootprintFeaturesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceShaderImageFootprintFeaturesNV
           #{size VkPhysicalDeviceShaderImageFootprintFeaturesNV}
           #{alignment VkPhysicalDeviceShaderImageFootprintFeaturesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceShaderImageFootprintFeaturesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceShaderImageFootprintFeaturesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imageFootprint" VkBool32 'False 
                                                          #{offset VkPhysicalDeviceShaderImageFootprintFeaturesNV, imageFootprint}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         shaderIntegerFunctions2;
--   > } VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL registry at www.khronos.org>
type VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL =
     VkStruct VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL' -- ' closing tick for hsc2hs

data VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL
         where
    type StructRep VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL
         =
         'StructMeta "VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL" -- ' closing tick for hsc2hs
           VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL
           #{size VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL}
           #{alignment VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderIntegerFunctions2" VkBool32 'False 
                                                                   #{offset VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL, shaderIntegerFunctions2}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceShaderSMBuiltinsFeaturesNV {
--   >     VkStructureTypesType;
--   >     void*    pNext;
--   >     VkBool32                       shaderSMBuiltins;
--   > } VkPhysicalDeviceShaderSMBuiltinsFeaturesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceShaderSMBuiltinsFeaturesNV VkPhysicalDeviceShaderSMBuiltinsFeaturesNV registry at www.khronos.org>
type VkPhysicalDeviceShaderSMBuiltinsFeaturesNV =
     VkStruct VkPhysicalDeviceShaderSMBuiltinsFeaturesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceShaderSMBuiltinsFeaturesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceShaderSMBuiltinsFeaturesNV
         where
    type StructRep VkPhysicalDeviceShaderSMBuiltinsFeaturesNV =
         'StructMeta "VkPhysicalDeviceShaderSMBuiltinsFeaturesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceShaderSMBuiltinsFeaturesNV
           #{size VkPhysicalDeviceShaderSMBuiltinsFeaturesNV}
           #{alignment VkPhysicalDeviceShaderSMBuiltinsFeaturesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceShaderSMBuiltinsFeaturesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceShaderSMBuiltinsFeaturesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSMBuiltins" VkBool32 'False 
                                                            #{offset VkPhysicalDeviceShaderSMBuiltinsFeaturesNV, shaderSMBuiltins}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceShaderSMBuiltinsPropertiesNV {
--   >     VkStructureType sType;
--   >     void*                          pNext;
--   >     uint32_t                       shaderSMCount;
--   >     uint32_t                       shaderWarpsPerSM;
--   > } VkPhysicalDeviceShaderSMBuiltinsPropertiesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceShaderSMBuiltinsPropertiesNV VkPhysicalDeviceShaderSMBuiltinsPropertiesNV registry at www.khronos.org>
type VkPhysicalDeviceShaderSMBuiltinsPropertiesNV =
     VkStruct VkPhysicalDeviceShaderSMBuiltinsPropertiesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceShaderSMBuiltinsPropertiesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceShaderSMBuiltinsPropertiesNV
         where
    type StructRep VkPhysicalDeviceShaderSMBuiltinsPropertiesNV =
         'StructMeta "VkPhysicalDeviceShaderSMBuiltinsPropertiesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceShaderSMBuiltinsPropertiesNV
           #{size VkPhysicalDeviceShaderSMBuiltinsPropertiesNV}
           #{alignment VkPhysicalDeviceShaderSMBuiltinsPropertiesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceShaderSMBuiltinsPropertiesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceShaderSMBuiltinsPropertiesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSMCount" Word32 'False 
                                                       #{offset VkPhysicalDeviceShaderSMBuiltinsPropertiesNV, shaderSMCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderWarpsPerSM" Word32 'False 
                                                          #{offset VkPhysicalDeviceShaderSMBuiltinsPropertiesNV, shaderWarpsPerSM}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures {
--   >     VkStructureType sType;
--   >     void*                          pNext;
--   >     VkBool32 shaderSubgroupExtendedTypes;
--   > } VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures registry at www.khronos.org>
type VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures =
     VkStruct VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures
         where
    type StructRep VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures
         =
         'StructMeta "VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures
           #{size VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures}
           #{alignment VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSubgroupExtendedTypes" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures, shaderSubgroupExtendedTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures`
type VkPhysicalDeviceShaderSubgroupExtendedTypesFeaturesKHR =
     VkPhysicalDeviceShaderSubgroupExtendedTypesFeatures

-- | > typedef struct VkPhysicalDeviceShadingRateImageFeaturesNV {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     VkBool32                            shadingRateImage;
--   >     VkBool32                            shadingRateCoarseSampleOrder;
--   > } VkPhysicalDeviceShadingRateImageFeaturesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceShadingRateImageFeaturesNV VkPhysicalDeviceShadingRateImageFeaturesNV registry at www.khronos.org>
type VkPhysicalDeviceShadingRateImageFeaturesNV =
     VkStruct VkPhysicalDeviceShadingRateImageFeaturesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceShadingRateImageFeaturesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceShadingRateImageFeaturesNV
         where
    type StructRep VkPhysicalDeviceShadingRateImageFeaturesNV =
         'StructMeta "VkPhysicalDeviceShadingRateImageFeaturesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceShadingRateImageFeaturesNV
           #{size VkPhysicalDeviceShadingRateImageFeaturesNV}
           #{alignment VkPhysicalDeviceShadingRateImageFeaturesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceShadingRateImageFeaturesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceShadingRateImageFeaturesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shadingRateImage" VkBool32 'False 
                                                            #{offset VkPhysicalDeviceShadingRateImageFeaturesNV, shadingRateImage}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shadingRateCoarseSampleOrder" VkBool32 'False
                #{offset VkPhysicalDeviceShadingRateImageFeaturesNV, shadingRateCoarseSampleOrder}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceShadingRateImagePropertiesNV {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     VkExtent2D                          shadingRateTexelSize;
--   >     uint32_t                            shadingRatePaletteSize;
--   >     uint32_t                            shadingRateMaxCoarseSamples;
--   > } VkPhysicalDeviceShadingRateImagePropertiesNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceShadingRateImagePropertiesNV VkPhysicalDeviceShadingRateImagePropertiesNV registry at www.khronos.org>
type VkPhysicalDeviceShadingRateImagePropertiesNV =
     VkStruct VkPhysicalDeviceShadingRateImagePropertiesNV' -- ' closing tick for hsc2hs

data VkPhysicalDeviceShadingRateImagePropertiesNV' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceShadingRateImagePropertiesNV
         where
    type StructRep VkPhysicalDeviceShadingRateImagePropertiesNV =
         'StructMeta "VkPhysicalDeviceShadingRateImagePropertiesNV" -- ' closing tick for hsc2hs
           VkPhysicalDeviceShadingRateImagePropertiesNV
           #{size VkPhysicalDeviceShadingRateImagePropertiesNV}
           #{alignment VkPhysicalDeviceShadingRateImagePropertiesNV}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceShadingRateImagePropertiesNV, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceShadingRateImagePropertiesNV, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shadingRateTexelSize" VkExtent2D 'False 
                                                                  #{offset VkPhysicalDeviceShadingRateImagePropertiesNV, shadingRateTexelSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shadingRatePaletteSize" Word32 'False 
                                                                #{offset VkPhysicalDeviceShadingRateImagePropertiesNV, shadingRatePaletteSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shadingRateMaxCoarseSamples" Word32 'False 
                                                                     #{offset VkPhysicalDeviceShadingRateImagePropertiesNV, shadingRateMaxCoarseSamples}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceSparseImageFormatInfo2 VkPhysicalDeviceSparseImageFormatInfo2 registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceSparseProperties VkPhysicalDeviceSparseProperties registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceSubgroupProperties VkPhysicalDeviceSubgroupProperties registry at www.khronos.org>
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

-- | > typedef struct VkPhysicalDeviceSubgroupSizeControlFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                          pNext;
--   >     VkBool32 subgroupSizeControl;
--   >     VkBool32 computeFullSubgroups;
--   > } VkPhysicalDeviceSubgroupSizeControlFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceSubgroupSizeControlFeaturesEXT VkPhysicalDeviceSubgroupSizeControlFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceSubgroupSizeControlFeaturesEXT =
     VkStruct VkPhysicalDeviceSubgroupSizeControlFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceSubgroupSizeControlFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceSubgroupSizeControlFeaturesEXT
         where
    type StructRep VkPhysicalDeviceSubgroupSizeControlFeaturesEXT =
         'StructMeta "VkPhysicalDeviceSubgroupSizeControlFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceSubgroupSizeControlFeaturesEXT
           #{size VkPhysicalDeviceSubgroupSizeControlFeaturesEXT}
           #{alignment VkPhysicalDeviceSubgroupSizeControlFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceSubgroupSizeControlFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceSubgroupSizeControlFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subgroupSizeControl" VkBool32 'False 
                                                               #{offset VkPhysicalDeviceSubgroupSizeControlFeaturesEXT, subgroupSizeControl}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "computeFullSubgroups" VkBool32 'False 
                                                                #{offset VkPhysicalDeviceSubgroupSizeControlFeaturesEXT, computeFullSubgroups}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceSubgroupSizeControlPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                          pNext;
--   >     uint32_t minSubgroupSize;
--   >     uint32_t maxSubgroupSize;
--   >     uint32_t maxComputeWorkgroupSubgroups;
--   >     VkShaderStageFlags             requiredSubgroupSizeStages;
--   > } VkPhysicalDeviceSubgroupSizeControlPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceSubgroupSizeControlPropertiesEXT VkPhysicalDeviceSubgroupSizeControlPropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceSubgroupSizeControlPropertiesEXT =
     VkStruct VkPhysicalDeviceSubgroupSizeControlPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceSubgroupSizeControlPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceSubgroupSizeControlPropertiesEXT
         where
    type StructRep VkPhysicalDeviceSubgroupSizeControlPropertiesEXT =
         'StructMeta "VkPhysicalDeviceSubgroupSizeControlPropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceSubgroupSizeControlPropertiesEXT
           #{size VkPhysicalDeviceSubgroupSizeControlPropertiesEXT}
           #{alignment VkPhysicalDeviceSubgroupSizeControlPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceSubgroupSizeControlPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceSubgroupSizeControlPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "minSubgroupSize" Word32 'False 
                                                         #{offset VkPhysicalDeviceSubgroupSizeControlPropertiesEXT, minSubgroupSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxSubgroupSize" Word32 'False 
                                                         #{offset VkPhysicalDeviceSubgroupSizeControlPropertiesEXT, maxSubgroupSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxComputeWorkgroupSubgroups" Word32 'False 
                                                                      #{offset VkPhysicalDeviceSubgroupSizeControlPropertiesEXT, maxComputeWorkgroupSubgroups}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "requiredSubgroupSizeStages" VkShaderStageFlags 'False
                #{offset VkPhysicalDeviceSubgroupSizeControlPropertiesEXT, requiredSubgroupSizeStages}
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
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceSurfaceInfo2KHR VkPhysicalDeviceSurfaceInfo2KHR registry at www.khronos.org>
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

-- | > typedef struct VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           texelBufferAlignment;
--   > } VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT =
     VkStruct VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT
         where
    type StructRep VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT =
         'StructMeta "VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT
           #{size VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT}
           #{alignment VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "texelBufferAlignment" VkBool32 'False 
                                                                #{offset VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT, texelBufferAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkDeviceSize                       storageTexelBufferOffsetAlignmentBytes;
--   >     VkBool32                           storageTexelBufferOffsetSingleTexelAlignment;
--   >     VkDeviceSize                       uniformTexelBufferOffsetAlignmentBytes;
--   >     VkBool32                           uniformTexelBufferOffsetSingleTexelAlignment;
--   > } VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT =
     VkStruct VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT
         where
    type StructRep VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT =
         'StructMeta "VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT
           #{size VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT}
           #{alignment VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "storageTexelBufferOffsetAlignmentBytes" VkDeviceSize -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT, storageTexelBufferOffsetAlignmentBytes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "storageTexelBufferOffsetSingleTexelAlignment" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT, storageTexelBufferOffsetSingleTexelAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "uniformTexelBufferOffsetAlignmentBytes" VkDeviceSize -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT, uniformTexelBufferOffsetAlignmentBytes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "uniformTexelBufferOffsetSingleTexelAlignment" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT, uniformTexelBufferOffsetSingleTexelAlignment}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     VkBool32               textureCompressionASTC_HDR;
--   > } VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT =
     VkStruct VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT
         where
    type StructRep VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT
         =
         'StructMeta "VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT
           #{size VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT}
           #{alignment VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "textureCompressionASTC_HDR" VkBool32 'False 
                                                                      #{offset VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT, textureCompressionASTC_HDR}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceTimelineSemaphoreFeatures {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     VkBool32               timelineSemaphore;
--   > } VkPhysicalDeviceTimelineSemaphoreFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceTimelineSemaphoreFeatures VkPhysicalDeviceTimelineSemaphoreFeatures registry at www.khronos.org>
type VkPhysicalDeviceTimelineSemaphoreFeatures =
     VkStruct VkPhysicalDeviceTimelineSemaphoreFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceTimelineSemaphoreFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceTimelineSemaphoreFeatures
         where
    type StructRep VkPhysicalDeviceTimelineSemaphoreFeatures =
         'StructMeta "VkPhysicalDeviceTimelineSemaphoreFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceTimelineSemaphoreFeatures
           #{size VkPhysicalDeviceTimelineSemaphoreFeatures}
           #{alignment VkPhysicalDeviceTimelineSemaphoreFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceTimelineSemaphoreFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceTimelineSemaphoreFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "timelineSemaphore" VkBool32 'False 
                                                             #{offset VkPhysicalDeviceTimelineSemaphoreFeatures, timelineSemaphore}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceTimelineSemaphoreFeatures`
type VkPhysicalDeviceTimelineSemaphoreFeaturesKHR =
     VkPhysicalDeviceTimelineSemaphoreFeatures

-- | > typedef struct VkPhysicalDeviceTimelineSemaphoreProperties {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     uint64_t               maxTimelineSemaphoreValueDifference;
--   > } VkPhysicalDeviceTimelineSemaphoreProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceTimelineSemaphoreProperties VkPhysicalDeviceTimelineSemaphoreProperties registry at www.khronos.org>
type VkPhysicalDeviceTimelineSemaphoreProperties =
     VkStruct VkPhysicalDeviceTimelineSemaphoreProperties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceTimelineSemaphoreProperties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceTimelineSemaphoreProperties
         where
    type StructRep VkPhysicalDeviceTimelineSemaphoreProperties =
         'StructMeta "VkPhysicalDeviceTimelineSemaphoreProperties" -- ' closing tick for hsc2hs
           VkPhysicalDeviceTimelineSemaphoreProperties
           #{size VkPhysicalDeviceTimelineSemaphoreProperties}
           #{alignment VkPhysicalDeviceTimelineSemaphoreProperties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceTimelineSemaphoreProperties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceTimelineSemaphoreProperties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTimelineSemaphoreValueDifference" Word64 'False
                #{offset VkPhysicalDeviceTimelineSemaphoreProperties, maxTimelineSemaphoreValueDifference}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceTimelineSemaphoreProperties`
type VkPhysicalDeviceTimelineSemaphorePropertiesKHR =
     VkPhysicalDeviceTimelineSemaphoreProperties

-- | > typedef struct VkPhysicalDeviceToolPropertiesEXT {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     char            name[VK_MAX_EXTENSION_NAME_SIZE];
--   >     char            version[VK_MAX_EXTENSION_NAME_SIZE];
--   >     VkToolPurposeFlagsEXT purposes;
--   >     char            description[VK_MAX_DESCRIPTION_SIZE];
--   >     char            layer[VK_MAX_EXTENSION_NAME_SIZE];
--   > } VkPhysicalDeviceToolPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceToolPropertiesEXT VkPhysicalDeviceToolPropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceToolPropertiesEXT =
     VkStruct VkPhysicalDeviceToolPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceToolPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceToolPropertiesEXT where
    type StructRep VkPhysicalDeviceToolPropertiesEXT =
         'StructMeta "VkPhysicalDeviceToolPropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceToolPropertiesEXT
           #{size VkPhysicalDeviceToolPropertiesEXT}
           #{alignment VkPhysicalDeviceToolPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceToolPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceToolPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "name" CChar 'False 
                                             #{offset VkPhysicalDeviceToolPropertiesEXT, name}
                VK_MAX_EXTENSION_NAME_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "version" CChar 'False 
                                                #{offset VkPhysicalDeviceToolPropertiesEXT, version}
                VK_MAX_EXTENSION_NAME_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "purposes" VkToolPurposeFlagsEXT 'False 
                                                                 #{offset VkPhysicalDeviceToolPropertiesEXT, purposes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "description" CChar 'False 
                                                    #{offset VkPhysicalDeviceToolPropertiesEXT, description}
                VK_MAX_DESCRIPTION_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "layer" CChar 'False 
                                              #{offset VkPhysicalDeviceToolPropertiesEXT, layer}
                VK_MAX_EXTENSION_NAME_SIZE
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceTransformFeedbackFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     VkBool32               transformFeedback;
--   >     VkBool32               geometryStreams;
--   > } VkPhysicalDeviceTransformFeedbackFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceTransformFeedbackFeaturesEXT VkPhysicalDeviceTransformFeedbackFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceTransformFeedbackFeaturesEXT =
     VkStruct VkPhysicalDeviceTransformFeedbackFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceTransformFeedbackFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceTransformFeedbackFeaturesEXT
         where
    type StructRep VkPhysicalDeviceTransformFeedbackFeaturesEXT =
         'StructMeta "VkPhysicalDeviceTransformFeedbackFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceTransformFeedbackFeaturesEXT
           #{size VkPhysicalDeviceTransformFeedbackFeaturesEXT}
           #{alignment VkPhysicalDeviceTransformFeedbackFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceTransformFeedbackFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceTransformFeedbackFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "transformFeedback" VkBool32 'False 
                                                             #{offset VkPhysicalDeviceTransformFeedbackFeaturesEXT, transformFeedback}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "geometryStreams" VkBool32 'False 
                                                           #{offset VkPhysicalDeviceTransformFeedbackFeaturesEXT, geometryStreams}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceTransformFeedbackPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     uint32_t               maxTransformFeedbackStreams;
--   >     uint32_t               maxTransformFeedbackBuffers;
--   >     VkDeviceSize           maxTransformFeedbackBufferSize;
--   >     uint32_t               maxTransformFeedbackStreamDataSize;
--   >     uint32_t               maxTransformFeedbackBufferDataSize;
--   >     uint32_t               maxTransformFeedbackBufferDataStride;
--   >     VkBool32               transformFeedbackQueries;
--   >     VkBool32               transformFeedbackStreamsLinesTriangles;
--   >     VkBool32               transformFeedbackRasterizationStreamSelect;
--   >     VkBool32               transformFeedbackDraw;
--   > } VkPhysicalDeviceTransformFeedbackPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceTransformFeedbackPropertiesEXT VkPhysicalDeviceTransformFeedbackPropertiesEXT registry at www.khronos.org>
type VkPhysicalDeviceTransformFeedbackPropertiesEXT =
     VkStruct VkPhysicalDeviceTransformFeedbackPropertiesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceTransformFeedbackPropertiesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceTransformFeedbackPropertiesEXT
         where
    type StructRep VkPhysicalDeviceTransformFeedbackPropertiesEXT =
         'StructMeta "VkPhysicalDeviceTransformFeedbackPropertiesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceTransformFeedbackPropertiesEXT
           #{size VkPhysicalDeviceTransformFeedbackPropertiesEXT}
           #{alignment VkPhysicalDeviceTransformFeedbackPropertiesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceTransformFeedbackPropertiesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceTransformFeedbackPropertiesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTransformFeedbackStreams" Word32 'False 
                                                                     #{offset VkPhysicalDeviceTransformFeedbackPropertiesEXT, maxTransformFeedbackStreams}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTransformFeedbackBuffers" Word32 'False 
                                                                     #{offset VkPhysicalDeviceTransformFeedbackPropertiesEXT, maxTransformFeedbackBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTransformFeedbackBufferSize" VkDeviceSize 'False
                #{offset VkPhysicalDeviceTransformFeedbackPropertiesEXT, maxTransformFeedbackBufferSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTransformFeedbackStreamDataSize" Word32 'False
                #{offset VkPhysicalDeviceTransformFeedbackPropertiesEXT, maxTransformFeedbackStreamDataSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTransformFeedbackBufferDataSize" Word32 'False
                #{offset VkPhysicalDeviceTransformFeedbackPropertiesEXT, maxTransformFeedbackBufferDataSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTransformFeedbackBufferDataStride" Word32 'False
                #{offset VkPhysicalDeviceTransformFeedbackPropertiesEXT, maxTransformFeedbackBufferDataStride}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "transformFeedbackQueries" VkBool32 'False 
                                                                    #{offset VkPhysicalDeviceTransformFeedbackPropertiesEXT, transformFeedbackQueries}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "transformFeedbackStreamsLinesTriangles" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceTransformFeedbackPropertiesEXT, transformFeedbackStreamsLinesTriangles}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "transformFeedbackRasterizationStreamSelect" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceTransformFeedbackPropertiesEXT, transformFeedbackRasterizationStreamSelect}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "transformFeedbackDraw" VkBool32 'False 
                                                                 #{offset VkPhysicalDeviceTransformFeedbackPropertiesEXT, transformFeedbackDraw}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceUniformBufferStandardLayoutFeatures {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     VkBool32                            uniformBufferStandardLayout;
--   > } VkPhysicalDeviceUniformBufferStandardLayoutFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceUniformBufferStandardLayoutFeatures VkPhysicalDeviceUniformBufferStandardLayoutFeatures registry at www.khronos.org>
type VkPhysicalDeviceUniformBufferStandardLayoutFeatures =
     VkStruct VkPhysicalDeviceUniformBufferStandardLayoutFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceUniformBufferStandardLayoutFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceUniformBufferStandardLayoutFeatures
         where
    type StructRep VkPhysicalDeviceUniformBufferStandardLayoutFeatures
         =
         'StructMeta "VkPhysicalDeviceUniformBufferStandardLayoutFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceUniformBufferStandardLayoutFeatures
           #{size VkPhysicalDeviceUniformBufferStandardLayoutFeatures}
           #{alignment VkPhysicalDeviceUniformBufferStandardLayoutFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceUniformBufferStandardLayoutFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceUniformBufferStandardLayoutFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "uniformBufferStandardLayout" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceUniformBufferStandardLayoutFeatures, uniformBufferStandardLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceUniformBufferStandardLayoutFeatures`
type VkPhysicalDeviceUniformBufferStandardLayoutFeaturesKHR =
     VkPhysicalDeviceUniformBufferStandardLayoutFeatures

-- | Alias for `VkPhysicalDeviceVariablePointersFeatures`
type VkPhysicalDeviceVariablePointerFeatures =
     VkPhysicalDeviceVariablePointersFeatures

-- | Alias for `VkPhysicalDeviceVariablePointersFeatures`
type VkPhysicalDeviceVariablePointerFeaturesKHR =
     VkPhysicalDeviceVariablePointersFeatures

-- | > typedef struct VkPhysicalDeviceVariablePointersFeatures {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         variablePointersStorageBuffer;
--   >     VkBool32                         variablePointers;
--   > } VkPhysicalDeviceVariablePointersFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVariablePointersFeatures VkPhysicalDeviceVariablePointersFeatures registry at www.khronos.org>
type VkPhysicalDeviceVariablePointersFeatures =
     VkStruct VkPhysicalDeviceVariablePointersFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceVariablePointersFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceVariablePointersFeatures
         where
    type StructRep VkPhysicalDeviceVariablePointersFeatures =
         'StructMeta "VkPhysicalDeviceVariablePointersFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceVariablePointersFeatures
           #{size VkPhysicalDeviceVariablePointersFeatures}
           #{alignment VkPhysicalDeviceVariablePointersFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceVariablePointersFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceVariablePointersFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "variablePointersStorageBuffer" VkBool32 'False
                #{offset VkPhysicalDeviceVariablePointersFeatures, variablePointersStorageBuffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "variablePointers" VkBool32 'False 
                                                            #{offset VkPhysicalDeviceVariablePointersFeatures, variablePointers}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceVariablePointersFeatures`
type VkPhysicalDeviceVariablePointersFeaturesKHR =
     VkPhysicalDeviceVariablePointersFeatures

-- | > typedef struct VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           vertexAttributeInstanceRateDivisor;
--   >     VkBool32                           vertexAttributeInstanceRateZeroDivisor;
--   > } VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT =
     VkStruct VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal
           VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT
         where
    type StructRep VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT =
         'StructMeta "VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT
           #{size VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT}
           #{alignment VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexAttributeInstanceRateDivisor" VkBool32 'False
                #{offset VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT, vertexAttributeInstanceRateDivisor}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vertexAttributeInstanceRateZeroDivisor" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT, vertexAttributeInstanceRateZeroDivisor}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     uint32_t               maxVertexAttribDivisor;
--   > } VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT registry at www.khronos.org>
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
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceVulkan11Features {
--   >     VkStructureTypesType;
--   >     void*    pNext;
--   >     VkBool32                         storageBuffer16BitAccess;
--   >     VkBool32                         uniformAndStorageBuffer16BitAccess;
--   >     VkBool32                         storagePushConstant16;
--   >     VkBool32                         storageInputOutput16;
--   >     VkBool32                         multiview;
--   >     VkBool32                         multiviewGeometryShader;
--   >     VkBool32                         multiviewTessellationShader;
--   >     VkBool32                         variablePointersStorageBuffer;
--   >     VkBool32                         variablePointers;
--   >     VkBool32                         protectedMemory;
--   >     VkBool32                         samplerYcbcrConversion;
--   >     VkBool32                         shaderDrawParameters;
--   > } VkPhysicalDeviceVulkan11Features;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVulkan11Features VkPhysicalDeviceVulkan11Features registry at www.khronos.org>
type VkPhysicalDeviceVulkan11Features =
     VkStruct VkPhysicalDeviceVulkan11Features' -- ' closing tick for hsc2hs

data VkPhysicalDeviceVulkan11Features' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceVulkan11Features where
    type StructRep VkPhysicalDeviceVulkan11Features =
         'StructMeta "VkPhysicalDeviceVulkan11Features" -- ' closing tick for hsc2hs
           VkPhysicalDeviceVulkan11Features
           #{size VkPhysicalDeviceVulkan11Features}
           #{alignment VkPhysicalDeviceVulkan11Features}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceVulkan11Features, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceVulkan11Features, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "storageBuffer16BitAccess" VkBool32 'False 
                                                                    #{offset VkPhysicalDeviceVulkan11Features, storageBuffer16BitAccess}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "uniformAndStorageBuffer16BitAccess" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan11Features, uniformAndStorageBuffer16BitAccess}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "storagePushConstant16" VkBool32 'False 
                                                                 #{offset VkPhysicalDeviceVulkan11Features, storagePushConstant16}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "storageInputOutput16" VkBool32 'False 
                                                                #{offset VkPhysicalDeviceVulkan11Features, storageInputOutput16}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "multiview" VkBool32 'False 
                                                     #{offset VkPhysicalDeviceVulkan11Features, multiview}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "multiviewGeometryShader" VkBool32 'False 
                                                                   #{offset VkPhysicalDeviceVulkan11Features, multiviewGeometryShader}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "multiviewTessellationShader" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceVulkan11Features, multiviewTessellationShader}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "variablePointersStorageBuffer" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan11Features, variablePointersStorageBuffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "variablePointers" VkBool32 'False 
                                                            #{offset VkPhysicalDeviceVulkan11Features, variablePointers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "protectedMemory" VkBool32 'False 
                                                           #{offset VkPhysicalDeviceVulkan11Features, protectedMemory}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "samplerYcbcrConversion" VkBool32 'False 
                                                                  #{offset VkPhysicalDeviceVulkan11Features, samplerYcbcrConversion}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDrawParameters" VkBool32 'False 
                                                                #{offset VkPhysicalDeviceVulkan11Features, shaderDrawParameters}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceVulkan11Properties {
--   >     VkStructureTypesType;
--   >     void*      pNext;
--   >     uint8_t                          deviceUUID[VK_UUID_SIZE];
--   >     uint8_t                          driverUUID[VK_UUID_SIZE];
--   >     uint8_t                          deviceLUID[VK_LUID_SIZE];
--   >     uint32_t                         deviceNodeMask;
--   >     VkBool32                         deviceLUIDValid;
--   >     uint32_t                      subgroupSize;
--   >     VkShaderStageFlags            subgroupSupportedStages;
--   >     VkSubgroupFeatureFlags        subgroupSupportedOperations;
--   >     VkBool32                      subgroupQuadOperationsInAllStages;
--   >     VkPointClippingBehavior          pointClippingBehavior;
--   >     uint32_t                         maxMultiviewViewCount;
--   >     uint32_t                         maxMultiviewInstanceIndex;
--   >     VkBool32                         protectedNoFault;
--   >     uint32_t                         maxPerSetDescriptors;
--   >     VkDeviceSize                     maxMemoryAllocationSize;
--   > } VkPhysicalDeviceVulkan11Properties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVulkan11Properties VkPhysicalDeviceVulkan11Properties registry at www.khronos.org>
type VkPhysicalDeviceVulkan11Properties =
     VkStruct VkPhysicalDeviceVulkan11Properties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceVulkan11Properties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceVulkan11Properties where
    type StructRep VkPhysicalDeviceVulkan11Properties =
         'StructMeta "VkPhysicalDeviceVulkan11Properties" -- ' closing tick for hsc2hs
           VkPhysicalDeviceVulkan11Properties
           #{size VkPhysicalDeviceVulkan11Properties}
           #{alignment VkPhysicalDeviceVulkan11Properties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceVulkan11Properties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceVulkan11Properties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceUUID" Word8 'False 
                                                   #{offset VkPhysicalDeviceVulkan11Properties, deviceUUID}
                VK_UUID_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "driverUUID" Word8 'False 
                                                   #{offset VkPhysicalDeviceVulkan11Properties, driverUUID}
                VK_UUID_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceLUID" Word8 'False 
                                                   #{offset VkPhysicalDeviceVulkan11Properties, deviceLUID}
                VK_LUID_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceNodeMask" Word32 'False 
                                                        #{offset VkPhysicalDeviceVulkan11Properties, deviceNodeMask}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "deviceLUIDValid" VkBool32 'False 
                                                           #{offset VkPhysicalDeviceVulkan11Properties, deviceLUIDValid}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subgroupSize" Word32 'False 
                                                      #{offset VkPhysicalDeviceVulkan11Properties, subgroupSize}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subgroupSupportedStages" VkShaderStageFlags 'False
                #{offset VkPhysicalDeviceVulkan11Properties, subgroupSupportedStages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subgroupSupportedOperations" VkSubgroupFeatureFlags -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan11Properties, subgroupSupportedOperations}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subgroupQuadOperationsInAllStages" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan11Properties, subgroupQuadOperationsInAllStages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pointClippingBehavior" VkPointClippingBehavior 'False
                #{offset VkPhysicalDeviceVulkan11Properties, pointClippingBehavior}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxMultiviewViewCount" Word32 'False 
                                                               #{offset VkPhysicalDeviceVulkan11Properties, maxMultiviewViewCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxMultiviewInstanceIndex" Word32 'False 
                                                                   #{offset VkPhysicalDeviceVulkan11Properties, maxMultiviewInstanceIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "protectedNoFault" VkBool32 'False 
                                                            #{offset VkPhysicalDeviceVulkan11Properties, protectedNoFault}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerSetDescriptors" Word32 'False 
                                                              #{offset VkPhysicalDeviceVulkan11Properties, maxPerSetDescriptors}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxMemoryAllocationSize" VkDeviceSize 'False 
                                                                       #{offset VkPhysicalDeviceVulkan11Properties, maxMemoryAllocationSize}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceVulkan12Features {
--   >     VkStructureTypesType;
--   >     void*    pNext;
--   >     VkBool32                         samplerMirrorClampToEdge;
--   >     VkBool32                         drawIndirectCount;
--   >     VkBool32                         storageBuffer8BitAccess;
--   >     VkBool32                         uniformAndStorageBuffer8BitAccess;
--   >     VkBool32                         storagePushConstant8;
--   >     VkBool32                         shaderBufferInt64Atomics;
--   >     VkBool32                         shaderSharedInt64Atomics;
--   >     VkBool32                         shaderFloat16;
--   >     VkBool32                         shaderInt8;
--   >     VkBool32                         descriptorIndexing;
--   >     VkBool32                         shaderInputAttachmentArrayDynamicIndexing;
--   >     VkBool32                         shaderUniformTexelBufferArrayDynamicIndexing;
--   >     VkBool32                         shaderStorageTexelBufferArrayDynamicIndexing;
--   >     VkBool32                         shaderUniformBufferArrayNonUniformIndexing;
--   >     VkBool32                         shaderSampledImageArrayNonUniformIndexing;
--   >     VkBool32                         shaderStorageBufferArrayNonUniformIndexing;
--   >     VkBool32                         shaderStorageImageArrayNonUniformIndexing;
--   >     VkBool32                         shaderInputAttachmentArrayNonUniformIndexing;
--   >     VkBool32                         shaderUniformTexelBufferArrayNonUniformIndexing;
--   >     VkBool32                         shaderStorageTexelBufferArrayNonUniformIndexing;
--   >     VkBool32                         descriptorBindingUniformBufferUpdateAfterBind;
--   >     VkBool32                         descriptorBindingSampledImageUpdateAfterBind;
--   >     VkBool32                         descriptorBindingStorageImageUpdateAfterBind;
--   >     VkBool32                         descriptorBindingStorageBufferUpdateAfterBind;
--   >     VkBool32                         descriptorBindingUniformTexelBufferUpdateAfterBind;
--   >     VkBool32                         descriptorBindingStorageTexelBufferUpdateAfterBind;
--   >     VkBool32                         descriptorBindingUpdateUnusedWhilePending;
--   >     VkBool32                         descriptorBindingPartiallyBound;
--   >     VkBool32                         descriptorBindingVariableDescriptorCount;
--   >     VkBool32                         runtimeDescriptorArray;
--   >     VkBool32                         samplerFilterMinmax;
--   >     VkBool32                         scalarBlockLayout;
--   >     VkBool32                         imagelessFramebuffer;
--   >     VkBool32                         uniformBufferStandardLayout;
--   >     VkBool32                         shaderSubgroupExtendedTypes;
--   >     VkBool32                         separateDepthStencilLayouts;
--   >     VkBool32                         hostQueryReset;
--   >     VkBool32                         timelineSemaphore;
--   >     VkBool32                         bufferDeviceAddress;
--   >     VkBool32                         bufferDeviceAddressCaptureReplay;
--   >     VkBool32                         bufferDeviceAddressMultiDevice;
--   >     VkBool32                         vulkanMemoryModel;
--   >     VkBool32                         vulkanMemoryModelDeviceScope;
--   >     VkBool32                         vulkanMemoryModelAvailabilityVisibilityChains;
--   >     VkBool32                         shaderOutputViewportIndex;
--   >     VkBool32                         shaderOutputLayer;
--   >     VkBool32                         subgroupBroadcastDynamicId;
--   > } VkPhysicalDeviceVulkan12Features;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVulkan12Features VkPhysicalDeviceVulkan12Features registry at www.khronos.org>
type VkPhysicalDeviceVulkan12Features =
     VkStruct VkPhysicalDeviceVulkan12Features' -- ' closing tick for hsc2hs

data VkPhysicalDeviceVulkan12Features' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceVulkan12Features where
    type StructRep VkPhysicalDeviceVulkan12Features =
         'StructMeta "VkPhysicalDeviceVulkan12Features" -- ' closing tick for hsc2hs
           VkPhysicalDeviceVulkan12Features
           #{size VkPhysicalDeviceVulkan12Features}
           #{alignment VkPhysicalDeviceVulkan12Features}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceVulkan12Features, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceVulkan12Features, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "samplerMirrorClampToEdge" VkBool32 'False 
                                                                    #{offset VkPhysicalDeviceVulkan12Features, samplerMirrorClampToEdge}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "drawIndirectCount" VkBool32 'False 
                                                             #{offset VkPhysicalDeviceVulkan12Features, drawIndirectCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "storageBuffer8BitAccess" VkBool32 'False 
                                                                   #{offset VkPhysicalDeviceVulkan12Features, storageBuffer8BitAccess}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "uniformAndStorageBuffer8BitAccess" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Features, uniformAndStorageBuffer8BitAccess}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "storagePushConstant8" VkBool32 'False 
                                                                #{offset VkPhysicalDeviceVulkan12Features, storagePushConstant8}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderBufferInt64Atomics" VkBool32 'False 
                                                                    #{offset VkPhysicalDeviceVulkan12Features, shaderBufferInt64Atomics}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSharedInt64Atomics" VkBool32 'False 
                                                                    #{offset VkPhysicalDeviceVulkan12Features, shaderSharedInt64Atomics}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderFloat16" VkBool32 'False 
                                                         #{offset VkPhysicalDeviceVulkan12Features, shaderFloat16}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderInt8" VkBool32 'False 
                                                      #{offset VkPhysicalDeviceVulkan12Features, shaderInt8}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorIndexing" VkBool32 'False 
                                                              #{offset VkPhysicalDeviceVulkan12Features, descriptorIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderInputAttachmentArrayDynamicIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, shaderInputAttachmentArrayDynamicIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderUniformTexelBufferArrayDynamicIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, shaderUniformTexelBufferArrayDynamicIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageTexelBufferArrayDynamicIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, shaderStorageTexelBufferArrayDynamicIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderUniformBufferArrayNonUniformIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, shaderUniformBufferArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSampledImageArrayNonUniformIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, shaderSampledImageArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageBufferArrayNonUniformIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, shaderStorageBufferArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageImageArrayNonUniformIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, shaderStorageImageArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderInputAttachmentArrayNonUniformIndexing" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, shaderInputAttachmentArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderUniformTexelBufferArrayNonUniformIndexing" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, shaderUniformTexelBufferArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageTexelBufferArrayNonUniformIndexing" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, shaderStorageTexelBufferArrayNonUniformIndexing}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingUniformBufferUpdateAfterBind" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, descriptorBindingUniformBufferUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingSampledImageUpdateAfterBind" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, descriptorBindingSampledImageUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingStorageImageUpdateAfterBind" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, descriptorBindingStorageImageUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingStorageBufferUpdateAfterBind" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, descriptorBindingStorageBufferUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingUniformTexelBufferUpdateAfterBind" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, descriptorBindingUniformTexelBufferUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingStorageTexelBufferUpdateAfterBind" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, descriptorBindingStorageTexelBufferUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingUpdateUnusedWhilePending" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, descriptorBindingUpdateUnusedWhilePending}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingPartiallyBound" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Features, descriptorBindingPartiallyBound}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "descriptorBindingVariableDescriptorCount" VkBool32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, descriptorBindingVariableDescriptorCount}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "runtimeDescriptorArray" VkBool32 'False 
                                                                  #{offset VkPhysicalDeviceVulkan12Features, runtimeDescriptorArray}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "samplerFilterMinmax" VkBool32 'False 
                                                               #{offset VkPhysicalDeviceVulkan12Features, samplerFilterMinmax}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "scalarBlockLayout" VkBool32 'False 
                                                             #{offset VkPhysicalDeviceVulkan12Features, scalarBlockLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "imagelessFramebuffer" VkBool32 'False 
                                                                #{offset VkPhysicalDeviceVulkan12Features, imagelessFramebuffer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "uniformBufferStandardLayout" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceVulkan12Features, uniformBufferStandardLayout}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSubgroupExtendedTypes" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceVulkan12Features, shaderSubgroupExtendedTypes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "separateDepthStencilLayouts" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceVulkan12Features, separateDepthStencilLayouts}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "hostQueryReset" VkBool32 'False 
                                                          #{offset VkPhysicalDeviceVulkan12Features, hostQueryReset}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "timelineSemaphore" VkBool32 'False 
                                                             #{offset VkPhysicalDeviceVulkan12Features, timelineSemaphore}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bufferDeviceAddress" VkBool32 'False 
                                                               #{offset VkPhysicalDeviceVulkan12Features, bufferDeviceAddress}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bufferDeviceAddressCaptureReplay" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Features, bufferDeviceAddressCaptureReplay}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "bufferDeviceAddressMultiDevice" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Features, bufferDeviceAddressMultiDevice}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vulkanMemoryModel" VkBool32 'False 
                                                             #{offset VkPhysicalDeviceVulkan12Features, vulkanMemoryModel}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vulkanMemoryModelDeviceScope" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Features, vulkanMemoryModelDeviceScope}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vulkanMemoryModelAvailabilityVisibilityChains" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Features, vulkanMemoryModelAvailabilityVisibilityChains}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderOutputViewportIndex" VkBool32 'False 
                                                                     #{offset VkPhysicalDeviceVulkan12Features, shaderOutputViewportIndex}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderOutputLayer" VkBool32 'False 
                                                             #{offset VkPhysicalDeviceVulkan12Features, shaderOutputLayer}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "subgroupBroadcastDynamicId" VkBool32 'False 
                                                                      #{offset VkPhysicalDeviceVulkan12Features, subgroupBroadcastDynamicId}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceVulkan12Properties {
--   >     VkStructureTypesType;
--   >     void*    pNext;
--   >     VkDriverId                       driverID;
--   >     char                             driverName[VK_MAX_DRIVER_NAME_SIZE];
--   >     char                             driverInfo[VK_MAX_DRIVER_INFO_SIZE];
--   >     VkConformanceVersion             conformanceVersion;
--   >     VkShaderFloatControlsIndependencedenormBehaviorIndependence;
--   >     VkShaderFloatControlsIndependenceroundingModeIndependence;
--   >     VkBool32                         shaderSignedZeroInfNanPreserveFloat16;
--   >     VkBool32                         shaderSignedZeroInfNanPreserveFloat32;
--   >     VkBool32                         shaderSignedZeroInfNanPreserveFloat64;
--   >     VkBool32                         shaderDenormPreserveFloat16;
--   >     VkBool32                         shaderDenormPreserveFloat32;
--   >     VkBool32                         shaderDenormPreserveFloat64;
--   >     VkBool32                         shaderDenormFlushToZeroFloat16;
--   >     VkBool32                         shaderDenormFlushToZeroFloat32;
--   >     VkBool32                         shaderDenormFlushToZeroFloat64;
--   >     VkBool32                         shaderRoundingModeRTEFloat16;
--   >     VkBool32                         shaderRoundingModeRTEFloat32;
--   >     VkBool32                         shaderRoundingModeRTEFloat64;
--   >     VkBool32                         shaderRoundingModeRTZFloat16;
--   >     VkBool32                         shaderRoundingModeRTZFloat32;
--   >     VkBool32                         shaderRoundingModeRTZFloat64;
--   >     uint32_t                         maxUpdateAfterBindDescriptorsInAllPools;
--   >     VkBool32                         shaderUniformBufferArrayNonUniformIndexingNative;
--   >     VkBool32                         shaderSampledImageArrayNonUniformIndexingNative;
--   >     VkBool32                         shaderStorageBufferArrayNonUniformIndexingNative;
--   >     VkBool32                         shaderStorageImageArrayNonUniformIndexingNative;
--   >     VkBool32                         shaderInputAttachmentArrayNonUniformIndexingNative;
--   >     VkBool32                         robustBufferAccessUpdateAfterBind;
--   >     VkBool32                         quadDivergentImplicitLod;
--   >     uint32_t                         maxPerStageDescriptorUpdateAfterBindSamplers;
--   >     uint32_t                         maxPerStageDescriptorUpdateAfterBindUniformBuffers;
--   >     uint32_t                         maxPerStageDescriptorUpdateAfterBindStorageBuffers;
--   >     uint32_t                         maxPerStageDescriptorUpdateAfterBindSampledImages;
--   >     uint32_t                         maxPerStageDescriptorUpdateAfterBindStorageImages;
--   >     uint32_t                         maxPerStageDescriptorUpdateAfterBindInputAttachments;
--   >     uint32_t                         maxPerStageUpdateAfterBindResources;
--   >     uint32_t                         maxDescriptorSetUpdateAfterBindSamplers;
--   >     uint32_t                         maxDescriptorSetUpdateAfterBindUniformBuffers;
--   >     uint32_t                         maxDescriptorSetUpdateAfterBindUniformBuffersDynamic;
--   >     uint32_t                         maxDescriptorSetUpdateAfterBindStorageBuffers;
--   >     uint32_t                         maxDescriptorSetUpdateAfterBindStorageBuffersDynamic;
--   >     uint32_t                         maxDescriptorSetUpdateAfterBindSampledImages;
--   >     uint32_t                         maxDescriptorSetUpdateAfterBindStorageImages;
--   >     uint32_t                         maxDescriptorSetUpdateAfterBindInputAttachments;
--   >     VkResolveModeFlags               supportedDepthResolveModes;
--   >     VkResolveModeFlags               supportedStencilResolveModes;
--   >     VkBool32                         independentResolveNone;
--   >     VkBool32                         independentResolve;
--   >     VkBool32                         filterMinmaxSingleComponentFormats;
--   >     VkBool32                         filterMinmaxImageComponentMapping;
--   >     uint64_t                         maxTimelineSemaphoreValueDifference;
--   >     VkSampleCountFlags framebufferIntegerColorSampleCounts;
--   > } VkPhysicalDeviceVulkan12Properties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVulkan12Properties VkPhysicalDeviceVulkan12Properties registry at www.khronos.org>
type VkPhysicalDeviceVulkan12Properties =
     VkStruct VkPhysicalDeviceVulkan12Properties' -- ' closing tick for hsc2hs

data VkPhysicalDeviceVulkan12Properties' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceVulkan12Properties where
    type StructRep VkPhysicalDeviceVulkan12Properties =
         'StructMeta "VkPhysicalDeviceVulkan12Properties" -- ' closing tick for hsc2hs
           VkPhysicalDeviceVulkan12Properties
           #{size VkPhysicalDeviceVulkan12Properties}
           #{alignment VkPhysicalDeviceVulkan12Properties}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceVulkan12Properties, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceVulkan12Properties, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "driverID" VkDriverId 'False 
                                                      #{offset VkPhysicalDeviceVulkan12Properties, driverID}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "driverName" CChar 'False 
                                                   #{offset VkPhysicalDeviceVulkan12Properties, driverName}
                VK_MAX_DRIVER_NAME_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "driverInfo" CChar 'False 
                                                   #{offset VkPhysicalDeviceVulkan12Properties, driverInfo}
                VK_MAX_DRIVER_INFO_SIZE
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "conformanceVersion" VkConformanceVersion 'False
                #{offset VkPhysicalDeviceVulkan12Properties, conformanceVersion}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "denormBehaviorIndependence" -- ' closing tick for hsc2hs
                VkShaderFloatControlsIndependence
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, denormBehaviorIndependence}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "roundingModeIndependence" -- ' closing tick for hsc2hs
                VkShaderFloatControlsIndependence
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, roundingModeIndependence}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSignedZeroInfNanPreserveFloat16" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, shaderSignedZeroInfNanPreserveFloat16}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSignedZeroInfNanPreserveFloat32" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, shaderSignedZeroInfNanPreserveFloat32}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSignedZeroInfNanPreserveFloat64" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, shaderSignedZeroInfNanPreserveFloat64}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDenormPreserveFloat16" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceVulkan12Properties, shaderDenormPreserveFloat16}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDenormPreserveFloat32" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceVulkan12Properties, shaderDenormPreserveFloat32}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDenormPreserveFloat64" VkBool32 'False 
                                                                       #{offset VkPhysicalDeviceVulkan12Properties, shaderDenormPreserveFloat64}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDenormFlushToZeroFloat16" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, shaderDenormFlushToZeroFloat16}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDenormFlushToZeroFloat32" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, shaderDenormFlushToZeroFloat32}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderDenormFlushToZeroFloat64" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, shaderDenormFlushToZeroFloat64}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderRoundingModeRTEFloat16" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, shaderRoundingModeRTEFloat16}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderRoundingModeRTEFloat32" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, shaderRoundingModeRTEFloat32}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderRoundingModeRTEFloat64" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, shaderRoundingModeRTEFloat64}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderRoundingModeRTZFloat16" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, shaderRoundingModeRTZFloat16}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderRoundingModeRTZFloat32" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, shaderRoundingModeRTZFloat32}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderRoundingModeRTZFloat64" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, shaderRoundingModeRTZFloat64}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxUpdateAfterBindDescriptorsInAllPools" Word32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, maxUpdateAfterBindDescriptorsInAllPools}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderUniformBufferArrayNonUniformIndexingNative" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, shaderUniformBufferArrayNonUniformIndexingNative}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderSampledImageArrayNonUniformIndexingNative" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, shaderSampledImageArrayNonUniformIndexingNative}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageBufferArrayNonUniformIndexingNative" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, shaderStorageBufferArrayNonUniformIndexingNative}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderStorageImageArrayNonUniformIndexingNative" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, shaderStorageImageArrayNonUniformIndexingNative}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "shaderInputAttachmentArrayNonUniformIndexingNative" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, shaderInputAttachmentArrayNonUniformIndexingNative}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "robustBufferAccessUpdateAfterBind" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, robustBufferAccessUpdateAfterBind}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "quadDivergentImplicitLod" VkBool32 'False 
                                                                    #{offset VkPhysicalDeviceVulkan12Properties, quadDivergentImplicitLod}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindSamplers" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, maxPerStageDescriptorUpdateAfterBindSamplers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindUniformBuffers" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, maxPerStageDescriptorUpdateAfterBindUniformBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindStorageBuffers" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, maxPerStageDescriptorUpdateAfterBindStorageBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindSampledImages" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, maxPerStageDescriptorUpdateAfterBindSampledImages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindStorageImages" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, maxPerStageDescriptorUpdateAfterBindStorageImages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageDescriptorUpdateAfterBindInputAttachments" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, maxPerStageDescriptorUpdateAfterBindInputAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxPerStageUpdateAfterBindResources" Word32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, maxPerStageUpdateAfterBindResources}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindSamplers" Word32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, maxDescriptorSetUpdateAfterBindSamplers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindUniformBuffers" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, maxDescriptorSetUpdateAfterBindUniformBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, maxDescriptorSetUpdateAfterBindUniformBuffersDynamic}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindStorageBuffers" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, maxDescriptorSetUpdateAfterBindStorageBuffers}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, maxDescriptorSetUpdateAfterBindStorageBuffersDynamic}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindSampledImages" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, maxDescriptorSetUpdateAfterBindSampledImages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindStorageImages" Word32 -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, maxDescriptorSetUpdateAfterBindStorageImages}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxDescriptorSetUpdateAfterBindInputAttachments" -- ' closing tick for hsc2hs
                Word32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, maxDescriptorSetUpdateAfterBindInputAttachments}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "supportedDepthResolveModes" VkResolveModeFlags 'False
                #{offset VkPhysicalDeviceVulkan12Properties, supportedDepthResolveModes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "supportedStencilResolveModes" VkResolveModeFlags -- ' closing tick for hsc2hs
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, supportedStencilResolveModes}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "independentResolveNone" VkBool32 'False 
                                                                  #{offset VkPhysicalDeviceVulkan12Properties, independentResolveNone}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "independentResolve" VkBool32 'False 
                                                              #{offset VkPhysicalDeviceVulkan12Properties, independentResolve}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "filterMinmaxSingleComponentFormats" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, filterMinmaxSingleComponentFormats}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "filterMinmaxImageComponentMapping" VkBool32 'False
                #{offset VkPhysicalDeviceVulkan12Properties, filterMinmaxImageComponentMapping}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "maxTimelineSemaphoreValueDifference" Word64 'False
                #{offset VkPhysicalDeviceVulkan12Properties, maxTimelineSemaphoreValueDifference}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "framebufferIntegerColorSampleCounts" -- ' closing tick for hsc2hs
                VkSampleCountFlags
                'True -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkan12Properties, framebufferIntegerColorSampleCounts}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'True -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

-- | > typedef struct VkPhysicalDeviceVulkanMemoryModelFeatures {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     VkBool32                         vulkanMemoryModel;
--   >     VkBool32                         vulkanMemoryModelDeviceScope;
--   >     VkBool32                         vulkanMemoryModelAvailabilityVisibilityChains;
--   > } VkPhysicalDeviceVulkanMemoryModelFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceVulkanMemoryModelFeatures VkPhysicalDeviceVulkanMemoryModelFeatures registry at www.khronos.org>
type VkPhysicalDeviceVulkanMemoryModelFeatures =
     VkStruct VkPhysicalDeviceVulkanMemoryModelFeatures' -- ' closing tick for hsc2hs

data VkPhysicalDeviceVulkanMemoryModelFeatures' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceVulkanMemoryModelFeatures
         where
    type StructRep VkPhysicalDeviceVulkanMemoryModelFeatures =
         'StructMeta "VkPhysicalDeviceVulkanMemoryModelFeatures" -- ' closing tick for hsc2hs
           VkPhysicalDeviceVulkanMemoryModelFeatures
           #{size VkPhysicalDeviceVulkanMemoryModelFeatures}
           #{alignment VkPhysicalDeviceVulkanMemoryModelFeatures}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceVulkanMemoryModelFeatures, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceVulkanMemoryModelFeatures, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vulkanMemoryModel" VkBool32 'False 
                                                             #{offset VkPhysicalDeviceVulkanMemoryModelFeatures, vulkanMemoryModel}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vulkanMemoryModelDeviceScope" VkBool32 'False
                #{offset VkPhysicalDeviceVulkanMemoryModelFeatures, vulkanMemoryModelDeviceScope}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "vulkanMemoryModelAvailabilityVisibilityChains" -- ' closing tick for hsc2hs
                VkBool32
                'False -- ' closing tick for hsc2hs
                #{offset VkPhysicalDeviceVulkanMemoryModelFeatures, vulkanMemoryModelAvailabilityVisibilityChains}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

-- | Alias for `VkPhysicalDeviceVulkanMemoryModelFeatures`
type VkPhysicalDeviceVulkanMemoryModelFeaturesKHR =
     VkPhysicalDeviceVulkanMemoryModelFeatures

-- | > typedef struct VkPhysicalDeviceYcbcrImageArraysFeaturesEXT {
--   >     VkStructureType sType;
--   >     void*        pNext;
--   >     VkBool32                           ycbcrImageArrays;
--   > } VkPhysicalDeviceYcbcrImageArraysFeaturesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPhysicalDeviceYcbcrImageArraysFeaturesEXT VkPhysicalDeviceYcbcrImageArraysFeaturesEXT registry at www.khronos.org>
type VkPhysicalDeviceYcbcrImageArraysFeaturesEXT =
     VkStruct VkPhysicalDeviceYcbcrImageArraysFeaturesEXT' -- ' closing tick for hsc2hs

data VkPhysicalDeviceYcbcrImageArraysFeaturesEXT' -- ' closing tick for hsc2hs

instance VulkanMarshal VkPhysicalDeviceYcbcrImageArraysFeaturesEXT
         where
    type StructRep VkPhysicalDeviceYcbcrImageArraysFeaturesEXT =
         'StructMeta "VkPhysicalDeviceYcbcrImageArraysFeaturesEXT" -- ' closing tick for hsc2hs
           VkPhysicalDeviceYcbcrImageArraysFeaturesEXT
           #{size VkPhysicalDeviceYcbcrImageArraysFeaturesEXT}
           #{alignment VkPhysicalDeviceYcbcrImageArraysFeaturesEXT}
           '[('FieldMeta "sType" VkStructureType 'False  -- ' closing tick for hsc2hs
                                                        #{offset VkPhysicalDeviceYcbcrImageArraysFeaturesEXT, sType}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "pNext" (Ptr Void) 'False 
                                                   #{offset VkPhysicalDeviceYcbcrImageArraysFeaturesEXT, pNext}
                1
                'True -- ' closing tick for hsc2hs
                'True), -- ' closing tick for hsc2hs
             ('FieldMeta "ycbcrImageArrays" VkBool32 'False 
                                                            #{offset VkPhysicalDeviceYcbcrImageArraysFeaturesEXT, ycbcrImageArrays}
                1
                'True -- ' closing tick for hsc2hs
                'True)] -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           'False -- ' closing tick for hsc2hs
           '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs
