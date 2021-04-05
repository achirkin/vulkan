{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_fragment_density_map
       (-- * Vulkan extension: @VK_EXT_fragment_density_map@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Matthew Netsch @mnetsch@
        --
        -- author: @EXT@
        --
        -- type: @device@
        --
        -- Extension number: @219@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal, VkAccessBitmask(..),
        VkAccessFlagBits(), VkAccessFlags(), VkAttachmentDescription,
        VkAttachmentDescription2, VkAttachmentDescriptionBitmask(..),
        VkAttachmentLoadOp(..), VkAttachmentStoreOp(..),
        VkAttachmentDescriptionFlagBits(), VkAttachmentDescriptionFlags(),
        VkAttachmentReference, VkAttachmentReference2, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkDependencyBitmask(..), VkDependencyFlagBits(),
        VkDependencyFlags(), VkAndroidSurfaceCreateFlagsKHR(..),
        VkBufferViewCreateFlags(..),
        VkBuildAccelerationStructureFlagsNV(..),
        VkCommandPoolTrimFlags(..), VkCommandPoolTrimFlagsKHR(..),
        VkDebugUtilsMessengerCallbackDataFlagsEXT(..),
        VkDebugUtilsMessengerCreateFlagsEXT(..),
        VkDescriptorBindingFlagsEXT(..), VkDescriptorPoolResetFlags(..),
        VkDescriptorUpdateTemplateCreateFlags(..),
        VkDescriptorUpdateTemplateCreateFlagsKHR(..),
        VkDeviceCreateFlags(..), VkDirectFBSurfaceCreateFlagsEXT(..),
        VkDisplayModeCreateFlagsKHR(..),
        VkDisplaySurfaceCreateFlagsKHR(..), VkEventCreateFlags(..),
        VkExternalFenceFeatureFlagsKHR(..),
        VkExternalFenceHandleTypeFlagsKHR(..),
        VkExternalMemoryFeatureFlagsKHR(..),
        VkExternalMemoryHandleTypeFlagsKHR(..),
        VkExternalSemaphoreFeatureFlagsKHR(..),
        VkExternalSemaphoreHandleTypeFlagsKHR(..),
        VkFenceImportFlagsKHR(..), VkGeometryFlagsNV(..),
        VkGeometryInstanceFlagsNV(..), VkHeadlessSurfaceCreateFlagsEXT(..),
        VkIOSSurfaceCreateFlagsMVK(..),
        VkImagePipeSurfaceCreateFlagsFUCHSIA(..),
        VkInstanceCreateFlags(..), VkMacOSSurfaceCreateFlagsMVK(..),
        VkMemoryAllocateFlagsKHR(..), VkMemoryMapFlags(..),
        VkMetalSurfaceCreateFlagsEXT(..), VkPeerMemoryFeatureFlagsKHR(..),
        VkPipelineColorBlendStateCreateFlags(..),
        VkPipelineCoverageModulationStateCreateFlagsNV(..),
        VkPipelineCoverageReductionStateCreateFlagsNV(..),
        VkPipelineCoverageToColorStateCreateFlagsNV(..),
        VkPipelineDepthStencilStateCreateFlags(..),
        VkPipelineDiscardRectangleStateCreateFlagsEXT(..),
        VkPipelineDynamicStateCreateFlags(..),
        VkPipelineInputAssemblyStateCreateFlags(..),
        VkPipelineLayoutCreateFlags(..),
        VkPipelineMultisampleStateCreateFlags(..),
        VkPipelineRasterizationConservativeStateCreateFlagsEXT(..),
        VkPipelineRasterizationDepthClipStateCreateFlagsEXT(..),
        VkPipelineRasterizationStateCreateFlags(..),
        VkPipelineRasterizationStateStreamCreateFlagsEXT(..),
        VkPipelineTessellationStateCreateFlags(..),
        VkPipelineVertexInputStateCreateFlags(..),
        VkPipelineViewportStateCreateFlags(..),
        VkPipelineViewportSwizzleStateCreateFlagsNV(..),
        VkQueryPoolCreateFlags(..), VkResolveModeFlagsKHR(..),
        VkSemaphoreCreateFlags(..), VkSemaphoreImportFlagsKHR(..),
        VkSemaphoreWaitFlagsKHR(..),
        VkStreamDescriptorSurfaceCreateFlagsGGP(..),
        VkValidationCacheCreateFlagsEXT(..), VkViSurfaceCreateFlagsNN(..),
        VkWaylandSurfaceCreateFlagsKHR(..),
        VkWin32SurfaceCreateFlagsKHR(..), VkXcbSurfaceCreateFlagsKHR(..),
        VkXlibSurfaceCreateFlagsKHR(..), VkDeviceCreateInfo,
        VkDeviceDiagnosticsConfigBitmaskNV(..), VkDeviceEventTypeEXT(..),
        VkDeviceGroupPresentModeBitmaskKHR(..), VkDeviceCreateFlagBits(..),
        VkDeviceDiagnosticsConfigFlagBitsNV(),
        VkDeviceDiagnosticsConfigFlagsNV(),
        VkDeviceGroupPresentModeFlagBitsKHR(),
        VkDeviceGroupPresentModeFlagsKHR(), VkDeviceQueueCreateBitmask(..),
        VkDeviceQueueCreateFlagBits(), VkDeviceQueueCreateFlags(),
        VkDeviceQueueCreateInfo, VkExtent2D, VkFormat(..),
        VkFormatFeatureBitmask(..), VkFormatFeatureFlagBits(),
        VkFormatFeatureFlags(), VkImageAspectBitmask(..),
        VkImageCreateBitmask(..), VkImageLayout(..), VkImageTiling(..),
        VkImageType(..), VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(),
        VkImageViewCreateBitmask(..), VkImageViewCreateFlagBits(),
        VkImageViewCreateFlags(), VkPhysicalDeviceFeatures,
        VkPhysicalDeviceFeatures2,
        VkPhysicalDeviceFragmentDensityMapFeaturesEXT,
        VkPhysicalDeviceFragmentDensityMapPropertiesEXT,
        VkPhysicalDeviceLimits, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2, VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceType(..), VkPipelineBindPoint(..),
        VkPipelineCacheHeaderVersion(..), VkPipelineCreateBitmask(..),
        VkPipelineCreationFeedbackBitmaskEXT(..),
        VkPipelineExecutableStatisticFormatKHR(..),
        VkPipelineStageBitmask(..), VkPipelineCacheCreateBitmask(..),
        VkPipelineCacheCreateFlagBits(), VkPipelineCacheCreateFlags(),
        VkPipelineCompilerControlBitmaskAMD(..),
        VkPipelineCompilerControlFlagBitsAMD(),
        VkPipelineCompilerControlFlagsAMD(), VkPipelineCreateFlagBits(),
        VkPipelineCreateFlags(), VkPipelineCreationFeedbackFlagBitsEXT(),
        VkPipelineCreationFeedbackFlagsEXT(),
        VkPipelineShaderStageCreateBitmask(..),
        VkPipelineShaderStageCreateFlagBits(),
        VkPipelineShaderStageCreateFlags(), VkPipelineStageFlagBits(),
        VkPipelineStageFlags(), VkRenderPassCreateBitmask(..),
        VkRenderPassCreateFlagBits(), VkRenderPassCreateFlags(),
        VkRenderPassCreateInfo, VkRenderPassCreateInfo2,
        VkRenderPassFragmentDensityMapCreateInfoEXT,
        VkSampleCountBitmask(..), VkSampleCountFlagBits(),
        VkSampleCountFlags(), VkStructureType(..), VkSubpassDependency,
        VkSubpassDependency2, VkSubpassDescription, VkSubpassDescription2,
        VkSubpassContents(..), VkSubpassDescriptionBitmask(..),
        VkSubpassDescriptionFlagBits(), VkSubpassDescriptionFlags(),
        -- > #include "vk_platform.h"
        VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION,
        pattern VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION,
        VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME,
        pattern VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT,
        pattern VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT,
        pattern VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT,
        pattern VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT,
        pattern VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT,
        pattern VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT,
        pattern VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT,
        pattern VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT,
        pattern VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT,
        pattern VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.AccessFlags
import Graphics.Vulkan.Types.Enum.Attachment
import Graphics.Vulkan.Types.Enum.DependencyFlags
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.Format
import Graphics.Vulkan.Types.Enum.Image
import Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import Graphics.Vulkan.Types.Enum.Pipeline
import Graphics.Vulkan.Types.Enum.RenderPassCreateFlags
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.Sampler                  (VkSamplerCreateBitmask (..))
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Enum.Subpass
import Graphics.Vulkan.Types.Struct.Attachment             (VkAttachmentDescription,
                                                            VkAttachmentDescription2,
                                                            VkAttachmentReference,
                                                            VkAttachmentReference2)
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.Extent                 (VkExtent2D)
import Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceFeatures2,
                                                            VkPhysicalDeviceFragmentDensityMapFeaturesEXT,
                                                            VkPhysicalDeviceFragmentDensityMapPropertiesEXT,
                                                            VkPhysicalDeviceLimits,
                                                            VkPhysicalDeviceProperties,
                                                            VkPhysicalDeviceProperties2,
                                                            VkPhysicalDeviceSparseProperties)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)
import Graphics.Vulkan.Types.Struct.RenderPass             (VkRenderPassCreateInfo,
                                                            VkRenderPassCreateInfo2,
                                                            VkRenderPassFragmentDensityMapCreateInfoEXT)
import Graphics.Vulkan.Types.Struct.Subpass                (VkSubpassDependency,
                                                            VkSubpassDependency2,
                                                            VkSubpassDescription,
                                                            VkSubpassDescription2)

pattern VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION = 1

type VK_EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION = 1

pattern VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME :: CString

pattern VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME <-
        (is_VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME -> True)
  where
    VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
      = _VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME

{-# INLINE _VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME #-}

_VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME :: CString
_VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
  = Ptr "VK_EXT_fragment_density_map\NUL"#

{-# INLINE is_VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME #-}

is_VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME

type VK_EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME =
     "VK_EXT_fragment_density_map"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT
        = VkStructureType 1000218000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT
        = VkStructureType 1000218001

pattern VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT
        = VkStructureType 1000218002

-- | bitpos = @14@
pattern VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT ::
        VkImageCreateBitmask a

pattern VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT =
        VkImageCreateBitmask 16384

pattern VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT ::
        VkImageLayout

pattern VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT =
        VkImageLayout 1000218000

-- | bitpos = @24@
pattern VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT ::
        VkAccessBitmask a

pattern VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT =
        VkAccessBitmask 16777216

-- | bitpos = @24@
pattern VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT ::
        VkFormatFeatureBitmask a

pattern VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT =
        VkFormatFeatureBitmask 16777216

-- | bitpos = @9@
pattern VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT ::
        VkImageUsageBitmask a

pattern VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT =
        VkImageUsageBitmask 512

-- | bitpos = @0@
pattern VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
        :: VkImageViewCreateBitmask a

pattern VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT =
        VkImageViewCreateBitmask 1

-- | bitpos = @23@
pattern VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT ::
        VkPipelineStageBitmask a

pattern VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT =
        VkPipelineStageBitmask 8388608

-- | bitpos = @0@
pattern VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT ::
        VkSamplerCreateBitmask a

pattern VK_SAMPLER_CREATE_SUBSAMPLED_BIT_EXT =
        VkSamplerCreateBitmask 1

-- | bitpos = @1@
pattern VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT
        :: VkSamplerCreateBitmask a

pattern VK_SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT
        = VkSamplerCreateBitmask 2
