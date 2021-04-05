{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_custom_border_color
       (-- * Vulkan extension: @VK_EXT_custom_border_color@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Liam Middlebrook @liam-middlebrook@
        --
        -- author: @EXT@
        --
        -- type: @device@
        --
        -- Extension number: @288@
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..), VkBorderColor(..),
        VkClearColorValue, VkCompareOp(..),
        VkAndroidSurfaceCreateFlagsKHR(..), VkBufferViewCreateFlags(..),
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
        VkDeviceQueueCreateInfo, VkFilter(..), VkFormat(..),
        VkFormatFeatureBitmask(..), VkFormatFeatureFlagBits(),
        VkFormatFeatureFlags(),
        VkPhysicalDeviceCustomBorderColorFeaturesEXT,
        VkPhysicalDeviceCustomBorderColorPropertiesEXT,
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        VkPhysicalDeviceLimits, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2, VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceType(..), VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(),
        VkSamplerAddressMode(..), VkSamplerMipmapMode(..),
        VkSamplerReductionMode(..), VkSamplerYcbcrModelConversion(..),
        VkSamplerYcbcrRange(..), VkSamplerCreateBitmask(..),
        VkSamplerCreateFlagBits(), VkSamplerCreateFlags(),
        VkSamplerReductionModeEXT(..),
        VkSamplerYcbcrModelConversionKHR(..), VkSamplerYcbcrRangeKHR(..),
        VkSamplerCreateInfo, VkSamplerCustomBorderColorCreateInfoEXT,
        VkStructureType(..), -- > #include "vk_platform.h"
                             VK_EXT_CUSTOM_BORDER_COLOR_SPEC_VERSION,
        pattern VK_EXT_CUSTOM_BORDER_COLOR_SPEC_VERSION,
        VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME,
        pattern VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT,
        pattern VK_BORDER_COLOR_FLOAT_CUSTOM_EXT,
        pattern VK_BORDER_COLOR_INT_CUSTOM_EXT)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.BorderColor
import Graphics.Vulkan.Types.Enum.CompareOp
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.Filter
import Graphics.Vulkan.Types.Enum.Format
import Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.Sampler
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Struct.Clear                  (VkClearColorValue)
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceCustomBorderColorFeaturesEXT,
                                                            VkPhysicalDeviceCustomBorderColorPropertiesEXT,
                                                            VkPhysicalDeviceFeatures2,
                                                            VkPhysicalDeviceLimits,
                                                            VkPhysicalDeviceProperties,
                                                            VkPhysicalDeviceProperties2,
                                                            VkPhysicalDeviceSparseProperties)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)
import Graphics.Vulkan.Types.Struct.Sampler                (VkSamplerCreateInfo, VkSamplerCustomBorderColorCreateInfoEXT)

pattern VK_EXT_CUSTOM_BORDER_COLOR_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_EXT_CUSTOM_BORDER_COLOR_SPEC_VERSION = 12

type VK_EXT_CUSTOM_BORDER_COLOR_SPEC_VERSION = 12

pattern VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME :: CString

pattern VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME <-
        (is_VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME -> True)
  where
    VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME
      = _VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME

{-# INLINE _VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME #-}

_VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME :: CString
_VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME
  = Ptr "VK_EXT_custom_border_color\NUL"#

{-# INLINE is_VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME #-}

is_VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME

type VK_EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME =
     "VK_EXT_custom_border_color"

pattern VK_STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT
        = VkStructureType 1000287000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT
        = VkStructureType 1000287001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT
        = VkStructureType 1000287002

pattern VK_BORDER_COLOR_FLOAT_CUSTOM_EXT :: VkBorderColor

pattern VK_BORDER_COLOR_FLOAT_CUSTOM_EXT = VkBorderColor 1000287003

pattern VK_BORDER_COLOR_INT_CUSTOM_EXT :: VkBorderColor

pattern VK_BORDER_COLOR_INT_CUSTOM_EXT = VkBorderColor 1000287004
