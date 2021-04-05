{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_INTEL_shader_integer_functions2
       (-- * Vulkan extension: @VK_INTEL_shader_integer_functions2@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Ian Romanick @ianromanick@
        --
        -- author: @INTEL@
        --
        -- type: @device@
        --
        -- Extension number: @210@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
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
        VkDeviceQueueCreateInfo, VkPhysicalDeviceFeatures,
        VkPhysicalDeviceFeatures2,
        VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL,
        VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_SPEC_VERSION,
        pattern VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_SPEC_VERSION,
        VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME,
        pattern VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceFeatures2,
                                                            VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)

pattern VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_SPEC_VERSION = 1

type VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_SPEC_VERSION = 1

pattern VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME ::
        CString

pattern VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME <-
        (is_VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME -> True)
  where
    VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME
      = _VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME

{-# INLINE _VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME #-}

_VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME :: CString
_VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME
  = Ptr "VK_INTEL_shader_integer_functions2\NUL"#

{-# INLINE is_VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME
           #-}

is_VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME ::
                                                      CString -> Bool
is_VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME

type VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME =
     "VK_INTEL_shader_integer_functions2"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL
        = VkStructureType 1000209000
