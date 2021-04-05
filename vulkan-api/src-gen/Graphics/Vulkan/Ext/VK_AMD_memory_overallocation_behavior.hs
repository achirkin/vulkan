{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_AMD_memory_overallocation_behavior
       (-- * Vulkan extension: @VK_AMD_memory_overallocation_behavior@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Martin Dinkov @mdinkov@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @190@
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
        VkDeviceMemoryOverallocationCreateInfoAMD,
        VkDeviceDiagnosticsConfigBitmaskNV(..), VkDeviceEventTypeEXT(..),
        VkDeviceGroupPresentModeBitmaskKHR(..), VkDeviceCreateFlagBits(..),
        VkDeviceDiagnosticsConfigFlagBitsNV(),
        VkDeviceDiagnosticsConfigFlagsNV(),
        VkDeviceGroupPresentModeFlagBitsKHR(),
        VkDeviceGroupPresentModeFlagsKHR(), VkDeviceQueueCreateBitmask(..),
        VkDeviceQueueCreateFlagBits(), VkDeviceQueueCreateFlags(),
        VkDeviceQueueCreateInfo, VkMemoryAllocateBitmask(..),
        VkMemoryHeapBitmask(..), VkMemoryOverallocationBehaviorAMD(..),
        VkMemoryPropertyBitmask(..), VkMemoryAllocateFlagBits(),
        VkMemoryAllocateFlagBitsKHR(..), VkMemoryAllocateFlags(),
        VkMemoryHeapFlagBits(), VkMemoryHeapFlags(),
        VkMemoryPropertyFlagBits(), VkMemoryPropertyFlags(),
        VkPhysicalDeviceFeatures, VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION,
        pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION,
        VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME,
        pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.Memory
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceMemoryOverallocationCreateInfoAMD,
                                                            VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)

pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION = 1

type VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION = 1

pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME ::
        CString

pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME <-
        (is_VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME -> True)
  where
    VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME
      = _VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME

{-# INLINE _VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME
           #-}

_VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME :: CString
_VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME
  = Ptr "VK_AMD_memory_overallocation_behavior\NUL"#

{-# INLINE is_VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME
           #-}

is_VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME ::
                                                        CString -> Bool
is_VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME

type VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME =
     "VK_AMD_memory_overallocation_behavior"

pattern VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD
        = VkStructureType 1000189000
