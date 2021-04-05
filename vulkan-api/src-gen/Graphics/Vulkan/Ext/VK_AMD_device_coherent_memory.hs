{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_AMD_device_coherent_memory
       (-- * Vulkan extension: @VK_AMD_device_coherent_memory@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Tobias Hector @tobski@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @230@
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
        VkDeviceQueueCreateInfo, VkPhysicalDeviceCoherentMemoryFeaturesAMD,
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        VkStructureType(..), -- > #include "vk_platform.h"
                             VK_AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION,
        pattern VK_AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION,
        VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME,
        pattern VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME,
        pattern VK_MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD,
        pattern VK_MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.Memory                   (VkMemoryPropertyBitmask (..))
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceCoherentMemoryFeaturesAMD,
                                                            VkPhysicalDeviceFeatures2)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)

pattern VK_AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION = 1

type VK_AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION = 1

pattern VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME :: CString

pattern VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME <-
        (is_VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME -> True)
  where
    VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME
      = _VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME

{-# INLINE _VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME #-}

_VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME :: CString
_VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME
  = Ptr "VK_AMD_device_coherent_memory\NUL"#

{-# INLINE is_VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME #-}

is_VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME :: CString -> Bool
is_VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME

type VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME =
     "VK_AMD_device_coherent_memory"

-- | bitpos = @6@
pattern VK_MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD ::
        VkMemoryPropertyBitmask a

pattern VK_MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD =
        VkMemoryPropertyBitmask 64

-- | bitpos = @7@
pattern VK_MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD ::
        VkMemoryPropertyBitmask a

pattern VK_MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD =
        VkMemoryPropertyBitmask 128

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD
        = VkStructureType 1000229000
