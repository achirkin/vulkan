{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_pipeline_creation_cache_control
       (-- * Vulkan extension: @VK_EXT_pipeline_creation_cache_control@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Gregory Grebe @grgrebe_amd@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @298@
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
        VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT,
        VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION,
        pattern VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION,
        VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME,
        pattern VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT,
        pattern VK_PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT,
        pattern VK_PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT,
        pattern VK_PIPELINE_COMPILE_REQUIRED_EXT,
        pattern VK_ERROR_PIPELINE_COMPILE_REQUIRED_EXT,
        pattern VK_PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.Pipeline                 (VkPipelineCacheCreateBitmask (..),
                                                            VkPipelineCreateBitmask (..))
import Graphics.Vulkan.Types.Enum.Result                   (VkResult (..))
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceFeatures2,
                                                            VkPhysicalDevicePipelineCreationCacheControlFeaturesEXT)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)

pattern VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION = 3

type VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_SPEC_VERSION = 3

pattern VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME ::
        CString

pattern VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME <-
        (is_VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME -> True)
  where
    VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME
      = _VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME

{-# INLINE _VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME
           #-}

_VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME :: CString
_VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME
  = Ptr "VK_EXT_pipeline_creation_cache_control\NUL"#

{-# INLINE is_VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME
           #-}

is_VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME ::
                                                         CString -> Bool
is_VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME

type VK_EXT_PIPELINE_CREATION_CACHE_CONTROL_EXTENSION_NAME =
     "VK_EXT_pipeline_creation_cache_control"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT
        = VkStructureType 1000297000

-- | bitpos = @8@
pattern VK_PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT
        :: VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT
        = VkPipelineCreateBitmask 256

-- | bitpos = @9@
pattern VK_PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT ::
        VkPipelineCreateBitmask a

pattern VK_PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT =
        VkPipelineCreateBitmask 512

pattern VK_PIPELINE_COMPILE_REQUIRED_EXT :: VkResult

pattern VK_PIPELINE_COMPILE_REQUIRED_EXT = VkResult 1000297000

pattern VK_ERROR_PIPELINE_COMPILE_REQUIRED_EXT =
        VK_PIPELINE_COMPILE_REQUIRED_EXT

-- | bitpos = @0@
pattern VK_PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT ::
        VkPipelineCacheCreateBitmask a

pattern VK_PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT =
        VkPipelineCacheCreateBitmask 1
