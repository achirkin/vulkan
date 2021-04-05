{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NV_framebuffer_mixed_samples
       (AHardwareBuffer(), ANativeWindow(), CAMetalLayer(), VkBool32(..),
        VkDeviceAddress(..), VkDeviceSize(..), VkFlags(..),
        VkSampleMask(..), VkCoverageModulationModeNV(..),
        VkCoverageReductionModeNV(..), VkAndroidSurfaceCreateFlagsKHR(..),
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
        VkXlibSurfaceCreateFlagsKHR(..),
        VkPipelineCoverageModulationStateCreateInfoNV,
        VkPipelineMultisampleStateCreateInfo, VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION,
        pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION,
        VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME,
        pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV)
       where
import GHC.Ptr                                     (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Coverage
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Struct.Pipeline       (VkPipelineCoverageModulationStateCreateInfoNV,
                                                    VkPipelineMultisampleStateCreateInfo)

pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION = 1

type VK_NV_FRAMEBUFFER_MIXED_SAMPLES_SPEC_VERSION = 1

pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME :: CString

pattern VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME <-
        (is_VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME -> True)
  where
    VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME
      = _VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME

{-# INLINE _VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME #-}

_VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME :: CString
_VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME
  = Ptr "VK_NV_framebuffer_mixed_samples\NUL"#

{-# INLINE is_VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME #-}

is_VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME ::
                                                  CString -> Bool
is_VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME

type VK_NV_FRAMEBUFFER_MIXED_SAMPLES_EXTENSION_NAME =
     "VK_NV_framebuffer_mixed_samples"

pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
        = VkStructureType 1000152000
