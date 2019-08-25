{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NV_fragment_coverage_to_color
       (VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkAndroidSurfaceCreateFlagsKHR(..), VkBufferViewCreateFlags(..),
        VkCommandPoolTrimFlags(..), VkCommandPoolTrimFlagsKHR(..),
        VkDebugUtilsMessengerCallbackDataFlagsEXT(..),
        VkDebugUtilsMessengerCreateFlagsEXT(..),
        VkDescriptorPoolResetFlags(..),
        VkDescriptorUpdateTemplateCreateFlags(..),
        VkDescriptorUpdateTemplateCreateFlagsKHR(..),
        VkDeviceCreateFlags(..), VkDisplayModeCreateFlagsKHR(..),
        VkDisplaySurfaceCreateFlagsKHR(..), VkEventCreateFlags(..),
        VkExternalFenceFeatureFlagsKHR(..),
        VkExternalFenceHandleTypeFlagsKHR(..),
        VkExternalMemoryFeatureFlagsKHR(..),
        VkExternalMemoryHandleTypeFlagsKHR(..),
        VkExternalSemaphoreFeatureFlagsKHR(..),
        VkExternalSemaphoreHandleTypeFlagsKHR(..),
        VkFenceImportFlagsKHR(..), VkFramebufferCreateFlags(..),
        VkIOSSurfaceCreateFlagsMVK(..), VkImageViewCreateFlags(..),
        VkInstanceCreateFlags(..), VkMacOSSurfaceCreateFlagsMVK(..),
        VkMemoryAllocateFlagsKHR(..), VkMemoryMapFlags(..),
        VkMirSurfaceCreateFlagsKHR(..), VkPeerMemoryFeatureFlagsKHR(..),
        VkPipelineCacheCreateFlags(..),
        VkPipelineColorBlendStateCreateFlags(..),
        VkPipelineCoverageModulationStateCreateFlagsNV(..),
        VkPipelineCoverageToColorStateCreateFlagsNV(..),
        VkPipelineDepthStencilStateCreateFlags(..),
        VkPipelineDiscardRectangleStateCreateFlagsEXT(..),
        VkPipelineDynamicStateCreateFlags(..),
        VkPipelineInputAssemblyStateCreateFlags(..),
        VkPipelineLayoutCreateFlags(..),
        VkPipelineMultisampleStateCreateFlags(..),
        VkPipelineRasterizationConservativeStateCreateFlagsEXT(..),
        VkPipelineRasterizationStateCreateFlags(..),
        VkPipelineShaderStageCreateFlags(..),
        VkPipelineTessellationStateCreateFlags(..),
        VkPipelineVertexInputStateCreateFlags(..),
        VkPipelineViewportStateCreateFlags(..),
        VkPipelineViewportSwizzleStateCreateFlagsNV(..),
        VkQueryPoolCreateFlags(..), VkRenderPassCreateFlags(..),
        VkSamplerCreateFlags(..), VkSemaphoreCreateFlags(..),
        VkSemaphoreImportFlagsKHR(..), VkShaderModuleCreateFlags(..),
        VkValidationCacheCreateFlagsEXT(..), VkViSurfaceCreateFlagsNN(..),
        VkWaylandSurfaceCreateFlagsKHR(..),
        VkWin32SurfaceCreateFlagsKHR(..), VkXcbSurfaceCreateFlagsKHR(..),
        VkXlibSurfaceCreateFlagsKHR(..),
        VkPipelineCoverageToColorStateCreateInfoNV,
        VkPipelineMultisampleStateCreateInfo, VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION,
        pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION,
        VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME,
        pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV)
       where
import           GHC.Ptr                                     (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Struct.Pipeline       (VkPipelineCoverageToColorStateCreateInfoNV,
                                                              VkPipelineMultisampleStateCreateInfo)

pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION = 1

type VK_NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION = 1

pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME :: CString

pattern VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME <-
        (is_VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME -> True)
  where
    VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
      = _VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME

{-# INLINE _VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME #-}

_VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME :: CString
_VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
  = Ptr "VK_NV_fragment_coverage_to_color\NUL"#

{-# INLINE is_VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME #-}

is_VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME ::
                                                   CString -> Bool
is_VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME

type VK_NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME =
     "VK_NV_fragment_coverage_to_color"

pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
        = VkStructureType 1000149000
