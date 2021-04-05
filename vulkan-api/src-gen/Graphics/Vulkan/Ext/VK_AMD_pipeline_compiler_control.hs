{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_AMD_pipeline_compiler_control
       (-- * Vulkan extension: @VK_AMD_pipeline_compiler_control@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Matthaeus G. Chajdas @anteru@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @184@
        module Graphics.Vulkan.Marshal, VkBlendFactor(..), VkBlendOp(..),
        VkBlendOverlapEXT(..), AHardwareBuffer(), ANativeWindow(),
        CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR,
        VkColorComponentBitmask(..), VkColorSpaceKHR(..),
        VkColorComponentFlagBits(), VkColorComponentFlags(),
        VkCompareOp(..), VkComputePipelineCreateInfo,
        VkCullModeBitmask(..), VkCullModeFlagBits(), VkCullModeFlags(),
        VkDynamicState(..), VkExtent2D, VkFormat(..),
        VkFormatFeatureBitmask(..), VkFormatFeatureFlagBits(),
        VkFormatFeatureFlags(), VkFrontFace(..),
        VkGraphicsPipelineCreateInfo, VkLogicOp(..), VkOffset2D,
        VkPipelineColorBlendAttachmentState,
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
        VkXlibSurfaceCreateFlagsKHR(..),
        VkPipelineColorBlendStateCreateInfo,
        VkPipelineCompilerControlCreateInfoAMD, VkPipelineBindPoint(..),
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
        VkPipelineStageFlags(), VkPipelineDepthStencilStateCreateInfo,
        VkPipelineDynamicStateCreateInfo,
        VkPipelineInputAssemblyStateCreateInfo,
        VkPipelineMultisampleStateCreateInfo,
        VkPipelineRasterizationStateCreateInfo,
        VkPipelineShaderStageCreateInfo,
        VkPipelineTessellationStateCreateInfo,
        VkPipelineVertexInputStateCreateInfo,
        VkPipelineViewportStateCreateInfo, VkPolygonMode(..),
        VkPrimitiveTopology(..), VkRect2D, VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(),
        VkShaderFloatControlsIndependence(..), VkShaderInfoTypeAMD(..),
        VkShaderStageBitmask(..), VkShaderCorePropertiesBitmaskAMD(..),
        VkShaderCorePropertiesFlagBitsAMD(),
        VkShaderCorePropertiesFlagsAMD(),
        VkShaderFloatControlsIndependenceKHR(..),
        VkShaderModuleCreateBitmask(..), VkShaderModuleCreateFlagBits(),
        VkShaderModuleCreateFlags(), VkShaderStageFlagBits(),
        VkShaderStageFlags(), VkSpecializationInfo,
        VkSpecializationMapEntry, VkStencilFaceBitmask(..),
        VkStencilOp(..), VkStencilFaceFlagBits(), VkStencilFaceFlags(),
        VkStencilOpState, VkStructureType(..),
        VkVertexInputAttributeDescription, VkVertexInputBindingDescription,
        VkVertexInputRate(..), VkViewport,
        -- > #include "vk_platform.h"
        VK_AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION,
        pattern VK_AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION,
        VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME,
        pattern VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD)
       where
import GHC.Ptr                                                (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Blend
import Graphics.Vulkan.Types.Enum.Color
import Graphics.Vulkan.Types.Enum.CompareOp
import Graphics.Vulkan.Types.Enum.CullModeFlags
import Graphics.Vulkan.Types.Enum.DynamicState
import Graphics.Vulkan.Types.Enum.Format
import Graphics.Vulkan.Types.Enum.FrontFace
import Graphics.Vulkan.Types.Enum.LogicOp
import Graphics.Vulkan.Types.Enum.Pipeline
import Graphics.Vulkan.Types.Enum.PolygonMode
import Graphics.Vulkan.Types.Enum.PrimitiveTopology
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.Shader
import Graphics.Vulkan.Types.Enum.Stencil
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Enum.VertexInputRate
import Graphics.Vulkan.Types.Struct.ComputePipelineCreateInfo (VkComputePipelineCreateInfo)
import Graphics.Vulkan.Types.Struct.Extent                    (VkExtent2D)
import Graphics.Vulkan.Types.Struct.Offset                    (VkOffset2D)
import Graphics.Vulkan.Types.Struct.Pipeline                  (VkGraphicsPipelineCreateInfo,
                                                               VkPipelineColorBlendAttachmentState,
                                                               VkPipelineColorBlendStateCreateInfo,
                                                               VkPipelineCompilerControlCreateInfoAMD,
                                                               VkPipelineDepthStencilStateCreateInfo,
                                                               VkPipelineDynamicStateCreateInfo,
                                                               VkPipelineInputAssemblyStateCreateInfo,
                                                               VkPipelineMultisampleStateCreateInfo,
                                                               VkPipelineRasterizationStateCreateInfo,
                                                               VkPipelineShaderStageCreateInfo,
                                                               VkPipelineTessellationStateCreateInfo,
                                                               VkPipelineVertexInputStateCreateInfo,
                                                               VkPipelineViewportStateCreateInfo)
import Graphics.Vulkan.Types.Struct.Rect                      (VkRect2D)
import Graphics.Vulkan.Types.Struct.Specialization            (VkSpecializationInfo,
                                                               VkSpecializationMapEntry)
import Graphics.Vulkan.Types.Struct.StencilOpState            (VkStencilOpState)
import Graphics.Vulkan.Types.Struct.VertexInput               (VkVertexInputAttributeDescription,
                                                               VkVertexInputBindingDescription)
import Graphics.Vulkan.Types.Struct.Viewport                  (VkViewport)

pattern VK_AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION = 1

type VK_AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION = 1

pattern VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME :: CString

pattern VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME <-
        (is_VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME -> True)
  where
    VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME
      = _VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME

{-# INLINE _VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME #-}

_VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME :: CString
_VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME
  = Ptr "VK_AMD_pipeline_compiler_control\NUL"#

{-# INLINE is_VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME #-}

is_VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME ::
                                                   CString -> Bool
is_VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME

type VK_AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME =
     "VK_AMD_pipeline_compiler_control"

pattern VK_STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD
        = VkStructureType 1000183000
