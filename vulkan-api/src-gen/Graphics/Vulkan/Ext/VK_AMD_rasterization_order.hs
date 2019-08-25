{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_AMD_rasterization_order
       (VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkCullModeBitmask(..), VkCullModeFlagBits(), VkCullModeFlags(),
        VkFrontFace(..), VkAndroidSurfaceCreateFlagsKHR(..),
        VkBufferViewCreateFlags(..), VkCommandPoolTrimFlags(..),
        VkCommandPoolTrimFlagsKHR(..),
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
        VkPipelineRasterizationStateCreateInfo,
        VkPipelineRasterizationStateRasterizationOrderAMD,
        VkPolygonMode(..), VkRasterizationOrderAMD(..),
        VkStructureType(..), -- > #include "vk_platform.h"
                             VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION,
        pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION,
        VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME,
        pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD)
       where
import           GHC.Ptr                                          (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.CullModeFlags
import           Graphics.Vulkan.Types.Enum.FrontFace
import           Graphics.Vulkan.Types.Enum.PolygonMode
import           Graphics.Vulkan.Types.Enum.RasterizationOrderAMD
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Struct.Pipeline            (VkPipelineRasterizationStateCreateInfo,
                                                                   VkPipelineRasterizationStateRasterizationOrderAMD)

pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION = 1

type VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION = 1

pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME :: CString

pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME <-
        (is_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME -> True)
  where
    VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME
      = _VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME

{-# INLINE _VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME #-}

_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME :: CString
_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME
  = Ptr "VK_AMD_rasterization_order\NUL"#

{-# INLINE is_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME #-}

is_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME :: CString -> Bool
is_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME

type VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME =
     "VK_AMD_rasterization_order"

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
        = VkStructureType 1000018000
