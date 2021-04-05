{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_KHR_depth_stencil_resolve
       (-- * Vulkan extension: @VK_KHR_depth_stencil_resolve@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jan-Harald Fredriksen @janharald@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @200@
        --
        -- Required extensions: 'VK_KHR_create_renderpass2'.
        --

        -- ** Required extensions: 'VK_KHR_create_renderpass2'.
        VkPhysicalDeviceDepthStencilResolvePropertiesKHR,
        VkResolveModeBitmask(..), VkResolveModeFlagBits(),
        VkResolveModeFlagBitsKHR(..), VkResolveModeFlags(),
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
        VkSubpassDescriptionDepthStencilResolveKHR,
        VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION,
        pattern VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION,
        VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME,
        pattern VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR,
        pattern VK_RESOLVE_MODE_NONE_KHR,
        pattern VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR,
        pattern VK_RESOLVE_MODE_AVERAGE_BIT_KHR,
        pattern VK_RESOLVE_MODE_MIN_BIT_KHR,
        pattern VK_RESOLVE_MODE_MAX_BIT_KHR)
       where
import GHC.Ptr                                     (Ptr (..))
import Graphics.Vulkan.Core_1_2                    (pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES,
                                                    pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.ResolveModeFlag
import Graphics.Vulkan.Types.Struct.PhysicalDevice (VkPhysicalDeviceDepthStencilResolvePropertiesKHR)
import Graphics.Vulkan.Types.Struct.Subpass        (VkSubpassDescriptionDepthStencilResolveKHR)

pattern VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION = 1

type VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION = 1

pattern VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME :: CString

pattern VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME <-
        (is_VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME -> True)
  where
    VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME
      = _VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME

{-# INLINE _VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME #-}

_VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME :: CString
_VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME
  = Ptr "VK_KHR_depth_stencil_resolve\NUL"#

{-# INLINE is_VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME #-}

is_VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME

type VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME =
     "VK_KHR_depth_stencil_resolve"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR
        =
        VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES

pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR
        = VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE

pattern VK_RESOLVE_MODE_NONE_KHR = VK_RESOLVE_MODE_NONE

pattern VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR =
        VK_RESOLVE_MODE_SAMPLE_ZERO_BIT

pattern VK_RESOLVE_MODE_AVERAGE_BIT_KHR =
        VK_RESOLVE_MODE_AVERAGE_BIT

pattern VK_RESOLVE_MODE_MIN_BIT_KHR = VK_RESOLVE_MODE_MIN_BIT

pattern VK_RESOLVE_MODE_MAX_BIT_KHR = VK_RESOLVE_MODE_MAX_BIT
