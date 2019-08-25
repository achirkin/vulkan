{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_conservative_rasterization
       (-- * Vulkan extension: @VK_EXT_conservative_rasterization@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Piers Daniell @pdaniell-nv@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @102@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal, VkBool32(..), VkDeviceSize(..),
        VkFlags(..), VkSampleMask(..),
        VkConservativeRasterizationModeEXT(..), VkCullModeBitmask(..),
        VkCullModeFlagBits(), VkCullModeFlags(), VkFrontFace(..),
        VkPhysicalDeviceConservativeRasterizationPropertiesEXT,
        VkPhysicalDeviceLimits, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2, VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceType(..), VkAndroidSurfaceCreateFlagsKHR(..),
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
        VkPipelineRasterizationConservativeStateCreateInfoEXT,
        VkPipelineRasterizationStateCreateInfo, VkPolygonMode(..),
        VkSampleCountBitmask(..), VkSampleCountFlagBits(),
        VkSampleCountFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION,
        pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION,
        VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME,
        pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT)
       where
import           GHC.Ptr                                                     (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.ConservativeRasterizationModeEXT
import           Graphics.Vulkan.Types.Enum.CullModeFlags
import           Graphics.Vulkan.Types.Enum.FrontFace
import           Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.PolygonMode
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Struct.PhysicalDevice                 (VkPhysicalDeviceConservativeRasterizationPropertiesEXT,
                                                                              VkPhysicalDeviceLimits,
                                                                              VkPhysicalDeviceProperties,
                                                                              VkPhysicalDeviceProperties2,
                                                                              VkPhysicalDeviceSparseProperties)
import           Graphics.Vulkan.Types.Struct.Pipeline                       (VkPipelineRasterizationConservativeStateCreateInfoEXT,
                                                                              VkPipelineRasterizationStateCreateInfo)

pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1

type VK_EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION = 1

pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME :: CString

pattern VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME <-
        (is_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME -> True)
  where
    VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
      = _VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME

{-# INLINE _VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME #-}

_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME :: CString
_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
  = Ptr "VK_EXT_conservative_rasterization\NUL"#

{-# INLINE is_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME #-}

is_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME ::
                                                    CString -> Bool
is_VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME

type VK_EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME =
     "VK_EXT_conservative_rasterization"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
        = VkStructureType 1000101000

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
        = VkStructureType 1000101001
