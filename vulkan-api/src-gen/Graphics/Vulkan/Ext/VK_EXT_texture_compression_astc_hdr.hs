{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_texture_compression_astc_hdr
       (-- * Vulkan extension: @VK_EXT_texture_compression_astc_hdr@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jan-Harald Fredriksen @janharaldfredriksen-arm@
        --
        -- author: @ARM@
        --
        -- type: @device@
        --
        -- Extension number: @67@
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
        VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT,
        VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION,
        pattern VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION,
        VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME,
        pattern VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT,
        pattern VK_FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT,
        pattern VK_FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT,
        pattern VK_FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT,
        pattern VK_FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT,
        pattern VK_FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT,
        pattern VK_FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT,
        pattern VK_FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT,
        pattern VK_FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT,
        pattern VK_FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT,
        pattern VK_FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT,
        pattern VK_FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT,
        pattern VK_FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT,
        pattern VK_FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT,
        pattern VK_FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.Format                   (VkFormat (..))
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceFeatures2,
                                                            VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)

pattern VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION = 1

type VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION = 1

pattern VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME ::
        CString

pattern VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME <-
        (is_VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME -> True)
  where
    VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME
      = _VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME

{-# INLINE _VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME #-}

_VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME :: CString
_VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME
  = Ptr "VK_EXT_texture_compression_astc_hdr\NUL"#

{-# INLINE is_VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME
           #-}

is_VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME ::
                                                      CString -> Bool
is_VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME

type VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME =
     "VK_EXT_texture_compression_astc_hdr"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT
        = VkStructureType 1000066000

pattern VK_FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT :: VkFormat

pattern VK_FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT = VkFormat 1000066000

pattern VK_FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT :: VkFormat

pattern VK_FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT = VkFormat 1000066001

pattern VK_FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT :: VkFormat

pattern VK_FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT = VkFormat 1000066002

pattern VK_FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT :: VkFormat

pattern VK_FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT = VkFormat 1000066003

pattern VK_FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT :: VkFormat

pattern VK_FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT = VkFormat 1000066004

pattern VK_FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT :: VkFormat

pattern VK_FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT = VkFormat 1000066005

pattern VK_FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT :: VkFormat

pattern VK_FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT = VkFormat 1000066006

pattern VK_FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT :: VkFormat

pattern VK_FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT = VkFormat 1000066007

pattern VK_FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT :: VkFormat

pattern VK_FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT = VkFormat 1000066008

pattern VK_FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT :: VkFormat

pattern VK_FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT = VkFormat 1000066009

pattern VK_FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT :: VkFormat

pattern VK_FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT = VkFormat 1000066010

pattern VK_FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT :: VkFormat

pattern VK_FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT = VkFormat 1000066011

pattern VK_FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT :: VkFormat

pattern VK_FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT = VkFormat 1000066012

pattern VK_FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT :: VkFormat

pattern VK_FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT = VkFormat 1000066013
