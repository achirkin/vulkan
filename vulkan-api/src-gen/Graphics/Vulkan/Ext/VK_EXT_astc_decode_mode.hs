{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_astc_decode_mode
       (-- * Vulkan extension: @VK_EXT_astc_decode_mode@
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
        -- Extension number: @68@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkComponentMapping, VkComponentSwizzle(..), VkComponentTypeNV(..),
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
        VkDeviceQueueCreateInfo, VkFormat(..), VkFormatFeatureBitmask(..),
        VkFormatFeatureFlagBits(), VkFormatFeatureFlags(),
        VkImageAspectBitmask(..), VkImageCreateBitmask(..),
        VkImageLayout(..), VkImageTiling(..), VkImageType(..),
        VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(),
        VkImageViewCreateBitmask(..), VkImageViewCreateFlagBits(),
        VkImageViewCreateFlags(), VkImageSubresourceRange,
        VkImageViewASTCDecodeModeEXT, VkImageViewCreateInfo,
        VkPhysicalDeviceASTCDecodeFeaturesEXT, VkPhysicalDeviceFeatures,
        VkPhysicalDeviceFeatures2, VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION,
        pattern VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION,
        VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME,
        pattern VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Component
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.Format
import Graphics.Vulkan.Types.Enum.Image
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Struct.ComponentMapping       (VkComponentMapping)
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.Image                  (VkImageSubresourceRange,
                                                            VkImageViewASTCDecodeModeEXT,
                                                            VkImageViewCreateInfo)
import Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceASTCDecodeFeaturesEXT,
                                                            VkPhysicalDeviceFeatures2)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)

pattern VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION = 1

type VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION = 1

pattern VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME :: CString

pattern VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME <-
        (is_VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME -> True)
  where
    VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME
      = _VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME

{-# INLINE _VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME #-}

_VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME :: CString
_VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME
  = Ptr "VK_EXT_astc_decode_mode\NUL"#

{-# INLINE is_VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME #-}

is_VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME

type VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME =
     "VK_EXT_astc_decode_mode"

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT =
        VkStructureType 1000067000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT
        = VkStructureType 1000067001
