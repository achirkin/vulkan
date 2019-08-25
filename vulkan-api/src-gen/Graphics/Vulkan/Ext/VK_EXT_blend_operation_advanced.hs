{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_blend_operation_advanced
       (-- * Vulkan extension: @VK_EXT_blend_operation_advanced@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jeffbolznv@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @149@
        module Graphics.Vulkan.Marshal, VkBlendFactor(..), VkBlendOp(..),
        VkBlendOverlapEXT(..), VkBool32(..), VkDeviceSize(..), VkFlags(..),
        VkSampleMask(..), VkColorComponentBitmask(..), VkColorSpaceKHR(..),
        VkColorComponentFlagBits(), VkColorComponentFlags(),
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
        VkXlibSurfaceCreateFlagsKHR(..), VkDeviceCreateInfo,
        VkDeviceEventTypeEXT(..), VkDeviceGroupPresentModeBitmaskKHR(..),
        VkDeviceCreateFlagBits(..), VkDeviceGroupPresentModeFlagBitsKHR(),
        VkDeviceGroupPresentModeFlagsKHR(), VkDeviceQueueCreateBitmask(..),
        VkDeviceQueueCreateFlagBits(), VkDeviceQueueCreateFlags(),
        VkDeviceQueueCreateInfo, VkLogicOp(..),
        VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT,
        VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT,
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        VkPhysicalDeviceLimits, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2, VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceType(..),
        VkPipelineColorBlendAdvancedStateCreateInfoEXT,
        VkPipelineColorBlendAttachmentState,
        VkPipelineColorBlendStateCreateInfo, VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION,
        pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION,
        VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME,
        pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT,
        pattern VK_BLEND_OP_ZERO_EXT, pattern VK_BLEND_OP_SRC_EXT,
        pattern VK_BLEND_OP_DST_EXT, pattern VK_BLEND_OP_SRC_OVER_EXT,
        pattern VK_BLEND_OP_DST_OVER_EXT, pattern VK_BLEND_OP_SRC_IN_EXT,
        pattern VK_BLEND_OP_DST_IN_EXT, pattern VK_BLEND_OP_SRC_OUT_EXT,
        pattern VK_BLEND_OP_DST_OUT_EXT, pattern VK_BLEND_OP_SRC_ATOP_EXT,
        pattern VK_BLEND_OP_DST_ATOP_EXT, pattern VK_BLEND_OP_XOR_EXT,
        pattern VK_BLEND_OP_MULTIPLY_EXT, pattern VK_BLEND_OP_SCREEN_EXT,
        pattern VK_BLEND_OP_OVERLAY_EXT, pattern VK_BLEND_OP_DARKEN_EXT,
        pattern VK_BLEND_OP_LIGHTEN_EXT,
        pattern VK_BLEND_OP_COLORDODGE_EXT,
        pattern VK_BLEND_OP_COLORBURN_EXT,
        pattern VK_BLEND_OP_HARDLIGHT_EXT,
        pattern VK_BLEND_OP_SOFTLIGHT_EXT,
        pattern VK_BLEND_OP_DIFFERENCE_EXT,
        pattern VK_BLEND_OP_EXCLUSION_EXT, pattern VK_BLEND_OP_INVERT_EXT,
        pattern VK_BLEND_OP_INVERT_RGB_EXT,
        pattern VK_BLEND_OP_LINEARDODGE_EXT,
        pattern VK_BLEND_OP_LINEARBURN_EXT,
        pattern VK_BLEND_OP_VIVIDLIGHT_EXT,
        pattern VK_BLEND_OP_LINEARLIGHT_EXT,
        pattern VK_BLEND_OP_PINLIGHT_EXT, pattern VK_BLEND_OP_HARDMIX_EXT,
        pattern VK_BLEND_OP_HSL_HUE_EXT,
        pattern VK_BLEND_OP_HSL_SATURATION_EXT,
        pattern VK_BLEND_OP_HSL_COLOR_EXT,
        pattern VK_BLEND_OP_HSL_LUMINOSITY_EXT,
        pattern VK_BLEND_OP_PLUS_EXT, pattern VK_BLEND_OP_PLUS_CLAMPED_EXT,
        pattern VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT,
        pattern VK_BLEND_OP_PLUS_DARKER_EXT, pattern VK_BLEND_OP_MINUS_EXT,
        pattern VK_BLEND_OP_MINUS_CLAMPED_EXT,
        pattern VK_BLEND_OP_CONTRAST_EXT,
        pattern VK_BLEND_OP_INVERT_OVG_EXT, pattern VK_BLEND_OP_RED_EXT,
        pattern VK_BLEND_OP_GREEN_EXT, pattern VK_BLEND_OP_BLUE_EXT,
        pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT)
       where
import           GHC.Ptr                                             (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.AccessFlags              (VkAccessBitmask (..))
import           Graphics.Vulkan.Types.Enum.Blend
import           Graphics.Vulkan.Types.Enum.Color
import           Graphics.Vulkan.Types.Enum.Device
import           Graphics.Vulkan.Types.Enum.LogicOp
import           Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo,
                                                                      VkDeviceQueueCreateInfo)
import           Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT,
                                                                      VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT,
                                                                      VkPhysicalDeviceFeatures2,
                                                                      VkPhysicalDeviceLimits,
                                                                      VkPhysicalDeviceProperties,
                                                                      VkPhysicalDeviceProperties2,
                                                                      VkPhysicalDeviceSparseProperties)
import           Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)
import           Graphics.Vulkan.Types.Struct.Pipeline               (VkPipelineColorBlendAdvancedStateCreateInfoEXT,
                                                                      VkPipelineColorBlendAttachmentState,
                                                                      VkPipelineColorBlendStateCreateInfo)

pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION = 2

type VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION = 2

pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME :: CString

pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME <-
        (is_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME -> True)
  where
    VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
      = _VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME

{-# INLINE _VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME #-}

_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME :: CString
_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
  = Ptr "VK_EXT_blend_operation_advanced\NUL"#

{-# INLINE is_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME #-}

is_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME ::
                                                  CString -> Bool
is_VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME

type VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME =
     "VK_EXT_blend_operation_advanced"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
        = VkStructureType 1000148000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
        = VkStructureType 1000148001

pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
        = VkStructureType 1000148002

pattern VK_BLEND_OP_ZERO_EXT :: VkBlendOp

pattern VK_BLEND_OP_ZERO_EXT = VkBlendOp 1000148000

pattern VK_BLEND_OP_SRC_EXT :: VkBlendOp

pattern VK_BLEND_OP_SRC_EXT = VkBlendOp 1000148001

pattern VK_BLEND_OP_DST_EXT :: VkBlendOp

pattern VK_BLEND_OP_DST_EXT = VkBlendOp 1000148002

pattern VK_BLEND_OP_SRC_OVER_EXT :: VkBlendOp

pattern VK_BLEND_OP_SRC_OVER_EXT = VkBlendOp 1000148003

pattern VK_BLEND_OP_DST_OVER_EXT :: VkBlendOp

pattern VK_BLEND_OP_DST_OVER_EXT = VkBlendOp 1000148004

pattern VK_BLEND_OP_SRC_IN_EXT :: VkBlendOp

pattern VK_BLEND_OP_SRC_IN_EXT = VkBlendOp 1000148005

pattern VK_BLEND_OP_DST_IN_EXT :: VkBlendOp

pattern VK_BLEND_OP_DST_IN_EXT = VkBlendOp 1000148006

pattern VK_BLEND_OP_SRC_OUT_EXT :: VkBlendOp

pattern VK_BLEND_OP_SRC_OUT_EXT = VkBlendOp 1000148007

pattern VK_BLEND_OP_DST_OUT_EXT :: VkBlendOp

pattern VK_BLEND_OP_DST_OUT_EXT = VkBlendOp 1000148008

pattern VK_BLEND_OP_SRC_ATOP_EXT :: VkBlendOp

pattern VK_BLEND_OP_SRC_ATOP_EXT = VkBlendOp 1000148009

pattern VK_BLEND_OP_DST_ATOP_EXT :: VkBlendOp

pattern VK_BLEND_OP_DST_ATOP_EXT = VkBlendOp 1000148010

pattern VK_BLEND_OP_XOR_EXT :: VkBlendOp

pattern VK_BLEND_OP_XOR_EXT = VkBlendOp 1000148011

pattern VK_BLEND_OP_MULTIPLY_EXT :: VkBlendOp

pattern VK_BLEND_OP_MULTIPLY_EXT = VkBlendOp 1000148012

pattern VK_BLEND_OP_SCREEN_EXT :: VkBlendOp

pattern VK_BLEND_OP_SCREEN_EXT = VkBlendOp 1000148013

pattern VK_BLEND_OP_OVERLAY_EXT :: VkBlendOp

pattern VK_BLEND_OP_OVERLAY_EXT = VkBlendOp 1000148014

pattern VK_BLEND_OP_DARKEN_EXT :: VkBlendOp

pattern VK_BLEND_OP_DARKEN_EXT = VkBlendOp 1000148015

pattern VK_BLEND_OP_LIGHTEN_EXT :: VkBlendOp

pattern VK_BLEND_OP_LIGHTEN_EXT = VkBlendOp 1000148016

pattern VK_BLEND_OP_COLORDODGE_EXT :: VkBlendOp

pattern VK_BLEND_OP_COLORDODGE_EXT = VkBlendOp 1000148017

pattern VK_BLEND_OP_COLORBURN_EXT :: VkBlendOp

pattern VK_BLEND_OP_COLORBURN_EXT = VkBlendOp 1000148018

pattern VK_BLEND_OP_HARDLIGHT_EXT :: VkBlendOp

pattern VK_BLEND_OP_HARDLIGHT_EXT = VkBlendOp 1000148019

pattern VK_BLEND_OP_SOFTLIGHT_EXT :: VkBlendOp

pattern VK_BLEND_OP_SOFTLIGHT_EXT = VkBlendOp 1000148020

pattern VK_BLEND_OP_DIFFERENCE_EXT :: VkBlendOp

pattern VK_BLEND_OP_DIFFERENCE_EXT = VkBlendOp 1000148021

pattern VK_BLEND_OP_EXCLUSION_EXT :: VkBlendOp

pattern VK_BLEND_OP_EXCLUSION_EXT = VkBlendOp 1000148022

pattern VK_BLEND_OP_INVERT_EXT :: VkBlendOp

pattern VK_BLEND_OP_INVERT_EXT = VkBlendOp 1000148023

pattern VK_BLEND_OP_INVERT_RGB_EXT :: VkBlendOp

pattern VK_BLEND_OP_INVERT_RGB_EXT = VkBlendOp 1000148024

pattern VK_BLEND_OP_LINEARDODGE_EXT :: VkBlendOp

pattern VK_BLEND_OP_LINEARDODGE_EXT = VkBlendOp 1000148025

pattern VK_BLEND_OP_LINEARBURN_EXT :: VkBlendOp

pattern VK_BLEND_OP_LINEARBURN_EXT = VkBlendOp 1000148026

pattern VK_BLEND_OP_VIVIDLIGHT_EXT :: VkBlendOp

pattern VK_BLEND_OP_VIVIDLIGHT_EXT = VkBlendOp 1000148027

pattern VK_BLEND_OP_LINEARLIGHT_EXT :: VkBlendOp

pattern VK_BLEND_OP_LINEARLIGHT_EXT = VkBlendOp 1000148028

pattern VK_BLEND_OP_PINLIGHT_EXT :: VkBlendOp

pattern VK_BLEND_OP_PINLIGHT_EXT = VkBlendOp 1000148029

pattern VK_BLEND_OP_HARDMIX_EXT :: VkBlendOp

pattern VK_BLEND_OP_HARDMIX_EXT = VkBlendOp 1000148030

pattern VK_BLEND_OP_HSL_HUE_EXT :: VkBlendOp

pattern VK_BLEND_OP_HSL_HUE_EXT = VkBlendOp 1000148031

pattern VK_BLEND_OP_HSL_SATURATION_EXT :: VkBlendOp

pattern VK_BLEND_OP_HSL_SATURATION_EXT = VkBlendOp 1000148032

pattern VK_BLEND_OP_HSL_COLOR_EXT :: VkBlendOp

pattern VK_BLEND_OP_HSL_COLOR_EXT = VkBlendOp 1000148033

pattern VK_BLEND_OP_HSL_LUMINOSITY_EXT :: VkBlendOp

pattern VK_BLEND_OP_HSL_LUMINOSITY_EXT = VkBlendOp 1000148034

pattern VK_BLEND_OP_PLUS_EXT :: VkBlendOp

pattern VK_BLEND_OP_PLUS_EXT = VkBlendOp 1000148035

pattern VK_BLEND_OP_PLUS_CLAMPED_EXT :: VkBlendOp

pattern VK_BLEND_OP_PLUS_CLAMPED_EXT = VkBlendOp 1000148036

pattern VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT :: VkBlendOp

pattern VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT = VkBlendOp 1000148037

pattern VK_BLEND_OP_PLUS_DARKER_EXT :: VkBlendOp

pattern VK_BLEND_OP_PLUS_DARKER_EXT = VkBlendOp 1000148038

pattern VK_BLEND_OP_MINUS_EXT :: VkBlendOp

pattern VK_BLEND_OP_MINUS_EXT = VkBlendOp 1000148039

pattern VK_BLEND_OP_MINUS_CLAMPED_EXT :: VkBlendOp

pattern VK_BLEND_OP_MINUS_CLAMPED_EXT = VkBlendOp 1000148040

pattern VK_BLEND_OP_CONTRAST_EXT :: VkBlendOp

pattern VK_BLEND_OP_CONTRAST_EXT = VkBlendOp 1000148041

pattern VK_BLEND_OP_INVERT_OVG_EXT :: VkBlendOp

pattern VK_BLEND_OP_INVERT_OVG_EXT = VkBlendOp 1000148042

pattern VK_BLEND_OP_RED_EXT :: VkBlendOp

pattern VK_BLEND_OP_RED_EXT = VkBlendOp 1000148043

pattern VK_BLEND_OP_GREEN_EXT :: VkBlendOp

pattern VK_BLEND_OP_GREEN_EXT = VkBlendOp 1000148044

pattern VK_BLEND_OP_BLUE_EXT :: VkBlendOp

pattern VK_BLEND_OP_BLUE_EXT = VkBlendOp 1000148045

-- | bitpos = @19@
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT ::
        VkAccessBitmask a

pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT =
        VkAccessBitmask 524288
