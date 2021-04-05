{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_inline_uniform_block
       (-- * Vulkan extension: @VK_EXT_inline_uniform_block@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Rakos @aqnuep@
        --
        -- author: @EXT@
        --
        -- type: @device@
        --
        -- Extension number: @139@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_maintenance1'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_maintenance1'.
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkDescriptorBufferInfo, VkDescriptorImageInfo,
        VkDescriptorBindingBitmask(..), VkDescriptorPoolCreateBitmask(..),
        VkDescriptorType(..), VkDescriptorUpdateTemplateType(..),
        VkDescriptorBindingFlagBits(), VkDescriptorBindingFlagBitsEXT(..),
        VkDescriptorBindingFlags(), VkDescriptorPoolCreateFlagBits(),
        VkDescriptorPoolCreateFlags(),
        VkDescriptorSetLayoutCreateBitmask(..),
        VkDescriptorSetLayoutCreateFlagBits(),
        VkDescriptorSetLayoutCreateFlags(),
        VkDescriptorUpdateTemplateTypeKHR(..), VkDescriptorPoolCreateInfo,
        VkDescriptorPoolInlineUniformBlockCreateInfoEXT,
        VkDescriptorPoolSize, VkAndroidSurfaceCreateFlagsKHR(..),
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
        VkXlibSurfaceCreateFlagsKHR(..), VkDeviceCreateInfo,
        VkDeviceDiagnosticsConfigBitmaskNV(..), VkDeviceEventTypeEXT(..),
        VkDeviceGroupPresentModeBitmaskKHR(..), VkDeviceCreateFlagBits(..),
        VkDeviceDiagnosticsConfigFlagBitsNV(),
        VkDeviceDiagnosticsConfigFlagsNV(),
        VkDeviceGroupPresentModeFlagBitsKHR(),
        VkDeviceGroupPresentModeFlagsKHR(), VkDeviceQueueCreateBitmask(..),
        VkDeviceQueueCreateFlagBits(), VkDeviceQueueCreateFlags(),
        VkDeviceQueueCreateInfo, VkImageAspectBitmask(..),
        VkImageCreateBitmask(..), VkImageLayout(..), VkImageTiling(..),
        VkImageType(..), VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(),
        VkImageViewCreateBitmask(..), VkImageViewCreateFlagBits(),
        VkImageViewCreateFlags(), VkPhysicalDeviceFeatures,
        VkPhysicalDeviceFeatures2,
        VkPhysicalDeviceInlineUniformBlockFeaturesEXT,
        VkPhysicalDeviceInlineUniformBlockPropertiesEXT,
        VkPhysicalDeviceLimits, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2, VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceType(..), VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(), VkStructureType(..),
        VkWriteDescriptorSet, VkWriteDescriptorSetInlineUniformBlockEXT,
        -- > #include "vk_platform.h"
        VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION,
        pattern VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION,
        VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME,
        pattern VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME,
        pattern VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Descriptor
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.Image
import Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import Graphics.Vulkan.Types.Enum.SampleCountFlags
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Struct.Descriptor             (VkDescriptorBufferInfo,
                                                            VkDescriptorImageInfo,
                                                            VkDescriptorPoolCreateInfo,
                                                            VkDescriptorPoolInlineUniformBlockCreateInfoEXT,
                                                            VkDescriptorPoolSize)
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceFeatures2,
                                                            VkPhysicalDeviceInlineUniformBlockFeaturesEXT,
                                                            VkPhysicalDeviceInlineUniformBlockPropertiesEXT,
                                                            VkPhysicalDeviceLimits,
                                                            VkPhysicalDeviceProperties,
                                                            VkPhysicalDeviceProperties2,
                                                            VkPhysicalDeviceSparseProperties)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)
import Graphics.Vulkan.Types.Struct.WriteDescriptorSet     (VkWriteDescriptorSet,
                                                            VkWriteDescriptorSetInlineUniformBlockEXT)

pattern VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION = 1

type VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION = 1

pattern VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME :: CString

pattern VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME <-
        (is_VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME -> True)
  where
    VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME
      = _VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME

{-# INLINE _VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME #-}

_VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME :: CString
_VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME
  = Ptr "VK_EXT_inline_uniform_block\NUL"#

{-# INLINE is_VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME #-}

is_VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME

type VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME =
     "VK_EXT_inline_uniform_block"

pattern VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT ::
        VkDescriptorType

pattern VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT =
        VkDescriptorType 1000138000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT
        = VkStructureType 1000138000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT
        = VkStructureType 1000138001

pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT
        = VkStructureType 1000138002

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT
        = VkStructureType 1000138003
