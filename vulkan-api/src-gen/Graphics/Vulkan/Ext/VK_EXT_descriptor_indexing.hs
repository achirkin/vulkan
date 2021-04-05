{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_EXT_descriptor_indexing
       (-- * Vulkan extension: @VK_EXT_descriptor_indexing@
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
        -- Extension number: @162@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_maintenance3'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_maintenance3'.
        VkDescriptorBindingBitmask(..), VkDescriptorPoolCreateBitmask(..),
        VkDescriptorType(..), VkDescriptorUpdateTemplateType(..),
        VkDescriptorBindingFlagBits(), VkDescriptorBindingFlagBitsEXT(..),
        VkDescriptorBindingFlags(), VkDescriptorPoolCreateFlagBits(),
        VkDescriptorPoolCreateFlags(),
        VkDescriptorSetLayoutCreateBitmask(..),
        VkDescriptorSetLayoutCreateFlagBits(),
        VkDescriptorSetLayoutCreateFlags(),
        VkDescriptorUpdateTemplateTypeKHR(..),
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
        VkDescriptorSetLayoutBindingFlagsCreateInfoEXT,
        VkDescriptorSetVariableDescriptorCountAllocateInfoEXT,
        VkDescriptorSetVariableDescriptorCountLayoutSupportEXT,
        VkPhysicalDeviceDescriptorIndexingFeaturesEXT,
        VkPhysicalDeviceDescriptorIndexingPropertiesEXT,
        VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION,
        pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION,
        VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME,
        pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT,
        pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT,
        pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT,
        pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT,
        pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT,
        pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT,
        pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT,
        pattern VK_ERROR_FRAGMENTATION_EXT)
       where
import GHC.Ptr                                     (Ptr (..))
import Graphics.Vulkan.Core_1_2                    (pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT,
                                                    pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT,
                                                    pattern VK_ERROR_FRAGMENTATION,
                                                    pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO,
                                                    pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO,
                                                    pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT,
                                                    pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES,
                                                    pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Descriptor
import Graphics.Vulkan.Types.Struct.Descriptor     (VkDescriptorSetLayoutBindingFlagsCreateInfoEXT,
                                                    VkDescriptorSetVariableDescriptorCountAllocateInfoEXT,
                                                    VkDescriptorSetVariableDescriptorCountLayoutSupportEXT)
import Graphics.Vulkan.Types.Struct.PhysicalDevice (VkPhysicalDeviceDescriptorIndexingFeaturesEXT,
                                                    VkPhysicalDeviceDescriptorIndexingPropertiesEXT)

pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION = 2

type VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION = 2

pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME :: CString

pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME <-
        (is_VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME -> True)
  where
    VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
      = _VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME

{-# INLINE _VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME #-}

_VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME :: CString
_VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
  = Ptr "VK_EXT_descriptor_indexing\NUL"#

{-# INLINE is_VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME #-}

is_VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME

type VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME =
     "VK_EXT_descriptor_indexing"

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT
        = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
        =
        VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
        =
        VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT

pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT =
        VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT

pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT =
        VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT

pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT =
        VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT

pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT =
        VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT

pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT =
        VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT

pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
        = VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT

pattern VK_ERROR_FRAGMENTATION_EXT = VK_ERROR_FRAGMENTATION
