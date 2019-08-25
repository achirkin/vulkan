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
        module Graphics.Vulkan.Marshal, VkBool32(..), VkDeviceSize(..),
        VkFlags(..), VkSampleMask(..), VkDescriptorBindingBitmaskEXT(..),
        VkDescriptorPoolCreateBitmask(..), VkDescriptorType(..),
        VkDescriptorUpdateTemplateType(..),
        VkDescriptorBindingFlagBitsEXT(), VkDescriptorBindingFlagsEXT(),
        VkDescriptorPoolCreateFlagBits(), VkDescriptorPoolCreateFlags(),
        VkDescriptorSetLayoutCreateBitmask(..),
        VkDescriptorSetLayoutCreateFlagBits(),
        VkDescriptorSetLayoutCreateFlags(),
        VkDescriptorUpdateTemplateTypeKHR(..), VkDescriptorSetAllocateInfo,
        VkDescriptorSetLayoutBinding,
        VkDescriptorSetLayoutBindingFlagsCreateInfoEXT,
        VkDescriptorSetLayoutCreateInfo, VkDescriptorSetLayoutSupport,
        VkDescriptorSetVariableDescriptorCountAllocateInfoEXT,
        VkDescriptorSetVariableDescriptorCountLayoutSupportEXT,
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
        VkDeviceQueueCreateInfo,
        VkPhysicalDeviceDescriptorIndexingFeaturesEXT,
        VkPhysicalDeviceDescriptorIndexingPropertiesEXT,
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        VkPhysicalDeviceLimits, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2, VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceType(..), VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(),
        VkShaderInfoTypeAMD(..), VkShaderStageBitmask(..),
        VkShaderStageFlagBits(), VkShaderStageFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION,
        pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION,
        VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME,
        pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT,
        pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT,
        pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT,
        pattern VK_ERROR_FRAGMENTATION_EXT)
       where
import           GHC.Ptr                                             (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.Descriptor
import           Graphics.Vulkan.Types.Enum.Device
import           Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.Result                   (VkResult (..))
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.Shader
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Struct.Descriptor             (VkDescriptorSetAllocateInfo,
                                                                      VkDescriptorSetLayoutBinding,
                                                                      VkDescriptorSetLayoutBindingFlagsCreateInfoEXT,
                                                                      VkDescriptorSetLayoutCreateInfo,
                                                                      VkDescriptorSetLayoutSupport,
                                                                      VkDescriptorSetVariableDescriptorCountAllocateInfoEXT,
                                                                      VkDescriptorSetVariableDescriptorCountLayoutSupportEXT)
import           Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo,
                                                                      VkDeviceQueueCreateInfo)
import           Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceDescriptorIndexingFeaturesEXT,
                                                                      VkPhysicalDeviceDescriptorIndexingPropertiesEXT,
                                                                      VkPhysicalDeviceFeatures2,
                                                                      VkPhysicalDeviceLimits,
                                                                      VkPhysicalDeviceProperties,
                                                                      VkPhysicalDeviceProperties2,
                                                                      VkPhysicalDeviceSparseProperties)
import           Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)

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
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT
        = VkStructureType 1000161000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
        = VkStructureType 1000161001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
        = VkStructureType 1000161002

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
        = VkStructureType 1000161003

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
        = VkStructureType 1000161004

-- | bitpos = @1@
pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT ::
        VkDescriptorPoolCreateBitmask a

pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT =
        VkDescriptorPoolCreateBitmask 2

-- | bitpos = @1@
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
        :: VkDescriptorSetLayoutCreateBitmask a

pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
        = VkDescriptorSetLayoutCreateBitmask 2

pattern VK_ERROR_FRAGMENTATION_EXT :: VkResult

pattern VK_ERROR_FRAGMENTATION_EXT = VkResult (-1000161000)
