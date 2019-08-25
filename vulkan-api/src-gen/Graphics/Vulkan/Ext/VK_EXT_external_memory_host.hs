{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_external_memory_host
       (-- * Vulkan extension: @VK_EXT_external_memory_host@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Rakos @drakos-amd@
        --
        -- author: @EXT@
        --
        -- type: @device@
        --
        -- Extension number: @179@
        --
        -- Required extensions: 'VK_KHR_external_memory'.
        --

        -- ** Required extensions: 'VK_KHR_external_memory'.
        module Graphics.Vulkan.Marshal, VkBool32(..), VkDeviceSize(..),
        VkFlags(..), VkSampleMask(..), VkExternalFenceFeatureBitmask(..),
        VkExternalFenceHandleTypeBitmask(..),
        VkExternalMemoryFeatureBitmask(..),
        VkExternalMemoryFeatureBitmaskNV(..),
        VkExternalMemoryHandleTypeBitmaskNV(..),
        VkExternalMemoryHandleTypeBitmask(..),
        VkExternalSemaphoreFeatureBitmask(..),
        VkExternalSemaphoreHandleTypeBitmask(..),
        VkExternalFenceFeatureFlagBits(),
        VkExternalFenceFeatureFlagBitsKHR(..),
        VkExternalFenceFeatureFlags(), VkExternalFenceHandleTypeFlagBits(),
        VkExternalFenceHandleTypeFlagBitsKHR(..),
        VkExternalFenceHandleTypeFlags(),
        VkExternalMemoryFeatureFlagBits(),
        VkExternalMemoryFeatureFlagBitsKHR(..),
        VkExternalMemoryFeatureFlagBitsNV(),
        VkExternalMemoryFeatureFlags(), VkExternalMemoryFeatureFlagsNV(),
        VkExternalMemoryHandleTypeFlagBits(),
        VkExternalMemoryHandleTypeFlagBitsKHR(..),
        VkExternalMemoryHandleTypeFlagBitsNV(),
        VkExternalMemoryHandleTypeFlags(),
        VkExternalMemoryHandleTypeFlagsNV(),
        VkExternalSemaphoreFeatureFlagBits(),
        VkExternalSemaphoreFeatureFlagBitsKHR(..),
        VkExternalSemaphoreFeatureFlags(),
        VkExternalSemaphoreHandleTypeFlagBits(),
        VkExternalSemaphoreHandleTypeFlagBitsKHR(..),
        VkExternalSemaphoreHandleTypeFlags(),
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
        VkXlibSurfaceCreateFlagsKHR(..), VkImportMemoryHostPointerInfoEXT,
        VkMemoryAllocateInfo, VkMemoryHostPointerPropertiesEXT,
        VkPhysicalDeviceExternalMemoryHostPropertiesEXT,
        VkPhysicalDeviceLimits, VkPhysicalDeviceProperties,
        VkPhysicalDeviceProperties2, VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceType(..), VkSampleCountBitmask(..),
        VkSampleCountFlagBits(), VkSampleCountFlags(), VkStructureType(..),
        -- > #include "vk_platform.h"
        VkGetMemoryHostPointerPropertiesEXT,
        pattern VkGetMemoryHostPointerPropertiesEXT,
        HS_vkGetMemoryHostPointerPropertiesEXT,
        PFN_vkGetMemoryHostPointerPropertiesEXT, VkResult(..), VkBuffer,
        VkBufferView, VkBufferView_T(), VkBuffer_T(), VkCommandBuffer,
        VkCommandBuffer_T(), VkCommandPool, VkCommandPool_T(),
        VkDebugReportCallbackEXT, VkDebugReportCallbackEXT_T(),
        VkDebugUtilsMessengerEXT, VkDebugUtilsMessengerEXT_T(),
        VkDescriptorPool, VkDescriptorPool_T(), VkDescriptorSet,
        VkDescriptorSetLayout, VkDescriptorSetLayout_T(),
        VkDescriptorSet_T(), VkDescriptorUpdateTemplate,
        VkDescriptorUpdateTemplateKHR, VkDescriptorUpdateTemplateKHR_T(),
        VkDescriptorUpdateTemplate_T(), VkDevice, VkDeviceMemory,
        VkDeviceMemory_T(), VkDevice_T(), VkDisplayKHR, VkDisplayKHR_T(),
        VkDisplayModeKHR, VkDisplayModeKHR_T(), VkEvent, VkEvent_T(),
        VkFence, VkFence_T(), VkFramebuffer, VkFramebuffer_T(), VkImage,
        VkImageView, VkImageView_T(), VkImage_T(),
        VkIndirectCommandsLayoutNVX, VkIndirectCommandsLayoutNVX_T(),
        VkInstance, VkInstance_T(), VkObjectTableNVX, VkObjectTableNVX_T(),
        VkPhysicalDevice, VkPhysicalDevice_T(), VkPipeline,
        VkPipelineCache, VkPipelineCache_T(), VkPipelineLayout,
        VkPipelineLayout_T(), VkPipeline_T(), VkQueryPool, VkQueryPool_T(),
        VkQueue, VkQueue_T(), VkRenderPass, VkRenderPass_T(), VkSampler,
        VkSamplerYcbcrConversion, VkSamplerYcbcrConversionKHR,
        VkSamplerYcbcrConversionKHR_T(), VkSamplerYcbcrConversion_T(),
        VkSampler_T(), VkSemaphore, VkSemaphore_T(), VkShaderModule,
        VkShaderModule_T(), VkSurfaceKHR, VkSurfaceKHR_T(), VkSwapchainKHR,
        VkSwapchainKHR_T(), VkValidationCacheEXT, VkValidationCacheEXT_T(),
        VkMemoryAllocateFlagsInfo, VkMemoryAllocateFlagsInfoKHR,
        VkMemoryBarrier, VkMemoryDedicatedAllocateInfo,
        VkMemoryDedicatedAllocateInfoKHR, VkMemoryDedicatedRequirements,
        VkMemoryDedicatedRequirementsKHR, VkMemoryFdPropertiesKHR,
        VkMemoryGetFdInfoKHR, VkMemoryHeap, VkMemoryRequirements,
        VkMemoryRequirements2, VkMemoryRequirements2KHR, VkMemoryType,
        VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION,
        pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION,
        VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME,
        pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT)
       where
import           GHC.Ptr                                       (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                  (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.External
import           Graphics.Vulkan.Types.Enum.PhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Import           (VkImportMemoryHostPointerInfoEXT)
import           Graphics.Vulkan.Types.Struct.Memory
import           Graphics.Vulkan.Types.Struct.PhysicalDevice   (VkPhysicalDeviceExternalMemoryHostPropertiesEXT,
                                                                VkPhysicalDeviceLimits,
                                                                VkPhysicalDeviceProperties,
                                                                VkPhysicalDeviceProperties2,
                                                                VkPhysicalDeviceSparseProperties)

pattern VkGetMemoryHostPointerPropertiesEXT :: CString

pattern VkGetMemoryHostPointerPropertiesEXT <-
        (is_VkGetMemoryHostPointerPropertiesEXT -> True)
  where
    VkGetMemoryHostPointerPropertiesEXT
      = _VkGetMemoryHostPointerPropertiesEXT

{-# INLINE _VkGetMemoryHostPointerPropertiesEXT #-}

_VkGetMemoryHostPointerPropertiesEXT :: CString
_VkGetMemoryHostPointerPropertiesEXT
  = Ptr "vkGetMemoryHostPointerPropertiesEXT\NUL"#

{-# INLINE is_VkGetMemoryHostPointerPropertiesEXT #-}

is_VkGetMemoryHostPointerPropertiesEXT :: CString -> Bool
is_VkGetMemoryHostPointerPropertiesEXT
  = (EQ ==) . cmpCStrings _VkGetMemoryHostPointerPropertiesEXT

type VkGetMemoryHostPointerPropertiesEXT =
     "vkGetMemoryHostPointerPropertiesEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_INVALID_EXTERNAL_HANDLE'.
--
--   > VkResult vkGetMemoryHostPointerPropertiesEXT
--   >     ( VkDevice device
--   >     , VkExternalMemoryHandleTypeFlagBits handleType
--   >     , const void* pHostPointer
--   >     , VkMemoryHostPointerPropertiesEXT* pMemoryHostPointerProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetMemoryHostPointerPropertiesEXT vkGetMemoryHostPointerPropertiesEXT registry at www.khronos.org>
type HS_vkGetMemoryHostPointerPropertiesEXT =
     VkDevice -- ^ device
              ->
       VkExternalMemoryHandleTypeFlagBits -- ^ handleType
                                          ->
         Ptr Void -- ^ pHostPointer
                  -> Ptr VkMemoryHostPointerPropertiesEXT -- ^ pMemoryHostPointerProperties
                                                          -> IO VkResult

type PFN_vkGetMemoryHostPointerPropertiesEXT =
     FunPtr HS_vkGetMemoryHostPointerPropertiesEXT

foreign import ccall unsafe "dynamic"
               unwrapVkGetMemoryHostPointerPropertiesEXTUnsafe ::
               PFN_vkGetMemoryHostPointerPropertiesEXT ->
                 HS_vkGetMemoryHostPointerPropertiesEXT

foreign import ccall safe "dynamic"
               unwrapVkGetMemoryHostPointerPropertiesEXTSafe ::
               PFN_vkGetMemoryHostPointerPropertiesEXT ->
                 HS_vkGetMemoryHostPointerPropertiesEXT

instance VulkanProc "vkGetMemoryHostPointerPropertiesEXT" where
    type VkProcType "vkGetMemoryHostPointerPropertiesEXT" =
         HS_vkGetMemoryHostPointerPropertiesEXT
    vkProcSymbol = _VkGetMemoryHostPointerPropertiesEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetMemoryHostPointerPropertiesEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetMemoryHostPointerPropertiesEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION = 1

type VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION = 1

pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME :: CString

pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME <-
        (is_VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME -> True)
  where
    VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME
      = _VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME

{-# INLINE _VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME #-}

_VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME :: CString
_VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME
  = Ptr "VK_EXT_external_memory_host\NUL"#

{-# INLINE is_VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME #-}

is_VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME

type VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME =
     "VK_EXT_external_memory_host"

pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT =
        VkStructureType 1000178000

pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT =
        VkStructureType 1000178001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT
        = VkStructureType 1000178002

-- | bitpos = @7@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT ::
        VkExternalMemoryHandleTypeBitmask a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT =
        VkExternalMemoryHandleTypeBitmask 128

-- | bitpos = @8@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT
        :: VkExternalMemoryHandleTypeBitmask a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT
        = VkExternalMemoryHandleTypeBitmask 256
