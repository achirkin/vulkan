{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
{-# OPTIONS_GHC -fno-warn-orphans#-}
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
module Graphics.Vulkan.Ext.VK_EXT_buffer_device_address
       (-- * Vulkan extension: @VK_EXT_buffer_device_address@
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
        -- Extension number: @245@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal, AHardwareBuffer(),
        ANativeWindow(), CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkBufferCreateBitmask(..), VkBufferUsageBitmask(..),
        VkBufferCreateFlagBits(), VkBufferCreateFlags(),
        VkBufferUsageFlagBits(), VkBufferUsageFlags(), VkBufferCreateInfo,
        VkBufferDeviceAddressCreateInfoEXT, VkBufferDeviceAddressInfoEXT,
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
        VkDeviceQueueCreateInfo, VkPhysicalDeviceBufferAddressFeaturesEXT,
        VkPhysicalDeviceBufferDeviceAddressFeaturesEXT,
        VkPhysicalDeviceFeatures, VkPhysicalDeviceFeatures2,
        VkSharingMode(..), VkStructureType(..),
        -- > #include "vk_platform.h"
        VkGetBufferDeviceAddressEXT, pattern VkGetBufferDeviceAddressEXT,
        HS_vkGetBufferDeviceAddressEXT, PFN_vkGetBufferDeviceAddressEXT,
        VkAccelerationStructureKHR, VkAccelerationStructureKHR_T(),
        VkAccelerationStructureNV, VkAccelerationStructureNV_T(), VkBuffer,
        VkBufferView, VkBufferView_T(), VkBuffer_T(), VkCommandBuffer,
        VkCommandBuffer_T(), VkCommandPool, VkCommandPool_T(),
        VkDebugReportCallbackEXT, VkDebugReportCallbackEXT_T(),
        VkDebugUtilsMessengerEXT, VkDebugUtilsMessengerEXT_T(),
        VkDeferredOperationKHR, VkDeferredOperationKHR_T(),
        VkDescriptorPool, VkDescriptorPool_T(), VkDescriptorSet,
        VkDescriptorSetLayout, VkDescriptorSetLayout_T(),
        VkDescriptorSet_T(), VkDescriptorUpdateTemplate,
        VkDescriptorUpdateTemplateKHR, VkDescriptorUpdateTemplateKHR_T(),
        VkDescriptorUpdateTemplate_T(), VkDevice, VkDeviceMemory,
        VkDeviceMemory_T(), VkDevice_T(), VkDisplayKHR, VkDisplayKHR_T(),
        VkDisplayModeKHR, VkDisplayModeKHR_T(), VkEvent, VkEvent_T(),
        VkFence, VkFence_T(), VkFramebuffer, VkFramebuffer_T(), VkImage,
        VkImageView, VkImageView_T(), VkImage_T(),
        VkIndirectCommandsLayoutNV, VkIndirectCommandsLayoutNV_T(),
        VkInstance, VkInstance_T(), VkPerformanceConfigurationINTEL,
        VkPerformanceConfigurationINTEL_T(), VkPhysicalDevice,
        VkPhysicalDevice_T(), VkPipeline, VkPipelineCache,
        VkPipelineCache_T(), VkPipelineLayout, VkPipelineLayout_T(),
        VkPipeline_T(), VkPrivateDataSlotEXT, VkPrivateDataSlotEXT_T(),
        VkQueryPool, VkQueryPool_T(), VkQueue, VkQueue_T(), VkRenderPass,
        VkRenderPass_T(), VkSampler, VkSamplerYcbcrConversion,
        VkSamplerYcbcrConversionKHR, VkSamplerYcbcrConversionKHR_T(),
        VkSamplerYcbcrConversion_T(), VkSampler_T(), VkSemaphore,
        VkSemaphore_T(), VkShaderModule, VkShaderModule_T(), VkSurfaceKHR,
        VkSurfaceKHR_T(), VkSwapchainKHR, VkSwapchainKHR_T(),
        VkValidationCacheEXT, VkValidationCacheEXT_T(), VkBufferCopy,
        VkBufferDeviceAddressInfo, VkBufferDeviceAddressInfoKHR,
        VkBufferImageCopy, VkBufferMemoryBarrier,
        VkBufferMemoryRequirementsInfo2,
        VkBufferMemoryRequirementsInfo2KHR,
        VkBufferOpaqueCaptureAddressCreateInfo,
        VkBufferOpaqueCaptureAddressCreateInfoKHR, VkBufferViewCreateInfo,
        VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION,
        pattern VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION,
        VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME,
        pattern VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT,
        pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT,
        pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT,
        pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT,
        pattern VK_ERROR_INVALID_DEVICE_ADDRESS_EXT)
       where
import GHC.Ptr                                             (Ptr (..))
import Graphics.Vulkan.Core_1_2                            (pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT,
                                                            pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT,
                                                            pattern VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS,
                                                            pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                        (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Bitmasks
import Graphics.Vulkan.Types.Enum.Buffer
import Graphics.Vulkan.Types.Enum.Device
import Graphics.Vulkan.Types.Enum.SharingMode
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.Buffer
import Graphics.Vulkan.Types.Struct.Device                 (VkDeviceCreateInfo, VkDeviceQueueCreateInfo)
import Graphics.Vulkan.Types.Struct.PhysicalDevice         (VkPhysicalDeviceBufferAddressFeaturesEXT,
                                                            VkPhysicalDeviceBufferDeviceAddressFeaturesEXT,
                                                            VkPhysicalDeviceFeatures2)
import Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)

pattern VkGetBufferDeviceAddressEXT :: CString

pattern VkGetBufferDeviceAddressEXT <-
        (is_VkGetBufferDeviceAddressEXT -> True)
  where
    VkGetBufferDeviceAddressEXT = _VkGetBufferDeviceAddressEXT

{-# INLINE _VkGetBufferDeviceAddressEXT #-}

_VkGetBufferDeviceAddressEXT :: CString
_VkGetBufferDeviceAddressEXT
  = Ptr "vkGetBufferDeviceAddressEXT\NUL"#

{-# INLINE is_VkGetBufferDeviceAddressEXT #-}

is_VkGetBufferDeviceAddressEXT :: CString -> Bool
is_VkGetBufferDeviceAddressEXT
  = (EQ ==) . cmpCStrings _VkGetBufferDeviceAddressEXT

type VkGetBufferDeviceAddressEXT = "vkGetBufferDeviceAddressEXT"

-- | This is an alias for `vkGetBufferDeviceAddress`.
--
--   > VkDeviceAddress vkGetBufferDeviceAddressEXT
--   >     ( VkDevice device
--   >     , const VkBufferDeviceAddressInfo* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetBufferDeviceAddressEXT vkGetBufferDeviceAddressEXT registry at www.khronos.org>
type HS_vkGetBufferDeviceAddressEXT =
     VkDevice -- ^ device
              -> Ptr VkBufferDeviceAddressInfo -- ^ pInfo
                                               -> IO VkDeviceAddress

type PFN_vkGetBufferDeviceAddressEXT =
     FunPtr HS_vkGetBufferDeviceAddressEXT

foreign import ccall unsafe "dynamic"
               unwrapVkGetBufferDeviceAddressEXTUnsafe ::
               PFN_vkGetBufferDeviceAddressEXT -> HS_vkGetBufferDeviceAddressEXT

foreign import ccall safe "dynamic"
               unwrapVkGetBufferDeviceAddressEXTSafe ::
               PFN_vkGetBufferDeviceAddressEXT -> HS_vkGetBufferDeviceAddressEXT

instance VulkanProc "vkGetBufferDeviceAddressEXT" where
    type VkProcType "vkGetBufferDeviceAddressEXT" =
         HS_vkGetBufferDeviceAddressEXT
    vkProcSymbol = _VkGetBufferDeviceAddressEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetBufferDeviceAddressEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetBufferDeviceAddressEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION = 2

type VK_EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION = 2

pattern VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME :: CString

pattern VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME <-
        (is_VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME -> True)
  where
    VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
      = _VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME

{-# INLINE _VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME #-}

_VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME :: CString
_VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
  = Ptr "VK_EXT_buffer_device_address\NUL"#

{-# INLINE is_VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME #-}

is_VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME

type VK_EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME =
     "VK_EXT_buffer_device_address"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
        = VkStructureType 1000244000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT
        =
        VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT

pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT =
        VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO

pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT =
        VkStructureType 1000244002

pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT =
        VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT

pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT =
        VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT

pattern VK_ERROR_INVALID_DEVICE_ADDRESS_EXT =
        VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS
