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
module Graphics.Vulkan.Ext.VK_KHR_buffer_device_address
       (-- * Vulkan extension: @VK_KHR_buffer_device_address@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jeffbolznv@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @258@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkBufferDeviceAddressInfoKHR,
        VkBufferOpaqueCaptureAddressCreateInfoKHR,
        VkDeviceMemoryOpaqueCaptureAddressInfoKHR,
        VkMemoryOpaqueCaptureAddressAllocateInfoKHR,
        VkPhysicalDeviceBufferDeviceAddressFeaturesKHR,
        VkGetBufferDeviceAddressKHR, pattern VkGetBufferDeviceAddressKHR,
        HS_vkGetBufferDeviceAddressKHR, PFN_vkGetBufferDeviceAddressKHR,
        VkGetBufferOpaqueCaptureAddressKHR,
        pattern VkGetBufferOpaqueCaptureAddressKHR,
        HS_vkGetBufferOpaqueCaptureAddressKHR,
        PFN_vkGetBufferOpaqueCaptureAddressKHR,
        VkGetDeviceMemoryOpaqueCaptureAddressKHR,
        pattern VkGetDeviceMemoryOpaqueCaptureAddressKHR,
        HS_vkGetDeviceMemoryOpaqueCaptureAddressKHR,
        PFN_vkGetDeviceMemoryOpaqueCaptureAddressKHR,
        module Graphics.Vulkan.Marshal, AHardwareBuffer(), ANativeWindow(),
        CAMetalLayer(), VkBool32(..), VkDeviceAddress(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkStructureType(..), VkAccelerationStructureKHR,
        VkAccelerationStructureKHR_T(), VkAccelerationStructureNV,
        VkAccelerationStructureNV_T(), VkBuffer, VkBufferView,
        VkBufferView_T(), VkBuffer_T(), VkCommandBuffer,
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
        VkBufferCreateInfo, VkBufferDeviceAddressCreateInfoEXT,
        VkBufferDeviceAddressInfo, VkBufferDeviceAddressInfoEXT,
        VkBufferImageCopy, VkBufferMemoryBarrier,
        VkBufferMemoryRequirementsInfo2,
        VkBufferMemoryRequirementsInfo2KHR,
        VkBufferOpaqueCaptureAddressCreateInfo, VkBufferViewCreateInfo,
        VkDeviceCreateInfo, VkDeviceDiagnosticsConfigCreateInfoNV,
        VkDeviceEventInfoEXT, VkDeviceGroupBindSparseInfo,
        VkDeviceGroupBindSparseInfoKHR,
        VkDeviceGroupCommandBufferBeginInfo,
        VkDeviceGroupCommandBufferBeginInfoKHR,
        VkDeviceGroupDeviceCreateInfo, VkDeviceGroupDeviceCreateInfoKHR,
        VkDeviceGroupPresentCapabilitiesKHR, VkDeviceGroupPresentInfoKHR,
        VkDeviceGroupRenderPassBeginInfo,
        VkDeviceGroupRenderPassBeginInfoKHR, VkDeviceGroupSubmitInfo,
        VkDeviceGroupSubmitInfoKHR, VkDeviceGroupSwapchainCreateInfoKHR,
        VkDeviceMemoryOpaqueCaptureAddressInfo,
        VkDeviceMemoryOverallocationCreateInfoAMD,
        VkDevicePrivateDataCreateInfoEXT, VkDeviceQueueCreateInfo,
        VkDeviceQueueGlobalPriorityCreateInfoEXT, VkDeviceQueueInfo2,
        VK_KHR_BUFFER_DEVICE_ADDRESS_SPEC_VERSION,
        pattern VK_KHR_BUFFER_DEVICE_ADDRESS_SPEC_VERSION,
        VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME,
        pattern VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_KHR,
        pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO_KHR,
        pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR,
        pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR,
        pattern VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT_KHR,
        pattern VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR,
        pattern VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR)
       where
import GHC.Ptr                                     (Ptr (..))
import Graphics.Vulkan.Core_1_2                    (pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT,
                                                    pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT,
                                                    pattern VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS,
                                                    pattern VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT,
                                                    pattern VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT,
                                                    pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO,
                                                    pattern VK_STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO,
                                                    pattern VK_STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO,
                                                    pattern VK_STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO,
                                                    pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                (VulkanProc (..))
import Graphics.Vulkan.Types.BaseTypes
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.Buffer
import Graphics.Vulkan.Types.Struct.Device
import Graphics.Vulkan.Types.Struct.Memory         (VkMemoryOpaqueCaptureAddressAllocateInfoKHR)
import Graphics.Vulkan.Types.Struct.PhysicalDevice (VkPhysicalDeviceBufferDeviceAddressFeaturesKHR)

pattern VkGetBufferDeviceAddressKHR :: CString

pattern VkGetBufferDeviceAddressKHR <-
        (is_VkGetBufferDeviceAddressKHR -> True)
  where
    VkGetBufferDeviceAddressKHR = _VkGetBufferDeviceAddressKHR

{-# INLINE _VkGetBufferDeviceAddressKHR #-}

_VkGetBufferDeviceAddressKHR :: CString
_VkGetBufferDeviceAddressKHR
  = Ptr "vkGetBufferDeviceAddressKHR\NUL"#

{-# INLINE is_VkGetBufferDeviceAddressKHR #-}

is_VkGetBufferDeviceAddressKHR :: CString -> Bool
is_VkGetBufferDeviceAddressKHR
  = (EQ ==) . cmpCStrings _VkGetBufferDeviceAddressKHR

type VkGetBufferDeviceAddressKHR = "vkGetBufferDeviceAddressKHR"

-- | This is an alias for `vkGetBufferDeviceAddress`.
--
--   > VkDeviceAddress vkGetBufferDeviceAddressKHR
--   >     ( VkDevice device
--   >     , const VkBufferDeviceAddressInfo* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetBufferDeviceAddressKHR vkGetBufferDeviceAddressKHR registry at www.khronos.org>
type HS_vkGetBufferDeviceAddressKHR =
     VkDevice -- ^ device
              -> Ptr VkBufferDeviceAddressInfo -- ^ pInfo
                                               -> IO VkDeviceAddress

type PFN_vkGetBufferDeviceAddressKHR =
     FunPtr HS_vkGetBufferDeviceAddressKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetBufferDeviceAddressKHRUnsafe ::
               PFN_vkGetBufferDeviceAddressKHR -> HS_vkGetBufferDeviceAddressKHR

foreign import ccall safe "dynamic"
               unwrapVkGetBufferDeviceAddressKHRSafe ::
               PFN_vkGetBufferDeviceAddressKHR -> HS_vkGetBufferDeviceAddressKHR

instance VulkanProc "vkGetBufferDeviceAddressKHR" where
    type VkProcType "vkGetBufferDeviceAddressKHR" =
         HS_vkGetBufferDeviceAddressKHR
    vkProcSymbol = _VkGetBufferDeviceAddressKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetBufferDeviceAddressKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetBufferDeviceAddressKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetBufferOpaqueCaptureAddressKHR :: CString

pattern VkGetBufferOpaqueCaptureAddressKHR <-
        (is_VkGetBufferOpaqueCaptureAddressKHR -> True)
  where
    VkGetBufferOpaqueCaptureAddressKHR
      = _VkGetBufferOpaqueCaptureAddressKHR

{-# INLINE _VkGetBufferOpaqueCaptureAddressKHR #-}

_VkGetBufferOpaqueCaptureAddressKHR :: CString
_VkGetBufferOpaqueCaptureAddressKHR
  = Ptr "vkGetBufferOpaqueCaptureAddressKHR\NUL"#

{-# INLINE is_VkGetBufferOpaqueCaptureAddressKHR #-}

is_VkGetBufferOpaqueCaptureAddressKHR :: CString -> Bool
is_VkGetBufferOpaqueCaptureAddressKHR
  = (EQ ==) . cmpCStrings _VkGetBufferOpaqueCaptureAddressKHR

type VkGetBufferOpaqueCaptureAddressKHR =
     "vkGetBufferOpaqueCaptureAddressKHR"

-- | This is an alias for `vkGetBufferOpaqueCaptureAddress`.
--
--   > uint64_t vkGetBufferOpaqueCaptureAddressKHR
--   >     ( VkDevice device
--   >     , const VkBufferDeviceAddressInfo* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetBufferOpaqueCaptureAddressKHR vkGetBufferOpaqueCaptureAddressKHR registry at www.khronos.org>
type HS_vkGetBufferOpaqueCaptureAddressKHR =
     VkDevice -- ^ device
              -> Ptr VkBufferDeviceAddressInfo -- ^ pInfo
                                               -> IO Word64

type PFN_vkGetBufferOpaqueCaptureAddressKHR =
     FunPtr HS_vkGetBufferOpaqueCaptureAddressKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetBufferOpaqueCaptureAddressKHRUnsafe ::
               PFN_vkGetBufferOpaqueCaptureAddressKHR ->
                 HS_vkGetBufferOpaqueCaptureAddressKHR

foreign import ccall safe "dynamic"
               unwrapVkGetBufferOpaqueCaptureAddressKHRSafe ::
               PFN_vkGetBufferOpaqueCaptureAddressKHR ->
                 HS_vkGetBufferOpaqueCaptureAddressKHR

instance VulkanProc "vkGetBufferOpaqueCaptureAddressKHR" where
    type VkProcType "vkGetBufferOpaqueCaptureAddressKHR" =
         HS_vkGetBufferOpaqueCaptureAddressKHR
    vkProcSymbol = _VkGetBufferOpaqueCaptureAddressKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetBufferOpaqueCaptureAddressKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetBufferOpaqueCaptureAddressKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetDeviceMemoryOpaqueCaptureAddressKHR :: CString

pattern VkGetDeviceMemoryOpaqueCaptureAddressKHR <-
        (is_VkGetDeviceMemoryOpaqueCaptureAddressKHR -> True)
  where
    VkGetDeviceMemoryOpaqueCaptureAddressKHR
      = _VkGetDeviceMemoryOpaqueCaptureAddressKHR

{-# INLINE _VkGetDeviceMemoryOpaqueCaptureAddressKHR #-}

_VkGetDeviceMemoryOpaqueCaptureAddressKHR :: CString
_VkGetDeviceMemoryOpaqueCaptureAddressKHR
  = Ptr "vkGetDeviceMemoryOpaqueCaptureAddressKHR\NUL"#

{-# INLINE is_VkGetDeviceMemoryOpaqueCaptureAddressKHR #-}

is_VkGetDeviceMemoryOpaqueCaptureAddressKHR :: CString -> Bool
is_VkGetDeviceMemoryOpaqueCaptureAddressKHR
  = (EQ ==) . cmpCStrings _VkGetDeviceMemoryOpaqueCaptureAddressKHR

type VkGetDeviceMemoryOpaqueCaptureAddressKHR =
     "vkGetDeviceMemoryOpaqueCaptureAddressKHR"

-- | This is an alias for `vkGetDeviceMemoryOpaqueCaptureAddress`.
--
--   > uint64_t vkGetDeviceMemoryOpaqueCaptureAddressKHR
--   >     ( VkDevice device
--   >     , const VkDeviceMemoryOpaqueCaptureAddressInfo* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetDeviceMemoryOpaqueCaptureAddressKHR vkGetDeviceMemoryOpaqueCaptureAddressKHR registry at www.khronos.org>
type HS_vkGetDeviceMemoryOpaqueCaptureAddressKHR =
     VkDevice -- ^ device
              -> Ptr VkDeviceMemoryOpaqueCaptureAddressInfo -- ^ pInfo
                                                            -> IO Word64

type PFN_vkGetDeviceMemoryOpaqueCaptureAddressKHR =
     FunPtr HS_vkGetDeviceMemoryOpaqueCaptureAddressKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetDeviceMemoryOpaqueCaptureAddressKHRUnsafe ::
               PFN_vkGetDeviceMemoryOpaqueCaptureAddressKHR ->
                 HS_vkGetDeviceMemoryOpaqueCaptureAddressKHR

foreign import ccall safe "dynamic"
               unwrapVkGetDeviceMemoryOpaqueCaptureAddressKHRSafe ::
               PFN_vkGetDeviceMemoryOpaqueCaptureAddressKHR ->
                 HS_vkGetDeviceMemoryOpaqueCaptureAddressKHR

instance VulkanProc "vkGetDeviceMemoryOpaqueCaptureAddressKHR"
         where
    type VkProcType "vkGetDeviceMemoryOpaqueCaptureAddressKHR" =
         HS_vkGetDeviceMemoryOpaqueCaptureAddressKHR
    vkProcSymbol = _VkGetDeviceMemoryOpaqueCaptureAddressKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetDeviceMemoryOpaqueCaptureAddressKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetDeviceMemoryOpaqueCaptureAddressKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_BUFFER_DEVICE_ADDRESS_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_BUFFER_DEVICE_ADDRESS_SPEC_VERSION = 1

type VK_KHR_BUFFER_DEVICE_ADDRESS_SPEC_VERSION = 1

pattern VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME :: CString

pattern VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME <-
        (is_VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME -> True)
  where
    VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
      = _VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME

{-# INLINE _VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME #-}

_VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME :: CString
_VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
  = Ptr "VK_KHR_buffer_device_address\NUL"#

{-# INLINE is_VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME #-}

is_VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME

type VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME =
     "VK_KHR_buffer_device_address"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_KHR
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES

pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_KHR =
        VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO

pattern VK_STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO_KHR
        = VK_STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO

pattern VK_STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO_KHR
        = VK_STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO

pattern VK_STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO_KHR
        = VK_STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO

pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR =
        VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT

pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR =
        VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT

pattern VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT_KHR =
        VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT

pattern VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR =
        VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT

pattern VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR =
        VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS
