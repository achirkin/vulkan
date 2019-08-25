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
module Graphics.Vulkan.Ext.VK_EXT_display_control
       (VkBool32(..), VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkColorComponentBitmask(..), VkColorSpaceKHR(..),
        VkColorComponentFlagBits(), VkColorComponentFlags(),
        VkCompositeAlphaBitmaskKHR(..), VkCompositeAlphaFlagBitsKHR(),
        VkCompositeAlphaFlagsKHR(), VkDeviceEventInfoEXT,
        VkDeviceEventTypeEXT(..), VkDeviceGroupPresentModeBitmaskKHR(..),
        VkDeviceCreateFlagBits(..), VkDeviceGroupPresentModeFlagBitsKHR(),
        VkDeviceGroupPresentModeFlagsKHR(), VkDeviceQueueCreateBitmask(..),
        VkDeviceQueueCreateFlagBits(), VkDeviceQueueCreateFlags(),
        VkDisplayEventInfoEXT, VkDisplayEventTypeEXT(..),
        VkDisplayPlaneAlphaBitmaskKHR(..), VkDisplayPowerStateEXT(..),
        VkDisplayPlaneAlphaFlagBitsKHR(), VkDisplayPlaneAlphaFlagsKHR(),
        VkDisplayPowerInfoEXT, VkExtent2D, VkFormat(..),
        VkFormatFeatureBitmask(..), VkFormatFeatureFlagBits(),
        VkFormatFeatureFlags(), VkImageAspectBitmask(..),
        VkImageCreateBitmask(..), VkImageLayout(..), VkImageTiling(..),
        VkImageType(..), VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(), VkPresentModeKHR(..),
        VkSharingMode(..), VkStructureType(..),
        VkSurfaceCounterBitmaskEXT(..), VkSurfaceTransformBitmaskKHR(..),
        VkSurfaceCounterFlagBitsEXT(), VkSurfaceCounterFlagsEXT(),
        VkSurfaceTransformFlagBitsKHR(), VkSurfaceTransformFlagsKHR(),
        VkSwapchainCounterCreateInfoEXT, VkSwapchainCreateBitmaskKHR(..),
        VkSwapchainCreateFlagBitsKHR(), VkSwapchainCreateFlagsKHR(),
        VkSwapchainCreateInfoKHR, -- > #include "vk_platform.h"
                                  VkDisplayPowerControlEXT,
        pattern VkDisplayPowerControlEXT, HS_vkDisplayPowerControlEXT,
        PFN_vkDisplayPowerControlEXT, VkRegisterDeviceEventEXT,
        pattern VkRegisterDeviceEventEXT, HS_vkRegisterDeviceEventEXT,
        PFN_vkRegisterDeviceEventEXT, VkRegisterDisplayEventEXT,
        pattern VkRegisterDisplayEventEXT, HS_vkRegisterDisplayEventEXT,
        PFN_vkRegisterDisplayEventEXT, VkGetSwapchainCounterEXT,
        pattern VkGetSwapchainCounterEXT, HS_vkGetSwapchainCounterEXT,
        PFN_vkGetSwapchainCounterEXT, module Graphics.Vulkan.Marshal,
        VkInternalAllocationType(..), VkResult(..),
        VkSystemAllocationScope(..), newVkAllocationFunction,
        newVkDebugReportCallbackEXT, newVkDebugUtilsMessengerCallbackEXT,
        newVkFreeFunction, newVkInternalAllocationNotification,
        newVkInternalFreeNotification, newVkReallocationFunction,
        newVkVoidFunction, unwrapVkAllocationFunction,
        unwrapVkDebugReportCallbackEXT,
        unwrapVkDebugUtilsMessengerCallbackEXT, unwrapVkFreeFunction,
        unwrapVkInternalAllocationNotification,
        unwrapVkInternalFreeNotification, unwrapVkReallocationFunction,
        unwrapVkVoidFunction, HS_vkAllocationFunction,
        HS_vkDebugReportCallbackEXT, HS_vkDebugUtilsMessengerCallbackEXT,
        HS_vkFreeFunction, HS_vkInternalAllocationNotification,
        HS_vkInternalFreeNotification, HS_vkReallocationFunction,
        HS_vkVoidFunction, PFN_vkAllocationFunction,
        PFN_vkDebugReportCallbackEXT, PFN_vkDebugUtilsMessengerCallbackEXT,
        PFN_vkFreeFunction, PFN_vkInternalAllocationNotification,
        PFN_vkInternalFreeNotification, PFN_vkReallocationFunction,
        PFN_vkVoidFunction, VkBuffer, VkBufferView, VkBufferView_T(),
        VkBuffer_T(), VkCommandBuffer, VkCommandBuffer_T(), VkCommandPool,
        VkCommandPool_T(), VkDebugReportCallbackEXT,
        VkDebugReportCallbackEXT_T(), VkDebugUtilsMessengerEXT,
        VkDebugUtilsMessengerEXT_T(), VkDescriptorPool,
        VkDescriptorPool_T(), VkDescriptorSet, VkDescriptorSetLayout,
        VkDescriptorSetLayout_T(), VkDescriptorSet_T(),
        VkDescriptorUpdateTemplate, VkDescriptorUpdateTemplateKHR,
        VkDescriptorUpdateTemplateKHR_T(), VkDescriptorUpdateTemplate_T(),
        VkDevice, VkDeviceMemory, VkDeviceMemory_T(), VkDevice_T(),
        VkDisplayKHR, VkDisplayKHR_T(), VkDisplayModeKHR,
        VkDisplayModeKHR_T(), VkEvent, VkEvent_T(), VkFence, VkFence_T(),
        VkFramebuffer, VkFramebuffer_T(), VkImage, VkImageView,
        VkImageView_T(), VkImage_T(), VkIndirectCommandsLayoutNVX,
        VkIndirectCommandsLayoutNVX_T(), VkInstance, VkInstance_T(),
        VkObjectTableNVX, VkObjectTableNVX_T(), VkPhysicalDevice,
        VkPhysicalDevice_T(), VkPipeline, VkPipelineCache,
        VkPipelineCache_T(), VkPipelineLayout, VkPipelineLayout_T(),
        VkPipeline_T(), VkQueryPool, VkQueryPool_T(), VkQueue, VkQueue_T(),
        VkRenderPass, VkRenderPass_T(), VkSampler,
        VkSamplerYcbcrConversion, VkSamplerYcbcrConversionKHR,
        VkSamplerYcbcrConversionKHR_T(), VkSamplerYcbcrConversion_T(),
        VkSampler_T(), VkSemaphore, VkSemaphore_T(), VkShaderModule,
        VkShaderModule_T(), VkSurfaceKHR, VkSurfaceKHR_T(), VkSwapchainKHR,
        VkSwapchainKHR_T(), VkValidationCacheEXT, VkValidationCacheEXT_T(),
        VkAllocationCallbacks, VkDeviceCreateInfo,
        VkDeviceGeneratedCommandsFeaturesNVX,
        VkDeviceGeneratedCommandsLimitsNVX, VkDeviceGroupBindSparseInfo,
        VkDeviceGroupBindSparseInfoKHR,
        VkDeviceGroupCommandBufferBeginInfo,
        VkDeviceGroupCommandBufferBeginInfoKHR,
        VkDeviceGroupDeviceCreateInfo, VkDeviceGroupDeviceCreateInfoKHR,
        VkDeviceGroupPresentCapabilitiesKHR, VkDeviceGroupPresentInfoKHR,
        VkDeviceGroupRenderPassBeginInfo,
        VkDeviceGroupRenderPassBeginInfoKHR, VkDeviceGroupSubmitInfo,
        VkDeviceGroupSubmitInfoKHR, VkDeviceGroupSwapchainCreateInfoKHR,
        VkDeviceQueueCreateInfo, VkDeviceQueueGlobalPriorityCreateInfoEXT,
        VkDeviceQueueInfo2, VkDisplayModeCreateInfoKHR,
        VkDisplayModeParametersKHR, VkDisplayModeProperties2KHR,
        VkDisplayModePropertiesKHR, VkDisplayPlaneCapabilities2KHR,
        VkDisplayPlaneCapabilitiesKHR, VkDisplayPlaneInfo2KHR,
        VkDisplayPlaneProperties2KHR, VkDisplayPlanePropertiesKHR,
        VkDisplayPresentInfoKHR, VkDisplayProperties2KHR,
        VkDisplayPropertiesKHR, VkDisplaySurfaceCreateInfoKHR,
        VK_EXT_DISPLAY_CONTROL_SPEC_VERSION,
        pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION,
        VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME,
        pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT)
       where
import           GHC.Ptr                                            (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                       (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.Color
import           Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR
import           Graphics.Vulkan.Types.Enum.Device
import           Graphics.Vulkan.Types.Enum.Display
import           Graphics.Vulkan.Types.Enum.Format
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.InternalAllocationType
import           Graphics.Vulkan.Types.Enum.PresentModeKHR
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.SharingMode
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.Surface
import           Graphics.Vulkan.Types.Enum.SwapchainCreateFlagsKHR
import           Graphics.Vulkan.Types.Enum.SystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.AllocationCallbacks
import           Graphics.Vulkan.Types.Struct.Device
import           Graphics.Vulkan.Types.Struct.Display
import           Graphics.Vulkan.Types.Struct.Extent                (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.SwapchainC            (VkSwapchainCounterCreateInfoEXT,
                                                                     VkSwapchainCreateInfoKHR)

pattern VkDisplayPowerControlEXT :: CString

pattern VkDisplayPowerControlEXT <-
        (is_VkDisplayPowerControlEXT -> True)
  where
    VkDisplayPowerControlEXT = _VkDisplayPowerControlEXT

{-# INLINE _VkDisplayPowerControlEXT #-}

_VkDisplayPowerControlEXT :: CString
_VkDisplayPowerControlEXT = Ptr "vkDisplayPowerControlEXT\NUL"#

{-# INLINE is_VkDisplayPowerControlEXT #-}

is_VkDisplayPowerControlEXT :: CString -> Bool
is_VkDisplayPowerControlEXT
  = (EQ ==) . cmpCStrings _VkDisplayPowerControlEXT

type VkDisplayPowerControlEXT = "vkDisplayPowerControlEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkDisplayPowerControlEXT
--   >     ( VkDevice device
--   >     , VkDisplayKHR display
--   >     , const VkDisplayPowerInfoEXT* pDisplayPowerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDisplayPowerControlEXT vkDisplayPowerControlEXT registry at www.khronos.org>
type HS_vkDisplayPowerControlEXT =
     VkDevice -- ^ device
              ->
       VkDisplayKHR -- ^ display
                    -> Ptr VkDisplayPowerInfoEXT -- ^ pDisplayPowerInfo
                                                 -> IO VkResult

type PFN_vkDisplayPowerControlEXT =
     FunPtr HS_vkDisplayPowerControlEXT

foreign import ccall unsafe "dynamic"
               unwrapVkDisplayPowerControlEXTUnsafe ::
               PFN_vkDisplayPowerControlEXT -> HS_vkDisplayPowerControlEXT

foreign import ccall safe "dynamic"
               unwrapVkDisplayPowerControlEXTSafe ::
               PFN_vkDisplayPowerControlEXT -> HS_vkDisplayPowerControlEXT

instance VulkanProc "vkDisplayPowerControlEXT" where
    type VkProcType "vkDisplayPowerControlEXT" =
         HS_vkDisplayPowerControlEXT
    vkProcSymbol = _VkDisplayPowerControlEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkDisplayPowerControlEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkDisplayPowerControlEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkRegisterDeviceEventEXT :: CString

pattern VkRegisterDeviceEventEXT <-
        (is_VkRegisterDeviceEventEXT -> True)
  where
    VkRegisterDeviceEventEXT = _VkRegisterDeviceEventEXT

{-# INLINE _VkRegisterDeviceEventEXT #-}

_VkRegisterDeviceEventEXT :: CString
_VkRegisterDeviceEventEXT = Ptr "vkRegisterDeviceEventEXT\NUL"#

{-# INLINE is_VkRegisterDeviceEventEXT #-}

is_VkRegisterDeviceEventEXT :: CString -> Bool
is_VkRegisterDeviceEventEXT
  = (EQ ==) . cmpCStrings _VkRegisterDeviceEventEXT

type VkRegisterDeviceEventEXT = "vkRegisterDeviceEventEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkRegisterDeviceEventEXT
--   >     ( VkDevice device
--   >     , const VkDeviceEventInfoEXT* pDeviceEventInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFence* pFence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkRegisterDeviceEventEXT vkRegisterDeviceEventEXT registry at www.khronos.org>
type HS_vkRegisterDeviceEventEXT =
     VkDevice -- ^ device
              ->
       Ptr VkDeviceEventInfoEXT -- ^ pDeviceEventInfo
                                ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkFence -- ^ pFence
                                                  -> IO VkResult

type PFN_vkRegisterDeviceEventEXT =
     FunPtr HS_vkRegisterDeviceEventEXT

foreign import ccall unsafe "dynamic"
               unwrapVkRegisterDeviceEventEXTUnsafe ::
               PFN_vkRegisterDeviceEventEXT -> HS_vkRegisterDeviceEventEXT

foreign import ccall safe "dynamic"
               unwrapVkRegisterDeviceEventEXTSafe ::
               PFN_vkRegisterDeviceEventEXT -> HS_vkRegisterDeviceEventEXT

instance VulkanProc "vkRegisterDeviceEventEXT" where
    type VkProcType "vkRegisterDeviceEventEXT" =
         HS_vkRegisterDeviceEventEXT
    vkProcSymbol = _VkRegisterDeviceEventEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkRegisterDeviceEventEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkRegisterDeviceEventEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkRegisterDisplayEventEXT :: CString

pattern VkRegisterDisplayEventEXT <-
        (is_VkRegisterDisplayEventEXT -> True)
  where
    VkRegisterDisplayEventEXT = _VkRegisterDisplayEventEXT

{-# INLINE _VkRegisterDisplayEventEXT #-}

_VkRegisterDisplayEventEXT :: CString
_VkRegisterDisplayEventEXT = Ptr "vkRegisterDisplayEventEXT\NUL"#

{-# INLINE is_VkRegisterDisplayEventEXT #-}

is_VkRegisterDisplayEventEXT :: CString -> Bool
is_VkRegisterDisplayEventEXT
  = (EQ ==) . cmpCStrings _VkRegisterDisplayEventEXT

type VkRegisterDisplayEventEXT = "vkRegisterDisplayEventEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkRegisterDisplayEventEXT
--   >     ( VkDevice device
--   >     , VkDisplayKHR display
--   >     , const VkDisplayEventInfoEXT* pDisplayEventInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFence* pFence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkRegisterDisplayEventEXT vkRegisterDisplayEventEXT registry at www.khronos.org>
type HS_vkRegisterDisplayEventEXT =
     VkDevice -- ^ device
              ->
       VkDisplayKHR -- ^ display
                    ->
         Ptr VkDisplayEventInfoEXT -- ^ pDisplayEventInfo
                                   ->
           Ptr VkAllocationCallbacks -- ^ pAllocator
                                     -> Ptr VkFence -- ^ pFence
                                                    -> IO VkResult

type PFN_vkRegisterDisplayEventEXT =
     FunPtr HS_vkRegisterDisplayEventEXT

foreign import ccall unsafe "dynamic"
               unwrapVkRegisterDisplayEventEXTUnsafe ::
               PFN_vkRegisterDisplayEventEXT -> HS_vkRegisterDisplayEventEXT

foreign import ccall safe "dynamic"
               unwrapVkRegisterDisplayEventEXTSafe ::
               PFN_vkRegisterDisplayEventEXT -> HS_vkRegisterDisplayEventEXT

instance VulkanProc "vkRegisterDisplayEventEXT" where
    type VkProcType "vkRegisterDisplayEventEXT" =
         HS_vkRegisterDisplayEventEXT
    vkProcSymbol = _VkRegisterDisplayEventEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkRegisterDisplayEventEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkRegisterDisplayEventEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetSwapchainCounterEXT :: CString

pattern VkGetSwapchainCounterEXT <-
        (is_VkGetSwapchainCounterEXT -> True)
  where
    VkGetSwapchainCounterEXT = _VkGetSwapchainCounterEXT

{-# INLINE _VkGetSwapchainCounterEXT #-}

_VkGetSwapchainCounterEXT :: CString
_VkGetSwapchainCounterEXT = Ptr "vkGetSwapchainCounterEXT\NUL"#

{-# INLINE is_VkGetSwapchainCounterEXT #-}

is_VkGetSwapchainCounterEXT :: CString -> Bool
is_VkGetSwapchainCounterEXT
  = (EQ ==) . cmpCStrings _VkGetSwapchainCounterEXT

type VkGetSwapchainCounterEXT = "vkGetSwapchainCounterEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR'.
--
--   > VkResult vkGetSwapchainCounterEXT
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , VkSurfaceCounterFlagBitsEXT counter
--   >     , uint64_t* pCounterValue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetSwapchainCounterEXT vkGetSwapchainCounterEXT registry at www.khronos.org>
type HS_vkGetSwapchainCounterEXT =
     VkDevice -- ^ device
              ->
       VkSwapchainKHR -- ^ swapchain
                      ->
         VkSurfaceCounterFlagBitsEXT -- ^ counter
                                     -> Ptr Word64 -- ^ pCounterValue
                                                   -> IO VkResult

type PFN_vkGetSwapchainCounterEXT =
     FunPtr HS_vkGetSwapchainCounterEXT

foreign import ccall unsafe "dynamic"
               unwrapVkGetSwapchainCounterEXTUnsafe ::
               PFN_vkGetSwapchainCounterEXT -> HS_vkGetSwapchainCounterEXT

foreign import ccall safe "dynamic"
               unwrapVkGetSwapchainCounterEXTSafe ::
               PFN_vkGetSwapchainCounterEXT -> HS_vkGetSwapchainCounterEXT

instance VulkanProc "vkGetSwapchainCounterEXT" where
    type VkProcType "vkGetSwapchainCounterEXT" =
         HS_vkGetSwapchainCounterEXT
    vkProcSymbol = _VkGetSwapchainCounterEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetSwapchainCounterEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetSwapchainCounterEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION = 1

type VK_EXT_DISPLAY_CONTROL_SPEC_VERSION = 1

pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME :: CString

pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME <-
        (is_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME -> True)
  where
    VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME
      = _VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME

{-# INLINE _VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME #-}

_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME :: CString
_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME
  = Ptr "VK_EXT_display_control\NUL"#

{-# INLINE is_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME #-}

is_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME

type VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME =
     "VK_EXT_display_control"

pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT =
        VkStructureType 1000091000

pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT =
        VkStructureType 1000091001

pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT =
        VkStructureType 1000091002

pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT =
        VkStructureType 1000091003
