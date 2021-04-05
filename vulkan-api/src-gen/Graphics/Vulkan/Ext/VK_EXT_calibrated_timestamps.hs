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
module Graphics.Vulkan.Ext.VK_EXT_calibrated_timestamps
       (-- * Vulkan extension: @VK_EXT_calibrated_timestamps@
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
        -- Extension number: @185@
        VkCalibratedTimestampInfoEXT, VkStructureType(..),
        VkTimeDomainEXT(..),
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceCalibrateableTimeDomainsEXT,
        pattern VkGetPhysicalDeviceCalibrateableTimeDomainsEXT,
        HS_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT,
        PFN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT,
        VkGetCalibratedTimestampsEXT, pattern VkGetCalibratedTimestampsEXT,
        HS_vkGetCalibratedTimestampsEXT, PFN_vkGetCalibratedTimestampsEXT,
        VkResult(..), VkAccelerationStructureKHR,
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
        VkValidationCacheEXT, VkValidationCacheEXT_T(),
        VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION,
        pattern VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION,
        VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME,
        pattern VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT)
       where
import GHC.Ptr                                                 (Ptr (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                            (VulkanProc (..))
import Graphics.Vulkan.Types.Enum.Result
import Graphics.Vulkan.Types.Enum.StructureType
import Graphics.Vulkan.Types.Enum.TimeDomainEXT
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.CalibratedTimestampInfoEXT

pattern VkGetPhysicalDeviceCalibrateableTimeDomainsEXT :: CString

pattern VkGetPhysicalDeviceCalibrateableTimeDomainsEXT <-
        (is_VkGetPhysicalDeviceCalibrateableTimeDomainsEXT -> True)
  where
    VkGetPhysicalDeviceCalibrateableTimeDomainsEXT
      = _VkGetPhysicalDeviceCalibrateableTimeDomainsEXT

{-# INLINE _VkGetPhysicalDeviceCalibrateableTimeDomainsEXT #-}

_VkGetPhysicalDeviceCalibrateableTimeDomainsEXT :: CString
_VkGetPhysicalDeviceCalibrateableTimeDomainsEXT
  = Ptr "vkGetPhysicalDeviceCalibrateableTimeDomainsEXT\NUL"#

{-# INLINE is_VkGetPhysicalDeviceCalibrateableTimeDomainsEXT #-}

is_VkGetPhysicalDeviceCalibrateableTimeDomainsEXT ::
                                                  CString -> Bool
is_VkGetPhysicalDeviceCalibrateableTimeDomainsEXT
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceCalibrateableTimeDomainsEXT

type VkGetPhysicalDeviceCalibrateableTimeDomainsEXT =
     "vkGetPhysicalDeviceCalibrateableTimeDomainsEXT"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceCalibrateableTimeDomainsEXT
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pTimeDomainCount
--   >     , VkTimeDomainEXT* pTimeDomains
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetPhysicalDeviceCalibrateableTimeDomainsEXT vkGetPhysicalDeviceCalibrateableTimeDomainsEXT registry at www.khronos.org>
type HS_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr Word32 -- ^ pTimeDomainCount
                  -> Ptr VkTimeDomainEXT -- ^ pTimeDomains
                                         -> IO VkResult

type PFN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT =
     FunPtr HS_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceCalibrateableTimeDomainsEXTUnsafe ::
               PFN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT ->
                 HS_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceCalibrateableTimeDomainsEXTSafe ::
               PFN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT ->
                 HS_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT

instance VulkanProc
           "vkGetPhysicalDeviceCalibrateableTimeDomainsEXT"
         where
    type VkProcType "vkGetPhysicalDeviceCalibrateableTimeDomainsEXT" =
         HS_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT
    vkProcSymbol = _VkGetPhysicalDeviceCalibrateableTimeDomainsEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetPhysicalDeviceCalibrateableTimeDomainsEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetPhysicalDeviceCalibrateableTimeDomainsEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetCalibratedTimestampsEXT :: CString

pattern VkGetCalibratedTimestampsEXT <-
        (is_VkGetCalibratedTimestampsEXT -> True)
  where
    VkGetCalibratedTimestampsEXT = _VkGetCalibratedTimestampsEXT

{-# INLINE _VkGetCalibratedTimestampsEXT #-}

_VkGetCalibratedTimestampsEXT :: CString
_VkGetCalibratedTimestampsEXT
  = Ptr "vkGetCalibratedTimestampsEXT\NUL"#

{-# INLINE is_VkGetCalibratedTimestampsEXT #-}

is_VkGetCalibratedTimestampsEXT :: CString -> Bool
is_VkGetCalibratedTimestampsEXT
  = (EQ ==) . cmpCStrings _VkGetCalibratedTimestampsEXT

type VkGetCalibratedTimestampsEXT = "vkGetCalibratedTimestampsEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetCalibratedTimestampsEXT
--   >     ( VkDevice device
--   >     , uint32_t timestampCount
--   >     , const VkCalibratedTimestampInfoEXT* pTimestampInfos
--   >     , uint64_t* pTimestamps
--   >     , uint64_t* pMaxDeviation
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetCalibratedTimestampsEXT vkGetCalibratedTimestampsEXT registry at www.khronos.org>
type HS_vkGetCalibratedTimestampsEXT =
     VkDevice -- ^ device
              ->
       Word32 -- ^ timestampCount
              ->
         Ptr VkCalibratedTimestampInfoEXT -- ^ pTimestampInfos
                                          ->
           Ptr Word64 -- ^ pTimestamps
                      -> Ptr Word64 -- ^ pMaxDeviation
                                    -> IO VkResult

type PFN_vkGetCalibratedTimestampsEXT =
     FunPtr HS_vkGetCalibratedTimestampsEXT

foreign import ccall unsafe "dynamic"
               unwrapVkGetCalibratedTimestampsEXTUnsafe ::
               PFN_vkGetCalibratedTimestampsEXT -> HS_vkGetCalibratedTimestampsEXT

foreign import ccall safe "dynamic"
               unwrapVkGetCalibratedTimestampsEXTSafe ::
               PFN_vkGetCalibratedTimestampsEXT -> HS_vkGetCalibratedTimestampsEXT

instance VulkanProc "vkGetCalibratedTimestampsEXT" where
    type VkProcType "vkGetCalibratedTimestampsEXT" =
         HS_vkGetCalibratedTimestampsEXT
    vkProcSymbol = _VkGetCalibratedTimestampsEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkGetCalibratedTimestampsEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkGetCalibratedTimestampsEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION = 1

type VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION = 1

pattern VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME :: CString

pattern VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME <-
        (is_VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME -> True)
  where
    VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
      = _VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME

{-# INLINE _VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME #-}

_VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME :: CString
_VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
  = Ptr "VK_EXT_calibrated_timestamps\NUL"#

{-# INLINE is_VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME #-}

is_VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME

type VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME =
     "VK_EXT_calibrated_timestamps"

pattern VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT =
        VkStructureType 1000184000
