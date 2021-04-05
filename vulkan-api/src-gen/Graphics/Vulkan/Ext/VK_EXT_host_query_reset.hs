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
module Graphics.Vulkan.Ext.VK_EXT_host_query_reset
       (-- * Vulkan extension: @VK_EXT_host_query_reset@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Bas Nieuwenhuizen @BNieuwenhuizen@
        --
        -- author: @EXT@
        --
        -- type: @device@
        --
        -- Extension number: @262@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkPhysicalDeviceHostQueryResetFeaturesEXT, VkResetQueryPoolEXT,
        pattern VkResetQueryPoolEXT, HS_vkResetQueryPoolEXT,
        PFN_vkResetQueryPoolEXT, VkAccelerationStructureKHR,
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
        VK_EXT_HOST_QUERY_RESET_SPEC_VERSION,
        pattern VK_EXT_HOST_QUERY_RESET_SPEC_VERSION,
        VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME,
        pattern VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT)
       where
import GHC.Ptr                                     (Ptr (..))
import Graphics.Vulkan.Core_1_2                    (pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Proc                (VulkanProc (..))
import Graphics.Vulkan.Types.Handles
import Graphics.Vulkan.Types.Struct.PhysicalDevice (VkPhysicalDeviceHostQueryResetFeaturesEXT)

pattern VkResetQueryPoolEXT :: CString

pattern VkResetQueryPoolEXT <- (is_VkResetQueryPoolEXT -> True)
  where
    VkResetQueryPoolEXT = _VkResetQueryPoolEXT

{-# INLINE _VkResetQueryPoolEXT #-}

_VkResetQueryPoolEXT :: CString
_VkResetQueryPoolEXT = Ptr "vkResetQueryPoolEXT\NUL"#

{-# INLINE is_VkResetQueryPoolEXT #-}

is_VkResetQueryPoolEXT :: CString -> Bool
is_VkResetQueryPoolEXT = (EQ ==) . cmpCStrings _VkResetQueryPoolEXT

type VkResetQueryPoolEXT = "vkResetQueryPoolEXT"

-- | This is an alias for `vkResetQueryPool`.
--
--   > void vkResetQueryPoolEXT
--   >     ( VkDevice device
--   >     , VkQueryPool queryPool
--   >     , uint32_t firstQuery
--   >     , uint32_t queryCount
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkResetQueryPoolEXT vkResetQueryPoolEXT registry at www.khronos.org>
type HS_vkResetQueryPoolEXT =
     VkDevice -- ^ device
              -> VkQueryPool -- ^ queryPool
                             -> Word32 -- ^ firstQuery
                                       -> Word32 -- ^ queryCount
                                                 -> IO ()

type PFN_vkResetQueryPoolEXT = FunPtr HS_vkResetQueryPoolEXT

foreign import ccall unsafe "dynamic"
               unwrapVkResetQueryPoolEXTUnsafe ::
               PFN_vkResetQueryPoolEXT -> HS_vkResetQueryPoolEXT

foreign import ccall safe "dynamic" unwrapVkResetQueryPoolEXTSafe
               :: PFN_vkResetQueryPoolEXT -> HS_vkResetQueryPoolEXT

instance VulkanProc "vkResetQueryPoolEXT" where
    type VkProcType "vkResetQueryPoolEXT" = HS_vkResetQueryPoolEXT
    vkProcSymbol = _VkResetQueryPoolEXT

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe = unwrapVkResetQueryPoolEXTUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkResetQueryPoolEXTSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_HOST_QUERY_RESET_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_HOST_QUERY_RESET_SPEC_VERSION = 1

type VK_EXT_HOST_QUERY_RESET_SPEC_VERSION = 1

pattern VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME :: CString

pattern VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME <-
        (is_VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME -> True)
  where
    VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME
      = _VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME

{-# INLINE _VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME #-}

_VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME :: CString
_VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME
  = Ptr "VK_EXT_host_query_reset\NUL"#

{-# INLINE is_VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME #-}

is_VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME

type VK_EXT_HOST_QUERY_RESET_EXTENSION_NAME =
     "VK_EXT_host_query_reset"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES
