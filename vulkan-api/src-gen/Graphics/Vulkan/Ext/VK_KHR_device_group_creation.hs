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
module Graphics.Vulkan.Ext.VK_KHR_device_group_creation
       (-- * Vulkan extension: @VK_KHR_device_group_creation@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jeffbolznv@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @71@
        VkDeviceGroupDeviceCreateInfoKHR,
        VkPhysicalDeviceGroupPropertiesKHR,
        VkEnumeratePhysicalDeviceGroupsKHR,
        pattern VkEnumeratePhysicalDeviceGroupsKHR,
        HS_vkEnumeratePhysicalDeviceGroupsKHR,
        PFN_vkEnumeratePhysicalDeviceGroupsKHR,
        module Graphics.Vulkan.Marshal, VkBool32(..), VkDeviceSize(..),
        VkFlags(..), VkSampleMask(..), VkResult(..), VkStructureType(..),
        VkBuffer, VkBufferView, VkBufferView_T(), VkBuffer_T(),
        VkCommandBuffer, VkCommandBuffer_T(), VkCommandPool,
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
        VkPhysicalDevice16BitStorageFeatures,
        VkPhysicalDevice16BitStorageFeaturesKHR,
        VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT,
        VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT,
        VkPhysicalDeviceConservativeRasterizationPropertiesEXT,
        VkPhysicalDeviceDescriptorIndexingFeaturesEXT,
        VkPhysicalDeviceDescriptorIndexingPropertiesEXT,
        VkPhysicalDeviceDiscardRectanglePropertiesEXT,
        VkPhysicalDeviceExternalBufferInfo,
        VkPhysicalDeviceExternalBufferInfoKHR,
        VkPhysicalDeviceExternalFenceInfo,
        VkPhysicalDeviceExternalFenceInfoKHR,
        VkPhysicalDeviceExternalImageFormatInfo,
        VkPhysicalDeviceExternalImageFormatInfoKHR,
        VkPhysicalDeviceExternalMemoryHostPropertiesEXT,
        VkPhysicalDeviceExternalSemaphoreInfo,
        VkPhysicalDeviceExternalSemaphoreInfoKHR,
        VkPhysicalDeviceFeatures2, VkPhysicalDeviceFeatures2KHR,
        VkPhysicalDeviceGroupProperties, VkPhysicalDeviceIDProperties,
        VkPhysicalDeviceIDPropertiesKHR, VkPhysicalDeviceImageFormatInfo2,
        VkPhysicalDeviceImageFormatInfo2KHR, VkPhysicalDeviceLimits,
        VkPhysicalDeviceMaintenance3Properties,
        VkPhysicalDeviceMaintenance3PropertiesKHR,
        VkPhysicalDeviceMemoryProperties,
        VkPhysicalDeviceMemoryProperties2,
        VkPhysicalDeviceMemoryProperties2KHR,
        VkPhysicalDeviceMultiviewFeatures,
        VkPhysicalDeviceMultiviewFeaturesKHR,
        VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX,
        VkPhysicalDeviceMultiviewProperties,
        VkPhysicalDeviceMultiviewPropertiesKHR,
        VkPhysicalDevicePointClippingProperties,
        VkPhysicalDevicePointClippingPropertiesKHR,
        VkPhysicalDeviceProperties, VkPhysicalDeviceProperties2,
        VkPhysicalDeviceProperties2KHR,
        VkPhysicalDeviceProtectedMemoryFeatures,
        VkPhysicalDeviceProtectedMemoryProperties,
        VkPhysicalDevicePushDescriptorPropertiesKHR,
        VkPhysicalDeviceSampleLocationsPropertiesEXT,
        VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT,
        VkPhysicalDeviceSamplerYcbcrConversionFeatures,
        VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR,
        VkPhysicalDeviceShaderCorePropertiesAMD,
        VkPhysicalDeviceShaderDrawParameterFeatures,
        VkPhysicalDeviceSparseImageFormatInfo2,
        VkPhysicalDeviceSparseImageFormatInfo2KHR,
        VkPhysicalDeviceSparseProperties,
        VkPhysicalDeviceSubgroupProperties,
        VkPhysicalDeviceSurfaceInfo2KHR,
        VkPhysicalDeviceVariablePointerFeatures,
        VkPhysicalDeviceVariablePointerFeaturesKHR,
        VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT,
        VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION,
        pattern VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION,
        VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME,
        pattern VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR,
        pattern VK_MAX_DEVICE_GROUP_SIZE_KHR,
        pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR)
       where
import           GHC.Ptr                                     (Ptr (..))
import           Graphics.Vulkan.Constants                   (pattern VK_MAX_DEVICE_GROUP_SIZE_KHR)
import           Graphics.Vulkan.Core_1_1                    (pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT,
                                                              pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO,
                                                              pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Device         (VkDeviceGroupDeviceCreateInfoKHR)
import           Graphics.Vulkan.Types.Struct.PhysicalDevice

pattern VkEnumeratePhysicalDeviceGroupsKHR :: CString

pattern VkEnumeratePhysicalDeviceGroupsKHR <-
        (is_VkEnumeratePhysicalDeviceGroupsKHR -> True)
  where
    VkEnumeratePhysicalDeviceGroupsKHR
      = _VkEnumeratePhysicalDeviceGroupsKHR

{-# INLINE _VkEnumeratePhysicalDeviceGroupsKHR #-}

_VkEnumeratePhysicalDeviceGroupsKHR :: CString
_VkEnumeratePhysicalDeviceGroupsKHR
  = Ptr "vkEnumeratePhysicalDeviceGroupsKHR\NUL"#

{-# INLINE is_VkEnumeratePhysicalDeviceGroupsKHR #-}

is_VkEnumeratePhysicalDeviceGroupsKHR :: CString -> Bool
is_VkEnumeratePhysicalDeviceGroupsKHR
  = (EQ ==) . cmpCStrings _VkEnumeratePhysicalDeviceGroupsKHR

type VkEnumeratePhysicalDeviceGroupsKHR =
     "vkEnumeratePhysicalDeviceGroupsKHR"

-- | This is an alias for `vkEnumeratePhysicalDeviceGroups`.
--
--   Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkEnumeratePhysicalDeviceGroupsKHR
--   >     ( VkInstance instance
--   >     , uint32_t* pPhysicalDeviceGroupCount
--   >     , VkPhysicalDeviceGroupProperties* pPhysicalDeviceGroupProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkEnumeratePhysicalDeviceGroupsKHR vkEnumeratePhysicalDeviceGroupsKHR registry at www.khronos.org>
type HS_vkEnumeratePhysicalDeviceGroupsKHR =
     VkInstance -- ^ instance
                ->
       Ptr Word32 -- ^ pPhysicalDeviceGroupCount
                  -> Ptr VkPhysicalDeviceGroupProperties -- ^ pPhysicalDeviceGroupProperties
                                                         -> IO VkResult

type PFN_vkEnumeratePhysicalDeviceGroupsKHR =
     FunPtr HS_vkEnumeratePhysicalDeviceGroupsKHR

foreign import ccall unsafe "dynamic"
               unwrapVkEnumeratePhysicalDeviceGroupsKHRUnsafe ::
               PFN_vkEnumeratePhysicalDeviceGroupsKHR ->
                 HS_vkEnumeratePhysicalDeviceGroupsKHR

foreign import ccall safe "dynamic"
               unwrapVkEnumeratePhysicalDeviceGroupsKHRSafe ::
               PFN_vkEnumeratePhysicalDeviceGroupsKHR ->
                 HS_vkEnumeratePhysicalDeviceGroupsKHR

instance VulkanProc "vkEnumeratePhysicalDeviceGroupsKHR" where
    type VkProcType "vkEnumeratePhysicalDeviceGroupsKHR" =
         HS_vkEnumeratePhysicalDeviceGroupsKHR
    vkProcSymbol = _VkEnumeratePhysicalDeviceGroupsKHR

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkEnumeratePhysicalDeviceGroupsKHRUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe = unwrapVkEnumeratePhysicalDeviceGroupsKHRSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION = 1

type VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION = 1

pattern VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME :: CString

pattern VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME <-
        (is_VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME -> True)
  where
    VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME
      = _VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME

{-# INLINE _VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME #-}

_VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME :: CString
_VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME
  = Ptr "VK_KHR_device_group_creation\NUL"#

{-# INLINE is_VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME #-}

is_VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME

type VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME =
     "VK_KHR_device_group_creation"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR =
        VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES

pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR =
        VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO

pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR =
        VK_MEMORY_HEAP_MULTI_INSTANCE_BIT
