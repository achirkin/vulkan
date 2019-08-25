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
module Graphics.Vulkan.Ext.VK_ANDROID_external_memory_android_hardware_buffer
       (-- * Vulkan extension: @VK_ANDROID_external_memory_android_hardware_buffer@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @critsec@
        --
        -- author: @ANDROID@
        --
        -- type: @device@
        --
        -- platform: @android@
        --
        -- Extension number: @130@
        --
        -- Required extensions: 'VK_KHR_sampler_ycbcr_conversion', 'VK_KHR_external_memory', 'VK_EXT_queue_family_foreign'.
        --

        -- ** Required extensions: 'VK_KHR_sampler_ycbcr_conversion', 'VK_KHR_external_memory', 'VK_EXT_queue_family_foreign'.
        VkAndroidHardwareBufferFormatPropertiesANDROID,
        VkAndroidHardwareBufferPropertiesANDROID,
        VkAndroidHardwareBufferUsageANDROID, VkBool32(..),
        VkDeviceSize(..), VkFlags(..), VkSampleMask(..),
        VkChromaLocation(..), VkChromaLocationKHR(..), VkComponentMapping,
        VkComponentSwizzle(..), VkExtent3D, VkExternalFormatANDROID,
        VkFilter(..), VkFormat(..), VkFormatFeatureBitmask(..),
        VkFormatFeatureFlagBits(), VkFormatFeatureFlags(),
        VkImageAspectBitmask(..), VkImageCreateBitmask(..),
        VkImageLayout(..), VkImageTiling(..), VkImageType(..),
        VkImageUsageBitmask(..), VkImageViewType(..),
        VkImageAspectFlagBits(), VkImageAspectFlags(),
        VkImageCreateFlagBits(), VkImageCreateFlags(),
        VkImageUsageFlagBits(), VkImageUsageFlags(), VkImageCreateInfo,
        VkImageFormatProperties, VkImageFormatProperties2,
        VkImportAndroidHardwareBufferInfoANDROID, VkMemoryAllocateInfo,
        VkMemoryGetAndroidHardwareBufferInfoANDROID,
        VkSampleCountBitmask(..), VkSampleCountFlagBits(),
        VkSampleCountFlags(), VkSamplerYcbcrConversionCreateInfo,
        VkSamplerAddressMode(..), VkSamplerMipmapMode(..),
        VkSamplerReductionModeEXT(..), VkSamplerYcbcrModelConversion(..),
        VkSamplerYcbcrRange(..), VkSamplerCreateFlagBits(..),
        VkSamplerYcbcrModelConversionKHR(..), VkSamplerYcbcrRangeKHR(..),
        VkSharingMode(..), VkStructureType(..),
        -- > #include "vk_platform.h"
        VkGetAndroidHardwareBufferPropertiesANDROID,
        pattern VkGetAndroidHardwareBufferPropertiesANDROID,
        HS_vkGetAndroidHardwareBufferPropertiesANDROID,
        PFN_vkGetAndroidHardwareBufferPropertiesANDROID,
        VkGetMemoryAndroidHardwareBufferANDROID,
        pattern VkGetMemoryAndroidHardwareBufferANDROID,
        HS_vkGetMemoryAndroidHardwareBufferANDROID,
        PFN_vkGetMemoryAndroidHardwareBufferANDROID,
        module Graphics.Vulkan.Marshal, VkResult(..), VkBuffer,
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
        VkAndroidSurfaceCreateInfoKHR,
        VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION,
        pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION,
        VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME,
        pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID,
        pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID,
        pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID,
        pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID,
        pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID,
        pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID)
       where
import           GHC.Ptr                                         (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                    (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Defines                   (AHardwareBuffer)
import           Graphics.Vulkan.Types.Enum.ChromaLocation
import           Graphics.Vulkan.Types.Enum.ComponentSwizzle
import           Graphics.Vulkan.Types.Enum.External             (VkExternalMemoryHandleTypeBitmask (..))
import           Graphics.Vulkan.Types.Enum.Filter
import           Graphics.Vulkan.Types.Enum.Format
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Enum.Sampler
import           Graphics.Vulkan.Types.Enum.SharingMode
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.ComponentMapping   (VkComponentMapping)
import           Graphics.Vulkan.Types.Struct.Extent             (VkExtent3D)
import           Graphics.Vulkan.Types.Struct.Image              (VkImageCreateInfo,
                                                                  VkImageFormatProperties,
                                                                  VkImageFormatProperties2)
import           Graphics.Vulkan.Types.Struct.Memory             (VkMemoryAllocateInfo)
import           Graphics.Vulkan.Types.Struct.PlatformAndroidKhr
import           Graphics.Vulkan.Types.Struct.Sampler            (VkSamplerYcbcrConversionCreateInfo)

pattern VkGetAndroidHardwareBufferPropertiesANDROID :: CString

pattern VkGetAndroidHardwareBufferPropertiesANDROID <-
        (is_VkGetAndroidHardwareBufferPropertiesANDROID -> True)
  where
    VkGetAndroidHardwareBufferPropertiesANDROID
      = _VkGetAndroidHardwareBufferPropertiesANDROID

{-# INLINE _VkGetAndroidHardwareBufferPropertiesANDROID #-}

_VkGetAndroidHardwareBufferPropertiesANDROID :: CString
_VkGetAndroidHardwareBufferPropertiesANDROID
  = Ptr "vkGetAndroidHardwareBufferPropertiesANDROID\NUL"#

{-# INLINE is_VkGetAndroidHardwareBufferPropertiesANDROID #-}

is_VkGetAndroidHardwareBufferPropertiesANDROID :: CString -> Bool
is_VkGetAndroidHardwareBufferPropertiesANDROID
  = (EQ ==) .
      cmpCStrings _VkGetAndroidHardwareBufferPropertiesANDROID

type VkGetAndroidHardwareBufferPropertiesANDROID =
     "vkGetAndroidHardwareBufferPropertiesANDROID"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkGetAndroidHardwareBufferPropertiesANDROID
--   >     ( VkDevice device
--   >     , const struct AHardwareBuffer* buffer
--   >     , VkAndroidHardwareBufferPropertiesANDROID* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetAndroidHardwareBufferPropertiesANDROID vkGetAndroidHardwareBufferPropertiesANDROID registry at www.khronos.org>
type HS_vkGetAndroidHardwareBufferPropertiesANDROID =
     VkDevice -- ^ device
              ->
       Ptr AHardwareBuffer -- ^ buffer
                           ->
         Ptr VkAndroidHardwareBufferPropertiesANDROID -- ^ pProperties
                                                      -> IO VkResult

type PFN_vkGetAndroidHardwareBufferPropertiesANDROID =
     FunPtr HS_vkGetAndroidHardwareBufferPropertiesANDROID

foreign import ccall unsafe "dynamic"
               unwrapVkGetAndroidHardwareBufferPropertiesANDROIDUnsafe ::
               PFN_vkGetAndroidHardwareBufferPropertiesANDROID ->
                 HS_vkGetAndroidHardwareBufferPropertiesANDROID

foreign import ccall safe "dynamic"
               unwrapVkGetAndroidHardwareBufferPropertiesANDROIDSafe ::
               PFN_vkGetAndroidHardwareBufferPropertiesANDROID ->
                 HS_vkGetAndroidHardwareBufferPropertiesANDROID

instance VulkanProc "vkGetAndroidHardwareBufferPropertiesANDROID"
         where
    type VkProcType "vkGetAndroidHardwareBufferPropertiesANDROID" =
         HS_vkGetAndroidHardwareBufferPropertiesANDROID
    vkProcSymbol = _VkGetAndroidHardwareBufferPropertiesANDROID

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetAndroidHardwareBufferPropertiesANDROIDUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetAndroidHardwareBufferPropertiesANDROIDSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetMemoryAndroidHardwareBufferANDROID :: CString

pattern VkGetMemoryAndroidHardwareBufferANDROID <-
        (is_VkGetMemoryAndroidHardwareBufferANDROID -> True)
  where
    VkGetMemoryAndroidHardwareBufferANDROID
      = _VkGetMemoryAndroidHardwareBufferANDROID

{-# INLINE _VkGetMemoryAndroidHardwareBufferANDROID #-}

_VkGetMemoryAndroidHardwareBufferANDROID :: CString
_VkGetMemoryAndroidHardwareBufferANDROID
  = Ptr "vkGetMemoryAndroidHardwareBufferANDROID\NUL"#

{-# INLINE is_VkGetMemoryAndroidHardwareBufferANDROID #-}

is_VkGetMemoryAndroidHardwareBufferANDROID :: CString -> Bool
is_VkGetMemoryAndroidHardwareBufferANDROID
  = (EQ ==) . cmpCStrings _VkGetMemoryAndroidHardwareBufferANDROID

type VkGetMemoryAndroidHardwareBufferANDROID =
     "vkGetMemoryAndroidHardwareBufferANDROID"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetMemoryAndroidHardwareBufferANDROID
--   >     ( VkDevice device
--   >     , const VkMemoryGetAndroidHardwareBufferInfoANDROID* pInfo
--   >     , struct AHardwareBuffer** pBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetMemoryAndroidHardwareBufferANDROID vkGetMemoryAndroidHardwareBufferANDROID registry at www.khronos.org>
type HS_vkGetMemoryAndroidHardwareBufferANDROID =
     VkDevice -- ^ device
              ->
       Ptr VkMemoryGetAndroidHardwareBufferInfoANDROID -- ^ pInfo
                                                       ->
         Ptr (Ptr AHardwareBuffer) -- ^ pBuffer
                                   -> IO VkResult

type PFN_vkGetMemoryAndroidHardwareBufferANDROID =
     FunPtr HS_vkGetMemoryAndroidHardwareBufferANDROID

foreign import ccall unsafe "dynamic"
               unwrapVkGetMemoryAndroidHardwareBufferANDROIDUnsafe ::
               PFN_vkGetMemoryAndroidHardwareBufferANDROID ->
                 HS_vkGetMemoryAndroidHardwareBufferANDROID

foreign import ccall safe "dynamic"
               unwrapVkGetMemoryAndroidHardwareBufferANDROIDSafe ::
               PFN_vkGetMemoryAndroidHardwareBufferANDROID ->
                 HS_vkGetMemoryAndroidHardwareBufferANDROID

instance VulkanProc "vkGetMemoryAndroidHardwareBufferANDROID" where
    type VkProcType "vkGetMemoryAndroidHardwareBufferANDROID" =
         HS_vkGetMemoryAndroidHardwareBufferANDROID
    vkProcSymbol = _VkGetMemoryAndroidHardwareBufferANDROID

    {-# INLINE vkProcSymbol #-}
    unwrapVkProcPtrUnsafe
      = unwrapVkGetMemoryAndroidHardwareBufferANDROIDUnsafe

    {-# INLINE unwrapVkProcPtrUnsafe #-}
    unwrapVkProcPtrSafe
      = unwrapVkGetMemoryAndroidHardwareBufferANDROIDSafe

    {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION
        :: (Num a, Eq a) => a

pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION
        = 3

type VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION
     = 3

pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
        :: CString

pattern VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
        <-
        (is_VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
           -> True)
  where
    VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
      = _VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME

{-# INLINE _VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
           #-}

_VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME ::
                                                                   CString
_VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
  = Ptr "VK_ANDROID_external_memory_android_hardware_buffer\NUL"#

{-# INLINE is_VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
           #-}

is_VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME ::
                                                                     CString -> Bool
is_VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings
        _VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME

type VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
     = "VK_ANDROID_external_memory_android_hardware_buffer"

-- | bitpos = @10@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
        :: VkExternalMemoryHandleTypeBitmask a

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
        = VkExternalMemoryHandleTypeBitmask 1024

pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID =
        VkStructureType 1000129000

pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
        = VkStructureType 1000129001

pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID
        = VkStructureType 1000129002

pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
        = VkStructureType 1000129003

pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
        = VkStructureType 1000129004

pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID =
        VkStructureType 1000129005
