{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_HADDOCK not-home#-}
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
        -- contact: @Jesse Hall @jessehall@
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
        module Graphics.Vulkan.Types.Struct.VkAndroidHardwareBufferFormatPropertiesANDROID,
        module Graphics.Vulkan.Types.Struct.VkAndroidHardwareBufferPropertiesANDROID,
        module Graphics.Vulkan.Types.Struct.VkAndroidHardwareBufferUsageANDROID,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkChromaLocation,
        module Graphics.Vulkan.Types.Struct.VkComponentMapping,
        module Graphics.Vulkan.Types.Enum.VkComponentSwizzle,
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.Struct.VkExternalFormatANDROID,
        module Graphics.Vulkan.Types.Enum.VkFilter,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkFormatFeatureFlags,
        module Graphics.Vulkan.Types.Enum.VkImageCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkImageCreateInfo,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties2,
        module Graphics.Vulkan.Types.Enum.VkImageLayout,
        module Graphics.Vulkan.Types.Enum.VkImageTiling,
        module Graphics.Vulkan.Types.Enum.VkImageType,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Struct.VkImportAndroidHardwareBufferInfoANDROID,
        module Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo,
        module Graphics.Vulkan.Types.Struct.VkMemoryGetAndroidHardwareBufferInfoANDROID,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionCreateInfo,
        module Graphics.Vulkan.Types.Enum.VkSamplerYcbcrModelConversion,
        module Graphics.Vulkan.Types.Enum.VkSamplerYcbcrRange,
        module Graphics.Vulkan.Types.Enum.VkSharingMode,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        VkGetAndroidHardwareBufferPropertiesANDROID,
        pattern VkGetAndroidHardwareBufferPropertiesANDROID,
        HS_vkGetAndroidHardwareBufferPropertiesANDROID,
        PFN_vkGetAndroidHardwareBufferPropertiesANDROID,
        unwrapVkGetAndroidHardwareBufferPropertiesANDROID,
        VkGetMemoryAndroidHardwareBufferANDROID,
        pattern VkGetMemoryAndroidHardwareBufferANDROID,
        HS_vkGetMemoryAndroidHardwareBufferANDROID,
        PFN_vkGetMemoryAndroidHardwareBufferANDROID,
        unwrapVkGetMemoryAndroidHardwareBufferANDROID,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Handles,
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
import           GHC.Ptr
                                                                                              (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
                                                                                              (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Defines
                                                                                              (AHardwareBuffer)
import           Graphics.Vulkan.Types.Enum.VkChromaLocation
import           Graphics.Vulkan.Types.Enum.VkComponentSwizzle
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlags
                                                                                              (VkExternalMemoryHandleTypeBitmask (..),
                                                                                              VkExternalMemoryHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.VkFilter
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkFormatFeatureFlags
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags
import           Graphics.Vulkan.Types.Enum.VkImageLayout
import           Graphics.Vulkan.Types.Enum.VkImageTiling
import           Graphics.Vulkan.Types.Enum.VkImageType
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkSamplerYcbcrModelConversion
import           Graphics.Vulkan.Types.Enum.VkSamplerYcbcrRange
import           Graphics.Vulkan.Types.Enum.VkSharingMode
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAndroidHardwareBufferFormatPropertiesANDROID
import           Graphics.Vulkan.Types.Struct.VkAndroidHardwareBufferPropertiesANDROID
import           Graphics.Vulkan.Types.Struct.VkAndroidHardwareBufferUsageANDROID
import           Graphics.Vulkan.Types.Struct.VkComponentMapping
import           Graphics.Vulkan.Types.Struct.VkExtent3D
import           Graphics.Vulkan.Types.Struct.VkExternalFormatANDROID
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties2
import           Graphics.Vulkan.Types.Struct.VkImportAndroidHardwareBufferInfoANDROID
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo
import           Graphics.Vulkan.Types.Struct.VkMemoryGetAndroidHardwareBufferInfoANDROID
import           Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionCreateInfo

pattern VkGetAndroidHardwareBufferPropertiesANDROID :: CString

pattern VkGetAndroidHardwareBufferPropertiesANDROID <-
        (is_VkGetAndroidHardwareBufferPropertiesANDROID -> True)
  where VkGetAndroidHardwareBufferPropertiesANDROID
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetAndroidHardwareBufferPropertiesANDROIDvkGetAndroidHardwareBufferPropertiesANDROID registry at www.khronos.org>
type HS_vkGetAndroidHardwareBufferPropertiesANDROID =
     VkDevice -- ^ device
              ->
       Ptr AHardwareBuffer -- ^ buffer
                           ->
         Ptr VkAndroidHardwareBufferPropertiesANDROID -- ^ pProperties
                                                      -> IO VkResult

type PFN_vkGetAndroidHardwareBufferPropertiesANDROID =
     FunPtr HS_vkGetAndroidHardwareBufferPropertiesANDROID

foreign import ccall "dynamic"
               unwrapVkGetAndroidHardwareBufferPropertiesANDROID ::
               PFN_vkGetAndroidHardwareBufferPropertiesANDROID ->
                 HS_vkGetAndroidHardwareBufferPropertiesANDROID

instance VulkanProc "vkGetAndroidHardwareBufferPropertiesANDROID"
         where
        type VkProcType "vkGetAndroidHardwareBufferPropertiesANDROID" =
             HS_vkGetAndroidHardwareBufferPropertiesANDROID
        vkProcSymbol = _VkGetAndroidHardwareBufferPropertiesANDROID

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetAndroidHardwareBufferPropertiesANDROID

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetMemoryAndroidHardwareBufferANDROID :: CString

pattern VkGetMemoryAndroidHardwareBufferANDROID <-
        (is_VkGetMemoryAndroidHardwareBufferANDROID -> True)
  where VkGetMemoryAndroidHardwareBufferANDROID
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetMemoryAndroidHardwareBufferANDROIDvkGetMemoryAndroidHardwareBufferANDROID registry at www.khronos.org>
type HS_vkGetMemoryAndroidHardwareBufferANDROID =
     VkDevice -- ^ device
              ->
       Ptr VkMemoryGetAndroidHardwareBufferInfoANDROID -- ^ pInfo
                                                       ->
         Ptr (Ptr AHardwareBuffer) -- ^ pBuffer
                                   -> IO VkResult

type PFN_vkGetMemoryAndroidHardwareBufferANDROID =
     FunPtr HS_vkGetMemoryAndroidHardwareBufferANDROID

foreign import ccall "dynamic"
               unwrapVkGetMemoryAndroidHardwareBufferANDROID ::
               PFN_vkGetMemoryAndroidHardwareBufferANDROID ->
                 HS_vkGetMemoryAndroidHardwareBufferANDROID

instance VulkanProc "vkGetMemoryAndroidHardwareBufferANDROID" where
        type VkProcType "vkGetMemoryAndroidHardwareBufferANDROID" =
             HS_vkGetMemoryAndroidHardwareBufferANDROID
        vkProcSymbol = _VkGetMemoryAndroidHardwareBufferANDROID

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetMemoryAndroidHardwareBufferANDROID

        {-# INLINE unwrapVkProcPtr #-}

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
  where VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
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
        :: VkExternalMemoryHandleTypeFlagBits

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID
        = VkExternalMemoryHandleTypeFlagBits 1024

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
