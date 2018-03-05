{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_external_memory_capabilities
       (-- * Vulkan extension: @VK_KHR_external_memory_capabilities@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @72@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkBufferCreateFlags,
        module Graphics.Vulkan.Types.Enum.VkBufferUsageFlags,
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.Struct.VkExternalBufferPropertiesKHR,
        module Graphics.Vulkan.Types.Struct.VkExternalImageFormatPropertiesKHR,
        module Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlagsKHR,
        module Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR,
        module Graphics.Vulkan.Types.Struct.VkExternalMemoryPropertiesKHR,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkImageCreateFlags,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties2KHR,
        module Graphics.Vulkan.Types.Enum.VkImageTiling,
        module Graphics.Vulkan.Types.Enum.VkImageType,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalBufferInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalImageFormatInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceIDPropertiesKHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceImageFormatInfo2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties,
        module Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        vkGetPhysicalDeviceExternalBufferPropertiesKHR,
        vkGetPhysicalDeviceExternalBufferPropertiesKHRSafe,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION,
        VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR,
        pattern VK_LUID_SIZE_KHR, VK_LUID_SIZE_KHR)
       where
import           GHC.Ptr
                                                                                          (Ptr (..))
import           Graphics.Vulkan.Constants
                                                                                          (VK_LUID_SIZE_KHR,
                                                                                          pattern VK_LUID_SIZE_KHR)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkBufferCreateFlags
import           Graphics.Vulkan.Types.Enum.VkBufferUsageFlags
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags
import           Graphics.Vulkan.Types.Enum.VkImageTiling
import           Graphics.Vulkan.Types.Enum.VkImageType
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkExtent3D
import           Graphics.Vulkan.Types.Struct.VkExternalBufferPropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkExternalImageFormatPropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkExternalMemoryPropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalBufferInfoKHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalImageFormatInfoKHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceIDPropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceImageFormatInfo2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties

-- | > () vkGetPhysicalDeviceExternalBufferPropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalBufferInfoKHR* pExternalBufferInfo
--   >     , VkExternalBufferPropertiesKHR* pExternalBufferProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceExternalBufferPropertiesKHR.html vkGetPhysicalDeviceExternalBufferPropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalBufferPropertiesKHR"
               vkGetPhysicalDeviceExternalBufferPropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalBufferInfoKHR -- ^ pExternalBufferInfo
                                                           ->
                   Ptr VkExternalBufferPropertiesKHR -- ^ pExternalBufferProperties
                                                     -> IO ()

-- | > () vkGetPhysicalDeviceExternalBufferPropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalBufferInfoKHR* pExternalBufferInfo
--   >     , VkExternalBufferPropertiesKHR* pExternalBufferProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceExternalBufferPropertiesKHR.html vkGetPhysicalDeviceExternalBufferPropertiesKHR registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceExternalBufferPropertiesKHR"
               vkGetPhysicalDeviceExternalBufferPropertiesKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalBufferInfoKHR -- ^ pExternalBufferInfo
                                                           ->
                   Ptr VkExternalBufferPropertiesKHR -- ^ pExternalBufferProperties
                                                     -> IO ()

pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME ::
        CString

pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
  = Ptr "VK_KHR_external_memory_capabilities\NUL"#

{-# INLINE is_VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
           #-}

is_VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME ::
                                                      CString -> Bool
is_VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME

type VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME =
     "VK_KHR_external_memory_capabilities"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR
        = VkStructureType 1000071000

pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR =
        VkStructureType 1000071001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR
        = VkStructureType 1000071002

pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR =
        VkStructureType 1000071003

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR =
        VkStructureType 1000071004
