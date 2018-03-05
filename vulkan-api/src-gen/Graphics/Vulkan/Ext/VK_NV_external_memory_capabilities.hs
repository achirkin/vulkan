{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_NV_external_memory_capabilities
       (-- * Vulkan extension: @VK_NV_external_memory_capabilities@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @NV@
        --
        -- type: @instance@
        --
        -- Extension number: @56@
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkExtent3D,
        module Graphics.Vulkan.Types.Struct.VkExternalImageFormatPropertiesNV,
        module Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlagsNV,
        module Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsNV,
        module Graphics.Vulkan.Types.Struct.VkImageFormatProperties,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        -- > #include "vk_platform.h"
        vkGetPhysicalDeviceExternalImageFormatPropertiesNV,
        vkGetPhysicalDeviceExternalImageFormatPropertiesNVSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkImageCreateFlags,
        module Graphics.Vulkan.Types.Enum.VkImageTiling,
        module Graphics.Vulkan.Types.Enum.VkImageType,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Handles,
        VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION,
        pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION,
        VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME,
        pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME)
       where
import           GHC.Ptr                                                        (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlagsNV
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsNV
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags
import           Graphics.Vulkan.Types.Enum.VkImageTiling
import           Graphics.Vulkan.Types.Enum.VkImageType
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkExtent3D
import           Graphics.Vulkan.Types.Struct.VkExternalImageFormatPropertiesNV
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceExternalImageFormatPropertiesNV
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkImageType type
--   >     , VkImageTiling tiling
--   >     , VkImageUsageFlags usage
--   >     , VkImageCreateFlags flags
--   >     , VkExternalMemoryHandleTypeFlagsNV externalHandleType
--   >     , VkExternalImageFormatPropertiesNV* pExternalImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceExternalImageFormatPropertiesNV.html vkGetPhysicalDeviceExternalImageFormatPropertiesNV registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalImageFormatPropertiesNV"
               vkGetPhysicalDeviceExternalImageFormatPropertiesNV ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkFormat -- ^ format
                          ->
                   VkImageType -- ^ type
                               ->
                     VkImageTiling -- ^ tiling
                                   ->
                       VkImageUsageFlags -- ^ usage
                                         ->
                         VkImageCreateFlags -- ^ flags
                                            ->
                           VkExternalMemoryHandleTypeFlagsNV -- ^ externalHandleType
                                                             ->
                             Ptr VkExternalImageFormatPropertiesNV -- ^ pExternalImageFormatProperties
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceExternalImageFormatPropertiesNV
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkImageType type
--   >     , VkImageTiling tiling
--   >     , VkImageUsageFlags usage
--   >     , VkImageCreateFlags flags
--   >     , VkExternalMemoryHandleTypeFlagsNV externalHandleType
--   >     , VkExternalImageFormatPropertiesNV* pExternalImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceExternalImageFormatPropertiesNV.html vkGetPhysicalDeviceExternalImageFormatPropertiesNV registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceExternalImageFormatPropertiesNV"
               vkGetPhysicalDeviceExternalImageFormatPropertiesNVSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkFormat -- ^ format
                          ->
                   VkImageType -- ^ type
                               ->
                     VkImageTiling -- ^ tiling
                                   ->
                       VkImageUsageFlags -- ^ usage
                                         ->
                         VkImageCreateFlags -- ^ flags
                                            ->
                           VkExternalMemoryHandleTypeFlagsNV -- ^ externalHandleType
                                                             ->
                             Ptr VkExternalImageFormatPropertiesNV -- ^ pExternalImageFormatProperties
                                                                   -> IO VkResult

pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1

type VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1

pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME ::
        CString

pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME <-
        (is_VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME -> True)
  where VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
          = _VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME

{-# INLINE _VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME #-}

_VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME :: CString
_VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
  = Ptr "VK_NV_external_memory_capabilities\NUL"#

{-# INLINE is_VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME #-}

is_VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME ::
                                                     CString -> Bool
is_VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME

type VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME =
     "VK_NV_external_memory_capabilities"
