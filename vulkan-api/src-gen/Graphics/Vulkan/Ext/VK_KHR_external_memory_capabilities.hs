{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
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
        module Graphics.Vulkan.Types.Struct.VkExternalBufferPropertiesKHR,
        module Graphics.Vulkan.Types.Struct.VkExternalImageFormatPropertiesKHR,
        module Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlagBitsKHR,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagBitsKHR,
        module Graphics.Vulkan.Types.Struct.VkExternalMemoryPropertiesKHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalBufferInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalImageFormatInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceIDPropertiesKHR,
        VkGetPhysicalDeviceExternalBufferPropertiesKHR,
        pattern VkGetPhysicalDeviceExternalBufferPropertiesKHR,
        HS_vkGetPhysicalDeviceExternalBufferPropertiesKHR,
        PFN_vkGetPhysicalDeviceExternalBufferPropertiesKHR,
        unwrapVkGetPhysicalDeviceExternalBufferPropertiesKHR,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkBufferCreateFlags,
        module Graphics.Vulkan.Types.Enum.VkBufferUsageFlags,
        module Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlags,
        module Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkExternalBufferProperties,
        module Graphics.Vulkan.Types.Struct.VkExternalMemoryProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalBufferInfo,
        VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION,
        VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR,
        pattern VK_LUID_SIZE_KHR,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR,
        pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR)
       where
import           GHC.Ptr
                                                                                          (Ptr (..))
import           Graphics.Vulkan.Constants
                                                                                          (pattern VK_LUID_SIZE_KHR)
import           Graphics.Vulkan.Core_1_1
                                                                                          (pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES,
                                                                                          pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES,
                                                                                          pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO,
                                                                                          pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO,
                                                                                          pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
                                                                                          (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkBufferCreateFlags
import           Graphics.Vulkan.Types.Enum.VkBufferUsageFlags
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlagBitsKHR
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryFeatureFlags
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagBitsKHR
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkExternalBufferProperties
import           Graphics.Vulkan.Types.Struct.VkExternalBufferPropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkExternalImageFormatPropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkExternalMemoryProperties
import           Graphics.Vulkan.Types.Struct.VkExternalMemoryPropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalBufferInfo
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalBufferInfoKHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalImageFormatInfoKHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceIDPropertiesKHR

pattern VkGetPhysicalDeviceExternalBufferPropertiesKHR :: CString

pattern VkGetPhysicalDeviceExternalBufferPropertiesKHR <-
        (is_VkGetPhysicalDeviceExternalBufferPropertiesKHR -> True)
  where VkGetPhysicalDeviceExternalBufferPropertiesKHR
          = _VkGetPhysicalDeviceExternalBufferPropertiesKHR

{-# INLINE _VkGetPhysicalDeviceExternalBufferPropertiesKHR #-}

_VkGetPhysicalDeviceExternalBufferPropertiesKHR :: CString
_VkGetPhysicalDeviceExternalBufferPropertiesKHR
  = Ptr "vkGetPhysicalDeviceExternalBufferPropertiesKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceExternalBufferPropertiesKHR #-}

is_VkGetPhysicalDeviceExternalBufferPropertiesKHR ::
                                                  CString -> Bool
is_VkGetPhysicalDeviceExternalBufferPropertiesKHR
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceExternalBufferPropertiesKHR

type VkGetPhysicalDeviceExternalBufferPropertiesKHR =
     "vkGetPhysicalDeviceExternalBufferPropertiesKHR"

-- | This is an alias for `vkGetPhysicalDeviceExternalBufferProperties`.
--
--   > () vkGetPhysicalDeviceExternalBufferPropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalBufferInfo* pExternalBufferInfo
--   >     , VkExternalBufferProperties* pExternalBufferProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalBufferPropertiesKHRvkGetPhysicalDeviceExternalBufferPropertiesKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceExternalBufferPropertiesKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkPhysicalDeviceExternalBufferInfo -- ^ pExternalBufferInfo
                                              ->
         Ptr VkExternalBufferProperties -- ^ pExternalBufferProperties
                                        -> IO ()

type PFN_vkGetPhysicalDeviceExternalBufferPropertiesKHR =
     FunPtr HS_vkGetPhysicalDeviceExternalBufferPropertiesKHR

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceExternalBufferPropertiesKHR ::
               PFN_vkGetPhysicalDeviceExternalBufferPropertiesKHR ->
                 HS_vkGetPhysicalDeviceExternalBufferPropertiesKHR

instance VulkanProc
           "vkGetPhysicalDeviceExternalBufferPropertiesKHR"
         where
        type VkProcType "vkGetPhysicalDeviceExternalBufferPropertiesKHR" =
             HS_vkGetPhysicalDeviceExternalBufferPropertiesKHR
        vkProcSymbol = _VkGetPhysicalDeviceExternalBufferPropertiesKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr
          = unwrapVkGetPhysicalDeviceExternalBufferPropertiesKHR

        {-# INLINE unwrapVkProcPtr #-}

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
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO

pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR =
        VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO

pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR =
        VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR =
        VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR =
        VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR =
        VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR =
        VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR =
        VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR =
        VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR =
        VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR =
        VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT

pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR =
        VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT

pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR =
        VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT

pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR =
        VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT
