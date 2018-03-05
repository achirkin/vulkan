{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_external_fence_capabilities
       (-- * Vulkan extension: @VK_KHR_external_fence_capabilities@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @jessehall@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @113@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkExternalFenceFeatureFlagsKHR,
        module Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlagsKHR,
        module Graphics.Vulkan.Types.Struct.VkExternalFencePropertiesKHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalFenceInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceIDPropertiesKHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties,
        module Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType,
        module Graphics.Vulkan.Types.Enum.VkSampleCountFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        vkGetPhysicalDeviceExternalFencePropertiesKHR,
        vkGetPhysicalDeviceExternalFencePropertiesKHRSafe,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION,
        VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR,
        pattern VK_LUID_SIZE_KHR, VK_LUID_SIZE_KHR)
       where
import           GHC.Ptr
                                                                                    (Ptr (..))
import           Graphics.Vulkan.Constants
                                                                                    (VK_LUID_SIZE_KHR,
                                                                                    pattern VK_LUID_SIZE_KHR)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkExternalFenceFeatureFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkPhysicalDeviceType
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkExternalFencePropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalFenceInfoKHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceIDPropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceLimits
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties

-- | > () vkGetPhysicalDeviceExternalFencePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalFenceInfoKHR* pExternalFenceInfo
--   >     , VkExternalFencePropertiesKHR* pExternalFenceProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceExternalFencePropertiesKHR.html vkGetPhysicalDeviceExternalFencePropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalFencePropertiesKHR"
               vkGetPhysicalDeviceExternalFencePropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalFenceInfoKHR -- ^ pExternalFenceInfo
                                                          ->
                   Ptr VkExternalFencePropertiesKHR -- ^ pExternalFenceProperties
                                                    -> IO ()

-- | > () vkGetPhysicalDeviceExternalFencePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalFenceInfoKHR* pExternalFenceInfo
--   >     , VkExternalFencePropertiesKHR* pExternalFenceProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceExternalFencePropertiesKHR.html vkGetPhysicalDeviceExternalFencePropertiesKHR registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceExternalFencePropertiesKHR"
               vkGetPhysicalDeviceExternalFencePropertiesKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalFenceInfoKHR -- ^ pExternalFenceInfo
                                                          ->
                   Ptr VkExternalFencePropertiesKHR -- ^ pExternalFenceProperties
                                                    -> IO ()

pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME ::
        CString

pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
  = Ptr "VK_KHR_external_fence_capabilities\NUL"#

{-# INLINE is_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME ::
                                                     CString -> Bool
is_VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME

type VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME =
     "VK_KHR_external_fence_capabilities"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR =
        VkStructureType 1000112000

pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR =
        VkStructureType 1000112001
