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
        module Graphics.Vulkan.Types.Enum.VkExternalFenceFeatureFlagBitsKHR,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlagBitsKHR,
        module Graphics.Vulkan.Types.Struct.VkExternalFencePropertiesKHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalFenceInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceIDPropertiesKHR,
        VkGetPhysicalDeviceExternalFencePropertiesKHR,
        pattern VkGetPhysicalDeviceExternalFencePropertiesKHR,
        HS_vkGetPhysicalDeviceExternalFencePropertiesKHR,
        PFN_vkGetPhysicalDeviceExternalFencePropertiesKHR,
        unwrapVkGetPhysicalDeviceExternalFencePropertiesKHR,
        vkGetPhysicalDeviceExternalFencePropertiesKHR,
        vkGetPhysicalDeviceExternalFencePropertiesKHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkExternalFenceFeatureFlags,
        module Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlags,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkExternalFenceProperties,
        module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalFenceInfo,
        VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION,
        VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR,
        pattern VK_LUID_SIZE_KHR,
        pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR,
        pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR,
        pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR,
        pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR,
        pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR,
        pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR)
       where
import           GHC.Ptr
                                                                                    (Ptr (..))
import           Graphics.Vulkan.Constants
                                                                                    (pattern VK_LUID_SIZE_KHR)
import           Graphics.Vulkan.Core_1_1
                                                                                    (pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES,
                                                                                    pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.InstanceProc
                                                                                    (VulkanInstanceProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkExternalFenceFeatureFlagBitsKHR
import           Graphics.Vulkan.Types.Enum.VkExternalFenceFeatureFlags
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlagBitsKHR
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlags
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkExternalFenceProperties
import           Graphics.Vulkan.Types.Struct.VkExternalFencePropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalFenceInfo
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalFenceInfoKHR
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceIDPropertiesKHR

pattern VkGetPhysicalDeviceExternalFencePropertiesKHR :: CString

pattern VkGetPhysicalDeviceExternalFencePropertiesKHR <-
        (is_VkGetPhysicalDeviceExternalFencePropertiesKHR -> True)
  where VkGetPhysicalDeviceExternalFencePropertiesKHR
          = _VkGetPhysicalDeviceExternalFencePropertiesKHR

{-# INLINE _VkGetPhysicalDeviceExternalFencePropertiesKHR #-}

_VkGetPhysicalDeviceExternalFencePropertiesKHR :: CString
_VkGetPhysicalDeviceExternalFencePropertiesKHR
  = Ptr "vkGetPhysicalDeviceExternalFencePropertiesKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceExternalFencePropertiesKHR #-}

is_VkGetPhysicalDeviceExternalFencePropertiesKHR :: CString -> Bool
is_VkGetPhysicalDeviceExternalFencePropertiesKHR
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceExternalFencePropertiesKHR

type VkGetPhysicalDeviceExternalFencePropertiesKHR =
     "vkGetPhysicalDeviceExternalFencePropertiesKHR"

-- | This is an alias for `vkGetPhysicalDeviceExternalFenceProperties`.
--
--   > () vkGetPhysicalDeviceExternalFencePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalFenceInfo* pExternalFenceInfo
--   >     , VkExternalFenceProperties* pExternalFenceProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceExternalFencePropertiesKHR.html vkGetPhysicalDeviceExternalFencePropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceExternalFencePropertiesKHR"
               vkGetPhysicalDeviceExternalFencePropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalFenceInfo -- ^ pExternalFenceInfo
                                                       ->
                   Ptr VkExternalFenceProperties -- ^ pExternalFenceProperties
                                                 -> IO ()

-- | This is an alias for `vkGetPhysicalDeviceExternalFenceProperties`.
--
--   > () vkGetPhysicalDeviceExternalFencePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalFenceInfo* pExternalFenceInfo
--   >     , VkExternalFenceProperties* pExternalFenceProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceExternalFencePropertiesKHR.html vkGetPhysicalDeviceExternalFencePropertiesKHR registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceExternalFencePropertiesKHR"
               vkGetPhysicalDeviceExternalFencePropertiesKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceExternalFenceInfo -- ^ pExternalFenceInfo
                                                       ->
                   Ptr VkExternalFenceProperties -- ^ pExternalFenceProperties
                                                 -> IO ()

-- | This is an alias for `vkGetPhysicalDeviceExternalFenceProperties`.
--
--   > () vkGetPhysicalDeviceExternalFencePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalFenceInfo* pExternalFenceInfo
--   >     , VkExternalFenceProperties* pExternalFenceProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceExternalFencePropertiesKHR.html vkGetPhysicalDeviceExternalFencePropertiesKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceExternalFencePropertiesKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkPhysicalDeviceExternalFenceInfo -- ^ pExternalFenceInfo
                                             ->
         Ptr VkExternalFenceProperties -- ^ pExternalFenceProperties
                                       -> IO ()

type PFN_vkGetPhysicalDeviceExternalFencePropertiesKHR =
     FunPtr HS_vkGetPhysicalDeviceExternalFencePropertiesKHR

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceExternalFencePropertiesKHR ::
               PFN_vkGetPhysicalDeviceExternalFencePropertiesKHR ->
                 HS_vkGetPhysicalDeviceExternalFencePropertiesKHR

instance VulkanInstanceProc
           "vkGetPhysicalDeviceExternalFencePropertiesKHR"
         where
        type VkInstanceProcType
               "vkGetPhysicalDeviceExternalFencePropertiesKHR"
             = HS_vkGetPhysicalDeviceExternalFencePropertiesKHR
        vkInstanceProcSymbol
          = _VkGetPhysicalDeviceExternalFencePropertiesKHR

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc
          = unwrapVkGetPhysicalDeviceExternalFencePropertiesKHR

        {-# INLINE unwrapVkInstanceProc #-}

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

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR =
        VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO

pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR =
        VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR =
        VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR =
        VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR =
        VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT

pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR =
        VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT

pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR =
        VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT

pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR =
        VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
