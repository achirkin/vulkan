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
module Graphics.Vulkan.Ext.VK_KHR_external_semaphore_capabilities
       (-- * Vulkan extension: @VK_KHR_external_semaphore_capabilities@
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
        -- Extension number: @77@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        module Graphics.Vulkan.Types.Enum.External,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.External,
        module Graphics.Vulkan.Types.Struct.PhysicalDevice,
        VkGetPhysicalDeviceExternalSemaphorePropertiesKHR,
        pattern VkGetPhysicalDeviceExternalSemaphorePropertiesKHR,
        HS_vkGetPhysicalDeviceExternalSemaphorePropertiesKHR,
        PFN_vkGetPhysicalDeviceExternalSemaphorePropertiesKHR,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION,
        VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR,
        pattern VK_LUID_SIZE_KHR,
        pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR,
        pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR,
        pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR,
        pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR,
        pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR,
        pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR,
        pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR)
       where
import           GHC.Ptr                                     (Ptr (..))
import           Graphics.Vulkan.Constants                   (pattern VK_LUID_SIZE_KHR)
import           Graphics.Vulkan.Core_1_1                    (pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES,
                                                              pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.External
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.External
import           Graphics.Vulkan.Types.Struct.PhysicalDevice

pattern VkGetPhysicalDeviceExternalSemaphorePropertiesKHR ::
        CString

pattern VkGetPhysicalDeviceExternalSemaphorePropertiesKHR <-
        (is_VkGetPhysicalDeviceExternalSemaphorePropertiesKHR -> True)
  where VkGetPhysicalDeviceExternalSemaphorePropertiesKHR
          = _VkGetPhysicalDeviceExternalSemaphorePropertiesKHR

{-# INLINE _VkGetPhysicalDeviceExternalSemaphorePropertiesKHR #-}

_VkGetPhysicalDeviceExternalSemaphorePropertiesKHR :: CString
_VkGetPhysicalDeviceExternalSemaphorePropertiesKHR
  = Ptr "vkGetPhysicalDeviceExternalSemaphorePropertiesKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceExternalSemaphorePropertiesKHR #-}

is_VkGetPhysicalDeviceExternalSemaphorePropertiesKHR ::
                                                     CString -> Bool
is_VkGetPhysicalDeviceExternalSemaphorePropertiesKHR
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceExternalSemaphorePropertiesKHR

type VkGetPhysicalDeviceExternalSemaphorePropertiesKHR =
     "vkGetPhysicalDeviceExternalSemaphorePropertiesKHR"

-- | This is an alias for `vkGetPhysicalDeviceExternalSemaphoreProperties`.
--
--   > void vkGetPhysicalDeviceExternalSemaphorePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceExternalSemaphoreInfo* pExternalSemaphoreInfo
--   >     , VkExternalSemaphoreProperties* pExternalSemaphoreProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalSemaphorePropertiesKHR vkGetPhysicalDeviceExternalSemaphorePropertiesKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceExternalSemaphorePropertiesKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkPhysicalDeviceExternalSemaphoreInfo -- ^ pExternalSemaphoreInfo
                                                 ->
         Ptr VkExternalSemaphoreProperties -- ^ pExternalSemaphoreProperties
                                           -> IO ()

type PFN_vkGetPhysicalDeviceExternalSemaphorePropertiesKHR =
     FunPtr HS_vkGetPhysicalDeviceExternalSemaphorePropertiesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceExternalSemaphorePropertiesKHRUnsafe ::
               PFN_vkGetPhysicalDeviceExternalSemaphorePropertiesKHR ->
                 HS_vkGetPhysicalDeviceExternalSemaphorePropertiesKHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceExternalSemaphorePropertiesKHRSafe ::
               PFN_vkGetPhysicalDeviceExternalSemaphorePropertiesKHR ->
                 HS_vkGetPhysicalDeviceExternalSemaphorePropertiesKHR

instance VulkanProc
           "vkGetPhysicalDeviceExternalSemaphorePropertiesKHR"
         where
        type VkProcType "vkGetPhysicalDeviceExternalSemaphorePropertiesKHR"
             = HS_vkGetPhysicalDeviceExternalSemaphorePropertiesKHR
        vkProcSymbol = _VkGetPhysicalDeviceExternalSemaphorePropertiesKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkGetPhysicalDeviceExternalSemaphorePropertiesKHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceExternalSemaphorePropertiesKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME ::
        CString

pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
           #-}

_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
  = Ptr "VK_KHR_external_semaphore_capabilities\NUL"#

{-# INLINE is_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
           #-}

is_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME ::
                                                         CString -> Bool
is_VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME

type VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME =
     "VK_KHR_external_semaphore_capabilities"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR
        = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO

pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR =
        VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR =
        VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR =
        VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
        = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR =
        VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT

pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR =
        VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT

pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR =
        VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT

pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR =
        VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT
