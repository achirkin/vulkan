{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
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
        module Graphics.Vulkan.Types.Struct.Extent,
        module Graphics.Vulkan.Types.Struct.External,
        module Graphics.Vulkan.Types.Enum.External,
        module Graphics.Vulkan.Types.Struct.Image,
        module Graphics.Vulkan.Types.Enum.SampleCountFlags,
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceExternalImageFormatPropertiesNV,
        pattern VkGetPhysicalDeviceExternalImageFormatPropertiesNV,
        HS_vkGetPhysicalDeviceExternalImageFormatPropertiesNV,
        PFN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.Format,
        module Graphics.Vulkan.Types.Enum.Image,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Handles,
        VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION,
        pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION,
        VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME,
        pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME)
       where
import           GHC.Ptr                                     (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.External
import           Graphics.Vulkan.Types.Enum.Format
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.SampleCountFlags
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.External
import           Graphics.Vulkan.Types.Struct.Image

pattern VkGetPhysicalDeviceExternalImageFormatPropertiesNV ::
        CString

pattern VkGetPhysicalDeviceExternalImageFormatPropertiesNV <-
        (is_VkGetPhysicalDeviceExternalImageFormatPropertiesNV -> True)
  where VkGetPhysicalDeviceExternalImageFormatPropertiesNV
          = _VkGetPhysicalDeviceExternalImageFormatPropertiesNV

{-# INLINE _VkGetPhysicalDeviceExternalImageFormatPropertiesNV #-}

_VkGetPhysicalDeviceExternalImageFormatPropertiesNV :: CString
_VkGetPhysicalDeviceExternalImageFormatPropertiesNV
  = Ptr "vkGetPhysicalDeviceExternalImageFormatPropertiesNV\NUL"#

{-# INLINE is_VkGetPhysicalDeviceExternalImageFormatPropertiesNV
           #-}

is_VkGetPhysicalDeviceExternalImageFormatPropertiesNV ::
                                                      CString -> Bool
is_VkGetPhysicalDeviceExternalImageFormatPropertiesNV
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceExternalImageFormatPropertiesNV

type VkGetPhysicalDeviceExternalImageFormatPropertiesNV =
     "vkGetPhysicalDeviceExternalImageFormatPropertiesNV"

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceExternalImageFormatPropertiesNV vkGetPhysicalDeviceExternalImageFormatPropertiesNV registry at www.khronos.org>
type HS_vkGetPhysicalDeviceExternalImageFormatPropertiesNV =
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

type PFN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV =
     FunPtr HS_vkGetPhysicalDeviceExternalImageFormatPropertiesNV

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceExternalImageFormatPropertiesNVUnsafe ::
               PFN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV ->
                 HS_vkGetPhysicalDeviceExternalImageFormatPropertiesNV

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceExternalImageFormatPropertiesNVSafe ::
               PFN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV ->
                 HS_vkGetPhysicalDeviceExternalImageFormatPropertiesNV

instance VulkanProc
           "vkGetPhysicalDeviceExternalImageFormatPropertiesNV"
         where
        type VkProcType
               "vkGetPhysicalDeviceExternalImageFormatPropertiesNV"
             = HS_vkGetPhysicalDeviceExternalImageFormatPropertiesNV
        vkProcSymbol = _VkGetPhysicalDeviceExternalImageFormatPropertiesNV

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkGetPhysicalDeviceExternalImageFormatPropertiesNVUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceExternalImageFormatPropertiesNVSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
