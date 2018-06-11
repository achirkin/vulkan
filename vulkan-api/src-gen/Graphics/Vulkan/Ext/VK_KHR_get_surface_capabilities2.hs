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
module Graphics.Vulkan.Ext.VK_KHR_get_surface_capabilities2
       (-- * Vulkan extension: @VK_KHR_get_surface_capabilities2@
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
        -- Extension number: @120@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        module Graphics.Vulkan.Types.Enum.Color,
        module Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR,
        module Graphics.Vulkan.Types.Struct.Extent,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.Format,
        module Graphics.Vulkan.Types.Enum.Image,
        module Graphics.Vulkan.Types.Struct.PhysicalDevice,
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Struct.Surface,
        module Graphics.Vulkan.Types.Enum.Surface,
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceSurfaceCapabilities2KHR,
        pattern VkGetPhysicalDeviceSurfaceCapabilities2KHR,
        HS_vkGetPhysicalDeviceSurfaceCapabilities2KHR,
        PFN_vkGetPhysicalDeviceSurfaceCapabilities2KHR,
        VkGetPhysicalDeviceSurfaceFormats2KHR,
        pattern VkGetPhysicalDeviceSurfaceFormats2KHR,
        HS_vkGetPhysicalDeviceSurfaceFormats2KHR,
        PFN_vkGetPhysicalDeviceSurfaceFormats2KHR,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION,
        pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION,
        VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME,
        pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR,
        pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR)
       where
import           GHC.Ptr                                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                      (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.Color
import           Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR
import           Graphics.Vulkan.Types.Enum.Format
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.Surface
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.PhysicalDevice
import           Graphics.Vulkan.Types.Struct.Surface

pattern VkGetPhysicalDeviceSurfaceCapabilities2KHR :: CString

pattern VkGetPhysicalDeviceSurfaceCapabilities2KHR <-
        (is_VkGetPhysicalDeviceSurfaceCapabilities2KHR -> True)
  where VkGetPhysicalDeviceSurfaceCapabilities2KHR
          = _VkGetPhysicalDeviceSurfaceCapabilities2KHR

{-# INLINE _VkGetPhysicalDeviceSurfaceCapabilities2KHR #-}

_VkGetPhysicalDeviceSurfaceCapabilities2KHR :: CString
_VkGetPhysicalDeviceSurfaceCapabilities2KHR
  = Ptr "vkGetPhysicalDeviceSurfaceCapabilities2KHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceSurfaceCapabilities2KHR #-}

is_VkGetPhysicalDeviceSurfaceCapabilities2KHR :: CString -> Bool
is_VkGetPhysicalDeviceSurfaceCapabilities2KHR
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceSurfaceCapabilities2KHR

type VkGetPhysicalDeviceSurfaceCapabilities2KHR =
     "vkGetPhysicalDeviceSurfaceCapabilities2KHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceCapabilities2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSurfaceInfo2KHR* pSurfaceInfo
--   >     , VkSurfaceCapabilities2KHR* pSurfaceCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfaceCapabilities2KHR vkGetPhysicalDeviceSurfaceCapabilities2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceSurfaceCapabilities2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkPhysicalDeviceSurfaceInfo2KHR -- ^ pSurfaceInfo
                                           ->
         Ptr VkSurfaceCapabilities2KHR -- ^ pSurfaceCapabilities
                                       -> IO VkResult

type PFN_vkGetPhysicalDeviceSurfaceCapabilities2KHR =
     FunPtr HS_vkGetPhysicalDeviceSurfaceCapabilities2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceCapabilities2KHRUnsafe ::
               PFN_vkGetPhysicalDeviceSurfaceCapabilities2KHR ->
                 HS_vkGetPhysicalDeviceSurfaceCapabilities2KHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceCapabilities2KHRSafe ::
               PFN_vkGetPhysicalDeviceSurfaceCapabilities2KHR ->
                 HS_vkGetPhysicalDeviceSurfaceCapabilities2KHR

instance VulkanProc "vkGetPhysicalDeviceSurfaceCapabilities2KHR"
         where
        type VkProcType "vkGetPhysicalDeviceSurfaceCapabilities2KHR" =
             HS_vkGetPhysicalDeviceSurfaceCapabilities2KHR
        vkProcSymbol = _VkGetPhysicalDeviceSurfaceCapabilities2KHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkGetPhysicalDeviceSurfaceCapabilities2KHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceSurfaceCapabilities2KHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceSurfaceFormats2KHR :: CString

pattern VkGetPhysicalDeviceSurfaceFormats2KHR <-
        (is_VkGetPhysicalDeviceSurfaceFormats2KHR -> True)
  where VkGetPhysicalDeviceSurfaceFormats2KHR
          = _VkGetPhysicalDeviceSurfaceFormats2KHR

{-# INLINE _VkGetPhysicalDeviceSurfaceFormats2KHR #-}

_VkGetPhysicalDeviceSurfaceFormats2KHR :: CString
_VkGetPhysicalDeviceSurfaceFormats2KHR
  = Ptr "vkGetPhysicalDeviceSurfaceFormats2KHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceSurfaceFormats2KHR #-}

is_VkGetPhysicalDeviceSurfaceFormats2KHR :: CString -> Bool
is_VkGetPhysicalDeviceSurfaceFormats2KHR
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceSurfaceFormats2KHR

type VkGetPhysicalDeviceSurfaceFormats2KHR =
     "vkGetPhysicalDeviceSurfaceFormats2KHR"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceFormats2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSurfaceInfo2KHR* pSurfaceInfo
--   >     , uint32_t* pSurfaceFormatCount
--   >     , VkSurfaceFormat2KHR* pSurfaceFormats
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfaceFormats2KHR vkGetPhysicalDeviceSurfaceFormats2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceSurfaceFormats2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkPhysicalDeviceSurfaceInfo2KHR -- ^ pSurfaceInfo
                                           ->
         Ptr Word32 -- ^ pSurfaceFormatCount
                    -> Ptr VkSurfaceFormat2KHR -- ^ pSurfaceFormats
                                               -> IO VkResult

type PFN_vkGetPhysicalDeviceSurfaceFormats2KHR =
     FunPtr HS_vkGetPhysicalDeviceSurfaceFormats2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceFormats2KHRUnsafe ::
               PFN_vkGetPhysicalDeviceSurfaceFormats2KHR ->
                 HS_vkGetPhysicalDeviceSurfaceFormats2KHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceFormats2KHRSafe ::
               PFN_vkGetPhysicalDeviceSurfaceFormats2KHR ->
                 HS_vkGetPhysicalDeviceSurfaceFormats2KHR

instance VulkanProc "vkGetPhysicalDeviceSurfaceFormats2KHR" where
        type VkProcType "vkGetPhysicalDeviceSurfaceFormats2KHR" =
             HS_vkGetPhysicalDeviceSurfaceFormats2KHR
        vkProcSymbol = _VkGetPhysicalDeviceSurfaceFormats2KHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkGetPhysicalDeviceSurfaceFormats2KHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceSurfaceFormats2KHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION = 1

type VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION = 1

pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME :: CString

pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME <-
        (is_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME -> True)
  where VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
          = _VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME

{-# INLINE _VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME #-}

_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME :: CString
_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
  = Ptr "VK_KHR_get_surface_capabilities2\NUL"#

{-# INLINE is_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME #-}

is_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME ::
                                                    CString -> Bool
is_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME

type VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME =
     "VK_KHR_get_surface_capabilities2"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR =
        VkStructureType 1000119000

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR =
        VkStructureType 1000119001

pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR :: VkStructureType

pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR =
        VkStructureType 1000119002
