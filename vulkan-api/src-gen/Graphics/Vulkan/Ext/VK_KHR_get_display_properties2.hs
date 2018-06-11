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
module Graphics.Vulkan.Ext.VK_KHR_get_display_properties2
       (-- * Vulkan extension: @VK_KHR_get_display_properties2@
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
        -- Extension number: @122@
        --
        -- Required extensions: 'VK_KHR_display'.
        --

        -- ** Required extensions: 'VK_KHR_display'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.Display,
        module Graphics.Vulkan.Types.Enum.Display,
        module Graphics.Vulkan.Types.Struct.Extent,
        module Graphics.Vulkan.Types.Struct.Offset,
        module Graphics.Vulkan.Types.Enum.StructureType,
        module Graphics.Vulkan.Types.Enum.Surface,
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceDisplayProperties2KHR,
        pattern VkGetPhysicalDeviceDisplayProperties2KHR,
        HS_vkGetPhysicalDeviceDisplayProperties2KHR,
        PFN_vkGetPhysicalDeviceDisplayProperties2KHR,
        VkGetPhysicalDeviceDisplayPlaneProperties2KHR,
        pattern VkGetPhysicalDeviceDisplayPlaneProperties2KHR,
        HS_vkGetPhysicalDeviceDisplayPlaneProperties2KHR,
        PFN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR,
        VkGetDisplayModeProperties2KHR,
        pattern VkGetDisplayModeProperties2KHR,
        HS_vkGetDisplayModeProperties2KHR,
        PFN_vkGetDisplayModeProperties2KHR,
        VkGetDisplayPlaneCapabilities2KHR,
        pattern VkGetDisplayPlaneCapabilities2KHR,
        HS_vkGetDisplayPlaneCapabilities2KHR,
        PFN_vkGetDisplayPlaneCapabilities2KHR,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Handles,
        VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION,
        pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION,
        VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME,
        pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR,
        pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR)
       where
import           GHC.Ptr                                  (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc             (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.Display
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.Surface
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Display
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.Offset

pattern VkGetPhysicalDeviceDisplayProperties2KHR :: CString

pattern VkGetPhysicalDeviceDisplayProperties2KHR <-
        (is_VkGetPhysicalDeviceDisplayProperties2KHR -> True)
  where VkGetPhysicalDeviceDisplayProperties2KHR
          = _VkGetPhysicalDeviceDisplayProperties2KHR

{-# INLINE _VkGetPhysicalDeviceDisplayProperties2KHR #-}

_VkGetPhysicalDeviceDisplayProperties2KHR :: CString
_VkGetPhysicalDeviceDisplayProperties2KHR
  = Ptr "vkGetPhysicalDeviceDisplayProperties2KHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceDisplayProperties2KHR #-}

is_VkGetPhysicalDeviceDisplayProperties2KHR :: CString -> Bool
is_VkGetPhysicalDeviceDisplayProperties2KHR
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceDisplayProperties2KHR

type VkGetPhysicalDeviceDisplayProperties2KHR =
     "vkGetPhysicalDeviceDisplayProperties2KHR"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceDisplayProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayProperties2KHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceDisplayProperties2KHR vkGetPhysicalDeviceDisplayProperties2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceDisplayProperties2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr Word32 -- ^ pPropertyCount
                  -> Ptr VkDisplayProperties2KHR -- ^ pProperties
                                                 -> IO VkResult

type PFN_vkGetPhysicalDeviceDisplayProperties2KHR =
     FunPtr HS_vkGetPhysicalDeviceDisplayProperties2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceDisplayProperties2KHRUnsafe ::
               PFN_vkGetPhysicalDeviceDisplayProperties2KHR ->
                 HS_vkGetPhysicalDeviceDisplayProperties2KHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceDisplayProperties2KHRSafe ::
               PFN_vkGetPhysicalDeviceDisplayProperties2KHR ->
                 HS_vkGetPhysicalDeviceDisplayProperties2KHR

instance VulkanProc "vkGetPhysicalDeviceDisplayProperties2KHR"
         where
        type VkProcType "vkGetPhysicalDeviceDisplayProperties2KHR" =
             HS_vkGetPhysicalDeviceDisplayProperties2KHR
        vkProcSymbol = _VkGetPhysicalDeviceDisplayProperties2KHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkGetPhysicalDeviceDisplayProperties2KHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceDisplayProperties2KHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceDisplayPlaneProperties2KHR :: CString

pattern VkGetPhysicalDeviceDisplayPlaneProperties2KHR <-
        (is_VkGetPhysicalDeviceDisplayPlaneProperties2KHR -> True)
  where VkGetPhysicalDeviceDisplayPlaneProperties2KHR
          = _VkGetPhysicalDeviceDisplayPlaneProperties2KHR

{-# INLINE _VkGetPhysicalDeviceDisplayPlaneProperties2KHR #-}

_VkGetPhysicalDeviceDisplayPlaneProperties2KHR :: CString
_VkGetPhysicalDeviceDisplayPlaneProperties2KHR
  = Ptr "vkGetPhysicalDeviceDisplayPlaneProperties2KHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceDisplayPlaneProperties2KHR #-}

is_VkGetPhysicalDeviceDisplayPlaneProperties2KHR :: CString -> Bool
is_VkGetPhysicalDeviceDisplayPlaneProperties2KHR
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceDisplayPlaneProperties2KHR

type VkGetPhysicalDeviceDisplayPlaneProperties2KHR =
     "vkGetPhysicalDeviceDisplayPlaneProperties2KHR"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceDisplayPlaneProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayPlaneProperties2KHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceDisplayPlaneProperties2KHR vkGetPhysicalDeviceDisplayPlaneProperties2KHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceDisplayPlaneProperties2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr Word32 -- ^ pPropertyCount
                  -> Ptr VkDisplayPlaneProperties2KHR -- ^ pProperties
                                                      -> IO VkResult

type PFN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR =
     FunPtr HS_vkGetPhysicalDeviceDisplayPlaneProperties2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceDisplayPlaneProperties2KHRUnsafe ::
               PFN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR ->
                 HS_vkGetPhysicalDeviceDisplayPlaneProperties2KHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceDisplayPlaneProperties2KHRSafe ::
               PFN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR ->
                 HS_vkGetPhysicalDeviceDisplayPlaneProperties2KHR

instance VulkanProc "vkGetPhysicalDeviceDisplayPlaneProperties2KHR"
         where
        type VkProcType "vkGetPhysicalDeviceDisplayPlaneProperties2KHR" =
             HS_vkGetPhysicalDeviceDisplayPlaneProperties2KHR
        vkProcSymbol = _VkGetPhysicalDeviceDisplayPlaneProperties2KHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkGetPhysicalDeviceDisplayPlaneProperties2KHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceDisplayPlaneProperties2KHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetDisplayModeProperties2KHR :: CString

pattern VkGetDisplayModeProperties2KHR <-
        (is_VkGetDisplayModeProperties2KHR -> True)
  where VkGetDisplayModeProperties2KHR
          = _VkGetDisplayModeProperties2KHR

{-# INLINE _VkGetDisplayModeProperties2KHR #-}

_VkGetDisplayModeProperties2KHR :: CString
_VkGetDisplayModeProperties2KHR
  = Ptr "vkGetDisplayModeProperties2KHR\NUL"#

{-# INLINE is_VkGetDisplayModeProperties2KHR #-}

is_VkGetDisplayModeProperties2KHR :: CString -> Bool
is_VkGetDisplayModeProperties2KHR
  = (EQ ==) . cmpCStrings _VkGetDisplayModeProperties2KHR

type VkGetDisplayModeProperties2KHR =
     "vkGetDisplayModeProperties2KHR"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayModeProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayModeProperties2KHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDisplayModeProperties2KHR vkGetDisplayModeProperties2KHR registry at www.khronos.org>
type HS_vkGetDisplayModeProperties2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       VkDisplayKHR -- ^ display
                    ->
         Ptr Word32 -- ^ pPropertyCount
                    -> Ptr VkDisplayModeProperties2KHR -- ^ pProperties
                                                       -> IO VkResult

type PFN_vkGetDisplayModeProperties2KHR =
     FunPtr HS_vkGetDisplayModeProperties2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetDisplayModeProperties2KHRUnsafe ::
               PFN_vkGetDisplayModeProperties2KHR ->
                 HS_vkGetDisplayModeProperties2KHR

foreign import ccall safe "dynamic"
               unwrapVkGetDisplayModeProperties2KHRSafe ::
               PFN_vkGetDisplayModeProperties2KHR ->
                 HS_vkGetDisplayModeProperties2KHR

instance VulkanProc "vkGetDisplayModeProperties2KHR" where
        type VkProcType "vkGetDisplayModeProperties2KHR" =
             HS_vkGetDisplayModeProperties2KHR
        vkProcSymbol = _VkGetDisplayModeProperties2KHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkGetDisplayModeProperties2KHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkGetDisplayModeProperties2KHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetDisplayPlaneCapabilities2KHR :: CString

pattern VkGetDisplayPlaneCapabilities2KHR <-
        (is_VkGetDisplayPlaneCapabilities2KHR -> True)
  where VkGetDisplayPlaneCapabilities2KHR
          = _VkGetDisplayPlaneCapabilities2KHR

{-# INLINE _VkGetDisplayPlaneCapabilities2KHR #-}

_VkGetDisplayPlaneCapabilities2KHR :: CString
_VkGetDisplayPlaneCapabilities2KHR
  = Ptr "vkGetDisplayPlaneCapabilities2KHR\NUL"#

{-# INLINE is_VkGetDisplayPlaneCapabilities2KHR #-}

is_VkGetDisplayPlaneCapabilities2KHR :: CString -> Bool
is_VkGetDisplayPlaneCapabilities2KHR
  = (EQ ==) . cmpCStrings _VkGetDisplayPlaneCapabilities2KHR

type VkGetDisplayPlaneCapabilities2KHR =
     "vkGetDisplayPlaneCapabilities2KHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayPlaneCapabilities2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkDisplayPlaneInfo2KHR* pDisplayPlaneInfo
--   >     , VkDisplayPlaneCapabilities2KHR* pCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDisplayPlaneCapabilities2KHR vkGetDisplayPlaneCapabilities2KHR registry at www.khronos.org>
type HS_vkGetDisplayPlaneCapabilities2KHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr VkDisplayPlaneInfo2KHR -- ^ pDisplayPlaneInfo
                                  ->
         Ptr VkDisplayPlaneCapabilities2KHR -- ^ pCapabilities
                                            -> IO VkResult

type PFN_vkGetDisplayPlaneCapabilities2KHR =
     FunPtr HS_vkGetDisplayPlaneCapabilities2KHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetDisplayPlaneCapabilities2KHRUnsafe ::
               PFN_vkGetDisplayPlaneCapabilities2KHR ->
                 HS_vkGetDisplayPlaneCapabilities2KHR

foreign import ccall safe "dynamic"
               unwrapVkGetDisplayPlaneCapabilities2KHRSafe ::
               PFN_vkGetDisplayPlaneCapabilities2KHR ->
                 HS_vkGetDisplayPlaneCapabilities2KHR

instance VulkanProc "vkGetDisplayPlaneCapabilities2KHR" where
        type VkProcType "vkGetDisplayPlaneCapabilities2KHR" =
             HS_vkGetDisplayPlaneCapabilities2KHR
        vkProcSymbol = _VkGetDisplayPlaneCapabilities2KHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe
          = unwrapVkGetDisplayPlaneCapabilities2KHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkGetDisplayPlaneCapabilities2KHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION = 1

type VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION = 1

pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME :: CString

pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME <-
        (is_VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME -> True)
  where VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME
          = _VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME

{-# INLINE _VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME #-}

_VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME :: CString
_VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME
  = Ptr "VK_KHR_get_display_properties2\NUL"#

{-# INLINE is_VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME #-}

is_VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME ::
                                                  CString -> Bool
is_VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME
  = (EQ ==) .
      cmpCStrings _VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME

type VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME =
     "VK_KHR_get_display_properties2"

pattern VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR =
        VkStructureType 1000121000

pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR =
        VkStructureType 1000121001

pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR =
        VkStructureType 1000121002

pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR =
        VkStructureType 1000121003

pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR =
        VkStructureType 1000121004
