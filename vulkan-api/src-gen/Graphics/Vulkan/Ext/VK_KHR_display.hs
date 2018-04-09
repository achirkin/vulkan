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
module Graphics.Vulkan.Ext.VK_KHR_display
       (-- * Vulkan extension: @VK_KHR_display@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo,Norbert Nopper @FslNopper@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @3@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkDisplayModeCreateInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkDisplayModeParametersKHR,
        module Graphics.Vulkan.Types.Struct.VkDisplayModePropertiesKHR,
        module Graphics.Vulkan.Types.Enum.VkDisplayPlaneAlphaFlagsKHR,
        module Graphics.Vulkan.Types.Struct.VkDisplayPlaneCapabilitiesKHR,
        module Graphics.Vulkan.Types.Struct.VkDisplayPlanePropertiesKHR,
        module Graphics.Vulkan.Types.Struct.VkDisplayPropertiesKHR,
        module Graphics.Vulkan.Types.Struct.VkDisplaySurfaceCreateInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkExtent2D,
        module Graphics.Vulkan.Types.Struct.VkOffset2D,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR,
        -- > #include "vk_platform.h"
        VkGetPhysicalDeviceDisplayPropertiesKHR,
        pattern VkGetPhysicalDeviceDisplayPropertiesKHR,
        HS_vkGetPhysicalDeviceDisplayPropertiesKHR,
        PFN_vkGetPhysicalDeviceDisplayPropertiesKHR,
        unwrapVkGetPhysicalDeviceDisplayPropertiesKHR,
        vkGetPhysicalDeviceDisplayPropertiesKHR,
        vkGetPhysicalDeviceDisplayPropertiesKHRSafe,
        VkGetPhysicalDeviceDisplayPlanePropertiesKHR,
        pattern VkGetPhysicalDeviceDisplayPlanePropertiesKHR,
        HS_vkGetPhysicalDeviceDisplayPlanePropertiesKHR,
        PFN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR,
        unwrapVkGetPhysicalDeviceDisplayPlanePropertiesKHR,
        vkGetPhysicalDeviceDisplayPlanePropertiesKHR,
        vkGetPhysicalDeviceDisplayPlanePropertiesKHRSafe,
        VkGetDisplayPlaneSupportedDisplaysKHR,
        pattern VkGetDisplayPlaneSupportedDisplaysKHR,
        HS_vkGetDisplayPlaneSupportedDisplaysKHR,
        PFN_vkGetDisplayPlaneSupportedDisplaysKHR,
        unwrapVkGetDisplayPlaneSupportedDisplaysKHR,
        vkGetDisplayPlaneSupportedDisplaysKHR,
        vkGetDisplayPlaneSupportedDisplaysKHRSafe,
        VkGetDisplayModePropertiesKHR,
        pattern VkGetDisplayModePropertiesKHR,
        HS_vkGetDisplayModePropertiesKHR,
        PFN_vkGetDisplayModePropertiesKHR,
        unwrapVkGetDisplayModePropertiesKHR, vkGetDisplayModePropertiesKHR,
        vkGetDisplayModePropertiesKHRSafe, VkCreateDisplayModeKHR,
        pattern VkCreateDisplayModeKHR, HS_vkCreateDisplayModeKHR,
        PFN_vkCreateDisplayModeKHR, unwrapVkCreateDisplayModeKHR,
        vkCreateDisplayModeKHR, vkCreateDisplayModeKHRSafe,
        VkGetDisplayPlaneCapabilitiesKHR,
        pattern VkGetDisplayPlaneCapabilitiesKHR,
        HS_vkGetDisplayPlaneCapabilitiesKHR,
        PFN_vkGetDisplayPlaneCapabilitiesKHR,
        unwrapVkGetDisplayPlaneCapabilitiesKHR,
        vkGetDisplayPlaneCapabilitiesKHR,
        vkGetDisplayPlaneCapabilitiesKHRSafe,
        VkCreateDisplayPlaneSurfaceKHR,
        pattern VkCreateDisplayPlaneSurfaceKHR,
        HS_vkCreateDisplayPlaneSurfaceKHR,
        PFN_vkCreateDisplayPlaneSurfaceKHR,
        unwrapVkCreateDisplayPlaneSurfaceKHR,
        vkCreateDisplayPlaneSurfaceKHR, vkCreateDisplayPlaneSurfaceKHRSafe,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        VK_KHR_DISPLAY_SPEC_VERSION, pattern VK_KHR_DISPLAY_SPEC_VERSION,
        VK_KHR_DISPLAY_EXTENSION_NAME,
        pattern VK_KHR_DISPLAY_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR,
        pattern VK_OBJECT_TYPE_DISPLAY_KHR,
        pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR)
       where
import           GHC.Ptr                                                    (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc                               (VulkanProc (..))
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkDisplayPlaneAlphaFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkInternalAllocationType
import           Graphics.Vulkan.Types.Enum.VkObjectType                    (VkObjectType (..))
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkSystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
import           Graphics.Vulkan.Types.Struct.VkDisplayModeCreateInfoKHR
import           Graphics.Vulkan.Types.Struct.VkDisplayModeParametersKHR
import           Graphics.Vulkan.Types.Struct.VkDisplayModePropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkDisplayPlaneCapabilitiesKHR
import           Graphics.Vulkan.Types.Struct.VkDisplayPlanePropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkDisplayPropertiesKHR
import           Graphics.Vulkan.Types.Struct.VkDisplaySurfaceCreateInfoKHR
import           Graphics.Vulkan.Types.Struct.VkExtent2D
import           Graphics.Vulkan.Types.Struct.VkOffset2D

pattern VkGetPhysicalDeviceDisplayPropertiesKHR :: CString

pattern VkGetPhysicalDeviceDisplayPropertiesKHR <-
        (is_VkGetPhysicalDeviceDisplayPropertiesKHR -> True)
  where VkGetPhysicalDeviceDisplayPropertiesKHR
          = _VkGetPhysicalDeviceDisplayPropertiesKHR

{-# INLINE _VkGetPhysicalDeviceDisplayPropertiesKHR #-}

_VkGetPhysicalDeviceDisplayPropertiesKHR :: CString
_VkGetPhysicalDeviceDisplayPropertiesKHR
  = Ptr "vkGetPhysicalDeviceDisplayPropertiesKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceDisplayPropertiesKHR #-}

is_VkGetPhysicalDeviceDisplayPropertiesKHR :: CString -> Bool
is_VkGetPhysicalDeviceDisplayPropertiesKHR
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceDisplayPropertiesKHR

type VkGetPhysicalDeviceDisplayPropertiesKHR =
     "vkGetPhysicalDeviceDisplayPropertiesKHR"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceDisplayPropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayPropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceDisplayPropertiesKHRvkGetPhysicalDeviceDisplayPropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceDisplayPropertiesKHR"
               vkGetPhysicalDeviceDisplayPropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pPropertyCount
                            -> Ptr VkDisplayPropertiesKHR -- ^ pProperties
                                                          -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceDisplayPropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayPropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceDisplayPropertiesKHRvkGetPhysicalDeviceDisplayPropertiesKHR registry at www.khronos.org>
foreign import ccall safe "vkGetPhysicalDeviceDisplayPropertiesKHR"
               vkGetPhysicalDeviceDisplayPropertiesKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pPropertyCount
                            -> Ptr VkDisplayPropertiesKHR -- ^ pProperties
                                                          -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceDisplayPropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayPropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceDisplayPropertiesKHRvkGetPhysicalDeviceDisplayPropertiesKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceDisplayPropertiesKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr Word32 -- ^ pPropertyCount
                  -> Ptr VkDisplayPropertiesKHR -- ^ pProperties
                                                -> IO VkResult

type PFN_vkGetPhysicalDeviceDisplayPropertiesKHR =
     FunPtr HS_vkGetPhysicalDeviceDisplayPropertiesKHR

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceDisplayPropertiesKHR ::
               PFN_vkGetPhysicalDeviceDisplayPropertiesKHR ->
                 HS_vkGetPhysicalDeviceDisplayPropertiesKHR

instance VulkanProc "vkGetPhysicalDeviceDisplayPropertiesKHR" where
        type VkProcType "vkGetPhysicalDeviceDisplayPropertiesKHR" =
             HS_vkGetPhysicalDeviceDisplayPropertiesKHR
        vkProcSymbol = _VkGetPhysicalDeviceDisplayPropertiesKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceDisplayPropertiesKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceDisplayPlanePropertiesKHR :: CString

pattern VkGetPhysicalDeviceDisplayPlanePropertiesKHR <-
        (is_VkGetPhysicalDeviceDisplayPlanePropertiesKHR -> True)
  where VkGetPhysicalDeviceDisplayPlanePropertiesKHR
          = _VkGetPhysicalDeviceDisplayPlanePropertiesKHR

{-# INLINE _VkGetPhysicalDeviceDisplayPlanePropertiesKHR #-}

_VkGetPhysicalDeviceDisplayPlanePropertiesKHR :: CString
_VkGetPhysicalDeviceDisplayPlanePropertiesKHR
  = Ptr "vkGetPhysicalDeviceDisplayPlanePropertiesKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceDisplayPlanePropertiesKHR #-}

is_VkGetPhysicalDeviceDisplayPlanePropertiesKHR :: CString -> Bool
is_VkGetPhysicalDeviceDisplayPlanePropertiesKHR
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceDisplayPlanePropertiesKHR

type VkGetPhysicalDeviceDisplayPlanePropertiesKHR =
     "vkGetPhysicalDeviceDisplayPlanePropertiesKHR"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceDisplayPlanePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayPlanePropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceDisplayPlanePropertiesKHRvkGetPhysicalDeviceDisplayPlanePropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceDisplayPlanePropertiesKHR"
               vkGetPhysicalDeviceDisplayPlanePropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pPropertyCount
                            -> Ptr VkDisplayPlanePropertiesKHR -- ^ pProperties
                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceDisplayPlanePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayPlanePropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceDisplayPlanePropertiesKHRvkGetPhysicalDeviceDisplayPlanePropertiesKHR registry at www.khronos.org>
foreign import ccall safe
               "vkGetPhysicalDeviceDisplayPlanePropertiesKHR"
               vkGetPhysicalDeviceDisplayPlanePropertiesKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pPropertyCount
                            -> Ptr VkDisplayPlanePropertiesKHR -- ^ pProperties
                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceDisplayPlanePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayPlanePropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceDisplayPlanePropertiesKHRvkGetPhysicalDeviceDisplayPlanePropertiesKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceDisplayPlanePropertiesKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Ptr Word32 -- ^ pPropertyCount
                  -> Ptr VkDisplayPlanePropertiesKHR -- ^ pProperties
                                                     -> IO VkResult

type PFN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR =
     FunPtr HS_vkGetPhysicalDeviceDisplayPlanePropertiesKHR

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceDisplayPlanePropertiesKHR ::
               PFN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR ->
                 HS_vkGetPhysicalDeviceDisplayPlanePropertiesKHR

instance VulkanProc "vkGetPhysicalDeviceDisplayPlanePropertiesKHR"
         where
        type VkProcType "vkGetPhysicalDeviceDisplayPlanePropertiesKHR" =
             HS_vkGetPhysicalDeviceDisplayPlanePropertiesKHR
        vkProcSymbol = _VkGetPhysicalDeviceDisplayPlanePropertiesKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr
          = unwrapVkGetPhysicalDeviceDisplayPlanePropertiesKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetDisplayPlaneSupportedDisplaysKHR :: CString

pattern VkGetDisplayPlaneSupportedDisplaysKHR <-
        (is_VkGetDisplayPlaneSupportedDisplaysKHR -> True)
  where VkGetDisplayPlaneSupportedDisplaysKHR
          = _VkGetDisplayPlaneSupportedDisplaysKHR

{-# INLINE _VkGetDisplayPlaneSupportedDisplaysKHR #-}

_VkGetDisplayPlaneSupportedDisplaysKHR :: CString
_VkGetDisplayPlaneSupportedDisplaysKHR
  = Ptr "vkGetDisplayPlaneSupportedDisplaysKHR\NUL"#

{-# INLINE is_VkGetDisplayPlaneSupportedDisplaysKHR #-}

is_VkGetDisplayPlaneSupportedDisplaysKHR :: CString -> Bool
is_VkGetDisplayPlaneSupportedDisplaysKHR
  = (EQ ==) . cmpCStrings _VkGetDisplayPlaneSupportedDisplaysKHR

type VkGetDisplayPlaneSupportedDisplaysKHR =
     "vkGetDisplayPlaneSupportedDisplaysKHR"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayPlaneSupportedDisplaysKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t planeIndex
--   >     , uint32_t* pDisplayCount
--   >     , VkDisplayKHR* pDisplays
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDisplayPlaneSupportedDisplaysKHRvkGetDisplayPlaneSupportedDisplaysKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetDisplayPlaneSupportedDisplaysKHR"
               vkGetDisplayPlaneSupportedDisplaysKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ planeIndex
                        -> Ptr Word32 -- ^ pDisplayCount
                                      -> Ptr VkDisplayKHR -- ^ pDisplays
                                                          -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayPlaneSupportedDisplaysKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t planeIndex
--   >     , uint32_t* pDisplayCount
--   >     , VkDisplayKHR* pDisplays
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDisplayPlaneSupportedDisplaysKHRvkGetDisplayPlaneSupportedDisplaysKHR registry at www.khronos.org>
foreign import ccall safe "vkGetDisplayPlaneSupportedDisplaysKHR"
               vkGetDisplayPlaneSupportedDisplaysKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ planeIndex
                        -> Ptr Word32 -- ^ pDisplayCount
                                      -> Ptr VkDisplayKHR -- ^ pDisplays
                                                          -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayPlaneSupportedDisplaysKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t planeIndex
--   >     , uint32_t* pDisplayCount
--   >     , VkDisplayKHR* pDisplays
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDisplayPlaneSupportedDisplaysKHRvkGetDisplayPlaneSupportedDisplaysKHR registry at www.khronos.org>
type HS_vkGetDisplayPlaneSupportedDisplaysKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Word32 -- ^ planeIndex
              -> Ptr Word32 -- ^ pDisplayCount
                            -> Ptr VkDisplayKHR -- ^ pDisplays
                                                -> IO VkResult

type PFN_vkGetDisplayPlaneSupportedDisplaysKHR =
     FunPtr HS_vkGetDisplayPlaneSupportedDisplaysKHR

foreign import ccall "dynamic"
               unwrapVkGetDisplayPlaneSupportedDisplaysKHR ::
               PFN_vkGetDisplayPlaneSupportedDisplaysKHR ->
                 HS_vkGetDisplayPlaneSupportedDisplaysKHR

instance VulkanProc "vkGetDisplayPlaneSupportedDisplaysKHR" where
        type VkProcType "vkGetDisplayPlaneSupportedDisplaysKHR" =
             HS_vkGetDisplayPlaneSupportedDisplaysKHR
        vkProcSymbol = _VkGetDisplayPlaneSupportedDisplaysKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetDisplayPlaneSupportedDisplaysKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetDisplayModePropertiesKHR :: CString

pattern VkGetDisplayModePropertiesKHR <-
        (is_VkGetDisplayModePropertiesKHR -> True)
  where VkGetDisplayModePropertiesKHR
          = _VkGetDisplayModePropertiesKHR

{-# INLINE _VkGetDisplayModePropertiesKHR #-}

_VkGetDisplayModePropertiesKHR :: CString
_VkGetDisplayModePropertiesKHR
  = Ptr "vkGetDisplayModePropertiesKHR\NUL"#

{-# INLINE is_VkGetDisplayModePropertiesKHR #-}

is_VkGetDisplayModePropertiesKHR :: CString -> Bool
is_VkGetDisplayModePropertiesKHR
  = (EQ ==) . cmpCStrings _VkGetDisplayModePropertiesKHR

type VkGetDisplayModePropertiesKHR =
     "vkGetDisplayModePropertiesKHR"

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayModePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayModePropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDisplayModePropertiesKHRvkGetDisplayModePropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetDisplayModePropertiesKHR"
               vkGetDisplayModePropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkDisplayKHR -- ^ display
                              ->
                   Ptr Word32 -- ^ pPropertyCount
                              -> Ptr VkDisplayModePropertiesKHR -- ^ pProperties
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayModePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayModePropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDisplayModePropertiesKHRvkGetDisplayModePropertiesKHR registry at www.khronos.org>
foreign import ccall safe "vkGetDisplayModePropertiesKHR"
               vkGetDisplayModePropertiesKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkDisplayKHR -- ^ display
                              ->
                   Ptr Word32 -- ^ pPropertyCount
                              -> Ptr VkDisplayModePropertiesKHR -- ^ pProperties
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayModePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayModePropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDisplayModePropertiesKHRvkGetDisplayModePropertiesKHR registry at www.khronos.org>
type HS_vkGetDisplayModePropertiesKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       VkDisplayKHR -- ^ display
                    ->
         Ptr Word32 -- ^ pPropertyCount
                    -> Ptr VkDisplayModePropertiesKHR -- ^ pProperties
                                                      -> IO VkResult

type PFN_vkGetDisplayModePropertiesKHR =
     FunPtr HS_vkGetDisplayModePropertiesKHR

foreign import ccall "dynamic" unwrapVkGetDisplayModePropertiesKHR
               ::
               PFN_vkGetDisplayModePropertiesKHR ->
                 HS_vkGetDisplayModePropertiesKHR

instance VulkanProc "vkGetDisplayModePropertiesKHR" where
        type VkProcType "vkGetDisplayModePropertiesKHR" =
             HS_vkGetDisplayModePropertiesKHR
        vkProcSymbol = _VkGetDisplayModePropertiesKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetDisplayModePropertiesKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateDisplayModeKHR :: CString

pattern VkCreateDisplayModeKHR <-
        (is_VkCreateDisplayModeKHR -> True)
  where VkCreateDisplayModeKHR = _VkCreateDisplayModeKHR

{-# INLINE _VkCreateDisplayModeKHR #-}

_VkCreateDisplayModeKHR :: CString
_VkCreateDisplayModeKHR = Ptr "vkCreateDisplayModeKHR\NUL"#

{-# INLINE is_VkCreateDisplayModeKHR #-}

is_VkCreateDisplayModeKHR :: CString -> Bool
is_VkCreateDisplayModeKHR
  = (EQ ==) . cmpCStrings _VkCreateDisplayModeKHR

type VkCreateDisplayModeKHR = "vkCreateDisplayModeKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkCreateDisplayModeKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     , const VkDisplayModeCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDisplayModeKHR* pMode
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDisplayModeKHRvkCreateDisplayModeKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDisplayModeKHR"
               vkCreateDisplayModeKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkDisplayKHR -- ^ display
                              ->
                   Ptr VkDisplayModeCreateInfoKHR -- ^ pCreateInfo
                                                  ->
                     Ptr VkAllocationCallbacks -- ^ pAllocator
                                               -> Ptr VkDisplayModeKHR -- ^ pMode
                                                                       -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkCreateDisplayModeKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     , const VkDisplayModeCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDisplayModeKHR* pMode
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDisplayModeKHRvkCreateDisplayModeKHR registry at www.khronos.org>
foreign import ccall safe "vkCreateDisplayModeKHR"
               vkCreateDisplayModeKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkDisplayKHR -- ^ display
                              ->
                   Ptr VkDisplayModeCreateInfoKHR -- ^ pCreateInfo
                                                  ->
                     Ptr VkAllocationCallbacks -- ^ pAllocator
                                               -> Ptr VkDisplayModeKHR -- ^ pMode
                                                                       -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkCreateDisplayModeKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     , const VkDisplayModeCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDisplayModeKHR* pMode
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDisplayModeKHRvkCreateDisplayModeKHR registry at www.khronos.org>
type HS_vkCreateDisplayModeKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       VkDisplayKHR -- ^ display
                    ->
         Ptr VkDisplayModeCreateInfoKHR -- ^ pCreateInfo
                                        ->
           Ptr VkAllocationCallbacks -- ^ pAllocator
                                     -> Ptr VkDisplayModeKHR -- ^ pMode
                                                             -> IO VkResult

type PFN_vkCreateDisplayModeKHR = FunPtr HS_vkCreateDisplayModeKHR

foreign import ccall "dynamic" unwrapVkCreateDisplayModeKHR ::
               PFN_vkCreateDisplayModeKHR -> HS_vkCreateDisplayModeKHR

instance VulkanProc "vkCreateDisplayModeKHR" where
        type VkProcType "vkCreateDisplayModeKHR" =
             HS_vkCreateDisplayModeKHR
        vkProcSymbol = _VkCreateDisplayModeKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateDisplayModeKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetDisplayPlaneCapabilitiesKHR :: CString

pattern VkGetDisplayPlaneCapabilitiesKHR <-
        (is_VkGetDisplayPlaneCapabilitiesKHR -> True)
  where VkGetDisplayPlaneCapabilitiesKHR
          = _VkGetDisplayPlaneCapabilitiesKHR

{-# INLINE _VkGetDisplayPlaneCapabilitiesKHR #-}

_VkGetDisplayPlaneCapabilitiesKHR :: CString
_VkGetDisplayPlaneCapabilitiesKHR
  = Ptr "vkGetDisplayPlaneCapabilitiesKHR\NUL"#

{-# INLINE is_VkGetDisplayPlaneCapabilitiesKHR #-}

is_VkGetDisplayPlaneCapabilitiesKHR :: CString -> Bool
is_VkGetDisplayPlaneCapabilitiesKHR
  = (EQ ==) . cmpCStrings _VkGetDisplayPlaneCapabilitiesKHR

type VkGetDisplayPlaneCapabilitiesKHR =
     "vkGetDisplayPlaneCapabilitiesKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayPlaneCapabilitiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayModeKHR mode
--   >     , uint32_t planeIndex
--   >     , VkDisplayPlaneCapabilitiesKHR* pCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDisplayPlaneCapabilitiesKHRvkGetDisplayPlaneCapabilitiesKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetDisplayPlaneCapabilitiesKHR"
               vkGetDisplayPlaneCapabilitiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkDisplayModeKHR -- ^ mode
                                  ->
                   Word32 -- ^ planeIndex
                          -> Ptr VkDisplayPlaneCapabilitiesKHR -- ^ pCapabilities
                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayPlaneCapabilitiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayModeKHR mode
--   >     , uint32_t planeIndex
--   >     , VkDisplayPlaneCapabilitiesKHR* pCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDisplayPlaneCapabilitiesKHRvkGetDisplayPlaneCapabilitiesKHR registry at www.khronos.org>
foreign import ccall safe "vkGetDisplayPlaneCapabilitiesKHR"
               vkGetDisplayPlaneCapabilitiesKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkDisplayModeKHR -- ^ mode
                                  ->
                   Word32 -- ^ planeIndex
                          -> Ptr VkDisplayPlaneCapabilitiesKHR -- ^ pCapabilities
                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayPlaneCapabilitiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayModeKHR mode
--   >     , uint32_t planeIndex
--   >     , VkDisplayPlaneCapabilitiesKHR* pCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetDisplayPlaneCapabilitiesKHRvkGetDisplayPlaneCapabilitiesKHR registry at www.khronos.org>
type HS_vkGetDisplayPlaneCapabilitiesKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       VkDisplayModeKHR -- ^ mode
                        ->
         Word32 -- ^ planeIndex
                -> Ptr VkDisplayPlaneCapabilitiesKHR -- ^ pCapabilities
                                                     -> IO VkResult

type PFN_vkGetDisplayPlaneCapabilitiesKHR =
     FunPtr HS_vkGetDisplayPlaneCapabilitiesKHR

foreign import ccall "dynamic"
               unwrapVkGetDisplayPlaneCapabilitiesKHR ::
               PFN_vkGetDisplayPlaneCapabilitiesKHR ->
                 HS_vkGetDisplayPlaneCapabilitiesKHR

instance VulkanProc "vkGetDisplayPlaneCapabilitiesKHR" where
        type VkProcType "vkGetDisplayPlaneCapabilitiesKHR" =
             HS_vkGetDisplayPlaneCapabilitiesKHR
        vkProcSymbol = _VkGetDisplayPlaneCapabilitiesKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetDisplayPlaneCapabilitiesKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkCreateDisplayPlaneSurfaceKHR :: CString

pattern VkCreateDisplayPlaneSurfaceKHR <-
        (is_VkCreateDisplayPlaneSurfaceKHR -> True)
  where VkCreateDisplayPlaneSurfaceKHR
          = _VkCreateDisplayPlaneSurfaceKHR

{-# INLINE _VkCreateDisplayPlaneSurfaceKHR #-}

_VkCreateDisplayPlaneSurfaceKHR :: CString
_VkCreateDisplayPlaneSurfaceKHR
  = Ptr "vkCreateDisplayPlaneSurfaceKHR\NUL"#

{-# INLINE is_VkCreateDisplayPlaneSurfaceKHR #-}

is_VkCreateDisplayPlaneSurfaceKHR :: CString -> Bool
is_VkCreateDisplayPlaneSurfaceKHR
  = (EQ ==) . cmpCStrings _VkCreateDisplayPlaneSurfaceKHR

type VkCreateDisplayPlaneSurfaceKHR =
     "vkCreateDisplayPlaneSurfaceKHR"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDisplayPlaneSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkDisplaySurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDisplayPlaneSurfaceKHRvkCreateDisplayPlaneSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDisplayPlaneSurfaceKHR"
               vkCreateDisplayPlaneSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkDisplaySurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDisplayPlaneSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkDisplaySurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDisplayPlaneSurfaceKHRvkCreateDisplayPlaneSurfaceKHR registry at www.khronos.org>
foreign import ccall safe "vkCreateDisplayPlaneSurfaceKHR"
               vkCreateDisplayPlaneSurfaceKHRSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkDisplaySurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDisplayPlaneSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkDisplaySurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateDisplayPlaneSurfaceKHRvkCreateDisplayPlaneSurfaceKHR registry at www.khronos.org>
type HS_vkCreateDisplayPlaneSurfaceKHR =
     VkInstance -- ^ instance
                ->
       Ptr VkDisplaySurfaceCreateInfoKHR -- ^ pCreateInfo
                                         ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateDisplayPlaneSurfaceKHR =
     FunPtr HS_vkCreateDisplayPlaneSurfaceKHR

foreign import ccall "dynamic" unwrapVkCreateDisplayPlaneSurfaceKHR
               ::
               PFN_vkCreateDisplayPlaneSurfaceKHR ->
                 HS_vkCreateDisplayPlaneSurfaceKHR

instance VulkanProc "vkCreateDisplayPlaneSurfaceKHR" where
        type VkProcType "vkCreateDisplayPlaneSurfaceKHR" =
             HS_vkCreateDisplayPlaneSurfaceKHR
        vkProcSymbol = _VkCreateDisplayPlaneSurfaceKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateDisplayPlaneSurfaceKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_KHR_DISPLAY_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_DISPLAY_SPEC_VERSION = 21

type VK_KHR_DISPLAY_SPEC_VERSION = 21

pattern VK_KHR_DISPLAY_EXTENSION_NAME :: CString

pattern VK_KHR_DISPLAY_EXTENSION_NAME <-
        (is_VK_KHR_DISPLAY_EXTENSION_NAME -> True)
  where VK_KHR_DISPLAY_EXTENSION_NAME
          = _VK_KHR_DISPLAY_EXTENSION_NAME

{-# INLINE _VK_KHR_DISPLAY_EXTENSION_NAME #-}

_VK_KHR_DISPLAY_EXTENSION_NAME :: CString
_VK_KHR_DISPLAY_EXTENSION_NAME = Ptr "VK_KHR_display\NUL"#

{-# INLINE is_VK_KHR_DISPLAY_EXTENSION_NAME #-}

is_VK_KHR_DISPLAY_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_DISPLAY_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_DISPLAY_EXTENSION_NAME

type VK_KHR_DISPLAY_EXTENSION_NAME = "VK_KHR_display"

pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR =
        VkStructureType 1000002000

pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000002001

-- | VkDisplayKHR
pattern VK_OBJECT_TYPE_DISPLAY_KHR :: VkObjectType

pattern VK_OBJECT_TYPE_DISPLAY_KHR = VkObjectType 1000002000

-- | VkDisplayModeKHR
pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR :: VkObjectType

pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR = VkObjectType 1000002001
