{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
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
        vkGetPhysicalDeviceDisplayPropertiesKHR,
        vkGetPhysicalDeviceDisplayPropertiesKHRSafe,
        vkGetPhysicalDeviceDisplayPlanePropertiesKHR,
        vkGetPhysicalDeviceDisplayPlanePropertiesKHRSafe,
        vkGetDisplayPlaneSupportedDisplaysKHR,
        vkGetDisplayPlaneSupportedDisplaysKHRSafe,
        vkGetDisplayModePropertiesKHR, vkGetDisplayModePropertiesKHRSafe,
        vkCreateDisplayModeKHR, vkCreateDisplayModeKHRSafe,
        vkGetDisplayPlaneCapabilitiesKHR,
        vkGetDisplayPlaneCapabilitiesKHRSafe,
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceDisplayPropertiesKHR.html vkGetPhysicalDeviceDisplayPropertiesKHR registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceDisplayPropertiesKHR.html vkGetPhysicalDeviceDisplayPropertiesKHR registry at www.khronos.org>
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
--   > VkResult vkGetPhysicalDeviceDisplayPlanePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayPlanePropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceDisplayPlanePropertiesKHR.html vkGetPhysicalDeviceDisplayPlanePropertiesKHR registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetPhysicalDeviceDisplayPlanePropertiesKHR.html vkGetPhysicalDeviceDisplayPlanePropertiesKHR registry at www.khronos.org>
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
--   > VkResult vkGetDisplayPlaneSupportedDisplaysKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t planeIndex
--   >     , uint32_t* pDisplayCount
--   >     , VkDisplayKHR* pDisplays
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetDisplayPlaneSupportedDisplaysKHR.html vkGetDisplayPlaneSupportedDisplaysKHR registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetDisplayPlaneSupportedDisplaysKHR.html vkGetDisplayPlaneSupportedDisplaysKHR registry at www.khronos.org>
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
--   > VkResult vkGetDisplayModePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayModePropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetDisplayModePropertiesKHR.html vkGetDisplayModePropertiesKHR registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetDisplayModePropertiesKHR.html vkGetDisplayModePropertiesKHR registry at www.khronos.org>
foreign import ccall safe "vkGetDisplayModePropertiesKHR"
               vkGetDisplayModePropertiesKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkDisplayKHR -- ^ display
                              ->
                   Ptr Word32 -- ^ pPropertyCount
                              -> Ptr VkDisplayModePropertiesKHR -- ^ pProperties
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCreateDisplayModeKHR.html vkCreateDisplayModeKHR registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCreateDisplayModeKHR.html vkCreateDisplayModeKHR registry at www.khronos.org>
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
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayPlaneCapabilitiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayModeKHR mode
--   >     , uint32_t planeIndex
--   >     , VkDisplayPlaneCapabilitiesKHR* pCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetDisplayPlaneCapabilitiesKHR.html vkGetDisplayPlaneCapabilitiesKHR registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetDisplayPlaneCapabilitiesKHR.html vkGetDisplayPlaneCapabilitiesKHR registry at www.khronos.org>
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
--   > VkResult vkCreateDisplayPlaneSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkDisplaySurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCreateDisplayPlaneSurfaceKHR.html vkCreateDisplayPlaneSurfaceKHR registry at www.khronos.org>
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkCreateDisplayPlaneSurfaceKHR.html vkCreateDisplayPlaneSurfaceKHR registry at www.khronos.org>
foreign import ccall safe "vkCreateDisplayPlaneSurfaceKHR"
               vkCreateDisplayPlaneSurfaceKHRSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkDisplaySurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

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
