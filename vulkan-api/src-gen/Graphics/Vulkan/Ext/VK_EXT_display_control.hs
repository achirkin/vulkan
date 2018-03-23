{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_display_control
       (-- * Vulkan extension: @VK_EXT_display_control@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @92@
        --
        -- Required extensions: 'VK_EXT_display_surface_counter', 'VK_KHR_swapchain'.
        --

        -- ** Required extensions: 'VK_EXT_display_surface_counter', 'VK_KHR_swapchain'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkColorSpaceKHR,
        module Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR,
        module Graphics.Vulkan.Types.Struct.VkDeviceEventInfoEXT,
        module Graphics.Vulkan.Types.Enum.VkDeviceEventTypeEXT,
        module Graphics.Vulkan.Types.Struct.VkDisplayEventInfoEXT,
        module Graphics.Vulkan.Types.Enum.VkDisplayEventTypeEXT,
        module Graphics.Vulkan.Types.Struct.VkDisplayPowerInfoEXT,
        module Graphics.Vulkan.Types.Enum.VkDisplayPowerStateEXT,
        module Graphics.Vulkan.Types.Struct.VkExtent2D,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Enum.VkPresentModeKHR,
        module Graphics.Vulkan.Types.Enum.VkSharingMode,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Enum.VkSurfaceCounterFlagsEXT,
        module Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR,
        module Graphics.Vulkan.Types.Struct.VkSwapchainCounterCreateInfoEXT,
        module Graphics.Vulkan.Types.Enum.VkSwapchainCreateFlagsKHR,
        module Graphics.Vulkan.Types.Struct.VkSwapchainCreateInfoKHR,
        -- > #include "vk_platform.h"
        vkDisplayPowerControlEXT, vkDisplayPowerControlEXTSafe,
        vkRegisterDeviceEventEXT, vkRegisterDeviceEventEXTSafe,
        vkRegisterDisplayEventEXT, vkRegisterDisplayEventEXTSafe,
        vkGetSwapchainCounterEXT, vkGetSwapchainCounterEXTSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        VK_EXT_DISPLAY_CONTROL_SPEC_VERSION,
        pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION,
        VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME,
        pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT)
       where
import           GHC.Ptr                                                      (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkColorSpaceKHR
import           Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkDeviceEventTypeEXT
import           Graphics.Vulkan.Types.Enum.VkDisplayEventTypeEXT
import           Graphics.Vulkan.Types.Enum.VkDisplayPowerStateEXT
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkInternalAllocationType
import           Graphics.Vulkan.Types.Enum.VkPresentModeKHR
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkSharingMode
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSurfaceCounterFlagsEXT
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkSwapchainCreateFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkSystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
import           Graphics.Vulkan.Types.Struct.VkDeviceEventInfoEXT
import           Graphics.Vulkan.Types.Struct.VkDisplayEventInfoEXT
import           Graphics.Vulkan.Types.Struct.VkDisplayPowerInfoEXT
import           Graphics.Vulkan.Types.Struct.VkExtent2D
import           Graphics.Vulkan.Types.Struct.VkSwapchainCounterCreateInfoEXT
import           Graphics.Vulkan.Types.Struct.VkSwapchainCreateInfoKHR

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkDisplayPowerControlEXT
--   >     ( VkDevice device
--   >     , VkDisplayKHR display
--   >     , const VkDisplayPowerInfoEXT* pDisplayPowerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDisplayPowerControlEXT.html vkDisplayPowerControlEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDisplayPowerControlEXT"
               vkDisplayPowerControlEXT ::
               VkDevice -- ^ device
                        ->
                 VkDisplayKHR -- ^ display
                              -> Ptr VkDisplayPowerInfoEXT -- ^ pDisplayPowerInfo
                                                           -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkDisplayPowerControlEXT
--   >     ( VkDevice device
--   >     , VkDisplayKHR display
--   >     , const VkDisplayPowerInfoEXT* pDisplayPowerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDisplayPowerControlEXT.html vkDisplayPowerControlEXT registry at www.khronos.org>
foreign import ccall safe "vkDisplayPowerControlEXT"
               vkDisplayPowerControlEXTSafe ::
               VkDevice -- ^ device
                        ->
                 VkDisplayKHR -- ^ display
                              -> Ptr VkDisplayPowerInfoEXT -- ^ pDisplayPowerInfo
                                                           -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkRegisterDeviceEventEXT
--   >     ( VkDevice device
--   >     , const VkDeviceEventInfoEXT* pDeviceEventInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFence* pFence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkRegisterDeviceEventEXT.html vkRegisterDeviceEventEXT registry at www.khronos.org>
foreign import ccall unsafe "vkRegisterDeviceEventEXT"
               vkRegisterDeviceEventEXT ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDeviceEventInfoEXT -- ^ pDeviceEventInfo
                                          ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkFence -- ^ pFence
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkRegisterDeviceEventEXT
--   >     ( VkDevice device
--   >     , const VkDeviceEventInfoEXT* pDeviceEventInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFence* pFence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkRegisterDeviceEventEXT.html vkRegisterDeviceEventEXT registry at www.khronos.org>
foreign import ccall safe "vkRegisterDeviceEventEXT"
               vkRegisterDeviceEventEXTSafe ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDeviceEventInfoEXT -- ^ pDeviceEventInfo
                                          ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkFence -- ^ pFence
                                                            -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkRegisterDisplayEventEXT
--   >     ( VkDevice device
--   >     , VkDisplayKHR display
--   >     , const VkDisplayEventInfoEXT* pDisplayEventInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFence* pFence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkRegisterDisplayEventEXT.html vkRegisterDisplayEventEXT registry at www.khronos.org>
foreign import ccall unsafe "vkRegisterDisplayEventEXT"
               vkRegisterDisplayEventEXT ::
               VkDevice -- ^ device
                        ->
                 VkDisplayKHR -- ^ display
                              ->
                   Ptr VkDisplayEventInfoEXT -- ^ pDisplayEventInfo
                                             ->
                     Ptr VkAllocationCallbacks -- ^ pAllocator
                                               -> Ptr VkFence -- ^ pFence
                                                              -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   > VkResult vkRegisterDisplayEventEXT
--   >     ( VkDevice device
--   >     , VkDisplayKHR display
--   >     , const VkDisplayEventInfoEXT* pDisplayEventInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFence* pFence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkRegisterDisplayEventEXT.html vkRegisterDisplayEventEXT registry at www.khronos.org>
foreign import ccall safe "vkRegisterDisplayEventEXT"
               vkRegisterDisplayEventEXTSafe ::
               VkDevice -- ^ device
                        ->
                 VkDisplayKHR -- ^ display
                              ->
                   Ptr VkDisplayEventInfoEXT -- ^ pDisplayEventInfo
                                             ->
                     Ptr VkAllocationCallbacks -- ^ pAllocator
                                               -> Ptr VkFence -- ^ pFence
                                                              -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR'.
--
--   > VkResult vkGetSwapchainCounterEXT
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , VkSurfaceCounterFlagBitsEXT counter
--   >     , uint64_t* pCounterValue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetSwapchainCounterEXT.html vkGetSwapchainCounterEXT registry at www.khronos.org>
foreign import ccall unsafe "vkGetSwapchainCounterEXT"
               vkGetSwapchainCounterEXT ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                ->
                   VkSurfaceCounterFlagBitsEXT -- ^ counter
                                               -> Ptr Word64 -- ^ pCounterValue
                                                             -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR'.
--
--   > VkResult vkGetSwapchainCounterEXT
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     , VkSurfaceCounterFlagBitsEXT counter
--   >     , uint64_t* pCounterValue
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetSwapchainCounterEXT.html vkGetSwapchainCounterEXT registry at www.khronos.org>
foreign import ccall safe "vkGetSwapchainCounterEXT"
               vkGetSwapchainCounterEXTSafe ::
               VkDevice -- ^ device
                        ->
                 VkSwapchainKHR -- ^ swapchain
                                ->
                   VkSurfaceCounterFlagBitsEXT -- ^ counter
                                               -> Ptr Word64 -- ^ pCounterValue
                                                             -> IO VkResult

pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_DISPLAY_CONTROL_SPEC_VERSION = 1

type VK_EXT_DISPLAY_CONTROL_SPEC_VERSION = 1

pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME :: CString

pattern VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME <-
        (is_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME -> True)
  where VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME
          = _VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME

{-# INLINE _VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME #-}

_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME :: CString
_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME
  = Ptr "VK_EXT_display_control\NUL"#

{-# INLINE is_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME #-}

is_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME

type VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME =
     "VK_EXT_display_control"

pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT =
        VkStructureType 1000091000

pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT =
        VkStructureType 1000091001

pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT =
        VkStructureType 1000091002

pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT =
        VkStructureType 1000091003
