{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
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
        VkDisplayPowerControlEXT, pattern VkDisplayPowerControlEXT,
        HS_vkDisplayPowerControlEXT, PFN_vkDisplayPowerControlEXT,
        unwrapVkDisplayPowerControlEXT, vkDisplayPowerControlEXT,
        vkDisplayPowerControlEXTSafe, VkRegisterDeviceEventEXT,
        pattern VkRegisterDeviceEventEXT, HS_vkRegisterDeviceEventEXT,
        PFN_vkRegisterDeviceEventEXT, unwrapVkRegisterDeviceEventEXT,
        vkRegisterDeviceEventEXT, vkRegisterDeviceEventEXTSafe,
        VkRegisterDisplayEventEXT, pattern VkRegisterDisplayEventEXT,
        HS_vkRegisterDisplayEventEXT, PFN_vkRegisterDisplayEventEXT,
        unwrapVkRegisterDisplayEventEXT, vkRegisterDisplayEventEXT,
        vkRegisterDisplayEventEXTSafe, VkGetSwapchainCounterEXT,
        pattern VkGetSwapchainCounterEXT, HS_vkGetSwapchainCounterEXT,
        PFN_vkGetSwapchainCounterEXT, unwrapVkGetSwapchainCounterEXT,
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
import           Graphics.Vulkan.Marshal.InstanceProc                         (VulkanInstanceProc (..))
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

pattern VkDisplayPowerControlEXT :: CString

pattern VkDisplayPowerControlEXT <-
        (is_VkDisplayPowerControlEXT -> True)
  where VkDisplayPowerControlEXT = _VkDisplayPowerControlEXT

{-# INLINE _VkDisplayPowerControlEXT #-}

_VkDisplayPowerControlEXT :: CString
_VkDisplayPowerControlEXT = Ptr "vkDisplayPowerControlEXT\NUL"#

{-# INLINE is_VkDisplayPowerControlEXT #-}

is_VkDisplayPowerControlEXT :: CString -> Bool
is_VkDisplayPowerControlEXT
  = (EQ ==) . cmpCStrings _VkDisplayPowerControlEXT

type VkDisplayPowerControlEXT = "vkDisplayPowerControlEXT"

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
--   > VkResult vkDisplayPowerControlEXT
--   >     ( VkDevice device
--   >     , VkDisplayKHR display
--   >     , const VkDisplayPowerInfoEXT* pDisplayPowerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkDisplayPowerControlEXT.html vkDisplayPowerControlEXT registry at www.khronos.org>
type HS_vkDisplayPowerControlEXT =
     VkDevice -- ^ device
              ->
       VkDisplayKHR -- ^ display
                    -> Ptr VkDisplayPowerInfoEXT -- ^ pDisplayPowerInfo
                                                 -> IO VkResult

type PFN_vkDisplayPowerControlEXT =
     FunPtr HS_vkDisplayPowerControlEXT

foreign import ccall "dynamic" unwrapVkDisplayPowerControlEXT ::
               PFN_vkDisplayPowerControlEXT -> HS_vkDisplayPowerControlEXT

instance VulkanInstanceProc "vkDisplayPowerControlEXT" where
        type VkInstanceProcType "vkDisplayPowerControlEXT" =
             HS_vkDisplayPowerControlEXT
        vkInstanceProcSymbol = _VkDisplayPowerControlEXT

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkDisplayPowerControlEXT

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkRegisterDeviceEventEXT :: CString

pattern VkRegisterDeviceEventEXT <-
        (is_VkRegisterDeviceEventEXT -> True)
  where VkRegisterDeviceEventEXT = _VkRegisterDeviceEventEXT

{-# INLINE _VkRegisterDeviceEventEXT #-}

_VkRegisterDeviceEventEXT :: CString
_VkRegisterDeviceEventEXT = Ptr "vkRegisterDeviceEventEXT\NUL"#

{-# INLINE is_VkRegisterDeviceEventEXT #-}

is_VkRegisterDeviceEventEXT :: CString -> Bool
is_VkRegisterDeviceEventEXT
  = (EQ ==) . cmpCStrings _VkRegisterDeviceEventEXT

type VkRegisterDeviceEventEXT = "vkRegisterDeviceEventEXT"

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
--   > VkResult vkRegisterDeviceEventEXT
--   >     ( VkDevice device
--   >     , const VkDeviceEventInfoEXT* pDeviceEventInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFence* pFence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkRegisterDeviceEventEXT.html vkRegisterDeviceEventEXT registry at www.khronos.org>
type HS_vkRegisterDeviceEventEXT =
     VkDevice -- ^ device
              ->
       Ptr VkDeviceEventInfoEXT -- ^ pDeviceEventInfo
                                ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkFence -- ^ pFence
                                                  -> IO VkResult

type PFN_vkRegisterDeviceEventEXT =
     FunPtr HS_vkRegisterDeviceEventEXT

foreign import ccall "dynamic" unwrapVkRegisterDeviceEventEXT ::
               PFN_vkRegisterDeviceEventEXT -> HS_vkRegisterDeviceEventEXT

instance VulkanInstanceProc "vkRegisterDeviceEventEXT" where
        type VkInstanceProcType "vkRegisterDeviceEventEXT" =
             HS_vkRegisterDeviceEventEXT
        vkInstanceProcSymbol = _VkRegisterDeviceEventEXT

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkRegisterDeviceEventEXT

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkRegisterDisplayEventEXT :: CString

pattern VkRegisterDisplayEventEXT <-
        (is_VkRegisterDisplayEventEXT -> True)
  where VkRegisterDisplayEventEXT = _VkRegisterDisplayEventEXT

{-# INLINE _VkRegisterDisplayEventEXT #-}

_VkRegisterDisplayEventEXT :: CString
_VkRegisterDisplayEventEXT = Ptr "vkRegisterDisplayEventEXT\NUL"#

{-# INLINE is_VkRegisterDisplayEventEXT #-}

is_VkRegisterDisplayEventEXT :: CString -> Bool
is_VkRegisterDisplayEventEXT
  = (EQ ==) . cmpCStrings _VkRegisterDisplayEventEXT

type VkRegisterDisplayEventEXT = "vkRegisterDisplayEventEXT"

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
--   > VkResult vkRegisterDisplayEventEXT
--   >     ( VkDevice device
--   >     , VkDisplayKHR display
--   >     , const VkDisplayEventInfoEXT* pDisplayEventInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkFence* pFence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkRegisterDisplayEventEXT.html vkRegisterDisplayEventEXT registry at www.khronos.org>
type HS_vkRegisterDisplayEventEXT =
     VkDevice -- ^ device
              ->
       VkDisplayKHR -- ^ display
                    ->
         Ptr VkDisplayEventInfoEXT -- ^ pDisplayEventInfo
                                   ->
           Ptr VkAllocationCallbacks -- ^ pAllocator
                                     -> Ptr VkFence -- ^ pFence
                                                    -> IO VkResult

type PFN_vkRegisterDisplayEventEXT =
     FunPtr HS_vkRegisterDisplayEventEXT

foreign import ccall "dynamic" unwrapVkRegisterDisplayEventEXT ::
               PFN_vkRegisterDisplayEventEXT -> HS_vkRegisterDisplayEventEXT

instance VulkanInstanceProc "vkRegisterDisplayEventEXT" where
        type VkInstanceProcType "vkRegisterDisplayEventEXT" =
             HS_vkRegisterDisplayEventEXT
        vkInstanceProcSymbol = _VkRegisterDisplayEventEXT

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkRegisterDisplayEventEXT

        {-# INLINE unwrapVkInstanceProc #-}

pattern VkGetSwapchainCounterEXT :: CString

pattern VkGetSwapchainCounterEXT <-
        (is_VkGetSwapchainCounterEXT -> True)
  where VkGetSwapchainCounterEXT = _VkGetSwapchainCounterEXT

{-# INLINE _VkGetSwapchainCounterEXT #-}

_VkGetSwapchainCounterEXT :: CString
_VkGetSwapchainCounterEXT = Ptr "vkGetSwapchainCounterEXT\NUL"#

{-# INLINE is_VkGetSwapchainCounterEXT #-}

is_VkGetSwapchainCounterEXT :: CString -> Bool
is_VkGetSwapchainCounterEXT
  = (EQ ==) . cmpCStrings _VkGetSwapchainCounterEXT

type VkGetSwapchainCounterEXT = "vkGetSwapchainCounterEXT"

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
type HS_vkGetSwapchainCounterEXT =
     VkDevice -- ^ device
              ->
       VkSwapchainKHR -- ^ swapchain
                      ->
         VkSurfaceCounterFlagBitsEXT -- ^ counter
                                     -> Ptr Word64 -- ^ pCounterValue
                                                   -> IO VkResult

type PFN_vkGetSwapchainCounterEXT =
     FunPtr HS_vkGetSwapchainCounterEXT

foreign import ccall "dynamic" unwrapVkGetSwapchainCounterEXT ::
               PFN_vkGetSwapchainCounterEXT -> HS_vkGetSwapchainCounterEXT

instance VulkanInstanceProc "vkGetSwapchainCounterEXT" where
        type VkInstanceProcType "vkGetSwapchainCounterEXT" =
             HS_vkGetSwapchainCounterEXT
        vkInstanceProcSymbol = _VkGetSwapchainCounterEXT

        {-# INLINE vkInstanceProcSymbol #-}
        unwrapVkInstanceProc = unwrapVkGetSwapchainCounterEXT

        {-# INLINE unwrapVkInstanceProc #-}

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
