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
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_display_swapchain
       (-- * Vulkan extension: @VK_KHR_display_swapchain@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @4@
        --
        -- Required extensions: 'VK_KHR_swapchain', 'VK_KHR_display'.
        --

        -- ** Required extensions: 'VK_KHR_swapchain', 'VK_KHR_display'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Struct.VkDisplayPresentInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkExtent2D,
        module Graphics.Vulkan.Types.Struct.VkOffset2D,
        module Graphics.Vulkan.Types.Struct.VkPresentInfoKHR,
        module Graphics.Vulkan.Types.Struct.VkRect2D,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        VkCreateSharedSwapchainsKHR, pattern VkCreateSharedSwapchainsKHR,
        HS_vkCreateSharedSwapchainsKHR, PFN_vkCreateSharedSwapchainsKHR,
        vkCreateSharedSwapchainsKHR, vkCreateSharedSwapchainsKHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkColorSpaceKHR,
        module Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkPresentModeKHR,
        module Graphics.Vulkan.Types.Enum.VkSharingMode,
        module Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR,
        module Graphics.Vulkan.Types.Enum.VkSwapchainCreateFlagsKHR,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        module Graphics.Vulkan.Types.Struct.VkSwapchainCreateInfoKHR,
        VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION,
        pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION,
        VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME,
        pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR,
        pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR)
       where
import           GHC.Ptr                                               (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.VkColorSpaceKHR
import           Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkFormat
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags
import           Graphics.Vulkan.Types.Enum.VkInternalAllocationType
import           Graphics.Vulkan.Types.Enum.VkPresentModeKHR
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkSharingMode
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkSwapchainCreateFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkSystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
import           Graphics.Vulkan.Types.Struct.VkDisplayPresentInfoKHR
import           Graphics.Vulkan.Types.Struct.VkExtent2D
import           Graphics.Vulkan.Types.Struct.VkOffset2D
import           Graphics.Vulkan.Types.Struct.VkPresentInfoKHR
import           Graphics.Vulkan.Types.Struct.VkRect2D
import           Graphics.Vulkan.Types.Struct.VkSwapchainCreateInfoKHR
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

pattern VkCreateSharedSwapchainsKHR :: CString

pattern VkCreateSharedSwapchainsKHR <-
        (is_VkCreateSharedSwapchainsKHR -> True)
  where VkCreateSharedSwapchainsKHR = _VkCreateSharedSwapchainsKHR

{-# INLINE _VkCreateSharedSwapchainsKHR #-}

_VkCreateSharedSwapchainsKHR :: CString
_VkCreateSharedSwapchainsKHR
  = Ptr "vkCreateSharedSwapchainsKHR\NUL"#

{-# INLINE is_VkCreateSharedSwapchainsKHR #-}

is_VkCreateSharedSwapchainsKHR :: CString -> Bool
is_VkCreateSharedSwapchainsKHR
  = (EQ ==) . cmpCStrings _VkCreateSharedSwapchainsKHR

type VkCreateSharedSwapchainsKHR = "vkCreateSharedSwapchainsKHR"

#ifdef NATIVE_FFI_VK_VERSION_1_0
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INCOMPATIBLE_DISPLAY_KHR', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkCreateSharedSwapchainsKHR
-- >     ( VkDevice device
-- >     , uint32_t swapchainCount
-- >     , const VkSwapchainCreateInfoKHR* pCreateInfos
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSwapchainKHR* pSwapchains
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSharedSwapchainsKHR vkCreateSharedSwapchainsKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkCreateSharedSwapchainsKHR"
               vkCreateSharedSwapchainsKHR ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ swapchainCount
                        ->
                   Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfos
                                                ->
                     Ptr VkAllocationCallbacks -- ^ pAllocator
                                               -> Ptr VkSwapchainKHR -- ^ pSwapchains
                                                                     -> IO VkResult

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INCOMPATIBLE_DISPLAY_KHR', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkCreateSharedSwapchainsKHR
-- >     ( VkDevice device
-- >     , uint32_t swapchainCount
-- >     , const VkSwapchainCreateInfoKHR* pCreateInfos
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSwapchainKHR* pSwapchains
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSharedSwapchainsKHR vkCreateSharedSwapchainsKHR registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-0@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSharedSwapchainsKHR <- vkGetDeviceProc @VkCreateSharedSwapchainsKHR vkDevice
--
vkCreateSharedSwapchainsKHR ::
                            VkDevice -- ^ device
                                     ->
                              Word32 -- ^ swapchainCount
                                     ->
                                Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfos
                                                             ->
                                  Ptr VkAllocationCallbacks -- ^ pAllocator
                                                            -> Ptr VkSwapchainKHR -- ^ pSwapchains
                                                                                  -> IO VkResult
vkCreateSharedSwapchainsKHR d
  = unsafeDupablePerformIO
      (vkGetDeviceProc @VkCreateSharedSwapchainsKHR d)
      d

{-# INLINE vkCreateSharedSwapchainsKHR #-}

{-# WARNING
vkCreateSharedSwapchainsKHR"This function requires useNativeFFI-1-0 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-0.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_0
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INCOMPATIBLE_DISPLAY_KHR', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkCreateSharedSwapchainsKHR
-- >     ( VkDevice device
-- >     , uint32_t swapchainCount
-- >     , const VkSwapchainCreateInfoKHR* pCreateInfos
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSwapchainKHR* pSwapchains
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSharedSwapchainsKHR vkCreateSharedSwapchainsKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkCreateSharedSwapchainsKHR"
               vkCreateSharedSwapchainsKHRSafe ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ swapchainCount
                        ->
                   Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfos
                                                ->
                     Ptr VkAllocationCallbacks -- ^ pAllocator
                                               -> Ptr VkSwapchainKHR -- ^ pSwapchains
                                                                     -> IO VkResult

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INCOMPATIBLE_DISPLAY_KHR', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkCreateSharedSwapchainsKHR
-- >     ( VkDevice device
-- >     , uint32_t swapchainCount
-- >     , const VkSwapchainCreateInfoKHR* pCreateInfos
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSwapchainKHR* pSwapchains
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSharedSwapchainsKHR vkCreateSharedSwapchainsKHR registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-0@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSharedSwapchainsKHR <- vkGetDeviceProc @VkCreateSharedSwapchainsKHR vkDevice
--
vkCreateSharedSwapchainsKHRSafe ::
                                VkDevice -- ^ device
                                         ->
                                  Word32 -- ^ swapchainCount
                                         ->
                                    Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfos
                                                                 ->
                                      Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                -> Ptr VkSwapchainKHR -- ^ pSwapchains
                                                                                      -> IO VkResult
vkCreateSharedSwapchainsKHRSafe = vkCreateSharedSwapchainsKHR

{-# INLINE vkCreateSharedSwapchainsKHRSafe #-}

{-# WARNING
vkCreateSharedSwapchainsKHRSafe"This function requires useNativeFFI-1-0 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetDeviceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-0.\n"
 #-}
#endif

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INCOMPATIBLE_DISPLAY_KHR', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkCreateSharedSwapchainsKHR
--   >     ( VkDevice device
--   >     , uint32_t swapchainCount
--   >     , const VkSwapchainCreateInfoKHR* pCreateInfos
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSwapchainKHR* pSwapchains
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateSharedSwapchainsKHR vkCreateSharedSwapchainsKHR registry at www.khronos.org>
type HS_vkCreateSharedSwapchainsKHR =
     VkDevice -- ^ device
              ->
       Word32 -- ^ swapchainCount
              ->
         Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfos
                                      ->
           Ptr VkAllocationCallbacks -- ^ pAllocator
                                     -> Ptr VkSwapchainKHR -- ^ pSwapchains
                                                           -> IO VkResult

type PFN_vkCreateSharedSwapchainsKHR =
     FunPtr HS_vkCreateSharedSwapchainsKHR

foreign import ccall "dynamic" unwrapVkCreateSharedSwapchainsKHR ::
               PFN_vkCreateSharedSwapchainsKHR -> HS_vkCreateSharedSwapchainsKHR

instance VulkanProc "vkCreateSharedSwapchainsKHR" where
        type VkProcType "vkCreateSharedSwapchainsKHR" =
             HS_vkCreateSharedSwapchainsKHR
        vkProcSymbol = _VkCreateSharedSwapchainsKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateSharedSwapchainsKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION = 9

type VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION = 9

pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME :: CString

pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME <-
        (is_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME -> True)
  where VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
          = _VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME

{-# INLINE _VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME #-}

_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME :: CString
_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
  = Ptr "VK_KHR_display_swapchain\NUL"#

{-# INLINE is_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME #-}

is_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME

type VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME =
     "VK_KHR_display_swapchain"

pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR =
        VkStructureType 1000003000

pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR :: VkResult

pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR = VkResult (-1000003001)
