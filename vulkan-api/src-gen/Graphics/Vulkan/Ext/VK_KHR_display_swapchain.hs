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
        module Graphics.Vulkan.Types.Struct.Display,
        module Graphics.Vulkan.Types.Struct.Extent,
        module Graphics.Vulkan.Types.Struct.Offset,
        module Graphics.Vulkan.Types.Struct.Present,
        module Graphics.Vulkan.Types.Struct.Rect,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Enum.StructureType,
        -- > #include "vk_platform.h"
        VkCreateSharedSwapchainsKHR, pattern VkCreateSharedSwapchainsKHR,
        HS_vkCreateSharedSwapchainsKHR, PFN_vkCreateSharedSwapchainsKHR,
        vkCreateSharedSwapchainsKHR, vkCreateSharedSwapchainsKHRUnsafe,
        vkCreateSharedSwapchainsKHRSafe, module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.Color,
        module Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR,
        module Graphics.Vulkan.Types.Enum.Format,
        module Graphics.Vulkan.Types.Enum.Image,
        module Graphics.Vulkan.Types.Enum.InternalAllocationType,
        module Graphics.Vulkan.Types.Enum.PresentModeKHR,
        module Graphics.Vulkan.Types.Enum.SharingMode,
        module Graphics.Vulkan.Types.Enum.Surface,
        module Graphics.Vulkan.Types.Enum.SwapchainCreateFlagsKHR,
        module Graphics.Vulkan.Types.Enum.SystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.AllocationCallbacks,
        module Graphics.Vulkan.Types.Struct.SwapchainC,
        VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION,
        pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION,
        VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME,
        pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR,
        pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR)
       where
import           GHC.Ptr                                            (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.Color
import           Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR
import           Graphics.Vulkan.Types.Enum.Format
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.InternalAllocationType
import           Graphics.Vulkan.Types.Enum.PresentModeKHR
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.SharingMode
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.Surface
import           Graphics.Vulkan.Types.Enum.SwapchainCreateFlagsKHR
import           Graphics.Vulkan.Types.Enum.SystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.AllocationCallbacks
import           Graphics.Vulkan.Types.Struct.Display
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.Offset
import           Graphics.Vulkan.Types.Struct.Present
import           Graphics.Vulkan.Types.Struct.Rect
import           Graphics.Vulkan.Types.Struct.SwapchainC
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

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
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSharedSwapchainsKHR <- vkGetDeviceProc @VkCreateSharedSwapchainsKHR vkDevice
--
-- or less efficient:
--
-- > myCreateSharedSwapchainsKHR <- vkGetProc @VkCreateSharedSwapchainsKHR
--
-- __Note:__ @vkCreateSharedSwapchainsKHRUnsafe@ and @vkCreateSharedSwapchainsKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateSharedSwapchainsKHR@ is an alias
--           of @vkCreateSharedSwapchainsKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateSharedSwapchainsKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateSharedSwapchainsKHR"
               vkCreateSharedSwapchainsKHRUnsafe ::
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
vkCreateSharedSwapchainsKHRUnsafe ::
                                  VkDevice -- ^ device
                                           ->
                                    Word32 -- ^ swapchainCount
                                           ->
                                      Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfos
                                                                   ->
                                        Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                  ->
                                          Ptr VkSwapchainKHR -- ^ pSwapchains
                                                             -> IO VkResult
vkCreateSharedSwapchainsKHRUnsafe
  = unsafeDupablePerformIO
      (vkGetProcUnsafe @VkCreateSharedSwapchainsKHR)

{-# NOINLINE vkCreateSharedSwapchainsKHRUnsafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSharedSwapchainsKHR <- vkGetDeviceProc @VkCreateSharedSwapchainsKHR vkDevice
--
-- or less efficient:
--
-- > myCreateSharedSwapchainsKHR <- vkGetProc @VkCreateSharedSwapchainsKHR
--
-- __Note:__ @vkCreateSharedSwapchainsKHRUnsafe@ and @vkCreateSharedSwapchainsKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateSharedSwapchainsKHR@ is an alias
--           of @vkCreateSharedSwapchainsKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateSharedSwapchainsKHRSafe@.
--
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
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
vkCreateSharedSwapchainsKHRSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkCreateSharedSwapchainsKHR)

{-# NOINLINE vkCreateSharedSwapchainsKHRSafe #-}
#endif

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
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateSharedSwapchainsKHR <- vkGetDeviceProc @VkCreateSharedSwapchainsKHR vkDevice
--
-- or less efficient:
--
-- > myCreateSharedSwapchainsKHR <- vkGetProc @VkCreateSharedSwapchainsKHR
--
-- __Note:__ @vkCreateSharedSwapchainsKHRUnsafe@ and @vkCreateSharedSwapchainsKHRSafe@ are the @unsafe@ and @safe@
--           FFI imports of this function, respectively. @vkCreateSharedSwapchainsKHR@ is an alias
--           of @vkCreateSharedSwapchainsKHRUnsafe@ when the @useUnsafeFFIDefault@ cabal flag
--           is enabled; otherwise, it is an alias of @vkCreateSharedSwapchainsKHRSafe@.
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
#ifdef UNSAFE_FFI_DEFAULT
vkCreateSharedSwapchainsKHR = vkCreateSharedSwapchainsKHRUnsafe
#else
vkCreateSharedSwapchainsKHR = vkCreateSharedSwapchainsKHRSafe

#endif
{-# INLINE vkCreateSharedSwapchainsKHR #-}

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

foreign import ccall unsafe "dynamic"
               unwrapVkCreateSharedSwapchainsKHRUnsafe ::
               PFN_vkCreateSharedSwapchainsKHR -> HS_vkCreateSharedSwapchainsKHR

foreign import ccall safe "dynamic"
               unwrapVkCreateSharedSwapchainsKHRSafe ::
               PFN_vkCreateSharedSwapchainsKHR -> HS_vkCreateSharedSwapchainsKHR

instance VulkanProc "vkCreateSharedSwapchainsKHR" where
        type VkProcType "vkCreateSharedSwapchainsKHR" =
             HS_vkCreateSharedSwapchainsKHR
        vkProcSymbol = _VkCreateSharedSwapchainsKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkCreateSharedSwapchainsKHRUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkCreateSharedSwapchainsKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

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
