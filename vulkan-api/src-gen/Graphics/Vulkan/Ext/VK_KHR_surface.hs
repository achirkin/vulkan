{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures#-}
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
module Graphics.Vulkan.Ext.VK_KHR_surface
       (-- * Vulkan extension: @VK_KHR_surface@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo,Ian Elliott ianelliott@google.com@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @1@
        VkDestroySurfaceKHR, pattern VkDestroySurfaceKHR,
        HS_vkDestroySurfaceKHR, PFN_vkDestroySurfaceKHR,
        vkDestroySurfaceKHR, vkDestroySurfaceKHRSafe,
        VkGetPhysicalDeviceSurfaceSupportKHR,
        pattern VkGetPhysicalDeviceSurfaceSupportKHR,
        HS_vkGetPhysicalDeviceSurfaceSupportKHR,
        PFN_vkGetPhysicalDeviceSurfaceSupportKHR,
        vkGetPhysicalDeviceSurfaceSupportKHR,
        vkGetPhysicalDeviceSurfaceSupportKHRSafe,
        VkGetPhysicalDeviceSurfaceCapabilitiesKHR,
        pattern VkGetPhysicalDeviceSurfaceCapabilitiesKHR,
        HS_vkGetPhysicalDeviceSurfaceCapabilitiesKHR,
        PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR,
        vkGetPhysicalDeviceSurfaceCapabilitiesKHR,
        vkGetPhysicalDeviceSurfaceCapabilitiesKHRSafe,
        VkGetPhysicalDeviceSurfaceFormatsKHR,
        pattern VkGetPhysicalDeviceSurfaceFormatsKHR,
        HS_vkGetPhysicalDeviceSurfaceFormatsKHR,
        PFN_vkGetPhysicalDeviceSurfaceFormatsKHR,
        vkGetPhysicalDeviceSurfaceFormatsKHR,
        vkGetPhysicalDeviceSurfaceFormatsKHRSafe,
        VkGetPhysicalDeviceSurfacePresentModesKHR,
        pattern VkGetPhysicalDeviceSurfacePresentModesKHR,
        HS_vkGetPhysicalDeviceSurfacePresentModesKHR,
        PFN_vkGetPhysicalDeviceSurfacePresentModesKHR,
        vkGetPhysicalDeviceSurfacePresentModesKHR,
        vkGetPhysicalDeviceSurfacePresentModesKHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.Color,
        module Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR,
        module Graphics.Vulkan.Types.Enum.Format,
        module Graphics.Vulkan.Types.Enum.Image,
        module Graphics.Vulkan.Types.Enum.InternalAllocationType,
        module Graphics.Vulkan.Types.Enum.PresentModeKHR,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Enum.Surface,
        module Graphics.Vulkan.Types.Enum.SystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.AllocationCallbacks,
        module Graphics.Vulkan.Types.Struct.Extent,
        module Graphics.Vulkan.Types.Struct.Surface,
        VK_KHR_SURFACE_SPEC_VERSION, pattern VK_KHR_SURFACE_SPEC_VERSION,
        VK_KHR_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_SURFACE_EXTENSION_NAME,
        pattern VK_ERROR_SURFACE_LOST_KHR,
        pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR,
        pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR,
        pattern VK_OBJECT_TYPE_SURFACE_KHR)
       where
import           GHC.Ptr                                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Enum.Color
import           Graphics.Vulkan.Types.Enum.CompositeAlphaFlagsKHR
import           Graphics.Vulkan.Types.Enum.Format
import           Graphics.Vulkan.Types.Enum.Image
import           Graphics.Vulkan.Types.Enum.InternalAllocationType
import           Graphics.Vulkan.Types.Enum.Object                 (VkObjectType (..))
import           Graphics.Vulkan.Types.Enum.PresentModeKHR
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.Surface
import           Graphics.Vulkan.Types.Enum.SystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.AllocationCallbacks
import           Graphics.Vulkan.Types.Struct.Extent
import           Graphics.Vulkan.Types.Struct.Surface
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

pattern VkDestroySurfaceKHR :: CString

pattern VkDestroySurfaceKHR <- (is_VkDestroySurfaceKHR -> True)
  where VkDestroySurfaceKHR = _VkDestroySurfaceKHR

{-# INLINE _VkDestroySurfaceKHR #-}

_VkDestroySurfaceKHR :: CString
_VkDestroySurfaceKHR = Ptr "vkDestroySurfaceKHR\NUL"#

{-# INLINE is_VkDestroySurfaceKHR #-}

is_VkDestroySurfaceKHR :: CString -> Bool
is_VkDestroySurfaceKHR = (EQ ==) . cmpCStrings _VkDestroySurfaceKHR

type VkDestroySurfaceKHR = "vkDestroySurfaceKHR"

-- |
-- > void vkDestroySurfaceKHR
-- >     ( VkInstance instance
-- >     , VkSurfaceKHR surface
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySurfaceKHR vkDestroySurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroySurfaceKHR <- vkGetInstanceProc @VkDestroySurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myDestroySurfaceKHR <- vkGetProc @VkDestroySurfaceKHR
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroySurfaceKHR"
               vkDestroySurfaceKHR ::
               VkInstance -- ^ instance
                          -> VkSurfaceKHR -- ^ surface
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

#else
vkDestroySurfaceKHR ::
                    VkInstance -- ^ instance
                               -> VkSurfaceKHR -- ^ surface
                                               -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                            -> IO ()
vkDestroySurfaceKHR
  = unsafeDupablePerformIO (vkGetProc @VkDestroySurfaceKHR)

{-# NOINLINE vkDestroySurfaceKHR #-}
#endif

-- |
-- > void vkDestroySurfaceKHR
-- >     ( VkInstance instance
-- >     , VkSurfaceKHR surface
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySurfaceKHR vkDestroySurfaceKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myDestroySurfaceKHR <- vkGetInstanceProc @VkDestroySurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myDestroySurfaceKHR <- vkGetProc @VkDestroySurfaceKHR
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroySurfaceKHR"
               vkDestroySurfaceKHRSafe ::
               VkInstance -- ^ instance
                          -> VkSurfaceKHR -- ^ surface
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

#else
vkDestroySurfaceKHRSafe ::
                        VkInstance -- ^ instance
                                   -> VkSurfaceKHR -- ^ surface
                                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                -> IO ()
vkDestroySurfaceKHRSafe
  = unsafeDupablePerformIO (vkGetProcSafe @VkDestroySurfaceKHR)

{-# NOINLINE vkDestroySurfaceKHRSafe #-}
#endif

-- | > void vkDestroySurfaceKHR
--   >     ( VkInstance instance
--   >     , VkSurfaceKHR surface
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySurfaceKHR vkDestroySurfaceKHR registry at www.khronos.org>
type HS_vkDestroySurfaceKHR =
     VkInstance -- ^ instance
                -> VkSurfaceKHR -- ^ surface
                                -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                             -> IO ()

type PFN_vkDestroySurfaceKHR = FunPtr HS_vkDestroySurfaceKHR

foreign import ccall unsafe "dynamic" unwrapVkDestroySurfaceKHR ::
               PFN_vkDestroySurfaceKHR -> HS_vkDestroySurfaceKHR

foreign import ccall safe "dynamic" unwrapVkDestroySurfaceKHRSafe
               :: PFN_vkDestroySurfaceKHR -> HS_vkDestroySurfaceKHR

instance VulkanProc "vkDestroySurfaceKHR" where
        type VkProcType "vkDestroySurfaceKHR" = HS_vkDestroySurfaceKHR
        vkProcSymbol = _VkDestroySurfaceKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroySurfaceKHR

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe = unwrapVkDestroySurfaceKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceSurfaceSupportKHR :: CString

pattern VkGetPhysicalDeviceSurfaceSupportKHR <-
        (is_VkGetPhysicalDeviceSurfaceSupportKHR -> True)
  where VkGetPhysicalDeviceSurfaceSupportKHR
          = _VkGetPhysicalDeviceSurfaceSupportKHR

{-# INLINE _VkGetPhysicalDeviceSurfaceSupportKHR #-}

_VkGetPhysicalDeviceSurfaceSupportKHR :: CString
_VkGetPhysicalDeviceSurfaceSupportKHR
  = Ptr "vkGetPhysicalDeviceSurfaceSupportKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceSurfaceSupportKHR #-}

is_VkGetPhysicalDeviceSurfaceSupportKHR :: CString -> Bool
is_VkGetPhysicalDeviceSurfaceSupportKHR
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceSurfaceSupportKHR

type VkGetPhysicalDeviceSurfaceSupportKHR =
     "vkGetPhysicalDeviceSurfaceSupportKHR"

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkGetPhysicalDeviceSurfaceSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , VkSurfaceKHR surface
-- >     , VkBool32* pSupported
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfaceSupportKHR vkGetPhysicalDeviceSurfaceSupportKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceSurfaceSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceSurfaceSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceSurfaceSupportKHR <- vkGetProc @VkGetPhysicalDeviceSurfaceSupportKHR
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetPhysicalDeviceSurfaceSupportKHR"
               vkGetPhysicalDeviceSurfaceSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ queueFamilyIndex
                        -> VkSurfaceKHR -- ^ surface
                                        -> Ptr VkBool32 -- ^ pSupported
                                                        -> IO VkResult

#else
vkGetPhysicalDeviceSurfaceSupportKHR ::
                                     VkPhysicalDevice -- ^ physicalDevice
                                                      ->
                                       Word32 -- ^ queueFamilyIndex
                                              -> VkSurfaceKHR -- ^ surface
                                                              -> Ptr VkBool32 -- ^ pSupported
                                                                              -> IO VkResult
vkGetPhysicalDeviceSurfaceSupportKHR
  = unsafeDupablePerformIO
      (vkGetProc @VkGetPhysicalDeviceSurfaceSupportKHR)

{-# NOINLINE vkGetPhysicalDeviceSurfaceSupportKHR #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkGetPhysicalDeviceSurfaceSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , VkSurfaceKHR surface
-- >     , VkBool32* pSupported
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfaceSupportKHR vkGetPhysicalDeviceSurfaceSupportKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceSurfaceSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceSurfaceSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceSurfaceSupportKHR <- vkGetProc @VkGetPhysicalDeviceSurfaceSupportKHR
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetPhysicalDeviceSurfaceSupportKHR"
               vkGetPhysicalDeviceSurfaceSupportKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ queueFamilyIndex
                        -> VkSurfaceKHR -- ^ surface
                                        -> Ptr VkBool32 -- ^ pSupported
                                                        -> IO VkResult

#else
vkGetPhysicalDeviceSurfaceSupportKHRSafe ::
                                         VkPhysicalDevice -- ^ physicalDevice
                                                          ->
                                           Word32 -- ^ queueFamilyIndex
                                                  -> VkSurfaceKHR -- ^ surface
                                                                  -> Ptr VkBool32 -- ^ pSupported
                                                                                  -> IO VkResult
vkGetPhysicalDeviceSurfaceSupportKHRSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceSurfaceSupportKHR)

{-# NOINLINE vkGetPhysicalDeviceSurfaceSupportKHRSafe #-}
#endif

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , VkSurfaceKHR surface
--   >     , VkBool32* pSupported
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfaceSupportKHR vkGetPhysicalDeviceSurfaceSupportKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceSurfaceSupportKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Word32 -- ^ queueFamilyIndex
              -> VkSurfaceKHR -- ^ surface
                              -> Ptr VkBool32 -- ^ pSupported
                                              -> IO VkResult

type PFN_vkGetPhysicalDeviceSurfaceSupportKHR =
     FunPtr HS_vkGetPhysicalDeviceSurfaceSupportKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceSupportKHR ::
               PFN_vkGetPhysicalDeviceSurfaceSupportKHR ->
                 HS_vkGetPhysicalDeviceSurfaceSupportKHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceSupportKHRSafe ::
               PFN_vkGetPhysicalDeviceSurfaceSupportKHR ->
                 HS_vkGetPhysicalDeviceSurfaceSupportKHR

instance VulkanProc "vkGetPhysicalDeviceSurfaceSupportKHR" where
        type VkProcType "vkGetPhysicalDeviceSurfaceSupportKHR" =
             HS_vkGetPhysicalDeviceSurfaceSupportKHR
        vkProcSymbol = _VkGetPhysicalDeviceSurfaceSupportKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceSurfaceSupportKHR

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceSurfaceSupportKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceSurfaceCapabilitiesKHR :: CString

pattern VkGetPhysicalDeviceSurfaceCapabilitiesKHR <-
        (is_VkGetPhysicalDeviceSurfaceCapabilitiesKHR -> True)
  where VkGetPhysicalDeviceSurfaceCapabilitiesKHR
          = _VkGetPhysicalDeviceSurfaceCapabilitiesKHR

{-# INLINE _VkGetPhysicalDeviceSurfaceCapabilitiesKHR #-}

_VkGetPhysicalDeviceSurfaceCapabilitiesKHR :: CString
_VkGetPhysicalDeviceSurfaceCapabilitiesKHR
  = Ptr "vkGetPhysicalDeviceSurfaceCapabilitiesKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceSurfaceCapabilitiesKHR #-}

is_VkGetPhysicalDeviceSurfaceCapabilitiesKHR :: CString -> Bool
is_VkGetPhysicalDeviceSurfaceCapabilitiesKHR
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceSurfaceCapabilitiesKHR

type VkGetPhysicalDeviceSurfaceCapabilitiesKHR =
     "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkGetPhysicalDeviceSurfaceCapabilitiesKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkSurfaceKHR surface
-- >     , VkSurfaceCapabilitiesKHR* pSurfaceCapabilities
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfaceCapabilitiesKHR vkGetPhysicalDeviceSurfaceCapabilitiesKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceSurfaceCapabilitiesKHR <- vkGetInstanceProc @VkGetPhysicalDeviceSurfaceCapabilitiesKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceSurfaceCapabilitiesKHR <- vkGetProc @VkGetPhysicalDeviceSurfaceCapabilitiesKHR
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe
               "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"
               vkGetPhysicalDeviceSurfaceCapabilitiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr VkSurfaceCapabilitiesKHR -- ^ pSurfaceCapabilities
                                                              -> IO VkResult

#else
vkGetPhysicalDeviceSurfaceCapabilitiesKHR ::
                                          VkPhysicalDevice -- ^ physicalDevice
                                                           ->
                                            VkSurfaceKHR -- ^ surface
                                                         ->
                                              Ptr VkSurfaceCapabilitiesKHR -- ^ pSurfaceCapabilities
                                                                           -> IO VkResult
vkGetPhysicalDeviceSurfaceCapabilitiesKHR
  = unsafeDupablePerformIO
      (vkGetProc @VkGetPhysicalDeviceSurfaceCapabilitiesKHR)

{-# NOINLINE vkGetPhysicalDeviceSurfaceCapabilitiesKHR #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkGetPhysicalDeviceSurfaceCapabilitiesKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkSurfaceKHR surface
-- >     , VkSurfaceCapabilitiesKHR* pSurfaceCapabilities
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfaceCapabilitiesKHR vkGetPhysicalDeviceSurfaceCapabilitiesKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceSurfaceCapabilitiesKHR <- vkGetInstanceProc @VkGetPhysicalDeviceSurfaceCapabilitiesKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceSurfaceCapabilitiesKHR <- vkGetProc @VkGetPhysicalDeviceSurfaceCapabilitiesKHR
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe
               "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"
               vkGetPhysicalDeviceSurfaceCapabilitiesKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr VkSurfaceCapabilitiesKHR -- ^ pSurfaceCapabilities
                                                              -> IO VkResult

#else
vkGetPhysicalDeviceSurfaceCapabilitiesKHRSafe ::
                                              VkPhysicalDevice -- ^ physicalDevice
                                                               ->
                                                VkSurfaceKHR -- ^ surface
                                                             ->
                                                  Ptr VkSurfaceCapabilitiesKHR -- ^ pSurfaceCapabilities
                                                                               -> IO VkResult
vkGetPhysicalDeviceSurfaceCapabilitiesKHRSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceSurfaceCapabilitiesKHR)

{-# NOINLINE vkGetPhysicalDeviceSurfaceCapabilitiesKHRSafe #-}
#endif

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceCapabilitiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSurfaceKHR surface
--   >     , VkSurfaceCapabilitiesKHR* pSurfaceCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfaceCapabilitiesKHR vkGetPhysicalDeviceSurfaceCapabilitiesKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceSurfaceCapabilitiesKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       VkSurfaceKHR -- ^ surface
                    -> Ptr VkSurfaceCapabilitiesKHR -- ^ pSurfaceCapabilities
                                                    -> IO VkResult

type PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR =
     FunPtr HS_vkGetPhysicalDeviceSurfaceCapabilitiesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceCapabilitiesKHR ::
               PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR ->
                 HS_vkGetPhysicalDeviceSurfaceCapabilitiesKHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceCapabilitiesKHRSafe ::
               PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR ->
                 HS_vkGetPhysicalDeviceSurfaceCapabilitiesKHR

instance VulkanProc "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"
         where
        type VkProcType "vkGetPhysicalDeviceSurfaceCapabilitiesKHR" =
             HS_vkGetPhysicalDeviceSurfaceCapabilitiesKHR
        vkProcSymbol = _VkGetPhysicalDeviceSurfaceCapabilitiesKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceSurfaceCapabilitiesKHR

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceSurfaceCapabilitiesKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceSurfaceFormatsKHR :: CString

pattern VkGetPhysicalDeviceSurfaceFormatsKHR <-
        (is_VkGetPhysicalDeviceSurfaceFormatsKHR -> True)
  where VkGetPhysicalDeviceSurfaceFormatsKHR
          = _VkGetPhysicalDeviceSurfaceFormatsKHR

{-# INLINE _VkGetPhysicalDeviceSurfaceFormatsKHR #-}

_VkGetPhysicalDeviceSurfaceFormatsKHR :: CString
_VkGetPhysicalDeviceSurfaceFormatsKHR
  = Ptr "vkGetPhysicalDeviceSurfaceFormatsKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceSurfaceFormatsKHR #-}

is_VkGetPhysicalDeviceSurfaceFormatsKHR :: CString -> Bool
is_VkGetPhysicalDeviceSurfaceFormatsKHR
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceSurfaceFormatsKHR

type VkGetPhysicalDeviceSurfaceFormatsKHR =
     "vkGetPhysicalDeviceSurfaceFormatsKHR"

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkGetPhysicalDeviceSurfaceFormatsKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkSurfaceKHR surface
-- >     , uint32_t* pSurfaceFormatCount
-- >     , VkSurfaceFormatKHR* pSurfaceFormats
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfaceFormatsKHR vkGetPhysicalDeviceSurfaceFormatsKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceSurfaceFormatsKHR <- vkGetInstanceProc @VkGetPhysicalDeviceSurfaceFormatsKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceSurfaceFormatsKHR <- vkGetProc @VkGetPhysicalDeviceSurfaceFormatsKHR
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkGetPhysicalDeviceSurfaceFormatsKHR"
               vkGetPhysicalDeviceSurfaceFormatsKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr Word32 -- ^ pSurfaceFormatCount
                                            -> Ptr VkSurfaceFormatKHR -- ^ pSurfaceFormats
                                                                      -> IO VkResult

#else
vkGetPhysicalDeviceSurfaceFormatsKHR ::
                                     VkPhysicalDevice -- ^ physicalDevice
                                                      ->
                                       VkSurfaceKHR -- ^ surface
                                                    ->
                                         Ptr Word32 -- ^ pSurfaceFormatCount
                                                    -> Ptr VkSurfaceFormatKHR -- ^ pSurfaceFormats
                                                                              -> IO VkResult
vkGetPhysicalDeviceSurfaceFormatsKHR
  = unsafeDupablePerformIO
      (vkGetProc @VkGetPhysicalDeviceSurfaceFormatsKHR)

{-# NOINLINE vkGetPhysicalDeviceSurfaceFormatsKHR #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkGetPhysicalDeviceSurfaceFormatsKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkSurfaceKHR surface
-- >     , uint32_t* pSurfaceFormatCount
-- >     , VkSurfaceFormatKHR* pSurfaceFormats
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfaceFormatsKHR vkGetPhysicalDeviceSurfaceFormatsKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceSurfaceFormatsKHR <- vkGetInstanceProc @VkGetPhysicalDeviceSurfaceFormatsKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceSurfaceFormatsKHR <- vkGetProc @VkGetPhysicalDeviceSurfaceFormatsKHR
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkGetPhysicalDeviceSurfaceFormatsKHR"
               vkGetPhysicalDeviceSurfaceFormatsKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr Word32 -- ^ pSurfaceFormatCount
                                            -> Ptr VkSurfaceFormatKHR -- ^ pSurfaceFormats
                                                                      -> IO VkResult

#else
vkGetPhysicalDeviceSurfaceFormatsKHRSafe ::
                                         VkPhysicalDevice -- ^ physicalDevice
                                                          ->
                                           VkSurfaceKHR -- ^ surface
                                                        ->
                                             Ptr Word32 -- ^ pSurfaceFormatCount
                                                        -> Ptr VkSurfaceFormatKHR -- ^ pSurfaceFormats
                                                                                  -> IO VkResult
vkGetPhysicalDeviceSurfaceFormatsKHRSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceSurfaceFormatsKHR)

{-# NOINLINE vkGetPhysicalDeviceSurfaceFormatsKHRSafe #-}
#endif

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceFormatsKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSurfaceKHR surface
--   >     , uint32_t* pSurfaceFormatCount
--   >     , VkSurfaceFormatKHR* pSurfaceFormats
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfaceFormatsKHR vkGetPhysicalDeviceSurfaceFormatsKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceSurfaceFormatsKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       VkSurfaceKHR -- ^ surface
                    -> Ptr Word32 -- ^ pSurfaceFormatCount
                                  -> Ptr VkSurfaceFormatKHR -- ^ pSurfaceFormats
                                                            -> IO VkResult

type PFN_vkGetPhysicalDeviceSurfaceFormatsKHR =
     FunPtr HS_vkGetPhysicalDeviceSurfaceFormatsKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceFormatsKHR ::
               PFN_vkGetPhysicalDeviceSurfaceFormatsKHR ->
                 HS_vkGetPhysicalDeviceSurfaceFormatsKHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceFormatsKHRSafe ::
               PFN_vkGetPhysicalDeviceSurfaceFormatsKHR ->
                 HS_vkGetPhysicalDeviceSurfaceFormatsKHR

instance VulkanProc "vkGetPhysicalDeviceSurfaceFormatsKHR" where
        type VkProcType "vkGetPhysicalDeviceSurfaceFormatsKHR" =
             HS_vkGetPhysicalDeviceSurfaceFormatsKHR
        vkProcSymbol = _VkGetPhysicalDeviceSurfaceFormatsKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceSurfaceFormatsKHR

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceSurfaceFormatsKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkGetPhysicalDeviceSurfacePresentModesKHR :: CString

pattern VkGetPhysicalDeviceSurfacePresentModesKHR <-
        (is_VkGetPhysicalDeviceSurfacePresentModesKHR -> True)
  where VkGetPhysicalDeviceSurfacePresentModesKHR
          = _VkGetPhysicalDeviceSurfacePresentModesKHR

{-# INLINE _VkGetPhysicalDeviceSurfacePresentModesKHR #-}

_VkGetPhysicalDeviceSurfacePresentModesKHR :: CString
_VkGetPhysicalDeviceSurfacePresentModesKHR
  = Ptr "vkGetPhysicalDeviceSurfacePresentModesKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceSurfacePresentModesKHR #-}

is_VkGetPhysicalDeviceSurfacePresentModesKHR :: CString -> Bool
is_VkGetPhysicalDeviceSurfacePresentModesKHR
  = (EQ ==) . cmpCStrings _VkGetPhysicalDeviceSurfacePresentModesKHR

type VkGetPhysicalDeviceSurfacePresentModesKHR =
     "vkGetPhysicalDeviceSurfacePresentModesKHR"

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkGetPhysicalDeviceSurfacePresentModesKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkSurfaceKHR surface
-- >     , uint32_t* pPresentModeCount
-- >     , VkPresentModeKHR* pPresentModes
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfacePresentModesKHR vkGetPhysicalDeviceSurfacePresentModesKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceSurfacePresentModesKHR <- vkGetInstanceProc @VkGetPhysicalDeviceSurfacePresentModesKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceSurfacePresentModesKHR <- vkGetProc @VkGetPhysicalDeviceSurfacePresentModesKHR
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe
               "vkGetPhysicalDeviceSurfacePresentModesKHR"
               vkGetPhysicalDeviceSurfacePresentModesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr Word32 -- ^ pPresentModeCount
                                            -> Ptr VkPresentModeKHR -- ^ pPresentModes
                                                                    -> IO VkResult

#else
vkGetPhysicalDeviceSurfacePresentModesKHR ::
                                          VkPhysicalDevice -- ^ physicalDevice
                                                           ->
                                            VkSurfaceKHR -- ^ surface
                                                         ->
                                              Ptr Word32 -- ^ pPresentModeCount
                                                         -> Ptr VkPresentModeKHR -- ^ pPresentModes
                                                                                 -> IO VkResult
vkGetPhysicalDeviceSurfacePresentModesKHR
  = unsafeDupablePerformIO
      (vkGetProc @VkGetPhysicalDeviceSurfacePresentModesKHR)

{-# NOINLINE vkGetPhysicalDeviceSurfacePresentModesKHR #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
-- > VkResult vkGetPhysicalDeviceSurfacePresentModesKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , VkSurfaceKHR surface
-- >     , uint32_t* pPresentModeCount
-- >     , VkPresentModeKHR* pPresentModes
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfacePresentModesKHR vkGetPhysicalDeviceSurfacePresentModesKHR registry at www.khronos.org>
--
-- __Note:__ When @useNativeFFI-1-0@ cabal flag is enabled, this function is linked statically
--           as a @foreign import@ call to C Vulkan loader.
--           Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceSurfacePresentModesKHR <- vkGetInstanceProc @VkGetPhysicalDeviceSurfacePresentModesKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceSurfacePresentModesKHR <- vkGetProc @VkGetPhysicalDeviceSurfacePresentModesKHR
--
-- __Note:__ @vkXxx@ and @vkXxxSafe@ versions of the call refer to
--           using @unsafe@ of @safe@ FFI respectively.
--
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe
               "vkGetPhysicalDeviceSurfacePresentModesKHR"
               vkGetPhysicalDeviceSurfacePresentModesKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkSurfaceKHR -- ^ surface
                              -> Ptr Word32 -- ^ pPresentModeCount
                                            -> Ptr VkPresentModeKHR -- ^ pPresentModes
                                                                    -> IO VkResult

#else
vkGetPhysicalDeviceSurfacePresentModesKHRSafe ::
                                              VkPhysicalDevice -- ^ physicalDevice
                                                               ->
                                                VkSurfaceKHR -- ^ surface
                                                             ->
                                                  Ptr Word32 -- ^ pPresentModeCount
                                                             -> Ptr VkPresentModeKHR -- ^ pPresentModes
                                                                                     -> IO VkResult
vkGetPhysicalDeviceSurfacePresentModesKHRSafe
  = unsafeDupablePerformIO
      (vkGetProcSafe @VkGetPhysicalDeviceSurfacePresentModesKHR)

{-# NOINLINE vkGetPhysicalDeviceSurfacePresentModesKHRSafe #-}
#endif

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfacePresentModesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkSurfaceKHR surface
--   >     , uint32_t* pPresentModeCount
--   >     , VkPresentModeKHR* pPresentModes
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceSurfacePresentModesKHR vkGetPhysicalDeviceSurfacePresentModesKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceSurfacePresentModesKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       VkSurfaceKHR -- ^ surface
                    -> Ptr Word32 -- ^ pPresentModeCount
                                  -> Ptr VkPresentModeKHR -- ^ pPresentModes
                                                          -> IO VkResult

type PFN_vkGetPhysicalDeviceSurfacePresentModesKHR =
     FunPtr HS_vkGetPhysicalDeviceSurfacePresentModesKHR

foreign import ccall unsafe "dynamic"
               unwrapVkGetPhysicalDeviceSurfacePresentModesKHR ::
               PFN_vkGetPhysicalDeviceSurfacePresentModesKHR ->
                 HS_vkGetPhysicalDeviceSurfacePresentModesKHR

foreign import ccall safe "dynamic"
               unwrapVkGetPhysicalDeviceSurfacePresentModesKHRSafe ::
               PFN_vkGetPhysicalDeviceSurfacePresentModesKHR ->
                 HS_vkGetPhysicalDeviceSurfacePresentModesKHR

instance VulkanProc "vkGetPhysicalDeviceSurfacePresentModesKHR"
         where
        type VkProcType "vkGetPhysicalDeviceSurfacePresentModesKHR" =
             HS_vkGetPhysicalDeviceSurfacePresentModesKHR
        vkProcSymbol = _VkGetPhysicalDeviceSurfacePresentModesKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceSurfacePresentModesKHR

        {-# INLINE unwrapVkProcPtr #-}
        unwrapVkProcPtrSafe
          = unwrapVkGetPhysicalDeviceSurfacePresentModesKHRSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_KHR_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_SURFACE_SPEC_VERSION = 25

type VK_KHR_SURFACE_SPEC_VERSION = 25

pattern VK_KHR_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_SURFACE_EXTENSION_NAME -> True)
  where VK_KHR_SURFACE_EXTENSION_NAME
          = _VK_KHR_SURFACE_EXTENSION_NAME

{-# INLINE _VK_KHR_SURFACE_EXTENSION_NAME #-}

_VK_KHR_SURFACE_EXTENSION_NAME :: CString
_VK_KHR_SURFACE_EXTENSION_NAME = Ptr "VK_KHR_surface\NUL"#

{-# INLINE is_VK_KHR_SURFACE_EXTENSION_NAME #-}

is_VK_KHR_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_SURFACE_EXTENSION_NAME

type VK_KHR_SURFACE_EXTENSION_NAME = "VK_KHR_surface"

pattern VK_ERROR_SURFACE_LOST_KHR :: VkResult

pattern VK_ERROR_SURFACE_LOST_KHR = VkResult (-1000000000)

pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR :: VkResult

pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR = VkResult (-1000000001)

pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR =
        VK_COLOR_SPACE_SRGB_NONLINEAR_KHR

-- | VkSurfaceKHR
pattern VK_OBJECT_TYPE_SURFACE_KHR :: VkObjectType

pattern VK_OBJECT_TYPE_SURFACE_KHR = VkObjectType 1000000000
