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
        unwrapVkDestroySurfaceKHR, vkDestroySurfaceKHR,
        vkDestroySurfaceKHRSafe, VkGetPhysicalDeviceSurfaceSupportKHR,
        pattern VkGetPhysicalDeviceSurfaceSupportKHR,
        HS_vkGetPhysicalDeviceSurfaceSupportKHR,
        PFN_vkGetPhysicalDeviceSurfaceSupportKHR,
        unwrapVkGetPhysicalDeviceSurfaceSupportKHR,
        vkGetPhysicalDeviceSurfaceSupportKHR,
        vkGetPhysicalDeviceSurfaceSupportKHRSafe,
        VkGetPhysicalDeviceSurfaceCapabilitiesKHR,
        pattern VkGetPhysicalDeviceSurfaceCapabilitiesKHR,
        HS_vkGetPhysicalDeviceSurfaceCapabilitiesKHR,
        PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR,
        unwrapVkGetPhysicalDeviceSurfaceCapabilitiesKHR,
        vkGetPhysicalDeviceSurfaceCapabilitiesKHR,
        vkGetPhysicalDeviceSurfaceCapabilitiesKHRSafe,
        VkGetPhysicalDeviceSurfaceFormatsKHR,
        pattern VkGetPhysicalDeviceSurfaceFormatsKHR,
        HS_vkGetPhysicalDeviceSurfaceFormatsKHR,
        PFN_vkGetPhysicalDeviceSurfaceFormatsKHR,
        unwrapVkGetPhysicalDeviceSurfaceFormatsKHR,
        vkGetPhysicalDeviceSurfaceFormatsKHR,
        vkGetPhysicalDeviceSurfaceFormatsKHRSafe,
        VkGetPhysicalDeviceSurfacePresentModesKHR,
        pattern VkGetPhysicalDeviceSurfacePresentModesKHR,
        HS_vkGetPhysicalDeviceSurfacePresentModesKHR,
        PFN_vkGetPhysicalDeviceSurfacePresentModesKHR,
        unwrapVkGetPhysicalDeviceSurfacePresentModesKHR,
        vkGetPhysicalDeviceSurfacePresentModesKHR,
        vkGetPhysicalDeviceSurfacePresentModesKHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkColorSpaceKHR,
        module Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR,
        module Graphics.Vulkan.Types.Enum.VkFormat,
        module Graphics.Vulkan.Types.Enum.VkImageUsageFlags,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkPresentModeKHR,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        module Graphics.Vulkan.Types.Struct.VkExtent2D,
        module Graphics.Vulkan.Types.Struct.VkSurfaceCapabilitiesKHR,
        module Graphics.Vulkan.Types.Struct.VkSurfaceFormatKHR,
        VK_KHR_SURFACE_SPEC_VERSION, pattern VK_KHR_SURFACE_SPEC_VERSION,
        VK_KHR_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_SURFACE_EXTENSION_NAME,
        pattern VK_ERROR_SURFACE_LOST_KHR,
        pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR,
        pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR,
        pattern VK_OBJECT_TYPE_SURFACE_KHR)
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
import           Graphics.Vulkan.Types.Enum.VkObjectType               (VkObjectType (..))
import           Graphics.Vulkan.Types.Enum.VkPresentModeKHR
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR
import           Graphics.Vulkan.Types.Enum.VkSystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
import           Graphics.Vulkan.Types.Struct.VkExtent2D
import           Graphics.Vulkan.Types.Struct.VkSurfaceCapabilitiesKHR
import           Graphics.Vulkan.Types.Struct.VkSurfaceFormatKHR
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

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
-- > () vkDestroySurfaceKHR
-- >     ( VkInstance instance
-- >     , VkSurfaceKHR surface
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySurfaceKHR vkDestroySurfaceKHR registry at www.khronos.org>
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkDestroySurfaceKHR"
               vkDestroySurfaceKHR ::
               VkInstance -- ^ instance
                          -> VkSurfaceKHR -- ^ surface
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

#else
-- Note: without @useNativeFFI-1-0@ cabal flag this function may call `vkGetInstanceProcAddr` every time you execute it.
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag to call it natively to make sure you get the best performance.
vkDestroySurfaceKHR ::
                    VkInstance -- ^ instance
                               -> VkSurfaceKHR -- ^ surface
                                               -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                            -> IO ()
vkDestroySurfaceKHR d
  = unsafeDupablePerformIO (vkGetInstanceProc @VkDestroySurfaceKHR d)
      d

{-# INLINE vkDestroySurfaceKHR #-}

{-# WARNING
vkDestroySurfaceKHR"This function could be very inefficient. It may call vkGetInstanceProcAddr every time you call it. I suggest you to either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
#endif

-- |
-- > () vkDestroySurfaceKHR
-- >     ( VkInstance instance
-- >     , VkSurfaceKHR surface
-- >     , const VkAllocationCallbacks* pAllocator
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDestroySurfaceKHR vkDestroySurfaceKHR registry at www.khronos.org>
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkDestroySurfaceKHR"
               vkDestroySurfaceKHRSafe ::
               VkInstance -- ^ instance
                          -> VkSurfaceKHR -- ^ surface
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

#else
-- Note: without @useNativeFFI-1-0@ cabal flag this function may call `vkGetInstanceProcAddr` every time you execute it.
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag to call it natively to make sure you get the best performance.
vkDestroySurfaceKHRSafe ::
                        VkInstance -- ^ instance
                                   -> VkSurfaceKHR -- ^ surface
                                                   -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                                -> IO ()
vkDestroySurfaceKHRSafe = vkDestroySurfaceKHR

{-# INLINE vkDestroySurfaceKHRSafe #-}

{-# WARNING
vkDestroySurfaceKHRSafe"This function could be very inefficient. It may call vkGetInstanceProcAddr every time you call it. I suggest you to either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
#endif

-- | > () vkDestroySurfaceKHR
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

foreign import ccall "dynamic" unwrapVkDestroySurfaceKHR ::
               PFN_vkDestroySurfaceKHR -> HS_vkDestroySurfaceKHR

instance VulkanProc "vkDestroySurfaceKHR" where
        type VkProcType "vkDestroySurfaceKHR" = HS_vkDestroySurfaceKHR
        vkProcSymbol = _VkDestroySurfaceKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkDestroySurfaceKHR

        {-# INLINE unwrapVkProcPtr #-}

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
-- Warning: without @useNativeFFI-1-0@ cabal flag this function returns error!
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag.
vkGetPhysicalDeviceSurfaceSupportKHR ::
                                     VkPhysicalDevice -- ^ physicalDevice
                                                      ->
                                       Word32 -- ^ queueFamilyIndex
                                              -> VkSurfaceKHR -- ^ surface
                                                              -> Ptr VkBool32 -- ^ pSupported
                                                                              -> IO VkResult
vkGetPhysicalDeviceSurfaceSupportKHR
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDeviceSurfaceSupportKHR\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-0 cabal flag."

{-# WARNING
vkGetPhysicalDeviceSurfaceSupportKHR"This function will return error! Either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
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
-- Warning: without @useNativeFFI-1-0@ cabal flag this function returns error!
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag.
vkGetPhysicalDeviceSurfaceSupportKHRSafe ::
                                         VkPhysicalDevice -- ^ physicalDevice
                                                          ->
                                           Word32 -- ^ queueFamilyIndex
                                                  -> VkSurfaceKHR -- ^ surface
                                                                  -> Ptr VkBool32 -- ^ pSupported
                                                                                  -> IO VkResult
vkGetPhysicalDeviceSurfaceSupportKHRSafe
  = vkGetPhysicalDeviceSurfaceSupportKHR

{-# INLINE vkGetPhysicalDeviceSurfaceSupportKHRSafe #-}

{-# WARNING
vkGetPhysicalDeviceSurfaceSupportKHRSafe"This function will return error! Either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
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

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceSupportKHR ::
               PFN_vkGetPhysicalDeviceSurfaceSupportKHR ->
                 HS_vkGetPhysicalDeviceSurfaceSupportKHR

instance VulkanProc "vkGetPhysicalDeviceSurfaceSupportKHR" where
        type VkProcType "vkGetPhysicalDeviceSurfaceSupportKHR" =
             HS_vkGetPhysicalDeviceSurfaceSupportKHR
        vkProcSymbol = _VkGetPhysicalDeviceSurfaceSupportKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceSurfaceSupportKHR

        {-# INLINE unwrapVkProcPtr #-}

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
-- Warning: without @useNativeFFI-1-0@ cabal flag this function returns error!
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag.
vkGetPhysicalDeviceSurfaceCapabilitiesKHR ::
                                          VkPhysicalDevice -- ^ physicalDevice
                                                           ->
                                            VkSurfaceKHR -- ^ surface
                                                         ->
                                              Ptr VkSurfaceCapabilitiesKHR -- ^ pSurfaceCapabilities
                                                                           -> IO VkResult
vkGetPhysicalDeviceSurfaceCapabilitiesKHR
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDeviceSurfaceCapabilitiesKHR\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-0 cabal flag."

{-# WARNING
vkGetPhysicalDeviceSurfaceCapabilitiesKHR"This function will return error! Either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
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
-- Warning: without @useNativeFFI-1-0@ cabal flag this function returns error!
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag.
vkGetPhysicalDeviceSurfaceCapabilitiesKHRSafe ::
                                              VkPhysicalDevice -- ^ physicalDevice
                                                               ->
                                                VkSurfaceKHR -- ^ surface
                                                             ->
                                                  Ptr VkSurfaceCapabilitiesKHR -- ^ pSurfaceCapabilities
                                                                               -> IO VkResult
vkGetPhysicalDeviceSurfaceCapabilitiesKHRSafe
  = vkGetPhysicalDeviceSurfaceCapabilitiesKHR

{-# INLINE vkGetPhysicalDeviceSurfaceCapabilitiesKHRSafe #-}

{-# WARNING
vkGetPhysicalDeviceSurfaceCapabilitiesKHRSafe"This function will return error! Either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
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

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceCapabilitiesKHR ::
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
-- Warning: without @useNativeFFI-1-0@ cabal flag this function returns error!
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag.
vkGetPhysicalDeviceSurfaceFormatsKHR ::
                                     VkPhysicalDevice -- ^ physicalDevice
                                                      ->
                                       VkSurfaceKHR -- ^ surface
                                                    ->
                                         Ptr Word32 -- ^ pSurfaceFormatCount
                                                    -> Ptr VkSurfaceFormatKHR -- ^ pSurfaceFormats
                                                                              -> IO VkResult
vkGetPhysicalDeviceSurfaceFormatsKHR
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDeviceSurfaceFormatsKHR\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-0 cabal flag."

{-# WARNING
vkGetPhysicalDeviceSurfaceFormatsKHR"This function will return error! Either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
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
-- Warning: without @useNativeFFI-1-0@ cabal flag this function returns error!
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag.
vkGetPhysicalDeviceSurfaceFormatsKHRSafe ::
                                         VkPhysicalDevice -- ^ physicalDevice
                                                          ->
                                           VkSurfaceKHR -- ^ surface
                                                        ->
                                             Ptr Word32 -- ^ pSurfaceFormatCount
                                                        -> Ptr VkSurfaceFormatKHR -- ^ pSurfaceFormats
                                                                                  -> IO VkResult
vkGetPhysicalDeviceSurfaceFormatsKHRSafe
  = vkGetPhysicalDeviceSurfaceFormatsKHR

{-# INLINE vkGetPhysicalDeviceSurfaceFormatsKHRSafe #-}

{-# WARNING
vkGetPhysicalDeviceSurfaceFormatsKHRSafe"This function will return error! Either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
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

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceSurfaceFormatsKHR ::
               PFN_vkGetPhysicalDeviceSurfaceFormatsKHR ->
                 HS_vkGetPhysicalDeviceSurfaceFormatsKHR

instance VulkanProc "vkGetPhysicalDeviceSurfaceFormatsKHR" where
        type VkProcType "vkGetPhysicalDeviceSurfaceFormatsKHR" =
             HS_vkGetPhysicalDeviceSurfaceFormatsKHR
        vkProcSymbol = _VkGetPhysicalDeviceSurfaceFormatsKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkGetPhysicalDeviceSurfaceFormatsKHR

        {-# INLINE unwrapVkProcPtr #-}

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
-- Warning: without @useNativeFFI-1-0@ cabal flag this function returns error!
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag.
vkGetPhysicalDeviceSurfacePresentModesKHR ::
                                          VkPhysicalDevice -- ^ physicalDevice
                                                           ->
                                            VkSurfaceKHR -- ^ surface
                                                         ->
                                              Ptr Word32 -- ^ pPresentModeCount
                                                         -> Ptr VkPresentModeKHR -- ^ pPresentModes
                                                                                 -> IO VkResult
vkGetPhysicalDeviceSurfacePresentModesKHR
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDeviceSurfacePresentModesKHR\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-0 cabal flag."

{-# WARNING
vkGetPhysicalDeviceSurfacePresentModesKHR"This function will return error! Either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
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
-- Warning: without @useNativeFFI-1-0@ cabal flag this function returns error!
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag.
vkGetPhysicalDeviceSurfacePresentModesKHRSafe ::
                                              VkPhysicalDevice -- ^ physicalDevice
                                                               ->
                                                VkSurfaceKHR -- ^ surface
                                                             ->
                                                  Ptr Word32 -- ^ pPresentModeCount
                                                             -> Ptr VkPresentModeKHR -- ^ pPresentModes
                                                                                     -> IO VkResult
vkGetPhysicalDeviceSurfacePresentModesKHRSafe
  = vkGetPhysicalDeviceSurfacePresentModesKHR

{-# INLINE vkGetPhysicalDeviceSurfacePresentModesKHRSafe #-}

{-# WARNING
vkGetPhysicalDeviceSurfacePresentModesKHRSafe"This function will return error! Either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
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

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceSurfacePresentModesKHR ::
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
