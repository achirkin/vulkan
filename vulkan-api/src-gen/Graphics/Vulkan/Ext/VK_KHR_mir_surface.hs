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
module Graphics.Vulkan.Ext.VK_KHR_mir_surface
       (-- * Vulkan extension: @VK_KHR_mir_surface@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @jessehall,Ian Elliott ianelliott@google.com@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- platform: @mir@
        --
        -- Extension number: @8@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkMirSurfaceCreateInfoKHR,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        -- > #include "vk_platform.h"
        VkCreateMirSurfaceKHR, pattern VkCreateMirSurfaceKHR,
        HS_vkCreateMirSurfaceKHR, PFN_vkCreateMirSurfaceKHR,
        unwrapVkCreateMirSurfaceKHR, vkCreateMirSurfaceKHR,
        vkCreateMirSurfaceKHRSafe,
        VkGetPhysicalDeviceMirPresentationSupportKHR,
        pattern VkGetPhysicalDeviceMirPresentationSupportKHR,
        HS_vkGetPhysicalDeviceMirPresentationSupportKHR,
        PFN_vkGetPhysicalDeviceMirPresentationSupportKHR,
        unwrapVkGetPhysicalDeviceMirPresentationSupportKHR,
        vkGetPhysicalDeviceMirPresentationSupportKHR,
        vkGetPhysicalDeviceMirPresentationSupportKHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Include,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        VK_KHR_MIR_SURFACE_SPEC_VERSION,
        pattern VK_KHR_MIR_SURFACE_SPEC_VERSION,
        VK_KHR_MIR_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR)
       where
import           GHC.Ptr                                                (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.VkInternalAllocationType
import           Graphics.Vulkan.Types.Enum.VkResult
import           Graphics.Vulkan.Types.Enum.VkStructureType
import           Graphics.Vulkan.Types.Enum.VkSystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Include
import           Graphics.Vulkan.Types.Struct.VkAllocationCallbacks
import           Graphics.Vulkan.Types.Struct.VkMirSurfaceCreateInfoKHR
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

pattern VkCreateMirSurfaceKHR :: CString

pattern VkCreateMirSurfaceKHR <- (is_VkCreateMirSurfaceKHR -> True)
  where VkCreateMirSurfaceKHR = _VkCreateMirSurfaceKHR

{-# INLINE _VkCreateMirSurfaceKHR #-}

_VkCreateMirSurfaceKHR :: CString
_VkCreateMirSurfaceKHR = Ptr "vkCreateMirSurfaceKHR\NUL"#

{-# INLINE is_VkCreateMirSurfaceKHR #-}

is_VkCreateMirSurfaceKHR :: CString -> Bool
is_VkCreateMirSurfaceKHR
  = (EQ ==) . cmpCStrings _VkCreateMirSurfaceKHR

type VkCreateMirSurfaceKHR = "vkCreateMirSurfaceKHR"

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateMirSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkMirSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateMirSurfaceKHR vkCreateMirSurfaceKHR registry at www.khronos.org>
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateMirSurfaceKHR"
               vkCreateMirSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkMirSurfaceCreateInfoKHR -- ^ pCreateInfo
                                               ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
-- Note: without @useNativeFFI-1-0@ cabal flag this function may call `vkGetInstanceProcAddr` every time you execute it.
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag to call it natively to make sure you get the best performance.
vkCreateMirSurfaceKHR ::
                      VkInstance -- ^ instance
                                 ->
                        Ptr VkMirSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                      ->
                          Ptr VkAllocationCallbacks -- ^ pAllocator
                                                    -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                        -> IO VkResult
vkCreateMirSurfaceKHR d
  = unsafeDupablePerformIO
      (vkGetInstanceProc @VkCreateMirSurfaceKHR d)
      d

{-# INLINE vkCreateMirSurfaceKHR #-}

{-# WARNING
vkCreateMirSurfaceKHR"This function could be very inefficient. It may call vkGetInstanceProcAddr every time you call it. I suggest you to either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateMirSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkMirSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateMirSurfaceKHR vkCreateMirSurfaceKHR registry at www.khronos.org>
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateMirSurfaceKHR"
               vkCreateMirSurfaceKHRSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkMirSurfaceCreateInfoKHR -- ^ pCreateInfo
                                               ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
-- Note: without @useNativeFFI-1-0@ cabal flag this function may call `vkGetInstanceProcAddr` every time you execute it.
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag to call it natively to make sure you get the best performance.
vkCreateMirSurfaceKHRSafe ::
                          VkInstance -- ^ instance
                                     ->
                            Ptr VkMirSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                          ->
                              Ptr VkAllocationCallbacks -- ^ pAllocator
                                                        -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                            -> IO VkResult
vkCreateMirSurfaceKHRSafe = vkCreateMirSurfaceKHR

{-# INLINE vkCreateMirSurfaceKHRSafe #-}

{-# WARNING
vkCreateMirSurfaceKHRSafe"This function could be very inefficient. It may call vkGetInstanceProcAddr every time you call it. I suggest you to either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
#endif

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateMirSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkMirSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateMirSurfaceKHR vkCreateMirSurfaceKHR registry at www.khronos.org>
type HS_vkCreateMirSurfaceKHR =
     VkInstance -- ^ instance
                ->
       Ptr VkMirSurfaceCreateInfoKHR -- ^ pCreateInfo
                                     ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateMirSurfaceKHR = FunPtr HS_vkCreateMirSurfaceKHR

foreign import ccall "dynamic" unwrapVkCreateMirSurfaceKHR ::
               PFN_vkCreateMirSurfaceKHR -> HS_vkCreateMirSurfaceKHR

instance VulkanProc "vkCreateMirSurfaceKHR" where
        type VkProcType "vkCreateMirSurfaceKHR" = HS_vkCreateMirSurfaceKHR
        vkProcSymbol = _VkCreateMirSurfaceKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateMirSurfaceKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceMirPresentationSupportKHR :: CString

pattern VkGetPhysicalDeviceMirPresentationSupportKHR <-
        (is_VkGetPhysicalDeviceMirPresentationSupportKHR -> True)
  where VkGetPhysicalDeviceMirPresentationSupportKHR
          = _VkGetPhysicalDeviceMirPresentationSupportKHR

{-# INLINE _VkGetPhysicalDeviceMirPresentationSupportKHR #-}

_VkGetPhysicalDeviceMirPresentationSupportKHR :: CString
_VkGetPhysicalDeviceMirPresentationSupportKHR
  = Ptr "vkGetPhysicalDeviceMirPresentationSupportKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceMirPresentationSupportKHR #-}

is_VkGetPhysicalDeviceMirPresentationSupportKHR :: CString -> Bool
is_VkGetPhysicalDeviceMirPresentationSupportKHR
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceMirPresentationSupportKHR

type VkGetPhysicalDeviceMirPresentationSupportKHR =
     "vkGetPhysicalDeviceMirPresentationSupportKHR"

-- |
-- > VkBool32 vkGetPhysicalDeviceMirPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , MirConnection* connection
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMirPresentationSupportKHR vkGetPhysicalDeviceMirPresentationSupportKHR registry at www.khronos.org>
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe
               "vkGetPhysicalDeviceMirPresentationSupportKHR"
               vkGetPhysicalDeviceMirPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> Ptr MirConnection -- ^ connection
                                                               -> IO VkBool32

#else
-- Warning: without @useNativeFFI-1-0@ cabal flag this function returns error!
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag.
vkGetPhysicalDeviceMirPresentationSupportKHR ::
                                             VkPhysicalDevice -- ^ physicalDevice
                                                              ->
                                               Word32 -- ^ queueFamilyIndex
                                                      -> Ptr MirConnection -- ^ connection
                                                                           -> IO VkBool32
vkGetPhysicalDeviceMirPresentationSupportKHR
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDeviceMirPresentationSupportKHR\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-0 cabal flag."

{-# WARNING
vkGetPhysicalDeviceMirPresentationSupportKHR"This function will return error! Either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
#endif

-- |
-- > VkBool32 vkGetPhysicalDeviceMirPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , MirConnection* connection
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMirPresentationSupportKHR vkGetPhysicalDeviceMirPresentationSupportKHR registry at www.khronos.org>
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe
               "vkGetPhysicalDeviceMirPresentationSupportKHR"
               vkGetPhysicalDeviceMirPresentationSupportKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> Ptr MirConnection -- ^ connection
                                                               -> IO VkBool32

#else
-- Warning: without @useNativeFFI-1-0@ cabal flag this function returns error!
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag.
vkGetPhysicalDeviceMirPresentationSupportKHRSafe ::
                                                 VkPhysicalDevice -- ^ physicalDevice
                                                                  ->
                                                   Word32 -- ^ queueFamilyIndex
                                                          -> Ptr MirConnection -- ^ connection
                                                                               -> IO VkBool32
vkGetPhysicalDeviceMirPresentationSupportKHRSafe
  = vkGetPhysicalDeviceMirPresentationSupportKHR

{-# INLINE vkGetPhysicalDeviceMirPresentationSupportKHRSafe #-}

{-# WARNING
vkGetPhysicalDeviceMirPresentationSupportKHRSafe"This function will return error! Either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
#endif

-- | > VkBool32 vkGetPhysicalDeviceMirPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , MirConnection* connection
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMirPresentationSupportKHR vkGetPhysicalDeviceMirPresentationSupportKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceMirPresentationSupportKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      -> Word32 -- ^ queueFamilyIndex
                                -> Ptr MirConnection -- ^ connection
                                                     -> IO VkBool32

type PFN_vkGetPhysicalDeviceMirPresentationSupportKHR =
     FunPtr HS_vkGetPhysicalDeviceMirPresentationSupportKHR

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceMirPresentationSupportKHR ::
               PFN_vkGetPhysicalDeviceMirPresentationSupportKHR ->
                 HS_vkGetPhysicalDeviceMirPresentationSupportKHR

instance VulkanProc "vkGetPhysicalDeviceMirPresentationSupportKHR"
         where
        type VkProcType "vkGetPhysicalDeviceMirPresentationSupportKHR" =
             HS_vkGetPhysicalDeviceMirPresentationSupportKHR
        vkProcSymbol = _VkGetPhysicalDeviceMirPresentationSupportKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr
          = unwrapVkGetPhysicalDeviceMirPresentationSupportKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_KHR_MIR_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_MIR_SURFACE_SPEC_VERSION = 4

type VK_KHR_MIR_SURFACE_SPEC_VERSION = 4

pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_MIR_SURFACE_EXTENSION_NAME -> True)
  where VK_KHR_MIR_SURFACE_EXTENSION_NAME
          = _VK_KHR_MIR_SURFACE_EXTENSION_NAME

{-# INLINE _VK_KHR_MIR_SURFACE_EXTENSION_NAME #-}

_VK_KHR_MIR_SURFACE_EXTENSION_NAME :: CString
_VK_KHR_MIR_SURFACE_EXTENSION_NAME = Ptr "VK_KHR_mir_surface\NUL"#

{-# INLINE is_VK_KHR_MIR_SURFACE_EXTENSION_NAME #-}

is_VK_KHR_MIR_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_MIR_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_MIR_SURFACE_EXTENSION_NAME

type VK_KHR_MIR_SURFACE_EXTENSION_NAME = "VK_KHR_mir_surface"

pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000007000
