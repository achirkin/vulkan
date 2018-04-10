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
module Graphics.Vulkan.Ext.VK_KHR_xcb_surface
       (-- * Vulkan extension: @VK_KHR_xcb_surface@
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
        -- platform: @xcb@
        --
        -- Extension number: @6@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.VkStructureType,
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.VkXcbSurfaceCreateInfoKHR,
        -- > #include "vk_platform.h"
        VkCreateXcbSurfaceKHR, pattern VkCreateXcbSurfaceKHR,
        HS_vkCreateXcbSurfaceKHR, PFN_vkCreateXcbSurfaceKHR,
        unwrapVkCreateXcbSurfaceKHR, vkCreateXcbSurfaceKHR,
        vkCreateXcbSurfaceKHRSafe,
        VkGetPhysicalDeviceXcbPresentationSupportKHR,
        pattern VkGetPhysicalDeviceXcbPresentationSupportKHR,
        HS_vkGetPhysicalDeviceXcbPresentationSupportKHR,
        PFN_vkGetPhysicalDeviceXcbPresentationSupportKHR,
        unwrapVkGetPhysicalDeviceXcbPresentationSupportKHR,
        vkGetPhysicalDeviceXcbPresentationSupportKHR,
        vkGetPhysicalDeviceXcbPresentationSupportKHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.VkInternalAllocationType,
        module Graphics.Vulkan.Types.Enum.VkResult,
        module Graphics.Vulkan.Types.Enum.VkSystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Include,
        module Graphics.Vulkan.Types.Struct.VkAllocationCallbacks,
        VK_KHR_XCB_SURFACE_SPEC_VERSION,
        pattern VK_KHR_XCB_SURFACE_SPEC_VERSION,
        VK_KHR_XCB_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR)
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
import           Graphics.Vulkan.Types.Struct.VkXcbSurfaceCreateInfoKHR
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

pattern VkCreateXcbSurfaceKHR :: CString

pattern VkCreateXcbSurfaceKHR <- (is_VkCreateXcbSurfaceKHR -> True)
  where VkCreateXcbSurfaceKHR = _VkCreateXcbSurfaceKHR

{-# INLINE _VkCreateXcbSurfaceKHR #-}

_VkCreateXcbSurfaceKHR :: CString
_VkCreateXcbSurfaceKHR = Ptr "vkCreateXcbSurfaceKHR\NUL"#

{-# INLINE is_VkCreateXcbSurfaceKHR #-}

is_VkCreateXcbSurfaceKHR :: CString -> Bool
is_VkCreateXcbSurfaceKHR
  = (EQ ==) . cmpCStrings _VkCreateXcbSurfaceKHR

type VkCreateXcbSurfaceKHR = "vkCreateXcbSurfaceKHR"

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateXcbSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkXcbSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateXcbSurfaceKHR vkCreateXcbSurfaceKHR registry at www.khronos.org>
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe "vkCreateXcbSurfaceKHR"
               vkCreateXcbSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkXcbSurfaceCreateInfoKHR -- ^ pCreateInfo
                                               ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
-- Note: without @useNativeFFI-1-0@ cabal flag this function may call `vkGetInstanceProcAddr` every time you execute it.
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag to call it natively to make sure you get the best performance.
vkCreateXcbSurfaceKHR ::
                      VkInstance -- ^ instance
                                 ->
                        Ptr VkXcbSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                      ->
                          Ptr VkAllocationCallbacks -- ^ pAllocator
                                                    -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                        -> IO VkResult
vkCreateXcbSurfaceKHR d
  = unsafeDupablePerformIO
      (vkGetInstanceProc @VkCreateXcbSurfaceKHR d)
      d

{-# INLINE vkCreateXcbSurfaceKHR #-}

{-# WARNING
vkCreateXcbSurfaceKHR"This function could be very inefficient. It may call vkGetInstanceProcAddr every time you call it. I suggest you to either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
#endif

-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
-- > VkResult vkCreateXcbSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkXcbSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateXcbSurfaceKHR vkCreateXcbSurfaceKHR registry at www.khronos.org>
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe "vkCreateXcbSurfaceKHR"
               vkCreateXcbSurfaceKHRSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkXcbSurfaceCreateInfoKHR -- ^ pCreateInfo
                                               ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
-- Note: without @useNativeFFI-1-0@ cabal flag this function may call `vkGetInstanceProcAddr` every time you execute it.
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag to call it natively to make sure you get the best performance.
vkCreateXcbSurfaceKHRSafe ::
                          VkInstance -- ^ instance
                                     ->
                            Ptr VkXcbSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                          ->
                              Ptr VkAllocationCallbacks -- ^ pAllocator
                                                        -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                            -> IO VkResult
vkCreateXcbSurfaceKHRSafe = vkCreateXcbSurfaceKHR

{-# INLINE vkCreateXcbSurfaceKHRSafe #-}

{-# WARNING
vkCreateXcbSurfaceKHRSafe"This function could be very inefficient. It may call vkGetInstanceProcAddr every time you call it. I suggest you to either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
#endif

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateXcbSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkXcbSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateXcbSurfaceKHR vkCreateXcbSurfaceKHR registry at www.khronos.org>
type HS_vkCreateXcbSurfaceKHR =
     VkInstance -- ^ instance
                ->
       Ptr VkXcbSurfaceCreateInfoKHR -- ^ pCreateInfo
                                     ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateXcbSurfaceKHR = FunPtr HS_vkCreateXcbSurfaceKHR

foreign import ccall "dynamic" unwrapVkCreateXcbSurfaceKHR ::
               PFN_vkCreateXcbSurfaceKHR -> HS_vkCreateXcbSurfaceKHR

instance VulkanProc "vkCreateXcbSurfaceKHR" where
        type VkProcType "vkCreateXcbSurfaceKHR" = HS_vkCreateXcbSurfaceKHR
        vkProcSymbol = _VkCreateXcbSurfaceKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateXcbSurfaceKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VkGetPhysicalDeviceXcbPresentationSupportKHR :: CString

pattern VkGetPhysicalDeviceXcbPresentationSupportKHR <-
        (is_VkGetPhysicalDeviceXcbPresentationSupportKHR -> True)
  where VkGetPhysicalDeviceXcbPresentationSupportKHR
          = _VkGetPhysicalDeviceXcbPresentationSupportKHR

{-# INLINE _VkGetPhysicalDeviceXcbPresentationSupportKHR #-}

_VkGetPhysicalDeviceXcbPresentationSupportKHR :: CString
_VkGetPhysicalDeviceXcbPresentationSupportKHR
  = Ptr "vkGetPhysicalDeviceXcbPresentationSupportKHR\NUL"#

{-# INLINE is_VkGetPhysicalDeviceXcbPresentationSupportKHR #-}

is_VkGetPhysicalDeviceXcbPresentationSupportKHR :: CString -> Bool
is_VkGetPhysicalDeviceXcbPresentationSupportKHR
  = (EQ ==) .
      cmpCStrings _VkGetPhysicalDeviceXcbPresentationSupportKHR

type VkGetPhysicalDeviceXcbPresentationSupportKHR =
     "vkGetPhysicalDeviceXcbPresentationSupportKHR"

-- |
-- > VkBool32 vkGetPhysicalDeviceXcbPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , xcb_connection_t* connection
-- >     , xcb_visualid_t visual_id
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceXcbPresentationSupportKHR vkGetPhysicalDeviceXcbPresentationSupportKHR registry at www.khronos.org>
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall unsafe
               "vkGetPhysicalDeviceXcbPresentationSupportKHR"
               vkGetPhysicalDeviceXcbPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ queueFamilyIndex
                        -> Ptr XcbConnectionT -- ^ connection
                                              -> XcbVisualidT -- ^ visual_id
                                                              -> IO VkBool32

#else
-- Warning: without @useNativeFFI-1-0@ cabal flag this function returns error!
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag.
vkGetPhysicalDeviceXcbPresentationSupportKHR ::
                                             VkPhysicalDevice -- ^ physicalDevice
                                                              ->
                                               Word32 -- ^ queueFamilyIndex
                                                      ->
                                                 Ptr XcbConnectionT -- ^ connection
                                                                    -> XcbVisualidT -- ^ visual_id
                                                                                    -> IO VkBool32
vkGetPhysicalDeviceXcbPresentationSupportKHR
  = error $
      "Cannot lookup C symbol \"vkGetPhysicalDeviceXcbPresentationSupportKHR\" because its signature does not provide VkInstance argument. "
        ++
        "Either lookup the function manually or enable useNativeFFI-1-0 cabal flag."

{-# WARNING
vkGetPhysicalDeviceXcbPresentationSupportKHR"This function will return error! Either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
#endif

-- |
-- > VkBool32 vkGetPhysicalDeviceXcbPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , xcb_connection_t* connection
-- >     , xcb_visualid_t visual_id
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceXcbPresentationSupportKHR vkGetPhysicalDeviceXcbPresentationSupportKHR registry at www.khronos.org>
#ifdef NATIVE_FFI_VK_VERSION_1_0
foreign import ccall safe
               "vkGetPhysicalDeviceXcbPresentationSupportKHR"
               vkGetPhysicalDeviceXcbPresentationSupportKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ queueFamilyIndex
                        -> Ptr XcbConnectionT -- ^ connection
                                              -> XcbVisualidT -- ^ visual_id
                                                              -> IO VkBool32

#else
-- Warning: without @useNativeFFI-1-0@ cabal flag this function returns error!
-- Either lookup the function manually or enable @useNativeFFI-1-0@ cabal flag.
vkGetPhysicalDeviceXcbPresentationSupportKHRSafe ::
                                                 VkPhysicalDevice -- ^ physicalDevice
                                                                  ->
                                                   Word32 -- ^ queueFamilyIndex
                                                          ->
                                                     Ptr XcbConnectionT -- ^ connection
                                                                        ->
                                                       XcbVisualidT -- ^ visual_id
                                                                    -> IO VkBool32
vkGetPhysicalDeviceXcbPresentationSupportKHRSafe
  = vkGetPhysicalDeviceXcbPresentationSupportKHR

{-# INLINE vkGetPhysicalDeviceXcbPresentationSupportKHRSafe #-}

{-# WARNING
vkGetPhysicalDeviceXcbPresentationSupportKHRSafe"This function will return error! Either lookup the function address manually or enable flag useNativeFFI-1-0"
 #-}
#endif

-- | > VkBool32 vkGetPhysicalDeviceXcbPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , xcb_connection_t* connection
--   >     , xcb_visualid_t visual_id
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceXcbPresentationSupportKHR vkGetPhysicalDeviceXcbPresentationSupportKHR registry at www.khronos.org>
type HS_vkGetPhysicalDeviceXcbPresentationSupportKHR =
     VkPhysicalDevice -- ^ physicalDevice
                      ->
       Word32 -- ^ queueFamilyIndex
              -> Ptr XcbConnectionT -- ^ connection
                                    -> XcbVisualidT -- ^ visual_id
                                                    -> IO VkBool32

type PFN_vkGetPhysicalDeviceXcbPresentationSupportKHR =
     FunPtr HS_vkGetPhysicalDeviceXcbPresentationSupportKHR

foreign import ccall "dynamic"
               unwrapVkGetPhysicalDeviceXcbPresentationSupportKHR ::
               PFN_vkGetPhysicalDeviceXcbPresentationSupportKHR ->
                 HS_vkGetPhysicalDeviceXcbPresentationSupportKHR

instance VulkanProc "vkGetPhysicalDeviceXcbPresentationSupportKHR"
         where
        type VkProcType "vkGetPhysicalDeviceXcbPresentationSupportKHR" =
             HS_vkGetPhysicalDeviceXcbPresentationSupportKHR
        vkProcSymbol = _VkGetPhysicalDeviceXcbPresentationSupportKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr
          = unwrapVkGetPhysicalDeviceXcbPresentationSupportKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_KHR_XCB_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_XCB_SURFACE_SPEC_VERSION = 6

type VK_KHR_XCB_SURFACE_SPEC_VERSION = 6

pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_XCB_SURFACE_EXTENSION_NAME -> True)
  where VK_KHR_XCB_SURFACE_EXTENSION_NAME
          = _VK_KHR_XCB_SURFACE_EXTENSION_NAME

{-# INLINE _VK_KHR_XCB_SURFACE_EXTENSION_NAME #-}

_VK_KHR_XCB_SURFACE_EXTENSION_NAME :: CString
_VK_KHR_XCB_SURFACE_EXTENSION_NAME = Ptr "VK_KHR_xcb_surface\NUL"#

{-# INLINE is_VK_KHR_XCB_SURFACE_EXTENSION_NAME #-}

is_VK_KHR_XCB_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_XCB_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_XCB_SURFACE_EXTENSION_NAME

type VK_KHR_XCB_SURFACE_EXTENSION_NAME = "VK_KHR_xcb_surface"

pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000005000
