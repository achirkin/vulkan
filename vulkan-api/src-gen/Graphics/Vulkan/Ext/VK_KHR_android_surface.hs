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
module Graphics.Vulkan.Ext.VK_KHR_android_surface
       (-- * Vulkan extension: @VK_KHR_android_surface@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @jessehall@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- platform: @android@
        --
        -- Extension number: @9@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        module Graphics.Vulkan.Types.Bitmasks,
        module Graphics.Vulkan.Types.Struct.PlatformAndroidKhr,
        module Graphics.Vulkan.Types.BaseTypes,
        module Graphics.Vulkan.Types.Enum.StructureType,
        -- > #include "vk_platform.h"
        VkCreateAndroidSurfaceKHR, pattern VkCreateAndroidSurfaceKHR,
        HS_vkCreateAndroidSurfaceKHR, PFN_vkCreateAndroidSurfaceKHR,
        vkCreateAndroidSurfaceKHR, vkCreateAndroidSurfaceKHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Defines,
        module Graphics.Vulkan.Types.Enum.InternalAllocationType,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Enum.SystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Struct.AllocationCallbacks,
        VK_KHR_ANDROID_SURFACE_SPEC_VERSION,
        pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION,
        VK_KHR_ANDROID_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR)
       where
import           GHC.Ptr                                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Defines
import           Graphics.Vulkan.Types.Enum.InternalAllocationType
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.SystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.AllocationCallbacks
import           Graphics.Vulkan.Types.Struct.PlatformAndroidKhr
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

pattern VkCreateAndroidSurfaceKHR :: CString

pattern VkCreateAndroidSurfaceKHR <-
        (is_VkCreateAndroidSurfaceKHR -> True)
  where VkCreateAndroidSurfaceKHR = _VkCreateAndroidSurfaceKHR

{-# INLINE _VkCreateAndroidSurfaceKHR #-}

_VkCreateAndroidSurfaceKHR :: CString
_VkCreateAndroidSurfaceKHR = Ptr "vkCreateAndroidSurfaceKHR\NUL"#

{-# INLINE is_VkCreateAndroidSurfaceKHR #-}

is_VkCreateAndroidSurfaceKHR :: CString -> Bool
is_VkCreateAndroidSurfaceKHR
  = (EQ ==) . cmpCStrings _VkCreateAndroidSurfaceKHR

type VkCreateAndroidSurfaceKHR = "vkCreateAndroidSurfaceKHR"

#ifdef NATIVE_FFI_VK_VERSION_1_0
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
-- > VkResult vkCreateAndroidSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkAndroidSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateAndroidSurfaceKHR vkCreateAndroidSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe "vkCreateAndroidSurfaceKHR"
               vkCreateAndroidSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkAndroidSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
-- > VkResult vkCreateAndroidSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkAndroidSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateAndroidSurfaceKHR vkCreateAndroidSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-0@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateAndroidSurfaceKHR <- vkGetInstanceProc @VkCreateAndroidSurfaceKHR vkInstance
--
vkCreateAndroidSurfaceKHR ::
                          VkInstance -- ^ instance
                                     ->
                            Ptr VkAndroidSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                              ->
                              Ptr VkAllocationCallbacks -- ^ pAllocator
                                                        -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                            -> IO VkResult
vkCreateAndroidSurfaceKHR d
  = unsafeDupablePerformIO
      (vkGetInstanceProc @VkCreateAndroidSurfaceKHR d)
      d

{-# INLINE vkCreateAndroidSurfaceKHR #-}

{-# WARNING
vkCreateAndroidSurfaceKHR"This function requires useNativeFFI-1-0 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetInstanceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-0.\n"
 #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_0
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
-- > VkResult vkCreateAndroidSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkAndroidSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateAndroidSurfaceKHR vkCreateAndroidSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe "vkCreateAndroidSurfaceKHR"
               vkCreateAndroidSurfaceKHRSafe ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkAndroidSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

#else
-- |
-- Success codes: 'VK_SUCCESS'.
--
-- Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
-- > VkResult vkCreateAndroidSurfaceKHR
-- >     ( VkInstance instance
-- >     , const VkAndroidSurfaceCreateInfoKHR* pCreateInfo
-- >     , const VkAllocationCallbacks* pAllocator
-- >     , VkSurfaceKHR* pSurface
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateAndroidSurfaceKHR vkCreateAndroidSurfaceKHR registry at www.khronos.org>
--
-- __Note:__ You should refrain from using this function directly
--           unless flag @useNativeFFI-1-0@ is enabled.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateAndroidSurfaceKHR <- vkGetInstanceProc @VkCreateAndroidSurfaceKHR vkInstance
--
vkCreateAndroidSurfaceKHRSafe ::
                              VkInstance -- ^ instance
                                         ->
                                Ptr VkAndroidSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                                  ->
                                  Ptr VkAllocationCallbacks -- ^ pAllocator
                                                            -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                                -> IO VkResult
vkCreateAndroidSurfaceKHRSafe = vkCreateAndroidSurfaceKHR

{-# INLINE vkCreateAndroidSurfaceKHRSafe #-}

{-# WARNING
vkCreateAndroidSurfaceKHRSafe"This function requires useNativeFFI-1-0 to use FFI for locating the C symbol statically.\nOtherwise it may call vkGetInstanceProcAddr every time you execute it if not inlined.\nYou should either lookup the function address manually or enable flag useNativeFFI-1-0.\n"
 #-}
#endif

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateAndroidSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkAndroidSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCreateAndroidSurfaceKHR vkCreateAndroidSurfaceKHR registry at www.khronos.org>
type HS_vkCreateAndroidSurfaceKHR =
     VkInstance -- ^ instance
                ->
       Ptr VkAndroidSurfaceCreateInfoKHR -- ^ pCreateInfo
                                         ->
         Ptr VkAllocationCallbacks -- ^ pAllocator
                                   -> Ptr VkSurfaceKHR -- ^ pSurface
                                                       -> IO VkResult

type PFN_vkCreateAndroidSurfaceKHR =
     FunPtr HS_vkCreateAndroidSurfaceKHR

foreign import ccall "dynamic" unwrapVkCreateAndroidSurfaceKHR ::
               PFN_vkCreateAndroidSurfaceKHR -> HS_vkCreateAndroidSurfaceKHR

instance VulkanProc "vkCreateAndroidSurfaceKHR" where
        type VkProcType "vkCreateAndroidSurfaceKHR" =
             HS_vkCreateAndroidSurfaceKHR
        vkProcSymbol = _VkCreateAndroidSurfaceKHR

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtr = unwrapVkCreateAndroidSurfaceKHR

        {-# INLINE unwrapVkProcPtr #-}

pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION = 6

type VK_KHR_ANDROID_SURFACE_SPEC_VERSION = 6

pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME -> True)
  where VK_KHR_ANDROID_SURFACE_EXTENSION_NAME
          = _VK_KHR_ANDROID_SURFACE_EXTENSION_NAME

{-# INLINE _VK_KHR_ANDROID_SURFACE_EXTENSION_NAME #-}

_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME :: CString
_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME
  = Ptr "VK_KHR_android_surface\NUL"#

{-# INLINE is_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME #-}

is_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_ANDROID_SURFACE_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_KHR_ANDROID_SURFACE_EXTENSION_NAME

type VK_KHR_ANDROID_SURFACE_EXTENSION_NAME =
     "VK_KHR_android_surface"

pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000008000
