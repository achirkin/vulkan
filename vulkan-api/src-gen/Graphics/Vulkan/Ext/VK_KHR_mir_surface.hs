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
        module Graphics.Vulkan.Types.Struct.PlatformMirKhr,
        module Graphics.Vulkan.Types.Enum.StructureType,
        -- > #include "vk_platform.h"
        VkCreateMirSurfaceKHR, pattern VkCreateMirSurfaceKHR,
        HS_vkCreateMirSurfaceKHR, PFN_vkCreateMirSurfaceKHR,
        vkCreateMirSurfaceKHR, vkCreateMirSurfaceKHRSafe,
        VkGetPhysicalDeviceMirPresentationSupportKHR,
        pattern VkGetPhysicalDeviceMirPresentationSupportKHR,
        HS_vkGetPhysicalDeviceMirPresentationSupportKHR,
        PFN_vkGetPhysicalDeviceMirPresentationSupportKHR,
        vkGetPhysicalDeviceMirPresentationSupportKHR,
        vkGetPhysicalDeviceMirPresentationSupportKHRSafe,
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Enum.InternalAllocationType,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Enum.SystemAllocationScope,
        module Graphics.Vulkan.Types.Funcpointers,
        module Graphics.Vulkan.Types.Handles,
        module Graphics.Vulkan.Types.Include,
        module Graphics.Vulkan.Types.Struct.AllocationCallbacks,
        VK_KHR_MIR_SURFACE_SPEC_VERSION,
        pattern VK_KHR_MIR_SURFACE_SPEC_VERSION,
        VK_KHR_MIR_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR)
       where
import           GHC.Ptr                                           (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc
import           Graphics.Vulkan.Types.BaseTypes
import           Graphics.Vulkan.Types.Bitmasks
import           Graphics.Vulkan.Types.Enum.InternalAllocationType
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Enum.SystemAllocationScope
import           Graphics.Vulkan.Types.Funcpointers
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Include
import           Graphics.Vulkan.Types.Struct.AllocationCallbacks
import           Graphics.Vulkan.Types.Struct.PlatformMirKhr
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

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

#ifdef NATIVE_FFI_VK_VERSION_1_0
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
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
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
--
-- __Note:__ flag @useNativeFFI-1-0@ is disabled, so this function is looked up
--           dynamically at runtime;
--           @vkCreateMirSurfaceKHRSafe@ and @vkCreateMirSurfaceKHR@ are synonyms.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateMirSurfaceKHR <- vkGetInstanceProc @VkCreateMirSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateMirSurfaceKHR <- vkGetProc @VkCreateMirSurfaceKHR
--
vkCreateMirSurfaceKHR ::
                      VkInstance -- ^ instance
                                 ->
                        Ptr VkMirSurfaceCreateInfoKHR -- ^ pCreateInfo
                                                      ->
                          Ptr VkAllocationCallbacks -- ^ pAllocator
                                                    -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                        -> IO VkResult
vkCreateMirSurfaceKHR
  = unsafeDupablePerformIO (vkGetProc @VkCreateMirSurfaceKHR)

{-# NOINLINE vkCreateMirSurfaceKHR #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_0
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
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
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
--
-- __Note:__ flag @useNativeFFI-1-0@ is disabled, so this function is looked up
--           dynamically at runtime;
--           @vkCreateMirSurfaceKHRSafe@ and @vkCreateMirSurfaceKHR@ are synonyms.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myCreateMirSurfaceKHR <- vkGetInstanceProc @VkCreateMirSurfaceKHR vkInstance
--
-- or less efficient:
--
-- > myCreateMirSurfaceKHR <- vkGetProc @VkCreateMirSurfaceKHR
--
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

#ifdef NATIVE_FFI_VK_VERSION_1_0
-- |
-- > VkBool32 vkGetPhysicalDeviceMirPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , MirConnection* connection
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMirPresentationSupportKHR vkGetPhysicalDeviceMirPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall unsafe
               "vkGetPhysicalDeviceMirPresentationSupportKHR"
               vkGetPhysicalDeviceMirPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> Ptr MirConnection -- ^ connection
                                                               -> IO VkBool32

#else
-- |
-- > VkBool32 vkGetPhysicalDeviceMirPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , MirConnection* connection
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMirPresentationSupportKHR vkGetPhysicalDeviceMirPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is disabled, so this function is looked up
--           dynamically at runtime;
--           @vkGetPhysicalDeviceMirPresentationSupportKHRSafe@ and @vkGetPhysicalDeviceMirPresentationSupportKHR@ are synonyms.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceMirPresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceMirPresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceMirPresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceMirPresentationSupportKHR
--
vkGetPhysicalDeviceMirPresentationSupportKHR ::
                                             VkPhysicalDevice -- ^ physicalDevice
                                                              ->
                                               Word32 -- ^ queueFamilyIndex
                                                      -> Ptr MirConnection -- ^ connection
                                                                           -> IO VkBool32
vkGetPhysicalDeviceMirPresentationSupportKHR
  = unsafeDupablePerformIO
      (vkGetProc @VkGetPhysicalDeviceMirPresentationSupportKHR)

{-# NOINLINE vkGetPhysicalDeviceMirPresentationSupportKHR #-}
#endif

#ifdef NATIVE_FFI_VK_VERSION_1_0
-- |
-- > VkBool32 vkGetPhysicalDeviceMirPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , MirConnection* connection
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMirPresentationSupportKHR vkGetPhysicalDeviceMirPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is enabled, so this function is implemented
--           as a @foreign import@ call to C Vulkan loader.
--
foreign import ccall safe
               "vkGetPhysicalDeviceMirPresentationSupportKHR"
               vkGetPhysicalDeviceMirPresentationSupportKHRSafe ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Word32 -- ^ queueFamilyIndex
                                          -> Ptr MirConnection -- ^ connection
                                                               -> IO VkBool32

#else
-- |
-- > VkBool32 vkGetPhysicalDeviceMirPresentationSupportKHR
-- >     ( VkPhysicalDevice physicalDevice
-- >     , uint32_t queueFamilyIndex
-- >     , MirConnection* connection
-- >     )
--
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkGetPhysicalDeviceMirPresentationSupportKHR vkGetPhysicalDeviceMirPresentationSupportKHR registry at www.khronos.org>
--
-- __Note:__ flag @useNativeFFI-1-0@ is disabled, so this function is looked up
--           dynamically at runtime;
--           @vkGetPhysicalDeviceMirPresentationSupportKHRSafe@ and @vkGetPhysicalDeviceMirPresentationSupportKHR@ are synonyms.
--
-- Independently of the flag setting, you can lookup the function manually at runtime:
--
-- > myGetPhysicalDeviceMirPresentationSupportKHR <- vkGetInstanceProc @VkGetPhysicalDeviceMirPresentationSupportKHR vkInstance
--
-- or less efficient:
--
-- > myGetPhysicalDeviceMirPresentationSupportKHR <- vkGetProc @VkGetPhysicalDeviceMirPresentationSupportKHR
--
vkGetPhysicalDeviceMirPresentationSupportKHRSafe ::
                                                 VkPhysicalDevice -- ^ physicalDevice
                                                                  ->
                                                   Word32 -- ^ queueFamilyIndex
                                                          -> Ptr MirConnection -- ^ connection
                                                                               -> IO VkBool32
vkGetPhysicalDeviceMirPresentationSupportKHRSafe
  = vkGetPhysicalDeviceMirPresentationSupportKHR

{-# INLINE vkGetPhysicalDeviceMirPresentationSupportKHRSafe #-}
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
