{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- | This module allows to load vulkan symbols at runtime.
--
--   It is based on Vulkan API function
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetInstanceProcAddr.html vkGetInstanceProcAddr>
--   that is a part of Vulkan core 1.0.
--   Also, have a look at
--   <https://vulkan.lunarg.com/doc/view/1.1.70.1/windows/loader_and_layer_interface.html#user-content-instance-versus-device Vulkan loader>
--   page to see other reasons to load symbols manually.
module Graphics.Vulkan.Marshal.Proc
  ( VulkanProc (..)
  , vkGetInstanceProc, vkLookupInstanceProc
  , vkGetDeviceProc, vkLookupDeviceProc
  , vkGetProc, vkLookupProc
    -- * Re-export `Foreign.Ptr`
  , FunPtr, nullFunPtr
  ) where

import           Control.Monad                 (when)
import           Data.Void                     (Void)
import           Foreign.C.String              (CString, peekCString)
import           Foreign.ForeignPtr            (ForeignPtr, newForeignPtr,
                                                withForeignPtr)
import           Foreign.Marshal.Alloc         (alloca)
import           Foreign.Ptr                   (FunPtr, nullFunPtr, nullPtr)
import           Foreign.Storable              (peek)
import           GHC.Ptr                       (Ptr (..))
import           GHC.TypeLits                  (Symbol)
import           System.IO.Unsafe              (unsafePerformIO)



import           Graphics.Vulkan.Types.Handles (VkDevice, VkInstance)

-- | Some of the vulkan functions defined in vulkan extensions are not
--   available at the program linking time.
--   These functions should be discovered at runtime.
--   Vulkan api provides special functions for this,
--     called @vkGetInstanceProcAddr@ and @vkGetDeviceProcAddr@.
--   This class provides a simpler discovery mechanism based on that function.
--   For example, you can get @vkCreateDebugReportCallbackEXT@ function
--   as follows:
--
--   > vkGetInstanceProc @VkCreateDebugReportCallbackEXT vkInstance
class VulkanProc (proc :: Symbol) where
    -- | Haskell signature for the vulkan function
    type VkProcType proc
    -- | Name of the vulkan function
    vkProcSymbol :: CString
    -- | Convert C function pointer to an ordinary haskell function.
    unwrapVkProcPtr :: FunPtr (VkProcType proc) -> VkProcType proc

-- | An alternative to @vkGetInstanceProcAddr@ with type inference
--   and protection against typos.
--
--   Note, this is an unsafe function;
--   it does not check if the result of @vkGetInstanceProcAddr@
--   is a null function pointer.
vkGetInstanceProc :: forall proc . VulkanProc proc
                  => VkInstance -> IO (VkProcType proc)
vkGetInstanceProc i
  = unwrapVkProcPtr @proc
  <$> c'vkGetInstanceProcAddr i (vkProcSymbol @proc)
{-# INLINE vkGetInstanceProc #-}

-- | An alternative to @vkGetInstanceProcAddr@ with type inference
--   and protection against typos.
vkLookupInstanceProc :: forall proc . VulkanProc proc
                     => VkInstance -> IO (Maybe (VkProcType proc))
vkLookupInstanceProc i
    = f <$> c'vkGetInstanceProcAddr i (vkProcSymbol @proc)
  where
    f p = if p == nullFunPtr then Nothing else Just (unwrapVkProcPtr @proc p)
{-# INLINE vkLookupInstanceProc #-}


-- | An alternative to @vkGetDeviceProcAddr@ with type inference
--   and protection against typos.
--
--   Note, this is an unsafe function;
--   it does not check if the result of @vkGetInstanceProcAddr@
--   is a null function pointer.
vkGetDeviceProc :: forall proc . VulkanProc proc
                => VkDevice -> IO (VkProcType proc)
vkGetDeviceProc i
  = unwrapVkProcPtr @proc
  <$> c'vkGetDeviceProcAddr i (vkProcSymbol @proc)
{-# INLINE vkGetDeviceProc #-}

-- | An alternative to @vkGetDeviceProcAddr@ with type inference
--   and protection against typos.
vkLookupDeviceProc :: forall proc . VulkanProc proc
                   => VkDevice -> IO (Maybe (VkProcType proc))
vkLookupDeviceProc i
    = f <$> c'vkGetDeviceProcAddr i (vkProcSymbol @proc)
  where
    f p = if p == nullFunPtr then Nothing else Just (unwrapVkProcPtr @proc p)
{-# INLINE vkLookupDeviceProc #-}


-- | Locate Vulkan symbol dynamically at runtime using platform-dependent machinery,
--   such as @dlsym@ or @GetProcAddress@.
--   This function throws an error on failure.
--
--   Consider using `vkGetDeviceProc` or `vkGetInstanceProc` for loading a symbol,
--    because they can return a more optimized version of a function.
--   Also note, you are likely not able to lookup an extension funcion using
--   `vkGetProc`, because a corresponding symbol is simply not present in the
--   vulkan loader library.
vkGetProc :: forall proc . VulkanProc proc => IO (VkProcType proc)
vkGetProc = alloca $ \errPtr -> do
    fp <- withForeignPtr _vkDlHandle $ \h ->
      c'vkdll_dlsym h (vkProcSymbol @proc) errPtr
    when (fp == nullFunPtr) $ peek errPtr >>= peekCString >>= fail .
        ("An error happened while trying to load vulkan symbol dynamically: " ++)
    return $ unwrapVkProcPtr @proc fp
{-# INLINE vkGetProc #-}

-- | Locate Vulkan symbol dynamically at runtime using platform-dependent machinery,
--   such as @dlsym@ or @GetProcAddress@.
--   This function returns @Nothing@ on failure ignoring an error message.
--
--   Consider using `vkGetDeviceProc` or `vkGetInstanceProc` for loading a symbol,
--    because they can return a more optimized version of a function.
--   Also note, you are likely not able to lookup an extension funcion using
--   `vkLookupProc`, because a corresponding symbol is simply not present in the
--   vulkan loader library.
vkLookupProc :: forall proc . VulkanProc proc => IO (Maybe (VkProcType proc))
vkLookupProc = alloca $ \errPtr -> do
    fp <- withForeignPtr _vkDlHandle $ \h ->
      c'vkdll_dlsym h (vkProcSymbol @proc) errPtr
    return $ if fp == nullFunPtr then Nothing else Just (unwrapVkProcPtr @proc fp)
{-# INLINE vkLookupProc #-}







#ifdef VK_NO_PROTOTYPES

c'vkGetInstanceProcAddr :: VkInstance -> CString -> IO (FunPtr a)
c'vkGetInstanceProcAddr = unsafePerformIO $ alloca $ \errPtr -> do
    fp <- withForeignPtr _vkDlHandle $ \h ->
      c'vkdll_dlsym h (Ptr "vkGetInstanceProcAddr"#) errPtr
    when (fp == nullFunPtr) $
      peek errPtr >>= peekCString >>= fail .
        ("Could not load 'vkGetInstanceProcAddr' C function from vulkan library dynamically: " ++)
    return $ unwrap'vkGetInstanceProcAddr fp

c'vkGetDeviceProcAddr :: VkDevice -> CString -> IO (FunPtr a)
c'vkGetDeviceProcAddr = unsafePerformIO $ alloca $ \errPtr -> do
    fp <- withForeignPtr _vkDlHandle $ \h ->
      c'vkdll_dlsym h (Ptr "vkGetDeviceProcAddr"#) errPtr
    when (fp == nullFunPtr) $ peek errPtr >>= peekCString >>= fail .
        ("Could not load 'vkGetDeviceProcAddr' C function from vulkan library dynamically: " ++)
    return $ unwrap'vkGetDeviceProcAddr fp

foreign import ccall unsafe "dynamic"
  unwrap'vkGetInstanceProcAddr
    :: FunPtr (VkInstance -> CString -> IO (FunPtr a))
    -> VkInstance -> CString -> IO (FunPtr a)

foreign import ccall unsafe "dynamic"
  unwrap'vkGetDeviceProcAddr
    :: FunPtr (VkDevice -> CString -> IO (FunPtr a))
    -> VkDevice -> CString -> IO (FunPtr a)

#else

foreign import ccall unsafe "vkGetInstanceProcAddr"
  c'vkGetInstanceProcAddr :: VkInstance -> CString -> IO (FunPtr a)

foreign import ccall unsafe "vkGetDeviceProcAddr"
  c'vkGetDeviceProcAddr :: VkDevice -> CString -> IO (FunPtr a)

#endif


foreign import ccall safe "_vkdll_dlinit"
  c'vkdll_dlinit :: Ptr CString -> IO (Ptr Void)

foreign import ccall safe "_vkdll_dlsym"
  c'vkdll_dlsym :: Ptr Void -> CString -> Ptr CString -> IO (FunPtr a)

foreign import ccall safe "&_vkdll_dlclose"
  p'vk_dlclose :: FunPtr (Ptr Void -> IO ())

_vkDlHandle :: ForeignPtr Void
_vkDlHandle = unsafePerformIO $ alloca $ \errPtr -> do
  handle <- c'vkdll_dlinit errPtr
  if handle == nullPtr
  then
    peek errPtr >>= peekCString >>= fail .
      ("An error happened while trying to load vulkan library dynamically: " ++)
  else
    newForeignPtr p'vk_dlclose handle
{-# NOINLINE _vkDlHandle #-}
