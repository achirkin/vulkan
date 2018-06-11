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
--
--   All FFI functions are present in two variants:
--   @xxxUnsafe@ and @xxxSafe@, the names stand for @foreign import unsafe xxx@
--   @foreign import safe xxx@ respectively.
--   In particular, that does not mean that @vkGetXxxProcSafe@ function cannot fail;
--   it does error if the symbol is not present in the implementation!
module Graphics.Vulkan.Marshal.Proc
  ( VulkanProc (..)
  , vkGetInstanceProc, vkGetInstanceProcUnsafe, vkGetInstanceProcSafe
  , vkLookupInstanceProc, vkLookupInstanceProcUnsafe, vkLookupInstanceProcSafe
  , vkGetDeviceProc, vkGetDeviceProcUnsafe, vkGetDeviceProcSafe
  , vkLookupDeviceProc, vkLookupDeviceProcUnsafe, vkLookupDeviceProcSafe
  , vkGetProc, vkGetProcUnsafe, vkGetProcSafe
  , vkLookupProc, vkLookupProcUnsafe, vkLookupProcSafe
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
    --   Use unsafe FFI (@foreign import unsafe "dynamic" ...@).
    unwrapVkProcPtrUnsafe :: FunPtr (VkProcType proc) -> VkProcType proc
    -- | Convert C function pointer to an ordinary haskell function.
    --   Use safe FFI (@foreign import safe "dynamic" ...@).
    unwrapVkProcPtrSafe :: FunPtr (VkProcType proc) -> VkProcType proc



--------------------------------------------------------------------------------
-- Unsafe FFI version
--------------------------------------------------------------------------------





-- | An alternative to @vkGetInstanceProcAddr@ with type inference
--   and protection against typos.
--
--   Note, this is an unsafe function;
--   it does not check if the result of @vkGetInstanceProcAddr@
--   is a null function pointer.
vkGetInstanceProcUnsafe :: forall proc . VulkanProc proc
                        => VkInstance -> IO (VkProcType proc)
vkGetInstanceProcUnsafe i
  = unwrapVkProcPtrUnsafe @proc
  <$> c'vkGetInstanceProcAddrUnsafe i (vkProcSymbol @proc)
{-# INLINE vkGetInstanceProcUnsafe #-}

-- | An alternative to @vkGetInstanceProcAddr@ with type inference
--   and protection against typos.
vkLookupInstanceProcUnsafe :: forall proc . VulkanProc proc
                           => VkInstance -> IO (Maybe (VkProcType proc))
vkLookupInstanceProcUnsafe i
    = f <$> c'vkGetInstanceProcAddrUnsafe i (vkProcSymbol @proc)
  where
    f p = if p == nullFunPtr then Nothing else Just (unwrapVkProcPtrUnsafe @proc p)
{-# INLINE vkLookupInstanceProcUnsafe #-}


-- | An alternative to @vkGetDeviceProcAddr@ with type inference
--   and protection against typos.
--
--   Note, this is an unsafe function;
--   it does not check if the result of @vkGetInstanceProcAddr@
--   is a null function pointer.
vkGetDeviceProcUnsafe :: forall proc . VulkanProc proc
                      => VkDevice -> IO (VkProcType proc)
vkGetDeviceProcUnsafe i
  = unwrapVkProcPtrUnsafe @proc
  <$> c'vkGetDeviceProcAddrUnsafe i (vkProcSymbol @proc)
{-# INLINE vkGetDeviceProcUnsafe #-}

-- | An alternative to @vkGetDeviceProcAddr@ with type inference
--   and protection against typos.
vkLookupDeviceProcUnsafe :: forall proc . VulkanProc proc
                         => VkDevice -> IO (Maybe (VkProcType proc))
vkLookupDeviceProcUnsafe i
    = f <$> c'vkGetDeviceProcAddrUnsafe i (vkProcSymbol @proc)
  where
    f p = if p == nullFunPtr then Nothing else Just (unwrapVkProcPtrUnsafe @proc p)
{-# INLINE vkLookupDeviceProcUnsafe #-}


-- | Locate Vulkan symbol dynamically at runtime using platform-dependent machinery,
--   such as @dlsym@ or @GetProcAddress@.
--   This function throws an error on failure.
--
--   Consider using `vkGetDeviceProc` or `vkGetInstanceProc` for loading a symbol,
--    because they can return a more optimized version of a function.
--   Also note, you are likely not able to lookup an extension funcion using
--   `vkGetProc`, because a corresponding symbol is simply not present in the
--   vulkan loader library.
vkGetProcUnsafe :: forall proc . VulkanProc proc => IO (VkProcType proc)
vkGetProcUnsafe = alloca $ \errPtr -> do
    fp <- withForeignPtr _vkDlHandle $ \h ->
      c'vkdll_dlsym h (vkProcSymbol @proc) errPtr
    when (fp == nullFunPtr) $ peek errPtr >>= peekCString >>= fail .
        ("An error happened while trying to load vulkan symbol dynamically: " ++)
    return $ unwrapVkProcPtrUnsafe @proc fp
{-# INLINE vkGetProcUnsafe #-}

-- | Locate Vulkan symbol dynamically at runtime using platform-dependent machinery,
--   such as @dlsym@ or @GetProcAddress@.
--   This function returns @Nothing@ on failure ignoring an error message.
--
--   Consider using `vkGetDeviceProc` or `vkGetInstanceProc` for loading a symbol,
--    because they can return a more optimized version of a function.
--   Also note, you are likely not able to lookup an extension funcion using
--   `vkLookupProc`, because a corresponding symbol is simply not present in the
--   vulkan loader library.
vkLookupProcUnsafe :: forall proc . VulkanProc proc => IO (Maybe (VkProcType proc))
vkLookupProcUnsafe = alloca $ \errPtr -> do
    fp <- withForeignPtr _vkDlHandle $ \h ->
      c'vkdll_dlsym h (vkProcSymbol @proc) errPtr
    return $ if fp == nullFunPtr then Nothing else Just (unwrapVkProcPtrUnsafe @proc fp)
{-# INLINE vkLookupProcUnsafe #-}



--------------------------------------------------------------------------------
-- Safe FFI version
--------------------------------------------------------------------------------



-- | An alternative to @vkGetInstanceProcAddr@ with type inference
--   and protection against typos.
--
--   Note, this is an unsafe function;
--   it does not check if the result of @vkGetInstanceProcAddr@
--   is a null function pointer.
vkGetInstanceProcSafe :: forall proc . VulkanProc proc
                  => VkInstance -> IO (VkProcType proc)
vkGetInstanceProcSafe i
  = unwrapVkProcPtrSafe @proc
  <$> c'vkGetInstanceProcAddrSafe i (vkProcSymbol @proc)
{-# INLINE vkGetInstanceProcSafe #-}

-- | An alternative to @vkGetInstanceProcAddr@ with type inference
--   and protection against typos.
vkLookupInstanceProcSafe :: forall proc . VulkanProc proc
                     => VkInstance -> IO (Maybe (VkProcType proc))
vkLookupInstanceProcSafe i
    = f <$> c'vkGetInstanceProcAddrSafe i (vkProcSymbol @proc)
  where
    f p = if p == nullFunPtr then Nothing else Just (unwrapVkProcPtrSafe @proc p)
{-# INLINE vkLookupInstanceProcSafe #-}


-- | An alternative to @vkGetDeviceProcAddr@ with type inference
--   and protection against typos.
--
--   Note, this is an unsafe function;
--   it does not check if the result of @vkGetInstanceProcAddr@
--   is a null function pointer.
vkGetDeviceProcSafe :: forall proc . VulkanProc proc
                => VkDevice -> IO (VkProcType proc)
vkGetDeviceProcSafe i
  = unwrapVkProcPtrSafe @proc
  <$> c'vkGetDeviceProcAddrSafe i (vkProcSymbol @proc)
{-# INLINE vkGetDeviceProcSafe #-}

-- | An alternative to @vkGetDeviceProcAddr@ with type inference
--   and protection against typos.
vkLookupDeviceProcSafe :: forall proc . VulkanProc proc
                   => VkDevice -> IO (Maybe (VkProcType proc))
vkLookupDeviceProcSafe i
    = f <$> c'vkGetDeviceProcAddrSafe i (vkProcSymbol @proc)
  where
    f p = if p == nullFunPtr then Nothing else Just (unwrapVkProcPtrSafe @proc p)
{-# INLINE vkLookupDeviceProcSafe #-}


-- | Locate Vulkan symbol dynamically at runtime using platform-dependent machinery,
--   such as @dlsym@ or @GetProcAddress@.
--   This function throws an error on failure.
--
--   Consider using `vkGetDeviceProc` or `vkGetInstanceProc` for loading a symbol,
--    because they can return a more optimized version of a function.
--   Also note, you are likely not able to lookup an extension funcion using
--   `vkGetProc`, because a corresponding symbol is simply not present in the
--   vulkan loader library.
vkGetProcSafe :: forall proc . VulkanProc proc => IO (VkProcType proc)
vkGetProcSafe = alloca $ \errPtr -> do
    fp <- withForeignPtr _vkDlHandle $ \h ->
      c'vkdll_dlsym h (vkProcSymbol @proc) errPtr
    when (fp == nullFunPtr) $ peek errPtr >>= peekCString >>= fail .
        ("An error happened while trying to load vulkan symbol dynamically: " ++)
    return $ unwrapVkProcPtrSafe @proc fp
{-# INLINE vkGetProcSafe #-}

-- | Locate Vulkan symbol dynamically at runtime using platform-dependent machinery,
--   such as @dlsym@ or @GetProcAddress@.
--   This function returns @Nothing@ on failure ignoring an error message.
--
--   Consider using `vkLookupDeviceProc` or `vkLookupInstanceProc` for loading a symbol,
--    because they can return a more optimized version of a function.
--   Also note, you are likely not able to lookup an extension funcion using
--   `vkLookupProc`, because a corresponding symbol is simply not present in the
--   vulkan loader library.
vkLookupProcSafe :: forall proc . VulkanProc proc => IO (Maybe (VkProcType proc))
vkLookupProcSafe = alloca $ \errPtr -> do
    fp <- withForeignPtr _vkDlHandle $ \h ->
      c'vkdll_dlsym h (vkProcSymbol @proc) errPtr
    return $ if fp == nullFunPtr then Nothing else Just (unwrapVkProcPtrSafe @proc fp)
{-# INLINE vkLookupProcSafe #-}



--------------------------------------------------------------------------------
-- Default FFI version
--------------------------------------------------------------------------------



-- | An alternative to @vkGetInstanceProcAddr@ with type inference
--   and protection against typos.
--
--   Note, this is an unsafe function;
--   it does not check if the result of @vkGetInstanceProcAddr@
--   is a null function pointer.
vkGetInstanceProc :: forall proc . VulkanProc proc
                  => VkInstance -> IO (VkProcType proc)
vkGetInstanceProc =
#ifdef UNSAFE_FFI_DEFAULT
  vkGetInstanceProcUnsafe @proc
#else
  vkGetInstanceProcSafe @proc
#endif
{-# INLINE vkGetInstanceProc #-}

-- | An alternative to @vkGetInstanceProcAddr@ with type inference
--   and protection against typos.
vkLookupInstanceProc :: forall proc . VulkanProc proc
                     => VkInstance -> IO (Maybe (VkProcType proc))
vkLookupInstanceProc =
#ifdef UNSAFE_FFI_DEFAULT
  vkLookupInstanceProcUnsafe @proc
#else
  vkLookupInstanceProcSafe @proc
#endif
{-# INLINE vkLookupInstanceProc #-}


-- | An alternative to @vkGetDeviceProcAddr@ with type inference
--   and protection against typos.
--
--   Note, this is an unsafe function;
--   it does not check if the result of @vkGetInstanceProcAddr@
--   is a null function pointer.
vkGetDeviceProc :: forall proc . VulkanProc proc
                => VkDevice -> IO (VkProcType proc)
vkGetDeviceProc =
#ifdef UNSAFE_FFI_DEFAULT
  vkGetDeviceProcUnsafe @proc
#else
  vkGetDeviceProcSafe @proc
#endif
{-# INLINE vkGetDeviceProc #-}

-- | An alternative to @vkGetDeviceProcAddr@ with type inference
--   and protection against typos.
vkLookupDeviceProc :: forall proc . VulkanProc proc
                   => VkDevice -> IO (Maybe (VkProcType proc))
vkLookupDeviceProc =
#ifdef UNSAFE_FFI_DEFAULT
  vkLookupDeviceProcUnsafe @proc
#else
  vkLookupDeviceProcSafe @proc
#endif
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
vkGetProc =
#ifdef UNSAFE_FFI_DEFAULT
  vkGetProcUnsafe @proc
#else
  vkGetProcSafe @proc
#endif
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
vkLookupProc =
#ifdef UNSAFE_FFI_DEFAULT
  vkLookupProcUnsafe @proc
#else
  vkLookupProcSafe @proc
#endif
{-# INLINE vkLookupProc #-}




--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------




#ifdef VK_NO_PROTOTYPES

c'vkGetInstanceProcAddrSafe :: VkInstance -> CString -> IO (FunPtr a)
c'vkGetInstanceProcAddrSafe = c'vkGetInstanceProcAddr' unwrap'vkGetInstanceProcAddrSafe

c'vkGetInstanceProcAddrUnsafe :: VkInstance -> CString -> IO (FunPtr a)
c'vkGetInstanceProcAddrUnsafe = c'vkGetInstanceProcAddr' unwrap'vkGetInstanceProcAddrUnsafe

c'vkGetDeviceProcAddrSafe :: VkDevice -> CString -> IO (FunPtr a)
c'vkGetDeviceProcAddrSafe = c'vkGetDeviceProcAddr' unwrap'vkGetDeviceProcAddrSafe

c'vkGetDeviceProcAddrUnsafe :: VkDevice -> CString -> IO (FunPtr a)
c'vkGetDeviceProcAddrUnsafe = c'vkGetDeviceProcAddr' unwrap'vkGetDeviceProcAddrUnsafe

c'vkGetInstanceProcAddr'
    :: ( FunPtr (VkInstance -> CString -> IO (FunPtr a))
          -> VkInstance -> CString -> IO (FunPtr a)
       )
    -> VkInstance -> CString -> IO (FunPtr a)
c'vkGetInstanceProcAddr' k = unsafePerformIO $ alloca $ \errPtr -> do
    fp <- withForeignPtr _vkDlHandle $ \h ->
      c'vkdll_dlsym h (Ptr "vkGetInstanceProcAddr"#) errPtr
    when (fp == nullFunPtr) $
      peek errPtr >>= peekCString >>= fail .
        ("Could not load 'vkGetInstanceProcAddr' C function from vulkan library dynamically: " ++)
    return $ k fp

c'vkGetDeviceProcAddr'
    :: ( FunPtr (VkDevice -> CString -> IO (FunPtr a))
          -> VkDevice -> CString -> IO (FunPtr a)
       )
    -> VkDevice -> CString -> IO (FunPtr a)
c'vkGetDeviceProcAddr' k = unsafePerformIO $ alloca $ \errPtr -> do
    fp <- withForeignPtr _vkDlHandle $ \h ->
      c'vkdll_dlsym h (Ptr "vkGetDeviceProcAddr"#) errPtr
    when (fp == nullFunPtr) $ peek errPtr >>= peekCString >>= fail .
        ("Could not load 'vkGetDeviceProcAddr' C function from vulkan library dynamically: " ++)
    return $ k fp

foreign import ccall safe "dynamic"
  unwrap'vkGetInstanceProcAddrSafe
    :: FunPtr (VkInstance -> CString -> IO (FunPtr a))
    -> VkInstance -> CString -> IO (FunPtr a)

foreign import ccall safe "dynamic"
  unwrap'vkGetDeviceProcAddrSafe
    :: FunPtr (VkDevice -> CString -> IO (FunPtr a))
    -> VkDevice -> CString -> IO (FunPtr a)

foreign import ccall unsafe "dynamic"
  unwrap'vkGetInstanceProcAddrUnsafe
    :: FunPtr (VkInstance -> CString -> IO (FunPtr a))
    -> VkInstance -> CString -> IO (FunPtr a)

foreign import ccall unsafe "dynamic"
  unwrap'vkGetDeviceProcAddrUnsafe
    :: FunPtr (VkDevice -> CString -> IO (FunPtr a))
    -> VkDevice -> CString -> IO (FunPtr a)

#else

foreign import ccall safe "vkGetInstanceProcAddr"
  c'vkGetInstanceProcAddrSafe :: VkInstance -> CString -> IO (FunPtr a)

foreign import ccall safe "vkGetDeviceProcAddr"
  c'vkGetDeviceProcAddrSafe :: VkDevice -> CString -> IO (FunPtr a)

foreign import ccall unsafe "vkGetInstanceProcAddr"
  c'vkGetInstanceProcAddrUnsafe :: VkInstance -> CString -> IO (FunPtr a)

foreign import ccall unsafe "vkGetDeviceProcAddr"
  c'vkGetDeviceProcAddrUnsafe :: VkDevice -> CString -> IO (FunPtr a)


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
