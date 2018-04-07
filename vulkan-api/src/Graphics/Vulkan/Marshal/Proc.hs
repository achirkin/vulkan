{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
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
    -- * Re-export `Foreign.Ptr`
  , FunPtr, nullFunPtr
  ) where

import           Foreign.C.String              (CString)
import           Foreign.Ptr                   (FunPtr, nullFunPtr)
import           GHC.TypeLits                  (Symbol)
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


foreign import ccall unsafe "vkGetInstanceProcAddr"
  c'vkGetInstanceProcAddr :: VkInstance -> CString -> IO (FunPtr a)

foreign import ccall unsafe "vkGetDeviceProcAddr"
  c'vkGetDeviceProcAddr :: VkDevice -> CString -> IO (FunPtr a)
