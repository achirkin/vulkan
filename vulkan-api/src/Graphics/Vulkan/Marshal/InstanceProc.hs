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
module Graphics.Vulkan.Marshal.InstanceProc
  ( VulkanInstanceProc (..), vkGetInstanceProc
    -- * Re-export `Foreign.Ptr`
  , FunPtr, nullFunPtr
  ) where

import           Foreign.C.String              (CString)
import           Foreign.Ptr                   (FunPtr, nullFunPtr)
import           GHC.TypeLits                  (Symbol)
import           Graphics.Vulkan.Types.Handles (VkInstance)


-- | Some of the vulkan functions defined in vulkan extensions are not
--   available at the program linking time.
--   These functions should be discovered at runtime.
--   Vulkan api provides a special function for this,
--     called @vkGetInstanceProcAddr@.
--   This class provides a simpler discovery mechanism based on that function.
--   For example, you can get @vkCreateDebugReportCallbackEXT@ function
--   as follows:
--
--   > vkGetInstanceProc @VkCreateDebugReportCallbackEXT vkInstance
class VulkanInstanceProc (proc :: Symbol) where
  -- | Haskell signature for the vulkan function
  type VkInstanceProcType proc
  -- | Name of the vulkan function
  vkInstanceProcSymbol :: CString
  -- | Convert C function pointer to an ordinary haskell function.
  unwrapVkInstanceProc :: FunPtr (VkInstanceProcType proc) -> VkInstanceProcType proc

-- | An alternative to @vkGetInstanceProcAddr@ with type inference
--   and protection against typos.
vkGetInstanceProc :: forall proc . VulkanInstanceProc proc
                  => VkInstance -> IO (VkInstanceProcType proc)
vkGetInstanceProc i
  = unwrapVkInstanceProc @proc
  <$> c'vkGetInstanceProcAddr i (vkInstanceProcSymbol @proc)
{-# INLINE vkGetInstanceProc #-}

foreign import ccall unsafe "vkGetInstanceProcAddr"
  c'vkGetInstanceProcAddr :: VkInstance -> CString -> IO (FunPtr a)
