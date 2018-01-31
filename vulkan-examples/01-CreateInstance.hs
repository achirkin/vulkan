{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Graphics.Vulkan
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
-- import Foreign.Marshal.Array
-- import Foreign.Marshal.Utils
import Foreign.C.String
-- import Data.Bits

main :: IO ()
main = withInstance $ \vulkanInstance ->
  destroyInstance vulkanInstance

-- | Allocate memory for VkInstance,
--   initialize it, and run an action with it.
withInstance :: (VkInstance -> IO ()) -> IO ()
withInstance action
  = alloca $ \vkInstPtr ->
    -- allocate some strings - names
    withCString "01-CreateInstance" $ \progNamePtr ->
    withCString "My Perfect Haskell Engine" $ \engineNamePtr ->

    fmap snd . allocaVkData @VkApplicationInfo @()
             $ \appInfoMut appInfoPtr -> do
      -- write VkApplicationInfo
      writeVkEngineVersion appInfoMut 0
      writeVkPEngineName appInfoMut engineNamePtr
      writeVkApplicationVersion appInfoMut 1
      writeVkPApplicationName appInfoMut progNamePtr
      writeVkPNext appInfoMut vkNullPtr
      writeVkSType appInfoMut VK_STRUCTURE_TYPE_APPLICATION_INFO
      writeVkApiVersion appInfoMut (_VK_MAKE_VERSION 1 0 68)

      fmap snd . allocaVkData @VkInstanceCreateInfo @()
               $ \iCreateInfoMut iCreateInfoPtr -> do
        -- write VkInstanceCreateInfo
        writeVkPApplicationInfo iCreateInfoMut appInfoPtr
        writeVkPpEnabledExtensionNames iCreateInfoMut vkNullPtr
        writeVkEnabledExtensionCount iCreateInfoMut 0
        writeVkPpEnabledLayerNames iCreateInfoMut vkNullPtr
        writeVkEnabledLayerCount iCreateInfoMut 0
        writeVkFlags iCreateInfoMut 0
        writeVkPNext iCreateInfoMut vkNullPtr
        writeVkSType appInfoMut VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO

        -- execute createInstance
        VkResult vkRes <- vkCreateInstance iCreateInfoPtr nullPtr vkInstPtr
        vkInst <- peek vkInstPtr
        print vkRes
        action vkInst


destroyInstance :: VkInstance -> IO ()
destroyInstance inst = vkDestroyInstance inst nullPtr
