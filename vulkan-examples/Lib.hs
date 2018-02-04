module Lib
    ( withVulkanInstance
    ) where

import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.Vulkan


-- | Run an action with vulkan instance
withVulkanInstance :: String -- ^ program name
                   -> (Int, Ptr CString) -- ^ required extensions
                   -> (Int, Ptr CString) -- ^ required layers
                   -> (VkInstance -> IO VkResult) -> IO VkResult
withVulkanInstance
  progName (extCount, extNames) (layerCount, layerNames) action =
  -- allocate some strings - names
  withCString progName $ \progNamePtr ->
  withCString "My Perfect Haskell Engine" $ \engineNamePtr -> do

    -- write VkApplicationInfo
    appInfo <- newVkData $ \appInfoPtr -> do
      writeVkEngineVersion      appInfoPtr (_VK_MAKE_VERSION 1 0 0)
      writeVkPEngineName        appInfoPtr engineNamePtr
      writeVkApplicationVersion appInfoPtr (_VK_MAKE_VERSION 1 0 0)
      writeVkPApplicationName   appInfoPtr progNamePtr
      writeVkPNext              appInfoPtr vkNullPtr
      writeVkSType              appInfoPtr VK_STRUCTURE_TYPE_APPLICATION_INFO
      writeVkApiVersion         appInfoPtr (_VK_MAKE_VERSION 1 0 68)

    -- write VkInstanceCreateInfo
    iCreateInfo <- newVkData $ \iCreateInfoPtr -> do
      writeVkPApplicationInfo        iCreateInfoPtr
        (unsafePtr appInfo)  -- must keep appInfo alive!
      writeVkPpEnabledExtensionNames iCreateInfoPtr extNames
      writeVkEnabledExtensionCount   iCreateInfoPtr (fromIntegral extCount)
      writeVkPpEnabledLayerNames     iCreateInfoPtr layerNames
      writeVkEnabledLayerCount       iCreateInfoPtr (fromIntegral layerCount)
      writeVkFlags                   iCreateInfoPtr 0
      writeVkPNext                   iCreateInfoPtr vkNullPtr
      writeVkSType                   iCreateInfoPtr VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO

    -- execute createInstance
    (vkResult, vkInstance) <- alloca $ \vkInstPtr -> do
      vkRes <- vkCreateInstance
        (unsafePtr iCreateInfo) -- must keep iCreateInfo alive!
        nullPtr vkInstPtr
      vkI <- peek vkInstPtr
      return (vkRes, vkI)

    -- run a supplied action if instance creation was successful
    if vkResult == VK_SUCCESS
    then do
      r <- action vkInstance
      vkDestroyInstance vkInstance vkNullPtr
      pure r
    else pure vkResult
