{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
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
      writeField @"sType"              appInfoPtr VK_STRUCTURE_TYPE_APPLICATION_INFO
      writeField @"pNext"              appInfoPtr VK_NULL_HANDLE
      writeField @"pApplicationName"   appInfoPtr progNamePtr
      writeField @"applicationVersion" appInfoPtr (_VK_MAKE_VERSION 1 0 0)
      writeField @"pEngineName"        appInfoPtr engineNamePtr
      writeField @"engineVersion"      appInfoPtr (_VK_MAKE_VERSION 1 0 0)
      writeField @"apiVersion"         appInfoPtr (_VK_MAKE_VERSION 1 0 68)

    -- write VkInstanceCreateInfo
    iCreateInfo <- newVkData $ \iCreateInfoPtr -> do
      writeField @"sType"
        iCreateInfoPtr VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
      writeField @"pNext"
        iCreateInfoPtr VK_NULL_HANDLE
      writeField @"flags"
        iCreateInfoPtr 0
      writeField @"pApplicationInfo"
        iCreateInfoPtr (unsafePtr appInfo)  -- must keep appInfo alive!
      writeField @"enabledLayerCount"
        iCreateInfoPtr (fromIntegral layerCount)
      writeField @"ppEnabledLayerNames"
        iCreateInfoPtr layerNames
      writeField @"enabledExtensionCount"
        iCreateInfoPtr (fromIntegral extCount)
      writeField @"ppEnabledExtensionNames"
        iCreateInfoPtr extNames

    -- execute createInstance
    (vkResult, vkInstance) <- alloca $ \vkInstPtr -> do
      vkRes <- vkCreateInstance
        (unsafePtr iCreateInfo) -- must keep iCreateInfo alive!
        VK_NULL_HANDLE vkInstPtr
      vkI <- peek vkInstPtr
      return (vkRes, vkI)

    -- run a supplied action if instance creation was successful
    vkResult2 <-
      if vkResult == VK_SUCCESS
      then do
        r <- action vkInstance
        vkDestroyInstance vkInstance VK_NULL_HANDLE
        pure r
      else pure vkResult

    -- make sure our data structures exist until the end of the program.
    touchVkData appInfo
    touchVkData iCreateInfo
    return vkResult2
