{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Lib.Vulkan
    ( withVulkanInstance
    , pickPhysicalDevice
    , isDeviceSuitable
    ) where

import           Control.Exception
import           Control.Monad
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import           Graphics.Vulkan

import           Lib.Utils

-- | Run an action with vulkan instance
withVulkanInstance :: String -- ^ program name
                   -> [CString]
                      -- ^ required extensions
                      --   passed as a list of CStrings, because they are
                      --   available either via vulkan-api pattern synonyms,
                      --   or from GLFW
                   -> [String]
                      -- ^ required layer names
                   -> (VkInstance -> IO ()) -> IO ()
withVulkanInstance
  progName extensions layers action = do
  execRez <-
    -- allocate some strings - names
    withArrayLen extensions $ \extCount extNames ->
    withCStringList layers $ \layerCount layerNames ->
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
      if vkResult == VK_SUCCESS
      then do
          eerr <- try $ action vkInstance
          vkDestroyInstance vkInstance VK_NULL_HANDLE
          -- make sure our data structures exist until the end of the program.
          touchVkData appInfo
          touchVkData iCreateInfo
          pure eerr
      else pure $ Left
                $ SomeException
                $ VulkanException
                   (Just vkResult)
                   "vkCreateInstance: Failed to create vkInstance."

  case execRez of
    Right ()                 -> pure ()
    Left (SomeException err) -> throwIO err


pickPhysicalDevice :: VkInstance -> IO VkPhysicalDevice
pickPhysicalDevice vkInstance = do
    devs <- alloca $ \deviceCountPtr -> do
      throwingVK "pickPhysicalDevice: Failed to enumerate physical devices."
        $ vkEnumeratePhysicalDevices vkInstance deviceCountPtr VK_NULL_HANDLE
      devCount <- fromIntegral <$> peek deviceCountPtr
      when (devCount <= 0) $ throwVKMsg "Zero device count!"
      putStrLn $ "Found " ++ show devCount ++ " devices."

      allocaArray devCount $ \devicesPtr -> do
        throwingVK "pickPhysicalDevice: Failed to enumerate physical devices."
          $ vkEnumeratePhysicalDevices vkInstance deviceCountPtr devicesPtr
        peekArray devCount devicesPtr

    selectFirstSuitable devs
  where
    selectFirstSuitable [] = throwVKMsg "No suitable devices!"
    selectFirstSuitable (x:xs) = isDeviceSuitable x >>= \yes ->
      if yes then pure x
             else selectFirstSuitable xs


isDeviceSuitable :: VkPhysicalDevice -> IO Bool
isDeviceSuitable _ = pure True
