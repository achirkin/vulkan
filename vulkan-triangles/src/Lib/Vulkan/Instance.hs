{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Lib.Vulkan.Instance
    ( createVulkanInstance
    , defaultLayers
    ) where

import           Foreign.C.String
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create

import           Lib.Program
import           Lib.Program.Foreign

-- | Run an action with vulkan instance
createVulkanInstance :: String -- ^ application name
                     -> String -- ^ engine name
                     -> [CString]
                        -- ^ required extensions
                        --   passed as a list of CStrings, because they are
                        --   available either via vulkan-api pattern synonyms,
                        --   or from GLFW
                     -> [String]
                        -- ^ required layer names
                     -> Program r VkInstance
createVulkanInstance progName engineName extensions layers' =
  allocResource destroyVulkanInstance $ do

    extStrings <- liftIO $ mapM peekCString extensions
    logDebug $ unlines
      $ "Enabling instance extensions: " : map ("  " ++) extStrings

    logDebug $ unlines
      $ "Enabling instance layers: " : map ("  " ++) layers

    withVkPtr iCreateInfo $ \iciPtr ->
      allocaPeek $ runVk . vkCreateInstance iciPtr VK_NULL
  where
    layers = layers' ++ defaultLayers
    appInfo = createVk @VkApplicationInfo
      $  set       @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO
      &* set       @"pNext" VK_NULL
      &* setStrRef @"pApplicationName" progName
      &* set       @"applicationVersion" (_VK_MAKE_VERSION 1 0 0)
      &* setStrRef @"pEngineName" engineName
      &* set       @"engineVersion" (_VK_MAKE_VERSION 1 0 0)
      &* set       @"apiVersion" (_VK_MAKE_VERSION 1 0 68)

    iCreateInfo = createVk @VkInstanceCreateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
      &* set           @"pNext" VK_NULL
      &* setVkRef      @"pApplicationInfo" appInfo
      &* set           @"enabledLayerCount" (fromIntegral $ length layers)
      &* setStrListRef @"ppEnabledLayerNames" layers
      &* set           @"enabledExtensionCount" (fromIntegral $ length extensions)
      &* setListRef    @"ppEnabledExtensionNames" extensions

destroyVulkanInstance :: VkInstance -> Program r ()
destroyVulkanInstance vkInstance
  = liftIO (vkDestroyInstance vkInstance VK_NULL)
  >> inDev (logDebug "Destroyed vkInstance.")


defaultLayers :: [String]
defaultLayers
  = ["VK_LAYER_LUNARG_standard_validation" | isDev]
