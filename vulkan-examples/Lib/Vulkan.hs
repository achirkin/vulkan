{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Lib.Vulkan
    ( withVulkanInstance
    , pickPhysicalDevice
    , isDeviceSuitable
    , SwapChainSupportDetails (..), querySwapChainSupport
    , checkDeviceExtensionSupport
    ) where

import           Control.Exception
import           Control.Monad
import           Data.List                            ((\\))
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_surface
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create

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
                   -> (VkInstance -> IO a) -> IO a
withVulkanInstance progName extensions layers action = do
    vkInstance <-
      createVulkanInstance progName "My perfect Haskell engine"
                           extensions layers
    finally (action vkInstance) $ destroyVulkanInstance vkInstance

createVulkanInstance :: String -- ^ application name
                     -> String -- ^ engine name
                     -> [CString]
                        -- ^ required extensions
                        --   passed as a list of CStrings, because they are
                        --   available either via vulkan-api pattern synonyms,
                        --   or from GLFW
                     -> [String]
                        -- ^ required layer names
                     -> IO VkInstance
createVulkanInstance progName engineName extensions layers =
    withPtr iCreateInfo $ \iciPtr ->
      alloca $ \vkInstPtr -> do
        throwingVK "vkCreateInstance: Failed to create vkInstance."
          $ vkCreateInstance iciPtr VK_NULL vkInstPtr
        peek vkInstPtr
  where
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

destroyVulkanInstance :: VkInstance -> IO ()
destroyVulkanInstance vkInstance = vkDestroyInstance vkInstance VK_NULL

pickPhysicalDevice :: VkInstance
                   -> Maybe VkSurfaceKHR
                   -> IO (Maybe SwapChainSupportDetails, VkPhysicalDevice)
pickPhysicalDevice vkInstance mVkSurf = do
    devs <- asListVK
      $ \x ->
        throwingVK "pickPhysicalDevice: Failed to enumerate physical devices."
      . vkEnumeratePhysicalDevices vkInstance x

    when (null devs) $ throwVKMsg "Zero device count!"
    putStrLn $ "Found " ++ show (length devs) ++ " devices."

    selectFirstSuitable devs
  where
    selectFirstSuitable [] = throwVKMsg "No suitable devices!"
    selectFirstSuitable (x:xs) = do
      (mscsd, indeed) <- isDeviceSuitable mVkSurf x
      if indeed then pure (mscsd, x)
                else selectFirstSuitable xs

data SwapChainSupportDetails
  = SwapChainSupportDetails
  { capabilities :: VkSurfaceCapabilitiesKHR
  , formats      :: [VkSurfaceFormatKHR]
  , presentModes :: [VkPresentModeKHR]
  } deriving (Eq, Show)


querySwapChainSupport :: VkPhysicalDevice
                      -> VkSurfaceKHR
                      -> IO SwapChainSupportDetails
querySwapChainSupport pdev surf = do

  capabilities <- newVkData
    $ throwingVK "vkGetPhysicalDeviceSurfaceCapabilitiesKHR error"
    . vkGetPhysicalDeviceSurfaceCapabilitiesKHR pdev surf

  formats <- asListVK
    $ \x ->
      throwingVK "vkGetPhysicalDeviceSurfaceFormatsKHR error"
    . vkGetPhysicalDeviceSurfaceFormatsKHR pdev surf x

  presentModes <- asListVK
    $ \x ->
      throwingVK "vkGetPhysicalDeviceSurfacePresentModesKHR error"
    . vkGetPhysicalDeviceSurfacePresentModesKHR pdev surf x

  return SwapChainSupportDetails {..}


checkDeviceExtensionSupport :: VkPhysicalDevice
                            -> [CString]
                            -> IO Bool
checkDeviceExtensionSupport pdev extensions = do
  reqExts <- mapM peekCString extensions
  availExtsC <- asListVK
    $ \x ->
      throwingVK "vkEnumerateDeviceExtensionProperties error"
    . vkEnumerateDeviceExtensionProperties pdev VK_NULL_HANDLE x
  availExts <- mapM ( peekCString
                    . castPtr
                    . ( `plusPtr`
                          fieldOffset @"extensionName" @VkExtensionProperties
                      )
                    . unsafePtr) availExtsC
  return . null $ reqExts \\ availExts



isDeviceSuitable :: Maybe VkSurfaceKHR
                 -> VkPhysicalDevice
                 -> IO (Maybe SwapChainSupportDetails, Bool)
isDeviceSuitable mVkSurf pdev = do
  extsGood <- checkDeviceExtensionSupport pdev
    [VK_KHR_SWAPCHAIN_EXTENSION_NAME]

  (mscsd, surfGood) <- case mVkSurf of
    Nothing -> pure (Nothing, True)
    Just vkSurf
      | not extsGood -> pure (Nothing, False)
      | otherwise -> do
      scsd@SwapChainSupportDetails {..} <- querySwapChainSupport pdev vkSurf
      return  ( Just scsd
              ,    not (null formats)
                 && not (null presentModes)
              )

  pure (mscsd, extsGood && surfGood)
