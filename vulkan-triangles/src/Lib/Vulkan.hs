{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Lib.Vulkan
    ( createVulkanInstance
    , pickPhysicalDevice
    , isDeviceSuitable
    , SwapChainSupportDetails (..), querySwapChainSupport
    , checkDeviceExtensionSupport
    , defaultLayers
    ) where

import           Control.Monad
import           Data.List                            ((\\))
import           Foreign.C.String
import           Foreign.Ptr
import           Graphics.Vulkan
import           Graphics.Vulkan.Ext.VK_KHR_surface
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create

import           Lib.Program

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

pickPhysicalDevice :: VkInstance
                   -> Maybe VkSurfaceKHR
                   -> Program r (Maybe SwapChainSupportDetails, VkPhysicalDevice)
pickPhysicalDevice vkInstance mVkSurf = do
    devs <- asListVk
      $ \x -> runVk . vkEnumeratePhysicalDevices vkInstance x

    when (null devs) $ throwVkMsg "Zero device count!"
    logInfo $ "Found " ++ show (length devs) ++ " devices."

    selectFirstSuitable devs
  where
    selectFirstSuitable [] = throwVkMsg "No suitable devices!"
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
                      -> Program r SwapChainSupportDetails
querySwapChainSupport pdev surf = do

  capabilities <- allocaPeekVk
    $ runVk . vkGetPhysicalDeviceSurfaceCapabilitiesKHR pdev surf

  formats <- asListVk
    $ \x -> runVk . vkGetPhysicalDeviceSurfaceFormatsKHR pdev surf x

  presentModes <- asListVk
    $ \x -> runVk . vkGetPhysicalDeviceSurfacePresentModesKHR pdev surf x

  return SwapChainSupportDetails {..}


checkDeviceExtensionSupport :: VkPhysicalDevice
                            -> [CString]
                            -> Program r Bool
checkDeviceExtensionSupport pdev extensions = do
  reqExts <- liftIO $ mapM peekCString extensions
  availExtsC <- asListVk
    $ \x -> runVk . vkEnumerateDeviceExtensionProperties pdev VK_NULL_HANDLE x
  availExts <- forM availExtsC . flip withVkPtr $
    liftIO . peekCString . castPtr
           . ( `plusPtr` fieldOffset @"extensionName" @VkExtensionProperties)
  return . null $ reqExts \\ availExts



isDeviceSuitable :: Maybe VkSurfaceKHR
                 -> VkPhysicalDevice
                 -> Program r (Maybe SwapChainSupportDetails, Bool)
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


defaultLayers :: [String]
defaultLayers
  = ["VK_LAYER_LUNARG_standard_validation" | isDev]
