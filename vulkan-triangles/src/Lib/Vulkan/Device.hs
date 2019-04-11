{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Lib.Vulkan.Device
    ( pickPhysicalDevice
    , isDeviceSuitable
    , SwapChainSupportDetails (..), querySwapChainSupport
    , checkDeviceExtensionSupport
    ) where

import           Control.Monad
import           Data.List                            ((\\))
import           Foreign.C.String
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_surface
import           Graphics.Vulkan.Ext.VK_KHR_swapchain

import           Lib.Program
import           Lib.Program.Foreign

data SwapChainSupportDetails
  = SwapChainSupportDetails
  { capabilities :: VkSurfaceCapabilitiesKHR
  , formats      :: [VkSurfaceFormatKHR]
  , presentModes :: [VkPresentModeKHR]
  } deriving (Eq, Show)


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
    liftIO . peekCString
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

  supportedFeatures <- allocaPeek $
    liftIO . vkGetPhysicalDeviceFeatures pdev

  let supportsAnisotropy = getField @"samplerAnisotropy" supportedFeatures == VK_TRUE

  pure (mscsd, extsGood && surfGood && supportsAnisotropy)
