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
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import           Foreign.Ptr
import           Graphics.Vulkan
import           Graphics.Vulkan.Ext.VK_KHR_surface
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import Data.List ((\\))

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
