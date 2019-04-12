{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Lib.Vulkan.Device
    ( pickPhysicalDevice
    , isDeviceSuitable
    , DevQueues (..)
    , SwapchainSupportDetails (..)
    , querySwapchainSupport
    , checkDeviceExtensionSupport
    , createGraphicsDevice
    ) where

import           Control.Monad
import           Data.Bits
import           Data.List                            ((\\))
import qualified Data.Map                             as Map
import           Foreign.C.String                     (peekCString)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_surface
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create
import           Lib.Vulkan.Instance

import           Lib.Program
import           Lib.Program.Foreign

data SwapchainSupportDetails
  = SwapchainSupportDetails
  { capabilities :: VkSurfaceCapabilitiesKHR
  , formats      :: [VkSurfaceFormatKHR]
  , presentModes :: [VkPresentModeKHR]
  } deriving (Eq, Show)


pickPhysicalDevice :: VkInstance
                   -> Maybe VkSurfaceKHR
                   -> Program r (Maybe SwapchainSupportDetails, VkPhysicalDevice)
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


querySwapchainSupport :: VkPhysicalDevice
                      -> VkSurfaceKHR
                      -> Program r SwapchainSupportDetails
querySwapchainSupport pdev surf = do

  capabilities <- allocaPeekVk
    $ runVk . vkGetPhysicalDeviceSurfaceCapabilitiesKHR pdev surf

  formats <- asListVk
    $ \x -> runVk . vkGetPhysicalDeviceSurfaceFormatsKHR pdev surf x

  presentModes <- asListVk
    $ \x -> runVk . vkGetPhysicalDeviceSurfacePresentModesKHR pdev surf x

  return SwapchainSupportDetails {..}


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
                 -> Program r (Maybe SwapchainSupportDetails, Bool)
isDeviceSuitable mVkSurf pdev = do
  extsGood <- checkDeviceExtensionSupport pdev
    [VK_KHR_SWAPCHAIN_EXTENSION_NAME]

  (mscsd, surfGood) <- case mVkSurf of
    Nothing -> pure (Nothing, True)
    Just vkSurf
      | not extsGood -> pure (Nothing, False)
      | otherwise -> do
      scsd@SwapchainSupportDetails {..} <- querySwapchainSupport pdev vkSurf
      return  ( Just scsd
              ,    not (null formats)
                 && not (null presentModes)
              )

  supportedFeatures <- allocaPeek $
    liftIO . vkGetPhysicalDeviceFeatures pdev

  let supportsAnisotropy = getField @"samplerAnisotropy" supportedFeatures == VK_TRUE

  pure (mscsd, extsGood && surfGood && supportsAnisotropy)



getQueueFamilies :: VkPhysicalDevice -> Program r [(Word32, VkQueueFamilyProperties)]
getQueueFamilies pdev = do
  fams <- asListVk
    $ \c -> liftIO . vkGetPhysicalDeviceQueueFamilyProperties pdev c
  when (null fams) $ throwVkMsg "Zero queue family count!"
  return $ zip [0..] fams


-- | Throw an error otherwise
selectGraphicsFamily :: [(Word32, VkQueueFamilyProperties)]
                     -> Program r (Word32, VkQueueFamilyProperties)
selectGraphicsFamily []
  = throwVkMsg "selectGraphicsFamily: not found!"
selectGraphicsFamily (x@(_,qfp):xs)
  = if  getField @"queueCount" qfp > 0
     && getField @"queueFlags" qfp .&. VK_QUEUE_GRAPHICS_BIT /= zeroBits
    then pure x
    else selectGraphicsFamily xs


-- | Throw an error otherwise
selectPresentationFamily :: VkPhysicalDevice
                         -> VkSurfaceKHR
                         -> [(Word32, VkQueueFamilyProperties)]
                         -> Program r (Word32, VkQueueFamilyProperties)
selectPresentationFamily _ _ []
  = throwVkMsg "selectPresentationFamily: not found!"
selectPresentationFamily dev surf (x@(i,qfp):xs)
  | getField @"queueCount" qfp <= 0 = selectGraphicsFamily xs
  | otherwise = do
    supported <- allocaPeek $
      runVk . vkGetPhysicalDeviceSurfaceSupportKHR dev i surf
    if supported == VK_TRUE
    then pure x
    else selectPresentationFamily dev surf xs


data DevQueues
  = DevQueues
  { graphicsQueue  :: VkQueue
  , presentQueue   :: VkQueue
  , qFamIndices    :: Ptr Word32
  , graphicsFamIdx :: Word32
  , presentFamIdx  :: Word32
  } deriving (Eq, Show)


createGraphicsDevice :: VkPhysicalDevice
                     -> VkSurfaceKHR
                     -> Program r (VkDevice, DevQueues)
createGraphicsDevice pdev surf
  | layers <- defaultLayers
  , extensions <- [VK_KHR_SWAPCHAIN_EXTENSION_NAME] = do
  -- check physical device extensions

  -- find an appropriate queue family
  qfams <- getQueueFamilies pdev
  (gFamIdx, _gFam) <- selectGraphicsFamily qfams
  (pFamIdx, _pFam) <- selectPresentationFamily pdev surf qfams
  let qFamIndices = Map.fromList [(gFamIdx, gFamIdx), (pFamIdx, pFamIdx)]
  famIndsPtr <- newArrayRes $ Map.elems qFamIndices

  let qcInfoMap = flip fmap qFamIndices $ \qFamIdx ->
               createVk @VkDeviceQueueCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" 0
        &* set @"queueFamilyIndex" qFamIdx
        &* set @"queueCount" 1
        &* setListRef @"pQueuePriorities" [1.0]

      pdevFeatures = createVk @VkPhysicalDeviceFeatures
        $  set @"samplerAnisotropy" VK_TRUE

      devCreateInfo = createVk @VkDeviceCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" 0
        &* setListRef @"pQueueCreateInfos" (Map.elems qcInfoMap)
        &* set @"queueCreateInfoCount" (fromIntegral $ Map.size qcInfoMap)
        &* set @"enabledLayerCount" (fromIntegral $ length layers)
        &* setStrListRef @"ppEnabledLayerNames" layers
        &* set @"enabledExtensionCount" (fromIntegral $ length extensions)
        &* setListRef @"ppEnabledExtensionNames" extensions
        &* setVkRef @"pEnabledFeatures" pdevFeatures

  -- try to create a device
  dev <- allocResource (\dev -> liftIO $ vkDestroyDevice dev VK_NULL) $
    withVkPtr devCreateInfo $ \dciPtr ->
      allocaPeek $ runVk . vkCreateDevice pdev dciPtr VK_NULL

  -- get the queues
  gQueues <- flip Map.traverseWithKey qcInfoMap $ \qFamIdx _ ->
             allocaPeek $ liftIO . vkGetDeviceQueue dev qFamIdx 0

  mdevQueues <- maybe (throwVkMsg "Some queues lost!") pure
                $ DevQueues
               <$> Map.lookup gFamIdx gQueues
               <*> Map.lookup pFamIdx gQueues
               <*> Just famIndsPtr
               <*> Just gFamIdx
               <*> Just pFamIdx

  return (dev, mdevQueues)
