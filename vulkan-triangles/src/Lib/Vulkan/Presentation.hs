{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
module Lib.Vulkan.Presentation
  ( SwapChainImgInfo (..)
  , DevQueues (..)
  , createSurface, createGraphicsDevice, createSwapChain, createImageViews
  ) where

import           Control.Monad
import           Data.Bits
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromMaybe)
import           Data.Semigroup
import qualified Foreign.Marshal.Alloc                as Foreign
import qualified Foreign.Marshal.Array                as Foreign
import qualified Graphics.UI.GLFW                     as GLFW
import           Graphics.Vulkan
import           Graphics.Vulkan.Ext.VK_KHR_surface
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create

import           Lib.Program
import           Lib.Vulkan.Device
import           Lib.Vulkan.Instance


createSurface :: VkInstance -> GLFW.Window -> Program r VkSurfaceKHR
createSurface vkInstance window =
  allocResource (\s -> liftIO $ vkDestroySurfaceKHR vkInstance s VK_NULL) $
    allocaPeek $
      runVk . GLFW.createWindowSurface vkInstance window VK_NULL






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
  famIndsPtr <- allocResource (liftIO . Foreign.free) $
    liftIO $ Foreign.newArray (Map.elems qFamIndices)

  let qcInfoMap = flip fmap qFamIndices $ \qFamIdx ->
               createVk @VkDeviceQueueCreateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" 0
        &* set @"queueFamilyIndex" qFamIdx
        &* set @"queueCount" 1
        &* setListRef @"pQueuePriorities" [1.0]

      pdevFeatures = createVk
       (pure () :: CreateVkStruct VkPhysicalDeviceFeatures '[] ())

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



chooseSwapSurfaceFormat :: SwapChainSupportDetails
                        -> Program r VkSurfaceFormatKHR
chooseSwapSurfaceFormat SwapChainSupportDetails {..}
    = maybe (throwVkMsg "No available surface formats!")
            (pure . argVal . getMin)
    . getOption
    $ foldMap (Option . Just . Min . fmtCost) formats
  where
    argVal (Arg _ b) = b
    bestFmt :: VkSurfaceFormatKHR
    bestFmt = createVk @VkSurfaceFormatKHR
      $  set @"format" VK_FORMAT_B8G8R8A8_UNORM
      &* set @"colorSpace" VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
    fmtCost :: VkSurfaceFormatKHR -> Arg Int VkSurfaceFormatKHR
    fmtCost f = case (getField @"format" f, getField @"colorSpace" f) of
      (VK_FORMAT_UNDEFINED, _) -> Arg 0 bestFmt
      (VK_FORMAT_B8G8R8A8_UNORM, VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) -> Arg 1 f
      (_, _) -> Arg 2 f


chooseSwapPresentMode :: SwapChainSupportDetails -> VkPresentModeKHR
chooseSwapPresentMode SwapChainSupportDetails {..}
    = argVal . getMin
    . fromMaybe (Min $ Arg 0 VK_PRESENT_MODE_FIFO_KHR)
                -- VK_PRESENT_MODE_FIFO_KHR is guaranteed to be available
    . getOption
    $ foldMap (Option . Just . pmCost) presentModes
  where
    argVal (Arg _ b) = b
    pmCost :: VkPresentModeKHR -> ArgMin Int VkPresentModeKHR
    pmCost VK_PRESENT_MODE_MAILBOX_KHR = Min $ Arg 0 VK_PRESENT_MODE_MAILBOX_KHR
    pmCost VK_PRESENT_MODE_IMMEDIATE_KHR = Min $ Arg 1 VK_PRESENT_MODE_IMMEDIATE_KHR
    pmCost VK_PRESENT_MODE_FIFO_KHR = Min $ Arg 2 VK_PRESENT_MODE_FIFO_KHR
    pmCost pm = Min $ Arg 3 pm

chooseSwapExtent :: SwapChainSupportDetails -> VkExtent2D
chooseSwapExtent SwapChainSupportDetails {..}
    = createVk @VkExtent2D
    $  set @"width"
      ( max (ew $ getField @"minImageExtent" capabilities)
                $ min (ew $ getField @"maxImageExtent" capabilities)
                      (ew $ getField @"currentExtent"  capabilities)
      )
    &* set @"height"
      ( max (eh $ getField @"minImageExtent" capabilities)
                $ min (eh $ getField @"maxImageExtent" capabilities)
                      (eh $ getField @"currentExtent"  capabilities)
      )
  where
    ew = getField @"width"
    eh = getField @"height"


data SwapChainImgInfo
  = SwapChainImgInfo
  { swapchain   :: VkSwapchainKHR
  , swImgs      :: [VkImage]
  , swImgFormat :: VkFormat
  , swExtent    :: VkExtent2D
  } deriving (Eq, Show)

createSwapChain :: VkDevice
                -> SwapChainSupportDetails
                -> DevQueues
                -> VkSurfaceKHR
                -> Program r SwapChainImgInfo
createSwapChain dev scsd queues surf = do
  surfFmt <- chooseSwapSurfaceFormat scsd
  let spMode = chooseSwapPresentMode scsd
      sExtent = chooseSwapExtent scsd

  -- try tripple buffering
  let maxIC = getField @"maxImageCount" $ capabilities scsd
      minIC = getField @"minImageCount" $ capabilities scsd
      imageCount = if maxIC <= 0
                   then max minIC 3
                   else min maxIC $ max minIC 3

  -- write VkSwapchainCreateInfoKHR
  let swCreateInfo = createVk @VkSwapchainCreateInfoKHR
        $  set @"sType" VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
        &* set @"pNext" VK_NULL_HANDLE
        &* set @"flags" 0
        &* set @"surface" surf
        &* set @"minImageCount" imageCount
        &* set @"imageFormat" (getField @"format" surfFmt)
        &* set @"imageColorSpace" (getField @"colorSpace" surfFmt)
        &* set @"imageExtent" sExtent
        &* set @"imageArrayLayers" 1
        &* set @"imageUsage" VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
        &*
        ( if graphicsQueue queues /= presentQueue queues
          then set @"imageSharingMode" VK_SHARING_MODE_CONCURRENT
            &* set @"queueFamilyIndexCount" 2
            &* set @"pQueueFamilyIndices" (qFamIndices queues)
          else set @"imageSharingMode" VK_SHARING_MODE_EXCLUSIVE
            &* set @"queueFamilyIndexCount" 0
            &* set @"pQueueFamilyIndices" VK_NULL_HANDLE
        )
        &* set @"preTransform" (getField @"currentTransform" $ capabilities scsd)
        &* set @"compositeAlpha" VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
        &* set @"presentMode" spMode
        &* set @"clipped" VK_TRUE
        &* set @"oldSwapchain" VK_NULL_HANDLE

  swapChain <- allocResource
    (\sw -> liftIO $ vkDestroySwapchainKHR dev sw VK_NULL)
    $ withVkPtr swCreateInfo $ \swciPtr -> allocaPeek
      $ runVk . vkCreateSwapchainKHR dev swciPtr VK_NULL


  swImgs <- asListVk
    $ \x -> runVk . vkGetSwapchainImagesKHR dev swapChain x

  return SwapChainImgInfo
        { swapchain   = swapChain
        , swImgs      = swImgs
        , swImgFormat = getField @"format" surfFmt
        , swExtent    = sExtent
        }


createImageViews :: VkDevice
                 -> SwapChainImgInfo
                 -> Program r [VkImageView]
createImageViews dev SwapChainImgInfo {..} = do
    let cmapping = createVk
          $  set @"r" VK_COMPONENT_SWIZZLE_IDENTITY
          &* set @"g" VK_COMPONENT_SWIZZLE_IDENTITY
          &* set @"b" VK_COMPONENT_SWIZZLE_IDENTITY
          &* set @"a" VK_COMPONENT_SWIZZLE_IDENTITY
        srrange = createVk
          $  set @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT
          &* set @"baseMipLevel" 0
          &* set @"levelCount" 1
          &* set @"baseArrayLayer" 0
          &* set @"layerCount" 1
        imgvCreateInfos = map (mkImageViewCreateInfo cmapping srrange) swImgs


    allocResource (liftIO . mapM_ (flip (vkDestroyImageView dev) VK_NULL)) $
      forM imgvCreateInfos $ flip withVkPtr $ \imgvciPtr ->
         allocaPeek $ runVk . vkCreateImageView dev imgvciPtr VK_NULL
  where
    mkImageViewCreateInfo cmapping srrange img
      = createVk @VkImageViewCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
      &* set @"pNext" VK_NULL_HANDLE
      &* set @"flags" 0
      &* set @"image" img
      &* set @"viewType" VK_IMAGE_VIEW_TYPE_2D
      &* set @"format" swImgFormat
      &* set @"components" cmapping
      &* set @"subresourceRange" srrange
