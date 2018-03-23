{- |

In this example, I follow vulkan-tutorial.com > Presentation

-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           Control.Exception
import           Control.Monad
import           Data.Bits
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromMaybe)
import           Data.Semigroup
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import qualified Graphics.UI.GLFW                     as GLFW
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_surface
import           Graphics.Vulkan.Ext.VK_KHR_swapchain

import           Lib.GLFW
import           Lib.Utils
import           Lib.Vulkan


main :: IO ()
main = withGLFWWindow 800 600 "03-Presentation-Window" $ \window ->
       withGLFWVulkanInstance "03-Presentation" $ \vulkanInstance ->
       withSurface vulkanInstance window $ \vulkanSurface -> do
        (Just scsd, pdev)
          <- pickPhysicalDevice vulkanInstance (Just vulkanSurface)
        withGraphicsDevice pdev vulkanSurface $ \dev queues ->
          withSwapChain dev scsd queues vulkanSurface $ \swInfo ->
          withImageViews dev swInfo $ \imgViews -> do
            putStrLn $ "Selected physical device: " ++ show pdev
            putStrLn $ "Createad surface: " ++ show vulkanSurface
            putStrLn $ "Createad device: " ++ show dev
            putStrLn $ "Createad queues: " ++ show queues
            putStrLn $ "Createad swapchain: " ++ show swInfo
            putStrLn $ "Createad image views: " ++ show imgViews
            glfwMainLoop window (return ())


withSurface :: VkInstance -> GLFW.Window -> (VkSurfaceKHR -> IO ()) -> IO ()
withSurface vkInstance window action = do
  surface <- alloca $ \surfPtr -> do
    throwingVK "glfwCreateWindowSurface: failed to create window surface"
      $ GLFW.createWindowSurface vkInstance window VK_NULL_HANDLE surfPtr
    peek surfPtr

  finally (action surface) $
    vkDestroySurfaceKHR vkInstance surface VK_NULL_HANDLE




getQueueFamilies :: VkPhysicalDevice -> IO [(Word32, VkQueueFamilyProperties)]
getQueueFamilies pdev = alloca $ \qFamCountPtr -> do
  vkGetPhysicalDeviceQueueFamilyProperties pdev qFamCountPtr VK_NULL_HANDLE
  aFamCount <- fromIntegral <$> peek qFamCountPtr
  when (aFamCount <= 0) $ throwVKMsg "Zero queue family count!"
  putStrLn $ "Found " ++ show aFamCount ++ " queue families."

  allocaArray aFamCount $ \familiesPtr -> do
    vkGetPhysicalDeviceQueueFamilyProperties pdev qFamCountPtr familiesPtr
    zip [0..] <$> peekArray aFamCount familiesPtr


-- | Throw an error otherwise
selectGraphicsFamily :: [(Word32, VkQueueFamilyProperties)]
                     -> IO (Word32, VkQueueFamilyProperties)
selectGraphicsFamily []
  = throwVKMsg "selectGraphicsFamily: not found!"
selectGraphicsFamily (x@(_,qfp):xs)
  = if  getField @"queueCount" qfp > 0
     && getField @"queueFlags" qfp .&. VK_QUEUE_GRAPHICS_BIT /= zeroBits
    then pure x
    else selectGraphicsFamily xs


-- | Throw an error otherwise
selectPresentationFamily :: VkPhysicalDevice
                         -> VkSurfaceKHR
                         -> [(Word32, VkQueueFamilyProperties)]
                         -> IO (Word32, VkQueueFamilyProperties)
selectPresentationFamily _ _ []
  = throw $ VulkanException Nothing "selectGraphicsFamily: not found!"
selectPresentationFamily dev surf (x@(i,qfp):xs)
  | getField @"queueCount" qfp <= 0 = selectGraphicsFamily xs
  | otherwise = do
    supported <- alloca $ \supportedPtr -> do
      throwingVK
        "vkGetPhysicalDeviceSurfaceSupportKHR: failed to check for presentation support."
        $ vkGetPhysicalDeviceSurfaceSupportKHR dev i surf supportedPtr
      peek supportedPtr
    if supported == VK_TRUE
    then pure x
    else selectPresentationFamily dev surf xs


data DevQueues
  = DevQueues
  { graphicsQueue :: VkQueue
  , presentQueue  :: VkQueue
  , qFamIndices   :: Ptr Word32
  } deriving (Eq, Show)


withGraphicsDevice :: VkPhysicalDevice
                   -> VkSurfaceKHR
                   -> (VkDevice -> DevQueues -> IO ()) -> IO ()
withGraphicsDevice pdev surf action
  | layers <- ["VK_LAYER_LUNARG_standard_validation"]
  , extensions <- [VK_KHR_SWAPCHAIN_EXTENSION_NAME]
  =
  alloca $ \queuePrioritiesPtr ->
  withArrayLen extensions $ \extCount extNames ->
  withCStringList layers $ \layerCount layerNames -> do

  -- check physical device extensions

  -- find an appropriate queue family
  qfams <- getQueueFamilies pdev
  (gFamIdx, _gFam) <- selectGraphicsFamily qfams
  (pFamIdx, _pFam) <- selectPresentationFamily pdev surf qfams
  let qFamIndices = Map.fromList [(gFamIdx, gFamIdx), (pFamIdx, pFamIdx)]
  famIndsPtr <- newArray (Map.elems qFamIndices)

  qcInfoMap <- forM qFamIndices $ \qFamIdx ->
               newVkData @VkDeviceQueueCreateInfo $ \qcInfoPtr -> do
    writeField @"sType"
      qcInfoPtr VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
    writeField @"pNext"
      qcInfoPtr VK_NULL_HANDLE
    writeField @"flags"
      qcInfoPtr 0
    writeField @"queueFamilyIndex"
      qcInfoPtr qFamIdx
    writeField @"queueCount"
      qcInfoPtr 1
    poke queuePrioritiesPtr 1.0
    writeField @"pQueuePriorities"
      qcInfoPtr queuePrioritiesPtr
  -- all qcInfos are copied, no need to touch qcInfoMap anymore
  qcInfosPts <- newArray $ Map.elems qcInfoMap

  pdevFeatures <- newVkData @VkPhysicalDeviceFeatures clearStorable

  devCreateInfo <- newVkData @VkDeviceCreateInfo
                          $ \devCreateInfoPtr -> do
    writeField @"sType"
      devCreateInfoPtr VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
    writeField @"pNext"
      devCreateInfoPtr VK_NULL_HANDLE
    writeField @"flags"
      devCreateInfoPtr 0
    writeField @"pQueueCreateInfos"
      devCreateInfoPtr qcInfosPts
    writeField @"queueCreateInfoCount"
      devCreateInfoPtr (fromIntegral $ Map.size qcInfoMap)
    writeField @"enabledLayerCount"
      devCreateInfoPtr (fromIntegral layerCount)
    writeField @"ppEnabledLayerNames"
      devCreateInfoPtr layerNames
    writeField @"enabledExtensionCount"
      devCreateInfoPtr (fromIntegral extCount)
    writeField @"ppEnabledExtensionNames"
      devCreateInfoPtr extNames
    writeField @"pEnabledFeatures"
      devCreateInfoPtr (unsafePtr pdevFeatures)

  -- try to create a device
  dev <- alloca $ \devPtr -> do
    throwingVK "vkCreateDevice: failed to create vkDevice"
      $ vkCreateDevice pdev (unsafePtr devCreateInfo)
                       VK_NULL_HANDLE devPtr
    peek devPtr

  -- get the queues
  gQueues <- flip Map.traverseWithKey qcInfoMap $ \qFamIdx _ ->
             alloca $ \quPtr -> do
     vkGetDeviceQueue dev qFamIdx 0 quPtr
     peek quPtr
  let mdevQueues = DevQueues
               <$> Map.lookup gFamIdx gQueues
               <*> Map.lookup pFamIdx gQueues
               <*> Just famIndsPtr

  finally ( maybe (throwVKMsg "Some queues lost!") pure mdevQueues
           >>= action dev
          ) $ do
    vkDestroyDevice dev VK_NULL_HANDLE
    touchVkData devCreateInfo
    touchVkData pdevFeatures
    free qcInfosPts
    free famIndsPtr


chooseSwapSurfaceFormat :: SwapChainSupportDetails -> IO VkSurfaceFormatKHR
chooseSwapSurfaceFormat SwapChainSupportDetails {..}
    = argVal . getMin
    . fromMaybe (throw $ VulkanException Nothing "No available surface formats!")
    . getOption
    . foldMap (Option . Just) <$> mapM fmtCost formats
  where
    argVal (Arg _ b) = b
    fmtCost :: VkSurfaceFormatKHR -> IO (ArgMin Int VkSurfaceFormatKHR)
    fmtCost f = case (getField @"format" f, getField @"colorSpace" f) of
      (VK_FORMAT_UNDEFINED, _) ->
        fmap (Min . Arg 0) $ newVkData $ \sfPtr -> do
          writeField @"format" sfPtr VK_FORMAT_B8G8R8A8_UNORM
          writeField @"colorSpace" sfPtr VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
      (VK_FORMAT_B8G8R8A8_UNORM, VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) ->
        pure . Min $ Arg 1 f
      (_, _) ->
        pure . Min $ Arg 2 f


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

chooseSwapExtent :: SwapChainSupportDetails -> IO VkExtent2D
chooseSwapExtent SwapChainSupportDetails {..}
    = newVkData @VkExtent2D $ \ePtr -> do
    writeField @"width" ePtr $ max (ew $ getField @"minImageExtent" capabilities)
                             $ min (ew $ getField @"maxImageExtent" capabilities)
                                   (ew $ getField @"currentExtent"  capabilities)
    writeField @"height" ePtr $ max (eh $ getField @"minImageExtent" capabilities)
                              $ min (eh $ getField @"maxImageExtent" capabilities)
                                    (eh $ getField @"currentExtent"  capabilities)
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

withSwapChain :: VkDevice
              -> SwapChainSupportDetails
              -> DevQueues
              -> VkSurfaceKHR
              -> (SwapChainImgInfo -> IO a)
              -> IO a
withSwapChain dev scsd queues surf action = do
  surfFmt <- chooseSwapSurfaceFormat scsd
  let spMode = chooseSwapPresentMode scsd
  sExtent <- chooseSwapExtent scsd
  putStrLn $ "Selected swap surface format: " ++ show surfFmt
  putStrLn $ "Selected swap present mode: " ++ show spMode
  putStrLn $ "Selected swap extent: " ++ show sExtent

  -- try tripple buffering
  let maxIC = getField @"maxImageCount" $ capabilities scsd
      minIC = getField @"minImageCount" $ capabilities scsd
      imageCount = if maxIC <= 0
                   then max minIC 3
                   else min maxIC $ max minIC 3

  -- write VkSwapchainCreateInfoKHR
  swCreateInfo <- newVkData @VkSwapchainCreateInfoKHR $ \swCreateInfoPtr -> do
    writeField @"sType"
      swCreateInfoPtr VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
    writeField @"pNext"
      swCreateInfoPtr VK_NULL_HANDLE
    writeField @"flags"
      swCreateInfoPtr 0
    writeField @"surface"
      swCreateInfoPtr surf
    writeField @"minImageCount"
      swCreateInfoPtr imageCount
    writeField @"imageFormat"
      swCreateInfoPtr (getField @"format" surfFmt)
    writeField @"imageColorSpace"
      swCreateInfoPtr (getField @"colorSpace" surfFmt)
    writeField @"imageExtent"
      swCreateInfoPtr sExtent
    writeField @"imageArrayLayers"
      swCreateInfoPtr 1
    writeField @"imageUsage"
      swCreateInfoPtr VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
    if graphicsQueue queues /= presentQueue queues
    then do
      writeField @"imageSharingMode"
        swCreateInfoPtr VK_SHARING_MODE_CONCURRENT
      writeField @"queueFamilyIndexCount"
        swCreateInfoPtr 2
      writeField @"pQueueFamilyIndices"
        swCreateInfoPtr (qFamIndices queues)
    else do
      writeField @"imageSharingMode"
        swCreateInfoPtr VK_SHARING_MODE_EXCLUSIVE
      writeField @"queueFamilyIndexCount"
        swCreateInfoPtr 0
      writeField @"pQueueFamilyIndices"
        swCreateInfoPtr VK_NULL_HANDLE
    writeField @"preTransform"
      swCreateInfoPtr (getField @"currentTransform" $ capabilities scsd)
    writeField @"compositeAlpha"
      swCreateInfoPtr VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
    writeField @"presentMode"
      swCreateInfoPtr spMode
    writeField @"clipped"
      swCreateInfoPtr VK_TRUE
    writeField @"oldSwapchain"
      swCreateInfoPtr VK_NULL_HANDLE

  swapChain <- alloca $ \swPtr -> do
    throwingVK "vkCreateSwapchainKHR failed!"
      $ vkCreateSwapchainKHR dev (unsafePtr swCreateInfo) VK_NULL_HANDLE swPtr
    peek swPtr

  swImgs <- asListVK
    $ \x ->
      throwingVK "vkGetSwapchainImagesKHR error"
    . vkGetSwapchainImagesKHR dev swapChain x

  let swInfo = SwapChainImgInfo
        { swapchain   = swapChain
        , swImgs      = swImgs
        , swImgFormat = getField @"format" surfFmt
        , swExtent    = sExtent
        }

  finally (action swInfo) $ do
    vkDestroySwapchainKHR dev swapChain VK_NULL_HANDLE
    touchVkData swCreateInfo


withImageViews :: VkDevice
               -> SwapChainImgInfo
               -> ([VkImageView] -> IO a)
               -> IO a
withImageViews dev SwapChainImgInfo {..} action = do
    cmapping <- newVkData $ \cmappingPtr -> do
      writeField @"r" cmappingPtr VK_COMPONENT_SWIZZLE_IDENTITY
      writeField @"g" cmappingPtr VK_COMPONENT_SWIZZLE_IDENTITY
      writeField @"b" cmappingPtr VK_COMPONENT_SWIZZLE_IDENTITY
      writeField @"a" cmappingPtr VK_COMPONENT_SWIZZLE_IDENTITY
    srrange <- newVkData $ \srrangePtr -> do
      writeField @"aspectMask"
        srrangePtr VK_IMAGE_ASPECT_COLOR_BIT
      writeField @"baseMipLevel"   srrangePtr 0
      writeField @"levelCount"     srrangePtr 1
      writeField @"baseArrayLayer" srrangePtr 0
      writeField @"layerCount"     srrangePtr 1
    imgvCreateInfos <- mapM (mkImageViewCreateInfo cmapping srrange) swImgs

    imgViews <- forM imgvCreateInfos
      $ \imgvCreateInfo ->
        alloca $ \imgViewPtr -> do
          throwingVK "vkCreateImageView error"
            $ vkCreateImageView dev (unsafePtr imgvCreateInfo)
                                VK_NULL_HANDLE imgViewPtr
          peek imgViewPtr

    finally (action imgViews) $ do
      mapM_ (flip (vkDestroyImageView dev) VK_NULL_HANDLE) imgViews
      mapM_ touchVkData imgvCreateInfos
  where
    mkImageViewCreateInfo cmapping srrange img
      = newVkData @VkImageViewCreateInfo $ \viewPtr -> do
      writeField @"sType"
        viewPtr VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
      writeField @"pNext"
        viewPtr VK_NULL_HANDLE
      writeField @"flags"
        viewPtr 0
      writeField @"image"
        viewPtr img
      writeField @"viewType"
        viewPtr VK_IMAGE_VIEW_TYPE_2D
      writeField @"format"
        viewPtr swImgFormat
      writeField @"components"
        viewPtr cmapping
      writeField @"subresourceRange"
        viewPtr srrange
