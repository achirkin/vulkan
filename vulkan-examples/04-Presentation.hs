{- |

In this example, I follow vulkan-tutorial.com > Presentation

-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           Control.Exception
import           Control.Monad
import           Data.Bits
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import           Graphics.Vulkan
import qualified Graphics.UI.GLFW      as GLFW
import           Graphics.Vulkan.Ext.VK_KHR_surface
import qualified Data.Map as Map

import           Lib.GLFW
import           Lib.Utils
import           Lib.Vulkan


main :: IO ()
main = withGLFWWindow 800 600 "03-LogicalDev-Window" $ \window ->
       withGLFWVulkanInstance "03-LogicalDevice" $ \vulkanInstance ->
       withSurface vulkanInstance window $ \vulkanSurface -> do
        pdev <- pickPhysicalDevice vulkanInstance
        withGraphicsDevice pdev vulkanSurface $ \dev queues -> do
          putStrLn $ "Selected physical device: " ++ show pdev
          putStrLn $ "Createad surface: " ++ show vulkanSurface
          putStrLn $ "Createad device: " ++ show dev
          putStrLn $ "Createad queues: " ++ show queues
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
  { _graphicsQueue :: VkQueue
  , _presentQueue  :: VkQueue
  } deriving (Eq, Show)


withGraphicsDevice :: VkPhysicalDevice
                   -> VkSurfaceKHR
                   -> (VkDevice -> DevQueues -> IO ()) -> IO ()
withGraphicsDevice pdev surf action
  | layers <- ["VK_LAYER_LUNARG_standard_validation"]
  =
  alloca $ \queuePrioritiesPtr ->
  withCStringList layers $ \layerCount layerNames -> do

  -- find an appropriate queue family
  qfams <- getQueueFamilies pdev
  (gFamIdx, _gFam) <- selectGraphicsFamily qfams
  (pFamIdx, _pFam) <- selectPresentationFamily pdev surf qfams
  let qFamIndices = Map.fromList [(gFamIdx, gFamIdx), (pFamIdx, pFamIdx)]


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
      devCreateInfoPtr 0
    writeField @"ppEnabledExtensionNames"
      devCreateInfoPtr VK_NULL_HANDLE
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

  finally ( maybe (throwVKMsg "Some queues lost!") pure mdevQueues
           >>= action dev
          ) $ do
    vkDestroyDevice dev VK_NULL_HANDLE
    touchVkData devCreateInfo
    touchVkData pdevFeatures
    free qcInfosPts
