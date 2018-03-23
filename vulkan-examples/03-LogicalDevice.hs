{- |

In this example, I follow vulkan-tutorial.com > Setup part
to create logical device and a queue

Parts of the code are moved to Lib
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
import           Graphics.Vulkan.Core_1_0

import           Lib.GLFW
import           Lib.Utils
import           Lib.Vulkan               (pickPhysicalDevice)

main :: IO ()
main = withGLFWWindow 800 600 "03-LogicalDev-Window" $ \window ->
       withGLFWVulkanInstance "03-LogicalDevice" $ \vulkanInstance -> do
        (_, pdev) <- pickPhysicalDevice vulkanInstance Nothing
        withGraphicsDevice pdev $ \dev queue -> do
          putStrLn $ "Selected physical device: " ++ show pdev
          putStrLn $ "Createad device: " ++ show dev
          putStrLn $ "Createad queue: " ++ show queue
          glfwMainLoop window (return ())




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
                     -> (Word32, VkQueueFamilyProperties)
selectGraphicsFamily []
  = throw $ VulkanException Nothing "selectGraphicsFamily: not found!"
selectGraphicsFamily (x@(_,qfp):xs)
  = if  getField @"queueCount" qfp > 0
     && getField @"queueFlags" qfp .&. VK_QUEUE_GRAPHICS_BIT /= zeroBits
    then x
    else selectGraphicsFamily xs


withGraphicsDevice :: VkPhysicalDevice
                   -> (VkDevice -> VkQueue -> IO ()) -> IO ()
withGraphicsDevice pdev action
  | layers <- ["VK_LAYER_LUNARG_standard_validation"]
  =
  alloca $ \queuePrioritiesPtr ->
  withCStringList layers $ \layerCount layerNames -> do

  -- find an appropriate queue family
  (gFamIdx, _gFam) <- selectGraphicsFamily <$> getQueueFamilies pdev


  qcInfo <- newVkData @VkDeviceQueueCreateInfo
                      $ \qcInfoPtr -> do
    writeField @"sType"
      qcInfoPtr VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
    writeField @"pNext"
      qcInfoPtr VK_NULL_HANDLE
    writeField @"flags"
      qcInfoPtr 0
    writeField @"queueFamilyIndex"
      qcInfoPtr gFamIdx
    writeField @"queueCount"
      qcInfoPtr 1
    poke queuePrioritiesPtr 1.0
    writeField @"pQueuePriorities"
      qcInfoPtr queuePrioritiesPtr

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
      devCreateInfoPtr (unsafePtr qcInfo)
    writeField @"queueCreateInfoCount"
      devCreateInfoPtr 1
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

  -- get the first and the only requested queue
  gQueue <- alloca $ \quPtr -> do
     vkGetDeviceQueue dev gFamIdx 0 quPtr
     peek quPtr

  finally (action dev gQueue) $ do
    vkDestroyDevice dev VK_NULL_HANDLE
    touchVkData devCreateInfo
    touchVkData pdevFeatures
    touchVkData qcInfo
