module Main (main) where

import Graphics.Vulkan
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.String

main :: IO ()
main = do
    res <- withVulkanInstance $ \vkInstance -> do
      putStrLn $ "Success! " ++ show vkInstance
      pure VK_SUCCESS
    print res

-- | Run an action with vulkan instance
withVulkanInstance :: (VkInstance -> IO VkResult) -> IO VkResult
withVulkanInstance action =
  -- allocate some strings - names
  withCString "01-CreateInstance" $ \progNamePtr ->
  withCString "My Perfect Haskell Engine" $ \engineNamePtr -> do

    -- write VkApplicationInfo
    appInfo <- newVkData $ \appInfoPtr -> do
      writeVkEngineVersion      appInfoPtr (_VK_MAKE_VERSION 1 0 0)
      writeVkPEngineName        appInfoPtr engineNamePtr
      writeVkApplicationVersion appInfoPtr (_VK_MAKE_VERSION 1 0 0)
      writeVkPApplicationName   appInfoPtr progNamePtr
      writeVkPNext              appInfoPtr VK_NULL_HANDLE
      writeVkSType              appInfoPtr VK_STRUCTURE_TYPE_APPLICATION_INFO
      writeVkApiVersion         appInfoPtr (_VK_MAKE_VERSION 1 0 68)

    -- write VkInstanceCreateInfo
    iCreateInfo <- newVkData $ \iCreateInfoPtr -> do
      writeVkPApplicationInfo        iCreateInfoPtr
        (unsafePtr appInfo)  -- must keep appInfo alive!
      writeVkPpEnabledExtensionNames iCreateInfoPtr VK_NULL_HANDLE
      writeVkEnabledExtensionCount   iCreateInfoPtr 0
      writeVkPpEnabledLayerNames     iCreateInfoPtr VK_NULL_HANDLE
      writeVkEnabledLayerCount       iCreateInfoPtr 0
      writeVkFlags                   iCreateInfoPtr 0
      writeVkPNext                   iCreateInfoPtr VK_NULL_HANDLE
      writeVkSType                   iCreateInfoPtr VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO

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
      r <- action vkInstance
      vkDestroyInstance vkInstance VK_NULL_HANDLE
      pure r
    else pure vkResult
