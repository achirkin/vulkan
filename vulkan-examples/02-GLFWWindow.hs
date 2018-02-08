module Main (main) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import           Graphics.UI.GLFW           (ClientAPI (..), WindowHint (..))
import qualified Graphics.UI.GLFW           as GLFW
import           Graphics.Vulkan

import           Lib

main :: IO ()
main = do
    successfulInit <- GLFW.init
    if not successfulInit
    then error "Failed to init GLFW"
    else do
      GLFW.getVersionString >>= mapM_ (putStrLn . ("GLFW version: " ++))
      vkSupport <- GLFW.vulkanSupported
      if not vkSupport
      then putStrLn "GLFW says Vulkan is not supported!"
      else do
        GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
        GLFW.windowHint $ WindowHint'Resizable False
        mw <- GLFW.createWindow 800 600 "Vulkan window" Nothing Nothing
        case mw of
          Nothing -> putStrLn "Failed to initialize GLFW window"
          Just w  -> do
            vkRes <- withVulkanInstanceExt $ \vulkanInstance -> do
              edev <- pickPhysicalDevice vulkanInstance
              case edev of
                Left err -> putStrLn err
                Right dev -> do
                  print dev
                  mainLoop w
              return VK_SUCCESS
            print vkRes
      putStrLn "Done."
      GLFW.terminate
      putStrLn "Terminated GLFW."
  where
    mainLoop w = do
      shouldClose <- GLFW.windowShouldClose w
      if shouldClose
      then do
        putStrLn "Closing the window..."
        GLFW.destroyWindow w
      else do
        GLFW.pollEvents
        mainLoop w


withVulkanInstanceExt :: (VkInstance -> IO VkResult) -> IO VkResult
withVulkanInstanceExt action = do
    -- get required extension names from GLFW
    (reLength, reqExts) <- GLFW.getRequiredInstanceExtensions
    -- get required layers
    withCString "VK_LAYER_LUNARG_standard_validation" $ \stdValidationLayer ->
      let vlList = [ stdValidationLayer
                   ]
      in withArray vlList $ \validationLayersPtr ->
          withVulkanInstance "02-GLFWWindow"
                             (fromIntegral reLength, reqExts)
                             (length vlList, validationLayersPtr)
                             action



pickPhysicalDevice :: VkInstance -> IO (Either String VkPhysicalDevice)
pickPhysicalDevice vkInstance = runExceptT $ do
    devs <- ExceptT $ alloca $ \deviceCountPtr -> runExceptT $ do
      throwingVK "pickPhysicalDevice: Failed to enumerate physical devices"
                $ vkEnumeratePhysicalDevices vkInstance deviceCountPtr VK_NULL_HANDLE
      devCount <- lift $ fromIntegral <$> peek deviceCountPtr
      when (devCount <= 0) $ throwE "Zero device count!"
      devices <- ExceptT $ allocaArray devCount $ \devicesPtr -> runExceptT $ do
        throwingVK "pickPhysicalDevice: Failed to enumerate physical devices"
                $ vkEnumeratePhysicalDevices vkInstance deviceCountPtr devicesPtr
        lift $ peekArray devCount devicesPtr
      lift $ putStrLn $ "pickPhysicalDevice: Found " ++ show devCount ++ " devices."
      return devices

    selectFirstSuitable devs
  where
    selectFirstSuitable [] = throwE "No suitable devices!"
    selectFirstSuitable (x:xs) = lift (isDeviceSuitable x) >>= \yes ->
      if yes then pure x
             else selectFirstSuitable xs

isDeviceSuitable :: VkPhysicalDevice -> IO Bool
isDeviceSuitable _ = pure True




throwingVK :: String
           -> IO VkResult
           -> ExceptT String IO ()
throwingVK msg f = do
  vkRez <- lift f
  unless (vkRez == VK_SUCCESS || vkRez == VK_INCOMPLETE) $
    throwE $ msg ++ " " ++ show vkRez
