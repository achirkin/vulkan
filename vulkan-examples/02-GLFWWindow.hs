module Main (main) where

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Graphics.Vulkan
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.C.String
import           Graphics.UI.GLFW (WindowHint(..), ClientAPI (..))
import qualified Graphics.UI.GLFW as GLFW

-- import System.Mem (performGC)
-- import Control.Concurrent (threadDelay)

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
            putStrLn $ "Initialized a window: " ++ show w
            (vkRes, vulkanInstance) <- createInstance
            print vkRes
            edev <- pickPhysicalDevice vulkanInstance
            case edev of
              Left err -> putStrLn err
              Right dev ->
                mainLoop w

            destroyInstance vulkanInstance
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




createInstance :: IO (VkResult, VkInstance)
createInstance = alloca $ \vkInstPtr -> do

    -- allocate some strings - names
    progNamePtr <- newCString "01-CreateInstance"
    engineNamePtr <- newCString "My Perfect Haskell Engine"

    -- write VkApplicationInfo
    appInfoMut <- newVkData
    writeVkEngineVersion      appInfoMut (_VK_MAKE_VERSION 1 0 0)
    writeVkPEngineName        appInfoMut engineNamePtr
    writeVkApplicationVersion appInfoMut (_VK_MAKE_VERSION 1 0 0)
    writeVkPApplicationName   appInfoMut progNamePtr
    writeVkPNext              appInfoMut vkNullPtr
    writeVkSType              appInfoMut VK_STRUCTURE_TYPE_APPLICATION_INFO
    writeVkApiVersion         appInfoMut (_VK_MAKE_VERSION 1 0 68)

    -- get required extension names from GLFW
    (reqExtCount, reqExtsPtr) <- GLFW.getRequiredInstanceExtensions

    stdValidationLayer <- newCString "VK_LAYER_LUNARG_standard_validation"
    let vlList = [ stdValidationLayer
                 ]
    validationLayersPtr <- newArray vlList

    -- write VkInstanceCreateInfo
    iCreateInfoMut <- newVkData
    writeVkPApplicationInfo        iCreateInfoMut
      (unsafeRef appInfoMut)  -- must remember to keep appInfoMut alive!
    writeVkPpEnabledExtensionNames iCreateInfoMut reqExtsPtr
    writeVkEnabledExtensionCount   iCreateInfoMut (fromIntegral reqExtCount)
    writeVkPpEnabledLayerNames     iCreateInfoMut validationLayersPtr
    writeVkEnabledLayerCount       iCreateInfoMut (fromIntegral $ length vlList)
    writeVkFlags                   iCreateInfoMut 0
    writeVkPNext                   iCreateInfoMut vkNullPtr
    writeVkSType                   iCreateInfoMut VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO

    -- execute createInstance
    vkRes <- vkCreateInstance
        (unsafeRef iCreateInfoMut) -- must remember to keep appInfoMut alive!
        nullPtr vkInstPtr
    vkInst <- peek vkInstPtr

    -- add finalizers
    -- the first two are added just to check if they are working
    addMutableFinalizer appInfoMut $ putStrLn "Finalizing appInfoMut..."
    addMutableFinalizer iCreateInfoMut $ putStrLn "Finalizing iCreateInfoMut..."
    -- this finalizer is important, we need it to cleanup resources
    --   after VkInstance is no longer in use
    addVkDataFinalizer vkInst $ do
      -- managed objects are GCed on their own, we just need to make sure
      --  that they are alive while vkInst is alive
      touchMutable appInfoMut
      touchMutable iCreateInfoMut
      -- unmanaged objects must be released explicitly
      free progNamePtr
      free engineNamePtr
      free validationLayersPtr
      free stdValidationLayer
      putStrLn "Finalizing vkInstance..."

    -- .. and, finally, return.
    return (vkRes, vkInst)


destroyInstance :: VkInstance -> IO ()
destroyInstance inst = vkDestroyInstance inst nullPtr >> touchVkData inst


pickPhysicalDevice :: VkInstance -> IO (Either String VkPhysicalDevice)
pickPhysicalDevice vkInstance = runExceptT $ do
    devs <- ExceptT $ alloca $ \deviceCountPtr -> runExceptT $ do
      throwingVK "pickPhysicalDevice: Failed to enumerate physical devices"
                $ vkEnumeratePhysicalDevices vkInstance deviceCountPtr vkNullPtr
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
