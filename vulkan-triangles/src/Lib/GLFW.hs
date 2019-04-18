{-# LANGUAGE Strict #-}
module Lib.GLFW
    ( createGLFWVulkanInstance
    , initGLFWWindow
    , glfwMainLoop
    , glfwWaitMinimized
    ) where

import           Control.Monad       (unless, when)
import           Data.IORef
import           Graphics.UI.GLFW    (ClientAPI (..), WindowHint (..))
import qualified Graphics.UI.GLFW    as GLFW
import           Graphics.Vulkan

import           Lib.Program
import           Lib.Vulkan.Instance


initGLFWWindow :: Int -- ^ Window width
               -> Int -- ^ Window height
               -> String -- ^ Window name
               -> IORef Bool -- ^ Window size change signalling
               -> Program r GLFW.Window
initGLFWWindow w h n windowSizeChanged = do

    -- even if something bad happens, we need to terminate GLFW
    allocResource
      (\() -> liftIO GLFW.terminate >> logInfo "Terminated GLFW.")
      ( liftIO GLFW.init >>= flip unless
          (throwVkMsg "Failed to initialize GLFW.")
      )

    liftIO GLFW.getVersionString >>= mapM_ (logInfo . ("GLFW version: " ++))

    liftIO GLFW.vulkanSupported >>= flip unless
      (throwVkMsg "GLFW reports that vulkan is not supported!")

    liftIO . GLFW.windowHint $ WindowHint'ClientAPI ClientAPI'NoAPI
    liftIO . GLFW.windowHint $ WindowHint'Resizable True

    allocResource
      ( \window -> do
          liftIO (GLFW.destroyWindow window)
          logDebug "Closed GLFW window."
      ) $ do
      mw <- liftIO $ GLFW.createWindow w h n Nothing Nothing
      case mw of
        Nothing -> throwVkMsg "Failed to initialize GLFW window."
        Just window  -> do
          logDebug "Initialized GLFW window."
          liftIO $ GLFW.setWindowSizeCallback window $
            Just (\_ _ _ -> atomicWriteIORef windowSizeChanged True)
          return window


-- | action returns False -> continue to loop,
--   action returns True -> stop loop and return True
--   window should close -> stop loop and return False
glfwMainLoop :: GLFW.Window -> Program' Status -> Program r Status
glfwMainLoop w action = go
  where
    go = do
      should <- liftIO $ GLFW.windowShouldClose w
      if not should then do
        status <- liftIO GLFW.pollEvents >> locally action
        if status /= OK
        then return status
        else go
      else return Exit


glfwWaitMinimized :: GLFW.Window -> Program r ()
glfwWaitMinimized win = liftIO go where
  go = do
    (x,y) <- GLFW.getFramebufferSize win
    GLFW.waitEvents
    when (x == 0 && y == 0) go

createGLFWVulkanInstance :: String -> Program r VkInstance
createGLFWVulkanInstance progName = do
    -- get required extension names from GLFW
    glfwReqExts <- liftIO GLFW.getRequiredInstanceExtensions
    createVulkanInstance
      progName
      "My perfect Haskell engine"
      glfwReqExts
      []
