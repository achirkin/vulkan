{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Lib (runVulkanProgram) where

import           Control.Exception                    (displayException)
import           Control.Monad                        (forM_, when)
import           Data.IORef
import           Data.Maybe                           (fromJust)
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Numeric.DataFrame
import           Numeric.Dimensions

import           Lib.GLFW
import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Descriptor
import           Lib.Vulkan.Device
import           Lib.Vulkan.Drawing
import           Lib.Vulkan.Pipeline
import           Lib.Vulkan.Presentation
import           Lib.Vulkan.Shader
import           Lib.Vulkan.Shader.TH
import           Lib.Vulkan.TransformationObject
import           Lib.Vulkan.Vertex
import           Lib.Vulkan.VertexBuffer


-- | Interleaved array of vertices containing at least 3 entries.
--
--   Obviously, in real world vertices come from a separate file and not known at compile time.
--   The shader pipeline requires at least 3 unique vertices (for a triangle)
--   to render something on a screen. Setting `XN 3` here is just a handy way
--   to statically ensure the program satisfies this requirement.
--   This way, not-enough-vertices error occures at the moment of DataFrame initialization
--   instead of silently failing to render something onto a screen.
--
--   Note: in this program, `n >= 3` requirement is also forced in `Lib/Vulkan/VertexBuffer.hs`,
--         where it is not strictly necessary but allows to avoid specifying DataFrame constraints
--         in function signatures (such as, e.g. `KnownDim n`).
vertices :: DataFrame Vertex '[XN 3]
vertices = fromJust . constrainDF @'[XN 3] @'[XN 0] $ fromList
  [ -- rectangle
    scalar $ Vertex (vec2 (-0.5) (-0.5)) (vec3 1 0 0)
  , scalar $ Vertex (vec2   0.4  (-0.5)) (vec3 0 1 0)
  , scalar $ Vertex (vec2   0.4    0.4 ) (vec3 0 0 1)
  , scalar $ Vertex (vec2 (-0.5)   0.4 ) (vec3 1 1 1)
    -- triangle
  , scalar $ Vertex (vec2   0.9 (-0.4)) (vec3 0.2 0.5 0)
  , scalar $ Vertex (vec2   0.5 (-0.4)) (vec3 0 1 1)
  , scalar $ Vertex (vec2   0.7 (-0.8)) (vec3 1 0 0.4)
  ]

indices :: DataFrame Word16 '[XN 3]
indices = fromJust . constrainDF @'[XN 3] @'[XN 0] $ fromList
  [ -- rectangle
    0, 1, 2, 2, 3, 0
    -- triangle
  , 4, 5, 6
  ]

-- | Get number of points in a vector
dfLen :: DataFrame t ((xns :: [XNat])) -> Word32
dfLen (XFrame (_ :: DataFrame t ns)) = case dims @ns of
  n :* _ -> fromIntegral $ dimVal n
  U      -> 1

data OnDemand = OnDemand
  { oldChainLen      :: Int
  , descriptorSets   :: [VkDescriptorSet]
  , transObjMemories :: Ptr VkDeviceMemory
  }

runVulkanProgram :: IO ()
runVulkanProgram = runProgram checkStatus $ do
    windowSizeChanged <- liftIO $ newIORef False
    window <- initGLFWWindow 800 600 "vulkan-triangles-GLFW" windowSizeChanged

    vulkanInstance <- createGLFWVulkanInstance "vulkan-triangles-instance"

    vulkanSurface <- createSurface vulkanInstance window
    logInfo $ "Createad surface: " ++ show vulkanSurface

    (_, pdev) <- pickPhysicalDevice vulkanInstance (Just vulkanSurface)
    logInfo $ "Selected physical device: " ++ show pdev

    (dev, queues) <- createGraphicsDevice pdev vulkanSurface
    logInfo $ "Createad device: " ++ show dev
    logInfo $ "Createad queues: " ++ show queues

    shaderVert
      <- createVkShaderStageCI dev
            $(compileGLSL "shaders/triangle.vert")
            VK_SHADER_STAGE_VERTEX_BIT

    shaderFrag
      <- createVkShaderStageCI dev
            $(compileGLSL "shaders/triangle.frag")
            VK_SHADER_STAGE_FRAGMENT_BIT

    logInfo $ "Createad vertex shader module: " ++ show shaderVert
    logInfo $ "Createad fragment shader module: " ++ show shaderFrag

    curFrameRef <- liftIO $ newIORef 0
    rendFinS <- createSemaphores dev
    imAvailS <- createSemaphores dev
    inFlightF <- createFences dev
    commandPool <- createCommandPool dev queues
    logInfo $ "Createad command pool: " ++ show commandPool

    -- we need this later, but don't want to realloc every swapchain recreation.
    imgIPtr <- mallocRes

    vertexBuffer <-
      createVertexBuffer pdev dev commandPool (graphicsQueue queues) vertices

    indexBuffer <-
      createIndexBuffer pdev dev commandPool (graphicsQueue queues) indices

    descriptorSetLayout <- createDescriptorSetLayout dev

    onDemand <- liftIO $ newIORef Nothing

    -- The code below re-runs on every VK_ERROR_OUT_OF_DATE_KHR error
    --  (window resize event kind-of).
    redoOnOutdate dev $ do
      -- wait as long as window has width=0 and height=0
      glfwWaitMinimized window
      -- If a window size change did happen, it will be respected by (re-)creating
      -- the swap chain below, no matter if it was signalled via exception or
      -- the IORef, so reset the IORef now:
      liftIO $ atomicWriteIORef windowSizeChanged False

      scsd <- querySwapChainSupport pdev vulkanSurface
      swInfo <- createSwapChain dev scsd queues vulkanSurface
      let swapChainLen = length (swImgs swInfo)
      imgViews <- createImageViews dev swInfo

      -- things that only need to be recreated when the swapchain length changes
      let redoOnDemand = do
            transObjBuffers <- createTransObjBuffers pdev dev swapChainLen
            descriptorBufferInfos <- mapM (transObjBufferInfo . snd) transObjBuffers

            descriptorPool <- createDescriptorPool dev swapChainLen
            descriptorSetLayouts <- newArrayRes $ replicate swapChainLen descriptorSetLayout
            descriptorSets <- createDescriptorSets dev descriptorPool swapChainLen descriptorSetLayouts

            forM_ (zip descriptorBufferInfos descriptorSets) . uncurry $
              prepareDescriptorSet dev

            transObjMemories <- newArrayRes $ map fst transObjBuffers

            let val = OnDemand { oldChainLen = swapChainLen, .. }
            liftIO $ writeIORef onDemand $ Just val
            return val

      OnDemand {..} <- liftIO (readIORef onDemand) >>= \case
        Nothing -> redoOnDemand
        Just OnDemand { oldChainLen }
          | oldChainLen /= swapChainLen -> do
              -- no idea if this can actually happen
              logInfo "Swap chain length changed, recreating length dependents"
              redoOnDemand
        Just x -> return x

      renderPass <- createRenderPass dev swInfo
      pipelineLayout <- createPipelineLayout dev descriptorSetLayout
      graphicsPipeline
        <- createGraphicsPipeline dev swInfo
                                  vertIBD vertIADs
                                  [shaderVert, shaderFrag]
                                  renderPass
                                  pipelineLayout

      framebuffers
        <- createFramebuffers dev renderPass swInfo imgViews

      cmdBuffersPtr <- createCommandBuffers dev graphicsPipeline commandPool
                                         renderPass pipelineLayout swInfo
                                         vertexBuffer
                                         (dfLen indices, indexBuffer)
                                         framebuffers
                                         descriptorSets

      let rdata = RenderData
            { device             = dev
            , swapChainInfo      = swInfo
            , deviceQueues       = queues
            , imgIndexPtr        = imgIPtr
            , currentFrame       = curFrameRef
            , renderFinishedSems = rendFinS
            , imageAvailableSems = imAvailS
            , inFlightFences     = inFlightF
            , commandBuffers     = cmdBuffersPtr
            , memories           = transObjMemories
            , memoryMutator      = updateTransObj dev (swExtent swInfo)
            }

      logInfo $ "Createad image views: " ++ show imgViews
      logInfo $ "Createad renderpass: " ++ show renderPass
      logInfo $ "Createad pipeline: " ++ show graphicsPipeline
      logInfo $ "Createad framebuffers: " ++ show framebuffers
      cmdBuffers <- peekArray swapChainLen cmdBuffersPtr
      logInfo $ "Createad command buffers: " ++ show cmdBuffers

      -- part of dumb fps counter
      frameCount <- liftIO $ newIORef @Int 0
      currentSec <- liftIO $ newIORef @Int 0

      glfwMainLoop window $ do
        return () -- do some app logic

        -- Not needed anymore after waiting for inFlightFence has been implemented:
        -- runVk $ vkQueueWaitIdle . presentQueue $ deviceQueues rdata

        isSuboptimal <- drawFrame rdata

        -- part of dumb fps counter
        seconds <- getTime
        liftIO $ do
          cur <- readIORef currentSec
          if floor seconds /= cur then do
            count <- readIORef frameCount
            when (cur /= 0) $ print count
            writeIORef currentSec (floor seconds)
            writeIORef frameCount 0
          else do
            modifyIORef frameCount $ \c -> c + 1

        sizeChanged <- liftIO $ readIORef windowSizeChanged
        return $ isSuboptimal || sizeChanged -- triggers redo


checkStatus :: Either VulkanException () -> IO ()
checkStatus (Right ()) = pure ()
checkStatus (Left err) = putStrLn $ displayException err

-- | Run the whole sequence of commands one more time if a particular error happens.
--   Run that sequence locally, so that all acquired resources are released
--   before running it again.
--   Ensures that vkDeviceWaitIdle happens before releasing resources, it is
--   needed for most release actions like vkDestroySwapchainKHR to succeed.
--   The action can return True if it wishes to be restarted without an exception.
redoOnOutdate :: VkDevice -> Program' Bool -> Program r ()
redoOnOutdate dev action = go >> return () where
  action' = finally action (runVk $ vkDeviceWaitIdle dev)
  go = do
    restart <- locally action' `catchError` ( \err@(VulkanException ecode _) ->
      case ecode of
        Just VK_ERROR_OUT_OF_DATE_KHR -> do
          logInfo "Have got a VK_ERROR_OUT_OF_DATE_KHR error, retrying..."
          return True
        _ -> throwError err
      )
    if restart then go else return False
