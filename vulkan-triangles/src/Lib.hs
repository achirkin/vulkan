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

import           Control.Concurrent.MVar
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
import           Lib.Vulkan.Image
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
    --              coordinate           color        texture coordinate
    scalar $ Vertex (vec2 (-0.5) (-0.5)) (vec3 1 0 0) (vec2 0 0)
  , scalar $ Vertex (vec2   0.4  (-0.5)) (vec3 0 1 0) (vec2 1 0)
  , scalar $ Vertex (vec2   0.4    0.4 ) (vec3 0 0 1) (vec2 1 1)
  , scalar $ Vertex (vec2 (-0.5)   0.4 ) (vec3 1 1 1) (vec2 0 1)
    -- triangle
  -- , scalar $ Vertex (vec2   0.9 (-0.4)) (vec3 0.2 0.5 0)
  -- , scalar $ Vertex (vec2   0.5 (-0.4)) (vec3 0 1 1)
  -- , scalar $ Vertex (vec2   0.7 (-0.8)) (vec3 1 0 0.4)
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

    texture <- createTextureImage pdev dev commandPool (graphicsQueue queues) "textures/texture.jpg"
    textureView <- createTextureImageView dev texture
    textureSampler <- createTextureSampler dev

    -- handling lifetime of swapchain manually. createSwapChain does not deallocate via continuation.
    swapchainResource <- liftIO $ newEmptyMVar

    let beforeSwapchainCreation = do
          -- wait as long as window has width=0 and height=0
          glfwWaitMinimized window
          -- If a window size change did happen, it will be respected by (re-)creating
          -- the swapchain below, no matter if it was signalled via exception or
          -- the IORef, so reset the IORef now:
          liftIO $ atomicWriteIORef windowSizeChanged False

    -- creating first swapchain before loop
    beforeSwapchainCreation
    scsd <- querySwapchainSupport pdev vulkanSurface
    swapInfoRef <- createSwapchain dev scsd queues vulkanSurface swapchainResource >>= liftIO . newIORef

    -- The code below re-runs when the swapchain was re-created and has a
    -- different number of images than before:
    _ <- flip finally (destroySwapchainIfNecessary dev swapchainResource)
      $ whileStatus DifferentLength $ do
      logInfo "Creating things that depend on the swapchain length.."
      swapInfo0 <- liftIO $ readIORef swapInfoRef
      let swapchainLen0 = length (swapImgs swapInfo0)

      (transObjMems, transObjBufs) <- unzip <$> createTransObjBuffers pdev dev swapchainLen0
      descriptorBufferInfos <- mapM transObjBufferInfo transObjBufs
      descriptorTextureInfo <- textureImageInfo textureView textureSampler

      descriptorPool <- createDescriptorPool dev swapchainLen0
      descriptorSetLayouts <- newArrayRes $ replicate swapchainLen0 descriptorSetLayout
      descriptorSets <- createDescriptorSets dev descriptorPool swapchainLen0 descriptorSetLayouts

      forM_ (zip descriptorBufferInfos descriptorSets) $
        \(bufInfo, dSet) -> prepareDescriptorSet dev bufInfo descriptorTextureInfo dSet

      transObjMemories <- newArrayRes $ transObjMems

      -- The code below re-runs when the swapchain was re-created and has the
      -- same number of images as before, or continues from enclosing scope.
      whileStatus SameLength $ do
        logInfo "Creating things that depend on the swapchain, but not its length.."
        swapInfo <- liftIO $ readIORef swapInfoRef
        let swapchainLen = length (swapImgs swapInfo)
        imgViews <- mapM (flip (createImageView dev) (swapImgFormat swapInfo)) (swapImgs swapInfo)

        renderPass <- createRenderPass dev swapInfo
        pipelineLayout <- createPipelineLayout dev descriptorSetLayout
        graphicsPipeline
          <- createGraphicsPipeline dev swapInfo
                                    vertIBD vertIADs
                                    [shaderVert, shaderFrag]
                                    renderPass
                                    pipelineLayout

        framebuffers
          <- createFramebuffers dev renderPass swapInfo imgViews

        cmdBuffersPtr <- createCommandBuffers dev graphicsPipeline commandPool
                                          renderPass pipelineLayout swapInfo
                                          vertexBuffer
                                          (dfLen indices, indexBuffer)
                                          framebuffers
                                          descriptorSets

        let rdata = RenderData
              { device             = dev
              , swapchainInfo      = swapInfo
              , deviceQueues       = queues
              , imgIndexPtr        = imgIPtr
              , currentFrame       = curFrameRef
              , renderFinishedSems = rendFinS
              , imageAvailableSems = imAvailS
              , inFlightFences     = inFlightF
              , commandBuffers     = cmdBuffersPtr
              , memories           = transObjMemories
              , memoryMutator      = updateTransObj dev (swapExtent swapInfo)
              }

        logInfo $ "Createad image views: " ++ show imgViews
        logInfo $ "Createad renderpass: " ++ show renderPass
        logInfo $ "Createad pipeline: " ++ show graphicsPipeline
        logInfo $ "Createad framebuffers: " ++ show framebuffers
        cmdBuffers <- peekArray swapchainLen cmdBuffersPtr
        logInfo $ "Createad command buffers: " ++ show cmdBuffers

        -- part of dumb fps counter
        frameCount <- liftIO $ newIORef @Int 0
        currentSec <- liftIO $ newIORef @Int 0

        status <- glfwMainLoop window $ do
          return () -- do some app logic

          -- Not needed anymore after waiting for inFlightFence has been implemented:
          -- runVk $ vkQueueWaitIdle . presentQueue $ deviceQueues rdata

          needRecreation <- drawFrame rdata `catchError` ( \err@(VulkanException ecode _) ->
            case ecode of
              Just VK_ERROR_OUT_OF_DATE_KHR -> do
                logInfo "Have got a VK_ERROR_OUT_OF_DATE_KHR error"
                return True
              _ -> throwError err
            )

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
          when sizeChanged $ logInfo "Have got a windowSizeCallback from GLFW"
          if needRecreation || sizeChanged then do
            beforeSwapchainCreation
            let oldSwapchainLen = length (swapImgs swapInfo)
            -- TODO this is effectively a wait for the queue to become idle as long as it's not decoupled from the loops
            --      -> need coroutines/threads that yield before the wait and deallocations
            runVk $ vkWaitForFences dev (fromIntegral _MAX_FRAMES_IN_FLIGHT) inFlightF VK_TRUE (maxBound :: Word64)
            logInfo "Recreating swapchain.."
            newScsd <- querySwapchainSupport pdev vulkanSurface
            newSwapInfo <- createSwapchain dev newScsd queues vulkanSurface swapchainResource
            liftIO $ writeIORef swapInfoRef newSwapInfo
            let newSwapchainLen = length (swapImgs swapInfo)
            return $ if oldSwapchainLen /= newSwapchainLen then DifferentLength else SameLength
            -- for testing, because different length is unlikely to happen:
            -- return DifferentLength
          else return OK
        when (status == Exit) $ runVk $ vkDeviceWaitIdle dev
        return status
    return ()

checkStatus :: Either VulkanException () -> IO ()
checkStatus (Right ()) = pure ()
checkStatus (Left err) = putStrLn $ displayException err
