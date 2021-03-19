{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Lib
  ( runVulkanProgram
  , WhichDemo (..)
  ) where

import Control.Monad                        (forM_, when, unless)
import Data.IORef
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Numeric.DataFrame
import System.Directory                     (doesFileExist)

import Lib.GLFW
import Lib.Program
import Lib.Program.Foreign
import Lib.Vulkan.Command
import Lib.Vulkan.Descriptor
import Lib.Vulkan.Device
import Lib.Vulkan.Drawing
import Lib.Vulkan.Image
import Lib.Vulkan.Pipeline
import Lib.Vulkan.Presentation
import Lib.Vulkan.Shader
import Lib.Vulkan.Shader.TH
import Lib.Vulkan.TransformationObject
import Lib.Vulkan.Vertex
import Lib.Vulkan.VertexBuffer


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
rectVertices :: DataFrame Vertex '[XN 3]
rectVertices = XFrame $
    square
    `appendDF`
    withPos (+ vec4 0 0 0.5 0) square
    `appendDF`
    withPos (\p -> p %* rotateX (pi/2) + vec4 0 0 (-0.5) 0) square
  where
    square :: Vector Vertex 4
    square = fromFlatList (D4 :* U) (Vertex 0 0 0) -- default point for type safety
      [  -- rectangle
          --     coordinate                  color        texture coordinate
        Vertex (vec3 (-0.5) (-0.5) 0) (vec3 1 0 0) (vec2 0 0)
      , Vertex (vec3   0.4  (-0.5) 0) (vec3 0 1 0) (vec2 1 0)
      , Vertex (vec3   0.4    0.4  0) (vec3 0 0 1) (vec2 1 1)
      , Vertex (vec3 (-0.5)   0.4  0) (vec3 1 1 1) (vec2 0 1)
      ]
    withPos :: (Vec4f -> Vec4f) -> Vector Vertex 4 -> Vector Vertex 4
    withPos f = ewmap (\(S v) -> S v { pos = fromHom . f . toHomPoint $ pos v })



rectIndices :: DataFrame Word32 '[XN 3]
rectIndices = atLeastThree $ fromList $
  oneRectIndices
  ++
  map (+4) oneRectIndices
  ++
  map (+8) oneRectIndices
  where
    -- indices for one rectangle
    oneRectIndices = [0, 3, 2, 2, 1, 0]


data WhichDemo = Squares | Chalet | VikingRoom

runVulkanProgram :: WhichDemo -> IO ()
runVulkanProgram demo = runProgram checkStatus $ do
  windowSizeChanged <- liftIO $ newIORef False
  window <- initGLFWWindow 800 600 "vulkan-triangles-GLFW" windowSizeChanged

  vulkanInstance <- createGLFWVulkanInstance "vulkan-triangles-instance"

  vulkanSurface <- createSurface vulkanInstance window
  logInfo $ "Createad surface: " ++ show vulkanSurface

  glfwWaitEventsMeanwhile $ do
    (_, pdev) <- pickPhysicalDevice vulkanInstance (Just vulkanSurface)
    logInfo $ "Selected physical device: " ++ show pdev
    msaaSamples <- getMaxUsableSampleCount pdev

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

    frameIndexRef <- liftIO $ newIORef 0
    renderFinishedSems <- createFrameSemaphores dev
    imageAvailableSems <- createFrameSemaphores dev
    inFlightFences <- createFrameFences dev

    commandPool <- createCommandPool dev queues
    logInfo $ "Createad command pool: " ++ show commandPool

    -- we need this later, but don't want to realloc every swapchain recreation.
    imgIndexPtr <- mallocRes

    let
      load basename = do
        let obj_path = "models/" ++ basename ++ ".obj"
            tex_path = "textures/" ++ basename ++ ".jpg/.png"
        modelExist <- liftIO . doesFileExist $ obj_path
        unless modelExist $
          throwVkMsg $ "Get " ++ obj_path ++ " and " ++ tex_path ++ " from the links in https://vulkan-tutorial.com/Loading_models"
        loadModel obj_path

    (vertices, indices) <- case demo of
      Squares -> return (rectVertices, rectIndices)
      Chalet -> load "chalet"
      VikingRoom -> load "viking_room"

    vertexBuffer <-
      createVertexBuffer pdev dev commandPool (graphicsQueue queues) vertices

    indexBuffer <-
      createIndexBuffer pdev dev commandPool (graphicsQueue queues) indices

    descriptorSetLayout <- createDescriptorSetLayout dev
    pipelineLayout <- createPipelineLayout dev descriptorSetLayout

    let texturePath = case demo of
          Squares -> "textures/texture.jpg"
          Chalet  -> "textures/chalet.jpg"
          VikingRoom  -> "textures/viking_room.png"
    (textureView, mipLevels) <- createTextureImageView pdev dev commandPool (graphicsQueue queues) texturePath
    textureSampler <- createTextureSampler dev mipLevels
    descriptorTextureInfo <- textureImageInfo textureView textureSampler

    depthFormat <- findDepthFormat pdev

    let beforeSwapchainCreation :: Program r ()
        beforeSwapchainCreation =
          -- wait as long as window has width=0 and height=0
          -- commented out because this only works in the main thread:
          -- glfwWaitMinimized window

          -- If a window size change did happen, it will be respected by (re-)creating
          -- the swapchain below, no matter if it was signalled via exception or
          -- the IORef, so reset the IORef now:
          liftIO $ atomicWriteIORef windowSizeChanged False

    -- The code below re-runs when the swapchain needs to be re-created
    loop $ do
      logInfo "Creating new swapchain.."
      scsd <- querySwapchainSupport pdev vulkanSurface
      beforeSwapchainCreation
      swapInfo <- createSwapchain dev scsd queues vulkanSurface
      let swapchainLen = length (swapImgs swapInfo)

      (transObjMems, transObjBufs) <- unzip <$> createTransObjBuffers pdev dev swapchainLen
      descriptorBufferInfos <- mapM transObjBufferInfo transObjBufs

      descriptorPool <- createDescriptorPool dev swapchainLen
      descriptorSetLayouts <- newArrayRes $ replicate swapchainLen descriptorSetLayout
      descriptorSets <- createDescriptorSets dev descriptorPool swapchainLen descriptorSetLayouts

      forM_ (zip descriptorBufferInfos descriptorSets) $
        \(bufInfo, dSet) -> prepareDescriptorSet dev bufInfo descriptorTextureInfo dSet

      transObjMemories <- newArrayRes transObjMems

      imgViews <- mapM (\image -> createImageView dev image (swapImgFormat swapInfo) VK_IMAGE_ASPECT_COLOR_BIT 1) (swapImgs swapInfo)
      renderPass <- createRenderPass dev swapInfo depthFormat msaaSamples
      graphicsPipeline
        <- createGraphicsPipeline dev swapInfo
                                  vertIBD vertIADs
                                  [shaderVert, shaderFrag]
                                  renderPass
                                  pipelineLayout
                                  msaaSamples

      colorAttImgView <- createColorAttImgView pdev dev commandPool (graphicsQueue queues)
                          (swapImgFormat swapInfo) (swapExtent swapInfo) msaaSamples
      depthAttImgView <- createDepthAttImgView pdev dev commandPool (graphicsQueue queues)
                          (swapExtent swapInfo) msaaSamples
      framebuffers
        <- createFramebuffers dev renderPass swapInfo imgViews depthAttImgView colorAttImgView
      cmdBuffersPtr <- createCommandBuffers dev graphicsPipeline commandPool
                                        renderPass pipelineLayout swapInfo
                                        vertexBuffer
                                        (dfLen indices, indexBuffer)
                                        framebuffers
                                        descriptorSets

      let rdata = RenderData
            { dev
            , swapInfo
            , queues
            , imgIndexPtr
            , frameIndexRef
            , renderFinishedSems
            , imageAvailableSems
            , inFlightFences
            , cmdBuffersPtr
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

      shouldExit <- glfwMainLoop window $ do
        -- do some app logic here

        needRecreation <- drawFrame rdata `catchError` ( \err@(VulkanException ecode _) ->
          case ecode of
            Just VK_ERROR_OUT_OF_DATE_KHR -> do
              logInfo "Have got a VK_ERROR_OUT_OF_DATE_KHR error"
              return True
            _ -> throwError err
          )

        -- part of dumb fps counter
        seconds <- getTime
        cur <- liftIO $ readIORef currentSec
        if floor seconds /= cur
        then do
          count <- liftIO $ readIORef frameCount
          when (cur /= 0) $ logInfo $
            "Running for " <> show cur <> "s; current fps: " <> show count
          liftIO $ do
            writeIORef currentSec (floor seconds)
            writeIORef frameCount 0
        else
          liftIO $ modifyIORef' frameCount succ

        sizeChanged <- liftIO $ readIORef windowSizeChanged
        when sizeChanged $ logInfo "Have got a windowSizeCallback from GLFW"
        return $ if needRecreation || sizeChanged then AbortLoop else ContinueLoop
      -- after glfwMainLoop exits, we need to wait for the frame to finish before deallocating things
      -- things are deallocated implicitly thanks to allocResource
      runVk $ vkDeviceWaitIdle dev
      return $ if shouldExit then AbortLoop else ContinueLoop
  return ()
