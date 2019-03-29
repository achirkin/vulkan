{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Lib (runVulkanProgram) where

import           Control.Exception                    (displayException)
import           Control.Monad                        (forM_)
import           Foreign.Marshal.Array                (pokeArray)
import           Foreign.Ptr                          (castPtr)
import           Foreign.Storable                     (poke)
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create
import           Numeric.DataFrame
import           Numeric.Dimensions
--import           Numeric.Matrix.Class (rotateZ) -- is not implemented yet
import           Data.Maybe                           (fromJust)

import           Lib.GLFW
import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Device
import           Lib.Vulkan.Drawing
import           Lib.Vulkan.Pipeline
import           Lib.Vulkan.Presentation
import           Lib.Vulkan.Shader
import           Lib.Vulkan.Shader.TH
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
vertices = fromJust $ fromList (D @3)
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
indices = fromJust $ fromList (D @3)
  [ -- rectangle
    0, 1, 2, 2, 3, 0
    -- triangle
  , 4, 5, 6
  ]

rotX :: Float -> Mat44f
rotX a = mat44 (vec4 1 0 0 0)
               (vec4 0 (cos a) (- sin a) 0)
               (vec4 0 (sin a) (cos a) 0)
               (vec4 0 0 0 1)

rotZ :: Float -> Mat44f
rotZ a = mat44 (vec4 (cos a) (- sin a) 0 0)
               (vec4 (sin a) (cos a) 0 0)
               (vec4 0 0 1 0)
               (vec4 0 0 0 1)

identM :: Mat44f
identM = mat44 (vec4 1 0 0 0)
               (vec4 0 1 0 0)
               (vec4 0 0 1 0)
               (vec4 0 0 0 1)

trans :: Float -> Mat44f
trans time = rotZ (time * pi / 2)

updateUB :: VkDevice -> VkDeviceMemory -> Float -> Program r ()
updateUB device uniBuf seconds = do
      uboPtr <- allocaPeek $
        runVk . vkMapMemory device uniBuf 0 (fromIntegral $ sizeOf (undefined :: Mat44f)) 0
      liftIO $ poke (castPtr uboPtr) (trans seconds)
      liftIO $ vkUnmapMemory device uniBuf

runVulkanProgram :: IO ()
runVulkanProgram = runProgram checkStatus $ do

    window <- initGLFWWindow 800 600 "vulkan-triangles-GLFW"

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

    rendFinS <- createSemaphore dev
    imAvailS <- createSemaphore dev
    commandPool <- createCommandPool dev queues
    logInfo $ "Createad command pool: " ++ show commandPool

    -- we need this later, but don't want to realloc every swapchain recreation.
    imgIPtr <- mallocRes

    vertexBuffer <-
      createVertexBuffer pdev dev commandPool (graphicsQueue queues) vertices

    indexBuffer <-
      createIndexBuffer pdev dev commandPool (graphicsQueue queues) indices

    descriptorSetLayout <- createDescriptorSetLayout dev

    -- The code below re-runs on every VK_ERROR_OUT_OF_DATE_KHR error
    --  (window resize event kind-of).
    redoOnOutdate $ do
      scsd <- querySwapChainSupport pdev vulkanSurface
      swInfo <- createSwapChain dev scsd queues vulkanSurface
      let swapChainLen = length (swImgs swInfo)
      imgViews <- createImageViews dev swInfo

      uniformBuffers <-
        createUniformBuffers pdev dev
          (fromIntegral $ sizeOf (undefined :: Mat44f)) swapChainLen

      descriptorPool <- createDescriptorPool dev swapChainLen
      descriptorSetLayouts <- newArrayRes $ replicate swapChainLen descriptorSetLayout
      descriptorSets <- createDescriptorSets dev descriptorPool swapChainLen descriptorSetLayouts

      forM_ (zip (map snd uniformBuffers) descriptorSets) . uncurry $
        \uniformBuffer descriptorSet ->
          let bufferInfo = createVk @VkDescriptorBufferInfo
                $  set @"buffer" uniformBuffer
                &* set @"offset" 0
                &* set @"range" (fromIntegral $ sizeOf (undefined :: Mat44f))
              descriptorWrite = createVk @VkWriteDescriptorSet
                $  set @"sType" VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
                &* set @"pNext" VK_NULL
                &* set @"dstSet" descriptorSet
                &* set @"dstBinding" 0
                &* set @"dstArrayElement" 0
                &* set @"descriptorType" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
                &* set @"descriptorCount" 1
                &* setVkRef @"pBufferInfo" bufferInfo
                &* set @"pImageInfo" VK_NULL
                &* set @"pTexelBufferView" VK_NULL
          in withVkPtr descriptorWrite $ \dwPtr ->
             liftIO $ vkUpdateDescriptorSets dev 1 dwPtr 0 VK_NULL

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
                                         (fromIntegral $ dimSize1 indices, indexBuffer)
                                         framebuffers
                                         descriptorSets

      uniformBuffersPtr <- newArrayRes $ map fst uniformBuffers

      let rdata = RenderData
            { renderFinished = rendFinS
            , imageAvailable = imAvailS
            , device         = dev
            , swapChainInfo  = swInfo
            , deviceQueues   = queues
            , imgIndexPtr    = imgIPtr
            , commandBuffers = cmdBuffersPtr
            , uniformBuffers = uniformBuffersPtr
            , updateUniformBuffer = updateUB
            }

      logInfo $ "Createad image views: " ++ show imgViews
      logInfo $ "Createad renderpass: " ++ show renderPass
      logInfo $ "Createad pipeline: " ++ show graphicsPipeline
      logInfo $ "Createad framebuffers: " ++ show framebuffers
      cmdBuffers <- peekArray swapChainLen cmdBuffersPtr
      logInfo $ "Createad command buffers: " ++ show cmdBuffers

      glfwMainLoop window $ do
        return () -- do some app logic

        runVk $ vkQueueWaitIdle . presentQueue $ deviceQueues rdata

        drawFrame rdata

        runVk $ vkDeviceWaitIdle dev

checkStatus :: Either VulkanException () -> IO ()
checkStatus (Right ()) = pure ()
checkStatus (Left err) = putStrLn $ displayException err

-- | Run the whole sequence of commands one more time if a particular error happens.
--   Run that sequence locally, so that all acquired resources are released
--   before running it again.
redoOnOutdate :: Program' a -> Program r a
redoOnOutdate action =
  locally action `catchError` ( \err@(VulkanException ecode _) ->
    case ecode of
      Just VK_ERROR_OUT_OF_DATE_KHR -> do
        logInfo "Have got a VK_ERROR_OUT_OF_DATE_KHR error, retrying..."
        redoOnOutdate action
      _ -> throwError err
  )
