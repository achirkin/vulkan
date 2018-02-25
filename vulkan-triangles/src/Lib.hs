{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Lib (runVulkanProgram) where

import           Control.Exception       (displayException)
import           Graphics.Vulkan

import           Lib.GLFW
import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Device
import           Lib.Vulkan.Drawing
import           Lib.Vulkan.Pipeline
import           Lib.Vulkan.Presentation
import           Lib.Vulkan.Shader
import           Lib.Vulkan.Shader.TH


windowWidth, windowHeight :: Num a => a
windowWidth = 800
windowHeight = 600

runVulkanProgram :: IO ()
runVulkanProgram = runProgram checkStatus $ do
  window <- initGLFWWindow windowWidth windowHeight "05-GraphicsPipeline-Window"

  vulkanInstance <- createGLFWVulkanInstance "05-GraphicsPipeline"

  vulkanSurface <- createSurface vulkanInstance window

  (Just scsd, pdev)
      <- pickPhysicalDevice vulkanInstance (Just vulkanSurface)
  (dev, queues) <- createGraphicsDevice pdev vulkanSurface
  swInfo <- createSwapChain dev scsd queues vulkanSurface
  imgViews <- createImageViews dev swInfo

  shaderVert
    <- createVkShaderStageCI dev
          $(compileGLSL "shaders/triangle.vert")
          VK_SHADER_STAGE_VERTEX_BIT

  shaderFrag
    <- createVkShaderStageCI dev
          $(compileGLSL "shaders/triangle.frag")
          VK_SHADER_STAGE_FRAGMENT_BIT

  renderPass <- createRenderPass dev swInfo
  graphicsPipeline
    <- createGraphicsPipeline dev swInfo [shaderVert, shaderFrag] renderPass

  framebuffers
    <- createFramebuffers dev renderPass swInfo imgViews

  commandPool <- createCommandPool dev queues
  cmdBuffers <- createCommandBuffers dev graphicsPipeline commandPool
                                     renderPass swInfo framebuffers

  rendFinS <- createSemaphore dev
  imAvailS <- createSemaphore dev

  imgIPtr <- mallocRes
  let rdata = RenderData
        { renderFinished = rendFinS
        , imageAvailable = imAvailS
        , device         = dev
        , swapChainInfo  = swInfo
        , deviceQueues   = queues
        , imgIndexPtr    = imgIPtr
        , commandBuffers = cmdBuffers
        }

  logInfo $ "Selected physical device: " ++ show pdev
  logInfo $ "Createad surface: " ++ show vulkanSurface
  logInfo $ "Createad device: " ++ show dev
  logInfo $ "Createad queues: " ++ show queues
  logInfo $ "Createad swapchain: " ++ show swInfo
  logInfo $ "Createad image views: " ++ show imgViews
  logInfo $ "Createad vertex shader module: " ++ show shaderVert
  logInfo $ "Createad fragment shader module: " ++ show shaderFrag
  logInfo $ "Createad renderpass: " ++ show renderPass
  logInfo $ "Createad pipeline: " ++ show graphicsPipeline
  logInfo $ "Createad framebuffers: " ++ show framebuffers
  logInfo $ "Createad command pool: " ++ show commandPool
  logInfo $ "Createad command buffers: " ++ show cmdBuffers

  glfwMainLoop window $ do
    return () -- do some app logic

    runVk $ vkQueueWaitIdle . presentQueue $ deviceQueues rdata

    drawFrame rdata

    runVk $ vkDeviceWaitIdle dev

checkStatus :: Either VulkanException () -> IO ()
checkStatus (Right ()) = pure ()
checkStatus (Left err) = putStrLn $ displayException err
