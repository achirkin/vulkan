{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Lib (runVulkanProgram) where

import           Control.Monad.IO.Class
import           Data.IORef
-- import           Foreign.Marshal.Alloc
import           Graphics.Vulkan

import           Lib.GLFW
import           Lib.Program
import           Lib.Utils
import           Lib.Utils.TH
import           Lib.Vulkan
import           Lib.Vulkan.Drawing
import           Lib.Vulkan.Pipeline
import           Lib.Vulkan.Presentation


windowWidth, windowHeight :: Num a => a
windowWidth = 800
windowHeight = 600

runVulkanProgram :: IO ()
runVulkanProgram = runProgram print $ do
  window <- initGLFWWindow windowWidth windowHeight "05-GraphicsPipeline-Window"

  vulkanInstance <- createGLFWVulkanInstance "05-GraphicsPipeline"

  withSurface vulkanInstance window $ \vulkanSurface -> do
    (Just scsd, pdev)
      <- pickPhysicalDevice vulkanInstance (Just vulkanSurface)
    withGraphicsDevice pdev vulkanSurface $ \dev queues ->
      withSwapChain dev scsd queues vulkanSurface $ \swInfo ->
      withImageViews dev swInfo $ \imgViews ->
      withVkShaderStageCI dev
          $(compileGLSL "shaders/triangle.vert")
          VK_SHADER_STAGE_VERTEX_BIT
          $ \shaderVert ->
      withVkShaderStageCI dev
          $(compileGLSL "shaders/triangle.frag")
          VK_SHADER_STAGE_FRAGMENT_BIT
          $ \shaderFrag ->
      withRenderPass dev swInfo $ \renderPass ->
      withGraphicsPipeline dev swInfo [shaderVert, shaderFrag] renderPass
          $ \graphicsPipeline ->
      withFramebuffers dev renderPass swInfo imgViews
          $ \framebuffers ->
      withCommandPool dev queues $ \commandPool ->
      withCommandBuffers dev graphicsPipeline commandPool
                         renderPass swInfo framebuffers
          $ \cmdBuffers ->
      withSemaphore dev $ \rendFinS ->
      withSemaphore dev $ \imAvailS ->
      alloca $ \imgIPtr -> do
        let rdata = RenderData
              { renderFinished = rendFinS
              , imageAvailable = imAvailS
              , device         = dev
              , swapChainInfo  = swInfo
              , deviceQueues   = queues
              , imgIndexPtr    = imgIPtr
              , commandBuffers = cmdBuffers
              }
        putStrLn $ "Selected physical device: " ++ show pdev
        putStrLn $ "Createad surface: " ++ show vulkanSurface
        putStrLn $ "Createad device: " ++ show dev
        putStrLn $ "Createad queues: " ++ show queues
        putStrLn $ "Createad swapchain: " ++ show swInfo
        putStrLn $ "Createad image views: " ++ show imgViews
        putStrLn $ "Createad vertex shader module: " ++ show shaderVert
        putStrLn $ "Createad fragment shader module: " ++ show shaderFrag
        putStrLn $ "Createad renderpass: " ++ show renderPass
        putStrLn $ "Createad pipeline: " ++ show graphicsPipeline
        putStrLn $ "Createad framebuffers: " ++ show framebuffers
        putStrLn $ "Createad command pool: " ++ show commandPool
        putStrLn $ "Createad command buffers: " ++ show cmdBuffers
        glfwMainLoop window $ do
          return () -- do some app logic
          throwingVK "vkQueueWaitIdle failed!"
            $ vkQueueWaitIdle . presentQueue $ deviceQueues rdata
          drawFrame rdata
        throwingVK "vkDeviceWaitIdle failed!"
          $ vkDeviceWaitIdle dev
