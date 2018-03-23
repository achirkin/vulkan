{- |

In this example, I follow vulkan-tutorial.com > Drawing

Finally, it draws that damn triangle!

-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           Control.Exception
import           Control.Monad                        (forM_)
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create

import           Lib.GLFW
import           Lib.Utils
import           Lib.Utils.TH
import           Lib.Vulkan
import           Lib.Vulkan.Pipeline
import           Lib.Vulkan.Presentation

windowWidth, windowHeight :: Num a => a
windowWidth = 800
windowHeight = 600

main :: IO ()
main = withGLFWWindow windowWidth windowHeight "05-GraphicsPipeline-Window"
          $ \window ->
       withGLFWVulkanInstance "05-GraphicsPipeline" $ \vulkanInstance ->
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


withFramebuffers :: VkDevice
                 -> VkRenderPass
                 -> SwapChainImgInfo
                 -> [VkImageView]
                 -> ([VkFramebuffer] -> IO a)
                 -> IO a
withFramebuffers dev renderPass SwapChainImgInfo{..} imgviews action = do
    bufs <- mapM createFB imgviews
    finally (action bufs) $
      forM_ bufs $ \fb ->
        vkDestroyFramebuffer dev fb VK_NULL_HANDLE
  where
    createFB imgView =
      let fbci = createVk
            $  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" 0
            &* set @"renderPass" renderPass
            &* set @"attachmentCount" 1
            &* setListRef @"pAttachments" [imgView]
            &* set @"width" (getField @"width" swExtent)
            &* set @"height" (getField @"height" swExtent)
            &* set @"layers" 1
      in alloca $ \fbPtr -> withPtr fbci $ \fbciPtr -> do
        throwingVK "vkCreateFramebuffer failed!"
          $ vkCreateFramebuffer dev fbciPtr VK_NULL fbPtr
        peek fbPtr


withCommandPool :: VkDevice -> DevQueues
                -> (VkCommandPool -> IO a)
                -> IO a
withCommandPool dev DevQueues{..} action = do

  commandPool <- alloca $ \pPtr -> do
    withPtr
      ( createVk
        $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" 0
        &* set @"queueFamilyIndex" graphicsFamIdx
      ) $ \ciPtr ->
      throwingVK "vkCreateCommandPool failed!"
        $ vkCreateCommandPool dev ciPtr VK_NULL pPtr
    peek pPtr

  finally (action commandPool) $
    vkDestroyCommandPool dev commandPool VK_NULL

withCommandBuffers :: VkDevice
                   -> VkPipeline
                   -> VkCommandPool
                   -> VkRenderPass
                   -> SwapChainImgInfo
                   -> [VkFramebuffer]
                   -> ([VkCommandBuffer] -> IO a)
                   -> IO a
withCommandBuffers
    dev pipeline commandPool rpass  SwapChainImgInfo{..} fbs action
  | buffersCount <- length fbs =
  -- allocate a pointer to an array of command buffer handles
  allocaArray buffersCount $ \cbsPtr -> do

    let allocInfo = createVk @VkCommandBufferAllocateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"commandPool" commandPool
          &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
          &* set @"commandBufferCount" (fromIntegral buffersCount)

    withPtr allocInfo $ \aiPtr ->
      throwingVK "vkAllocateCommandBuffers failed!"
        $ vkAllocateCommandBuffers dev aiPtr cbsPtr
    commandBuffers <- peekArray buffersCount cbsPtr

    -- record command buffers
    forM_ (zip fbs commandBuffers) $ \(frameBuffer, cmdBuffer) -> do

      -- begin commands
      let cmdBufBeginInfo = createVk @VkCommandBufferBeginInfo
            $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT

      withPtr cmdBufBeginInfo
        $ throwingVK "vkBeginCommandBuffer failed!"
        . vkBeginCommandBuffer cmdBuffer

      -- render pass
      let renderPassBeginInfo = createVk @VkRenderPassBeginInfo
            $  set @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
            &* set @"pNext" VK_NULL
            &* set @"renderPass" rpass
            &* set @"framebuffer" frameBuffer
            &* setVk @"renderArea"
                (  setVk @"offset"
                   ( set @"x" 0 &* set @"y" 0 )
                &* set @"extent" swExtent
                )
            &* set @"clearValueCount" 1
            &* setVkRef @"pClearValues"
               ( createVk $ setVk @"color"
                  $  setAt @"float32" @0 0
                  &* setAt @"float32" @1 0
                  &* setAt @"float32" @2 0.2
                  &* setAt @"float32" @3 1
               )

      withPtr renderPassBeginInfo $ \rpibPtr ->
        vkCmdBeginRenderPass cmdBuffer rpibPtr VK_SUBPASS_CONTENTS_INLINE

      -- basic drawing commands
      vkCmdBindPipeline cmdBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
      vkCmdDraw cmdBuffer 3 1 0 0

      -- finishing up
      vkCmdEndRenderPass cmdBuffer

      throwingVK "vkEndCommandBuffer failed!"
        $ vkEndCommandBuffer cmdBuffer


    finally (action commandBuffers) $
      vkFreeCommandBuffers dev commandPool (fromIntegral buffersCount) cbsPtr


withSemaphore :: VkDevice
              -> (VkSemaphore -> IO a)
              -> IO a
withSemaphore dev action = do

  semaphore <- alloca $ \sPtr -> do
    withPtr
      ( createVk
        $  set @"sType" VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" 0
      ) $ \ciPtr -> throwingVK "vkCreateSemaphore failed!"
                      $ vkCreateSemaphore dev ciPtr VK_NULL sPtr
    peek sPtr

  finally (action semaphore) $
    vkDestroySemaphore dev semaphore VK_NULL


data RenderData
  = RenderData
  { renderFinished :: VkSemaphore
  , imageAvailable :: VkSemaphore
  , device         :: VkDevice
  , swapChainInfo  :: SwapChainImgInfo
  , deviceQueues   :: DevQueues
  , imgIndexPtr    :: Ptr Word32
  , commandBuffers :: [VkCommandBuffer]
  }


drawFrame :: RenderData -> IO ()
drawFrame RenderData {..} =
    withArray commandBuffers
      $ \commandBuffersPtr -> do

    -- Acquiring an image from the swap chain
    throwingVK "vkAcquireNextImageKHR failed!"
      $ vkAcquireNextImageKHR
          device swapchain maxBound
          imageAvailable VK_NULL_HANDLE imgIndexPtr
    bufPtr <- (\i -> commandBuffersPtr `plusPtr`
                        (fromIntegral i * sizeOf (undefined :: VkCommandBuffer))
              ) <$> peek imgIndexPtr

    -- Submitting the command buffer
    withPtr (mkSubmitInfo bufPtr) $ \siPtr ->
      throwingVK "vkQueueSubmit failed!"
        $ vkQueueSubmit graphicsQueue 1 siPtr VK_NULL


    -- RENDERRR!!!
    withPtr presentInfo $
      throwingVK "vkQueuePresentKHR failed!" . vkQueuePresentKHR presentQueue
  where
    SwapChainImgInfo {..} = swapChainInfo
    DevQueues {..} = deviceQueues
    -- Submitting the command buffer
    mkSubmitInfo bufPtr = createVk @VkSubmitInfo
      $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
      &* set @"pNext" VK_NULL
      &* set @"waitSemaphoreCount" 1
      &* setListRef @"pWaitSemaphores"   [imageAvailable]
      &* setListRef @"pWaitDstStageMask" [VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
      &* set @"commandBufferCount" 1
      &* set @"pCommandBuffers" bufPtr
      &* set @"signalSemaphoreCount" 1
      &* setListRef @"pSignalSemaphores" [renderFinished]
    -- Presentation
    presentInfo = createVk @VkPresentInfoKHR
      $  set @"sType" VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
      &* set @"pNext" VK_NULL
      &* set @"pImageIndices" imgIndexPtr
      &* set        @"waitSemaphoreCount" 1
      &* setListRef @"pWaitSemaphores" [renderFinished]
      &* set        @"swapchainCount" 1
      &* setListRef @"pSwapchains"    [swapchain]
