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
import           Control.Monad           (forM_)
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Storable
import           Foreign.Ptr
import           Graphics.Vulkan
import           Graphics.Vulkan.Ext.VK_KHR_swapchain

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

    imgvPtrs <- mapM new imgviews
    bufs <- mapM createFB imgvPtrs

    finally (action $ map snd bufs) $ do
      forM_ bufs $ \(fbci, fb) -> do
        vkDestroyFramebuffer dev fb VK_NULL_HANDLE
        touchVkData fbci
      mapM_ free imgvPtrs
  where
    createFB imgViewPtr = do

      fbci <- newVkData @VkFramebufferCreateInfo $ \fbciPtr -> do
        writeField @"sType" fbciPtr VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
        writeField @"flags" fbciPtr 0
        writeField @"renderPass" fbciPtr renderPass
        writeField @"attachmentCount" fbciPtr 1
        writeField @"pAttachments" fbciPtr imgViewPtr
        writeField @"width" fbciPtr (fromIntegral $ getField @"width" swExtent)
        writeField @"height" fbciPtr (fromIntegral $ getField @"height" swExtent)
        writeField @"layers" fbciPtr 1

      alloca $ \fbPtr -> do
        throwingVK "vkCreateFramebuffer failed!"
          $ vkCreateFramebuffer
              dev (unsafePtr fbci) VK_NULL_HANDLE fbPtr
        (,) fbci <$> peek fbPtr


withCommandPool :: VkDevice -> DevQueues
                -> (VkCommandPool -> IO a)
                -> IO a
withCommandPool dev DevQueues{..} action = do

  poolInfo <- newVkData @VkCommandPoolCreateInfo $ \cpPtr -> do
    writeField @"sType"
      cpPtr VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
    writeField @"pNext"
      cpPtr VK_NULL_HANDLE
    writeField @"flags"
      cpPtr 0
    writeField @"queueFamilyIndex"
      cpPtr graphicsFamIdx

  commandPool <- alloca $ \pPtr -> do
    throwingVK "vkCreateCommandPool failed!"
      $ vkCreateCommandPool
          dev (unsafePtr poolInfo) VK_NULL_HANDLE pPtr
    peek pPtr

  finally (action commandPool) $ do
    vkDestroyCommandPool dev commandPool VK_NULL_HANDLE
    touchVkData poolInfo

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

    allocInfo <- newVkData @VkCommandBufferAllocateInfo $ \aiPtr -> do
      writeField @"sType"
        aiPtr VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
      writeField @"pNext"
        aiPtr VK_NULL_HANDLE
      writeField @"commandPool"
        aiPtr commandPool
      writeField @"level"
        aiPtr VK_COMMAND_BUFFER_LEVEL_PRIMARY
      writeField @"commandBufferCount"
        aiPtr (fromIntegral buffersCount)

    throwingVK "vkAllocateCommandBuffers failed!"
      $ vkAllocateCommandBuffers dev (unsafePtr allocInfo) cbsPtr
    commandBuffers <- peekArray buffersCount cbsPtr

    -- record command buffers
    forM_ (zip fbs commandBuffers) $ \(frameBuffer, cmdBuffer) -> do

      -- begin commands
      beginInfo <- newVkData @VkCommandBufferBeginInfo
                             $ \biPtr -> do
        writeField @"sType"
          biPtr VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
        writeField @"pNext"
          biPtr VK_NULL_HANDLE
        writeField @"flags"
          biPtr VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
        writeField @"pInheritanceInfo"
          biPtr VK_NULL_HANDLE -- optional

      throwingVK "vkBeginCommandBuffer failed!"
        $ vkBeginCommandBuffer cmdBuffer (unsafePtr beginInfo)

      touchVkData beginInfo

      -- render pass
      clearColor <- newVkData $ \ccPtr ->
        (>>= writeField @"color" ccPtr) . newVkData $ \cPtr -> do
          writeFieldArray @"float32" @0 cPtr 0
          writeFieldArray @"float32" @1 cPtr 0
          writeFieldArray @"float32" @2 cPtr 0
          writeFieldArray @"float32" @3 cPtr 1

      renderPassInfo <- newVkData @VkRenderPassBeginInfo $ \rpiPtr -> do
        writeField @"sType"
          rpiPtr VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
        writeField @"pNext"
          rpiPtr VK_NULL_HANDLE
        writeField @"renderPass"
          rpiPtr rpass
        writeField @"framebuffer"
          rpiPtr frameBuffer
        renderArea <- newVkData $ \raPtr -> do
          raOffset <- newVkData $ \raoPtr -> do
            writeField @"x" raoPtr 0
            writeField @"y" raoPtr 0
          writeField @"offset" raPtr raOffset
          writeField @"extent" raPtr swExtent
        writeField @"renderArea"
          rpiPtr renderArea
        writeField @"clearValueCount" rpiPtr 1
        writeField @"pClearValues"
          rpiPtr (unsafePtr clearColor)

      vkCmdBeginRenderPass
        cmdBuffer (unsafePtr renderPassInfo)
        VK_SUBPASS_CONTENTS_INLINE

      touchVkData clearColor
      touchVkData renderPassInfo

      -- basic drawing commands
      vkCmdBindPipeline cmdBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
      vkCmdDraw cmdBuffer 3 1 0 0

      -- finishing up
      vkCmdEndRenderPass cmdBuffer

      throwingVK "vkEndCommandBuffer failed!"
        $ vkEndCommandBuffer cmdBuffer


    finally (action commandBuffers) $ do
      vkFreeCommandBuffers dev commandPool (fromIntegral buffersCount) cbsPtr
      touchVkData allocInfo


withSemaphore :: VkDevice
              -> (VkSemaphore -> IO a)
              -> IO a
withSemaphore dev action = do

  semaphoreInfo <- newVkData @VkSemaphoreCreateInfo $ \sciPtr -> do
    writeField @"sType"
      sciPtr VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
    writeField @"pNext"
      sciPtr VK_NULL_HANDLE
    writeField @"flags"
      sciPtr 0

  semaphore <- alloca $ \sPtr -> do
    throwingVK "vkCreateSemaphore failed!"
      $ vkCreateSemaphore dev (unsafePtr semaphoreInfo) VK_NULL_HANDLE sPtr
    peek sPtr
  touchVkData semaphoreInfo

  finally (action semaphore) $
    vkDestroySemaphore dev semaphore VK_NULL_HANDLE


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
drawFrame RenderData {..}
  | SwapChainImgInfo {..} <- swapChainInfo
  , DevQueues {..} <- deviceQueues =
  withArrayLen [imageAvailable]
    $ \waitSemCount waitSemaphores ->
  withArrayLen [renderFinished]
    $ \signalSemCount signalSemaphores ->
  withArray commandBuffers
    $ \commandBuffersPtr ->
  withArrayLen [swapchain]
    $ \swapchainCount swapchainsPtr ->
  withArray [VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
    $ \waitStages -> do

  -- Acquiring an image from the swap chain
  throwingVK "vkAcquireNextImageKHR failed!"
    $ vkAcquireNextImageKHR
        device swapchain maxBound
        imageAvailable VK_NULL_HANDLE imgIndexPtr
  bufPtr <- (\i -> commandBuffersPtr `plusPtr`
                      (fromIntegral i * sizeOf (undefined :: VkCommandBuffer))
            ) <$> peek imgIndexPtr

  -- Submitting the command buffer
  submitInfo <- newVkData @VkSubmitInfo $ \siPtr -> do
    writeField @"sType" siPtr VK_STRUCTURE_TYPE_SUBMIT_INFO
    writeField @"pNext" siPtr VK_NULL_HANDLE
    writeField @"waitSemaphoreCount" siPtr $ fromIntegral waitSemCount
    writeField @"pWaitSemaphores" siPtr waitSemaphores
    writeField @"pWaitDstStageMask" siPtr waitStages
    writeField @"commandBufferCount" siPtr 1
    writeField @"pCommandBuffers" siPtr bufPtr
    writeField @"signalSemaphoreCount" siPtr $ fromIntegral signalSemCount
    writeField @"pSignalSemaphores" siPtr signalSemaphores

  throwingVK "vkQueueSubmit failed!"
    $ vkQueueSubmit graphicsQueue 1 (unsafePtr submitInfo) VK_NULL_HANDLE
  touchVkData submitInfo

  -- Presentation
  presentInfo <- newVkData @VkPresentInfoKHR $ \piPtr -> do
    writeField @"sType" piPtr VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
    writeField @"pNext" piPtr VK_NULL_HANDLE
    writeField @"waitSemaphoreCount" piPtr $ fromIntegral signalSemCount
    writeField @"pWaitSemaphores" piPtr signalSemaphores
    writeField @"swapchainCount" piPtr $ fromIntegral swapchainCount
    writeField @"pSwapchains" piPtr swapchainsPtr
    writeField @"pImageIndices" piPtr imgIndexPtr
    writeField @"pResults" piPtr VK_NULL_HANDLE

  -- RENDERRR!!!
  throwingVK "vkQueuePresentKHR failed!"
    $ vkQueuePresentKHR presentQueue (unsafePtr presentInfo)
  touchVkData presentInfo
