{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Lib.Vulkan.Drawing
  ( RenderData (..)
  , createFramebuffers
  , createCommandPool
  , createCommandBuffers
  , createSemaphore
  , drawFrame
  ) where

import           Control.Monad                        (forM_)
import           Graphics.Vulkan
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create

import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Presentation


createFramebuffers :: VkDevice
                   -> VkRenderPass
                   -> SwapChainImgInfo
                   -> [VkImageView]
                   -> Program r [VkFramebuffer]
createFramebuffers dev renderPass SwapChainImgInfo{..} imgviews =
    allocResource
      (liftIO . mapM_  (\fb -> vkDestroyFramebuffer dev fb VK_NULL) )
      (mapM createFB imgviews)
  where
    createFB imgView =
      let fbci = createVk @VkFramebufferCreateInfo
            $  set @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" 0
            &* set @"renderPass" renderPass
            &* set @"attachmentCount" 1
            &* setListRef @"pAttachments" [imgView]
            &* set @"width" (getField @"width" swExtent)
            &* set @"height" (getField @"height" swExtent)
            &* set @"layers" 1
      in allocaPeek $ \fbPtr -> withVkPtr fbci $ \fbciPtr ->
          runVk $ vkCreateFramebuffer dev fbciPtr VK_NULL fbPtr


createCommandPool :: VkDevice -> DevQueues -> Program r VkCommandPool
createCommandPool dev DevQueues{..} =
  allocResource (liftIO . flip (vkDestroyCommandPool dev) VK_NULL) $
    allocaPeek $ \pPtr -> withVkPtr
      ( createVk
        $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" 0
        &* set @"queueFamilyIndex" graphicsFamIdx
      ) $ \ciPtr -> runVk $ vkCreateCommandPool dev ciPtr VK_NULL pPtr


createCommandBuffers :: VkDevice
                     -> VkPipeline
                     -> VkCommandPool
                     -> VkRenderPass
                     -> SwapChainImgInfo
                     -> [VkFramebuffer]
                     -> Program r [VkCommandBuffer]
createCommandBuffers
    dev pipeline commandPool rpass  SwapChainImgInfo{..} fbs
  | buffersCount <- length fbs = do
  -- allocate a pointer to an array of command buffer handles
  cbsPtr <- mallocArrayRes buffersCount

  allocResource
    ( \_ -> liftIO $
      vkFreeCommandBuffers dev commandPool (fromIntegral buffersCount) cbsPtr
    ) $ do
    let allocInfo = createVk @VkCommandBufferAllocateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"commandPool" commandPool
          &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
          &* set @"commandBufferCount" (fromIntegral buffersCount)

    withVkPtr allocInfo $ \aiPtr ->
      runVk $ vkAllocateCommandBuffers dev aiPtr cbsPtr
    commandBuffers <- peekArray buffersCount cbsPtr

    -- record command buffers
    forM_ (zip fbs commandBuffers) $ \(frameBuffer, cmdBuffer) -> do

      -- begin commands
      let cmdBufBeginInfo = createVk @VkCommandBufferBeginInfo
            $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
            &* set @"pNext" VK_NULL
            &* set @"flags" VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT

      withVkPtr cmdBufBeginInfo
        $ runVk . vkBeginCommandBuffer cmdBuffer

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

      withVkPtr renderPassBeginInfo $ \rpibPtr ->
        liftIO $ vkCmdBeginRenderPass cmdBuffer rpibPtr VK_SUBPASS_CONTENTS_INLINE

      -- basic drawing commands
      liftIO $ vkCmdBindPipeline cmdBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
      liftIO $ vkCmdDraw cmdBuffer 3 1 0 0

      -- finishing up
      liftIO $ vkCmdEndRenderPass cmdBuffer

      runVk $ vkEndCommandBuffer cmdBuffer

    return commandBuffers


createSemaphore :: VkDevice -> Program r VkSemaphore
createSemaphore dev =
  allocResource
    (liftIO .  flip (vkDestroySemaphore dev) VK_NULL)
    $ allocaPeek $ \sPtr -> withVkPtr
      ( createVk
        $  set @"sType" VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" 0
      ) $ \ciPtr -> runVk $ vkCreateSemaphore dev ciPtr VK_NULL sPtr



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


drawFrame :: RenderData -> Program r ()
drawFrame RenderData {..} = do
    commandBuffersPtr <- newArrayRes commandBuffers

    -- Acquiring an image from the swap chain
    runVk $ vkAcquireNextImageKHR
          device swapchain maxBound
          imageAvailable VK_NULL_HANDLE imgIndexPtr
    bufPtr <- (\i -> commandBuffersPtr `plusPtr`
                        (fromIntegral i * sizeOf (undefined :: VkCommandBuffer))
              ) <$> peek imgIndexPtr

    -- Submitting the command buffer
    withVkPtr (mkSubmitInfo bufPtr) $ \siPtr ->
      runVk $ vkQueueSubmit graphicsQueue 1 siPtr VK_NULL

    -- RENDERRR!!!
    withVkPtr presentInfo $
      runVk . vkQueuePresentKHR presentQueue
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
