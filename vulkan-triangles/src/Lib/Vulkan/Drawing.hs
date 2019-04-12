{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeApplications    #-}
module Lib.Vulkan.Drawing
  ( RenderData (..)
  , createFramebuffers
  , createCommandPool
  , createCommandBuffers
  , createSemaphores
  , createFences
  , drawFrame
  ) where

import           Control.Monad                            (forM_)
import           Data.IORef
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create
import           Graphics.Vulkan.Marshal.Create.DataFrame
import           Numeric.DataFrame

import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Presentation
import           Lib.Vulkan.Device

_MAX_FRAMES_IN_FLIGHT :: Int
_MAX_FRAMES_IN_FLIGHT = 2

createFramebuffers :: VkDevice
                   -> VkRenderPass
                   -> SwapchainInfo
                   -> [VkImageView]
                   -> Program r [VkFramebuffer]
createFramebuffers dev renderPass SwapchainInfo{..} imgviews =
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
            &* set @"width" (getField @"width" swapExtent)
            &* set @"height" (getField @"height" swapExtent)
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
                     -> VkPipelineLayout
                     -> SwapchainInfo
                     -> VkBuffer -- vertex data
                     -> (Word32, VkBuffer) -- nr of indices and index data
                     -> [VkFramebuffer]
                     -> [VkDescriptorSet]
                     -> Program r (Ptr VkCommandBuffer)
createCommandBuffers
    dev pipeline commandPool rpass pipelineLayout SwapchainInfo{..}
    vertexBuffer
    (nIndices, indexBuffer) fbs descriptorSets
  | buffersCount <- length fbs = do
  -- allocate a pointer to an array of command buffer handles
  cbsPtr <- mallocArrayRes buffersCount
  vertexBufArr <- newArrayRes [vertexBuffer]
  vertexOffArr <- newArrayRes [0]

  allocResource
    (const $ liftIO $ vkFreeCommandBuffers dev commandPool (fromIntegral buffersCount) cbsPtr)
    $ do
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
    forM_ (zip3 fbs descriptorSets commandBuffers) $
      \(frameBuffer, descriptorSet, cmdBuffer) -> do

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
                &* set @"extent" swapExtent
                )
            &* set @"clearValueCount" 1
            &* setVkRef @"pClearValues"
               ( createVk $ setVk @"color"
                  $ setVec @"float32" (vec4 0 0 0.2 1)
               )

      withVkPtr renderPassBeginInfo $ \rpibPtr ->
        liftIO $ vkCmdBeginRenderPass cmdBuffer rpibPtr VK_SUBPASS_CONTENTS_INLINE

      -- basic drawing commands
      liftIO $ vkCmdBindPipeline cmdBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
      liftIO $ vkCmdBindVertexBuffers
                 cmdBuffer 0 1 vertexBufArr vertexOffArr
      liftIO $ vkCmdBindIndexBuffer cmdBuffer indexBuffer 0 VK_INDEX_TYPE_UINT16
      dsPtr <- newArrayRes [descriptorSet]
      liftIO $ vkCmdBindDescriptorSets cmdBuffer VK_PIPELINE_BIND_POINT_GRAPHICS pipelineLayout 0 1 dsPtr 0 VK_NULL
      liftIO $ vkCmdDrawIndexed cmdBuffer nIndices 1 0 0 0

      -- finishing up
      liftIO $ vkCmdEndRenderPass cmdBuffer

      runVk $ vkEndCommandBuffer cmdBuffer

    return cbsPtr


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

createSemaphores :: VkDevice -> Program r (Ptr VkSemaphore)
createSemaphores dev = newArrayRes =<< (sequence $ replicate _MAX_FRAMES_IN_FLIGHT (createSemaphore dev))

createFence :: VkDevice -> Program r VkFence
createFence dev =
  allocResource
    (liftIO .  flip (vkDestroyFence dev) VK_NULL)
    $ allocaPeek $ \sPtr -> withVkPtr
      ( createVk
        $  set @"sType" VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_FENCE_CREATE_SIGNALED_BIT
      ) $ \ciPtr -> runVk $ vkCreateFence dev ciPtr VK_NULL sPtr

createFences :: VkDevice -> Program r (Ptr VkFence)
createFences dev = newArrayRes =<< (sequence $ replicate _MAX_FRAMES_IN_FLIGHT (createFence dev))

data RenderData
  = RenderData
  { device             :: VkDevice
  , swapchainInfo      :: SwapchainInfo
  , deviceQueues       :: DevQueues
  , imgIndexPtr        :: Ptr Word32
  , currentFrame       :: IORef Int
  , renderFinishedSems :: Ptr VkSemaphore
    -- ^ one per frame-in-flight
  , imageAvailableSems :: Ptr VkSemaphore
    -- ^ one per frame-in-flight
  , inFlightFences     :: Ptr VkFence
    -- ^ one per frame-in-flight
  , commandBuffers     :: Ptr VkCommandBuffer
    -- ^ one per swapchain image
  , memories           :: Ptr VkDeviceMemory
    -- ^ one per swapchain image
  , memoryMutator      :: forall r. VkDeviceMemory -> Program r ()
    -- ^ to execute on memories[*imgIndexPtr] before drawing
  }

drawFrame :: RenderData -> Program r Bool
drawFrame RenderData {..} = do
    frameIndex <- liftIO $ readIORef currentFrame
    let inFlightFencePtr = inFlightFences `ptrAtIndex` frameIndex
    runVk $ vkWaitForFences device 1 inFlightFencePtr VK_TRUE (maxBound :: Word64)

    let SwapchainInfo {..} = swapchainInfo
        DevQueues {..} = deviceQueues

    imageAvailable <- peek (imageAvailableSems `ptrAtIndex` frameIndex)
    renderFinished <- peek (renderFinishedSems `ptrAtIndex` frameIndex)
    inFlightFence <- peek inFlightFencePtr
    -- Acquiring an image from the swap chain
    runVk $ vkAcquireNextImageKHR
          device swapchain maxBound
          imageAvailable VK_NULL_HANDLE imgIndexPtr
    imgIndex <- fromIntegral <$> peek imgIndexPtr
    let bufPtr = commandBuffers `ptrAtIndex` imgIndex
    let memoryPtr = memories `ptrAtIndex` imgIndex
    mem <- peek memoryPtr
    memoryMutator mem

    -- Submitting the command buffer
    let submitInfo = createVk @VkSubmitInfo
          $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
          &* set @"pNext" VK_NULL
          &* set @"waitSemaphoreCount" 1
          &* setListRef @"pWaitSemaphores"   [imageAvailable]
          &* setListRef @"pWaitDstStageMask" [VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
          &* set @"commandBufferCount" 1
          &* set @"pCommandBuffers" bufPtr
          &* set @"signalSemaphoreCount" 1
          &* setListRef @"pSignalSemaphores" [renderFinished]

    runVk $ vkResetFences device 1 inFlightFencePtr
    withVkPtr submitInfo $ \siPtr ->
      runVk $ vkQueueSubmit graphicsQueue 1 siPtr inFlightFence

    -- Presentation
    let presentInfo = createVk @VkPresentInfoKHR
          $  set @"sType" VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
          &* set @"pNext" VK_NULL
          &* set @"pImageIndices" imgIndexPtr
          &* set        @"waitSemaphoreCount" 1
          &* setListRef @"pWaitSemaphores" [renderFinished]
          &* set        @"swapchainCount" 1
          &* setListRef @"pSwapchains"    [swapchain]

    withVkPtr presentInfo $
      runVk . vkQueuePresentKHR presentQueue
    isSuboptimal <- (== VK_SUBOPTIMAL_KHR) . currentStatus <$> get

    liftIO $ writeIORef currentFrame $ (frameIndex + 1) `mod` _MAX_FRAMES_IN_FLIGHT

    return isSuboptimal
