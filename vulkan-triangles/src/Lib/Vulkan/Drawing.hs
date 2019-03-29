{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Vulkan.Drawing
  ( RenderData (..)
  , createFramebuffers
  , createCommandPool
  , createCommandBuffers
  , createDescriptorPool
  , createDescriptorSets
  , createSemaphore
  , drawFrame
  ) where

import           Control.Monad                            (forM_)
import           Data.Time.Clock.System
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create
import           Graphics.Vulkan.Marshal.Create.DataFrame
import           Numeric.DataFrame

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

createDescriptorPool :: VkDevice -> Int -> Program r VkDescriptorPool
createDescriptorPool dev n =
  allocResource (liftIO . flip (vkDestroyDescriptorPool dev) VK_NULL) $
    allocaPeek $ \pPtr -> withVkPtr
      ( createVk
        $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" 0
        &* set @"poolSizeCount" 1
        &* setVkRef @"pPoolSizes"
          ( createVk
          $  set @"type" VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
          &* set @"descriptorCount" (fromIntegral n)
          )
        &* set @"maxSets" (fromIntegral n)
      ) $ \ciPtr -> runVk $ vkCreateDescriptorPool dev ciPtr VK_NULL pPtr

createDescriptorSets :: VkDevice
                     -> VkDescriptorPool
                     -> Int
                     -> Ptr VkDescriptorSetLayout
                     -> Program r [VkDescriptorSet]
createDescriptorSets dev descriptorPool n layoutsPtr =
  let dsai = createVk @VkDescriptorSetAllocateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"descriptorPool" descriptorPool
        &* set @"descriptorSetCount" (fromIntegral n)
        &* set @"pSetLayouts" layoutsPtr
  in allocaArray n $ \dsPtr -> withVkPtr dsai $ \dsaiPtr -> do
      runVk $ vkAllocateDescriptorSets dev dsaiPtr dsPtr
      peekArray n dsPtr


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
                     -> SwapChainImgInfo
                     -> VkBuffer -- vertex data
                     -> (Word32, VkBuffer) -- nr of indices and index data
                     -> [VkFramebuffer]
                     -> [VkDescriptorSet]
                     -> Program r (Ptr VkCommandBuffer)
createCommandBuffers
    dev pipeline commandPool rpass pipelineLayout SwapChainImgInfo{..}
    vertexBuffer
    (nIndices, indexBuffer) fbs descriptorSets
  | buffersCount <- length fbs = do
  -- allocate a pointer to an array of command buffer handles
  cbsPtr <- mallocArrayRes buffersCount
  vertexBufArr <- newArrayRes [vertexBuffer]
  vertexOffArr <- newArrayRes [0]

  allocResource
    ( \_ -> do
      runVk $ vkDeviceWaitIdle dev
      liftIO $
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
                &* set @"extent" swExtent
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



data RenderData
  = RenderData
  { renderFinished      :: VkSemaphore
  , imageAvailable      :: VkSemaphore
  , device              :: VkDevice
  , swapChainInfo       :: SwapChainImgInfo
  , deviceQueues        :: DevQueues
  , imgIndexPtr         :: Ptr Word32
  , commandBuffers      :: Ptr VkCommandBuffer
  , uniformBuffers      :: Ptr VkDeviceMemory
  , updateUniformBuffer :: forall r. VkDevice -> VkDeviceMemory -> Double -> Program r ()
  }


drawFrame :: RenderData -> Program r ()
drawFrame RenderData {..} = do
    -- Acquiring an image from the swap chain
    runVk $ vkAcquireNextImageKHR
          device swapchain maxBound
          imageAvailable VK_NULL_HANDLE imgIndexPtr
    imgIndex <- peek imgIndexPtr
    let bufPtr = commandBuffers `plusPtr`
                 (fromIntegral imgIndex * sizeOf (undefined :: VkCommandBuffer))
    let uniformBufPtr = uniformBuffers `plusPtr`
                 (fromIntegral imgIndex * sizeOf (undefined :: VkDeviceMemory))
    uniBuf <- peek @VkDeviceMemory uniformBufPtr
    now <- liftIO getSystemTime
    startTime <- startTime <$> get
    let deltaSeconds = systemSeconds now - systemSeconds startTime
        -- Have to nanoseconds convert from Word64 before subtraction to allow negative delta.
        deltaNanoseconds :: Int64 = fromIntegral (systemNanoseconds now) - fromIntegral (systemNanoseconds startTime)
        -- Seconds in Double keep at least microsecond-precision for 285 years.
        -- Float is not good enough even for millisecond-precision over more than a few hours.
        seconds :: Double = fromIntegral deltaSeconds + fromIntegral deltaNanoseconds / 1e9
    updateUniformBuffer device uniBuf seconds
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
