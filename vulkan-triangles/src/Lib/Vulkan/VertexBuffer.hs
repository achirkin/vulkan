{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Lib.Vulkan.VertexBuffer
  ( createVertexBuffer
  , createIndexBuffer
  , createUniformBuffers
  ) where


import           Data.Bits
import           Foreign.Ptr                              (castPtr)
import           Foreign.Storable                         (poke)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Graphics.Vulkan.Marshal.Create.DataFrame
import           Numeric.DataFrame
import           Numeric.Dimensions

import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Vertex


createVertexBuffer :: VkPhysicalDevice
                   -> VkDevice
                   -> VkCommandPool
                   -> VkQueue
                   -> DataFrame Vertex '[XN 3]
                      -- ^ A collection of at least three vertices
                   -> Program r VkBuffer
createVertexBuffer pdev dev cmdPool cmdQueue (XFrame vertices) = do

    let bSize = fromIntegral $ bSizeOf vertices

    (_, vertexBuf) <-
      createBuffer pdev dev bSize
        ( VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_VERTEX_BUFFER_BIT )
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    -- Use "locally" to destroy temporary staging buffer after data copy is complete
    locally $ do
      (stagingMem, stagingBuf) <-
        createBuffer pdev dev bSize VK_BUFFER_USAGE_TRANSFER_SRC_BIT
          ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )

      -- copy data
      dataPtr <- allocaPeek $
        runVk . vkMapMemory dev stagingMem 0 bSize 0
      liftIO $ poke (castPtr dataPtr) vertices
      liftIO $ vkUnmapMemory dev stagingMem
      copyBuffer dev cmdPool cmdQueue stagingBuf vertexBuf bSize

    return vertexBuf


createIndexBuffer :: VkPhysicalDevice
                  -> VkDevice
                  -> VkCommandPool
                  -> VkQueue
                  -> DataFrame Word16 '[XN 3]
                     -- ^ A collection of at least three indices
                  -> Program r VkBuffer
createIndexBuffer pdev dev cmdPool cmdQueue (XFrame indices) = do

    let bSize = fromIntegral $ bSizeOf indices

    (_, vertexBuf) <-
      createBuffer pdev dev bSize
        ( VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_INDEX_BUFFER_BIT )
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    -- Use "locally" to destroy temporary staging buffer after data copy is complete
    locally $ do
      (stagingMem, stagingBuf) <-
        createBuffer pdev dev bSize VK_BUFFER_USAGE_TRANSFER_SRC_BIT
          ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )

      -- copy data
      dataPtr <- allocaPeek $
        runVk . vkMapMemory dev stagingMem 0 bSize 0
      liftIO $ poke (castPtr dataPtr) indices
      liftIO $ vkUnmapMemory dev stagingMem
      copyBuffer dev cmdPool cmdQueue stagingBuf vertexBuf bSize

    return vertexBuf


createUniformBuffers
  :: VkPhysicalDevice
  -> VkDevice
  -> VkDeviceSize
  -> Int
  -> Program r [(VkDeviceMemory, VkBuffer)]
createUniformBuffers pdev dev bSize n = do
      sequence $ replicate n $ createBuffer pdev dev bSize
         VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
         ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )


createBuffer :: VkPhysicalDevice
             -> VkDevice
             -> VkDeviceSize
             -> VkBufferUsageFlags
             -> VkMemoryPropertyFlags
             -> Program r (VkDeviceMemory, VkBuffer)
createBuffer pdev dev bSize bUsage bMemPropFlags = do
    -- create buffer
    let bufferInfo = createVk @VkBufferCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"size" bSize
          &* set @"usage" bUsage
          &* set @"sharingMode" VK_SHARING_MODE_EXCLUSIVE
          &* set @"queueFamilyIndexCount" 0
          &* set @"pQueueFamilyIndices" VK_NULL
    (buf, freeBufLater) <- allocResource'
      (\vb -> liftIO $ vkDestroyBuffer dev vb VK_NULL) $
      withVkPtr bufferInfo $ \biPtr -> allocaPeek $
        runVk . vkCreateBuffer dev biPtr VK_NULL

    -- find its memory requirements
    memRequirements <- allocaPeek $
      liftIO . vkGetBufferMemoryRequirements dev buf

    memIndex <- findMemoryType pdev (getField @"memoryTypeBits" memRequirements)
                                    bMemPropFlags

    -- allocate memory
    let allocInfo = createVk @VkMemoryAllocateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"allocationSize" (getField @"size" memRequirements)
          &* set @"memoryTypeIndex" memIndex

    vertexBufferMemory <- allocResource
      (\vbm -> liftIO $ vkFreeMemory dev vbm VK_NULL) $
      withVkPtr allocInfo $ \aiPtr -> allocaPeek $
        runVk . vkAllocateMemory dev aiPtr VK_NULL
    -- The buf will be released before release of any of the resources
    -- allocated above, but after release on any allocations below.
    freeBufLater

    -- associate memory with buffer
    runVk $ vkBindBufferMemory dev buf vertexBufferMemory 0

    return (vertexBufferMemory, buf)

copyBuffer :: VkDevice
           -> VkCommandPool
           -> VkQueue
           -> VkBuffer -> VkBuffer -> VkDeviceSize -> Program r ()
copyBuffer dev commandPool cmdQueue srcBuffer dstBuffer bSize = do

    -- create command buffer
    let allocInfo = createVk @VkCommandBufferAllocateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
          &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
          &* set @"commandPool" commandPool
          &* set @"commandBufferCount" 1
          &* set @"pNext" VK_NULL

    cmdBufs <- allocResource
      (liftIO . flip withDFPtr (vkFreeCommandBuffers dev commandPool 1)) $
      withVkPtr allocInfo $ \aiPtr -> allocaPeekDF $
        runVk . vkAllocateCommandBuffers dev aiPtr

    -- record command buffer
    let cmdbBI = createVk @VkCommandBufferBeginInfo
          $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
          &* set @"flags" VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
          &* set @"pNext" VK_NULL
        copyRegion = createVk @VkBufferCopy
          $  set @"srcOffset" 0
          &* set @"dstOffset" 0
          &* set @"size" bSize
        cmdBuf = unScalar cmdBufs
    withVkPtr cmdbBI $ runVk . vkBeginCommandBuffer cmdBuf
    withVkPtr copyRegion $ liftIO . vkCmdCopyBuffer cmdBuf srcBuffer dstBuffer 1
    runVk $ vkEndCommandBuffer cmdBuf

    -- execute command in a give queue
    let submitInfo = createVk @VkSubmitInfo
          $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
          &* set @"pNext" VK_NULL
          &* set @"waitSemaphoreCount" 0
          &* set @"pWaitSemaphores"   VK_NULL
          &* set @"pWaitDstStageMask" VK_NULL
          &* set @"commandBufferCount" 1
          &* setDFRef @"pCommandBuffers" cmdBufs
          &* set @"signalSemaphoreCount" 0
          &* set @"pSignalSemaphores" VK_NULL
    withVkPtr submitInfo $ \siPtr ->
      runVk $ vkQueueSubmit cmdQueue 1 siPtr VK_NULL_HANDLE
    runVk $ vkQueueWaitIdle cmdQueue




-- | Return an index of a memory type for a device
findMemoryType :: VkPhysicalDevice
               -> Word32 -- ^ type filter bitfield
               -> VkMemoryPropertyFlags
                  -- ^ desired memory properties
               -> Program r Word32
findMemoryType pdev typeFilter properties = do
    memProps <- allocaPeek $ liftIO . vkGetPhysicalDeviceMemoryProperties pdev
    let mtCount = getField @"memoryTypeCount" memProps
        memTypes = getVec @"memoryTypes" memProps
        go i | i == mtCount = throwVkMsg "Failed to find suitable memory type!"
             | otherwise = if    testBit typeFilter (fromIntegral i)
                              && ( getField @"propertyFlags"
                                        (ixOff (fromIntegral i) memTypes)
                                    .&. properties
                                 ) == properties
                           then return i
                           else go (i+1)
    go 0
