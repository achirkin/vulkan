{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Lib.Vulkan.Buffer
  ( createBuffer
  , copyBuffer
  , findMemoryType
  ) where

import           Data.Bits
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Graphics.Vulkan.Marshal.Create.DataFrame
import           Numeric.DataFrame

import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Command


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
copyBuffer dev commandPool cmdQueue srcBuffer dstBuffer bSize =
  runCommandsOnce dev commandPool cmdQueue $ \cmdBuf -> do
    let copyRegion = createVk @VkBufferCopy
          $  set @"srcOffset" 0
          &* set @"dstOffset" 0
          &* set @"size" bSize
    withVkPtr copyRegion $ liftIO . vkCmdCopyBuffer cmdBuf srcBuffer dstBuffer 1


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
