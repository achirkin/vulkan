{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Lib.Vulkan.VertexBuffer
  ( createVertexBuffer
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
                   -> DataFrame Vertex '[XN 3]
                      -- ^ A collection of at least three vertices
                   -> Program r VkBuffer
createVertexBuffer pdev dev (XFrame vertices) = do
    -- create buffer
    let bufferInfo = createVk @VkBufferCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"size" (fromIntegral $ sizeOf vertices)
          &* set @"usage" VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
          &* set @"sharingMode" VK_SHARING_MODE_EXCLUSIVE
          &* set @"queueFamilyIndexCount" 0
          &* set @"pQueueFamilyIndices" VK_NULL
    buf <- allocResource
      (\vb -> liftIO $ vkDestroyBuffer dev vb VK_NULL) $
      withVkPtr bufferInfo $ \biPtr -> allocaPeek $
        runVk . vkCreateBuffer dev biPtr VK_NULL

    -- find its memory requirements
    memRequirements <- allocaPeek $
      liftIO . vkGetBufferMemoryRequirements dev buf

    memIndex <- findMemoryType pdev (getField @"memoryTypeBits" memRequirements)
                                     $  VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                     .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT

    -- allocate memory
    let allocInfo = createVk @VkMemoryAllocateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"allocationSize" (getField @"size" memRequirements)
          &* set @"memoryTypeIndex" memIndex

    -- TODO: change the code so that memory is destroyed after buffer
    vertexBufferMemory <- allocResource
      (\vbm -> liftIO $ vkFreeMemory dev vbm VK_NULL) $
      withVkPtr allocInfo $ \aiPtr -> allocaPeek $
        runVk . vkAllocateMemory dev aiPtr VK_NULL

    -- associate memory with buffer
    runVk $ vkBindBufferMemory dev buf vertexBufferMemory 0

    -- copy data
    dataPtr <- allocaPeek $
      runVk . vkMapMemory dev vertexBufferMemory 0
                          (getField @"size" bufferInfo) 0
    liftIO $ poke (castPtr dataPtr) vertices
    liftIO $ vkUnmapMemory dev vertexBufferMemory

    return buf




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
