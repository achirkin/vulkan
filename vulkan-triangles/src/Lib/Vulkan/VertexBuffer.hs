{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Lib.Vulkan.VertexBuffer
  ( createVertexBuffer
  , createIndexBuffer
  ) where

import           Data.Bits
import           Foreign.Ptr                              (castPtr)
import           Foreign.Storable                         (poke)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Numeric.DataFrame
import           Numeric.Dimensions

import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Buffer
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

