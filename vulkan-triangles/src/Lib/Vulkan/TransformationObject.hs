{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeApplications    #-}
module Lib.Vulkan.TransformationObject
  ( updateTransObj
  , createTransObjBuffers
  , transObjBufferInfo
  ) where

import           Data.Bits                      ((.|.))
import           Foreign.Ptr                    (castPtr)
import           Foreign.Storable
import           GHC.Generics                   (Generic)
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Numeric.DataFrame
import           Numeric.PrimBytes

import           Lib.Program
import           Lib.Program.Foreign
import           Lib.Vulkan.Buffer


data TransformationObject = TransformationObject
  { model :: Mat44f
  , view  :: Mat44f
  , proj  :: Mat44f
  } deriving (Show, Generic)

instance PrimBytes TransformationObject

rotation :: Double -> Mat44f
rotation seconds =
  let rate = 0.25 -- rotations per second
      (_::Int, phaseTau) = properFraction $ seconds * rate
  in rotate (vec3 0 0 1) (realToFrac phaseTau * 2 * pi)

updateTransObj :: VkDevice -> VkExtent2D -> VkDeviceMemory -> Program r ()
updateTransObj device extent uniBuf = do
      uboPtr <- allocaPeek $
        runVk . vkMapMemory device uniBuf 0 (fromIntegral $ sizeOf @(Scalar TransformationObject) undefined) 0
      seconds <- getTime
      let width = getField @"width" extent
      let height = getField @"height" extent
      let aspectRatio = fromIntegral width / fromIntegral height
      let ubo = TransformationObject
            { model = rotation seconds
            , view = lookAt (vec3 0 0 (-1)) (vec3 2 2 2) (vec3 0 0 0)
            , proj = perspective 0.1 20 (45/360*2*pi) aspectRatio
            }
      liftIO $ poke (castPtr uboPtr) (scalar ubo)
      liftIO $ vkUnmapMemory device uniBuf

createTransObjBuffers
  :: VkPhysicalDevice
  -> VkDevice
  -> Int
  -> Program r [(VkDeviceMemory, VkBuffer)]
createTransObjBuffers pdev dev n = do
      sequence $ replicate n $ createBuffer pdev dev (fromIntegral $ sizeOf @(Scalar TransformationObject) undefined)
         VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
         ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )

transObjBufferInfo :: VkBuffer -> Program r VkDescriptorBufferInfo
transObjBufferInfo uniformBuffer = return $ createVk @VkDescriptorBufferInfo
        $  set @"buffer" uniformBuffer
        &* set @"offset" 0
        &* set @"range" (fromIntegral $ sizeOf @(Scalar TransformationObject) undefined)
