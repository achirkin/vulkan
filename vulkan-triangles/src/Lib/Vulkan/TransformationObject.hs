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

import Data.Bits                      ((.|.))
import Foreign.Ptr                    (castPtr)
import GHC.Generics                   (Generic)
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Numeric.DataFrame

import Lib.Program
import Lib.Program.Foreign
import Lib.Vulkan.Buffer

{- |
  Here, in the `view` matrix, we have to decide on our world coordinates.
  This should always be a right-triple, which is forced by lookAt matrix and
  many others. So, here are popular options
    right  up   forward
     x     y      -z     <-- classical OpenGL approach, coincides with projection space
     x     z       y     <-- all positive, z being "up" is kind of intuitive
     ... and a few others, e.g.
     y     z      -x

  I stick x-z-y, because the same triple is used in chalet.obj from the tutorial.
  This affects the choice of axis rotation and lookAt up vector below.

  More about these transformtions can be found be the link:
  https://en.wikibooks.org/wiki/GLSL_Programming/Vertex_Transformations
 -}
data TransformationObject = TransformationObject
  { model :: Mat44f
  , view  :: Mat44f
  , proj  :: Mat44f
  } deriving (Show, Generic)

instance PrimBytes TransformationObject

rotation :: Double -> Mat44f
rotation seconds =
  let rate = 1/16 -- rotations per second
      (_::Int, phaseTau) = properFraction $ seconds * rate
  in rotate (vec3 0 0 1) (realToFrac phaseTau * 2 * pi)

updateTransObj :: VkDevice -> VkExtent2D -> VkDeviceMemory -> Program r ()
updateTransObj device extent uniBuf = do
      uboPtr <- allocaPeek $
        runVk . vkMapMemory device uniBuf 0 (bSizeOf @TransformationObject undefined) 0
      seconds <- getTime
      let -- rotate the world and objects
          model = rotation seconds
      poke (castPtr uboPtr) (scalar $ TransformationObject {..})
      liftIO $ vkUnmapMemory device uniBuf
  where
    -- how world is seen from camera (in camera coordinates)
    view = lookAt (vec3 0 0 1) (vec3 2 2 2) (vec3 0 0 0)
    -- projection onto the screen
    proj' = perspective 0.1 20 (45/360*2*pi) aspectRatio
    -- now, here is a problem:
    --   vulkan screen coordinates differ from usual clipping coordinates
    --   (x,y) now are (right, bottom) instead of (right, top).
    -- one way to account for this is to negate "y" component of the projection matrix
    proj = update i11 (negate $ proj' ! i11) proj'
    width = getField @"width" extent
    height = getField @"height" extent
    aspectRatio = fromIntegral width / fromIntegral height
    i11 = 1 :* 1 :* U :: Idxs '[4,4]

createTransObjBuffers
  :: VkPhysicalDevice
  -> VkDevice
  -> Int
  -> Program r [(VkDeviceMemory, VkBuffer)]
createTransObjBuffers pdev dev n = do
      sequence $ replicate n $ createBuffer pdev dev (bSizeOf @TransformationObject undefined)
         VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
         ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )

transObjBufferInfo :: VkBuffer -> Program r VkDescriptorBufferInfo
transObjBufferInfo uniformBuffer = return $ createVk @VkDescriptorBufferInfo
        $  set @"buffer" uniformBuffer
        &* set @"offset" 0
        &* set @"range" (bSizeOf @TransformationObject undefined)
