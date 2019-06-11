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

import Control.Monad                  (replicateM)
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
        runVk . vkMapMemory device uniBuf 0 (bSizeOf @TransformationObject undefined) VK_ZERO_FLAGS
      seconds <- getTime
      let -- rotate the world and objects
          model = rotation seconds
      poke (castPtr uboPtr) (scalar $ TransformationObject {..})
      liftIO $ vkUnmapMemory device uniBuf
  where
    -- how world is seen from camera (in camera coordinates)
    view = lookAt (vec3 0 0 1) (vec3 2 2 2) (vec3 0 0 0)
    -- projection onto the screen ...
    proj = proj' %* clip
    -- ... which is a normal perspective projection matrix that maps the view space
    --     onto the clip space cube {x: -1..1, y: -1..1, z: -1..1}
    proj' = perspective 0.1 20 (45/360*2*pi) aspectRatio
    -- ... and a {clip space -> screen space} matrix that converts points into
    --     the vulkan screen space {x: -1..1, y: 1..-1, z: 0..1}
    clip = DF4
      (DF4 1   0   0   0)
      (DF4 0 (-1)  0   0)
      (DF4 0   0  0.5  0)
      (DF4 0   0  0.5  1)

    -- calculate aspect ratio for the
    width = getField @"width" extent
    height = getField @"height" extent
    aspectRatio = fromIntegral width / fromIntegral height


createTransObjBuffers
  :: VkPhysicalDevice
  -> VkDevice
  -> Int
  -> Program r [(VkDeviceMemory, VkBuffer)]
createTransObjBuffers pdev dev n = replicateM n $
    createBuffer pdev dev (bSizeOf @TransformationObject undefined)
         VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
         ( VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT )

transObjBufferInfo :: VkBuffer -> Program r VkDescriptorBufferInfo
transObjBufferInfo uniformBuffer = return $ createVk @VkDescriptorBufferInfo
        $  set @"buffer" uniformBuffer
        &* set @"offset" 0
        &* set @"range" (bSizeOf @TransformationObject undefined)
