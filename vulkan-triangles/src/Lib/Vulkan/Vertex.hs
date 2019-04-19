{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE TypeApplications #-}
module Lib.Vulkan.Vertex
  ( Vertex (..), vertIBD, vertIADs
  , loadModel
  ) where


import           Codec.Wavefront
import qualified Control.Monad.ST                         as ST
import           Data.Maybe
import qualified Data.Set                                 as Set
import qualified Data.Vector                              as Vec
import           GHC.Generics                             (Generic)
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Graphics.Vulkan.Marshal.Create.DataFrame ()
import           Numeric.DataFrame
import qualified Numeric.DataFrame.ST                     as ST

import           Lib.Program


-- | Preparing Vertex data to make an interleaved array.
data Vertex = Vertex
  { pos      :: Vec3f
  , color    :: Vec3f
  , texCoord :: Vec2f
  } deriving (Eq, Ord, Show, Generic)

-- We need an instance of PrimBytes to fit Vertex into a DataFrame.
-- Luckily, Generics can do it for us.
instance PrimBytes Vertex


vertIBD :: VkVertexInputBindingDescription
vertIBD = createVk
  $  set @"binding" 0
  &* set @"stride"  (bSizeOf @Vertex undefined)
  &* set @"inputRate" VK_VERTEX_INPUT_RATE_VERTEX

-- We can use DataFrames to keep several vulkan structures in a contiguous
-- memory areas, so that we can pass a pointer to a DataFrame directly into
-- a vulkan function with no copy.
--
-- However, we must make sure the created DataFrame is pinned!
vertIADs :: Vector VkVertexInputAttributeDescription 3
vertIADs = ST.runST $ do
    mv <- ST.newPinnedDataFrame
    ST.writeDataFrame mv 0 . scalar $ createVk
        $  set @"location" 0
        &* set @"binding" 0
        &* set @"format" VK_FORMAT_R32G32B32_SFLOAT
        &* set @"offset" (bFieldOffsetOf @"pos" @Vertex undefined)
    ST.writeDataFrame mv 1 . scalar $ createVk
        $  set @"location" 1
        &* set @"binding" 0
        &* set @"format" VK_FORMAT_R32G32B32_SFLOAT
        &* set @"offset" (bFieldOffsetOf @"color" @Vertex undefined)
                          -- Now we can use bFieldOffsetOf derived
                          -- in PrimBytes via Generics. How cool is that!
    ST.writeDataFrame mv 2 . scalar $ createVk
        $  set @"location" 2
        &* set @"binding" 0
        &* set @"format" VK_FORMAT_R32G32_SFLOAT
        &* set @"offset" (bFieldOffsetOf @"texCoord" @Vertex undefined)
    ST.unsafeFreezeDataFrame mv

data Tri = Tri {-# UNPACK #-}!FaceIndex
               {-# UNPACK #-}!FaceIndex
               {-# UNPACK #-}!FaceIndex

triangleToFaceIndices :: Tri -> [FaceIndex]
-- reversal here for correct culling in combination with the (-y) below
triangleToFaceIndices (Tri a b c) = [c, b, a]

faceToTriangles :: Face -> [Tri]
faceToTriangles (Face a b c []) = [Tri a b c]
faceToTriangles (Face a b c is) = pairwise (Tri a) (b:c:is)
  where pairwise f xs = zipWith f xs (tail xs)

loadModel :: FilePath
          -> Program r (DataFrame Vertex '[XN 3], DataFrame Word32 '[XN 3])
loadModel file = do
  logInfo "Loading model.."
  obj <- either throwVkMsg pure =<< Codec.Wavefront.fromFile file
  logInfo "Processing geometry.."
  let triangles = concatMap (faceToTriangles . elValue) (Vec.toList (objFaces obj))
  let faceIndices = concatMap triangleToFaceIndices triangles
  let allVertices = flip map faceIndices $ \fi ->
        -- No idea why indices are off by one. Could be a bug in the wavefront library.
        let Location x y z _ = objLocations obj Vec.! (faceLocIndex fi - 1)
            TexCoord r s _ = objTexCoords obj Vec.! (fromJust (faceTexCoordIndex fi) - 1)
        in scalar $ Vertex (vec3 x (-y) z) (vec3 1 1 1) (vec2 r (1 - s))
  let vertSet = Set.fromList allVertices
  let vertices = fromJust . constrainDF @'[XN 3] @'[XN 0] . fromList $ Set.toList vertSet
  let indices = fromJust . constrainDF @'[XN 3] @'[XN 0] . fromList
              $ map (fromIntegral . flip Set.findIndex vertSet) allVertices
  logInfo "Geometry done."
  -- logInfo $ "allVertices length " ++ show (length allVertices)
  -- logInfo $ "vertices length " ++ show (dimSize1 vertices)
  return (vertices, indices)
