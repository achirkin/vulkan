{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Lib.Vulkan.Vertex
  ( Vertex (..), vertIBD, vertIADs
  , loadModel, atLeastThree, dfLen
  ) where


import           Codec.Wavefront
import qualified Control.Monad.ST                         as ST
import           Data.Foldable                            (toList)
import           Data.Maybe
import qualified Data.Set                                 as Set
import           GHC.Generics                             (Generic)
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Graphics.Vulkan.Marshal.Create.DataFrame ()
import           Numeric.DataFrame
import qualified Numeric.DataFrame.ST                     as ST
import           Numeric.Dimensions

import Lib.Program


-- | Preparing Vertex data to make an interleaved array.
data Vertex = Vertex
  { pos      :: Vec3f
  , color    :: Vec3f
  , texCoord :: Vec2f
  } deriving (Eq, Ord, Show, Generic)

{-

We need an instance of PrimBytes to fit Vertex into a DataFrame.
Luckily, Generics can do it for us.

This automatic instance tries to pack fields of your data type as dense as possible
while respecting the alignment requirements.
The result is something like a C-struct with the same fields (hopefully identical).

When Vertex has an instance of PrimBytes, you can put it into DataFrames, but
also you can do things with pointers:

  * bSizeOf  -- size of the packed binary representation
  * bAlignOf  -- alignment of the packed binary representation
  * bFieldOffsetOf @"fieldName" -- offset of a field (e.g. "pos", "color")
  * bPeek/bPoke and others -- write data to Ptr similar to Storable
 -}
instance PrimBytes Vertex

-- | Check if the frame has enough elements.
atLeastThree :: (All KnownDimType ns, BoundedDims ns)
             => DataFrame t (n ': ns) -> DataFrame t (XN 3 ': ns)
atLeastThree = fromMaybe (error "Lib.Vulkan.Vertex.atLeastThree: not enough points")
             . constrainDF

-- | Get number of points in a vector
dfLen :: DataFrame t (xns :: [XNat]) -> Word32
dfLen (XFrame (_ :: DataFrame t ns)) = case dims @ns of
  n :* _ -> fromIntegral $ dimVal n
  U      -> 1

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
    ST.writeDataFrame mv (0 :* Empty) . scalar $ createVk
        $  set @"location" 0
        &* set @"binding" 0
        &* set @"format" VK_FORMAT_R32G32B32_SFLOAT
        &* set @"offset" (bFieldOffsetOf @"pos" @Vertex undefined)
    ST.writeDataFrame mv (1 :* Empty) . scalar $ createVk
        $  set @"location" 1
        &* set @"binding" 0
        &* set @"format" VK_FORMAT_R32G32B32_SFLOAT
        &* set @"offset" (bFieldOffsetOf @"color" @Vertex undefined)
                          -- Now we can use bFieldOffsetOf derived
                          -- in PrimBytes via Generics. How cool is that!
    ST.writeDataFrame mv (2 :* Empty) . scalar $ createVk
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
  obj `seq` logInfo "Processing geometry.."
  let r = objVertices obj
  r `seq` logInfo "Geometry done."
  return r


objVertices :: WavefrontOBJ -> (DataFrame Vertex '[XN 3], DataFrame Word32 '[XN 3])
objVertices WavefrontOBJ {..}
  | XFrame objLocs  <- fromList . map fromLoc  $ toList objLocations
  , XFrame objTexCs <- fromList . map fromTexC $ toList objTexCoords
    -- the two lines below let GHC know the value length of objLocs and objTexCs
    -- at the type level; we need this for the mkVertex function below.
  , D :* _ <- dims `inSpaceOf` objLocs
  , D :* _ <- dims `inSpaceOf` objTexCs
  , allVertices <- map (scalar . mkVertex objLocs objTexCs) faceIndices
  , vertSet <- Set.fromList allVertices
    = ( atLeastThree . fromList $ Set.toList vertSet
      , atLeastThree . fromList
          $ map (fromIntegral . flip Set.findIndex vertSet) allVertices
      )
  | otherwise = error "objVertices: impossible arguments"
  where
    triangles = concatMap (faceToTriangles . elValue) $ toList objFaces
    faceIndices = concatMap triangleToFaceIndices triangles
    fromLoc (Location x y z _) = vec3 x y z
    fromTexC (TexCoord r s _) = vec2 r (1 - s)
    {- Note, we need to substract 1 from all indices, because Wavefron OBJ indices
       are 1-based (rather than 0-based indices of vector or easytensor packages).

       More info: http://www.martinreddy.net/gfx/3d/OBJ.spec
       "Each of these types of vertices is numbered separately, starting with 1"
     -}
    mkVertex :: ( KnownDim n, KnownDim m)
             => Matrix Float n 3 -> Matrix Float m 2
             -> FaceIndex -> Vertex
    mkVertex objLocs objTexCs FaceIndex {..} = Vertex
      { pos      = objLocs ! fromIntegral (faceLocIndex - 1)
      , color    = vec3 1 1 1
      , texCoord = objTexCs ! fromIntegral (fromJust faceTexCoordIndex - 1)
      }
