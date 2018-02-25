{-# LANGUAGE Strict #-}
-- | Collection of functions adapted from @Foreign@ module hierarchy
module Lib.Program.Foreign
    ( Ptr, plusPtr, Storable.sizeOf
    , alloca, allocaArray
    , peek, peekArray
    , asListVk
    , allocaPeek, allocaPeekVk
    , mallocRes, mallocArrayRes, newArrayRes
    ) where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import qualified Foreign.Marshal.Alloc   as Foreign
import qualified Foreign.Marshal.Array   as Foreign
import           Foreign.Ptr
import           Foreign.Storable        (Storable)
import qualified Foreign.Storable        as Storable
import           Graphics.Vulkan.Marshal

import           Lib.Program



alloca :: Storable a
       => (Ptr a -> Program' b)
       -> Program r b
alloca = liftIOWith Foreign.alloca

-- | Despite its name, this command does not copy data from a created pointer.
--   It uses `newVkData` function inside.
allocaPeekVk :: VulkanMarshal a
             => (Ptr a -> Program () ())
             -> Program r a
allocaPeekVk pf = Program $ \ref c -> do
  locVar <- liftIO newEmptyMVar
  a <- newVkData (\ptr -> unProgram (pf ptr) ref (putMVar locVar))
  takeMVar locVar >>= c . (a <$)

allocaArray :: Storable a
            => Int
            -> (Ptr a -> Program' b)
            -> Program r b
allocaArray = liftIOWith . Foreign.allocaArray


allocaPeek :: Storable a
           => (Ptr a -> Program (Either VulkanException a) ())
           -> Program r a
allocaPeek f = alloca $ \ptr -> f ptr >> liftIO (Storable.peek ptr)


peekArray :: Storable a => Int -> Ptr a -> Program r [a]
peekArray n = liftIO . Foreign.peekArray n

peek :: Storable a => Ptr a -> Program r a
peek = liftIO . Storable.peek


-- | Get size of action output and then get the result,
--   performing data copy.
asListVk :: Storable x
         => (Ptr Word32 -> Ptr x -> Program (Either VulkanException [x]) ())
         -> Program r [x]
asListVk action = alloca $ \counterPtr -> do
  action counterPtr VK_NULL_HANDLE
  counter <- liftIO $ fromIntegral <$> Storable.peek counterPtr
  if counter <= 0
  then pure []
  else allocaArray counter $ \valPtr -> do
    action counterPtr valPtr
    liftIO $ Foreign.peekArray counter valPtr

-- | Allocate an array and release it after continuation finishes.
--   Uses @allocaArray@ from @Foreign@ inside.
--
--   Use `locally` to bound the scope of resource allocation.
mallocArrayRes :: Storable a => Int -> Program r (Ptr a)
mallocArrayRes n = Program $ \_ c -> Foreign.allocaArray n (c . Right)

-- | Allocate some memory for Storable and release it after continuation finishes.
--   Uses @alloca@ from @Foreign@ inside.
--
--   Use `locally` to bound the scope of resource allocation.
mallocRes :: Storable a => Program r (Ptr a)
mallocRes = Program $ \_ c -> Foreign.alloca (c . Right)

-- | Temporarily store a list of storable values in memory
--   and release it after continuation finishes.
--   Uses @withArray@ from @Foreign@ inside.
--
--   Use `locally` to bound the scope of resource allocation.
newArrayRes :: Storable a => [a] -> Program r (Ptr a)
newArrayRes xs = Program $ \_ c -> Foreign.withArray xs (c . Right)
