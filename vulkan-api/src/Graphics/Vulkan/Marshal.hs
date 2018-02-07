{-# LANGUAGE Strict #-}
-- | This module is not part of auto-generated code based on vk.xml.
--   Instead, it is hand-written to provide common types and classes.
module Graphics.Vulkan.Marshal
  ( VulkanMarshal (..)
    -- * Re-exported functions from 'Foreign.ForeignPtr'
  , mallocForeignPtr, withForeignPtr, addForeignPtrFinalizer
    -- * Re-exported common types
  , Int8, Int16, Int32, Int64
  , Word8, Word16, Word32, Word64
  , Ptr, Void, CString
  ) where

import           Data.Int           (Int16, Int32, Int64, Int8)
import           Data.Void          (Void)
import           Data.Word          (Word16, Word32, Word64, Word8)
import           Foreign.C.String   (CString)
import           Foreign.ForeignPtr (ForeignPtr, addForeignPtrFinalizer,
                                     mallocForeignPtr, withForeignPtr)
import           Foreign.Ptr        (Ptr)

-- | All Vulkan structures are stored as-is in byte arrays to avoid any overheads
--   for wrapping and unwrapping haskell values.
--   VulkanMarshal provides an interfaces to write and read these structures
--   in an imperative way.
class VulkanMarshal a where
  -- | Allocate a pinned aligned byte array to keep vulkan data structure
  --   and fill it using a foreign function.
  newVkData :: (Ptr a -> IO ()) -> IO a
  -- | Get pointer to vulkan structure.
  --   Note, the address is only valid as long as a given vulkan structure exists.
  --   Structures created with newVkData are stored in pinned byte arrays,
  --   so their memory is maintained by Haskell GC.
  unsafePtr  :: a -> Ptr a
  -- | Get vulkan structure referenced by a 'ForeignPtr' trying to avoid copying data.
  --
  --   This function does copy data if called on an unmanaged `ForeignPtr`
  --   (i.e. one created from ordinary `Ptr` using something like `newForeignPtr`.).
  --
  --   This function does not copy data if called on a managed `ForeignPtr`
  --   (i.e. one created using `mallocForeignPtr`, or `toForeignPtr`, or `toPlainForeignPtr`).
  --
  --   Note, `fromForeignPtr` does not copy finalizers of `ForeignPtr`.
  --   Thus, if all references to original `ForeignPtr` are lost,
  --     its attached finalizers may run even if the created structure is alive.
  fromForeignPtr :: ForeignPtr a -> IO a
  -- | Create a `ForeignPtr` referencing the structure without copying data.
  toForeignPtr :: a -> IO (ForeignPtr a)
  -- | Create a `ForeignPtr` referencing the structure without copying data.
  --   This version of a pointer carries no finalizers.
  --
  -- It is not possible to add a finalizer to a ForeignPtr created with
  -- @unsafeToPlainForeignPtr@.
  -- Attempts to add a finalizer to a ForeignPtr created this way, or to
  -- finalize such a pointer, will throw an exception.
  toPlainForeignPtr :: a -> IO (ForeignPtr a)
  -- | Make sure this data is alive at a given point in a sequence of IO actions.
  touchVkData  :: a -> IO ()
