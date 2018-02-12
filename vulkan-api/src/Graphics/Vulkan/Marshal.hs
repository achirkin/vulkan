{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE ViewPatterns               #-}
-- | This module is not part of auto-generated code based on vk.xml.
--   Instead, it is hand-written to provide common types and classes.
module Graphics.Vulkan.Marshal
  ( VulkanMarshal (..)
  , VulkanPtr (..)
  , VkPtr (..)
  , pattern VK_NULL_HANDLE
  , clearStorable
    -- * Type-indexed access to struct members
  , HasField (..), CanReadField (..), CanWriteField (..)
  , CanReadFieldArray (..), CanWriteFieldArray (..), IndexInBounds
    -- * Re-exported functions from 'Foreign.ForeignPtr'
  , mallocForeignPtr, withForeignPtr, addForeignPtrFinalizer
    -- * Re-exported common types
  , Int8, Int16, Int32, Int64
  , Word8, Word16, Word32, Word64
  , Ptr, FunPtr, Void, CString
    -- * Utilities for string types
  , withCStringField, unsafeCStringField
  , getStringField, readStringField, writeStringField
  , eqCStrings, eqCStringsN
  ) where

import           Data.Data             (Data)
import           Data.Int              (Int16, Int32, Int64, Int8)
import           Data.Kind             (Constraint)
import           Data.Void             (Void)
import           Data.Word             (Word16, Word32, Word64, Word8)
import           Foreign.C.String      (CString, peekCString)
import           Foreign.C.Types       (CChar, CInt (..), CSize (..))
import           Foreign.ForeignPtr    (ForeignPtr, addForeignPtrFinalizer,
                                        mallocForeignPtr, withForeignPtr)
import           Foreign.Marshal.Array (pokeArray0)
import           Foreign.Marshal.Utils (fillBytes)
import           Foreign.Ptr           (FunPtr, Ptr, castPtr, nullPtr, plusPtr)
import           Foreign.Storable      (Storable (sizeOf))
import           GHC.Generics          (Generic)
import           GHC.TypeLits
import           System.IO.Unsafe      (unsafeDupablePerformIO)

-- | All Vulkan structures are stored as-is in byte arrays to avoid any overheads
--   for wrapping and unwrapping haskell values.
--   VulkanMarshal provides an interfaces to write and read these structures
--   in an imperative way.
class VulkanMarshal a where
  -- | Names of fields in vulkan structure or union,
  --   in the same order as they appear in C typedef.
  type StructFields a :: [Symbol]
  -- | Allocate a pinned aligned byte array to keep vulkan data structure
  --   and fill it using a foreign function.
  --
  --   Note, the function is supposed to use `newAlignedPinnedByteArray#`
  --   and does not guarantee to fill memory with zeroes.
  --   Use `clearStorable` to make sure all bytes are set to zero.
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


-- | Fill all bytes to zero getting data size from `Storable` instance.
clearStorable :: Storable a => Ptr a -> IO ()
clearStorable p = fillBytes p 0 (sizeOf $ unptr p)
  where
    unptr :: Ptr b -> b
    unptr ~_ = undefined

-- | ===== @VK_DEFINE_NON_DISPATCHABLE_HANDLE@
-- Non-dispatchable handles are represented as `VkPtr`
--
-- Represented as `Word64`
--
-- >
-- > #if !defined(VK_DEFINE_NON_DISPATCHABLE_HANDLE)
-- > #if defined(__LP64__) || defined(_WIN64) || (defined(__x86_64__) && !defined(__ILP32__) ) || defined(_M_X64) || defined(__ia64) || defined (_M_IA64) || defined(__aarch64__) || defined(__powerpc64__)
-- >         #define VK_DEFINE_NON_DISPATCHABLE_HANDLE(object) typedef struct object##_T *object;
-- > #else
-- >         #define VK_DEFINE_NON_DISPATCHABLE_HANDLE(object) typedef uint64_t object;
-- > #endif
-- > #endif
-- >
--
newtype VkPtr a = VkPtr Word64
  deriving (Eq, Ord, Show, Storable, Generic, Data)
type role VkPtr phantom


-- | Unify dispatchable and non-dispatchable vulkan pointer types.
--
--  Dispatchable handles are represented as `Ptr`.
--
--  Non-dispatchable handles are represented as `VkPtr`.
--
class VulkanPtr ptr where
  vkNullPtr :: ptr a

instance VulkanPtr Ptr where
  vkNullPtr = nullPtr
  {-# INLINE vkNullPtr #-}

instance VulkanPtr VkPtr where
  vkNullPtr = VkPtr 0
  {-# INLINE vkNullPtr #-}

isNullPtr :: (Eq (ptr a), VulkanPtr ptr) => ptr a -> Bool
isNullPtr = (vkNullPtr ==)
{-# INLINE isNullPtr #-}

-- | >
--   > #define VK_NULL_HANDLE 0
--   >
pattern VK_NULL_HANDLE :: (Eq (ptr a), VulkanPtr ptr) => ptr a
pattern VK_NULL_HANDLE <- (isNullPtr -> True)
  where VK_NULL_HANDLE = vkNullPtr

-- | Describe fields of a vulkan structure or union.
class HasField (fname :: Symbol) (a :: *) where
  -- | Type of a field in a vulkan structure or union.
  type FieldType fname a     :: *
  -- | Whether this field marked optional in vulkan specification.
  --   Usually, this means that `VK_NULL_HANDLE` can be written in place
  --   of this field.
  type FieldOptional fname a :: Bool
  -- | Offset of a field in bytes.
  type FieldOffset fname a :: Nat
  -- | Whether this field marked optional in vulkan specification.
  --   Usually, this means that `VK_NULL_HANDLE` can be written in place
  --   of this field.
  fieldOptional :: Bool
  -- | Offset of a field in bytes.
  fieldOffset :: Int

class HasField fname a => CanReadField (fname :: Symbol) (a :: *) where
  getField :: a -> FieldType fname a
  readField :: Ptr a -> IO (FieldType fname a)

class CanReadField fname a => CanWriteField (fname :: Symbol) (a :: *) where
  writeField :: Ptr a -> FieldType fname a -> IO ()

class ( HasField fname a
      , IndexInBounds fname idx a
      ) => CanReadFieldArray (fname :: Symbol) (idx :: Nat) (a :: *) where
  -- | Length of an array that is a field of a structure or union
  type FieldArrayLength fname a :: Nat
  -- | Length of an array that is a field of a structure or union
  fieldArrayLength :: Int
  getFieldArray :: a -> FieldType fname a
  readFieldArray :: Ptr a -> IO (FieldType fname a)

class CanReadFieldArray fname idx a
      => CanWriteFieldArray (fname :: Symbol) (idx :: Nat) (a :: *) where
  writeFieldArray :: Ptr a -> FieldType fname a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError (NoField fname a) => HasField fname a where
instance {-# OVERLAPPABLE #-}
         TypeError (NoField fname a) => CanReadField fname a where
instance {-# OVERLAPPABLE #-}
         TypeError (NoField fname a) => CanWriteField fname a where
instance {-# OVERLAPPABLE #-}
         ( TypeError (NoField fname a)
         , IndexInBounds fname idx a
         ) => CanReadFieldArray fname idx a where
instance {-# OVERLAPPABLE #-}
         ( TypeError (NoField fname a)
         , IndexInBounds fname idx a
         ) => CanWriteFieldArray fname idx a where

type NoField s a = 'Text "Structure " ':<>: 'ShowType a
  ':<>: 'Text " does not have field '" ':<>: 'Text s ':<>: 'Text "'."

type IndexInBounds (s :: Symbol) (i :: Nat) (a :: *)
  = IndexInBounds' s i a (CmpNat i (FieldArrayLength s a))

type family IndexInBounds' (s :: Symbol)
                           (i :: Nat)
                           (a :: *) (r :: Ordering) :: Constraint where
  IndexInBounds' _ _ _ 'LT = ()
  IndexInBounds' s i a _ = TypeError
    ( 'Text "Array index " ':<>: 'ShowType i
     ':<>: 'Text " is out of bounds for '"
     ':<>: 'Text s ':<>: 'Text "',  member of type "
     ':<>: 'ShowType a ':<>: 'Text "."
     ':$$: 'Text "Note: the array size is "
        ':<>: 'ShowType (FieldArrayLength s a) ':<>: 'Text "."
    )

-- | Perform an action on a C string field.
--   The string pointers should not be used outside the callback.
--   It will point to a correct location only as long as the struct is alive.
withCStringField :: forall fname a b
                 . ( CanReadFieldArray fname 0 a
                   , FieldType fname a ~ CChar
                   , VulkanMarshal a
                   )
                 => a -> (CString -> IO b) -> IO b
withCStringField x f = do
  r <- f (unsafeCStringField @fname @a x)
  touchVkData x
  pure r

-- | Get pointer to a memory location of the C string field in a structure.
unsafeCStringField :: forall fname a
                   . ( CanReadFieldArray fname 0 a
                     , FieldType fname a ~ CChar
                     , VulkanMarshal a
                     )
                   => a -> CString
unsafeCStringField x = castPtr $ unsafePtr x `plusPtr` fieldOffset @fname @a


getStringField :: forall fname a
                . ( CanReadFieldArray fname 0 a
                  , FieldType fname a ~ CChar
                  , VulkanMarshal a
                  )
               => a -> String
getStringField x
    = case takeForce (fieldArrayLength @fname @0 @a)
         . unsafeDupablePerformIO
         $ withCStringField @fname @a x peekCString of
        ((), s) -> s

readStringField :: forall fname a
                . ( CanReadFieldArray fname 0 a
                  , FieldType fname a ~ CChar
                  , VulkanMarshal a
                  )
               => Ptr a -> IO String
readStringField px = do
  ((), s) <- takeForce (fieldArrayLength @fname @0 @a)
         <$> peekCString (castPtr $ px `plusPtr` fieldOffset @fname @a)
  return s

writeStringField :: forall fname a
                  . ( CanWriteFieldArray fname 0 a
                    , FieldType fname a ~ CChar
                    , VulkanMarshal a
                    )
               => Ptr a -> String -> IO ()
writeStringField px =
  pokeArray0 '\0' (castPtr $ px `plusPtr` fieldOffset @fname @a)

takeForce :: Int -> String -> ((), String)
takeForce 0 _      = ((), [])
takeForce _ []     = ((), [])
takeForce n (x:xs) = seq x $ (x:) <$> takeForce (n-1) xs


-- | Check first if two CString point to the same memory location.
--   Otherwise, compare them using C @strcmp@ function.
eqCStrings :: CString -> CString -> Bool
eqCStrings a b
  | a == b = True
  | c_strcmp a b == 0 = True
  | otherwise = False

-- | Check first if two CString point to the same memory location.
--   Otherwise, compare them using C @strncmp@ function.
--   It may be useful to provide maximum number of characters to compare.
eqCStringsN :: CString -> CString -> Int -> Bool
eqCStringsN a b n
  | a == b = True
  | c_strncmp a b (fromIntegral n) == 0 = True
  | otherwise = False


foreign import ccall unsafe "strncmp"
  c_strncmp :: CString -> CString -> CSize -> CInt

foreign import ccall unsafe "strcmp"
  c_strcmp :: CString -> CString -> CInt
