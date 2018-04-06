{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
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
  ( FlagType (..), FlagMask, FlagBit
  , VulkanMarshal (..), VulkanMarshalPrim ()
  , VulkanPtr (..)
  , VkPtr (..)
  , pattern VK_NULL_HANDLE, pattern VK_NULL
  , clearStorable, withPtr
    -- * Type-indexed access to struct members
  , HasField (..), CanReadField (..), CanWriteField (..)
  , CanReadFieldArray (..), CanWriteFieldArray (..)
  , IsFieldArray, IndexInBounds
    -- * Re-exported functions from 'Foreign.ForeignPtr'
  , mallocForeignPtr, withForeignPtr, addForeignPtrFinalizer
    -- * Re-exported common types
  , Int8, Int16, Int32, Int64
  , Word8, Word16, Word32, Word64
  , Ptr, FunPtr, Void, CString
  , CInt (..), CSize (..), CChar (..), CWchar (..), CULong (..)
    -- * Utilities for string types
  , withCStringField, unsafeCStringField
  , getStringField, readStringField, writeStringField
  , cmpCStrings, cmpCStringsN
  ) where

import           Data.Data                        (Data)
import           Data.Int                         (Int16, Int32, Int64, Int8)
import           Data.Kind                        (Constraint, Type)
import           Data.Void                        (Void)
import           Data.Word                        (Word16, Word32, Word64,
                                                   Word8)
import           Foreign.C.String                 (CString, peekCString)
import           Foreign.C.Types                  (CChar (..), CWchar (..), CInt (..), CSize (..), CULong (..))
import           Foreign.ForeignPtr               (ForeignPtr,
                                                   addForeignPtrFinalizer,
                                                   mallocForeignPtr,
                                                   withForeignPtr)
import           Foreign.Marshal.Array            (pokeArray0)
import           Foreign.Marshal.Utils            (fillBytes)
import           Foreign.Ptr                      (FunPtr, nullPtr, plusPtr)
import           Foreign.Storable                 (Storable (sizeOf))
import           GHC.Generics                     (Generic)
import           GHC.Ptr                          (Ptr (..))
import           GHC.TypeLits
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

import           Graphics.Vulkan.Marshal.Internal


-- | Distinguish single bits and bitmasks in vulkan flags
data FlagType = FlagMask | FlagBit

-- | Vulkan flags type that can have multiple bits set.
type FlagMask = 'FlagMask
-- | Vulkan single bit flag value.
type FlagBit  = 'FlagBit


-- | All Vulkan structures are stored as-is in byte arrays to avoid any overheads
--   for wrapping and unwrapping haskell values.
--   VulkanMarshal provides an interfaces to write and read these structures
--   in an imperative way.
class VulkanMarshal a where
  -- | Names of fields in vulkan structure or union,
  --   in the same order as they appear in C typedef.
  type StructFields a :: [Symbol]
  -- | Whether this type is a C union.
  --   Otherwise this is a C structure.
  type CUnionType a   :: Bool
  -- | Notes that this struct or union is going to be filled in by the API,
  --   rather than an application filling it out and passing it to the API.
  type ReturnedOnly a :: Bool
  -- | Comma-separated list of structures whose "pNext" can include this type.
  type StructExtends a :: [Type]
  -- | Allocate a pinned aligned byte array to keep vulkan data structure
  --   and fill it using a foreign function.
  --
  --   Note, the function is supposed to use `newAlignedPinnedByteArray#`
  --   and does not guarantee to fill memory with zeroes.
  --   Use `clearStorable` to make sure all bytes are set to zero.
  --
  --   Note, the memory is managed by GHC, thus no need for freeing it manually.
  newVkData :: (Ptr a -> IO ()) -> IO a
  default newVkData :: (Storable a, VulkanMarshalPrim a)
                    => (Ptr a -> IO ()) -> IO a
  newVkData = newVkData#
  {-# INLINE newVkData #-}
  -- | Allocate a pinned aligned byte array to keep vulkan data structure.
  --
  --   Note, the function is supposed to use `newAlignedPinnedByteArray#`
  --   and does not guarantee to fill memory with zeroes.
  --   Use `clearStorable` to make sure all bytes are set to zero.
  --
  --   Note, the memory is managed by GHC, thus no need for freeing it manually.
  mallocVkData :: IO a
  default mallocVkData :: (Storable a, VulkanMarshalPrim a) => IO a
  mallocVkData = mallocVkData#
  {-# INLINE mallocVkData #-}
  -- | Allocate a pinned aligned byte array to keep vulkan data structures.
  --   Returned `Ptr a` points to the first element in the contiguous array of
  --   returned structures. Returned list elements point to the same memory area.
  --   This function is unsafe in two ways:
  --
  --     * Several structures are stored next to each other, with no gaps;
  --       it would break its alignment if the size is not multiple of alignment.
  --     * Returned pointer is not tracked by GHC as a reference to the managed
  --       memory. Thus, the array can be GCed if all references to the returned
  --       list are lost.
  --
  --   Note, the function is supposed to use `newAlignedPinnedByteArray#`
  --   and does not guarantee to fill memory with zeroes.
  --   Use `clearStorable` to make sure all bytes are set to zero.
  --
  --   Note, the memory is managed by GHC, thus no need for freeing it manually.
  mallocVkDataArray :: Int -> IO (Ptr a, [a])
  default mallocVkDataArray :: (Storable a, VulkanMarshalPrim a)
                            => Int -> IO (Ptr a, [a])
  mallocVkDataArray = mallocVkDataArray#
  {-# INLINE mallocVkDataArray #-}
  -- | Get pointer to vulkan structure.
  --   Note, the address is only valid as long as a given vulkan structure exists.
  --   Structures created with newVkData are stored in pinned byte arrays,
  --   so their memory is maintained by Haskell GC.
  unsafePtr  :: a -> Ptr a
  default unsafePtr :: VulkanMarshalPrim a => a -> Ptr a
  unsafePtr a = Ptr (unsafeAddr a)
  {-# INLINE unsafePtr #-}
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
  default fromForeignPtr :: (Storable a, VulkanMarshalPrim a)
                         => ForeignPtr a -> IO a
  fromForeignPtr = fromForeignPtr#
  {-# INLINE fromForeignPtr #-}
  -- | Create a `ForeignPtr` referencing the structure without copying data.
  toForeignPtr :: a -> IO (ForeignPtr a)
  default toForeignPtr :: VulkanMarshalPrim a => a -> IO (ForeignPtr a)
  toForeignPtr = toForeignPtr#
  {-# INLINE toForeignPtr #-}
  -- | Create a `ForeignPtr` referencing the structure without copying data.
  --   This version of a pointer carries no finalizers.
  --
  -- It is not possible to add a finalizer to a ForeignPtr created with
  -- @toPlainForeignPtr@.
  -- Attempts to add a finalizer to a ForeignPtr created this way, or to
  -- finalize such a pointer, will throw an exception.
  toPlainForeignPtr :: a -> IO (ForeignPtr a)
  default toPlainForeignPtr :: VulkanMarshalPrim a => a -> IO (ForeignPtr a)
  toPlainForeignPtr = toPlainForeignPtr#
  {-# INLINE toPlainForeignPtr #-}
  -- | Make sure this data is alive at a given point in a sequence of IO actions.
  touchVkData  :: a -> IO ()
  default touchVkData :: VulkanMarshalPrim a => a -> IO ()
  touchVkData = touchVkData#
  {-# INLINE touchVkData #-}

-- | Run some operation with a pointer to vulkan structure.
--
--   Should be used with care:
--     the structure pretends to be immutable, so it is better to only read
--     from the pointed memory area, not to write.
--     If an action needs to write something to the pointer, use `newVkData`.
withPtr :: VulkanMarshal a => a -> (Ptr a -> IO b) -> IO b
withPtr x k = do
  b <- k (unsafePtr x)
  touchVkData x
  return b

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

-- | Null pointer (either dispatchable or non-dispatchable)
pattern VK_NULL :: (Eq (ptr a), VulkanPtr ptr) => ptr a
pattern VK_NULL <- (isNullPtr -> True)
  where VK_NULL = vkNullPtr

-- | >
--   > #define VK_NULL_HANDLE 0
--   >
pattern VK_NULL_HANDLE :: (Eq (ptr a), VulkanPtr ptr) => ptr a
pattern VK_NULL_HANDLE = VK_NULL

-- | Describe fields of a vulkan structure or union.
class HasField (fname :: Symbol) (a :: Type) where
  -- | Type of a field in a vulkan structure or union.
  type FieldType fname a     :: Type
  -- | Whether this field marked optional in vulkan specification.
  --   Usually, this means that `VK_NULL` can be written in place
  --   of this field.
  type FieldOptional fname a :: Bool
  -- | Offset of a field in bytes.
  type FieldOffset fname a :: Nat
  -- | Whether this field is a fixed-length array stored directly in a struct.
  type FieldIsArray fname a :: Bool
  -- | Whether this field marked optional in vulkan specification.
  --   Usually, this means that `VK_NULL` can be written in place
  --   of this field.
  fieldOptional :: Bool
  -- | Offset of a field in bytes.
  fieldOffset :: Int

class ( HasField fname a
      , IsFieldArray fname a 'False
      ) => CanReadField (fname :: Symbol) (a :: Type) where
  getField :: a -> FieldType fname a
  readField :: Ptr a -> IO (FieldType fname a)

class CanReadField fname a => CanWriteField (fname :: Symbol) (a :: Type) where
  writeField :: Ptr a -> FieldType fname a -> IO ()

class ( HasField fname a
      , IndexInBounds fname idx a
      , IsFieldArray fname a 'True
      ) => CanReadFieldArray (fname :: Symbol) (idx :: Nat) (a :: Type) where
  -- | Length of an array that is a field of a structure or union
  type FieldArrayLength fname a :: Nat
  -- | Length of an array that is a field of a structure or union
  fieldArrayLength :: Int
  getFieldArray :: a -> FieldType fname a
  readFieldArray :: Ptr a -> IO (FieldType fname a)

class CanReadFieldArray fname idx a
      => CanWriteFieldArray (fname :: Symbol) (idx :: Nat) (a :: Type) where
  writeFieldArray :: Ptr a -> FieldType fname a -> IO ()

instance {-# OVERLAPPABLE #-}
         TypeError (ErrorNoSuchField fname a) => HasField fname a where

instance {-# OVERLAPPABLE #-}
         ( HasField fname a
         , IsFieldArray fname a 'False
         , TypeError (ErrorNotReadableField fname a)
         ) => CanReadField fname a where
instance {-# OVERLAPPABLE #-}
         ( CanReadField fname a
         , TypeError (ErrorNotWritableField fname a)
         ) => CanWriteField fname a where
instance {-# OVERLAPPABLE #-}
         ( HasField fname a
         , IsFieldArray fname a 'True
         , IndexInBounds fname idx a
         , TypeError (ErrorNotReadableField fname a)
         ) => CanReadFieldArray fname idx a where
instance {-# OVERLAPPABLE #-}
         ( CanReadFieldArray fname idx a
         , TypeError (ErrorNotWritableField fname a)
         ) => CanWriteFieldArray fname idx a where


type IndexInBounds (s :: Symbol) (i :: Nat) (a :: Type)
  = IndexInBounds' s i a (CmpNat i (FieldArrayLength s a))

type family IndexInBounds' (s :: Symbol)
                           (i :: Nat)
                           (a :: Type) (r :: Ordering) :: Constraint where
  IndexInBounds' _ _ _ 'LT = ()
  IndexInBounds' s i a _ = TypeError ( ErrorIndexOutOfBounds s i a )


type IsFieldArray s a e = IsFieldArray' s a (FieldIsArray s a) e

type family IsFieldArray' (s :: Symbol)
                          (a :: Type)
                          (actual :: Bool)
                          (expected :: Bool) :: Constraint where
  IsFieldArray' _ _ 'True  'True  = ()
  IsFieldArray' _ _ 'False 'False = ()
  IsFieldArray' s a 'True  'False = TypeError (ErrorIsArrayField s a)
  IsFieldArray' s a 'False 'True  = TypeError (ErrorIsNotArrayField s a)


--------------------------------------------------------------------------------
-- * Type-level errors
--------------------------------------------------------------------------------


type ErrorNoSuchField (s :: Symbol) (a :: Type)
  = 'Text "Structure " ':<>: 'ShowType a
  ':<>: 'Text " does not have field " ':<>: 'ShowType s ':<>: 'Text "."
  ':$$: 'Text "Note, this structure has following fields: "
        ':<>: 'ShowType (StructFields a)


type ErrorIsNotArrayField (s :: Symbol) (a :: Type)
  = 'Text "Field " ':<>: 'ShowType s ':<>:
    'Text " of structure " ':<>: 'ShowType a ':<>:
    'Text " is not an array field."
  ':$$: 'Text "Don't use ***FieldArray functions on it."

type ErrorIsArrayField (s :: Symbol) (a :: Type)
  = 'Text "Field " ':<>: 'ShowType s ':<>:
    'Text " of structure " ':<>: 'ShowType a ':<>:
    'Text " is an array field."
  ':$$: 'Text "Use ***FieldArray functions on it."

type ErrorIndexOutOfBounds (s :: Symbol) (i :: Nat) (a :: Type)
  = 'Text "Array index " ':<>: 'ShowType i ':<>:
    'Text " is out of bounds for '" ':<>:
    'Text s ':<>: 'Text "',  member of type " ':<>: 'ShowType a ':<>: 'Text "."
  ':$$:
    'Text "Note: the array size is "
      ':<>: 'ShowType (FieldArrayLength s a) ':<>: 'Text "."

type ErrorNotReadableField (s :: Symbol) (a :: Type)
  = 'Text "Field " ':<>: 'ShowType s ':<>:
    'Text " of structure " ':<>: 'ShowType a ':<>:
    'Text " is not readable."

type ErrorNotWritableField (s :: Symbol) (a :: Type)
  = 'Text "Field " ':<>: 'ShowType s ':<>:
    'Text " of structure " ':<>: 'ShowType a ':<>:
    'Text " is not writable."


--------------------------------------------------------------------------------
-- * Utilities for CString
--------------------------------------------------------------------------------

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
unsafeCStringField x = unsafePtr x `plusPtr` fieldOffset @fname @a


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
         <$> peekCString (px `plusPtr` fieldOffset @fname @a)
  return s

writeStringField :: forall fname a
                  . ( CanWriteFieldArray fname 0 a
                    , FieldType fname a ~ CChar
                    , VulkanMarshal a
                    )
               => Ptr a -> String -> IO ()
writeStringField px =
  pokeArray0 '\0' (px `plusPtr` fieldOffset @fname @a)

takeForce :: Int -> String -> ((), String)
takeForce 0 _      = ((), [])
takeForce _ []     = ((), [])
takeForce n (x:xs) = seq x $ (x:) <$> takeForce (n-1) xs


-- | Check first if two CString point to the same memory location.
--   Otherwise, compare them using C @strcmp@ function.
cmpCStrings :: CString -> CString -> Ordering
cmpCStrings a b
  | a == b = EQ
  | otherwise = c_strcmp a b `compare` 0

-- | Check first if two CString point to the same memory location.
--   Otherwise, compare them using C @strncmp@ function.
--   It may be useful to provide maximum number of characters to compare.
cmpCStringsN :: CString -> CString -> Int -> Ordering
cmpCStringsN a b n
  | a == b = EQ
  | otherwise = c_strncmp a b (fromIntegral n) `compare` 0


foreign import ccall unsafe "strncmp"
  c_strncmp :: CString -> CString -> CSize -> CInt

foreign import ccall unsafe "strcmp"
  c_strcmp :: CString -> CString -> CInt
