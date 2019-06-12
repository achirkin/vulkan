{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE UnliftedFFITypes           #-}
-- | This module is not part of auto-generated code based on vk.xml.
--   Instead, it is hand-written to provide common types and classes.
--
--   DANGER!
--   This is an internal module; it can change a lot between package versions;
--   it provides low-level functions, most of which have user-friendly analogues.
module Graphics.Vulkan.Marshal.Internal
  ( VkStruct (..), unsafeFromByteArrayOffset
  , VulkanMarshal (..)
  , newVkData, mallocVkData, mallocVkDataArray, unsafePtr
  , fromForeignPtr, toForeignPtr, toPlainForeignPtr, touchVkData
    -- * Type-indexed access to struct members
  , StructFields, CUnionType, ReturnedOnly, StructExtends
  , StructFieldNames, HasField, FieldRep, FieldType
  , FieldOptional, FieldOffset
  , FieldIsArray, FieldArrayLength
  , CanReadField, CanWriteField
  , CanReadFieldArray, CanWriteFieldArray
  , fieldOptional, fieldOffset, fieldArrayLength
  , getField, readField, writeField
  , getFieldArrayUnsafe, readFieldArrayUnsafe, writeFieldArrayUnsafe
  , getFieldArray, readFieldArray, writeFieldArray
  , IndexInBounds
    -- * Type-level info about Structs
  , VulkanStruct (..), VulkanField (..), VulkanFields (..), KnownBool (..)
  , FieldMeta (..), StructMeta (..)
    -- * Utilities for string types
  , withCStringField, unsafeCStringField
  , getStringField, readStringField, writeStringField
  , cmpCStrings, cmpCStringsN
  ) where


import Data.Kind             (Constraint, Type)
import Data.Type.Equality
import Foreign.C.String      (CString, peekCString)
import Foreign.C.Types       (CChar, CInt (..), CSize (..))
import Foreign.ForeignPtr    (ForeignPtr, newForeignPtr_)
import Foreign.Marshal.Array (pokeArray0)
import Foreign.Ptr           (plusPtr)
import Foreign.Storable
import GHC.Base              (Addr#, ByteArray#, IO (..), Int (..), Int#,
                              byteArrayContents#, copyAddrToByteArray#, eqAddr#,
                              isTrue#, minusAddr#, newAlignedPinnedByteArray#,
                              plusAddr#, touch#, unsafeCoerce#,
                              unsafeFreezeByteArray#, (*#), (+#), (>=#))
import GHC.Exts              (Proxy#, proxy#)
import GHC.ForeignPtr        (ForeignPtr (..), ForeignPtrContents (..))
import GHC.Ptr               (Ptr (..))
import GHC.TypeLits
import System.IO.Unsafe      (unsafeDupablePerformIO)
import Unsafe.Coerce         (unsafeCoerce)



{- | Internal representation of all Vulkan structures:
     a pinned byte array and an address pointing to an area in this array.
 -}
data VkStruct a = VkStruct
  { unsafeAddr      :: Addr#
    -- ^ Get address of vulkan structure.
    --   Note, the address is only valid as long as a given vulkan structure exists.
    --   Structures created with newVkData are stored in pinned byte arrays,
    --   so their memory is maintained by Haskell GC.
  , unsafeByteArray :: ByteArray#
    -- ^ Get a @ByteArray#@ that keeps the data.
    --
    --   Note, the data structure does not necessarily starts at zero offset.
  }

-- | Get the type parameter of a `VkStruct`.
type family VkStruct' (a :: Type) :: Type where
    VkStruct' (VkStruct a) = a

-- | This type must be a `VkStruct`.
type IsVkStruct a = a ~ VkStruct (VkStruct' a)

-- | Combine a vulkan structure from ByteArray and an offset in this array.
unsafeFromByteArrayOffset :: Int# -> ByteArray# -> VkStruct a
unsafeFromByteArrayOffset off b
  = VkStruct (plusAddr# (byteArrayContents# b) off) b
{-# INLINE unsafeFromByteArrayOffset #-}



{- |
@FieldMeta fieldName fieldType optional byteOffset length canRead canWrite@
represents a Vulkan structure field at the type level.
 -}
data FieldMeta
  = FieldMeta Symbol Type Bool Nat Nat Bool Bool

{- |
@StructMeta structName structType size alignment fields isUnion isReturnedOnly structExtends@
represents a Vulkan structure at the type level.
 -}
data StructMeta
  = StructMeta Symbol Type Nat Nat [FieldMeta] Bool Bool [Type]

-- | This class give a term-level boolean associated with a type-level boolean.
--
--   The same as `KnownNat` for integers.
class KnownBool (b :: Bool) where
    boolSing :: Bool

instance KnownBool 'True  where boolSing = True
instance KnownBool 'False where boolSing = False

class (Show (FType m), Storable (FType m))
     => VulkanField (m :: FieldMeta) where
    type FName m       :: Symbol
    type FType m       :: Type
    type FOptional m   :: Bool
    type FByteOffset m :: Nat
    type FLength m     :: Nat
    type FCanRead m    :: Bool
    type FCanWrite m   :: Bool
    fName :: String
    fOptional :: Bool
    fByteOffset :: Int
    fLength :: Int
    fCanRead :: Bool
    fCanWrite :: Bool

instance ( KnownSymbol fieldName
         , Show t, Storable t
         , KnownBool   optional
         , KnownNat    byteOffset
         , KnownNat    length
         , KnownBool   canRead
         , KnownBool   canWrite
         ) => VulkanField ('FieldMeta fieldName t optional byteOffset length canRead canWrite) where
    type FName       ('FieldMeta fieldName t optional byteOffset length canRead canWrite) = fieldName
    type FType       ('FieldMeta fieldName t optional byteOffset length canRead canWrite) = t
    type FOptional   ('FieldMeta fieldName t optional byteOffset length canRead canWrite) = optional
    type FByteOffset ('FieldMeta fieldName t optional byteOffset length canRead canWrite) = byteOffset
    type FLength     ('FieldMeta fieldName t optional byteOffset length canRead canWrite) = length
    type FCanRead    ('FieldMeta fieldName t optional byteOffset length canRead canWrite) = canRead
    type FCanWrite   ('FieldMeta fieldName t optional byteOffset length canRead canWrite) = canWrite
    fName       = symbolVal' @fieldName proxy#
    fOptional   = boolSing   @optional
    fByteOffset = fromInteger $ natVal' @byteOffset proxy#
    fLength     = fromInteger $ natVal' @length proxy#
    fCanRead    = boolSing   @canRead
    fCanWrite   = boolSing   @canWrite

type family GetFieldMeta (errMsg :: ErrorMessage) (fname :: Symbol) (ms :: [FieldMeta]) :: FieldMeta where
    GetFieldMeta _ n ('FieldMeta n t o b l r w ': _) = 'FieldMeta n t o b l r w
    GetFieldMeta e n (_ ': ms) = GetFieldMeta e n ms
    GetFieldMeta e n '[] = TypeError e

class VulkanFields (ms :: [FieldMeta]) where
    withField :: forall (fname :: Symbol) (r :: Type) (errMsg :: ErrorMessage)
               . KnownSymbol fname
              => Proxy# fname
              -> Proxy# errMsg
              -> (VulkanField (GetFieldMeta errMsg fname ms) => r) -> r
    enumerateFields :: forall  (a :: Type)
                     . (forall (m :: FieldMeta) . VulkanField m
                             => Proxy# m -> a -> a)
                    -> a -> a

instance VulkanFields '[] where
    withField = error "VulkanFields.withField: unreachable code (no such field guarded by type family)."
    enumerateFields _ = id

instance (VulkanField m, VulkanFields ms) => VulkanFields (m ': ms) where
    withField pName pErr f
      | symbolVal' pName == fName @m
      , Refl <- proofm  pName pErr = f
      | Refl <- proofms pName pErr = withField @ms pName pErr f
      where
        proofm :: Proxy# fname -> Proxy# errMsg
              -> (m :~: GetFieldMeta errMsg fname (m : ms))
        proofm _ = unsafeCoerce Refl
        proofms :: Proxy# fname -> Proxy# errMsg
                -> (GetFieldMeta errMsg fname ms :~: GetFieldMeta errMsg fname (m : ms))
        proofms _ = unsafeCoerce Refl
    enumerateFields k = k (proxy# @_ @m) . enumerateFields @ms k


class VulkanFields (SFields m)
   => VulkanStruct (m :: StructMeta) where
    type SName m           :: Symbol
    type SType m           :: Type
    type SSize m           :: Nat
    type SAlign m          :: Nat
    type SFields m         :: [FieldMeta]
    type SIsUnion m        :: Bool
    type SIsReturnedOnly m :: Bool
    type SStructExtends m  :: [Type]
    sName           :: String
    sSize           :: Int
    sAlign          :: Int
    sIsUnion        :: Bool
    sIsReturnedOnly :: Bool

instance ( KnownSymbol structName
         , KnownNat size
         , KnownNat alignment
         , VulkanFields fields
         , KnownBool isUnion
         , KnownBool isReturnedOnly
         )
      => VulkanStruct
          ('StructMeta structName structType size alignment
                         fields isUnion isReturnedOnly structExtends) where
    type SName           ('StructMeta structName structType size alignment fields isUnion isReturnedOnly structExtends) = structName
    type SType           ('StructMeta structName structType size alignment fields isUnion isReturnedOnly structExtends) = structType
    type SSize           ('StructMeta structName structType size alignment fields isUnion isReturnedOnly structExtends) = size
    type SAlign          ('StructMeta structName structType size alignment fields isUnion isReturnedOnly structExtends) = alignment
    type SFields         ('StructMeta structName structType size alignment fields isUnion isReturnedOnly structExtends) = fields
    type SIsUnion        ('StructMeta structName structType size alignment fields isUnion isReturnedOnly structExtends) = isUnion
    type SIsReturnedOnly ('StructMeta structName structType size alignment fields isUnion isReturnedOnly structExtends) = isReturnedOnly
    type SStructExtends  ('StructMeta structName structType size alignment fields isUnion isReturnedOnly structExtends) = structExtends
    sName           = symbolVal' @structName proxy#
    sSize           = fromInteger $ natVal' @size proxy#
    sAlign          = fromInteger $ natVal' @alignment proxy#
    sIsUnion        = boolSing @isUnion
    sIsReturnedOnly = boolSing @isReturnedOnly


-- | Descriptions of all fields of a vulkan struct
type StructFields a = SFields (StructRep a)
-- | Whether this type is a C union.
--   Otherwise this is a C structure.
type CUnionType a = SIsUnion (StructRep a)
-- | Notes that this struct or union is going to be filled in by the API,
--   rather than an application filling it out and passing it to the API.
type ReturnedOnly a = SIsReturnedOnly (StructRep a)
-- | Comma-separated list of structures whose "pNext" can include this type.
type StructExtends a = SStructExtends (StructRep a)

-- | All Vulkan structures are stored as-is in byte arrays to avoid any overheads
--   for wrapping and unwrapping haskell values.
--   VulkanMarshal provides an interfaces to write and read these structures
--   in an imperative way.
class (VulkanStruct (StructRep a), IsVkStruct a)
     => VulkanMarshal a where
    type StructRep a :: StructMeta

-- | Allocate a pinned aligned byte array to keep vulkan data structure
--   and fill it using a foreign function.
--
--   Note, the function is supposed to use `newAlignedPinnedByteArray#`
--   and does not guarantee to fill memory with zeroes.
--   Use `clearStorable` to make sure all bytes are set to zero.
--
--   Note, the memory is managed by GHC, thus no need for freeing it manually.
newVkData :: forall a . VulkanMarshal a => (Ptr a -> IO ()) -> IO a
newVkData f
  | I# n <- sSize @(StructRep a)
  , I# a <- sAlign @(StructRep a)
  = IO
  (\s0 -> case newAlignedPinnedByteArray# n a s0 of
    (# s1, mba #) -> case unsafeFreezeByteArray# mba s1 of
      (# s2, ba #) -> case f (Ptr (byteArrayContents# ba)) of
        IO k -> case k s2 of
          (# s3, () #) -> (# s3, unsafeFromByteArrayOffset 0# ba #)
  )
{-# INLINE newVkData #-}

-- | Allocate a pinned aligned byte array to keep vulkan data structure.
--
--   Note, the function is supposed to use `newAlignedPinnedByteArray#`
--   and does not guarantee to fill memory with zeroes.
--   Use `clearStorable` to make sure all bytes are set to zero.
--
--   Note, the memory is managed by GHC, thus no need for freeing it manually.
mallocVkData :: forall a . VulkanMarshal a => IO a
mallocVkData
  | I# n <- sSize @(StructRep a)
  , I# a <- sAlign @(StructRep a)
  = IO
  (\s0 -> case newAlignedPinnedByteArray# n a s0 of
    (# s1, mba #) -> case unsafeFreezeByteArray# mba s1 of
      (# s2, ba #) -> (# s2, unsafeFromByteArrayOffset 0# ba #)
  )
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
mallocVkDataArray :: forall a . VulkanMarshal a => Int -> IO (Ptr a, [a])
mallocVkDataArray (I# m)
  | I# n <- sSize @(StructRep a)
  , I# a <- sAlign @(StructRep a)
  , nm <- n *# m
  = IO
  (\s0 -> case newAlignedPinnedByteArray# nm a s0 of
    (# s1, mba #) -> case unsafeFreezeByteArray# mba s1 of
      (# s2, ba #) ->
        (# s2
        , ( Ptr (byteArrayContents# ba)
          , let go k | isTrue# (k >=# nm) = []
                     | otherwise = unsafeFromByteArrayOffset k ba
                                 : go (k +# n)
            in go 0#
          )
        #)
  )
{-# INLINE mallocVkDataArray #-}

-- | Get pointer to vulkan structure.
--   Note, the address is only valid as long as a given vulkan structure exists.
--   Structures created with newVkData are stored in pinned byte arrays,
--   so their memory is maintained by Haskell GC.
unsafePtr  :: IsVkStruct a => a -> Ptr a
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
fromForeignPtr :: forall a . VulkanMarshal a => ForeignPtr a -> IO a
fromForeignPtr (ForeignPtr addr PlainForeignPtr{})
  | I# n <- sSize @(StructRep a)
  , I# a <- sAlign @(StructRep a)
  = IO
  (\s0 -> case newAlignedPinnedByteArray# n a s0 of
    (# s1, mba #) -> case copyAddrToByteArray# addr mba 0# n s1 of
      s2 -> case unsafeFreezeByteArray# mba s2 of
        (# s3, ba #) -> (# s3, unsafeFromByteArrayOffset 0# ba  #)
  )
fromForeignPtr (ForeignPtr addr (MallocPtr mba _))
  = IO
  (\s0 -> case unsafeFreezeByteArray# mba s0 of
    (# s1, ba #) -> (# s1, unsafeFromByteArrayOffset
                           (minusAddr# addr (byteArrayContents# ba)) ba  #)
  )
fromForeignPtr (ForeignPtr addr (PlainPtr mba))
  = IO
  (\s0 -> case unsafeFreezeByteArray# mba s0 of
    (# s1, ba #) -> (# s1, unsafeFromByteArrayOffset
                           (minusAddr# addr (byteArrayContents# ba)) ba  #)
  )
{-# INLINE fromForeignPtr #-}


-- | Create a `ForeignPtr` referencing the structure without copying data.
toForeignPtr :: IsVkStruct a => a -> IO (ForeignPtr a)
toForeignPtr x
  | a <- unsafeAddr x
  , b <- unsafeByteArray x = do
    ForeignPtr _ (PlainForeignPtr r)
      <- newForeignPtr_ (Ptr a)
    IO (\s -> (# s, ForeignPtr a (MallocPtr (unsafeCoerce# b) r) #))
{-# INLINE toForeignPtr #-}

-- | Create a `ForeignPtr` referencing the structure without copying data.
--   This version of a pointer carries no finalizers.
--
-- It is not possible to add a finalizer to a ForeignPtr created with
-- @toPlainForeignPtr@.
-- Attempts to add a finalizer to a ForeignPtr created this way, or to
-- finalize such a pointer, will throw an exception.
toPlainForeignPtr :: IsVkStruct a => a -> IO (ForeignPtr a)
toPlainForeignPtr (VkStruct a b) = IO
    (\s -> (# s, ForeignPtr a (PlainPtr (unsafeCoerce# b)) #))
{-# INLINE toPlainForeignPtr #-}

-- | Make sure this data is alive at a given point in a sequence of IO actions.
touchVkData  :: IsVkStruct a => a -> IO ()
touchVkData (VkStruct _ b) = IO (\s -> (# touch# b s, () #))
{-# INLINE touchVkData #-}



type StructFieldNames (a :: Type) = FieldNames (StructFields a)

type family FieldNames (ms :: [FieldMeta]) :: [Symbol] where
    FieldNames '[] = '[]
    FieldNames (m ': ms) = FName m ': FieldNames ms

-- | A Constraint: a vulkan struct must have a field with a given name.
type HasField (fname :: Symbol) (a :: Type)
    = (VulkanMarshal a, VulkanField (FieldRep fname a))

-- | Type-level description of a Vulkan structure field.
type FieldRep (fname :: Symbol) (a :: Type)
    = GetFieldMeta (ErrorNoSuchField fname a) fname (StructFields a)

-- | Type of a field in a vulkan structure or union.
type FieldType (fname :: Symbol) (a :: Type)
    = FType (FieldRep fname a)

-- | Whether this field marked optional in vulkan specification.
--   Usually, this means that `VK_NULL` can be written in place
--   of this field.
type FieldOptional (fname :: Symbol) (a :: Type)
    = FOptional (FieldRep fname a)

-- | Offset of a field in bytes.
type FieldOffset (fname :: Symbol) (a :: Type)
    = FByteOffset (FieldRep fname a)

-- | Whether this field is a fixed-length array stored directly in a struct.
type FieldIsArray (fname :: Symbol) (a :: Type)
    = IsArrayLen (FLength (FieldRep fname a))

type family IsArrayLen (l :: Nat) :: Bool where
    IsArrayLen 1 = 'False
    IsArrayLen _ = 'True

-- | Length of an array that is a field of a structure or union
type FieldArrayLength (fname :: Symbol) (a :: Type)
    = FLength (FieldRep fname a)

type CanReadField (fname :: Symbol) (a :: Type)
    = ( HasField fname a
      , IsTrue (ErrorNotReadableField fname a)
               (FCanRead (FieldRep fname a))
      , Storable (FieldType fname a))

type CanWriteField (fname :: Symbol) (a :: Type)
    = ( HasField fname a
      , IsTrue (ErrorNotWritableField fname a)
               (FCanWrite (FieldRep fname a))
      , Storable (FieldType fname a))

type CanReadFieldArray (fname :: Symbol) (a :: Type)
    = CanReadField fname a

type CanWriteFieldArray (fname :: Symbol) (a :: Type)
    = CanWriteField fname a

instance VulkanMarshal (VkStruct a)
       => Eq (VkStruct a) where
    a == b = EQ == cmpBytes# (sizeOf a) (unsafeAddr a) (unsafeAddr b)
    {-# INLINE (==) #-}

instance VulkanMarshal (VkStruct a)
       => Ord (VkStruct a) where
    compare a b = cmpBytes# (sizeOf a) (unsafeAddr a) (unsafeAddr b)
    {-# INLINE compare #-}

instance VulkanMarshal (VkStruct a)
       => Storable (VkStruct a) where
    sizeOf ~_ = sSize @(StructRep (VkStruct a))
    {-# INLINE sizeOf #-}
    alignment ~_ = sAlign @(StructRep (VkStruct a))
    {-# INLINE alignment #-}
    peek (Ptr addr)
      | I# n <- sSize @(StructRep (VkStruct a))
      , I# a <- sAlign @(StructRep (VkStruct a))
      = IO
      (\s -> case newAlignedPinnedByteArray# n a s of
        (# s1, mba #) -> case copyAddrToByteArray# addr mba 0# n s1 of
          s2 -> case unsafeFreezeByteArray# mba s2 of
            (# s3, ba #) -> (# s3, unsafeFromByteArrayOffset 0# ba #)
      )
    {-# INLINE peek #-}
    poke (Ptr addr) x
      = c_memcpy addr (unsafeAddr x) (fromIntegral $ sSize @(StructRep (VkStruct a)))
    {-# INLINE poke #-}


instance VulkanMarshal (VkStruct a)
       => Show (VkStruct a) where
    showsPrec d x
      = showParen (d >= 11)
      $ (.) (showString (sName @(StructRep (VkStruct a))) . showString " {")
      $ (\(b, s) -> if b then dropIt . s else s )
      $ enumerateFields @(StructFields (VkStruct a))
        ( \(_ :: Proxy# m) s -> case isThatField @m of
            Refl ->
              ( True
              , sepIt . showString (fName @m)
                . showString " = " . showField @(FName m) @m
                . snd s
              )
        ) (False, showString "}")

      where
        (dropIt, sepIt) = if sIsUnion @(StructRep (VkStruct a))
                          then (drop 3, showString " | ")
                          else (drop 2, showString ", ")
        isThatField :: m :~: FieldRep (FName m) (VkStruct a)
        isThatField = unsafeCoerce (Refl :: m :~: m)
        showField :: forall (fname :: Symbol) (m :: FieldMeta)
                   . ( VulkanField m
                     , fname ~ FName m
                     , m ~ FieldRep fname (VkStruct a)
                     )
                  => ShowS
        showField = case fLength @m of
            0 -> showString "[]"
            1 -> shows @(FType m) (getF 0)
            m -> showChar '[' . drop 2
               . foldr (\i s -> showString ", " . shows @(FType m) (getF i) . s)
                       id [0..m-1]
               . showChar ']'
          where
            getF :: Int -> FType m
            getF i = unsafeDupablePerformIO $
              peekByteOff @(FType m) (unsafePtr x)
                (fByteOffset @m + i * sizeOf @(FType m) undefined)
            {-# NOINLINE getF #-}



-- | Whether this field marked optional in vulkan specification.
--   Usually, this means that `VK_NULL` can be written in place
--   of this field.
fieldOptional :: forall (fname :: Symbol) (a :: Type)
               . HasField fname a => Bool
fieldOptional = fOptional @(FieldRep fname a)

-- | Offset of a field in bytes.
fieldOffset :: forall (fname :: Symbol) (a :: Type)
             . HasField fname a => Int
fieldOffset = fByteOffset @(FieldRep fname a)

-- | Length of an array that is a field of a structure or union.
--
--   Returns @1@ if this field is not an array.
fieldArrayLength :: forall (fname :: Symbol) (a :: Type)
                  . HasField fname a => Int
fieldArrayLength = fLength @(FieldRep fname a)

getField :: forall (fname :: Symbol) (a :: Type)
          . CanReadField fname a => a -> FieldType fname a
getField x = unsafeDupablePerformIO $
    peekByteOff (unsafePtr x) (fieldOffset @fname @a)
{-# NOINLINE getField #-}

readField :: forall (fname :: Symbol) (a :: Type)
           . CanReadField fname a => Ptr a -> IO (FieldType fname a)
readField p = peekByteOff p (fieldOffset @fname @a)

writeField :: forall (fname :: Symbol) (a :: Type)
            . CanWriteField fname a => Ptr a -> FieldType fname a -> IO ()
writeField p = pokeByteOff p (fieldOffset @fname @a)

-- | Index an array-type field. No bound checks.
getFieldArrayUnsafe :: forall (fname :: Symbol) (a :: Type)
                     . CanReadFieldArray fname a => Int -> a -> FieldType fname a
getFieldArrayUnsafe i = f
  where
    off = fieldOffset @fname @a + i * sizeOf @(FieldType fname a) undefined
    f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
    {-# NOINLINE f #-}

-- | Read from an array-type field. No bound checks.
readFieldArrayUnsafe :: forall (fname :: Symbol) (a :: Type)
                      . CanReadFieldArray fname a => Int -> Ptr a -> IO (FieldType fname a)
readFieldArrayUnsafe i p = peekByteOff p off
  where
    off = fieldOffset @fname @a + i * sizeOf @(FieldType fname a) undefined

-- | Write to an array-type field. No bound checks.
writeFieldArrayUnsafe :: forall (fname :: Symbol) (a :: Type)
                       . CanWriteFieldArray fname a
                      => Int -> Ptr a -> FieldType fname a -> IO ()
writeFieldArrayUnsafe i p = pokeByteOff p off
  where
    off = fieldOffset @fname @a + i * sizeOf @(FieldType fname a) undefined


getFieldArray :: forall fname idx a
               . (CanReadFieldArray fname a, IndexInBounds fname idx a, KnownNat idx)
              => a -> FieldType fname a
getFieldArray = getFieldArrayUnsafe @fname @a
  (fromInteger $ natVal' (proxy# :: Proxy# idx))
{-# INLINE getFieldArray #-}

readFieldArray :: forall fname idx a
                . (CanReadFieldArray fname a, IndexInBounds fname idx a, KnownNat idx)
               => Ptr a -> IO (FieldType fname a)
readFieldArray = readFieldArrayUnsafe @fname @a
  (fromInteger $ natVal' (proxy# :: Proxy# idx))
{-# INLINE readFieldArray #-}


writeFieldArray :: forall fname idx a
                 . (CanWriteFieldArray fname a, IndexInBounds fname idx a, KnownNat idx)
                => Ptr a -> FieldType fname a -> IO ()
writeFieldArray = writeFieldArrayUnsafe @fname @a
  (fromInteger $ natVal' (proxy# :: Proxy# idx))
{-# INLINE writeFieldArray #-}



type IndexInBounds (s :: Symbol) (i :: Nat) (a :: Type)
  = IndexInBounds' s i a (CmpNat i (FieldArrayLength s a))

type family IndexInBounds' (s :: Symbol)
                           (i :: Nat)
                           (a :: Type) (r :: Ordering) :: Constraint where
  IndexInBounds' _ _ _ 'LT = ()
  IndexInBounds' s i a _ = TypeError ( ErrorIndexOutOfBounds s i a )


--------------------------------------------------------------------------------
-- * Type-level errors
--------------------------------------------------------------------------------

type family IsTrue (errMsg :: ErrorMessage) (bool :: Bool) :: Constraint where
  IsTrue _   'True  = ()
  IsTrue err 'False = TypeError err

type ErrorNoSuchField (s :: Symbol) (a :: Type)
  = 'Text "Structure " ':<>: 'ShowType a
  ':<>: 'Text " does not have field " ':<>: 'ShowType s ':<>: 'Text "."
  ':$$: 'Text "Note, this structure has following fields: "
        ':<>: 'ShowType (StructFieldNames a)

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
                 . ( CanReadFieldArray fname a
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
                   . ( CanReadFieldArray fname a
                     , FieldType fname a ~ CChar
                     , VulkanMarshal a
                     )
                   => a -> CString
unsafeCStringField x = unsafePtr x `plusPtr` fieldOffset @fname @a


getStringField :: forall fname a
                . ( CanReadFieldArray fname a
                  , FieldType fname a ~ CChar
                  , VulkanMarshal a
                  )
               => a -> String
getStringField x
    = case takeForce (fieldArrayLength @fname @a)
         . unsafeDupablePerformIO
         $ withCStringField @fname @a x peekCString of
        ((), s) -> s

readStringField :: forall fname a
                . ( CanReadFieldArray fname a
                  , FieldType fname a ~ CChar
                  , VulkanMarshal a
                  )
               => Ptr a -> IO String
readStringField px = do
  ((), s) <- takeForce (fieldArrayLength @fname @a)
         <$> peekCString (px `plusPtr` fieldOffset @fname @a)
  return s

writeStringField :: forall fname a
                  . ( CanWriteFieldArray fname a
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

-- | Internal function used to implement Eq and Ord instances for Vulkan structs.
--   Compares first n bytes of two memory areas.
--
--   Uses lexicographic ordering (c memcmp inside).
--
--   This is a helper that should be used in VulkanMarshal instances only.
cmpBytes# :: Int -> Addr# -> Addr# -> Ordering
cmpBytes# n a b
  | isTrue# (eqAddr# a b) = EQ
  | otherwise = c_memcmp a b (fromIntegral n) `compare` 0


foreign import ccall unsafe "memcmp"
  c_memcmp :: Addr# -> Addr# -> CSize -> CInt


foreign import ccall unsafe "memcpy"
  c_memcpy :: Addr# -> Addr# -> CSize -> IO ()
