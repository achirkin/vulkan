{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | This module is not part of auto-generated code based on vk.xml.
--   It is also not included into `Graphics.Vulkan`.
--   It just provides convenient helpers for creation of vulkan structures.
module Graphics.Vulkan.Marshal.Create
    ( CreateVkStruct ()
    , createVk, (&*)
    , set, setAt, setVk, setVkRef, setStr, setStrRef
    , setStrListRef, setStrListCountAndRef, setListRef, setListCountAndRef
    , SetOptionalFields (..), HandleRemainingFields (..), HandleRemFields
    , unsafeIOCreate
    ) where

import Data.Coerce
import Data.Kind             (Constraint, Type)
import Data.Type.Bool        (If, type (||))
import Data.Type.Equality    (type (==))
import Foreign.C.String      (newCString)
import Foreign.C.Types       (CChar)
import Foreign.Marshal.Alloc (finalizerFree, free)
import Foreign.Marshal.Array (newArray, pokeArray0)
import Foreign.Ptr           (nullPtr, plusPtr)
import Foreign.Storable      (Storable)
import GHC.Base              (ByteArray#, IO (..), RealWorld, State#, Weak#,
                              addCFinalizerToWeak#, mkWeak#, mkWeakNoFinalizer#,
                              nullAddr#)
import GHC.Ptr               (FunPtr (..), Ptr (..))
import GHC.TypeLits
import System.IO.Unsafe      (unsafeDupablePerformIO)

import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.BaseTypes  (VkBool32)


-- | Safely fill-in a new vulkan structure
newtype CreateVkStruct x (fs :: [Symbol]) a
  = CreateVkStruct
    { unCreateVkStruct
      :: Ptr x  -- Reference to memory containing the struct
      -> IO ( ( [Ptr ()]
                -- Pointers to data that was allocated with malloc or alike.
                -- The memory is guaranteed to be freed by means of finalizerFree
                --  at some point.
              , [IO ()]
                -- A list of functions that have a chance to run on finalization.
                -- GHC sometimes does not run them, but they are still suitable
                -- for touching GHC-managed memory to ensure an object exists
                -- at least as long as the created structure.
              )

           , a)
    }

-- | Unsafe perform arbitrary IO action with a pointer to data under construction.
--   This is used to add more functionality to this monad.
unsafeIOCreate :: (Ptr x -> IO a) -> CreateVkStruct x fs a
unsafeIOCreate k = CreateVkStruct $ fmap ((,) ([],[])) . k

instance Functor (CreateVkStruct x fs) where
  fmap f = CreateVkStruct . fmap (fmap $ fmap f) . unCreateVkStruct

instance Applicative (CreateVkStruct x fs) where
  pure = CreateVkStruct . const . pure . (,) ([],[])
  csf <*> csx = CreateVkStruct $ \ptr ->
      g <$> unCreateVkStruct csf ptr <*> unCreateVkStruct csx ptr
    where
      g ((as1, as2), f) ((bs1, bs2), x) = ((as1 ++ bs1, as2 ++ bs2), f x)

instance Monad (CreateVkStruct x fs) where
  return = pure
  csx >>= k = CreateVkStruct $ \ptr -> do
    ((as1, as2), x) <- unCreateVkStruct csx ptr
    ((bs1, bs2), y) <- unCreateVkStruct (k x) ptr
    return ((as1 ++ bs1, as2 ++ bs2), y)


-- | Create a vulkan structure.
--
--   Use smart creation functions like `setVk`, `setStrRef`, `setListRef`, etc
--   to keep GC aware of references between dependent structures.
--
--   `createVk` produces at most one weak reference to a created structure
--     with a set of haskell and C finalizers.
--   These finalizers make sure all `malloc`ed memory is released and
--    no managed memory gets purged too early.
createVk :: forall a fs .
            ( VulkanMarshal a
            , HandleRemFields a fs
            ) => CreateVkStruct a fs () -> a
createVk a = unsafeDupablePerformIO $ do
    x <- mallocVkData
    withPtr x $ \xptr -> do
      ((cDeps, hFins), ()) <- unCreateVkStruct (a >> handleRemFields) xptr
      IO $ \s0 -> case (# cDeps, hFins #) of
        (# [], [] #) -> (# s0, () #)
        (# _ , _  #) -> case mkW (unsafeByteArray x) hFins s0 of
          (# s1, w #) -> let go [] s = (# s, () #)
                             go xxs@(Ptr c : xs) s =
                               case addCFinalizerToWeak# fp c 0# nullAddr# w s of
                                 (# s' , 0# #) -> case mapM_ free xxs of
                                    IO k -> k s'
                                 (# s' , _  #) -> go xs s'
                         in go cDeps s1
    return x
  where
    !(FunPtr fp) = finalizerFree @()
    mkW :: ByteArray# -> [IO ()]
        -> State# RealWorld -> (# State# RealWorld, Weak# () #)
    mkW ba [] = mkWeakNoFinalizer# ba ()
    mkW ba xs = case sequence_ xs of
                  IO k -> mkWeak# ba () k
{-# NOINLINE createVk #-}

-- | `writeField` wrapped into `CreateVkStruct` monad.
set :: forall fname x . CanWriteField fname x
    => FieldType fname x -> CreateVkStruct x '[fname] ()
set v = CreateVkStruct $ \p -> (,) ([],[]) <$> writeField @fname @x p v


-- | `writeFieldArray` wrapped into `CreateVkStruct` monad.
setAt :: forall fname i x
       . ( CanWriteFieldArray fname x, IndexInBounds fname i x, KnownNat i)
      => FieldType fname x -> CreateVkStruct x '[fname] ()
setAt v = CreateVkStruct $ \p -> (,) ([],[]) <$> writeFieldArray @fname @i @x p v

-- | Write fields of a member struct.
setVk :: forall fname x afs a
       . ( CanWriteField fname x
         , a ~ FieldType fname x
         , VulkanMarshal a
         , HandleRemFields a afs
         )
      => CreateVkStruct a afs ()
      -> CreateVkStruct x '[fname] ()
setVk ma = CreateVkStruct $ \p ->
  unCreateVkStruct (ma >> handleRemFields) (plusPtr p (fieldOffset @fname @x))

-- | Write a String into a vulkan struct in-place.
setStr :: forall fname x
        . ( CanWriteFieldArray fname x
          , FieldType fname x ~ CChar
          )
       => String -> CreateVkStruct x '[fname] ()
setStr v = CreateVkStruct $ \p ->
  (,) ([],[]) <$> pokeArray0 '\0' (p `plusPtr` fieldOffset @fname @x) v

-- | Allocate memory for a CString, store it,
--    and write a pointer to it into vulkan structure.
--
--   This function also attaches a reliable finalizer to the vulkan struct,
--    so that the allocated memory is freed when the structure is GCed.
setStrRef :: forall fname x
           . ( CanWriteField fname x
             , FieldType fname x ~ CString
             )
          => String -> CreateVkStruct x '[fname] ()
setStrRef v = CreateVkStruct $ \p -> do
  sPtr <- newCString v
  (,) ([coerce sPtr],[]) <$> writeField @fname @x p sPtr

-- | Allocate memory for an array of elements, store them,
--    and write a pointer to the array into vulkan structure.
--
--   This function also attaches a reliable finalizer to the vulkan struct,
--    so that the array memory is freed when the structure is GCed.
--
--   This function writes null pointer if used with an empty list.
setListRef :: forall fname x a
            . ( CanWriteField fname x
              , FieldType fname x ~ Ptr a
              , Storable a
              )
           => [a] -> CreateVkStruct x '[fname] ()
setListRef [] = CreateVkStruct $ \p ->
  (,) ([],[]) <$> writeField @fname @x p nullPtr
setListRef v = CreateVkStruct $ \p -> do
  aPtr <- newArray v
  (,) ([coerce aPtr],[]) <$> writeField @fname @x p aPtr

-- | Equivalent to 'set' on a count field and 'setListRef' on a corresponding list field,
--    where the count is set to the length of the list.
setListCountAndRef :: forall countfname listfname x a
                  . ( CanWriteField countfname x
                    , CanWriteField listfname x
                    , FieldType countfname x ~ Word32
                    , FieldType listfname x ~ Ptr a
                    , Storable a
                    )
                  => [a] -> CreateVkStruct x (Union x '[countfname] '[listfname]) ()
setListCountAndRef list =
  set @countfname (fromIntegral $ length list) &*
  setListRef @listfname list

-- | Allocate memory for an array of elements, store them,
--    and write a pointer to the array into vulkan structure.
--
--   This function also attaches a reliable finalizer to the vulkan struct,
--    so that the array memory is freed when the structure is GCed.
--
--   This function writes null pointer if used with an empty list.
setStrListRef :: forall fname x
              . ( CanWriteField fname x
                , FieldType fname x ~ Ptr CString
                )
              => [String] -> CreateVkStruct x '[fname] ()
setStrListRef [] = CreateVkStruct $ \p ->
  (,) ([],[]) <$> writeField @fname @x p nullPtr
setStrListRef v = CreateVkStruct $ \p -> do
  strptrs <- mapM newCString v
  aPtr <- newArray strptrs
  (,) (coerce aPtr : coerce strptrs,[]) <$> writeField @fname @x p aPtr

-- | Equivalent to 'set' on a count field and 'setStrListRef' on a corresponding list field,
--    where the count is set to the length of the list.
setStrListCountAndRef :: forall countfname listfname x
                      . ( CanWriteField countfname x
                        , CanWriteField listfname x
                        , FieldType countfname x ~ Word32
                        , FieldType listfname x ~ Ptr CString
                        )
                      => [String] -> CreateVkStruct x (Union x '[countfname] '[listfname]) ()
setStrListCountAndRef list =
  set @countfname (fromIntegral $ length list) &*
  setStrListRef @listfname list

-- | Write a pointer to a vulkan structure - member of current structure
--    and make sure the member exists as long as this structure exists.
--
--   Prefer this function to using @unsafePtr a@, because the latter
--    does not keep the dependency information in GC, which results in
--    member structure being garbage-collected and the reference being invalid.
setVkRef :: forall fname x a
          . ( CanWriteField fname x
            , FieldType fname x ~ Ptr a
            , VulkanMarshal a
            )
         => a -> CreateVkStruct x '[fname] ()
setVkRef v = CreateVkStruct $ \p ->
  (,) ([],[touchVkData v]) <$> writeField @fname @x p (unsafePtr v)

-- | Combine multiple field writes.
infixl 1 &*
(&*) :: CreateVkStruct x as () -> CreateVkStruct x bs ()
     -> CreateVkStruct x (Union x as bs) ()
CreateVkStruct a &* CreateVkStruct b = CreateVkStruct a >> CreateVkStruct b



type family Union (x :: Type) (as :: [Symbol]) (bs :: [Symbol]) :: [Symbol] where
  Union _ as '[] = as
  Union x '[] (b ': bs) = Union x '[b] bs
  Union x (a ': as) (a ': bs) = If (FieldIsArray a x)
    ( Union x as (a ': bs) )
    ( TypeError
      ( 'Text "The same field " ':<>: 'ShowType x
      ':<>: 'Text " should not be set twice."
      )
    )
  Union x (a ': as) (b ': bs) = a ': Union x as (b ': bs)

type family Difference (as :: [Symbol]) (bs :: [Symbol]) :: [Symbol] where
  Difference '[] _  = '[]
  Difference as '[] = as
  Difference as (b ': bs) = Difference (Delete b as) bs


type family Delete (a :: Symbol) (as :: [Symbol]) :: [Symbol] where
  Delete _ '[] = '[]
  Delete a (a ': as) = as
  Delete a (b ': bs) = b ': Delete a bs



-- | Notify user if some required fields are missing.
type HandleRemFields x fs = HandleRemainingFields x fs (CUnionType x)


-- | Notify user if some required fields are missing and fill in optional fields.
class CUnionType x ~ isUnion
      => HandleRemainingFields (x :: Type) (fs :: [Symbol]) (isUnion :: Bool) where
  handleRemFields :: CreateVkStruct x fs ()



type SetUnionMsg x =
   'Text "You have to set exactly one field for a union type " ':<>: 'ShowType x
   ':$$: 'Text "Note, this type has following fields: "
         ':<>: 'ShowType (StructFieldNames x)

instance ( TypeError ( SetUnionMsg x )
         , CUnionType x ~ 'True
         ) => HandleRemainingFields x '[] 'True where
  handleRemFields = pure ()

instance CUnionType x ~ 'True => HandleRemainingFields x '[f] 'True where
  handleRemFields = pure ()

instance ( TypeError ( SetUnionMsg x )
         , CUnionType x ~ 'True
         ) => HandleRemainingFields x (a ': b ': fs) 'True where
  handleRemFields = pure ()


instance ( SetOptionalFields x (Difference (StructFieldNames x) fs)
         , CUnionType x ~ 'False
         ) => HandleRemainingFields x fs 'False where
  handleRemFields
    = ( coerce :: CreateVkStruct x (Difference (StructFieldNames x) fs) ()
               -> CreateVkStruct x fs ()
      ) setOptionalFields




class SetOptionalFields (x :: Type) (fs :: [Symbol]) where
  setOptionalFields :: CreateVkStruct x fs ()

instance SetOptionalFields x '[] where
  setOptionalFields = pure ()

instance ( SetOptionalFields x fs
         , FieldMustBeOptional f x
         , Storable (FieldType f x)
         , HasField f x
         ) => SetOptionalFields x (f ': fs) where
  setOptionalFields = coerce $ x >> setOptionalFields
    where
      x :: CreateVkStruct x fs ()
      x = CreateVkStruct $
        \ptr -> (,) ([],[]) <$>
                clearStorable ( plusPtr ptr (fieldOffset @f @x)
                                  :: Ptr (FieldType f x)
                              )

type family FieldMustBeOptional (f :: Symbol) (x :: Type) :: Constraint where
  FieldMustBeOptional f x
    = If (FieldOptional f x || FieldType f x == VkBool32) (() :: Constraint)
    ( TypeError
      ( 'Text "Non-optional field " ':<>: 'ShowType f
        ':<>: 'Text " of structure " ':<>: 'ShowType x
        ':<>: 'Text " is not set."
      )
    )
