{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
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
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE ViewPatterns               #-}
-- | This module is not part of auto-generated code based on vk.xml.
--   Instead, it is hand-written to provide common types and classes.
module Graphics.Vulkan.Marshal
  ( FlagType (..), FlagMask, FlagBit
  , bitToMask, maskToBits
  , VulkanMarshal (..)
  , newVkData, mallocVkData, mallocVkDataArray, unsafePtr
  , fromForeignPtr, toForeignPtr, toPlainForeignPtr, touchVkData
  , VulkanPtr (..)
  , VkPtr (..)
  , pattern VK_ZERO_FLAGS
  , pattern VK_NULL_HANDLE, pattern VK_NULL
  , clearStorable, withPtr
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

import Data.Bits             (Bits (..))
import Data.Coerce
import Data.Int              (Int16, Int32, Int64, Int8)
import Data.Void             (Void)
import Data.Word             (Word16, Word32, Word64, Word8)
import Foreign.C.String      (CString)
import Foreign.C.Types       (CChar (..), CInt (..), CSize (..), CULong (..),
                              CWchar (..))
import Foreign.ForeignPtr    (addForeignPtrFinalizer, mallocForeignPtr,
                              withForeignPtr)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr           (FunPtr, nullPtr)
import Foreign.Storable
import GHC.Ptr               (Ptr (..))


import Graphics.Vulkan.Marshal.Internal


-- | Distinguish single bits and bitmasks in vulkan flags
data FlagType = FlagMask | FlagBit

-- | Vulkan flags type that can have multiple bits set.
type FlagMask = 'FlagMask
-- | Vulkan single bit flag value.
type FlagBit  = 'FlagBit

-- | A synonym for `zeroBits`
pattern VK_ZERO_FLAGS :: Bits a => a
pattern VK_ZERO_FLAGS <- (popCount -> 0)
  where
    VK_ZERO_FLAGS = zeroBits

-- | Convert a single bit (@XxxBits@) to a bitmask (@XxxFlags@)
bitToMask :: Coercible (x FlagBit) (x FlagMask) => x FlagBit -> x FlagMask
bitToMask = coerce

-- | List all set bits of a bitmask (@XxxFlags@) in the increasing order.
maskToBits :: (Bits (x FlagMask), Coercible (x FlagBit) (x FlagMask))
           => x FlagMask -> [x FlagBit]
maskToBits x = go (popCount x) (bit 0)
  where
    zero = zeroBits
    go 0 _ = []
    go n i = let b = i .&. x
                 i' = unsafeShiftL i 1
             in if b == zero
                then go n i'
                else coerce b : go (n-1) i'


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
  deriving (Eq, Ord, Show, Storable)
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
