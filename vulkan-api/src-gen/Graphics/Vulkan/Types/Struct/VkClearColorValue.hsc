#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.VkClearColorValue
       (VkClearColorValue(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                        (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | // Union allowing specification of floating point, integer, or unsigned integer color data. Actual value selected is based on image/attachment being cleared.
--
--   > typedef union VkClearColorValue {
--   >     float                  float32[4];
--   >     int32_t                int32[4];
--   >     uint32_t               uint32[4];
--   > } VkClearColorValue;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkClearColorValue.html VkClearColorValue registry at www.khronos.org>
data VkClearColorValue = VkClearColorValue## Addr## ByteArray##

instance Eq VkClearColorValue where
        (VkClearColorValue## a _) == x@(VkClearColorValue## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkClearColorValue where
        (VkClearColorValue## a _) `compare` x@(VkClearColorValue## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkClearColorValue where
        sizeOf ~_ = #{size VkClearColorValue}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkClearColorValue}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkClearColorValue where
        unsafeAddr (VkClearColorValue## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkClearColorValue## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkClearColorValue## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkClearColorValue where
        type StructFields VkClearColorValue =
             '["float32", "int32", "uint32"] -- ' closing tick for hsc2hs
        type CUnionType VkClearColorValue = 'True -- ' closing tick for hsc2hs
        type ReturnedOnly VkClearColorValue = 'False -- ' closing tick for hsc2hs
        type StructExtends VkClearColorValue = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkFloat32Array VkClearColorValue
         where
        type VkFloat32ArrayMType VkClearColorValue =
             #{type float}

        {-# NOINLINE vkFloat32Array #-}
        vkFloat32Array x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: #{type float}) +
                    #{offset VkClearColorValue, float32}))

        {-# INLINE vkFloat32ArrayByteOffset #-}
        vkFloat32ArrayByteOffset ~_
          = #{offset VkClearColorValue, float32}

        {-# INLINE readVkFloat32Array #-}
        readVkFloat32Array p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkClearColorValue, float32})

        {-# INLINE writeVkFloat32Array #-}
        writeVkFloat32Array p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkClearColorValue, float32})

instance {-# OVERLAPPING #-} HasField "float32" VkClearColorValue
         where
        type FieldType "float32" VkClearColorValue =
             #{type float}
        type FieldOptional "float32" VkClearColorValue = 'False -- ' closing tick for hsc2hs
        type FieldOffset "float32" VkClearColorValue =
             #{offset VkClearColorValue, float32}
        type FieldIsArray "float32" VkClearColorValue = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearColorValue, float32}

instance (KnownNat idx,
          IndexInBounds "float32" idx VkClearColorValue) =>
         CanReadFieldArray "float32" idx VkClearColorValue
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "float32" 0 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "float32" 1 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "float32" 2 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "float32" 3 VkClearColorValue #-}
        type FieldArrayLength "float32" VkClearColorValue = 4

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 4

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkFloat32Array x (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkFloat32Array x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "float32" idx VkClearColorValue) =>
         CanWriteFieldArray "float32" idx VkClearColorValue
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "float32" 0 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "float32" 1 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "float32" 2 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "float32" 3 VkClearColorValue #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray x
          = writeVkFloat32Array x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkInt32Array VkClearColorValue
         where
        type VkInt32ArrayMType VkClearColorValue = Int32

        {-# NOINLINE vkInt32Array #-}
        vkInt32Array x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: Int32) +
                    #{offset VkClearColorValue, int32}))

        {-# INLINE vkInt32ArrayByteOffset #-}
        vkInt32ArrayByteOffset ~_
          = #{offset VkClearColorValue, int32}

        {-# INLINE readVkInt32Array #-}
        readVkInt32Array p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: Int32) +
                 #{offset VkClearColorValue, int32})

        {-# INLINE writeVkInt32Array #-}
        writeVkInt32Array p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: Int32) +
                 #{offset VkClearColorValue, int32})

instance {-# OVERLAPPING #-} HasField "int32" VkClearColorValue
         where
        type FieldType "int32" VkClearColorValue = Int32
        type FieldOptional "int32" VkClearColorValue = 'False -- ' closing tick for hsc2hs
        type FieldOffset "int32" VkClearColorValue =
             #{offset VkClearColorValue, int32}
        type FieldIsArray "int32" VkClearColorValue = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearColorValue, int32}

instance (KnownNat idx,
          IndexInBounds "int32" idx VkClearColorValue) =>
         CanReadFieldArray "int32" idx VkClearColorValue
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "int32" 0 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "int32" 1 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "int32" 2 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "int32" 3 VkClearColorValue #-}
        type FieldArrayLength "int32" VkClearColorValue = 4

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 4

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkInt32Array x (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkInt32Array x (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "int32" idx VkClearColorValue) =>
         CanWriteFieldArray "int32" idx VkClearColorValue
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "int32" 0 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "int32" 1 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "int32" 2 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "int32" 3 VkClearColorValue #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray x
          = writeVkInt32Array x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkUint32Array VkClearColorValue
         where
        type VkUint32ArrayMType VkClearColorValue = Word32

        {-# NOINLINE vkUint32Array #-}
        vkUint32Array x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: Word32) +
                    #{offset VkClearColorValue, uint32}))

        {-# INLINE vkUint32ArrayByteOffset #-}
        vkUint32ArrayByteOffset ~_
          = #{offset VkClearColorValue, uint32}

        {-# INLINE readVkUint32Array #-}
        readVkUint32Array p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: Word32) +
                 #{offset VkClearColorValue, uint32})

        {-# INLINE writeVkUint32Array #-}
        writeVkUint32Array p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: Word32) +
                 #{offset VkClearColorValue, uint32})

instance {-# OVERLAPPING #-} HasField "uint32" VkClearColorValue
         where
        type FieldType "uint32" VkClearColorValue = Word32
        type FieldOptional "uint32" VkClearColorValue = 'False -- ' closing tick for hsc2hs
        type FieldOffset "uint32" VkClearColorValue =
             #{offset VkClearColorValue, uint32}
        type FieldIsArray "uint32" VkClearColorValue = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearColorValue, uint32}

instance (KnownNat idx,
          IndexInBounds "uint32" idx VkClearColorValue) =>
         CanReadFieldArray "uint32" idx VkClearColorValue
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "uint32" 0 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "uint32" 1 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "uint32" 2 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "uint32" 3 VkClearColorValue #-}
        type FieldArrayLength "uint32" VkClearColorValue = 4

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 4

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkUint32Array x (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkUint32Array x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "uint32" idx VkClearColorValue) =>
         CanWriteFieldArray "uint32" idx VkClearColorValue
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "uint32" 0 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "uint32" 1 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "uint32" 2 VkClearColorValue #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "uint32" 3 VkClearColorValue #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray x
          = writeVkUint32Array x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance Show VkClearColorValue where
        showsPrec d x
          = showString "VkClearColorValue {" .
              showString "vkFloat32Array = [" .
                showsPrec d (map (vkFloat32Array x) [1 .. 4]) .
                  showChar ']' .
                    showString ", " .
                      showString "vkInt32Array = [" .
                        showsPrec d (map (vkInt32Array x) [1 .. 4]) .
                          showChar ']' .
                            showString ", " .
                              showString "vkUint32Array = [" .
                                showsPrec d (map (vkUint32Array x) [1 .. 4]) .
                                  showChar ']' . showChar '}'
