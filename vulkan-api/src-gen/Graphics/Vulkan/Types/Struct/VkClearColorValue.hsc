#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.VkClearColorValue
       (VkClearColorValue(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Base                         (Addr##, ByteArray##, Proxy##,
                                                   byteArrayContents##,
                                                   plusAddr##, proxy##)
import           GHC.TypeLits                     (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | // Union allowing specification of floating point, integer, or unsigned integer color data. Actual value selected is based on image/attachment being cleared.
--
--   > typedef union VkClearColorValue {
--   >     float                  float32[4];
--   >     int32_t                int32[4];
--   >     uint32_t               uint32[4];
--   > } VkClearColorValue;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkClearColorValueVkClearColorValue registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         (KnownNat idx, IndexInBounds "float32" idx VkClearColorValue) =>
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
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkClearColorValue, float32} +
                      sizeOf (undefined :: #{type float}) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkClearColorValue, float32} +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx, IndexInBounds "float32" idx VkClearColorValue) =>
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
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkClearColorValue, float32} +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         (KnownNat idx, IndexInBounds "int32" idx VkClearColorValue) =>
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
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkClearColorValue, int32} +
                      sizeOf (undefined :: Int32) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkClearColorValue, int32} +
                 sizeOf (undefined :: Int32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx, IndexInBounds "int32" idx VkClearColorValue) =>
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
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkClearColorValue, int32} +
                 sizeOf (undefined :: Int32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         (KnownNat idx, IndexInBounds "uint32" idx VkClearColorValue) =>
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
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkClearColorValue, uint32} +
                      sizeOf (undefined :: Word32) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkClearColorValue, uint32} +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx, IndexInBounds "uint32" idx VkClearColorValue) =>
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
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkClearColorValue, uint32} +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance Show VkClearColorValue where
        showsPrec d x
          = showString "VkClearColorValue {" .
              (showString "float32 = [" .
                 showsPrec d
                   (let s = sizeOf
                              (undefined :: FieldType "float32" VkClearColorValue)
                        o = fieldOffset @"float32" @VkClearColorValue
                        f i
                          = peekByteOff (unsafePtr x) i ::
                              IO (FieldType "float32" VkClearColorValue)
                      in
                      unsafeDupablePerformIO . mapM f $
                        map (\ i -> o + i * s) [0 .. 4 - 1])
                   . showChar ']')
                .
                showString ", " .
                  (showString "int32 = [" .
                     showsPrec d
                       (let s = sizeOf (undefined :: FieldType "int32" VkClearColorValue)
                            o = fieldOffset @"int32" @VkClearColorValue
                            f i
                              = peekByteOff (unsafePtr x) i ::
                                  IO (FieldType "int32" VkClearColorValue)
                          in
                          unsafeDupablePerformIO . mapM f $
                            map (\ i -> o + i * s) [0 .. 4 - 1])
                       . showChar ']')
                    .
                    showString ", " .
                      (showString "uint32 = [" .
                         showsPrec d
                           (let s = sizeOf (undefined :: FieldType "uint32" VkClearColorValue)
                                o = fieldOffset @"uint32" @VkClearColorValue
                                f i
                                  = peekByteOff (unsafePtr x) i ::
                                      IO (FieldType "uint32" VkClearColorValue)
                              in
                              unsafeDupablePerformIO . mapM f $
                                map (\ i -> o + i * s) [0 .. 4 - 1])
                           . showChar ']')
                        . showChar '}'
