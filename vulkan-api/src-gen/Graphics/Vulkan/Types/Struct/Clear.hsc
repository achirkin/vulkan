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
module Graphics.Vulkan.Types.Struct.Clear
       (VkClearAttachment(..), VkClearColorValue(..),
        VkClearDepthStencilValue(..), VkClearRect(..), VkClearValue(..))
       where
import           Foreign.Storable                  (Storable (..))
import           GHC.Base                          (Addr##, ByteArray##, Proxy##,
                                                    byteArrayContents##,
                                                    plusAddr##, proxy##)
import           GHC.TypeLits                      (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Image  (VkImageAspectFlags)
import           Graphics.Vulkan.Types.Struct.Rect (VkRect2D)
import           System.IO.Unsafe                  (unsafeDupablePerformIO)

-- | > typedef struct VkClearAttachment {
--   >     VkImageAspectFlags     aspectMask;
--   >     uint32_t               colorAttachment;
--   >     VkClearValue           clearValue;
--   > } VkClearAttachment;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkClearAttachment VkClearAttachment registry at www.khronos.org>
data VkClearAttachment = VkClearAttachment## Addr## ByteArray##

instance Eq VkClearAttachment where
        (VkClearAttachment## a _) == x@(VkClearAttachment## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkClearAttachment where
        (VkClearAttachment## a _) `compare` x@(VkClearAttachment## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkClearAttachment where
        sizeOf ~_ = #{size VkClearAttachment}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkClearAttachment}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkClearAttachment where
        unsafeAddr (VkClearAttachment## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkClearAttachment## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkClearAttachment## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkClearAttachment where
        type StructFields VkClearAttachment =
             '["aspectMask", "colorAttachment", "clearValue"] -- ' closing tick for hsc2hs
        type CUnionType VkClearAttachment = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkClearAttachment = 'False -- ' closing tick for hsc2hs
        type StructExtends VkClearAttachment = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "aspectMask" VkClearAttachment where
        type FieldType "aspectMask" VkClearAttachment = VkImageAspectFlags
        type FieldOptional "aspectMask" VkClearAttachment = 'False -- ' closing tick for hsc2hs
        type FieldOffset "aspectMask" VkClearAttachment =
             #{offset VkClearAttachment, aspectMask}
        type FieldIsArray "aspectMask" VkClearAttachment = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearAttachment, aspectMask}

instance {-# OVERLAPPING #-}
         CanReadField "aspectMask" VkClearAttachment where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearAttachment, aspectMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkClearAttachment, aspectMask}

instance {-# OVERLAPPING #-}
         CanWriteField "aspectMask" VkClearAttachment where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearAttachment, aspectMask}

instance {-# OVERLAPPING #-}
         HasField "colorAttachment" VkClearAttachment where
        type FieldType "colorAttachment" VkClearAttachment = Word32
        type FieldOptional "colorAttachment" VkClearAttachment = 'False -- ' closing tick for hsc2hs
        type FieldOffset "colorAttachment" VkClearAttachment =
             #{offset VkClearAttachment, colorAttachment}
        type FieldIsArray "colorAttachment" VkClearAttachment = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkClearAttachment, colorAttachment}

instance {-# OVERLAPPING #-}
         CanReadField "colorAttachment" VkClearAttachment where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearAttachment, colorAttachment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkClearAttachment, colorAttachment}

instance {-# OVERLAPPING #-}
         CanWriteField "colorAttachment" VkClearAttachment where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearAttachment, colorAttachment}

instance {-# OVERLAPPING #-}
         HasField "clearValue" VkClearAttachment where
        type FieldType "clearValue" VkClearAttachment = VkClearValue
        type FieldOptional "clearValue" VkClearAttachment = 'False -- ' closing tick for hsc2hs
        type FieldOffset "clearValue" VkClearAttachment =
             #{offset VkClearAttachment, clearValue}
        type FieldIsArray "clearValue" VkClearAttachment = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearAttachment, clearValue}

instance {-# OVERLAPPING #-}
         CanReadField "clearValue" VkClearAttachment where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearAttachment, clearValue})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkClearAttachment, clearValue}

instance {-# OVERLAPPING #-}
         CanWriteField "clearValue" VkClearAttachment where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearAttachment, clearValue}

instance Show VkClearAttachment where
        showsPrec d x
          = showString "VkClearAttachment {" .
              showString "aspectMask = " .
                showsPrec d (getField @"aspectMask" x) .
                  showString ", " .
                    showString "colorAttachment = " .
                      showsPrec d (getField @"colorAttachment" x) .
                        showString ", " .
                          showString "clearValue = " .
                            showsPrec d (getField @"clearValue" x) . showChar '}'

-- | // Union allowing specification of floating point, integer, or unsigned integer color data. Actual value selected is based on image/attachment being cleared.
--
--   > typedef union VkClearColorValue {
--   >     float                  float32[4];
--   >     int32_t                int32[4];
--   >     uint32_t               uint32[4];
--   > } VkClearColorValue;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkClearColorValue VkClearColorValue registry at www.khronos.org>
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

-- | > typedef struct VkClearDepthStencilValue {
--   >     float                  depth;
--   >     uint32_t               stencil;
--   > } VkClearDepthStencilValue;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkClearDepthStencilValue VkClearDepthStencilValue registry at www.khronos.org>
data VkClearDepthStencilValue = VkClearDepthStencilValue## Addr##
                                                          ByteArray##

instance Eq VkClearDepthStencilValue where
        (VkClearDepthStencilValue## a _) ==
          x@(VkClearDepthStencilValue## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkClearDepthStencilValue where
        (VkClearDepthStencilValue## a _) `compare`
          x@(VkClearDepthStencilValue## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkClearDepthStencilValue where
        sizeOf ~_ = #{size VkClearDepthStencilValue}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkClearDepthStencilValue}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkClearDepthStencilValue where
        unsafeAddr (VkClearDepthStencilValue## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkClearDepthStencilValue## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkClearDepthStencilValue## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkClearDepthStencilValue where
        type StructFields VkClearDepthStencilValue = '["depth", "stencil"] -- ' closing tick for hsc2hs
        type CUnionType VkClearDepthStencilValue = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkClearDepthStencilValue = 'False -- ' closing tick for hsc2hs
        type StructExtends VkClearDepthStencilValue = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "depth" VkClearDepthStencilValue where
        type FieldType "depth" VkClearDepthStencilValue =
             #{type float}
        type FieldOptional "depth" VkClearDepthStencilValue = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depth" VkClearDepthStencilValue =
             #{offset VkClearDepthStencilValue, depth}
        type FieldIsArray "depth" VkClearDepthStencilValue = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearDepthStencilValue, depth}

instance {-# OVERLAPPING #-}
         CanReadField "depth" VkClearDepthStencilValue where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearDepthStencilValue, depth})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkClearDepthStencilValue, depth}

instance {-# OVERLAPPING #-}
         CanWriteField "depth" VkClearDepthStencilValue where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearDepthStencilValue, depth}

instance {-# OVERLAPPING #-}
         HasField "stencil" VkClearDepthStencilValue where
        type FieldType "stencil" VkClearDepthStencilValue = Word32
        type FieldOptional "stencil" VkClearDepthStencilValue = 'False -- ' closing tick for hsc2hs
        type FieldOffset "stencil" VkClearDepthStencilValue =
             #{offset VkClearDepthStencilValue, stencil}
        type FieldIsArray "stencil" VkClearDepthStencilValue = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkClearDepthStencilValue, stencil}

instance {-# OVERLAPPING #-}
         CanReadField "stencil" VkClearDepthStencilValue where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearDepthStencilValue, stencil})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkClearDepthStencilValue, stencil}

instance {-# OVERLAPPING #-}
         CanWriteField "stencil" VkClearDepthStencilValue where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearDepthStencilValue, stencil}

instance Show VkClearDepthStencilValue where
        showsPrec d x
          = showString "VkClearDepthStencilValue {" .
              showString "depth = " .
                showsPrec d (getField @"depth" x) .
                  showString ", " .
                    showString "stencil = " .
                      showsPrec d (getField @"stencil" x) . showChar '}'

-- | > typedef struct VkClearRect {
--   >     VkRect2D       rect;
--   >     uint32_t       baseArrayLayer;
--   >     uint32_t       layerCount;
--   > } VkClearRect;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkClearRect VkClearRect registry at www.khronos.org>
data VkClearRect = VkClearRect## Addr## ByteArray##

instance Eq VkClearRect where
        (VkClearRect## a _) == x@(VkClearRect## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkClearRect where
        (VkClearRect## a _) `compare` x@(VkClearRect## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkClearRect where
        sizeOf ~_ = #{size VkClearRect}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkClearRect}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkClearRect where
        unsafeAddr (VkClearRect## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkClearRect## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkClearRect## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkClearRect where
        type StructFields VkClearRect =
             '["rect", "baseArrayLayer", "layerCount"] -- ' closing tick for hsc2hs
        type CUnionType VkClearRect = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkClearRect = 'False -- ' closing tick for hsc2hs
        type StructExtends VkClearRect = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "rect" VkClearRect where
        type FieldType "rect" VkClearRect = VkRect2D
        type FieldOptional "rect" VkClearRect = 'False -- ' closing tick for hsc2hs
        type FieldOffset "rect" VkClearRect =
             #{offset VkClearRect, rect}
        type FieldIsArray "rect" VkClearRect = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearRect, rect}

instance {-# OVERLAPPING #-} CanReadField "rect" VkClearRect where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearRect, rect})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkClearRect, rect}

instance {-# OVERLAPPING #-} CanWriteField "rect" VkClearRect where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearRect, rect}

instance {-# OVERLAPPING #-} HasField "baseArrayLayer" VkClearRect
         where
        type FieldType "baseArrayLayer" VkClearRect = Word32
        type FieldOptional "baseArrayLayer" VkClearRect = 'False -- ' closing tick for hsc2hs
        type FieldOffset "baseArrayLayer" VkClearRect =
             #{offset VkClearRect, baseArrayLayer}
        type FieldIsArray "baseArrayLayer" VkClearRect = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearRect, baseArrayLayer}

instance {-# OVERLAPPING #-}
         CanReadField "baseArrayLayer" VkClearRect where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearRect, baseArrayLayer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkClearRect, baseArrayLayer}

instance {-# OVERLAPPING #-}
         CanWriteField "baseArrayLayer" VkClearRect where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearRect, baseArrayLayer}

instance {-# OVERLAPPING #-} HasField "layerCount" VkClearRect
         where
        type FieldType "layerCount" VkClearRect = Word32
        type FieldOptional "layerCount" VkClearRect = 'False -- ' closing tick for hsc2hs
        type FieldOffset "layerCount" VkClearRect =
             #{offset VkClearRect, layerCount}
        type FieldIsArray "layerCount" VkClearRect = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearRect, layerCount}

instance {-# OVERLAPPING #-} CanReadField "layerCount" VkClearRect
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearRect, layerCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkClearRect, layerCount}

instance {-# OVERLAPPING #-} CanWriteField "layerCount" VkClearRect
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearRect, layerCount}

instance Show VkClearRect where
        showsPrec d x
          = showString "VkClearRect {" .
              showString "rect = " .
                showsPrec d (getField @"rect" x) .
                  showString ", " .
                    showString "baseArrayLayer = " .
                      showsPrec d (getField @"baseArrayLayer" x) .
                        showString ", " .
                          showString "layerCount = " .
                            showsPrec d (getField @"layerCount" x) . showChar '}'

-- | // Union allowing specification of color or depth and stencil values. Actual value selected is based on attachment being cleared.
--
--   > typedef union VkClearValue {
--   >     VkClearColorValue      color;
--   >     VkClearDepthStencilValue depthStencil;
--   > } VkClearValue;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkClearValue VkClearValue registry at www.khronos.org>
data VkClearValue = VkClearValue## Addr## ByteArray##

instance Eq VkClearValue where
        (VkClearValue## a _) == x@(VkClearValue## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkClearValue where
        (VkClearValue## a _) `compare` x@(VkClearValue## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkClearValue where
        sizeOf ~_ = #{size VkClearValue}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkClearValue}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkClearValue where
        unsafeAddr (VkClearValue## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkClearValue## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkClearValue## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkClearValue where
        type StructFields VkClearValue = '["color", "depthStencil"] -- ' closing tick for hsc2hs
        type CUnionType VkClearValue = 'True -- ' closing tick for hsc2hs
        type ReturnedOnly VkClearValue = 'False -- ' closing tick for hsc2hs
        type StructExtends VkClearValue = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "color" VkClearValue where
        type FieldType "color" VkClearValue = VkClearColorValue
        type FieldOptional "color" VkClearValue = 'False -- ' closing tick for hsc2hs
        type FieldOffset "color" VkClearValue =
             #{offset VkClearValue, color}
        type FieldIsArray "color" VkClearValue = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearValue, color}

instance {-# OVERLAPPING #-} CanReadField "color" VkClearValue
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearValue, color})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkClearValue, color}

instance {-# OVERLAPPING #-} CanWriteField "color" VkClearValue
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearValue, color}

instance {-# OVERLAPPING #-} HasField "depthStencil" VkClearValue
         where
        type FieldType "depthStencil" VkClearValue =
             VkClearDepthStencilValue
        type FieldOptional "depthStencil" VkClearValue = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthStencil" VkClearValue =
             #{offset VkClearValue, depthStencil}
        type FieldIsArray "depthStencil" VkClearValue = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearValue, depthStencil}

instance {-# OVERLAPPING #-}
         CanReadField "depthStencil" VkClearValue where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearValue, depthStencil})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkClearValue, depthStencil}

instance {-# OVERLAPPING #-}
         CanWriteField "depthStencil" VkClearValue where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearValue, depthStencil}

instance Show VkClearValue where
        showsPrec d x
          = showString "VkClearValue {" .
              showString "color = " .
                showsPrec d (getField @"color" x) .
                  showString ", " .
                    showString "depthStencil = " .
                      showsPrec d (getField @"depthStencil" x) . showChar '}'
