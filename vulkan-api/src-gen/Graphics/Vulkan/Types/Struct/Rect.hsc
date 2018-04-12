#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Rect
       (VkRect2D(..), VkRectLayerKHR(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Base                            (Addr##, ByteArray##,
                                                      byteArrayContents##,
                                                      plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.Extent (VkExtent2D)
import           Graphics.Vulkan.Types.Struct.Offset (VkOffset2D)
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkRect2D {
--   >     VkOffset2D     offset;
--   >     VkExtent2D     extent;
--   > } VkRect2D;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkRect2D VkRect2D registry at www.khronos.org>
data VkRect2D = VkRect2D## Addr## ByteArray##

instance Eq VkRect2D where
        (VkRect2D## a _) == x@(VkRect2D## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRect2D where
        (VkRect2D## a _) `compare` x@(VkRect2D## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRect2D where
        sizeOf ~_ = #{size VkRect2D}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkRect2D}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkRect2D where
        unsafeAddr (VkRect2D## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkRect2D## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRect2D## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkRect2D where
        type StructFields VkRect2D = '["offset", "extent"] -- ' closing tick for hsc2hs
        type CUnionType VkRect2D = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRect2D = 'False -- ' closing tick for hsc2hs
        type StructExtends VkRect2D = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "offset" VkRect2D where
        type FieldType "offset" VkRect2D = VkOffset2D
        type FieldOptional "offset" VkRect2D = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkRect2D =
             #{offset VkRect2D, offset}
        type FieldIsArray "offset" VkRect2D = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRect2D, offset}

instance {-# OVERLAPPING #-} CanReadField "offset" VkRect2D where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRect2D, offset})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkRect2D, offset}

instance {-# OVERLAPPING #-} CanWriteField "offset" VkRect2D where
        {-# INLINE writeField #-}
        writeField p = pokeByteOff p #{offset VkRect2D, offset}

instance {-# OVERLAPPING #-} HasField "extent" VkRect2D where
        type FieldType "extent" VkRect2D = VkExtent2D
        type FieldOptional "extent" VkRect2D = 'False -- ' closing tick for hsc2hs
        type FieldOffset "extent" VkRect2D =
             #{offset VkRect2D, extent}
        type FieldIsArray "extent" VkRect2D = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRect2D, extent}

instance {-# OVERLAPPING #-} CanReadField "extent" VkRect2D where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRect2D, extent})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkRect2D, extent}

instance {-# OVERLAPPING #-} CanWriteField "extent" VkRect2D where
        {-# INLINE writeField #-}
        writeField p = pokeByteOff p #{offset VkRect2D, extent}

instance Show VkRect2D where
        showsPrec d x
          = showString "VkRect2D {" .
              showString "offset = " .
                showsPrec d (getField @"offset" x) .
                  showString ", " .
                    showString "extent = " .
                      showsPrec d (getField @"extent" x) . showChar '}'

-- | > typedef struct VkRectLayerKHR {
--   >     VkOffset2D                       offset;
--   >     VkExtent2D                       extent;
--   >     uint32_t                         layer;
--   > } VkRectLayerKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkRectLayerKHR VkRectLayerKHR registry at www.khronos.org>
data VkRectLayerKHR = VkRectLayerKHR## Addr## ByteArray##

instance Eq VkRectLayerKHR where
        (VkRectLayerKHR## a _) == x@(VkRectLayerKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRectLayerKHR where
        (VkRectLayerKHR## a _) `compare` x@(VkRectLayerKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRectLayerKHR where
        sizeOf ~_ = #{size VkRectLayerKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkRectLayerKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkRectLayerKHR where
        unsafeAddr (VkRectLayerKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkRectLayerKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRectLayerKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkRectLayerKHR where
        type StructFields VkRectLayerKHR = '["offset", "extent", "layer"] -- ' closing tick for hsc2hs
        type CUnionType VkRectLayerKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRectLayerKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkRectLayerKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "offset" VkRectLayerKHR where
        type FieldType "offset" VkRectLayerKHR = VkOffset2D
        type FieldOptional "offset" VkRectLayerKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkRectLayerKHR =
             #{offset VkRectLayerKHR, offset}
        type FieldIsArray "offset" VkRectLayerKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRectLayerKHR, offset}

instance {-# OVERLAPPING #-} CanReadField "offset" VkRectLayerKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRectLayerKHR, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRectLayerKHR, offset}

instance {-# OVERLAPPING #-} CanWriteField "offset" VkRectLayerKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRectLayerKHR, offset}

instance {-# OVERLAPPING #-} HasField "extent" VkRectLayerKHR where
        type FieldType "extent" VkRectLayerKHR = VkExtent2D
        type FieldOptional "extent" VkRectLayerKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "extent" VkRectLayerKHR =
             #{offset VkRectLayerKHR, extent}
        type FieldIsArray "extent" VkRectLayerKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRectLayerKHR, extent}

instance {-# OVERLAPPING #-} CanReadField "extent" VkRectLayerKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRectLayerKHR, extent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRectLayerKHR, extent}

instance {-# OVERLAPPING #-} CanWriteField "extent" VkRectLayerKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRectLayerKHR, extent}

instance {-# OVERLAPPING #-} HasField "layer" VkRectLayerKHR where
        type FieldType "layer" VkRectLayerKHR = Word32
        type FieldOptional "layer" VkRectLayerKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "layer" VkRectLayerKHR =
             #{offset VkRectLayerKHR, layer}
        type FieldIsArray "layer" VkRectLayerKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRectLayerKHR, layer}

instance {-# OVERLAPPING #-} CanReadField "layer" VkRectLayerKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRectLayerKHR, layer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRectLayerKHR, layer}

instance {-# OVERLAPPING #-} CanWriteField "layer" VkRectLayerKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRectLayerKHR, layer}

instance Show VkRectLayerKHR where
        showsPrec d x
          = showString "VkRectLayerKHR {" .
              showString "offset = " .
                showsPrec d (getField @"offset" x) .
                  showString ", " .
                    showString "extent = " .
                      showsPrec d (getField @"extent" x) .
                        showString ", " .
                          showString "layer = " .
                            showsPrec d (getField @"layer" x) . showChar '}'
