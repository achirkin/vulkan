#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Extent
       (VkExtent2D(..), VkExtent3D(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Base                         (Addr##, ByteArray##,
                                                   byteArrayContents##,
                                                   plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkExtent2D {
--   >     uint32_t        width;
--   >     uint32_t        height;
--   > } VkExtent2D;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExtent2D VkExtent2D registry at www.khronos.org>
data VkExtent2D = VkExtent2D## Addr## ByteArray##

instance Eq VkExtent2D where
        (VkExtent2D## a _) == x@(VkExtent2D## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExtent2D where
        (VkExtent2D## a _) `compare` x@(VkExtent2D## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExtent2D where
        sizeOf ~_ = #{size VkExtent2D}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExtent2D}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExtent2D where
        unsafeAddr (VkExtent2D## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExtent2D## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExtent2D## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExtent2D where
        type StructFields VkExtent2D = '["width", "height"] -- ' closing tick for hsc2hs
        type CUnionType VkExtent2D = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExtent2D = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExtent2D = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "width" VkExtent2D where
        type FieldType "width" VkExtent2D = Word32
        type FieldOptional "width" VkExtent2D = 'False -- ' closing tick for hsc2hs
        type FieldOffset "width" VkExtent2D =
             #{offset VkExtent2D, width}
        type FieldIsArray "width" VkExtent2D = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkExtent2D, width}

instance {-# OVERLAPPING #-} CanReadField "width" VkExtent2D where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExtent2D, width})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkExtent2D, width}

instance {-# OVERLAPPING #-} CanWriteField "width" VkExtent2D where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExtent2D, width}

instance {-# OVERLAPPING #-} HasField "height" VkExtent2D where
        type FieldType "height" VkExtent2D = Word32
        type FieldOptional "height" VkExtent2D = 'False -- ' closing tick for hsc2hs
        type FieldOffset "height" VkExtent2D =
             #{offset VkExtent2D, height}
        type FieldIsArray "height" VkExtent2D = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkExtent2D, height}

instance {-# OVERLAPPING #-} CanReadField "height" VkExtent2D where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExtent2D, height})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExtent2D, height}

instance {-# OVERLAPPING #-} CanWriteField "height" VkExtent2D
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExtent2D, height}

instance Show VkExtent2D where
        showsPrec d x
          = showString "VkExtent2D {" .
              showString "width = " .
                showsPrec d (getField @"width" x) .
                  showString ", " .
                    showString "height = " .
                      showsPrec d (getField @"height" x) . showChar '}'

-- | > typedef struct VkExtent3D {
--   >     uint32_t        width;
--   >     uint32_t        height;
--   >     uint32_t        depth;
--   > } VkExtent3D;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExtent3D VkExtent3D registry at www.khronos.org>
data VkExtent3D = VkExtent3D## Addr## ByteArray##

instance Eq VkExtent3D where
        (VkExtent3D## a _) == x@(VkExtent3D## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExtent3D where
        (VkExtent3D## a _) `compare` x@(VkExtent3D## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExtent3D where
        sizeOf ~_ = #{size VkExtent3D}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExtent3D}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExtent3D where
        unsafeAddr (VkExtent3D## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExtent3D## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExtent3D## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExtent3D where
        type StructFields VkExtent3D = '["width", "height", "depth"] -- ' closing tick for hsc2hs
        type CUnionType VkExtent3D = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExtent3D = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExtent3D = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "width" VkExtent3D where
        type FieldType "width" VkExtent3D = Word32
        type FieldOptional "width" VkExtent3D = 'False -- ' closing tick for hsc2hs
        type FieldOffset "width" VkExtent3D =
             #{offset VkExtent3D, width}
        type FieldIsArray "width" VkExtent3D = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkExtent3D, width}

instance {-# OVERLAPPING #-} CanReadField "width" VkExtent3D where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExtent3D, width})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkExtent3D, width}

instance {-# OVERLAPPING #-} CanWriteField "width" VkExtent3D where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExtent3D, width}

instance {-# OVERLAPPING #-} HasField "height" VkExtent3D where
        type FieldType "height" VkExtent3D = Word32
        type FieldOptional "height" VkExtent3D = 'False -- ' closing tick for hsc2hs
        type FieldOffset "height" VkExtent3D =
             #{offset VkExtent3D, height}
        type FieldIsArray "height" VkExtent3D = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkExtent3D, height}

instance {-# OVERLAPPING #-} CanReadField "height" VkExtent3D where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExtent3D, height})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExtent3D, height}

instance {-# OVERLAPPING #-} CanWriteField "height" VkExtent3D
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExtent3D, height}

instance {-# OVERLAPPING #-} HasField "depth" VkExtent3D where
        type FieldType "depth" VkExtent3D = Word32
        type FieldOptional "depth" VkExtent3D = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depth" VkExtent3D =
             #{offset VkExtent3D, depth}
        type FieldIsArray "depth" VkExtent3D = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkExtent3D, depth}

instance {-# OVERLAPPING #-} CanReadField "depth" VkExtent3D where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExtent3D, depth})

        {-# INLINE readField #-}
        readField p = peekByteOff p #{offset VkExtent3D, depth}

instance {-# OVERLAPPING #-} CanWriteField "depth" VkExtent3D where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExtent3D, depth}

instance Show VkExtent3D where
        showsPrec d x
          = showString "VkExtent3D {" .
              showString "width = " .
                showsPrec d (getField @"width" x) .
                  showString ", " .
                    showString "height = " .
                      showsPrec d (getField @"height" x) .
                        showString ", " .
                          showString "depth = " .
                            showsPrec d (getField @"depth" x) . showChar '}'
