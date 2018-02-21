#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExtent2D (VkExtent2D(..))
       where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkExtent2D {
--   >     uint32_t        width;
--   >     uint32_t        height;
--   > } VkExtent2D;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExtent2D.html VkExtent2D registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkWidth VkExtent2D where
        type VkWidthMType VkExtent2D = Word32

        {-# NOINLINE vkWidth #-}
        vkWidth x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExtent2D, width})

        {-# INLINE vkWidthByteOffset #-}
        vkWidthByteOffset ~_ = #{offset VkExtent2D, width}

        {-# INLINE readVkWidth #-}
        readVkWidth p
          = peekByteOff p #{offset VkExtent2D, width}

        {-# INLINE writeVkWidth #-}
        writeVkWidth p
          = pokeByteOff p #{offset VkExtent2D, width}

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

instance CanReadField "width" VkExtent2D where
        {-# INLINE getField #-}
        getField = vkWidth

        {-# INLINE readField #-}
        readField = readVkWidth

instance CanWriteField "width" VkExtent2D where
        {-# INLINE writeField #-}
        writeField = writeVkWidth

instance {-# OVERLAPPING #-} HasVkHeight VkExtent2D where
        type VkHeightMType VkExtent2D = Word32

        {-# NOINLINE vkHeight #-}
        vkHeight x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExtent2D, height})

        {-# INLINE vkHeightByteOffset #-}
        vkHeightByteOffset ~_ = #{offset VkExtent2D, height}

        {-# INLINE readVkHeight #-}
        readVkHeight p
          = peekByteOff p #{offset VkExtent2D, height}

        {-# INLINE writeVkHeight #-}
        writeVkHeight p
          = pokeByteOff p #{offset VkExtent2D, height}

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

instance CanReadField "height" VkExtent2D where
        {-# INLINE getField #-}
        getField = vkHeight

        {-# INLINE readField #-}
        readField = readVkHeight

instance CanWriteField "height" VkExtent2D where
        {-# INLINE writeField #-}
        writeField = writeVkHeight

instance Show VkExtent2D where
        showsPrec d x
          = showString "VkExtent2D {" .
              showString "vkWidth = " .
                showsPrec d (vkWidth x) .
                  showString ", " .
                    showString "vkHeight = " . showsPrec d (vkHeight x) . showChar '}'
