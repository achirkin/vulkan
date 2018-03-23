#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryRequirements
       (VkMemoryRequirements(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes  (VkDeviceSize)
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryRequirements {
--   >     VkDeviceSize           size;
--   >     VkDeviceSize           alignment;
--   >     uint32_t               memoryTypeBits;
--   > } VkMemoryRequirements;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkMemoryRequirements.html VkMemoryRequirements registry at www.khronos.org>
data VkMemoryRequirements = VkMemoryRequirements## Addr## ByteArray##

instance Eq VkMemoryRequirements where
        (VkMemoryRequirements## a _) == x@(VkMemoryRequirements## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryRequirements where
        (VkMemoryRequirements## a _) `compare` x@(VkMemoryRequirements## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryRequirements where
        sizeOf ~_ = #{size VkMemoryRequirements}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryRequirements}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryRequirements where
        unsafeAddr (VkMemoryRequirements## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryRequirements## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryRequirements## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryRequirements where
        type StructFields VkMemoryRequirements =
             '["size", "alignment", "memoryTypeBits"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryRequirements = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryRequirements = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryRequirements = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "size" VkMemoryRequirements
         where
        type FieldType "size" VkMemoryRequirements = VkDeviceSize
        type FieldOptional "size" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkMemoryRequirements =
             #{offset VkMemoryRequirements, size}
        type FieldIsArray "size" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryRequirements, size}

instance {-# OVERLAPPING #-}
         CanReadField "size" VkMemoryRequirements where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements, size})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryRequirements, size}

instance {-# OVERLAPPING #-}
         CanWriteField "size" VkMemoryRequirements where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryRequirements, size}

instance {-# OVERLAPPING #-}
         HasField "alignment" VkMemoryRequirements where
        type FieldType "alignment" VkMemoryRequirements = VkDeviceSize
        type FieldOptional "alignment" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs
        type FieldOffset "alignment" VkMemoryRequirements =
             #{offset VkMemoryRequirements, alignment}
        type FieldIsArray "alignment" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryRequirements, alignment}

instance {-# OVERLAPPING #-}
         CanReadField "alignment" VkMemoryRequirements where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements, alignment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryRequirements, alignment}

instance {-# OVERLAPPING #-}
         CanWriteField "alignment" VkMemoryRequirements where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryRequirements, alignment}

instance {-# OVERLAPPING #-}
         HasField "memoryTypeBits" VkMemoryRequirements where
        type FieldType "memoryTypeBits" VkMemoryRequirements = Word32
        type FieldOptional "memoryTypeBits" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypeBits" VkMemoryRequirements =
             #{offset VkMemoryRequirements, memoryTypeBits}
        type FieldIsArray "memoryTypeBits" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryRequirements, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanReadField "memoryTypeBits" VkMemoryRequirements where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements, memoryTypeBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryRequirements, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryTypeBits" VkMemoryRequirements where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryRequirements, memoryTypeBits}

instance Show VkMemoryRequirements where
        showsPrec d x
          = showString "VkMemoryRequirements {" .
              showString "size = " .
                showsPrec d (getField @"size" x) .
                  showString ", " .
                    showString "alignment = " .
                      showsPrec d (getField @"alignment" x) .
                        showString ", " .
                          showString "memoryTypeBits = " .
                            showsPrec d (getField @"memoryTypeBits" x) . showChar '}'
