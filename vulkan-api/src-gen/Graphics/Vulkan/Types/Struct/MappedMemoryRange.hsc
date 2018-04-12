#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.MappedMemoryRange
       (VkMappedMemoryRange(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkDeviceMemory)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkMappedMemoryRange {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDeviceMemory         memory;
--   >     VkDeviceSize           offset;
--   >     VkDeviceSize           size;
--   > } VkMappedMemoryRange;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMappedMemoryRange VkMappedMemoryRange registry at www.khronos.org>
data VkMappedMemoryRange = VkMappedMemoryRange## Addr## ByteArray##

instance Eq VkMappedMemoryRange where
        (VkMappedMemoryRange## a _) == x@(VkMappedMemoryRange## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMappedMemoryRange where
        (VkMappedMemoryRange## a _) `compare` x@(VkMappedMemoryRange## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMappedMemoryRange where
        sizeOf ~_ = #{size VkMappedMemoryRange}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMappedMemoryRange}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMappedMemoryRange where
        unsafeAddr (VkMappedMemoryRange## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMappedMemoryRange## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMappedMemoryRange## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMappedMemoryRange where
        type StructFields VkMappedMemoryRange =
             '["sType", "pNext", "memory", "offset", "size"] -- ' closing tick for hsc2hs
        type CUnionType VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMappedMemoryRange = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkMappedMemoryRange
         where
        type FieldType "sType" VkMappedMemoryRange = VkStructureType
        type FieldOptional "sType" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMappedMemoryRange =
             #{offset VkMappedMemoryRange, sType}
        type FieldIsArray "sType" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMappedMemoryRange, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMappedMemoryRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMappedMemoryRange, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMappedMemoryRange, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMappedMemoryRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMappedMemoryRange, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkMappedMemoryRange
         where
        type FieldType "pNext" VkMappedMemoryRange = Ptr Void
        type FieldOptional "pNext" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMappedMemoryRange =
             #{offset VkMappedMemoryRange, pNext}
        type FieldIsArray "pNext" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMappedMemoryRange, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMappedMemoryRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMappedMemoryRange, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMappedMemoryRange, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMappedMemoryRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMappedMemoryRange, pNext}

instance {-# OVERLAPPING #-} HasField "memory" VkMappedMemoryRange
         where
        type FieldType "memory" VkMappedMemoryRange = VkDeviceMemory
        type FieldOptional "memory" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkMappedMemoryRange =
             #{offset VkMappedMemoryRange, memory}
        type FieldIsArray "memory" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMappedMemoryRange, memory}

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkMappedMemoryRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMappedMemoryRange, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMappedMemoryRange, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkMappedMemoryRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMappedMemoryRange, memory}

instance {-# OVERLAPPING #-} HasField "offset" VkMappedMemoryRange
         where
        type FieldType "offset" VkMappedMemoryRange = VkDeviceSize
        type FieldOptional "offset" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkMappedMemoryRange =
             #{offset VkMappedMemoryRange, offset}
        type FieldIsArray "offset" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMappedMemoryRange, offset}

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkMappedMemoryRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMappedMemoryRange, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMappedMemoryRange, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkMappedMemoryRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMappedMemoryRange, offset}

instance {-# OVERLAPPING #-} HasField "size" VkMappedMemoryRange
         where
        type FieldType "size" VkMappedMemoryRange = VkDeviceSize
        type FieldOptional "size" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkMappedMemoryRange =
             #{offset VkMappedMemoryRange, size}
        type FieldIsArray "size" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMappedMemoryRange, size}

instance {-# OVERLAPPING #-}
         CanReadField "size" VkMappedMemoryRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMappedMemoryRange, size})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMappedMemoryRange, size}

instance {-# OVERLAPPING #-}
         CanWriteField "size" VkMappedMemoryRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMappedMemoryRange, size}

instance Show VkMappedMemoryRange where
        showsPrec d x
          = showString "VkMappedMemoryRange {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memory = " .
                            showsPrec d (getField @"memory" x) .
                              showString ", " .
                                showString "offset = " .
                                  showsPrec d (getField @"offset" x) .
                                    showString ", " .
                                      showString "size = " .
                                        showsPrec d (getField @"size" x) . showChar '}'
