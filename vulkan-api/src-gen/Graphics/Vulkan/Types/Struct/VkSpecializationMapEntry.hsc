#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSpecializationMapEntry
       (VkSpecializationMapEntry(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkSpecializationMapEntry {
--   >     uint32_t               constantID;
--   >     uint32_t               offset;
--   >     size_t                 size;
--   > } VkSpecializationMapEntry;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkSpecializationMapEntry.html VkSpecializationMapEntry registry at www.khronos.org>
data VkSpecializationMapEntry = VkSpecializationMapEntry## Addr##
                                                          ByteArray##

instance Eq VkSpecializationMapEntry where
        (VkSpecializationMapEntry## a _) ==
          x@(VkSpecializationMapEntry## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSpecializationMapEntry where
        (VkSpecializationMapEntry## a _) `compare`
          x@(VkSpecializationMapEntry## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSpecializationMapEntry where
        sizeOf ~_ = #{size VkSpecializationMapEntry}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSpecializationMapEntry}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSpecializationMapEntry where
        unsafeAddr (VkSpecializationMapEntry## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSpecializationMapEntry## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSpecializationMapEntry## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSpecializationMapEntry where
        type StructFields VkSpecializationMapEntry =
             '["constantID", "offset", "size"] -- ' closing tick for hsc2hs
        type CUnionType VkSpecializationMapEntry = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSpecializationMapEntry = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSpecializationMapEntry = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "constantID" VkSpecializationMapEntry where
        type FieldType "constantID" VkSpecializationMapEntry = Word32
        type FieldOptional "constantID" VkSpecializationMapEntry = 'False -- ' closing tick for hsc2hs
        type FieldOffset "constantID" VkSpecializationMapEntry =
             #{offset VkSpecializationMapEntry, constantID}
        type FieldIsArray "constantID" VkSpecializationMapEntry = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSpecializationMapEntry, constantID}

instance {-# OVERLAPPING #-}
         CanReadField "constantID" VkSpecializationMapEntry where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSpecializationMapEntry, constantID})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSpecializationMapEntry, constantID}

instance {-# OVERLAPPING #-}
         CanWriteField "constantID" VkSpecializationMapEntry where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSpecializationMapEntry, constantID}

instance {-# OVERLAPPING #-}
         HasField "offset" VkSpecializationMapEntry where
        type FieldType "offset" VkSpecializationMapEntry = Word32
        type FieldOptional "offset" VkSpecializationMapEntry = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkSpecializationMapEntry =
             #{offset VkSpecializationMapEntry, offset}
        type FieldIsArray "offset" VkSpecializationMapEntry = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSpecializationMapEntry, offset}

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkSpecializationMapEntry where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSpecializationMapEntry, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSpecializationMapEntry, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkSpecializationMapEntry where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSpecializationMapEntry, offset}

instance {-# OVERLAPPING #-}
         HasField "size" VkSpecializationMapEntry where
        type FieldType "size" VkSpecializationMapEntry = CSize
        type FieldOptional "size" VkSpecializationMapEntry = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkSpecializationMapEntry =
             #{offset VkSpecializationMapEntry, size}
        type FieldIsArray "size" VkSpecializationMapEntry = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSpecializationMapEntry, size}

instance {-# OVERLAPPING #-}
         CanReadField "size" VkSpecializationMapEntry where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSpecializationMapEntry, size})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSpecializationMapEntry, size}

instance {-# OVERLAPPING #-}
         CanWriteField "size" VkSpecializationMapEntry where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSpecializationMapEntry, size}

instance Show VkSpecializationMapEntry where
        showsPrec d x
          = showString "VkSpecializationMapEntry {" .
              showString "constantID = " .
                showsPrec d (getField @"constantID" x) .
                  showString ", " .
                    showString "offset = " .
                      showsPrec d (getField @"offset" x) .
                        showString ", " .
                          showString "size = " .
                            showsPrec d (getField @"size" x) . showChar '}'
