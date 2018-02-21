#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSpecializationMapEntry
       (VkSpecializationMapEntry(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkSpecializationMapEntry {
--   >     uint32_t               constantID;
--   >     uint32_t               offset;
--   >     size_t                 size;
--   > } VkSpecializationMapEntry;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSpecializationMapEntry.html VkSpecializationMapEntry registry at www.khronos.org>
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
         HasVkConstantID VkSpecializationMapEntry where
        type VkConstantIDMType VkSpecializationMapEntry = Word32

        {-# NOINLINE vkConstantID #-}
        vkConstantID x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSpecializationMapEntry, constantID})

        {-# INLINE vkConstantIDByteOffset #-}
        vkConstantIDByteOffset ~_
          = #{offset VkSpecializationMapEntry, constantID}

        {-# INLINE readVkConstantID #-}
        readVkConstantID p
          = peekByteOff p #{offset VkSpecializationMapEntry, constantID}

        {-# INLINE writeVkConstantID #-}
        writeVkConstantID p
          = pokeByteOff p #{offset VkSpecializationMapEntry, constantID}

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

instance CanReadField "constantID" VkSpecializationMapEntry where
        {-# INLINE getField #-}
        getField = vkConstantID

        {-# INLINE readField #-}
        readField = readVkConstantID

instance CanWriteField "constantID" VkSpecializationMapEntry where
        {-# INLINE writeField #-}
        writeField = writeVkConstantID

instance {-# OVERLAPPING #-} HasVkOffset VkSpecializationMapEntry
         where
        type VkOffsetMType VkSpecializationMapEntry = Word32

        {-# NOINLINE vkOffset #-}
        vkOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSpecializationMapEntry, offset})

        {-# INLINE vkOffsetByteOffset #-}
        vkOffsetByteOffset ~_
          = #{offset VkSpecializationMapEntry, offset}

        {-# INLINE readVkOffset #-}
        readVkOffset p
          = peekByteOff p #{offset VkSpecializationMapEntry, offset}

        {-# INLINE writeVkOffset #-}
        writeVkOffset p
          = pokeByteOff p #{offset VkSpecializationMapEntry, offset}

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

instance CanReadField "offset" VkSpecializationMapEntry where
        {-# INLINE getField #-}
        getField = vkOffset

        {-# INLINE readField #-}
        readField = readVkOffset

instance CanWriteField "offset" VkSpecializationMapEntry where
        {-# INLINE writeField #-}
        writeField = writeVkOffset

instance {-# OVERLAPPING #-} HasVkSize VkSpecializationMapEntry
         where
        type VkSizeMType VkSpecializationMapEntry = CSize

        {-# NOINLINE vkSize #-}
        vkSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSpecializationMapEntry, size})

        {-# INLINE vkSizeByteOffset #-}
        vkSizeByteOffset ~_
          = #{offset VkSpecializationMapEntry, size}

        {-# INLINE readVkSize #-}
        readVkSize p
          = peekByteOff p #{offset VkSpecializationMapEntry, size}

        {-# INLINE writeVkSize #-}
        writeVkSize p
          = pokeByteOff p #{offset VkSpecializationMapEntry, size}

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

instance CanReadField "size" VkSpecializationMapEntry where
        {-# INLINE getField #-}
        getField = vkSize

        {-# INLINE readField #-}
        readField = readVkSize

instance CanWriteField "size" VkSpecializationMapEntry where
        {-# INLINE writeField #-}
        writeField = writeVkSize

instance Show VkSpecializationMapEntry where
        showsPrec d x
          = showString "VkSpecializationMapEntry {" .
              showString "vkConstantID = " .
                showsPrec d (vkConstantID x) .
                  showString ", " .
                    showString "vkOffset = " .
                      showsPrec d (vkOffset x) .
                        showString ", " .
                          showString "vkSize = " . showsPrec d (vkSize x) . showChar '}'
