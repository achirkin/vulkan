#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Specialization
       (VkSpecializationInfo(..), VkSpecializationMapEntry(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Base                         (Addr##, ByteArray##,
                                                   byteArrayContents##,
                                                   plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkSpecializationInfo {
--   >     uint32_t               mapEntryCount;
--   >     const VkSpecializationMapEntry* pMapEntries;
--   >     size_t                 dataSize;
--   >     const void*            pData;
--   > } VkSpecializationInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSpecializationInfo VkSpecializationInfo registry at www.khronos.org>
data VkSpecializationInfo = VkSpecializationInfo## Addr## ByteArray##

instance Eq VkSpecializationInfo where
        (VkSpecializationInfo## a _) == x@(VkSpecializationInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSpecializationInfo where
        (VkSpecializationInfo## a _) `compare` x@(VkSpecializationInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSpecializationInfo where
        sizeOf ~_ = #{size VkSpecializationInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSpecializationInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSpecializationInfo where
        unsafeAddr (VkSpecializationInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSpecializationInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSpecializationInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSpecializationInfo where
        type StructFields VkSpecializationInfo =
             '["mapEntryCount", "pMapEntries", "dataSize", "pData"] -- ' closing tick for hsc2hs
        type CUnionType VkSpecializationInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSpecializationInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSpecializationInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "mapEntryCount" VkSpecializationInfo where
        type FieldType "mapEntryCount" VkSpecializationInfo = Word32
        type FieldOptional "mapEntryCount" VkSpecializationInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "mapEntryCount" VkSpecializationInfo =
             #{offset VkSpecializationInfo, mapEntryCount}
        type FieldIsArray "mapEntryCount" VkSpecializationInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSpecializationInfo, mapEntryCount}

instance {-# OVERLAPPING #-}
         CanReadField "mapEntryCount" VkSpecializationInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSpecializationInfo, mapEntryCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSpecializationInfo, mapEntryCount}

instance {-# OVERLAPPING #-}
         CanWriteField "mapEntryCount" VkSpecializationInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSpecializationInfo, mapEntryCount}

instance {-# OVERLAPPING #-}
         HasField "pMapEntries" VkSpecializationInfo where
        type FieldType "pMapEntries" VkSpecializationInfo =
             Ptr VkSpecializationMapEntry
        type FieldOptional "pMapEntries" VkSpecializationInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pMapEntries" VkSpecializationInfo =
             #{offset VkSpecializationInfo, pMapEntries}
        type FieldIsArray "pMapEntries" VkSpecializationInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSpecializationInfo, pMapEntries}

instance {-# OVERLAPPING #-}
         CanReadField "pMapEntries" VkSpecializationInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSpecializationInfo, pMapEntries})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSpecializationInfo, pMapEntries}

instance {-# OVERLAPPING #-}
         CanWriteField "pMapEntries" VkSpecializationInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSpecializationInfo, pMapEntries}

instance {-# OVERLAPPING #-}
         HasField "dataSize" VkSpecializationInfo where
        type FieldType "dataSize" VkSpecializationInfo = CSize
        type FieldOptional "dataSize" VkSpecializationInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dataSize" VkSpecializationInfo =
             #{offset VkSpecializationInfo, dataSize}
        type FieldIsArray "dataSize" VkSpecializationInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSpecializationInfo, dataSize}

instance {-# OVERLAPPING #-}
         CanReadField "dataSize" VkSpecializationInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSpecializationInfo, dataSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSpecializationInfo, dataSize}

instance {-# OVERLAPPING #-}
         CanWriteField "dataSize" VkSpecializationInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSpecializationInfo, dataSize}

instance {-# OVERLAPPING #-} HasField "pData" VkSpecializationInfo
         where
        type FieldType "pData" VkSpecializationInfo = Ptr Void
        type FieldOptional "pData" VkSpecializationInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pData" VkSpecializationInfo =
             #{offset VkSpecializationInfo, pData}
        type FieldIsArray "pData" VkSpecializationInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSpecializationInfo, pData}

instance {-# OVERLAPPING #-}
         CanReadField "pData" VkSpecializationInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSpecializationInfo, pData})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSpecializationInfo, pData}

instance {-# OVERLAPPING #-}
         CanWriteField "pData" VkSpecializationInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSpecializationInfo, pData}

instance Show VkSpecializationInfo where
        showsPrec d x
          = showString "VkSpecializationInfo {" .
              showString "mapEntryCount = " .
                showsPrec d (getField @"mapEntryCount" x) .
                  showString ", " .
                    showString "pMapEntries = " .
                      showsPrec d (getField @"pMapEntries" x) .
                        showString ", " .
                          showString "dataSize = " .
                            showsPrec d (getField @"dataSize" x) .
                              showString ", " .
                                showString "pData = " .
                                  showsPrec d (getField @"pData" x) . showChar '}'

-- | > typedef struct VkSpecializationMapEntry {
--   >     uint32_t               constantID;
--   >     uint32_t               offset;
--   >     size_t                 size;
--   > } VkSpecializationMapEntry;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSpecializationMapEntry VkSpecializationMapEntry registry at www.khronos.org>
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
