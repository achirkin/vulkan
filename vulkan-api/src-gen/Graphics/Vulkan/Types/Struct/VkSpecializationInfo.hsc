#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSpecializationInfo
       (VkSpecializationInfo(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkSpecializationMapEntry (VkSpecializationMapEntry)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkSpecializationInfo {
--   >     uint32_t               mapEntryCount;
--   >     const VkSpecializationMapEntry* pMapEntries;
--   >     size_t                 dataSize;
--   >     const void*            pData;
--   > } VkSpecializationInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSpecializationInfo.html VkSpecializationInfo registry at www.khronos.org>
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
         HasVkMapEntryCount VkSpecializationInfo where
        type VkMapEntryCountMType VkSpecializationInfo = Word32

        {-# NOINLINE vkMapEntryCount #-}
        vkMapEntryCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSpecializationInfo, mapEntryCount})

        {-# INLINE vkMapEntryCountByteOffset #-}
        vkMapEntryCountByteOffset ~_
          = #{offset VkSpecializationInfo, mapEntryCount}

        {-# INLINE readVkMapEntryCount #-}
        readVkMapEntryCount p
          = peekByteOff p #{offset VkSpecializationInfo, mapEntryCount}

        {-# INLINE writeVkMapEntryCount #-}
        writeVkMapEntryCount p
          = pokeByteOff p #{offset VkSpecializationInfo, mapEntryCount}

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

instance CanReadField "mapEntryCount" VkSpecializationInfo where
        {-# INLINE getField #-}
        getField = vkMapEntryCount

        {-# INLINE readField #-}
        readField = readVkMapEntryCount

instance CanWriteField "mapEntryCount" VkSpecializationInfo where
        {-# INLINE writeField #-}
        writeField = writeVkMapEntryCount

instance {-# OVERLAPPING #-} HasVkPMapEntries VkSpecializationInfo
         where
        type VkPMapEntriesMType VkSpecializationInfo =
             Ptr VkSpecializationMapEntry

        {-# NOINLINE vkPMapEntries #-}
        vkPMapEntries x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSpecializationInfo, pMapEntries})

        {-# INLINE vkPMapEntriesByteOffset #-}
        vkPMapEntriesByteOffset ~_
          = #{offset VkSpecializationInfo, pMapEntries}

        {-# INLINE readVkPMapEntries #-}
        readVkPMapEntries p
          = peekByteOff p #{offset VkSpecializationInfo, pMapEntries}

        {-# INLINE writeVkPMapEntries #-}
        writeVkPMapEntries p
          = pokeByteOff p #{offset VkSpecializationInfo, pMapEntries}

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

instance CanReadField "pMapEntries" VkSpecializationInfo where
        {-# INLINE getField #-}
        getField = vkPMapEntries

        {-# INLINE readField #-}
        readField = readVkPMapEntries

instance CanWriteField "pMapEntries" VkSpecializationInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPMapEntries

instance {-# OVERLAPPING #-} HasVkDataSize VkSpecializationInfo
         where
        type VkDataSizeMType VkSpecializationInfo = CSize

        {-# NOINLINE vkDataSize #-}
        vkDataSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSpecializationInfo, dataSize})

        {-# INLINE vkDataSizeByteOffset #-}
        vkDataSizeByteOffset ~_
          = #{offset VkSpecializationInfo, dataSize}

        {-# INLINE readVkDataSize #-}
        readVkDataSize p
          = peekByteOff p #{offset VkSpecializationInfo, dataSize}

        {-# INLINE writeVkDataSize #-}
        writeVkDataSize p
          = pokeByteOff p #{offset VkSpecializationInfo, dataSize}

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

instance CanReadField "dataSize" VkSpecializationInfo where
        {-# INLINE getField #-}
        getField = vkDataSize

        {-# INLINE readField #-}
        readField = readVkDataSize

instance CanWriteField "dataSize" VkSpecializationInfo where
        {-# INLINE writeField #-}
        writeField = writeVkDataSize

instance {-# OVERLAPPING #-} HasVkPData VkSpecializationInfo where
        type VkPDataMType VkSpecializationInfo = Ptr Void

        {-# NOINLINE vkPData #-}
        vkPData x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSpecializationInfo, pData})

        {-# INLINE vkPDataByteOffset #-}
        vkPDataByteOffset ~_
          = #{offset VkSpecializationInfo, pData}

        {-# INLINE readVkPData #-}
        readVkPData p
          = peekByteOff p #{offset VkSpecializationInfo, pData}

        {-# INLINE writeVkPData #-}
        writeVkPData p
          = pokeByteOff p #{offset VkSpecializationInfo, pData}

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

instance CanReadField "pData" VkSpecializationInfo where
        {-# INLINE getField #-}
        getField = vkPData

        {-# INLINE readField #-}
        readField = readVkPData

instance CanWriteField "pData" VkSpecializationInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPData

instance Show VkSpecializationInfo where
        showsPrec d x
          = showString "VkSpecializationInfo {" .
              showString "vkMapEntryCount = " .
                showsPrec d (vkMapEntryCount x) .
                  showString ", " .
                    showString "vkPMapEntries = " .
                      showsPrec d (vkPMapEntries x) .
                        showString ", " .
                          showString "vkDataSize = " .
                            showsPrec d (vkDataSize x) .
                              showString ", " .
                                showString "vkPData = " . showsPrec d (vkPData x) . showChar '}'
