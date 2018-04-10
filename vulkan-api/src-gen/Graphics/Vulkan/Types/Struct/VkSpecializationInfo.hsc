#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSpecializationInfo
       (VkSpecializationInfo(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Base                                              (Addr##, ByteArray##,
                                                                        byteArrayContents##,
                                                                        plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkSpecializationMapEntry (VkSpecializationMapEntry)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkSpecializationInfo {
--   >     uint32_t               mapEntryCount;
--   >     const VkSpecializationMapEntry* pMapEntries;
--   >     size_t                 dataSize;
--   >     const void*            pData;
--   > } VkSpecializationInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkSpecializationInfo VkSpecializationInfo registry at www.khronos.org>
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
