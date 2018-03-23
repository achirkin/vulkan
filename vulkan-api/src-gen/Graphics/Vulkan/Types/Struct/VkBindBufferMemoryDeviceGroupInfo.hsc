#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBindBufferMemoryDeviceGroupInfo
       (VkBindBufferMemoryDeviceGroupInfo(..)) where
import           Foreign.Storable                                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType          (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkBindBufferMemoryInfo (VkBindBufferMemoryInfo)
import           System.IO.Unsafe                                    (unsafeDupablePerformIO)

-- | > typedef struct VkBindBufferMemoryDeviceGroupInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         deviceIndexCount;
--   >     const uint32_t*  pDeviceIndices;
--   > } VkBindBufferMemoryDeviceGroupInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkBindBufferMemoryDeviceGroupInfo.html VkBindBufferMemoryDeviceGroupInfo registry at www.khronos.org>
data VkBindBufferMemoryDeviceGroupInfo = VkBindBufferMemoryDeviceGroupInfo## Addr##
                                                                            ByteArray##

instance Eq VkBindBufferMemoryDeviceGroupInfo where
        (VkBindBufferMemoryDeviceGroupInfo## a _) ==
          x@(VkBindBufferMemoryDeviceGroupInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindBufferMemoryDeviceGroupInfo where
        (VkBindBufferMemoryDeviceGroupInfo## a _) `compare`
          x@(VkBindBufferMemoryDeviceGroupInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindBufferMemoryDeviceGroupInfo where
        sizeOf ~_ = #{size VkBindBufferMemoryDeviceGroupInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBindBufferMemoryDeviceGroupInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindBufferMemoryDeviceGroupInfo where
        unsafeAddr (VkBindBufferMemoryDeviceGroupInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindBufferMemoryDeviceGroupInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindBufferMemoryDeviceGroupInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindBufferMemoryDeviceGroupInfo where
        type StructFields VkBindBufferMemoryDeviceGroupInfo =
             '["sType", "pNext", "deviceIndexCount", "pDeviceIndices"] -- ' closing tick for hsc2hs
        type CUnionType VkBindBufferMemoryDeviceGroupInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindBufferMemoryDeviceGroupInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindBufferMemoryDeviceGroupInfo =
             '[VkBindBufferMemoryInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindBufferMemoryDeviceGroupInfo where
        type FieldType "sType" VkBindBufferMemoryDeviceGroupInfo =
             VkStructureType
        type FieldOptional "sType" VkBindBufferMemoryDeviceGroupInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindBufferMemoryDeviceGroupInfo =
             #{offset VkBindBufferMemoryDeviceGroupInfo, sType}
        type FieldIsArray "sType" VkBindBufferMemoryDeviceGroupInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindBufferMemoryDeviceGroupInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindBufferMemoryDeviceGroupInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindBufferMemoryDeviceGroupInfo where
        type FieldType "pNext" VkBindBufferMemoryDeviceGroupInfo = Ptr Void
        type FieldOptional "pNext" VkBindBufferMemoryDeviceGroupInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindBufferMemoryDeviceGroupInfo =
             #{offset VkBindBufferMemoryDeviceGroupInfo, pNext}
        type FieldIsArray "pNext" VkBindBufferMemoryDeviceGroupInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindBufferMemoryDeviceGroupInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindBufferMemoryDeviceGroupInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "deviceIndexCount" VkBindBufferMemoryDeviceGroupInfo where
        type FieldType "deviceIndexCount" VkBindBufferMemoryDeviceGroupInfo
             = Word32
        type FieldOptional "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfo
             =
             #{offset VkBindBufferMemoryDeviceGroupInfo, deviceIndexCount}
        type FieldIsArray "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfo, deviceIndexCount}

instance {-# OVERLAPPING #-}
         CanReadField "deviceIndexCount" VkBindBufferMemoryDeviceGroupInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfo, deviceIndexCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfo, deviceIndexCount}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceIndexCount" VkBindBufferMemoryDeviceGroupInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfo, deviceIndexCount}

instance {-# OVERLAPPING #-}
         HasField "pDeviceIndices" VkBindBufferMemoryDeviceGroupInfo where
        type FieldType "pDeviceIndices" VkBindBufferMemoryDeviceGroupInfo =
             Ptr Word32
        type FieldOptional "pDeviceIndices"
               VkBindBufferMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceIndices" VkBindBufferMemoryDeviceGroupInfo
             =
             #{offset VkBindBufferMemoryDeviceGroupInfo, pDeviceIndices}
        type FieldIsArray "pDeviceIndices"
               VkBindBufferMemoryDeviceGroupInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfo, pDeviceIndices}

instance {-# OVERLAPPING #-}
         CanReadField "pDeviceIndices" VkBindBufferMemoryDeviceGroupInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfo, pDeviceIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfo, pDeviceIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pDeviceIndices" VkBindBufferMemoryDeviceGroupInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfo, pDeviceIndices}

instance Show VkBindBufferMemoryDeviceGroupInfo where
        showsPrec d x
          = showString "VkBindBufferMemoryDeviceGroupInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "deviceIndexCount = " .
                            showsPrec d (getField @"deviceIndexCount" x) .
                              showString ", " .
                                showString "pDeviceIndices = " .
                                  showsPrec d (getField @"pDeviceIndices" x) . showChar '}'
