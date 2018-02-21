#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBindSparseInfo
       (VkBindSparseInfo(..)) where
import           Foreign.Storable                                               (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                     (VkStructureType)
import           Graphics.Vulkan.Types.Handles                                  (VkSemaphore)
import           Graphics.Vulkan.Types.Struct.VkSparseBufferMemoryBindInfo      (VkSparseBufferMemoryBindInfo)
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBindInfo       (VkSparseImageMemoryBindInfo)
import           Graphics.Vulkan.Types.Struct.VkSparseImageOpaqueMemoryBindInfo (VkSparseImageOpaqueMemoryBindInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                               (unsafeDupablePerformIO)

-- | > typedef struct VkBindSparseInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     uint32_t               waitSemaphoreCount;
--   >     const VkSemaphore*     pWaitSemaphores;
--   >     uint32_t               bufferBindCount;
--   >     const VkSparseBufferMemoryBindInfo* pBufferBinds;
--   >     uint32_t               imageOpaqueBindCount;
--   >     const VkSparseImageOpaqueMemoryBindInfo* pImageOpaqueBinds;
--   >     uint32_t               imageBindCount;
--   >     const VkSparseImageMemoryBindInfo* pImageBinds;
--   >     uint32_t               signalSemaphoreCount;
--   >     const VkSemaphore*     pSignalSemaphores;
--   > } VkBindSparseInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBindSparseInfo.html VkBindSparseInfo registry at www.khronos.org>
data VkBindSparseInfo = VkBindSparseInfo## Addr## ByteArray##

instance Eq VkBindSparseInfo where
        (VkBindSparseInfo## a _) == x@(VkBindSparseInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindSparseInfo where
        (VkBindSparseInfo## a _) `compare` x@(VkBindSparseInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindSparseInfo where
        sizeOf ~_ = #{size VkBindSparseInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBindSparseInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindSparseInfo where
        unsafeAddr (VkBindSparseInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindSparseInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindSparseInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindSparseInfo where
        type StructFields VkBindSparseInfo =
             '["sType", "pNext", "waitSemaphoreCount", "pWaitSemaphores", -- ' closing tick for hsc2hs
               "bufferBindCount", "pBufferBinds", "imageOpaqueBindCount",
               "pImageOpaqueBinds", "imageBindCount", "pImageBinds",
               "signalSemaphoreCount", "pSignalSemaphores"]
        type CUnionType VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindSparseInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkBindSparseInfo where
        type VkSTypeMType VkBindSparseInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkBindSparseInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkBindSparseInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkBindSparseInfo, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkBindSparseInfo
         where
        type FieldType "sType" VkBindSparseInfo = VkStructureType
        type FieldOptional "sType" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindSparseInfo =
             #{offset VkBindSparseInfo, sType}
        type FieldIsArray "sType" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindSparseInfo, sType}

instance CanReadField "sType" VkBindSparseInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkBindSparseInfo where
        type VkPNextMType VkBindSparseInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkBindSparseInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkBindSparseInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkBindSparseInfo, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkBindSparseInfo
         where
        type FieldType "pNext" VkBindSparseInfo = Ptr Void
        type FieldOptional "pNext" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindSparseInfo =
             #{offset VkBindSparseInfo, pNext}
        type FieldIsArray "pNext" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindSparseInfo, pNext}

instance CanReadField "pNext" VkBindSparseInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkWaitSemaphoreCount VkBindSparseInfo where
        type VkWaitSemaphoreCountMType VkBindSparseInfo = Word32

        {-# NOINLINE vkWaitSemaphoreCount #-}
        vkWaitSemaphoreCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, waitSemaphoreCount})

        {-# INLINE vkWaitSemaphoreCountByteOffset #-}
        vkWaitSemaphoreCountByteOffset ~_
          = #{offset VkBindSparseInfo, waitSemaphoreCount}

        {-# INLINE readVkWaitSemaphoreCount #-}
        readVkWaitSemaphoreCount p
          = peekByteOff p #{offset VkBindSparseInfo, waitSemaphoreCount}

        {-# INLINE writeVkWaitSemaphoreCount #-}
        writeVkWaitSemaphoreCount p
          = pokeByteOff p #{offset VkBindSparseInfo, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         HasField "waitSemaphoreCount" VkBindSparseInfo where
        type FieldType "waitSemaphoreCount" VkBindSparseInfo = Word32
        type FieldOptional "waitSemaphoreCount" VkBindSparseInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "waitSemaphoreCount" VkBindSparseInfo =
             #{offset VkBindSparseInfo, waitSemaphoreCount}
        type FieldIsArray "waitSemaphoreCount" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindSparseInfo, waitSemaphoreCount}

instance CanReadField "waitSemaphoreCount" VkBindSparseInfo where
        {-# INLINE getField #-}
        getField = vkWaitSemaphoreCount

        {-# INLINE readField #-}
        readField = readVkWaitSemaphoreCount

instance CanWriteField "waitSemaphoreCount" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField = writeVkWaitSemaphoreCount

instance {-# OVERLAPPING #-} HasVkPWaitSemaphores VkBindSparseInfo
         where
        type VkPWaitSemaphoresMType VkBindSparseInfo = Ptr VkSemaphore

        {-# NOINLINE vkPWaitSemaphores #-}
        vkPWaitSemaphores x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pWaitSemaphores})

        {-# INLINE vkPWaitSemaphoresByteOffset #-}
        vkPWaitSemaphoresByteOffset ~_
          = #{offset VkBindSparseInfo, pWaitSemaphores}

        {-# INLINE readVkPWaitSemaphores #-}
        readVkPWaitSemaphores p
          = peekByteOff p #{offset VkBindSparseInfo, pWaitSemaphores}

        {-# INLINE writeVkPWaitSemaphores #-}
        writeVkPWaitSemaphores p
          = pokeByteOff p #{offset VkBindSparseInfo, pWaitSemaphores}

instance {-# OVERLAPPING #-}
         HasField "pWaitSemaphores" VkBindSparseInfo where
        type FieldType "pWaitSemaphores" VkBindSparseInfo = Ptr VkSemaphore
        type FieldOptional "pWaitSemaphores" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pWaitSemaphores" VkBindSparseInfo =
             #{offset VkBindSparseInfo, pWaitSemaphores}
        type FieldIsArray "pWaitSemaphores" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindSparseInfo, pWaitSemaphores}

instance CanReadField "pWaitSemaphores" VkBindSparseInfo where
        {-# INLINE getField #-}
        getField = vkPWaitSemaphores

        {-# INLINE readField #-}
        readField = readVkPWaitSemaphores

instance CanWriteField "pWaitSemaphores" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPWaitSemaphores

instance {-# OVERLAPPING #-} HasVkBufferBindCount VkBindSparseInfo
         where
        type VkBufferBindCountMType VkBindSparseInfo = Word32

        {-# NOINLINE vkBufferBindCount #-}
        vkBufferBindCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, bufferBindCount})

        {-# INLINE vkBufferBindCountByteOffset #-}
        vkBufferBindCountByteOffset ~_
          = #{offset VkBindSparseInfo, bufferBindCount}

        {-# INLINE readVkBufferBindCount #-}
        readVkBufferBindCount p
          = peekByteOff p #{offset VkBindSparseInfo, bufferBindCount}

        {-# INLINE writeVkBufferBindCount #-}
        writeVkBufferBindCount p
          = pokeByteOff p #{offset VkBindSparseInfo, bufferBindCount}

instance {-# OVERLAPPING #-}
         HasField "bufferBindCount" VkBindSparseInfo where
        type FieldType "bufferBindCount" VkBindSparseInfo = Word32
        type FieldOptional "bufferBindCount" VkBindSparseInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "bufferBindCount" VkBindSparseInfo =
             #{offset VkBindSparseInfo, bufferBindCount}
        type FieldIsArray "bufferBindCount" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindSparseInfo, bufferBindCount}

instance CanReadField "bufferBindCount" VkBindSparseInfo where
        {-# INLINE getField #-}
        getField = vkBufferBindCount

        {-# INLINE readField #-}
        readField = readVkBufferBindCount

instance CanWriteField "bufferBindCount" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField = writeVkBufferBindCount

instance {-# OVERLAPPING #-} HasVkPBufferBinds VkBindSparseInfo
         where
        type VkPBufferBindsMType VkBindSparseInfo =
             Ptr VkSparseBufferMemoryBindInfo

        {-# NOINLINE vkPBufferBinds #-}
        vkPBufferBinds x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pBufferBinds})

        {-# INLINE vkPBufferBindsByteOffset #-}
        vkPBufferBindsByteOffset ~_
          = #{offset VkBindSparseInfo, pBufferBinds}

        {-# INLINE readVkPBufferBinds #-}
        readVkPBufferBinds p
          = peekByteOff p #{offset VkBindSparseInfo, pBufferBinds}

        {-# INLINE writeVkPBufferBinds #-}
        writeVkPBufferBinds p
          = pokeByteOff p #{offset VkBindSparseInfo, pBufferBinds}

instance {-# OVERLAPPING #-}
         HasField "pBufferBinds" VkBindSparseInfo where
        type FieldType "pBufferBinds" VkBindSparseInfo =
             Ptr VkSparseBufferMemoryBindInfo
        type FieldOptional "pBufferBinds" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pBufferBinds" VkBindSparseInfo =
             #{offset VkBindSparseInfo, pBufferBinds}
        type FieldIsArray "pBufferBinds" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindSparseInfo, pBufferBinds}

instance CanReadField "pBufferBinds" VkBindSparseInfo where
        {-# INLINE getField #-}
        getField = vkPBufferBinds

        {-# INLINE readField #-}
        readField = readVkPBufferBinds

instance CanWriteField "pBufferBinds" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPBufferBinds

instance {-# OVERLAPPING #-}
         HasVkImageOpaqueBindCount VkBindSparseInfo where
        type VkImageOpaqueBindCountMType VkBindSparseInfo = Word32

        {-# NOINLINE vkImageOpaqueBindCount #-}
        vkImageOpaqueBindCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, imageOpaqueBindCount})

        {-# INLINE vkImageOpaqueBindCountByteOffset #-}
        vkImageOpaqueBindCountByteOffset ~_
          = #{offset VkBindSparseInfo, imageOpaqueBindCount}

        {-# INLINE readVkImageOpaqueBindCount #-}
        readVkImageOpaqueBindCount p
          = peekByteOff p #{offset VkBindSparseInfo, imageOpaqueBindCount}

        {-# INLINE writeVkImageOpaqueBindCount #-}
        writeVkImageOpaqueBindCount p
          = pokeByteOff p #{offset VkBindSparseInfo, imageOpaqueBindCount}

instance {-# OVERLAPPING #-}
         HasField "imageOpaqueBindCount" VkBindSparseInfo where
        type FieldType "imageOpaqueBindCount" VkBindSparseInfo = Word32
        type FieldOptional "imageOpaqueBindCount" VkBindSparseInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "imageOpaqueBindCount" VkBindSparseInfo =
             #{offset VkBindSparseInfo, imageOpaqueBindCount}
        type FieldIsArray "imageOpaqueBindCount" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindSparseInfo, imageOpaqueBindCount}

instance CanReadField "imageOpaqueBindCount" VkBindSparseInfo where
        {-# INLINE getField #-}
        getField = vkImageOpaqueBindCount

        {-# INLINE readField #-}
        readField = readVkImageOpaqueBindCount

instance CanWriteField "imageOpaqueBindCount" VkBindSparseInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkImageOpaqueBindCount

instance {-# OVERLAPPING #-}
         HasVkPImageOpaqueBinds VkBindSparseInfo where
        type VkPImageOpaqueBindsMType VkBindSparseInfo =
             Ptr VkSparseImageOpaqueMemoryBindInfo

        {-# NOINLINE vkPImageOpaqueBinds #-}
        vkPImageOpaqueBinds x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pImageOpaqueBinds})

        {-# INLINE vkPImageOpaqueBindsByteOffset #-}
        vkPImageOpaqueBindsByteOffset ~_
          = #{offset VkBindSparseInfo, pImageOpaqueBinds}

        {-# INLINE readVkPImageOpaqueBinds #-}
        readVkPImageOpaqueBinds p
          = peekByteOff p #{offset VkBindSparseInfo, pImageOpaqueBinds}

        {-# INLINE writeVkPImageOpaqueBinds #-}
        writeVkPImageOpaqueBinds p
          = pokeByteOff p #{offset VkBindSparseInfo, pImageOpaqueBinds}

instance {-# OVERLAPPING #-}
         HasField "pImageOpaqueBinds" VkBindSparseInfo where
        type FieldType "pImageOpaqueBinds" VkBindSparseInfo =
             Ptr VkSparseImageOpaqueMemoryBindInfo
        type FieldOptional "pImageOpaqueBinds" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pImageOpaqueBinds" VkBindSparseInfo =
             #{offset VkBindSparseInfo, pImageOpaqueBinds}
        type FieldIsArray "pImageOpaqueBinds" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindSparseInfo, pImageOpaqueBinds}

instance CanReadField "pImageOpaqueBinds" VkBindSparseInfo where
        {-# INLINE getField #-}
        getField = vkPImageOpaqueBinds

        {-# INLINE readField #-}
        readField = readVkPImageOpaqueBinds

instance CanWriteField "pImageOpaqueBinds" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPImageOpaqueBinds

instance {-# OVERLAPPING #-} HasVkImageBindCount VkBindSparseInfo
         where
        type VkImageBindCountMType VkBindSparseInfo = Word32

        {-# NOINLINE vkImageBindCount #-}
        vkImageBindCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, imageBindCount})

        {-# INLINE vkImageBindCountByteOffset #-}
        vkImageBindCountByteOffset ~_
          = #{offset VkBindSparseInfo, imageBindCount}

        {-# INLINE readVkImageBindCount #-}
        readVkImageBindCount p
          = peekByteOff p #{offset VkBindSparseInfo, imageBindCount}

        {-# INLINE writeVkImageBindCount #-}
        writeVkImageBindCount p
          = pokeByteOff p #{offset VkBindSparseInfo, imageBindCount}

instance {-# OVERLAPPING #-}
         HasField "imageBindCount" VkBindSparseInfo where
        type FieldType "imageBindCount" VkBindSparseInfo = Word32
        type FieldOptional "imageBindCount" VkBindSparseInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "imageBindCount" VkBindSparseInfo =
             #{offset VkBindSparseInfo, imageBindCount}
        type FieldIsArray "imageBindCount" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindSparseInfo, imageBindCount}

instance CanReadField "imageBindCount" VkBindSparseInfo where
        {-# INLINE getField #-}
        getField = vkImageBindCount

        {-# INLINE readField #-}
        readField = readVkImageBindCount

instance CanWriteField "imageBindCount" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField = writeVkImageBindCount

instance {-# OVERLAPPING #-} HasVkPImageBinds VkBindSparseInfo
         where
        type VkPImageBindsMType VkBindSparseInfo =
             Ptr VkSparseImageMemoryBindInfo

        {-# NOINLINE vkPImageBinds #-}
        vkPImageBinds x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pImageBinds})

        {-# INLINE vkPImageBindsByteOffset #-}
        vkPImageBindsByteOffset ~_
          = #{offset VkBindSparseInfo, pImageBinds}

        {-# INLINE readVkPImageBinds #-}
        readVkPImageBinds p
          = peekByteOff p #{offset VkBindSparseInfo, pImageBinds}

        {-# INLINE writeVkPImageBinds #-}
        writeVkPImageBinds p
          = pokeByteOff p #{offset VkBindSparseInfo, pImageBinds}

instance {-# OVERLAPPING #-}
         HasField "pImageBinds" VkBindSparseInfo where
        type FieldType "pImageBinds" VkBindSparseInfo =
             Ptr VkSparseImageMemoryBindInfo
        type FieldOptional "pImageBinds" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pImageBinds" VkBindSparseInfo =
             #{offset VkBindSparseInfo, pImageBinds}
        type FieldIsArray "pImageBinds" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindSparseInfo, pImageBinds}

instance CanReadField "pImageBinds" VkBindSparseInfo where
        {-# INLINE getField #-}
        getField = vkPImageBinds

        {-# INLINE readField #-}
        readField = readVkPImageBinds

instance CanWriteField "pImageBinds" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPImageBinds

instance {-# OVERLAPPING #-}
         HasVkSignalSemaphoreCount VkBindSparseInfo where
        type VkSignalSemaphoreCountMType VkBindSparseInfo = Word32

        {-# NOINLINE vkSignalSemaphoreCount #-}
        vkSignalSemaphoreCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, signalSemaphoreCount})

        {-# INLINE vkSignalSemaphoreCountByteOffset #-}
        vkSignalSemaphoreCountByteOffset ~_
          = #{offset VkBindSparseInfo, signalSemaphoreCount}

        {-# INLINE readVkSignalSemaphoreCount #-}
        readVkSignalSemaphoreCount p
          = peekByteOff p #{offset VkBindSparseInfo, signalSemaphoreCount}

        {-# INLINE writeVkSignalSemaphoreCount #-}
        writeVkSignalSemaphoreCount p
          = pokeByteOff p #{offset VkBindSparseInfo, signalSemaphoreCount}

instance {-# OVERLAPPING #-}
         HasField "signalSemaphoreCount" VkBindSparseInfo where
        type FieldType "signalSemaphoreCount" VkBindSparseInfo = Word32
        type FieldOptional "signalSemaphoreCount" VkBindSparseInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "signalSemaphoreCount" VkBindSparseInfo =
             #{offset VkBindSparseInfo, signalSemaphoreCount}
        type FieldIsArray "signalSemaphoreCount" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindSparseInfo, signalSemaphoreCount}

instance CanReadField "signalSemaphoreCount" VkBindSparseInfo where
        {-# INLINE getField #-}
        getField = vkSignalSemaphoreCount

        {-# INLINE readField #-}
        readField = readVkSignalSemaphoreCount

instance CanWriteField "signalSemaphoreCount" VkBindSparseInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkSignalSemaphoreCount

instance {-# OVERLAPPING #-}
         HasVkPSignalSemaphores VkBindSparseInfo where
        type VkPSignalSemaphoresMType VkBindSparseInfo = Ptr VkSemaphore

        {-# NOINLINE vkPSignalSemaphores #-}
        vkPSignalSemaphores x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pSignalSemaphores})

        {-# INLINE vkPSignalSemaphoresByteOffset #-}
        vkPSignalSemaphoresByteOffset ~_
          = #{offset VkBindSparseInfo, pSignalSemaphores}

        {-# INLINE readVkPSignalSemaphores #-}
        readVkPSignalSemaphores p
          = peekByteOff p #{offset VkBindSparseInfo, pSignalSemaphores}

        {-# INLINE writeVkPSignalSemaphores #-}
        writeVkPSignalSemaphores p
          = pokeByteOff p #{offset VkBindSparseInfo, pSignalSemaphores}

instance {-# OVERLAPPING #-}
         HasField "pSignalSemaphores" VkBindSparseInfo where
        type FieldType "pSignalSemaphores" VkBindSparseInfo =
             Ptr VkSemaphore
        type FieldOptional "pSignalSemaphores" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pSignalSemaphores" VkBindSparseInfo =
             #{offset VkBindSparseInfo, pSignalSemaphores}
        type FieldIsArray "pSignalSemaphores" VkBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindSparseInfo, pSignalSemaphores}

instance CanReadField "pSignalSemaphores" VkBindSparseInfo where
        {-# INLINE getField #-}
        getField = vkPSignalSemaphores

        {-# INLINE readField #-}
        readField = readVkPSignalSemaphores

instance CanWriteField "pSignalSemaphores" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPSignalSemaphores

instance Show VkBindSparseInfo where
        showsPrec d x
          = showString "VkBindSparseInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkWaitSemaphoreCount = " .
                            showsPrec d (vkWaitSemaphoreCount x) .
                              showString ", " .
                                showString "vkPWaitSemaphores = " .
                                  showsPrec d (vkPWaitSemaphores x) .
                                    showString ", " .
                                      showString "vkBufferBindCount = " .
                                        showsPrec d (vkBufferBindCount x) .
                                          showString ", " .
                                            showString "vkPBufferBinds = " .
                                              showsPrec d (vkPBufferBinds x) .
                                                showString ", " .
                                                  showString "vkImageOpaqueBindCount = " .
                                                    showsPrec d (vkImageOpaqueBindCount x) .
                                                      showString ", " .
                                                        showString "vkPImageOpaqueBinds = " .
                                                          showsPrec d (vkPImageOpaqueBinds x) .
                                                            showString ", " .
                                                              showString "vkImageBindCount = " .
                                                                showsPrec d (vkImageBindCount x) .
                                                                  showString ", " .
                                                                    showString "vkPImageBinds = " .
                                                                      showsPrec d (vkPImageBinds x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "vkSignalSemaphoreCount = "
                                                                            .
                                                                            showsPrec d
                                                                              (vkSignalSemaphoreCount
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "vkPSignalSemaphores = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (vkPSignalSemaphores
                                                                                       x)
                                                                                    . showChar '}'
