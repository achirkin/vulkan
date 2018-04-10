#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBindSparseInfo
       (VkBindSparseInfo(..)) where
import           Foreign.Storable                                               (Storable (..))
import           GHC.Base                                                       (Addr##,
                                                                                 ByteArray##,
                                                                                 byteArrayContents##,
                                                                                 plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                     (VkStructureType)
import           Graphics.Vulkan.Types.Handles                                  (VkSemaphore)
import           Graphics.Vulkan.Types.Struct.VkSparseBufferMemoryBindInfo      (VkSparseBufferMemoryBindInfo)
import           Graphics.Vulkan.Types.Struct.VkSparseImageMemoryBindInfo       (VkSparseImageMemoryBindInfo)
import           Graphics.Vulkan.Types.Struct.VkSparseImageOpaqueMemoryBindInfo (VkSparseImageOpaqueMemoryBindInfo)
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkBindSparseInfo VkBindSparseInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} CanReadField "sType" VkBindSparseInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, sType}

instance {-# OVERLAPPING #-} CanWriteField "sType" VkBindSparseInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, sType}

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

instance {-# OVERLAPPING #-} CanReadField "pNext" VkBindSparseInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, pNext}

instance {-# OVERLAPPING #-} CanWriteField "pNext" VkBindSparseInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "waitSemaphoreCount" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, waitSemaphoreCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanWriteField "waitSemaphoreCount" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, waitSemaphoreCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pWaitSemaphores" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pWaitSemaphores})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, pWaitSemaphores}

instance {-# OVERLAPPING #-}
         CanWriteField "pWaitSemaphores" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, pWaitSemaphores}

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

instance {-# OVERLAPPING #-}
         CanReadField "bufferBindCount" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, bufferBindCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, bufferBindCount}

instance {-# OVERLAPPING #-}
         CanWriteField "bufferBindCount" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, bufferBindCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pBufferBinds" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pBufferBinds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, pBufferBinds}

instance {-# OVERLAPPING #-}
         CanWriteField "pBufferBinds" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, pBufferBinds}

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

instance {-# OVERLAPPING #-}
         CanReadField "imageOpaqueBindCount" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, imageOpaqueBindCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, imageOpaqueBindCount}

instance {-# OVERLAPPING #-}
         CanWriteField "imageOpaqueBindCount" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, imageOpaqueBindCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pImageOpaqueBinds" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pImageOpaqueBinds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, pImageOpaqueBinds}

instance {-# OVERLAPPING #-}
         CanWriteField "pImageOpaqueBinds" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, pImageOpaqueBinds}

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

instance {-# OVERLAPPING #-}
         CanReadField "imageBindCount" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, imageBindCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, imageBindCount}

instance {-# OVERLAPPING #-}
         CanWriteField "imageBindCount" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, imageBindCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pImageBinds" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pImageBinds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, pImageBinds}

instance {-# OVERLAPPING #-}
         CanWriteField "pImageBinds" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, pImageBinds}

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

instance {-# OVERLAPPING #-}
         CanReadField "signalSemaphoreCount" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, signalSemaphoreCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, signalSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanWriteField "signalSemaphoreCount" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, signalSemaphoreCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pSignalSemaphores" VkBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindSparseInfo, pSignalSemaphores})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindSparseInfo, pSignalSemaphores}

instance {-# OVERLAPPING #-}
         CanWriteField "pSignalSemaphores" VkBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindSparseInfo, pSignalSemaphores}

instance Show VkBindSparseInfo where
        showsPrec d x
          = showString "VkBindSparseInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "waitSemaphoreCount = " .
                            showsPrec d (getField @"waitSemaphoreCount" x) .
                              showString ", " .
                                showString "pWaitSemaphores = " .
                                  showsPrec d (getField @"pWaitSemaphores" x) .
                                    showString ", " .
                                      showString "bufferBindCount = " .
                                        showsPrec d (getField @"bufferBindCount" x) .
                                          showString ", " .
                                            showString "pBufferBinds = " .
                                              showsPrec d (getField @"pBufferBinds" x) .
                                                showString ", " .
                                                  showString "imageOpaqueBindCount = " .
                                                    showsPrec d (getField @"imageOpaqueBindCount" x)
                                                      .
                                                      showString ", " .
                                                        showString "pImageOpaqueBinds = " .
                                                          showsPrec d
                                                            (getField @"pImageOpaqueBinds" x)
                                                            .
                                                            showString ", " .
                                                              showString "imageBindCount = " .
                                                                showsPrec d
                                                                  (getField @"imageBindCount" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString "pImageBinds = " .
                                                                      showsPrec d
                                                                        (getField @"pImageBinds" x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "signalSemaphoreCount = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"signalSemaphoreCount"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "pSignalSemaphores = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"pSignalSemaphores"
                                                                                       x)
                                                                                    . showChar '}'
