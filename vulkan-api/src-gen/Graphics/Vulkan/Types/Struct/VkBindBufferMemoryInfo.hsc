#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBindBufferMemoryInfo
       (VkBindBufferMemoryInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkBuffer,
                                                             VkDeviceMemory)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkBindBufferMemoryInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBuffer                         buffer;
--   >     VkDeviceMemory                   memory;
--   >     VkDeviceSize                     memoryOffset;
--   > } VkBindBufferMemoryInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkBindBufferMemoryInfoVkBindBufferMemoryInfo registry at www.khronos.org>
data VkBindBufferMemoryInfo = VkBindBufferMemoryInfo## Addr##
                                                      ByteArray##

instance Eq VkBindBufferMemoryInfo where
        (VkBindBufferMemoryInfo## a _) == x@(VkBindBufferMemoryInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindBufferMemoryInfo where
        (VkBindBufferMemoryInfo## a _) `compare`
          x@(VkBindBufferMemoryInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindBufferMemoryInfo where
        sizeOf ~_ = #{size VkBindBufferMemoryInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBindBufferMemoryInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindBufferMemoryInfo where
        unsafeAddr (VkBindBufferMemoryInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindBufferMemoryInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindBufferMemoryInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindBufferMemoryInfo where
        type StructFields VkBindBufferMemoryInfo =
             '["sType", "pNext", "buffer", "memory", "memoryOffset"] -- ' closing tick for hsc2hs
        type CUnionType VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindBufferMemoryInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindBufferMemoryInfo where
        type FieldType "sType" VkBindBufferMemoryInfo = VkStructureType
        type FieldOptional "sType" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindBufferMemoryInfo =
             #{offset VkBindBufferMemoryInfo, sType}
        type FieldIsArray "sType" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindBufferMemoryInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindBufferMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindBufferMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindBufferMemoryInfo where
        type FieldType "pNext" VkBindBufferMemoryInfo = Ptr Void
        type FieldOptional "pNext" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindBufferMemoryInfo =
             #{offset VkBindBufferMemoryInfo, pNext}
        type FieldIsArray "pNext" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindBufferMemoryInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindBufferMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindBufferMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkBindBufferMemoryInfo where
        type FieldType "buffer" VkBindBufferMemoryInfo = VkBuffer
        type FieldOptional "buffer" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkBindBufferMemoryInfo =
             #{offset VkBindBufferMemoryInfo, buffer}
        type FieldIsArray "buffer" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindBufferMemoryInfo, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkBindBufferMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfo, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryInfo, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkBindBufferMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryInfo, buffer}

instance {-# OVERLAPPING #-}
         HasField "memory" VkBindBufferMemoryInfo where
        type FieldType "memory" VkBindBufferMemoryInfo = VkDeviceMemory
        type FieldOptional "memory" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkBindBufferMemoryInfo =
             #{offset VkBindBufferMemoryInfo, memory}
        type FieldIsArray "memory" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBindBufferMemoryInfo, memory}

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkBindBufferMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfo, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryInfo, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkBindBufferMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryInfo, memory}

instance {-# OVERLAPPING #-}
         HasField "memoryOffset" VkBindBufferMemoryInfo where
        type FieldType "memoryOffset" VkBindBufferMemoryInfo = VkDeviceSize
        type FieldOptional "memoryOffset" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryOffset" VkBindBufferMemoryInfo =
             #{offset VkBindBufferMemoryInfo, memoryOffset}
        type FieldIsArray "memoryOffset" VkBindBufferMemoryInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryInfo, memoryOffset}

instance {-# OVERLAPPING #-}
         CanReadField "memoryOffset" VkBindBufferMemoryInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfo, memoryOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryInfo, memoryOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryOffset" VkBindBufferMemoryInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryInfo, memoryOffset}

instance Show VkBindBufferMemoryInfo where
        showsPrec d x
          = showString "VkBindBufferMemoryInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "buffer = " .
                            showsPrec d (getField @"buffer" x) .
                              showString ", " .
                                showString "memory = " .
                                  showsPrec d (getField @"memory" x) .
                                    showString ", " .
                                      showString "memoryOffset = " .
                                        showsPrec d (getField @"memoryOffset" x) . showChar '}'
