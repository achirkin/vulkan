#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBindBufferMemoryInfoKHR
       (VkBindBufferMemoryInfoKHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkBuffer,
                                                             VkDeviceMemory)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkBindBufferMemoryInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBuffer                         buffer;
--   >     VkDeviceMemory                   memory;
--   >     VkDeviceSize                     memoryOffset;
--   > } VkBindBufferMemoryInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBindBufferMemoryInfoKHR.html VkBindBufferMemoryInfoKHR registry at www.khronos.org>
data VkBindBufferMemoryInfoKHR = VkBindBufferMemoryInfoKHR## Addr##
                                                            ByteArray##

instance Eq VkBindBufferMemoryInfoKHR where
        (VkBindBufferMemoryInfoKHR## a _) ==
          x@(VkBindBufferMemoryInfoKHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindBufferMemoryInfoKHR where
        (VkBindBufferMemoryInfoKHR## a _) `compare`
          x@(VkBindBufferMemoryInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindBufferMemoryInfoKHR where
        sizeOf ~_ = #{size VkBindBufferMemoryInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBindBufferMemoryInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindBufferMemoryInfoKHR where
        unsafeAddr (VkBindBufferMemoryInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindBufferMemoryInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindBufferMemoryInfoKHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindBufferMemoryInfoKHR where
        type StructFields VkBindBufferMemoryInfoKHR =
             '["sType", "pNext", "buffer", "memory", "memoryOffset"] -- ' closing tick for hsc2hs
        type CUnionType VkBindBufferMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindBufferMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindBufferMemoryInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindBufferMemoryInfoKHR where
        type FieldType "sType" VkBindBufferMemoryInfoKHR = VkStructureType
        type FieldOptional "sType" VkBindBufferMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindBufferMemoryInfoKHR =
             #{offset VkBindBufferMemoryInfoKHR, sType}
        type FieldIsArray "sType" VkBindBufferMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindBufferMemoryInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindBufferMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindBufferMemoryInfoKHR where
        type FieldType "pNext" VkBindBufferMemoryInfoKHR = Ptr Void
        type FieldOptional "pNext" VkBindBufferMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindBufferMemoryInfoKHR =
             #{offset VkBindBufferMemoryInfoKHR, pNext}
        type FieldIsArray "pNext" VkBindBufferMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindBufferMemoryInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindBufferMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkBindBufferMemoryInfoKHR where
        type FieldType "buffer" VkBindBufferMemoryInfoKHR = VkBuffer
        type FieldOptional "buffer" VkBindBufferMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkBindBufferMemoryInfoKHR =
             #{offset VkBindBufferMemoryInfoKHR, buffer}
        type FieldIsArray "buffer" VkBindBufferMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryInfoKHR, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkBindBufferMemoryInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfoKHR, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryInfoKHR, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkBindBufferMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryInfoKHR, buffer}

instance {-# OVERLAPPING #-}
         HasField "memory" VkBindBufferMemoryInfoKHR where
        type FieldType "memory" VkBindBufferMemoryInfoKHR = VkDeviceMemory
        type FieldOptional "memory" VkBindBufferMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkBindBufferMemoryInfoKHR =
             #{offset VkBindBufferMemoryInfoKHR, memory}
        type FieldIsArray "memory" VkBindBufferMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryInfoKHR, memory}

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkBindBufferMemoryInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfoKHR, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryInfoKHR, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkBindBufferMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryInfoKHR, memory}

instance {-# OVERLAPPING #-}
         HasField "memoryOffset" VkBindBufferMemoryInfoKHR where
        type FieldType "memoryOffset" VkBindBufferMemoryInfoKHR =
             VkDeviceSize
        type FieldOptional "memoryOffset" VkBindBufferMemoryInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryOffset" VkBindBufferMemoryInfoKHR =
             #{offset VkBindBufferMemoryInfoKHR, memoryOffset}
        type FieldIsArray "memoryOffset" VkBindBufferMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryInfoKHR, memoryOffset}

instance {-# OVERLAPPING #-}
         CanReadField "memoryOffset" VkBindBufferMemoryInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfoKHR, memoryOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryInfoKHR, memoryOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryOffset" VkBindBufferMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryInfoKHR, memoryOffset}

instance Show VkBindBufferMemoryInfoKHR where
        showsPrec d x
          = showString "VkBindBufferMemoryInfoKHR {" .
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
