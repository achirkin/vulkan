#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
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
import           Graphics.Vulkan.Types.StructMembers
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

instance {-# OVERLAPPING #-} HasVkSType VkBindBufferMemoryInfoKHR
         where
        type VkSTypeMType VkBindBufferMemoryInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkBindBufferMemoryInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkBindBufferMemoryInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkBindBufferMemoryInfoKHR, sType}

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

instance CanReadField "sType" VkBindBufferMemoryInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkBindBufferMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkBindBufferMemoryInfoKHR
         where
        type VkPNextMType VkBindBufferMemoryInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkBindBufferMemoryInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkBindBufferMemoryInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkBindBufferMemoryInfoKHR, pNext}

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

instance CanReadField "pNext" VkBindBufferMemoryInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkBindBufferMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkBuffer VkBindBufferMemoryInfoKHR
         where
        type VkBufferMType VkBindBufferMemoryInfoKHR = VkBuffer

        {-# NOINLINE vkBuffer #-}
        vkBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfoKHR, buffer})

        {-# INLINE vkBufferByteOffset #-}
        vkBufferByteOffset ~_
          = #{offset VkBindBufferMemoryInfoKHR, buffer}

        {-# INLINE readVkBuffer #-}
        readVkBuffer p
          = peekByteOff p #{offset VkBindBufferMemoryInfoKHR, buffer}

        {-# INLINE writeVkBuffer #-}
        writeVkBuffer p
          = pokeByteOff p #{offset VkBindBufferMemoryInfoKHR, buffer}

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

instance CanReadField "buffer" VkBindBufferMemoryInfoKHR where
        {-# INLINE getField #-}
        getField = vkBuffer

        {-# INLINE readField #-}
        readField = readVkBuffer

instance CanWriteField "buffer" VkBindBufferMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkBuffer

instance {-# OVERLAPPING #-} HasVkMemory VkBindBufferMemoryInfoKHR
         where
        type VkMemoryMType VkBindBufferMemoryInfoKHR = VkDeviceMemory

        {-# NOINLINE vkMemory #-}
        vkMemory x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfoKHR, memory})

        {-# INLINE vkMemoryByteOffset #-}
        vkMemoryByteOffset ~_
          = #{offset VkBindBufferMemoryInfoKHR, memory}

        {-# INLINE readVkMemory #-}
        readVkMemory p
          = peekByteOff p #{offset VkBindBufferMemoryInfoKHR, memory}

        {-# INLINE writeVkMemory #-}
        writeVkMemory p
          = pokeByteOff p #{offset VkBindBufferMemoryInfoKHR, memory}

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

instance CanReadField "memory" VkBindBufferMemoryInfoKHR where
        {-# INLINE getField #-}
        getField = vkMemory

        {-# INLINE readField #-}
        readField = readVkMemory

instance CanWriteField "memory" VkBindBufferMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkMemory

instance {-# OVERLAPPING #-}
         HasVkMemoryOffset VkBindBufferMemoryInfoKHR where
        type VkMemoryOffsetMType VkBindBufferMemoryInfoKHR = VkDeviceSize

        {-# NOINLINE vkMemoryOffset #-}
        vkMemoryOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryInfoKHR, memoryOffset})

        {-# INLINE vkMemoryOffsetByteOffset #-}
        vkMemoryOffsetByteOffset ~_
          = #{offset VkBindBufferMemoryInfoKHR, memoryOffset}

        {-# INLINE readVkMemoryOffset #-}
        readVkMemoryOffset p
          = peekByteOff p #{offset VkBindBufferMemoryInfoKHR, memoryOffset}

        {-# INLINE writeVkMemoryOffset #-}
        writeVkMemoryOffset p
          = pokeByteOff p #{offset VkBindBufferMemoryInfoKHR, memoryOffset}

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

instance CanReadField "memoryOffset" VkBindBufferMemoryInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkMemoryOffset

        {-# INLINE readField #-}
        readField = readVkMemoryOffset

instance CanWriteField "memoryOffset" VkBindBufferMemoryInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkMemoryOffset

instance Show VkBindBufferMemoryInfoKHR where
        showsPrec d x
          = showString "VkBindBufferMemoryInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkBuffer = " .
                            showsPrec d (vkBuffer x) .
                              showString ", " .
                                showString "vkMemory = " .
                                  showsPrec d (vkMemory x) .
                                    showString ", " .
                                      showString "vkMemoryOffset = " .
                                        showsPrec d (vkMemoryOffset x) . showChar '}'
