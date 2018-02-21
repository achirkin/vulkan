#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMappedMemoryRange
       (VkMappedMemoryRange(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkDeviceMemory)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkMappedMemoryRange {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDeviceMemory         memory;
--   >     VkDeviceSize           offset;
--   >     VkDeviceSize           size;
--   > } VkMappedMemoryRange;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMappedMemoryRange.html VkMappedMemoryRange registry at www.khronos.org>
data VkMappedMemoryRange = VkMappedMemoryRange## Addr## ByteArray##

instance Eq VkMappedMemoryRange where
        (VkMappedMemoryRange## a _) == x@(VkMappedMemoryRange## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMappedMemoryRange where
        (VkMappedMemoryRange## a _) `compare` x@(VkMappedMemoryRange## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMappedMemoryRange where
        sizeOf ~_ = #{size VkMappedMemoryRange}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMappedMemoryRange}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMappedMemoryRange where
        unsafeAddr (VkMappedMemoryRange## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMappedMemoryRange## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMappedMemoryRange## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMappedMemoryRange where
        type StructFields VkMappedMemoryRange =
             '["sType", "pNext", "memory", "offset", "size"] -- ' closing tick for hsc2hs
        type CUnionType VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMappedMemoryRange = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkMappedMemoryRange where
        type VkSTypeMType VkMappedMemoryRange = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMappedMemoryRange, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMappedMemoryRange, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMappedMemoryRange, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMappedMemoryRange, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkMappedMemoryRange
         where
        type FieldType "sType" VkMappedMemoryRange = VkStructureType
        type FieldOptional "sType" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMappedMemoryRange =
             #{offset VkMappedMemoryRange, sType}
        type FieldIsArray "sType" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMappedMemoryRange, sType}

instance CanReadField "sType" VkMappedMemoryRange where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMappedMemoryRange where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkMappedMemoryRange where
        type VkPNextMType VkMappedMemoryRange = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMappedMemoryRange, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMappedMemoryRange, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMappedMemoryRange, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMappedMemoryRange, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkMappedMemoryRange
         where
        type FieldType "pNext" VkMappedMemoryRange = Ptr Void
        type FieldOptional "pNext" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMappedMemoryRange =
             #{offset VkMappedMemoryRange, pNext}
        type FieldIsArray "pNext" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMappedMemoryRange, pNext}

instance CanReadField "pNext" VkMappedMemoryRange where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMappedMemoryRange where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkMemory VkMappedMemoryRange where
        type VkMemoryMType VkMappedMemoryRange = VkDeviceMemory

        {-# NOINLINE vkMemory #-}
        vkMemory x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMappedMemoryRange, memory})

        {-# INLINE vkMemoryByteOffset #-}
        vkMemoryByteOffset ~_
          = #{offset VkMappedMemoryRange, memory}

        {-# INLINE readVkMemory #-}
        readVkMemory p
          = peekByteOff p #{offset VkMappedMemoryRange, memory}

        {-# INLINE writeVkMemory #-}
        writeVkMemory p
          = pokeByteOff p #{offset VkMappedMemoryRange, memory}

instance {-# OVERLAPPING #-} HasField "memory" VkMappedMemoryRange
         where
        type FieldType "memory" VkMappedMemoryRange = VkDeviceMemory
        type FieldOptional "memory" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkMappedMemoryRange =
             #{offset VkMappedMemoryRange, memory}
        type FieldIsArray "memory" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMappedMemoryRange, memory}

instance CanReadField "memory" VkMappedMemoryRange where
        {-# INLINE getField #-}
        getField = vkMemory

        {-# INLINE readField #-}
        readField = readVkMemory

instance CanWriteField "memory" VkMappedMemoryRange where
        {-# INLINE writeField #-}
        writeField = writeVkMemory

instance {-# OVERLAPPING #-} HasVkOffset VkMappedMemoryRange where
        type VkOffsetMType VkMappedMemoryRange = VkDeviceSize

        {-# NOINLINE vkOffset #-}
        vkOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMappedMemoryRange, offset})

        {-# INLINE vkOffsetByteOffset #-}
        vkOffsetByteOffset ~_
          = #{offset VkMappedMemoryRange, offset}

        {-# INLINE readVkOffset #-}
        readVkOffset p
          = peekByteOff p #{offset VkMappedMemoryRange, offset}

        {-# INLINE writeVkOffset #-}
        writeVkOffset p
          = pokeByteOff p #{offset VkMappedMemoryRange, offset}

instance {-# OVERLAPPING #-} HasField "offset" VkMappedMemoryRange
         where
        type FieldType "offset" VkMappedMemoryRange = VkDeviceSize
        type FieldOptional "offset" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkMappedMemoryRange =
             #{offset VkMappedMemoryRange, offset}
        type FieldIsArray "offset" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMappedMemoryRange, offset}

instance CanReadField "offset" VkMappedMemoryRange where
        {-# INLINE getField #-}
        getField = vkOffset

        {-# INLINE readField #-}
        readField = readVkOffset

instance CanWriteField "offset" VkMappedMemoryRange where
        {-# INLINE writeField #-}
        writeField = writeVkOffset

instance {-# OVERLAPPING #-} HasVkSize VkMappedMemoryRange where
        type VkSizeMType VkMappedMemoryRange = VkDeviceSize

        {-# NOINLINE vkSize #-}
        vkSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMappedMemoryRange, size})

        {-# INLINE vkSizeByteOffset #-}
        vkSizeByteOffset ~_
          = #{offset VkMappedMemoryRange, size}

        {-# INLINE readVkSize #-}
        readVkSize p
          = peekByteOff p #{offset VkMappedMemoryRange, size}

        {-# INLINE writeVkSize #-}
        writeVkSize p
          = pokeByteOff p #{offset VkMappedMemoryRange, size}

instance {-# OVERLAPPING #-} HasField "size" VkMappedMemoryRange
         where
        type FieldType "size" VkMappedMemoryRange = VkDeviceSize
        type FieldOptional "size" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkMappedMemoryRange =
             #{offset VkMappedMemoryRange, size}
        type FieldIsArray "size" VkMappedMemoryRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMappedMemoryRange, size}

instance CanReadField "size" VkMappedMemoryRange where
        {-# INLINE getField #-}
        getField = vkSize

        {-# INLINE readField #-}
        readField = readVkSize

instance CanWriteField "size" VkMappedMemoryRange where
        {-# INLINE writeField #-}
        writeField = writeVkSize

instance Show VkMappedMemoryRange where
        showsPrec d x
          = showString "VkMappedMemoryRange {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMemory = " .
                            showsPrec d (vkMemory x) .
                              showString ", " .
                                showString "vkOffset = " .
                                  showsPrec d (vkOffset x) .
                                    showString ", " .
                                      showString "vkSize = " . showsPrec d (vkSize x) . showChar '}'
