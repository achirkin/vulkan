#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBufferMemoryBarrier
       (VkBufferMemoryBarrier(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkAccessFlags   (VkAccessFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkBuffer)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkBufferMemoryBarrier {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkAccessFlags          srcAccessMask;
--   >     VkAccessFlags          dstAccessMask;
--   >     uint32_t               srcQueueFamilyIndex;
--   >     uint32_t               dstQueueFamilyIndex;
--   >     VkBuffer               buffer;
--   >     VkDeviceSize           offset;
--   >     VkDeviceSize           size;
--   > } VkBufferMemoryBarrier;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBufferMemoryBarrier.html VkBufferMemoryBarrier registry at www.khronos.org>
data VkBufferMemoryBarrier = VkBufferMemoryBarrier## Addr##
                                                    ByteArray##

instance Eq VkBufferMemoryBarrier where
        (VkBufferMemoryBarrier## a _) == x@(VkBufferMemoryBarrier## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBufferMemoryBarrier where
        (VkBufferMemoryBarrier## a _) `compare`
          x@(VkBufferMemoryBarrier## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBufferMemoryBarrier where
        sizeOf ~_ = #{size VkBufferMemoryBarrier}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBufferMemoryBarrier}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBufferMemoryBarrier where
        unsafeAddr (VkBufferMemoryBarrier## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBufferMemoryBarrier## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBufferMemoryBarrier## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBufferMemoryBarrier where
        type StructFields VkBufferMemoryBarrier =
             '["sType", "pNext", "srcAccessMask", "dstAccessMask", -- ' closing tick for hsc2hs
               "srcQueueFamilyIndex", "dstQueueFamilyIndex", "buffer", "offset",
               "size"]
        type CUnionType VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBufferMemoryBarrier = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkBufferMemoryBarrier where
        type VkSTypeMType VkBufferMemoryBarrier = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkBufferMemoryBarrier, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkBufferMemoryBarrier, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkBufferMemoryBarrier
         where
        type FieldType "sType" VkBufferMemoryBarrier = VkStructureType
        type FieldOptional "sType" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, sType}
        type FieldIsArray "sType" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferMemoryBarrier, sType}

instance CanReadField "sType" VkBufferMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkBufferMemoryBarrier where
        type VkPNextMType VkBufferMemoryBarrier = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkBufferMemoryBarrier, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkBufferMemoryBarrier, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkBufferMemoryBarrier
         where
        type FieldType "pNext" VkBufferMemoryBarrier = Ptr Void
        type FieldOptional "pNext" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, pNext}
        type FieldIsArray "pNext" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferMemoryBarrier, pNext}

instance CanReadField "pNext" VkBufferMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSrcAccessMask VkBufferMemoryBarrier where
        type VkSrcAccessMaskMType VkBufferMemoryBarrier = VkAccessFlags

        {-# NOINLINE vkSrcAccessMask #-}
        vkSrcAccessMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, srcAccessMask})

        {-# INLINE vkSrcAccessMaskByteOffset #-}
        vkSrcAccessMaskByteOffset ~_
          = #{offset VkBufferMemoryBarrier, srcAccessMask}

        {-# INLINE readVkSrcAccessMask #-}
        readVkSrcAccessMask p
          = peekByteOff p #{offset VkBufferMemoryBarrier, srcAccessMask}

        {-# INLINE writeVkSrcAccessMask #-}
        writeVkSrcAccessMask p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         HasField "srcAccessMask" VkBufferMemoryBarrier where
        type FieldType "srcAccessMask" VkBufferMemoryBarrier =
             VkAccessFlags
        type FieldOptional "srcAccessMask" VkBufferMemoryBarrier = 'True -- ' closing tick for hsc2hs
        type FieldOffset "srcAccessMask" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, srcAccessMask}
        type FieldIsArray "srcAccessMask" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryBarrier, srcAccessMask}

instance CanReadField "srcAccessMask" VkBufferMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkSrcAccessMask

        {-# INLINE readField #-}
        readField = readVkSrcAccessMask

instance CanWriteField "srcAccessMask" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkSrcAccessMask

instance {-# OVERLAPPING #-}
         HasVkDstAccessMask VkBufferMemoryBarrier where
        type VkDstAccessMaskMType VkBufferMemoryBarrier = VkAccessFlags

        {-# NOINLINE vkDstAccessMask #-}
        vkDstAccessMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, dstAccessMask})

        {-# INLINE vkDstAccessMaskByteOffset #-}
        vkDstAccessMaskByteOffset ~_
          = #{offset VkBufferMemoryBarrier, dstAccessMask}

        {-# INLINE readVkDstAccessMask #-}
        readVkDstAccessMask p
          = peekByteOff p #{offset VkBufferMemoryBarrier, dstAccessMask}

        {-# INLINE writeVkDstAccessMask #-}
        writeVkDstAccessMask p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         HasField "dstAccessMask" VkBufferMemoryBarrier where
        type FieldType "dstAccessMask" VkBufferMemoryBarrier =
             VkAccessFlags
        type FieldOptional "dstAccessMask" VkBufferMemoryBarrier = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dstAccessMask" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, dstAccessMask}
        type FieldIsArray "dstAccessMask" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryBarrier, dstAccessMask}

instance CanReadField "dstAccessMask" VkBufferMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkDstAccessMask

        {-# INLINE readField #-}
        readField = readVkDstAccessMask

instance CanWriteField "dstAccessMask" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkDstAccessMask

instance {-# OVERLAPPING #-}
         HasVkSrcQueueFamilyIndex VkBufferMemoryBarrier where
        type VkSrcQueueFamilyIndexMType VkBufferMemoryBarrier = Word32

        {-# NOINLINE vkSrcQueueFamilyIndex #-}
        vkSrcQueueFamilyIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex})

        {-# INLINE vkSrcQueueFamilyIndexByteOffset #-}
        vkSrcQueueFamilyIndexByteOffset ~_
          = #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex}

        {-# INLINE readVkSrcQueueFamilyIndex #-}
        readVkSrcQueueFamilyIndex p
          = peekByteOff p #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex}

        {-# INLINE writeVkSrcQueueFamilyIndex #-}
        writeVkSrcQueueFamilyIndex p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         HasField "srcQueueFamilyIndex" VkBufferMemoryBarrier where
        type FieldType "srcQueueFamilyIndex" VkBufferMemoryBarrier = Word32
        type FieldOptional "srcQueueFamilyIndex" VkBufferMemoryBarrier =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "srcQueueFamilyIndex" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex}
        type FieldIsArray "srcQueueFamilyIndex" VkBufferMemoryBarrier =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex}

instance CanReadField "srcQueueFamilyIndex" VkBufferMemoryBarrier
         where
        {-# INLINE getField #-}
        getField = vkSrcQueueFamilyIndex

        {-# INLINE readField #-}
        readField = readVkSrcQueueFamilyIndex

instance CanWriteField "srcQueueFamilyIndex" VkBufferMemoryBarrier
         where
        {-# INLINE writeField #-}
        writeField = writeVkSrcQueueFamilyIndex

instance {-# OVERLAPPING #-}
         HasVkDstQueueFamilyIndex VkBufferMemoryBarrier where
        type VkDstQueueFamilyIndexMType VkBufferMemoryBarrier = Word32

        {-# NOINLINE vkDstQueueFamilyIndex #-}
        vkDstQueueFamilyIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex})

        {-# INLINE vkDstQueueFamilyIndexByteOffset #-}
        vkDstQueueFamilyIndexByteOffset ~_
          = #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex}

        {-# INLINE readVkDstQueueFamilyIndex #-}
        readVkDstQueueFamilyIndex p
          = peekByteOff p #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex}

        {-# INLINE writeVkDstQueueFamilyIndex #-}
        writeVkDstQueueFamilyIndex p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         HasField "dstQueueFamilyIndex" VkBufferMemoryBarrier where
        type FieldType "dstQueueFamilyIndex" VkBufferMemoryBarrier = Word32
        type FieldOptional "dstQueueFamilyIndex" VkBufferMemoryBarrier =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "dstQueueFamilyIndex" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex}
        type FieldIsArray "dstQueueFamilyIndex" VkBufferMemoryBarrier =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex}

instance CanReadField "dstQueueFamilyIndex" VkBufferMemoryBarrier
         where
        {-# INLINE getField #-}
        getField = vkDstQueueFamilyIndex

        {-# INLINE readField #-}
        readField = readVkDstQueueFamilyIndex

instance CanWriteField "dstQueueFamilyIndex" VkBufferMemoryBarrier
         where
        {-# INLINE writeField #-}
        writeField = writeVkDstQueueFamilyIndex

instance {-# OVERLAPPING #-} HasVkBuffer VkBufferMemoryBarrier
         where
        type VkBufferMType VkBufferMemoryBarrier = VkBuffer

        {-# NOINLINE vkBuffer #-}
        vkBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, buffer})

        {-# INLINE vkBufferByteOffset #-}
        vkBufferByteOffset ~_
          = #{offset VkBufferMemoryBarrier, buffer}

        {-# INLINE readVkBuffer #-}
        readVkBuffer p
          = peekByteOff p #{offset VkBufferMemoryBarrier, buffer}

        {-# INLINE writeVkBuffer #-}
        writeVkBuffer p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, buffer}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkBufferMemoryBarrier where
        type FieldType "buffer" VkBufferMemoryBarrier = VkBuffer
        type FieldOptional "buffer" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, buffer}
        type FieldIsArray "buffer" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferMemoryBarrier, buffer}

instance CanReadField "buffer" VkBufferMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkBuffer

        {-# INLINE readField #-}
        readField = readVkBuffer

instance CanWriteField "buffer" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkBuffer

instance {-# OVERLAPPING #-} HasVkOffset VkBufferMemoryBarrier
         where
        type VkOffsetMType VkBufferMemoryBarrier = VkDeviceSize

        {-# NOINLINE vkOffset #-}
        vkOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, offset})

        {-# INLINE vkOffsetByteOffset #-}
        vkOffsetByteOffset ~_
          = #{offset VkBufferMemoryBarrier, offset}

        {-# INLINE readVkOffset #-}
        readVkOffset p
          = peekByteOff p #{offset VkBufferMemoryBarrier, offset}

        {-# INLINE writeVkOffset #-}
        writeVkOffset p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, offset}

instance {-# OVERLAPPING #-}
         HasField "offset" VkBufferMemoryBarrier where
        type FieldType "offset" VkBufferMemoryBarrier = VkDeviceSize
        type FieldOptional "offset" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, offset}
        type FieldIsArray "offset" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferMemoryBarrier, offset}

instance CanReadField "offset" VkBufferMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkOffset

        {-# INLINE readField #-}
        readField = readVkOffset

instance CanWriteField "offset" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkOffset

instance {-# OVERLAPPING #-} HasVkSize VkBufferMemoryBarrier where
        type VkSizeMType VkBufferMemoryBarrier = VkDeviceSize

        {-# NOINLINE vkSize #-}
        vkSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, size})

        {-# INLINE vkSizeByteOffset #-}
        vkSizeByteOffset ~_
          = #{offset VkBufferMemoryBarrier, size}

        {-# INLINE readVkSize #-}
        readVkSize p
          = peekByteOff p #{offset VkBufferMemoryBarrier, size}

        {-# INLINE writeVkSize #-}
        writeVkSize p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, size}

instance {-# OVERLAPPING #-} HasField "size" VkBufferMemoryBarrier
         where
        type FieldType "size" VkBufferMemoryBarrier = VkDeviceSize
        type FieldOptional "size" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, size}
        type FieldIsArray "size" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferMemoryBarrier, size}

instance CanReadField "size" VkBufferMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkSize

        {-# INLINE readField #-}
        readField = readVkSize

instance CanWriteField "size" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkSize

instance Show VkBufferMemoryBarrier where
        showsPrec d x
          = showString "VkBufferMemoryBarrier {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSrcAccessMask = " .
                            showsPrec d (vkSrcAccessMask x) .
                              showString ", " .
                                showString "vkDstAccessMask = " .
                                  showsPrec d (vkDstAccessMask x) .
                                    showString ", " .
                                      showString "vkSrcQueueFamilyIndex = " .
                                        showsPrec d (vkSrcQueueFamilyIndex x) .
                                          showString ", " .
                                            showString "vkDstQueueFamilyIndex = " .
                                              showsPrec d (vkDstQueueFamilyIndex x) .
                                                showString ", " .
                                                  showString "vkBuffer = " .
                                                    showsPrec d (vkBuffer x) .
                                                      showString ", " .
                                                        showString "vkOffset = " .
                                                          showsPrec d (vkOffset x) .
                                                            showString ", " .
                                                              showString "vkSize = " .
                                                                showsPrec d (vkSize x) .
                                                                  showChar '}'
