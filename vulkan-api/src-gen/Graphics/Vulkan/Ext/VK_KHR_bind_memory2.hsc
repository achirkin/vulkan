#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_bind_memory2
       (-- * Vulkan extension: @VK_KHR_bind_memory2@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Tobias Hector @tobias@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @158@
        VkBindBufferMemoryInfoKHR(..), VkBindImageMemoryInfoKHR(..),
        vkBindBufferMemory2KHR, vkBindImageMemory2KHR,
        VK_KHR_BIND_MEMORY_2_SPEC_VERSION,
        pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION,
        VK_KHR_BIND_MEMORY_2_EXTENSION_NAME,
        pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR,
        pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkBindBufferMemoryInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBuffer                         buffer;
--   >     VkDeviceMemory                   memory;
--   >     VkDeviceSize                     memoryOffset;
--   > } VkBindBufferMemoryInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBindBufferMemoryInfoKHR.html VkBindBufferMemoryInfoKHR registry at www.khronos.org>
data VkBindBufferMemoryInfoKHR = VkBindBufferMemoryInfoKHR## ByteArray##

instance Eq VkBindBufferMemoryInfoKHR where
        (VkBindBufferMemoryInfoKHR## a) == (VkBindBufferMemoryInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkBindBufferMemoryInfoKHR where
        (VkBindBufferMemoryInfoKHR## a) `compare`
          (VkBindBufferMemoryInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkBindBufferMemoryInfoKHR where
        sizeOf ~_ = #{size VkBindBufferMemoryInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBindBufferMemoryInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkBindBufferMemoryInfoKHR),
            I## a <- alignment (undefined :: VkBindBufferMemoryInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkBindBufferMemoryInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkBindBufferMemoryInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkBindBufferMemoryInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkBindBufferMemoryInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkBindBufferMemoryInfoKHR),
            I## a <- alignment (undefined :: VkBindBufferMemoryInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkBindBufferMemoryInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkBindBufferMemoryInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkBindBufferMemoryInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkBindBufferMemoryInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkBindBufferMemoryInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkBindBufferMemoryInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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

-- | > typedef struct VkBindImageMemoryInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImage                          image;
--   >     VkDeviceMemory                   memory;
--   >     VkDeviceSize                     memoryOffset;
--   > } VkBindImageMemoryInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBindImageMemoryInfoKHR.html VkBindImageMemoryInfoKHR registry at www.khronos.org>
data VkBindImageMemoryInfoKHR = VkBindImageMemoryInfoKHR## ByteArray##

instance Eq VkBindImageMemoryInfoKHR where
        (VkBindImageMemoryInfoKHR## a) == (VkBindImageMemoryInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkBindImageMemoryInfoKHR where
        (VkBindImageMemoryInfoKHR## a) `compare`
          (VkBindImageMemoryInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkBindImageMemoryInfoKHR where
        sizeOf ~_ = #{size VkBindImageMemoryInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBindImageMemoryInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkBindImageMemoryInfoKHR),
            I## a <- alignment (undefined :: VkBindImageMemoryInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkBindImageMemoryInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkBindImageMemoryInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkBindImageMemoryInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkBindImageMemoryInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkBindImageMemoryInfoKHR),
            I## a <- alignment (undefined :: VkBindImageMemoryInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkBindImageMemoryInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkBindImageMemoryInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkBindImageMemoryInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkBindImageMemoryInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkBindImageMemoryInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkBindImageMemoryInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkBindImageMemoryInfoKHR
         where
        type VkSTypeMType VkBindImageMemoryInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkBindImageMemoryInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkBindImageMemoryInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkBindImageMemoryInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindImageMemoryInfoKHR where
        type FieldType "sType" VkBindImageMemoryInfoKHR = VkStructureType
        type FieldOptional "sType" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkBindImageMemoryInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkBindImageMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkBindImageMemoryInfoKHR
         where
        type VkPNextMType VkBindImageMemoryInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkBindImageMemoryInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkBindImageMemoryInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkBindImageMemoryInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindImageMemoryInfoKHR where
        type FieldType "pNext" VkBindImageMemoryInfoKHR = Ptr Void
        type FieldOptional "pNext" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkBindImageMemoryInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkBindImageMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkImage VkBindImageMemoryInfoKHR
         where
        type VkImageMType VkBindImageMemoryInfoKHR = VkImage

        {-# NOINLINE vkImage #-}
        vkImage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfoKHR, image})

        {-# INLINE vkImageByteOffset #-}
        vkImageByteOffset ~_
          = #{offset VkBindImageMemoryInfoKHR, image}

        {-# INLINE readVkImage #-}
        readVkImage p
          = peekByteOff p #{offset VkBindImageMemoryInfoKHR, image}

        {-# INLINE writeVkImage #-}
        writeVkImage p
          = pokeByteOff p #{offset VkBindImageMemoryInfoKHR, image}

instance {-# OVERLAPPING #-}
         HasField "image" VkBindImageMemoryInfoKHR where
        type FieldType "image" VkBindImageMemoryInfoKHR = VkImage
        type FieldOptional "image" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "image" VkBindImageMemoryInfoKHR where
        {-# INLINE getField #-}
        getField = vkImage

        {-# INLINE readField #-}
        readField = readVkImage

instance CanWriteField "image" VkBindImageMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkImage

instance {-# OVERLAPPING #-} HasVkMemory VkBindImageMemoryInfoKHR
         where
        type VkMemoryMType VkBindImageMemoryInfoKHR = VkDeviceMemory

        {-# NOINLINE vkMemory #-}
        vkMemory x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfoKHR, memory})

        {-# INLINE vkMemoryByteOffset #-}
        vkMemoryByteOffset ~_
          = #{offset VkBindImageMemoryInfoKHR, memory}

        {-# INLINE readVkMemory #-}
        readVkMemory p
          = peekByteOff p #{offset VkBindImageMemoryInfoKHR, memory}

        {-# INLINE writeVkMemory #-}
        writeVkMemory p
          = pokeByteOff p #{offset VkBindImageMemoryInfoKHR, memory}

instance {-# OVERLAPPING #-}
         HasField "memory" VkBindImageMemoryInfoKHR where
        type FieldType "memory" VkBindImageMemoryInfoKHR = VkDeviceMemory
        type FieldOptional "memory" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "memory" VkBindImageMemoryInfoKHR where
        {-# INLINE getField #-}
        getField = vkMemory

        {-# INLINE readField #-}
        readField = readVkMemory

instance CanWriteField "memory" VkBindImageMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkMemory

instance {-# OVERLAPPING #-}
         HasVkMemoryOffset VkBindImageMemoryInfoKHR where
        type VkMemoryOffsetMType VkBindImageMemoryInfoKHR = VkDeviceSize

        {-# NOINLINE vkMemoryOffset #-}
        vkMemoryOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImageMemoryInfoKHR, memoryOffset})

        {-# INLINE vkMemoryOffsetByteOffset #-}
        vkMemoryOffsetByteOffset ~_
          = #{offset VkBindImageMemoryInfoKHR, memoryOffset}

        {-# INLINE readVkMemoryOffset #-}
        readVkMemoryOffset p
          = peekByteOff p #{offset VkBindImageMemoryInfoKHR, memoryOffset}

        {-# INLINE writeVkMemoryOffset #-}
        writeVkMemoryOffset p
          = pokeByteOff p #{offset VkBindImageMemoryInfoKHR, memoryOffset}

instance {-# OVERLAPPING #-}
         HasField "memoryOffset" VkBindImageMemoryInfoKHR where
        type FieldType "memoryOffset" VkBindImageMemoryInfoKHR =
             VkDeviceSize
        type FieldOptional "memoryOffset" VkBindImageMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "memoryOffset" VkBindImageMemoryInfoKHR where
        {-# INLINE getField #-}
        getField = vkMemoryOffset

        {-# INLINE readField #-}
        readField = readVkMemoryOffset

instance CanWriteField "memoryOffset" VkBindImageMemoryInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkMemoryOffset

instance Show VkBindImageMemoryInfoKHR where
        showsPrec d x
          = showString "VkBindImageMemoryInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkImage = " .
                            showsPrec d (vkImage x) .
                              showString ", " .
                                showString "vkMemory = " .
                                  showsPrec d (vkMemory x) .
                                    showString ", " .
                                      showString "vkMemoryOffset = " .
                                        showsPrec d (vkMemoryOffset x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindBufferMemory2KHR
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindBufferMemoryInfoKHR* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkBindBufferMemory2KHR.html vkBindBufferMemory2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkBindBufferMemory2KHR"
               vkBindBufferMemory2KHR ::
               VkDevice -- ^ device
                        -> Word32 -- ^ bindInfoCount
                                  -> Ptr VkBindBufferMemoryInfoKHR -- ^ pBindInfos
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkBindImageMemory2KHR
--   >     ( VkDevice device
--   >     , uint32_t bindInfoCount
--   >     , const VkBindImageMemoryInfoKHR* pBindInfos
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkBindImageMemory2KHR.html vkBindImageMemory2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkBindImageMemory2KHR"
               vkBindImageMemory2KHR ::
               VkDevice -- ^ device
                        -> Word32 -- ^ bindInfoCount
                                  -> Ptr VkBindImageMemoryInfoKHR -- ^ pBindInfos
                                                                  -> IO VkResult

pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION = 1

type VK_KHR_BIND_MEMORY_2_SPEC_VERSION = 1

pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME :: CString

pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME <-
        (is_VK_KHR_BIND_MEMORY_2_EXTENSION_NAME -> True)
  where VK_KHR_BIND_MEMORY_2_EXTENSION_NAME
          = _VK_KHR_BIND_MEMORY_2_EXTENSION_NAME

_VK_KHR_BIND_MEMORY_2_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_BIND_MEMORY_2_EXTENSION_NAME #-}
_VK_KHR_BIND_MEMORY_2_EXTENSION_NAME
  = Ptr "VK_KHR_bind_memory2\NUL"##

is_VK_KHR_BIND_MEMORY_2_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_BIND_MEMORY_2_EXTENSION_NAME #-}
is_VK_KHR_BIND_MEMORY_2_EXTENSION_NAME
  = (_VK_KHR_BIND_MEMORY_2_EXTENSION_NAME ==)

type VK_KHR_BIND_MEMORY_2_EXTENSION_NAME = "VK_KHR_bind_memory2"

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR =
        VkStructureType 1000157000

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR =
        VkStructureType 1000157001

-- | bitpos = @10@
pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR :: VkImageCreateFlagBits

pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR = VkImageCreateFlagBits 1024
