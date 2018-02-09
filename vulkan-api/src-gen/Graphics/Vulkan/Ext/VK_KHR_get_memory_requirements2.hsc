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
module Graphics.Vulkan.Ext.VK_KHR_get_memory_requirements2
       (-- * Vulkan extension: @VK_KHR_get_memory_requirements2@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jason Ekstrand @jekstrand@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @147@
        VkBufferMemoryRequirementsInfo2KHR(..),
        VkImageMemoryRequirementsInfo2KHR(..),
        VkImageSparseMemoryRequirementsInfo2KHR(..),
        VkMemoryRequirements2KHR(..),
        VkSparseImageMemoryRequirements2KHR(..),
        vkGetImageMemoryRequirements2KHR,
        vkGetBufferMemoryRequirements2KHR,
        vkGetImageSparseMemoryRequirements2KHR,
        VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION,
        pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION,
        VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME,
        pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR,
        pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR,
        pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR,
        pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR,
        pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base             (VkMemoryRequirements, VkSparseImageMemoryRequirements)
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkBufferMemoryRequirementsInfo2KHR {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkBuffer                                                             buffer;
--   > } VkBufferMemoryRequirementsInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkBufferMemoryRequirementsInfo2KHR.html VkBufferMemoryRequirementsInfo2KHR registry at www.khronos.org>
data VkBufferMemoryRequirementsInfo2KHR = VkBufferMemoryRequirementsInfo2KHR## ByteArray##

instance Eq VkBufferMemoryRequirementsInfo2KHR where
        (VkBufferMemoryRequirementsInfo2KHR## a) ==
          (VkBufferMemoryRequirementsInfo2KHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkBufferMemoryRequirementsInfo2KHR where
        (VkBufferMemoryRequirementsInfo2KHR## a) `compare`
          (VkBufferMemoryRequirementsInfo2KHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkBufferMemoryRequirementsInfo2KHR where
        sizeOf ~_ = #{size VkBufferMemoryRequirementsInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBufferMemoryRequirementsInfo2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkBufferMemoryRequirementsInfo2KHR),
            I## a <- alignment (undefined :: VkBufferMemoryRequirementsInfo2KHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkBufferMemoryRequirementsInfo2KHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkBufferMemoryRequirementsInfo2KHR## ba)
          | I## n <- sizeOf (undefined :: VkBufferMemoryRequirementsInfo2KHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkBufferMemoryRequirementsInfo2KHR where
        type StructFields VkBufferMemoryRequirementsInfo2KHR =
             '["sType", "pNext", "buffer"] -- ' closing tick for hsc2hs

        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkBufferMemoryRequirementsInfo2KHR),
            I## a <- alignment (undefined :: VkBufferMemoryRequirementsInfo2KHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkBufferMemoryRequirementsInfo2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkBufferMemoryRequirementsInfo2KHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkBufferMemoryRequirementsInfo2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkBufferMemoryRequirementsInfo2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkBufferMemoryRequirementsInfo2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkBufferMemoryRequirementsInfo2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkBufferMemoryRequirementsInfo2KHR where
        type VkSTypeMType VkBufferMemoryRequirementsInfo2KHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryRequirementsInfo2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkBufferMemoryRequirementsInfo2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkBufferMemoryRequirementsInfo2KHR where
        type FieldType "sType" VkBufferMemoryRequirementsInfo2KHR =
             VkStructureType
        type FieldOptional "sType" VkBufferMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBufferMemoryRequirementsInfo2KHR =
             #{offset VkBufferMemoryRequirementsInfo2KHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryRequirementsInfo2KHR, sType}

instance CanReadField "sType" VkBufferMemoryRequirementsInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkBufferMemoryRequirementsInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkBufferMemoryRequirementsInfo2KHR where
        type VkPNextMType VkBufferMemoryRequirementsInfo2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryRequirementsInfo2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkBufferMemoryRequirementsInfo2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBufferMemoryRequirementsInfo2KHR where
        type FieldType "pNext" VkBufferMemoryRequirementsInfo2KHR =
             Ptr Void
        type FieldOptional "pNext" VkBufferMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBufferMemoryRequirementsInfo2KHR =
             #{offset VkBufferMemoryRequirementsInfo2KHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryRequirementsInfo2KHR, pNext}

instance CanReadField "pNext" VkBufferMemoryRequirementsInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkBufferMemoryRequirementsInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkBuffer VkBufferMemoryRequirementsInfo2KHR where
        type VkBufferMType VkBufferMemoryRequirementsInfo2KHR = VkBuffer

        {-# NOINLINE vkBuffer #-}
        vkBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryRequirementsInfo2KHR, buffer})

        {-# INLINE vkBufferByteOffset #-}
        vkBufferByteOffset ~_
          = #{offset VkBufferMemoryRequirementsInfo2KHR, buffer}

        {-# INLINE readVkBuffer #-}
        readVkBuffer p
          = peekByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, buffer}

        {-# INLINE writeVkBuffer #-}
        writeVkBuffer p
          = pokeByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, buffer}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkBufferMemoryRequirementsInfo2KHR where
        type FieldType "buffer" VkBufferMemoryRequirementsInfo2KHR =
             VkBuffer
        type FieldOptional "buffer" VkBufferMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkBufferMemoryRequirementsInfo2KHR =
             #{offset VkBufferMemoryRequirementsInfo2KHR, buffer}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryRequirementsInfo2KHR, buffer}

instance CanReadField "buffer" VkBufferMemoryRequirementsInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkBuffer

        {-# INLINE readField #-}
        readField = readVkBuffer

instance CanWriteField "buffer" VkBufferMemoryRequirementsInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkBuffer

instance Show VkBufferMemoryRequirementsInfo2KHR where
        showsPrec d x
          = showString "VkBufferMemoryRequirementsInfo2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkBuffer = " . showsPrec d (vkBuffer x) . showChar '}'

-- | > typedef struct VkImageMemoryRequirementsInfo2KHR {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkImage                                                              image;
--   > } VkImageMemoryRequirementsInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageMemoryRequirementsInfo2KHR.html VkImageMemoryRequirementsInfo2KHR registry at www.khronos.org>
data VkImageMemoryRequirementsInfo2KHR = VkImageMemoryRequirementsInfo2KHR## ByteArray##

instance Eq VkImageMemoryRequirementsInfo2KHR where
        (VkImageMemoryRequirementsInfo2KHR## a) ==
          (VkImageMemoryRequirementsInfo2KHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkImageMemoryRequirementsInfo2KHR where
        (VkImageMemoryRequirementsInfo2KHR## a) `compare`
          (VkImageMemoryRequirementsInfo2KHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkImageMemoryRequirementsInfo2KHR where
        sizeOf ~_ = #{size VkImageMemoryRequirementsInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageMemoryRequirementsInfo2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkImageMemoryRequirementsInfo2KHR),
            I## a <- alignment (undefined :: VkImageMemoryRequirementsInfo2KHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkImageMemoryRequirementsInfo2KHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkImageMemoryRequirementsInfo2KHR## ba)
          | I## n <- sizeOf (undefined :: VkImageMemoryRequirementsInfo2KHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkImageMemoryRequirementsInfo2KHR where
        type StructFields VkImageMemoryRequirementsInfo2KHR =
             '["sType", "pNext", "image"] -- ' closing tick for hsc2hs

        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkImageMemoryRequirementsInfo2KHR),
            I## a <- alignment (undefined :: VkImageMemoryRequirementsInfo2KHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkImageMemoryRequirementsInfo2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkImageMemoryRequirementsInfo2KHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkImageMemoryRequirementsInfo2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkImageMemoryRequirementsInfo2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkImageMemoryRequirementsInfo2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkImageMemoryRequirementsInfo2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkImageMemoryRequirementsInfo2KHR where
        type VkSTypeMType VkImageMemoryRequirementsInfo2KHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryRequirementsInfo2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImageMemoryRequirementsInfo2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImageMemoryRequirementsInfo2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImageMemoryRequirementsInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageMemoryRequirementsInfo2KHR where
        type FieldType "sType" VkImageMemoryRequirementsInfo2KHR =
             VkStructureType
        type FieldOptional "sType" VkImageMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageMemoryRequirementsInfo2KHR =
             #{offset VkImageMemoryRequirementsInfo2KHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryRequirementsInfo2KHR, sType}

instance CanReadField "sType" VkImageMemoryRequirementsInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImageMemoryRequirementsInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkImageMemoryRequirementsInfo2KHR where
        type VkPNextMType VkImageMemoryRequirementsInfo2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryRequirementsInfo2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImageMemoryRequirementsInfo2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImageMemoryRequirementsInfo2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImageMemoryRequirementsInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageMemoryRequirementsInfo2KHR where
        type FieldType "pNext" VkImageMemoryRequirementsInfo2KHR = Ptr Void
        type FieldOptional "pNext" VkImageMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageMemoryRequirementsInfo2KHR =
             #{offset VkImageMemoryRequirementsInfo2KHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryRequirementsInfo2KHR, pNext}

instance CanReadField "pNext" VkImageMemoryRequirementsInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImageMemoryRequirementsInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkImage VkImageMemoryRequirementsInfo2KHR where
        type VkImageMType VkImageMemoryRequirementsInfo2KHR = VkImage

        {-# NOINLINE vkImage #-}
        vkImage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryRequirementsInfo2KHR, image})

        {-# INLINE vkImageByteOffset #-}
        vkImageByteOffset ~_
          = #{offset VkImageMemoryRequirementsInfo2KHR, image}

        {-# INLINE readVkImage #-}
        readVkImage p
          = peekByteOff p #{offset VkImageMemoryRequirementsInfo2KHR, image}

        {-# INLINE writeVkImage #-}
        writeVkImage p
          = pokeByteOff p #{offset VkImageMemoryRequirementsInfo2KHR, image}

instance {-# OVERLAPPING #-}
         HasField "image" VkImageMemoryRequirementsInfo2KHR where
        type FieldType "image" VkImageMemoryRequirementsInfo2KHR = VkImage
        type FieldOptional "image" VkImageMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkImageMemoryRequirementsInfo2KHR =
             #{offset VkImageMemoryRequirementsInfo2KHR, image}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryRequirementsInfo2KHR, image}

instance CanReadField "image" VkImageMemoryRequirementsInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkImage

        {-# INLINE readField #-}
        readField = readVkImage

instance CanWriteField "image" VkImageMemoryRequirementsInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkImage

instance Show VkImageMemoryRequirementsInfo2KHR where
        showsPrec d x
          = showString "VkImageMemoryRequirementsInfo2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkImage = " . showsPrec d (vkImage x) . showChar '}'

-- | > typedef struct VkImageSparseMemoryRequirementsInfo2KHR {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkImage                                                              image;
--   > } VkImageSparseMemoryRequirementsInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageSparseMemoryRequirementsInfo2KHR.html VkImageSparseMemoryRequirementsInfo2KHR registry at www.khronos.org>
data VkImageSparseMemoryRequirementsInfo2KHR = VkImageSparseMemoryRequirementsInfo2KHR## ByteArray##

instance Eq VkImageSparseMemoryRequirementsInfo2KHR where
        (VkImageSparseMemoryRequirementsInfo2KHR## a) ==
          (VkImageSparseMemoryRequirementsInfo2KHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkImageSparseMemoryRequirementsInfo2KHR where
        (VkImageSparseMemoryRequirementsInfo2KHR## a) `compare`
          (VkImageSparseMemoryRequirementsInfo2KHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkImageSparseMemoryRequirementsInfo2KHR where
        sizeOf ~_
          = #{size VkImageSparseMemoryRequirementsInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageSparseMemoryRequirementsInfo2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkImageSparseMemoryRequirementsInfo2KHR),
            I## a <- alignment
                      (undefined :: VkImageSparseMemoryRequirementsInfo2KHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkImageSparseMemoryRequirementsInfo2KHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkImageSparseMemoryRequirementsInfo2KHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkImageSparseMemoryRequirementsInfo2KHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkImageSparseMemoryRequirementsInfo2KHR
         where
        type StructFields VkImageSparseMemoryRequirementsInfo2KHR =
             '["sType", "pNext", "image"] -- ' closing tick for hsc2hs

        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkImageSparseMemoryRequirementsInfo2KHR),
            I## a <- alignment
                      (undefined :: VkImageSparseMemoryRequirementsInfo2KHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkImageSparseMemoryRequirementsInfo2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkImageSparseMemoryRequirementsInfo2KHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkImageSparseMemoryRequirementsInfo2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkImageSparseMemoryRequirementsInfo2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkImageSparseMemoryRequirementsInfo2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkImageSparseMemoryRequirementsInfo2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkImageSparseMemoryRequirementsInfo2KHR where
        type VkSTypeMType VkImageSparseMemoryRequirementsInfo2KHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageSparseMemoryRequirementsInfo2KHR where
        type FieldType "sType" VkImageSparseMemoryRequirementsInfo2KHR =
             VkStructureType
        type FieldOptional "sType" VkImageSparseMemoryRequirementsInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageSparseMemoryRequirementsInfo2KHR =
             #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType}

instance CanReadField "sType"
           VkImageSparseMemoryRequirementsInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkImageSparseMemoryRequirementsInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkImageSparseMemoryRequirementsInfo2KHR where
        type VkPNextMType VkImageSparseMemoryRequirementsInfo2KHR =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageSparseMemoryRequirementsInfo2KHR where
        type FieldType "pNext" VkImageSparseMemoryRequirementsInfo2KHR =
             Ptr Void
        type FieldOptional "pNext" VkImageSparseMemoryRequirementsInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageSparseMemoryRequirementsInfo2KHR =
             #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext}

instance CanReadField "pNext"
           VkImageSparseMemoryRequirementsInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkImageSparseMemoryRequirementsInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkImage VkImageSparseMemoryRequirementsInfo2KHR where
        type VkImageMType VkImageSparseMemoryRequirementsInfo2KHR = VkImage

        {-# NOINLINE vkImage #-}
        vkImage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSparseMemoryRequirementsInfo2KHR, image})

        {-# INLINE vkImageByteOffset #-}
        vkImageByteOffset ~_
          = #{offset VkImageSparseMemoryRequirementsInfo2KHR, image}

        {-# INLINE readVkImage #-}
        readVkImage p
          = peekByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, image}

        {-# INLINE writeVkImage #-}
        writeVkImage p
          = pokeByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, image}

instance {-# OVERLAPPING #-}
         HasField "image" VkImageSparseMemoryRequirementsInfo2KHR where
        type FieldType "image" VkImageSparseMemoryRequirementsInfo2KHR =
             VkImage
        type FieldOptional "image" VkImageSparseMemoryRequirementsInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkImageSparseMemoryRequirementsInfo2KHR =
             #{offset VkImageSparseMemoryRequirementsInfo2KHR, image}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSparseMemoryRequirementsInfo2KHR, image}

instance CanReadField "image"
           VkImageSparseMemoryRequirementsInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkImage

        {-# INLINE readField #-}
        readField = readVkImage

instance CanWriteField "image"
           VkImageSparseMemoryRequirementsInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkImage

instance Show VkImageSparseMemoryRequirementsInfo2KHR where
        showsPrec d x
          = showString "VkImageSparseMemoryRequirementsInfo2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkImage = " . showsPrec d (vkImage x) . showChar '}'

-- | > typedef struct VkMemoryRequirements2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkMemoryRequirements                                                 memoryRequirements;
--   > } VkMemoryRequirements2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryRequirements2KHR.html VkMemoryRequirements2KHR registry at www.khronos.org>
data VkMemoryRequirements2KHR = VkMemoryRequirements2KHR## ByteArray##

instance Eq VkMemoryRequirements2KHR where
        (VkMemoryRequirements2KHR## a) == (VkMemoryRequirements2KHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkMemoryRequirements2KHR where
        (VkMemoryRequirements2KHR## a) `compare`
          (VkMemoryRequirements2KHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkMemoryRequirements2KHR where
        sizeOf ~_ = #{size VkMemoryRequirements2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryRequirements2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkMemoryRequirements2KHR),
            I## a <- alignment (undefined :: VkMemoryRequirements2KHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkMemoryRequirements2KHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkMemoryRequirements2KHR## ba)
          | I## n <- sizeOf (undefined :: VkMemoryRequirements2KHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryRequirements2KHR where
        type StructFields VkMemoryRequirements2KHR =
             '["sType", "pNext", "memoryRequirements"] -- ' closing tick for hsc2hs

        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkMemoryRequirements2KHR),
            I## a <- alignment (undefined :: VkMemoryRequirements2KHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkMemoryRequirements2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkMemoryRequirements2KHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkMemoryRequirements2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkMemoryRequirements2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkMemoryRequirements2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkMemoryRequirements2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkMemoryRequirements2KHR
         where
        type VkSTypeMType VkMemoryRequirements2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMemoryRequirements2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryRequirements2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryRequirements2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryRequirements2KHR where
        type FieldType "sType" VkMemoryRequirements2KHR = VkStructureType
        type FieldOptional "sType" VkMemoryRequirements2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryRequirements2KHR =
             #{offset VkMemoryRequirements2KHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryRequirements2KHR, sType}

instance CanReadField "sType" VkMemoryRequirements2KHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkMemoryRequirements2KHR
         where
        type VkPNextMType VkMemoryRequirements2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMemoryRequirements2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryRequirements2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryRequirements2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryRequirements2KHR where
        type FieldType "pNext" VkMemoryRequirements2KHR = Ptr Void
        type FieldOptional "pNext" VkMemoryRequirements2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryRequirements2KHR =
             #{offset VkMemoryRequirements2KHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryRequirements2KHR, pNext}

instance CanReadField "pNext" VkMemoryRequirements2KHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkMemoryRequirements VkMemoryRequirements2KHR where
        type VkMemoryRequirementsMType VkMemoryRequirements2KHR =
             VkMemoryRequirements

        {-# NOINLINE vkMemoryRequirements #-}
        vkMemoryRequirements x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements2KHR, memoryRequirements})

        {-# INLINE vkMemoryRequirementsByteOffset #-}
        vkMemoryRequirementsByteOffset ~_
          = #{offset VkMemoryRequirements2KHR, memoryRequirements}

        {-# INLINE readVkMemoryRequirements #-}
        readVkMemoryRequirements p
          = peekByteOff p #{offset VkMemoryRequirements2KHR, memoryRequirements}

        {-# INLINE writeVkMemoryRequirements #-}
        writeVkMemoryRequirements p
          = pokeByteOff p #{offset VkMemoryRequirements2KHR, memoryRequirements}

instance {-# OVERLAPPING #-}
         HasField "memoryRequirements" VkMemoryRequirements2KHR where
        type FieldType "memoryRequirements" VkMemoryRequirements2KHR =
             VkMemoryRequirements
        type FieldOptional "memoryRequirements" VkMemoryRequirements2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryRequirements" VkMemoryRequirements2KHR =
             #{offset VkMemoryRequirements2KHR, memoryRequirements}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryRequirements2KHR, memoryRequirements}

instance CanReadField "memoryRequirements" VkMemoryRequirements2KHR
         where
        {-# INLINE getField #-}
        getField = vkMemoryRequirements

        {-# INLINE readField #-}
        readField = readVkMemoryRequirements

instance Show VkMemoryRequirements2KHR where
        showsPrec d x
          = showString "VkMemoryRequirements2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMemoryRequirements = " .
                            showsPrec d (vkMemoryRequirements x) . showChar '}'

-- | > typedef struct VkSparseImageMemoryRequirements2KHR {
--   >     VkStructureType sType;
--   >     void*                                       pNext;
--   >     VkSparseImageMemoryRequirements                                      memoryRequirements;
--   > } VkSparseImageMemoryRequirements2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSparseImageMemoryRequirements2KHR.html VkSparseImageMemoryRequirements2KHR registry at www.khronos.org>
data VkSparseImageMemoryRequirements2KHR = VkSparseImageMemoryRequirements2KHR## ByteArray##

instance Eq VkSparseImageMemoryRequirements2KHR where
        (VkSparseImageMemoryRequirements2KHR## a) ==
          (VkSparseImageMemoryRequirements2KHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageMemoryRequirements2KHR where
        (VkSparseImageMemoryRequirements2KHR## a) `compare`
          (VkSparseImageMemoryRequirements2KHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkSparseImageMemoryRequirements2KHR where
        sizeOf ~_ = #{size VkSparseImageMemoryRequirements2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseImageMemoryRequirements2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkSparseImageMemoryRequirements2KHR),
            I## a <- alignment
                      (undefined :: VkSparseImageMemoryRequirements2KHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkSparseImageMemoryRequirements2KHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkSparseImageMemoryRequirements2KHR## ba)
          | I## n <- sizeOf (undefined :: VkSparseImageMemoryRequirements2KHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkSparseImageMemoryRequirements2KHR where
        type StructFields VkSparseImageMemoryRequirements2KHR =
             '["sType", "pNext", "memoryRequirements"] -- ' closing tick for hsc2hs

        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkSparseImageMemoryRequirements2KHR),
            I## a <- alignment
                      (undefined :: VkSparseImageMemoryRequirements2KHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkSparseImageMemoryRequirements2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkSparseImageMemoryRequirements2KHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkSparseImageMemoryRequirements2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkSparseImageMemoryRequirements2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkSparseImageMemoryRequirements2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkSparseImageMemoryRequirements2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkSparseImageMemoryRequirements2KHR where
        type VkSTypeMType VkSparseImageMemoryRequirements2KHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSparseImageMemoryRequirements2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkSparseImageMemoryRequirements2KHR where
        type FieldType "sType" VkSparseImageMemoryRequirements2KHR =
             VkStructureType
        type FieldOptional "sType" VkSparseImageMemoryRequirements2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSparseImageMemoryRequirements2KHR =
             #{offset VkSparseImageMemoryRequirements2KHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements2KHR, sType}

instance CanReadField "sType" VkSparseImageMemoryRequirements2KHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkSparseImageMemoryRequirements2KHR where
        type VkPNextMType VkSparseImageMemoryRequirements2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSparseImageMemoryRequirements2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSparseImageMemoryRequirements2KHR where
        type FieldType "pNext" VkSparseImageMemoryRequirements2KHR =
             Ptr Void
        type FieldOptional "pNext" VkSparseImageMemoryRequirements2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSparseImageMemoryRequirements2KHR =
             #{offset VkSparseImageMemoryRequirements2KHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements2KHR, pNext}

instance CanReadField "pNext" VkSparseImageMemoryRequirements2KHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkMemoryRequirements VkSparseImageMemoryRequirements2KHR where
        type VkMemoryRequirementsMType VkSparseImageMemoryRequirements2KHR
             = VkSparseImageMemoryRequirements

        {-# NOINLINE vkMemoryRequirements #-}
        vkMemoryRequirements x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements2KHR, memoryRequirements})

        {-# INLINE vkMemoryRequirementsByteOffset #-}
        vkMemoryRequirementsByteOffset ~_
          = #{offset VkSparseImageMemoryRequirements2KHR, memoryRequirements}

        {-# INLINE readVkMemoryRequirements #-}
        readVkMemoryRequirements p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements2KHR, memoryRequirements}

        {-# INLINE writeVkMemoryRequirements #-}
        writeVkMemoryRequirements p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements2KHR, memoryRequirements}

instance {-# OVERLAPPING #-}
         HasField "memoryRequirements" VkSparseImageMemoryRequirements2KHR
         where
        type FieldType "memoryRequirements"
               VkSparseImageMemoryRequirements2KHR
             = VkSparseImageMemoryRequirements
        type FieldOptional "memoryRequirements"
               VkSparseImageMemoryRequirements2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryRequirements"
               VkSparseImageMemoryRequirements2KHR
             =
             #{offset VkSparseImageMemoryRequirements2KHR, memoryRequirements}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements2KHR, memoryRequirements}

instance CanReadField "memoryRequirements"
           VkSparseImageMemoryRequirements2KHR
         where
        {-# INLINE getField #-}
        getField = vkMemoryRequirements

        {-# INLINE readField #-}
        readField = readVkMemoryRequirements

instance Show VkSparseImageMemoryRequirements2KHR where
        showsPrec d x
          = showString "VkSparseImageMemoryRequirements2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMemoryRequirements = " .
                            showsPrec d (vkMemoryRequirements x) . showChar '}'

-- | > void vkGetImageMemoryRequirements2KHR
--   >     ( VkDevice device
--   >     , const VkImageMemoryRequirementsInfo2KHR* pInfo
--   >     , VkMemoryRequirements2KHR* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetImageMemoryRequirements2KHR.html vkGetImageMemoryRequirements2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetImageMemoryRequirements2KHR"
               vkGetImageMemoryRequirements2KHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageMemoryRequirementsInfo2KHR -- ^ pInfo
                                                       ->
                   Ptr VkMemoryRequirements2KHR -- ^ pMemoryRequirements
                                                -> IO ()

-- | > void vkGetBufferMemoryRequirements2KHR
--   >     ( VkDevice device
--   >     , const VkBufferMemoryRequirementsInfo2KHR* pInfo
--   >     , VkMemoryRequirements2KHR* pMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetBufferMemoryRequirements2KHR.html vkGetBufferMemoryRequirements2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetBufferMemoryRequirements2KHR"
               vkGetBufferMemoryRequirements2KHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkBufferMemoryRequirementsInfo2KHR -- ^ pInfo
                                                        ->
                   Ptr VkMemoryRequirements2KHR -- ^ pMemoryRequirements
                                                -> IO ()

-- | > void vkGetImageSparseMemoryRequirements2KHR
--   >     ( VkDevice device
--   >     , const VkImageSparseMemoryRequirementsInfo2KHR* pInfo
--   >     , uint32_t* pSparseMemoryRequirementCount
--   >     , VkSparseImageMemoryRequirements2KHR* pSparseMemoryRequirements
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetImageSparseMemoryRequirements2KHR.html vkGetImageSparseMemoryRequirements2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetImageSparseMemoryRequirements2KHR"
               vkGetImageSparseMemoryRequirements2KHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkImageSparseMemoryRequirementsInfo2KHR -- ^ pInfo
                                                             ->
                   Ptr Word32 -- ^ pSparseMemoryRequirementCount
                              -> Ptr VkSparseImageMemoryRequirements2KHR -- ^ pSparseMemoryRequirements
                                                                         -> IO ()

pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION = 1

type VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION = 1

pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME :: CString

pattern VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME <-
        (is_VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME -> True)
  where VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME
          = _VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME

_VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME #-}
_VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME
  = Ptr "VK_KHR_get_memory_requirements2\NUL"##

is_VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME ::
                                                   CString -> Bool

{-# INLINE is_VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME #-}
is_VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME
  = (_VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME ==)

type VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME =
     "VK_KHR_get_memory_requirements2"

pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR =
        VkStructureType 1000146000

pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR =
        VkStructureType 1000146001

pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR
        = VkStructureType 1000146002

pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR =
        VkStructureType 1000146003

pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR =
        VkStructureType 1000146004
