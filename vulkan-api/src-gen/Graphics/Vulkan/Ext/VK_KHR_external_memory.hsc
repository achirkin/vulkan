#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_KHR_external_memory
       (-- * Vulkan extension: @VK_KHR_external_memory@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @73@
        --
        -- Required extensions: 'VK_KHR_external_memory_capabilities'.
        --

        -- ** Required extensions: 'VK_KHR_external_memory_capabilities'.
        VkExternalMemoryImageCreateInfoKHR(..),
        VkExternalMemoryBufferCreateInfoKHR(..),
        VkExportMemoryAllocateInfoKHR(..),
        VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION,
        VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR,
        pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR,
        pattern VK_QUEUE_FAMILY_EXTERNAL_KHR, VK_QUEUE_FAMILY_EXTERNAL_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VK_QUEUE_FAMILY_EXTERNAL_KHR, pattern VK_QUEUE_FAMILY_EXTERNAL_KHR,
                                                   VkExternalMemoryHandleTypeFlagsKHR,
                                                   VkResult (..),
                                                   VkStructureType,
                                                   VkStructureType (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkExternalMemoryImageCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsKHR handleTypes;
--   > } VkExternalMemoryImageCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalMemoryImageCreateInfoKHR.html VkExternalMemoryImageCreateInfoKHR registry at www.khronos.org>
data VkExternalMemoryImageCreateInfoKHR = VkExternalMemoryImageCreateInfoKHR## ByteArray##

instance Eq VkExternalMemoryImageCreateInfoKHR where
        (VkExternalMemoryImageCreateInfoKHR## a) ==
          (VkExternalMemoryImageCreateInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryImageCreateInfoKHR where
        (VkExternalMemoryImageCreateInfoKHR## a) `compare`
          (VkExternalMemoryImageCreateInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryImageCreateInfoKHR where
        sizeOf ~_ = #{size VkExternalMemoryImageCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryImageCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkExternalMemoryImageCreateInfoKHR),
            I## a <- alignment (undefined :: VkExternalMemoryImageCreateInfoKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExternalMemoryImageCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExternalMemoryImageCreateInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkExternalMemoryImageCreateInfoKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExternalMemoryImageCreateInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkExternalMemoryImageCreateInfoKHR),
            I## a <- alignment (undefined :: VkExternalMemoryImageCreateInfoKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExternalMemoryImageCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExternalMemoryImageCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkExternalMemoryImageCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExternalMemoryImageCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExternalMemoryImageCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExternalMemoryImageCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkExternalMemoryImageCreateInfoKHR where
        type VkSTypeMType VkExternalMemoryImageCreateInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExternalMemoryImageCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalMemoryImageCreateInfoKHR where
        type FieldType "sType" VkExternalMemoryImageCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExternalMemoryImageCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkExternalMemoryImageCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkExternalMemoryImageCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkExternalMemoryImageCreateInfoKHR where
        type VkPNextMType VkExternalMemoryImageCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExternalMemoryImageCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalMemoryImageCreateInfoKHR where
        type FieldType "pNext" VkExternalMemoryImageCreateInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkExternalMemoryImageCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkExternalMemoryImageCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkExternalMemoryImageCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkHandleTypes VkExternalMemoryImageCreateInfoKHR where
        type VkHandleTypesMType VkExternalMemoryImageCreateInfoKHR =
             VkExternalMemoryHandleTypeFlagsKHR

        {-# NOINLINE vkHandleTypes #-}
        vkHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoKHR, handleTypes})

        {-# INLINE vkHandleTypesByteOffset #-}
        vkHandleTypesByteOffset ~_
          = #{offset VkExternalMemoryImageCreateInfoKHR, handleTypes}

        {-# INLINE readVkHandleTypes #-}
        readVkHandleTypes p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, handleTypes}

        {-# INLINE writeVkHandleTypes #-}
        writeVkHandleTypes p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExternalMemoryImageCreateInfoKHR where
        type FieldType "handleTypes" VkExternalMemoryImageCreateInfoKHR =
             VkExternalMemoryHandleTypeFlagsKHR
        type FieldOptional "handleTypes" VkExternalMemoryImageCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "handleTypes"
           VkExternalMemoryImageCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleTypes

        {-# INLINE readField #-}
        readField = readVkHandleTypes

instance CanWriteField "handleTypes"
           VkExternalMemoryImageCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleTypes

instance Show VkExternalMemoryImageCreateInfoKHR where
        showsPrec d x
          = showString "VkExternalMemoryImageCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleTypes = " .
                            showsPrec d (vkHandleTypes x) . showChar '}'

-- | > typedef struct VkExternalMemoryBufferCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsKHR handleTypes;
--   > } VkExternalMemoryBufferCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalMemoryBufferCreateInfoKHR.html VkExternalMemoryBufferCreateInfoKHR registry at www.khronos.org>
data VkExternalMemoryBufferCreateInfoKHR = VkExternalMemoryBufferCreateInfoKHR## ByteArray##

instance Eq VkExternalMemoryBufferCreateInfoKHR where
        (VkExternalMemoryBufferCreateInfoKHR## a) ==
          (VkExternalMemoryBufferCreateInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryBufferCreateInfoKHR where
        (VkExternalMemoryBufferCreateInfoKHR## a) `compare`
          (VkExternalMemoryBufferCreateInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryBufferCreateInfoKHR where
        sizeOf ~_ = #{size VkExternalMemoryBufferCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryBufferCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkExternalMemoryBufferCreateInfoKHR),
            I## a <- alignment
                      (undefined :: VkExternalMemoryBufferCreateInfoKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExternalMemoryBufferCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExternalMemoryBufferCreateInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkExternalMemoryBufferCreateInfoKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExternalMemoryBufferCreateInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkExternalMemoryBufferCreateInfoKHR),
            I## a <- alignment
                      (undefined :: VkExternalMemoryBufferCreateInfoKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExternalMemoryBufferCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExternalMemoryBufferCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkExternalMemoryBufferCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExternalMemoryBufferCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExternalMemoryBufferCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExternalMemoryBufferCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkExternalMemoryBufferCreateInfoKHR where
        type VkSTypeMType VkExternalMemoryBufferCreateInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryBufferCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExternalMemoryBufferCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalMemoryBufferCreateInfoKHR where
        type FieldType "sType" VkExternalMemoryBufferCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExternalMemoryBufferCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkExternalMemoryBufferCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkExternalMemoryBufferCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkExternalMemoryBufferCreateInfoKHR where
        type VkPNextMType VkExternalMemoryBufferCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryBufferCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExternalMemoryBufferCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalMemoryBufferCreateInfoKHR where
        type FieldType "pNext" VkExternalMemoryBufferCreateInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkExternalMemoryBufferCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkExternalMemoryBufferCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkExternalMemoryBufferCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkHandleTypes VkExternalMemoryBufferCreateInfoKHR where
        type VkHandleTypesMType VkExternalMemoryBufferCreateInfoKHR =
             VkExternalMemoryHandleTypeFlagsKHR

        {-# NOINLINE vkHandleTypes #-}
        vkHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryBufferCreateInfoKHR, handleTypes})

        {-# INLINE vkHandleTypesByteOffset #-}
        vkHandleTypesByteOffset ~_
          = #{offset VkExternalMemoryBufferCreateInfoKHR, handleTypes}

        {-# INLINE readVkHandleTypes #-}
        readVkHandleTypes p
          = peekByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, handleTypes}

        {-# INLINE writeVkHandleTypes #-}
        writeVkHandleTypes p
          = pokeByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExternalMemoryBufferCreateInfoKHR where
        type FieldType "handleTypes" VkExternalMemoryBufferCreateInfoKHR =
             VkExternalMemoryHandleTypeFlagsKHR
        type FieldOptional "handleTypes"
               VkExternalMemoryBufferCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs

instance CanReadField "handleTypes"
           VkExternalMemoryBufferCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleTypes

        {-# INLINE readField #-}
        readField = readVkHandleTypes

instance CanWriteField "handleTypes"
           VkExternalMemoryBufferCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleTypes

instance Show VkExternalMemoryBufferCreateInfoKHR where
        showsPrec d x
          = showString "VkExternalMemoryBufferCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleTypes = " .
                            showsPrec d (vkHandleTypes x) . showChar '}'

-- | > typedef struct VkExportMemoryAllocateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsKHR handleTypes;
--   > } VkExportMemoryAllocateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExportMemoryAllocateInfoKHR.html VkExportMemoryAllocateInfoKHR registry at www.khronos.org>
data VkExportMemoryAllocateInfoKHR = VkExportMemoryAllocateInfoKHR## ByteArray##

instance Eq VkExportMemoryAllocateInfoKHR where
        (VkExportMemoryAllocateInfoKHR## a) ==
          (VkExportMemoryAllocateInfoKHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExportMemoryAllocateInfoKHR where
        (VkExportMemoryAllocateInfoKHR## a) `compare`
          (VkExportMemoryAllocateInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExportMemoryAllocateInfoKHR where
        sizeOf ~_ = #{size VkExportMemoryAllocateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportMemoryAllocateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkExportMemoryAllocateInfoKHR),
            I## a <- alignment (undefined :: VkExportMemoryAllocateInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExportMemoryAllocateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExportMemoryAllocateInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkExportMemoryAllocateInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExportMemoryAllocateInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkExportMemoryAllocateInfoKHR),
            I## a <- alignment (undefined :: VkExportMemoryAllocateInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExportMemoryAllocateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExportMemoryAllocateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkExportMemoryAllocateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExportMemoryAllocateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExportMemoryAllocateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExportMemoryAllocateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkExportMemoryAllocateInfoKHR where
        type VkSTypeMType VkExportMemoryAllocateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExportMemoryAllocateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportMemoryAllocateInfoKHR where
        type FieldType "sType" VkExportMemoryAllocateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExportMemoryAllocateInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkExportMemoryAllocateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkExportMemoryAllocateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkExportMemoryAllocateInfoKHR where
        type VkPNextMType VkExportMemoryAllocateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExportMemoryAllocateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportMemoryAllocateInfoKHR where
        type FieldType "pNext" VkExportMemoryAllocateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkExportMemoryAllocateInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkExportMemoryAllocateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkExportMemoryAllocateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkHandleTypes VkExportMemoryAllocateInfoKHR where
        type VkHandleTypesMType VkExportMemoryAllocateInfoKHR =
             VkExternalMemoryHandleTypeFlagsKHR

        {-# NOINLINE vkHandleTypes #-}
        vkHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoKHR, handleTypes})

        {-# INLINE vkHandleTypesByteOffset #-}
        vkHandleTypesByteOffset ~_
          = #{offset VkExportMemoryAllocateInfoKHR, handleTypes}

        {-# INLINE readVkHandleTypes #-}
        readVkHandleTypes p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoKHR, handleTypes}

        {-# INLINE writeVkHandleTypes #-}
        writeVkHandleTypes p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExportMemoryAllocateInfoKHR where
        type FieldType "handleTypes" VkExportMemoryAllocateInfoKHR =
             VkExternalMemoryHandleTypeFlagsKHR
        type FieldOptional "handleTypes" VkExportMemoryAllocateInfoKHR =
             'True -- ' closing tick for hsc2hs

instance CanReadField "handleTypes" VkExportMemoryAllocateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleTypes

        {-# INLINE readField #-}
        readField = readVkHandleTypes

instance CanWriteField "handleTypes" VkExportMemoryAllocateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleTypes

instance Show VkExportMemoryAllocateInfoKHR where
        showsPrec d x
          = showString "VkExportMemoryAllocateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleTypes = " .
                            showsPrec d (vkHandleTypes x) . showChar '}'

pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME

_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME #-}
_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME
  = Ptr "VK_KHR_external_memory\NUL"##

is_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME #-}
is_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME
  = (_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME ==)

type VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME =
     "VK_KHR_external_memory"

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR =
        VkStructureType 1000072000

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR =
        VkStructureType 1000072001

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR =
        VkStructureType 1000072002

pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR :: VkResult

pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR =
        VkResult (-1000072003)
