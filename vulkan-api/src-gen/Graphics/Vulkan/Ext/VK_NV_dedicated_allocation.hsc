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
module Graphics.Vulkan.Ext.VK_NV_dedicated_allocation
       (-- * Vulkan extension: @VK_NV_dedicated_allocation@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jbolz@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @27@
        VkDedicatedAllocationImageCreateInfoNV(..),
        VkDedicatedAllocationBufferCreateInfoNV(..),
        VkDedicatedAllocationMemoryAllocateInfoNV(..),
        VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION,
        pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION,
        VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME,
        pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkBool32, VkBuffer, VkImage,
                                                   VkStructureType (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkDedicatedAllocationImageCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         dedicatedAllocation;
--   > } VkDedicatedAllocationImageCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDedicatedAllocationImageCreateInfoNV.html VkDedicatedAllocationImageCreateInfoNV registry at www.khronos.org>
data VkDedicatedAllocationImageCreateInfoNV = VkDedicatedAllocationImageCreateInfoNV## ByteArray##

instance Eq VkDedicatedAllocationImageCreateInfoNV where
        (VkDedicatedAllocationImageCreateInfoNV## a) ==
          (VkDedicatedAllocationImageCreateInfoNV## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDedicatedAllocationImageCreateInfoNV where
        (VkDedicatedAllocationImageCreateInfoNV## a) `compare`
          (VkDedicatedAllocationImageCreateInfoNV## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDedicatedAllocationImageCreateInfoNV where
        sizeOf ~_
          = #{size VkDedicatedAllocationImageCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDedicatedAllocationImageCreateInfoNV}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkDedicatedAllocationImageCreateInfoNV),
            I## a <- alignment
                      (undefined :: VkDedicatedAllocationImageCreateInfoNV)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDedicatedAllocationImageCreateInfoNV##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDedicatedAllocationImageCreateInfoNV## ba)
          | I## n <- sizeOf
                      (undefined :: VkDedicatedAllocationImageCreateInfoNV)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDedicatedAllocationImageCreateInfoNV where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkDedicatedAllocationImageCreateInfoNV),
            I## a <- alignment
                      (undefined :: VkDedicatedAllocationImageCreateInfoNV)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDedicatedAllocationImageCreateInfoNV##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDedicatedAllocationImageCreateInfoNV## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkDedicatedAllocationImageCreateInfoNV##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDedicatedAllocationImageCreateInfoNV## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDedicatedAllocationImageCreateInfoNV## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDedicatedAllocationImageCreateInfoNV## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkDedicatedAllocationImageCreateInfoNV where
        type VkSTypeMType VkDedicatedAllocationImageCreateInfoNV =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationImageCreateInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDedicatedAllocationImageCreateInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDedicatedAllocationImageCreateInfoNV where
        type FieldType "sType" VkDedicatedAllocationImageCreateInfoNV =
             VkStructureType
        type FieldOptional "sType" VkDedicatedAllocationImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDedicatedAllocationImageCreateInfoNV =
             #{offset VkDedicatedAllocationImageCreateInfoNV, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationImageCreateInfoNV, sType}

instance CanReadField "sType"
           VkDedicatedAllocationImageCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkDedicatedAllocationImageCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDedicatedAllocationImageCreateInfoNV where
        type VkPNextMType VkDedicatedAllocationImageCreateInfoNV = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationImageCreateInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDedicatedAllocationImageCreateInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDedicatedAllocationImageCreateInfoNV where
        type FieldType "pNext" VkDedicatedAllocationImageCreateInfoNV =
             Ptr Void
        type FieldOptional "pNext" VkDedicatedAllocationImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDedicatedAllocationImageCreateInfoNV =
             #{offset VkDedicatedAllocationImageCreateInfoNV, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationImageCreateInfoNV, pNext}

instance CanReadField "pNext"
           VkDedicatedAllocationImageCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkDedicatedAllocationImageCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDedicatedAllocation VkDedicatedAllocationImageCreateInfoNV
         where
        type VkDedicatedAllocationMType
               VkDedicatedAllocationImageCreateInfoNV
             = VkBool32

        {-# NOINLINE vkDedicatedAllocation #-}
        vkDedicatedAllocation x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation})

        {-# INLINE vkDedicatedAllocationByteOffset #-}
        vkDedicatedAllocationByteOffset ~_
          = #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation}

        {-# INLINE readVkDedicatedAllocation #-}
        readVkDedicatedAllocation p
          = peekByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation}

        {-# INLINE writeVkDedicatedAllocation #-}
        writeVkDedicatedAllocation p
          = pokeByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation}

instance {-# OVERLAPPING #-}
         HasField "dedicatedAllocation"
           VkDedicatedAllocationImageCreateInfoNV
         where
        type FieldType "dedicatedAllocation"
               VkDedicatedAllocationImageCreateInfoNV
             = VkBool32
        type FieldOptional "dedicatedAllocation"
               VkDedicatedAllocationImageCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dedicatedAllocation"
               VkDedicatedAllocationImageCreateInfoNV
             =
             #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation}

instance CanReadField "dedicatedAllocation"
           VkDedicatedAllocationImageCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkDedicatedAllocation

        {-# INLINE readField #-}
        readField = readVkDedicatedAllocation

instance CanWriteField "dedicatedAllocation"
           VkDedicatedAllocationImageCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkDedicatedAllocation

instance Show VkDedicatedAllocationImageCreateInfoNV where
        showsPrec d x
          = showString "VkDedicatedAllocationImageCreateInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDedicatedAllocation = " .
                            showsPrec d (vkDedicatedAllocation x) . showChar '}'

-- | > typedef struct VkDedicatedAllocationBufferCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         dedicatedAllocation;
--   > } VkDedicatedAllocationBufferCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDedicatedAllocationBufferCreateInfoNV.html VkDedicatedAllocationBufferCreateInfoNV registry at www.khronos.org>
data VkDedicatedAllocationBufferCreateInfoNV = VkDedicatedAllocationBufferCreateInfoNV## ByteArray##

instance Eq VkDedicatedAllocationBufferCreateInfoNV where
        (VkDedicatedAllocationBufferCreateInfoNV## a) ==
          (VkDedicatedAllocationBufferCreateInfoNV## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDedicatedAllocationBufferCreateInfoNV where
        (VkDedicatedAllocationBufferCreateInfoNV## a) `compare`
          (VkDedicatedAllocationBufferCreateInfoNV## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDedicatedAllocationBufferCreateInfoNV where
        sizeOf ~_
          = #{size VkDedicatedAllocationBufferCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDedicatedAllocationBufferCreateInfoNV}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkDedicatedAllocationBufferCreateInfoNV),
            I## a <- alignment
                      (undefined :: VkDedicatedAllocationBufferCreateInfoNV)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDedicatedAllocationBufferCreateInfoNV##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDedicatedAllocationBufferCreateInfoNV## ba)
          | I## n <- sizeOf
                      (undefined :: VkDedicatedAllocationBufferCreateInfoNV)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDedicatedAllocationBufferCreateInfoNV
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkDedicatedAllocationBufferCreateInfoNV),
            I## a <- alignment
                      (undefined :: VkDedicatedAllocationBufferCreateInfoNV)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDedicatedAllocationBufferCreateInfoNV##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDedicatedAllocationBufferCreateInfoNV## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkDedicatedAllocationBufferCreateInfoNV##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDedicatedAllocationBufferCreateInfoNV## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDedicatedAllocationBufferCreateInfoNV## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDedicatedAllocationBufferCreateInfoNV## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkDedicatedAllocationBufferCreateInfoNV where
        type VkSTypeMType VkDedicatedAllocationBufferCreateInfoNV =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationBufferCreateInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDedicatedAllocationBufferCreateInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDedicatedAllocationBufferCreateInfoNV where
        type FieldType "sType" VkDedicatedAllocationBufferCreateInfoNV =
             VkStructureType
        type FieldOptional "sType" VkDedicatedAllocationBufferCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDedicatedAllocationBufferCreateInfoNV =
             #{offset VkDedicatedAllocationBufferCreateInfoNV, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationBufferCreateInfoNV, sType}

instance CanReadField "sType"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDedicatedAllocationBufferCreateInfoNV where
        type VkPNextMType VkDedicatedAllocationBufferCreateInfoNV =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDedicatedAllocationBufferCreateInfoNV where
        type FieldType "pNext" VkDedicatedAllocationBufferCreateInfoNV =
             Ptr Void
        type FieldOptional "pNext" VkDedicatedAllocationBufferCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDedicatedAllocationBufferCreateInfoNV =
             #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext}

instance CanReadField "pNext"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDedicatedAllocation VkDedicatedAllocationBufferCreateInfoNV
         where
        type VkDedicatedAllocationMType
               VkDedicatedAllocationBufferCreateInfoNV
             = VkBool32

        {-# NOINLINE vkDedicatedAllocation #-}
        vkDedicatedAllocation x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation})

        {-# INLINE vkDedicatedAllocationByteOffset #-}
        vkDedicatedAllocationByteOffset ~_
          = #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation}

        {-# INLINE readVkDedicatedAllocation #-}
        readVkDedicatedAllocation p
          = peekByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation}

        {-# INLINE writeVkDedicatedAllocation #-}
        writeVkDedicatedAllocation p
          = pokeByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation}

instance {-# OVERLAPPING #-}
         HasField "dedicatedAllocation"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        type FieldType "dedicatedAllocation"
               VkDedicatedAllocationBufferCreateInfoNV
             = VkBool32
        type FieldOptional "dedicatedAllocation"
               VkDedicatedAllocationBufferCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dedicatedAllocation"
               VkDedicatedAllocationBufferCreateInfoNV
             =
             #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation}

instance CanReadField "dedicatedAllocation"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkDedicatedAllocation

        {-# INLINE readField #-}
        readField = readVkDedicatedAllocation

instance CanWriteField "dedicatedAllocation"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkDedicatedAllocation

instance Show VkDedicatedAllocationBufferCreateInfoNV where
        showsPrec d x
          = showString "VkDedicatedAllocationBufferCreateInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDedicatedAllocation = " .
                            showsPrec d (vkDedicatedAllocation x) . showChar '}'

-- | > typedef struct VkDedicatedAllocationMemoryAllocateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImage          image;
--   >     VkBuffer         buffer;
--   > } VkDedicatedAllocationMemoryAllocateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDedicatedAllocationMemoryAllocateInfoNV.html VkDedicatedAllocationMemoryAllocateInfoNV registry at www.khronos.org>
data VkDedicatedAllocationMemoryAllocateInfoNV = VkDedicatedAllocationMemoryAllocateInfoNV## ByteArray##

instance Eq VkDedicatedAllocationMemoryAllocateInfoNV where
        (VkDedicatedAllocationMemoryAllocateInfoNV## a) ==
          (VkDedicatedAllocationMemoryAllocateInfoNV## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDedicatedAllocationMemoryAllocateInfoNV where
        (VkDedicatedAllocationMemoryAllocateInfoNV## a) `compare`
          (VkDedicatedAllocationMemoryAllocateInfoNV## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDedicatedAllocationMemoryAllocateInfoNV where
        sizeOf ~_
          = #{size VkDedicatedAllocationMemoryAllocateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDedicatedAllocationMemoryAllocateInfoNV}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkDedicatedAllocationMemoryAllocateInfoNV),
            I## a <- alignment
                      (undefined :: VkDedicatedAllocationMemoryAllocateInfoNV)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDedicatedAllocationMemoryAllocateInfoNV##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDedicatedAllocationMemoryAllocateInfoNV## ba)
          | I## n <- sizeOf
                      (undefined :: VkDedicatedAllocationMemoryAllocateInfoNV)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkDedicatedAllocationMemoryAllocateInfoNV),
            I## a <- alignment
                      (undefined :: VkDedicatedAllocationMemoryAllocateInfoNV)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDedicatedAllocationMemoryAllocateInfoNV##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDedicatedAllocationMemoryAllocateInfoNV## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkDedicatedAllocationMemoryAllocateInfoNV##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDedicatedAllocationMemoryAllocateInfoNV## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDedicatedAllocationMemoryAllocateInfoNV## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDedicatedAllocationMemoryAllocateInfoNV## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkDedicatedAllocationMemoryAllocateInfoNV where
        type VkSTypeMType VkDedicatedAllocationMemoryAllocateInfoNV =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDedicatedAllocationMemoryAllocateInfoNV where
        type FieldType "sType" VkDedicatedAllocationMemoryAllocateInfoNV =
             VkStructureType
        type FieldOptional "sType"
               VkDedicatedAllocationMemoryAllocateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDedicatedAllocationMemoryAllocateInfoNV
             =
             #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType}

instance CanReadField "sType"
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDedicatedAllocationMemoryAllocateInfoNV where
        type VkPNextMType VkDedicatedAllocationMemoryAllocateInfoNV =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDedicatedAllocationMemoryAllocateInfoNV where
        type FieldType "pNext" VkDedicatedAllocationMemoryAllocateInfoNV =
             Ptr Void
        type FieldOptional "pNext"
               VkDedicatedAllocationMemoryAllocateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDedicatedAllocationMemoryAllocateInfoNV
             =
             #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext}

instance CanReadField "pNext"
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkImage VkDedicatedAllocationMemoryAllocateInfoNV where
        type VkImageMType VkDedicatedAllocationMemoryAllocateInfoNV =
             VkImage

        {-# NOINLINE vkImage #-}
        vkImage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image})

        {-# INLINE vkImageByteOffset #-}
        vkImageByteOffset ~_
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image}

        {-# INLINE readVkImage #-}
        readVkImage p
          = peekByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image}

        {-# INLINE writeVkImage #-}
        writeVkImage p
          = pokeByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image}

instance {-# OVERLAPPING #-}
         HasField "image" VkDedicatedAllocationMemoryAllocateInfoNV where
        type FieldType "image" VkDedicatedAllocationMemoryAllocateInfoNV =
             VkImage
        type FieldOptional "image"
               VkDedicatedAllocationMemoryAllocateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "image" VkDedicatedAllocationMemoryAllocateInfoNV
             =
             #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image}

instance CanReadField "image"
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkImage

        {-# INLINE readField #-}
        readField = readVkImage

instance CanWriteField "image"
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkImage

instance {-# OVERLAPPING #-}
         HasVkBuffer VkDedicatedAllocationMemoryAllocateInfoNV where
        type VkBufferMType VkDedicatedAllocationMemoryAllocateInfoNV =
             VkBuffer

        {-# NOINLINE vkBuffer #-}
        vkBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer})

        {-# INLINE vkBufferByteOffset #-}
        vkBufferByteOffset ~_
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer}

        {-# INLINE readVkBuffer #-}
        readVkBuffer p
          = peekByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer}

        {-# INLINE writeVkBuffer #-}
        writeVkBuffer p
          = pokeByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkDedicatedAllocationMemoryAllocateInfoNV where
        type FieldType "buffer" VkDedicatedAllocationMemoryAllocateInfoNV =
             VkBuffer
        type FieldOptional "buffer"
               VkDedicatedAllocationMemoryAllocateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkDedicatedAllocationMemoryAllocateInfoNV
             =
             #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer}

instance CanReadField "buffer"
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkBuffer

        {-# INLINE readField #-}
        readField = readVkBuffer

instance CanWriteField "buffer"
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkBuffer

instance Show VkDedicatedAllocationMemoryAllocateInfoNV where
        showsPrec d x
          = showString "VkDedicatedAllocationMemoryAllocateInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkImage = " .
                            showsPrec d (vkImage x) .
                              showString ", " .
                                showString "vkBuffer = " . showsPrec d (vkBuffer x) . showChar '}'

pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION = 1

type VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION = 1

pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME :: CString

pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME <-
        (is_VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME -> True)
  where VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME
          = _VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME

_VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME :: CString

{-# INLINE _VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME #-}
_VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME
  = Ptr "VK_NV_dedicated_allocation\NUL"##

is_VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME #-}
is_VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME
  = (_VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME ==)

type VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME =
     "VK_NV_dedicated_allocation"

pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
        = VkStructureType 1000026000

pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
        = VkStructureType 1000026001

pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
        = VkStructureType 1000026002
