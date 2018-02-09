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
module Graphics.Vulkan.Ext.VK_KHR_dedicated_allocation
       (-- * Vulkan extension: @VK_KHR_dedicated_allocation@
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
        -- Extension number: @128@
        --
        -- Required extensions: 'VK_KHR_get_memory_requirements2'.
        --

        -- ** Required extensions: 'VK_KHR_get_memory_requirements2'.
        VkMemoryDedicatedRequirementsKHR(..),
        VkMemoryDedicatedAllocateInfoKHR(..),
        VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION,
        pattern VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION,
        VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME,
        pattern VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR,
        pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR)
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

-- | > typedef struct VkMemoryDedicatedRequirementsKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         prefersDedicatedAllocation;
--   >     VkBool32                         requiresDedicatedAllocation;
--   > } VkMemoryDedicatedRequirementsKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryDedicatedRequirementsKHR.html VkMemoryDedicatedRequirementsKHR registry at www.khronos.org>
data VkMemoryDedicatedRequirementsKHR = VkMemoryDedicatedRequirementsKHR## ByteArray##

instance Eq VkMemoryDedicatedRequirementsKHR where
        (VkMemoryDedicatedRequirementsKHR## a) ==
          (VkMemoryDedicatedRequirementsKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkMemoryDedicatedRequirementsKHR where
        (VkMemoryDedicatedRequirementsKHR## a) `compare`
          (VkMemoryDedicatedRequirementsKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkMemoryDedicatedRequirementsKHR where
        sizeOf ~_ = #{size VkMemoryDedicatedRequirementsKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryDedicatedRequirementsKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkMemoryDedicatedRequirementsKHR),
            I## a <- alignment (undefined :: VkMemoryDedicatedRequirementsKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkMemoryDedicatedRequirementsKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkMemoryDedicatedRequirementsKHR## ba)
          | I## n <- sizeOf (undefined :: VkMemoryDedicatedRequirementsKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryDedicatedRequirementsKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkMemoryDedicatedRequirementsKHR),
            I## a <- alignment (undefined :: VkMemoryDedicatedRequirementsKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkMemoryDedicatedRequirementsKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkMemoryDedicatedRequirementsKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkMemoryDedicatedRequirementsKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkMemoryDedicatedRequirementsKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkMemoryDedicatedRequirementsKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkMemoryDedicatedRequirementsKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkMemoryDedicatedRequirementsKHR where
        type VkSTypeMType VkMemoryDedicatedRequirementsKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedRequirementsKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMemoryDedicatedRequirementsKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryDedicatedRequirementsKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryDedicatedRequirementsKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryDedicatedRequirementsKHR where
        type FieldType "sType" VkMemoryDedicatedRequirementsKHR =
             VkStructureType
        type FieldOptional "sType" VkMemoryDedicatedRequirementsKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryDedicatedRequirementsKHR =
             #{offset VkMemoryDedicatedRequirementsKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedRequirementsKHR, sType}

instance CanReadField "sType" VkMemoryDedicatedRequirementsKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkMemoryDedicatedRequirementsKHR where
        type VkPNextMType VkMemoryDedicatedRequirementsKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedRequirementsKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMemoryDedicatedRequirementsKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryDedicatedRequirementsKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryDedicatedRequirementsKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryDedicatedRequirementsKHR where
        type FieldType "pNext" VkMemoryDedicatedRequirementsKHR = Ptr Void
        type FieldOptional "pNext" VkMemoryDedicatedRequirementsKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryDedicatedRequirementsKHR =
             #{offset VkMemoryDedicatedRequirementsKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedRequirementsKHR, pNext}

instance CanReadField "pNext" VkMemoryDedicatedRequirementsKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkPrefersDedicatedAllocation VkMemoryDedicatedRequirementsKHR
         where
        type VkPrefersDedicatedAllocationMType
               VkMemoryDedicatedRequirementsKHR
             = VkBool32

        {-# NOINLINE vkPrefersDedicatedAllocation #-}
        vkPrefersDedicatedAllocation x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedRequirementsKHR, prefersDedicatedAllocation})

        {-# INLINE vkPrefersDedicatedAllocationByteOffset #-}
        vkPrefersDedicatedAllocationByteOffset ~_
          = #{offset VkMemoryDedicatedRequirementsKHR, prefersDedicatedAllocation}

        {-# INLINE readVkPrefersDedicatedAllocation #-}
        readVkPrefersDedicatedAllocation p
          = peekByteOff p #{offset VkMemoryDedicatedRequirementsKHR, prefersDedicatedAllocation}

        {-# INLINE writeVkPrefersDedicatedAllocation #-}
        writeVkPrefersDedicatedAllocation p
          = pokeByteOff p #{offset VkMemoryDedicatedRequirementsKHR, prefersDedicatedAllocation}

instance {-# OVERLAPPING #-}
         HasField "prefersDedicatedAllocation"
           VkMemoryDedicatedRequirementsKHR
         where
        type FieldType "prefersDedicatedAllocation"
               VkMemoryDedicatedRequirementsKHR
             = VkBool32
        type FieldOptional "prefersDedicatedAllocation"
               VkMemoryDedicatedRequirementsKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "prefersDedicatedAllocation"
               VkMemoryDedicatedRequirementsKHR
             =
             #{offset VkMemoryDedicatedRequirementsKHR, prefersDedicatedAllocation}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedRequirementsKHR, prefersDedicatedAllocation}

instance CanReadField "prefersDedicatedAllocation"
           VkMemoryDedicatedRequirementsKHR
         where
        {-# INLINE getField #-}
        getField = vkPrefersDedicatedAllocation

        {-# INLINE readField #-}
        readField = readVkPrefersDedicatedAllocation

instance {-# OVERLAPPING #-}
         HasVkRequiresDedicatedAllocation VkMemoryDedicatedRequirementsKHR
         where
        type VkRequiresDedicatedAllocationMType
               VkMemoryDedicatedRequirementsKHR
             = VkBool32

        {-# NOINLINE vkRequiresDedicatedAllocation #-}
        vkRequiresDedicatedAllocation x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedRequirementsKHR, requiresDedicatedAllocation})

        {-# INLINE vkRequiresDedicatedAllocationByteOffset #-}
        vkRequiresDedicatedAllocationByteOffset ~_
          = #{offset VkMemoryDedicatedRequirementsKHR, requiresDedicatedAllocation}

        {-# INLINE readVkRequiresDedicatedAllocation #-}
        readVkRequiresDedicatedAllocation p
          = peekByteOff p #{offset VkMemoryDedicatedRequirementsKHR, requiresDedicatedAllocation}

        {-# INLINE writeVkRequiresDedicatedAllocation #-}
        writeVkRequiresDedicatedAllocation p
          = pokeByteOff p #{offset VkMemoryDedicatedRequirementsKHR, requiresDedicatedAllocation}

instance {-# OVERLAPPING #-}
         HasField "requiresDedicatedAllocation"
           VkMemoryDedicatedRequirementsKHR
         where
        type FieldType "requiresDedicatedAllocation"
               VkMemoryDedicatedRequirementsKHR
             = VkBool32
        type FieldOptional "requiresDedicatedAllocation"
               VkMemoryDedicatedRequirementsKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "requiresDedicatedAllocation"
               VkMemoryDedicatedRequirementsKHR
             =
             #{offset VkMemoryDedicatedRequirementsKHR, requiresDedicatedAllocation}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedRequirementsKHR, requiresDedicatedAllocation}

instance CanReadField "requiresDedicatedAllocation"
           VkMemoryDedicatedRequirementsKHR
         where
        {-# INLINE getField #-}
        getField = vkRequiresDedicatedAllocation

        {-# INLINE readField #-}
        readField = readVkRequiresDedicatedAllocation

instance Show VkMemoryDedicatedRequirementsKHR where
        showsPrec d x
          = showString "VkMemoryDedicatedRequirementsKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPrefersDedicatedAllocation = " .
                            showsPrec d (vkPrefersDedicatedAllocation x) .
                              showString ", " .
                                showString "vkRequiresDedicatedAllocation = " .
                                  showsPrec d (vkRequiresDedicatedAllocation x) . showChar '}'

-- | > typedef struct VkMemoryDedicatedAllocateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImage          image;
--   >     VkBuffer         buffer;
--   > } VkMemoryDedicatedAllocateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryDedicatedAllocateInfoKHR.html VkMemoryDedicatedAllocateInfoKHR registry at www.khronos.org>
data VkMemoryDedicatedAllocateInfoKHR = VkMemoryDedicatedAllocateInfoKHR## ByteArray##

instance Eq VkMemoryDedicatedAllocateInfoKHR where
        (VkMemoryDedicatedAllocateInfoKHR## a) ==
          (VkMemoryDedicatedAllocateInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkMemoryDedicatedAllocateInfoKHR where
        (VkMemoryDedicatedAllocateInfoKHR## a) `compare`
          (VkMemoryDedicatedAllocateInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkMemoryDedicatedAllocateInfoKHR where
        sizeOf ~_ = #{size VkMemoryDedicatedAllocateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryDedicatedAllocateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkMemoryDedicatedAllocateInfoKHR),
            I## a <- alignment (undefined :: VkMemoryDedicatedAllocateInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkMemoryDedicatedAllocateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkMemoryDedicatedAllocateInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkMemoryDedicatedAllocateInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryDedicatedAllocateInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkMemoryDedicatedAllocateInfoKHR),
            I## a <- alignment (undefined :: VkMemoryDedicatedAllocateInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkMemoryDedicatedAllocateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkMemoryDedicatedAllocateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkMemoryDedicatedAllocateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkMemoryDedicatedAllocateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkMemoryDedicatedAllocateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkMemoryDedicatedAllocateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkMemoryDedicatedAllocateInfoKHR where
        type VkSTypeMType VkMemoryDedicatedAllocateInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedAllocateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMemoryDedicatedAllocateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryDedicatedAllocateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryDedicatedAllocateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryDedicatedAllocateInfoKHR where
        type FieldType "sType" VkMemoryDedicatedAllocateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkMemoryDedicatedAllocateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryDedicatedAllocateInfoKHR =
             #{offset VkMemoryDedicatedAllocateInfoKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedAllocateInfoKHR, sType}

instance CanReadField "sType" VkMemoryDedicatedAllocateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMemoryDedicatedAllocateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkMemoryDedicatedAllocateInfoKHR where
        type VkPNextMType VkMemoryDedicatedAllocateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedAllocateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMemoryDedicatedAllocateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryDedicatedAllocateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryDedicatedAllocateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryDedicatedAllocateInfoKHR where
        type FieldType "pNext" VkMemoryDedicatedAllocateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkMemoryDedicatedAllocateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryDedicatedAllocateInfoKHR =
             #{offset VkMemoryDedicatedAllocateInfoKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedAllocateInfoKHR, pNext}

instance CanReadField "pNext" VkMemoryDedicatedAllocateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMemoryDedicatedAllocateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkImage VkMemoryDedicatedAllocateInfoKHR where
        type VkImageMType VkMemoryDedicatedAllocateInfoKHR = VkImage

        {-# NOINLINE vkImage #-}
        vkImage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedAllocateInfoKHR, image})

        {-# INLINE vkImageByteOffset #-}
        vkImageByteOffset ~_
          = #{offset VkMemoryDedicatedAllocateInfoKHR, image}

        {-# INLINE readVkImage #-}
        readVkImage p
          = peekByteOff p #{offset VkMemoryDedicatedAllocateInfoKHR, image}

        {-# INLINE writeVkImage #-}
        writeVkImage p
          = pokeByteOff p #{offset VkMemoryDedicatedAllocateInfoKHR, image}

instance {-# OVERLAPPING #-}
         HasField "image" VkMemoryDedicatedAllocateInfoKHR where
        type FieldType "image" VkMemoryDedicatedAllocateInfoKHR = VkImage
        type FieldOptional "image" VkMemoryDedicatedAllocateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "image" VkMemoryDedicatedAllocateInfoKHR =
             #{offset VkMemoryDedicatedAllocateInfoKHR, image}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedAllocateInfoKHR, image}

instance CanReadField "image" VkMemoryDedicatedAllocateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkImage

        {-# INLINE readField #-}
        readField = readVkImage

instance CanWriteField "image" VkMemoryDedicatedAllocateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkImage

instance {-# OVERLAPPING #-}
         HasVkBuffer VkMemoryDedicatedAllocateInfoKHR where
        type VkBufferMType VkMemoryDedicatedAllocateInfoKHR = VkBuffer

        {-# NOINLINE vkBuffer #-}
        vkBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedAllocateInfoKHR, buffer})

        {-# INLINE vkBufferByteOffset #-}
        vkBufferByteOffset ~_
          = #{offset VkMemoryDedicatedAllocateInfoKHR, buffer}

        {-# INLINE readVkBuffer #-}
        readVkBuffer p
          = peekByteOff p #{offset VkMemoryDedicatedAllocateInfoKHR, buffer}

        {-# INLINE writeVkBuffer #-}
        writeVkBuffer p
          = pokeByteOff p #{offset VkMemoryDedicatedAllocateInfoKHR, buffer}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkMemoryDedicatedAllocateInfoKHR where
        type FieldType "buffer" VkMemoryDedicatedAllocateInfoKHR = VkBuffer
        type FieldOptional "buffer" VkMemoryDedicatedAllocateInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkMemoryDedicatedAllocateInfoKHR =
             #{offset VkMemoryDedicatedAllocateInfoKHR, buffer}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedAllocateInfoKHR, buffer}

instance CanReadField "buffer" VkMemoryDedicatedAllocateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkBuffer

        {-# INLINE readField #-}
        readField = readVkBuffer

instance CanWriteField "buffer" VkMemoryDedicatedAllocateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkBuffer

instance Show VkMemoryDedicatedAllocateInfoKHR where
        showsPrec d x
          = showString "VkMemoryDedicatedAllocateInfoKHR {" .
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

pattern VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION = 3

type VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION = 3

pattern VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME :: CString

pattern VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME <-
        (is_VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME -> True)
  where VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME
          = _VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME

_VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME #-}
_VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME
  = Ptr "VK_KHR_dedicated_allocation\NUL"##

is_VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME #-}
is_VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME
  = (_VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME ==)

type VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME =
     "VK_KHR_dedicated_allocation"

pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR =
        VkStructureType 1000127000

pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR =
        VkStructureType 1000127001
