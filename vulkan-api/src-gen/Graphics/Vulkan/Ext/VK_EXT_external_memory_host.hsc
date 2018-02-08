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
module Graphics.Vulkan.Ext.VK_EXT_external_memory_host
       (-- * Vulkan extension: @VK_EXT_external_memory_host@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Rakos @aqnuep@
        --
        -- author: @EXT@
        --
        -- type: @device@
        --
        -- Extension number: @179@
        --
        -- Required extensions: 'VK_KHR_external_memory'.
        --

        -- ** Required extensions: 'VK_KHR_external_memory'.
        VkImportMemoryHostPointerInfoEXT(..),
        VkMemoryHostPointerPropertiesEXT(..),
        VkPhysicalDeviceExternalMemoryHostPropertiesEXT(..),
        vkGetMemoryHostPointerPropertiesEXT,
        VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION,
        pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION,
        VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME,
        pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT,
        pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT)
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

-- | > typedef struct VkImportMemoryHostPointerInfoEXT {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkExternalMemoryHandleTypeFlagBitsKHR handleType;
--   >     void* pHostPointer;
--   > } VkImportMemoryHostPointerInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImportMemoryHostPointerInfoEXT.html VkImportMemoryHostPointerInfoEXT registry at www.khronos.org>
data VkImportMemoryHostPointerInfoEXT = VkImportMemoryHostPointerInfoEXT## ByteArray##

instance Eq VkImportMemoryHostPointerInfoEXT where
        (VkImportMemoryHostPointerInfoEXT## a) ==
          (VkImportMemoryHostPointerInfoEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkImportMemoryHostPointerInfoEXT where
        (VkImportMemoryHostPointerInfoEXT## a) `compare`
          (VkImportMemoryHostPointerInfoEXT## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkImportMemoryHostPointerInfoEXT where
        sizeOf ~_ = #{size VkImportMemoryHostPointerInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportMemoryHostPointerInfoEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkImportMemoryHostPointerInfoEXT),
            I## a <- alignment (undefined :: VkImportMemoryHostPointerInfoEXT) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkImportMemoryHostPointerInfoEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkImportMemoryHostPointerInfoEXT## ba)
          | I## n <- sizeOf (undefined :: VkImportMemoryHostPointerInfoEXT) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkImportMemoryHostPointerInfoEXT where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkImportMemoryHostPointerInfoEXT),
            I## a <- alignment (undefined :: VkImportMemoryHostPointerInfoEXT) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkImportMemoryHostPointerInfoEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkImportMemoryHostPointerInfoEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkImportMemoryHostPointerInfoEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkImportMemoryHostPointerInfoEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkImportMemoryHostPointerInfoEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkImportMemoryHostPointerInfoEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkImportMemoryHostPointerInfoEXT where
        type VkSTypeMType VkImportMemoryHostPointerInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryHostPointerInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImportMemoryHostPointerInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImportMemoryHostPointerInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImportMemoryHostPointerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportMemoryHostPointerInfoEXT where
        type FieldType "sType" VkImportMemoryHostPointerInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkImportMemoryHostPointerInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImportMemoryHostPointerInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkImportMemoryHostPointerInfoEXT where
        type VkPNextMType VkImportMemoryHostPointerInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryHostPointerInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImportMemoryHostPointerInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImportMemoryHostPointerInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImportMemoryHostPointerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportMemoryHostPointerInfoEXT where
        type FieldType "pNext" VkImportMemoryHostPointerInfoEXT = Ptr Void
        type FieldOptional "pNext" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkImportMemoryHostPointerInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImportMemoryHostPointerInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkHandleType VkImportMemoryHostPointerInfoEXT where
        type VkHandleTypeMType VkImportMemoryHostPointerInfoEXT =
             VkExternalMemoryHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryHostPointerInfoEXT, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkImportMemoryHostPointerInfoEXT, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkImportMemoryHostPointerInfoEXT, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkImportMemoryHostPointerInfoEXT, handleType}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportMemoryHostPointerInfoEXT where
        type FieldType "handleType" VkImportMemoryHostPointerInfoEXT =
             VkExternalMemoryHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs

instance CanReadField "handleType" VkImportMemoryHostPointerInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType"
           VkImportMemoryHostPointerInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

instance {-# OVERLAPPING #-}
         HasVkPHostPointer VkImportMemoryHostPointerInfoEXT where
        type VkPHostPointerMType VkImportMemoryHostPointerInfoEXT =
             Ptr Void

        {-# NOINLINE vkPHostPointer #-}
        vkPHostPointer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer})

        {-# INLINE vkPHostPointerByteOffset #-}
        vkPHostPointerByteOffset ~_
          = #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}

        {-# INLINE readVkPHostPointer #-}
        readVkPHostPointer p
          = peekByteOff p #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}

        {-# INLINE writeVkPHostPointer #-}
        writeVkPHostPointer p
          = pokeByteOff p #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}

instance {-# OVERLAPPING #-}
         HasField "pHostPointer" VkImportMemoryHostPointerInfoEXT where
        type FieldType "pHostPointer" VkImportMemoryHostPointerInfoEXT =
             Ptr Void
        type FieldOptional "pHostPointer" VkImportMemoryHostPointerInfoEXT
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "pHostPointer"
           VkImportMemoryHostPointerInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPHostPointer

        {-# INLINE readField #-}
        readField = readVkPHostPointer

instance CanWriteField "pHostPointer"
           VkImportMemoryHostPointerInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPHostPointer

instance Show VkImportMemoryHostPointerInfoEXT where
        showsPrec d x
          = showString "VkImportMemoryHostPointerInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleType = " .
                            showsPrec d (vkHandleType x) .
                              showString ", " .
                                showString "vkPHostPointer = " .
                                  showsPrec d (vkPHostPointer x) . showChar '}'

-- | > typedef struct VkMemoryHostPointerPropertiesEXT {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     uint32_t memoryTypeBits;
--   > } VkMemoryHostPointerPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryHostPointerPropertiesEXT.html VkMemoryHostPointerPropertiesEXT registry at www.khronos.org>
data VkMemoryHostPointerPropertiesEXT = VkMemoryHostPointerPropertiesEXT## ByteArray##

instance Eq VkMemoryHostPointerPropertiesEXT where
        (VkMemoryHostPointerPropertiesEXT## a) ==
          (VkMemoryHostPointerPropertiesEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkMemoryHostPointerPropertiesEXT where
        (VkMemoryHostPointerPropertiesEXT## a) `compare`
          (VkMemoryHostPointerPropertiesEXT## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkMemoryHostPointerPropertiesEXT where
        sizeOf ~_ = #{size VkMemoryHostPointerPropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryHostPointerPropertiesEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkMemoryHostPointerPropertiesEXT),
            I## a <- alignment (undefined :: VkMemoryHostPointerPropertiesEXT) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkMemoryHostPointerPropertiesEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkMemoryHostPointerPropertiesEXT## ba)
          | I## n <- sizeOf (undefined :: VkMemoryHostPointerPropertiesEXT) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryHostPointerPropertiesEXT where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkMemoryHostPointerPropertiesEXT),
            I## a <- alignment (undefined :: VkMemoryHostPointerPropertiesEXT) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkMemoryHostPointerPropertiesEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkMemoryHostPointerPropertiesEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkMemoryHostPointerPropertiesEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkMemoryHostPointerPropertiesEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkMemoryHostPointerPropertiesEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkMemoryHostPointerPropertiesEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkMemoryHostPointerPropertiesEXT where
        type VkSTypeMType VkMemoryHostPointerPropertiesEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHostPointerPropertiesEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMemoryHostPointerPropertiesEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryHostPointerPropertiesEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryHostPointerPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryHostPointerPropertiesEXT where
        type FieldType "sType" VkMemoryHostPointerPropertiesEXT =
             VkStructureType
        type FieldOptional "sType" VkMemoryHostPointerPropertiesEXT =
             'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkMemoryHostPointerPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMemoryHostPointerPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkMemoryHostPointerPropertiesEXT where
        type VkPNextMType VkMemoryHostPointerPropertiesEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHostPointerPropertiesEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMemoryHostPointerPropertiesEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryHostPointerPropertiesEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryHostPointerPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryHostPointerPropertiesEXT where
        type FieldType "pNext" VkMemoryHostPointerPropertiesEXT = Ptr Void
        type FieldOptional "pNext" VkMemoryHostPointerPropertiesEXT =
             'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkMemoryHostPointerPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMemoryHostPointerPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkMemoryTypeBits VkMemoryHostPointerPropertiesEXT where
        type VkMemoryTypeBitsMType VkMemoryHostPointerPropertiesEXT =
             Word32

        {-# NOINLINE vkMemoryTypeBits #-}
        vkMemoryTypeBits x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits})

        {-# INLINE vkMemoryTypeBitsByteOffset #-}
        vkMemoryTypeBitsByteOffset ~_
          = #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits}

        {-# INLINE readVkMemoryTypeBits #-}
        readVkMemoryTypeBits p
          = peekByteOff p #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits}

        {-# INLINE writeVkMemoryTypeBits #-}
        writeVkMemoryTypeBits p
          = pokeByteOff p #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits}

instance {-# OVERLAPPING #-}
         HasField "memoryTypeBits" VkMemoryHostPointerPropertiesEXT where
        type FieldType "memoryTypeBits" VkMemoryHostPointerPropertiesEXT =
             Word32
        type FieldOptional "memoryTypeBits"
               VkMemoryHostPointerPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "memoryTypeBits"
           VkMemoryHostPointerPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkMemoryTypeBits

        {-# INLINE readField #-}
        readField = readVkMemoryTypeBits

instance CanWriteField "memoryTypeBits"
           VkMemoryHostPointerPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkMemoryTypeBits

instance Show VkMemoryHostPointerPropertiesEXT where
        showsPrec d x
          = showString "VkMemoryHostPointerPropertiesEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMemoryTypeBits = " .
                            showsPrec d (vkMemoryTypeBits x) . showChar '}'

-- | > typedef struct VkPhysicalDeviceExternalMemoryHostPropertiesEXT {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkDeviceSize minImportedHostPointerAlignment;
--   > } VkPhysicalDeviceExternalMemoryHostPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceExternalMemoryHostPropertiesEXT.html VkPhysicalDeviceExternalMemoryHostPropertiesEXT registry at www.khronos.org>
data VkPhysicalDeviceExternalMemoryHostPropertiesEXT = VkPhysicalDeviceExternalMemoryHostPropertiesEXT## ByteArray##

instance Eq VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
        (VkPhysicalDeviceExternalMemoryHostPropertiesEXT## a) ==
          (VkPhysicalDeviceExternalMemoryHostPropertiesEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
        (VkPhysicalDeviceExternalMemoryHostPropertiesEXT## a) `compare`
          (VkPhysicalDeviceExternalMemoryHostPropertiesEXT## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceExternalMemoryHostPropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalMemoryHostPropertiesEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceExternalMemoryHostPropertiesEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr)
          (VkPhysicalDeviceExternalMemoryHostPropertiesEXT## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceExternalMemoryHostPropertiesEXT)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceExternalMemoryHostPropertiesEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceExternalMemoryHostPropertiesEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPhysicalDeviceExternalMemoryHostPropertiesEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceExternalMemoryHostPropertiesEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr
          (VkPhysicalDeviceExternalMemoryHostPropertiesEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceExternalMemoryHostPropertiesEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
        type VkSTypeMType VkPhysicalDeviceExternalMemoryHostPropertiesEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        type FieldType "sType"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
        type VkPNextMType VkPhysicalDeviceExternalMemoryHostPropertiesEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        type FieldType "pNext"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkMinImportedHostPointerAlignment
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        type VkMinImportedHostPointerAlignmentMType
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = VkDeviceSize

        {-# NOINLINE vkMinImportedHostPointerAlignment #-}
        vkMinImportedHostPointerAlignment x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, minImportedHostPointerAlignment})

        {-# INLINE vkMinImportedHostPointerAlignmentByteOffset #-}
        vkMinImportedHostPointerAlignmentByteOffset ~_
          = #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, minImportedHostPointerAlignment}

        {-# INLINE readVkMinImportedHostPointerAlignment #-}
        readVkMinImportedHostPointerAlignment p
          = peekByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, minImportedHostPointerAlignment}

        {-# INLINE writeVkMinImportedHostPointerAlignment #-}
        writeVkMinImportedHostPointerAlignment p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, minImportedHostPointerAlignment}

instance {-# OVERLAPPING #-}
         HasField "minImportedHostPointerAlignment"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        type FieldType "minImportedHostPointerAlignment"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = VkDeviceSize
        type FieldOptional "minImportedHostPointerAlignment"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "minImportedHostPointerAlignment"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkMinImportedHostPointerAlignment

        {-# INLINE readField #-}
        readField = readVkMinImportedHostPointerAlignment

instance CanWriteField "minImportedHostPointerAlignment"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinImportedHostPointerAlignment

instance Show VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
        showsPrec d x
          = showString "VkPhysicalDeviceExternalMemoryHostPropertiesEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMinImportedHostPointerAlignment = " .
                            showsPrec d (vkMinImportedHostPointerAlignment x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkGetMemoryHostPointerPropertiesEXT
--   >     ( VkDevice device
--   >     , VkExternalMemoryHandleTypeFlagBitsKHR handleType
--   >     , const void* pHostPointer
--   >     , VkMemoryHostPointerPropertiesEXT* pMemoryHostPointerProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetMemoryHostPointerPropertiesEXT.html vkGetMemoryHostPointerPropertiesEXT registry at www.khronos.org>
foreign import ccall unsafe "vkGetMemoryHostPointerPropertiesEXT"
               vkGetMemoryHostPointerPropertiesEXT ::
               VkDevice -- ^ device
                        ->
                 VkExternalMemoryHandleTypeFlagBitsKHR -- ^ handleType
                                                       ->
                   Ptr Void -- ^ pHostPointer
                            -> Ptr VkMemoryHostPointerPropertiesEXT -- ^ pMemoryHostPointerProperties
                                                                    -> IO VkResult

pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION = 1

type VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION = 1

pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME :: CString

pattern VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME <-
        (is_VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME -> True)
  where VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME
          = _VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME

_VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME :: CString

{-# INLINE _VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME #-}
_VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME
  = Ptr "VK_EXT_external_memory_host\NUL"##

is_VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME #-}
is_VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME
  = (_VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME ==)

type VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME =
     "VK_EXT_external_memory_host"

pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT =
        VkStructureType 1000178000

pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT =
        VkStructureType 1000178001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT
        = VkStructureType 1000178002

-- | bitpos = @7@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT ::
        VkExternalMemoryHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT =
        VkExternalMemoryHandleTypeFlagBitsKHR 128

-- | bitpos = @8@
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT
        :: VkExternalMemoryHandleTypeFlagBitsKHR

pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT
        = VkExternalMemoryHandleTypeFlagBitsKHR 256
