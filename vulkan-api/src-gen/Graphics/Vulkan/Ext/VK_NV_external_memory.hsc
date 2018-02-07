#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE UnboxedTuples   #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NV_external_memory
       (-- * Vulkan extension: @VK_NV_external_memory@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @57@
        --
        -- Required extensions: 'VK_NV_external_memory_capabilities'.
        --

        -- ** Required extensions: 'VK_NV_external_memory_capabilities'.
        VkExternalMemoryImageCreateInfoNV(..),
        VkExportMemoryAllocateInfoNV(..),
        VK_NV_EXTERNAL_MEMORY_SPEC_VERSION,
        pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION,
        VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME,
        pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkExternalMemoryHandleTypeFlagsNV,
                                                   VkStructureType,
                                                   VkStructureType (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkExternalMemoryImageCreateInfoNV = VkExternalMemoryImageCreateInfoNV## ByteArray##

instance Eq VkExternalMemoryImageCreateInfoNV where
        (VkExternalMemoryImageCreateInfoNV## a) ==
          (VkExternalMemoryImageCreateInfoNV## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryImageCreateInfoNV where
        (VkExternalMemoryImageCreateInfoNV## a) `compare`
          (VkExternalMemoryImageCreateInfoNV## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryImageCreateInfoNV where
        sizeOf ~_ = #{size VkExternalMemoryImageCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryImageCreateInfoNV}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkExternalMemoryImageCreateInfoNV),
            I## a <- alignment (undefined :: VkExternalMemoryImageCreateInfoNV)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExternalMemoryImageCreateInfoNV##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExternalMemoryImageCreateInfoNV## ba)
          | I## n <- sizeOf (undefined :: VkExternalMemoryImageCreateInfoNV) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExternalMemoryImageCreateInfoNV where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkExternalMemoryImageCreateInfoNV),
            I## a <- alignment (undefined :: VkExternalMemoryImageCreateInfoNV)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExternalMemoryImageCreateInfoNV##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExternalMemoryImageCreateInfoNV## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkExternalMemoryImageCreateInfoNV##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExternalMemoryImageCreateInfoNV## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExternalMemoryImageCreateInfoNV## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExternalMemoryImageCreateInfoNV## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkExternalMemoryImageCreateInfoNV where
        type VkSTypeMType VkExternalMemoryImageCreateInfoNV =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExternalMemoryImageCreateInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkExternalMemoryImageCreateInfoNV where
        type VkPNextMType VkExternalMemoryImageCreateInfoNV = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExternalMemoryImageCreateInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasVkHandleTypes VkExternalMemoryImageCreateInfoNV where
        type VkHandleTypesMType VkExternalMemoryImageCreateInfoNV =
             VkExternalMemoryHandleTypeFlagsNV

        {-# NOINLINE vkHandleTypes #-}
        vkHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoNV, handleTypes})

        {-# INLINE vkHandleTypesByteOffset #-}
        vkHandleTypesByteOffset ~_
          = #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}

        {-# INLINE readVkHandleTypes #-}
        readVkHandleTypes p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}

        {-# INLINE writeVkHandleTypes #-}
        writeVkHandleTypes p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}

instance Show VkExternalMemoryImageCreateInfoNV where
        showsPrec d x
          = showString "VkExternalMemoryImageCreateInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleTypes = " .
                            showsPrec d (vkHandleTypes x) . showChar '}'

data VkExportMemoryAllocateInfoNV = VkExportMemoryAllocateInfoNV## ByteArray##

instance Eq VkExportMemoryAllocateInfoNV where
        (VkExportMemoryAllocateInfoNV## a) ==
          (VkExportMemoryAllocateInfoNV## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExportMemoryAllocateInfoNV where
        (VkExportMemoryAllocateInfoNV## a) `compare`
          (VkExportMemoryAllocateInfoNV## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExportMemoryAllocateInfoNV where
        sizeOf ~_ = #{size VkExportMemoryAllocateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportMemoryAllocateInfoNV}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkExportMemoryAllocateInfoNV),
            I## a <- alignment (undefined :: VkExportMemoryAllocateInfoNV) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExportMemoryAllocateInfoNV##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExportMemoryAllocateInfoNV## ba)
          | I## n <- sizeOf (undefined :: VkExportMemoryAllocateInfoNV) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExportMemoryAllocateInfoNV where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkExportMemoryAllocateInfoNV),
            I## a <- alignment (undefined :: VkExportMemoryAllocateInfoNV) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExportMemoryAllocateInfoNV##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExportMemoryAllocateInfoNV## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkExportMemoryAllocateInfoNV##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExportMemoryAllocateInfoNV## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExportMemoryAllocateInfoNV## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExportMemoryAllocateInfoNV## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkExportMemoryAllocateInfoNV where
        type VkSTypeMType VkExportMemoryAllocateInfoNV = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExportMemoryAllocateInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkExportMemoryAllocateInfoNV where
        type VkPNextMType VkExportMemoryAllocateInfoNV = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExportMemoryAllocateInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasVkHandleTypes VkExportMemoryAllocateInfoNV where
        type VkHandleTypesMType VkExportMemoryAllocateInfoNV =
             VkExternalMemoryHandleTypeFlagsNV

        {-# NOINLINE vkHandleTypes #-}
        vkHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoNV, handleTypes})

        {-# INLINE vkHandleTypesByteOffset #-}
        vkHandleTypesByteOffset ~_
          = #{offset VkExportMemoryAllocateInfoNV, handleTypes}

        {-# INLINE readVkHandleTypes #-}
        readVkHandleTypes p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoNV, handleTypes}

        {-# INLINE writeVkHandleTypes #-}
        writeVkHandleTypes p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoNV, handleTypes}

instance Show VkExportMemoryAllocateInfoNV where
        showsPrec d x
          = showString "VkExportMemoryAllocateInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleTypes = " .
                            showsPrec d (vkHandleTypes x) . showChar '}'

pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION = 1

type VK_NV_EXTERNAL_MEMORY_SPEC_VERSION = 1

pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME :: CString

pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME <-
        (is_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME -> True)
  where VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME
          = _VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME

_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME :: CString

{-# INLINE _VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME #-}
_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME
  = Ptr "VK_NV_external_memory\NUL"##

is_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME #-}
is_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME
  = (_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME ==)

type VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_NV_external_memory"

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV =
        VkStructureType 1000056000

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV =
        VkStructureType 1000056001