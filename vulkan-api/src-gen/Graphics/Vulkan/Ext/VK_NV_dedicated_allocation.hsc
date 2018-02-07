#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedFFITypes     #-}
{-# LANGUAGE ViewPatterns         #-}
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
import           Data.Int
import           Data.Void                        (Void)
import           Data.Word
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Core
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

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
