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
module Graphics.Vulkan.Ext.VK_KHR_external_fence_fd
       (-- * Vulkan extension: @VK_KHR_external_fence_fd@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @jessehall@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @116@
        --
        -- Required extensions: 'VK_KHR_external_fence'.
        --

        -- ** Required extensions: 'VK_KHR_external_fence'.
        VkImportFenceFdInfoKHR(..), VkFenceGetFdInfoKHR(..),
        vkImportFenceFdKHR, vkGetFenceFdKHR,
        VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION,
        VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR)
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

-- | > typedef struct VkImportFenceFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkFence              fence;
--   >     VkFenceImportFlagsKHR  flags;
--   >     VkExternalFenceHandleTypeFlagBitsKHR   handleType;
--   >     int                                    fd;
--   > } VkImportFenceFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImportFenceFdInfoKHR.html VkImportFenceFdInfoKHR registry at www.khronos.org>
data VkImportFenceFdInfoKHR = VkImportFenceFdInfoKHR## ByteArray##

instance Eq VkImportFenceFdInfoKHR where
        (VkImportFenceFdInfoKHR## a) == (VkImportFenceFdInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkImportFenceFdInfoKHR where
        (VkImportFenceFdInfoKHR## a) `compare` (VkImportFenceFdInfoKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkImportFenceFdInfoKHR where
        sizeOf ~_ = #{size VkImportFenceFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImportFenceFdInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkImportFenceFdInfoKHR),
            I## a <- alignment (undefined :: VkImportFenceFdInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkImportFenceFdInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkImportFenceFdInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkImportFenceFdInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkImportFenceFdInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkImportFenceFdInfoKHR),
            I## a <- alignment (undefined :: VkImportFenceFdInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkImportFenceFdInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkImportFenceFdInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkImportFenceFdInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkImportFenceFdInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkImportFenceFdInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkImportFenceFdInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkImportFenceFdInfoKHR
         where
        type VkSTypeMType VkImportFenceFdInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImportFenceFdInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportFenceFdInfoKHR where
        type FieldType "sType" VkImportFenceFdInfoKHR = VkStructureType
        type FieldOptional "sType" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkImportFenceFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkImportFenceFdInfoKHR
         where
        type VkPNextMType VkImportFenceFdInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImportFenceFdInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportFenceFdInfoKHR where
        type FieldType "pNext" VkImportFenceFdInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkImportFenceFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFence VkImportFenceFdInfoKHR
         where
        type VkFenceMType VkImportFenceFdInfoKHR = VkFence

        {-# NOINLINE vkFence #-}
        vkFence x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, fence})

        {-# INLINE vkFenceByteOffset #-}
        vkFenceByteOffset ~_
          = #{offset VkImportFenceFdInfoKHR, fence}

        {-# INLINE readVkFence #-}
        readVkFence p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, fence}

        {-# INLINE writeVkFence #-}
        writeVkFence p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, fence}

instance {-# OVERLAPPING #-}
         HasField "fence" VkImportFenceFdInfoKHR where
        type FieldType "fence" VkImportFenceFdInfoKHR = VkFence
        type FieldOptional "fence" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "fence" VkImportFenceFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkFence

        {-# INLINE readField #-}
        readField = readVkFence

instance CanWriteField "fence" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFence

instance {-# OVERLAPPING #-} HasVkFlags VkImportFenceFdInfoKHR
         where
        type VkFlagsMType VkImportFenceFdInfoKHR = VkFenceImportFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkImportFenceFdInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkImportFenceFdInfoKHR where
        type FieldType "flags" VkImportFenceFdInfoKHR =
             VkFenceImportFlagsKHR
        type FieldOptional "flags" VkImportFenceFdInfoKHR = 'True -- ' closing tick for hsc2hs

instance CanReadField "flags" VkImportFenceFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-} HasVkHandleType VkImportFenceFdInfoKHR
         where
        type VkHandleTypeMType VkImportFenceFdInfoKHR =
             VkExternalFenceHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkImportFenceFdInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportFenceFdInfoKHR where
        type FieldType "handleType" VkImportFenceFdInfoKHR =
             VkExternalFenceHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "handleType" VkImportFenceFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

instance {-# OVERLAPPING #-} HasVkFd VkImportFenceFdInfoKHR where
        type VkFdMType VkImportFenceFdInfoKHR = #{type int}

        {-# NOINLINE vkFd #-}
        vkFd x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, fd})

        {-# INLINE vkFdByteOffset #-}
        vkFdByteOffset ~_
          = #{offset VkImportFenceFdInfoKHR, fd}

        {-# INLINE readVkFd #-}
        readVkFd p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, fd}

        {-# INLINE writeVkFd #-}
        writeVkFd p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, fd}

instance {-# OVERLAPPING #-} HasField "fd" VkImportFenceFdInfoKHR
         where
        type FieldType "fd" VkImportFenceFdInfoKHR =
             #{type int}
        type FieldOptional "fd" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "fd" VkImportFenceFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkFd

        {-# INLINE readField #-}
        readField = readVkFd

instance CanWriteField "fd" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFd

instance Show VkImportFenceFdInfoKHR where
        showsPrec d x
          = showString "VkImportFenceFdInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFence = " .
                            showsPrec d (vkFence x) .
                              showString ", " .
                                showString "vkFlags = " .
                                  showsPrec d (vkFlags x) .
                                    showString ", " .
                                      showString "vkHandleType = " .
                                        showsPrec d (vkHandleType x) .
                                          showString ", " .
                                            showString "vkFd = " .
                                              showsPrec d (vkFd x) . showChar '}'

-- | > typedef struct VkFenceGetFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkFence                                fence;
--   >     VkExternalFenceHandleTypeFlagBitsKHR   handleType;
--   > } VkFenceGetFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFenceGetFdInfoKHR.html VkFenceGetFdInfoKHR registry at www.khronos.org>
data VkFenceGetFdInfoKHR = VkFenceGetFdInfoKHR## ByteArray##

instance Eq VkFenceGetFdInfoKHR where
        (VkFenceGetFdInfoKHR## a) == (VkFenceGetFdInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkFenceGetFdInfoKHR where
        (VkFenceGetFdInfoKHR## a) `compare` (VkFenceGetFdInfoKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkFenceGetFdInfoKHR where
        sizeOf ~_ = #{size VkFenceGetFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkFenceGetFdInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkFenceGetFdInfoKHR),
            I## a <- alignment (undefined :: VkFenceGetFdInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkFenceGetFdInfoKHR## ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkFenceGetFdInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkFenceGetFdInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkFenceGetFdInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkFenceGetFdInfoKHR),
            I## a <- alignment (undefined :: VkFenceGetFdInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkFenceGetFdInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkFenceGetFdInfoKHR## ba) = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkFenceGetFdInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkFenceGetFdInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkFenceGetFdInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkFenceGetFdInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkFenceGetFdInfoKHR where
        type VkSTypeMType VkFenceGetFdInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetFdInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkFenceGetFdInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkFenceGetFdInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkFenceGetFdInfoKHR, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkFenceGetFdInfoKHR
         where
        type FieldType "sType" VkFenceGetFdInfoKHR = VkStructureType
        type FieldOptional "sType" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkFenceGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkFenceGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkFenceGetFdInfoKHR where
        type VkPNextMType VkFenceGetFdInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetFdInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkFenceGetFdInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkFenceGetFdInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkFenceGetFdInfoKHR, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkFenceGetFdInfoKHR
         where
        type FieldType "pNext" VkFenceGetFdInfoKHR = Ptr Void
        type FieldOptional "pNext" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkFenceGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkFenceGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFence VkFenceGetFdInfoKHR where
        type VkFenceMType VkFenceGetFdInfoKHR = VkFence

        {-# NOINLINE vkFence #-}
        vkFence x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetFdInfoKHR, fence})

        {-# INLINE vkFenceByteOffset #-}
        vkFenceByteOffset ~_
          = #{offset VkFenceGetFdInfoKHR, fence}

        {-# INLINE readVkFence #-}
        readVkFence p
          = peekByteOff p #{offset VkFenceGetFdInfoKHR, fence}

        {-# INLINE writeVkFence #-}
        writeVkFence p
          = pokeByteOff p #{offset VkFenceGetFdInfoKHR, fence}

instance {-# OVERLAPPING #-} HasField "fence" VkFenceGetFdInfoKHR
         where
        type FieldType "fence" VkFenceGetFdInfoKHR = VkFence
        type FieldOptional "fence" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "fence" VkFenceGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkFence

        {-# INLINE readField #-}
        readField = readVkFence

instance CanWriteField "fence" VkFenceGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFence

instance {-# OVERLAPPING #-} HasVkHandleType VkFenceGetFdInfoKHR
         where
        type VkHandleTypeMType VkFenceGetFdInfoKHR =
             VkExternalFenceHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetFdInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkFenceGetFdInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkFenceGetFdInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkFenceGetFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkFenceGetFdInfoKHR where
        type FieldType "handleType" VkFenceGetFdInfoKHR =
             VkExternalFenceHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "handleType" VkFenceGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType" VkFenceGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

instance Show VkFenceGetFdInfoKHR where
        showsPrec d x
          = showString "VkFenceGetFdInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFence = " .
                            showsPrec d (vkFence x) .
                              showString ", " .
                                showString "vkHandleType = " .
                                  showsPrec d (vkHandleType x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkImportFenceFdKHR
--   >     ( VkDevice device
--   >     , const VkImportFenceFdInfoKHR* pImportFenceFdInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkImportFenceFdKHR.html vkImportFenceFdKHR registry at www.khronos.org>
foreign import ccall unsafe "vkImportFenceFdKHR" vkImportFenceFdKHR
               :: VkDevice -- ^ device
                           -> Ptr VkImportFenceFdInfoKHR -- ^ pImportFenceFdInfo
                                                         -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetFenceFdKHR
--   >     ( VkDevice device
--   >     , const VkFenceGetFdInfoKHR* pGetFdInfo
--   >     , int* pFd
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetFenceFdKHR.html vkGetFenceFdKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetFenceFdKHR" vkGetFenceFdKHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkFenceGetFdInfoKHR -- ^ pGetFdInfo
                                         ->
                   Ptr #{type int} -- ^ pFd
                                               -> IO VkResult

pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME

_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME #-}
_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
  = Ptr "VK_KHR_external_fence_fd\NUL"##

is_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME #-}
is_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
  = (_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME ==)

type VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME =
     "VK_KHR_external_fence_fd"

pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR =
        VkStructureType 1000115000

pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR :: VkStructureType

pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR =
        VkStructureType 1000115001
