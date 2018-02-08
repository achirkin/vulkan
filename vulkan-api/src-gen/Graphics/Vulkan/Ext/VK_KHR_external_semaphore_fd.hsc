#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_external_semaphore_fd
       (-- * Vulkan extension: @VK_KHR_external_semaphore_fd@
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
        -- Extension number: @80@
        --
        -- Required extensions: 'VK_KHR_external_semaphore'.
        --

        -- ** Required extensions: 'VK_KHR_external_semaphore'.
        VkImportSemaphoreFdInfoKHR(..), VkSemaphoreGetFdInfoKHR(..),
        vkImportSemaphoreFdKHR, vkGetSemaphoreFdKHR,
        VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION,
        VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkDevice, VkExternalSemaphoreHandleTypeFlagBitsKHR,
                                                   VkResult (..), VkSemaphore,
                                                   VkSemaphoreImportFlagsKHR,
                                                   VkStructureType,
                                                   VkStructureType (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkImportSemaphoreFdInfoKHR = VkImportSemaphoreFdInfoKHR## ByteArray##

instance Eq VkImportSemaphoreFdInfoKHR where
        (VkImportSemaphoreFdInfoKHR## a) == (VkImportSemaphoreFdInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkImportSemaphoreFdInfoKHR where
        (VkImportSemaphoreFdInfoKHR## a) `compare`
          (VkImportSemaphoreFdInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkImportSemaphoreFdInfoKHR where
        sizeOf ~_ = #{size VkImportSemaphoreFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImportSemaphoreFdInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkImportSemaphoreFdInfoKHR),
            I## a <- alignment (undefined :: VkImportSemaphoreFdInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkImportSemaphoreFdInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkImportSemaphoreFdInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkImportSemaphoreFdInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkImportSemaphoreFdInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkImportSemaphoreFdInfoKHR),
            I## a <- alignment (undefined :: VkImportSemaphoreFdInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkImportSemaphoreFdInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkImportSemaphoreFdInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkImportSemaphoreFdInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkImportSemaphoreFdInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkImportSemaphoreFdInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkImportSemaphoreFdInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkImportSemaphoreFdInfoKHR
         where
        type VkSTypeMType VkImportSemaphoreFdInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreFdInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImportSemaphoreFdInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImportSemaphoreFdInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImportSemaphoreFdInfoKHR, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkImportSemaphoreFdInfoKHR
         where
        type VkPNextMType VkImportSemaphoreFdInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreFdInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImportSemaphoreFdInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImportSemaphoreFdInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImportSemaphoreFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkSemaphore VkImportSemaphoreFdInfoKHR where
        type VkSemaphoreMType VkImportSemaphoreFdInfoKHR = VkSemaphore

        {-# NOINLINE vkSemaphore #-}
        vkSemaphore x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreFdInfoKHR, semaphore})

        {-# INLINE vkSemaphoreByteOffset #-}
        vkSemaphoreByteOffset ~_
          = #{offset VkImportSemaphoreFdInfoKHR, semaphore}

        {-# INLINE readVkSemaphore #-}
        readVkSemaphore p
          = peekByteOff p #{offset VkImportSemaphoreFdInfoKHR, semaphore}

        {-# INLINE writeVkSemaphore #-}
        writeVkSemaphore p
          = pokeByteOff p #{offset VkImportSemaphoreFdInfoKHR, semaphore}

instance {-# OVERLAPPING #-} HasVkFlags VkImportSemaphoreFdInfoKHR
         where
        type VkFlagsMType VkImportSemaphoreFdInfoKHR =
             VkSemaphoreImportFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreFdInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkImportSemaphoreFdInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkImportSemaphoreFdInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkImportSemaphoreFdInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasVkHandleType VkImportSemaphoreFdInfoKHR where
        type VkHandleTypeMType VkImportSemaphoreFdInfoKHR =
             VkExternalSemaphoreHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreFdInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkImportSemaphoreFdInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkImportSemaphoreFdInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkImportSemaphoreFdInfoKHR, handleType}

instance {-# OVERLAPPING #-} HasVkFd VkImportSemaphoreFdInfoKHR
         where
        type VkFdMType VkImportSemaphoreFdInfoKHR = #{type int}

        {-# NOINLINE vkFd #-}
        vkFd x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreFdInfoKHR, fd})

        {-# INLINE vkFdByteOffset #-}
        vkFdByteOffset ~_
          = #{offset VkImportSemaphoreFdInfoKHR, fd}

        {-# INLINE readVkFd #-}
        readVkFd p
          = peekByteOff p #{offset VkImportSemaphoreFdInfoKHR, fd}

        {-# INLINE writeVkFd #-}
        writeVkFd p
          = pokeByteOff p #{offset VkImportSemaphoreFdInfoKHR, fd}

instance Show VkImportSemaphoreFdInfoKHR where
        showsPrec d x
          = showString "VkImportSemaphoreFdInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSemaphore = " .
                            showsPrec d (vkSemaphore x) .
                              showString ", " .
                                showString "vkFlags = " .
                                  showsPrec d (vkFlags x) .
                                    showString ", " .
                                      showString "vkHandleType = " .
                                        showsPrec d (vkHandleType x) .
                                          showString ", " .
                                            showString "vkFd = " .
                                              showsPrec d (vkFd x) . showChar '}'

data VkSemaphoreGetFdInfoKHR = VkSemaphoreGetFdInfoKHR## ByteArray##

instance Eq VkSemaphoreGetFdInfoKHR where
        (VkSemaphoreGetFdInfoKHR## a) == (VkSemaphoreGetFdInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkSemaphoreGetFdInfoKHR where
        (VkSemaphoreGetFdInfoKHR## a) `compare` (VkSemaphoreGetFdInfoKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkSemaphoreGetFdInfoKHR where
        sizeOf ~_ = #{size VkSemaphoreGetFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSemaphoreGetFdInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkSemaphoreGetFdInfoKHR),
            I## a <- alignment (undefined :: VkSemaphoreGetFdInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkSemaphoreGetFdInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkSemaphoreGetFdInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkSemaphoreGetFdInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkSemaphoreGetFdInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkSemaphoreGetFdInfoKHR),
            I## a <- alignment (undefined :: VkSemaphoreGetFdInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkSemaphoreGetFdInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkSemaphoreGetFdInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkSemaphoreGetFdInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkSemaphoreGetFdInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkSemaphoreGetFdInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkSemaphoreGetFdInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkSemaphoreGetFdInfoKHR
         where
        type VkSTypeMType VkSemaphoreGetFdInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetFdInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSemaphoreGetFdInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSemaphoreGetFdInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSemaphoreGetFdInfoKHR, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkSemaphoreGetFdInfoKHR
         where
        type VkPNextMType VkSemaphoreGetFdInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetFdInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSemaphoreGetFdInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSemaphoreGetFdInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSemaphoreGetFdInfoKHR, pNext}

instance {-# OVERLAPPING #-} HasVkSemaphore VkSemaphoreGetFdInfoKHR
         where
        type VkSemaphoreMType VkSemaphoreGetFdInfoKHR = VkSemaphore

        {-# NOINLINE vkSemaphore #-}
        vkSemaphore x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetFdInfoKHR, semaphore})

        {-# INLINE vkSemaphoreByteOffset #-}
        vkSemaphoreByteOffset ~_
          = #{offset VkSemaphoreGetFdInfoKHR, semaphore}

        {-# INLINE readVkSemaphore #-}
        readVkSemaphore p
          = peekByteOff p #{offset VkSemaphoreGetFdInfoKHR, semaphore}

        {-# INLINE writeVkSemaphore #-}
        writeVkSemaphore p
          = pokeByteOff p #{offset VkSemaphoreGetFdInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         HasVkHandleType VkSemaphoreGetFdInfoKHR where
        type VkHandleTypeMType VkSemaphoreGetFdInfoKHR =
             VkExternalSemaphoreHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetFdInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkSemaphoreGetFdInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkSemaphoreGetFdInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkSemaphoreGetFdInfoKHR, handleType}

instance Show VkSemaphoreGetFdInfoKHR where
        showsPrec d x
          = showString "VkSemaphoreGetFdInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSemaphore = " .
                            showsPrec d (vkSemaphore x) .
                              showString ", " .
                                showString "vkHandleType = " .
                                  showsPrec d (vkHandleType x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkImportSemaphoreFdKHR
--   >     ( VkDevice device
--   >     , const VkImportSemaphoreFdInfoKHR* pImportSemaphoreFdInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkImportSemaphoreFdKHR.html vkImportSemaphoreFdKHR registry at www.khronos.org>
foreign import ccall unsafe "vkImportSemaphoreFdKHR"
               vkImportSemaphoreFdKHR ::
               VkDevice -- ^ device
                        -> Ptr VkImportSemaphoreFdInfoKHR -- ^ pImportSemaphoreFdInfo
                                                          -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetSemaphoreFdKHR
--   >     ( VkDevice device
--   >     , const VkSemaphoreGetFdInfoKHR* pGetFdInfo
--   >     , int* pFd
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetSemaphoreFdKHR.html vkGetSemaphoreFdKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetSemaphoreFdKHR"
               vkGetSemaphoreFdKHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSemaphoreGetFdInfoKHR -- ^ pGetFdInfo
                                             ->
                   Ptr #{type int} -- ^ pFd
                                               -> IO VkResult

pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME

_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME #-}
_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
  = Ptr "VK_KHR_external_semaphore_fd\NUL"##

is_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME #-}
is_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
  = (_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME ==)

type VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME =
     "VK_KHR_external_semaphore_fd"

pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR =
        VkStructureType 1000079000

pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR =
        VkStructureType 1000079001
