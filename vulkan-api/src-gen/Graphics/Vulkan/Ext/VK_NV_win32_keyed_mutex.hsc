#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE UnboxedTuples   #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vulkan.Ext.VK_NV_win32_keyed_mutex
       (-- * Vulkan extension: @VK_NV_win32_keyed_mutex@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Carsten Rohde@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @59@
        --
        -- Required extensions: 'VK_NV_external_memory_win32'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_WIN32_KHR@
        --

        -- ** Required extensions: 'VK_NV_external_memory_win32'.
        VkWin32KeyedMutexAcquireReleaseInfoNV(..),
        VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION,
        pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION,
        VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME,
        pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkStructureType (..), Word32)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkWin32KeyedMutexAcquireReleaseInfoNV = VkWin32KeyedMutexAcquireReleaseInfoNV## ByteArray##

instance Eq VkWin32KeyedMutexAcquireReleaseInfoNV where
        (VkWin32KeyedMutexAcquireReleaseInfoNV## a) ==
          (VkWin32KeyedMutexAcquireReleaseInfoNV## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkWin32KeyedMutexAcquireReleaseInfoNV where
        (VkWin32KeyedMutexAcquireReleaseInfoNV## a) `compare`
          (VkWin32KeyedMutexAcquireReleaseInfoNV## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkWin32KeyedMutexAcquireReleaseInfoNV where
        sizeOf ~_
          = #{size VkWin32KeyedMutexAcquireReleaseInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkWin32KeyedMutexAcquireReleaseInfoNV}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkWin32KeyedMutexAcquireReleaseInfoNV),
            I## a <- alignment
                      (undefined :: VkWin32KeyedMutexAcquireReleaseInfoNV)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkWin32KeyedMutexAcquireReleaseInfoNV##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkWin32KeyedMutexAcquireReleaseInfoNV## ba)
          | I## n <- sizeOf
                      (undefined :: VkWin32KeyedMutexAcquireReleaseInfoNV)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkWin32KeyedMutexAcquireReleaseInfoNV where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkWin32KeyedMutexAcquireReleaseInfoNV),
            I## a <- alignment
                      (undefined :: VkWin32KeyedMutexAcquireReleaseInfoNV)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkWin32KeyedMutexAcquireReleaseInfoNV##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkWin32KeyedMutexAcquireReleaseInfoNV## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkWin32KeyedMutexAcquireReleaseInfoNV##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkWin32KeyedMutexAcquireReleaseInfoNV## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkWin32KeyedMutexAcquireReleaseInfoNV## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkWin32KeyedMutexAcquireReleaseInfoNV## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkWin32KeyedMutexAcquireReleaseInfoNV where
        type VkSTypeMType VkWin32KeyedMutexAcquireReleaseInfoNV =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkWin32KeyedMutexAcquireReleaseInfoNV where
        type VkPNextMType VkWin32KeyedMutexAcquireReleaseInfoNV = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasVkAcquireCount VkWin32KeyedMutexAcquireReleaseInfoNV where
        type VkAcquireCountMType VkWin32KeyedMutexAcquireReleaseInfoNV =
             Word32

        {-# NOINLINE vkAcquireCount #-}
        vkAcquireCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount})

        {-# INLINE vkAcquireCountByteOffset #-}
        vkAcquireCountByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount}

        {-# INLINE readVkAcquireCount #-}
        readVkAcquireCount p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount}

        {-# INLINE writeVkAcquireCount #-}
        writeVkAcquireCount p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount}

instance {-# OVERLAPPING #-}
         HasVkPAcquireSyncs VkWin32KeyedMutexAcquireReleaseInfoNV where
        type VkPAcquireSyncsMType VkWin32KeyedMutexAcquireReleaseInfoNV =
             Ptr VkDeviceMemory

        {-# NOINLINE vkPAcquireSyncs #-}
        vkPAcquireSyncs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs})

        {-# INLINE vkPAcquireSyncsByteOffset #-}
        vkPAcquireSyncsByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs}

        {-# INLINE readVkPAcquireSyncs #-}
        readVkPAcquireSyncs p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs}

        {-# INLINE writeVkPAcquireSyncs #-}
        writeVkPAcquireSyncs p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs}

instance {-# OVERLAPPING #-}
         HasVkPAcquireKeys VkWin32KeyedMutexAcquireReleaseInfoNV where
        type VkPAcquireKeysMType VkWin32KeyedMutexAcquireReleaseInfoNV =
             Ptr Word64

        {-# NOINLINE vkPAcquireKeys #-}
        vkPAcquireKeys x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys})

        {-# INLINE vkPAcquireKeysByteOffset #-}
        vkPAcquireKeysByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys}

        {-# INLINE readVkPAcquireKeys #-}
        readVkPAcquireKeys p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys}

        {-# INLINE writeVkPAcquireKeys #-}
        writeVkPAcquireKeys p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys}

instance {-# OVERLAPPING #-}
         HasVkPAcquireTimeoutMilliseconds
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        type VkPAcquireTimeoutMillisecondsMType
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = Ptr Word32

        {-# NOINLINE vkPAcquireTimeoutMilliseconds #-}
        vkPAcquireTimeoutMilliseconds x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds})

        {-# INLINE vkPAcquireTimeoutMillisecondsByteOffset #-}
        vkPAcquireTimeoutMillisecondsByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds}

        {-# INLINE readVkPAcquireTimeoutMilliseconds #-}
        readVkPAcquireTimeoutMilliseconds p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds}

        {-# INLINE writeVkPAcquireTimeoutMilliseconds #-}
        writeVkPAcquireTimeoutMilliseconds p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds}

instance {-# OVERLAPPING #-}
         HasVkReleaseCount VkWin32KeyedMutexAcquireReleaseInfoNV where
        type VkReleaseCountMType VkWin32KeyedMutexAcquireReleaseInfoNV =
             Word32

        {-# NOINLINE vkReleaseCount #-}
        vkReleaseCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount})

        {-# INLINE vkReleaseCountByteOffset #-}
        vkReleaseCountByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount}

        {-# INLINE readVkReleaseCount #-}
        readVkReleaseCount p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount}

        {-# INLINE writeVkReleaseCount #-}
        writeVkReleaseCount p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount}

instance {-# OVERLAPPING #-}
         HasVkPReleaseSyncs VkWin32KeyedMutexAcquireReleaseInfoNV where
        type VkPReleaseSyncsMType VkWin32KeyedMutexAcquireReleaseInfoNV =
             Ptr VkDeviceMemory

        {-# NOINLINE vkPReleaseSyncs #-}
        vkPReleaseSyncs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs})

        {-# INLINE vkPReleaseSyncsByteOffset #-}
        vkPReleaseSyncsByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs}

        {-# INLINE readVkPReleaseSyncs #-}
        readVkPReleaseSyncs p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs}

        {-# INLINE writeVkPReleaseSyncs #-}
        writeVkPReleaseSyncs p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs}

instance {-# OVERLAPPING #-}
         HasVkPReleaseKeys VkWin32KeyedMutexAcquireReleaseInfoNV where
        type VkPReleaseKeysMType VkWin32KeyedMutexAcquireReleaseInfoNV =
             Ptr Word64

        {-# NOINLINE vkPReleaseKeys #-}
        vkPReleaseKeys x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys})

        {-# INLINE vkPReleaseKeysByteOffset #-}
        vkPReleaseKeysByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys}

        {-# INLINE readVkPReleaseKeys #-}
        readVkPReleaseKeys p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys}

        {-# INLINE writeVkPReleaseKeys #-}
        writeVkPReleaseKeys p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys}

instance Show VkWin32KeyedMutexAcquireReleaseInfoNV where
        showsPrec d x
          = showString "VkWin32KeyedMutexAcquireReleaseInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkAcquireCount = " .
                            showsPrec d (vkAcquireCount x) .
                              showString ", " .
                                showString "vkPAcquireSyncs = " .
                                  showsPrec d (vkPAcquireSyncs x) .
                                    showString ", " .
                                      showString "vkPAcquireKeys = " .
                                        showsPrec d (vkPAcquireKeys x) .
                                          showString ", " .
                                            showString "vkPAcquireTimeoutMilliseconds = " .
                                              showsPrec d (vkPAcquireTimeoutMilliseconds x) .
                                                showString ", " .
                                                  showString "vkReleaseCount = " .
                                                    showsPrec d (vkReleaseCount x) .
                                                      showString ", " .
                                                        showString "vkPReleaseSyncs = " .
                                                          showsPrec d (vkPReleaseSyncs x) .
                                                            showString ", " .
                                                              showString "vkPReleaseKeys = " .
                                                                showsPrec d (vkPReleaseKeys x) .
                                                                  showChar '}'

pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION = 1

type VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION = 1

pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME :: CString

pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME <-
        (is_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME -> True)
  where VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
          = _VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME

_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME :: CString

{-# INLINE _VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME #-}
_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
  = Ptr "VK_NV_win32_keyed_mutex\NUL"##

is_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME #-}
is_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
  = (_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME ==)

type VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME =
     "VK_NV_win32_keyed_mutex"

pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
        = VkStructureType 1000058000
