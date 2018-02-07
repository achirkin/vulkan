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
module Graphics.Vulkan.Ext.VK_KHR_win32_keyed_mutex
       (-- * Vulkan extension: @VK_KHR_win32_keyed_mutex@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Carsten Rohde@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @76@
        --
        -- Required extensions: 'VK_KHR_external_memory_win32'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_WIN32_KHR@
        --

        -- ** Required extensions: 'VK_KHR_external_memory_win32'.
        VkWin32KeyedMutexAcquireReleaseInfoKHR(..),
        VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION,
        pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION,
        VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME,
        pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR)
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

data VkWin32KeyedMutexAcquireReleaseInfoKHR = VkWin32KeyedMutexAcquireReleaseInfoKHR## ByteArray##

instance Eq VkWin32KeyedMutexAcquireReleaseInfoKHR where
        (VkWin32KeyedMutexAcquireReleaseInfoKHR## a) ==
          (VkWin32KeyedMutexAcquireReleaseInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkWin32KeyedMutexAcquireReleaseInfoKHR where
        (VkWin32KeyedMutexAcquireReleaseInfoKHR## a) `compare`
          (VkWin32KeyedMutexAcquireReleaseInfoKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkWin32KeyedMutexAcquireReleaseInfoKHR where
        sizeOf ~_
          = #{size VkWin32KeyedMutexAcquireReleaseInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkWin32KeyedMutexAcquireReleaseInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkWin32KeyedMutexAcquireReleaseInfoKHR),
            I## a <- alignment
                      (undefined :: VkWin32KeyedMutexAcquireReleaseInfoKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkWin32KeyedMutexAcquireReleaseInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkWin32KeyedMutexAcquireReleaseInfoKHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkWin32KeyedMutexAcquireReleaseInfoKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkWin32KeyedMutexAcquireReleaseInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkWin32KeyedMutexAcquireReleaseInfoKHR),
            I## a <- alignment
                      (undefined :: VkWin32KeyedMutexAcquireReleaseInfoKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkWin32KeyedMutexAcquireReleaseInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkWin32KeyedMutexAcquireReleaseInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkWin32KeyedMutexAcquireReleaseInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkWin32KeyedMutexAcquireReleaseInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkWin32KeyedMutexAcquireReleaseInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkWin32KeyedMutexAcquireReleaseInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkWin32KeyedMutexAcquireReleaseInfoKHR where
        type VkSTypeMType VkWin32KeyedMutexAcquireReleaseInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkWin32KeyedMutexAcquireReleaseInfoKHR where
        type VkPNextMType VkWin32KeyedMutexAcquireReleaseInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkAcquireCount VkWin32KeyedMutexAcquireReleaseInfoKHR where
        type VkAcquireCountMType VkWin32KeyedMutexAcquireReleaseInfoKHR =
             Word32

        {-# NOINLINE vkAcquireCount #-}
        vkAcquireCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, acquireCount})

        {-# INLINE vkAcquireCountByteOffset #-}
        vkAcquireCountByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, acquireCount}

        {-# INLINE readVkAcquireCount #-}
        readVkAcquireCount p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, acquireCount}

        {-# INLINE writeVkAcquireCount #-}
        writeVkAcquireCount p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, acquireCount}

instance {-# OVERLAPPING #-}
         HasVkPAcquireSyncs VkWin32KeyedMutexAcquireReleaseInfoKHR where
        type VkPAcquireSyncsMType VkWin32KeyedMutexAcquireReleaseInfoKHR =
             Ptr VkDeviceMemory

        {-# NOINLINE vkPAcquireSyncs #-}
        vkPAcquireSyncs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireSyncs})

        {-# INLINE vkPAcquireSyncsByteOffset #-}
        vkPAcquireSyncsByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireSyncs}

        {-# INLINE readVkPAcquireSyncs #-}
        readVkPAcquireSyncs p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireSyncs}

        {-# INLINE writeVkPAcquireSyncs #-}
        writeVkPAcquireSyncs p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireSyncs}

instance {-# OVERLAPPING #-}
         HasVkPAcquireKeys VkWin32KeyedMutexAcquireReleaseInfoKHR where
        type VkPAcquireKeysMType VkWin32KeyedMutexAcquireReleaseInfoKHR =
             Ptr Word64

        {-# NOINLINE vkPAcquireKeys #-}
        vkPAcquireKeys x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireKeys})

        {-# INLINE vkPAcquireKeysByteOffset #-}
        vkPAcquireKeysByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireKeys}

        {-# INLINE readVkPAcquireKeys #-}
        readVkPAcquireKeys p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireKeys}

        {-# INLINE writeVkPAcquireKeys #-}
        writeVkPAcquireKeys p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireKeys}

instance {-# OVERLAPPING #-}
         HasVkPAcquireTimeouts VkWin32KeyedMutexAcquireReleaseInfoKHR where
        type VkPAcquireTimeoutsMType VkWin32KeyedMutexAcquireReleaseInfoKHR
             = Ptr Word32

        {-# NOINLINE vkPAcquireTimeouts #-}
        vkPAcquireTimeouts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireTimeouts})

        {-# INLINE vkPAcquireTimeoutsByteOffset #-}
        vkPAcquireTimeoutsByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireTimeouts}

        {-# INLINE readVkPAcquireTimeouts #-}
        readVkPAcquireTimeouts p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireTimeouts}

        {-# INLINE writeVkPAcquireTimeouts #-}
        writeVkPAcquireTimeouts p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireTimeouts}

instance {-# OVERLAPPING #-}
         HasVkReleaseCount VkWin32KeyedMutexAcquireReleaseInfoKHR where
        type VkReleaseCountMType VkWin32KeyedMutexAcquireReleaseInfoKHR =
             Word32

        {-# NOINLINE vkReleaseCount #-}
        vkReleaseCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, releaseCount})

        {-# INLINE vkReleaseCountByteOffset #-}
        vkReleaseCountByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, releaseCount}

        {-# INLINE readVkReleaseCount #-}
        readVkReleaseCount p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, releaseCount}

        {-# INLINE writeVkReleaseCount #-}
        writeVkReleaseCount p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, releaseCount}

instance {-# OVERLAPPING #-}
         HasVkPReleaseSyncs VkWin32KeyedMutexAcquireReleaseInfoKHR where
        type VkPReleaseSyncsMType VkWin32KeyedMutexAcquireReleaseInfoKHR =
             Ptr VkDeviceMemory

        {-# NOINLINE vkPReleaseSyncs #-}
        vkPReleaseSyncs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseSyncs})

        {-# INLINE vkPReleaseSyncsByteOffset #-}
        vkPReleaseSyncsByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseSyncs}

        {-# INLINE readVkPReleaseSyncs #-}
        readVkPReleaseSyncs p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseSyncs}

        {-# INLINE writeVkPReleaseSyncs #-}
        writeVkPReleaseSyncs p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseSyncs}

instance {-# OVERLAPPING #-}
         HasVkPReleaseKeys VkWin32KeyedMutexAcquireReleaseInfoKHR where
        type VkPReleaseKeysMType VkWin32KeyedMutexAcquireReleaseInfoKHR =
             Ptr Word64

        {-# NOINLINE vkPReleaseKeys #-}
        vkPReleaseKeys x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseKeys})

        {-# INLINE vkPReleaseKeysByteOffset #-}
        vkPReleaseKeysByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseKeys}

        {-# INLINE readVkPReleaseKeys #-}
        readVkPReleaseKeys p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseKeys}

        {-# INLINE writeVkPReleaseKeys #-}
        writeVkPReleaseKeys p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseKeys}

instance Show VkWin32KeyedMutexAcquireReleaseInfoKHR where
        showsPrec d x
          = showString "VkWin32KeyedMutexAcquireReleaseInfoKHR {" .
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
                                            showString "vkPAcquireTimeouts = " .
                                              showsPrec d (vkPAcquireTimeouts x) .
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

pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION = 1

type VK_KHR_WIN32_KEYED_MUTEX_SPEC_VERSION = 1

pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME :: CString

pattern VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME <-
        (is_VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME -> True)
  where VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME
          = _VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME

_VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME #-}
_VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME
  = Ptr "VK_KHR_win32_keyed_mutex\NUL"##

is_VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME #-}
is_VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME
  = (_VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME ==)

type VK_KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME =
     "VK_KHR_win32_keyed_mutex"

pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR
        = VkStructureType 1000075000
