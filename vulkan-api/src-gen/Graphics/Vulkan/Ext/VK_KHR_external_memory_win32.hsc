#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE UnliftedFFITypes         #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_external_memory_win32
       (-- * Vulkan extension: @VK_KHR_external_memory_win32@
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
        -- Extension number: @74@
        --
        -- Required extensions: 'VK_KHR_external_memory'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_WIN32_KHR@
        --

        -- ** Required extensions: 'VK_KHR_external_memory'.
        VkImportMemoryWin32HandleInfoKHR(..),
        VkExportMemoryWin32HandleInfoKHR(..),
        VkMemoryWin32HandlePropertiesKHR(..),
        VkMemoryGetWin32HandleInfoKHR(..), vkGetMemoryWin32HandleKHR,
        vkGetMemoryWin32HandlePropertiesKHR,
        VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION,
        VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR)
       where
import           Data.Int
import           Data.Void                        (Void)
import           Data.Word
import           Foreign.C.String                 (CString)
import           Foreign.C.Types                  (CChar (..), CFloat (..),
                                                   CInt (..), CSize (..),
                                                   CULong (..))
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

data VkImportMemoryWin32HandleInfoKHR = VkImportMemoryWin32HandleInfoKHR## ByteArray##

instance Eq VkImportMemoryWin32HandleInfoKHR where
        (VkImportMemoryWin32HandleInfoKHR## a) ==
          (VkImportMemoryWin32HandleInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkImportMemoryWin32HandleInfoKHR where
        (VkImportMemoryWin32HandleInfoKHR## a) `compare`
          (VkImportMemoryWin32HandleInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkImportMemoryWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkImportMemoryWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportMemoryWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkImportMemoryWin32HandleInfoKHR),
            I## a <- alignment (undefined :: VkImportMemoryWin32HandleInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkImportMemoryWin32HandleInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkImportMemoryWin32HandleInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkImportMemoryWin32HandleInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkImportMemoryWin32HandleInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkImportMemoryWin32HandleInfoKHR),
            I## a <- alignment (undefined :: VkImportMemoryWin32HandleInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkImportMemoryWin32HandleInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkImportMemoryWin32HandleInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkImportMemoryWin32HandleInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkImportMemoryWin32HandleInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkImportMemoryWin32HandleInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkImportMemoryWin32HandleInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkImportMemoryWin32HandleInfoKHR where
        type VkSTypeMType VkImportMemoryWin32HandleInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImportMemoryWin32HandleInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkImportMemoryWin32HandleInfoKHR where
        type VkPNextMType VkImportMemoryWin32HandleInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImportMemoryWin32HandleInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkHandleType VkImportMemoryWin32HandleInfoKHR where
        type VkHandleTypeMType VkImportMemoryWin32HandleInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkImportMemoryWin32HandleInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasVkHandle VkImportMemoryWin32HandleInfoKHR where
        type VkHandleMType VkImportMemoryWin32HandleInfoKHR = HANDLE

        {-# NOINLINE vkHandle #-}
        vkHandle x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoKHR, handle})

        {-# INLINE vkHandleByteOffset #-}
        vkHandleByteOffset ~_
          = #{offset VkImportMemoryWin32HandleInfoKHR, handle}

        {-# INLINE readVkHandle #-}
        readVkHandle p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, handle}

        {-# INLINE writeVkHandle #-}
        writeVkHandle p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         HasVkName VkImportMemoryWin32HandleInfoKHR where
        type VkNameMType VkImportMemoryWin32HandleInfoKHR = LPCWSTR

        {-# NOINLINE vkName #-}
        vkName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoKHR, name})

        {-# INLINE vkNameByteOffset #-}
        vkNameByteOffset ~_
          = #{offset VkImportMemoryWin32HandleInfoKHR, name}

        {-# INLINE readVkName #-}
        readVkName p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, name}

        {-# INLINE writeVkName #-}
        writeVkName p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, name}

instance Show VkImportMemoryWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkImportMemoryWin32HandleInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleType = " .
                            showsPrec d (vkHandleType x) .
                              showString ", " .
                                showString "vkHandle = " .
                                  showsPrec d (vkHandle x) .
                                    showString ", " .
                                      showString "vkName = " . showsPrec d (vkName x) . showChar '}'

data VkExportMemoryWin32HandleInfoKHR = VkExportMemoryWin32HandleInfoKHR## ByteArray##

instance Eq VkExportMemoryWin32HandleInfoKHR where
        (VkExportMemoryWin32HandleInfoKHR## a) ==
          (VkExportMemoryWin32HandleInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExportMemoryWin32HandleInfoKHR where
        (VkExportMemoryWin32HandleInfoKHR## a) `compare`
          (VkExportMemoryWin32HandleInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExportMemoryWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkExportMemoryWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportMemoryWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkExportMemoryWin32HandleInfoKHR),
            I## a <- alignment (undefined :: VkExportMemoryWin32HandleInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExportMemoryWin32HandleInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExportMemoryWin32HandleInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkExportMemoryWin32HandleInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExportMemoryWin32HandleInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkExportMemoryWin32HandleInfoKHR),
            I## a <- alignment (undefined :: VkExportMemoryWin32HandleInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExportMemoryWin32HandleInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExportMemoryWin32HandleInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkExportMemoryWin32HandleInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExportMemoryWin32HandleInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExportMemoryWin32HandleInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExportMemoryWin32HandleInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkExportMemoryWin32HandleInfoKHR where
        type VkSTypeMType VkExportMemoryWin32HandleInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExportMemoryWin32HandleInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkExportMemoryWin32HandleInfoKHR where
        type VkPNextMType VkExportMemoryWin32HandleInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExportMemoryWin32HandleInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkPAttributes VkExportMemoryWin32HandleInfoKHR where
        type VkPAttributesMType VkExportMemoryWin32HandleInfoKHR =
             Ptr SECURITY_ATTRIBUTES

        {-# NOINLINE vkPAttributes #-}
        vkPAttributes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes})

        {-# INLINE vkPAttributesByteOffset #-}
        vkPAttributesByteOffset ~_
          = #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes}

        {-# INLINE readVkPAttributes #-}
        readVkPAttributes p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes}

        {-# INLINE writeVkPAttributes #-}
        writeVkPAttributes p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         HasVkDwAccess VkExportMemoryWin32HandleInfoKHR where
        type VkDwAccessMType VkExportMemoryWin32HandleInfoKHR = DWORD

        {-# NOINLINE vkDwAccess #-}
        vkDwAccess x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess})

        {-# INLINE vkDwAccessByteOffset #-}
        vkDwAccessByteOffset ~_
          = #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess}

        {-# INLINE readVkDwAccess #-}
        readVkDwAccess p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess}

        {-# INLINE writeVkDwAccess #-}
        writeVkDwAccess p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         HasVkName VkExportMemoryWin32HandleInfoKHR where
        type VkNameMType VkExportMemoryWin32HandleInfoKHR = LPCWSTR

        {-# NOINLINE vkName #-}
        vkName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoKHR, name})

        {-# INLINE vkNameByteOffset #-}
        vkNameByteOffset ~_
          = #{offset VkExportMemoryWin32HandleInfoKHR, name}

        {-# INLINE readVkName #-}
        readVkName p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, name}

        {-# INLINE writeVkName #-}
        writeVkName p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, name}

instance Show VkExportMemoryWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkExportMemoryWin32HandleInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPAttributes = " .
                            showsPrec d (vkPAttributes x) .
                              showString ", " .
                                showString "vkDwAccess = " .
                                  showsPrec d (vkDwAccess x) .
                                    showString ", " .
                                      showString "vkName = " . showsPrec d (vkName x) . showChar '}'

data VkMemoryWin32HandlePropertiesKHR = VkMemoryWin32HandlePropertiesKHR## ByteArray##

instance Eq VkMemoryWin32HandlePropertiesKHR where
        (VkMemoryWin32HandlePropertiesKHR## a) ==
          (VkMemoryWin32HandlePropertiesKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkMemoryWin32HandlePropertiesKHR where
        (VkMemoryWin32HandlePropertiesKHR## a) `compare`
          (VkMemoryWin32HandlePropertiesKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkMemoryWin32HandlePropertiesKHR where
        sizeOf ~_ = #{size VkMemoryWin32HandlePropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryWin32HandlePropertiesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkMemoryWin32HandlePropertiesKHR),
            I## a <- alignment (undefined :: VkMemoryWin32HandlePropertiesKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkMemoryWin32HandlePropertiesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkMemoryWin32HandlePropertiesKHR## ba)
          | I## n <- sizeOf (undefined :: VkMemoryWin32HandlePropertiesKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryWin32HandlePropertiesKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkMemoryWin32HandlePropertiesKHR),
            I## a <- alignment (undefined :: VkMemoryWin32HandlePropertiesKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkMemoryWin32HandlePropertiesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkMemoryWin32HandlePropertiesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkMemoryWin32HandlePropertiesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkMemoryWin32HandlePropertiesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkMemoryWin32HandlePropertiesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkMemoryWin32HandlePropertiesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkMemoryWin32HandlePropertiesKHR where
        type VkSTypeMType VkMemoryWin32HandlePropertiesKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryWin32HandlePropertiesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMemoryWin32HandlePropertiesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkMemoryWin32HandlePropertiesKHR where
        type VkPNextMType VkMemoryWin32HandlePropertiesKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryWin32HandlePropertiesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMemoryWin32HandlePropertiesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkMemoryTypeBits VkMemoryWin32HandlePropertiesKHR where
        type VkMemoryTypeBitsMType VkMemoryWin32HandlePropertiesKHR =
             Word32

        {-# NOINLINE vkMemoryTypeBits #-}
        vkMemoryTypeBits x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits})

        {-# INLINE vkMemoryTypeBitsByteOffset #-}
        vkMemoryTypeBitsByteOffset ~_
          = #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits}

        {-# INLINE readVkMemoryTypeBits #-}
        readVkMemoryTypeBits p
          = peekByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits}

        {-# INLINE writeVkMemoryTypeBits #-}
        writeVkMemoryTypeBits p
          = pokeByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits}

instance Show VkMemoryWin32HandlePropertiesKHR where
        showsPrec d x
          = showString "VkMemoryWin32HandlePropertiesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMemoryTypeBits = " .
                            showsPrec d (vkMemoryTypeBits x) . showChar '}'

data VkMemoryGetWin32HandleInfoKHR = VkMemoryGetWin32HandleInfoKHR## ByteArray##

instance Eq VkMemoryGetWin32HandleInfoKHR where
        (VkMemoryGetWin32HandleInfoKHR## a) ==
          (VkMemoryGetWin32HandleInfoKHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkMemoryGetWin32HandleInfoKHR where
        (VkMemoryGetWin32HandleInfoKHR## a) `compare`
          (VkMemoryGetWin32HandleInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkMemoryGetWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkMemoryGetWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryGetWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkMemoryGetWin32HandleInfoKHR),
            I## a <- alignment (undefined :: VkMemoryGetWin32HandleInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkMemoryGetWin32HandleInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkMemoryGetWin32HandleInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkMemoryGetWin32HandleInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkMemoryGetWin32HandleInfoKHR),
            I## a <- alignment (undefined :: VkMemoryGetWin32HandleInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkMemoryGetWin32HandleInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkMemoryGetWin32HandleInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkMemoryGetWin32HandleInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkMemoryGetWin32HandleInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkMemoryGetWin32HandleInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkMemoryGetWin32HandleInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkMemoryGetWin32HandleInfoKHR where
        type VkSTypeMType VkMemoryGetWin32HandleInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetWin32HandleInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMemoryGetWin32HandleInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkMemoryGetWin32HandleInfoKHR where
        type VkPNextMType VkMemoryGetWin32HandleInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetWin32HandleInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMemoryGetWin32HandleInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkMemory VkMemoryGetWin32HandleInfoKHR where
        type VkMemoryMType VkMemoryGetWin32HandleInfoKHR = VkDeviceMemory

        {-# NOINLINE vkMemory #-}
        vkMemory x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetWin32HandleInfoKHR, memory})

        {-# INLINE vkMemoryByteOffset #-}
        vkMemoryByteOffset ~_
          = #{offset VkMemoryGetWin32HandleInfoKHR, memory}

        {-# INLINE readVkMemory #-}
        readVkMemory p
          = peekByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, memory}

        {-# INLINE writeVkMemory #-}
        writeVkMemory p
          = pokeByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, memory}

instance {-# OVERLAPPING #-}
         HasVkHandleType VkMemoryGetWin32HandleInfoKHR where
        type VkHandleTypeMType VkMemoryGetWin32HandleInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetWin32HandleInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkMemoryGetWin32HandleInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, handleType}

instance Show VkMemoryGetWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkMemoryGetWin32HandleInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMemory = " .
                            showsPrec d (vkMemory x) .
                              showString ", " .
                                showString "vkHandleType = " .
                                  showsPrec d (vkHandleType x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetMemoryWin32HandleKHR
--   >     ( VkDevice device
--   >     , const VkMemoryGetWin32HandleInfoKHR* pGetWin32HandleInfo
--   >     , HANDLE* pHandle
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetMemoryWin32HandleKHR.html vkGetMemoryWin32HandleKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetMemoryWin32HandleKHR"
               vkGetMemoryWin32HandleKHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkMemoryGetWin32HandleInfoKHR -- ^ pGetWin32HandleInfo
                                                   -> Ptr HANDLE -- ^ pHandle
                                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkGetMemoryWin32HandlePropertiesKHR
--   >     ( VkDevice device
--   >     , VkExternalMemoryHandleTypeFlagBitsKHR handleType
--   >     , HANDLE handle
--   >     , VkMemoryWin32HandlePropertiesKHR* pMemoryWin32HandleProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetMemoryWin32HandlePropertiesKHR.html vkGetMemoryWin32HandlePropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetMemoryWin32HandlePropertiesKHR"
               vkGetMemoryWin32HandlePropertiesKHR ::
               VkDevice -- ^ device
                        ->
                 VkExternalMemoryHandleTypeFlagBitsKHR -- ^ handleType
                                                       ->
                   HANDLE -- ^ handle
                          -> Ptr VkMemoryWin32HandlePropertiesKHR -- ^ pMemoryWin32HandleProperties
                                                                  -> IO VkResult

pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME

_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME #-}
_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  = Ptr "VK_KHR_external_memory_win32\NUL"##

is_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME #-}
is_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  = (_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME ==)

type VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME =
     "VK_KHR_external_memory_win32"

pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR =
        VkStructureType 1000073000

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR =
        VkStructureType 1000073001

pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR =
        VkStructureType 1000073002

pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR =
        VkStructureType 1000073003
