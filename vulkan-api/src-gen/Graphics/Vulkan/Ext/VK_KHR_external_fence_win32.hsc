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
module Graphics.Vulkan.Ext.VK_KHR_external_fence_win32
       (-- * Vulkan extension: @VK_KHR_external_fence_win32@
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
        -- Extension number: @115@
        --
        -- Required extensions: 'VK_KHR_external_fence'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_WIN32_KHR@
        --

        -- ** Required extensions: 'VK_KHR_external_fence'.
        VkImportFenceWin32HandleInfoKHR(..),
        VkExportFenceWin32HandleInfoKHR(..),
        VkFenceGetWin32HandleInfoKHR(..), vkImportFenceWin32HandleKHR,
        vkGetFenceWin32HandleKHR, VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION,
        VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR)
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

data VkImportFenceWin32HandleInfoKHR = VkImportFenceWin32HandleInfoKHR## ByteArray##

instance Eq VkImportFenceWin32HandleInfoKHR where
        (VkImportFenceWin32HandleInfoKHR## a) ==
          (VkImportFenceWin32HandleInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkImportFenceWin32HandleInfoKHR where
        (VkImportFenceWin32HandleInfoKHR## a) `compare`
          (VkImportFenceWin32HandleInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkImportFenceWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkImportFenceWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportFenceWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkImportFenceWin32HandleInfoKHR),
            I## a <- alignment (undefined :: VkImportFenceWin32HandleInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkImportFenceWin32HandleInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkImportFenceWin32HandleInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkImportFenceWin32HandleInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkImportFenceWin32HandleInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkImportFenceWin32HandleInfoKHR),
            I## a <- alignment (undefined :: VkImportFenceWin32HandleInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkImportFenceWin32HandleInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkImportFenceWin32HandleInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkImportFenceWin32HandleInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkImportFenceWin32HandleInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkImportFenceWin32HandleInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkImportFenceWin32HandleInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkImportFenceWin32HandleInfoKHR where
        type VkSTypeMType VkImportFenceWin32HandleInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImportFenceWin32HandleInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkImportFenceWin32HandleInfoKHR where
        type VkPNextMType VkImportFenceWin32HandleInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImportFenceWin32HandleInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkFence VkImportFenceWin32HandleInfoKHR where
        type VkFenceMType VkImportFenceWin32HandleInfoKHR = VkFence

        {-# NOINLINE vkFence #-}
        vkFence x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, fence})

        {-# INLINE vkFenceByteOffset #-}
        vkFenceByteOffset ~_
          = #{offset VkImportFenceWin32HandleInfoKHR, fence}

        {-# INLINE readVkFence #-}
        readVkFence p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, fence}

        {-# INLINE writeVkFence #-}
        writeVkFence p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, fence}

instance {-# OVERLAPPING #-}
         HasVkFlags VkImportFenceWin32HandleInfoKHR where
        type VkFlagsMType VkImportFenceWin32HandleInfoKHR =
             VkFenceImportFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkImportFenceWin32HandleInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasVkHandleType VkImportFenceWin32HandleInfoKHR where
        type VkHandleTypeMType VkImportFenceWin32HandleInfoKHR =
             VkExternalFenceHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkImportFenceWin32HandleInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasVkHandle VkImportFenceWin32HandleInfoKHR where
        type VkHandleMType VkImportFenceWin32HandleInfoKHR = HANDLE

        {-# NOINLINE vkHandle #-}
        vkHandle x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, handle})

        {-# INLINE vkHandleByteOffset #-}
        vkHandleByteOffset ~_
          = #{offset VkImportFenceWin32HandleInfoKHR, handle}

        {-# INLINE readVkHandle #-}
        readVkHandle p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, handle}

        {-# INLINE writeVkHandle #-}
        writeVkHandle p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         HasVkName VkImportFenceWin32HandleInfoKHR where
        type VkNameMType VkImportFenceWin32HandleInfoKHR = LPCWSTR

        {-# NOINLINE vkName #-}
        vkName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, name})

        {-# INLINE vkNameByteOffset #-}
        vkNameByteOffset ~_
          = #{offset VkImportFenceWin32HandleInfoKHR, name}

        {-# INLINE readVkName #-}
        readVkName p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, name}

        {-# INLINE writeVkName #-}
        writeVkName p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, name}

instance Show VkImportFenceWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkImportFenceWin32HandleInfoKHR {" .
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
                                            showString "vkHandle = " .
                                              showsPrec d (vkHandle x) .
                                                showString ", " .
                                                  showString "vkName = " .
                                                    showsPrec d (vkName x) . showChar '}'

data VkExportFenceWin32HandleInfoKHR = VkExportFenceWin32HandleInfoKHR## ByteArray##

instance Eq VkExportFenceWin32HandleInfoKHR where
        (VkExportFenceWin32HandleInfoKHR## a) ==
          (VkExportFenceWin32HandleInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExportFenceWin32HandleInfoKHR where
        (VkExportFenceWin32HandleInfoKHR## a) `compare`
          (VkExportFenceWin32HandleInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExportFenceWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkExportFenceWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportFenceWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkExportFenceWin32HandleInfoKHR),
            I## a <- alignment (undefined :: VkExportFenceWin32HandleInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExportFenceWin32HandleInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExportFenceWin32HandleInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkExportFenceWin32HandleInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExportFenceWin32HandleInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkExportFenceWin32HandleInfoKHR),
            I## a <- alignment (undefined :: VkExportFenceWin32HandleInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExportFenceWin32HandleInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExportFenceWin32HandleInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkExportFenceWin32HandleInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExportFenceWin32HandleInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExportFenceWin32HandleInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExportFenceWin32HandleInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkExportFenceWin32HandleInfoKHR where
        type VkSTypeMType VkExportFenceWin32HandleInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceWin32HandleInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExportFenceWin32HandleInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExportFenceWin32HandleInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExportFenceWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkExportFenceWin32HandleInfoKHR where
        type VkPNextMType VkExportFenceWin32HandleInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceWin32HandleInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExportFenceWin32HandleInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExportFenceWin32HandleInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExportFenceWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkPAttributes VkExportFenceWin32HandleInfoKHR where
        type VkPAttributesMType VkExportFenceWin32HandleInfoKHR =
             Ptr SECURITY_ATTRIBUTES

        {-# NOINLINE vkPAttributes #-}
        vkPAttributes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceWin32HandleInfoKHR, pAttributes})

        {-# INLINE vkPAttributesByteOffset #-}
        vkPAttributesByteOffset ~_
          = #{offset VkExportFenceWin32HandleInfoKHR, pAttributes}

        {-# INLINE readVkPAttributes #-}
        readVkPAttributes p
          = peekByteOff p #{offset VkExportFenceWin32HandleInfoKHR, pAttributes}

        {-# INLINE writeVkPAttributes #-}
        writeVkPAttributes p
          = pokeByteOff p #{offset VkExportFenceWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         HasVkDwAccess VkExportFenceWin32HandleInfoKHR where
        type VkDwAccessMType VkExportFenceWin32HandleInfoKHR = DWORD

        {-# NOINLINE vkDwAccess #-}
        vkDwAccess x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceWin32HandleInfoKHR, dwAccess})

        {-# INLINE vkDwAccessByteOffset #-}
        vkDwAccessByteOffset ~_
          = #{offset VkExportFenceWin32HandleInfoKHR, dwAccess}

        {-# INLINE readVkDwAccess #-}
        readVkDwAccess p
          = peekByteOff p #{offset VkExportFenceWin32HandleInfoKHR, dwAccess}

        {-# INLINE writeVkDwAccess #-}
        writeVkDwAccess p
          = pokeByteOff p #{offset VkExportFenceWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         HasVkName VkExportFenceWin32HandleInfoKHR where
        type VkNameMType VkExportFenceWin32HandleInfoKHR = LPCWSTR

        {-# NOINLINE vkName #-}
        vkName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceWin32HandleInfoKHR, name})

        {-# INLINE vkNameByteOffset #-}
        vkNameByteOffset ~_
          = #{offset VkExportFenceWin32HandleInfoKHR, name}

        {-# INLINE readVkName #-}
        readVkName p
          = peekByteOff p #{offset VkExportFenceWin32HandleInfoKHR, name}

        {-# INLINE writeVkName #-}
        writeVkName p
          = pokeByteOff p #{offset VkExportFenceWin32HandleInfoKHR, name}

instance Show VkExportFenceWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkExportFenceWin32HandleInfoKHR {" .
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

data VkFenceGetWin32HandleInfoKHR = VkFenceGetWin32HandleInfoKHR## ByteArray##

instance Eq VkFenceGetWin32HandleInfoKHR where
        (VkFenceGetWin32HandleInfoKHR## a) ==
          (VkFenceGetWin32HandleInfoKHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkFenceGetWin32HandleInfoKHR where
        (VkFenceGetWin32HandleInfoKHR## a) `compare`
          (VkFenceGetWin32HandleInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkFenceGetWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkFenceGetWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkFenceGetWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkFenceGetWin32HandleInfoKHR),
            I## a <- alignment (undefined :: VkFenceGetWin32HandleInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkFenceGetWin32HandleInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkFenceGetWin32HandleInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkFenceGetWin32HandleInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkFenceGetWin32HandleInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkFenceGetWin32HandleInfoKHR),
            I## a <- alignment (undefined :: VkFenceGetWin32HandleInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkFenceGetWin32HandleInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkFenceGetWin32HandleInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkFenceGetWin32HandleInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkFenceGetWin32HandleInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkFenceGetWin32HandleInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkFenceGetWin32HandleInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkFenceGetWin32HandleInfoKHR where
        type VkSTypeMType VkFenceGetWin32HandleInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetWin32HandleInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkFenceGetWin32HandleInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkFenceGetWin32HandleInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkFenceGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkFenceGetWin32HandleInfoKHR where
        type VkPNextMType VkFenceGetWin32HandleInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetWin32HandleInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkFenceGetWin32HandleInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkFenceGetWin32HandleInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkFenceGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkFence VkFenceGetWin32HandleInfoKHR where
        type VkFenceMType VkFenceGetWin32HandleInfoKHR = VkFence

        {-# NOINLINE vkFence #-}
        vkFence x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetWin32HandleInfoKHR, fence})

        {-# INLINE vkFenceByteOffset #-}
        vkFenceByteOffset ~_
          = #{offset VkFenceGetWin32HandleInfoKHR, fence}

        {-# INLINE readVkFence #-}
        readVkFence p
          = peekByteOff p #{offset VkFenceGetWin32HandleInfoKHR, fence}

        {-# INLINE writeVkFence #-}
        writeVkFence p
          = pokeByteOff p #{offset VkFenceGetWin32HandleInfoKHR, fence}

instance {-# OVERLAPPING #-}
         HasVkHandleType VkFenceGetWin32HandleInfoKHR where
        type VkHandleTypeMType VkFenceGetWin32HandleInfoKHR =
             VkExternalFenceHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetWin32HandleInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkFenceGetWin32HandleInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkFenceGetWin32HandleInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkFenceGetWin32HandleInfoKHR, handleType}

instance Show VkFenceGetWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkFenceGetWin32HandleInfoKHR {" .
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
--   > VkResult vkImportFenceWin32HandleKHR
--   >     ( VkDevice device
--   >     , const VkImportFenceWin32HandleInfoKHR* pImportFenceWin32HandleInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkImportFenceWin32HandleKHR.html vkImportFenceWin32HandleKHR registry at www.khronos.org>
foreign import ccall unsafe "vkImportFenceWin32HandleKHR"
               vkImportFenceWin32HandleKHR ::
               VkDevice -- ^ device
                        -> Ptr VkImportFenceWin32HandleInfoKHR -- ^ pImportFenceWin32HandleInfo
                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetFenceWin32HandleKHR
--   >     ( VkDevice device
--   >     , const VkFenceGetWin32HandleInfoKHR* pGetWin32HandleInfo
--   >     , HANDLE* pHandle
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetFenceWin32HandleKHR.html vkGetFenceWin32HandleKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetFenceWin32HandleKHR"
               vkGetFenceWin32HandleKHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkFenceGetWin32HandleInfoKHR -- ^ pGetWin32HandleInfo
                                                  -> Ptr HANDLE -- ^ pHandle
                                                                -> IO VkResult

pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME

_VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME #-}
_VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
  = Ptr "VK_KHR_external_fence_win32\NUL"##

is_VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME #-}
is_VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
  = (_VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME ==)

type VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME =
     "VK_KHR_external_fence_win32"

pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR =
        VkStructureType 1000114000

pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR =
        VkStructureType 1000114001

pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR =
        VkStructureType 1000114002
