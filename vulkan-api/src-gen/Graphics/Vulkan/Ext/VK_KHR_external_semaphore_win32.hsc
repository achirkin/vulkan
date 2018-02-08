#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_external_semaphore_win32
       (-- * Vulkan extension: @VK_KHR_external_semaphore_win32@
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
        -- Extension number: @79@
        --
        -- Required extensions: 'VK_KHR_external_semaphore'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_WIN32_KHR@
        --

        -- ** Required extensions: 'VK_KHR_external_semaphore'.
        VkImportSemaphoreWin32HandleInfoKHR(..),
        VkExportSemaphoreWin32HandleInfoKHR(..),
        VkD3D12FenceSubmitInfoKHR(..),
        VkSemaphoreGetWin32HandleInfoKHR(..),
        vkImportSemaphoreWin32HandleKHR, vkGetSemaphoreWin32HandleKHR,
        VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION,
        VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR)
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

data VkImportSemaphoreWin32HandleInfoKHR = VkImportSemaphoreWin32HandleInfoKHR## ByteArray##

instance Eq VkImportSemaphoreWin32HandleInfoKHR where
        (VkImportSemaphoreWin32HandleInfoKHR## a) ==
          (VkImportSemaphoreWin32HandleInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkImportSemaphoreWin32HandleInfoKHR where
        (VkImportSemaphoreWin32HandleInfoKHR## a) `compare`
          (VkImportSemaphoreWin32HandleInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkImportSemaphoreWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkImportSemaphoreWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportSemaphoreWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkImportSemaphoreWin32HandleInfoKHR),
            I## a <- alignment
                      (undefined :: VkImportSemaphoreWin32HandleInfoKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkImportSemaphoreWin32HandleInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkImportSemaphoreWin32HandleInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkImportSemaphoreWin32HandleInfoKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkImportSemaphoreWin32HandleInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkImportSemaphoreWin32HandleInfoKHR),
            I## a <- alignment
                      (undefined :: VkImportSemaphoreWin32HandleInfoKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkImportSemaphoreWin32HandleInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkImportSemaphoreWin32HandleInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkImportSemaphoreWin32HandleInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkImportSemaphoreWin32HandleInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkImportSemaphoreWin32HandleInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkImportSemaphoreWin32HandleInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkImportSemaphoreWin32HandleInfoKHR where
        type VkSTypeMType VkImportSemaphoreWin32HandleInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkImportSemaphoreWin32HandleInfoKHR where
        type VkPNextMType VkImportSemaphoreWin32HandleInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkSemaphore VkImportSemaphoreWin32HandleInfoKHR where
        type VkSemaphoreMType VkImportSemaphoreWin32HandleInfoKHR =
             VkSemaphore

        {-# NOINLINE vkSemaphore #-}
        vkSemaphore x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore})

        {-# INLINE vkSemaphoreByteOffset #-}
        vkSemaphoreByteOffset ~_
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore}

        {-# INLINE readVkSemaphore #-}
        readVkSemaphore p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore}

        {-# INLINE writeVkSemaphore #-}
        writeVkSemaphore p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         HasVkFlags VkImportSemaphoreWin32HandleInfoKHR where
        type VkFlagsMType VkImportSemaphoreWin32HandleInfoKHR =
             VkSemaphoreImportFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasVkHandleType VkImportSemaphoreWin32HandleInfoKHR where
        type VkHandleTypeMType VkImportSemaphoreWin32HandleInfoKHR =
             VkExternalSemaphoreHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasVkHandle VkImportSemaphoreWin32HandleInfoKHR where
        type VkHandleMType VkImportSemaphoreWin32HandleInfoKHR = HANDLE

        {-# NOINLINE vkHandle #-}
        vkHandle x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, handle})

        {-# INLINE vkHandleByteOffset #-}
        vkHandleByteOffset ~_
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, handle}

        {-# INLINE readVkHandle #-}
        readVkHandle p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, handle}

        {-# INLINE writeVkHandle #-}
        writeVkHandle p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         HasVkName VkImportSemaphoreWin32HandleInfoKHR where
        type VkNameMType VkImportSemaphoreWin32HandleInfoKHR = LPCWSTR

        {-# NOINLINE vkName #-}
        vkName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, name})

        {-# INLINE vkNameByteOffset #-}
        vkNameByteOffset ~_
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, name}

        {-# INLINE readVkName #-}
        readVkName p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, name}

        {-# INLINE writeVkName #-}
        writeVkName p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, name}

instance Show VkImportSemaphoreWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkImportSemaphoreWin32HandleInfoKHR {" .
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
                                            showString "vkHandle = " .
                                              showsPrec d (vkHandle x) .
                                                showString ", " .
                                                  showString "vkName = " .
                                                    showsPrec d (vkName x) . showChar '}'

data VkExportSemaphoreWin32HandleInfoKHR = VkExportSemaphoreWin32HandleInfoKHR## ByteArray##

instance Eq VkExportSemaphoreWin32HandleInfoKHR where
        (VkExportSemaphoreWin32HandleInfoKHR## a) ==
          (VkExportSemaphoreWin32HandleInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExportSemaphoreWin32HandleInfoKHR where
        (VkExportSemaphoreWin32HandleInfoKHR## a) `compare`
          (VkExportSemaphoreWin32HandleInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExportSemaphoreWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkExportSemaphoreWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportSemaphoreWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkExportSemaphoreWin32HandleInfoKHR),
            I## a <- alignment
                      (undefined :: VkExportSemaphoreWin32HandleInfoKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExportSemaphoreWin32HandleInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExportSemaphoreWin32HandleInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkExportSemaphoreWin32HandleInfoKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExportSemaphoreWin32HandleInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkExportSemaphoreWin32HandleInfoKHR),
            I## a <- alignment
                      (undefined :: VkExportSemaphoreWin32HandleInfoKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExportSemaphoreWin32HandleInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExportSemaphoreWin32HandleInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkExportSemaphoreWin32HandleInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExportSemaphoreWin32HandleInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExportSemaphoreWin32HandleInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExportSemaphoreWin32HandleInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkExportSemaphoreWin32HandleInfoKHR where
        type VkSTypeMType VkExportSemaphoreWin32HandleInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkExportSemaphoreWin32HandleInfoKHR where
        type VkPNextMType VkExportSemaphoreWin32HandleInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkPAttributes VkExportSemaphoreWin32HandleInfoKHR where
        type VkPAttributesMType VkExportSemaphoreWin32HandleInfoKHR =
             Ptr SECURITY_ATTRIBUTES

        {-# NOINLINE vkPAttributes #-}
        vkPAttributes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes})

        {-# INLINE vkPAttributesByteOffset #-}
        vkPAttributesByteOffset ~_
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}

        {-# INLINE readVkPAttributes #-}
        readVkPAttributes p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}

        {-# INLINE writeVkPAttributes #-}
        writeVkPAttributes p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         HasVkDwAccess VkExportSemaphoreWin32HandleInfoKHR where
        type VkDwAccessMType VkExportSemaphoreWin32HandleInfoKHR = DWORD

        {-# NOINLINE vkDwAccess #-}
        vkDwAccess x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess})

        {-# INLINE vkDwAccessByteOffset #-}
        vkDwAccessByteOffset ~_
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}

        {-# INLINE readVkDwAccess #-}
        readVkDwAccess p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}

        {-# INLINE writeVkDwAccess #-}
        writeVkDwAccess p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         HasVkName VkExportSemaphoreWin32HandleInfoKHR where
        type VkNameMType VkExportSemaphoreWin32HandleInfoKHR = LPCWSTR

        {-# NOINLINE vkName #-}
        vkName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, name})

        {-# INLINE vkNameByteOffset #-}
        vkNameByteOffset ~_
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, name}

        {-# INLINE readVkName #-}
        readVkName p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, name}

        {-# INLINE writeVkName #-}
        writeVkName p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, name}

instance Show VkExportSemaphoreWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkExportSemaphoreWin32HandleInfoKHR {" .
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

data VkD3D12FenceSubmitInfoKHR = VkD3D12FenceSubmitInfoKHR## ByteArray##

instance Eq VkD3D12FenceSubmitInfoKHR where
        (VkD3D12FenceSubmitInfoKHR## a) == (VkD3D12FenceSubmitInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkD3D12FenceSubmitInfoKHR where
        (VkD3D12FenceSubmitInfoKHR## a) `compare`
          (VkD3D12FenceSubmitInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkD3D12FenceSubmitInfoKHR where
        sizeOf ~_ = #{size VkD3D12FenceSubmitInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkD3D12FenceSubmitInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkD3D12FenceSubmitInfoKHR),
            I## a <- alignment (undefined :: VkD3D12FenceSubmitInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkD3D12FenceSubmitInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkD3D12FenceSubmitInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkD3D12FenceSubmitInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkD3D12FenceSubmitInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkD3D12FenceSubmitInfoKHR),
            I## a <- alignment (undefined :: VkD3D12FenceSubmitInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkD3D12FenceSubmitInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkD3D12FenceSubmitInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkD3D12FenceSubmitInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkD3D12FenceSubmitInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkD3D12FenceSubmitInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkD3D12FenceSubmitInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkD3D12FenceSubmitInfoKHR
         where
        type VkSTypeMType VkD3D12FenceSubmitInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkD3D12FenceSubmitInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkD3D12FenceSubmitInfoKHR
         where
        type VkPNextMType VkD3D12FenceSubmitInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkD3D12FenceSubmitInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkWaitSemaphoreValuesCount VkD3D12FenceSubmitInfoKHR where
        type VkWaitSemaphoreValuesCountMType VkD3D12FenceSubmitInfoKHR =
             Word32

        {-# NOINLINE vkWaitSemaphoreValuesCount #-}
        vkWaitSemaphoreValuesCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, waitSemaphoreValuesCount})

        {-# INLINE vkWaitSemaphoreValuesCountByteOffset #-}
        vkWaitSemaphoreValuesCountByteOffset ~_
          = #{offset VkD3D12FenceSubmitInfoKHR, waitSemaphoreValuesCount}

        {-# INLINE readVkWaitSemaphoreValuesCount #-}
        readVkWaitSemaphoreValuesCount p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, waitSemaphoreValuesCount}

        {-# INLINE writeVkWaitSemaphoreValuesCount #-}
        writeVkWaitSemaphoreValuesCount p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, waitSemaphoreValuesCount}

instance {-# OVERLAPPING #-}
         HasVkPWaitSemaphoreValues VkD3D12FenceSubmitInfoKHR where
        type VkPWaitSemaphoreValuesMType VkD3D12FenceSubmitInfoKHR =
             Ptr Word64

        {-# NOINLINE vkPWaitSemaphoreValues #-}
        vkPWaitSemaphoreValues x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, pWaitSemaphoreValues})

        {-# INLINE vkPWaitSemaphoreValuesByteOffset #-}
        vkPWaitSemaphoreValuesByteOffset ~_
          = #{offset VkD3D12FenceSubmitInfoKHR, pWaitSemaphoreValues}

        {-# INLINE readVkPWaitSemaphoreValues #-}
        readVkPWaitSemaphoreValues p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pWaitSemaphoreValues}

        {-# INLINE writeVkPWaitSemaphoreValues #-}
        writeVkPWaitSemaphoreValues p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pWaitSemaphoreValues}

instance {-# OVERLAPPING #-}
         HasVkSignalSemaphoreValuesCount VkD3D12FenceSubmitInfoKHR where
        type VkSignalSemaphoreValuesCountMType VkD3D12FenceSubmitInfoKHR =
             Word32

        {-# NOINLINE vkSignalSemaphoreValuesCount #-}
        vkSignalSemaphoreValuesCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, signalSemaphoreValuesCount})

        {-# INLINE vkSignalSemaphoreValuesCountByteOffset #-}
        vkSignalSemaphoreValuesCountByteOffset ~_
          = #{offset VkD3D12FenceSubmitInfoKHR, signalSemaphoreValuesCount}

        {-# INLINE readVkSignalSemaphoreValuesCount #-}
        readVkSignalSemaphoreValuesCount p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, signalSemaphoreValuesCount}

        {-# INLINE writeVkSignalSemaphoreValuesCount #-}
        writeVkSignalSemaphoreValuesCount p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, signalSemaphoreValuesCount}

instance {-# OVERLAPPING #-}
         HasVkPSignalSemaphoreValues VkD3D12FenceSubmitInfoKHR where
        type VkPSignalSemaphoreValuesMType VkD3D12FenceSubmitInfoKHR =
             Ptr Word64

        {-# NOINLINE vkPSignalSemaphoreValues #-}
        vkPSignalSemaphoreValues x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkD3D12FenceSubmitInfoKHR, pSignalSemaphoreValues})

        {-# INLINE vkPSignalSemaphoreValuesByteOffset #-}
        vkPSignalSemaphoreValuesByteOffset ~_
          = #{offset VkD3D12FenceSubmitInfoKHR, pSignalSemaphoreValues}

        {-# INLINE readVkPSignalSemaphoreValues #-}
        readVkPSignalSemaphoreValues p
          = peekByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pSignalSemaphoreValues}

        {-# INLINE writeVkPSignalSemaphoreValues #-}
        writeVkPSignalSemaphoreValues p
          = pokeByteOff p #{offset VkD3D12FenceSubmitInfoKHR, pSignalSemaphoreValues}

instance Show VkD3D12FenceSubmitInfoKHR where
        showsPrec d x
          = showString "VkD3D12FenceSubmitInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkWaitSemaphoreValuesCount = " .
                            showsPrec d (vkWaitSemaphoreValuesCount x) .
                              showString ", " .
                                showString "vkPWaitSemaphoreValues = " .
                                  showsPrec d (vkPWaitSemaphoreValues x) .
                                    showString ", " .
                                      showString "vkSignalSemaphoreValuesCount = " .
                                        showsPrec d (vkSignalSemaphoreValuesCount x) .
                                          showString ", " .
                                            showString "vkPSignalSemaphoreValues = " .
                                              showsPrec d (vkPSignalSemaphoreValues x) .
                                                showChar '}'

data VkSemaphoreGetWin32HandleInfoKHR = VkSemaphoreGetWin32HandleInfoKHR## ByteArray##

instance Eq VkSemaphoreGetWin32HandleInfoKHR where
        (VkSemaphoreGetWin32HandleInfoKHR## a) ==
          (VkSemaphoreGetWin32HandleInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkSemaphoreGetWin32HandleInfoKHR where
        (VkSemaphoreGetWin32HandleInfoKHR## a) `compare`
          (VkSemaphoreGetWin32HandleInfoKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkSemaphoreGetWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkSemaphoreGetWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSemaphoreGetWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkSemaphoreGetWin32HandleInfoKHR),
            I## a <- alignment (undefined :: VkSemaphoreGetWin32HandleInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkSemaphoreGetWin32HandleInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkSemaphoreGetWin32HandleInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkSemaphoreGetWin32HandleInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkSemaphoreGetWin32HandleInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkSemaphoreGetWin32HandleInfoKHR),
            I## a <- alignment (undefined :: VkSemaphoreGetWin32HandleInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkSemaphoreGetWin32HandleInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkSemaphoreGetWin32HandleInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkSemaphoreGetWin32HandleInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkSemaphoreGetWin32HandleInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkSemaphoreGetWin32HandleInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkSemaphoreGetWin32HandleInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkSemaphoreGetWin32HandleInfoKHR where
        type VkSTypeMType VkSemaphoreGetWin32HandleInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetWin32HandleInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSemaphoreGetWin32HandleInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkSemaphoreGetWin32HandleInfoKHR where
        type VkPNextMType VkSemaphoreGetWin32HandleInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetWin32HandleInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSemaphoreGetWin32HandleInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkSemaphore VkSemaphoreGetWin32HandleInfoKHR where
        type VkSemaphoreMType VkSemaphoreGetWin32HandleInfoKHR =
             VkSemaphore

        {-# NOINLINE vkSemaphore #-}
        vkSemaphore x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetWin32HandleInfoKHR, semaphore})

        {-# INLINE vkSemaphoreByteOffset #-}
        vkSemaphoreByteOffset ~_
          = #{offset VkSemaphoreGetWin32HandleInfoKHR, semaphore}

        {-# INLINE readVkSemaphore #-}
        readVkSemaphore p
          = peekByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, semaphore}

        {-# INLINE writeVkSemaphore #-}
        writeVkSemaphore p
          = pokeByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         HasVkHandleType VkSemaphoreGetWin32HandleInfoKHR where
        type VkHandleTypeMType VkSemaphoreGetWin32HandleInfoKHR =
             VkExternalSemaphoreHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetWin32HandleInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkSemaphoreGetWin32HandleInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, handleType}

instance Show VkSemaphoreGetWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkSemaphoreGetWin32HandleInfoKHR {" .
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
--   > VkResult vkImportSemaphoreWin32HandleKHR
--   >     ( VkDevice device
--   >     , const VkImportSemaphoreWin32HandleInfoKHR* pImportSemaphoreWin32HandleInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkImportSemaphoreWin32HandleKHR.html vkImportSemaphoreWin32HandleKHR registry at www.khronos.org>
foreign import ccall unsafe "vkImportSemaphoreWin32HandleKHR"
               vkImportSemaphoreWin32HandleKHR ::
               VkDevice -- ^ device
                        -> Ptr VkImportSemaphoreWin32HandleInfoKHR -- ^ pImportSemaphoreWin32HandleInfo
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetSemaphoreWin32HandleKHR
--   >     ( VkDevice device
--   >     , const VkSemaphoreGetWin32HandleInfoKHR* pGetWin32HandleInfo
--   >     , HANDLE* pHandle
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetSemaphoreWin32HandleKHR.html vkGetSemaphoreWin32HandleKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetSemaphoreWin32HandleKHR"
               vkGetSemaphoreWin32HandleKHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkSemaphoreGetWin32HandleInfoKHR -- ^ pGetWin32HandleInfo
                                                      -> Ptr HANDLE -- ^ pHandle
                                                                    -> IO VkResult

pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME

_VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME #-}
_VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
  = Ptr "VK_KHR_external_semaphore_win32\NUL"##

is_VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME ::
                                                  CString -> Bool

{-# INLINE is_VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME #-}
is_VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
  = (_VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME ==)

type VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME =
     "VK_KHR_external_semaphore_win32"

pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR =
        VkStructureType 1000078000

pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR =
        VkStructureType 1000078001

pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR =
        VkStructureType 1000078002

pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR =
        VkStructureType 1000078003
