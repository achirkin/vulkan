#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_NV_external_memory_win32
       (-- * Vulkan extension: @VK_NV_external_memory_win32@
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
        -- Extension number: @58@
        --
        -- Required extensions: 'VK_NV_external_memory'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_WIN32_KHR@
        --

        -- ** Required extensions: 'VK_NV_external_memory'.
        VkImportMemoryWin32HandleInfoNV(..),
        VkExportMemoryWin32HandleInfoNV(..), vkGetMemoryWin32HandleNV,
        VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION,
        pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION,
        VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME,
        pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV)
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

-- | > typedef struct VkImportMemoryWin32HandleInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsNV handleType;
--   >     HANDLE                           handle;
--   > } VkImportMemoryWin32HandleInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImportMemoryWin32HandleInfoNV.html VkImportMemoryWin32HandleInfoNV registry at www.khronos.org>
data VkImportMemoryWin32HandleInfoNV = VkImportMemoryWin32HandleInfoNV## ByteArray##

instance Eq VkImportMemoryWin32HandleInfoNV where
        (VkImportMemoryWin32HandleInfoNV## a) ==
          (VkImportMemoryWin32HandleInfoNV## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkImportMemoryWin32HandleInfoNV where
        (VkImportMemoryWin32HandleInfoNV## a) `compare`
          (VkImportMemoryWin32HandleInfoNV## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkImportMemoryWin32HandleInfoNV where
        sizeOf ~_ = #{size VkImportMemoryWin32HandleInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportMemoryWin32HandleInfoNV}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkImportMemoryWin32HandleInfoNV),
            I## a <- alignment (undefined :: VkImportMemoryWin32HandleInfoNV) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkImportMemoryWin32HandleInfoNV##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkImportMemoryWin32HandleInfoNV## ba)
          | I## n <- sizeOf (undefined :: VkImportMemoryWin32HandleInfoNV) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkImportMemoryWin32HandleInfoNV where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkImportMemoryWin32HandleInfoNV),
            I## a <- alignment (undefined :: VkImportMemoryWin32HandleInfoNV) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkImportMemoryWin32HandleInfoNV##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkImportMemoryWin32HandleInfoNV## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkImportMemoryWin32HandleInfoNV##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkImportMemoryWin32HandleInfoNV## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkImportMemoryWin32HandleInfoNV## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkImportMemoryWin32HandleInfoNV## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkImportMemoryWin32HandleInfoNV where
        type VkSTypeMType VkImportMemoryWin32HandleInfoNV = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImportMemoryWin32HandleInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkImportMemoryWin32HandleInfoNV where
        type VkPNextMType VkImportMemoryWin32HandleInfoNV = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImportMemoryWin32HandleInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasVkHandleType VkImportMemoryWin32HandleInfoNV where
        type VkHandleTypeMType VkImportMemoryWin32HandleInfoNV =
             VkExternalMemoryHandleTypeFlagsNV

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoNV, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkImportMemoryWin32HandleInfoNV, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoNV, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoNV, handleType}

instance {-# OVERLAPPING #-}
         HasVkHandle VkImportMemoryWin32HandleInfoNV where
        type VkHandleMType VkImportMemoryWin32HandleInfoNV = HANDLE

        {-# NOINLINE vkHandle #-}
        vkHandle x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoNV, handle})

        {-# INLINE vkHandleByteOffset #-}
        vkHandleByteOffset ~_
          = #{offset VkImportMemoryWin32HandleInfoNV, handle}

        {-# INLINE readVkHandle #-}
        readVkHandle p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoNV, handle}

        {-# INLINE writeVkHandle #-}
        writeVkHandle p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoNV, handle}

instance Show VkImportMemoryWin32HandleInfoNV where
        showsPrec d x
          = showString "VkImportMemoryWin32HandleInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleType = " .
                            showsPrec d (vkHandleType x) .
                              showString ", " .
                                showString "vkHandle = " . showsPrec d (vkHandle x) . showChar '}'

-- | > typedef struct VkExportMemoryWin32HandleInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const SECURITY_ATTRIBUTES*       pAttributes;
--   >     DWORD                            dwAccess;
--   > } VkExportMemoryWin32HandleInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExportMemoryWin32HandleInfoNV.html VkExportMemoryWin32HandleInfoNV registry at www.khronos.org>
data VkExportMemoryWin32HandleInfoNV = VkExportMemoryWin32HandleInfoNV## ByteArray##

instance Eq VkExportMemoryWin32HandleInfoNV where
        (VkExportMemoryWin32HandleInfoNV## a) ==
          (VkExportMemoryWin32HandleInfoNV## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkExportMemoryWin32HandleInfoNV where
        (VkExportMemoryWin32HandleInfoNV## a) `compare`
          (VkExportMemoryWin32HandleInfoNV## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkExportMemoryWin32HandleInfoNV where
        sizeOf ~_ = #{size VkExportMemoryWin32HandleInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportMemoryWin32HandleInfoNV}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkExportMemoryWin32HandleInfoNV),
            I## a <- alignment (undefined :: VkExportMemoryWin32HandleInfoNV) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkExportMemoryWin32HandleInfoNV##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkExportMemoryWin32HandleInfoNV## ba)
          | I## n <- sizeOf (undefined :: VkExportMemoryWin32HandleInfoNV) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkExportMemoryWin32HandleInfoNV where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkExportMemoryWin32HandleInfoNV),
            I## a <- alignment (undefined :: VkExportMemoryWin32HandleInfoNV) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkExportMemoryWin32HandleInfoNV##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkExportMemoryWin32HandleInfoNV## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkExportMemoryWin32HandleInfoNV##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkExportMemoryWin32HandleInfoNV## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkExportMemoryWin32HandleInfoNV## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkExportMemoryWin32HandleInfoNV## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkExportMemoryWin32HandleInfoNV where
        type VkSTypeMType VkExportMemoryWin32HandleInfoNV = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExportMemoryWin32HandleInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkExportMemoryWin32HandleInfoNV where
        type VkPNextMType VkExportMemoryWin32HandleInfoNV = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExportMemoryWin32HandleInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasVkPAttributes VkExportMemoryWin32HandleInfoNV where
        type VkPAttributesMType VkExportMemoryWin32HandleInfoNV =
             Ptr SECURITY_ATTRIBUTES

        {-# NOINLINE vkPAttributes #-}
        vkPAttributes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoNV, pAttributes})

        {-# INLINE vkPAttributesByteOffset #-}
        vkPAttributesByteOffset ~_
          = #{offset VkExportMemoryWin32HandleInfoNV, pAttributes}

        {-# INLINE readVkPAttributes #-}
        readVkPAttributes p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoNV, pAttributes}

        {-# INLINE writeVkPAttributes #-}
        writeVkPAttributes p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoNV, pAttributes}

instance {-# OVERLAPPING #-}
         HasVkDwAccess VkExportMemoryWin32HandleInfoNV where
        type VkDwAccessMType VkExportMemoryWin32HandleInfoNV = DWORD

        {-# NOINLINE vkDwAccess #-}
        vkDwAccess x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoNV, dwAccess})

        {-# INLINE vkDwAccessByteOffset #-}
        vkDwAccessByteOffset ~_
          = #{offset VkExportMemoryWin32HandleInfoNV, dwAccess}

        {-# INLINE readVkDwAccess #-}
        readVkDwAccess p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoNV, dwAccess}

        {-# INLINE writeVkDwAccess #-}
        writeVkDwAccess p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoNV, dwAccess}

instance Show VkExportMemoryWin32HandleInfoNV where
        showsPrec d x
          = showString "VkExportMemoryWin32HandleInfoNV {" .
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
                                  showsPrec d (vkDwAccess x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetMemoryWin32HandleNV
--   >     ( VkDevice device
--   >     , VkDeviceMemory memory
--   >     , VkExternalMemoryHandleTypeFlagsNV handleType
--   >     , HANDLE* pHandle
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetMemoryWin32HandleNV.html vkGetMemoryWin32HandleNV registry at www.khronos.org>
foreign import ccall unsafe "vkGetMemoryWin32HandleNV"
               vkGetMemoryWin32HandleNV ::
               VkDevice -- ^ device
                        ->
                 VkDeviceMemory -- ^ memory
                                ->
                   VkExternalMemoryHandleTypeFlagsNV -- ^ handleType
                                                     -> Ptr HANDLE -- ^ pHandle
                                                                   -> IO VkResult

pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1

type VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1

pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: CString

pattern VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME <-
        (is_VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME -> True)
  where VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
          = _VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME

_VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: CString

{-# INLINE _VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME #-}
_VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  = Ptr "VK_NV_external_memory_win32\NUL"##

is_VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME #-}
is_VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  = (_VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME ==)

type VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME =
     "VK_NV_external_memory_win32"

pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV =
        VkStructureType 1000057000

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV =
        VkStructureType 1000057001
