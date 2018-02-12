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

-- | > typedef struct VkImportMemoryWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagBitsKHR handleType;
--   >     HANDLE           handle;
--   >     LPCWSTR          name;
--   > } VkImportMemoryWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImportMemoryWin32HandleInfoKHR.html VkImportMemoryWin32HandleInfoKHR registry at www.khronos.org>
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
        type StructFields VkImportMemoryWin32HandleInfoKHR =
             '["sType", "pNext", "handleType", "handle", "name"] -- ' closing tick for hsc2hs

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
         HasField "sType" VkImportMemoryWin32HandleInfoKHR where
        type FieldType "sType" VkImportMemoryWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkImportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportMemoryWin32HandleInfoKHR =
             #{offset VkImportMemoryWin32HandleInfoKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoKHR, sType}

instance CanReadField "sType" VkImportMemoryWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImportMemoryWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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
         HasField "pNext" VkImportMemoryWin32HandleInfoKHR where
        type FieldType "pNext" VkImportMemoryWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportMemoryWin32HandleInfoKHR =
             #{offset VkImportMemoryWin32HandleInfoKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoKHR, pNext}

instance CanReadField "pNext" VkImportMemoryWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImportMemoryWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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
         HasField "handleType" VkImportMemoryWin32HandleInfoKHR where
        type FieldType "handleType" VkImportMemoryWin32HandleInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkImportMemoryWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportMemoryWin32HandleInfoKHR =
             #{offset VkImportMemoryWin32HandleInfoKHR, handleType}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoKHR, handleType}

instance CanReadField "handleType" VkImportMemoryWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType"
           VkImportMemoryWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

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
         HasField "handle" VkImportMemoryWin32HandleInfoKHR where
        type FieldType "handle" VkImportMemoryWin32HandleInfoKHR = HANDLE
        type FieldOptional "handle" VkImportMemoryWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handle" VkImportMemoryWin32HandleInfoKHR =
             #{offset VkImportMemoryWin32HandleInfoKHR, handle}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoKHR, handle}

instance CanReadField "handle" VkImportMemoryWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandle

        {-# INLINE readField #-}
        readField = readVkHandle

instance CanWriteField "handle" VkImportMemoryWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandle

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

instance {-# OVERLAPPING #-}
         HasField "name" VkImportMemoryWin32HandleInfoKHR where
        type FieldType "name" VkImportMemoryWin32HandleInfoKHR = LPCWSTR
        type FieldOptional "name" VkImportMemoryWin32HandleInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "name" VkImportMemoryWin32HandleInfoKHR =
             #{offset VkImportMemoryWin32HandleInfoKHR, name}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoKHR, name}

instance CanReadField "name" VkImportMemoryWin32HandleInfoKHR where
        {-# INLINE getField #-}
        getField = vkName

        {-# INLINE readField #-}
        readField = readVkName

instance CanWriteField "name" VkImportMemoryWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkName

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

-- | > typedef struct VkExportMemoryWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const SECURITY_ATTRIBUTES* pAttributes;
--   >     DWORD                            dwAccess;
--   >     LPCWSTR                          name;
--   > } VkExportMemoryWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExportMemoryWin32HandleInfoKHR.html VkExportMemoryWin32HandleInfoKHR registry at www.khronos.org>
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
        type StructFields VkExportMemoryWin32HandleInfoKHR =
             '["sType", "pNext", "pAttributes", "dwAccess", "name"] -- ' closing tick for hsc2hs

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
         HasField "sType" VkExportMemoryWin32HandleInfoKHR where
        type FieldType "sType" VkExportMemoryWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportMemoryWin32HandleInfoKHR =
             #{offset VkExportMemoryWin32HandleInfoKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoKHR, sType}

instance CanReadField "sType" VkExportMemoryWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkExportMemoryWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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
         HasField "pNext" VkExportMemoryWin32HandleInfoKHR where
        type FieldType "pNext" VkExportMemoryWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkExportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportMemoryWin32HandleInfoKHR =
             #{offset VkExportMemoryWin32HandleInfoKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoKHR, pNext}

instance CanReadField "pNext" VkExportMemoryWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkExportMemoryWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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
         HasField "pAttributes" VkExportMemoryWin32HandleInfoKHR where
        type FieldType "pAttributes" VkExportMemoryWin32HandleInfoKHR =
             Ptr SECURITY_ATTRIBUTES
        type FieldOptional "pAttributes" VkExportMemoryWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pAttributes" VkExportMemoryWin32HandleInfoKHR =
             #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes}

instance CanReadField "pAttributes"
           VkExportMemoryWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPAttributes

        {-# INLINE readField #-}
        readField = readVkPAttributes

instance CanWriteField "pAttributes"
           VkExportMemoryWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPAttributes

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
         HasField "dwAccess" VkExportMemoryWin32HandleInfoKHR where
        type FieldType "dwAccess" VkExportMemoryWin32HandleInfoKHR = DWORD
        type FieldOptional "dwAccess" VkExportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "dwAccess" VkExportMemoryWin32HandleInfoKHR =
             #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess}

instance CanReadField "dwAccess" VkExportMemoryWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkDwAccess

        {-# INLINE readField #-}
        readField = readVkDwAccess

instance CanWriteField "dwAccess" VkExportMemoryWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDwAccess

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

instance {-# OVERLAPPING #-}
         HasField "name" VkExportMemoryWin32HandleInfoKHR where
        type FieldType "name" VkExportMemoryWin32HandleInfoKHR = LPCWSTR
        type FieldOptional "name" VkExportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "name" VkExportMemoryWin32HandleInfoKHR =
             #{offset VkExportMemoryWin32HandleInfoKHR, name}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoKHR, name}

instance CanReadField "name" VkExportMemoryWin32HandleInfoKHR where
        {-# INLINE getField #-}
        getField = vkName

        {-# INLINE readField #-}
        readField = readVkName

instance CanWriteField "name" VkExportMemoryWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkName

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

-- | > typedef struct VkMemoryWin32HandlePropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         memoryTypeBits;
--   > } VkMemoryWin32HandlePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryWin32HandlePropertiesKHR.html VkMemoryWin32HandlePropertiesKHR registry at www.khronos.org>
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
        type StructFields VkMemoryWin32HandlePropertiesKHR =
             '["sType", "pNext", "memoryTypeBits"] -- ' closing tick for hsc2hs

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
         HasField "sType" VkMemoryWin32HandlePropertiesKHR where
        type FieldType "sType" VkMemoryWin32HandlePropertiesKHR =
             VkStructureType
        type FieldOptional "sType" VkMemoryWin32HandlePropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryWin32HandlePropertiesKHR =
             #{offset VkMemoryWin32HandlePropertiesKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryWin32HandlePropertiesKHR, sType}

instance CanReadField "sType" VkMemoryWin32HandlePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

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
         HasField "pNext" VkMemoryWin32HandlePropertiesKHR where
        type FieldType "pNext" VkMemoryWin32HandlePropertiesKHR = Ptr Void
        type FieldOptional "pNext" VkMemoryWin32HandlePropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryWin32HandlePropertiesKHR =
             #{offset VkMemoryWin32HandlePropertiesKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryWin32HandlePropertiesKHR, pNext}

instance CanReadField "pNext" VkMemoryWin32HandlePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

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

instance {-# OVERLAPPING #-}
         HasField "memoryTypeBits" VkMemoryWin32HandlePropertiesKHR where
        type FieldType "memoryTypeBits" VkMemoryWin32HandlePropertiesKHR =
             Word32
        type FieldOptional "memoryTypeBits"
               VkMemoryWin32HandlePropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypeBits" VkMemoryWin32HandlePropertiesKHR
             =
             #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits}

instance CanReadField "memoryTypeBits"
           VkMemoryWin32HandlePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMemoryTypeBits

        {-# INLINE readField #-}
        readField = readVkMemoryTypeBits

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

-- | > typedef struct VkMemoryGetWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceMemory                   memory;
--   >     VkExternalMemoryHandleTypeFlagBitsKHR handleType;
--   > } VkMemoryGetWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMemoryGetWin32HandleInfoKHR.html VkMemoryGetWin32HandleInfoKHR registry at www.khronos.org>
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
        type StructFields VkMemoryGetWin32HandleInfoKHR =
             '["sType", "pNext", "memory", "handleType"] -- ' closing tick for hsc2hs

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
         HasField "sType" VkMemoryGetWin32HandleInfoKHR where
        type FieldType "sType" VkMemoryGetWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryGetWin32HandleInfoKHR =
             #{offset VkMemoryGetWin32HandleInfoKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetWin32HandleInfoKHR, sType}

instance CanReadField "sType" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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
         HasField "pNext" VkMemoryGetWin32HandleInfoKHR where
        type FieldType "pNext" VkMemoryGetWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryGetWin32HandleInfoKHR =
             #{offset VkMemoryGetWin32HandleInfoKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetWin32HandleInfoKHR, pNext}

instance CanReadField "pNext" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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
         HasField "memory" VkMemoryGetWin32HandleInfoKHR where
        type FieldType "memory" VkMemoryGetWin32HandleInfoKHR =
             VkDeviceMemory
        type FieldOptional "memory" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkMemoryGetWin32HandleInfoKHR =
             #{offset VkMemoryGetWin32HandleInfoKHR, memory}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetWin32HandleInfoKHR, memory}

instance CanReadField "memory" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE getField #-}
        getField = vkMemory

        {-# INLINE readField #-}
        readField = readVkMemory

instance CanWriteField "memory" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkMemory

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

instance {-# OVERLAPPING #-}
         HasField "handleType" VkMemoryGetWin32HandleInfoKHR where
        type FieldType "handleType" VkMemoryGetWin32HandleInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkMemoryGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkMemoryGetWin32HandleInfoKHR =
             #{offset VkMemoryGetWin32HandleInfoKHR, handleType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetWin32HandleInfoKHR, handleType}

instance CanReadField "handleType" VkMemoryGetWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType" VkMemoryGetWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

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

{-# INLINE _VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  = Ptr "VK_KHR_external_memory_win32\NUL"##

{-# INLINE is_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
  = eqCStrings _VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME

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
