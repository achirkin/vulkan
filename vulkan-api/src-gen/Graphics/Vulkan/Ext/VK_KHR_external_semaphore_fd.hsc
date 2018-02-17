#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
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
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkImportSemaphoreFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSemaphore    semaphore;
--   >     VkSemaphoreImportFlagsKHR flags;
--   >     VkExternalSemaphoreHandleTypeFlagBitsKHR handleType;
--   >     int                              fd;
--   > } VkImportSemaphoreFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImportSemaphoreFdInfoKHR.html VkImportSemaphoreFdInfoKHR registry at www.khronos.org>
data VkImportSemaphoreFdInfoKHR = VkImportSemaphoreFdInfoKHR## Addr##
                                                              ByteArray##

instance Eq VkImportSemaphoreFdInfoKHR where
        (VkImportSemaphoreFdInfoKHR## a _) ==
          x@(VkImportSemaphoreFdInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportSemaphoreFdInfoKHR where
        (VkImportSemaphoreFdInfoKHR## a _) `compare`
          x@(VkImportSemaphoreFdInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportSemaphoreFdInfoKHR where
        sizeOf ~_ = #{size VkImportSemaphoreFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImportSemaphoreFdInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportSemaphoreFdInfoKHR where
        unsafeAddr (VkImportSemaphoreFdInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportSemaphoreFdInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportSemaphoreFdInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportSemaphoreFdInfoKHR where
        type StructFields VkImportSemaphoreFdInfoKHR =
             '["sType", "pNext", "semaphore", "flags", "handleType", "fd"] -- ' closing tick for hsc2hs
        type CUnionType VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportSemaphoreFdInfoKHR = '[] -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportSemaphoreFdInfoKHR where
        type FieldType "sType" VkImportSemaphoreFdInfoKHR = VkStructureType
        type FieldOptional "sType" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportSemaphoreFdInfoKHR =
             #{offset VkImportSemaphoreFdInfoKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreFdInfoKHR, sType}

instance CanReadField "sType" VkImportSemaphoreFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImportSemaphoreFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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
         HasField "pNext" VkImportSemaphoreFdInfoKHR where
        type FieldType "pNext" VkImportSemaphoreFdInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportSemaphoreFdInfoKHR =
             #{offset VkImportSemaphoreFdInfoKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreFdInfoKHR, pNext}

instance CanReadField "pNext" VkImportSemaphoreFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImportSemaphoreFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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

instance {-# OVERLAPPING #-}
         HasField "semaphore" VkImportSemaphoreFdInfoKHR where
        type FieldType "semaphore" VkImportSemaphoreFdInfoKHR = VkSemaphore
        type FieldOptional "semaphore" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "semaphore" VkImportSemaphoreFdInfoKHR =
             #{offset VkImportSemaphoreFdInfoKHR, semaphore}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreFdInfoKHR, semaphore}

instance CanReadField "semaphore" VkImportSemaphoreFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkSemaphore

        {-# INLINE readField #-}
        readField = readVkSemaphore

instance CanWriteField "semaphore" VkImportSemaphoreFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSemaphore

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
         HasField "flags" VkImportSemaphoreFdInfoKHR where
        type FieldType "flags" VkImportSemaphoreFdInfoKHR =
             VkSemaphoreImportFlagsKHR
        type FieldOptional "flags" VkImportSemaphoreFdInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkImportSemaphoreFdInfoKHR =
             #{offset VkImportSemaphoreFdInfoKHR, flags}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreFdInfoKHR, flags}

instance CanReadField "flags" VkImportSemaphoreFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkImportSemaphoreFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

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

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportSemaphoreFdInfoKHR where
        type FieldType "handleType" VkImportSemaphoreFdInfoKHR =
             VkExternalSemaphoreHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportSemaphoreFdInfoKHR =
             #{offset VkImportSemaphoreFdInfoKHR, handleType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreFdInfoKHR, handleType}

instance CanReadField "handleType" VkImportSemaphoreFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType" VkImportSemaphoreFdInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

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

instance {-# OVERLAPPING #-}
         HasField "fd" VkImportSemaphoreFdInfoKHR where
        type FieldType "fd" VkImportSemaphoreFdInfoKHR =
             #{type int}
        type FieldOptional "fd" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fd" VkImportSemaphoreFdInfoKHR =
             #{offset VkImportSemaphoreFdInfoKHR, fd}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportSemaphoreFdInfoKHR, fd}

instance CanReadField "fd" VkImportSemaphoreFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkFd

        {-# INLINE readField #-}
        readField = readVkFd

instance CanWriteField "fd" VkImportSemaphoreFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFd

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

-- | > typedef struct VkSemaphoreGetFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSemaphore                      semaphore;
--   >     VkExternalSemaphoreHandleTypeFlagBitsKHR handleType;
--   > } VkSemaphoreGetFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSemaphoreGetFdInfoKHR.html VkSemaphoreGetFdInfoKHR registry at www.khronos.org>
data VkSemaphoreGetFdInfoKHR = VkSemaphoreGetFdInfoKHR## Addr##
                                                        ByteArray##

instance Eq VkSemaphoreGetFdInfoKHR where
        (VkSemaphoreGetFdInfoKHR## a _) == x@(VkSemaphoreGetFdInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSemaphoreGetFdInfoKHR where
        (VkSemaphoreGetFdInfoKHR## a _) `compare`
          x@(VkSemaphoreGetFdInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSemaphoreGetFdInfoKHR where
        sizeOf ~_ = #{size VkSemaphoreGetFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSemaphoreGetFdInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSemaphoreGetFdInfoKHR where
        unsafeAddr (VkSemaphoreGetFdInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSemaphoreGetFdInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSemaphoreGetFdInfoKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSemaphoreGetFdInfoKHR where
        type StructFields VkSemaphoreGetFdInfoKHR =
             '["sType", "pNext", "semaphore", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkSemaphoreGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSemaphoreGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSemaphoreGetFdInfoKHR = '[] -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         HasField "sType" VkSemaphoreGetFdInfoKHR where
        type FieldType "sType" VkSemaphoreGetFdInfoKHR = VkStructureType
        type FieldOptional "sType" VkSemaphoreGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSemaphoreGetFdInfoKHR =
             #{offset VkSemaphoreGetFdInfoKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSemaphoreGetFdInfoKHR, sType}

instance CanReadField "sType" VkSemaphoreGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkSemaphoreGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSemaphoreGetFdInfoKHR where
        type FieldType "pNext" VkSemaphoreGetFdInfoKHR = Ptr Void
        type FieldOptional "pNext" VkSemaphoreGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSemaphoreGetFdInfoKHR =
             #{offset VkSemaphoreGetFdInfoKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSemaphoreGetFdInfoKHR, pNext}

instance CanReadField "pNext" VkSemaphoreGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkSemaphoreGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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
         HasField "semaphore" VkSemaphoreGetFdInfoKHR where
        type FieldType "semaphore" VkSemaphoreGetFdInfoKHR = VkSemaphore
        type FieldOptional "semaphore" VkSemaphoreGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "semaphore" VkSemaphoreGetFdInfoKHR =
             #{offset VkSemaphoreGetFdInfoKHR, semaphore}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSemaphoreGetFdInfoKHR, semaphore}

instance CanReadField "semaphore" VkSemaphoreGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkSemaphore

        {-# INLINE readField #-}
        readField = readVkSemaphore

instance CanWriteField "semaphore" VkSemaphoreGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSemaphore

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

instance {-# OVERLAPPING #-}
         HasField "handleType" VkSemaphoreGetFdInfoKHR where
        type FieldType "handleType" VkSemaphoreGetFdInfoKHR =
             VkExternalSemaphoreHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkSemaphoreGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkSemaphoreGetFdInfoKHR =
             #{offset VkSemaphoreGetFdInfoKHR, handleType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSemaphoreGetFdInfoKHR, handleType}

instance CanReadField "handleType" VkSemaphoreGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType" VkSemaphoreGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

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

{-# INLINE _VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
  = Ptr "VK_KHR_external_semaphore_fd\NUL"##

{-# INLINE is_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
  = eqCStrings _VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME

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
