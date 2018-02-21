#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImportSemaphoreFdInfoKHR
       (VkImportSemaphoreFdInfoKHR(..)) where
import           Foreign.Storable
                                                                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalSemaphoreHandleTypeFlagsKHR
                                                                                   (VkExternalSemaphoreHandleTypeFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkSemaphoreImportFlagsKHR
                                                                                   (VkSemaphoreImportFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                   (VkStructureType)
import           Graphics.Vulkan.Types.Handles
                                                                                   (VkSemaphore)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe
                                                                                   (unsafeDupablePerformIO)

-- | > typedef struct VkImportSemaphoreFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSemaphore    semaphore;
--   >     VkSemaphoreImportFlagsKHR flags;
--   >     VkExternalSemaphoreHandleTypeFlagBitsKHR handleType;
--   >     int                              fd;
--   > } VkImportSemaphoreFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImportSemaphoreFdInfoKHR.html VkImportSemaphoreFdInfoKHR registry at www.khronos.org>
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
        type FieldIsArray "sType" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "pNext" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "semaphore" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "flags" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "handleType" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs

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
        type VkFdMType VkImportSemaphoreFdInfoKHR = CInt

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
        type FieldType "fd" VkImportSemaphoreFdInfoKHR = CInt
        type FieldOptional "fd" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fd" VkImportSemaphoreFdInfoKHR =
             #{offset VkImportSemaphoreFdInfoKHR, fd}
        type FieldIsArray "fd" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs

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
