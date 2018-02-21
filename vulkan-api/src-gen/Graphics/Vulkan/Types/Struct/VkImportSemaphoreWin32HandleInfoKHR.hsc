#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImportSemaphoreWin32HandleInfoKHR
       (VkImportSemaphoreWin32HandleInfoKHR(..)) where
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
import           Graphics.Vulkan.Types.Include
                                                                                   (HANDLE,
                                                                                   LPCWSTR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe
                                                                                   (unsafeDupablePerformIO)

-- | > typedef struct VkImportSemaphoreWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSemaphore    semaphore;
--   >     VkSemaphoreImportFlagsKHR flags;
--   >     VkExternalSemaphoreHandleTypeFlagBitsKHR handleType;
--   >     HANDLE           handle;
--   >     LPCWSTR          name;
--   > } VkImportSemaphoreWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImportSemaphoreWin32HandleInfoKHR.html VkImportSemaphoreWin32HandleInfoKHR registry at www.khronos.org>
data VkImportSemaphoreWin32HandleInfoKHR = VkImportSemaphoreWin32HandleInfoKHR## Addr##
                                                                                ByteArray##

instance Eq VkImportSemaphoreWin32HandleInfoKHR where
        (VkImportSemaphoreWin32HandleInfoKHR## a _) ==
          x@(VkImportSemaphoreWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportSemaphoreWin32HandleInfoKHR where
        (VkImportSemaphoreWin32HandleInfoKHR## a _) `compare`
          x@(VkImportSemaphoreWin32HandleInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportSemaphoreWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkImportSemaphoreWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportSemaphoreWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportSemaphoreWin32HandleInfoKHR
         where
        unsafeAddr (VkImportSemaphoreWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportSemaphoreWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportSemaphoreWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportSemaphoreWin32HandleInfoKHR where
        type StructFields VkImportSemaphoreWin32HandleInfoKHR =
             '["sType", "pNext", "semaphore", "flags", "handleType", "handle", -- ' closing tick for hsc2hs
               "name"]
        type CUnionType VkImportSemaphoreWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportSemaphoreWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportSemaphoreWin32HandleInfoKHR = '[] -- ' closing tick for hsc2hs

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
         HasField "sType" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "sType" VkImportSemaphoreWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, sType}

instance CanReadField "sType" VkImportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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
         HasField "pNext" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "pNext" VkImportSemaphoreWin32HandleInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext}

instance CanReadField "pNext" VkImportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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
         HasField "semaphore" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "semaphore" VkImportSemaphoreWin32HandleInfoKHR =
             VkSemaphore
        type FieldOptional "semaphore" VkImportSemaphoreWin32HandleInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "semaphore" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore}
        type FieldIsArray "semaphore" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore}

instance CanReadField "semaphore"
           VkImportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSemaphore

        {-# INLINE readField #-}
        readField = readVkSemaphore

instance CanWriteField "semaphore"
           VkImportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSemaphore

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
         HasField "flags" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "flags" VkImportSemaphoreWin32HandleInfoKHR =
             VkSemaphoreImportFlagsKHR
        type FieldOptional "flags" VkImportSemaphoreWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, flags}
        type FieldIsArray "flags" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, flags}

instance CanReadField "flags" VkImportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkImportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

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
         HasField "handleType" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "handleType" VkImportSemaphoreWin32HandleInfoKHR =
             VkExternalSemaphoreHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkImportSemaphoreWin32HandleInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType}
        type FieldIsArray "handleType" VkImportSemaphoreWin32HandleInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType}

instance CanReadField "handleType"
           VkImportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType"
           VkImportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

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
         HasField "handle" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "handle" VkImportSemaphoreWin32HandleInfoKHR =
             HANDLE
        type FieldOptional "handle" VkImportSemaphoreWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handle" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, handle}
        type FieldIsArray "handle" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, handle}

instance CanReadField "handle" VkImportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandle

        {-# INLINE readField #-}
        readField = readVkHandle

instance CanWriteField "handle" VkImportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandle

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

instance {-# OVERLAPPING #-}
         HasField "name" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "name" VkImportSemaphoreWin32HandleInfoKHR = LPCWSTR
        type FieldOptional "name" VkImportSemaphoreWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "name" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, name}
        type FieldIsArray "name" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, name}

instance CanReadField "name" VkImportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkName

        {-# INLINE readField #-}
        readField = readVkName

instance CanWriteField "name" VkImportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkName

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
